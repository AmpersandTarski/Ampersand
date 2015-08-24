{-# LANGUAGE ScopedTypeVariables #-}
-- This module provides an interface to be able to parse a script and to
-- return an FSpec, as tuned by the command line options.
-- This might include that RAP is included in the returned FSpec.
module Database.Design.Ampersand.Input.Parsing (
    parseADL, parseADL1pExpr, parseRule, parseCtx, runParser
) where

import Control.Applicative
import Data.List
import Data.Char(toLower)
import Data.Traversable (sequenceA)
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Input.ADL1.CtxError
import Database.Design.Ampersand.Input.ADL1.Lexer
import Database.Design.Ampersand.Input.ADL1.Parser
import Database.Design.Ampersand.Core.ParseTree (mkContextOfPopsOnly)
import Database.Design.Ampersand.Misc
import Prelude hiding (putStrLn, writeFile) -- make sure everything is UTF8
import System.Directory
import System.FilePath
import Text.Parsec.Error (Message(..), showErrorMessages, errorMessages, ParseError, errorPos)
import Text.Parsec.Prim (runP)
import Database.Design.Ampersand.Input.Xslx.XLSX
import Control.Exception

fatal :: Int -> String -> a
fatal = fatalMsg "Parsing"

-- | Parse an Ampersand file and all transitive includes
parseADL ::  Options                -- ^ The options given through the command line
         -> FilePath                -- ^ The path of the file to be parsed
         -> IO (Guarded P_Context)  -- ^ The resulting context
parseADL opts filePath =
  whenCheckedIO (parseSingleADL opts filePath) $ \(ctxt, filePaths) ->
    whenCheckedIO (parseADLs opts [filePath] filePaths) $ \ctxts ->
      return $ Checked $ foldl mergeContexts ctxt ctxts

-- | Parses several ADL files
parseADLs :: Options                    -- ^ The options given through the command line
          -> [FilePath]                 -- ^ The list of files that have already been parsed
          -> [FilePath]                 -- ^ The list of files to parse
          -> IO (Guarded [P_Context])   -- ^ The resulting contexts
parseADLs _    _               []        = return $ Checked []
parseADLs opts parsedFilePaths filePaths =
 do { let filePathsToParse = nub filePaths \\ parsedFilePaths
    ; whenCheckedIO (sequenceA <$> mapM (parseSingleADL opts) filePathsToParse) $ \ctxtNewFilePathss ->
       do { let (ctxts, newFilesToParse) = unzip ctxtNewFilePathss
          ; whenCheckedIO (parseADLs opts (parsedFilePaths ++ filePaths) $ concat newFilesToParse) $ \ctxts' ->
              return $ Checked $ ctxts ++ ctxts'
          }
    }

-- | Parse an Ampersand file, but not its includes (which are simply returned as a list)
parseSingleADL :: Options -> FilePath -> IO (Guarded (P_Context, [FilePath]))
parseSingleADL opts filePath
 = do verboseLn opts $ "Reading file " ++ filePath
      exists <- doesFileExist filePath
      if exists 
      then parseSingleADL'
      else return . makeError $ "Could not find `"++filePath++"`."
    where
     parseSingleADL' :: IO(Guarded (P_Context, [FilePath]))
     parseSingleADL'
         | extension == ".xlsx" = 
             do { popFromExcel <- catchInvalidXlsx $ parseXlsxFile opts filePath
                ; return ((\pops -> (mkContextOfPopsOnly pops,[])) <$> popFromExcel)  -- Excel file cannot contain include files
                }
         | otherwise =   
             do { mFileContents <- readUTF8File filePath
                ; case mFileContents of
                    Left err -> return $ makeError ("ERROR reading file " ++ filePath ++ ":\n" ++ err)
                    Right fileContents ->
                         whenCheckedIO (return $ parseCtx filePath fileContents) $ \(ctxts, relativePaths) -> 
                               do filePaths <- mapM normalizePath relativePaths
                                  return (Checked (ctxts, filePaths))
            }
         where normalizePath relativePath = canonicalizePath $ takeDirectory filePath </> relativePath 
               extension = map toLower $ takeExtension filePath
               catchInvalidXlsx :: IO a -> IO a 
               catchInvalidXlsx m = catch m f 
                 where f :: SomeException -> IO a
                       f exception = fatal 34 $ "The file does not seem to have a valid .xlsx structure:\n  "++show exception
             
parseErrors :: Lang -> ParseError -> [CtxError]
parseErrors lang err = [PE (Message msg)]
                where msg :: String
                      msg = "In file " ++ show (errorPos err) ++ ":" ++ showLang lang (errorMessages err)
                      showLang :: Lang -> [Message] -> String
                      showLang English = showErrorMessages "or" "unknown parse error"   "at that point expecting" "Parsing stumbled upon" "end of input"
                      showLang Dutch   = showErrorMessages "of" "onbekende parsingfout" "verwacht"  "onverwacht" "einde van de invoer"

parse :: AmpParser a -> FilePath -> [Token] -> Guarded a
parse p fn ts =
      -- runP :: Parsec s u a -> u -> FilePath -> s -> Either ParseError a 
    case runP p pos fn ts of
        --TODO: Add language support to the parser errors
        Left err -> Errors $ parseErrors English err
        Right a -> Checked a
    where pos | null ts   = initPos fn
              | otherwise = tokPos (head ts)

--TODO: Give the errors in a better way
lexerError2CtxError :: LexerError -> CtxError
lexerError2CtxError (LexerError pos err) = 
   PE (Message ("Lexer error at "++show pos++"\n  "
                ++ intercalate "\n    " (showLexerErrorInfo err)
               )
      )

-- | Runs the given parser
runParser :: AmpParser a -- ^ The parser to run
          -> FilePath    -- ^ Name of the file (for error messages)
          -> String      -- ^ String to parse
          -> Guarded a   -- ^ The result
runParser parser filename input =
  -- lexer :: [Options] -> String -> [Char] -> Either LexerError ([Token], [LexerWarning])
  --TODO: Give options to the lexer
  let lexed = lexer [] filename input
  in case lexed of
    Left err -> Errors [lexerError2CtxError err]
    --TODO: Do something with the warnings. The warnings cannot be shown with the current Guarded data type
    Right (tokens, _)  -> whenChecked (parse parser filename tokens) Checked

-- | Parses an isolated rule
-- In order to read derivation rules, we use the Ampersand parser.
-- Since it is applied on static code only, error messagea may be produced as fatals.
parseRule :: String         -- ^ The string to be parsed
          -> Term TermPrim  -- ^ The resulting rule
parseRule str
   = case  runParser pRule "inside Haskell code" str of
       Checked result -> result
       Errors  msg    -> fatal 274 ("Parse errors in "++str++":\n   "++show msg)

-- | Parses an isolated ADL1 expression string
parseADL1pExpr :: String            -- ^ The string to be parsed
               -> FilePath          -- ^ The name of the file (used for error messages)
               -> Either String (Term TermPrim)  -- ^ The result: Either an error message, or a good result
parseADL1pExpr str fn =
  case runParser pTerm fn str of
      Checked result -> Right result
      Errors  msg    -> Left $ "Parse errors:\n"++show msg

-- | Parses an Ampersand context
parseCtx :: FilePath -- ^ The file name (used for error messages)
         -> String   -- ^ The string to be parsed
         -> Guarded (P_Context, [String]) -- ^ The context and a list of included files
parseCtx = runParser pContext

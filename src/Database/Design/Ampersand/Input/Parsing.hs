{-# LANGUAGE ScopedTypeVariables #-}
-- This module provides an interface to be able to parse a script and to
-- return an FSpec, as tuned by the command line options.
-- This might include that RAP is included in the returned FSpec.
module Database.Design.Ampersand.Input.Parsing (
    parseADL, parseADL1pExpr, parseRule, parseCtx, runParser
) where

import Control.Applicative
import Data.List
import Data.Traversable (sequenceA)
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Input.ADL1.CtxError
import Database.Design.Ampersand.Input.ADL1.Lexer
import Database.Design.Ampersand.Input.ADL1.Parser
import Database.Design.Ampersand.Misc
import Prelude hiding (putStrLn, writeFile) -- make sure everything is UTF8
import System.Directory
import System.FilePath
import Text.Parsec.Error (Message(..), showErrorMessages, errorMessages, ParseError, errorPos)
import Text.Parsec.Prim (runP)

fatal :: Int -> String -> a
fatal = fatalMsg "Parsing"

-- Parse an Ampersand file and all transitive includes
parseADL ::  Options -> FilePath -> IO (Guarded P_Context)
parseADL opts filePath =
  whenCheckedIO (parseSingleADL opts filePath) $ \(ctxt, filePaths) ->
    whenCheckedIO (parseADLs opts [filePath] filePaths) $ \ctxts ->
      return $ Checked $ foldl mergeContexts ctxt ctxts

parseADLs :: Options -> [FilePath] -> [FilePath] -> IO (Guarded [P_Context])
parseADLs _    _               []        = return $ Checked []
parseADLs opts parsedFilePaths filePaths =
 do { let filePathsToParse = nub filePaths \\ parsedFilePaths
    ; whenCheckedIO (sequenceA <$> mapM (parseSingleADL opts) filePathsToParse) $ \ctxtNewFilePathss ->
       do { let (ctxts, newFilessToParse) = unzip ctxtNewFilePathss
          ; whenCheckedIO (parseADLs opts (parsedFilePaths ++ filePaths) $ concat newFilessToParse) $ \ctxts' ->
              return $ Checked $ ctxts ++ ctxts'
          }
    }

-- Parse an Ampersand file, but not its includes (which are simply returned as a list)
parseSingleADL :: Options -> FilePath -> IO (Guarded (P_Context, [FilePath]))
parseSingleADL opts filePath =
 do { verboseLn opts $ "Reading file " ++ filePath
    ; mFileContents <- readUTF8File filePath
    ; case mFileContents of
        Left err -> return $ makeError ("ERROR reading file " ++ filePath ++ ":\n" ++ err)
        Right fileContents ->
             whenCheckedIO (return $ parseCtx filePath fileContents) $ \(ctxts, relativePaths) -> 
                   do filePaths <- mapM normalizePath relativePaths
                      return (Checked (ctxts, filePaths))
    }
 where normalizePath relativePath = canonicalizePath $ takeDirectory filePath </> relativePath 

parseErrors :: Lang -> ParseError -> [CtxError]
parseErrors lang err = [PE (Message msg)]
                where msg :: String
                      msg = show (errorPos err) ++ ":" ++ showLang lang (errorMessages err)
                      showLang :: Lang -> [Message] -> String
                      showLang English = showErrorMessages "or" "unknown parse error"   "expecting" "unexpected" "end of input"
                      showLang Dutch   = showErrorMessages "of" "onbekende parsingfout" "verwacht"  "onverwacht" "einde van de invoer"

parse :: AmpParser a -> [Token] -> Guarded a
parse p ts =
      -- runP :: Parsec s u a -> u -> FilePath -> s -> Either ParseError a 
    case runP p pos fn ts of
        --TODO: Add language support to the parser errors
        Left err -> Errors $ parseErrors English err
        Right a -> Checked a
    where pos = tokPos (head ts)
          fn  = nm pos where nm (FilePos file _ _ ) = file

--TODO: Give the errors in a better way
lexerErrors :: LexerError -> [CtxError]
lexerErrors err = [PE (Message ("Lexer error "++show err))]

runParser :: AmpParser a -> Filename -> String -> Guarded a
runParser parser filename input =
  -- lexer :: [Options] -> String -> [Char] -> Either LexerError ([Token], [LexerWarning])
  --TODO: Give options to the lexer
  let lexed = lexer [] filename input
  in case lexed of
    Left err -> Errors $ lexerErrors err
    --TODO: Do something with the warnings. The warnings cannot be shown with the current Guarded data type
    Right (tokens, _)  -> whenChecked (parse parser tokens) Checked

-- In order to read derivation rules, we use the Ampersand parser.
-- Since it is applied on static code only, error messagea may be produced as fatals.
parseRule :: String -> Term TermPrim
parseRule str
   = case  runParser pRule "inside Haskell code" str of
       Checked result -> result
       Errors  msg    -> fatal 274 ("Parse errors in "++str++":\n   "++show msg)

-- | Parse isolated ADL1 expression strings
parseADL1pExpr :: String            -- ^ The string to be parsed
               -> FilePath          -- ^ The name of the file (used for error messages)
               -> Either String (Term TermPrim)  -- ^ The result: Either an error message,  or a good result
parseADL1pExpr str fn =
  case runParser pTerm fn str of
      Checked result -> Right result
      Errors  msg    -> Left $ "Parse errors:\n"++show msg

parseCtx :: FilePath -> String -> Guarded (P_Context, [String])
parseCtx = runParser pContext

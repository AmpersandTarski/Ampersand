{-# LANGUAGE ScopedTypeVariables #-}
-- This module provides an interface to be able to parse a script and to
-- return an FSpec, as tuned by the command line options.
-- This might include that RAP is included in the returned FSpec.
module Ampersand.Input.Parsing (
      parseADL
    , parseMeta
    , parseSystemContext
    , parseRule
    , runParser
) where

import           Ampersand.ADL1
import           Ampersand.Input.PreProcessor
import           Ampersand.Basics
import           Ampersand.Core.ParseTree (mkContextOfPopsOnly)
import           Ampersand.Input.ADL1.CtxError
import           Ampersand.Input.ADL1.Lexer
import           Ampersand.Input.ADL1.Parser
import           Ampersand.Input.Xslx.XLSX
import           Ampersand.Prototype.StaticFiles_Generated(getStaticFileContent,FileKind(FormalAmpersand,SystemContext))
import           Ampersand.Misc
import           Control.Exception
import           Data.Char(toLower)
import           Data.List
import qualified Data.List.NonEmpty as NEL (NonEmpty(..))
import           Data.Maybe
import           System.Directory
import           System.FilePath
import           Text.Parsec.Error (Message(..), showErrorMessages, errorMessages, ParseError, errorPos)
import           Text.Parsec.Prim (runP)

-- | Parse an Ampersand file and all transitive includes
parseADL :: Options                    -- ^ The options given through the command line
         -> FilePath   -- ^ The path of the file to be parsed, either absolute or relative to the current user's path
         -> IO (Guarded P_Context)     -- ^ The resulting context
parseADL opts fp = do curDir <- getCurrentDirectory
                      canonical <- canonicalizePath fp
                      parseThing opts (ParseCandidate (Just curDir) Nothing fp Nothing canonical [])

parseMeta :: Options -> IO (Guarded P_Context)
parseMeta opts = parseThing opts (ParseCandidate Nothing (Just $ Origin "Formal Ampersand specification") "AST.adl" (Just FormalAmpersand) "AST.adl" [])

parseSystemContext :: Options -> IO (Guarded P_Context)
parseSystemContext opts = parseThing opts (ParseCandidate Nothing (Just $ Origin "Ampersand specific system context") "SystemContext.adl" (Just SystemContext) "SystemContext.adl" [])

parseThing :: Options -> ParseCandidate -> IO (Guarded P_Context) 
parseThing opts pc =
  whenCheckedIO (parseADLs opts [] [pc] ) $ \ctxts ->
      return . pure $ foldl1 mergeContexts ctxts

-- | Parses several ADL files
parseADLs :: Options                  -- ^ The options given through the command line
          -> [ParseCandidate]         -- ^ The list of files that have already been parsed
          -> [ParseCandidate]         -- ^ A list of files that still are to be parsed.
          -> IO (Guarded [P_Context]) -- ^ The resulting contexts
parseADLs opts parsedFilePaths fpIncludes =
  case fpIncludes of
    [] -> return $ pure []
    x:xs -> if x `elem` parsedFilePaths
            then parseADLs opts parsedFilePaths xs
            else whenCheckedIO (parseSingleADL opts x) parseTheRest
        where parseTheRest :: (P_Context, [ParseCandidate]) -> IO (Guarded [P_Context])
              parseTheRest (ctx, includes) = whenCheckedIO (parseADLs opts (x:parsedFilePaths) (includes++xs)) $
                                                  return . pure . (:) ctx 

data ParseCandidate = ParseCandidate 
       { pcBasePath :: Maybe FilePath -- The absolute path to prepend in case of relative filePaths 
       , pcOrigin   :: Maybe Origin
       , pcFilePath :: FilePath -- The absolute or relative filename as found in the INCLUDE statement
       , pcFileKind :: Maybe FileKind -- In case the file is included into ampersand.exe, its FileKind.
       , pcCanonical :: FilePath -- The canonicalized path of the candicate
       , pcDefineds :: [PreProcDefine]
       }
instance Eq ParseCandidate where
 a == b = pcFileKind a == pcFileKind b && pcCanonical a `equalFilePath` pcCanonical b


-- | Parse an Ampersand file, but not its includes (which are simply returned as a list)
parseSingleADL ::
    Options
 -> ParseCandidate -> IO (Guarded (P_Context, [ParseCandidate]))
parseSingleADL opts pc
 = do verboseLn opts $ "Reading file " ++ filePath 
                         ++ (case pcFileKind pc of
                              Just _ -> " (from within ampersand.exe)"
                              Nothing -> mempty)
      exists <- doesFileExist filePath
      if isJust (pcFileKind pc) || exists
      then parseSingleADL'
      else return $ mkErrorReadingINCLUDE (pcOrigin pc) filePath "File does not exist."
    where
     filePath = pcCanonical pc
     parseSingleADL' :: IO(Guarded (P_Context, [ParseCandidate]))
     parseSingleADL'
         | extension == ".xlsx" =
             do { popFromExcel <- catchInvalidXlsx $ parseXlsxFile opts (pcFileKind pc) filePath
                ; return ((\pops -> (mkContextOfPopsOnly pops,[])) <$> popFromExcel)  -- Excel file cannot contain include files
                }
         | otherwise =
             do { mFileContents
                    <- case pcFileKind pc of
                       Just fileKind
                         -> case getStaticFileContent fileKind filePath of
                              Just cont -> return (Right $ stripBom cont)
                              Nothing -> fatal ("Statically included "++ show fileKind++ " files. \n  Cannot find `"++filePath++"`.")
                       Nothing
                         -> readUTF8File filePath
                ; case mFileContents of
                    Left err -> return $ mkErrorReadingINCLUDE (pcOrigin pc) filePath err
                    Right fileContents ->
                         whenCheckedIO
                           (return $ parseCtx filePath =<< (preProcess filePath (pcDefineds pc) fileContents))
                           $ \(ctxts, includes) ->
                                 do parseCandidates <- mapM include2ParseCandidate includes
                                    return . pure $ (ctxts, parseCandidates)
                }
         where 
               include2ParseCandidate :: Include -> IO ParseCandidate
               include2ParseCandidate (Include org str defs) = do
                  let canonical = myNormalise ( takeDirectory filePath </> str )
                  return ParseCandidate { pcBasePath  = Just filePath
                                        , pcOrigin    = Just org
                                        , pcFilePath  = str
                                        , pcFileKind  = pcFileKind pc
                                        , pcCanonical = canonical
                                        , pcDefineds  = pcDefineds pc ++ defs
                                        }
               myNormalise :: FilePath -> FilePath 
               -- see http://neilmitchell.blogspot.nl/2015/10/filepaths-are-subtle-symlinks-are-hard.html why System.Filepath doesn't support reduction of x/foo/../bar into x/bar. 
               -- However, for most Ampersand use cases, we will not deal with symlinks. 
               -- As long as that assumption holds, we can make the following reductions
               myNormalise fp = joinDrive drive . joinPath $ f [] dirs ++ [file]
                 where
                   (drive,path) = splitDrive (normalise fp)
                   (dirs,file)  = case splitPath path of
                                   [] -> fatal ("Illegal filePath: "++show fp)
                                   xs -> (init xs,last xs)
                   
                   f :: [FilePath] -> [FilePath] -> [FilePath]
                   f ds [] = ds
                   f ds (x:xs) | is "."  x = f ds xs   -- reduce /a/b/./c to /a/b/c/ 
                               | is ".." x = case ds of
                                              [] -> fatal ("Illegal filePath: "++show fp)
                                              _  -> f (init ds) xs --reduce a/b/c/../d/ to a/b/d/
                               | otherwise = f (ds++[x]) xs
               is :: String -> FilePath -> Bool
               is str fp = case stripPrefix str fp of
                             Just [chr] -> chr `elem` pathSeparators  
                             _          -> False
               stripBom :: String -> String
               stripBom ('\239':'\187':'\191': s) = s
               stripBom s = s
               extension = map toLower $ takeExtension filePath
               catchInvalidXlsx :: IO a -> IO a
               catchInvalidXlsx m = catch m f
                 where f :: SomeException -> IO a
                       f exception = fatal ("The file does not seem to have a valid .xlsx structure:\n  "++show exception)

parseErrors :: Lang -> ParseError -> NEL.NonEmpty CtxError
parseErrors lang err = pure $ PE (Message msg)
                where msg :: String
                      msg = "In file " ++ show (errorPos err) ++ ":" ++ showLang lang (errorMessages err)
                      showLang :: Lang -> [Message] -> String
                      showLang English = showErrorMessages "or" "unknown parse error"   "at that point expecting" "Parsing stumbled upon" "end of input"
                      showLang Dutch   = showErrorMessages "of" "onbekende parsingfout" "verwacht"  "onverwacht" "einde van de invoer"

parse :: AmpParser a -> FilePath -> [Token] -> Guarded a
parse p fn ts =
      -- runP :: Parsec s u a -> u -> FilePath -> s -> Either ParseError a
    case runP p pos' fn ts of
        --TODO: Add language support to the parser errors
        Left err -> Errors $ parseErrors English err
        Right a -> pure a
    where pos' | null ts   = initPos fn
               | otherwise = tokPos (head ts)

--TODO: Give the errors in a better way
lexerError2CtxError :: LexerError -> CtxError
lexerError2CtxError (LexerError pos' err) =
   PE (Message ("Lexer error at "++show pos'++"\n  "
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
    Left err -> Errors . pure $ lexerError2CtxError err
    --TODO: Do something with the warnings. The warnings cannot be shown with the current Guarded data type
    Right (tokens, lexerWarnings)  -> whenChecked (parse parser filename tokens) pure

-- | Parses an isolated rule
-- In order to read derivation rules, we use the Ampersand parser.
-- Since it is applied on static code only, error messagea may be produced as fatals.
parseRule :: String         -- ^ The string to be parsed
          -> Term TermPrim  -- ^ The resulting rule
parseRule str
   = case  runParser pRule "inside Haskell code" str of
       Checked result _ -> result
       Errors  msg      -> fatal ("Parse errors in "++str++":\n   "++show msg)

-- | Parses an Ampersand context
parseCtx :: FilePath -- ^ The file name (used for error messages)
         -> String   -- ^ The string to be parsed
         -> Guarded (P_Context, [Include]) -- ^ The context and a list of included files
parseCtx = runParser pContext

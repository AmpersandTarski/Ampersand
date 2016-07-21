{-# LANGUAGE ScopedTypeVariables #-}
-- This module provides an interface to be able to parse a script and to
-- return an FSpec, as tuned by the command line options.
-- This might include that RAP is included in the returned FSpec.
module Database.Design.Ampersand.Input.Parsing (
    parseADL,parseMeta , parseADL1pExpr, parseRule, parseCtx, runParser
) where

import Control.Applicative
import Data.List
import Data.Char(toLower)
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
import Database.Design.Ampersand.Prototype.StaticFiles_Generated(getStaticFileContent,FileKind(FormalAmpersand))

-- | Parse an Ampersand file and all transitive includes
parseADL :: Options                    -- ^ The options given through the command line
         -> FilePath   -- ^ The path of the file to be parsed
         -> IO (Guarded P_Context)     -- ^ The resulting context
parseADL opts fp = parseThing opts (fp,Nothing) False

parseMeta :: Options -> IO (Guarded P_Context)
parseMeta opts = parseThing opts ("AST.adl",Just $ Origin "Formal Ampersand specification") True -- This is the top file from FormalAmpersand. 

parseThing :: Options -> SingleFileToParse -> Bool -> IO (Guarded P_Context) 
parseThing opts mainFile useAllStaticFiles =
  whenCheckedIO (parseSingleADL opts useAllStaticFiles mainFile) $ \(ctxt, includes) ->
    whenCheckedIO (parseADLs opts useAllStaticFiles [fst mainFile] includes) $ \ctxts ->
      return $ Checked $ foldl mergeContexts ctxt ctxts

-- | Parses several ADL files
parseADLs :: Options                    -- ^ The options given through the command line
          -> Bool                       -- ^ True iff the file is from FormalAmpersand files in `allStaticFiles`
          -> [FilePath]                 -- ^ The list of files that have already been parsed
          -> [SingleFileToParse]        -- ^ A list of files that still are to be parsed. 
          -> IO (Guarded [P_Context])   -- ^ The resulting contexts
parseADLs opts useAllStaticFiles parsedFilePaths fpIncludes =
  case fpIncludes of
    [] -> return $ Checked []
    _  -> do { let filePathsToParse = determineWhatElseToParse parsedFilePaths fpIncludes
             ; whenCheckedIO (sequenceA <$> mapM (parseSingleADL opts useAllStaticFiles) filePathsToParse) $ noot
             }
           where 
              noot :: [(P_Context, [SingleFileToParse])] -> IO (Guarded [P_Context])
              noot results =
                do { let (ctxts, includesPerFile) = unzip results
                         processed = nub (parsedFilePaths ++ map fst fpIncludes)
                   ; whenCheckedIO (parseADLs opts useAllStaticFiles processed $ concat includesPerFile) $ \ctxts' ->
                       return $ Checked $ ctxts ++ ctxts'
                   }
          
              determineWhatElseToParse :: [FilePath] -> [SingleFileToParse] -> [SingleFileToParse]
              determineWhatElseToParse allreadyParsedFiles = 
                  filter (not . isParsedAlready) . uniques 
                where
                  isParsedAlready :: SingleFileToParse -> Bool
                  isParsedAlready (x,_)= x `elem` allreadyParsedFiles
                  uniques :: [SingleFileToParse] -> [SingleFileToParse]
                  uniques = map head . groupBy eql
                  eql :: Eq a => (a,b) -> (a,c) -> Bool 
                  eql a b = fst a == fst b

type SingleFileToParse = (FilePath, Maybe Origin) -- The origin of why this file still has to be parsed.
-- | Parse an Ampersand file, but not its includes (which are simply returned as a list)
parseSingleADL ::
    Options
 -> Bool   -- True iff the file is from FormalAmpersand files in `allStaticFiles`
 -> SingleFileToParse -> IO (Guarded (P_Context, [SingleFileToParse]))
parseSingleADL opts useAllStaticFiles singleFile
 = do verboseLn opts $ "Reading file " ++ filePath ++ if useAllStaticFiles then " (from within ampersand.exe)" else ""
      exists <- doesFileExist filePath
      if useAllStaticFiles || exists
      then parseSingleADL'
      else return $ mkErrorReadingINCLUDE (snd singleFile) filePath "File does not exist."
    where
     filePath = fst singleFile
     parseSingleADL' :: IO(Guarded (P_Context, [SingleFileToParse]))
     parseSingleADL'
         | extension == ".xlsx" =
             do { popFromExcel <- catchInvalidXlsx $ parseXlsxFile opts useAllStaticFiles filePath
                ; return ((\pops -> (mkContextOfPopsOnly pops,[])) <$> popFromExcel)  -- Excel file cannot contain include files
                }
         | otherwise =
             do { mFileContents
                    <- if useAllStaticFiles
                       then case getStaticFileContent FormalAmpersand filePath of
                             Just cont -> do return (Right $ stripBom cont)
                             Nothing -> fatal 0 ("Statically included "++ show FormalAmpersand++ " files. \n  Cannot find `"++filePath++"`.")
                       else readUTF8File filePath
                ; case mFileContents of
                    Left err -> return $ mkErrorReadingINCLUDE (snd singleFile) filePath err
                    Right fileContents ->
                         whenCheckedIO (return $ parseCtx filePath fileContents) $ \(ctxts, relativePaths) ->
                               do return (Checked (ctxts, relativePaths))
                }
         where -- showDcl dcl = name dcl ++show(dec_sign dcl)
               stripBom :: String -> String
               stripBom ('\239':'\187':'\191': s) = s
               stripBom s = s
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
         -> Guarded (P_Context, [SingleFileToParse]) -- ^ The context and a list of included files
parseCtx base content = 
   case runParser pContext base content of
     Errors err -> Errors err
     Checked x -> Checked (f x)
     where f (pctx,includes) = (pctx, map include2SingleFileToParse includes)
           include2SingleFileToParse (Include orig str) = (normalise (takeDirectory base </> str) , Just orig)

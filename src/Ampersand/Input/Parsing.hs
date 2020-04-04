{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- This module provides an interface to be able to parse a script and to
-- return an FSpec, as tuned by the command line options.
-- This might include that RAP is included in the returned FSpec.
module Ampersand.Input.Parsing (
      parseFileTransitive
    , parseFormalAmpersand
    , parsePrototypeContext
    , parseRule
    , runParser
    , ParseCandidate(..) -- exported for use with --daemon
) where

import           Ampersand.ADL1
import           Ampersand.Basics
import           Ampersand.Core.ShowPStruct
import           Ampersand.Input.ADL1.CtxError
import           Ampersand.Input.ADL1.Lexer
import           Ampersand.Input.ADL1.Parser
import           Ampersand.Input.Archi.ArchiAnalyze
import           Ampersand.Input.PreProcessor
import           Ampersand.Input.Xslx.XLSX
import           Ampersand.Misc.HasClasses
import           Ampersand.Prototype.StaticFiles_Generated
import           RIO.Char(toLower)
import qualified RIO.List as L
import qualified RIO.Set as Set
import qualified RIO.Text as T
import           System.Directory
import           System.FilePath
import           Text.Parsec.Prim (runP)



-- | Parse an Ampersand file and all transitive includes
parseFileTransitive :: (HasFSpecGenOpts env, HasLogFunc env) =>
            FilePath   -- ^ The path of the file to be parsed, either absolute or relative to the current user's path
         -> RIO env ([ParseCandidate], Guarded P_Context) -- ^ A tuple containing a list of parsed files and the The resulting context
parseFileTransitive fp = do 
    curDir <- liftIO getCurrentDirectory
    canonical <- liftIO $ canonicalizePath fp
    parseThing' ParseCandidate
       { pcBasePath  = Just curDir
       , pcOrigin    = Nothing
       , pcFileKind  = Nothing
       , pcCanonical = canonical
       , pcDefineds  = Set.empty
       }
 
parseFormalAmpersand :: (HasFSpecGenOpts env, HasLogFunc env) => RIO env (Guarded P_Context)
parseFormalAmpersand = parseThing ParseCandidate
       { pcBasePath  = Nothing
       , pcOrigin    = Just $ Origin "Formal Ampersand specification"
       , pcFileKind  = Just FormalAmpersand
       , pcCanonical = "AST.adl"
       , pcDefineds  = Set.empty
       }
parsePrototypeContext :: (HasFSpecGenOpts env, HasLogFunc env) => RIO env (Guarded P_Context)
parsePrototypeContext = parseThing ParseCandidate
       { pcBasePath  = Nothing
       , pcOrigin    = Just $ Origin "Ampersand specific system context"
       , pcFileKind  = Just PrototypeContext
       , pcCanonical = "PrototypeContext.adl"
       , pcDefineds  = Set.empty
       }

parseThing :: (HasFSpecGenOpts env, HasLogFunc env) =>
              ParseCandidate -> RIO env (Guarded P_Context)
parseThing pc = snd <$> parseThing' pc 

parseThing' :: (HasFSpecGenOpts env, HasLogFunc env) =>
               ParseCandidate -> RIO env ([ParseCandidate], Guarded P_Context) 
parseThing' pc = do
  results <- parseADLs [] [pc]
  case results of 
     Errors err    -> return ([pc], Errors err)
     Checked xs ws -> return ( candidates
                             , Checked mergedContexts ws
                             )
              where (candidates,contexts) = L.unzip xs
                    mergedContexts = case contexts of
                          [] -> fatal "Impossible"
                          h:tl -> foldr mergeContexts h tl

-- | Parses several ADL files
parseADLs :: (HasFSpecGenOpts env, HasLogFunc env) =>
             [ParseCandidate]         -- ^ The list of files that have already been parsed
          -> [ParseCandidate]         -- ^ A list of files that still are to be parsed.
          -> RIO env (Guarded [(ParseCandidate, P_Context)]) -- ^ The resulting contexts and the ParseCandidate that is the source for that P_Context
parseADLs parsedFilePaths fpIncludes =
  case fpIncludes of
    [] -> return $ pure []
    x:xs -> if x `elem` parsedFilePaths
            then parseADLs parsedFilePaths xs
            else whenCheckedM (parseSingleADL x) parseTheRest
        where parseTheRest :: (HasFSpecGenOpts env, HasLogFunc env) =>
                              (P_Context, [ParseCandidate]) 
                           -> RIO env (Guarded [(ParseCandidate, P_Context)])
              parseTheRest (ctx, includes) = 
                  whenCheckedM (parseADLs (parsedFilePaths<>[x]) (includes<>xs)) 
                                          (\rst -> pure . pure $ (x,ctx):rst)        --return . pure . (:) (x,ctx) 

data ParseCandidate = ParseCandidate 
       { pcBasePath :: Maybe FilePath -- The absolute path to prepend in case of relative filePaths 
       , pcOrigin   :: Maybe Origin
       , pcFileKind :: Maybe FileKind -- In case the file is included into ampersand.exe, its FileKind.
       , pcCanonical :: FilePath -- The canonicalized path of the candicate
       , pcDefineds :: Set.Set PreProcDefine
       }
instance Eq ParseCandidate where
 a == b = pcFileKind a == pcFileKind b && pcCanonical a `equalFilePath` pcCanonical b


-- | Parse an Ampersand file, but not its includes (which are simply returned as a list)
parseSingleADL :: (HasFSpecGenOpts env, HasLogFunc env) =>
    ParseCandidate -> RIO env (Guarded (P_Context, [ParseCandidate]))
parseSingleADL pc
 = do case pcFileKind pc of
        Just _ -> {- reading a file that is included into ampersand.exe -} 
                   logDebug $ "Reading internal file " <> display (T.pack filePath) 
        Nothing -> logInfo $ "Reading file " <> display (T.pack filePath) 
      exists <- liftIO $ doesFileExist filePath
      if isJust (pcFileKind pc) || exists
      then parseSingleADL'
      else return $ mkErrorReadingINCLUDE (pcOrigin pc) [ "While looking for "<>T.pack filePath
                                                        , "   File does not exist." ]
    where
     filePath = pcCanonical pc
     parseSingleADL' :: (HasFSpecGenOpts env, HasLogFunc env) => RIO env (Guarded (P_Context, [ParseCandidate]))
     parseSingleADL'
         | -- This feature enables the parsing of Excell files, that are prepared for Ampersand.
           extension == ".xlsx" = do 
              popFromExcel <- catchInvalidXlsx $ parseXlsxFile (pcFileKind pc) filePath
              return ((\pops -> (mkContextOfPopsOnly pops,[])) <$> popFromExcel)  -- Excel file cannot contain include files
         | -- This feature enables the parsing of Archimate models in ArchiMate® Model Exchange File Format
           extension == ".xml" = do 
              ctxFromArchi <- archi2PContext filePath  -- e.g. "CA repository.xml"
              logInfo (display (T.pack filePath) <> " has been interpreted as an Archi-repository.")
              case ctxFromArchi of
                    Checked ctx _ -> do
                         writeFileUtf8 "ArchiMetaModel.adl" (showP ctx)
                         logInfo ("ArchiMetaModel.adl written")
                    Errors _ -> pure ()
              return ((,) <$> ctxFromArchi
                          <*> pure [] -- ArchiMate file cannot contain include files
                         )
         | otherwise = do
              mFileContents
                    <- case pcFileKind pc of
                       Just fileKind
                         -> case getStaticFileContent fileKind filePath of
                              Just cont -> return (Right . stripBom . decodeUtf8 $ cont)
                              Nothing -> fatal ("Statically included "<> tshow fileKind<> " files. \n  Cannot find `"<>T.pack filePath<>"`.")
                       Nothing
                         -> Right <$> readFileUtf8 filePath
              case mFileContents of
                    Left err -> return $ mkErrorReadingINCLUDE (pcOrigin pc) (map T.pack err)
                    Right fileContents ->
                         let -- TODO: This should be cleaned up. Probably better to do all the file reading
                             --       first, then parsing and typechecking of each module, building a tree P_Contexts
                             meat :: Guarded (P_Context, [Include])
                             meat = preProcess filePath (pcDefineds pc) (T.unpack fileContents) >>= parseCtx filePath . T.pack 
                             proces :: Guarded (P_Context,[Include]) -> RIO env (Guarded (P_Context, [ParseCandidate]))
                             proces (Errors err) = pure (Errors err)
                             proces (Checked (ctxts, includes) ws) = 
                                addWarnings ws <$> foo <$> mapM include2ParseCandidate includes
                              where
                                foo :: [Guarded ParseCandidate] -> (Guarded (P_Context, [ParseCandidate]))
                                foo xs = (\x -> (ctxts,x)) <$> sequence xs
                         in
                         proces meat
         where 
               include2ParseCandidate :: Include -> RIO env (Guarded ParseCandidate)
               include2ParseCandidate (Include org str defs) = do
                  let canonical = myNormalise ( takeDirectory filePath </> str )
                      defineds  = processFlags (pcDefineds pc) (map T.unpack defs)
                  return $ Checked ParseCandidate { pcBasePath  = Just filePath
                                        , pcOrigin    = Just org
                                        , pcFileKind  = pcFileKind pc
                                        , pcCanonical = canonical
                                        , pcDefineds  = defineds
                                        } []
               myNormalise :: FilePath -> FilePath 
               -- see http://neilmitchell.blogspot.nl/2015/10/filepaths-are-subtle-symlinks-are-hard.html why System.Filepath doesn't support reduction of x/foo/../bar into x/bar. 
               -- However, for most Ampersand use cases, we will not deal with symlinks. 
               -- As long as that assumption holds, we can make the following reductions
               myNormalise fp = joinDrive drive . joinPath $ f [] dirs <> [file]
                 where
                   (drive,path) = splitDrive (normalise fp)
                   (dirs,file)  = case reverse $ splitPath path of
                                   [] -> fatal ("Illegal filePath: "<>tshow fp)
                                   last:reverseInit -> (reverse reverseInit,last)
                   
                   f :: [FilePath] -> [FilePath] -> [FilePath]
                   f ds [] = ds
                   f ds (x:xs) | is "."  x = f ds xs   -- reduce /a/b/./c to /a/b/c/ 
                               | is ".." x = case reverse ds of
                                              [] -> fatal ("Illegal filePath: "<>tshow fp)
                                              _:reverseInit -> f (reverse reverseInit) xs --reduce a/b/c/../d/ to a/b/d/
                               | otherwise = f (ds<>[x]) xs
               is :: FilePath -> FilePath -> Bool
               is str fp = case L.stripPrefix str fp of
                             Just [chr] -> chr `elem` pathSeparators  
                             _          -> False
               stripBom :: Text -> Text
               stripBom s = T.dropPrefix (T.pack ['\239','\187','\191']) s
               extension = map toLower $ takeExtension filePath
               catchInvalidXlsx :: RIO env a -> RIO env a
               catchInvalidXlsx m = catch m f
                 where f :: SomeException -> RIO env a
                       f exception = fatal ("The file does not seem to have a valid .xlsx structure:\n  "<>tshow exception)

-- | To enable roundtrip testing, all data can be exported.
-- For this purpose mkContextOfPopsOnly exports the population only
mkContextOfPopsOnly :: [P_Population] -> P_Context
mkContextOfPopsOnly pops =
  PCtx{ ctx_nm     = ""
      , ctx_pos    = []
      , ctx_lang   = Nothing
      , ctx_markup = Nothing
      , ctx_pats   = []
      , ctx_rs     = []
      , ctx_ds     = []
      , ctx_cs     = []
      , ctx_ks     = []
      , ctx_rrules = []
      , ctx_reprs  = []
      , ctx_vs     = []
      , ctx_gs     = []
      , ctx_ifcs   = []
      , ctx_ps     = []
      , ctx_pops   = pops
      , ctx_metas  = []
      }

parse :: AmpParser a -> FilePath -> [Token] -> Guarded a
parse p fn ts =
      -- runP :: Parsec s u a -> u -> FilePath -> s -> Either ParseError a
    case runP p pos' fn ts of
        --TODO: Add language support to the parser errors
        Left err -> Errors $ pure $ PE err
        Right a -> pure a
    where pos' = case ts of
                   [] -> initPos fn
                   h:_ -> tokPos h


-- | Runs the given parser
runParser :: AmpParser a -- ^ The parser to run
          -> FilePath    -- ^ Name of the file (for error messages)
          -> Text      -- ^ Text to parse
          -> Guarded a   -- ^ The result
runParser parser filename input =
  let lexed = lexer filename (T.unpack input)
  in case lexed of
    Left err -> Errors . pure $ lexerError2CtxError err
    Right (tokens, lexerWarnings) 
             -> addWarnings (map lexerWarning2Warning lexerWarnings)
                            (parse parser filename tokens)

-- | Parses an isolated rule
-- In order to read derivation rules, we use the Ampersand parser.
-- Since it is applied on static code only, error messagea may be produced as fatals.
parseRule :: Text         -- ^ The string to be parsed
          -> Term TermPrim  -- ^ The resulting rule
parseRule str
   = case  runParser pRule "inside Haskell code" str of
       Checked result _ -> result
       Errors  msg      -> fatal ("Parse errors in "<>str<>":\n   "<>tshow msg)

-- | Parses an Ampersand context
parseCtx :: FilePath -- ^ The file name (used for error messages)
         -> Text   -- ^ The string to be parsed
         -> Guarded (P_Context, [Include]) -- ^ The context and a list of included files
parseCtx = runParser pContext

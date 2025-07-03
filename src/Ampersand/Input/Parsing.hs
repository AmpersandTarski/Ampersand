{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- This module provides an interface to be able to parse a script and to
-- return an FSpec, as tuned by the command line options.
-- This might include that RAP is included in the returned FSpec.
module Ampersand.Input.Parsing
  ( parseFilesTransitive,
    parseFormalAmpersand,
    parsePrototypeContext,
    parseRule,
    parseTerm,
    parseCtx,
    ParseCandidate (..), -- exported for use with --daemon
  )
where

import Ampersand.ADL1
  ( Origin (Origin),
    P_Context,
    Term,
    TermPrim,
    mergeContexts,
  )
import Ampersand.Basics
import Ampersand.Core.ShowPStruct (showP)
import Ampersand.Input.ADL1.CtxError
  ( Guarded (..),
    addWarnings,
    mkErrorReadingINCLUDE,
    mkParserStateWarning,
    whenCheckedM,
  )
import Ampersand.Input.ADL1.Parser
  ( Include (..),
    pContext,
    pRule,
    pTerm,
  )
import Ampersand.Input.ADL1.ParsingLib
import Ampersand.Input.Archi.ArchiAnalyze (archi2PContext)
import Ampersand.Input.AtlasImport
import Ampersand.Input.PreProcessor
  ( PreProcDefine,
    preProcess,
    processFlags,
  )
import Ampersand.Input.SemWeb.Turtle
import Ampersand.Input.Xslx.XLSX (parseXlsxFile)
import Ampersand.Misc.HasClasses
  
import Ampersand.Prototype.StaticFiles_Generated
  ( FileKind (FormalAmpersand, PrototypeContext),
    getStaticFileContent,
  )
import Data.RDF
import RIO.Char (toLower)
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import System.Directory
  ( canonicalizePath,
    doesFileExist,
    getCurrentDirectory,
  )
import System.FilePath
  ( equalFilePath,
    joinDrive,
    joinPath,
    normalise,
    pathSeparators,
    splitDrive,
    splitPath,
    takeDirectory,
    takeExtension,
    (</>),
  )
import Text.Parsec (getState)

-- | Parse Ampersand files and all transitive includes
parseFilesTransitive ::
  (HasDirOutput env, HasFSpecGenOpts env,HasTrimXLSXOpts env, HasLogFunc env) =>
  Roots ->
  -- | A tuple containing a list of parsed files and the The resulting context
  RIO env (NonEmpty ParseCandidate, Guarded P_Context)
parseFilesTransitive xs = do
  -- parseFileTransitive . NE.head . getRoots --TODO Fix this, to also take the tail files into account.
  curDir <- liftIO getCurrentDirectory
  canonical <- liftIO . mapM canonicalizePath . getRoots $ xs
  let candidates = mkCandidate curDir <$> canonical
  do
    result <- parseThings candidates
    return (candidates, result)
  where
    mkCandidate :: FilePath -> FilePath -> ParseCandidate
    mkCandidate curdir canonical =
      ParseCandidate
        { pcBasePath = Just curdir,
          pcOrigin = Nothing,
          pcFileKind = Nothing,
          pcCanonical = canonical,
          pcDefineds = Set.empty
        }

parseFormalAmpersand :: (HasDirOutput env, HasFSpecGenOpts env,HasTrimXLSXOpts env, HasLogFunc env) => RIO env (Guarded P_Context)
parseFormalAmpersand = do
  parseThings
    $ ParseCandidate
      { pcBasePath = Nothing,
        pcOrigin = Just $ Origin "Formal Ampersand specification",
        pcFileKind = Just FormalAmpersand,
        pcCanonical = "FormalAmpersand.adl",
        pcDefineds = Set.empty
      }
    NE.:| []

parsePrototypeContext :: (HasDirOutput env, HasFSpecGenOpts env,HasTrimXLSXOpts env, HasLogFunc env) => RIO env (Guarded P_Context)
parsePrototypeContext = do
  parseThings
    $ ParseCandidate
      { pcBasePath = Nothing,
        pcOrigin = Just $ Origin "Ampersand specific system context",
        pcFileKind = Just PrototypeContext,
        pcCanonical = "PrototypeContext.adl",
        pcDefineds = Set.empty
      }
    NE.:| []

parseThings ::
  (HasDirOutput env, HasFSpecGenOpts env, HasTrimXLSXOpts env, HasLogFunc env) =>
  NonEmpty ParseCandidate ->
  RIO env (Guarded P_Context)
parseThings pcs = do
  results <- parseADLs [] (NE.toList pcs)
  finalize results
  where
    -- \| After collecting the results of all parsed files, we need to
    --   combine all graphs (if any) into a single P_Context. Then, we
    --   need to merge the contexts, and finally, we can
    --   return the resulting P_Context.
    finalize :: (HasFSpecGenOpts  env, HasDirOutput env, HasLogFunc env) => Guarded [(a, SingleFileResult)] -> RIO env (Guarded P_Context)
    finalize (Errors err) = pure (Errors err)
    finalize (Checked results warns) = do
      let (contexts, graphs) = partitionEithers (map snd results)

      triplesCtx <- case graphs of
        [] -> pure Nothing
        h : tl -> do
          let combined = mergeGraphs (h NE.:| tl)
          writeRdfTList 0 combined
          pure (Just $ graph2P_Context combined)
      pure $ case triplesCtx of
        Nothing -> Checked (bar contexts) warns
        Just (Checked pCtx ws2) -> Checked (bar (contexts <> [pCtx])) (warns <> ws2)
        Just (Errors err) -> Errors err
      where
        bar :: [P_Context] -> P_Context
        bar xs = case xs of
          [] -> fatal "Impossible"
          h : tl -> foldl' mergeContexts h tl

-- writeSingleRDF ::
--   (HasLogFunc env) =>
--   Guarded [(ParseCandidate, SingleFileResult)] ->
--   RIO env ()
-- writeSingleRDF results = do
--   let graphs = rights . fmap snd <$> results
--   case graphs of
--     Checked xs@(_ : _) _ ->
--       mapM_ (uncurry writeRdfTList) $ zip [0 ..] xs
--     _ -> pure ()

-- | Parses several ADL files
parseADLs ::
  (HasTrimXLSXOpts env, HasLogFunc env) =>
  -- | The list of files that have already been parsed
  [ParseCandidate] ->
  -- | A list of files that still are to be parsed.
  [ParseCandidate] ->
  -- | The resulting contexts and the ParseCandidate that is the source for that P_Context
  RIO env (Guarded [(ParseCandidate, SingleFileResult)])
parseADLs parsedFilePaths fpIncludes =
  case fpIncludes of
    [] -> return $ pure []
    x : xs ->
      if x `elem` parsedFilePaths
        then parseADLs parsedFilePaths xs
        else whenCheckedM (parseSingleADL x) parseTheRest
      where
        parseTheRest ::
          (HasTrimXLSXOpts env, HasLogFunc env) =>
          (SingleFileResult, [ParseCandidate]) ->
          RIO env (Guarded [(ParseCandidate, SingleFileResult)])
        parseTheRest (ctx, includes) =
          whenCheckedM
            (parseADLs (parsedFilePaths <> [x]) (includes <> xs))
            (\rst -> pure . pure $ (x, ctx) : rst) -- return . pure . (:) (x,ctx)

-- | ParseCandidate is intended to represent an INCLUDE-statement.
--   This information is gathered while parsing and returned alongside the parse result.
data ParseCandidate = ParseCandidate
  { pcBasePath :: Maybe FilePath, -- The absolute path to prepend in case of relative filePaths
    pcOrigin :: Maybe Origin,
    pcFileKind :: Maybe FileKind, -- In case the file is included into ampersand.exe, its FileKind.
    pcCanonical :: FilePath, -- The canonicalized path of the candicate
    pcDefineds :: Set.Set PreProcDefine
  }

instance Eq ParseCandidate where
  a == b = pcFileKind a == pcFileKind b && pcCanonical a `equalFilePath` pcCanonical b

type SingleFileResult = Either P_Context (RDF TList)

-- | Parse an Ampersand file, but not its includes (which are simply returned as a list)
parseSingleADL ::
  (HasTrimXLSXOpts env, HasLogFunc env) =>
  ParseCandidate ->
  RIO env (Guarded (SingleFileResult, [ParseCandidate]))
parseSingleADL pc =
  do
    case pcFileKind pc of
      Just _ ->
        {- reading a file that is included into ampersand.exe -}
        logDebug $ "Reading internal file " <> display (T.pack filePath)
      Nothing -> logDebug $ "Reading file " <> display (T.pack filePath)
    exists <- liftIO $ doesFileExist filePath
    if isJust (pcFileKind pc) || exists
      then parseSingleADL'
      else
        return
          $ mkErrorReadingINCLUDE
            (pcOrigin pc)
            [ "While looking for " <> T.pack filePath,
              "   File does not exist."
            ]
  where
    filePath = pcCanonical pc
    parseSingleADL' :: (HasTrimXLSXOpts env, HasLogFunc env) => RIO env (Guarded (SingleFileResult, [ParseCandidate]))
    parseSingleADL'
      | -- This feature enables the parsing of Excel files, that are prepared for Ampersand.
        extension == ".xlsx" = do
          popFromExcel <- catchInvalidXlsx $ parseXlsxFile (pcFileKind pc) filePath
          return ((,[]) . fromContext <$> popFromExcel) -- An Excel file does not contain include files
      | -- This feature enables the parsing of Archimate models in ArchiMate® Model Exchange File Format
        extension == ".archimate" = do
          ctxFromArchi <- archi2PContext filePath -- e.g. "CA repository.xml"
          logInfo (display (T.pack filePath) <> " has been interpreted as an Archi-repository.")
          case ctxFromArchi of
            Checked ctx _ -> do
              writeFileUtf8 "ArchiMetaModel.adl" (showP ctx)
              logInfo "ArchiMetaModel.adl written"
            Errors _ -> pure ()
          return ((,[]) . fromContext <$> ctxFromArchi) -- An Archimate file does not contain include files
      | -- This feature enables the parsing of .json files, that can be generated with the Atlas.
        extension == ".json" = do
          ctxFromAtlas <- catchInvalidJSON $ parseJsonFile filePath
          return ((,[]) . fromContext <$> ctxFromAtlas) -- A .json file does not contain include files
      | -- This feature enables the parsing of .json files, that can be generated with the Atlas.
        extension == ".ttl" = do
          ctxFromTurtle <- catchInvalidTurtle $ readTurtle filePath
          return ((,[]) . fromGraph <$> ctxFromTurtle)
      | otherwise = do
          mFileContents <-
            case pcFileKind pc of
              Just fileKind ->
                case getStaticFileContent fileKind filePath of
                  Just cont -> return (Right . stripBom . decodeUtf8 $ cont)
                  Nothing -> fatal ("Statically included " <> tshow fileKind <> " files. \n  Cannot find `" <> T.pack filePath <> "`.")
              Nothing ->
                Right <$> readFileUtf8 filePath
          case mFileContents of
            Left err -> return $ mkErrorReadingINCLUDE (pcOrigin pc) (map T.pack err)
            Right fileContents ->
              let -- TODO: This should be cleaned up. Probably better to do all the file reading
                  --       first, then parsing and typechecking of each module, building a tree P_Contexts
                  meat :: Guarded (SingleFileResult, [Include])
                  meat = preProcess filePath (pcDefineds pc) (T.unpack fileContents) >>= guardedFromContext . parseCtx filePath . T.pack
                  proces :: Guarded (SingleFileResult, [Include]) -> RIO env (Guarded (SingleFileResult, [ParseCandidate]))
                  proces (Errors err) = pure (Errors err)
                  proces (Checked (ctxts, includes) ws) =
                    addWarnings ws . foo <$> mapM include2ParseCandidate includes
                    where
                      foo :: [Guarded ParseCandidate] -> Guarded (SingleFileResult, [ParseCandidate])
                      foo xs = (ctxts,) <$> sequence xs
               in proces meat
      where
        guardedFromContext :: Guarded (P_Context, [Include]) -> Guarded (SingleFileResult, [Include])
        guardedFromContext gIn = do
          (ctx, includes) <- gIn
          return (fromContext ctx, includes)
        fromContext :: P_Context -> SingleFileResult
        fromContext = Left
        fromGraph :: RDF TList -> SingleFileResult
        fromGraph = Right
        include2ParseCandidate :: Include -> RIO env (Guarded ParseCandidate)
        include2ParseCandidate (Include org str defs) = do
          let canonical = myNormalise (takeDirectory filePath </> str)
              defineds = processFlags (pcDefineds pc) (map T.unpack defs)
          return
            $ Checked
              ParseCandidate
                { pcBasePath = Just filePath,
                  pcOrigin = Just org,
                  pcFileKind = pcFileKind pc,
                  pcCanonical = canonical,
                  pcDefineds = defineds
                }
              []
        myNormalise :: FilePath -> FilePath
        -- see http://neilmitchell.blogspot.nl/2015/10/filepaths-are-subtle-symlinks-are-hard.html why System.Filepath doesn't support reduction of x/foo/../bar into x/bar.
        -- However, for most Ampersand use cases, we will not deal with symlinks.
        -- As long as that assumption holds, we can make the following reductions
        myNormalise fp = joinDrive drive . joinPath $ f [] dirs <> [file]
          where
            (drive, path) = splitDrive (normalise fp)
            (dirs, file) = case reverse $ splitPath path of
              [] -> fatal ("Illegal filePath: " <> tshow fp)
              last : reverseInit -> (reverse reverseInit, last)

            f :: [FilePath] -> [FilePath] -> [FilePath]
            f ds [] = ds
            f ds (x : xs)
              | is "." x = f ds xs -- reduce /a/b/./c to /a/b/c/
              | is ".." x = case reverse ds of
                  [] -> fatal ("Illegal filePath: " <> tshow fp)
                  _ : reverseInit -> f (reverse reverseInit) xs -- reduce a/b/c/../d/ to a/b/d/
              | otherwise = f (ds <> [x]) xs
        is :: FilePath -> FilePath -> Bool
        is str fp = case L.stripPrefix str fp of
          Just [chr] -> chr `elem` pathSeparators
          _ -> False
        stripBom :: Text -> Text
        stripBom = T.dropPrefix (T.pack ['\239', '\187', '\191'])
        extension = map toLower $ takeExtension filePath
        catchInvalidXlsx :: RIO env a -> RIO env a
        catchInvalidXlsx m = catch m f
          where
            f :: SomeException -> RIO env a
            f exception = fatal ("The file does not seem to have a valid .xlsx structure:\n  " <> tshow exception)
        catchInvalidJSON :: RIO env a -> RIO env a
        catchInvalidJSON m = catch m f
          where
            f :: SomeException -> RIO env a
            f exception = fatal ("The file does not seem to have a valid .json structure:\n  " <> tshow exception)
        catchInvalidTurtle :: RIO env (Guarded (RDF TList)) -> RIO env (Guarded (RDF TList))
        catchInvalidTurtle m = catch m f
          where
            f :: SomeException -> RIO env a
            f exception = fatal ("The file does not seem to have a valid Turtle structure:\n  " <> tshow exception)

-- | Parses an isolated rule
-- In order to read derivation rules, we use the Ampersand parser.
-- Since it is applied on static code only, error messagea may be produced as fatals.
parseRule ::
  -- | The string to be parsed
  Text ->
  -- | The resulting rule
  Term TermPrim
parseRule str =
  case runParser pRule "inside Haskell code" str of
    Checked result _ -> result
    Errors msg -> fatal ("Parse errors in " <> str <> ":\n   " <> tshow msg)

parseTerm :: FilePath -> Text -> Guarded (Term TermPrim)
parseTerm = runParser pTerm

-- | Parses an Ampersand context
parseCtx ::
  -- | The file name (used for error messages)
  FilePath ->
  -- | The string to be parsed
  Text ->
  -- | The context and a list of included files
  Guarded (P_Context, [Include])
parseCtx inp = do
  x <- runParser pContext' inp
  return $ case x of
    Errors err -> Errors err
    Checked (result, state) warns -> Checked result $ warns ++ map toWarning (parseMessages state)
  where
    pContext' = build <$> pContext <*> getState
    build :: a -> ParserState -> (a, ParserState)
    build res state = (res, state)
    toWarning (orig, msg) = mkParserStateWarning orig msg

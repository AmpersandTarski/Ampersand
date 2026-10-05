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
    ctx_ds,
    ctx_ifcs,
    ctx_nm,
    ctx_pops,
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
import Ampersand.Input.IFC.IFCAnalyze
  ( defaultSchemaName,
    fileSchemaName,
    ifc2PContextFromTexts,
  )
import Ampersand.Input.PreProcessor
  ( PreProcDefine,
    preProcess,
    processFlags,
  )
import Ampersand.Input.Qualify
  ( checkForeignConcepts,
    checkOwnership,
    conceptNamesOf,
    qualifyContext,
  )
import Ampersand.Input.SemWeb.Turtle
import Ampersand.Input.Xslx.XLSX (XlsxIfcSheet, parseXlsxFile, xlsxIfcSheet2pops)
import Ampersand.Misc.HasClasses
import Ampersand.Prototype.StaticFiles_Generated
  ( FileKind (FormalAmpersand, IFCSchemas, PrototypeContext),
    getStaticFileContent,
  )
import Ampersand.Runners (logLevel)
import Ampersand.Types.Config (HasRunner (..))
import Data.RDF
import RIO.Char (toLower)
import RIO.Directory
  ( canonicalizePath,
    doesFileExist,
    getCurrentDirectory,
  )
import RIO.FilePath
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
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import Text.Parsec (getState)

-- | Parse Ampersand files and all transitive includes
parseFilesTransitive ::
  (HasDirOutput env, HasFSpecGenOpts env, HasTrimXLSXOpts env, HasRunner env) =>
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
          pcDefineds = Set.empty,
          pcAlias = Nothing
        }

parseFormalAmpersand :: (HasDirOutput env, HasFSpecGenOpts env, HasTrimXLSXOpts env, HasRunner env) => RIO env (Guarded P_Context)
parseFormalAmpersand = do
  parseThings
    $ ParseCandidate
      { pcBasePath = Nothing,
        pcOrigin = Just $ Origin "Formal Ampersand specification",
        pcFileKind = Just FormalAmpersand,
        pcCanonical = "FormalAmpersand.adl",
        pcDefineds = Set.empty,
        pcAlias = Nothing
      }
    NE.:| []

parsePrototypeContext :: (HasDirOutput env, HasFSpecGenOpts env, HasTrimXLSXOpts env, HasRunner env) => RIO env (Guarded P_Context)
parsePrototypeContext = do
  parseThings
    $ ParseCandidate
      { pcBasePath = Nothing,
        pcOrigin = Just $ Origin "Ampersand specific system context",
        pcFileKind = Just PrototypeContext,
        pcCanonical = "PrototypeContext.adl",
        pcDefineds = Set.empty,
        pcAlias = Nothing
      }
    NE.:| []

parseThings ::
  (HasDirOutput env, HasFSpecGenOpts env, HasTrimXLSXOpts env, HasRunner env) =>
  NonEmpty ParseCandidate ->
  RIO env (Guarded P_Context)
parseThings = parseContextTree []

-- | Parse the files of one context, and the contexts it includes under an alias.
--   A file that is included without an alias belongs to the same context (a union).
--   A file that is included with an alias is the root of a context of its own.
--   That context is parsed by a recursive call and its names get the alias as prefix,
--   so that its contribution to the result is disjoint from everything else.
parseContextTree ::
  (HasDirOutput env, HasFSpecGenOpts env, HasTrimXLSXOpts env, HasRunner env) =>
  -- | The root files of the contexts in which this context is being included.
  --   A context that would include one of these under an alias would contain itself.
  [ParseCandidate] ->
  NonEmpty ParseCandidate ->
  RIO env (Guarded P_Context)
parseContextTree ancestors pcs = do
  results <- parseADLs parseAliased [] (NE.toList pcs)
  finalize results
  where
    ancestors' = ancestors <> NE.toList pcs
    parseAliased ::
      (HasDirOutput env, HasFSpecGenOpts env, HasTrimXLSXOpts env, HasRunner env) =>
      ParseCandidate ->
      NamePart ->
      RIO env (Guarded P_Context)
    parseAliased pc alias
      | bare `elem` ancestors' =
          pure
            $ mkErrorReadingINCLUDE
              (pcOrigin pc)
              [ "The file " <> T.pack (pcCanonical pc) <> " cannot be included as " <> namePartToText alias <> ",",
                "  because this INCLUDE statement is part of that file or of a context that it includes.",
                "  A context that is included under an alias gets a copy of its own. So, it cannot contain itself."
              ]
      | otherwise = fmap (qualifyContext [alias]) <$> parseContextTree ancestors' (bare NE.:| [])
      where
        bare = pc {pcAlias = Nothing}
    -- \| After collecting the results of all parsed files, we need to
    --   combine all graphs (if any) into a single P_Context. Then, we
    --   need to merge the contexts, and finally, we can
    --   return the resulting P_Context.
    finalize :: (HasFSpecGenOpts env, HasDirOutput env, HasRunner env, HasTrimXLSXOpts env) => Guarded [(ParseCandidate, SingleFileResult)] -> RIO env (Guarded P_Context)
    finalize (Errors err) = pure (Errors err)
    finalize (Checked results warns) = do
      runner <- Ampersand.Basics.view runnerL
      doTrim <- Ampersand.Basics.view trimXLSXCellsL
      let (contexts, ifcSheets, graphs) = partitionResults [r | (pc, r) <- results, isNothing (pcAlias pc)]
          included = [(pc, alias, c) | (pc, FromADL c) <- results, Just alias <- [pcAlias pc]]
          aliasChecks = checkAliases contexts included
      triplesCtx <- case graphs of
        [] -> pure Nothing
        h : tl -> do
          let combined = mergeGraphs (h NE.:| tl)
          when (logLevel runner == LevelDebug) (writeRdfTList 0 combined)
          pure (Just $ graph2P_Context combined)
      -- Resolve the interface-format worksheets against the merged context, where the
      -- INTERFACE definitions and relations are finally available, and add the resulting
      -- populations. See 'xlsxIfcSheet2pops'.
      let withXlsxIfcPops ctxs ws =
            let merged = bar ctxs
             in case concat <$> traverse (xlsxIfcSheet2pops doTrim (ctx_ifcs merged) (ctx_ds merged)) ifcSheets of
                  Errors err -> Errors err
                  Checked pops ws3 -> Checked merged {ctx_pops = ctx_pops merged <> pops} (ws <> ws3)
      pure
        $ aliasChecks
        *> ( addIncluded [c | (_, _, c) <- included] <$> case triplesCtx of
               Nothing -> withXlsxIfcPops contexts warns
               Just (Checked pCtx ws2) -> withXlsxIfcPops (contexts <> [pCtx]) (warns <> ws2)
               Just (Errors err) -> Errors err
           )
      where
        -- The contexts that are included under an alias are added last,
        -- so that the name, the language and the markup of the including context prevail.
        addIncluded :: [P_Context] -> P_Context -> P_Context
        addIncluded cs merged = foldl' mergeContexts merged cs
        checkAliases :: [P_Context] -> [(ParseCandidate, NamePart, P_Context)] -> Guarded ()
        checkAliases own incl =
          traverse_ aliasIsUnambiguous incl
            *> traverse_ aliasDiffersFromContext incl
            *> traverse_ aliasIsNotReserved incl
            *> traverse_ (checkForeignConcepts [(alias, known alias) | (_, alias, _) <- incl]) own
            *> traverse_ (checkOwnership [alias | (_, alias, _) <- incl]) own
          where
            known alias = Set.unions [conceptNamesOf c | (_, a, c) <- incl, a == alias]
            aliasIsUnambiguous (pc, alias, _) =
              case [pc' | (pc', alias', _) <- incl, alias' == alias, pc' {pcAlias = Nothing} /= pc {pcAlias = Nothing}] of
                [] -> pure ()
                other : _ ->
                  mkErrorReadingINCLUDE
                    (pcOrigin pc)
                    [ "The alias " <> namePartToText alias <> " stands for two different files:",
                      "  " <> T.pack (pcCanonical pc),
                      "  " <> T.pack (pcCanonical other),
                      "  Give each of them an alias of its own."
                    ]
            aliasIsNotReserved (pc, alias, _) =
              when (isReservedNameSpace alias)
                $ mkErrorReadingINCLUDE
                  (pcOrigin pc)
                  [ "The alias " <> namePartToText alias <> " is a name space of the Ampersand system.",
                    "  Choose another alias."
                  ]
            aliasDiffersFromContext (pc, alias, _) =
              when (alias `elem` map (localName . ctx_nm) own)
                $ mkErrorReadingINCLUDE
                  (pcOrigin pc)
                  [ "The alias " <> namePartToText alias <> " is the name of the including context.",
                    "  Choose another alias, so that it is clear which context a name belongs to."
                  ]
        partitionResults :: [SingleFileResult] -> ([P_Context], [XlsxIfcSheet], [RDF TList])
        partitionResults = foldr step ([], [], [])
          where
            step (FromADL c) (cs, ss, gs) = (c : cs, ss, gs)
            step (FromXlsx c s) (cs, ss, gs) = (c : cs, s <> ss, gs)
            step (FromGraph g) (cs, ss, gs) = (cs, ss, g : gs)
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
  -- | How to parse a file that is included under an alias, as a context of its own.
  (ParseCandidate -> NamePart -> RIO env (Guarded P_Context)) ->
  -- | The list of files that have already been parsed
  [ParseCandidate] ->
  -- | A list of files that still are to be parsed.
  [ParseCandidate] ->
  -- | The resulting contexts and the ParseCandidate that is the source for that P_Context
  RIO env (Guarded [(ParseCandidate, SingleFileResult)])
parseADLs parseAliased parsedFilePaths fpIncludes =
  case fpIncludes of
    [] -> return $ pure []
    x : xs ->
      if x `elem` parsedFilePaths
        then parseADLs parseAliased parsedFilePaths xs
        else case pcAlias x of
          Nothing -> whenCheckedM (parseSingleADL x) parseTheRest
          Just alias -> whenCheckedM (parseAliased x alias) (\ctx -> parseTheRest (FromADL ctx, []))
      where
        parseTheRest (ctx, includes) =
          whenCheckedM
            (parseADLs parseAliased (parsedFilePaths <> [x]) (includes <> xs))
            (\rst -> pure . pure $ (x, ctx) : rst) -- return . pure . (:) (x,ctx)

-- | ParseCandidate is intended to represent an INCLUDE-statement.
--   This information is gathered while parsing and returned alongside the parse result.
data ParseCandidate = ParseCandidate
  { pcBasePath :: Maybe FilePath, -- The absolute path to prepend in case of relative filePaths
    pcOrigin :: Maybe Origin,
    pcFileKind :: Maybe FileKind, -- In case the file is included into ampersand.exe, its FileKind.
    pcCanonical :: FilePath, -- The canonicalized path of the candicate
    pcDefineds :: Set.Set PreProcDefine,
    pcAlias :: Maybe NamePart -- The alias, in case the file is included as a context of its own (INCLUDE "file" AS alias)
  }

instance Eq ParseCandidate where
  a == b = pcFileKind a == pcFileKind b && pcCanonical a `equalFilePath` pcCanonical b && pcAlias a == pcAlias b

-- | The result of parsing a single file. An .xlsx file additionally carries its raw
--   interface-format worksheets ('XlsxIfcSheet'), which can only be resolved after all
--   contexts are merged (because the INTERFACE definitions live in sibling .adl files).
data SingleFileResult
  = FromADL P_Context
  | FromXlsx P_Context [XlsxIfcSheet]
  | FromGraph (RDF TList)

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
          return ((\(ctx, sheets) -> (FromXlsx ctx sheets, [])) <$> popFromExcel) -- An Excel file does not contain include files
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
          mfileContents <- readFileUtf8Lenient filePath
          case mfileContents of
            Left err -> return $ mkErrorReadingINCLUDE (pcOrigin pc) err
            Right fileContents -> return $ (,[]) . fromGraph <$> parseTurtle fileContents
      | -- This feature enables reading IFC (STEP/Part-21) files. Note that the .ifc
        -- extension is overloaded: Ampersand also uses it for interface scripts. We
        -- discriminate on the Part-21 magic header ("ISO-10303-21"); only then do we
        -- treat the file as IFC. Anything else falls through to ordinary ADL parsing.
        extension == ".ifc" = do
          mfileContents <- readFileContents
          case mfileContents of
            Left err -> return $ mkErrorReadingINCLUDE (pcOrigin pc) err
            Right fileContents
              | isStepFile fileContents -> do
                  ctxFromIfc <- parseIfcStep filePath fileContents
                  logInfo (display (T.pack filePath) <> " has been interpreted as an IFC (STEP/Part-21) file.")
                  return ((,[]) . fromContext <$> ctxFromIfc) -- An IFC file does not contain include files
              | otherwise -> parseAsAdl fileContents
      | otherwise = do
          mFileContents <- readFileContents
          case mFileContents of
            Left err -> return $ mkErrorReadingINCLUDE (pcOrigin pc) err
            Right fileContents -> parseAsAdl fileContents
      where
        -- \| Read the file's contents, honouring 'pcFileKind': internal (statically
        -- bundled) files come from the embedded archive, external files from disk.
        -- Both the .ifc branch and the ordinary ADL branch use this, so an internal
        -- interface script such as PrototypeContext's @Interfaces.ifc@ is still found
        -- by the installed binary, which has no source tree on disk.
        readFileContents =
          case pcFileKind pc of
            Just fileKind ->
              case getStaticFileContent fileKind filePath of
                Just cont -> return (Right . stripBom . decodeUtf8 $ cont)
                Nothing -> fatal ("Statically included " <> tshow fileKind <> " files. \n  Cannot find `" <> T.pack filePath <> "`.")
            Nothing ->
              readFileUtf8Lenient filePath
        -- \| Parse already-read text as an ordinary ADL script (with INCLUDEs).
        parseAsAdl :: Text -> RIO env (Guarded (SingleFileResult, [ParseCandidate]))
        parseAsAdl fileContents =
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
        -- \| A Part-21 (STEP/IFC) file starts with the @ISO-10303-21@ magic token.
        isStepFile :: Text -> Bool
        isStepFile t = "ISO-10303-21" `T.isPrefixOf` T.stripStart (stripBomText t)
        stripBomText :: Text -> Text
        stripBomText t = fromMaybe t (T.stripPrefix "\xFEFF" t)
        -- \| Bind an IFC (STEP) file to its EXPRESS schema (chosen via the
        -- FILE_SCHEMA header, default IFC4X3_ADD2) and produce a P_Context. The
        -- schema is loaded from the statically bundled IFCSchemas resource.
        parseIfcStep :: FilePath -> Text -> RIO env (Guarded P_Context)
        parseIfcStep fp ifcText = do
          let schemaName = fromMaybe defaultSchemaName (fileSchemaName ifcText)
              schemaFile = T.unpack schemaName <> ".exp"
          case getStaticFileContent IFCSchemas schemaFile of
            Just cont ->
              pure $ ifc2PContextFromTexts (T.pack fp) ifcText (decodeUtf8 cont)
            Nothing ->
              -- Unknown schema name: fall back to the default schema if possible.
              case getStaticFileContent IFCSchemas (T.unpack defaultSchemaName <> ".exp") of
                Just cont ->
                  pure $ ifc2PContextFromTexts (T.pack fp) ifcText (decodeUtf8 cont)
                Nothing ->
                  pure
                    $ mkErrorReadingINCLUDE
                      (pcOrigin pc)
                      ["No bundled EXPRESS schema found for " <> schemaName <> " (looked for " <> T.pack schemaFile <> ")."]
        guardedFromContext :: Guarded (P_Context, [Include]) -> Guarded (SingleFileResult, [Include])
        guardedFromContext gIn = do
          (ctx, includes) <- gIn
          return (fromContext ctx, includes)
        fromContext :: P_Context -> SingleFileResult
        fromContext = FromADL
        fromGraph :: RDF TList -> SingleFileResult
        fromGraph = FromGraph
        include2ParseCandidate :: Include -> RIO env (Guarded ParseCandidate)
        include2ParseCandidate (Include org str defs mAlias) = do
          let canonical = myNormalise (takeDirectory filePath </> str)
              defineds = processFlags (pcDefineds pc) (map T.unpack defs)
          return
            $ Checked
              ParseCandidate
                { pcBasePath = Just filePath,
                  pcOrigin = Just org,
                  pcFileKind = pcFileKind pc,
                  pcCanonical = canonical,
                  pcDefineds = defineds,
                  pcAlias = mAlias
                }
              []
        myNormalise :: FilePath -> FilePath
        -- see http://neilmitchell.blogspot.nl/2015/10/filepaths-are-subtle-symlinks-are-hard.html why RIO.FilePath doesn't support reduction of x/foo/../bar into x/bar.
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

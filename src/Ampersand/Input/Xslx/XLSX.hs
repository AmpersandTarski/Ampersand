{-# LANGUAGE DuplicateRecordFields #-}

module Ampersand.Input.Xslx.XLSX
  ( parseXlsxFile,
    XlsxIfcSheet (..),
    xlsxIfcSheet2pops,
  )
where

import Ampersand.Basics hiding (view, (^.), (^?))
import Ampersand.Core.ParseTree
import Ampersand.Core.ShowPStruct -- Just for debugging purposes
import Ampersand.Input.ADL1.CtxError
import Ampersand.Misc.HasClasses
import Ampersand.Prototype.StaticFiles_Generated
import Codec.Xlsx
import Control.Lens hiding (both)
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import RIO.Char
import RIO.FilePath (takeBaseName)
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T

parseXlsxFile ::
  (HasTrimXLSXOpts env) =>
  Maybe FileKind ->
  FilePath ->
  RIO env (Guarded (P_Context, [XlsxIfcSheet]))
parseXlsxFile mFk file =
  do
    env <- ask
    bytestr <-
      case mFk of
        Just fileKind ->
          case getStaticFileContent fileKind file of
            Just cont -> return cont
            Nothing -> fatal ("Statically included " <> tshow fileKind <> " files. \n  Cannot find `" <> T.pack file <> "`.")
        Nothing ->
          liftIO $ B.readFile file
    return . xlsx2pContext env . toXlsx . BL.fromStrict $ bytestr
  where
    file1 = case takeBaseName file of
      [] -> fatal "Filename must not be empty."
      h : tl -> Text1 h (T.pack tl)
    xlsx2pContext ::
      (HasTrimXLSXOpts env) =>
      env ->
      Xlsx ->
      Guarded (P_Context, [XlsxIfcSheet])
    xlsx2pContext env xlsx = do
      let orig = Origin $ "file `" <> tshow file1 <> "`"
      namepart <- toNameGuarded orig ContextName file1
      let pCtx = pop namepart
      -- The interface-format worksheets cannot be resolved here, because the INTERFACE
      -- definitions live in sibling .adl files that are only available after merging all
      -- parsed contexts. They are carried out raw and resolved in Parsing.hs (post-merge).
      ( case (ctx_pops pCtx, ifcSheets) of
          ([], []) -> addWarning $ mkParserStateWarning orig emptyMsg
          _ -> id
        )
        $ pure (pCtx, ifcSheets)
      where
        doTrim = view trimXLSXCellsL env
        ifcSheets = concatMap (interfaceSheet file) (xlsx ^. xlSheets)
        emptyMsg =
          T.intercalate
            "\n  "
            [ "File " <> tshow file1 <> " seems to be empty.",
              "Please make sure it is formatted as a regular Excel .xlsx file and that it is not empty."
            ]

        pop nm =
          mkContextOfPops (withNameSpace nameSpaceOfXLXSfiles nm)
            . concatMap (toPops doTrim nameSpaceOfXLXSfiles file)
            . concatMap (theSheetCellsForTable nameSpaceOfXLXSfiles)
            $ (xlsx ^. xlSheets)

toNameGuarded :: Origin -> NameType -> Text1 -> Guarded Name
toNameGuarded orig typ t = case try2Name typ (text1ToText t) of
  Left msg -> mustBeValidName orig msg
  Right (nm, _) -> pure nm

nameSpaceOfXLXSfiles :: NameSpace
nameSpaceOfXLXSfiles = [] -- Just for a start. Let's fix this whenever we learn more about namespaces.

mkContextOfPops :: Name -> [P_Population] -> P_Context
mkContextOfPops ctxName pops1 =
  -- trace ("pops1 = " <> tshow pops1)
  PCtx
    { ctx_nm = ctxName,
      ctx_lbl = Nothing,
      ctx_pos = map origin pops1,
      ctx_lang = Nothing,
      ctx_markup = Nothing,
      ctx_pats = [],
      ctx_rs = [],
      ctx_ds = mergeRels genericRelations,
      ctx_cs = [],
      ctx_ks = [],
      ctx_rrules = [],
      ctx_reprs = [],
      ctx_vs = [],
      ctx_gs = [],
      ctx_ifcs = [],
      ctx_ps = [],
      ctx_pops = genericPopulations,
      ctx_metas = [],
      ctx_enfs = []
    }
  where
    popRelations :: [P_Relation] -- relations that are "annotated" by the user in Excel-sheets.
    -- popRelations are derived from P_Populations only.
    popRelations =
      [ rel
        | pop@P_RelPopu {} <- pops1,
          rel <-
            [ P_Relation
                { dec_nm = name pop,
                  dec_sign = (fromMaybe (fatal ("Manufacturing error in Ampersand: relation "<>tshow (name pop)<>" ("<>tshow (origin pop)<>") has no signature.")) .
                              p_mbSign . p_nmdr) pop,
                  dec_label = Nothing,
                  dec_prps = mempty,
                  dec_defaults = mempty,
                  dec_pragma = Nothing,
                  dec_Mean = mempty,
                  dec_pos = origin pop
                }
            ]
      ]

    genericRelations :: [P_Relation] -- generalization of popRelations due to CLASSIFY statements
    genericPopulations :: [P_Population] -- generalization of popRelations due to CLASSIFY statements
    (genericRelations, genericPopulations) =
      recur [] popRelations pops2 invGen
      where
        recur :: [P_Concept] -> [P_Relation] -> [P_Population] -> [(P_Concept, Set.Set P_Concept)] -> ([P_Relation], [P_Population])
        recur seen unseenrels unseenpops ((g, specs) : invGens) =
          if g `elem` seen
            then fatal ("Concept " <> fullName g <> " has caused a cycle error.")
            else recur (g : seen) (genericRels <> remainder) (genericPops <> remainPop) invGens
          where
            sameNameTargetRels :: [NE.NonEmpty P_Relation]
            sameNameTargetRels = eqCl (\r -> (name r, targt r)) unseenrels
            genericRels :: [P_Relation]
            remainingRels :: [[P_Relation]]
            (genericRels, remainingRels) =
              L.unzip
                [ ( headrel
                      { dec_sign = P_Sign g (targt (NE.head sRel)),
                        dec_prps =
                          let test prop = prop `elem` foldr (Set.intersection . dec_prps) Set.empty sRel
                           in Set.fromList $ filter (not . test) [P_Uni, P_Tot, P_Inj, P_Sur]
                      }, -- the generic relation that summarizes sRel
                    [rel | rel <- NE.toList sRel, sourc rel `notElem` specs] -- the remaining relations
                  )
                  | sRel <- sameNameTargetRels,
                    specs `Set.isSubsetOf` (Set.fromList . NE.toList $ fmap sourc sRel),
                    headrel <- [NE.head sRel]
                ]
            remainder :: [P_Relation]
            remainder =
              concat
                ( remainingRels
                    <> fmap
                      NE.toList
                      [ sRel | sRel <- sameNameTargetRels, not (specs `Set.isSubsetOf` (Set.fromList . NE.toList $ fmap sourc sRel))
                      ]
                )
            sameNameTargetPops :: [NE.NonEmpty P_Population]
            sameNameTargetPops = eqCl (\r -> (name r, tgtPop r)) unseenpops
            genericPops :: [P_Population]
            remainingPops :: [[P_Population]]
            (genericPops, remainingPops) =
              L.unzip
                [ ( headPop {p_src = Just g}, -- the generic relation that summarizes sRel
                    [pop | pop <- NE.toList sPop, srcPop pop `notElem` specs] -- the remaining relations
                  )
                  | sPop <- sameNameTargetPops,
                    specs `Set.isSubsetOf` (Set.fromList . NE.toList $ fmap srcPop sPop),
                    headPop@P_RelPopu {} <- [NE.head sPop] -- Restrict to @P_RelPopu{} because field name p_src is being used
                ]
            remainPop :: [P_Population]
            remainPop =
              concat
                ( remainingPops
                    <> fmap
                      NE.toList
                      [ sPop | sPop <- sameNameTargetPops, not (specs `Set.isSubsetOf` (Set.fromList . NE.toList $ fmap srcPop sPop))
                      ]
                )
        recur _ rels popus [] = (rels, popus)
        srcPop, tgtPop :: P_Population -> P_Concept -- get the source concept of a P_Population.
        srcPop pop@P_CptPopu {} = PCpt (name pop)
        srcPop pop@P_RelPopu {p_src = src} = case src of Just s -> s; _ -> fatal ("srcPop (" <> showP pop <> ") is mistaken.")
        tgtPop pop@P_CptPopu {} = PCpt (name pop)
        tgtPop pop@P_RelPopu {p_tgt = tgt} = case tgt of Just t -> t; _ -> fatal ("tgtPop (" <> showP pop <> ") is mistaken.")

    sourc, targt :: P_Relation -> P_Concept -- get the source concept of a P_Relation.
    sourc = pSrc . dec_sign
    targt = pTgt . dec_sign
    invGen :: [(P_Concept, Set.Set P_Concept)] -- each pair contains a concept with all of its specializations
    invGen =
      [ (fst (NE.head cl), Set.fromList spcs)
        | cl <- eqCl fst [(g, specific gen) | gen <- [], g <- NE.toList (generics gen)],
          g <- [fst (NE.head cl)],
          spcs <- [[snd c | c <- NE.toList cl, snd c /= g]],
          not (null spcs)
      ]
    concepts =
      L.nub
        $ [PCpt (name pop) | pop@P_CptPopu {} <- pops1]
        <> [src' | P_RelPopu {p_src = src} <- pops1, Just src' <- [src]]
        <> [tgt' | P_RelPopu {p_tgt = tgt} <- pops1, Just tgt' <- [tgt]]
    pops2 = computeConceptPopulations pops1
    computeConceptPopulations :: [P_Population] -> [P_Population]
    computeConceptPopulations pps -- I feel this computation should be done in P2A_Converters.hs, so every A_structure has compliant populations.
      =
      [ P_CptPopu
          { pos = OriginUnknown,
            p_cpt = c,
            p_popas =
              L.nub
                $ [atom | cpt@P_CptPopu {} <- pps, name cpt == name c, atom <- p_popas cpt]
                <> [ ppLeft pair
                     | pop@P_RelPopu {p_src = src} <- pps,
                       Just src' <- [src],
                       src' == c,
                       pair <- p_popps pop
                   ]
                <> [ ppRight pair
                     | pop@P_RelPopu {p_tgt = tgt} <- pps,
                       Just tgt' <- [tgt],
                       tgt' == c,
                       pair <- p_popps pop
                   ]
          }
        | c <- concepts
      ]
        <> [ rpop {p_popps = concatMap p_popps cl}
             | cl <- eqCl (\pop -> (name pop, p_src pop, p_tgt pop)) [pop | pop@P_RelPopu {} <- pps],
               rpop <- [NE.head cl]
           ]

--  | This structure corresponds to a "wide table" (c.q. a Plug)
data SheetCellsForTable = Mapping
  { theSheetName :: Text,
    theCellMap :: CellMap,
    headerRowNrs :: [RowIndex], -- The row numbers of the table header
    popRowNrs :: [RowIndex], -- The row numbers of the population
    colNrs :: [ColumnIndex], -- The column numbers that contain a relation
    debugInfo :: [Text]
  }

instance Show SheetCellsForTable where -- for debugging only
  show x =
    T.unpack
      . T.unlines
      $ [ "Sheet       : " <> theSheetName x,
          "headerRowNrs: " <> tshow (headerRowNrs x),
          "popRowNrs   : " <> tshow (popRowNrs x),
          "colNrs      : " <> tshow (colNrs x)
        ]
      <> debugInfo x

toPops ::
  -- | whether to trim whitespace from text cells
  Bool ->
  NameSpace ->
  -- | The file name is needed for displaying errors in context
  FilePath ->
  SheetCellsForTable ->
  [P_Population]
toPops doTrim ns file x = map popForColumn (colNrs x)
  where
    -- A deterministic, collision-resistant identifier for a `_NEW` atom on a given row.
    -- (The runtime importer generates a random UUID; at compile time we need determinism.)
    freshId :: RowIndex -> Text
    freshId = freshAtomId file (theSheetName x)
    popForColumn :: ColumnIndex -> P_Population
    popForColumn i =
      if i == sourceCol
        then
          P_CptPopu
            { pos = popOrigin,
              p_cpt = mkPConcept sourceConceptName,
              p_popas =
                concat
                  [ case value (row, i) of
                      Nothing -> []
                      Just cv
                        | isNewCell cv -> [XlsxString popOrigin (freshId row)]
                        | otherwise -> cellToAtomValues doTrim mSourceConceptDelimiter cv popOrigin
                    | row <- popRowNrs x
                  ]
            }
        else
          P_RelPopu
            { pos = popOrigin,
              p_src = src,
              p_tgt = trg,
              p_nmdr = PNamedRel popOrigin relationName signature,
              p_popps = thePairs
            }
      where
        src, trg :: Maybe P_Concept
        (src, trg) = case mTargetConceptName of
          Just tCptName -> both (fmap mkPConcept) $ (if isFlipped' then swap else id) (Just sourceConceptName, Just tCptName)
          Nothing -> (Nothing, Nothing)
        signature :: Maybe P_Sign
        signature = case (src, trg) of
          (Just s, Just t) -> Just (P_Sign s t)
          _ -> Nothing
        popOrigin :: Origin
        popOrigin = originOfCell (relNamesRow, targetCol)
        relNamesRow, conceptNamesRow :: RowIndex
        (relNamesRow, conceptNamesRow) = case headerRowNrs x of
          [] -> fatal "headerRowNrs x is empty"
          [rnr] -> (rnr, fatal "headerRowNrs x has only one element")
          rnr : cnr : _ -> (rnr, cnr)
        sourceCol :: ColumnIndex
        sourceCol = case colNrs x of
          [] -> fatal "colNrs x is empty"
          c : _ -> c
        targetCol = i
        sourceConceptName :: Name
        mSourceConceptDelimiter :: Maybe Char
        (sourceConceptName, mSourceConceptDelimiter) =
          case value (conceptNamesRow, sourceCol) of
            Just (CellText t) ->
              fromMaybe
                (fatal "No valid source conceptname found. This should have been checked before")
                (conceptNameWithOptionalDelimiter ns t)
            _ -> fatal "No valid source conceptname found. This should have been checked before"
        mTargetConceptName :: Maybe Name
        mTargetConceptDelimiter :: Maybe Char
        (mTargetConceptName, mTargetConceptDelimiter) =
          case value (conceptNamesRow, targetCol) of
            Just (CellText t) ->
              let (nm, mDel) =
                    fromMaybe
                      (fatal "No valid source conceptname found. This should have been checked before")
                      (conceptNameWithOptionalDelimiter ns t)
               in (Just nm, mDel)
            _ -> (Nothing, Nothing)
        relationName :: Name
        isFlipped' :: Bool
        (relationName, isFlipped') =
          case value (relNamesRow, targetCol) of
            Just (CellText t) ->
              case T.uncons . T.reverse . trim $ t of
                Nothing -> fatal $ "A relation name was expected, but it isn't present." <> tshow (file, relNamesRow, targetCol)
                Just ('~', rest) -> case T.uncons . T.reverse $ rest of
                  Nothing -> fatal "the `~` symbol should be preceded by a relation name. However, it just isn't there."
                  Just (h, tl) ->
                    let (nm, _) = suggestName RelationName (Text1 h tl)
                     in (withNameSpace ns nm, True)
                Just (h, tl) ->
                  let (nm, _) = suggestName RelationName . reverse1 $ Text1 h tl
                      reverse1 :: Text1 -> Text1
                      reverse1 t1 = case T.uncons . T.reverse . text1ToText $ t1 of
                        Nothing -> fatal "Impossible: A Text1 cannot be empty"
                        Just (h', tl') -> Text1 h' tl'
                   in (withNameSpace ns nm, False)
            _ -> fatal ("No valid relation name found. This should have been checked before" <> tshow (relNamesRow, targetCol))
        thePairs :: [PAtomPair]
        thePairs = concat . mapMaybe pairsAtRow . popRowNrs $ x
        pairsAtRow :: RowIndex -> Maybe [PAtomPair]
        pairsAtRow r = case ( value (r, sourceCol),
                              value (r, targetCol)
                            ) of
          (Just s, Just t) ->
            Just
              $ (if isFlipped' then map flp else id) (pairsFor s t)
          _ -> Nothing
          where
            origSrc = XLSXLoc file (theSheetName x) (unRowIndex r, unColumnIndex sourceCol)
            origTrg = XLSXLoc file (theSheetName x) (unRowIndex r, unColumnIndex targetCol)
            -- `_NEW` in column A yields a fresh atom; `_NEW` in a data column refers to that
            -- row's source atom (mirrors the runtime importer's `$rightAtoms[] = $leftAtom`).
            pairsFor sCell tCell
              | isNewCell tCell = [mkPair origTrg s' s' | s' <- srcVals]
              | otherwise =
                  [ mkPair origTrg s' t'
                    | s' <- srcVals,
                      t' <- cellToAtomValues doTrim mTargetConceptDelimiter tCell origTrg
                  ]
              where
                srcVals
                  | isNewCell sCell = [XlsxString origSrc (freshId r)]
                  | otherwise = cellToAtomValues doTrim mSourceConceptDelimiter sCell origSrc
    originOfCell ::
      CellIndex ->
      Origin
    originOfCell (r, c) =
      XLSXLoc file (theSheetName x) (unRowIndex r, unColumnIndex c)

    value :: CellIndex -> Maybe CellValue
    value k = theCellMap x ^? ix k . cellValue . _Just

type CellIndex = (RowIndex, ColumnIndex)

-- This function processes one Excel worksheet and yields every "wide table" (a block of lines in the excel sheet) as a SheetCellsForTable
theSheetCellsForTable :: NameSpace -> (Text, Worksheet) -> [SheetCellsForTable]
theSheetCellsForTable ns (sheetName, ws) =
  catMaybes [theMapping (RowIndex i) | i <- [0 .. length tableStarters - 1]]
  where
    tableStarters :: [CellIndex]
    tableStarters = filter isStartOfTable $ Map.keys (ws ^. wsCells)
      where
        isStartOfTable :: CellIndex -> Bool
        isStartOfTable (rowNr, colNr)
          | colNr /= 1 = False
          | rowNr == 1 = isBracketed' (rowNr, colNr)
          | otherwise =
              isBracketed' (rowNr, colNr)
                && (not . isBracketed') (rowNr - 1, colNr)

    value :: CellIndex -> Maybe CellValue
    value k = (ws ^. wsCells) ^? ix k . cellValue . _Just
    isBracketed' :: CellIndex -> Bool
    isBracketed' k =
      case value k of
        Just (CellText t) -> isBracketed t
        _ -> False

    theMapping :: RowIndex -> Maybe SheetCellsForTable
    theMapping indexInTableStarters
      | length okHeaderRows /= unRowIndex nrOfHeaderRows = Nothing -- Because there are not enough header rows
      | otherwise =
          Just
            Mapping
              { theSheetName = sheetName,
                theCellMap = ws ^. wsCells,
                headerRowNrs = okHeaderRows,
                popRowNrs = populationRows,
                colNrs = theCols,
                debugInfo =
                  [ "indexInTableStarters: " <> tshow indexInTableStarters,
                    "maxRowOfWorksheet   : " <> tshow maxRowOfWorksheet,
                    "maxColOfWorksheet   : " <> tshow maxColOfWorksheet,
                    "startOfTable        : " <> tshow startOfTable,
                    "firstPopRowNr       : " <> tshow firstPopRowNr,
                    "lastPopRowNr        : " <> tshow lastPopRowNr,
                    "[(row,isProperRow)] : " <> T.concat [tshow (r, isProperRow r) | r <- [firstPopRowNr .. lastPopRowNr]],
                    "theCols             : " <> tshow theCols
                  ]
              }
      where
        startOfTable = tableStarters `L.genericIndex` indexInTableStarters
        firstHeaderRowNr = fst startOfTable
        firstColumNr = snd startOfTable
        relationNameRowNr = firstHeaderRowNr
        conceptNameRowNr = firstHeaderRowNr + 1
        nrOfHeaderRows :: RowIndex
        nrOfHeaderRows = 2
        maxRowOfWorksheet :: RowIndex
        maxRowOfWorksheet = case L.maximumMaybe (map fst (Map.keys (ws ^. wsCells))) of
          Nothing -> fatal "Maximum of an empty list is not defined!"
          Just m -> m
        maxColOfWorksheet = case L.maximumMaybe (map snd (Map.keys (ws ^. wsCells))) of
          Nothing -> fatal "Maximum of an empty list is not defined!"
          Just m -> m
        firstPopRowNr = firstHeaderRowNr + nrOfHeaderRows
        lastPopRowNr = ((map fst tableStarters <> [maxRowOfWorksheet + 1]) `L.genericIndex` (indexInTableStarters + 1)) - 1
        okHeaderRows = filter isProperRow [firstHeaderRowNr, firstHeaderRowNr + nrOfHeaderRows - 1]
        populationRows = filter isProperRow [firstPopRowNr .. lastPopRowNr]
        isProperRow :: RowIndex -> Bool
        isProperRow rowNr
          | rowNr == relationNameRowNr = True -- The first row was recognized as tableStarter
          | rowNr == conceptNameRowNr = isProperConceptName (rowNr, firstColumNr)
          | otherwise = notEmpty (rowNr, firstColumNr)
        notEmpty k =
          case value k of
            Just (CellText t) -> (not . T.null . trim) t
            Just (CellDouble _) -> True
            Just (CellBool _) -> True
            Just (CellRich _) -> True
            Just (CellError e) -> fatal $ "Error reading cell " <> tshow e
            Nothing -> False
        theCols = filter isProperCol [1 .. maxColOfWorksheet]
        isProperCol :: ColumnIndex -> Bool
        isProperCol colNr
          | colNr == 1 = isProperConceptName (conceptNameRowNr, colNr)
          | otherwise = isProperConceptName (conceptNameRowNr, colNr) && isProperRelName (relationNameRowNr, colNr)
        isProperConceptName k =
          case value k of
            Just (CellText t) -> isJust . conceptNameWithOptionalDelimiter ns $ t
            _ -> False
        isProperRelName k =
          case value k of
            Just (CellText t) -> (not . T.null . trim) t -- && (isLower . T.head . trim) t
            _ -> False

conceptNameWithOptionalDelimiter ::
  NameSpace ->
  Text ->
  Maybe
    ( Name {- Conceptname -},
      Maybe Char {- Delimiter   -}
    )
-- Cases:  1) "[" <> Conceptname <> delimiter <> "]"
--         2) Conceptname
--         3) none of above
--  Where Conceptname is any string starting with an uppercase character
conceptNameWithOptionalDelimiter ns t'
  | isBracketed t = do
      (d, revInit) <- T.uncons . T.reverse . T.dropEnd 1 . T.drop 1 $ t
      let nm = T.reverse revInit
       in if isDelimiter d && isConceptName nm
            then Just (mkName' nm, Just d)
            else Nothing
  | isConceptName t = Just (mkName' t, Nothing)
  | otherwise = Nothing
  where
    t = trim t'
    mkName' txt =
      withNameSpace
        ns
        ( case try2Name ConceptName txt of
            Left msg -> fatal $ "Not a valid NamePart: " <> msg
            Right (nm, _) -> nm
        )

isDelimiter :: Char -> Bool
isDelimiter = isPunctuation

isConceptName :: Text -> Bool
isConceptName t = case T.uncons t of
  Nothing -> False
  (Just (h, _)) -> isUpper h

-- | trim is used to remove leading and trailing spaces
trim :: Text -> Text
trim = T.reverse . trim' . T.reverse . trim'
  where
    trim' :: Text -> Text
    trim' t = case uncons t of
      Just (' ', t') -> trim' t'
      _ -> t

isBracketed :: Text -> Bool
isBracketed t =
  case T.uncons (trim t) of
    Just ('[', tl) -> case T.uncons (T.reverse tl) of
      Just (']', _) -> True
      _ -> False
    _ -> False

-- * Shared cell helpers (used by both the relation- and interface-approach)

-- | Convert one spreadsheet cell to zero or more atom values, splitting on the
--   optional delimiter. Hoisted to top-level so the interface resolver can reuse it.
cellToAtomValues ::
  -- | whether to trim whitespace from text cells
  Bool ->
  -- | the delimiter, if any, used as separator for multiple values in the cell
  Maybe Char ->
  -- | the value that is read from the cell
  CellValue ->
  -- | the origin of the value
  Origin ->
  [PAtomValue]
cellToAtomValues doTrim mDelimiter cv orig =
  case cv of
    CellText t ->
      map (XlsxString orig)
        . filter (not . T.null)
        . unDelimit mDelimiter
        . handleSpaces doTrim
        $ t
    CellDouble d -> [XlsxDouble orig d]
    CellBool b -> [ComnBool orig b]
    CellRich ts ->
      map (XlsxString orig)
        . filter (not . T.null)
        . unDelimit mDelimiter
        . handleSpaces doTrim
        . T.concat
        . map _richTextRunText
        $ ts
    CellError e ->
      fatal
        . T.intercalate "\n  "
        $ [ "Error reading cell at:",
            tshow orig,
            tshow e
          ]

unDelimit :: Maybe Char -> Text -> [Text]
unDelimit mDelimiter xs =
  case mDelimiter of
    Nothing -> [xs]
    (Just delimiter) -> map trim $ T.split (== delimiter) xs

handleSpaces :: Bool -> Text -> Text
handleSpaces doTrim = if doTrim then trim else id

-- | The literal that means "generate a fresh atom" in column A (and "the row's fresh atom"
--   in a later column), exactly as in the runtime importer.
newToken :: Text
newToken = "_NEW"

isNewCell :: CellValue -> Bool
isNewCell (CellText t) = trim t == newToken
isNewCell _ = False

-- | A deterministic, collision-resistant identifier for a `_NEW` atom on a given row.
--   The runtime importer uses a random UUID; the compiler needs a reproducible value,
--   so we derive one from the file, sheet and row. The `_NEW_` prefix keeps it clear of
--   ordinary data atoms.
freshAtomId :: FilePath -> Text -> RowIndex -> Text
freshAtomId file sheet r =
  "_NEW_" <> T.pack (takeBaseName file) <> "_" <> sheet <> "_" <> tshow (unRowIndex r)

-- * The INTERFACE approach
--
-- A worksheet whose title equals an interface label is imported through that interface,
-- exactly like the runtime importer's @parseWorksheetWithIfc@. Because the INTERFACE
-- definitions are not available while a single .xlsx file is parsed, such worksheets are
-- carried out raw as 'XlsxIfcSheet' and resolved post-merge (see 'xlsxIfcSheet2pops').

-- | The raw content of an interface-format worksheet:
--
-- >          | Column A   | Column B      | Column C      | ...
-- > Row 1    | <Concept>  | <ifc label x> | <ifc label y> | ...
-- > Row 2    | <Atom a1>  | <tgtAtom b1>  | <tgtAtom c1>  | ...
data XlsxIfcSheet = XlsxIfcSheet
  { xisFile :: !FilePath,
    -- | worksheet title; matched against an interface label
    xisTitle :: !Text,
    xisOrigin :: !Origin,
    -- | the (trimmed) text in cell A1: the interface's target concept
    xisA1 :: !Text,
    -- | the data columns: (column index, sub-interface label, optional delimiter)
    xisColumns :: ![(ColumnIndex, Text, Maybe Char)],
    -- | the data rows: (row index, column-A cell, the data cells by column)
    xisRows :: ![(RowIndex, Maybe CellValue, Map.Map ColumnIndex CellValue)]
  }

-- | Classify a worksheet and, when it is in interface-format, return its raw content.
--   Interface-format means: A1 is a non-bracketed concept-like name and B1 is non-empty.
--   This is mutually exclusive with the relation-format (which requires a bracketed A1),
--   so a worksheet never contributes to both approaches.
interfaceSheet :: FilePath -> (Text, Worksheet) -> [XlsxIfcSheet]
interfaceSheet file (sheetName, ws)
  | isInterfaceFormat =
      [ XlsxIfcSheet
          { xisFile = file,
            xisTitle = sheetName,
            xisOrigin = XLSXLoc file sheetName (1, 1),
            xisA1 = case val (1, 1) of
              Just (CellText t) -> trim t
              _ -> "",
            xisColumns = columns,
            xisRows = rows
          }
      ]
  | otherwise = []
  where
    val :: CellIndex -> Maybe CellValue
    val k = (ws ^. wsCells) ^? ix k . cellValue . _Just
    keys = Map.keys (ws ^. wsCells)
    maxCol = fromMaybe 1 (L.maximumMaybe (map snd keys))
    maxRow = fromMaybe 1 (L.maximumMaybe (map fst keys))
    isInterfaceFormat =
      case val (1, 1) of
        Just (CellText t) -> (not . isBracketed) t && isConceptName (trim t) && nonEmptyB1
        _ -> False
    nonEmptyB1 = case val (1, 2) of
      Just (CellText t) -> (not . T.null . trim) t
      Just _ -> True
      Nothing -> False
    columns =
      [ (c, lbl, del)
        | c <- [2 .. maxCol],
          Just (CellText t) <- [val (1, c)],
          let tr = trim t,
          not (T.null tr),
          let (lbl, del) = labelWithOptionalDelimiter tr
      ]
    dataCols = [c | (c, _, _) <- columns]
    -- Stop at the first relation-block starter (a bracketed cell in column A), so a mixed
    -- sheet keeps its relation blocks separate from the interface data.
    -- [2 .. maxRow] is ascending, so the first match is the lowest bracketed-A row.
    lastRow = case [r | r <- [2 .. maxRow], isBracketedA r] of
      [] -> maxRow
      r : _ -> r - 1
    isBracketedA r = case val (r, 1) of
      Just (CellText t) -> isBracketed t
      _ -> False
    rows =
      [ (r, val (r, 1), Map.fromList [(c, v) | c <- dataCols, Just v <- [val (r, c)]])
        | r <- [2 .. lastRow]
      ]

-- | Parse a sub-interface column header that may declare a multi-value column,
--   using the @[label<delim>]@ syntax (e.g. @[Role,]@). Mirrors the runtime importer's
--   @parseHeaderWithOptionalDelimiter@. Unlike 'conceptNameWithOptionalDelimiter', the
--   label is arbitrary text (an interface label), not a concept name.
labelWithOptionalDelimiter :: Text -> (Text, Maybe Char)
labelWithOptionalDelimiter raw =
  let v = trim raw
   in case T.stripPrefix "[" v of
        Just inner
          | "]" `T.isSuffixOf` inner,
            T.length inner >= 3 -> -- inner == <name><delim>]
              case T.uncons . T.reverse . T.dropEnd 1 $ inner of -- drop ']', then split off <delim>
                Just (d, revName) -> (trim (T.reverse revName), Just d)
                Nothing -> (v, Nothing)
        _ -> (v, Nothing)

-- | A resolved data column: the editable relation it populates, its declared signature,
--   and whether it is flipped relative to the interface's target concept.
data ResolvedCol = ResolvedCol
  { rcCol :: !ColumnIndex,
    rcRel :: !Name,
    rcSign :: !P_Sign,
    rcFlipped :: !Bool,
    rcDelim :: !(Maybe Char)
  }

-- | Resolve one interface-format worksheet against the (merged) interfaces and relations,
--   yielding the same populations the runtime importer's @parseWorksheetWithIfc@ produces.
--   If the worksheet title is not an interface label, the sheet is ignored (a no-op, as
--   before this feature existed).
xlsxIfcSheet2pops ::
  -- | whether to trim whitespace from text cells
  Bool ->
  [P_Interface] ->
  [P_Relation] ->
  XlsxIfcSheet ->
  Guarded [P_Population]
xlsxIfcSheet2pops doTrim ifcs rels sheet =
  case L.find ((== title) . label) ifcs of
    Nothing -> pure [] -- worksheet title is not an interface label: ignore
    Just ifc -> do
      cpt <- targetConcept ifc
      resolved <- traverse (resolveColumn ifc cpt) (xisColumns sheet)
      pure (cptPopulation cpt : concatMap relPopulation resolved)
  where
    title = xisTitle sheet
    file = xisFile sheet
    orig = xisOrigin sheet

    targetConcept :: P_Interface -> Guarded P_Concept
    targetConcept ifc =
      case conceptNameWithOptionalDelimiter [] (xisA1 sheet) of
        Nothing ->
          mkGenericParserError orig
            $ "Cell A1 of sheet '" <> title <> "' must contain the target concept of interface '" <> label ifc <> "'."
        Just (a1nm, _) ->
          case ifcSrcTgt (obj_term (ifc_Obj ifc)) of
            Nothing ->
              mkGenericParserError (origin ifc)
                $ "Cannot determine the target concept of interface '" <> label ifc <> "' for use as an import interface."
            Just (mSrc, tgt) -> do
              -- Best-effort SESSION check: only when the source is syntactically known.
              case mSrc of
                Just s
                  | name s /= nameOfSESSION ->
                      mkGenericParserError (origin ifc)
                        $ "Source concept of interface '" <> label ifc <> "' must be SESSION in order to be used as import interface."
                _ -> pure ()
              if sameConcept (mkPConcept a1nm) tgt
                then pure tgt
                else
                  mkGenericParserError orig
                    $ "Target concept of interface '" <> label ifc <> "' does not match the concept in cell A1 of sheet '" <> title <> "'."

    resolveColumn :: P_Interface -> P_Concept -> (ColumnIndex, Text, Maybe Char) -> Guarded ResolvedCol
    resolveColumn ifc cpt (col, lbl, delim) =
      case L.find ((== Just lbl) . boxItemLabel) (topBoxItems ifc) of
        Nothing ->
          mkGenericParserError orig
            $ "Sheet '" <> title <> "' has a column '" <> lbl <> "' that is not a sub-interface of interface '" <> label ifc <> "'."
        Just P_BxTxt {} -> notEditable lbl
        Just item@P_BoxItemTerm {} ->
          case editableRel (obj_term item) of
            Nothing -> notEditable lbl
            Just (relNm, isFlp) ->
              case resolveRelation cpt relNm isFlp of
                Left msg -> mkGenericParserError (origin item) msg
                Right sgn ->
                  pure
                    ResolvedCol
                      { rcCol = col,
                        rcRel = relNm,
                        rcSign = sgn,
                        rcFlipped = isFlp,
                        rcDelim = delim
                      }
      where
        notEditable l =
          mkGenericParserError orig
            $ "Column '" <> l <> "' in sheet '" <> title <> "' does not correspond to an editable relation in interface '" <> label ifc <> "'."

    -- Resolve a relation name (from a box item) against the declared relations, using the
    -- interface's target concept to disambiguate. Returns the declared signature.
    resolveRelation :: P_Concept -> Name -> Bool -> Either Text P_Sign
    resolveRelation cpt relNm isFlp =
      case (exact, candidates) of
        ([d], _) -> Right (dec_sign d)
        (_, [d]) -> Right (dec_sign d)
        ([], []) -> Left $ "Relation '" <> fullName relNm <> "' (used in the interface) is not declared."
        _ -> Left $ "Relation '" <> fullName relNm <> "' is ambiguous; cannot determine which declaration to use for import."
      where
        candidates = [d | d <- rels, sameName (dec_nm d) relNm]
        matchEnd d = if isFlp then pTgt (dec_sign d) else pSrc (dec_sign d)
        exact = [d | d <- candidates, sameConcept (matchEnd d) cpt]

    relPopulation :: ResolvedCol -> [P_Population]
    relPopulation rc =
      [ P_RelPopu
          { pos = orig,
            p_src = Just (pSrc (rcSign rc)),
            p_tgt = Just (pTgt (rcSign rc)),
            p_nmdr = PNamedRel orig (rcRel rc) (Just (rcSign rc)),
            p_popps = concatMap rowPairs (xisRows sheet)
          }
      ]
      where
        rowPairs (r, mA, dataMap) =
          case mA of
            Just aCell
              | not (isBlank aCell) ->
                  case Map.lookup (rcCol rc) dataMap of
                    Just cv
                      | not (isBlank cv) ->
                          let aOrig = XLSXLoc file title (unRowIndex r, 1)
                              cOrig = XLSXLoc file title (unRowIndex r, unColumnIndex (rcCol rc))
                              leftVals = leftAtoms r aCell aOrig
                           in if isNewCell cv
                                then [mkPair cOrig lv lv | lv <- leftVals]
                                else
                                  [ if rcFlipped rc then mkPair cOrig v lv else mkPair cOrig lv v
                                    | lv <- leftVals,
                                      v <- cellToAtomValues doTrim (rcDelim rc) cv cOrig
                                  ]
                    _ -> []
            _ -> []

    cptPopulation :: P_Concept -> P_Population
    cptPopulation cpt =
      P_CptPopu
        { pos = orig,
          p_cpt = cpt,
          p_popas = concatMap leftOfRow (xisRows sheet)
        }
      where
        leftOfRow (r, mA, _) =
          case mA of
            Just aCell
              | not (isBlank aCell) -> leftAtoms r aCell (XLSXLoc file title (unRowIndex r, 1))
            _ -> []

    -- The atom(s) in column A of a row (a single atom in practice). `_NEW` yields a fresh one.
    leftAtoms :: RowIndex -> CellValue -> Origin -> [PAtomValue]
    leftAtoms r aCell aOrig
      | isNewCell aCell = [XlsxString aOrig (freshAtomId file title r)]
      | otherwise = cellToAtomValues doTrim Nothing aCell aOrig

isBlank :: CellValue -> Bool
isBlank (CellText t) = T.null (trim t)
isBlank _ = False

sameName :: (Named a, Named b) => a -> b -> Bool
sameName a b = localNameOf a == localNameOf b

sameConcept :: P_Concept -> P_Concept -> Bool
sameConcept a b = localNameOf a == localNameOf b

-- | The sub-interface boxes directly under an interface's top BOX.
topBoxItems :: P_Interface -> [P_BoxItem TermPrim]
topBoxItems ifc = case ifc_Obj ifc of
  P_BoxItemTerm {obj_msub = Just P_Box {si_box = items}} -> items
  _ -> []

boxItemLabel :: P_BoxItem a -> Maybe Text
boxItemLabel = fmap text1ToText . obj_PlainName

-- | Extract, at the P-level (pre-typecheck), the editable relation and flip of a box item
--   term. There are no identity epsilons yet (those are inserted by the type checker), so
--   this is a syntactic peeler over the raw parsed term — the P-level analogue of
--   'Ampersand.Core.AbstractSyntaxTree.getExpressionRelation'.
editableRel :: Term TermPrim -> Maybe (Name, Bool)
editableRel t = case t of
  Prim (PNamedR r) -> Just (name r, False)
  Prim (PFlipped (PNamedR r)) -> Just (name r, True)
  PFlp _ e -> case editableRel e of
    Just (n, f) -> Just (n, not f)
    Nothing -> Nothing
  PBrk _ e -> editableRel e
  PCps _ a b
    | isIdentTerm a -> editableRel b
    | isIdentTerm b -> editableRel a
  _ -> Nothing
  where
    isIdentTerm (Prim (Pid _ _)) = True
    isIdentTerm (Prim (PI _)) = True
    isIdentTerm (PBrk _ e) = isIdentTerm e
    isIdentTerm _ = False

-- | The declared source/target concepts of an interface term, used to find the target
--   concept of an import interface (and best-effort SESSION check on the source).
ifcSrcTgt :: Term TermPrim -> Maybe (Maybe P_Concept, P_Concept)
ifcSrcTgt term = do
  tgt <- termTarget term
  pure (termSource term, tgt)

termTarget :: Term TermPrim -> Maybe P_Concept
termTarget t = case t of
  Prim p -> primTarget p
  PCps _ _ b -> termTarget b
  PBrk _ e -> termTarget e
  PFlp _ e -> termSource e
  PKl0 _ e -> termTarget e
  PKl1 _ e -> termTarget e
  _ -> Nothing

termSource :: Term TermPrim -> Maybe P_Concept
termSource t = case t of
  Prim p -> primSource p
  PCps _ a _ -> termSource a
  PBrk _ e -> termSource e
  PFlp _ e -> termTarget e
  PKl0 _ e -> termSource e
  PKl1 _ e -> termSource e
  _ -> Nothing

primTarget :: TermPrim -> Maybe P_Concept
primTarget p = case p of
  Pid _ c -> Just c
  Pfull _ _ c -> Just c
  Patm _ _ mc -> mc
  PNamedR (PNamedRel _ _ (Just (P_Sign _ c))) -> Just c
  PFlipped p' -> primSource p'
  _ -> Nothing

primSource :: TermPrim -> Maybe P_Concept
primSource p = case p of
  Pid _ c -> Just c
  Pfull _ c _ -> Just c
  Patm _ _ mc -> mc
  PNamedR (PNamedRel _ _ (Just (P_Sign c _))) -> Just c
  PFlipped p' -> primTarget p'
  _ -> Nothing

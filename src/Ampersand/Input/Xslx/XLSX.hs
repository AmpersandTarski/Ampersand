{-# LANGUAGE DuplicateRecordFields #-}

module Ampersand.Input.Xslx.XLSX (parseXlsxFile) where

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
  RIO env (Guarded P_Context)
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
      Guarded P_Context
    xlsx2pContext env xlsx = do
      let orig = Origin $ "file `" <> tshow file1 <> "`"
      namepart <- toNameGuarded orig ContextName file1
      let pCtx = pop namepart
      ( case ctx_pops pCtx of
          [] -> addWarning $ mkParserStateWarning orig emptyMsg
          _ -> id
        )
        $ pure pCtx
      where
        emptyMsg =
          T.intercalate
            "\n  "
            [ "File " <> tshow file1 <> " seems to be empty.",
              "Please make sure it is formatted as a regular Excel .xlsx file and that it is not empty."
            ]

        pop nm =
          mkContextOfPops (withNameSpace nameSpaceOfXLXSfiles nm)
            . concatMap (toPops env nameSpaceOfXLXSfiles file)
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
  PCtx
    { ctx_nm = ctxName,
      ctx_lbl = Nothing,
      ctx_pos = [],
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
        | pop@P_RelPopu {p_src = src, p_tgt = tgt} <- pops1,
          Just src' <- [src],
          Just tgt' <- [tgt],
          rel <-
            [ P_Relation
                { dec_nm = name pop,
                  dec_sign = P_Sign src' tgt',
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
                      --   , [ rel| rel<-sRel, sourc rel `elem` specs ]                    -- the specific (and therefore obsolete) relations
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
                --   , [ pop| pop<-sPop, srcPop pop `elem` specs ]    -- the specific (and therefore obsolete) populations
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
  (HasTrimXLSXOpts env) =>
  env ->
  NameSpace ->
  -- | The file name is needed for displaying errors in context
  FilePath ->
  SheetCellsForTable ->
  [P_Population]
toPops env ns file x = map popForColumn (colNrs x)
  where
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
                      Just cv -> cellToAtomValues mSourceConceptDelimiter cv popOrigin
                    | row <- popRowNrs x
                  ]
            }
        else
          P_RelPopu
            { pos = popOrigin,
              p_src = src,
              p_tgt = trg,
              p_nmdr = PNamedRel popOrigin relationName Nothing, -- The P-to-A converter must assign the type.
              p_popps = thePairs
            }
      where
        src, trg :: Maybe P_Concept
        (src, trg) = case mTargetConceptName of
          Just tCptName -> both (fmap mkPConcept) $ (if isFlipped' then swap else id) (Just sourceConceptName, Just tCptName)
          Nothing -> (Nothing, Nothing)
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
              $ (if isFlipped' then map flp else id)
                [ mkPair origTrg s' t'
                  | s' <- cellToAtomValues mSourceConceptDelimiter s origSrc,
                    t' <- cellToAtomValues mTargetConceptDelimiter t origTrg
                ]
          _ -> Nothing
          where
            origSrc = XLSXLoc file (theSheetName x) (unRowIndex r, unColumnIndex sourceCol)
            origTrg = XLSXLoc file (theSheetName x) (unRowIndex r, unColumnIndex targetCol)
        cellToAtomValues ::
          -- \| the delimiter, if there is any, used as seperator for multiple values in the cell
          Maybe Char ->
          -- \| The value that is read from the cell
          CellValue ->
          -- \| the origin of the value.
          Origin ->
          [PAtomValue]
        cellToAtomValues mDelimiter cv orig =
          case cv of
            CellText t ->
              map (XlsxString orig)
                . filter (not . T.null)
                . unDelimit mDelimiter
                . handleSpaces
                $ t
            CellDouble d -> [XlsxDouble orig d]
            CellBool b -> [ComnBool orig b]
            CellRich ts ->
              map (XlsxString orig)
                . filter (not . T.null)
                . unDelimit mDelimiter
                . handleSpaces
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
        handleSpaces = if view trimXLSXCellsL env then trim else id
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

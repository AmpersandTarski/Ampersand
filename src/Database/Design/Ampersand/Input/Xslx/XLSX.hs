{-# LANGUAGE OverloadedStrings #-}
module Database.Design.Ampersand.Input.Xslx.XLSX 
  (parseXlsxFile)
where
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Misc
import Prelude hiding (putStrLn, writeFile) -- make sure everything is UTF8
import Database.Design.Ampersand.Input.ADL1.CtxError
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.Core.ParseTree
import Codec.Xlsx
import qualified Data.ByteString.Lazy as L
import Control.Lens
import qualified Data.Text as T
import qualified Data.Map as M 
import Data.Maybe
import Data.Char
import Data.String
import Database.Design.Ampersand.Prototype.StaticFiles_Generated

parseXlsxFile :: Options 
              -> Bool   -- True iff the file is from FormalAmpersand files in `allStaticFiles` 
              -> FilePath -> IO (Guarded [P_Population])
parseXlsxFile _ useAllStaticFiles file =
  do bytestr <- if useAllStaticFiles
                then case getStaticFileContent FormalAmpersand file of
                      Just cont -> do return $ fromString cont
                      Nothing -> fatal 0 ("Statically included "++ show FormalAmpersand++ " files. \n  Cannot find `"++file++"`.")
                else L.readFile file
     return . xlsx2pContext . toXlsx $ bytestr
 where
  xlsx2pContext :: Xlsx -> Guarded [P_Population]
  xlsx2pContext xlsx 
    = Checked $ concatMap (toPops file) $
         Prelude.concatMap theSheetCellsForTable (xlsx ^. xlSheets . to M.toList)
      
data SheetCellsForTable 
       = Mapping{ theSheetName :: String
                , theCellMap   :: CellMap
                , headerRowNrs :: [Int]
                , popRowNrs    :: [Int]
                , colNrs       :: [Int]
                , debugInfo :: [String]
                }
instance Show SheetCellsForTable where  --for debugging only
  show x 
   = unlines $
      [ "Sheet       : "++theSheetName x
      , "headerRowNrs: "++show (headerRowNrs x)
      , "popRowNrs   : "++show (popRowNrs x)
      , "colNrs      : "++show (colNrs x)
      ] ++ debugInfo x 
toPops :: FilePath -> SheetCellsForTable -> [P_Population]
toPops file x = map popForColumn' (colNrs x)
  where
    popForColumn' i = -- trace (show x ++"(Now column: "++show i++")") $
                    popForColumn i
    popForColumn :: Int -> P_Population
    popForColumn i =
      if i  == sourceCol  
      then  P_CptPopu { p_orig = popOrigin
                      , p_cnme = sourceConceptName 
                      , p_popas = concat [ case value(row,i) of
                                             Nothing -> []
                                             Just cv -> cellToAtomValue mSourceConceptDelimiter cv popOrigin
                                         | row <- popRowNrs x
                                         ] 
                      }
      else  P_RelPopu { p_orig = popOrigin
                      , p_src = src
                      , p_tgt = trg
                      , p_nmdr 
                         = PNamedRel popOrigin relName Nothing
                      , p_popps = thePairs
                      }
     where                             
       src, trg :: Maybe String
       (src,trg) = case mTargetConceptName of
                  Just tCptName -> (if isFlipped then swap else id) (Just sourceConceptName, Just tCptName)
                  Nothing -> (Nothing,Nothing)
          where swap (a,b) = (b,a)
       popOrigin :: Origin
       popOrigin = originOfCell (relNamesRow, targetCol)
       conceptNamesRow = headerRowNrs x !! 1
       relNamesRow     = headerRowNrs x !! 0
       sourceCol       = colNrs x !! 0
       targetCol       = i 
       sourceConceptName :: String
       mSourceConceptDelimiter :: Maybe Char
       (sourceConceptName, mSourceConceptDelimiter)
          = case value (conceptNamesRow,sourceCol) of
                Just (CellText t) -> case conceptNameWithOptionalDelimiter t of
                                       Nothing -> fatal 94 "No valid source conceptname found. This should have been checked before"
                                       Just res -> res
                _ -> fatal 96 "No valid source conceptname found. This should have been checked before"
       mTargetConceptName :: Maybe String
       mTargetConceptDelimiter :: Maybe Char
       (mTargetConceptName, mTargetConceptDelimiter)
          = case value (conceptNamesRow,targetCol) of
                Just (CellText t) -> let (nm,mDel) = case conceptNameWithOptionalDelimiter t of
                                                      Nothing -> fatal 94 "No valid source conceptname found. This should have been checked before"
                                                      Just res -> res
                                     in (Just nm, mDel)
                _ -> (Nothing, Nothing)
       relName :: String
       isFlipped :: Bool
       (relName,isFlipped) 
          = case value (relNamesRow,targetCol) of
                Just (CellText t) -> 
                    let str = T.unpack t
                    in if last str == '~'
                       then (init str, True )
                       else (     str, False)
                _ -> fatal 87 $ "No valid relation name found. This should have been checked before" ++show (relNamesRow,targetCol)
       thePairs :: [PAtomPair]
       thePairs =  concat . catMaybes . map pairsAtRow . popRowNrs $ x
       pairsAtRow :: Int -> Maybe [PAtomPair]
       pairsAtRow r = case (value (r,sourceCol)
                          ,value (r,targetCol)
                          ) of
                       (Just s,Just t) -> Just $ 
                                            (if isFlipped then map flp else id) $
                                                [mkPair origTrg a b
                                                | a <- cellToAtomValue mSourceConceptDelimiter s origSrc
                                                , b <- cellToAtomValue mTargetConceptDelimiter t origTrg
                                                ]
                       _               -> Nothing
            where origSrc = XLSXLoc file (theSheetName x) (r,sourceCol)
                  origTrg = XLSXLoc file (theSheetName x) (r,targetCol)
       cellToAtomValue :: Maybe Char -> CellValue -> Origin -> [PAtomValue]  -- The value in a cell can contain the delimeter of the row
       cellToAtomValue mDelimiter cv orig
         = case cv of
             CellText t   -> map (XlsxString orig) (map T.unpack (unDelimit mDelimiter t))
             CellDouble d -> [XlsxDouble orig d]
             CellBool b -> [ComnBool orig b] 
             CellRich ts -> map (XlsxString orig) . map T.unpack . unDelimit mDelimiter . T.concat . map _richTextRunText $ ts
       unDelimit :: Maybe Char -> T.Text -> [T.Text]
       unDelimit mDelimiter xs =
         case mDelimiter of
           Nothing -> [xs]
           (Just delimiter) -> T.split (== delimiter) xs
            
    originOfCell :: (Int,Int) -- (row number,col number)
                 -> Origin
    originOfCell (r,c) 
      = XLSXLoc file (theSheetName x) (r,c) 

    value :: (Int,Int) -> Maybe CellValue
    value k = (theCellMap x) ^? ix k . cellValue . _Just


theSheetCellsForTable :: (T.Text,Worksheet) -> [SheetCellsForTable]
theSheetCellsForTable (sheetName,ws) 
  =  catMaybes [theMapping i | i <- [0..(Prelude.length tableStarters) - 1]]
  where
    tableStarters :: [(Int,Int)]
    tableStarters = Prelude.filter isStartOfTable $ M.keys (ws  ^. wsCells)  
      where isStartOfTable :: (Int,Int) -> Bool
            isStartOfTable (rowNr,colNr)
              | colNr /= 1 = False
              | rowNr == 1 = isBracketed (rowNr,colNr) 
              | otherwise  =           isBracketed (rowNr     ,colNr)  
                             && (not $ isBracketed (rowNr - 1, colNr))             
              
    value :: (Int,Int) -> Maybe CellValue
    value k = (ws  ^. wsCells) ^? ix k . cellValue . _Just
    isBracketed :: (Int,Int) -> Bool
    isBracketed k = 
       case value k of
         Just (CellText t) -> (not . T.null ) t && T.head t == '[' && T.last t == ']'
         _                 -> False      
    theMapping :: Int -> Maybe SheetCellsForTable
    theMapping indexInTableStarters 
     | length okHeaderRows /= nrOfHeaderRows = Nothing  -- Because there are not enough header rows
     | otherwise
     =  Just -- . (\x->trace (show x) x) $
             Mapping { theSheetName = T.unpack sheetName
                     , theCellMap   = ws  ^. wsCells
                     , headerRowNrs = okHeaderRows
                     , popRowNrs    = populationRows
                     , colNrs       = theCols
                     , debugInfo = [ "indexInTableStarters"++": "++show indexInTableStarters
                                   , "maxRowOfWorksheet"++": "++show maxRowOfWorksheet
                                   , "maxColOfWorksheet"++": "++show maxColOfWorksheet
                                   , "startOfTable     "++": "++show startOfTable
                                   , "firstPopRowNr    "++": "++show firstPopRowNr
                                   , "lastPopRowNr     "++": "++show lastPopRowNr
                                   , "[(row,isProperRow)] "++": "++concatMap show [(r,isProperRow r) | r<- [firstPopRowNr..lastPopRowNr]]
                                   , "theCols          "++": "++show theCols
                                   ] 
                     }
     where
       startOfTable = tableStarters !! indexInTableStarters 
       firstHeaderRowNr = fst startOfTable
       firstColumNr = snd startOfTable
       relationNameRowNr = firstHeaderRowNr
       conceptNameRowNr  = firstHeaderRowNr+1
       nrOfHeaderRows = 2
       maxRowOfWorksheet = Prelude.maximum (Prelude.map fst (M.keys (ws  ^. wsCells)))
       maxColOfWorksheet = Prelude.maximum (Prelude.map snd (M.keys (ws  ^. wsCells)))
       firstPopRowNr = firstHeaderRowNr + nrOfHeaderRows
       lastPopRowNr = ((map fst tableStarters++[maxRowOfWorksheet+1])!!(indexInTableStarters+1))-1
       okHeaderRows = filter isProperRow [firstHeaderRowNr,firstHeaderRowNr+nrOfHeaderRows-1]
       populationRows = filter isProperRow [firstPopRowNr..lastPopRowNr]
       isProperRow :: Int -> Bool
       isProperRow rowNr
          | rowNr == relationNameRowNr = True -- The first row was recognized as tableStarter
          | rowNr == conceptNameRowNr  = isProperConceptName(rowNr,firstColumNr)
          | otherwise                  = notEmpty (rowNr,firstColumNr)
       notEmpty k
          = case value k of
            Just (CellText t)   -> (not . T.null) t
            Just (CellDouble _) -> True
            Just (CellBool _)   -> True
            Just (CellRich _)   -> True
            Nothing -> False
       theCols = filter isProperCol [1..maxColOfWorksheet]
       isProperCol :: Int -> Bool
       isProperCol colNr
          | colNr == 1 = isProperConceptName (conceptNameRowNr,colNr)
          | otherwise  = isProperConceptName (conceptNameRowNr,colNr) && isProperRelName(relationNameRowNr,colNr)
       isProperConceptName k 
         = case value k of
            Just (CellText t) -> isJust (conceptNameWithOptionalDelimiter t)
            _ -> False
       isProperRelName k 
         = case value k of
            Just (CellText t) -> (not . T.null) t && isLower(T.head t)
            _ -> False
               
conceptNameWithOptionalDelimiter :: T.Text -> Maybe ( String     {- Conceptname -} 
                                                    , Maybe Char {- Delimiter   -}
                                             )
-- Cases:  1) "[" ++ Conceptname ++ delimiter ++ "]"
--         2) Conceptname
--         3) none of above
--  Where Conceptname is any string starting with an uppercase character
conceptNameWithOptionalDelimiter t
  | T.null t = Nothing
  | T.head t == '[' && T.last t == ']'
             = let mid = (T.reverse . T.tail . T.reverse . T.tail) t
                   (nm,d) = (T.init mid, T.last mid)
               in if isDelimiter d && isConceptName nm
                  then Just (T.unpack nm , Just d)
                  else Nothing
  | otherwise = if isConceptName t
                then Just (T.unpack t, Nothing)
                else Nothing
           
isDelimiter :: Char -> Bool
isDelimiter = isPunctuation
isConceptName :: T.Text -> Bool
isConceptName t = case T.uncons t of
                    Nothing  -> False
                    (Just (h,_)) -> isUpper h
 
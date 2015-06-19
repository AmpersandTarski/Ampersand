{-# LANGUAGE OverloadedStrings #-}
module Database.Design.Ampersand.Input.Xslx.XLSX 
  (parseXlsxFile)
where
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Misc
import Prelude hiding (putStrLn, writeFile) -- make sure everything is UTF8
import Database.Design.Ampersand.Input.ADL1.CtxError
import Database.Design.Ampersand.ADL1
import Codec.Xlsx
import qualified Data.ByteString.Lazy as L
import Control.Lens
import qualified Data.Text as T
import qualified Data.Map as M 
import Data.Maybe
import Data.Char

fatal :: Int -> String -> a
fatal = fatalMsg "XLSX"

parseXlsxFile :: Options -> FilePath -> IO (Guarded [P_Population])
parseXlsxFile _ filePath = 
  do bytestr <- L.readFile filePath
     return . xlsx2pContext . toXlsx $ bytestr
 where
  xlsx2pContext :: Xlsx -> Guarded [P_Population]
  xlsx2pContext xlsx 
    = Checked $ concatMap (toPops filePath) $
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
    popForColumn i 
      | i  == sourceCol  
        =  P_CptPopu { p_orig = popOrigin
                     , p_cnme = sourceConceptName 
                     , p_popas = [fromMaybe (fatal 49 $ 
                                                "this value should be present. (sheet,row,col) = "++show (theSheetName x,row,i)
                                            )
                                            (case value(row,i) of
                                               Just (CellText t)   -> Just (show t)
                                               Just (CellDouble d) -> Just (show d)
                                               Just (CellBool b)   -> Just (show b)
                                               Nothing -> Nothing)
                                 | row <- popRowNrs x] 
                     }
      | otherwise
        =  (case mTargetConceptName of
             Just tCptName 
                -> P_TRelPop { p_orig = popOrigin
                             , p_rnme = relName
                             , p_type = (if isFlipped then flp else id)
                                        P_Sign {pSrc = PCpt sourceConceptName
                                               ,pTgt = PCpt tCptName
                                               }
                             , p_popps = thePairs}
             Nothing
                -> P_RelPopu { p_orig = popOrigin
                             , p_rnme = relName
                             , p_popps = thePairs}
           )
     where                             
       popOrigin :: Origin
       popOrigin = originOfCell (relNamesRow, targetCol)
       conceptNamesRow = headerRowNrs x !! 1
       relNamesRow     = headerRowNrs x !! 0
       sourceCol       = colNrs x !! 0
       targetCol       = i 
       sourceConceptName :: String
       sourceConceptName 
          = case value (conceptNamesRow,sourceCol) of
                Just (CellText t) -> T.unpack t
                _ -> fatal 66 "No valid source conceptname found. This should have been checked before"
       mTargetConceptName :: Maybe String
       mTargetConceptName 
          = case value (conceptNamesRow,targetCol) of
                Just (CellText t) -> Just (T.unpack t)
                _ -> Nothing
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
       thePairs = catMaybes (map pairAtRow (popRowNrs x))
       pairAtRow :: Int -> Maybe Paire
       pairAtRow r = case (value (r,sourceCol)
                          ,value (r,targetCol)
                          ) of
                       (Just s,Just t) -> Just $ 
                                            (if isFlipped then flp else id) $
                                                mkPair (cellToString s) (cellToString t)
                       _ -> Nothing
       cellToString :: CellValue -> String
       cellToString cv = case cv of
                          CellText t -> T.unpack t
                          CellDouble d -> show d
                          CellBool b -> show b 
    originOfCell :: (Int,Int) -- (row number,col number)
                 -> Origin
    originOfCell (r,c) 
      = Origin $ file ++",\n  Sheet: "++theSheetName x++", Cell ("++show r++","++show c++")" --TODO: Make separate Origin constructor for this 

    value :: (Int,Int) -> Maybe CellValue
    value k = (theCellMap x) ^? ix k . cellValue . _Just


theSheetCellsForTable :: (T.Text,Worksheet) -> [SheetCellsForTable]
theSheetCellsForTable (sheetName,ws) 
  =  catMaybes [theMapping i | i <- [0..(Prelude.length tableStarters) - 1]]
  where
    tableStarters :: [(Int,Int)]
    tableStarters = Prelude.filter isStartOfTable $ M.keys (ws  ^. wsCells)  
      where isStartOfTable :: (Int,Int) -> Bool
            isStartOfTable k 
              = (snd k) == 1 && case  value k of
                             Just (CellText t) -> (not . T.null ) t && T.head t == '[' && T.last t == ']' 
                                          --      &&   let t' = T.init t in
                                          --           (not . T.null) t' && isDelimiter (T.last t')
                             _  -> False
    value :: (Int,Int) -> Maybe CellValue
    value k = (ws  ^. wsCells) ^? ix k . cellValue . _Just
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
                     , debugInfo = [ "maxRowOfWorksheet"++": "++show maxRowOfWorksheet
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
       lastPopRowNr = ((map fst tableStarters++[maxRowOfWorksheet])!!(indexInTableStarters+1))-1
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
            Nothing -> False
       theCols = filter isProperCol [1..maxColOfWorksheet]
       isProperCol :: Int -> Bool
       isProperCol colNr
          | colNr == 1 = isProperConceptName (conceptNameRowNr,colNr)
          | otherwise  = isProperConceptName (conceptNameRowNr,colNr) && isProperRelName(relationNameRowNr,colNr)
       isProperConceptName k 
         = case value k of
            Just (CellText t) -> ((not . T.null) t && isUpper(T.head t)) 
--                               || isJust (conceptNameAndDelimiter t)
            _ -> False
       isProperRelName k 
         = case value k of
            Just (CellText t) -> (not . T.null) t && isLower(T.head t)
            _ -> False
               
--conceptNameAndDelimiter :: T.Text -> Maybe (String,Char) --(Conceptname, Delimiter)
--conceptNameAndDelimiter t =
--  case T.uncons t of
--    Nothing         -> Nothing  --t is empty
--    Just (h,tl) 
--         | h /= '[' -> Nothing -- first character is not '['
--         | otherwise -> case T.uncons . T.reverse $ tl of
--                         Nothing -> Nothing -- only one character in t
--                         Just (l,revRest)
--                           | l /= ']' -> Nothing  -- last character is not ']'
--                           | otherwise -> case T.uncons revRest of
--                                            Nothing -> Nothing -- t == "[]"
--                                            Just (c,revName) -> if isDelimiter c
--                                                                then  case T.unpack . T.reverse $ revName of
--                                                                        [] -> Nothing
--                                                                        nm@(h':_) -> if isUpper h' 
--                                                                                     then Just (nm,c)
--                                                                                     else Nothing
--                                                                else Nothing
--               
           
               
isDelimiter :: Char -> Bool
isDelimiter = isPunctuation
         
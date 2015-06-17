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
                }
instance Show SheetCellsForTable where  --for debugging only
  show x 
   = unlines $
      [ "Sheet       : "++theSheetName x
      , "headerRowNrs: "++show (headerRowNrs x)
      , "popRowNrs   : "++show (popRowNrs x)
      , "colNrs      : "++show (colNrs x)
      ]  
toPops :: FilePath -> SheetCellsForTable -> [P_Population]
toPops file x = map popForColumn (colNrs x)
  where
    popForColumn :: Int -> P_Population
    popForColumn i 
      | i  == 1  
        =  P_CptPopu { p_orig = popOrigin
                     , p_cnme = sourceConceptName 
                     , p_popas = map T.unpack [fromMaybe (fatal 49 "this value should be present")
                                                         (case value(row,i) of
                                                           Just (CellText t) -> Just t
                                                           _ -> Nothing)
                                              | row <- popRowNrs x] 
                     }
      | otherwise
        =  (case mTargetConceptName of
             Just tCptName 
                -> P_TRelPop { p_orig = popOrigin
                             , p_rnme = relName
                             , p_type = P_Sign {pSrc = PCpt sourceConceptName
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
       relName 
          = case value (relNamesRow,targetCol) of
                Just (CellText t) -> T.unpack t
                _ -> fatal 87 $ "No valid relation name found. This should have been checked before" ++show (relNamesRow,targetCol)
       thePairs = catMaybes (map pairAtRow (popRowNrs x))
       pairAtRow :: Int -> Maybe Paire
       pairAtRow r = case (value (r,sourceCol)
                          ,value (r,targetCol)
                          ) of
                       (Just s,Just t) -> Just $ mkPair (cellToString s) (cellToString t)
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
                             _  -> False
    value :: (Int,Int) -> Maybe CellValue
    value k = (ws  ^. wsCells) ^? ix k . cellValue . _Just
    theMapping :: Int -> Maybe SheetCellsForTable
    theMapping indexInTableStarters 
     | length headerRows /= 2 = Nothing  -- Because there are not enough header rows
     | otherwise
     =  Just Mapping { theSheetName = T.unpack sheetName
                     , theCellMap   = ws  ^. wsCells
                     , headerRowNrs = headerRows
                     , popRowNrs    = populationRows
                     , colNrs       = theCols
                     }
     where
       (r1,_{-c1-}) = tableStarters !! indexInTableStarters 
       maxRowOfWorksheet = Prelude.maximum (Prelude.map fst (M.keys (ws  ^. wsCells)))
       maxColOfWorksheet = Prelude.maximum (Prelude.map snd (M.keys (ws  ^. wsCells)))
       maxRows = ((map fst tableStarters++[maxRowOfWorksheet])!!(indexInTableStarters+1))-r1+1
       headerRows = map (+ (r1-1)) $ filter isProperRow [1,2]
       populationRows = filter isProperRow [length headerRows+1..maxRows]
       isProperRow :: Int -> Bool
       isProperRow i
          | i == 1 = True -- The first row was recognized as tableStarter
          | i == 2 = isProperConceptName(r1-1+i,1)
          | otherwise = notEmpty (r1-1+i,1)
       notEmpty k
          = case value k of
            Just (CellText t) -> (not . T.null) t
            _ -> False
       theCols = filter isProperCol [1..maxColOfWorksheet]
       isProperCol :: Int -> Bool
       isProperCol i
          | i == 1    = isProperConceptName (r1+1,i)
          | otherwise = isProperConceptName (r1+1,i) && isProperRelName(r1,i)
       isProperConceptName k 
         = case value k of
            Just (CellText t) -> (not . T.null) t && isUpper(T.head t)
            _ -> False
       isProperRelName k 
         = case value k of
            Just (CellText t) -> (not . T.null) t && isLower(T.head t)
            _ -> False
               
   

         
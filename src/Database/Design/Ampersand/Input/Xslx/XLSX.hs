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

xlsx2pContext :: Xlsx -> Guarded [P_Population]
xlsx2pContext xlsx 
  = case Prelude.concatMap theSheetCellsForTable (xlsx ^. xlSheets . to M.toList) of --
                       [] -> fatal 28 "INCLUDE Excel files comming soon."
                       xs -> fatal 29 "INCLUDE Excel files comming soon."

data SheetCellsForTable 
       = Mapping{ theSheetName :: String
                , theCellMap   :: CellMap
                , headerRowNrs :: [Int]
                , popRowNrs    :: [Int]
                , colNrs       :: [Int]
                }

toPops :: FilePath -> SheetCellsForTable -> [P_Population]
toPops file x = map popForColumn (colNrs x)
  where
    popForColumn :: Int -> P_Population
    popForColumn i 
      | i  == 1  
         = P_CptPopu { p_orig = originOfCell (headerRowNrs x !! (2-1), colNrs x !! (1-1))
                     , p_cnme = case value (headerRowNrs x !! (2-1),i) of
                         Just (CellText t) -> T.unpack t
                         _ -> fatal 49 "No valid string fond. This should have been checked before" 
                     , p_popas = map T.unpack [] 
                     }
                             
      | otherwise = fatal 47 "TODO"
    originOfCell :: (Int,Int) -- (row number,col number)
                 -> Origin
    originOfCell (r,c) 
      = Origin $ file ++", sheet: "++theSheetName x++", Cell ("++show r++","++show c++")" --TODO: Make separate Origin constructor for this 

    value :: (Int,Int) -> Maybe CellValue
    value k = (theCellMap x) ^? ix k . cellValue . _Just



theSheetCellsForTable :: (T.Text,Worksheet) -> [SheetCellsForTable]
theSheetCellsForTable (sheetName,ws) 
  = trace (T.unpack sheetName ++": "++show tableStarters) $ 
     catMaybes [mapping i | i <- [0..(Prelude.length tableStarters) - 1]]
  where
    tableStarters :: [(Int,Int)]
    tableStarters = Prelude.filter isStartOfTable $ M.keys cm  
      where cm = ws  ^. wsCells
            ks = M.keys cm
            isStartOfTable :: (Int,Int) -> Bool
            isStartOfTable k@(r,c) 
              = c == 1 && case  value k of
                             Just (CellText t) -> (not . T.null ) t && T.head t == '[' && T.last t == ']'
                             _  -> False
    value :: (Int,Int) -> Maybe CellValue
    value k = (ws  ^. wsCells) ^? ix k . cellValue . _Just
    mapping :: Int -> Maybe SheetCellsForTable
    mapping indexInTableStarters 
     | length headerRows /= 2 = Nothing  -- Because there are not enough header rows
     | otherwise
     = --trace (show ((r1,c1),theRows,theCols)
       --       ) $
        Just Mapping { theSheetName = T.unpack sheetName
                     , theCellMap   = ws  ^. wsCells
                     , headerRowNrs = headerRows
                     , popRowNrs    = populationRows
                     , colNrs       = theCols
                     }
     where
       (r1,_{-c1-}) = tableStarters !! indexInTableStarters 
       maxRowOfWorksheet = Prelude.maximum (Prelude.map fst (M.keys (ws  ^. wsCells)))
       maxColOfWorksheet = Prelude.maximum (Prelude.map snd (M.keys (ws  ^. wsCells)))
       maxRows = (map fst tableStarters++[maxRowOfWorksheet])!!(indexInTableStarters+1)-r1
       headerRows = map (+ r1) $ filter isProperRow [1,2]
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
               
   


type TableContent = [[Cell]]
extractTableContents :: Xlsx -> [TableContent]
extractTableContents xlsx =  Prelude.concatMap tablesOfSheet' theSheets
  where 
        theSheets :: [(T.Text,Worksheet)]
        theSheets = xlsx ^. xlSheets . to M.toList   
        cellmap :: (T.Text,Worksheet) -> CellMap
        cellmap (t,ws) = ws  ^. wsCells
        


tablesOfSheet' :: (T.Text,Worksheet) -> [TableContent]
tablesOfSheet' (t,ws) = trace aap []
   where aap = T.unpack . T.intercalate ("\n  ") . Prelude.map T.pack $ 
                  [ "Sheet: "++T.unpack t
               --   , "columns: " ++ myshow (ws ^? wsColumns )
               --   , "rowprops:"++ show (ws ^? wsRowPropertiesMap  )
                    , "cells: " ++ showCells (ws ^? wsCells  )
               --   , "merges: " ++ show (ws ^? wsMerges  )
                  ]
         myshow :: Maybe [ColumnsWidth] -> String
         myshow Nothing = ""
         myshow (Just cws) = Prelude.concatMap show1 cws 
         show1 (ColumnsWidth min max width style) =  show (min,max,width,style)  
         colums = ws ^? wsCells  
         showCells :: Maybe CellMap -> String
         showCells (Just cm)= "Range: "++show (firstRow ks,lastRow ks)++ " - "++show ( firstCol ks,lastCol ks)
                                       ++show ("starters: "++show (tableStarters cm))
                                       ++Prelude.concat ["\nKeys vanaf start "++show start++" t/m "++show eind++": \n  "
                                                 -- ++show ks
                                                |(start,eind) <-
                                                     [(rij !! i, rij !! (i+1))| i <- [0..Prelude.length rij - 1]]]
                                                
           where ks = M.keys cm
                 rij = tableStarters cm++ [(lastRow ks,1)]
         firstRow ks= Prelude.minimum (Prelude.map fst ks)
         lastRow  ks= Prelude.maximum (Prelude.map fst ks)
         firstCol ks= Prelude.minimum (Prelude.map snd ks)
         lastCol  ks= Prelude.maximum (Prelude.map snd ks)
         tableStarters :: CellMap -> [(Int,Int)]
         tableStarters cm = Prelude.filter isStartOfTable $ M.keys cm  
           where ks = M.keys cm
                 isStartOfTable :: (Int,Int) -> Bool
                 isStartOfTable k@(r,c) 
                   = c == 1 && case  value k of
                                  Just (CellText t) -> (not . T.null ) t && T.head t == '[' && T.last t == ']'
                                  _  -> False
                 value :: (Int,Int) -> Maybe CellValue
                 value k = cm ^? ix k . cellValue . _Just
         tableWidth :: (Int,Int) -> Int
         tableWidth (r,c) = 1 -- length noot
           where -- noot = Prelude.takeWhile (isJust) ((fmap (lookup k          
                keysOfRow = Prelude.filter (\k -> r == fst k) (M.keys cm)
                cm = ws ^. wsCells  
         
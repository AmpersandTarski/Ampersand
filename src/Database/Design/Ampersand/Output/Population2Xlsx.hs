{-# LANGUAGE OverloadedStrings #-}
module Database.Design.Ampersand.Output.Population2Xlsx
  (fSpec2PopulationXlsx)
where
import Database.Design.Ampersand.FSpec
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import System.Time
import qualified Data.Map as M
import Codec.Xlsx
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Maybe
import Data.List

fSpec2PopulationXlsx :: ClockTime -> FSpec -> L.ByteString 
fSpec2PopulationXlsx ct fSpec = 
  fromXlsx ct xlsx
    where
      xlsx =def { _xlSheets = plugs2Sheets fSpec}
               
     

plugs2Sheets :: FSpec -> M.Map T.Text Worksheet
plugs2Sheets fSpec = M.fromList . catMaybes . Prelude.map plug2sheet $ plugInfos fSpec
  where
    plug2sheet :: PlugInfo -> Maybe (T.Text, Worksheet)
    plug2sheet ExternalPlug{} = Nothing  -- Not supported at present
    plug2sheet (InternalPlug plug) = fmap (\x -> (T.pack (name plug),x)) sheet
      where 
       sheet :: Maybe Worksheet
       sheet = case matrix of
                 Nothing -> Nothing
                 Just m -> Just def{_wsCells = fromRows . numberList . Prelude.map numberList $ m }
            where 
              numberList :: [c] -> [(Int, c)]
              numberList = zip [1..] 
       matrix :: Maybe  [[Cell]]
       matrix = 
         case plug of
           TblSQL{} -> Just $ headers ++ content  
           BinSQL{} -> Just $ headers ++ content
           ScalarSQL{} -> Nothing
         where
           headers :: [[Cell]]
           headers = transpose (Prelude.map f (zip (True : repeat False) (plugFields plug))) 
             where f :: (Bool,SqlField) -> [Cell]
                   f (isFirstField,fld) = Prelude.map toCell 
                         [ if isFirstField  -- In case of the first field of the table, we put the fieldname inbetween brackets,
                                            -- to be able to find the population again by the reader of the .xlsx file
                           then Just $ "["++name fld++"]" 
                           else Just . cleanUpRelName $ name fld
                         , Just $ name .target . fldexpr $ fld ]
                   cleanUpRelName :: String -> String
                   --TODO: This is a not-so-nice way to get the relationname from the fieldname.
                   cleanUpRelName orig
                     | isPrefixOf "tgt_" orig = drop 4 orig
                     | isPrefixOf "src_" orig = drop 4 orig
                     | otherwise         = orig
           content = fmap record2Cells (tblcontents (vgens fSpec) (initialPops fSpec) plug)
           record2Cells = fmap toCell
       toCell :: Maybe String -> Cell
       toCell mStr = Cell { _cellStyle = Nothing
                         , _cellValue = fmap (\x -> CellText . T.pack $ x) mStr
                         }
       

  
            
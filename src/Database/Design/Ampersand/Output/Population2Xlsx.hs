{-# LANGUAGE OverloadedStrings #-}
module Database.Design.Ampersand.Output.Population2Xlsx
  (fSpec2PopulationXlsx)
where
import Database.Design.Ampersand.FSpec
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import qualified Data.Map as M
import Codec.Xlsx
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Maybe
import Data.List
import Data.Time.Calendar
import Data.Time.Clock.POSIX

fSpec2PopulationXlsx :: POSIXTime -> FSpec -> L.ByteString 
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
           TblSQL{} -> if length (attributes plug) > 1
                       then Just $ headers ++ content
                       else Nothing
           BinSQL{} -> -- trace ("## Warning: Handling of link-tables isn't correct yet. Therefor, sheet`"++name plug++"` doesn't contain proper info") $
                       Just $ headers ++ content
           ScalarSQL{} -> Nothing
         where
           headers :: [[Cell]]
           headers = transpose (Prelude.map f (zip (True : repeat False) (plugAttributes plug))) 
             where f :: (Bool,SqlAttribute) -> [Cell]
                   f (isFirstField,att) = Prelude.map toCell 
                         [ if isFirstField  -- In case of the first field of the table, we put the fieldname inbetween brackets,
                                            -- to be able to find the population again by the reader of the .xlsx file
                           then Just $ "["++name att++"]" 
                           else Just . cleanUpRelName $
                                          case plug of
                                            TblSQL{}    -> name att
                                            BinSQL{}    -> name plug
                                            ScalarSQL{} -> fatal 57 "ScalarSQL not expected here"
                         , Just $ name .target . attExpr $ att ]
                   cleanUpRelName :: String -> String
                   --TODO: This is a not-so-nice way to get the relationname from the fieldname.
                   cleanUpRelName orig
                     | isPrefixOf "tgt_" orig = drop 4 orig
                     | isPrefixOf "src_" orig = drop 4 orig ++"~" --TODO: Make in less hacky! (See also the way the fieldname is constructed.
                     | otherwise         = orig
           content = fmap record2Cells (tableContents fSpec plug)
           record2Cells :: [Maybe AAtomValue] -> [Cell]
           record2Cells = map record2Cell
           record2Cell :: Maybe AAtomValue -> Cell
           record2Cell mVal = Cell Nothing (case mVal of
                                             Nothing -> Nothing
                                             Just aVal -> Just $
                                                case aVal of
                                                  AAVString _ str -> CellText $ T.pack str
                                                  AAVInteger _ int -> CellDouble (fromInteger int)
                                                  AAVFloat _ x -> CellDouble x
                                                  AAVBoolean _ b -> CellBool b
                                                  AAVDate _ day -> (CellDouble . fromInteger) (diffDays (fromGregorian 1900 1 1) day)
                                                  _ -> fatal 87 ( "Content found that cannot be converted to Excel (yet): "++show aVal) 
                                           )  
       toCell :: Maybe String -> Cell
       toCell mVal 
        = Cell { _cellStyle = Nothing
               , _cellValue = fmap (\x -> CellText . T.pack $ x) mVal
               }
       

  
            
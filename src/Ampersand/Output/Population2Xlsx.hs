{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.Population2Xlsx
  (fSpec2PopulationXlsx)
where

import           Ampersand.Basics
import           Ampersand.ADL1(AAtomValue(..),HasSignature(..),aavtxt)
import           Ampersand.FSpec
import           Codec.Xlsx
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import           RIO.Time
import           Data.Time.Clock.POSIX (POSIXTime)

fSpec2PopulationXlsx :: POSIXTime -> FSpec -> BL.ByteString 
fSpec2PopulationXlsx ct fSpec = 
  fromXlsx ct xlsx
    where
      xlsx =def { _xlSheets = plugs2Sheets fSpec}
               
plugs2Sheets :: FSpec -> [(Text, Worksheet)]
plugs2Sheets fSpec = mapMaybe plug2sheet $ plugInfos fSpec
  where
    plug2sheet :: PlugInfo -> Maybe (Text, Worksheet)
    plug2sheet (InternalPlug plug) = fmap (\x -> (name plug,x)) sheet
      where 
       sheet :: Maybe Worksheet
       sheet = case matrix of
                 Nothing -> Nothing
                 Just m -> Just def{_wsCells = fromRows . numberList . map numberList $ m }
            where 
              numberList :: [c] -> [(Int, c)]
              numberList = zip [1..] 
       matrix :: Maybe  [[Cell]]
       matrix = 
         case plug of
           TblSQL{} -> if length (attributes plug) > 1
                       then Just $ headers <> content
                       else Nothing
           BinSQL{} -> Just $ headers <> content
         where
           headers :: [[Cell]]
           headers = L.transpose (zipWith (curry f) (True : L.repeat False) (NE.toList $ plugAttributes plug)) 
             where f :: (Bool,SqlAttribute) -> [Cell]
                   f (isFirstField,att) = map toCell 
                         [ if isFirstField  -- In case of the first field of the table, we put the fieldname inbetween brackets,
                                            -- to be able to find the population again by the reader of the .xlsx file
                           then Just $ "["<>name att<>"]" 
                           else Just . cleanUpRelName $
                                          case plug of
                                            TblSQL{}    -> name att
                                            BinSQL{}    -> name plug
                         , Just $ name .target . attExpr $ att ]
                   cleanUpRelName :: Text -> Text
                   --TODO: This is a not-so-nice way to get the relationname from the fieldname.
                   cleanUpRelName orig
                     | "tgt_" `T.isPrefixOf` orig = T.drop 4 orig
                     | "src_" `T.isPrefixOf` orig = T.drop 4 orig <>"~" --TODO: Make in less hacky! (See also the way the fieldname is constructed.
                     | otherwise         = orig
           content = fmap record2Cells (tableContents fSpec plug)
           record2Cells :: [Maybe AAtomValue] -> [Cell]
           record2Cells = map record2Cell
           record2Cell :: Maybe AAtomValue -> Cell
           record2Cell mVal = 
              Cell { _cellStyle = Nothing
                   , _cellValue = case mVal of
                                    Nothing -> Nothing
                                    Just aVal -> Just $
                                      case aVal of
                                        AAVString{} -> CellText $ aavtxt aVal
                                        AAVInteger _ int -> CellDouble (fromInteger int)
                                        AAVFloat _ x -> CellDouble x
                                        AAVBoolean _ b -> CellBool b
                                        AAVDate _ day -> (CellDouble . fromInteger) (diffDays (fromGregorian 1900 1 1) day)
                                        _ -> fatal ( "Content found that cannot be converted to Excel (yet): "<>tshow aVal) 
                   , _cellComment = Nothing
                   , _cellFormula = Nothing
                   }
       toCell :: Maybe Text -> Cell
       toCell mVal 
        = Cell { _cellStyle = Nothing
               , _cellValue = fmap CellText mVal
               , _cellComment = Nothing
               , _cellFormula = Nothing
               }
       

  
            
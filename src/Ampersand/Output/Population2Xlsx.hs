{-# LANGUAGE TupleSections #-}

module Ampersand.Output.Population2Xlsx (fSpec2PopulationXlsx) where

import Ampersand.ADL1 (AAtomValue (..), HasSignature (..), aavtxt)
import Ampersand.Basics
import Ampersand.FSpec
import Codec.Xlsx
import Data.Time.Clock.POSIX (POSIXTime)
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import RIO.Time

fSpec2PopulationXlsx :: POSIXTime -> FSpec -> BL.ByteString
fSpec2PopulationXlsx ct fSpec =
  fromXlsx ct xlsx
  where
    xlsx = def {_xlSheets = plugs2Sheets fSpec}

plugs2Sheets :: FSpec -> [(Text, Worksheet)]
plugs2Sheets fSpec = mapMaybe plug2sheet $ plugInfos fSpec
  where
    plug2sheet :: PlugInfo -> Maybe (Text, Worksheet)
    plug2sheet (InternalPlug plug) = fmap (tshow . sqlname $ plug,) sheet
      where
        sheet :: Maybe Worksheet
        sheet = case matrix of
          Nothing -> Nothing
          Just m -> Just def {_wsCells = fromRows . rowsToMatrix . map cellsToRow $ m}
          where
            cellsToRow :: [Cell] -> [(ColumnIndex, Cell)]
            cellsToRow = zip [ColumnIndex 1 ..]
            -- rowsToMatrix :: [[(ColumnIndex, Cell)]] -> [(RowIndex, (ColumnIndex, Cell))]
            rowsToMatrix :: [b] -> [(RowIndex, b)]
            rowsToMatrix = zip [RowIndex 1 ..]
        matrix :: Maybe [[Cell]]
        matrix =
          case plug of
            TblSQL {} ->
              if length (attributes plug) > 1
                then Just $ headers <> content
                else Nothing
            BinSQL {} -> Just $ headers <> content
          where
            headers :: [[Cell]]
            headers = L.transpose (zipWith (curry f) (True : L.repeat False) (NE.toList $ plugAttributes plug))
              where
                f :: (Bool, SqlAttribute) -> [Cell]
                f (isFirstField, att) =
                  map
                    toCell
                    [ if isFirstField -- In case of the first field of the table, we put the fieldname inbetween brackets,
                    -- to be able to find the population again by the reader of the .xlsx file
                        then Just $ "[" <> (tshow . attSQLColName $ att) <> "]"
                        else Just $
                          case plug of
                            TblSQL {} -> tshow . attSQLColName $ att
                            BinSQL {} -> tshow . sqlname $ plug,
                      Just . fullName . target . attExpr $ att
                    ]
            content = fmap record2Cells (tableContents fSpec plug)
            record2Cells :: [Maybe AAtomValue] -> [Cell]
            record2Cells = map record2Cell
            record2Cell :: Maybe AAtomValue -> Cell
            record2Cell mVal =
              Cell
                { _cellStyle = Nothing,
                  _cellValue = case mVal of
                    Nothing -> Nothing
                    Just aVal -> Just $
                      case aVal of
                        AAVString {} -> CellText $ aavtxt aVal
                        AAVInteger _ int -> CellDouble (fromInteger int)
                        AAVFloat _ x -> CellDouble x
                        AAVBoolean _ b -> CellBool b
                        AAVDate _ day -> (CellDouble . fromInteger) (diffDays (fromGregorian 1900 1 1) day)
                        _ -> fatal ("Content found that cannot be converted to Excel (yet): " <> tshow aVal),
                  _cellComment = Nothing,
                  _cellFormula = Nothing
                }
        toCell :: Maybe Text -> Cell
        toCell mVal =
          Cell
            { _cellStyle = Nothing,
              _cellValue = fmap CellText mVal,
              _cellComment = Nothing,
              _cellFormula = Nothing
            }

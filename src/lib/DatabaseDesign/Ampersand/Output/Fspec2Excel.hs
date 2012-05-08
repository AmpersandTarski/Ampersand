{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Output.Fspec2Excel (fspec2Workbook,showSpreadsheet)
where
import Text.XML.SpreadsheetML.Builder
import Text.XML.SpreadsheetML.Types
import Text.XML.SpreadsheetML.Writer (showSpreadsheet)
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec



fspec2Workbook :: Fspc -> Options -> Workbook
fspec2Workbook fspec flags = emptyWorkbook -- for the time being. This is just the stub....
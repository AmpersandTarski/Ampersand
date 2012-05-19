{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Output.Fspec2Excel (fspec2Workbook,showSpreadsheet)
where
import Text.XML.SpreadsheetML.Builder
import Text.XML.SpreadsheetML.Types
import Text.XML.SpreadsheetML.Writer (showSpreadsheet)
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec



fspec2Workbook :: Fspc -> Options -> Workbook
fspec2Workbook _ _ = emptyWorkbook -- for the time being. This is just the stub. The original purpose of this sheet was to export various information to excel, to be able to import it again into Enterprise Architect. However, that has been done by generating an .xmi file.  This stub can now be used at will...
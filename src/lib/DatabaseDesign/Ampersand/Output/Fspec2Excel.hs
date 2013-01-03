{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Output.Fspec2Excel (fspec2Workbook,showSpreadsheet)
where
import Text.XML.SpreadsheetML.Builder
import Text.XML.SpreadsheetML.Types
import Text.XML.SpreadsheetML.Writer (showSpreadsheet)
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.Fspec.FPA
import DatabaseDesign.Ampersand.Basics

fspec2Workbook :: Fspc -> Options -> Workbook
fspec2Workbook fSpec flags =
   Workbook
      { workbookDocumentProperties = Just
          DocumentProperties { documentPropertiesTitle = Just $ "FunctiePuntAnalyse van "++baseName flags
                             , documentPropertiesSubject = Nothing
                             , documentPropertiesKeywords  = Nothing
                             , documentPropertiesDescription = Just $ "Dit document is gegenereerd dmv. "++ampersandVersionStr++"."
                             , documentPropertiesRevision = Nothing
                             , documentPropertiesAppName = Just "Ampersand"
                             , documentPropertiesCreated = Just $ show (genTime flags)
                             }
      , workbookWorksheets = [pimpWs wsDatasets,pimpWs wsFunctions]
      }
  where
    wsDatasets = 
      Worksheet { worksheetName = 
                            Name $ case language flags of
                                    English -> "Datasets"
                                    Dutch   -> "Gegevensverzamelingen"
                , worksheetTable = Just $ emptyTable 
                    { tableRows = 
                          [ mkRow [string "Gegevensverzamelingen" , (number.fromIntegral.length.plugInfos) fSpec]
                          ] ++ 
                          map (mkRow.showDetailsOfPlug flags)(plugInfos fSpec)
                       ++ map mkRow [replicate 2 emptyCell ++ [string "Totaal:", (number.fromIntegral.sum.(map fPoints)) [pSql | InternalPlug pSql <- plugInfos fSpec]]]
                    }
                }
    wsFunctions = 
      Worksheet { worksheetName  = 
                            Name $ case language flags of
                                    English -> "Functions"
                                    Dutch   -> "Functies"
                , worksheetTable = Just $ emptyTable 
                    { tableRows = 
                          [ mkRow [string "Interfaces in ADL-script", (number.fromIntegral.length.interfaceS) fSpec]
                          ] 
                          ++ map (mkRow.showDetailsOfFunction flags) (interfaceS fSpec)
                          ++ map mkRow [replicate 4 emptyCell ++ [string "Totaal:", (number.fromIntegral.sum.(map fPoints)) (interfaceS fSpec)]]
                          ++ map mkRow [[string "Gegenereerde interfaces" , (number.fromIntegral.length.interfaceG) fSpec]]
                          ++ map (mkRow.showDetailsOfFunction flags) (interfaceG fSpec)
                          ++ map mkRow [replicate 4 emptyCell ++ [string "Totaal:", (number.fromIntegral.sum.(map fPoints)) (interfaceG fSpec)]]
                          ++ map mkRow [replicate 5 emptyCell ++ [string "GrandTotaal:", (number.fromIntegral.sum.(map fPoints)) (interfaceS fSpec++interfaceG fSpec)]]
                    }
                }
          
          
showDetailsOfPlug :: Options -> PlugInfo -> [Cell]
showDetailsOfPlug _ plug =
    [ emptyCell
    , (string.name) plug
    , (number.fromIntegral) (case plug of
                              InternalPlug p -> fPoints p
                              ExternalPlug _ -> 0
                            )
    , string ( case plug of
                ExternalPlug _ -> "PHP plugs are not (yet) taken into account!"
                InternalPlug p@TblSQL{} -> "Tabel met "++(show.length.fields) p++" attributen."
                InternalPlug   BinSQL{} -> "Koppel table"
                InternalPlug   ScalarSQL{} -> "Toegestane waarden tabel"
             )
    ] 
showDetailsOfFunction :: Options -> Interface -> [Cell]
showDetailsOfFunction flags ifc =
    [ emptyCell
    , (string.name.ifcObj) ifc
    , string (showLang (language flags) (fpa ifc))
    , (number.fromIntegral.fPoints) ifc
    ]
    
pimpWs :: Worksheet -> Worksheet
pimpWs ws = ws{worksheetTable = fmap pimpWsT (worksheetTable ws)
              }
pimpWsT :: Table -> Table
pimpWsT t = t{tableColumns = map (\c -> c{columnAutoFitWidth = Just AutoFitWidth}) 
                              (tableColumns t ++ replicate (maximum (map (length.rowCells) (tableRows t))) emptyColumn)
             ,tableRows = map pimpRow (tableRows t)
             }
pimpRow :: Row -> Row
pimpRow r = r{rowAutoFitHeight = Just AutoFitHeight}
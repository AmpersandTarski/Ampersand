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
      , workbookWorksheets = [pimpWs wsResume,pimpWs wsDatasets,pimpWs wsFunctions]
      }
  where
    lang = language flags
    wsResume =
      Worksheet { worksheetName =
                            Name $ case lang of
                                    English -> "Resume"
                                    Dutch   -> "Overzicht"
                , worksheetTable = Just $ emptyTable 
                    { tableRows = 
                          [ mkRow [string $ case lang of
                                               English -> "Detailed function point count (according to NESMA 2.1) of the application "
                                               Dutch   -> "Gedetailleerde functiepunentelling (volgens NESMA 2.2) van het systeem "
                                             ++ baseName flags
                                  ]
                          , emptyRow
                          , mkRow [string totalen]
                          , mkRow [string gegevensverzamelingen,(number.fromIntegral) totaalFPgegevensverzamelingen]
                          , mkRow [string gebruikerstransacties,(number.fromIntegral) totaalFPgebruikerstransacties]
                          , mkRow [string grandTotaal,(number.fromIntegral) (totaalFPgegevensverzamelingen + totaalFPgebruikerstransacties)]
                          , emptyRow
                          , mkRow [string $ case lang of
                                    English -> "For this function point count it is assumed that:"
                                    Dutch   -> "Voor deze telling is aangenomen dat:"
                                  ]
                          , mkRow [string $ case lang of
                                    English -> "- the application is built from scratch."
                                    Dutch   -> "- het een nieuw informatiesyteem betreft."
                                  ]
                          , mkRow [string $ case lang of
                                    English -> "- all persistent information will be stored internally"
                                    Dutch   -> "- alle informatie wordt intern opgeslagen"
                                  ]
                          ]
                    }
                }    
    wsDatasets = 
      Worksheet { worksheetName = Name $ gegevensverzamelingen
                , worksheetTable = Just $ emptyTable 
                    { tableRows = 
                          [ mkRow [string gegevensverzamelingen, (number.fromIntegral.length.plugInfos) fSpec]
                          ] ++ 
                          map (mkRow.showDetailsOfPlug)(plugInfos fSpec)
                       ++ map mkRow [replicate 3 emptyCell ++ [string totaal, (number.fromIntegral) totaalFPgegevensverzamelingen]]
                    }
                }
    -- TODO: Also count the PHP plugs
    totaalFPgegevensverzamelingen :: Int
    totaalFPgegevensverzamelingen = (sum.(map fPoints)) [pSql | InternalPlug pSql <- plugInfos fSpec]
    wsFunctions = 
      Worksheet { worksheetName  = Name $ gebruikerstransacties
                , worksheetTable = Just $ emptyTable 
                    { tableRows = 
                          [ mkRow [string $ gebruikerstransacties, (number.fromIntegral.length.interfaceS) fSpec]
                          ] 
                          ++ map (mkRow.showDetailsOfFunction) (interfaceS fSpec)
                          ++ map mkRow [replicate 4 emptyCell ++ [string totaal, (number.fromIntegral.sum.(map fPoints)) (interfaceS fSpec)]]
                          ++ map mkRow [[string "Gegenereerde interfaces" , (number.fromIntegral.length.interfaceG) fSpec]]
                          ++ map (mkRow.showDetailsOfFunction) (interfaceG fSpec)
                          ++ map mkRow [replicate 4 emptyCell ++ [string totaal, (number.fromIntegral.sum.(map fPoints)) (interfaceG fSpec)]]
                          ++ map mkRow [replicate 5 emptyCell ++ [string grandTotaal, (number.fromIntegral) totaalFPgebruikerstransacties ]]
                    }
                }
    totaalFPgebruikerstransacties :: Int
    totaalFPgebruikerstransacties = (sum.(map fPoints)) (interfaceS fSpec++interfaceG fSpec)
    gegevensverzamelingen :: String
    gegevensverzamelingen = case lang of
       Dutch   -> "Gegevensverzamelingen"
       English -> "Data function types"

    gebruikerstransacties :: String
    gebruikerstransacties = case lang of
       Dutch   -> "Gebruikerstransacties"
       English -> "Transactional function types"
    totalen :: String
    totalen = case lang of
       Dutch   -> "Totalen:"
       English -> "Totals:"
    totaal :: String
    totaal = case lang of
       Dutch   -> "Totaal:"
       English -> "Total:"
    grandTotaal  :: String
    grandTotaal = case lang of
       Dutch   -> "Grandtotaal:"
       English -> "Grand total:"
    
    showDetailsOfPlug :: PlugInfo -> [Cell]
    showDetailsOfPlug plug =
       [ emptyCell
       , (string.name) plug
       , string (case plug of
                  InternalPlug p -> showLang lang (fpa p)
                  ExternalPlug _ -> "???"
                )
       , (number.fromIntegral) (case plug of
                                 InternalPlug p -> fPoints p
                                 ExternalPlug _ -> 0
                               )
       , string ( case (lang,plug) of
                   (English, ExternalPlug _)             -> "PHP plugs are not (yet) taken into account!"
                   (Dutch  , ExternalPlug _)             -> "PHP plugs worden (nog) niet meegerekend!"
                   (English, InternalPlug p@TblSQL{})    ->"Table with "++(show.length.fields) p++" attributes."
                   (Dutch  , InternalPlug p@TblSQL{})    -> "Tabel met "++(show.length.fields) p++" attributen."
                   (English, InternalPlug   BinSQL{})    -> "Link table"
                   (Dutch  , InternalPlug   BinSQL{})    -> "Koppel table"
                   (English, InternalPlug   ScalarSQL{}) -> "Enumeration tabel"
                   (Dutch  , InternalPlug   ScalarSQL{}) -> "Toegestane waarden tabel"
                )
       ] 
    showDetailsOfFunction :: Interface -> [Cell]
    showDetailsOfFunction ifc =
       [ emptyCell
       , (string.name.ifcObj) ifc
       , string (showLang lang (fpa ifc))
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
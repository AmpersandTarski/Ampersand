module Ampersand.Output.FSpec2Excel (fspec2FPA_Excel)
where
import Text.XML.SpreadsheetML.Builder
import Text.XML.SpreadsheetML.Types
import Text.XML.SpreadsheetML.Writer (showSpreadsheet)
import Ampersand.Misc
import Ampersand.FSpec
import Ampersand.FSpec.FPA
import Ampersand.Basics
import Data.Maybe

-- TODO: Get rid of package SpreadsheetML. Use http://hackage.haskell.org/package/xlsx (Reason: SpreadsheetML doesn.t have a writer. xlsx package does.

-- NOTE: this code was refactored to support the new FPA module, but has not been tested yet.

fspec2FPA_Excel :: FSpec -> String
fspec2FPA_Excel = showSpreadsheet . fspec2Workbook

fspec2Workbook :: FSpec -> Workbook
fspec2Workbook fSpec =
   Workbook
      { workbookDocumentProperties = Just
          DocumentProperties { documentPropertiesTitle = Just $ "FunctiePuntAnalyse van "++baseName (getOpts fSpec)
                             , documentPropertiesSubject = Nothing
                             , documentPropertiesKeywords  = Nothing
                             , documentPropertiesDescription = Just $ "Dit document is gegenereerd dmv. "++ampersandVersionStr++"."
                             , documentPropertiesRevision = Nothing
                             , documentPropertiesAppName = Just "Ampersand"
                             , documentPropertiesCreated = Just $ show (genTime (getOpts fSpec))
                             }
      , workbookWorksheets = [pimpWs wsResume,pimpWs wsDatasets,pimpWs wsFunctions]
      }
  where
    -- shorthand for easy localizing    
    l :: LocalizedStr -> String
    l = localize lang
    fpa = fpAnalyze fSpec -- TODO: Also count the PHP plugs
    (_,totaalFPgegevensverzamelingen) = dataModelFPA fpa
    (_,totaalFPgebruikerstransacties) = userTransactionFPA fpa
    lang = fsLang fSpec
    wsResume =
      Worksheet { worksheetName =
                            Name $ l (NL "Overzicht", EN "Resume")
                , worksheetTable = Just $ emptyTable
                    { tableRows =
                          [ mkRow [string $ l (NL "Gedetailleerde functiepunentelling (volgens NESMA 2.2) van het systeem "
                                              ,EN "Detailed function point count (according to NESMA 2.1) of the application ")
                                             ++ baseName (getOpts fSpec)
                                  ]
                          , emptyRow
                          , mkRow [string totalen]
                          , mkRow [string gegevensverzamelingen,(number.fromIntegral) totaalFPgegevensverzamelingen]
                          , mkRow [string gebruikerstransacties,(number.fromIntegral) totaalFPgebruikerstransacties]
                          , mkRow [string grandTotaal,(number.fromIntegral) (totaalFPgegevensverzamelingen + totaalFPgebruikerstransacties)]
                          , emptyRow
                          , mkRow [string $ l
                                    (NL "Voor deze telling is aangenomen dat:"
                                    ,EN "For this function point count it is assumed that:")
                                  ]
                          , mkRow [string $ l
                                    (NL "- het een nieuw informatiesyteem betreft."
                                    ,EN "- the application is built from scratch.")
                                  ]
                          , mkRow [string $ l
                                    (NL "- alle informatie wordt intern opgeslagen"
                                    ,EN "- all persistent information will be stored internally")
                                  ]
                          ]
                    }
                }
    wsDatasets =
      Worksheet { worksheetName = Name gegevensverzamelingen
                , worksheetTable = Just $ emptyTable
                    { tableRows =
                          [ mkRow [string gegevensverzamelingen, (number.fromIntegral.length.plugInfos) fSpec]
                          ] ++
                          (map mkRow . mapMaybe showDetailsOfPlug . plugInfos $ fSpec)
                       ++ map mkRow [replicate 3 emptyCell ++ [string totaal, (number.fromIntegral) totaalFPgegevensverzamelingen]]
                    }
                }
    wsFunctions =
      Worksheet { worksheetName  = Name gebruikerstransacties
                , worksheetTable = Just $ emptyTable
                    { tableRows =
                          [ mkRow [string gebruikerstransacties, (number.fromIntegral.length.interfaceS) fSpec]
                          ]
                          ++ map (mkRow.showDetailsOfFunction) (interfaceS fSpec)
                          ++ map mkRow [replicate 4 emptyCell ++ [string totaal, number . fromIntegral . sum . map (fpVal . fpaInterface) . interfaceS $ fSpec]]
                          ++ map mkRow [[string "Gegenereerde interfaces" , number . fromIntegral . length . interfaceG $ fSpec]]
                          ++ map (mkRow.showDetailsOfFunction) (interfaceG fSpec)
                          ++ map mkRow [replicate 4 emptyCell ++ [string totaal, number . fromIntegral . sum . map (fpVal . fpaInterface) . interfaceG $ fSpec]]
                          ++ map mkRow [replicate 5 emptyCell ++ [string grandTotaal, (number.fromIntegral) totaalFPgebruikerstransacties ]]
                    }
                }
    gegevensverzamelingen :: String
    gegevensverzamelingen = l
       (NL "Gegevensverzamelingen"
       ,EN "Data function types")

    gebruikerstransacties :: String
    gebruikerstransacties = l
       (NL "Gebruikerstransacties"
       ,EN "Transactional function types")
    totalen :: String
    totalen = l
       (NL "Totalen:"
       ,EN "Totals:")
    totaal :: String
    totaal =  l
       (NL "Totaal:"
       ,EN "Total:")
    grandTotaal :: String
    grandTotaal =  l
       (NL "Grandtotaal:"
       ,EN "Grand total:")

-- TODO: rewrite: either remove external, BinSQL and ScalarSQL cases or don't filter based on fpaPlugInfo (which only yields TblSql plugs)
    showDetailsOfPlug :: PlugInfo -> Maybe [Cell]
    showDetailsOfPlug plug | Just fpaplgInfo <- fpaPlugInfo plug = Just
       [ emptyCell
       , (string.name) plug
       , string (case plug of
                  InternalPlug _ -> showLang lang fpaplgInfo 
                  ExternalPlug _ -> "???"
                )
       , number . fromIntegral $ fpVal fpaplgInfo 
       , string ( case plug of
                   ExternalPlug{}          -> l ( NL "PHP plugs worden (nog) niet meegerekend!"
                                                , EN "PHP plugs are not (yet) taken into account!")
                   InternalPlug p@TblSQL{} -> l ( NL $ "Tabel met " ++(show . length . attributes) p++" attributen."
                                                , EN $ "Table with "++(show . length . attributes) p++" attributes.")
                   InternalPlug BinSQL{}   -> l ( NL "Koppel tabel"
                                                , EN "Link table")
                )
       ]
    showDetailsOfPlug _ = Nothing
    showDetailsOfFunction :: Interface -> [Cell]
    showDetailsOfFunction ifc =
       [ emptyCell
       , (string.name.ifcObj) ifc
       , string $ showLang lang $ fpaInterface ifc
       , (number . fromIntegral . fpVal . fpaInterface) ifc
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

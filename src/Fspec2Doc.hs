{-# OPTIONS_GHC -Wall #-}
module Fspec2Doc where

   import FspecDef
   import Data.Document
   import Languages
   import Options
   
   fSpec2document :: Fspc -> Options -> Document
   fSpec2document fspc flags = Doc { dflgs = flags
                                   , dtitle = Text title
                                   , dcont = [introduction]
                                   }
     where title :: String
           title = 
               case (language flags) of
                 Dutch   -> "Functionele Specificatie van "++ name fspc
                 English -> "Functional Specification of " ++ name fspc

           introduction :: DSContent
           introduction 
             = Section { dsLevel = 1
                       , dshead = case (language flags) of
                                    Dutch   ->  Text "Inleiding"   
                                    English ->  Text "Introduction"
                       , dscnts 
                          = [Par (
                              [Text "Dit document definieert de servicelaag van een systeem genaamd "]
                           ++ [Text (name fspc) ]
                           ++ [Text ". Het definieert infrastructuur-services in een systeem waarin mensen en applicaties samenwerken "]
                           ++ [Text "om afspraken na te leven die gelden in de context van "]
                           ++ [Text (name fspc) ]
                           ++ [Text ". Deze afspraken worden weergegeven door bedrijfsregels."]
                           ++ [Text "Deze regels staan beschreven in hoofdstuk "]
                           ++ [RefSection  "Ontwerpregels"  ]  
                           ++ [Text ", geordend op thema."]
                           ++ [Text "Een gegevensanalyse volgt in hoofdstuk"] 
                           ++ [RefSection  "Gegevensanalyse"]
                           ++ [Text ". In de daarop volgende hoofdstukken is elk thema"]
                           ++ [Text "uitgewerkt in definities van services."]
                           ++ [Text "Deze services ondersteunen gezamenlijk alle afspraken uit hoofdstuk "]
                           ++ [RefSection "Ontwerpregels"]
                           ++ [Text ". Deze ondersteuning bestaat uit het voorkomen dat een afspraak wordt overtreden, "]
                           ++ [Text "of het signaleren van overtredingen (opdat mensen kunnen ingrijpen)"]
                           ++ [Text "of het herstellen van een regel (door automatische acties op de database uit te voeren)."]
                              )]
                        }
                       
{-# OPTIONS_GHC -Wall #-}
module Fspec2Pandoc (fSpec2Pandoc,render2Pandoc)
where

   import Adl
   import FspecDef
   import Text.Pandoc
   import Version
   import Languages
   import Options
   
   render2Pandoc :: Options -> Pandoc -> String
   render2Pandoc _ pandoc = show pandoc

   fSpec2Pandoc :: Fspc -> Options -> Pandoc
   fSpec2Pandoc fSpec flags = Pandoc meta docContents
       where meta = Meta title authors date
             title = [Str (case (language flags) of
                            Dutch   -> "Functionele Specificatie van "
                            English -> "Functional Specification of "
                          )]
                   ++[Quoted  SingleQuote [Str (name fSpec)] ] 
             authors = [versionbanner]
             date = show(genTime flags)
             
             docContents = (introduction fSpec flags)
                        ++ (designPrinciples fSpec flags)
    
   introduction :: Fspc -> Options ->  [Block]
   introduction fSpec flags = [header] ++ introContents (language flags)
       where 
           header = Header 1 [Str (case (language flags) of
                                     Dutch   ->  "Inleiding"   
                                     English ->  "Introduction"
                                 )]
           introContents Dutch = 
            [Para (
                   [Str "Dit document definieert de ontwerpregels van "]
                ++ [Quoted  SingleQuote [Str (name fSpec)] ]
                ++ [Str ". Deze regels moeten door de oplossing worden nageleefd."]
                ++ [Str "Controle daarop vindt plaats door het architectuurteam."]
                ++ [Str "Tezamen vormen deze regels de architectuur van "]
                ++ [Quoted  SingleQuote [Str (name fSpec)] ]
                ++ [Str "."]
                  )]
           introContents English = 
            [Para (
                   [Str "This document defines the service layer of a system called "]
                ++ [Quoted  SingleQuote [Str (name fSpec)] ]
                ++ [Str "It defines infrastructural services in a system in which people and applications collaborate"]
                ++ [Str "to maintain agreements and commitments that apply to the context of "]
                ++ [Quoted  SingleQuote [Str (name fSpec)] ]
                ++ [Str "These agreements and commitments are represented by rules."]
                ++ [Str "They are presented in chapter "]
                ++ [Link  [Str "Design rules"] ("", "Design rules")]
                ++ [Str ", arranged by theme."]
                ++ [Str "A data analysis is presented in chapter "]
                ++ [Link [Str "Data Analysis"] ("", "Data Analysis")]
                ++ [Str "Subsequent chapters elaborate each theme by defining all applicable services."]
                ++ [Str "Together, these services support all rules from chapter \\ref{chp:Design rules}."]
                ++ [Str "This support consists of either preventing that a rule is violated,"]
                ++ [Str "signalling violations (for human intervention),"]
                ++ [Str "or fixing the content of databases (by automatic actions) to restore a rule."]
                  )]
   
   
   designPrinciples :: Fspc -> Options ->  [Block]
   designPrinciples fSpec flags = [header] ++ dpContents (language flags)
       where 
           header = Header 1 [Str (case (language flags) of
                                     Dutch   ->  "Ontwerpregels"   
                                     English ->  "Design Rules"
                                 )]
           dpContents Dutch = []
--            [Para (
--                   [Str "Dit document definieert de servicelaag van een systeem genaamd "]
--                ++ [Quoted  SingleQuote [Str (name fSpec)] ]
--                ++ [Str ". Het definieert infrastructuur-services in een systeem waarin mensen en applicaties samenwerken "]
--                ++ [Str "om afspraken na te leven die gelden in de context van "]
--                ++ [Quoted  SingleQuote [Str (name fSpec)] ]
--                ++ [Str ". Deze afspraken worden weergegeven door bedrijfsregels. "]
--                ++ [Str "Deze regels staan beschreven in hoofdstuk "]
--                ++ [Link  [Str "Ontwerpregels"] ("", "Ontwerpregels")]  
--                ++ [Str ", geordend op thema. "]
--                ++ [Str "Een gegevensanalyse volgt in hoofdstuk "] 
--                ++ [Link  [Str "Gegevensanalyse"] ("", "Gegevensanalyse")]  
--                ++ [Str ". In de daarop volgende hoofdstukken is elk thema "]
--                ++ [Str "uitgewerkt in definities van services. "]
--                ++ [Str "Deze services ondersteunen gezamenlijk alle afspraken uit hoofdstuk "]
--                ++ [Link  [Str "Ontwerpregels"] ("", "Ontwerpregels")]  
--                ++ [Str ". Deze ondersteuning bestaat uit het voorkomen dat een afspraak wordt overtreden, "]
--                ++ [Str "of het signaleren van overtredingen (opdat mensen kunnen ingrijpen), "]
--                ++ [Str "of het herstellen van een regel (door automatische acties op de database uit te voeren)."]
--                  )]


   
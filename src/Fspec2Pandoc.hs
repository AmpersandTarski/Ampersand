{-# OPTIONS_GHC -Wall #-}
module Fspec2Pandoc (fSpec2Pandoc,render2Pandoc,writeRTF,writeLaTeX)
where

   import Adl
   import FspecDef
   import Text.Pandoc   
     --Als de compiler hierover struikelt, dan moet je pandoc installeren. Dat is overigens in de volgende 3 stappen:
                             -- 1) Eerst installeer je Cabal (zie http://www.haskell.org/cabal/) en dan roep je op je command line: 
                             -- 2) cabal-install pandoc  (onder windows: cabal install pandoc)
                             -- 3) Het kan zijn dat dit nog niet werkt, zie http://groups.google.com/group/pandoc-discuss/browse_thread/thread/a8fc3a627aeec7f2
                             --    als dat het geval is, kan deze module worden overruled in Generators.hs                                 
   import Version        (versionbanner)
   import Languages      (Lang(..))
   import Options        (Options(..),FspecFormat(..))
 
   render2Pandoc :: Options -> Pandoc -> String
   render2Pandoc flags pandoc = case fspecFormat flags of
      FPandoc -> prettyPandoc pandoc
      FWord -> writeRTF defaultWriterOptions pandoc
      FLatex -> writeLaTeX defaultWriterOptions pandoc
      FHtml -> writeHtmlString defaultWriterOptions pandoc
      FUnknown -> prettyPandoc pandoc --REMARK -> will not occur at time of implementation because of user IO error.

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
             
             docContents = (introduction level fSpec flags)
                        ++ (designPrinciples level fSpec flags)
                        ++ (conceptualAnalisys level fSpec flags)
                        ++ (dataAnalisys level fSpec flags)
                        ++ (glossary level fSpec flags)
             level = 1
   
   introduction :: Int -> Fspc -> Options ->  [Block]
   introduction lev fSpec flags = header ++ introContents (language flags)
       where 
           header = labeledHeader lev (case (language flags) of
                                          Dutch   ->  "Inleiding"   
                                          English ->  "Introduction"
                                      )

           introContents Dutch = 
            [Para (
                   [Str "Dit document definieert de servicelaag van een systeem genaamd "]
                ++ [Quoted  SingleQuote [Str (name fSpec)] ]
                ++ [Str ". Het definieert infrastructuur-services in een systeem waarin mensen en applicaties samenwerken "]
                ++ [Str "om afspraken na te leven die gelden in de context van "]
                ++ [Quoted  SingleQuote [Str (name fSpec)] ]
                ++ [Str ". Deze afspraken worden weergegeven door bedrijfsregels. "]
                ++ [Str "Deze regels staan beschreven in hoofdstuk "]
                ++ xrefReference "Ontwerpregels"  
                ++ [Str ", geordend op thema. "]
                ++ [Str "Een gegevensanalyse volgt in hoofdstuk "] 
                ++ xrefReference "Gegevensanalyse"
                ++ [Str ". In de daarop volgende hoofdstukken is elk thema "]
                ++ [Str "uitgewerkt in definities van services. "]
                ++ [Str "Deze services ondersteunen gezamenlijk alle afspraken uit hoofdstuk "]
                ++ xrefReference "Ontwerpregels"
                ++ [Str ". Deze ondersteuning bestaat uit het voorkomen dat een afspraak wordt overtreden, "]
                ++ [Str "of het signaleren van overtredingen (opdat mensen kunnen ingrijpen), "]
                ++ [Str "of het herstellen van een regel (door automatische acties op de database uit te voeren)."]
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
                ++ xrefReference "Design rules"
                ++ [Str ", arranged by theme."]
                ++ [Str "A data analysis is presented in chapter "]
                ++ xrefReference "Data Analysis"
                ++ [Str "Subsequent chapters elaborate each theme by defining all applicable services."]
                ++ [Str "Together, these services support all rules from chapter \\ref{chp:Design rules}."]
                ++ [Str "This support consists of either preventing that a rule is violated,"]
                ++ [Str "signalling violations (for human intervention),"]
                ++ [Str "or fixing the content of databases (by automatic actions) to restore a rule."]
                  )]
   
   
   designPrinciples :: Int -> Fspc -> Options ->  [Block]
   designPrinciples lev fSpec flags = header ++ dpContents
       where 
           header :: [Block]
           header = labeledHeader lev (case (language flags) of
                                          Dutch   ->  "Ontwerpregels"   
                                          English ->  "Design Rules"
                                      )
           dpContents :: [Block]
           dpContents = 
            (case (language flags) of
               Dutch -> [Para (
                           [Str "Dit hoofdstuk definieert de ontwerpregels van "]
                        ++ [Quoted  SingleQuote [Str (name fSpec)] ]
                        ++ [Str ". Deze regels moeten door de oplossing worden nageleefd. "]
                        ++ [Str "Controle daarop vindt plaats door het architectuurteam. "]
                        ++ [Str "Tezamen vormen deze regels de architectuur van "]
                        ++ [Quoted  DoubleQuote [Str (name fSpec)] ]
                        ++ [Str "."]
                               )
                        ]
            ) ++ dpTms (themes fSpec) [][] 
              where
                dpTms :: [Ftheme]        -- This function will take a list of themes still to be processed,
                      -> [Ftheme]        -- and a list of themes that have been processed,
                      -> [Declaration]   -- and a list of declarations already explained,
                      -> [Block]         -- It will give this result.
                dpTms [] _ _ = []
                dpTms (t:tms) tmsProcessed dclsProcessed = 
                     (newBlocks t (1 + length tmsProcessed))
                   ++(dpTms tms 
                           (t:tmsProcessed) 
                           (dclsProcessed ++ (newDcls t)) 
                     )
                newDcls :: Ftheme -> [Declaration]
                newDcls t = []  --TODO  WAAROM? Stef, wat vind jij? Gaan we dit in Adl2Fspec regelen, of moet dit in Fspec2Pandoc? Oftewel, wat willen we in FThemes laten komen? Mijn voorkeur gaat er naar uit dat dit rekenwerk al zichtbaar is in FThemes. Per thema zie ik daar al de lijst van declarations die ik moet behandelen. 
                newBlocks :: Ftheme -> Int -> [Block]
                newBlocks t num = [Header (lev+1) [Str (name t)]]  --TODO  






   conceptualAnalisys :: Int -> Fspc -> Options ->  [Block]
   conceptualAnalisys lev fSpec flags = header ++ caContents
       where 
           header :: [Block]
           header = labeledHeader lev (case (language flags) of
                                          Dutch   ->  "Conceptuele Analyse"   
                                          English ->  "Conceptual Analisys"
                                      )
           caContents :: [Block]
           caContents = 
            (case (language flags) of
               Dutch -> [Para (
                           [Str "Dit hoofdstuk geeft een analyse van de regels uit hoofdstuk "]
                        ++ xrefReference "Ontwerpregels"
                        ++ [Str ". Ieder thema in dat hoofdstuk wordt geanalyseerd in termen van relaties "]
                        ++ [Str "en elke afspraak krijgt een formele representatie. "]
                        ++ [Str "De resultaten van functiepunt analyse staan vermeld in tabel "]
                        ++ xrefReference "tableFPA"
                        ++ [Str "."]
                               )
                        ]
               English -> [Para (
                           [Str "This chapter provides an analysis of the principles described in chapter "]
                        ++ xrefReference "Design rules"
                        ++ [Str ". Each section in that chapter is analysed in terms of relations "]
                        ++ [Str "and each principle is then translated in a rule. "]
                        ++ [Str "The results of function-point analysis is given in table "]
                        ++ xrefReference "tableFPA"
                        ++ [Str "."]
                               )
                        ]
            ) ++ fpa2Blocks lev fSpec flags

           
           
           

   dataAnalisys :: Int -> Fspc -> Options ->  [Block]
   dataAnalisys lev fSpec flags = header ++ daContents
       where 
           header :: [Block]
           header = labeledHeader lev (case (language flags) of
                                          Dutch   ->  "Gegevensanalyse"   
                                          English ->  "Conceptual Analisys"
                                      )
           daContents :: [Block]
           daContents = 
            (case (language flags) of
               Dutch -> [Para (
                           [Str "De keuzes, zoals beschreven in hoofdstuk "]
                        ++ xrefReference "Ontwerpregels"
                        ++ [Str " zijn in een gegevensanalyse vertaald naar het klassediagram in figuur "]
                        ++ xrefReference ("fig:"++baseName flags++"CD")
                        ++ [Str "Dit hoofdstuk geeft een uitwerking van de gegevensanalyse in de vorm van functionele specificaties. "]
                        ++ xrefReference "tableFPA"
                        ++ [Str "."]
                               )
                               
                        ]
            ) ++ [Para [Image [Str "BlaDieBlah"] ( "eenplaatje.png", "TestPlaatje" )]]
              ++ [] --TODO daadwerkelijke gegevensanalyse toevoegen

   glossary :: Int -> Fspc -> Options ->  [Block]
   glossary lev fSpec flags = []  --TODO

   fpa2Blocks :: Int -> Fspc -> Options -> [Block]
   fpa2Blocks lev fSpec flags = []  -- TODO: paragraaf over FPA toevoegen

--   xrefChptReference :: String -> [Inline]
--   xrefChptReference myLabel = [TeX ("\\ref{section:"++myLabel++"}")] --TODO werkt nog niet correct
---   xrefTableReference :: String -> [Inline]
--   xrefTableReference myLabel = [TeX ("\\ref{tab:"++myLabel++"}")]
   labeledHeader :: Int -> String -> [Block]
   labeledHeader lev str =
                    [Header lev ([Str str])]
                 ++ [Para (xrefLabel str)]
    
   xrefReference :: String -> [Inline]    -- uitbreidbaar voor andere rendering dan LaTeX
   xrefReference myLabel = [TeX ("\\ref{"++myLabel++"}")]
   xrefLabel :: String -> [Inline]        -- uitbreidbaar voor andere rendering dan LaTeX
   xrefLabel myLabel = [TeX ("\\label{"++myLabel++"}")]
   

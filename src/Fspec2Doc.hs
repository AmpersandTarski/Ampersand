{-# OPTIONS_GHC -Wall #-}
module Fspec2Doc where

   import FspecDef
   import Rendering.Document
   import Languages
   import Options
   import Adl

   fSpec2document :: Fspc -> Options -> Document
   fSpec2document fSpec flags = Doc { dflgs = flags
                                    , dtitle = Text title
                                    , dcont = [introduction fSpec flags]
                                           ++ [designPrinciples fSpec flags]
                                    }
     where title :: String
           title = 
               case (language flags) of
                 Dutch   -> "Functionele Specificatie van "++ name fSpec
                 English -> "Functional Specification of " ++ name fSpec

   introduction :: Fspc -> Options -> DSContent
   introduction fSpec flags
     = Section { dsLevel = 1
               , dshead = case (language flags) of
                            Dutch   ->  Text "Inleiding"   
                            English ->  Text "Introduction"
               , dscnts = case (language flags) of
                            Dutch   -> dutchIntro
                            English -> englishIntro
                       }
    where
     dutchIntro 
         = [Par (
                  [Text "Dit document definieert de servicelaag van een systeem genaamd "]
               ++ [Text (name fSpec) ]
               ++ [Text ". Het definieert infrastructuur-services in een systeem waarin mensen en applicaties samenwerken "]
               ++ [Text "om afspraken na te leven die gelden in de context van "]
               ++ [Text (name fSpec) ]
               ++ [Text ". Deze afspraken worden weergegeven door bedrijfsregels. "]
               ++ [Text "Deze regels staan beschreven in hoofdstuk "]
               ++ [RefSection  "Ontwerpregels"  ]  
               ++ [Text ", geordend op thema. "]
               ++ [Text "Een gegevensanalyse volgt in hoofdstuk "] 
               ++ [RefSection  "Gegevensanalyse"]
               ++ [Text ". In de daarop volgende hoofdstukken is elk thema "]
               ++ [Text "uitgewerkt in definities van services. "]
               ++ [Text "Deze services ondersteunen gezamenlijk alle afspraken uit hoofdstuk "]
               ++ [RefSection "Ontwerpregels"]
               ++ [Text ". Deze ondersteuning bestaat uit het voorkomen dat een afspraak wordt overtreden, "]
               ++ [Text "of het signaleren van overtredingen (opdat mensen kunnen ingrijpen), "]
               ++ [Text "of het herstellen van een regel (door automatische acties op de database uit te voeren)."]
           )]
     englishIntro = dutchIntro -- for the time being...       

                 
   designPrinciples :: Fspc -> Options -> DSContent
   designPrinciples fSpec flags = dp 
     where  
       strs :: [String]
       strs = case language flags of 
           English -> [  {- str1  -} "Design rules"
                       , {- str2  -} "This chapter discusses the rules that represent agreements between stakeholders about functionality."
                       , {- str3  -} "about"
                       , {- str4  -} "Concept"
                       , {- str5  -} "is defined in definition"
                       , {- str6  -} "on page"
                       , {- str7  -} "This section uses the following concepts:"
                       , {- str8  -} "The following concepts are defined previously in this chapter:"
                       , {- str9  -} "is used in this section, but is not yet defined in this document."
                       , {- str10 -} "Concepts"
                       , {- str11 -} "have not been defined in this document"
                       , {- str12 -} "A conceptual analysis about"
                       , {- str13 -} "is given in figure"
                       , {- str14 -} "Knowledge model of"
                       , {- str15 -} "relation"
                       , {- str16 -} "between"
                       , {- str17 -} "and"
                      ]
           Dutch   -> [  {- str1  -} "Ontwerpregels"
                       , {- str2  -} "Dit hoofdstuk bespreekt verschillende afspraken, die in volgende hoofdstukken zijn uitgewerkt tot een volledige functionele specificatie."
                       , {- str3  -} "over"
                       , {- str4  -} "Concept"
                       , {- str5  -} "is gedefinieerd in definitie"
                       , {- str6  -} "op pg."
                       , {- str7  -} "Deze sectie maakt gebruik van de volgende concepten:"
                       , {- str8  -} "De volgende concepten zijn eerder in dit hoofdstuk gedefinieerd:"
                       , {- str9  -} "wordt in deze sectie gebruikt, maar heeft nog geen definitie in dit document."
                       , {- str10 -} "De concepten"
                       , {- str11 -} "zijn in dit document nog niet gedefinieerd"
                       , {- str12 -} "Een conceptuele analyse over"
                       , {- str13 -} "is weergegeven in figuur"
                       , {- str14 -} "Kennismodel van"
                       , {- str15 -} "relatie"
                       , {- str16 -} "tussen"
                       , {- str17 -} "en"
                      ]
           
       dp = Section { dsLevel = 1
                    , dshead = Text (strs !! 0)
                    , dscnts = [ Par [Text (strs !! 1)]]
                            ++ (patSections [] [] (themes fSpec)) 
                              
                    }
       patSections :: Concepts -> Declarations -> [Ftheme] -> [DSContent]
       patSections _ _ [] = []         
       patSections prevWrittenConcepts prevWrittenDecls (t:ts)
        = Section { dsLevel=2
                  , dshead = Text (strs !! 0 ++ strs !! 2 ++ name t) 
                  , dscnts = [Par [Text ("Inhoud voor pattern "++ name t)]]
                  }
          : (patSections prevWrittenConcepts' prevWrittenDecls' ts)
          where
            prevWrittenConcepts'= prevWrittenConcepts
            prevWrittenDecls'= prevWrittenDecls
--   dp strs en context
--     = ( (chain "\n". filter (not.null))
--         ( [ latexChapter str1 str1
--           , "\t"++str2
--           ] ++
--           patSections [] [] (ctxpats context)
--         )
--       , length [d| d<-declarations context, not (isSignal d)] +    -- bereken het totaal aantal requirements
--         length (rules context++signals context)
--       )
--    where
--     (str1,str2,str3,str4,str5,str6,str7,str8,str9,str10,str11,str12,str13,str14,str15,str16,str17) = strs
--     patSections _ _ [] = []
--     patSections prevWrittenConcepts prevWrittenDecls (pat:pats)
--      = [latexSection (str1++" "++str3++" "++firstCaps (name pat)) (str1++name context++"Pat"++firstCaps (name pat))]++
--        elaborate (rd(newConcsWithDefs>-prevWrittenConcepts))
--                  [d| d<-declarations pat, not (isSignal d)]
--                  (sort' nr (rules pat++signals pat))++
--  -- Zet hierna referenties neer naar alle gebruikte en eerder gedefinieerde concepten (vueConcepts)
--        [ "\t"++if length vueConcepts==1 then str4++" "++latexEmph c++" "++str5++"~\\ref{dfn:"++c++"} "++str6++"~\\pageref{dfn:"++c++"}.\n" else
--          (if null newConcepts
--           then str7
--           else str8)++"\n\t"++
--           en [latexEmph c++ " (def.~\\ref{dfn:"++c++"}, pg.~\\pageref{dfn:"++c++"})"|c<-map (unCap.name) vueConcepts]++".\n"
--        | not (null vueConcepts), c<-[(unCap.name.head) vueConcepts]]++
--        [ "\t"++if length newConcsNoDefs==1 then str4++" "++(latexEmph.unCap.name) c++" "++str9 else
--          str10++" "++en (map (latexEmph.unCap.name) newConcsNoDefs)++" "++str11++".\n"
--        | not (null newConcsNoDefs), c<-[head newConcsNoDefs]]++
--        [ "\t"++str12++" "++firstCaps (name pat)++" "++str12++" \\ref{fig: concAnal"++clname (firstCaps (name pat))++"}.\n"]++
--        [ latexFigureHere (latexCenter ("  \\includegraphics[scale=.4]{"++name context++"_"++clname (name pat)++".png}\n"++
--                                        "  \\caption{"++str14++" "++firstCaps (name pat)++"}\n"++
--                                        "  \\label{fig: concAnal"++clname (firstCaps (name pat))++"}"))]++
--        patSections (prevWrittenConcepts++newConcsWithDefs) (prevWrittenDecls++newDeclarations) pats
--      where
--         patDecls         = decls ++ [d| d<-declarations pat, not (d `elem` decls)] -- tbv de volgorde van declaraties!
--                            where decls = rd [d| r<-rules pat++signals pat, d<-declarations r]
--         patConcepts      = concpts ++ [c| c<-concs pat, not (c `elem` concpts)] -- tbv de volgorde van concepten!
--                            where concpts = rd [c| d<-patDecls, c<-concs d]
--         vueConcepts      = [c| c<-patConcepts,      c `elem` prevWrittenConcepts ]
--         newConcepts      = [c| c<-patConcepts, not (c `elem` prevWrittenConcepts)]
--         newConcsWithDefs = [c| c<-newConcepts,      name c `elem` map name (conceptDefs context) ]
--         newConcsNoDefs   = [c| c<-newConcepts, not (name c `elem` map name (conceptDefs context))]
--         newDeclarations  = [d| d<-patDecls, not (d `elem` prevWrittenDecls)]
--         explDecl :: Declaration -> [String]
--         explDecl d = [ latexDesignrule (latexWord (str ++ " ("++str15++": "++name d++signature++")"))  | not (null str)]
--          where
--           str = explainDecl context Dutch d
--           signature | length [s|s<-patDecls, name s==name d] >1  = " "++str16++" "++name (source d)++" "++str17++" "++name (target d)
--                     | otherwise                                 = ""
--         elaborate :: Concepts ->Declarations -> Rules -> [String]
--         elaborate tobeWrittenConcs tobeWrittenDecls []
--          = map (explainConcept context Dutch) tobeWrittenConcs ++
--            concat (map explDecl tobeWrittenDecls)
--         elaborate tobeWrittenConcs tobeWrittenDecls (r:rs)
--          = map (explainConcept context Dutch) cConcs ++
--            concat (map explDecl cDecls) ++
--            [latexDesignrule (latexWord str)|str<-[explainRule context Dutch r], not (null str)]++
--            elaborate (tobeWrittenConcs>-cConcs) (tobeWrittenDecls>-cDecls) rs
--          where cConcs     = [c| c<-tobeWrittenConcs, c `elem` concs r ]
--                cDecls     = [d| d<-tobeWrittenDecls, d `elem` declarations r ]
                       
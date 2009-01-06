  module Graphic  ( dotGraph
                  , processDotgraphFile
                  , Graphic
                  , genGraphics
                  , processCdDataModelFile)
  where
   import Char (isSpace)
   import CommonClasses ( Identified(name) )
   import Collection (Collection(rd))
   import Auxiliaries (chain, eqClass, sort)
   import ADLdef ( Context,Pattern(..),ctxpats
                 , Rule(Ru),sign
                 , Expression(..)
                 , isMph, isSignal, makeDeclaration
                 , flp, inline, posNone
                 , source, target
                 , multiplicities
                 , union
                 , Morphism(..)
                 , concs
                 , declarations
                 , mors
                 , specs
                 )
   import CC_aux (order,isProperty,tot,inj,sur,fun)
   import Typology ( Inheritance(Isa) )
   import HtmlFilenames (fnConcept,fnRelation)
   import System (system, ExitCode(ExitSuccess,ExitFailure))
   import System.IO.Unsafe (unsafePerformIO) -- maakt het aanroepen van neato vanuit Haskell mogelijk.

   class Graphic a where
    dotGraph :: Context -> String -> String -> a -> String

   instance Graphic Pattern where
    dotGraph context style nm pat
     = "digraph "++show [x|x<-nm,not(isSpace x)]++introG
             -- Eerst de concepten definieren als nodes:
             ++ chain "" [if style=="crowfoot" then concs2box c else concs2bol c |c<-cpts]
             -- Vervolgens alle relaties definieren als verbindingen tussen die nodes:
             ++ chain ""
                  (
                   [ morf2dot m | cl<-eqClass order arcs, m <-take 1 (sort cl)]++  -- Let op: rare resultaten kunnen ontstaan door cyclische ISA relaties. Wie voorkomt dat source m <=source m' en target m'<=target m binnen één equivalentieklasse?
             -- and finally the ISA relations:
                   [ "\n   ; "++quote(conceptname c)++" -> "++quote(conceptname p)++" [arrowhead=onormal,arrowsize=1.2]"
                   | (p,c)<-isas ]
  --                 , not ((p,c) `elem` [(g,c)| (g,p)<-isas, (p',c)<-isas, p==p'])]   -- remove isa pairs that are redundant because of transitivity.
                   )++
                 "\n   }"
       where
         cpts = rd(concs pat++concs (declarations (mors pat))) -- SJ: 2007/9/14: when ISA's are treated as first class relations, remove the concs (declarations pat)
         isas = [(p,c)| p<-cpts, c<-cpts, p<c ]
         arcs = rd ([m'|m<-mors pat++mors(specs pat), isMph m, not (isSignal (makeDeclaration m)), not (isProperty m)
                    {- , take 5(name m)/="Clos_"  -}
                       , m'<-[m,flp m], inline m']++
                    [Mph (name d) posNone [source d,target d] (source d,target d) True d
                       | d<-declarations pat, not (isSignal d), not (isProperty d)])
         introG = "\n   { bgcolor=transparent"++(if style=="crowfoot" then " ; overlap=false ; splines=true\n" else "\n")
               ++ "   ; node [shape=plaintext,fontsize=12,font=helvetica]\n"
               ++ "   ; edge [len="++(if style=="crowfoot" then "2.0" else "1.2")++",fontsize=12,arrowsize=0.8]"
 








         --- Eerst nog even wat hulpfuncties. Die horen overigens waarschijnlijk niet hier...
         edgearrow = " -> "
         newline = "\n   ; "
         quote s = "\"" ++ s ++ "\" "
         --- Enkele stijldefinities van Dot:
         onzichtbaarlijntje percent = " [style=invis,len=" ++ show(0.2 * percent / 100) ++ "] "
         bolletje = " [shape=point,style=filled,color=black,width=0.2]"
         doosje c       = " [shape=box" ++
                          ", href=" ++ quote (fnConcept context c++".html")++
                          ", title=" ++ quote (name c)  ++
                          "]"
         onzichtbaarpuntje    = " [shape=point,color=transparent] "
 
         conceptlabel c = " [shape=plaintext,style=bold" ++
                          ", fontsize=12,font=helvetica" ++
                          ", label=" ++ quote (name c)  ++
                          ", href=" ++ quote (fnConcept context c++".html")++
                          ", title=" ++ quote (name c)  ++
                          "]"
 
         relatielabel m = "shape=plaintext" ++
                          ", fontsize=12,font=helvetica" ++
                          ", label=" ++ quote (name m) ++
                          ", href=" ++ quote (fnRelation context m++".html")++
                          ", title=" ++ quote (name m)
 
         concs2bol c = newline ++ -- > omzetten van een concept naar een bolletje, met daarnaast de naam (aanklikbaar)
                       quote(nodename1 c) ++ bolletje ++ newline ++
                       quote(nodename2 c) ++ conceptlabel c ++ newline ++
                       quote(nodename1 c) ++ edgearrow ++ quote(nodename2 c) ++
                                 onzichtbaarlijntje 100
           where
                 nodename1 c  = conceptname c
                 nodename2 c  = conceptname c ++ "*2*"

         concs2box c = newline ++ -- > omzetten van een concept naar een doosje, met daarin de naam (aanklikbaar)
                       quote(conceptname c) ++ doosje c
















         morf2dot m | source m == target m = newline ++
                                             point 1 px   ++ newline ++
                                             point 2 py   ++ newline ++
                                             point 2 pz   ++ newline ++
                                             line 1 pa py "0.7" "" ++ newline ++
                                             line 2 py px "0.5" "" ++ newline ++
                                             line 3 px pz "0.5" "" ++ newline ++
                                             line 4 pz pb "0.7" ""
                    | style=="crowfoot"    = newline ++ line 0 pa pb "2.7" (relatielabel (makeDeclaration m))
                    | otherwise            = newline ++
                                             point 1 px   ++ newline ++
                                             line 5 pa px "1.3" "" ++ newline ++
                                             line 6 px pb "1.3" ""
              where
                 --- Definities van de namen van de diverse punten:
                 s  = source (makeDeclaration m)
                 t  = target (makeDeclaration m)
                 pa = name s
                 pb = name t
                 px = name m ++ name s ++ name t
                 py = name m ++ name s ++ name t ++ "*y*"
                 pz = name m ++ name s ++ name t ++ "*z*"
                 --- Definities van de diverse soorten styles voor punten:
                 point 1 p =  quote p ++ onzichtbaarpuntje ++ newline ++
                              quote (p++"*x*") ++ " [" ++ relatielabel (makeDeclaration m) ++ "]" ++ newline ++
                              quote p ++ edgearrow ++ quote (p++"*x*") ++ onzichtbaarlijntje 50
                 point 2 p =  quote p ++ onzichtbaarpuntje
                 line s p1 p2 len lbl = quote p1 ++ edgearrow ++ quote p2 ++ linestyle s len lbl
                 --- De verschillende lijnstukken zijn nu genummerd wat stijl betreft. Hieronder
                 --- wordt per soort stijl de toeters en bellen aangebracht. (en testkleurtjes...)
                 linestyle s len lbl | s == 1    = constructstyle len (defcolor "red")    lbl geen staart
                                     | s == 2    = constructstyle len (defcolor "green")  lbl pijl geen
                                     | s == 3    = constructstyle len (defcolor "blue")   lbl geen lijp
                                     | s == 4    = constructstyle len (defcolor "orange") lbl kop geen
                                     | s == 5    = constructstyle len (defcolor "yellow") lbl pijl staart
                                     | s == 6    = constructstyle len (defcolor "purple") lbl kop lijp
                                     | otherwise = constructstyle len (defcolor "black")  lbl kop staart -- default: voor ongebroken pijlen
                    where
                      kop    | style=="crowfoot" = dotcrowfootnotation (uni (multiplicities m)) (tot (multiplicities m))
                             | otherwise         = ccnotation          (uni (multiplicities m)) (tot (multiplicities m))
                      staart | style=="crowfoot" = dotcrowfootnotation (inj (multiplicities m)) (sur (multiplicities m))
                             | otherwise         = ccnotation          (inj (multiplicities m)) (sur (multiplicities m))
                      pijl   | style=="crowfoot" = "none" --  was "normal"
                             | otherwise         = if      uni (multiplicities m) && tot (multiplicities m)  &&
                                                      not (inj (multiplicities m) && sur (multiplicities m))
                                                   then "none" else "onormal"
                      lijp   | style=="crowfoot" = "none" --  was "inv"
                             | otherwise         = "none"
                      constructstyle len col lbl dothead dottail
                                          = "[color=" ++ col ++
                                           ", len="++len ++
                                           (if null lbl then "" else ", "++lbl)++
                                           ", arrowhead=\"" ++ dothead ++ "\"" ++
                                           ", arrowtail=\"" ++ dottail ++ "\"" ++
                                           "]"
                      defcolor testcolor = "black" -- testcolor
                      geen   = "none"

         conceptname c = name c
         dotcrowfootnotation a b = ((if a then "tee" else "crow") ++ (if b then "tee" else "odot"))
         ccnotation True True    = "normal"
         ccnotation _ _          = "none"
 
         uni stefMagHierHetCommentaarVoorSchrijven = fun stefMagHierHetCommentaarVoorSchrijven

   instance Graphic Context where
    dotGraph context style nm context' 
     = dotGraph context style nm (foldr union (Pat "" [] [] [] [] []) (ctxpats context'))

   instance Graphic Morphism where
    dotGraph context style nm m -- @(Mph nm' _ atts sgn yin m')
     = dotGraph context style nm (Pat nm [Ru 'E' (F [Tm m]) posNone (F []) [] "" (sign m) 0 ""] [] [] [] [])

   processDotgraphFile     fnm = genGraphics fnm "neato"
   processCdDataModelFile  fnm = genGraphics fnm "dot"

   genGraphics fnm graphvizexecnm =
      do putStrLn ("Processing "++fnm++".dot ... :")
         result <- system (graphvizexecnm ++ " -Tpng "++fnm++".dot"++" -o "++fnm++".png")
         case result of
             ExitSuccess   -> putStrLn ("  "++fnm++".png created.")
             ExitFailure x -> putStrLn $ "Failure: " ++ show x
         result <- system (graphvizexecnm ++ " -Tgif "++fnm++".dot"++" -o "++fnm++".gif")
         case result of
             ExitSuccess   -> putStrLn ("  "++fnm++".gif created.")
             ExitFailure x -> putStrLn $ "Failure: " ++ show x
         result <- system (graphvizexecnm ++ " -Tcmapx "++fnm++".dot"++" -o "++fnm++".map")
         case result of
             ExitSuccess   -> putStrLn ("  "++fnm++".map created.")
             ExitFailure x -> putStrLn $ "Failure: " ++ show x

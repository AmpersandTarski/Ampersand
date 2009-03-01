
module Classes.Graphics where


   import Data.GraphViz
--     --Als de compiler hierover struikelt, dan moet je graphviz installeren. Dat is overigens in de volgende 3 stappen:
--                             -- 1) Eerst installeer je Cabal (zie http://www.haskell.org/cabal/) en dan roep je op je command line: 
--                             -- 2) cabal-install graphviz  (onder windows: cabal install graphviz)
--                             -- 3) er is geen stap 3!
   import Adl
   import FspecDef
   import Options
   import Collection (Collection(rd))
   import CC_aux (order,isProperty,tot,inj,sur,fun)
   import HtmlFilenames
   
   class Dotable a where
      toDot :: Fspc -> Options -> a -> DotGraph

   instance Dotable Pattern where
      toDot fspc flags pat 
        = DotGraph { graphAttributes = [BgColor transparent]
                                    ++ [Overlap False | crowfoot flags]
                                    ++ [Splines (Just True)  | crowfoot flags]
                   , graphNodes = conceptNodes 
                               ++ inBetweenNodes
                   , graphEdges = []
                   , directedGraph = True
                   }
            where nodeAtts   = [FontSize 12]
                            ++ [FontName "helvetica"]
                  edgeAtts   = [Len (if crowfoot flags then 2.0 else 1.2)]
                            ++ [FontSize 12]
                            ++ [ArrowSize 0.8]
                  
                  cpts = rd(concs pat++concs (declarations (mors pat))) -- SJ: 2007/9/14: when ISA's are treated as first class relations, remove the concs (declarations pat)
                  isas = [(p,c)| p<-cpts, c<-cpts, p<c ]
                  arcs = rd ([m'|m<-mors pat++mors(specs pat), isMph m, not (isSignal (makeDeclaration m)), not (isProperty m)
                                    {- , take 5(name m)/="Clos_"  -}
                                       , m'<-[m,flp m], inline m']++
                                    [Mph (name d) posNone [source d,target d] (source d,target d) True d
                                       | d<-declarations pat, not (isSignal d), not (isProperty d)])
                  conceptTable = case cpts of
                                   []   -> []
                                   c:cs -> zip cpts [1..(length cpts)]
                  conceptNodes = map conceptNode cpts                                    
                  conceptNode c = DotNode { nodeID         = case lookup c conceptTable of
                                                                 Just i -> i
                                                                 _ -> undefined  --kan niet gebeuren.
                                          , nodeAttributes = [Label (name c)]
                                                          ++ nodeAtts 
                                                          ++ if crowfoot flags then doosje flags c else bolletje
                                          }
                  arcsTable = case arcs of
                                 []    -> []
                                 c:cs  -> zip arcs [(length conceptTable + 1)..(length conceptTable + 1 + (length arcs))]                          
                  inBetweenNodes = map inBetweenNode arcs
                  inBetweenNode m = DotNode { nodeID          = case lookup m arcsTable of
                                                                  Just i -> i
                                                                  _ -> undefined  --kan niet gebeuren.
                                            , nodeAttributes = [Label (name m)]
                                            }


   doosje flags c = [Shape BoxShape]
                 ++ [Unknown "href" (htmlFileUrl flags c)]
                 ++ [Unknown "title" (show (name c))]
   bolletje = [Shape PointShape]
           ++ [Style Filled]
           ++ [Color black]
           ++ [Width 0.2]
   onzichtbaarpuntje = [Shape PointShape]
                     ++[Color transparent]



   transparent = RGBA 0 0 0 0 
   black       = RGB 255 255 255   
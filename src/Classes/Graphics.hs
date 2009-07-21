
module Classes.Graphics where
-- TODO Deze module is nog onderHANden
   --import Data.GraphViz.Commands
   import Data.GraphViz
--     --Als de compiler hierover struikelt, dan moet je graphviz installeren. Dat is overigens in de volgende 3 stappen:
--                             -- 1) Eerst installeer je Cabal (zie http://www.haskell.org/cabal/) en dan roep je op je command line: 
--                             -- 2) cabal-install graphviz  (onder windows: cabal install graphviz)
--                             -- 3) er is geen stap 3!
   import Adl
   import FspecDef
   import Options
   import Collection (Collection(rd))
   import CC_aux (isProperty,tot,inj,sur,fun)
   --import HtmlFilenames
   --
   class Dotable a where
      toDot :: Fspc -> Options -> a -> DotGraph

   instance Dotable Pattern where
      toDot fspc flags pat 
        = DotGraph { graphAttributes = [BgColor transparent]
                                    ++ [Overlap (Right False) | crowfoot flags]
                                    ++ [Splines (Left True)  | crowfoot flags]
                   , graphNodes = [conceptNode c | c<-cpts] 
                               ++ [inBetweenNode d | d<-arcs]
                   , graphEdges = [x | d<-arcs, x<-decledges d] ++ isaedges
                   , directedGraph = True
                   }
            where 
            nodeAtts   = [FontSize 12]
                      ++ [FontName "helvetica"]
            edgeAtts   = [Len (if crowfoot flags then 2.0 else 1.2)]
                      ++ [FontSize 12]
                      ++ [ArrowSize 0.8]
              
            cpts = rd (concs pat)
            arcs = rd $ [d | d<-[makeDeclaration m|m<-mors pat++mors(specs pat), isMph m, not (isProperty m)]
                           , not (isSignal d)]
                        ++ (ptdcs pat)
            conceptTable = case cpts of
               []   -> []
               c:cs -> zip cpts [1..]                                  
            conceptNode c = DotNode { nodeID         = lkup c conceptTable
                                    , nodeAttributes = [Label (Left$name c)]
                                                    ++ nodeAtts 
                                                    ++ if crowfoot flags then doosje flags c else bolletje
                                    }
            arcsTable = case arcs of
               []    -> []
               c:cs  -> zip arcs [(length conceptTable + 1)..]    
            inBetweenNode d = DotNode { nodeID          = lkup d arcsTable
                                      , nodeAttributes = [Label (Left$name d)]
                                      }
            decledges d = [DotEdge --attach source
                            {edgeHeadNodeID = lkup (source d) conceptTable
                            ,edgeTailNodeID = lkup d arcsTable
                            ,edgeAttributes = edgeAtts
                            ,directedEdge = True}
                          ,DotEdge --attach target
                            {edgeHeadNodeID = lkup d arcsTable
                            ,edgeTailNodeID = lkup (target d) conceptTable
                            ,edgeAttributes = edgeAtts
                            ,directedEdge = True}]
            isaedges = [ DotEdge
                          {edgeHeadNodeID = lkup (genspc g) conceptTable
                          ,edgeTailNodeID = lkup (gengen g) conceptTable
                          ,edgeAttributes = edgeAtts
                          ,directedEdge = True}
                       | g<-ptgns pat ]
            lkup x tbl = case lookup x tbl of
                 Just i -> i
                 _ -> error "element not found." --TODO


   doosje flags c = [Shape BoxShape]
                -- ++ [Unknown "href" (htmlFileUrl flags c)]
                 ++ [Label (Left$show (name c))]
   bolletje = [Shape PointShape]
           ++ [Style$Stl Filled Nothing]
           ++ [Color$Left black]
           ++ [Width 0.2]
   onzichtbaarpuntje = [Shape PointShape]
                     ++[Color$Left transparent]



   transparent = RGBA 0 0 0 0 
   black       = RGB 0 0 0   

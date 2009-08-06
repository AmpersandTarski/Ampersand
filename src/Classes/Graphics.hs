--TODO -> Alles uitgezet vanwege nieuwe versie graphviz
module Classes.Graphics where
-- TODO Deze module is nog onderHANden
-- TODO crowfoot flag (implement setCrowfoot, see below)
-- TODO url links for atlas
 
   --import Data.GraphViz.Commands
import Data.GraphViz
--     --Als de compiler hierover struikelt, dan moet je graphviz installeren. Dat is overigens in de volgende 3 stappen:
--                             -- 1) Eerst installeer je Cabal (zie http://www.haskell.org/cabal/) en dan roep je op je command line: 
--                             -- 2) cabal-install graphviz  (onder windows: cabal install graphviz)
--                             -- 3) er is geen stap 3!
--                             -- 4) build on graphviz-2999.1.0.2
import Adl
import FspecDef
import Options
import Collection (Collection(rd))
import CC_aux (isProperty,tot,inj,sur,fun)

class Dotable a where
   toDot :: Fspc -> Options -> a -> DotGraph

--DESCR -> a picture consists of arcs (relations), concepts, and ISA relations between concepts
--         arcs are attached to a source or target concept
--         arcs and concepts are points attached to a label
data PictureObject = ArcSrcEdge | ArcTgtEdge | ArcPoint | ArcLabel | ArcEdge
                     | CptPoint | CptLabel | CptEdge
                     | IsaEdge
                     | TotalPicture
instance Dotable Pattern where
   toDot fspc flags pat 
     = DotGraph { graphAttributes = setCrowfoot TotalPicture [Splines SplineEdges] -- [BgColor transparent]
                , graphNodes = [conceptLabel c | c<-cpts]
                            ++ [conceptPoint c | c<-cpts] 
                            ++ [inBetweenNode d | d<-arcs]
                            ++ [inBetweenPoint d | d<-arcs]
                , graphEdges = [x | d<-arcs, x<-decledges d] 
                            ++ isaedges
                            ++ [conceptEdge c | c<-cpts] 
                            ++ [inBetweenEdge d | d<-arcs]
                , directedGraph = True
                , strictGraph = False
                , graphID = Nothing
                }
         where 
         --DESCR -> default Node attributes
         nodeAtts   = [FontSize 12,FontName "helvetica"]
         --DESCR -> default Edge attributes
         edgeAtts   = [Len 1.2,ArrowSize 0.8]
         --DESCR -> get concepts and arcs from pattern
         cpts = rd (concs pat)
         arcs = rd $ [d | d<-[makeDeclaration m|m<-mors pat++mors(specs pat), isMph m, not (isProperty m)]
                        , not (isSignal d)]
                     ++ (ptdcs pat)
         --DESCR -> assign ID to concept related nodes
         conceptTable = case cpts of
            []   -> []
            c:cs -> zip cpts [1..] 
         conceptPointTable = case cpts of
            []   -> []
            c:cs -> zip cpts [(length conceptTable + 1)..]             
         --DESCR -> construct concept point related picture objects                                
         conceptLabel c = 
            DotNode { nodeID         = lkup c conceptTable
                    , nodeAttributes = setCrowfoot CptLabel$
                                      [Label$StrLabel (name c),Shape Plaintext,Style$Stl Filled Nothing
                                      ,LabelURL$UStr{urlString="CPT_" ++ (name c) ++ ".html"}]
                                    ++ nodeAtts 
                    }
         conceptPoint c = 
            DotNode { nodeID = lkup c conceptPointTable
                    , nodeAttributes = setCrowfoot CptPoint [Shape PointShape,Style$Stl Filled Nothing,Width 0.1]
                    }
         conceptEdge c  = 
            DotEdge {edgeHeadNodeID = lkup c conceptTable
                    ,edgeTailNodeID = lkup c conceptPointTable
                    ,edgeAttributes = setCrowfoot CptEdge [Len 0.4, Style$Stl Invisible Nothing] --Style$Stl Dotted Nothing]
                    ,directedEdge = True
                    }
         --DESCR -> assign ID to arc related nodes
         arcsTable = case arcs of
            []    -> []
            c:cs  -> zip arcs [(length conceptTable + length conceptPointTable + 1)..]   
         arcsPointTable = case arcs of
            []    -> []
            c:cs  -> zip arcs [(length conceptTable + length conceptPointTable + length arcsTable + 1)..]            
         --DESCR -> construct arc point related picture objects   
         inBetweenNode d = DotNode { nodeID          = lkup d arcsTable
                                   , nodeAttributes = setCrowfoot ArcLabel$
                                                      [Label$StrLabel (name d), Shape Plaintext]
                                                      ++ nodeAtts
                                   }   
         inBetweenPoint d = 
            DotNode { nodeID = lkup d arcsPointTable
                    , nodeAttributes = setCrowfoot ArcPoint [Shape PointShape,Style$Stl Invisible Nothing,Width 0.1]
                    }
         inBetweenEdge d  = 
            DotEdge {edgeHeadNodeID = lkup d arcsTable
                    ,edgeTailNodeID = lkup d arcsPointTable
                    ,edgeAttributes = setCrowfoot ArcEdge [Len 0.1, Style$Stl Invisible Nothing] --Style$Stl Dotted Nothing]
                    ,directedEdge = True
                    }          
         --DESCR -> construct arc edges
         decledges d = [DotEdge --attach source
                         {edgeHeadNodeID = lkup (source d) conceptPointTable
                         ,edgeTailNodeID = lkup d arcsPointTable
                         ,edgeAttributes = setCrowfoot ArcSrcEdge$ edgeAtts ++ [ArrowHead Open, ArrowTail NoArrow]
                         ,directedEdge = True}
                       ,DotEdge --attach target
                         {edgeHeadNodeID = lkup d arcsPointTable
                         ,edgeTailNodeID = lkup (target d) conceptPointTable
                         ,edgeAttributes = setCrowfoot ArcTgtEdge$ edgeAtts ++ [ArrowHead NoArrow, ArrowTail NoArrow]
                         ,directedEdge = True}]          
         --DESCR -> construct isa edges 
         isaedges = [ DotEdge
                       {edgeHeadNodeID = lkup (genspc g) conceptPointTable
                       ,edgeTailNodeID = lkup (gengen g) conceptPointTable
                       ,edgeAttributes = setCrowfoot IsaEdge edgeAtts
                       ,directedEdge = True}
                    | g<-ptgns pat ]
         --DESCR -> changes the set of attributes on a certain type of picture object to crowfoot format
         setCrowfoot :: PictureObject -> [Attribute] -> [Attribute]
         setCrowfoot po atts = 
            if crowfoot flags 
            then case po of
              ArcSrcEdge -> atts --used to be something like: Len (if crowfoot flags then 2.0 else 1.2)
              ArcTgtEdge -> atts --used to be something like: Len (if crowfoot flags then 2.0 else 1.2)
              ArcPoint -> atts
              ArcLabel -> atts
              ArcEdge -> atts
              CptPoint -> atts --used to be something like: if crowfoot flags then doosje flags c else bolletje
              CptLabel -> atts
              CptEdge -> atts
              IsaEdge -> atts
              TotalPicture -> atts
                              --   ++ [Overlap (Right False) | crowfoot flags]
                              --   ++ [Splines (Left True)  | crowfoot flags]
            else atts

--DESCR -> lookup the integer id of an object
lkup :: (Eq a) => a -> [(a,Int)] -> Int
lkup x tbl = case lookup x tbl of
   Just i -> i
   _ -> error "element not found." --TODO

 --  doosje flags c = [Shape BoxShape]
   --              ++ [Label$StrLabel (show (name c))]


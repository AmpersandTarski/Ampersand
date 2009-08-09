{-# OPTIONS_GHC -Wall #-}
--TODO -> Alles uitgezet vanwege nieuwe versie graphviz
module Classes.Graphics (Dotable(toDot)) where
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
data PictureObject = ArcSrcEdge Declaration 
                   | ArcTgtEdge Declaration
                   | ArcPoint 
                   | ArcLabel Declaration
                   | ArcEdge 
                   | CptPoint Concept
                   | CptLabel Concept
                   | CptEdge
                   | IsaEdge 
                   | TotalPicture
instance Dotable Pattern where
   toDot _ flags pat 
     = DotGraph { graphAttributes = handleFlags TotalPicture 
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
         arcs = rd $ [d | d<-[makeDeclaration mph|mph<-mors pat++mors(specs pat), isMph mph, not (isProperty mph)]
                        , not (isSignal d)]
                     ++ (ptdcs pat)
         --DESCR -> assign ID to concept related nodes
         conceptTable = case cpts of
            [] -> []
            _  -> zip cpts [1..] 
         conceptPointTable = case cpts of
            [] -> []
            _  -> zip cpts [(length conceptTable + 1)..]             
         --DESCR -> construct concept point related picture objects                                
         conceptLabel c = 
            DotNode { nodeID         = lkup c conceptTable
                    , nodeAttributes = handleFlags (CptLabel c)
                    }
         conceptPoint c = 
            DotNode { nodeID = lkup c conceptPointTable
                    , nodeAttributes = handleFlags (CptPoint c) 
                    }
         conceptEdge c  = 
            DotEdge {edgeHeadNodeID = lkup c conceptTable
                    ,edgeTailNodeID = lkup c conceptPointTable
                    ,edgeAttributes = handleFlags CptEdge 
                    ,directedEdge = True
                    }
         --DESCR -> assign ID to arc related nodes
         arcsTable = case arcs of
            [] -> []
            _  -> zip arcs [(length conceptTable + length conceptPointTable + 1)..]   
         arcsPointTable = case arcs of
            [] -> []
            _  -> zip arcs [(length conceptTable + length conceptPointTable + length arcsTable + 1)..]            
         --DESCR -> construct arc point related picture objects   
         inBetweenNode d = DotNode { nodeID          = lkup d arcsTable
                                   , nodeAttributes = handleFlags (ArcLabel d)
                                   }   
         inBetweenPoint d = 
            DotNode { nodeID = lkup d arcsPointTable
                    , nodeAttributes = handleFlags ArcPoint
                    }
         inBetweenEdge d  = 
            DotEdge {edgeHeadNodeID = lkup d arcsTable
                    ,edgeTailNodeID = lkup d arcsPointTable
                    ,edgeAttributes = handleFlags ArcEdge  
                    ,directedEdge = True
                    }          
         --DESCR -> construct arc edges
         decledges d = [DotEdge --attach source
                         {edgeHeadNodeID = lkup (source d) conceptPointTable
                         ,edgeTailNodeID = lkup d arcsPointTable
                         ,edgeAttributes = handleFlags (ArcSrcEdge d) 
                         ,directedEdge = True}
                       ,DotEdge --attach target
                         {edgeHeadNodeID = lkup d arcsPointTable
                         ,edgeTailNodeID = lkup (target d) conceptPointTable
                         ,edgeAttributes = handleFlags (ArcTgtEdge d)
                         ,directedEdge = True}]          
         --DESCR -> construct isa edges 
         isaedges = [ DotEdge
                       {edgeHeadNodeID = lkup (genspc g) conceptPointTable
                       ,edgeTailNodeID = lkup (gengen g) conceptPointTable
                       ,edgeAttributes = handleFlags (IsaEdge )
                       ,directedEdge = True}
                    | g<-ptgns pat ]
         --DESCR -> Take care of the crowfoot flag 
         --construct the set of attributes on a certain type of picture object based on flag settings (currently only crowfoot)
         handleFlags :: PictureObject  -> [Attribute]
         handleFlags po = 
            case po of
              ArcSrcEdge d -> edgeAtts ++
                              if crowfoot flags
                              then  [ArrowHead Empty] ++
                                    [ArrowTail (crowfootArrow (inj (multiplicities d)) 
                                                              (sur (multiplicities d)))]
                              else  [ArrowHead Open] ++
                                    [ArrowTail NoArrow]
              ArcTgtEdge d -> edgeAtts ++
                              if crowfoot flags
                              then  [ArrowHead (crowfootArrow (fun (multiplicities d)) 
                                                              (tot (multiplicities d)))] ++
                                    [ArrowTail NoArrow] 
                              else  [ArrowHead NoArrow] ++
                                    [ArrowTail NoArrow]
              ArcPoint   -> [Shape PointShape,Style$Stl Invisible Nothing,Width 0.1]
              ArcLabel d -> nodeAtts ++ [Label$StrLabel (name d), Shape Plaintext]
              ArcEdge    -> [Len 0.1, Style$Stl Invisible Nothing] --Style$Stl Dotted Nothing]
              CptPoint c -> if crowfoot flags
                            then nodeAtts ++ [Label$StrLabel (name c),Shape Plaintext,Style$Stl Filled Nothing
                                      ,LabelURL$UStr{urlString="CPT_" ++ (name c) ++ ".html"}]
                            else [Shape PointShape,Style$Stl Filled Nothing,Width 0.1] --used to be something like: if crowfoot flags then doosje flags c else bolletje
              CptLabel c -> if crowfoot flags
                            then [Shape PointShape,Style$Stl Invisible Nothing,Width 0.1]
                            else nodeAtts ++ [Label$StrLabel (name c),Shape Plaintext,Style$Stl Filled Nothing
                                      ,LabelURL$UStr{urlString="CPT_" ++ (name c) ++ ".html"}]
              CptEdge    -> [Len 0.4, Style$Stl Invisible Nothing] --Style$Stl Dotted Nothing]
              IsaEdge    -> edgeAtts 
              TotalPicture -> [Splines SplineEdges] -- [BgColor transparent]
                              --   ++ [Overlap (Right False) | crowfoot flags]
                              --   ++ [Splines (Left True)  | crowfoot flags]
--The current implementation of the Graphviz library (version of Graphviz of 25th of July 2009)
--does not support the combinators for arrowheads. For the time being, I just use different
--symbols for different cardinalities. Whenever the Graphviz library will support the needed arrowheads,
--it is very easy to use these symbols.
         crowfootArrow :: Bool -> Bool -> ArrowType
         crowfootArrow a b = if a then
                                  if b then Box
                                       else Tee
                                  else
                                  if b then Diamond
                                       else Crow

--DESCR -> lookup the integer id of an object
lkup :: (Eq a) => a -> [(a,Int)] -> Int
lkup x tbl = case lookup x tbl of
   Just i -> i
   _ -> error "element not found." --TODO

 --  doosje flags c = [Shape BoxShape]
   --              ++ [Label$StrLabel (show (name c))]


-- hulpfuncties, voor tijdelijk. TODO, opschonen
--orange :: Attribute
--orange = Color [ColorName "orange"]
--yellow :: Attribute
--yellow = Color [ColorName "yellow"]
--purple :: Attribute
--purple = Color [ColorName "purple"]
--cyan :: Attribute
--cyan = Color [ColorName "cyan"]
--brown :: Attribute
--brown = Color [ColorName "brown"]
--cgreen :: Attribute
--cgreen = Color [ColorName "green"]
--cred :: Attribute
--cred = Color [ColorName "red"]
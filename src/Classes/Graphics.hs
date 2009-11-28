{-# OPTIONS_GHC -Wall #-}
module Classes.Graphics (Dotable(toDot)
                        ,GraphvizCommand(..)
                        ,GraphvizOutput(..)
                        ,runGraphvizCommand) where
-- TODO url links for atlas
 
import Data.GraphViz
--     --Als de compiler hierover struikelt, dan moet je graphviz installeren. Dat is overigens in de volgende 3 stappen:
--              -- 1) Eerst installeer je Cabal (zie http://www.haskell.org/cabal/) en dan roep je op je command line: 
--              -- 2) cabal-install graphviz  (onder windows: cabal install graphviz)
--              -- 3) er is geen stap 3!
--              -- 4) build on graphviz-2999.5.0.0
import Adl
import FspecDef hiding (Attribute)
import Options
import Collection (Collection(rd))

class Dotable a where
   toDot :: Fspc -> Options -> a -> DotGraph Int

instance Dotable Pattern where
   toDot _ flags pat 
     = DotGraph { strictGraph = False
                , directedGraph = True
                , graphID = Nothing
                , graphStatements 
                      = DotStmts { attrStmts = [GraphAttrs (handleFlags TotalPicture flags)]
                                 , subGraphs = []
                                 , nodeStmts = [conceptLabel c | c<-cpts]
                                            ++ [conceptPoint c | c<-cpts] 
                                            ++ [inBetweenNode d | d<-arcs]
                                            ++ [inBetweenPoint d | d<-arcs]
                                 , edgeStmts = [x | d<-arcs, x<-decledges d] 
                                            ++ isaedges
                                            ++ [conceptEdge c | c<-cpts] 
                                            ++ [inBetweenEdge d | d<-arcs]
                                 }
                }
         where 
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
                    , nodeAttributes = handleFlags (CptLabel c) flags
                    }
         conceptPoint c = 
            DotNode { nodeID = lkup c conceptPointTable
                    , nodeAttributes = handleFlags (CptPoint c) flags
                    }
         conceptEdge c  = 
            DotEdge {edgeFromNodeID  = lkup c conceptTable
                    ,edgeToNodeID = lkup c conceptPointTable
                    ,edgeAttributes = handleFlags CptEdge flags
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
                                   , nodeAttributes = handleFlags (ArcLabel d) flags
                                   }   
         inBetweenPoint d = 
            DotNode { nodeID = lkup d arcsPointTable
                    , nodeAttributes = handleFlags ArcPoint flags
                    }
         inBetweenEdge d  = 
            DotEdge {edgeFromNodeID  = lkup d arcsTable
                    ,edgeToNodeID = lkup d arcsPointTable
                    ,edgeAttributes = handleFlags ArcEdge flags
                    ,directedEdge = True
                    }          
         --DESCR -> construct arc edges
         decledges d = [DotEdge --attach source
                         {edgeFromNodeID  = lkup (source d) conceptPointTable
                         ,edgeToNodeID = lkup d arcsPointTable
                         ,edgeAttributes = handleFlags (ArcSrcEdge d) flags
                         ,directedEdge = True}
                       ,DotEdge --attach target
                         {edgeFromNodeID  = lkup d arcsPointTable
                         ,edgeToNodeID = lkup (target d) conceptPointTable
                         ,edgeAttributes = handleFlags (ArcTgtEdge d) flags
                         ,directedEdge = True}]          
         --DESCR -> construct isa edges 
         isaedges = [ DotEdge
                       {edgeFromNodeID  = lkup (genspc g) conceptPointTable
                       ,edgeToNodeID  = lkup (gengen g) conceptPointTable
                       ,edgeAttributes = handleFlags (IsaEdge ) flags
                       ,directedEdge = True}
                    | g<-ptgns pat ]
         --DESCR -> Take care of the crowfoot flag 
         --construct the set of attributes on a certain type of picture object based on flag settings (currently only crowfoot)


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
         
handleFlags :: PictureObject  -> Options -> [Attribute]
handleFlags po flags = 
            case po of
              ArcSrcEdge d -> defaultEdgeAtts ++
                              if crowfoot flags
                              then  [ArrowHead noArrow] ++ --Empty] ++
                                    [ArrowTail (crowfootArrow (inj (multiplicities d)) 
                                                              (sur (multiplicities d)))]
                              else  [ArrowHead (AType[( ArrMod {arrowFill = OpenArrow
                                                               ,arrowSide = BothSides
                                                               }
                                                       ,Normal
                                                      )
                                                     ]
                                               )
                                    ] ++ -- Open ++
                                    [ArrowTail noArrow]
              ArcTgtEdge d -> defaultEdgeAtts ++
                              if crowfoot flags
                              then  [ArrowHead (crowfootArrow (fun (multiplicities d)) 
                                                              (tot (multiplicities d)))] ++
                                    [ArrowTail noArrow] 
                              else  [ArrowHead noArrow] ++
                                    [ArrowTail noArrow]
              ArcPoint   -> [Shape PointShape, invisible ] --Style$Stl Invisible Nothing,Width 0.1]
              ArcLabel d -> defaultNodeAtts ++ [Label$StrLabel (name d), Shape Plaintext]
              ArcEdge    -> [Len 0.1, invisible]--Style$Stl Invisible Nothing] --Style$Stl Dotted Nothing]
              CptPoint c -> if crowfoot flags
                            then defaultNodeAtts ++ 
                                 [ Label$StrLabel (name c)
                                 , Shape Plaintext
                                 , filled --Style$Stl Filled Nothing
                                 , LabelURL$UStr{urlString="CPT_" ++ (name c) ++ ".html"}
                                 ]
                            else [Shape PointShape, filled]--Style$Stl Filled Nothing,Width 0.1] --used to be something like: if crowfoot flags then doosje flags c else bolletje
              CptLabel c -> if crowfoot flags
                            then [Shape PointShape, invisible]--Style$Stl Invisible Nothing,Width 0.1]
                            else defaultNodeAtts ++
                                 [ Label$StrLabel (name c)
                                 , Shape Plaintext
                                 , filled --Style$Stl Filled Nothing
                                 , LabelURL$UStr{urlString="CPT_" ++ (name c) ++ ".html"}
                                 ]
              CptEdge    -> [Len 0.4, invisible]--Style$Stl Invisible Nothing] --Style$Stl Dotted Nothing]
              IsaEdge    -> defaultEdgeAtts 
              TotalPicture -> [Splines SplineEdges] -- [BgColor transparent]
                              --   ++ [Overlap (Right False) | crowfoot flags]
                              --   ++ [Splines (Left True)  | crowfoot flags]
--DESCR -> default Node attributes
defaultNodeAtts :: [Attribute]
defaultNodeAtts   = [FontSize 12,FontName "helvetica"]
--DESCR -> default Edge attributes
defaultEdgeAtts :: [Attribute]
defaultEdgeAtts   = [Len 1.2,ArrowSize 0.8]
invisible :: Attribute
invisible = Style [SItem Invisible []]
filled :: Attribute
filled = Style [SItem Filled []]
crowfootArrow :: Bool -> Bool -> ArrowType
crowfootArrow a b = 
    AType (case (a,b) of
            (True ,True ) -> [my_tee          ]
            (True ,False) -> [my_tee , my_crow]
            (False,True ) -> [my_tee, my_odot ]
            (False,False) -> [my_crow, my_odot]
          )
noMod :: ArrowModifier
noMod = ArrMod { arrowFill = FilledArrow
               , arrowSide = BothSides}
open  :: ArrowModifier
open  = noMod {arrowFill = OpenArrow}
my_tee :: ( ArrowModifier , ArrowShape )
my_tee = ( noMod , Tee )
my_odot :: ( ArrowModifier , ArrowShape )
my_odot= ( open, DotArrow )
my_crow :: ( ArrowModifier , ArrowShape )
my_crow= ( open, Crow )

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
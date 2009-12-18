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
-- Documentation about graphviz package: See http://hackage.haskell.org/package/graphviz
import Adl
import Data.Fspec (Fspc,Fservice(..))
import Options
import Collection (Collection(uni,isc,(>-)))

-- Chapter 1: All objects that can be transformed to a conceptual diagram are Dotable...
class Dotable a where
   toDot :: Fspc -> Options -> a -> DotGraph String

instance Dotable Concept where
   toDot fSpec flags c = dotG flags (name c) cpts dcls idgs
         where 
          rs   = [r| r<-rules fSpec, c `elem` concs r, not (isaRule r)]
          ss   = [s| s<-signals fSpec, c `elem` concs s]
          idgs = [ (nodePoint cpts (source (antecedent r)),nodePoint cpts (source (consequent r)))
                 | r<-rules fSpec, c `elem` concs r, isaRule r]
                 `uni`
                 [(nodePoint cpts g,nodePoint cpts s)| g<-cpts, s<-cpts, g<s, null [cpt| cpt<-cpts, g<cpt && cpt<s]]       --  all isa edges
-- TODO: removal of redundant isa edges might be done more efficiently
          cpts = concs rs `uni` concs ss
          dcls = [d | d<-declarations rs `uni` declarations ss
                    , not (isProp   d)     -- d is not a property
                    , not (isSignal d)]    -- d is not a signal

instance Dotable Pattern where
   toDot _ flags pat = dotG flags (name pat) cpts dcls idgs
        where 
         --DESCR -> get concepts and arcs from pattern
          idgs = [ (nodePoint cpts (source (antecedent r)),nodePoint cpts (source (consequent r)))
                 | r<-rules pat, isaRule r]
                 `uni`
                 [(nodePoint cpts g,nodePoint cpts s)| g<-cpts, s<-cpts, g<s, null [c| c<-cpts, g<c && c<s]]       --  all isa edges
-- TODO: removal of redundant isa edges might be done more efficiently
          cpts = concs pat
          dcls = declarations pat

instance Dotable Fservice where
   toDot fSpec flags svc = dotG flags (name svc) cpts dcls idgs
         where 
          rs         = [r| r<-rules fSpec, affected r]
          ss         = [s| s<-signals fSpec, affected s]
          affected r = not (null (declarations r `isc` declarations svc))
          idgs = [ (nodePoint cpts (source (antecedent r)),nodePoint cpts (source (consequent r)))
                 | r<-rs, isaRule r]
                 `uni`
                 [(nodePoint cpts g,nodePoint cpts s)| g<-cpts, s<-cpts, g<s, null [cpt| cpt<-cpts, g<cpt && cpt<s]]       --  all isa edges
-- TODO: removal of redundant isa edges might be done more efficiently
          cpts = concs rs `uni` concs ss
          dcls = [d | d<-declarations rs `uni` declarations ss
                    , not (isProp   d)    -- d is not a property
                    , not (isSignal d)]   -- d is not a signal

numberListFrom :: [x] -> Int -> [(x,Int)]    --TODO Deze functie is te generiek om hier in deze module thuis te horen. Verplaatsen naar andere module? 
numberListFrom xs i = zip xs [i..]

 
-- Chapter 2: Formation of a graph.
dotG :: Options
     -> String
     -> [Concept]
     -> [Declaration]
     -> [(String, String)]
     -> DotGraph String
dotG flags graphName cpts dcls idgs
     = DotGraph { strictGraph = False
                , directedGraph = True
                , graphID = Just (Str graphName)
                , graphStatements 
                      = DotStmts { attrStmts = [GraphAttrs (handleFlags TotalPicture flags)]
                                 , subGraphs = []
                                 , nodeStmts = [conceptLabel n c | (c,n)<-numberListFrom cpts 1]
                                            ++ [conceptPoint n c | (c,n)<-numberListFrom cpts (length cpts+1)] 
                                            ++ [inBetweenNode n d | (d,n)<-numberListFrom dcls 1]
                                            ++ [inBetweenPoint n  | (_,n)<-numberListFrom dcls (length dcls+1)]
                                 , edgeStmts = [x | d<-dcls, x<-declEdges (nodePoint cpts (source d))
                                                                          (arcHinge dcls d)
                                                                          (nodePoint cpts (target d)) d] 
                                            ++ isaedges idgs
                                            ++ [conceptEdge (nodePoint cpts c) (nodeLabel cpts c) | c<-cpts] 
                                            ++ [inBetweenEdge (arcHinge dcls d) (arcLabel dcls d) | d<-dcls]
                                 }
                }
       where
--DESCR -> construct arc point related picture objects
        inBetweenNode n d =
            DotNode { nodeID         = "a"++show n
                    , nodeAttributes = handleFlags (ArcLabel d) flags
                    }

        inBetweenPoint n = 
            DotNode { nodeID         = "a"++show n
                    , nodeAttributes = handleFlags ArcPoint flags
                    }

        inBetweenEdge h l {- h: hinge, l: label -}
          = DotEdge {edgeFromNodeID  = l
                    ,edgeToNodeID = h
                    ,edgeAttributes = handleFlags ArcEdge flags
                    ,directedEdge = True
                    }          

--DESCR -> construct arc edges
        declEdges from hinge to d
             = [ DotEdge {edgeFromNodeID  = from--attach source
                         ,edgeToNodeID    = hinge
                         ,edgeAttributes  = handleFlags (ArcSrcEdge d) flags
                         ,directedEdge    = True}
               , DotEdge {edgeFromNodeID  = hinge --attach target
                         ,edgeToNodeID    = to
                         ,edgeAttributes  = handleFlags (ArcTgtEdge d) flags
                         ,directedEdge    = True}
               ]          

--DESCR -> construct concept point related picture objects                                
        conceptLabel n c =
            DotNode { nodeID         = show n
                    , nodeAttributes = handleFlags (CptLabel c) flags
                    }

        conceptPoint n c
          = DotNode { nodeID         = show n
                    , nodeAttributes = handleFlags (CptPoint c) flags
                    }

        conceptEdge n l
          = DotEdge { edgeFromNodeID = l
                    , edgeToNodeID   = n
                    , edgeAttributes = handleFlags CptEdge flags
                    , directedEdge   = True
                    }

--DESCR -> construct isa edges 
        isaedges gss
          = [ DotEdge { edgeFromNodeID  = g
                      , edgeToNodeID    = s
                      , edgeAttributes  = handleFlags IsaEdge flags
                      , directedEdge    = True}
            | (g,s)<-gss ]
           --DESCR -> Take care of the crowfoot flag 
           --construct the set of attributes on a certain type of picture object based on flag settings (currently only crowfoot)

-- Chapter 3: Translation of individual concepts and declarations.
-- Every concept and every declaration must have unique node identifiers and arc identifiers

-- Translation of concepts to node-id's.
-- Each concept gets a point shaped node in the graph (a filled black circle)
-- and a node that represents the concept name (i.e. the label).
nodeLabel :: [Concept] -> Concept -> String
nodeLabel cpts c
 = case lookup c (numberListFrom cpts 1) of
   Just i -> show i
   _      -> error "!Fatal (module Graphics): element "++name c++" not found by nodeLabel."
nodePoint :: (Eq a, Identified a) => [a] -> a -> String
nodePoint cpts c
 = case lookup c (numberListFrom cpts (length cpts+1)) of
   Just i -> show i
   _      -> error "!Fatal (module Graphics): element "++name c++" not found by nodePoint."

-- Translation of declarations to arc-id's.
-- Each arc is drawn by an intermediate point, halfway the arc, which we call the "hinge".
-- The label of the arc is drawn as a separate node in the drawing, strongly linked to the hinge of the arc.
arcLabel :: (Eq a, Identified a) => [a] -> a -> String
arcLabel arcs d
 = case lookup d (numberListFrom arcs 1) of
   Just i -> "a"++show i
   _      -> error "!Fatal (module Graphics): element "++name d++" not found by arcLabel."
arcHinge :: [Declaration] -> Declaration -> String
arcHinge arcs d
 = case lookup d (numberListFrom arcs (length arcs +1)) of
   Just i -> "a"++show i
   _      -> error "!Fatal (module Graphics): element "++name d++" not found by arcHinge."


--DESCR -> a picture consists of arcs (relations), concepts, and ISA relations between concepts
--         arcs are attached to a source or target concept
--         arcs and concepts are points attached to a label
-- for Haddock support on GraphViz, click on:
--       http://hackage.haskell.org/packages/archive/graphviz/2999.6.0.0/doc/html/doc-index.html     or
--       http://hackage.haskell.org/packages/archive/graphviz/latest/doc/html/doc-index.html

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
                                    [ArrowTail (crowfootArrow (isInj d) 
                                                              (isSur d))]
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
                              then  [ArrowHead (crowfootArrow (isUni d) 
                                                              (isTot d))] ++
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
              TotalPicture -> -- [BgColor Transparent]++
                              if crowfoot flags
                              then [Overlap RemoveOverlaps]
                              else [Splines SplineEdges]
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
            (False,True ) -> [my_tee,  my_odot]
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

{-DESCR -> lookup the integer id of an object

lkup :: (Eq a) => a -> [(a,b)] -> b
lkup x tbl = case lookup x tbl of
   Just i -> i
   _ -> error "!Fatal (module Graphics): element not found." --TODO
-}

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
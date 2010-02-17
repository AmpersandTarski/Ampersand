{-# OPTIONS_GHC -Wall #-}
module Classes.Graphics (Dotable(makePicture)
                        ,GraphvizCommand(..)
                        ,GraphvizOutput(..)
                        ,runGraphvizCommand) where
-- TODO url links for atlas

import Data.GraphViz hiding(addExtension)
--     --Als de compiler hierover struikelt, dan moet je graphviz installeren. Dat is overigens in de volgende 3 stappen:
--              -- 1) Eerst installeer je Cabal (zie http://www.haskell.org/cabal/) en dan roep je op je command line: 
--              -- 2) cabal-install graphviz  (onder windows: cabal install graphviz)
--              -- 3) er is geen stap 3!
--              -- 4) build on graphviz-2999.5.0.0
-- Documentation about graphviz package: See http://hackage.haskell.org/package/graphviz
import Adl
import Data.Fspec (Fspc,Fservice(..))
import Options
import Collection (Collection(uni,isc,rd))
import Typology (Inheritance(..))
import Picture
import Switchboard
import Rendering.ClassDiagram
import System.FilePath   -- (replaceExtension,takeBaseName, (</>) )
import Strings

class Identified a => Navigatable a where
   theURL :: Options -> a -> URL    -- url of the web page in Atlas used when clicked on a node or edge in a .map file
   theURL flags a = UStr {urlString = dirAtlas flags </> addExtension (spacesToUnderscores (name a)) "php"} 

instance Navigatable Concept where
   theURL flags cpt = UStr { urlString = dirAtlas flags </> addExtension (spacesToUnderscores (name cpt)) "php"} 
instance Navigatable Declaration where 
   theURL flags d = UStr {urlString = dirAtlas flags </> addExtension (spacesToUnderscores (name d ++ name (source d) ++ name (target d))) "php"} 

-- Chapter 1: All objects that can be transformed to a conceptual diagram are Dotable...
class Identified a => Dotable a where
   picType :: a -> PictType
   toDot :: Fspc -> Options -> a -> DotGraph String
   makePicture :: Options -> Fspc -> a -> Picture
   makePicture flags fSpec dottable =
          makePictureObj flags (name dottable) (picType dottable) (printDotGraph(toDot fSpec flags dottable)) 
instance Dotable ClassDiag where
   picType _ = PTClassDiagram
   toDot _ _ _ = error ("!TODO (module Graphics 31): ClassDiagram moet nog netjes naar nieuwe Graphviz worden verbouwd.") 
   makePicture flags _ cd =
          makePictureObj flags (name cd) (picType cd) (classdiagram2dot cd) 
instance Dotable Concept where
   picType _ = PTConcept
   toDot fSpec flags c = dotG flags (name c) cpts dcls idgs
         where 
          rs   = [r| r<-rules fSpec, c `elem` concs r, not (isaRule r)]
          ss   = [s| s<-signals fSpec, c `elem` concs s]
          idgs = [(g,s)|(g,s)<-gs, elem g cpts' || elem s cpts']  --  all isa edges
          Isa gs _ = isa fSpec
-- TODO: removal of redundant isa edges might be done more efficiently
          cpts = rd$cpts' ++ [g|(g,s)<-gs, elem g cpts' || elem s cpts'] ++ [s|(g,s)<-gs, elem g cpts' || elem s cpts']
          cpts'  = concs rs `uni` concs ss
          dcls = [d | d@Sgn{}<-decls rs `uni` decls ss
                    , not (isProp d)     -- d is not a property
                    , decusr d]          -- d is user defined, and consequently not a signal either
   
instance Dotable Pattern where
   picType _ = PTPattern
   toDot fSpec flags pat = dotG flags (name pat) cpts dcls idgs
        where 
         --DESCR -> get concepts and arcs from pattern
          idgs = [(g,s)|(g,s)<-gs, elem g cpts' || elem s cpts']  --  all isa edges
          Isa gs _ = isa fSpec 
-- TODO: removal of redundant isa edges might be done more efficiently
          cpts = rd$cpts' ++ [g|(g,s)<-gs, elem g cpts' || elem s cpts'] ++ [s|(g,s)<-gs, elem g cpts' || elem s cpts']
          cpts'  = concs pat
          dcls = [d| d@Sgn{}<-declarations pat, decusr d] `uni` decls pat

instance Dotable Fservice where
   picType _ = PTFservice
   toDot fSpec flags svc = dotG flags (name svc) cpts dcls idgs
         where 
          rs         = [r| r<-rules fSpec, affected r]
          ss         = [s| s<-signals fSpec, affected s]
          affected r = not (null (decls r `isc` decls svc))
          idgs = [(g,s)|(g,s)<-gs, elem g cpts' || elem s cpts']  --  all isa edges
          Isa gs _ = isa fSpec
-- TODO: removal of redundant isa edges might be done more efficiently
          cpts = rd$cpts' ++ [g|(g,s)<-gs, elem g cpts' || elem s cpts'] ++ [s|(g,s)<-gs, elem g cpts' || elem s cpts']
          cpts'  = concs rs `uni` concs ss
          dcls = [d | d@Sgn{}<-decls rs `uni` decls ss
                    , not (isProp   d)    -- d is not a property
                    , decusr d]           -- d is user defined, and consequently not a signal either

instance Dotable SwitchBoard where
   picType _ = PTSwitchBoard
   toDot _ _ s = sbdotGraph s
   
instance Dotable Rule where
   picType _ = PTRule
   toDot fSpec flags r = dotG flags (name r) cpts dcls idgs
         where 
          idgs = [(g,s)|(g,s)<-gs, elem g cpts' || elem s cpts']  --  all isa edges
          Isa gs _ = isa fSpec
-- TODO: removal of redundant isa edges might be done more efficiently
          cpts = rd$cpts' ++ [g|(g,s)<-gs, elem g cpts' || elem s cpts'] ++ [s|(g,s)<-gs, elem g cpts' || elem s cpts']
          cpts' = concs r
          dcls = [d | d@Sgn{}<-decls r]

numberListFrom :: [x] -> Int -> [(x,Int)]    --TODO Deze functie is te generiek om hier in deze module thuis te horen. Verplaatsen naar andere module? 
numberListFrom xs i = zip xs [i..]

 
-- Chapter 2: Formation of a graph.
dotG :: Options            -- ^ the flags 
     -> String             -- ^ the name of the Graph
     -> Concepts           -- ^ The concepts to draw in the graph
     -> Declarations       -- ^ The declarations, (the edges in the graph)
     -> [(Concept, Concept)] -- ^ list of Gen relations 
     -> DotGraph String    -- ^ The resulting DotGraph
dotG flags graphName cpts dcls idgs
     = DotGraph { strictGraph = False
                , directedGraph = True
                , graphID = Just (Str graphName)
                , graphStatements 
                      = DotStmts { attrStmts = [GraphAttrs (handleFlags TotalPicture flags)]
                                 , subGraphs = []
                                 , nodeStmts = conceptNodes ++ declarationNodes ++ isaNodes
                                 , edgeStmts = conceptEdges ++ declarationEdges ++ isaEdges
                                 }
                }
       where
        (conceptNodes    ,conceptEdges    ) = (concat a, concat b)
              where (a,b) = unzip (map (conceptNodesAndEdges     ) cpts)
        (declarationNodes,declarationEdges) = (concat a, concat b) 
              where (a,b) = unzip (map (declarationNodesAndEdges ) numberedDeclarations)
        (isaNodes        ,isaEdges        ) = (concat a, concat b) 
              where (a,b) = unzip (map (isaNodesAndEdges         ) numberedIsas)

        numberedConcepts     :: [(Concept    , Int)]
        numberedConcepts     = numberListFrom cpts 1
        numberedDeclarations :: [(Declaration, Int)]
        numberedDeclarations = numberListFrom dcls 1
        numberedIsas         :: [((Concept, Concept), Int)]
        numberedIsas         = numberListFrom idgs 1

        baseNodeId :: Concept -> String  -- returns the NodeId of the node where edges to this node should connect to. 
        baseNodeId c 
            = case lookup c numberedConcepts of
                Just i -> "cpt_"++show i
                _      -> error ("!Fatal (module Graphics 105): element "++name c++" not found by nodeLabel.")

        -- | This function constructs a list of NodeStatements that must be drawn for a concept. 
        conceptNodesAndEdges :: 
                        Concept   -- ^ The concept for which the nodes and edges are constructed
                     -> ([DotNode String],[DotEdge String]) -- ^ the resulting tuple contains the NodeStatements and EdgeStatements
        conceptNodesAndEdges c
             = (   [cptOnlyOneNode    ] -- just only one node for a concept.
               ,   []
               )
          where
            cptOnlyOneNode   = constrNode (baseNodeId c) (CptOnlyOneNode   c) flags
        
        -- | This function constructs a list of NodeStatements that must be drawn for a concept.
        declarationNodesAndEdges ::
                            (Declaration,Int)  -- ^ tuple contains the declaration and its rank
                         -> ([DotNode String],[DotEdge String])   -- ^ the resulting tuple contains the NodeStatements and EdgeStatements
        declarationNodesAndEdges (d,n)
             = (    [dclHingeNode]   -- The node of the hinge 
                  ++[dclNameNode]    -- node to place the name of the declaration
               ,    [constrEdge (baseNodeId (source d)) (nodeID dclHingeNode)   (DclSrcEdge d) True flags]     -- edge to connect the source with the hinge
                  ++[constrEdge (nodeID dclHingeNode)   (baseNodeId (target d)) (DclTgtEdge d) True flags]     -- edge to connect the hinge to the target
                  ++[constrEdge (nodeID dclHingeNode)   (nodeID dclNameNode )    DclMiddleEdge True flags]  -- edge to connect the hinge node to the nameNode
               )
          where
            dclHingeNode   = constrNode ("dclHinge_"++show n) DclHingeNode   flags
            dclNameNode    = constrNode ("dclName_"++show n) (DclNameNode d) flags
            
        -- | This function constructs a list of NodeStatements that must be drawn for a concept.
        isaNodesAndEdges :: ((Concept, Concept),Int)  -- ^ tuple contains the declaration and its rank
                         -> ([DotNode String],[DotEdge String])   -- ^ the resulting tuple contains the NodeStatements and EdgeStatements
        isaNodesAndEdges ((s,t),_)
             = ( [] -- No node at all
               , [constrEdge (baseNodeId s) (baseNodeId t) IsaOnlyOneEdge True  flags] -- Just a single edge
               )
--           = (   [isaDotHingeNode]
--             ,   [constrEdge (baseNodeId s) (nodeID isaDotHingeNode) IsaEdge True flags]
--               ++[constrEdge (nodeID isaDotHingeNode) (baseNodeId t) IsaEdge True flags]
--             ) 
--          where
--            isaDotHingeNode  = constrNode ("isaHinge_"++show n) IsaHingeNode flags

                        
constrNode :: a -> PictureObject -> Options -> DotNode a
constrNode nodeId pObj flags
  = DotNode { nodeID = nodeId
            , nodeAttributes = handleFlags pObj flags
            }
constrEdge :: a -> a -> PictureObject -> Bool -> Options -> DotEdge a
constrEdge nodeFrom nodeTo pObj isDirected flags 
  = DotEdge { edgeFromNodeID = nodeFrom
            , edgeToNodeID   = nodeTo
            , edgeAttributes = handleFlags pObj flags
            , directedEdge   = isDirected
            }
--DESCR -> a picture consists of arcs (relations), concepts, and ISA relations between concepts
--         arcs are attached to a source or target concept
--         arcs and concepts are points attached to a label
-- for Haddock support on GraphViz, click on:
--       http://hackage.haskell.org/packages/archive/graphviz/2999.6.0.0/doc/html/doc-index.html     or
--       http://hackage.haskell.org/packages/archive/graphviz/latest/doc/html/doc-index.html

data PictureObject = CptOnlyOneNode Concept     -- ^ Node of a concept that serves as connector and shows the name
                   | CptConnectorNode Concept   -- ^ Node of a concept that serves as connector of relations to that concept
                   | CptNameNode Concept        -- ^ Node of a concept that shows the name
                   | CptEdge                    -- ^ Edge of a concept to connect its nodes
                   | DclOnlyOneEdge Declaration -- ^ Edge of a relation that connects to the source and the target
                   | DclSrcEdge Declaration     -- ^ Edge of a relation that connects to the source
                   | DclTgtEdge Declaration     -- ^ Edge of a relation that connects to the target
                   | DclHingeNode               -- ^ Node of a relation that serves as a hinge
                   | DclNameNode Declaration    -- ^ Node of a relation that shows the name
                   | DclMiddleEdge              -- ^ Edge of a relation to connect hinges and/or namenode
                   | IsaOnlyOneEdge             -- ^ Edge of an ISA relation without a hinge node
                   | IsaHingeNode               -- ^ Node of an ISA relation
                   | IsaEdge                    -- ^ Edge of a Gen to connec the source to the target of it
                   | TotalPicture               -- ^ Graph attributes
         
handleFlags :: PictureObject  -> Options -> [Attribute]
handleFlags po flags = 
    case po of
      CptConnectorNode c 
         -> if crowfoot flags
            then defaultNodeAtts ++ 
                 [ Label$StrLabel (name c)
                 , Shape Plaintext
                 , filled --Style$Stl Filled Nothing
                 , URL (theURL flags c)
                 ]
            else [Shape PointShape, filled]--Style$Stl Filled Nothing,Width 0.1] --used to be something like: if crowfoot flags then doosje flags c else bolletje
      CptNameNode c  -> if crowfoot flags
                        then [Shape PointShape, invisible]--Style$Stl Invisible Nothing,Width 0.1]
                        else defaultNodeAtts ++
                             [ Label$StrLabel (name c)
                             , Shape Plaintext
                             , filled --Style$Stl Filled Nothing
                             , URL (theURL flags c)
                             ]
      CptEdge    -> [Len 0.4, invisible]
      CptOnlyOneNode c -> defaultNodeAtts ++
                          [Label (StrLabel (name c))
                          , Shape Plaintext
                          , filled 
                          , URL (theURL flags c)
                          ]
      DclOnlyOneEdge d -> [ Label (StrLabel (name d))
                          , ArrowHead (if crowfoot flags
                                       then crowfootArrowType True d
                                       else plainArrowType True d
                                      )
                          , ArrowTail (if crowfoot flags
                                       then crowfootArrowType False d
                                       else plainArrowType False d
                                      )
                          ]
      DclSrcEdge d -> [Len 1.2
 --                     ,ArrowHead (AType [(open,Normal)])    -- Geeft de richting van de relatie aan.
                      ,ArrowHead ( if or [crowfoot flags, not (isFunction d)] 
                                   then AType [(open,Normal)]
                                   else noArrow
                                 )
                      ,ArrowTail ( if crowfoot flags
                                   then crowfootArrowType False d
                                   else noArrow
                                 )
                      ]
      DclTgtEdge d -> [Len 1.2
                      , ArrowHead ( if crowfoot flags
                                   then crowfootArrowType True d
                                   else plainArrowType True d
                                 )
                      ,ArrowTail noArrow
                      ] 
      DclHingeNode  -> [Shape PointShape, invisible ]
      DclNameNode d -> defaultNodeAtts ++ 
                       [ Label (StrLabel (name d))
                       , Shape Plaintext
                       , BgColor (ColorName "white") 
                       ]
      DclMiddleEdge -> [ Len 0.1
                       , ArrowHead noArrow
                       , ArrowTail noArrow
                       ]
      IsaOnlyOneEdge-> [ Color [ColorName "black"]
                       , Len 1.5
                       , ArrowHead (AType [(ArrMod OpenArrow BothSides, Normal)])
                       , ArrowTail noArrow
                       , dotted
                       ]
      IsaEdge       -> [ Color [ColorName "red"]
                       , Len 0.6
                       , ArrowHead (AType [(ArrMod OpenArrow BothSides, Normal)])	
                       , ArrowTail noArrow
                       ] 
      IsaHingeNode  -> handleFlags DclHingeNode flags
      TotalPicture -> [  Overlap CompressOverlap ]

--DESCR -> default Node attributes
defaultNodeAtts :: [Attribute]
defaultNodeAtts   = [FontSize 12,FontName "helvetica"]

invisible :: Attribute
invisible = Style [SItem Invisible []]

dotted :: Attribute
dotted = Style [SItem Dotted []]

--dashed :: Attribute
--dashed = Style [SItem Dashed []]

filled :: Attribute
filled = Style [SItem Filled []]

crowfootArrowType :: Bool -> Declaration -> ArrowType
crowfootArrowType isHead d 
   = AType (case isHead of
               True  -> getCrowfootShape (isUni d) (isTot d)
               False -> getCrowfootShape (isInj d) (isSur d)
           )
       where 
         getCrowfootShape :: Bool -> Bool -> [( ArrowModifier , ArrowShape )]
         getCrowfootShape a b =
          (case (a,b) of
            (True ,True ) -> [my_tee          ]
            (True ,False) -> [my_crow, my_tee]
            (False,True ) -> [my_odot, my_tee]
            (False,False) -> [my_crow, my_odot]
          )   
         my_tee :: ( ArrowModifier , ArrowShape )
         my_tee = ( noMod , Tee )
         my_odot :: ( ArrowModifier , ArrowShape )
         my_odot= ( open, DotArrow )
         my_crow :: ( ArrowModifier , ArrowShape )
         my_crow= ( open, Crow )

plainArrowType :: Bool -> Declaration -> ArrowType
plainArrowType isHead d
   = case isHead of 
       True -> if isFunction d
               then normal
               else noArrow 
       False -> noArrow

noMod :: ArrowModifier
noMod = ArrMod { arrowFill = FilledArrow
               , arrowSide = BothSides
               }
open  :: ArrowModifier
open  = noMod {arrowFill = OpenArrow}

--makeLabelTable :: String -> String
--makeLabelTable n
--  = n

-- hulpfuncties, voor tijdelijk. TODO, opschonen
--transparant :: Attribute
--transparant = Color [Transparant]
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

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
import Collection (Collection(uni,isc))

-- Chapter 1: All objects that can be transformed to a conceptual diagram are Dotable...
class Dotable a where
   toDot :: Fspc -> Options -> a -> DotGraph String

instance Dotable Concept where
   toDot fSpec flags c = dotG flags (name c) cpts dcls idgs
         where 
          rs   = [r| r<-rules fSpec, c `elem` concs r, not (isaRule r)]
          ss   = [s| s<-signals fSpec, c `elem` concs s]
          idgs = [ ((source (antecedent r)),(source (consequent r)))
                 | r<-rules fSpec, c `elem` concs r, isaRule r]
                 `uni`
                 [( g, s)| g<-cpts, s<-cpts, g<s, null [cpt| cpt<-cpts, g<cpt && cpt<s]]       --  all isa edges
-- TODO: removal of redundant isa edges might be done more efficiently
          cpts = concs rs `uni` concs ss
          dcls = [d | d<-declarations rs `uni` declarations ss
                    , not (isProp   d)     -- d is not a property
                    , not (isSignal d)]    -- d is not a signal

instance Dotable Pattern where
   toDot _ flags pat = dotG flags (name pat) cpts dcls idgs
        where 
         --DESCR -> get concepts and arcs from pattern
          idgs = [ ((source (antecedent r)),(source (consequent r)))
                 | r<-rules pat, isaRule r]
                 `uni`
                 [( g, s)| g<-cpts, s<-cpts, g<s, null [c| c<-cpts, g<c && c<s]]       --  all isa edges
-- TODO: removal of redundant isa edges might be done more efficiently
          cpts = concs pat
          dcls = declarations pat

instance Dotable Fservice where
   toDot fSpec flags svc = dotG flags (name svc) cpts dcls idgs
         where 
          rs         = [r| r<-rules fSpec, affected r]
          ss         = [s| s<-signals fSpec, affected s]
          affected r = not (null (declarations r `isc` declarations svc))
          idgs = [ ( (source (antecedent r)), (source (consequent r)))
                 | r<-rs, isaRule r]
                 `uni`
                 [( g, s)| g<-cpts, s<-cpts, g<s, null [cpt| cpt<-cpts, g<cpt && cpt<s]]       --  all isa edges
-- TODO: removal of redundant isa edges might be done more efficiently
          cpts = concs rs `uni` concs ss
          dcls = [d | d<-declarations rs `uni` declarations ss
                    , not (isProp   d)    -- d is not a property
                    , not (isSignal d)]   -- d is not a signal

numberListFrom :: [x] -> Int -> [(x,Int)]    --TODO Deze functie is te generiek om hier in deze module thuis te horen. Verplaatsen naar andere module? 
numberListFrom xs i = zip xs [i..]

 
-- Chapter 2: Formation of a graph.
dotG :: Options            -- ^ the flags 
     -> String             -- ^ the name of the Graph
     -> [Concept]          -- ^ list of the concepts to draw in the graph
     -> [Declaration]      -- ^ list of declarations, (the edges in the graph)
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
              where (a,b) = unzip (map (conceptNodesAndEdges     (length cpts)) numberedConcepts)
        (declarationNodes,declarationEdges) = (concat a, concat b) 
              where (a,b) = unzip (map (declarationNodesAndEdges (length dcls)) numberedDeclarations)
        (isaNodes        ,isaEdges        ) = (concat a, concat b) 
              where (a,b) = unzip (map (isaNodesAndEdges         (length idgs)) numberedIsas)

        numberedConcepts     = numberListFrom cpts 1
        numberedDeclarations = numberListFrom dcls 1
        numberedIsas         = numberListFrom idgs 1

        lableOfCptDotNode0 :: Concept -> String
        lableOfCptDotNode0 c 
            = case lookup c numberedConcepts of
                Just i -> show i
                _      -> error "!Fatal (module Graphics): element "++name c++" not found by nodeLabel."

        -- | This function constructs a list of NodeStatements that must be drawn for a concept. 
        conceptNodesAndEdges :: Int      -- ^ Each ID of a NodeStatement should be the rank of that concept, Modulo this integer.
                     -> (Concept,Int)    -- ^ tuple contains the concept and its rank
                     -> ([DotNode String],[DotEdge String]) -- ^ the resulting tuple contains the NodeStatements and EdgeStatements
        conceptNodesAndEdges increment (c,n)
             = case  dotStyle flags of
                 1 -> (   [cptOnlyOneNode] -- just only one node for a concept.
                      ,   []
                      )
                 2 -> (    [cptConnectorNode ]  -- node onto which the relations will connect
                         ++[cptNameNode ]  -- node that shows the name of the concept
                      ,    [cptEdge ]  -- edge connects the above nodes
                      )
                 _ -> undefined
          where
            cptOnlyOneNode
              = DotNode { nodeID         = show (n+increment*0)
                        , nodeAttributes = handleFlags ( CptOnlyOneNode c) flags
                        }
            cptConnectorNode
              = DotNode { nodeID         = nodeID cptOnlyOneNode
                        , nodeAttributes = handleFlags ( CptConnectorNode c) flags
                        }
            cptNameNode
              = DotNode { nodeID         = show (n+increment*1)
                        , nodeAttributes = handleFlags ( CptNameNode c) flags
                        }
            cptEdge
              = DotEdge { edgeFromNodeID = nodeID cptConnectorNode
                        , edgeToNodeID   = nodeID cptNameNode
                        , edgeAttributes = handleFlags CptEdge flags
                        , directedEdge   = True
                        }

        
        -- | This function constructs a list of NodeStatements that must be drawn for a concept.
        declarationNodesAndEdges :: Int        -- ^ Each ID of a NodeStatement should be the rank of that declaration, Modulo this integer.
                         -> (Declaration,Int)  -- ^ tuple contains the declaration and its rank
                         -> ([DotNode String],[DotEdge String])   -- ^ the resulting tuple contains the NodeStatements and EdgeStatements
        declarationNodesAndEdges increment (d,n)
             = case  dotStyle flags of
                 1 -> (    []               -- No node at all
                      ,    [dclOnlyOneEdge] -- Just a single edge
                      )
                 2 -> (    [dclHingeNode]   -- The node of the hinge 
                         ++[dclNameNode]   -- node to place the name of the declaration
                      ,    [dclSrcEdge]   -- edge to connect the source with the hinge
                         ++[dclTgtEdge]   -- edge to connect the hinge to the target
                         ++[dclMiddleEdge]   -- edge to connect the hinge node to the nameNode
                      )
                 _ -> undefined 
          where
            dclOnlyOneEdge
              = DotEdge { edgeFromNodeID = lableOfCptDotNode0 (source d)
                        , edgeToNodeID   = lableOfCptDotNode0 (target d)
                        , edgeAttributes = handleFlags (DclOnlyOneEdge d) flags
                        , directedEdge   = True
                        }
            dclHingeNode 
              = DotNode { nodeID         = "dcl"++show (n+increment*0)
                        , nodeAttributes = handleFlags DclHingeNode flags
                        }
            dclNameNode
              = DotNode { nodeID         = "dcl"++show (n+increment*1)
                        , nodeAttributes = handleFlags (DclNameNode d) flags
                        }
            dclSrcEdge 
              = DotEdge { edgeFromNodeID = lableOfCptDotNode0 (source d)
                        , edgeToNodeID   = nodeID dclHingeNode
                        , edgeAttributes = handleFlags (DclSrcEdge d) flags
                        , directedEdge   = True
                        }
            dclTgtEdge 
              = DotEdge { edgeFromNodeID = nodeID dclHingeNode
                        , edgeToNodeID   = lableOfCptDotNode0 (target d)
                        , edgeAttributes = handleFlags (DclTgtEdge d) flags
                        , directedEdge   = True
                        }
            dclMiddleEdge 
              = DotEdge { edgeFromNodeID = nodeID dclHingeNode
                        , edgeToNodeID   = nodeID dclNameNode
                        , edgeAttributes = handleFlags DclMiddleEdge flags
                        , directedEdge   = False
                        }
        -- | This function constructs a list of NodeStatements that must be drawn for a concept.
        isaNodesAndEdges :: Int        -- ^ Each ID of a NodeStatement should be the rank of that declaration, Modulo this integer.
                         -> ((Concept, Concept),Int)  -- ^ tuple contains the declaration and its rank
                         -> ([DotNode String],[DotEdge String])   -- ^ the resulting tuple contains the NodeStatements and EdgeStatements
        isaNodesAndEdges increment ((s,t),n)
           = (   [isaDotNode0]
             ,   [isaDotEdgeA]
               ++[isaDotEdgeB]
             ) 
          where
            isaDotNode0 
              = DotNode { nodeID         = "isa"++show (n+increment*0)
                        , nodeAttributes = handleFlags DclHingeNode flags
                        }
            isaDotEdgeA 
              = DotEdge { edgeFromNodeID = lableOfCptDotNode0 s
                        , edgeToNodeID   = nodeID isaDotNode0
                        , edgeAttributes = handleFlags IsaEdge flags
                        , directedEdge   = True
                        }
            isaDotEdgeB 
              = DotEdge { edgeFromNodeID = nodeID isaDotNode0
                        , edgeToNodeID   = lableOfCptDotNode0 t
                        , edgeAttributes = handleFlags IsaEdge flags
                        , directedEdge   = True
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
                 , LabelURL$UStr{urlString="CPT_" ++ (name c) ++ ".html"}
                 ]
            else [Shape PointShape, filled]--Style$Stl Filled Nothing,Width 0.1] --used to be something like: if crowfoot flags then doosje flags c else bolletje
      CptNameNode c  -> if crowfoot flags
                        then [Shape PointShape, invisible]--Style$Stl Invisible Nothing,Width 0.1]
                        else defaultNodeAtts ++
                             [ Label$StrLabel (name c)
                             , Shape Plaintext
                             , filled --Style$Stl Filled Nothing
                             , LabelURL$UStr{urlString="CPT_" ++ (name c) ++ ".html"}
                             ]
      CptEdge    -> [Len 0.4, invisible]
      CptOnlyOneNode c -> defaultNodeAtts ++
                          [Label (StrLabel (name c))
                          , Shape Plaintext
                          , filled 
                          , LabelURL$UStr{urlString="CPT_" ++ (name c) ++ ".html"}
                          ]
      DclOnlyOneEdge d -> defaultEdgeAtts ++ 
                          [ Decorate True
                          , Label (StrLabel (name d))
                          , Len 3
                          , ArrowHead (if crowfoot flags
                                       then crowfootArrowType True d
                                       else plainArrowType True d
                                      )
                          , ArrowTail (if crowfoot flags
                                       then crowfootArrowType False d
                                       else plainArrowType False d
                                      )
                          ]
      DclSrcEdge d -> defaultEdgeAtts ++ 
                      [ArrowHead noArrow
                      ,ArrowTail ( if crowfoot flags
                                   then crowfootArrowType False d
                                   else plainArrowType False d
                                 )
                      ]
      DclTgtEdge d -> defaultEdgeAtts ++
                      [ArrowHead ( if crowfoot flags
                                   then crowfootArrowType True d
                                   else plainArrowType True d
                                 )
                      ,ArrowTail noArrow
                      ] 
      DclHingeNode  -> [Shape PointShape, invisible ]
      DclNameNode d -> defaultNodeAtts ++ [Label$StrLabel (name d), Shape Plaintext]
      DclMiddleEdge -> [Len 0.1]
      IsaEdge    -> defaultEdgeAtts 
      TotalPicture -> --[BgColor Transparent]++
                      if crowfoot flags
                      then [Overlap RemoveOverlaps]
                      else [Splines SplineEdges]
--DESCR -> default Node attributes
defaultNodeAtts :: [Attribute]
defaultNodeAtts   = [FontSize 12,FontName "helvetica"]
--DESCR -> default Edge attributes
defaultEdgeAtts :: [Attribute]
defaultEdgeAtts   = [Len 1.2
                    ,ArrowSize 0.8
                    ]
invisible :: Attribute
invisible = Style [SItem Invisible []]
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
            (True ,False) -> [my_tee , my_crow]
            (False,True ) -> [my_tee,  my_odot]
            (False,False) -> [my_crow, my_odot]
          )   
         my_tee :: ( ArrowModifier , ArrowShape )
         my_tee = ( noMod , Tee )
         my_odot :: ( ArrowModifier , ArrowShape )
         my_odot= ( open, DotArrow )
         my_crow :: ( ArrowModifier , ArrowShape )
         my_crow= ( open, Crow )
plainArrowType :: Bool -> Declaration -> ArrowType
plainArrowType isHead _
   = case isHead of 
       True -> AType[ (open, NoArrow)
                    , (open, NoArrow)
                    , (ArrMod { arrowFill = OpenArrow
                              , arrowSide = BothSides
                              } , Normal)
                    ]
       False -> noArrow

noMod :: ArrowModifier
noMod = ArrMod { arrowFill = FilledArrow
               , arrowSide = BothSides
               }
open  :: ArrowModifier
open  = noMod {arrowFill = OpenArrow}


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
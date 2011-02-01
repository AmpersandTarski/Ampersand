{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
module DatabaseDesign.Ampersand.Fspec.Graphic.Graphics 
          (Dotable(makePicture)
    --      ,GraphvizCommand(..)
    --      ,GraphvizOutput(..)
    --      ,runGraphvizCommand) 
    )where
-- TODO url links for atlas

import Data.GraphViz hiding (addExtension )
import DatabaseDesign.Ampersand.ADL1 
import DatabaseDesign.Ampersand.Fspec.Fspec (Fspc,Fservice)
import DatabaseDesign.Ampersand.Fspec.Switchboard
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Basics (Inheritance(..),Collection(..),Identified(..))
import DatabaseDesign.Ampersand.Fspec.Graphic.Picture
import DatabaseDesign.Ampersand.Fspec.Graphic.ClassDiagram (ClassDiag,classdiagram2dot)

class Identified a => Navigatable a where
   servicename :: a -> String
   itemstring :: a -> String 
   theURL :: Options -> a -> EscString    -- url of the web page in Atlas used when clicked on a node or edge in a .map file
   theURL flags x 
     = "Atlas.php?content=" ++ servicename x
            ++  "&User=" ++ user
            ++  "&Script=" ++ script
            ++  "&"++servicename x ++"="++qualify++itemstring x
      where --copied from atlas.hs
      script = fileName flags
      user = takeWhile (/='.') (userAtlas flags)
      qualify = "("++user ++ "." ++ script ++ ")"  


instance Navigatable Concept where
   servicename _ = "Concept" --see Atlas.adl
   itemstring cpt = name cpt --copied from atlas.hs
 
instance (Eq c, Identified c) => Navigatable (Declaration c) where 
   servicename _ = "Relatiedetails" --see Atlas.adl
   itemstring x@(Sgn{}) = name x ++ "::" ++ name(source x)++"*"++name(target x) --copied from atlas.hs (function relpred+cpttype)
   itemstring x = name x    
     
-- Chapter 1: All objects that can be transformed to a conceptual diagram are Dotable...
class Identified a => Dotable a where
   picType :: a -> PictType
   toDot :: Fspc -> Options -> a -> DotGraph String
   makePicture :: Options -> Fspc -> a -> Picture
   makePicture flags fSpec dottable =
          makePictureObj flags (name dottable) (picType dottable) (printDotGraph(toDot fSpec flags dottable)) 
instance Dotable ClassDiag where
   picType _ = PTClassDiagram
   toDot _ _ _ = error ("!TODO (module Graphics 59): ClassDiagram moet nog netjes naar nieuwe Graphviz worden verbouwd.") 
   makePicture flags _ cd =
          makePictureObj flags (name cd) (picType cd) (classdiagram2dot flags cd) 
instance Dotable Concept where
   picType _ = PTConcept
   toDot fSpec flags c = dotG flags (name c) cpts (map makeDeclaration mrs) idgs
         where 
          rs    = [r| r<-rules fSpec, c `elem` concs r, not (isaRule r)]
          ss    = [s| s<-signals fSpec, c `elem` concs s]
          idgs  = [(g,s)|(g,s)<-gs, elem g cpts' || elem s cpts']  --  all isa edges
          Isa gs _ = isa fSpec
-- TODO: removal of redundant isa edges might be done more efficiently
          cpts  = rd$cpts' ++ [g|(g,s)<-gs, elem g cpts' || elem s cpts'] ++ [s|(g,s)<-gs, elem g cpts' || elem s cpts']
          cpts' = concs rs `uni` concs ss
          mrs   = [m | m<-mors rs `uni` mors ss
                     , not (isProp m)                       -- d is not a property
                     , decusr (makeDeclaration m)]          -- d is user defined, and consequently not a signal either
   
instance Dotable Pattern where
   picType _ = PTPattern
   toDot fSpec flags pat = dotG flags (name pat) cpts dcls idgs
        where 
         --DESCR -> get concepts and arcs from pattern
          idgs = [(g,s)|(g,s)<-gs, elem g cpts']  --  all isa edges
          Isa gs _ = isa fSpec 
-- TODO: removal of redundant isa edges might be done more efficiently
          cpts = rd(cpts' ++ [g|(g,_)<-idgs] ++ [s|(_,s)<-idgs])
          cpts'  = concs pat
          dcls = [d| d@Sgn{}<-declarations pat `uni` map makeDeclaration (mors pat), decusr d]

instance Dotable Fservice where
   picType _ = PTFservice
   toDot fSpec flags svc = dotG flags (name svc) cpts dcls idgs
         where 
          rs         = [r| r<-rules fSpec, affected r]
          ss         = [s| s<-signals fSpec, affected s]
          affected r = not (null (mors r `isc` mors svc))
          idgs = [(g,s)|(g,s)<-gs, elem g cpts' || elem s cpts']  --  all isa edges
          Isa gs _ = isa fSpec
-- TODO: removal of redundant isa edges might be done more efficiently
          cpts = rd$cpts' ++ [g|(g,s)<-gs, elem g cpts' || elem s cpts'] ++ [s|(g,s)<-gs, elem g cpts' || elem s cpts']
          cpts'  = concs rs `uni` concs ss
          dcls = [d | d@Sgn{}<-map makeDeclaration (mors rs `uni` mors ss)
                    , not (isProp   d)    -- d is not a property
                    , decusr d]           -- d is user defined, and consequently not a signal either

instance Dotable SwitchBoard where
   picType _ = PTSwitchBoard
   toDot _ _ s = sbdotGraph s
   
instance Dotable (Rule (Relation Concept)) where
   picType _ = PTRule
   toDot fSpec flags r = dotG flags (name r) cpts dcls idgs
         where 
          idgs = [(g,s)|(g,s)<-gs, elem g cpts' || elem s cpts']  --  all isa edges
          Isa gs _ = isa fSpec
-- TODO: removal of redundant isa edges might be done more efficiently
          cpts = rd$cpts' ++ [g|(g,s)<-gs, elem g cpts' || elem s cpts'] ++ [s|(g,s)<-gs, elem g cpts' || elem s cpts']
          cpts' = concs r
          dcls = [d | d@Sgn{}<-map makeDeclaration (mors r)]

numberListFrom :: [x] -> Int -> [(x,Int)]    --TODO Deze functie is te generiek om hier in deze module thuis te horen. Verplaatsen naar andere module? 
numberListFrom xs i = zip xs [i..]

 
-- Chapter 2: Formation of a graph.
dotG :: Options            -- ^ the flags 
     -> String             -- ^ the name of the Graph
     -> [Concept]           -- ^ The concepts to draw in the graph
     -> [Declaration Concept]      -- ^ The declarations, (the edges in the graph)
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
        numberedDeclarations :: [(Declaration Concept, Int)]
        numberedDeclarations = numberListFrom dcls 1
        numberedIsas         :: [((Concept, Concept), Int)]
        numberedIsas         = numberListFrom idgs 1

        baseNodeId :: Concept -> String  -- returns the NodeId of the node where edges to this node should connect to. 
        baseNodeId c 
            = case lookup c numberedConcepts of
                Just i -> "cpt_"++show i
                _      -> error ("!Fatal (module Graphics 161): element "++name c++" not found by nodeLabel.")

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
                            (Declaration Concept,Int)  -- ^ tuple contains the declaration and its rank
                         -> ([DotNode String],[DotEdge String])   -- ^ the resulting tuple contains the NodeStatements and EdgeStatements
        declarationNodesAndEdges (d,n)
             = (    [dclNameNode]    -- node to place the name of the declaration
               ,    [constrEdge (baseNodeId (source d)) (nodeID dclNameNode)   (DclSrcEdge d) True flags]     -- edge to connect the source with the hinge
                  ++[constrEdge (nodeID dclNameNode)   (baseNodeId (target d)) (DclTgtEdge d) True flags]     -- edge to connect the hinge to the target
               )
          where
        --    dclHingeNode   = constrNode ("dclHinge_"++show n) DclHingeNode   flags
            dclNameNode    = constrNode ("dclName_"++show n) (DclNameNode d) flags
            
        -- | This function constructs a list of NodeStatements that must be drawn for a concept.
        isaNodesAndEdges :: ((Concept, Concept),Int)  -- ^ tuple contains the declaration and its rank
                         -> ([DotNode String],[DotEdge String])   -- ^ the resulting tuple contains the NodeStatements and EdgeStatements
        isaNodesAndEdges ((g,s),_)
             = ( [] -- No node at all
               , [constrEdge (baseNodeId s) (baseNodeId g) IsaOnlyOneEdge True  flags] -- Just a single edge
               )

                        
constrNode :: a -> PictureObject -> Options -> DotNode a
constrNode nodeId pObj flags
  = DotNode { nodeID = nodeId
            , nodeAttributes = handleFlags pObj flags
            }
constrEdge :: a -> a -> PictureObject -> Bool -> Options -> DotEdge a
constrEdge nodeFrom nodeTo pObj isDirected' flags 
  = DotEdge { edgeFromNodeID = nodeFrom
            , edgeToNodeID   = nodeTo
            , edgeAttributes = handleFlags pObj flags
            , directedEdge   = isDirected'
            }
--DESCR -> a picture consists of arcs (relations), concepts, and ISA relations between concepts
--         arcs are attached to a source or target concept
--         arcs and concepts are points attached to a label
-- for Haddock support on GraphViz, click on:
--       http://hackage.haskell.org/packages/archive/graphviz/2999.6.0.0/doc/html/doc-index.html     or
--       http://hackage.haskell.org/packages/archive/graphviz/latest/doc/html/doc-index.html

data PictureObject = CptOnlyOneNode Concept             -- ^ Node of a concept that serves as connector and shows the name
                   | CptConnectorNode Concept           -- ^ Node of a concept that serves as connector of relations to that concept
                   | CptNameNode Concept                -- ^ Node of a concept that shows the name
                   | CptEdge                                -- ^ Edge of a concept to connect its nodes
                   | DclOnlyOneEdge (Declaration Concept) -- ^ Edge of a relation that connects to the source and the target
                   | DclSrcEdge     (Declaration Concept) -- ^ Edge of a relation that connects to the source
                   | DclTgtEdge     (Declaration Concept) -- ^ Edge of a relation that connects to the target
                   | DclNameNode    (Declaration Concept) -- ^ Node of a relation that shows the name
                   | IsaOnlyOneEdge                         -- ^ Edge of an ISA relation without a hinge node
                   | TotalPicture                           -- ^ Graph attributes
         
handleFlags :: PictureObject  -> Options -> [Attribute]
handleFlags po flags = 
    case po of
      CptConnectorNode c 
         -> if crowfoot flags
            then defaultNodeAtts ++ 
                 [ Label$StrLabel (name c)
                 , Shape PlainText
                 , filled --Style$Stl Filled Nothing
                 , URL (theURL flags c)
                 ]
            else [Shape PointShape, filled]--Style$Stl Filled Nothing,Width 0.1] --used to be something like: if crowfoot flags then doosje flags c else bolletje
      CptNameNode c  -> if crowfoot flags
                        then [Shape PointShape, invisible]--Style$Stl Invisible Nothing,Width 0.1]
                        else defaultNodeAtts ++
                             [ Label$StrLabel (name c)
                             , Shape PlainText
                             , filled --Style$Stl Filled Nothing
                             , URL (theURL flags c)
                             ]
      CptEdge    -> [Len 0.4, invisible]
      CptOnlyOneNode c -> defaultNodeAtts ++
                          [Label (StrLabel (name c))
                          , Shape PlainText
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
                          , Dir Both  -- Needed because of change in graphviz. See http://www.graphviz.org/bugs/b1951.html
                          , URL (theURL flags d)
                          ]
      DclSrcEdge d -> [Len 1.2
 --                     ,ArrowHead (AType [(open,Normal)])    -- Geeft de richting van de relatie aan.
                      ,ArrowHead ( if crowfoot flags
                                   then AType [(open,Normal)]
                                   else noArrow
                                 )
                      ,ArrowTail ( if crowfoot flags
                                   then crowfootArrowType False d
                                   else noArrow
                                 )
                      ,HeadClip False
                      , Dir Both  -- Needed because of change in graphviz. See http://www.graphviz.org/bugs/b1951.html
                      ]
      DclTgtEdge d -> [Len 1.2
                      , ArrowHead ( if crowfoot flags
                                   then crowfootArrowType True d
                                   else plainArrowType True d
                                 )
                      ,ArrowTail noArrow
                      ,TailClip False
                      ] 
      DclNameNode d -> defaultNodeAtts ++ 
                       [ Label (StrLabel (name d))
                       , Shape PlainText
                       , BgColor (X11Color White)
                       , URL (theURL flags d) 
                       ]
      IsaOnlyOneEdge-> [ Len 1.5
                       , ArrowHead vee
                       , ArrowTail noArrow
                       , dotted
                       ]
      TotalPicture -> [  Overlap RemoveOverlaps ]

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

crowfootArrowType :: Bool -> Declaration Concept -> ArrowType
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

plainArrowType :: Bool -> Declaration Concept -> ArrowType
plainArrowType isHead d
   = case isHead of 
       True -> if isFunction d
               then normal
               else AType [(open,Normal)] 
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
--transparent :: Attribute
--transparent = Color [X11Color Transparent]
--orange :: Attribute
--orange = Color [X11Color  Orange]
--yellow :: Attribute
--yellow = Color [X11Color  Yellow]
--purple :: Attribute
--purple = Color [X11Color  Purple]
--cyan :: Attribute
--cyan = Color [X11Color  Cyan]
--brown :: Attribute
--brown = Color [X11Color  Brown]
--cgreen :: Attribute
--cgreen = Color [X11Color  Green]
--cred :: Attribute
--cred = Color [X11Color  Red]

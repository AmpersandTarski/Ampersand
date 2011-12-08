{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec.Graphic.Graphics 
          (Dotable(..), makePictureObj, printDotGraph, DrawingType(..)
    --      ,GraphvizCommand(..)
    --      ,GraphvizOutput(..)
    --      ,runGraphvizCommand) 
    )where
-- TODO url links for atlas

import Data.GraphViz hiding (addExtension )
import DatabaseDesign.Ampersand.ADL1 
import DatabaseDesign.Ampersand.Fspec.Fspec
import DatabaseDesign.Ampersand.Classes 
import DatabaseDesign.Ampersand.Fspec.Switchboard
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Basics (fatalMsg,eqCl,Collection(..),Identified(..))
import DatabaseDesign.Ampersand.Fspec.Graphic.Picture
import DatabaseDesign.Ampersand.Fspec.Graphic.ClassDiagram (ClassDiag,classdiagram2dot)
import Data.List (nub)

fatal :: Int -> String -> a
fatal = fatalMsg "Fspec.Graphic.Graphics"

class Identified a => Navigatable a where
   interfacename :: a -> String
   itemstring :: a -> String 
   theURL :: Options -> a -> EscString    -- url of the web page in Atlas used when clicked on a node or edge in a .map file
   theURL flags x 
     = "Atlas.php?content=" ++ interfacename x
            ++  "&User=" ++ user
            ++  "&Script=" ++ script
            ++  "&"++interfacename x ++"="++qualify++itemstring x
      where --copied from atlas.hs
      script = fileName flags
      user = namespace flags
      qualify = "("++user ++ "." ++ script ++ ")"  


instance Navigatable A_Concept where
   interfacename _ = "Concept" --see Atlas.adl
   itemstring = name  --copied from atlas.hs
 
instance Navigatable Relation where 
   interfacename _ = "Relatiedetails"
   itemstring x = name x ++ "["
                  ++ (if source x==target x then name(source x) else name(source x)++"*"++name(target x))
                  ++ "]"

instance Navigatable Declaration where 
   interfacename _ = "Relatiedetails"
   itemstring x = name x ++ "["
                  ++ (if source x==target x then name(source x) else name(source x)++"*"++name(target x))
                  ++ "]"

data DrawingType
      = Plain_CG   -- Plain Conceptual graph. No frills
      | Rel_CG     -- Conceptual graph that focuses on relations
      | Gen_CG     -- Conceptual graph that focuses on generalizations

-- Chapter 1: All objects that can be transformed to a conceptual diagram are Dotable...
class Identified a => Dotable a where
   conceptualGraph :: Fspc
                      -> Options              -- the options
                      -> DrawingType          -- this parameter allows for different alternative graphs for the same a
                      -> a -> DotGraph String -- yields a function that maps a to a DotGraph
   -- makePicture is an abbreviation of three steps:
   --  1. conceptualGraph:  creates a DotGraph data structure
   --  2. printDotGraph:    makes a string, which is the contents of the dot-file for GraphViz
   --  3. makePictureObj:   creates a Picture data structure, containing the required metadata needed for production.
   makePicture :: Options -> Fspc -> DrawingType -> a -> Picture

instance Dotable ClassDiag where
   conceptualGraph _ _ _ _ = fatal 58 "TODO: ClassDiagram moet nog netjes naar nieuwe Graphviz worden verbouwd."
   makePicture flags _ _ cd =
          makePictureObj flags (name cd) PTClassDiagram (classdiagram2dot flags cd) 

instance Dotable A_Concept where
   conceptualGraph fSpec flags _ c = conceptual2Dot flags (name c) cpts rels idgs
         where 
          rs    = [r | r<-rules fSpec, c `elem` concs r, not (isaRule r)]
          idgs  = [(g,s) |(g,s)<-gs, elem g cpts' || elem s cpts']  --  all isa edges
          gs    = fsisa fSpec
-- TODO: removal of redundant isa edges might be done more efficiently
          cpts  = nub$cpts' ++ [g |(g,s)<-gs, elem g cpts' || elem s cpts'] ++ [s |(g,s)<-gs, elem g cpts' || elem s cpts']
          cpts' = concs rs
          rels  = [r | r<-(nub.map makeDeclaration.mors) rs   -- the use of "mors" restricts relations to those actually used in rs
                     , not (isProp r)    -- r is not a property
                     ]
   makePicture flags fSpec variant x =
          (makePictureObj flags (name x) PTConcept . printDotGraph . conceptualGraph fSpec flags variant) x

instance Dotable Pattern where
   conceptualGraph fSpec flags Plain_CG pat = conceptual2Dot flags (name pat) (cpts `uni` concs idgs) rels idgs
        where 
         --DESCR -> get concepts and arcs from pattern
          idgs = [(g,s) |(g,s)<-gs, g `elem` cpts, s `elem` cpts]    --  all isa edges within the concepts
                 `uni` [(g,s) |cl<-eqCl fst [(g,s) |(g,s)<-gs, g `elem` cpts || s `elem` cpts], length cl<4, (g,s)<-cl] -- related isas (if not too many)
          gs   = fsisa fSpec 
          cpts = concs pat `uni` concs rels
          rels = [r | r@Sgn{}<-(map makeDeclaration.mors) pat
                    , not (isProp r)    -- r is not a property
                    ]
   conceptualGraph fSpec flags Rel_CG pat = conceptual2Dot flags (name pat) (cpts `uni` concs idgs) rels idgs
        where 
         --DESCR -> get concepts and arcs from pattern
          idgs = [(g,s) |(g,s)<-gs, g `elem` cpts, s `elem` cpts]    --  all isa edges within the concepts
                 `uni` [(g,s) |cl<-eqCl fst [(g,s) |(g,s)<-gs, g `elem` cpts || s `elem` cpts], length cl<4, (g,s)<-cl] -- related isas (if not too many)
          gs   = fsisa fSpec 
          cpts = concs pat `uni` concs rels
          rels = [r | r@Sgn{}<-declarations pat
                    , not (isProp r), decusr r    -- r is not a property
                    ]
   conceptualGraph fSpec flags Gen_CG pat = conceptual2Dot flags (name pat) cpts [] edges
        where 
         --DESCR -> get concepts and arcs from pattern
          idgs  = [(g,s) |(g,s)<-gs, elem g cpts' || elem s cpts']  --  all isa edges
          gs    = fsisa fSpec 
          edges = clos gs idgs
          cpts  = concs edges
          cpts' = concs pat >- concs gs
          clos tuples ts = f (tuples>-ts) ts []
           where f  []  new result = result++new
                 f  _   []  result = result
                 f tups new result = f (tups>-new) [ t |t<-tups, (not.null) (concs t `isc` concs result') ] result'
                                     where result' = result++new
   makePicture flags fSpec variant pat =
          (makePictureObj flags (name pat) PTPattern . printDotGraph . conceptualGraph fSpec flags variant) pat

instance Dotable FProcess where
   conceptualGraph fSpec flags _ fproc = conceptual2Dot flags (name fproc) cpts rels idgs
        where 
         --DESCR -> get concepts and arcs from process
          idgs  = [(g,s) |(g,s)<-gs,  g `elem` cpts']  --  all isa edges
          gs    = fsisa fSpec 
          cpts  = nub(cpts' ++ [g |(g,_)<-idgs] ++ [s |(_,s)<-idgs])
          cpts' = concs (fpProc fproc)
          rels  = [r | r@Sgn{}<-(nub.map makeDeclaration.mors) (fpProc fproc), decusr r
                     , not (isProp r)    -- r is not a property
                     ]
   makePicture flags _ _ x =
          (makePictureObj flags (name x) PTProcess . printDotGraph . processModel) x
{- inspired by:
   makePicture flags _ _ cd =
          makePictureObj flags (name cd) PTClassDiagram (classdiagram2dot flags cd) -}

instance Dotable Activity where
   conceptualGraph fSpec flags _ ifc = conceptual2Dot flags (name ifc) cpts rels idgs
         where
         -- involve all rules from the specification that are affected by this interface
          rs         = [r | r<-rules fSpec, affected r]
          affected r = not (null (mors r `isc` mors ifc))
         -- involve all isa links from concepts touched by one of the affected rules
          idgs = [(g,s) |(g,s)<-gs, elem g cpts' || elem s cpts']  --  all isa edges
          gs   = fsisa fSpec
         -- involve all concepts involved either in the affected rules or in the isa-links
          cpts = nub $ cpts' ++ [c |(g,s)<-idgs, c<-[g,s]]
          cpts'  = concs rs
          rels = [r | r@Sgn{}<-(nub.map makeDeclaration.mors) ifc, decusr r
                    , not (isProp r)    -- r is not a property
                    ]
   makePicture flags fSpec variant x =
          (makePictureObj flags (name x) PTFinterface . printDotGraph . conceptualGraph fSpec flags variant) x

instance Dotable SwitchBdDiagram where
   conceptualGraph _ _ _ = sbdotGraph
   makePicture flags fSpec variant x =
          (makePictureObj flags (name x) PTSwitchBoard . printDotGraph . conceptualGraph fSpec flags variant) x

instance Dotable Rule where
   conceptualGraph fSpec flags _ r = conceptual2Dot flags (name r) cpts rels idgs
    where 
     idgs = [(g,s) | (g,s)<-fsisa fSpec
                   , g `elem` concs r || s `elem` concs r]  --  all isa edges
     cpts = nub $ concs r++[c |(g,s)<-idgs, c<-[g,s]]
     rels = [d | d@Sgn{}<-(nub.map makeDeclaration.mors) r, decusr d
               , not (isProp d)    -- d is not a property
               ]
   makePicture flags fSpec variant x =
          (makePictureObj flags (name x) PTRule . printDotGraph . conceptualGraph fSpec flags variant) x

-- Chapter 2: Formation of a conceptual graph as a DotGraph data structure.
conceptual2Dot :: Options                   -- ^ the flags 
               -> String                    -- ^ the name of the Graph
               -> [A_Concept]               -- ^ The concepts to draw in the graph
               -> [Declaration]   -- ^ The relations, (the edges in the graph)
               -> [(A_Concept, A_Concept)]  -- ^ list of Gen relations 
               -> DotGraph String           -- ^ The resulting DotGraph
conceptual2Dot flags graphName cpts' rels idgs
     = DotGraph { strictGraph = False
                , directedGraph = True
                , graphID = Just (Str graphName)
                , graphStatements 
                      = DotStmts { attrStmts = [GraphAttrs (handleFlags TotalPicture flags)]
                                 , subGraphs = []
                                 , nodeStmts = conceptNodes ++ declarationNodes
                                 , edgeStmts = declarationEdges ++ isaEdges
                                 }
                }
       where
        cpts = cpts' `uni` concs rels `uni` concs idgs
        conceptNodes = [constrNode (baseNodeId c) (CptOnlyOneNode c) flags | c<-cpts]
        (declarationNodes,declarationEdges) = (concat a, concat b) 
              where (a,b) = unzip [relationNodesAndEdges r | r<-zip rels [1..]]
        isaEdges = [constrEdge (baseNodeId s) (baseNodeId g) IsaOnlyOneEdge True  flags | (g,s)<-idgs]
              
        baseNodeId :: A_Concept -> String  -- returns the NodeId of the node where edges to this node should connect to. 
        baseNodeId c 
            = case lookup c (zip cpts [(1::Int)..]) of
                Just i -> "cpt_"++show i
                _      -> fatal 169 $ "element "++name c++" not found by nodeLabel."

        -- | This function constructs a list of NodeStatements that must be drawn for a concept.
        relationNodesAndEdges ::
             (Declaration,Int)           -- ^ tuple contains the declaration and its rank
          -> ([DotNode String],[DotEdge String]) -- ^ the resulting tuple contains the NodeStatements and EdgeStatements
        relationNodesAndEdges (r,n)
             = (  [ relNameNode ]    -- node to place the name of the relation
               ,  [ constrEdge (baseNodeId (source r)) (nodeID relNameNode)   (RelSrcEdge r) True flags     -- edge to connect the source with the hinge
                  , constrEdge (nodeID relNameNode)   (baseNodeId (target r)) (RelTgtEdge r) True flags]     -- edge to connect the hinge to the target
               )
          where
        --    relHingeNode   = constrNode ("relHinge_"++show n) RelHingeNode   flags
            relNameNode    = constrNode ("relName_"++show n) (RelNameNode r) flags
            
                                   
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

data PictureObject = CptOnlyOneNode A_Concept    -- ^ Node of a concept that serves as connector and shows the name
                   | CptConnectorNode A_Concept  -- ^ Node of a concept that serves as connector of relations to that concept
                   | CptNameNode A_Concept       -- ^ Node of a concept that shows the name
                   | CptEdge                     -- ^ Edge of a concept to connect its nodes
                   | RelOnlyOneEdge Declaration  -- ^ Edge of a relation that connects to the source and the target
                   | RelSrcEdge     Declaration  -- ^ Edge of a relation that connects to the source
                   | RelTgtEdge     Declaration  -- ^ Edge of a relation that connects to the target
                   | RelNameNode    Declaration  -- ^ Node of a relation that shows the name
                   | IsaOnlyOneEdge              -- ^ Edge of an ISA relation without a hinge node
                   | TotalPicture                -- ^ Graph attributes
         
handleFlags :: PictureObject  -> Options -> [Attribute]
handleFlags po flags = 
    case po of
      CptConnectorNode c 
         -> if crowfoot flags
            then defaultNodeAtts flags++ 
                 [ Label$StrLabel (name c)
                 , Shape PlainText
                 , filled --Style$Stl Filled Nothing
                 , URL (theURL flags c)
                 ]
            else [Shape PointShape, filled]--Style$Stl Filled Nothing,Width 0.1] --used to be something like: if crowfoot flags then doosje flags c else bolletje
      CptNameNode c  -> if crowfoot flags
                        then [Shape PointShape, invisible]--Style$Stl Invisible Nothing,Width 0.1]
                        else defaultNodeAtts flags++
                             [ Label$StrLabel (name c)
                             , Shape PlainText
                             , filled --Style$Stl Filled Nothing
                             , URL (theURL flags c)
                             ]
      CptEdge    -> [Len 0.4, invisible]
      CptOnlyOneNode c -> defaultNodeAtts flags++
                          [Label (StrLabel (name c))
                          , Shape PlainText
                          , filled 
                          , URL (theURL flags c)
                          ]
      RelOnlyOneEdge r -> [ Label (StrLabel (name r))
                          , ArrowHead (if crowfoot flags
                                       then crowfootArrowType True r
                                       else plainArrowType True r
                                      )
                          , ArrowTail (if crowfoot flags
                                       then crowfootArrowType False r
                                       else plainArrowType False r
                                      )
                          , Dir Both  -- Needed because of change in graphviz. See http://www.graphviz.org/bugs/b1951.html
                          , URL (theURL flags r)
                          ]
      RelSrcEdge r -> [Len 1.2
                      , ArrowHead ( if crowfoot flags  then noArrow                   else
                                    if isFunction r    then noArrow                   else
                                    if isInvFunction r then noArrow                   else
                                    noArrow
                                  )
                      , ArrowTail ( if crowfoot flags  then crowfootArrowType False r else
                                    if isFunction r    then noArrow                   else
                                    if isInvFunction r then normal                    else
                                    noArrow
                                  )
                      ,HeadClip False
                      , Dir Both  -- Needed because of change in graphviz. See http://www.graphviz.org/bugs/b1951.html
                      ]
      RelTgtEdge r -> [Len 1.2
                      , ArrowHead ( if crowfoot flags  then crowfootArrowType True r  else
                                    if isFunction r    then normal                    else
                                    if isInvFunction r then noArrow                   else
                                    noArrow
                                  )
                      , ArrowTail ( if crowfoot flags  then noArrow                   else
                                    if isFunction r    then noArrow                   else
                                    if isInvFunction r then AType [(noMod ,Inv)]      else
                                    AType [(noMod ,Inv)]
                                  )
                      , Dir Both  -- Needed because of change in graphviz. See http://www.graphviz.org/bugs/b1951.html
                      ,TailClip False
                      ] 
      RelNameNode r -> defaultNodeAtts flags++ 
                       [ Label (StrLabel (name r))
                       , Shape PlainText
                       , BgColor (X11Color White)
                       , URL (theURL flags r) 
                       ]
      IsaOnlyOneEdge-> [ Len 1.5
                       , ArrowHead (AType [(open,Normal)])
                       , ArrowTail noArrow
                       , if blackWhite flags then dotted else cRed
                       ]
      TotalPicture -> [  Overlap RemoveOverlaps ]

isInvFunction :: Declaration -> Bool
isInvFunction d = isInj d && isSur d

--DESCR -> default Node attributes
defaultNodeAtts :: Options -> [Attribute]
defaultNodeAtts flags = [FontSize 12, FontName (pangoFont flags)]

invisible :: Attribute
invisible = Style [SItem Invisible []]

dotted :: Attribute
dotted = Style [SItem Dotted []]

--dashed :: Attribute
--dashed = Style [SItem Dashed []]

filled :: Attribute
filled = Style [SItem Filled []]

crowfootArrowType :: Bool -> Declaration -> ArrowType
crowfootArrowType isHead r 
   = AType (case isHead of
               True  -> getCrowfootShape (isUni r) (isTot r)
               False -> getCrowfootShape (isInj r) (isSur r)
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
plainArrowType isHead r
   = case isHead of 
       True -> if isFunction r
               then noArrow
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
cRed :: Attribute
cRed = Color [X11Color  Red]

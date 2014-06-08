{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec.Graphic.Graphics 
          (makePicture
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
import Data.GraphViz.Attributes.Complete
import Data.List -- (nub)
import Data.String
--import Debug.Trace

fatal :: Int -> String -> a
fatal = fatalMsg "Fspec.Graphic.Graphics"

class Identified a => Navigatable a where
   interfacename :: a -> String
   itemstring :: a -> String 
   theURL :: Options -> a -> EscString    -- url of the web page in Atlas used when clicked on a node or edge in a .map file
   theURL flags x 
     = fromString ("Atlas.php?content=" ++ interfacename x
                   ++  "&User=" ++ user
                   ++  "&Script=" ++ script
                   ++  "&"++interfacename x ++"="++qualify++itemstring x
                  )
      where --copied from atlas.hs
      script = fileName flags
      user = namespace flags
      qualify = "("++user ++ "." ++ script ++ ")"  


instance Navigatable A_Concept where
   interfacename _ = "Concept" --see Atlas.adl
   itemstring = name  --copied from atlas.hs
 
instance Navigatable Declaration where 
   interfacename _ = "Relatiedetails"
   itemstring x = name x ++ "["
                  ++ (if source x==target x then name(source x) else name(source x)++"*"++name(target x))
                  ++ "]"

-- Chapter 1: All objects that can be transformed to a conceptual diagram are Dotable...
class Dotable a where
   conceptualGraph :: Fspc
                      -> Options              -- the options
                      -> PictType          -- this parameter allows for different alternative graphs for the same a
                      -> a -> DotGraph String -- yields a function that maps a to a DotGraph
   -- makePicture is an abbreviation of three steps:
   --  1. conceptualGraph:  creates a DotGraph data structure
   --  2. printDotGraph:    makes a string, which is the contents of the dot-file for GraphViz
   --  3. makePictureObj:   creates a Picture data structure, containing the required metadata needed for production.
   makePicture :: Options
               -> Fspc
               -> PictType
               -> a
               -> Picture
   pictureName :: PictType -> a -> String
{- This instance of Dotable is meant for drawing data models -}
instance Dotable ClassDiag where
   conceptualGraph _ _ _ = fatal 71 "ClassDiag is not ment to be drawn as a conceptualGraph"
   makePicture flags fSpec pt cd =
          makePictureObj flags (fsLang fSpec) nm pt (classdiagram2dot flags nm cd) 
     where nm = pictureName pt cd
   pictureName PTClassDiagram _ = "Classification"
   pictureName PTLogicalDM    _ = "LogicalDataModel"
   pictureName PTTechnicalDM  _ = "TechnicalDataModel"
   pictureName pt             _ = fatal 88 $ "undefined for "++show pt

instance Dotable A_Concept where
   conceptualGraph fSpec flags PTConcept c = conceptual2Dot flags (pictureName PTConcept c) cpts rels idgs
         where 
          rs    = [r | r<-udefrules fSpec, c `elem` concs r]
          idgs  = [(s,g) |(s,g)<-gs, elem g cpts' || elem s cpts']  --  all isa edges
          gs    = fsisa fSpec
-- TODO: removal of redundant isa edges might be done more efficiently
          cpts  = nub$cpts' ++ [g |(s,g)<-gs, elem g cpts' || elem s cpts'] ++ [s |(s,g)<-gs, elem g cpts' || elem s cpts']
          cpts' = concs rs
          rels  = [r | r@Sgn{} <- relsMentionedIn rs   -- the use of "relsMentionedIn" restricts relations to those actually used in rs
                     , not (isProp r)
                     ]
   makePicture flags fSpec PTConcept cpt =
          (makePictureObj flags (fsLang fSpec) nm PTConcept . conceptualGraph fSpec flags PTConcept) cpt
     where nm = "RulesWithConcept"++name cpt

instance Dotable Pattern where
   -- | The Plain_CG of pat makes a picture of at least the relations within pat; 
   --   extended with a limited number of more general concepts;
   --  and rels to prevent disconnected concepts, which can be connected given the entire context.
   conceptualGraph fSpec flags PTRelsUsedInPat pat = conceptual2Dot flags (pictureName PTRelsUsedInPat pat) cpts (rels `uni` xrels) idgs
        where 
         --DESCR -> get concepts and arcs from pattern
          idgs = [(s,g) |(s,g)<-gs, g `elem` cpts, s `elem` cpts]    --  all isa edges within the concepts
          gs   = fsisa fSpec 
          cpts = let cpts' = concs pat `uni` concs rels
                 in cpts' `uni` [g |cl<-eqCl id [g |(s,g)<-gs, s `elem` cpts'], length cl<3, g<-cl] -- up to two more general concepts
          rels = [r | r@Sgn{}<-relsMentionedIn pat
                    , not (isProp r)    -- r is not a property
                    ]
          -- extra rels to connect concepts without rels in this picture, but with rels in the fSpec
          xrels = let orphans = [c | c<-cpts, not(c `elem` map fst idgs || c `elem` map snd idgs || c `elem` map source rels  || c `elem` map target rels)]
                  in nub [r | c<-orphans, r@Sgn{}<-relsDefdIn fSpec
                        , (c == source r && target r `elem` cpts) || (c == target r  && source r `elem` cpts)
                        , source r /= target r, decusr r
                        ]
   -- | The Rel_CG of pat makes a picture of relations and gens within pat only 
   conceptualGraph fSpec flags PTDeclaredInPat pat
    = conceptual2Dot flags (pictureName PTDeclaredInPat pat) cpts rels idgs
        where 
         --DESCR -> get concepts and arcs from pattern
          idgs = [(s,g) |(s,g)<-gs, g `elem` cpts, s `elem` cpts]    --  all isa edges within the concepts
          gs   = fsisa fSpec
          decs = relsDefdIn pat `uni` relsMentionedIn (udefrules pat)
          cpts = concs decs `uni` concs (gens pat)
          rels = [r | r@Sgn{}<-decs
                    , not (isProp r), decusr r    -- r is not a property
                    ]
   conceptualGraph fSpec flags PTIsaInPattern pat = conceptual2Dot flags (pictureName PTIsaInPattern pat) cpts [] edges
        where 
         --DESCR -> get concepts and arcs from pattern
          idgs  = [(s,g) |(s,g)<-gs, elem g cpts' || elem s cpts']  --  all isa edges
          gs    = fsisa fSpec 
          edges = clos gs idgs
          cpts  = concs edges
          cpts' = concs pat >- concs gs
          clos tuples ts = f (tuples>-ts) ts []
           where f  []  new result = result++new
                 f  _   []  result = result
                 f tups new result = f (tups>-new) [ t |t<-tups, (not.null) (concs t `isc` concs result') ] result'
                                     where result' = result++new
   makePicture flags fSpec  pt pat =
          (makePictureObj flags (fsLang fSpec) nm pt . conceptualGraph fSpec flags pt) pat
     where nm = pictureName pt pat
   pictureName PTDeclaredInPat pat     = name pat++"RelationsInPattern"
   pictureName PTIsaInPattern pat= name pat++"IsasInPattern"
   pictureName PTRelsUsedInPat pat     = name pat++"RulesInPattern"
   pictureName pt _ = fatal 165 $ "undefined for "++show pt
   
instance Dotable FProcess where
   conceptualGraph fSpec flags pt fproc 
    = fatal 164 "conceptualGraph is currently undefined for processes. Should be integrated with Patterns"
   makePicture flags fSpec pt x =
          (makePictureObj flags (fsLang fSpec) nm pt . processModel) x
     where nm = pictureName pt x
   pictureName pt fp =
     case pt of
       PTProcess -> name fp++"ProcessModel"
       _         -> fatal 174 $ "undefined for "++show pt
instance Dotable Activity where
   conceptualGraph fSpec flags PTFinterface ifc = conceptual2Dot flags (pictureName PTFinterface ifc) cpts rels idgs
         where
         -- involve all rules from the specification that are affected by this interface
          rs         = [r | r<-udefrules fSpec, affected r]
          affected r = (not.null) [d | d@Sgn{} <- relsMentionedIn r `isc` relsMentionedIn ifc]
         -- involve all isa links from concepts touched by one of the affected rules
          idgs = [(s,g) |(s,g)<-gs, elem g cpts' || elem s cpts']  --  all isa edges
          gs   = fsisa fSpec
         -- involve all concepts involved either in the affected rules or in the isa-links
          cpts = nub $ cpts' ++ [c |(s,g)<-idgs, c<-[g,s]]
          cpts'  = concs rs
          rels = [r | r@Sgn{}<-relsMentionedIn ifc, decusr r
                    , not (isProp r)    -- r is not a property
                    ]
   makePicture flags fSpec PTFinterface act =
          (makePictureObj flags (fsLang fSpec) (pictureName PTFinterface act) PTFinterface . conceptualGraph fSpec flags PTFinterface) act
   makePicture _ _ pt _ = fatal 184 $ "undefined for "++show pt

   pictureName PTFinterface act = name act++"KnowledgeGraph"
   pictureName pt _ = fatal 207 $ "undefined for "++show pt

instance Dotable SwitchBdDiagram where
   conceptualGraph _ _ _ = fatal 210 $ "undefined."
   makePicture flags fSpec PTSwitchBoard x =
          (makePictureObj flags (fsLang fSpec) (pictureName PTSwitchBoard x) PTSwitchBoard . sbdotGraph) x
   makePicture _ _ pt _ = fatal 193 $ "undefined for "++show pt
   pictureName PTSwitchBoard x = name x ++"SwitchBoard"      
   pictureName pt _ = fatal 220 $ "undefined for "++show pt

instance Dotable Rule where
   conceptualGraph fSpec flags PTSingleRule r = conceptual2Dot flags (pictureName PTSingleRule r) cpts rels idgs
    where 
     idgs = [(s,g) | (s,g)<-fsisa fSpec
                   , g `elem` concs r || s `elem` concs r]  --  all isa edges
     cpts = nub $ concs r++[c |(s,g)<-idgs, c<-[g,s]]
     rels = [d | d@Sgn{}<-relsMentionedIn r, decusr d
               , not (isProp d)    -- d is not a property
               ]
   makePicture flags fSpec PTSingleRule rul =
          (makePictureObj flags (fsLang fSpec) (pictureName PTSingleRule rul) PTSingleRule . conceptualGraph fSpec flags PTSingleRule) rul

   makePicture _ _ pt _   = fatal 219 $ "undefined for "++show pt
   pictureName PTSingleRule r = "SingleRule"++name r
   pictureName pt _ = fatal 238 $ "undefined for "++show pt
-- Chapter 2: Formation of a conceptual graph as a DotGraph data structure.
conceptual2Dot :: Options                   -- ^ the flags 
               -> String                    -- ^ the name of the Graph
               -> [A_Concept]               -- ^ The concepts to draw in the graph
               -> [Declaration]   -- ^ The relations, (the edges in the graph)
               -> [(A_Concept, A_Concept)]  -- ^ list of Isa relations 
               -> DotGraph String           -- ^ The resulting DotGraph
conceptual2Dot flags graphName cpts' rels idgs
     = DotGraph { strictGraph = False
                , directedGraph = True
                , graphID = Just (Str (fromString graphName))
                , graphStatements 
                      = DotStmts { attrStmts = [GraphAttrs (handleFlags TotalPicture flags)]
                                 , subGraphs = []
                                 , nodeStmts = conceptNodes ++ relationNodes
                                 , edgeStmts = relationEdges ++ isaEdges
                                 }
                }
       where
        cpts = cpts' `uni` concs rels `uni` concs idgs
        conceptNodes = [constrNode (baseNodeId c) (CptOnlyOneNode c) flags | c<-cpts]
        (relationNodes,relationEdges) = (concat a, concat b) 
              where (a,b) = unzip [relationNodesAndEdges r | r<-zip rels [1..]]
        isaEdges = [constrEdge (baseNodeId s) (baseNodeId g) IsaOnlyOneEdge  flags | (s,g)<-idgs]
              
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
          | doubleEdges flags
             = (  [ relNameNode ]    -- node to place the name of the relation
               ,  [ constrEdge (baseNodeId (source r)) (nodeID relNameNode)   (RelSrcEdge r) flags     -- edge to connect the source with the hinge
                  , constrEdge (nodeID relNameNode)   (baseNodeId (target r)) (RelTgtEdge r) flags]     -- edge to connect the hinge to the target
               )
          | otherwise
               = ( [] --No intermediate node
                 , [constrEdge (baseNodeId (source r)) (baseNodeId (target r)) (RelOnlyOneEdge r)  flags]
                 )
          where
        --    relHingeNode   = constrNode ("relHinge_"++show n) RelHingeNode   flags
            relNameNode    = constrNode ("relName_"++show n) (RelIntermediateNode r) flags
                                   
constrNode :: a -> PictureObject -> Options -> DotNode a
constrNode nodeId pObj flags
  = DotNode { nodeID = nodeId
            , nodeAttributes = [ FontSize 10
                               , FontName (fromString(pangoFont flags))
                           --    , Width 0.1
                           --    , Height  0.1
                               ]++handleFlags pObj flags
            }

constrEdge :: a -> a -> PictureObject -> Options -> DotEdge a
constrEdge nodeFrom nodeTo pObj flags 
  = DotEdge { fromNode = nodeFrom
            , toNode   = nodeTo
            , edgeAttributes = [ FontSize 12
                               , FontName (fromString(pangoFont flags))
                               , Dir Forward
                            --   , LabelAngle (-25.0)
                               , Color [WC(X11Color Gray35)Nothing]
                               , LabelFontColor (X11Color Black)
                               , LabelFloat False
                               , Decorate False
                            --   , LabelDistance 2.0
                            --   , (HeadLabel . StrLabel . fromString) "Test"
                               ]++handleFlags pObj flags
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
                   | RelIntermediateNode    Declaration  -- ^ Intermediate node, as a hindge for the relation edges
                   | IsaOnlyOneEdge              -- ^ Edge of an ISA relation without a hinge node
                   | TotalPicture                -- ^ Graph attributes
         
handleFlags :: PictureObject  -> Options -> [Attribute]
handleFlags po flags = 
    case po of
      CptConnectorNode c 
         -> if crowfoot flags
            then 
                 [ (Label . StrLabel . fromString . name) c
                 , Shape PlainText
                 , Style [filled]
                 , URL (theURL flags c)
                 ]
            else [ Shape PointShape
                 , Style [filled]
                 ]
      CptNameNode c  -> if crowfoot flags
                        then [ Shape PointShape
                             , Style [invis]]
                        else 
                             [ (Label . StrLabel . fromString . name) c
                             , Shape PlainText
                             , Style [filled]
                             , URL (theURL flags c)
                             ]
      CptEdge    -> [Style [invis]
                    ]
      CptOnlyOneNode c -> 
                          [(Label . StrLabel . fromString . name) c
                          , Shape BoxShape
                          , Style [filled] 
                          , URL (theURL flags c)
                          ]
      RelOnlyOneEdge r -> [ URL (theURL flags r)
                          , (XLabel . StrLabel .fromString.name) r
                          ]
                    --    ++[ (HeadLabel . StrLabel .fromString) "1" | isTot r && isUni r]
                    --    ++[ (TailLabel . StrLabel .fromString) "1" | isSur r && isInj r]
                        ++[ ArrowTail noArrow, ArrowHead noArrow
                          , Dir Forward  -- Note that the tail arrow is not supported , so no crowfoot notation possible with a single edge.
                          , Style [SItem Tapered []] , PenWidth 5
                          ]
      RelSrcEdge r -> [ ArrowHead ( if crowfoot flags  then normal                    else
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
                --      , Dir Both  -- Needed because of change in graphviz. See http://www.graphviz.org/bugs/b1951.html
                      ]
      RelTgtEdge r -> [ (Label . StrLabel . fromString . name) r
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
                   --   , Dir Both  -- Needed because of change in graphviz. See http://www.graphviz.org/bugs/b1951.html
                      ,TailClip False
                      ] 
      RelIntermediateNode r -> 
                       [ Label (StrLabel (fromString("")))
                       , Shape PlainText
                       , bgColor White
                       , URL (theURL flags r) 
                       ]
      IsaOnlyOneEdge-> [ ArrowHead (AType [(open,Normal)])
                       , ArrowTail noArrow
                       , if blackWhite flags then Style [dotted] else Color [WC(X11Color Red)Nothing]
                       ]
      TotalPicture -> [ Sep (DVal (if doubleEdges flags then 1/2 else 2)) -- The minimal amount of whitespace between nodes
                      , OutputOrder  EdgesFirst --make sure the nodes are always on top...
                      , Overlap ScaleXYOverlaps
                      , Splines PolyLine  -- SplineEdges could work as well.
                      , Landscape False
                      ]

isInvFunction :: Declaration -> Bool
isInvFunction d = isInj d && isSur d


crowfootArrowType :: Bool -> Declaration -> ArrowType
crowfootArrowType isHead r 
   = AType (case isHead of
               True  -> getCrowfootShape (isUni r) (isTot r)
               False -> getCrowfootShape (isInj r) (isSur r)
           )
       where 
         getCrowfootShape :: Bool -> Bool -> [( ArrowModifier , ArrowShape )]
         getCrowfootShape a b =
           case (a,b) of
            (True ,True ) -> [my_tee          ]
            (False,True ) -> [my_crow, my_tee ]
            (True ,False) -> [my_odot, my_tee ]
            (False,False) -> [my_crow, my_odot]
             
         my_tee :: ( ArrowModifier , ArrowShape )
         my_tee = ( noMod , Tee )
         my_odot :: ( ArrowModifier , ArrowShape )
         my_odot= ( open, DotArrow )
         my_crow :: ( ArrowModifier , ArrowShape )
         my_crow= ( open, Crow )

noMod :: ArrowModifier
noMod = ArrMod { arrowFill = FilledArrow
               , arrowSide = BothSides
               }
open :: ArrowModifier
open  = noMod {arrowFill = OpenArrow}

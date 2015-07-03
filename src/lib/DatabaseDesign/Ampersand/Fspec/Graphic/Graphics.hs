{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec.Graphic.Graphics 
          (makePicture, writePicture, Picture(..), PictureReq(..),imagePath
    )where

import Data.GraphViz
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Fspec.Fspec
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Fspec.Switchboard
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Fspec.Graphic.ClassDiagram -- (ClassDiag,classdiagram2dot)
import Data.GraphViz.Attributes.Complete
import Data.List
import Data.String

import System.FilePath hiding (addExtension)
import System.Directory

fatal :: Int -> String -> a
fatal = fatalMsg "Fspc.Graphic.Graphics"

data PictureReq = PTClassDiagram
                | PTRelsUsedInPat Pattern
                | PTDeclaredInPat Pattern
                | PTProcess FProcess
                | PTConcept A_Concept
                | PTSwitchBoard Activity
                | PTFinterface Activity
                | PTIsaInPattern Pattern  -- Not used at all...
                | PTSingleRule Rule
                | PTLogicalDM
                | PTTechnicalDM

data Picture = Pict { pType :: PictureReq             -- ^ the type of the picture
                    , scale :: String                 -- ^ a scale factor, intended to pass on to LaTeX, because Pandoc seems to have a problem with scaling.
                    , dotSource :: DotGraph String    -- ^ the string representing the .dot
                    , dotProgName :: GraphvizCommand  -- ^ the name of the program to use  ("dot" or "neato" or "fdp" or "Sfdp")
                    , caption :: String               -- ^ a human readable name of this picture
                    }

makePicture :: Fspc -> PictureReq -> Picture
makePicture fSpec pr = undefined

pictureID :: PictureReq -> String
pictureID pr =
     case pr of
      PTClassDiagram   -> "Classification"
      PTLogicalDM      -> "LogicalDataModel"
      PTTechnicalDM    -> "TechnicalDataModel"
      PTConcept cpt    -> "RulesWithConcept"++name cpt
      PTDeclaredInPat pat -> "RelationsInPattern"++name pat
      PTIsaInPattern  pat -> "IsasInPattern"++name pat
      PTRelsUsedInPat pat -> "RulesInPattern"++name pat
      PTProcess fp        -> "ProcessModel"++name fp
      PTFinterface act    -> "KnowledgeGraph"++name act
      PTSwitchBoard x     -> "SwitchBoard"++name x
      PTSingleRule r      -> "SingleRule"++name r

conceptualGraph' :: Fspc -> PictureReq -> DotGraph String
conceptualGraph' fSpec pr = undefined

theURL = undefined

writePicture :: Options -> Picture -> IO()
writePicture opts pict
    = undefined

class ReferableFromPandoc a where
  imagePath :: Options -> a -> FilePath   -- ^ the full file path to the image file

instance ReferableFromPandoc Picture where
  imagePath opts p =
     (if genAtlas opts then dirPrototype opts </> "images" else dirOutput opts)
     </> (escapeNonAlphaNum . pictureID . pType ) p <.> "png"


data ConceptualStructure = CStruct { csCpts :: [A_Concept]               -- ^ The concepts to draw in the graph
                                   , csRels :: [Declaration]   -- ^ The relations, (the edges in the graph)
                                   , csIdgs :: [(A_Concept, A_Concept)]  -- ^ list of Isa relations
                                   }

conceptual2Dot :: Options -> ConceptualStructure -> DotGraph String
conceptual2Dot opts (CStruct cpts' rels idgs)
     = DotGraph { strictGraph = False
                , directedGraph = True
                , graphID = Nothing
                , graphStatements
                      = DotStmts { attrStmts = [GraphAttrs (handleFlags TotalPicture opts)]
                                 , subGraphs = []
                                 , nodeStmts = conceptNodes ++ relationNodes
                                 , edgeStmts = relationEdges ++ isaEdges
                                 }
                }
       where
        cpts = cpts' `uni` concs rels `uni` concs idgs
        conceptNodes = [constrNode (baseNodeId c) (CptOnlyOneNode c) opts | c<-cpts]
        (relationNodes,relationEdges) = (concat a, concat b)
              where (a,b) = unzip [relationNodesAndEdges r | r<-zip rels [1..]]
        isaEdges = [constrEdge (baseNodeId s) (baseNodeId g) IsaOnlyOneEdge opts | (s,g)<-idgs]

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
          | doubleEdges opts
             = (  [ relNameNode ]    -- node to place the name of the relation
               ,  [ constrEdge (baseNodeId (source r)) (nodeID relNameNode)   (RelSrcEdge r) opts     -- edge to connect the source with the hinge
                  , constrEdge (nodeID relNameNode)   (baseNodeId (target r)) (RelTgtEdge r) opts]     -- edge to connect the hinge to the target
               )
          | otherwise
               = ( [] --No intermediate node
                 , [constrEdge (baseNodeId (source r)) (baseNodeId (target r)) (RelOnlyOneEdge r)  opts]
                 )
          where
        --    relHingeNode   = constrNode ("relHinge_"++show n) RelHingeNode   opts
            relNameNode    = constrNode ("relName_"++show n) (RelIntermediateNode r) opts

constrNode :: a -> PictureObject -> Options -> DotNode a
constrNode nodeId pObj opts
  = DotNode { nodeID = nodeId
            , nodeAttributes = [ FontSize 10
                               , FontName (fromString(pangoFont opts))
                           --    , Width 0.1
                           --    , Height  0.1
                               ]++handleFlags pObj opts
            }

constrEdge :: a -> a -> PictureObject -> Options -> DotEdge a
constrEdge nodeFrom nodeTo pObj opts
  = DotEdge { fromNode = nodeFrom
            , toNode   = nodeTo
            , edgeAttributes = [ FontSize 12
                               , FontName (fromString(pangoFont opts))
                               , Dir Forward
                            --   , LabelAngle (-25.0)
                               , Color [WC(X11Color Gray35)Nothing]
                               , LabelFontColor (X11Color Black)
                               , LabelFloat False
                               , Decorate False
                            --   , LabelDistance 2.0
                            --   , (HeadLabel . StrLabel . fromString) "Test"
                               ]++handleFlags pObj opts
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
handleFlags po opts =
    case po of
      CptConnectorNode c
         -> if crowfoot opts
            then
                 [ (Label . StrLabel . fromString . name) c
                 , Shape PlainText
                 , Style [filled]
                 , URL (theURL opts c)
                 ]
            else [ Shape PointShape
                 , Style [filled]
                 ]
      CptNameNode c  -> if crowfoot opts
                        then [ Shape PointShape
                             , Style [invis]]
                        else
                             [ (Label . StrLabel . fromString . name) c
                             , Shape PlainText
                             , Style [filled]
                             , URL (theURL opts c)
                             ]
      CptEdge    -> [Style [invis]
                    ]
      CptOnlyOneNode c ->
                          [(Label . StrLabel . fromString . name) c
                          , Shape BoxShape
                          , Style [filled]
                          , URL (theURL opts c)
                          ]
      RelOnlyOneEdge r -> [ URL (theURL opts r)
                          , (XLabel . StrLabel .fromString.name) r
                          ]
                    --    ++[ (HeadLabel . StrLabel .fromString) "1" | isTot r && isUni r]
                    --    ++[ (TailLabel . StrLabel .fromString) "1" | isSur r && isInj r]
                        ++[ ArrowTail noArrow, ArrowHead noArrow
                          , Dir Forward  -- Note that the tail arrow is not supported , so no crowfoot notation possible with a single edge.
                          , Style [SItem Tapered []] , PenWidth 5
                          ]
      RelSrcEdge r -> [ ArrowHead ( if crowfoot opts  then normal                    else
                                    if isFunction r    then noArrow                   else
                                    if isInvFunction r then noArrow                   else
                                    noArrow
                                  )
                      , ArrowTail ( if crowfoot opts  then crowfootArrowType False r else
                                    if isFunction r    then noArrow                   else
                                    if isInvFunction r then normal                    else
                                    noArrow
                                  )
                      ,HeadClip False
                --      , Dir Both  -- Needed because of change in graphviz. See http://www.graphviz.org/bugs/b1951.html
                      ]
      RelTgtEdge r -> [ (Label . StrLabel . fromString . name) r
                      , ArrowHead ( if crowfoot opts  then crowfootArrowType True r  else
                                    if isFunction r    then normal                    else
                                    if isInvFunction r then noArrow                   else
                                    noArrow
                                  )
                      , ArrowTail ( if crowfoot opts  then noArrow                   else
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
                       , URL (theURL opts r)
                       ]
      IsaOnlyOneEdge-> [ ArrowHead (AType [(open,Normal)])
                       , ArrowTail noArrow
                       , if blackWhite opts then Style [dotted] else Color [WC(X11Color Red)Nothing]
                       ]
      TotalPicture -> [ Sep (DVal (if doubleEdges opts then 1/2 else 2)) -- The minimal amount of whitespace between nodes
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

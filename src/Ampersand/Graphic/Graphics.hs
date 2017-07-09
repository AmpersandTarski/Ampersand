module Ampersand.Graphic.Graphics
          (makePicture, writePicture, Picture(..), PictureReq(..),imagePath
    )where

import Data.GraphViz
import Ampersand.ADL1
import Ampersand.FSpec.FSpec
import Ampersand.Misc
import Ampersand.Basics
import Ampersand.Classes
import Ampersand.Graphic.Fspec2ClassDiagrams
import Ampersand.Graphic.ClassDiag2Dot
import Data.GraphViz.Attributes.Complete
import Data.List
import Data.String

import System.FilePath hiding (addExtension)
import System.Directory
import System.Process (callCommand)
import Control.Exception (catch, IOException)
import Prelude hiding (writeFile)

data PictureReq = PTClassDiagram
                | PTCDPattern Pattern
                | PTDeclaredInPat Pattern
                | PTCDConcept A_Concept
                | PTCDRule Rule
                | PTLogicalDM
                | PTTechnicalDM

data Picture = Pict { pType :: PictureReq             -- ^ the type of the picture
                    , scale :: String                 -- ^ a scale factor, intended to pass on to LaTeX, because Pandoc seems to have a problem with scaling.
                    , dotSource :: DotGraph String    -- ^ the string representing the .dot
                    , dotProgName :: GraphvizCommand  -- ^ the name of the program to use  ("dot" or "neato" or "fdp" or "Sfdp")
                    , caption :: String               -- ^ a human readable name of this picture
                    }

makePicture :: FSpec -> PictureReq -> Picture
makePicture fSpec pr =
  case pr of
   PTClassDiagram      -> Pict { pType = pr
                               , scale = scale'
                               , dotSource = classdiagram2dot (getOpts fSpec) (clAnalysis fSpec)
                               , dotProgName = Dot
                               , caption =
                                   case fsLang fSpec of
                                      English -> "Classification of " ++ name fSpec
                                      Dutch   -> "Classificatie van " ++ name fSpec
                               }
   PTLogicalDM         -> Pict { pType = pr
                               , scale = scale'
                               , dotSource = classdiagram2dot (getOpts fSpec) (cdAnalysis fSpec)
                               , dotProgName = Dot
                               , caption =
                                   case fsLang fSpec of
                                      English -> "Logical data model of " ++ name fSpec
                                      Dutch   -> "Logisch gegevensmodel van " ++ name fSpec
                               }
   PTTechnicalDM       -> Pict { pType = pr
                               , scale = scale'
                               , dotSource = classdiagram2dot (getOpts fSpec) (tdAnalysis fSpec)
                               , dotProgName = Dot
                               , caption =
                                   case fsLang fSpec of
                                      English -> "Technical data model of " ++ name fSpec
                                      Dutch   -> "Technisch gegevensmodel van " ++ name fSpec
                               }
   PTCDConcept cpt     -> Pict { pType = pr
                               , scale = scale'
                               , dotSource = conceptualGraph' fSpec pr
                               , dotProgName = graphVizCmdForConceptualGraph
                               , caption =
                                   case fsLang fSpec of
                                      English -> "Concept diagram of the rules about " ++ name cpt
                                      Dutch   -> "Conceptueel diagram van de regels rond " ++ name cpt
                               }
   PTDeclaredInPat pat -> Pict { pType = pr
                               , scale = scale'
                               , dotSource = conceptualGraph' fSpec pr
                               , dotProgName = graphVizCmdForConceptualGraph
                               , caption =
                                   case fsLang fSpec of
                                      English -> "Concept diagram of relations in " ++ name pat
                                      Dutch   -> "Conceptueel diagram van relaties in " ++ name pat
                               }
   PTCDPattern pat     -> Pict { pType = pr
                               , scale = scale'
                               , dotSource = conceptualGraph' fSpec pr
                               , dotProgName = graphVizCmdForConceptualGraph
                               , caption =
                                   case fsLang fSpec of
                                      English -> "Concept diagram of the rules in " ++ name pat
                                      Dutch   -> "Conceptueel diagram van de regels in " ++ name pat
                               }
   PTCDRule rul        -> Pict { pType = pr
                               , scale = scale'
                               , dotSource = conceptualGraph' fSpec pr
                               , dotProgName = graphVizCmdForConceptualGraph
                               , caption =
                                   case fsLang fSpec of
                                      English -> "Concept diagram of rule " ++ name rul
                                      Dutch   -> "Conceptueel diagram van regel " ++ name rul
                               }
 where
   scale' =
      case pr of
            PTClassDiagram -> "1.0"
            PTCDPattern{}-> "0.7"
            PTDeclaredInPat{}-> "0.6"
            PTCDRule{}   -> "0.7"
            PTCDConcept{}      -> "0.7"
            PTLogicalDM    -> "1.2"
            PTTechnicalDM  -> "1.2"
   graphVizCmdForConceptualGraph = Fdp -- (don't use Sfdp, because it causes a bug in linux. see http://www.graphviz.org/content/sfdp-graphviz-not-built-triangulation-library)

pictureID :: PictureReq -> String
pictureID pr =
     case pr of
      PTClassDiagram   -> "Classification"
      PTLogicalDM      -> "LogicalDataModel"
      PTTechnicalDM    -> "TechnicalDataModel"
      PTCDConcept cpt     -> "CDConcept"++name cpt
      PTDeclaredInPat pat -> "RelationsInPattern"++name pat
      PTCDPattern pat     -> "CDPattern"++name pat
      PTCDRule r          -> "CDRule"++name r

conceptualGraph' :: FSpec -> PictureReq -> DotGraph String
conceptualGraph' fSpec pr = conceptual2Dot (getOpts fSpec) cstruct
  where
    cstruct =
      case pr of
        PTCDConcept c ->
          let gs = fsisa fSpec
              cpts' = concs rs
              rs    = [r | r<-vrules fSpec, c `elem` concs r]
          in
          CStruct { csCpts = nub$cpts' ++ [g |(s,g)<-gs, elem g cpts' || elem s cpts'] ++ [s |(s,g)<-gs, elem g cpts' || elem s cpts']
                  , csRels = [r | r@Sgn{} <- relsMentionedIn rs   -- the use of "relsMentionedIn" restricts relations to those actually used in rs
                             , not (isProp r)
                             ]
                  , csIdgs = [(s,g) |(s,g)<-gs, elem g cpts' || elem s cpts']  --  all isa edges
                  }
        --  PTCDPattern makes a picture of at least the relations within pat;
        --  extended with a limited number of more general concepts;
        --  and rels to prevent disconnected concepts, which can be connected given the entire context.
        PTCDPattern pat ->
          let orphans = [c | c<-cpts, not(c `elem` map fst idgs || c `elem` map snd idgs || c `elem` map source rels  || c `elem` map target rels)]
              xrels = nub [r | c<-orphans, r@Sgn{}<-vrels fSpec
                        , (c == source r && target r `elem` cpts) || (c == target r  && source r `elem` cpts)
                        , source r /= target r, decusr r
                        ]
              idgs = [(s,g) |(s,g)<-gs, g `elem` cpts, s `elem` cpts]    --  all isa edges within the concepts
              gs   = fsisa fSpec
              cpts = cpts' `uni` [g |cl<-eqCl id [g |(s,g)<-gs, s `elem` cpts'], length cl<3, g<-cl] -- up to two more general concepts
              cpts' = concs pat `uni` concs rels
              rels = [r | r@Sgn{}<-relsMentionedIn pat
                             , not (isProp r)    -- r is not a property
                             ]
          in
          CStruct { csCpts = cpts' `uni` [g |cl<-eqCl id [g |(s,g)<-gs, s `elem` cpts'], length cl<3, g<-cl] -- up to two more general concepts
                  , csRels = rels `uni` xrels -- extra rels to connect concepts without rels in this picture, but with rels in the fSpec
                  , csIdgs = idgs
                  }

        -- PTDeclaredInPat makes a picture of relations and gens within pat only
        PTDeclaredInPat pat ->
          let gs   = fsisa fSpec
              cpts = concs decs `uni` concs (gens pat)
              decs = relsDefdIn pat `uni` relsMentionedIn (udefrules pat)
          in
          CStruct { csCpts = cpts
                  , csRels = [r | r@Sgn{}<-decs
                             , not (isProp r), decusr r    -- r is not a property
                             ]
                  , csIdgs = [(s,g) |(s,g)<-gs, g `elem` cpts, s `elem` cpts]    --  all isa edges within the concepts
                  }

        PTCDRule r ->
          let idgs = [(s,g) | (s,g)<-fsisa fSpec
                     , g `elem` concs r || s `elem` concs r]  --  all isa edges
          in
          CStruct { csCpts = nub $ concs r++[c |(s,g)<-idgs, c<-[g,s]]
                  , csRels = [d | d@Sgn{}<-relsMentionedIn r, decusr d
                             , not (isProp d)    -- d is not a property
                             ]
                  , csIdgs = idgs -- involve all isa links from concepts touched by one of the affected rules
                  }
        _  -> fatal 276 "No conceptual graph defined for this type."

writePicture :: Options -> Picture -> IO()
writePicture opts pict
    = sequence_ (
      [createDirectoryIfMissing True  (takeDirectory (imagePath opts pict)) ]++
   --   [dumpShow ]++
   --   [writeDot Canon  | genFSpec opts ]++  --Pretty-printed Dot output with no layout performed.
      [writeDot Png    | genFSpec opts ] ++  --handy format to include in github comments/issues
      [writeDot Svg    | genFSpec opts ] ++ -- format that is used when docx docs are being generated.
      [writePdf Eps    | genFSpec opts ] -- .eps file that is postprocessed to a .pdf file 
          )
   where
     writeDot :: GraphvizOutput -> IO ()
     writeDot = writeDotPostProcess Nothing
     writeDotPostProcess :: Maybe (FilePath -> IO ()) --Optional postprocessor
              -> GraphvizOutput
              -> IO ()
     writeDotPostProcess postProcess gvOutput  =
         do verboseLn opts ("Generating "++show gvOutput++" using "++show gvCommand++".")
            path <- addExtension (runGraphvizCommand gvCommand (dotSource pict)) gvOutput ((dropExtension . imagePath opts) pict)
            verboseLn opts (path++" written.")
            case postProcess of
              Nothing -> return ()
              Just x -> x path
       where  gvCommand = dotProgName pict
     -- The GraphVizOutput Pdf generates pixelised graphics on Linux
     -- the GraphVizOutput Eps generates extended postscript that can be postprocessed to PDF.
     makePdf :: FilePath -> IO ()
     makePdf path = do
         callCommand (ps2pdfCmd path)
         verboseLn opts (replaceExtension path ".pdf" ++ " written.")
       `catch` \ e -> verboseLn opts ("Could not invoke PostScript->PDF conversion."++
                                      "\n  Did you install MikTex? Can the command epstopdf be found?"++
                                      "\n  Your error message is:\n " ++ show (e :: IOException))
                   
     writePdf :: GraphvizOutput
              -> IO ()
     writePdf x = (writeDotPostProcess (Just makePdf) x)
       `catch` (\ e -> verboseLn opts ("Something went wrong while creating your Pdf."++  --see issue at https://github.com/AmpersandTarski/RAP/issues/21
                                       "\n  Your error message is:\n " ++ show (e :: IOException)))
     ps2pdfCmd path = "epstopdf " ++ path  -- epstopdf is installed in miktex.  (package epspdfconversion ?)

class ReferableFromPandoc a where
  imagePath :: Options -> a -> FilePath   -- ^ the full file path to the image file

instance ReferableFromPandoc Picture where
  imagePath opts p =
     dirOutput opts
     </> (escapeNonAlphaNum . pictureID . pType ) p <.> "svg"

{-
class Named a => Navigatable a where
   interfacename :: a -> String
   itemstring :: a -> String
   theURL :: Options -> a -> EscString    -- url of the web page in Atlas used when clicked on a node or edge in a .map file
   theURL _ x = fromString $ "HIER KAN EEN URL WORDEN GEBOUWD voor "++ interfacename x++" "++itemstring x
--     = fromString ("Atlas.php?content=" ++ interfacename x
--                   ++  "&User=" ++ user
--                   ++  "&Script=" ++ script
--                   ++  "&"++interfacename x ++"="++qualify++itemstring x
--                  )
--      where --copied from atlas.hs
--      script = fileName opts
--      user = namespace opts
--      qualify = "("++user ++ "." ++ script ++ ")"

instance Navigatable A_Concept where
   interfacename _ = "Concept" --see Atlas.adl
   itemstring = name  --copied from atlas.hs

instance Navigatable Declaration where
   interfacename _ = "Relatiedetails"
   itemstring x = name x ++ "["
                  ++ (if source x==target x then name(source x) else name(source x)++"*"++name(target x))
                  ++ "]"
-}

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
                               , FontName (fromString "sans")
                           --    , Width 0.1
                           --    , Height  0.1
                               ]++handleFlags pObj opts
            }

constrEdge :: a -> a -> PictureObject -> Options -> DotEdge a
constrEdge nodeFrom nodeTo pObj opts
  = DotEdge { fromNode = nodeFrom
            , toNode   = nodeTo
            , edgeAttributes = [ FontSize 12
                               , FontName (fromString "sans")
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
             --    , URL (theURL opts c)
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
               --              , URL (theURL opts c)
                             ]
      CptEdge    -> [Style [invis]
                    ]
      CptOnlyOneNode c ->
                          [(Label . StrLabel . fromString . name) c
                          , Shape BoxShape
                          , Style [filled]
                    --      , URL (theURL opts c)
                          ]
      RelOnlyOneEdge r ->  (XLabel . StrLabel .fromString.name) r
                       --   , URL (theURL opts r)
                          :
                          [ ArrowTail noArrow, ArrowHead noArrow
                          , Dir Forward  -- Note that the tail arrow is not supported , so no crowfoot notation possible with a single edge.
                          , Style [SItem Tapered []] , PenWidth 5
                          ]
      RelSrcEdge r -> [ ArrowHead ( if crowfoot opts   then normal                    else
                                    if isFunction r    then noArrow                   else
                                    directionArrow
                                  )
                      , ArrowTail ( if crowfoot opts   then crowfootArrowType False r else
                                    if isFunction r    then noArrow                   else
                                    if isInvFunction r then normal                    else
                                    noArrow
                                  )
                      ,HeadClip False
                      ]
      RelTgtEdge r -> [ (Label . StrLabel . fromString . name) r
                      , ArrowHead ( if crowfoot opts   then crowfootArrowType True r  else
                                    if isFunction r    then normal                    else
                                    noArrow
                                  )
                      , ArrowTail ( if crowfoot opts   || isFunction r    
                                                       then noArrow                   else
                                    AType [(noMod ,Inv)]
                                  )
                      ,TailClip False
                      ]
      RelIntermediateNode _ ->
                       [ Label (StrLabel (fromString "" ))
                       , Shape PlainText
                       , bgColor White
                    --   , URL (theURL opts r)
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
   = AType (if isHead 
            then getCrowfootShape (isUni r) (isTot r)
            else getCrowfootShape (isInj r) (isSur r)
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

directionArrow :: ArrowType
directionArrow = AType [(open,Vee)]

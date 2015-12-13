module Database.Design.Ampersand.Graphic.Graphics
          (makePicture, writePicture, Picture(..), PictureReq(..),imagePath
    )where

import Data.GraphViz
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.FSpec.Switchboard
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Graphic.Fspec2ClassDiagrams
import Database.Design.Ampersand.Graphic.ClassDiag2Dot
import Data.GraphViz.Attributes.Complete
import Data.List
import Data.String

import System.FilePath hiding (addExtension)
import System.Directory
import System.Process (callCommand)
import Control.Exception (catch, IOException)


data PictureReq = PTClassDiagram
                | PTRelsUsedInPat Pattern
                | PTDeclaredInPat Pattern
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
   PTConcept cpt       -> Pict { pType = pr
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
   PTIsaInPattern pat  -> Pict { pType = pr
                               , scale = scale'
                               , dotSource = conceptualGraph' fSpec pr
                               , dotProgName = graphVizCmdForConceptualGraph
                               , caption =
                                   case fsLang fSpec of
                                      English -> "Classifications of " ++ name pat
                                      Dutch   -> "Classificaties van " ++ name pat
                               }
   PTRelsUsedInPat pat -> Pict { pType = pr
                               , scale = scale'
                               , dotSource = conceptualGraph' fSpec pr
                               , dotProgName = graphVizCmdForConceptualGraph
                               , caption =
                                   case fsLang fSpec of
                                      English -> "Concept diagram of the rules in " ++ name pat
                                      Dutch   -> "Conceptueel diagram van de regels in " ++ name pat
                               }
   PTFinterface act    -> Pict { pType = pr
                               , scale = scale'
                               , dotSource = conceptualGraph' fSpec pr
                               , dotProgName = graphVizCmdForConceptualGraph
                               , caption =
                                   case fsLang fSpec of
                                      English -> "Concept diagram of interface " ++ name act
                                      Dutch   -> "Conceptueel diagram van interface " ++ name act
                               }
   PTSingleRule rul    -> Pict { pType = pr
                               , scale = scale'
                               , dotSource = conceptualGraph' fSpec pr
                               , dotProgName = graphVizCmdForConceptualGraph
                               , caption =
                                   case fsLang fSpec of
                                      English -> "Concept diagram of rule " ++ name rul
                                      Dutch   -> "Conceptueel diagram van regel " ++ name rul
                               }
   PTSwitchBoard act   -> Pict { pType = pr
                               , scale = scale'
                               , dotSource = sbdotGraph (switchboardAct fSpec act)
                               , dotProgName = graphVizCmdForConceptualGraph
                               , caption =
                                   case fsLang fSpec of
                                      English -> "Switchboard diagram of " ++ name act
                                      Dutch   -> "Schakelpaneel van " ++ name act
                               }
 where
   scale' =
      case pr of
            PTClassDiagram -> "1.0"
            PTRelsUsedInPat{}-> "0.7"
            PTDeclaredInPat{}-> "0.6"
            PTSwitchBoard{}  -> "0.4"
            PTIsaInPattern{} -> "0.7"
            PTSingleRule{}   -> "0.7"
            PTConcept{}      -> "0.7"
            PTFinterface{}   -> "0.7"
            PTLogicalDM    -> "1.2"
            PTTechnicalDM  -> "1.2"
   graphVizCmdForConceptualGraph = Sfdp

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
      PTFinterface act    -> "KnowledgeGraph"++name act
      PTSwitchBoard x     -> "SwitchBoard"++name x
      PTSingleRule r      -> "SingleRule"++name r

conceptualGraph' :: FSpec -> PictureReq -> DotGraph String
conceptualGraph' fSpec pr = conceptual2Dot (getOpts fSpec) cstruct
  where
    cstruct =
      case pr of
        PTConcept c ->
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
        --  PTRelsUsedInPat makes a picture of at least the relations within pat;
        --  extended with a limited number of more general concepts;
        --  and rels to prevent disconnected concepts, which can be connected given the entire context.
        PTRelsUsedInPat pat ->
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
        PTIsaInPattern pat ->
          let gs    = fsisa fSpec
              cpts  = concs edges
              cpts' = concs pat >- concs gs
              edges = clos gs idgs
              idgs  = [(s,g) |(s,g)<-gs, elem g cpts' || elem s cpts']  --  all isa edges
              clos tuples ts = f (tuples>-ts) ts []
               where f  []  new result = result++new
                     f  _   []  result = result
                     f tups new result = f (tups>-new) [ t |t<-tups, (not.null) (concs t `isc` concs result') ] result'
                                             where result' = result++new
          in
          CStruct { csCpts = cpts
                  , csRels = []
                  , csIdgs = idgs
                  }

        PTFinterface ifc ->
          let gs   = fsisa fSpec
              cpts = nub $ cpts' ++ [c |(s,g)<-idgs, c<-[g,s]]
              cpts'  = concs rs
              rs         = filter affected (vrules fSpec)
              affected r = (not.null) [d | d@Sgn{} <- relsMentionedIn r `isc` relsMentionedIn ifc]
              idgs = [(s,g) |(s,g)<-gs, elem g cpts' || elem s cpts']  --  all isa edges
              rels = [r | r@Sgn{}<-relsMentionedIn ifc, decusr r
                        , not (isProp r)    -- r is not a property
                     ]
          in
          CStruct { csCpts = cpts -- involve all concepts involved either in the affected rules or in the isa-links
                  , csRels = rels
                  , csIdgs = idgs -- involve all isa links from concepts touched by one of the affected rules
                  }
        PTSingleRule r ->
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
      [writeDot DotOutput  | genFSpec opts ]++  --Pretty-printed Dot output with no layout performed.
--      [writeDot Png    | genFSpec opts ] ++
      [writeDot Svg    | genFSpec opts ] ++
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
     writePdf = writeDotPostProcess (Just makePdf) 

     ps2pdfCmd path = "epstopdf " ++ path  -- epstopdf is installed in miktex.  (package epspdfconversion ?)

class ReferableFromPandoc a where
  imagePath :: Options -> a -> FilePath   -- ^ the full file path to the image file

instance ReferableFromPandoc Picture where
  imagePath opts p =
     ( dirOutput opts)
     </> (escapeNonAlphaNum . pictureID . pType ) p <.> "svg"

class Named a => Navigatable a where
   interfacename :: a -> String
   itemstring :: a -> String
   theURL :: Options -> a -> EscString    -- url of the web page in Atlas used when clicked on a node or edge in a .map file
   theURL opts x
     = fromString ("Atlas.php?content=" ++ interfacename x
                   ++  "&User=" ++ user
                   ++  "&Script=" ++ script
                   ++  "&"++interfacename x ++"="++qualify++itemstring x
                  )
      where --copied from atlas.hs
      script = fileName opts
      user = namespace opts
      qualify = "("++user ++ "." ++ script ++ ")"

instance Navigatable A_Concept where
   interfacename _ = "Concept" --see Atlas.adl
   itemstring = name  --copied from atlas.hs

instance Navigatable Declaration where
   interfacename _ = "Relatiedetails"
   itemstring x = name x ++ "["
                  ++ (if source x==target x then name(source x) else name(source x)++"*"++name(target x))
                  ++ "]"

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
      RelSrcEdge r -> [ ArrowHead ( if crowfoot opts   then normal                    else
                                    if isFunction r    then noArrow                   else
                                    if isInvFunction r then directionArrow            else
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
                                    if isInvFunction r then noArrow                   else
                                    noArrow
                                  )
                      , ArrowTail ( if crowfoot opts   then noArrow                   else
                                    if isFunction r    then noArrow                   else
                                    if isInvFunction r then AType [(noMod ,Inv)]      else
                                    AType [(noMod ,Inv)]
                                  )
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

directionArrow :: ArrowType
directionArrow = AType [(open,Vee)]

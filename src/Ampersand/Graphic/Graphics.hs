{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Ampersand.Graphic.Graphics (makePicture, writePicture, Picture (..), PictureTyp (..), imagePathRelativeToDirOutput) where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Classes
import Ampersand.FSpec.FSpec
import Ampersand.Graphic.ClassDiag2Dot
import Ampersand.Graphic.ClassDiagram (ClassDiag)
import Ampersand.Graphic.Fspec2ClassDiagrams
import Ampersand.Misc.HasClasses
import Ampersand.Output.PandocAux (outputLang)
import Data.GraphViz as GV
import Data.GraphViz.Attributes.Complete
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL
import System.Directory (createDirectoryIfMissing, makeAbsolute)
import System.FilePath

-- import           System.Process (callCommand)

data PictureTyp
  = PTClassDiagram -- classification model of the entire script
  | PTCDPattern Pattern -- conceptual diagram of the pattern
  | PTDeclaredInPat Pattern -- conceptual diagram of relations and gens within one pattern
  | PTCDConcept A_Concept -- conceptual diagram comprising all rules in which c is used
  | PTCDRule Rule -- conceptual diagram of the rule in isolation of any context.
  | PTLogicalDM !Bool -- logical data model of the entire script
  | PTTechnicalDM -- technical data model of the entire script
  | PTCDRelation Relation -- conceptual model of a relation in the script, with all the relations of both concepts

data DotContent
  = ClassDiagram ClassDiag
  | ConceptualDg ConceptualStructure

data Picture = Pict
  { -- | the type of the picture
    pType :: PictureTyp,
    -- | a scale factor, intended to pass on to LaTeX, because Pandoc seems to have a problem with scaling.
    scale :: Text,
    dotContent :: DotContent,
    -- | the name of the program to use  ("dot" or "neato" or "fdp" or "Sfdp")
    dotProgName :: GraphvizCommand,
    -- | a human readable name of this picture
    caption :: Text
  }

instance Named PictureTyp where -- for displaying a fatal error
  name PTClassDiagram = "PTClassDiagram"
  name (PTCDPattern pat) = name pat
  name (PTDeclaredInPat pat) = name pat
  name (PTCDConcept c) = name c
  name (PTCDRelation rel) = name rel
  name (PTCDRule r) = name r
  name (PTLogicalDM grouped) = "PTLogicalDM_" <> (if grouped then "grouped_by_patterns" else mempty)
  name PTTechnicalDM = "PTTechnicalDM"

makePicture :: (HasOutputLanguage env) => env -> FSpec -> PictureTyp -> Picture
makePicture env fSpec pr =
  case pr of
    PTClassDiagram ->
      Pict
        { pType = pr,
          scale = scale',
          dotContent = ClassDiagram $ clAnalysis fSpec,
          dotProgName = Dot,
          caption =
            case outputLang' of
              English -> "Classification of " <> name fSpec
              Dutch -> "Classificatie van " <> name fSpec
        }
    PTLogicalDM grouped ->
      Pict
        { pType = pr,
          scale = scale',
          dotContent = ClassDiagram (cdAnalysis grouped fSpec fSpec),
          dotProgName = Dot,
          caption =
            case outputLang' of
              English -> "Logical data model of " <> name fSpec
              Dutch -> "Logisch gegevensmodel van " <> name fSpec
        }
    PTTechnicalDM ->
      Pict
        { pType = pr,
          scale = scale',
          dotContent = ClassDiagram $ tdAnalysis fSpec,
          dotProgName = Dot,
          caption =
            case outputLang' of
              English -> "Technical data model of " <> name fSpec
              Dutch -> "Technisch gegevensmodel van " <> name fSpec
        }
    PTCDConcept cpt ->
      Pict
        { pType = pr,
          scale = scale',
          dotContent = ConceptualDg $ conceptualStructure fSpec pr,
          dotProgName = graphVizCmdForConceptualGraph,
          caption =
            case outputLang' of
              English -> "Concept diagram of " <> name cpt
              Dutch -> "Conceptueel diagram van " <> name cpt
        }
    PTCDRelation rel ->
      Pict
        { pType = pr,
          scale = scale',
          dotContent = ConceptualDg $ conceptualStructure fSpec pr,
          dotProgName = graphVizCmdForConceptualGraph,
          caption =
            case outputLang' of
              English -> "Concept diagram of " <> name rel
              Dutch -> "Conceptueel diagram van " <> name rel
        }
    PTDeclaredInPat pat ->
      Pict
        { pType = pr,
          scale = scale',
          dotContent = ClassDiagram $ cdAnalysis False fSpec pat,
          dotProgName = Dot,
          caption =
            case outputLang' of
              English -> "Concept diagram of relations in " <> name pat
              Dutch -> "Conceptueel diagram van relaties in " <> name pat
        }
    PTCDPattern pat ->
      Pict
        { pType = pr,
          scale = scale',
          dotContent = ClassDiagram $ cdAnalysis False fSpec pat,
          dotProgName = Dot,
          caption =
            case outputLang' of
              English -> "Concept diagram of the rules in " <> name pat
              Dutch -> "Conceptueel diagram van " <> name pat
        }
    PTCDRule rul ->
      Pict
        { pType = pr,
          scale = scale',
          dotContent = ConceptualDg $ conceptualStructure fSpec pr,
          dotProgName = graphVizCmdForConceptualGraph,
          caption =
            case outputLang' of
              English -> "Concept diagram of rule " <> name rul
              Dutch -> "Conceptueel diagram van regel " <> name rul
        }
  where
    outputLang' :: Lang
    outputLang' = outputLang env fSpec
    scale' =
      case pr of
        PTClassDiagram -> "1.0"
        PTCDPattern {} -> "0.7"
        PTDeclaredInPat {} -> "0.6"
        PTCDRule {} -> "0.7"
        PTCDConcept {} -> "0.7"
        PTCDRelation {} -> "0.7"
        PTLogicalDM {} -> "1.2"
        PTTechnicalDM -> "1.2"
    graphVizCmdForConceptualGraph =
      -- Dot gives bad results, but there seems no way to fiddle with the length of edges.
      Neato

-- Sfdp is a bad choice, because it causes a bug in linux. see http://www.graphviz.org/content/sfdp-graphviz-not-built-triangulation-library)

-- | pictureFileName is used in the filename of the picture.
--   Each pictureFileName must be unique (within fSpec) to prevent overwriting newly created files.
--   File names are urlEncoded to cater for the entire alphabet.
pictureFileName :: PictureTyp -> FilePath
pictureFileName pr = toBaseFileName
  $ case pr of
    PTClassDiagram -> "Classification"
    PTLogicalDM grouped -> "LogicalDataModel" <> if grouped then "_Grouped_By_Pattern" else mempty
    PTTechnicalDM -> "TechnicalDataModel"
    PTCDConcept cpt -> "CDConcept" <> urlEncodedName (name cpt)
    PTCDRelation rel -> "PTCDRelation" <> urlEncodedName (name rel) -- MO!
    PTDeclaredInPat pat -> "RelationsInPattern" <> urlEncodedName (name pat)
    PTCDPattern pat -> "CDPattern" <> urlEncodedName (name pat)
    PTCDRule r -> "CDRule" <> urlEncodedName (name r)

-- | conceptualStructure produces a uniform structure,
--   so the transformation to .dot-format can be done with one function.
conceptualStructure :: FSpec -> PictureTyp -> ConceptualStructure
conceptualStructure fSpec pr =
  case pr of
    --  A conceptual diagram comprising all rules in which c is used
    PTCDConcept c ->
      -- MO! Altered
      let c' = concs c -- de lijst van het concept waar t nu om gaat
          allrels = vrels fSpec
          rs = [r | r <- Set.elems allrels, c `elem` concs r]
       in CStruct --- MO!
            { csCpts = Set.elems c' <> [g | (s, g) <- gs, elem g c' || elem s c'] <> [s | (s, g) <- gs, elem g c' || elem s c'],
              csRels = rs, -- the use of "bindedRelationsIn" restricts relations to those actually used in rs
              csIdgs = [(s, g) | (s, g) <- gs, elem g c' || elem s c'], --  all isa edges of the 'main' concept
              csCptsMain = [c]
            }
    -- the PTCDRelation creates a picture of the 2 concepts involved in the relation and their relations
    PTCDRelation rel ->
      let c' = concs rel -- de lijst van het concept waar t nu om gaat
          allrels = vrels fSpec
          rs = L.nub [r | r <- Set.elems allrels, c <- Set.elems c', c `elem` concs r]
       in CStruct --- MO!
            { csCpts = Set.elems c' <> [g | (s, g) <- gs, elem g c' || elem s c'] <> [s | (s, g) <- gs, elem g c' || elem s c'], -- Hier moeten de 2 concepten die in de RELATION zitten gedefineerd worden
            -- concs sign
              csRels = rs,
              csIdgs = [(s, g) | (s, g) <- gs, elem g c' || elem s c'], --  all isa edges of the 'main' concept
              csCptsMain = Set.elems c' -- the main elements from the relations
            }
    --  PTCDPattern makes a picture of at least the relations within pat;
    --  extended with a limited number of more general concepts;
    --  and rels to prevent disconnected concepts, which can be connected given the entire context.
    PTCDPattern pat ->
      let orphans = [c | c <- Set.elems cpts, not (c `elem` concs idgs || c `elem` concs rels)]
          xrels =
            Set.fromList
              [ r | c <- orphans, r <- Set.elems $ vrels fSpec, (c == source r && target r `elem` cpts) || (c == target r && source r `elem` cpts), source r /= target r, decusr r
              ]
          idgs = isaEdges cpts --  all isa edges within the concepts
          cpts = cpts' `Set.union` Set.fromList [g | cl <- eqCl id [g | (s, g) <- gs, s `elem` cpts'], length cl < 3, g <- NE.toList cl] -- up to two more general concepts
          cpts' = concs pat `Set.union` concs rels
          rels = Set.filter (not . isProp . EDcD) . bindedRelationsIn $ pat
       in CStruct
            { csCpts = Set.elems cpts,
              csRels = Set.elems $ rels `Set.union` xrels, -- extra rels to connect concepts without rels in this picture, but with rels in the fSpec
              csIdgs = idgs,
              csCptsMain = []
            }
    -- PTDeclaredInPat makes a picture of relations and gens within pat only
    PTDeclaredInPat pat ->
      let cpts = concs decs `Set.union` concs (gens pat)
          decs = relsDefdIn pat `Set.union` bindedRelationsIn (udefrules pat)
       in CStruct
            { csCpts = Set.elems cpts,
              csRels =
                Set.elems
                  . Set.filter (not . isProp . EDcD)
                  . Set.filter decusr
                  $ decs,
              csIdgs = isaEdges cpts,
              csCptsMain = []
            }
    PTCDRule rule ->
      let cpts = concs rule
          idgs = isaEdges cpts
          rs = allRules
       in RStruct
            { csCpts = Set.elems $ concs rule `Set.union` Set.fromList [c | (s, g) <- idgs, c <- [g, s]],
              csRels =
                Set.elems
                  . Set.filter (not . isProp . EDcD)
                  . Set.filter decusr
                  $ bindedRelationsIn rule,
              csIdgs = idgs, -- involve all isa links from concepts touched by one of the affected rules
              csRule = [rule]
            }
    _ -> fatal ("No conceptual graph defined for pictureReq " <> name pr <> ".")
  where
    isaEdges cpts = [(s, g) | (s, g) <- gs, g `elem` cpts, s `elem` cpts]
    gs = fsisa fSpec
    allRules = fallRules fSpec

writePicture ::
  (HasDirOutput env, HasBlackWhite env, HasDocumentOpts env, HasLogFunc env) =>
  Picture ->
  RIO env ()
writePicture pict = do
  env <- ask
  graphvizIsInstalled <- liftIO isGraphvizInstalled
  unless graphvizIsInstalled $ exitWith GraphVizNotInstalled
  dirOutput <- view dirOutputL
  let imagePathRelativeToCurrentDir = dirOutput </> imagePathRelativeToDirOutput env pict
  logDebug $ "imagePathRelativeToCurrentDir = " <> display (T.pack imagePathRelativeToCurrentDir)
  liftIO $ createDirectoryIfMissing True (takeDirectory imagePathRelativeToCurrentDir)
  writeDot imagePathRelativeToCurrentDir Canon -- To obtain the Graphviz source code of the images
  --  writeDot imagePathRelativeToCurrentDir DotOutput --Reproduces the input along with layout information.
  writeDot imagePathRelativeToCurrentDir Png -- handy format to include in github comments/issues
  -- writeDot imagePathRelativeToCurrentDir Svg   -- format that is used when docx docs are being generated.
  -- writePdf imagePathRelativeToCurrentDir Eps   -- .eps file that is postprocessed to a .pdf file
  where
    writeDot ::
      (HasBlackWhite env, HasLogFunc env) =>
      FilePath ->
      GraphvizOutput ->
      RIO env ()
    writeDot fp = writeDotPostProcess fp Nothing
    writeDotPostProcess ::
      (HasBlackWhite env, HasLogFunc env) =>
      FilePath ->
      Maybe (FilePath -> RIO env ()) -> -- Optional postprocessor
      GraphvizOutput ->
      RIO env ()
    writeDotPostProcess fp postProcess gvOutput =
      do
        env <- ask
        logDebug $ "Generating " <> displayShow gvOutput <> " using " <> displayShow gvCommand <> "."
        let dotSource = mkDotGraph env pict
        --  writeFileUtf8 (dropExtension fp <.> "dotSource") (tshow dotSource)
        path <-
          liftIO
            . GV.addExtension (runGraphvizCommand gvCommand dotSource) gvOutput
            $ dropExtension fp
        absPath <- liftIO . makeAbsolute $ path
        logInfo $ display (T.pack absPath) <> " written."
        case postProcess of
          Nothing -> return ()
          Just x -> x path
      where
        gvCommand = dotProgName pict

-- The GraphVizOutput Pdf generates pixelized graphics on Linux
-- the GraphVizOutput Eps generates extended postscript that can be postprocessed to PDF.
--     makePdf :: (HasLogFunc env ) =>
--                FilePath -> RIO env ()
--     makePdf path = do
--         logDebug $ "Call to makePdf with path = "<>display (T.pack path)
--         liftIO $ callCommand (ps2pdfCmd path)
--         logDebug $ display (T.pack $ replaceExtension path ".pdf") <> " written."
--       `catch` \ e -> logDebug ("Could not invoke PostScript->PDF conversion."<>
--                                 "\n  Did you install MikTex? Can the command epstopdf be found?"<>
--                                 "\n  Your error message is:\n " <> displayShow (e :: IOException))
--
--     writePdf :: (HasBlackWhite env, HasLogFunc env)
--          => FilePath -> GraphvizOutput -> RIO env ()
--     writePdf fp x = writeDotPostProcess fp (Just makePdf) x
--       `catch` (\ e -> logDebug ("Something went wrong while creating your Pdf."<>  --see issue at https://github.com/AmpersandTarski/RAP/issues/21
--                                  "\n  Your error message is:\n " <> displayShow (e :: IOException)))
--     ps2pdfCmd path = "epstopdf " <> path  -- epstopdf is installed in miktex.  (package epspdfconversion ?)

mkDotGraph :: (HasBlackWhite env) => env -> Picture -> DotGraph Text
mkDotGraph env pict =
  case dotContent pict of
    ClassDiagram x -> classdiagram2dot env x
    ConceptualDg x -> conceptual2Dot x

class ReferableFromPandoc a where
  imagePathRelativeToDirOutput ::
    (HasDocumentOpts env, HasDirOutput env) =>
    env ->
    a ->
    FilePath
  -- ^ the file path to the image file. This must be relative from the path where the
  --   document is written.

instance ReferableFromPandoc Picture where
  imagePathRelativeToDirOutput env p =
    "images" </> filename <.> extention
    where
      filename = pictureFileName . pType $ p
      extention =
        case view fspecFormatL env of
          Fpdf -> "png" -- When Pandoc makes a PDF file, Ampersand delivers the pictures in .png format. .pdf-pictures don't seem to work.
          Fdocx -> "png" -- When Pandoc makes a .docx file, Ampersand delivers the pictures in .pdf format. The .svg format for scalable rendering does not work in MS-word.
          Fhtml -> "svg"
          _ -> "dot"

data ConceptualStructure
  = CStruct -- Concept structure
      { -- | The concepts to draw in the graph
        csCpts :: [A_Concept],
        -- | The relations, (the edges in the graph)
        csRels :: [Relation],
        -- | list of Isa relations
        csIdgs :: [(A_Concept, A_Concept)],
        -- | Main concept(s) to draw in the graph
        csCptsMain :: [A_Concept]
      }
  | RStruct -- R Structure
      { -- | The concepts to draw in the graph
        csCpts :: [A_Concept],
        -- | The relations, (the edges in the graph)
        csRels :: [Relation],
        -- | list of Isa relations
        csIdgs :: [(A_Concept, A_Concept)],
        -- list of rules
        csRule :: [Rule]
      }

conceptual2Dot :: ConceptualStructure -> DotGraph Text
conceptual2Dot cs =
  case cs of
    CStruct _ rels idgs maincpt -> createDotGraphCStruct cs rels idgs maincpt
    RStruct _ rels idgs rules -> createDotGraphRStruct cs rels idgs rules -- You can add handling for 'rules' if needed
    -- Helper function to generate DotGraph

createDotGraphCStruct :: ConceptualStructure -> [Relation] -> [(A_Concept, A_Concept)] -> [A_Concept] -> DotGraph Text
createDotGraphCStruct cs rels idgs maincpt =
  DotGraph
    { strictGraph = False,
      directedGraph = True,
      graphID = Nothing,
      graphStatements =
        DotStmts
          { attrStmts =
              [ GraphAttrs
                  [ BgColor [WC (X11Color White) Nothing],
                    Landscape False,
                    Mode IpSep,
                    OutputOrder EdgesFirst,
                    Overlap VoronoiOverlap,
                    Sep (DVal 0.8),
                    NodeSep 1.0,
                    Rank SameRank,
                    RankDir FromLeft,
                    RankSep [2],
                    ReMinCross True
                  ],
                NodeAttrs
                  [ Shape BoxShape,
                    BgColor [WC (X11Color LightGray) Nothing],
                    Style
                      [ SItem Rounded [],
                        SItem Filled [],
                        SItem Bold []
                      ]
                  ],
                EdgeAttrs
                  [ Color [WC (X11Color Black) Nothing],
                    edgeLenFactor 1
                  ]
              ],
            subGraphs = [],
            nodeStmts =
              concatMap nodes (allCpts cs)
                <> concatMap nodes rels
                <> concatMap nodes idgs
                <> concatMap mainnodes maincpt, -- MO!
            edgeStmts =
              concatMap edges (allCpts cs)
                <> concatMap edges rels
                <> concatMap edges idgs
          }
    }
  where
    nodes :: (HasDotParts a) => a -> [DotNode Text]
    nodes = dotNodes cs
    edges :: (HasDotParts a) => a -> [DotEdge Text]
    edges = dotEdges cs
    mainnodes :: (HasDotParts a) => a -> [DotNode Text]
    mainnodes = dotNodesMain cs

-- function to specifically add the rules to the graphs
createDotGraphRStruct :: ConceptualStructure -> [Relation] -> [(A_Concept, A_Concept)] -> [Rule] -> DotGraph Text
createDotGraphRStruct cs rels idgs rules =
  DotGraph
    { strictGraph = False,
      directedGraph = True,
      graphID = Nothing,
      graphStatements =
        DotStmts
          { attrStmts =
              [ GraphAttrs
                  [ BgColor [WC (X11Color White) Nothing],
                    Landscape False,
                    Mode IpSep,
                    OutputOrder EdgesFirst,
                    Overlap VoronoiOverlap,
                    Sep (DVal 0.8),
                    NodeSep 1.0,
                    -- Rank SameRank,
                    RankDir FromTop,
                    RankSep [2],
                    ReMinCross True
                  ],
                NodeAttrs
                  [ Shape BoxShape,
                    BgColor [WC (X11Color LightGray) Nothing],
                    Style
                      [ SItem Rounded [],
                        SItem Filled [],
                        SItem Bold []
                      ]
                  ],
                EdgeAttrs
                  [ Color [WC (X11Color Black) Nothing],
                    edgeLenFactor 0.7
                  ]
              ],
            subGraphs = [],
            nodeStmts =
              concatMap nodes (allCpts cs)
                <> concatMap nodes rels
                <> concatMap nodes idgs
                <> concatMap nodes rules,
            edgeStmts =
              concatMap edges (allCpts cs)
                <> concatMap edges rels
                <> concatMap edges idgs
                <> concatMap edges rules
          }
    }
  where
    nodes :: (HasDotParts a) => a -> [DotNode Text]
    nodes = dotNodes cs
    edges :: (HasDotParts a) => a -> [DotEdge Text]
    edges = dotEdges cs

class HasDotParts a where
  dotNodes :: ConceptualStructure -> a -> [DotNode Text]
  dotNodesMain :: ConceptualStructure -> a -> [DotNode Text]
  dotEdges :: ConceptualStructure -> a -> [DotEdge Text]

baseNodeId :: ConceptualStructure -> A_Concept -> Text
baseNodeId x c =
  case lookup c (zip (allCpts x) [(1 :: Int) ..]) of
    Just i -> "cpt_" <> tshow i
    _ -> fatal ("element " <> name c <> " not found by nodeLabel.")

allCpts :: ConceptualStructure -> [A_Concept]
allCpts cs = Set.elems $ case cs of
  CStruct cpts' rels idgs _ ->
    Set.fromList cpts' `Set.union` concs rels `Set.union` concs idgs
  RStruct cpts' rels idgs _ ->
    Set.fromList cpts' `Set.union` concs rels `Set.union` concs idgs -- Handle RStruct similar to CStruct

edgeLenFactor :: Double -> Attribute
edgeLenFactor x = Len (3 * x)

instance HasDotParts A_Concept where
  dotNodes x cpt =
    [ DotNode
        { nodeID = baseNodeId x cpt,
          nodeAttributes =
            [ Label . StrLabel . TL.fromStrict . name $ cpt
            ]
        }
    ]
  dotNodesMain x cpt =
    -- MO!
    [ DotNode
        { nodeID = baseNodeId x cpt,
          nodeAttributes =
            [ BgColor [WC (X11Color Blue) Nothing],
              Style
                [ SItem Filled [],
                  SItem Bold [],
                  SItem Rounded []
                ],
              FillColor [WC (X11Color SlateGray) Nothing],
              Label . StrLabel . TL.fromStrict . name $ cpt
            ]
        }
    ]
  dotEdges :: ConceptualStructure -> A_Concept -> [DotEdge Text]
  dotEdges _ _ = []

instance HasDotParts Relation where
  dotNodes x rel
    | isEndo rel =
        [ DotNode
            { nodeID = baseNodeId x (source rel) <> name rel, -- MO!
              nodeAttributes =
                [ BgColor [WC (X11Color LightGray) Nothing],
                  Style [],
                  Label
                    . StrLabel
                    . TL.fromStrict
                    . T.intercalate "\n"
                    $ name rel
                    : case Set.toList . properties $ rel of
                      [] -> []
                      ps -> ["[" <> (T.intercalate ", " . map (T.toLower . tshow) $ ps) <> "]"]
                ]
            }
        ]
    | otherwise = []
  dotEdges x rel
    | isEndo rel -- dit is een relatie met het concepts als de source en target hetzelfde zijn
      =
        [ DotEdge
            { fromNode = baseNodeId x . source $ rel,
              toNode = baseNodeId x (source rel) <> name rel,
              edgeAttributes =
                [ Dir NoDir,
                  Style [dotted],
                  edgeLenFactor 0.4,
                  Label . StrLabel . fromString $ ""
                ]
            }
        ]
    | otherwise =
        [ DotEdge
            { fromNode = baseNodeId x . source $ rel,
              toNode = baseNodeId x . target $ rel,
              edgeAttributes =
                [ Label
                    . StrLabel
                    . TL.fromStrict
                    . T.intercalate "\n"
                    $ name rel
                    : case Set.toList . properties $ rel of
                      [] -> []
                      ps -> ["[" <> (T.intercalate ", " . map (T.toLower . tshow) $ ps) <> "]"]
                ]
            }
        ]

instance HasDotParts (A_Concept, A_Concept) where
  dotNodes _ _ = []
  dotEdges x (gen, spc) =
    [ DotEdge
        { fromNode = baseNodeId x gen,
          toNode = baseNodeId x spc,
          edgeAttributes =
            [ edgeLenFactor 0.5,
              Label . StrLabel . fromString $ "",
              Color [WC (X11Color Red) Nothing],
              ArrowHead
                ( AType
                    [ ( ArrMod
                          { arrowFill = OpenArrow,
                            arrowSide = BothSides
                          },
                        Normal
                      )
                    ]
                )
            ]
        }
    ]

instance HasDotParts Rule where
  dotNodes x rule =
    [ DotNode
        { nodeID = "REGELNAAM!",
          nodeAttributes =
            [ Label . StrLabel . TL.fromStrict . name $ rule -- hier wil dit ding opeesn geen naam van de rule pakken fso?
            ]
        }
    ]
  dotEdges _ _ = []

{-
crowfootArrowType :: Bool -> Relation -> ArrowType
crowfootArrowType isHead r
   = AType (if isHead
            then getCrowfootShape (isUni bindedExpr) (isTot bindedExpr)
            else getCrowfootShape (isInj bindedExpr) (isSur bindedExpr)
           )
       where
         bindedExpr = EDcD r
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
 -}

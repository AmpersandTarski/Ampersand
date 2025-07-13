{-# LANGUAGE FlexibleInstances #-}

module Ampersand.Graphic.Graphics (makePicture, writePicture, Picture (..), PictureTyp (..), imagePathRelativeToDirOutput, isDatamodel) where

import Ampersand.ADL1
import Ampersand.Basics hiding (Label)
import Ampersand.Classes
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.Transformers (nameSpaceFormalAmpersand)
import Ampersand.Graphic.ClassDiag2Dot
import Ampersand.Graphic.ClassDiagram (ClassDiag)
import Ampersand.Graphic.Fspec2ClassDiagrams
import Ampersand.Misc.HasClasses
import Ampersand.Output.PandocAux (outputLang)
import Data.GraphViz as GV
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Exception
import RIO.Directory (createDirectoryIfMissing, makeAbsolute)
import RIO.FilePath
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL

-- import           System.Process (callCommand)

data PictureTyp
  = PTClassDiagram -- classification model of the entire script
  | PTCDPattern !Pattern -- conceptual diagram of the pattern
  | PTDeclaredInPat !Pattern -- conceptual diagram of relations and gens within one pattern
  | PTCDConcept !A_Concept -- conceptual diagram comprising all rules in which c is used
  | PTCDRule !Rule -- conceptual diagram of the rule in isolation of any context.
  | PTLogicalDM !Bool -- logical data model of the entire script
  | PTLogicalDMPattern !Pattern -- logical data model of the pattern
  | PTTechnicalDM -- technical data model of the entire script

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
  name pt = case pt of
    PTClassDiagram -> mkName' "PTClassDiagram"
    PTCDPattern pat -> name pat
    PTDeclaredInPat pat -> name pat
    PTCDConcept c -> name c
    PTCDRule r -> name r
    PTLogicalDM grouped -> mkName' $ "PTLogicalDM_" <> (if grouped then "grouped_by_patterns" else mempty)
    PTLogicalDMPattern pat -> mkName' $ "PTLogicalDM_" <> tshow (name pat)
    PTTechnicalDM -> mkName' "PTTechnicalDM"
    where
      mkName' :: Text -> Name
      mkName' x =
        withNameSpace nameSpaceFormalAmpersand
          . mkName ContextName
          . (:| [])
          $ ( case toNamePart x of
                Nothing -> fatal $ "Not a valid NamePart: " <> tshow x
                Just np -> np
            )

isDatamodel :: Picture -> Bool
isDatamodel = isDatamodelType . pType

isDatamodelType :: PictureTyp -> Bool
isDatamodelType pt = case pt of
  PTLogicalDM {} -> True
  PTTechnicalDM {} -> True
  PTLogicalDMPattern {} -> True
  _ -> False

makePicture :: (HasDocumentOpts env) => env -> FSpec -> PictureTyp -> Picture
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
              English -> "Classification of " <> fullName fSpec
              Dutch -> "Classificatie van " <> fullName fSpec
        }
    PTLogicalDM grouped ->
      Pict
        { pType = pr,
          scale = scale',
          dotContent =
            ClassDiagram
              ( cdAnalysis
                  grouped
                  env
                  fSpec
                  (fromMaybe (fatal "No context found in FSpec") (originalContext fSpec))
              ),
          dotProgName = Dot,
          caption =
            case outputLang' of
              English -> "Logical data model of " <> fullName fSpec
              Dutch -> "Logisch gegevensmodel van " <> fullName fSpec
        }
    PTLogicalDMPattern pat ->
      Pict
        { pType = pr,
          scale = scale',
          dotContent = ClassDiagram . cdAnalysis False env fSpec $ pat,
          dotProgName = Dot,
          caption =
            case outputLang' of
              English -> "Logical data model of " <> fullName pat
              Dutch -> "Logisch gegevensmodel van " <> fullName pat
        }
    PTTechnicalDM ->
      Pict
        { pType = pr,
          scale = scale',
          dotContent = ClassDiagram $ tdAnalysis fSpec,
          dotProgName = Dot,
          caption =
            case outputLang' of
              English -> "Technical data model of " <> fullName fSpec
              Dutch -> "Technisch gegevensmodel van " <> fullName fSpec
        }
    PTCDConcept cpt ->
      Pict
        { pType = pr,
          scale = scale',
          dotContent = ConceptualDg $ conceptualStructure fSpec pr,
          dotProgName = graphVizCmdForConceptualGraph,
          caption =
            case outputLang' of
              English -> "Concept diagram of " <> fullName cpt
              Dutch -> "Conceptueel diagram van " <> fullName cpt
        }
    PTDeclaredInPat pat ->
      Pict
        { pType = pr,
          scale = scale',
          dotContent = ConceptualDg $ conceptualStructure fSpec pr,
          dotProgName = Dot,
          caption =
            case outputLang' of
              English -> "Concept diagram of relations in " <> fullName pat
              Dutch -> "Conceptueel diagram van relaties in " <> fullName pat
        }
    PTCDPattern pat ->
      Pict
        { pType = pr,
          scale = scale',
          dotContent = ConceptualDg $ conceptualStructure fSpec pr,
          dotProgName = Dot,
          caption =
            case outputLang' of
              English -> "Concept diagram of the rules in " <> fullName pat
              Dutch -> "Conceptueel diagram van " <> fullName pat
        }
    PTCDRule rul ->
      Pict
        { pType = pr,
          scale = scale',
          dotContent = ConceptualDg $ conceptualStructure fSpec pr,
          dotProgName = graphVizCmdForConceptualGraph,
          caption =
            case outputLang' of
              English -> "Concept diagram of rule " <> fullName rul
              Dutch -> "Conceptueel diagram van regel " <> fullName rul
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
        PTLogicalDM {} -> "1.2"
        PTLogicalDMPattern {} -> "1.2"
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
    PTLogicalDMPattern pat -> "LogicalDataModel-" <> (text1ToText . urlEncodedName . name) pat
    PTTechnicalDM -> "TechnicalDataModel"
    PTCDConcept cpt -> "CDConcept" <> (text1ToText . urlEncodedName . name) cpt
    PTDeclaredInPat pat -> "RelationsInPattern" <> (text1ToText . urlEncodedName . name) pat
    PTCDPattern pat -> "CDPattern" <> (text1ToText . urlEncodedName . name) pat
    PTCDRule r -> "CDRule" <> (text1ToText . urlEncodedName . name) r

-- | conceptualStructure produces a uniform structure,
--   so the transformation to .dot-format can be done with one function.
conceptualStructure :: FSpec -> PictureTyp -> ConceptualStructure
conceptualStructure fSpec pr =
  case pr of
    --  A conceptual diagram comprising all rules and relations in which c is used
    PTCDConcept c ->
      let cpts' = Set.insert c $ concs rs `Set.union` concs directRels
          directRels =
            [ r | r <- Set.toList $ vrels fSpec, source r == c || target r == c
            ]
          rs = [r | r <- toList $ vrules fSpec, c `elem` concs r]
       in CStruct
            { csCpts = L.nub $ toList cpts' <> [g | (s, g) <- gs, g `elem` cpts' || s `elem` cpts'] <> [s | (s, g) <- gs, elem g cpts' || elem s cpts'],
              csRels =
                Set.toList
                  $ (Set.filter (not . isProp . EDcD) . bindedRelationsIn $ rs) -- the use of "bindedRelationsIn" restricts relations to those actually used in rs
                  `Set.union` Set.fromList directRels,
              csIdgs = [(s, g) | (s, g) <- gs, elem g cpts' || elem s cpts'] --  all isa edges
            }
    --  PTCDPattern makes a picture of at least the relations within pat;
    --  extended with a limited number of more general concepts;
    --  and rels to prevent disconnected concepts, which can be connected given the entire context.
    PTCDPattern pat ->
      let orphans = [c | c <- toList cpts, not (c `elem` concs idgs || c `elem` concs rels)]
          xrels =
            Set.fromList
              [ r | c <- orphans, r <- toList $ vrels fSpec, (c == source r && target r `elem` cpts) || (c == target r && source r `elem` cpts), source r /= target r, decusr r
              ]
          idgs = isaEdges cpts --  all isa edges within the concepts
          cpts = cpts' `Set.union` Set.fromList [g | cl <- eqCl id [g | (s, g) <- gs, s `elem` cpts'], length cl < 3, g <- NE.toList cl] -- up to two more general concepts
          cpts' = concs pat `Set.union` concs rels
          rels = Set.filter (not . isProp . EDcD) . bindedRelationsIn $ pat
       in CStruct
            { csCpts = toList cpts,
              csRels = toList $ rels `Set.union` xrels, -- extra rels to connect concepts without rels in this picture, but with rels in the fSpec
              csIdgs = idgs
            }
    -- PTDeclaredInPat makes a picture of relations and gens within pat only
    PTDeclaredInPat pat ->
      let cpts = concs decs `Set.union` concs (gens pat)
          decs = relsDefdIn pat `Set.union` bindedRelationsIn (udefrules pat)
       in CStruct
            { csCpts = toList cpts,
              csRels =
                toList
                  . Set.filter (not . isProp . EDcD)
                  . Set.filter decusr
                  $ decs,
              csIdgs = isaEdges cpts
            }
    PTCDRule r ->
      let cpts = concs r
          idgs = isaEdges cpts
       in CStruct
            { csCpts = toList $ concs r `Set.union` Set.fromList [c | (s, g) <- idgs, c <- [g, s]],
              csRels =
                toList
                  . Set.filter (not . isProp . EDcD)
                  . Set.filter decusr
                  $ bindedRelationsIn r,
              csIdgs = idgs -- involve all isa links from concepts touched by one of the affected rules
            }
    _ -> fatal ("No conceptual graph defined for pictureReq " <> fullName pr <> ".")
  where
    isaEdges cpts = [(s, g) | (s, g) <- gs, g `elem` cpts, s `elem` cpts]
    gs = fsisa fSpec

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
  writeDot imagePathRelativeToCurrentDir Pdf -- format that is used when docx docs are being generated.
  -- writePdf imagePathRelativeToCurrentDir Eps   -- .eps file that is postprocessed to a .pdf file
  where
    writeDot ::
      (HasBlackWhite env, HasLogFunc env) =>
      FilePath ->
      GraphvizOutput ->
      RIO env ()
    writeDot fp = writeAndPostProcess fp Nothing
    writeAndPostProcess ::
      (HasBlackWhite env, HasLogFunc env) =>
      FilePath ->
      Maybe (FilePath -> RIO env ()) -> -- Optional postprocessor
      GraphvizOutput ->
      RIO env ()
    writeAndPostProcess fp postProcess gvOutput =
      do
        env <- ask
        logDebug $ "Generating " <> displayShow gvOutput <> " using " <> displayShow gvCommand <> "."
        let dotSource = mkDotGraph env pict
        --  writeFileUtf8 (dropExtension fp <.> "dotSource") (tshow dotSource)
        path <- runGraphvizCommand' gvCommand dotSource
        -- path' <- liftIO . GV.addExtension
        -- path <-
        --   liftIO
        --     . GV.addExtension (runGraphvizCommand gvCommand dotSource) gvOutput
        --     $ dropExtension fp
        absPath <- liftIO . makeAbsolute $ path
        logDebug $ display (T.pack absPath) <> " written."
        case postProcess of
          Nothing -> return ()
          Just x -> x path
      where
        gvCommand = dotProgName pict
        runGraphvizCommand' ::
          (PrintDotRepr dg n, HasLogFunc env) =>
          GraphvizCommand ->
          dg n ->
          RIO env FilePath
        runGraphvizCommand' cmd dotGraph =
          liftIO (GV.addExtension (runGraphvizCommand cmd dotGraph) gvOutput . dropExtension $ fp) `catch` handler
          where
            handler :: (HasLogFunc env) => GraphvizException -> RIO env FilePath
            handler e = do
              logWarn
                $ "Graphviz had trouble creating: "
                <> displayShow
                  ( RIO.FilePath.addExtension
                      (dropExtension fp)
                      (defaultExtension gvOutput)
                  )
              logWarn "  It might still be useful, but it isn't guaranteed. "
              -- Run in debug mode to see the error message thrown by Graphviz, but they are gibberish:
              logDebug $ displayShow e
              return fp

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

mkDotGraph :: (HasBlackWhite env) => env -> Picture -> DotGraph MyDotNode
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
    "images" </> filename <.> extension
    where
      filename = pictureFileName . pType $ p
      extension =
        case view fspecFormatL env of
          Fpdf -> "png" -- When Pandoc makes a PDF file, Ampersand delivers the pictures in .png format. .pdf-pictures don't seem to work.
          Fdocx -> "png" -- When Pandoc makes a .docx file, Ampersand delivers the pictures in .pdf format. The .svg format for scalable rendering does not work in Pandoc.
          Fhtml -> "png"
          _ -> "dot"

data ConceptualStructure = CStruct
  { -- | The concepts to draw in the graph
    csCpts :: [A_Concept],
    -- | The relations, (the edges in the graph)
    csRels :: [Relation],
    -- | list of Isa relations
    csIdgs :: [(A_Concept, A_Concept)]
  }

conceptual2Dot :: ConceptualStructure -> DotGraph MyDotNode
conceptual2Dot cs@(CStruct _ rels idgs) =
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
                    RankDir FromTop,
                    RankSep [2.5],
                    ReMinCross True
                    {-  Commented out because of an issue: See https://gitlab.com/graphviz/graphviz/issues/1485
                          , Splines Curved
                    -}
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
                <> concatMap nodes idgs,
            edgeStmts =
              concatMap edges (allCpts cs)
                <> concatMap edges rels
                <> concatMap edges idgs
          }
    }
  where
    nodes :: (HasDotParts a) => a -> [DotNode MyDotNode]
    nodes = dotNodes cs
    edges :: (HasDotParts a) => a -> [DotEdge MyDotNode]
    edges = dotEdges cs

class HasDotParts a where
  dotNodes :: ConceptualStructure -> a -> [DotNode MyDotNode]
  dotEdges :: ConceptualStructure -> a -> [DotEdge MyDotNode]

baseNodeId :: ConceptualStructure -> A_Concept -> Name
baseNodeId x c =
  case lookup c (zip (allCpts x) [(1 :: Int) ..]) of
    Just i ->
      mkName ConceptName
        . (:| [])
        $ ( case toNamePart $ "cpt_" <> tshow i of
              Nothing -> fatal $ "Not a valid NamePart: " <> "cpt_" <> tshow i
              Just np -> np
          )
    _ -> fatal ("element " <> fullName c <> " not found by nodeLabel.")

allCpts :: ConceptualStructure -> [A_Concept]
allCpts (CStruct cpts' rels idgs) = toList $ Set.fromList cpts' `Set.union` concs rels `Set.union` concs idgs

edgeLenFactor :: Double -> Attribute
edgeLenFactor x = Len (4 * x)

instance HasDotParts A_Concept where
  dotNodes x cpt =
    [ DotNode
        { nodeID = toMyDotNode $ baseNodeId x cpt,
          nodeAttributes =
            [ Label . StrLabel . TL.fromStrict . fullName $ cpt
            ]
        }
    ]
  dotEdges _ _ = []

instance HasDotParts Relation where
  dotNodes x rel
    | isEndo rel =
        [ DotNode
            { nodeID = toMyDotNode . prependToPlainName (fullName . baseNodeId x . source $ rel) $ name rel,
              nodeAttributes =
                [ Color [WC (X11Color Transparent) Nothing],
                  Shape PlainText,
                  Label
                    . StrLabel
                    . TL.fromStrict
                    . T.intercalate "\n"
                    $ fullName rel
                    : case Set.toList . properties $ rel of
                      [] -> []
                      ps -> ["[" <> (T.intercalate ", " . map (T.toLower . tshow) $ ps) <> "]"]
                ]
            }
        ]
    | otherwise = []
  dotEdges x rel
    | isEndo rel =
        [ DotEdge
            { fromNode = toMyDotNode . baseNodeId x . source $ rel,
              toNode = toMyDotNode . prependToPlainName (fullName . baseNodeId x . source $ rel) $ name rel,
              edgeAttributes =
                [ Dir NoDir,
                  edgeLenFactor 0.4,
                  Label . StrLabel . fromString $ ""
                ]
            }
        ]
    | otherwise =
        [ DotEdge
            { fromNode = toMyDotNode . baseNodeId x . source $ rel,
              toNode = toMyDotNode . baseNodeId x . target $ rel,
              edgeAttributes =
                [ Label
                    . StrLabel
                    . TL.fromStrict
                    . T.intercalate "\n"
                    $ fullName rel
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
        { fromNode = toMyDotNode $ baseNodeId x gen,
          toNode = toMyDotNode $ baseNodeId x spc,
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

-- Copied from Data.GraphViz.Commands, because it is not exported:

-- | A default file extension for each 'GraphvizOutput'.
defaultExtension :: GraphvizOutput -> String
defaultExtension Bmp = "bmp"
defaultExtension Canon = "gv"
defaultExtension DotOutput = "gv"
defaultExtension XDot {} = "gv"
defaultExtension Eps = "eps"
defaultExtension Fig = "fig"
defaultExtension Gd = "gd"
defaultExtension Gd2 = "gd2"
defaultExtension Gif = "gif"
defaultExtension Ico = "ico"
defaultExtension Imap = "map"
defaultExtension Cmapx = "map"
defaultExtension ImapNP = "map"
defaultExtension CmapxNP = "map"
defaultExtension Jpeg = "jpg"
defaultExtension Pdf = "pdf"
defaultExtension Plain = "txt"
defaultExtension PlainExt = "txt"
defaultExtension Png = "png"
defaultExtension Ps = "ps"
defaultExtension Ps2 = "ps"
defaultExtension Svg = "svg"
defaultExtension SvgZ = "svgz"
defaultExtension Tiff = "tif"
defaultExtension Vml = "vml"
defaultExtension VmlZ = "vmlz"
defaultExtension Vrml = "vrml"
defaultExtension WBmp = "wbmp"
defaultExtension WebP = "webp"

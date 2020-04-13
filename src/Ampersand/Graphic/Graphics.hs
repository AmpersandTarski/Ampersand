{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Graphic.Graphics
          (makePicture, writePicture, Picture(..), PictureTyp(..), imagePathRelativeToDirOutput)
where

import           Ampersand.ADL1
import           Ampersand.Basics
import           Ampersand.Classes
import           Ampersand.Graphic.ClassDiagram(ClassDiag)
import           Ampersand.FSpec.FSpec
import           Ampersand.Graphic.ClassDiag2Dot
import           Ampersand.Graphic.Fspec2ClassDiagrams
import           Ampersand.Misc.HasClasses
import           Ampersand.Output.PandocAux (outputLang)
import           Data.GraphViz as GV
import           Data.GraphViz.Attributes.Complete
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL
import           System.Directory(createDirectoryIfMissing)
import           System.FilePath 
import           System.Process (callCommand)

data PictureTyp = PTClassDiagram           -- classification model of the entire script
                | PTCDPattern Pattern      -- conceptual diagram of the pattern
                | PTDeclaredInPat Pattern  -- conceptual diagram of relations and gens within one pattern
                | PTCDConcept A_Concept    -- conceptual diagram comprising all rules in which c is used
                | PTCDRule Rule            -- conceptual diagram of the rule in isolation of any context.
                | PTLogicalDM              -- logical data model of the entire script
                | PTTechnicalDM            -- technical data model of the entire script
data DotContent = 
     ClassDiagram ClassDiag
   | ConceptualDg ConceptualStructure
data Picture = Pict { pType :: PictureTyp             -- ^ the type of the picture
                    , scale :: Text                 -- ^ a scale factor, intended to pass on to LaTeX, because Pandoc seems to have a problem with scaling.
                    , dotContent :: DotContent
                    , dotProgName :: GraphvizCommand  -- ^ the name of the program to use  ("dot" or "neato" or "fdp" or "Sfdp")
                    , caption :: Text                 -- ^ a human readable name of this picture
                    }

instance Named PictureTyp where -- for displaying a fatal error
  name PTClassDiagram        = "PTClassDiagram"
  name (PTCDPattern pat)     = name pat
  name (PTDeclaredInPat pat) = name pat
  name (PTCDConcept c)       = name c
  name (PTCDRule r)          = name r
  name PTLogicalDM           = "PTLogicalDM"
  name PTTechnicalDM         = "PTTechnicalDM"

makePicture :: (HasOutputLanguage env) => env -> FSpec -> PictureTyp -> Picture
makePicture env fSpec pr =
  case pr of
   PTClassDiagram      -> Pict { pType = pr
                               , scale = scale'
                               , dotContent = ClassDiagram $ clAnalysis fSpec
                               , dotProgName = Dot
                               , caption =
                                   case outputLang' of
                                      English -> "Classification of " <> name fSpec
                                      Dutch   -> "Classificatie van " <> name fSpec
                               }
   PTLogicalDM         -> Pict { pType = pr
                               , scale = scale'
                               , dotContent = ClassDiagram $ cdAnalysis fSpec
                               , dotProgName = Dot
                               , caption =
                                   case outputLang' of
                                      English -> "Logical data model of " <> name fSpec
                                      Dutch   -> "Logisch gegevensmodel van " <> name fSpec
                               }
   PTTechnicalDM       -> Pict { pType = pr
                               , scale = scale'
                               , dotContent = ClassDiagram $ tdAnalysis fSpec
                               , dotProgName = Dot
                               , caption =
                                   case outputLang' of
                                      English -> "Technical data model of " <> name fSpec
                                      Dutch   -> "Technisch gegevensmodel van " <> name fSpec
                               }
   PTCDConcept cpt     -> Pict { pType = pr
                               , scale = scale'
                               , dotContent = ConceptualDg $ conceptualStructure fSpec pr
                               , dotProgName = graphVizCmdForConceptualGraph
                               , caption =
                                   case outputLang' of
                                      English -> "Concept diagram of the rules about " <> name cpt
                                      Dutch   -> "Conceptueel diagram van de regels rond " <> name cpt
                               }
   PTDeclaredInPat pat -> Pict { pType = pr
                               , scale = scale'
                               , dotContent = ConceptualDg $ conceptualStructure fSpec pr
                               , dotProgName = graphVizCmdForConceptualGraph
                               , caption =
                                   case outputLang' of
                                      English -> "Concept diagram of relations in " <> name pat
                                      Dutch   -> "Conceptueel diagram van relaties in " <> name pat
                               }
   PTCDPattern pat     -> Pict { pType = pr
                               , scale = scale'
                               , dotContent = ConceptualDg $ conceptualStructure fSpec pr
                               , dotProgName = graphVizCmdForConceptualGraph
                               , caption =
                                   case outputLang' of
                                      English -> "Concept diagram of the rules in " <> name pat
                                      Dutch   -> "Conceptueel diagram van de regels in " <> name pat
                               }
   PTCDRule rul        -> Pict { pType = pr
                               , scale = scale'
                               , dotContent = ConceptualDg $ conceptualStructure fSpec pr
                               , dotProgName = graphVizCmdForConceptualGraph
                               , caption =
                                   case outputLang' of
                                      English -> "Concept diagram of rule " <> name rul
                                      Dutch   -> "Conceptueel diagram van regel " <> name rul
                               }
 where
   outputLang' :: Lang
   outputLang' = outputLang env fSpec
   scale' =
      case pr of
            PTClassDiagram -> "1.0"
            PTCDPattern{}-> "0.7"
            PTDeclaredInPat{}-> "0.6"
            PTCDRule{}   -> "0.7"
            PTCDConcept{}      -> "0.7"
            PTLogicalDM    -> "1.2"
            PTTechnicalDM  -> "1.2"
   graphVizCmdForConceptualGraph = 
       -- Dot gives bad results, but there seems no way to fiddle with the length of edges. 
       Neato 
       -- Sfdp is a bad choice, because it causes a bug in linux. see http://www.graphviz.org/content/sfdp-graphviz-not-built-triangulation-library)

-- | pictureFileName is used in the filename of the picture.
--   Each pictureFileName must be unique (within fSpec) to prevent overwriting newly created files.
pictureFileName :: PictureTyp -> FilePath
pictureFileName pr = toBaseFileName $
     case pr of
      PTClassDiagram      -> "Classification"
      PTLogicalDM         -> "LogicalDataModel"
      PTTechnicalDM       -> "TechnicalDataModel"
      PTCDConcept cpt     -> "CDConcept"<>name cpt
      PTDeclaredInPat pat -> "RelationsInPattern"<>name pat
      PTCDPattern pat     -> "CDPattern"<>name pat
      PTCDRule r          -> "CDRule"<>name r

-- | conceptualStructure produces a uniform structure,
--   so the transformation to .dot-format can be done with one function.
conceptualStructure :: FSpec -> PictureTyp -> ConceptualStructure
conceptualStructure fSpec pr =
      case pr of
        --  A conceptual diagram comprising all rules in which c is used
        PTCDConcept c ->
          let gs = fsisa fSpec
              cpts' = concs rs
              rs    = [r | r<-Set.elems $ vrules fSpec, c `elem` concs r]
          in
          CStruct { csCpts = L.nub$ Set.elems cpts' <> [g |(s,g)<-gs, elem g cpts' || elem s cpts'] <> [s |(s,g)<-gs, elem g cpts' || elem s cpts']
                  , csRels = filter (not . isProp . EDcD) . Set.elems . bindedRelationsIn $ rs   -- the use of "bindedRelationsIn" restricts relations to those actually used in rs
                  , csIdgs = [(s,g) |(s,g)<-gs, elem g cpts' || elem s cpts']  --  all isa edges
                  }
        --  PTCDPattern makes a picture of at least the relations within pat;
        --  extended with a limited number of more general concepts;
        --  and rels to prevent disconnected concepts, which can be connected given the entire context.
        PTCDPattern pat ->
          let orphans = [c | c<-Set.elems cpts, not(c `elem` concs idgs || c `elem` concs rels)]
              xrels = Set.fromList 
                        [r | c<-orphans, r<-Set.elems $ vrels fSpec
                        , (c == source r && target r `elem` cpts) || (c == target r  && source r `elem` cpts)
                        , source r /= target r, decusr r
                        ]
              idgs = [(s,g) |(s,g)<-gs, g `elem` cpts, s `elem` cpts]    --  all isa edges within the concepts
              gs   = fsisa fSpec
              cpts = cpts' `Set.union` Set.fromList [g |cl<-eqCl id [g |(s,g)<-gs, s `elem` cpts'], length cl<3, g<-NE.toList cl] -- up to two more general concepts
              cpts' = concs pat `Set.union` concs rels
              rels = Set.filter (not . isProp . EDcD) . bindedRelationsIn $ pat
          in
          CStruct { csCpts = Set.elems $ cpts
                  , csRels = Set.elems $ rels  `Set.union` xrels -- extra rels to connect concepts without rels in this picture, but with rels in the fSpec
                  , csIdgs = idgs
                  }

        -- PTDeclaredInPat makes a picture of relations and gens within pat only
        PTDeclaredInPat pat ->
          let gs   = fsisa fSpec
              cpts = concs decs `Set.union` concs (gens pat)
              decs = relsDefdIn pat `Set.union` bindedRelationsIn (udefrules pat)
          in
          CStruct { csCpts = Set.elems cpts
                  , csRels = Set.elems 
                           . Set.filter (not . isProp . EDcD)
                           . Set.filter decusr
                           $ decs 
                  , csIdgs = [(s,g) |(s,g)<-gs, g `elem` cpts, s `elem` cpts]    --  all isa edges within the concepts
                  }

        PTCDRule r ->
          let idgs = [(s,g) | (s,g)<-fsisa fSpec
                     , g `elem` concs r || s `elem` concs r]  --  all isa edges
          in
          CStruct { csCpts = Set.elems $ concs r `Set.union` Set.fromList [c |(s,g)<-idgs, c<-[g,s]]
                  , csRels = Set.elems
                           . Set.filter (not . isProp . EDcD)
                           . Set.filter decusr
                           $ bindedRelationsIn r
                  , csIdgs = idgs -- involve all isa links from concepts touched by one of the affected rules
                  }
        _  -> fatal ("No conceptual graph defined for pictureReq "<>name pr<>".")

writePicture :: (HasDirOutput env, HasBlackWhite env, HasDocumentOpts env, HasLogFunc env) =>
                Picture -> RIO env ()
writePicture pict = do
    env <- ask
    dirOutput <- view dirOutputL
    let imagePathRelativeToCurrentDir = dirOutput </> imagePathRelativeToDirOutput env pict
    logDebug $ "imagePathRelativeToCurrentDir = "<> display (T.pack imagePathRelativeToCurrentDir)
    liftIO $ createDirectoryIfMissing True (takeDirectory imagePathRelativeToCurrentDir)
  --  writeDot Canon  --Pretty-printed Dot output with no layout performed.
  --  writeDot DotOutput --Reproduces the input along with layout information.
    writeDot imagePathRelativeToCurrentDir Png    --handy format to include in github comments/issues
    writeDot imagePathRelativeToCurrentDir Svg    -- format that is used when docx docs are being generated.
    writePdf imagePathRelativeToCurrentDir Eps    -- .eps file that is postprocessed to a .pdf file 
   where
     writeDot :: (HasBlackWhite env, HasLogFunc env) =>
                 FilePath -> GraphvizOutput -> RIO env ()
     writeDot fp = writeDotPostProcess fp Nothing
     writeDotPostProcess :: (HasBlackWhite env, HasLogFunc env) =>
                 FilePath
              -> Maybe (FilePath -> RIO env ()) --Optional postprocessor
              -> GraphvizOutput
              -> RIO env ()
     writeDotPostProcess fp postProcess gvOutput  =
         do env <- ask
            logDebug $ "Generating "<>displayShow gvOutput<>" using "<>displayShow gvCommand<>"."
            let dotSource = mkDotGraph env pict
            path <- liftIO $ GV.addExtension (runGraphvizCommand gvCommand dotSource) gvOutput $ 
                       (dropExtension fp)
            logInfo $ display (T.pack path)<>" written."
            case postProcess of
              Nothing -> return ()
              Just x -> x path
       where  gvCommand = dotProgName pict
     -- The GraphVizOutput Pdf generates pixelized graphics on Linux
     -- the GraphVizOutput Eps generates extended postscript that can be postprocessed to PDF.
     makePdf :: (HasLogFunc env ) => 
                FilePath -> RIO env ()
     makePdf path = do
         logDebug $ "Call to makePdf with path = "<>display (T.pack path)
         liftIO $ callCommand (ps2pdfCmd path)
         logDebug $ display (T.pack $ replaceExtension path ".pdf") <> " written."
       `catch` \ e -> logDebug ("Could not invoke PostScript->PDF conversion."<>
                                 "\n  Did you install MikTex? Can the command epstopdf be found?"<>
                                 "\n  Your error message is:\n " <> displayShow (e :: IOException))
                   
     writePdf :: (HasBlackWhite env, HasLogFunc env) 
          => FilePath -> GraphvizOutput -> RIO env ()
     writePdf fp x = writeDotPostProcess fp (Just makePdf) x
       `catch` (\ e -> logDebug ("Something went wrong while creating your Pdf."<>  --see issue at https://github.com/AmpersandTarski/RAP/issues/21
                                  "\n  Your error message is:\n " <> displayShow (e :: IOException)))
     ps2pdfCmd path = "epstopdf " <> path  -- epstopdf is installed in miktex.  (package epspdfconversion ?)

mkDotGraph :: (HasBlackWhite env) => env -> Picture -> DotGraph Text
mkDotGraph env pict =
  case dotContent pict of
    ClassDiagram x -> classdiagram2dot env x
    ConceptualDg x -> conceptual2Dot x

class ReferableFromPandoc a where
  imagePathRelativeToDirOutput :: (HasDocumentOpts env, HasDirOutput env) =>
     env -> a -> FilePath   
  -- ^ the file path to the image file. This must be relative from the path where the 
  --   document is written. 

instance ReferableFromPandoc Picture where
  imagePathRelativeToDirOutput env p =
    "images" </> filename <.> extention
    where 
      filename = pictureFileName . pType $ p
      extention =
         case view fspecFormatL env of
           Fpdf   -> "png"   -- If Pandoc makes a PDF file, the pictures must be delivered in .png format. .pdf-pictures don't seem to work.
           Fdocx  -> "svg"   -- If Pandoc makes a .docx file, the pictures are delivered in .svg format for scalable rendering in MS-word.
           Fhtml  -> "png"
           _      -> "pdf"

data ConceptualStructure = CStruct { csCpts :: [A_Concept]  -- ^ The concepts to draw in the graph
                                   , csRels :: [Relation]   -- ^ The relations, (the edges in the graph)
                                   , csIdgs :: [(A_Concept, A_Concept)]  -- ^ list of Isa relations
                                   }

conceptual2Dot :: ConceptualStructure -> DotGraph Text
conceptual2Dot cs@(CStruct _ rels idgs) = DotGraph 
    { strictGraph = False
    , directedGraph = True
    , graphID = Nothing
    , graphStatements = DotStmts
          { attrStmts = [GraphAttrs
                            [ BgColor [WC (X11Color White ) Nothing]
                            , Landscape False
                            , Mode IpSep
                            , OutputOrder EdgesFirst
                            , Overlap VoronoiOverlap
                            , Sep (DVal 0.8)
                            , NodeSep 1.0 
                            , Rank SameRank
                            , RankDir FromTop
                            , RankSep [2.5]
                            , ReMinCross True
                      {-  Commented out because of an issue: See https://gitlab.com/graphviz/graphviz/issues/1485
                            , Splines Curved  
                      -}  
                            ]
                        , NodeAttrs [ Shape BoxShape
                                    , BgColor [WC (X11Color LightGray ) Nothing]
                                    , Style [SItem Rounded []
                                            ,SItem Filled  []
                                            ,SItem Bold []
                                            ]
                                    ] 
                        , EdgeAttrs [ Color [WC (X11Color Black ) Nothing]
                                    , edgeLenFactor 1 ]
                        ]
          , subGraphs = []
          , nodeStmts = concatMap nodes (allCpts cs)
                      <>concatMap nodes rels
                      <>concatMap nodes idgs
          , edgeStmts = concatMap edges (allCpts cs)
                      <>concatMap edges rels
                      <>concatMap edges idgs
          }
    }
       where
    nodes :: HasDotParts a => a -> [DotNode Text]
    nodes = dotNodes cs
    edges :: HasDotParts a => a -> [DotEdge Text]
    edges = dotEdges cs

class HasDotParts a where
  dotNodes :: ConceptualStructure -> a -> [DotNode Text]
  dotEdges :: ConceptualStructure -> a -> [DotEdge Text]

baseNodeId :: ConceptualStructure -> A_Concept -> Text
baseNodeId x c =
  case lookup c (zip (allCpts x) [(1::Int)..]) of
    Just i -> "cpt_"<>tshow i
    _      -> fatal ("element "<>name c<>" not found by nodeLabel.")

allCpts :: ConceptualStructure -> [A_Concept]
allCpts (CStruct cpts' rels idgs) = Set.elems $ Set.fromList cpts' `Set.union` concs rels `Set.union` concs idgs

edgeLenFactor :: Double -> Attribute
edgeLenFactor x = Len (4 * x)

instance HasDotParts A_Concept where
  dotNodes x cpt =
    [DotNode 
      { nodeID = baseNodeId x cpt
      , nodeAttributes = [ Label . StrLabel . TL.fromStrict .name $ cpt
                         ]
      }
    ]
  dotEdges _ _ = []  
instance HasDotParts Relation where
  dotNodes x rel
    | isEndo rel = 
       [DotNode 
          { nodeID = baseNodeId x (source rel) <> name rel 
          , nodeAttributes = [ Color [WC (X11Color Transparent ) Nothing]
                             , Shape PlainText
                             , Label . StrLabel . TL.fromStrict . T.intercalate "\n" $ 
                                  name rel :
                                  case Set.toList . properties $ rel of
                                     []   -> []
                                     ps -> ["["<>(T.intercalate ", " . map (T.toLower . tshow) $ ps)<>"]"]
                              ]
                          }
       ]
    | otherwise  = []
  dotEdges x rel
    | isEndo rel = 
      [ DotEdge
          { fromNode       = baseNodeId x . source $ rel
          , toNode         = baseNodeId x (source rel) <> name rel
          , edgeAttributes = [ Dir NoDir
                             , edgeLenFactor 0.4
                             , Label . StrLabel . fromString $ "" 
                             ]
          }
      ]
    | otherwise =
      [ DotEdge
          { fromNode       = baseNodeId x . source $ rel
          , toNode         = baseNodeId x . target $ rel
          , edgeAttributes = [ Label . StrLabel . TL.fromStrict . T.intercalate "\n" $ 
                                  name rel :
                                  case Set.toList . properties $ rel of
                                     []   -> []
                                     ps -> ["["<>(T.intercalate ", " . map (T.toLower . tshow) $ ps)<>"]"]
                             ]
          }
      ]
instance HasDotParts (A_Concept,A_Concept) where
  dotNodes _ _ = []
  dotEdges x (gen,spc) = 
    [ DotEdge
         { fromNode       = baseNodeId x gen
         , toNode         = baseNodeId x spc
         , edgeAttributes = [ edgeLenFactor 0.5
                            , Label . StrLabel . fromString $ "" 
                            , Color [WC (X11Color Red ) Nothing]
                            , ArrowHead (AType [( ArrMod { arrowFill = OpenArrow
                                                         , arrowSide = BothSides
                                                         }
                                                , Normal
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
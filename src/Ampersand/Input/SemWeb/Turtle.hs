{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Ampersand.Input.SemWeb.Turtle
  ( graph2P_Context,
    writeRdfTList,
    mergeGraphs,
    parseTurtle,
    myPrefixMappings,
  )
where

import Ampersand.Basics
import Ampersand.Core.ParseTree
import Ampersand.Input.ADL1.CtxError
import Ampersand.Misc.HasClasses
import Data.RDF
import qualified Data.RDF.Vocabulary.OWL as OWL
import qualified Data.RDF.Vocabulary.RDF as RDF
import qualified Data.RDF.Vocabulary.RDFS as RDFS
import qualified Data.RDF.Vocabulary.SKOS as SKOS
import RIO.FilePath
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T

type Graph = RDF TList

parseTurtle :: Text -> Guarded Graph
parseTurtle raw = do
  let defBaseUrl = case filter (T.isPrefixOf "@base") . T.lines $ raw of
        [baseline] -> case take 1 . reverse . take 2 . T.words $ baseline of
          [x] -> Just (BaseUrl x)
          _ -> Nothing
        _ -> Nothing
      defMappings = Nothing
      parser = TurtleParser defBaseUrl defMappings
  case parseString parser raw of
    Left err -> mkGenericParserError (Origin "Parsing some turtle file (.ttl)") (tshow err)
    Right graph -> pure graph

writeRdfTList :: (HasDirOutput env, HasFSpecGenOpts env, HasLogFunc env) => Int -> Graph -> RIO env ()
writeRdfTList i rdfGraph = do
  env <- ask
  let filePath = filePath' env
  liftIO $ withFile filePath WriteMode writer
  logDebug $ "Written: " <> display (T.pack filePath)
  where
    filePath' env = Ampersand.Basics.view dirOutputL env </> (baseName env <> show i) -<.> ".ttl"
    writer h = do
      hWriteRdf serializer h rdfGraph
      where
        serializer = TurtleSerializer Nothing myPrefixMappings

myPrefixMappings :: PrefixMappings
myPrefixMappings =
  PrefixMappings
    . M.fromList
    $ [ ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
        ("rdfs", "http://www.w3.org/2000/01/rdf-schema#"),
        ("skos", "http://www.w3.org/2004/02/skos/core#"),
        ("owl", "http://www.w3.org/2002/07/owl#"),
        ("", "http://ampersand.example.org/"),
        ("xsd", "http://www.w3.org/2001/XMLSchema#")
      ]

-- merge multiple graphs into one
mergeGraphs :: NonEmpty Graph -> Graph
mergeGraphs (h NE.:| tl) = foldl' (mergeTriples Nothing) h tl

-- mergeTriples assumes the triples are expanded
mergeTriples :: Maybe Int -> Graph -> Graph -> Graph
mergeTriples maxBlankNode graph1 graph2 =
  case maxBlankNode of
    Nothing -> mergeTriples (Just $ getMaxBlankNode graph1) graph1 graph2
    Just i -> case map objectOf $ select graph2 Nothing Nothing (Just isBNode) of
      [] -> foldl' addTriple graph1 (uniqTriplesOf graph2)
      (h : _) -> mergeTriples (Just (i + 1)) graph1' graph2'
        where
          numberToReplace = case h of
            BNodeGen x -> x
            _ -> fatal $ "Expected a blank node, but found: " <> tshow h
          graph1' = foldl' addTriple graph1 (map replaceBlankNode triplesWithBlankNode)
          graph2' = foldl' removeTriple graph2 triplesWithBlankNode
          triplesWithBlankNode =
            select graph2 Nothing Nothing (is h)
              <> select graph2 (is h) Nothing Nothing
          replaceBlankNode (Triple sub p obj) = Triple sub' p obj'
            where
              sub' = substitute sub
              obj' = substitute obj
              substitute n = case n of
                BNodeGen x -> if x == numberToReplace then BNodeGen (i + 1) else n
                _ -> n

getMaxBlankNode :: Graph -> Int
getMaxBlankNode gr = length . map objectOf $ select gr Nothing Nothing (Just isBNode)

-- \| Convert a list of fully expanded Triples into a 'P_Context'.
graph2P_Context :: Graph -> Guarded P_Context
graph2P_Context graph = do
  ontologyName <- case select graph Nothing (is RDF._type) (is OWL._Ontology) of
    [] -> mkError "No ontology triple found in Turtle file"
    (Triple (UNode s) _ _) : _ -> pure . fst . suggestName ContextName . toText1Unsafe $ s
    (t : _) ->
      mkError
        $ T.unlines
          [ "Subject note of ontology triple should be a UNode.",
            "  Found: " <> tshow (subjectOf t)
          ]
  let cptDefsNodes = filter hasNoPattern (allConceptNodes graph)
        where
          hasNoPattern n = null $ select graph (is n) (is SKOS.inScheme) Nothing
  cptDefs <- mapM (mkConceptDef graph (CONTEXT ontologyName)) cptDefsNodes
  let isas =
        [ PClassify
            { specific = PCpt sName,
              generics = PCpt gName NE.:| [],
              pos = someTurtle
            }
          | Triple sNode _ gNode <- select graph Nothing (is RDFS.subClassOf) Nothing,
            sLbl <- labelsOf graph sNode,
            let (sName, _) = suggestName ContextName . toText1Unsafe $ sLbl,
            gLbl <- labelsOf graph gNode,
            let (gName, _) = suggestName ContextName . toText1Unsafe $ gLbl
        ]
  patDefs <- patternDefs
  pure
    $ PCtx
      { ctx_vs = mempty,
        ctx_rs = mempty,
        ctx_rrules = mempty,
        ctx_reprs = mempty,
        ctx_ps = mempty,
        ctx_pos = mempty,
        ctx_pops = mempty,
        ctx_pats = patDefs,
        ctx_nm = ontologyName,
        ctx_metas = mempty,
        ctx_markup = Just Markdown,
        ctx_lbl = Nothing,
        ctx_lang = Nothing,
        ctx_ks = mempty,
        ctx_ifcs = mempty,
        ctx_gs = isas,
        ctx_enfs = mempty,
        ctx_ds = relationDefs,
        ctx_cs = cptDefs
      }
  where
    mkError :: Text -> Guarded a
    mkError = mkGenericParserError someTurtle
    patternDefs :: Guarded [P_Pattern]
    patternDefs = mapM makePattern patNodes
      where
        patNodes :: [(Node, Text)]
        patNodes =
          [ (patNode, patLbl)
            | conceptScheme <- instancesOf graph SKOS._ConceptScheme,
              patNode <- conceptScheme : subclassesOf graph conceptScheme,
              patLbl <- labelsOf graph patNode
          ]
        makePattern :: (Node, Text) -> Guarded P_Pattern
        makePattern (patNode, patLbl) = do
          let (nm, l) = suggestName PatternName . toText1Unsafe $ patLbl
          let cptDefsNodes = filter thisPattern (allConceptNodes graph)
                where
                  thisPattern :: Node -> Bool
                  thisPattern n = not . null $ select graph (is n) (is SKOS.inScheme) (is patNode)
          cptDefs <- mapM (mkConceptDef graph (PATTERN nm)) cptDefsNodes

          pure
            P_Pat
              { pt_xps = mempty,
                pt_vds = mempty,
                pt_rls = mempty,
                pt_pop = mempty,
                pt_nm = nm,
                pt_lbl = l,
                pt_ids = mempty,
                pt_gns = mempty,
                pt_enfs = mempty,
                pt_end = someTurtle,
                pt_dcs = mempty,
                pt_cds = cptDefs,
                pt_Reprs = mempty,
                pt_RRuls = mempty,
                pos = someTurtle
              }
    relationDefs :: [P_Relation]
    relationDefs = relationsBasedOnRestrictions <> relationsWithoutRestrictions
      where
        relationsBasedOnRestrictions :: [P_Relation]
        relationsBasedOnRestrictions =
          [ P_Relation
              { dec_sign = P_Sign (PCpt src) (PCpt tgt),
                dec_prps = Set.fromList (getProps relNode blank),
                dec_pragma = Nothing,
                dec_pos = someTurtle,
                dec_nm = nm,
                dec_label = l,
                dec_defaults = mempty,
                dec_Mean = mempty
              }
            | relNode <- map subjectOf $ select graph Nothing (is RDF._type) (is OWL._ObjectProperty),
              blank <- map subjectOf $ select graph Nothing (is OWL.onProperty) (is relNode),
              tgtNode <- map objectOf $ select graph (is blank) (is OWL.onClass) Nothing,
              srcNode <- map subjectOf $ select graph Nothing (is RDFS.subClassOf) (is blank),
              relLbl <- labelsOf graph relNode,
              let (nm, l) = suggestName RelationName . toText1Unsafe $ relLbl,
              srcLbl <- labelsOf graph srcNode,
              let (src, _) = suggestName ContextName . toText1Unsafe $ srcLbl,
              tgtLbl <- labelsOf graph tgtNode,
              let (tgt, _) = suggestName ContextName . toText1Unsafe $ tgtLbl
          ]
          where
            getProps :: Node -> Node -> [PProp]
            getProps relNode restrictionNode =
              concat
                $ [[P_Uni, P_Tot] | _ <- select graph (is restrictionNode) (is OWL.qualifiedCardinality) Nothing]
                <> [[P_Uni] | _ <- select graph (is restrictionNode) (is OWL.maxCardinality) Nothing]
                <> [[P_Tot] | LNode (TypedL "1" _) <- map objectOf $ select graph (is restrictionNode) (is OWL.minCardinality) Nothing]
                <> [[P_Asy] | _ <- map subjectOf $ select graph Nothing (is OWL.inverseOf) (is restrictionNode)]
                <> [propsOfInverse inv | inv <- map subjectOf $ select graph Nothing (is OWL.inverseOf) (is relNode)]
              where
                propsOfInverse :: Node -> [PProp]
                propsOfInverse invRel =
                  case map subjectOf $ select graph Nothing (is OWL.onProperty) (is invRel) of
                    [blankInvRestriction] ->
                      [P_Inj | not . null $ uniList]
                        <> [P_Sur | not . null $ surList]
                      where
                        uniList = L.intersect [OWL.qualifiedCardinality, OWL.maxCardinality] cardinalityNodes
                        surList = L.intersect [OWL.qualifiedCardinality, OWL.minCardinality] cardinalityNodes
                        cardinalityNodes =
                          [p | Triple _ p (LNode (TypedL "1" _)) <- select graph (is blankInvRestriction) Nothing Nothing]
                    _ -> []

        relationsWithoutRestrictions :: [P_Relation]
        relationsWithoutRestrictions =
          [ P_Relation
              { dec_sign = P_Sign (PCpt src) (PCpt tgt),
                dec_prps = mempty,
                dec_pragma = Nothing,
                dec_pos = someTurtle,
                dec_nm = nm,
                dec_label = l,
                dec_defaults = mempty,
                dec_Mean = mempty
              }
            | relNode <-
                map subjectOf
                  $ select graph Nothing (is RDF._type) (is OWL._ObjectProperty),
              srcNode <-
                map objectOf
                  $ select graph (is relNode) (is RDFS.domain) Nothing,
              tgtNode <-
                map objectOf
                  $ select graph (is relNode) (is RDFS.range) Nothing,
              relLbl <- labelsOf graph relNode,
              let (nm, l) = suggestName RelationName . toText1Unsafe $ relLbl,
              let restrictions =
                    map subjectOf
                      $ select graph Nothing (is OWL.onProperty) (is relNode),
              null restrictions,
              srcLbl <- labelsOf graph srcNode,
              let (src, _) = suggestName ContextName . toText1Unsafe $ srcLbl,
              tgtLbl <- labelsOf graph tgtNode,
              let (tgt, _) = suggestName ContextName . toText1Unsafe $ tgtLbl
          ]

allConceptNodes :: Graph -> [Node]
allConceptNodes graph =
  [ cpt
    | cpt <- map subjectOf $ select graph Nothing (is RDF._type) (is OWL._Class),
      cpt `elem` map subjectOf (select graph (is cpt) (is RDF._type) (is OWL._NamedIndividual))
  ]

mkConceptDef :: Graph -> DefinitionContainer -> Node -> Guarded PConceptDef
mkConceptDef graph from cpt = do
  (nm, l) <- case map objectOf $ select graph (is cpt) (is RDFS.label) Nothing of
    [] ->
      addWarning
        (mkTurtleWarning someTurtle ["No label found for concept " <> tshow cpt <> ", using URI as label"])
        (getName cpt)
    [lbl] -> getName lbl
    (h : _) ->
      addWarning
        (mkTurtleWarning someTurtle ["Multiple labels found for concept " <> tshow cpt <> ", using the first one."])
        (getName h)
  pure
    PConceptDef
      { cdname = nm,
        cdmean = mempty,
        cdlbl = l,
        cdfrom = from,
        cddef2 = PCDDefLegacy def2 "",
        pos = someTurtle
      }
  where
    getName :: Node -> Guarded (Name, Maybe Label)
    getName lblNode = case suggestName ContextName . toText1Unsafe <$> literalTextOf lblNode of
      Nothing -> mkGenericParserError someTurtle $ "Label found for concept " <> tshow cpt <> " does not contain text."
      Just x -> pure x
    def2 = T.intercalate "\n" . mapMaybe (literalTextOf . objectOf) $ select graph (is cpt) (is SKOS.definition) Nothing

literalTextOf :: Node -> (Maybe Text)
literalTextOf n = case n of
  LNode (PlainL txt) -> Just txt
  LNode (PlainLL txt _) -> Just txt
  LNode (TypedL txt _) -> Just txt
  UNode txt -> Just txt -- TODO: Warning that this is not a literal
  _ -> Nothing

labelsOf :: Graph -> Node -> [Text]
labelsOf graph n =
  mapMaybe (literalTextOf . objectOf)
    $ select graph (is n) (is RDFS.label) Nothing

-- getExactlyOneMatchingTriple :: Graph -> Maybe Node -> Node -> Node -> Guarded Triple

instancesOf :: Graph -> Node -> [Node]
instancesOf graph cls =
  map subjectOf (select graph Nothing (is RDF._type) (is cls))
    <> concatMap (instancesOf graph) (subclassesOf graph cls)

subclassesOf :: Graph -> Node -> [Node]
subclassesOf graph cls =
  case map subjectOf
    $ select graph Nothing (is RDFS.subClassOf) (is cls) of
    [] -> []
    subs -> subs <> concatMap (subclassesOf graph) subs

is :: Node -> NodeSelector
is n = Just (== n)

someTurtle :: Origin
someTurtle = Origin "Somewhere in imported .ttl files."

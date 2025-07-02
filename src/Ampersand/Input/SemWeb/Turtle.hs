{-# LANGUAGE DuplicateRecordFields #-}

module Ampersand.Input.SemWeb.Turtle
  ( readTurtle,
    graph2P_Context,
    writeRdfTList,
    mergeGraphs,
  )
where

import Ampersand.Basics
import Ampersand.Core.ParseTree
import Ampersand.Input.ADL1.CtxError
import Data.RDF
import RIO.Directory (doesFileExist)
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T

-- | Parse a Turtle (.ttl) file into an RDF TList, using the RIO monad.
readTurtle :: FilePath -> RIO env (Guarded (RDF TList))
readTurtle filePath = do
  exists <- liftIO $ doesFileExist filePath
  if exists
    then do
      raw <- readUTF8File filePath
      let defBaseUrl =
            ( case raw of
                Left _ -> Nothing
                Right content -> case filter (T.isPrefixOf "@base") . T.lines $ content of
                  [baseline] -> case take 1 . reverse . take 2 . T.words $ baseline of
                    [x] -> Just (BaseUrl x)
                    _ -> Nothing
                  _ -> Nothing
            )
          defMappings = Nothing
          parser = TurtleParser defBaseUrl defMappings
      result <- liftIO $ parseFile parser filePath
      case result of
        Left err -> pure $ mkTurtleParseError filePath (tshow err)
        Right graph -> pure . pure $ graph
    else
      return
        $ mkErrorReadingINCLUDE
          Nothing
          [ "While looking for " <> T.pack filePath,
            "   File does not exist."
          ]

writeRdfTList :: (HasLogFunc env) => Int -> RDF TList -> RIO env ()
writeRdfTList i rdfGraph = do
  logDebug $ "Start schrijven van " <> display (T.pack filePath)
  liftIO
    $ withFile filePath WriteMode writer
  logDebug $ "Einde schrijven van " <> display (T.pack filePath)
  where
    filePath = "/workspaces/ampersand2/Graaf_" <> show i <> ".ttl"
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
mergeGraphs :: NonEmpty (RDF TList) -> RDF TList
mergeGraphs (h NE.:| tl) = foldl' (mergeTriples Nothing) h tl

-- mergeTriples assumes the triples are expanded
mergeTriples :: Maybe Int -> RDF TList -> RDF TList -> RDF TList
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

getMaxBlankNode :: RDF TList -> Int
getMaxBlankNode gr = length . map objectOf $ select gr Nothing Nothing (Just isBNode)

-- \| Convert a list of fully expanded Triples into a 'P_Context'.
graph2P_Context :: RDF TList -> Guarded P_Context
graph2P_Context graph = do
  ontologyName <- case select graph Nothing rdfType owlOntology of
    [] -> mkError "No ontology triple found in Turtle file"
    (Triple (UNode s) _ _) : _ -> pure . fst . suggestName ContextName . toText1Unsafe $ s
    (t : _) ->
      mkError
        $ T.unlines
          [ "Subject note of ontology triple should be a UNode.",
            "  Found: " <> tshow (subjectOf t)
          ]
  cptDefs <- conceptDefs (CONTEXT ontologyName)
  let relDefs = relationDefs
  let isas =
        [ PClassify
            { specific = PCpt sName,
              generics = PCpt gName NE.:| [],
              pos = orig
            }
          | Triple sNode _ gNode <- select graph Nothing rdfsSubClassOf Nothing,
            sLbl <- getLabels sNode,
            let (sName, _) = suggestName ContextName . toText1Unsafe $ sLbl,
            gLbl <- getLabels gNode,
            let (gName, _) = suggestName ContextName . toText1Unsafe $ gLbl
        ]
        where
          getLabels :: Node -> [Text]
          getLabels n =
            mapMaybe (getLiteralText . objectOf)
              $ select graph (is n) rdfsLabel Nothing

  pure
    $ PCtx
      { ctx_vs = mempty,
        ctx_rs = mempty,
        ctx_rrules = mempty,
        ctx_reprs = mempty,
        ctx_ps = mempty,
        ctx_pos = mempty,
        ctx_pops = mempty,
        ctx_pats = mempty,
        ctx_nm = ontologyName,
        ctx_metas = mempty,
        ctx_markup = Just Markdown,
        ctx_lbl = Nothing,
        ctx_lang = Nothing,
        ctx_ks = mempty,
        ctx_ifcs = mempty,
        ctx_gs = isas,
        ctx_enfs = mempty,
        ctx_ds = relDefs,
        ctx_cs = cptDefs
      }
  where
    mkError :: Text -> Guarded a
    mkError = mkGenericParserError orig
    orig = Origin "Somewhere in imported .ttl files."
    relationDefs :: [P_Relation]
    relationDefs =
      [ P_Relation
          { dec_sign = P_Sign (PCpt src) (PCpt tgt),
            dec_prps = Set.fromList (getProps relNode blank),
            dec_pragma = Nothing,
            dec_pos = orig,
            dec_nm = nm,
            dec_label = l,
            dec_defaults = mempty,
            dec_Mean = mempty
          }
        | relNode <- map subjectOf $ select graph Nothing rdfType owlObjectProperty,
          blank <- map subjectOf $ select graph Nothing owlOnProperty (is relNode),
          tgtNode <- map objectOf $ select graph (is blank) owlOnClass Nothing,
          srcNode <- map subjectOf $ select graph Nothing rdfsSubClassOf (is blank),
          relLbl <- getLabels relNode,
          let (nm, l) = suggestName RelationName . toText1Unsafe $ relLbl,
          srcLbl <- getLabels srcNode,
          let (src, _) = suggestName ContextName . toText1Unsafe $ srcLbl,
          tgtLbl <- getLabels tgtNode,
          let (tgt, _) = suggestName ContextName . toText1Unsafe $ tgtLbl
      ]
      where
        getLabels :: Node -> [Text]
        getLabels n =
          mapMaybe (getLiteralText . objectOf)
            $ select graph (is n) rdfsLabel Nothing
        getProps :: Node -> Node -> [PProp]
        getProps relNode restrictionNode =
          concat
            $ [[P_Uni, P_Tot] | _ <- select graph (is restrictionNode) owlQualifiedCardinality Nothing]
            <> [[P_Uni] | _ <- select graph (is restrictionNode) owlMaxCardinality Nothing]
            <> [[P_Tot] | LNode (TypedL "1" _) <- map objectOf $ select graph (is restrictionNode) owlMinCardinality Nothing]
            <> [[P_Asy] | _ <- map subjectOf $ select graph Nothing owlInverseOf (is restrictionNode)]
            <> [propsOfInverse inv | inv <- map subjectOf $ select graph Nothing owlInverseOf (is relNode)]
          where
            propsOfInverse :: Node -> [PProp]
            propsOfInverse invRel =
              case map subjectOf $ select graph Nothing owlOnProperty (is invRel) of
                [blankInvRestriction] ->
                  [P_Inj | not . null $ uniList]
                    <> [P_Sur | not . null $ surList]
                  where
                    uniList = L.intersect [unode "http://www.w3.org/2002/07/owl#qualifiedCardinality", unode "http://www.w3.org/2002/07/owl#maxCardinality"] cardinalityNodes
                    surList = L.intersect [unode "http://www.w3.org/2002/07/owl#qualifiedCardinality", unode "http://www.w3.org/2002/07/owl#minCardinality"] cardinalityNodes
                    cardinalityNodes =
                      [p | Triple _ p (LNode (TypedL "1" _)) <- select graph (is blankInvRestriction) Nothing Nothing]
                _ -> []
    conceptDefs :: DefinitionContainer -> Guarded [PConceptDef]
    conceptDefs frm =
      sequence
        [ mkConceptDef cpt
          | cpt <- map subjectOf $ select graph Nothing rdfType owlClass,
            cpt
              `elem` map subjectOf (select graph (is cpt) rdfType owlNamedIndividual)
        ]
      where
        mkConceptDef :: Node -> Guarded PConceptDef
        mkConceptDef cpt = do
          (nm, l) <- case map objectOf $ select graph (is cpt) rdfsLabel Nothing of
            [] -> mkGenericParserError orig $ "No label found for concept " <> tshow cpt
            [lbl] -> getName lbl
            (h : _) ->
              addWarning
                (mkTurtleWarning orig ["Multiple labels found for concept " <> tshow cpt <> ", using the first one."])
                (getName h)
          pure
            PConceptDef
              { cdname = nm,
                cdmean = mempty,
                cdlbl = l,
                cdfrom = frm,
                cddef2 = PCDDefLegacy def2 "",
                pos = orig
              }
          where
            getName :: Node -> Guarded (Name, Maybe Label)
            getName lblNode = case suggestName ContextName . toText1Unsafe <$> getLiteralText lblNode of
              Nothing -> mkGenericParserError orig $ "Label found for concept " <> tshow cpt <> " does not contain text."
              Just x -> pure x
            def2 = T.intercalate "\n" . mapMaybe (getLiteralText . objectOf) $ select graph (is cpt) skosDefinition Nothing

getLiteralText :: Node -> Maybe Text
getLiteralText n = case n of
  LNode (PlainL txt) -> Just txt
  LNode (PlainLL txt _) -> Just txt
  LNode (TypedL txt _) -> Just txt
  _ -> Nothing

-- getExactlyOneMatchingTriple :: RDF TList -> Maybe Node -> Node -> Node -> Guarded Triple

selector :: Text -> NodeSelector
selector txt = Just fun
  where
    fun x = x == unode txt

is :: Node -> NodeSelector
is n = Just (== n)

owlClass :: NodeSelector
owlClass = selector "http://www.w3.org/2002/07/owl#Class"

owlNamedIndividual :: NodeSelector
owlNamedIndividual = selector "http://www.w3.org/2002/07/owl#NamedIndividual"

owlQualifiedCardinality :: NodeSelector
owlQualifiedCardinality = selector "http://www.w3.org/2002/07/owl#qualifiedCardinality"

owlInverseOf :: NodeSelector
owlInverseOf = selector "http://www.w3.org/2002/07/owl#inverseOf"

owlMaxCardinality :: NodeSelector
owlMaxCardinality = selector "http://www.w3.org/2002/07/owl#maxCardinality"

owlMinCardinality :: NodeSelector
owlMinCardinality = selector "http://www.w3.org/2002/07/owl#minCardinality"

owlObjectProperty :: NodeSelector
owlObjectProperty = selector "http://www.w3.org/2002/07/owl#ObjectProperty"

owlOnProperty :: NodeSelector
owlOnProperty = selector "http://www.w3.org/2002/07/owl#onProperty"

owlOnClass :: NodeSelector
owlOnClass = selector "http://www.w3.org/2002/07/owl#onClass"

owlOntology :: NodeSelector
owlOntology = selector "http://www.w3.org/2002/07/owl#Ontology"

rdfsLabel :: NodeSelector
rdfsLabel = selector "http://www.w3.org/2000/01/rdf-schema#label"

rdfsSubClassOf :: NodeSelector
rdfsSubClassOf = selector "http://www.w3.org/2000/01/rdf-schema#subClassOf"

skosDefinition :: NodeSelector
skosDefinition = selector "http://www.w3.org/2004/02/skos/core#definition"

rdfType :: NodeSelector
rdfType = selector "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"

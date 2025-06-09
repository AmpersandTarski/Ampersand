{-# LANGUAGE DuplicateRecordFields #-}

module Ampersand.Input.SemWeb.Turtle
  ( parseTurtleFile,
  )
where

import Ampersand.Basics
import Ampersand.Core.ParseTree
import Ampersand.Input.ADL1.CtxError
import Data.RDF
import RIO.Directory (doesFileExist)
import qualified RIO.Map as M
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
                Right content -> case filter isBaseLine . T.lines $ content of
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
  where
    isBaseLine :: Text -> Bool
    isBaseLine = T.isPrefixOf "@base"

-- | Parse a Turtle file and convert it into a 'P_Context'.
parseTurtleFile :: FilePath -> RIO env (Guarded P_Context)
parseTurtleFile filePath = do
  guardedGraph <- readTurtle filePath
  pure $ getContext filePath guardedGraph

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

getContext :: FilePath -> Guarded (RDF TList) -> Guarded P_Context
getContext filePath guardedGraph = do
  graph <- guardedGraph
  nm <- ontologyName graph
  let cptDefs = conceptDefs (CONTEXT nm) graph
  let relDefs = relationDefs graph
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
        ctx_nm = nm,
        ctx_metas = mempty,
        ctx_markup = Just Markdown,
        ctx_lbl = Nothing,
        ctx_lang = Nothing,
        ctx_ks = mempty,
        ctx_ifcs = mempty,
        ctx_gs = mempty,
        ctx_enfs = mempty,
        ctx_ds = relDefs,
        ctx_cs = cptDefs
      }
  where
    mkError :: Text -> Guarded a
    mkError = mkTurtleParseError filePath
    orig = Origin $ "Somewhere in " <> T.pack filePath
    ontologyName :: RDF TList -> Guarded Name
    ontologyName graph = case select graph Nothing rdfType owlOntology of
      [] -> mkError "No ontology triple found in Turtle file"
      [Triple (UNode s) _ _] -> pure . fst . suggestName ContextName . toText1Unsafe $ s
      _ -> mkError "Multiple ontology triples found in Turtle file"
    relationDefs :: RDF TList -> [P_Relation]
    relationDefs graph =
      [ P_Relation
          { dec_sign = P_Sign (PCpt src) (PCpt tgt),
            dec_prps = mempty,
            dec_pragma = Nothing,
            dec_pos = orig,
            dec_nm = nm,
            dec_label = l,
            dec_defaults = mempty,
            dec_Mean = mempty
          }
        | relNode <-
            map subjectOf
              $ select graph Nothing rdfType owlObjectProperty,
          lbl <-
            mapMaybe (getLiteralText . objectOf)
              $ select graph (is relNode) rdfsLabel Nothing,
          let (nm, l) = suggestName RelationName . toText1Unsafe $ lbl,
          srcNode <- getSrcNode relNode,
          srcLbl <-
            mapMaybe (getLiteralText . objectOf)
              $ select graph (is srcNode) rdfsLabel Nothing,
          let (src, _) = suggestName ContextName . toText1Unsafe $ srcLbl,
          tgtNode <- getTgtNode relNode,
          tgtLbl <-
            mapMaybe (getLiteralText . objectOf)
              $ select graph (is tgtNode) rdfsLabel Nothing,
          let (tgt, _) = suggestName ContextName . toText1Unsafe $ tgtLbl
      ]
      where
        getSrcNode :: Node -> [Node]
        getSrcNode relNode =
          (objectOf <$> select graph (is relNode) rdfsRange Nothing)
            <> implicitByRestriction
          where
            implicitByRestriction =
              [ srcNode
                | blank <- map subjectOf $ select graph Nothing owlOnProperty (is relNode),
                  blank `elem` map subjectOf (select graph Nothing rdfType owlRestriction),
                  srcNode <- map subjectOf $ select graph Nothing rdfsSubClassOf (is blank)
              ]
        getTgtNode :: Node -> [Node]
        getTgtNode relNode =
          (objectOf <$> select graph (is relNode) rdfsDomain Nothing)
            <> implicitByRestriction
          where
            implicitByRestriction =
              [ tgtNode
                | blank <- map subjectOf $ select graph Nothing owlOnProperty (is relNode),
                  blank `elem` map subjectOf (select graph Nothing rdfType owlRestriction),
                  tgtNode <- map objectOf $ select graph (is blank) owlOnClass Nothing
              ]
    conceptDefs :: DefinitionContainer -> RDF TList -> [PConceptDef]
    conceptDefs frm graph =
      [ PConceptDef
          { cdname = nm,
            cdmean = mempty,
            cdlbl = l,
            cdfrom = frm,
            cddef2 = PCDDefLegacy def2 "",
            pos = orig
          }
        | cpt <- map subjectOf $ select graph Nothing rdfType owlClass,
          cpt
            `elem` map subjectOf (select graph (is cpt) rdfType owlNamedIndividual),
          lbl <- map objectOf $ select graph (is cpt) rdfsLabel Nothing,
          Just (nm, l) <- [suggestName ContextName . toText1Unsafe <$> getLiteralText lbl],
          def2 <- mapMaybe (getLiteralText . objectOf) $ select graph (is cpt) skosDefinition Nothing
      ]

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

owlObjectProperty :: NodeSelector
owlObjectProperty = selector "http://www.w3.org/2002/07/owl#ObjectProperty"

owlOnProperty :: NodeSelector
owlOnProperty = selector "http://www.w3.org/2002/07/owl#onProperty"

owlRestriction :: NodeSelector
owlRestriction = selector "http://www.w3.org/2002/07/owl#Restriction"

owlOnClass :: NodeSelector
owlOnClass = selector "http://www.w3.org/2002/07/owl#onClass"

owlOntology :: NodeSelector
owlOntology = selector "http://www.w3.org/2002/07/owl#Ontology"

rdfsLabel :: NodeSelector
rdfsLabel = selector "http://www.w3.org/2000/01/rdf-schema#label"

rdfsDomain :: NodeSelector
rdfsDomain = selector "http://www.w3.org/2000/01/rdf-schema#domain"

rdfsRange :: NodeSelector
rdfsRange = selector "http://www.w3.org/2000/01/rdf-schema#range"

rdfsSubClassOf :: NodeSelector
rdfsSubClassOf = selector "http://www.w3.org/2000/01/rdf-schema#subClassOf"

skosDefinition :: NodeSelector
skosDefinition = selector "http://www.w3.org/2004/02/skos/core#definition"

rdfType :: NodeSelector
rdfType = selector "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"

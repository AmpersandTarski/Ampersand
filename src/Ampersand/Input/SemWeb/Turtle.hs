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
        ctx_ds = mempty,
        ctx_cs = conceptDefs (CONTEXT nm) graph
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
          lbl <- mapMaybe (getLiteralText . objectOf) $ select graph (is cpt) rdfsLabel Nothing,
          let (nm, l) = suggestName ContextName . toText1Unsafe $ lbl,
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

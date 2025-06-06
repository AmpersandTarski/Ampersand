module Ampersand.Input.SemWeb.Turtle
  ( parseTurtleFile,
  )
where

import Ampersand.Basics
import Ampersand.Core.ParseTree
  ( P_Context (..),
  )
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
      let defBaseUrl = Nothing
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
        ctx_cs = mempty
      }
  where
    mkError :: Text -> Guarded a
    mkError = mkTurtleParseError filePath
    rdfType :: Node
    rdfType = unode "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"

    owlOntology :: Node
    owlOntology = unode "http://www.w3.org/2002/07/owl#Ontology"
    ontologyName :: RDF TList -> Guarded Name
    ontologyName graph = case query graph Nothing (Just rdfType) (Just owlOntology) of
      [] -> mkError "No ontology triple found in Turtle file"
      [Triple (UNode s) _ _] -> pure . fst . suggestName ContextName . toText1Unsafe $ s
      _ -> mkError "Multiple ontology triples found in Turtle file"

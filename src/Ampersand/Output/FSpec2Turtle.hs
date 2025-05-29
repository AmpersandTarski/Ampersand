module Ampersand.Output.FSpec2Turtle (writeTurtle) where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.FSpec
import Ampersand.Misc.HasClasses
import Data.RDF
import RIO.Directory
import RIO.FilePath
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.Text as T

writeTurtle :: (HasFSpecGenOpts env, HasDirOutput env, HasLogFunc env) => FSpec -> RIO env ()
writeTurtle fSpec = do
  env <- ask
  let outputFile = Ampersand.Basics.view dirOutputL env </> baseName env -<.> ".ttl"
      rdfGraph = fSpec2Graph fSpec
  writeGraphToFile outputFile rdfGraph
  logInfo $ "Turtle file written to " <> display (T.pack outputFile)

myBaseUrl :: Maybe BaseUrl
myBaseUrl = Just (BaseUrl "http://ampersand.example.org#")

myPrefixMappings :: PrefixMappings
myPrefixMappings =
  PrefixMappings
    . M.fromList
    $ [ ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
        ("rdfs", "http://www.w3.org/2000/01/rdf-schema#"),
        ("skos", "http://www.w3.org/2004/02/skos/core#"),
        ("owl", "http://www.w3.org/2002/07/owl#"),
        ("xsd", "http://www.w3.org/2001/XMLSchema#"),
        ("base", "http://ampersand.example.org#")
      ]

-- | Convert an FSpec to an RDF graph
fSpec2Graph :: FSpec -> RDF TList
fSpec2Graph fSpec = mkRdf shortenedTriples myBaseUrl myPrefixMappings
  where
    shortenedTriples =
      fmap (shortenTriple myBaseUrl (Just myPrefixMappings)) triples
    triples =
      concat
        $ fmap concept2triples (instanceList fSpec)
        <> fmap relation2triples (instanceList fSpec)

    concept2triples :: A_Concept -> Triples
    concept2triples cpt =
      [ triple uri (unode "rdf:type") (unode "owl:Class"),
        triple uri (unode "rdfs:label") (lnode . plainL . label $ cpt)
      ]
      where
        uri :: Node
        uri = unode $ maybe "" unBaseUrl myBaseUrl <> "Concept_" <> tshow (name cpt)

    relation2triples :: Relation -> Triples
    relation2triples rel =
      [ triple uri (unode "rdf:type") (unode "owl:ObjectProperty"),
        triple uri (unode "rdfs:label") (lnode . plainL . label $ rel)
      ]
      where
        uri :: Node
        uri = unode $ maybe "" unBaseUrl myBaseUrl <> "Relation_" <> tshow (name rel)

-- | Write the RDF graph to a file in Turtle format
writeGraphToFile :: (MonadIO m) => FilePath -> RDF TList -> m ()
writeGraphToFile path graph = do
  let serializer = TurtleSerializer Nothing myPrefixMappings
  -- Ensure the directory exists before writing the file
  liftIO $ createDirectoryIfMissing True (takeDirectory path)
  liftIO $ withFile path WriteMode $ \h ->
    hWriteRdf serializer h graph

-- | Replace long URIs with base-relative or prefixed forms when possible
shortenNode :: Maybe BaseUrl -> Maybe PrefixMappings -> Node -> Node
shortenNode baseUri mappings node = case node of
  UNode uri ->
    case baseUri of
      Just (BaseUrl base)
        | base `T.isPrefixOf` uri ->
            unode (T.drop (T.length base) uri)
      _ -> case mappings of
        Just (PrefixMappings ps) ->
          case L.find (\(_, ns) -> ns `T.isPrefixOf` uri) (M.toList ps) of
            Just (pfx, ns) -> unode (pfx <> ":" <> T.drop (T.length ns) uri)
            Nothing -> node
        Nothing -> node
  _ -> node

-- | Apply shortening to all nodes in a triple
shortenTriple :: Maybe BaseUrl -> Maybe PrefixMappings -> Triple -> Triple
shortenTriple b p (Triple s pr o) =
  Triple (shortenNode b p s) (shortenNode b p pr) (shortenNode b p o)

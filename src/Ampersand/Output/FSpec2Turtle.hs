module Ampersand.Output.FSpec2Turtle (writeTurtle) where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.FSpec
import Ampersand.Misc.HasClasses
import Data.RDF
import RIO.Directory
import RIO.FilePath
import qualified RIO.Text as T

writeTurtle :: (HasFSpecGenOpts env, HasDirOutput env, HasLogFunc env) => FSpec -> RIO env ()
writeTurtle fSpec = do
  env <- ask
  let outputFile = Ampersand.Basics.view dirOutputL env </> baseName env -<.> ".ttl"
      rdfGraph = fSpec2Graph fSpec
  writeGraphToFile outputFile rdfGraph
  logInfo $ "Turtle file written to " <> display (T.pack outputFile)

myBaseUrl :: Maybe BaseUrl
myBaseUrl = Nothing -- TODO: Define your base URL here

myPrefixMappings :: PrefixMappings
myPrefixMappings = mempty -- TODO: Define your prefix mappings here

-- | Convert an FSpec to an RDF graph
fSpec2Graph :: FSpec -> RDF TList
fSpec2Graph fSpec = mkRdf triples myBaseUrl myPrefixMappings
  where
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
        uri = unode $ maybe "" unBaseUrl myBaseUrl <> "Concept#" <> tshow (name cpt)

    relation2triples :: Relation -> Triples
    relation2triples rel =
      [ triple uri (unode "rdf:type") (unode "owl:ObjectProperty"),
        triple uri (unode "rdfs:label") (lnode . plainL . label $ rel)
      ]
      where
        uri :: Node
        uri = unode $ maybe "" unBaseUrl myBaseUrl <> "Relation#" <> tshow (name rel)

-- | Write the RDF graph to a file in Turtle format
writeGraphToFile :: (MonadIO m) => FilePath -> RDF TList -> m ()
writeGraphToFile path graph = do
  let serializer = TurtleSerializer Nothing myPrefixMappings
  -- Ensure the directory exists before writing the file
  liftIO $ createDirectoryIfMissing True (takeDirectory path)
  liftIO $ withFile path WriteMode $ \h ->
    hWriteRdf serializer h graph

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
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T

writeTurtle :: (HasFSpecGenOpts env, HasDirOutput env, HasLogFunc env) => FSpec -> RIO env ()
writeTurtle fSpec = do
  env <- ask
  let outputFile = Ampersand.Basics.view dirOutputL env </> baseName env -<.> ".ttl"
      rdfGraph = fSpec2Graph fSpec
  writeGraphToFile outputFile rdfGraph
  logInfo $ "Turtle file written to " <> display (T.pack outputFile)

myBaseUrl :: BaseUrl
myBaseUrl = BaseUrl "http://ampersand.example.org/"

myPrefixMappings :: PrefixMappings
myPrefixMappings =
  PrefixMappings
    . M.fromList
    $ [ ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
        ("rdfs", "http://www.w3.org/2000/01/rdf-schema#"),
        ("skos", "http://www.w3.org/2004/02/skos/core#"),
        ("owl", "http://www.w3.org/2002/07/owl#"),
        -- ("", "http://ampersand.example.org#")
        ("xsd", "http://www.w3.org/2001/XMLSchema#")
      ]

-- | Convert an FSpec to an RDF graph
fSpec2Graph :: FSpec -> RDF TList
fSpec2Graph fSpec = mkRdf shortenedTriples (Just myBaseUrl) myPrefixMappings
  where
    shortenedTriples =
      fmap (shortenTriple myBaseUrl myPrefixMappings) triples
    triples =
      concat
        $ fmap concept2triples (instanceList fSpec)
        <> fmap relation2triples (instanceList fSpec)

    concept2triples :: A_Concept -> Triples
    concept2triples cpt =
      [ triple (uri cpt) (unode "rdf:type") (unode "owl:Class"),
        --  triple (uri cpt) (unode "skos:prefLabel") (lnode . plainL . label $ cpt),
        triple (uri cpt) (unode "rdfs:label") (lnode . plainL . label $ cpt)
      ]
        <> [ triple
               (uri cpt)
               (unode "rdfs:isDefinedBy")
               ( lnode
                   $ plainLL
                     (markup2Markdown m)
                     ( case amLang m of
                         Dutch -> "nl"
                         English -> "en"
                     )
               )
             | cDef <- instanceList fSpec,
               acdcpt cDef == cpt,
               m <- map ameaMrk (acddef2 cDef : acdmean cDef),
               markup2Markdown m /= ""
           ]
        <> [ triple (uri cpt) (unode "rdfs:subClassOf") (uri greaterCpt)
             | greaterCpt <- concatMap greaters (instanceList fSpec)
           ]
      where
        greaters :: AClassify -> [A_Concept]
        greaters gen = filter (cpt /=) $ case gen of
          Isa {} -> [gengen gen | genspc gen == cpt]
          IsE {} -> [x | genspc gen == cpt, x <- NE.toList $ genrhs gen]

    relation2triples :: Relation -> Triples
    relation2triples rel =
      [ triple (uri rel) (unode "rdf:type") (unode "owl:ObjectProperty"),
        triple (uri rel) (unode "rdfs:label") (lnode . plainL . label $ rel),
        triple (uri rel) (unode "rdfs:domain") (uri (source rel)),
        triple (uri rel) (unode "rdfs:range") (uri (target rel))
      ]
        <> [ triple
               (uri rel)
               (unode "rdfs:isDefinedBy")
               ( lnode
                   $ plainLL
                     (markup2Markdown m)
                     ( case amLang m of
                         Dutch -> "nl"
                         English -> "en"
                     )
               )
             | m <- map ameaMrk $ decMean rel,
               markup2Markdown m /= ""
           ]

-- | Write the RDF graph to a file in Turtle format
writeGraphToFile :: (MonadIO m) => FilePath -> RDF TList -> m ()
writeGraphToFile path graph = do
  let serializer = TurtleSerializer Nothing myPrefixMappings
  -- Ensure the directory exists before writing the file
  liftIO $ createDirectoryIfMissing True (takeDirectory path)
  liftIO $ withFile path WriteMode $ \h ->
    hWriteRdf serializer h graph

-- | Replace long URIs with base-relative or prefixed forms when possible
shortenNode :: BaseUrl -> PrefixMappings -> Node -> Node
shortenNode baseUri mappings node =
  case node of
    UNode txt ->
      case baseUri of
        (BaseUrl base)
          | containsDot txt -> node
          | base `T.isPrefixOf` txt ->
              unode (T.drop (T.length base) txt)
        _ -> case L.find (\(_, ns) -> ns `T.isPrefixOf` txt) (M.toList ps) of
          Just (pfx, ns) -> unode (pfx <> ":" <> T.drop (T.length ns) txt)
          Nothing -> node
    _ -> node
  where
    PrefixMappings ps = mappings
    containsDot :: Text -> Bool
    containsDot = T.any (== '.')

-- | Apply shortening to all nodes in a triple
shortenTriple :: BaseUrl -> PrefixMappings -> Triple -> Triple
shortenTriple b p (Triple s pr o) =
  Triple (shortenNode b p s) (shortenNode b p pr) (shortenNode b p o)

uri :: (Unique a) => a -> Node
uri a = unode $ unBaseUrl myBaseUrl <> text1ToText (uniqueShowWithType a)
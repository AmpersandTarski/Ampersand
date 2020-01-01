{-# LANGUAGE RecordWildCards #-}
module Ampersand.Output.ToJSON.ToJson
  ( generateAllJSONfiles
  , generatePopJSONfile
  )
where
import           Ampersand.Output.ToJSON.Concepts 
import           Ampersand.Output.ToJSON.Conjuncts 
import           Ampersand.Output.ToJSON.Interfaces 
import           Ampersand.Output.ToJSON.JSONutils
import           Ampersand.Output.ToJSON.Populations
import           Ampersand.Output.ToJSON.Relations
import           Ampersand.Output.ToJSON.Roles
import           Ampersand.Output.ToJSON.Rules 
import           Ampersand.Output.ToJSON.Settings
import           Ampersand.Output.ToJSON.Views
import           RIO.FilePath

-- | Generate all JSON files from a given FSpec
generateAllJSONfiles :: (HasProtoOpts env, Show env, HasLogFunc env)
    => FilePath  -- ^ The directory where the files should be written
    -> FSpec     -- ^ The FSpec that contains all content of the generated files
    -> RIO env ()
generateAllJSONfiles dir fSpec = do
  env <- ask
  sequence_ $
    [ writeJSONFile (dir </> "settings"   <.>"json") (fromAmpersand' env fSpec fSpec :: Settings)
    , writeJSONFile (dir </> "relations"  <.>"json") (fromAmpersand env fSpec fSpec :: Relationz)
    , writeJSONFile (dir </> "rules"      <.>"json") (fromAmpersand env fSpec fSpec :: Rulez)
    , writeJSONFile (dir </> "concepts"   <.>"json") (fromAmpersand env fSpec fSpec :: Concepts)
    , writeJSONFile (dir </> "conjuncts"  <.>"json") (fromAmpersand env fSpec fSpec :: Conjuncts)
    , writeJSONFile (dir </> "interfaces" <.>"json") (fromAmpersand env fSpec fSpec :: Interfaces)
    , writeJSONFile (dir </> "views"      <.>"json") (fromAmpersand env fSpec fSpec :: Views)
    , writeJSONFile (dir </> "roles"      <.>"json") (fromAmpersand env fSpec fSpec :: Roles)
    ]
  generatePopJSONfile dir fSpec
-- | Generate only population.json
generatePopJSONfile :: (HasLogFunc env)
    => FilePath  -- ^ The directory where the files should be written
    -> FSpec     -- ^ The FSpec that contains all content of the generated files
    -> RIO env ()
generatePopJSONfile dir fSpec = do
 env <- ask
 writeJSONFile (dir </> "populations"<.>"json") (fromAmpersand env fSpec fSpec :: Populations)
 

 



  

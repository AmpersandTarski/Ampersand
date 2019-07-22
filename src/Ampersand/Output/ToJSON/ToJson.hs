{-# LANGUAGE RecordWildCards #-}
module Ampersand.Output.ToJSON.ToJson
  (generateJSONfiles
  )
where
import Ampersand.Output.ToJSON.JSONutils
import Ampersand.Output.ToJSON.Settings
import Ampersand.Output.ToJSON.Populations
import Ampersand.Output.ToJSON.Relations
import Ampersand.Output.ToJSON.Rules 
import Ampersand.Output.ToJSON.Concepts 
import Ampersand.Output.ToJSON.Conjuncts 
import Ampersand.Output.ToJSON.Interfaces 
import Ampersand.Output.ToJSON.Views
import Ampersand.Output.ToJSON.Roles

generateJSONfiles :: (HasProtoOpts env, HasEnvironment env, HasDirPrototype env, HasVerbosity env, HasHandle env, HasCommands env)
    => FSpec -> RIO env ()
generateJSONfiles fSpec = do
 env <- ask
 genRapPopulation <- view genRapPopulationL
 sequence_ $
  if genRapPopulation
  then [ writeJSONFile "metaPopulation" 
                                (fromAmpersand env fSpec fSpec :: Populations)
       ]
  else [ writeJSONFile "settings"   (fromAmpersand env fSpec fSpec :: Settings)
       , writeJSONFile "relations"  (fromAmpersand env fSpec fSpec :: Relationz)
       , writeJSONFile "rules"      (fromAmpersand env fSpec fSpec :: Rulez)
       , writeJSONFile "concepts"   (fromAmpersand env fSpec fSpec :: Concepts)
       , writeJSONFile "conjuncts"  (fromAmpersand env fSpec fSpec :: Conjuncts)
       , writeJSONFile "interfaces" (fromAmpersand env fSpec fSpec :: Interfaces)
       , writeJSONFile "views"      (fromAmpersand env fSpec fSpec :: Views)
       , writeJSONFile "roles"      (fromAmpersand env fSpec fSpec :: Roles)
       , writeJSONFile "populations"(fromAmpersand env fSpec fSpec :: Populations)
       ]


 



  

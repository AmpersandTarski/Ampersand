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

generateJSONfiles :: (Show env, HasProtoOpts env, HasDirPrototype env, HasLogFunc env)
    => Bool -> FSpec -> RIO env ()
generateJSONfiles genRapPopulation fSpec = do
 env <- ask
 sequence_ $
  if genRapPopulation
  then [ writeJSONFile fSpec "metaPopulation" 
                                    (fromAmpersand env fSpec fSpec :: Populations)
       ]
  else [ writeJSONFile fSpec "settings"   (fromAmpersand env fSpec fSpec :: Settings)
       , writeJSONFile fSpec "relations"  (fromAmpersand env fSpec fSpec :: Relationz)
       , writeJSONFile fSpec "rules"      (fromAmpersand env fSpec fSpec :: Rulez)
       , writeJSONFile fSpec "concepts"   (fromAmpersand env fSpec fSpec :: Concepts)
       , writeJSONFile fSpec "conjuncts"  (fromAmpersand env fSpec fSpec :: Conjuncts)
       , writeJSONFile fSpec "interfaces" (fromAmpersand env fSpec fSpec :: Interfaces)
       , writeJSONFile fSpec "views"      (fromAmpersand env fSpec fSpec :: Views)
       , writeJSONFile fSpec "roles"      (fromAmpersand env fSpec fSpec :: Roles)
       , writeJSONFile fSpec "populations"(fromAmpersand env fSpec fSpec :: Populations)
       ]


 



  

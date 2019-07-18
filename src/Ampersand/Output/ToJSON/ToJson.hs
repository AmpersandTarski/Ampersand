{-# LANGUAGE RecordWildCards #-}
module Ampersand.Output.ToJSON.ToJson
  (generateJSONfiles)
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

generateJSONfiles :: (HasDirPrototype env, HasVerbosity env, HasHandle env, HasCommands env, HasOptions env) => MultiFSpecs -> RIO env ()
generateJSONfiles multi = do
 env <- view optionsL
 genRapPopulation <- view genRapPopulationL
 sequence_ $
  if genRapPopulation
  then [ writeJSONFile "metaPopulation" 
                                (fromAmpersand env multi (multi,True) :: Populations)
       ]
  else [ writeJSONFile "settings"   (fromAmpersand env multi multi :: Settings)
       , writeJSONFile "relations"  (fromAmpersand env multi multi :: Relationz)
       , writeJSONFile "rules"      (fromAmpersand env multi multi :: Rulez)
       , writeJSONFile "concepts"   (fromAmpersand env multi multi :: Concepts)
       , writeJSONFile "conjuncts"  (fromAmpersand env multi multi :: Conjuncts)
       , writeJSONFile "interfaces" (fromAmpersand env multi multi :: Interfaces)
       , writeJSONFile "views"      (fromAmpersand env multi multi :: Views)
       , writeJSONFile "roles"      (fromAmpersand env multi multi :: Roles)
       , writeJSONFile "populations"(fromAmpersand env multi (multi,False) :: Populations)
       ]


 



  

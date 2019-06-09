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

generateJSONfiles :: MultiFSpecs -> RIO App ()
generateJSONfiles multi = do
 env <- ask
 let opts@Options{..} = getOptions env
 sequence_ $
  if genRapPopulationOnly
  then [ writeJSONFile "metaPopulation" 
                                (fromAmpersand opts multi (multi,True) :: Populations)
       ]
  else [ writeJSONFile "settings"   (fromAmpersand opts multi multi :: Settings)
       , writeJSONFile "relations"  (fromAmpersand opts multi multi :: Relationz)
       , writeJSONFile "rules"      (fromAmpersand opts multi multi :: Rulez)
       , writeJSONFile "concepts"   (fromAmpersand opts multi multi :: Concepts)
       , writeJSONFile "conjuncts"  (fromAmpersand opts multi multi :: Conjuncts)
       , writeJSONFile "interfaces" (fromAmpersand opts multi multi :: Interfaces)
       , writeJSONFile "views"      (fromAmpersand opts multi multi :: Views)
       , writeJSONFile "roles"      (fromAmpersand opts multi multi :: Roles)
       , writeJSONFile "populations"(fromAmpersand opts multi (multi,False) :: Populations)
       ]


 



  

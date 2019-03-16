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
generateJSONfiles :: Options -> MultiFSpecs -> IO ()
generateJSONfiles opts@Options{..} multi =
 sequence_ $
  if genRapPopulationOnly
  then [ writeJSON "metaPopulation" 
                                (fromAmpersand opts multi (multi,True) :: Populations)
       ]
  else [ writeJSON "settings"   (fromAmpersand opts multi multi :: Settings)
       , writeJSON "relations"  (fromAmpersand opts multi multi :: Relationz)
       , writeJSON "rules"      (fromAmpersand opts multi multi :: Rulez)
       , writeJSON "concepts"   (fromAmpersand opts multi multi :: Concepts)
       , writeJSON "conjuncts"  (fromAmpersand opts multi multi :: Conjuncts)
       , writeJSON "interfaces" (fromAmpersand opts multi multi :: Interfaces)
       , writeJSON "views"      (fromAmpersand opts multi multi :: Views)
       , writeJSON "roles"      (fromAmpersand opts multi multi :: Roles)
       , writeJSON "populations"(fromAmpersand opts multi (multi,False) :: Populations)
       ]

  where 
    writeJSON :: ToJSON  a => String -> a -> IO()
    writeJSON = writeJSONFile opts 


 



  

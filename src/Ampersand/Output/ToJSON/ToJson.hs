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
generateJSONfiles :: MultiFSpecs -> IO ()
generateJSONfiles multi =
 sequence_ $
  if genRapPopulationOnly opts
  then [ writeJSON "metaPopulation" 
                                (fromAmpersand multi (multi,True) :: Populations)
       ]
  else [ writeJSON "settings"   (fromAmpersand multi multi :: Settings)
       , writeJSON "relations"  (fromAmpersand multi multi :: Relationz)
       , writeJSON "rules"      (fromAmpersand multi multi :: Rulez)
       , writeJSON "concepts"   (fromAmpersand multi multi :: Concepts)
       , writeJSON "conjuncts"  (fromAmpersand multi multi :: Conjuncts)
       , writeJSON "interfaces" (fromAmpersand multi multi :: Interfaces)
       , writeJSON "views"      (fromAmpersand multi multi :: Views)
       , writeJSON "roles"      (fromAmpersand multi multi :: Roles)
       , writeJSON "populations"(fromAmpersand multi (multi,False) :: Populations)
       ]

  where 
    opts = getOpts . userFSpec $ multi
    writeJSON :: ToJSON  a => String -> a -> IO()
    writeJSON = writeJSONFile opts 


 



  

module Ampersand.Output.ToJSON.ToJson
  (generateJSONfiles)
where
import Ampersand.Output.ToJSON.JSONutils
import Ampersand.Output.ToJSON.Settings
import Ampersand.Output.ToJSON.MySQLInstaller
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
  if genRap
  then [ writeJSON "extraPopulation"
                                (fromAmpersand multi multi :: MySQLInstaller)]
  else [ writeJSON "settings"   (fromAmpersand multi multi :: Settings)
       , writeJSON "mysql-installer"
                                (fromAmpersand multi multi :: MySQLInstaller)
       , writeJSON "relations"  (fromAmpersand multi multi :: Relations)
       , writeJSON "rules"      (fromAmpersand multi multi :: Rules)
       , writeJSON "concepts"   (fromAmpersand multi multi :: Concepts)
       , writeJSON "conjuncts"  (fromAmpersand multi multi :: Conjuncts)
       , writeJSON "interfaces" (fromAmpersand multi multi :: Interfaces)
       , writeJSON "views"      (fromAmpersand multi multi :: Views)
       , writeJSON "roles"      (fromAmpersand multi multi :: Roles)
       ]

  where 
    genRap = genRapPopulationOnly opts
    opts = getOpts fSpec
    fSpec = userFSpec multi
    writeJSON :: ToJSON  a => String -> a -> IO()
    writeJSON = writeJSONFile opts 
{- Note on data structure convention
   The data definitions in this module are not ment to be exported. The idea on naming is that all names
   contain a substring `JSON`. The part following that substring will be the name of the JSON attribute  -}

 



  

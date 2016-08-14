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
generateJSONfiles :: FSpec -> IO ()
generateJSONfiles fSpec =
 sequence_ [ writeJSON "settings"   (fromAmpersand fSpec fSpec :: Settings)
           , writeJSON "mysql-installer"
                                    (fromAmpersand fSpec fSpec :: MySQLInstaller)
           , writeJSON "relations"  (fromAmpersand fSpec fSpec :: Relations)
           , writeJSON "rules"      (fromAmpersand fSpec fSpec :: Rules)
           , writeJSON "concepts"   (fromAmpersand fSpec fSpec :: Concepts)
           , writeJSON "conjuncts"  (fromAmpersand fSpec fSpec :: Conjuncts)
           , writeJSON "interfaces" (fromAmpersand fSpec fSpec :: Interfaces)
           , writeJSON "views"      (fromAmpersand fSpec fSpec :: Views)
           , writeJSON "roles"      (fromAmpersand fSpec fSpec :: Roles)
           ]

  where 
    writeJSON :: ToJSON  a => String -> a -> IO()
    writeJSON = writeJSONFile fSpec 

{- Note on data structure convention
   The data definitions in this module are not ment to be exported. The idea on naming is that all names
   contain a substring `JSON`. The part following that substring will be the name of the JSON attribute  -}

 



  

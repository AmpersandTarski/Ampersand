module Database.Design.Ampersand.Output.ToJSON.ToJson
  (generateJSONfiles)
where
import Database.Design.Ampersand.Output.ToJSON.JSONutils
import Database.Design.Ampersand.Output.ToJSON.Settings
import Database.Design.Ampersand.Output.ToJSON.MySQLInstaller
import Database.Design.Ampersand.Output.ToJSON.Relations
import Database.Design.Ampersand.Output.ToJSON.Rules 
import Database.Design.Ampersand.Output.ToJSON.Concepts 
import Database.Design.Ampersand.Output.ToJSON.Conjuncts 
import Database.Design.Ampersand.Output.ToJSON.Interfaces 
import Database.Design.Ampersand.Output.ToJSON.Others
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
           , writeJSON "tables"     (fromAmpersand fSpec fSpec :: TableColumnInfos)
           , writeJSON "views"      (fromAmpersand fSpec fSpec :: Views)
           ]

  where 
    writeJSON :: ToJSON  a => String -> a -> IO()
    writeJSON = writeJSONFile fSpec 

{- Note on data structure convention
   The data definitions in this module are not ment to be exported. The idea on naming is that all names
   contain a substring `JSON`. The part following that substring will be the name of the JSON attribute  -}

 



  

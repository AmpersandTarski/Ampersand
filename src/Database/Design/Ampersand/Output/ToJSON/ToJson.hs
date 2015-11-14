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

generateJSONfiles :: FSpec -> IO ()
generateJSONfiles fSpec =
 sequence_ [ writeJSON "settings" settings
           , writeJSON "mysql-installer" mySqlInstaller
           , writeJSON "relations" relations
           , writeJSON "rules" rules
           , writeJSON "concepts" concepts
           , writeJSON "conjuncts" conjuncts
           , writeJSON "interfaces" interfaces
           ]

  where 
    writeJSON :: ToJSON  a => String -> a -> IO()
    writeJSON = writeJSONFile fSpec 
    settings :: Settings
    settings = fromAmpersand fSpec fSpec
    mySqlInstaller :: MySQLInstaller
    mySqlInstaller = fromAmpersand fSpec fSpec
    relations :: Relations
    relations = fromAmpersand fSpec fSpec
    rules :: Rules
    rules = fromAmpersand fSpec fSpec
    concepts :: Concepts
    concepts = fromAmpersand fSpec fSpec
    conjuncts :: Conjuncts
    conjuncts = fromAmpersand fSpec fSpec
    interfaces :: Interfaces
    interfaces = fromAmpersand fSpec fSpec
{- Note on data structure convention
   The data definitions in this module are not ment to be exported. The idea on naming is that all names
   contain a substring `JSON`. The part following that substring will be the name of the JSON attribute  -}

 



  

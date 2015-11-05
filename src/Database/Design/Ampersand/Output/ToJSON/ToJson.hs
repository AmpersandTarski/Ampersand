{-# LANGUAGE DeriveGeneric #-}

module Database.Design.Ampersand.Output.ToJSON.ToJson
  (generateJSONfiles)
where
import GHC.Generics
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.Output.ToJSON.JSONutils


generateJSONfiles :: FSpec -> IO ()
generateJSONfiles fSpec =
 sequence_ [ writeJSON "settings" settings]

  where 
    writeJSON = writeJSONFile fSpec 
    settings :: Settings
    settings = fromFspec fSpec

{- Note on data structure convention
   The data definitions in this module are not ment to be exported. The idea on naming is that all names
   contain a substring `JSON`. The part following that substring will be the name of the JSON attribute  -}

data Settings = Settings 
  { sngJSONversionInfo :: String
  , sngJSONcontextName :: String
  , sngJSONmysqlSettings :: MySQLSettings
  } deriving (Generic, Show)
instance JSON Settings where
 fromFspec fSpec = Settings 
  { sngJSONversionInfo   = ampersandVersionStr
  , sngJSONcontextName   = fsName $ fSpec
  , sngJSONmysqlSettings = fromFspec $ fSpec
  } 
instance ToJSON Settings where
  toJSON = amp2Jason
data MySQLSettings = MySQLSettings
  { msqlJSONdbHost :: String
  , msqlJSONdbName :: String
  , msqlJSONdbUser :: String
  , msqlJSONdbPass :: String
  , msqlJSONdbsignalTableName :: String
  } deriving (Generic, Show)
instance JSON MySQLSettings where
 fromFspec fSpec = MySQLSettings 
  { msqlJSONdbHost = sqlHost  . getOpts $ fSpec
  , msqlJSONdbName = dbName   . getOpts $ fSpec
  , msqlJSONdbUser = sqlLogin . getOpts $ fSpec
  , msqlJSONdbPass = sqlPwd   . getOpts $ fSpec
  , msqlJSONdbsignalTableName = "__all_signals__"
  }
instance ToJSON MySQLSettings where
  toJSON = amp2Jason


  

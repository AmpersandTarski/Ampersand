{-# LANGUAGE DeriveGeneric #-}
module Database.Design.Ampersand.Output.ToJSON.Settings 
  (Settings)
where
import Database.Design.Ampersand.Output.ToJSON.JSONutils 

data Settings = Settings 
  { sngJSONversionInfo :: String
  , sngJSONcontextName :: String
  , sngJSONmysqlSettings :: MySQLSettings
  } deriving (Generic, Show)
instance ToJSON Settings where
  toJSON = amp2Jason
instance JSON Settings where
 fromFspec fSpec = Settings 
  { sngJSONversionInfo   = ampersandVersionStr
  , sngJSONcontextName   = fsName $ fSpec
  , sngJSONmysqlSettings = fromFspec $ fSpec
  } 


data MySQLSettings = MySQLSettings
  { msqlJSONdbHost :: String
  , msqlJSONdbName :: String
  , msqlJSONdbUser :: String
  , msqlJSONdbPass :: String
  , msqlJSONdbsignalTableName :: String
  } deriving (Generic, Show)
instance ToJSON MySQLSettings where
  toJSON = amp2Jason
instance JSON MySQLSettings where
 fromFspec fSpec = MySQLSettings 
  { msqlJSONdbHost = sqlHost  . getOpts $ fSpec
  , msqlJSONdbName = dbName   . getOpts $ fSpec
  , msqlJSONdbUser = sqlLogin . getOpts $ fSpec
  , msqlJSONdbPass = sqlPwd   . getOpts $ fSpec
  , msqlJSONdbsignalTableName = "__all_signals__"
  }

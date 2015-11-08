{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
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
instance JSON FSpec Settings where
 fromAmpersand fSpec _ = Settings 
  { sngJSONversionInfo   = ampersandVersionStr
  , sngJSONcontextName   = fsName $ fSpec
  , sngJSONmysqlSettings = fromAmpersand fSpec fSpec
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
instance JSON FSpec MySQLSettings where
 fromAmpersand fSpec _ = MySQLSettings 
  { msqlJSONdbHost = sqlHost  . getOpts $ fSpec
  , msqlJSONdbName = dbName   . getOpts $ fSpec
  , msqlJSONdbUser = sqlLogin . getOpts $ fSpec
  , msqlJSONdbPass = sqlPwd   . getOpts $ fSpec
  , msqlJSONdbsignalTableName = "__all_signals__"
  }

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
module Ampersand.Output.ToJSON.Settings 
  (Settings)
where
import Ampersand.Output.ToJSON.JSONutils 
import qualified Data.Text as Text

data Settings = Settings 
  { sngJSONversionInfo :: String
  , sngJSONcontextName :: String
  , sngJSONmysqlSettings :: MySQLSettings
  , sngJSONenvironment :: String
  } deriving (Generic, Show)
instance ToJSON Settings where
  toJSON = amp2Jason
instance JSON MultiFSpecs Settings where
 fromAmpersand multi _ = Settings 
  { sngJSONversionInfo   = ampersandVersionStr
  , sngJSONcontextName   = Text.unpack (fsName fSpec)
  , sngJSONmysqlSettings = fromAmpersand multi multi
  , sngJSONenvironment   = show . environment . getOpts $ fSpec
  } 
   where fSpec = userFSpec multi

data MySQLSettings = MySQLSettings
  { msqlJSONdbHost :: String
  , msqlJSONdbName :: String
  , msqlJSONdbUser :: String
  , msqlJSONdbPass :: String
  , msqlJSONdbsignalTableName :: String
  } deriving (Generic, Show)
instance ToJSON MySQLSettings where
  toJSON = amp2Jason
instance JSON MultiFSpecs MySQLSettings where
 fromAmpersand multi _ = MySQLSettings 
  { msqlJSONdbHost = sqlHost  opts
  , msqlJSONdbName = dbName   opts
  , msqlJSONdbUser = sqlLogin opts
  , msqlJSONdbPass = sqlPwd   opts
  , msqlJSONdbsignalTableName = "__all_signals__"
  }
   where opts = getOpts $ userFSpec multi

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
module Ampersand.Output.ToJSON.Settings 
  (Settings)
where
import           Ampersand.Output.ToJSON.JSONutils 
import           Data.Hashable
import qualified Data.Text as Text

data Settings = Settings 
  { sngJSONglobal_contextName :: String
  , sngJSONmysql_dbHost       :: String
  , sngJSONmysql_dbName       :: String
  , sngJSONmysql_dbUser       :: String
  , sngJSONmysql_dbPass       :: String
  , sngJSONcompiler_version   :: String
  , sngJSONcompiler_env       :: String
  , sngJSONcompiler_modelHash :: String
  } deriving (Generic, Show)
instance ToJSON Settings where
  toJSON = amp2Jason
instance JSON MultiFSpecs Settings where
 fromAmpersand multi _ = Settings 
  { sngJSONglobal_contextName = Text.unpack (fsName fSpec)
  , sngJSONmysql_dbHost       = sqlHost  opts
  , sngJSONmysql_dbName       = dbName   opts
  , sngJSONmysql_dbUser       = sqlLogin opts
  , sngJSONmysql_dbPass       = sqlPwd   opts
  , sngJSONcompiler_version   = ampersandVersionStr
  , sngJSONcompiler_env       = show . environment . getOpts $ fSpec
  , sngJSONcompiler_modelHash = show . hash $ fSpec
  } 
   where fSpec = userFSpec multi
         opts = getOpts fSpec

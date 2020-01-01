{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE RecordWildCards #-} 
module Ampersand.Output.ToJSON.Settings 
  (Settings)
where
import           Ampersand.Output.ToJSON.JSONutils 
import           Data.Hashable
import qualified RIO.Text as T

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
  toJSON = amp2Jason'
instance JSON' FSpec Settings where
 fromAmpersand' env fSpec _ = Settings 
  { sngJSONglobal_contextName = T.unpack (fsName fSpec)
  , sngJSONmysql_dbHost       = view sqlHostL env
  , sngJSONmysql_dbName       = case view dbNameL env of
                                  Nothing -> name fSpec
                                  Just nm -> nm
  , sngJSONmysql_dbUser       = view sqlLoginL env
  , sngJSONmysql_dbPass       = view sqlPwdL env
  , sngJSONcompiler_version   = ampersandVersionStr
  , sngJSONcompiler_env       = show env 
  , sngJSONcompiler_modelHash = show . hash $ fSpec
  } 

         

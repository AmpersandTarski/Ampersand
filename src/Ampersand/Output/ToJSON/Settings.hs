{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE RecordWildCards #-} 
module Ampersand.Output.ToJSON.Settings 
  (Settings)
where
import           Ampersand.Output.ToJSON.JSONutils 
import           Data.Hashable

data Settings = Settings 
  { sngJSONglobal_contextName :: Text
  , sngJSONmysql_dbHost       :: Text
  , sngJSONmysql_dbName       :: Text
  , sngJSONmysql_dbUser       :: Text
  , sngJSONmysql_dbPass       :: Text
  , sngJSONcompiler_version   :: Text
  , sngJSONcompiler_env       :: Text
  , sngJSONcompiler_modelHash :: Text
  } deriving (Generic, Show)
instance ToJSON Settings where
  toJSON = amp2Jason'
instance JSON' FSpec Settings where
 fromAmpersand' env fSpec _ = Settings 
  { sngJSONglobal_contextName = fsName fSpec
  , sngJSONmysql_dbHost       = view sqlHostL env
  , sngJSONmysql_dbName       = case view dbNameL env of
                                  Nothing -> name fSpec
                                  Just nm -> nm
  , sngJSONmysql_dbUser       = view sqlLoginL env
  , sngJSONmysql_dbPass       = view sqlPwdL env
  , sngJSONcompiler_version   = ampersandVersionStr
  , sngJSONcompiler_env       = tshow env
  , sngJSONcompiler_modelHash = tshow . hash $ fSpec
  } 

         

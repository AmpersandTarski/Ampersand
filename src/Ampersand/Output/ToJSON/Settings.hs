{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ampersand.Output.ToJSON.Settings (Settings) where

import Ampersand.Output.ToJSON.JSONutils

data Settings = Settings
  { sngJSONglobal_contextName :: Text,
    sngJSONglobal_productionEnv :: Bool,
    sngJSONcompiler_version :: Text,
    sngJSONcompiler_env :: Text,
    sngJSONcompiler_modelHash :: Text
  }
  deriving (Generic, Show)

instance ToJSON Settings where
  toJSON = amp2Jason'

instance JSON' FSpec Settings where
  fromAmpersand' env fSpec _ =
    Settings
      { sngJSONglobal_contextName = fullName fSpec,
        -- Pass the build target on to the prototype framework, so a production
        -- build hides developer interfaces and does not publish the OpenAPI docs.
        sngJSONglobal_productionEnv = xproduction (view protoOptsL env),
        sngJSONcompiler_version = longVersion appVersion,
        sngJSONcompiler_env = tshow env,
        sngJSONcompiler_modelHash = tshow . hash $ fSpec
      }

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ampersand.Output.ToJSON.Settings (Settings) where

import Ampersand.Output.ToJSON.JSONutils
import Data.Hashable

data Settings = Settings
  { sngJSONglobal_contextName :: Text,
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
      { sngJSONglobal_contextName = text1ToText . tName $fSpec,
        sngJSONcompiler_version = longVersion appVersion,
        sngJSONcompiler_env = tshow env,
        sngJSONcompiler_modelHash = tshow . hash $ fSpec
      }

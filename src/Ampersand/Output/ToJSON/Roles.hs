{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ampersand.Output.ToJSON.Roles (Roles) where

import Ampersand.ADL1
import Ampersand.ADL1.DerivedViews (rulesInEnforceOrder)
import Ampersand.Classes
import Ampersand.Output.ToJSON.JSONutils

newtype Roles = Roles [RoleJson] deriving (Generic, Show)

data RoleJson = RoleJson
  { roleJSONname :: Text,
    roleJSONlabel :: Text,
    roleJSONisService :: Bool,
    roleJSONmaintains :: [Text]
  }
  deriving (Generic, Show)

instance ToJSON RoleJson where
  toJSON = amp2Jason

instance ToJSON Roles where
  toJSON = amp2Jason

instance JSON FSpec Roles where
  fromAmpersand env fSpec _ = Roles . map (fromAmpersand env fSpec) . fRoles $ fSpec

instance JSON (Role, Int) RoleJson where
  fromAmpersand _ fSpec (role', _) =
    RoleJson
      { roleJSONname = text1ToText . idWithoutType' $ role',
        roleJSONlabel = label role',
        roleJSONisService = rlIsService role',
        -- The ENFORCE rules first, in the order of their dependencies (issue #1749).
        roleJSONmaintains = map fullName . rulesInEnforceOrder (enforces fSpec) . toList . fMaintains fSpec $ role'
      }

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
module Ampersand.Output.ToJSON.Roles 
    (Roles)
where

import Ampersand.Output.ToJSON.JSONutils
import Ampersand.Core.AbstractSyntaxTree

data Roles = Roles [RoleJson] deriving (Generic, Show)
data RoleJson = RoleJson
  { roleJSONid         :: Int
  , roleJSONname       :: String
  , roleJSONmaintains  :: [String] 
  , roleJSONinterfaces :: [String] 
  } deriving (Generic, Show)
instance ToJSON RoleJson where
  toJSON = amp2Jason
instance ToJSON Roles where
  toJSON = amp2Jason
instance JSON MultiFSpecs Roles where
 fromAmpersand multi _ = Roles . map (fromAmpersand multi) . fRoles $ fSpec
   where fSpec = userFSpec multi
instance JSON (Role,Int) RoleJson where
 fromAmpersand multi (role,i) = RoleJson
  { roleJSONid         = i
  , roleJSONname       = name role
  , roleJSONmaintains  = map name . fMaintains     fSpec $ role
  , roleJSONinterfaces = map (escapeIdentifier . name) . roleInterfaces fSpec $ role
  }
   where fSpec = userFSpec multi









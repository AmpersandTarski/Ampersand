{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE RecordWildCards #-} 
module Ampersand.Output.ToJSON.Roles 
    (Roles)
where
import           Ampersand.Output.ToJSON.JSONutils
import qualified RIO.Set as Set

data Roles = Roles [RoleJson] deriving (Generic, Show)
data RoleJson = RoleJson
  { roleJSONid         :: Text
  , roleJSONname       :: Text
  , roleJSONmaintains  :: [Text] 
  } deriving (Generic, Show)
instance ToJSON RoleJson where
  toJSON = amp2Jason
instance ToJSON Roles where
  toJSON = amp2Jason
instance JSON FSpec Roles where
 fromAmpersand env fSpec _ = Roles . map (fromAmpersand env fSpec) . fRoles $ fSpec
instance JSON (Role,Int) RoleJson where
 fromAmpersand _ fSpec (role',_) = RoleJson
  { roleJSONid         = idWithoutType role'
  , roleJSONname       = name role'
  , roleJSONmaintains  = map name . Set.elems . fMaintains fSpec $ role'
  }










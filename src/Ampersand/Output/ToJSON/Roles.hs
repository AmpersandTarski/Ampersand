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
  { roleJSONid         :: String
  , roleJSONname       :: String
  , roleJSONmaintains  :: [String] 
  } deriving (Generic, Show)
instance ToJSON RoleJson where
  toJSON = amp2Jason
instance ToJSON Roles where
  toJSON = amp2Jason
instance JSON MultiFSpecs Roles where
 fromAmpersand env multi _ = Roles . map (fromAmpersand env multi) . fRoles $ fSpec
   where fSpec = userFSpec multi
instance JSON (Role,Int) RoleJson where
 fromAmpersand _ multi (role',_) = RoleJson
  { roleJSONid         = idWithoutType role'
  , roleJSONname       = name role'
  , roleJSONmaintains  = map name . Set.elems . fMaintains fSpec $ role'
  }
   where fSpec = userFSpec multi









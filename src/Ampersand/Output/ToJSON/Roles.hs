{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE RecordWildCards #-} 
module Ampersand.Output.ToJSON.Roles 
    (Roles)
where
import           Ampersand.Output.ToJSON.JSONutils
import qualified Data.Set as Set

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
 fromAmpersand opts@Options{..} multi _ = Roles . map (fromAmpersand opts multi) . fRoles $ fSpec
   where fSpec = userFSpec multi
instance JSON (Role,Int) RoleJson where
 fromAmpersand _ multi (role',i) = RoleJson
  { roleJSONid         = i
  , roleJSONname       = name role'
  , roleJSONmaintains  = map name . Set.elems .fMaintains     fSpec $ role'
  , roleJSONinterfaces = map idWithoutType . roleInterfaces fSpec $ role'
  }
   where fSpec = userFSpec multi









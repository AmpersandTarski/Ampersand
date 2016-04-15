{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
module Database.Design.Ampersand.Output.ToJSON.Roles 
    (Roles)
where
import Database.Design.Ampersand.Output.ToJSON.JSONutils 
import Database.Design.Ampersand.Core.AbstractSyntaxTree 
import Database.Design.Ampersand.Prototype.ProtoUtil

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
instance JSON FSpec Roles where
 fromAmpersand fSpec _ = Roles . map (fromAmpersand fSpec) . fRoles $ fSpec
instance JSON (Role,Int) RoleJson where
 fromAmpersand fSpec (role,i) = RoleJson
  { roleJSONid         = i
  , roleJSONname       = name role
  , roleJSONmaintains  = map name . fMaintains     fSpec $ role
  , roleJSONinterfaces = map (escapeIdentifier . name) . roleInterfaces fSpec $ role
  }









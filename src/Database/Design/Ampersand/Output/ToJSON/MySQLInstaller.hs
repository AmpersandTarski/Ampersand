{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
module Database.Design.Ampersand.Output.ToJSON.MySQLInstaller 
  (MySQLInstaller)
where
import Database.Design.Ampersand.Output.ToJSON.JSONutils 
import Database.Design.Ampersand.Prototype.Generate 

data MySQLInstaller = MySQLInstaller
  { msiJSONallDBstructQueries :: [String]
  , msiJSONallDefPopQueries   :: [String]
  } deriving (Generic, Show)
instance ToJSON MySQLInstaller where
  toJSON = amp2Jason
instance JSON FSpec MySQLInstaller where
 fromAmpersand fSpec _ = MySQLInstaller
  { msiJSONallDBstructQueries = generateDBstructQueries  fSpec
  , msiJSONallDefPopQueries   = generateAllDefPopQueries fSpec
  }
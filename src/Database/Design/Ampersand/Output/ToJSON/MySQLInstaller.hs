{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
module Database.Design.Ampersand.Output.ToJSON.MySQLInstaller 
  (MySQLInstaller)
where
import Database.Design.Ampersand.Output.ToJSON.JSONutils 
import Database.Design.Ampersand.Prototype.Generate 
import qualified Data.Text as Text
 
data MySQLInstaller = MySQLInstaller
  { msiJSONallDBstructQueries :: [Text.Text]
  , msiJSONallDefPopQueries   :: [Text.Text]
  } deriving (Generic, Show)
instance ToJSON MySQLInstaller where
  toJSON = amp2Jason
instance JSON FSpec MySQLInstaller where
 fromAmpersand fSpec _ = MySQLInstaller
  { msiJSONallDBstructQueries = map Text.pack$ generateDBstructQueries  fSpec False
  , msiJSONallDefPopQueries   = generateAllDefPopQueries fSpec
  }
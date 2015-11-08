{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
module Database.Design.Ampersand.Output.ToJSON.MySQLInstaller 
  (MySQLInstaller)
where
import Database.Design.Ampersand.Output.ToJSON.JSONutils 
import Database.Design.Ampersand.Prototype.Generate 

data MySQLInstaller = MySQLInstaller
  { msiJSONallDBstructQueries :: [Ordered String]
  , msiJSONallDefPopQueries   :: [Ordered String]
  } deriving (Generic, Show)
instance ToJSON MySQLInstaller where
  toJSON = amp2Jason
instance JSON FSpec MySQLInstaller where
 fromAmpersand fSpec _ = MySQLInstaller
  { msiJSONallDBstructQueries = order $ generateDBstructQueries  True fSpec
  , msiJSONallDefPopQueries   = order $ generateAllDefPopQueries True fSpec
  }
order :: [a] -> [Ordered a]
order xs = map f (zip [0..] xs)
  where f (i,x) = Ordered i x
data Ordered a = Ordered 
 { ordJSONseqNr :: Int
 , ordJSONthing :: a
 } deriving (Generic, Show)
instance ToJSON a => ToJSON (Ordered a) where
  toJSON = amp2Jason
instance ToJSON a => JSON (Int, a) (Ordered a) where
  fromAmpersand _ (i,x) = Ordered
    { ordJSONseqNr = i
    , ordJSONthing = x
    }
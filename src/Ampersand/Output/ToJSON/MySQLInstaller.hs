{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
module Ampersand.Output.ToJSON.MySQLInstaller 
  (MySQLInstaller)
where
import Ampersand.Output.ToJSON.JSONutils 
import Ampersand.Prototype.Generate 
import qualified Data.Text as Text
import Data.Maybe

data MySQLInstaller = 
        CompleteDB
          { msiJSONallDBstructQueries :: [Text.Text]
          , msiJSONallDefPopQueries   :: [Text.Text]
          }
      | ExtraPopulation
          { msiJSONrapPopulation      :: [Text.Text]
          } 
        deriving (Generic, Show)
instance ToJSON MySQLInstaller where
  toJSON = amp2Jason
instance JSON MultiFSpecs MySQLInstaller where
 fromAmpersand _ multi
   | genRap = ExtraPopulation
        { msiJSONrapPopulation = generateAllDefPopQueries grindedFSpec
        }
   | otherwise = CompleteDB
        { msiJSONallDBstructQueries = map Text.pack $ generateDBstructQueries  fSpec False
        , msiJSONallDefPopQueries = generateAllDefPopQueries fSpec
        }
  where
    genRap = genRapPopulationOnly (getOpts fSpec)
    fSpec = userFSpec multi
    grindedFSpec = fromMaybe ftl (metaFSpec multi)
      where ftl = fatal 27 "There is no grinded fSpec."
-- TODO: zie Github issue #521
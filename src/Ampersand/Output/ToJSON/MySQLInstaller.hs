{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
module Ampersand.Output.ToJSON.MySQLInstaller 
  (MySQLInstaller,MetaPopulation)
where
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Output.ToJSON.JSONutils 
import Ampersand.Prototype.Generate 
import qualified Data.Text as Text
import Data.Maybe

data MySQLInstaller = MySQLInstaller
   { msiJSONallDBstructQueries :: [Text.Text]
   , msiJSONallDefPopQueries   :: [Text.Text]
   } deriving (Generic, Show)
instance ToJSON MySQLInstaller where
  toJSON = amp2Jason
instance JSON MultiFSpecs MySQLInstaller where
 fromAmpersand _ multi = MySQLInstaller
        { msiJSONallDBstructQueries = generateDBstructQueries fSpec False
        , msiJSONallDefPopQueries = generateInitialPopQueries fSpec
        }
  where
    fSpec = userFSpec multi

data MetaPopulation = MetaPopulation
   { epJSONatoms :: [AtomValuesOfConcept]
   , epJSONlinks :: [PairsOfRelation]
   } deriving (Generic, Show)
data AtomValuesOfConcept = AtomValuesOfConcept
   { avcJSONconcept :: Text.Text
   , avcJSONatoms :: [Text.Text]
   } deriving (Generic, Show)
data PairsOfRelation = PairsOfRelation
   { porJSONrelation :: Text.Text
   , porJSONlinks :: [JPair]
   } deriving (Generic, Show)
data JPair = JPair
   { prJSONsrc :: Text.Text
   , prJSONtgt :: Text.Text
   } deriving (Generic, Show)
instance ToJSON MetaPopulation where
  toJSON = amp2Jason
instance ToJSON AtomValuesOfConcept where
  toJSON = amp2Jason
instance ToJSON PairsOfRelation where
  toJSON = amp2Jason
instance ToJSON JPair where
  toJSON = amp2Jason
instance JSON MultiFSpecs MetaPopulation where
 fromAmpersand _ multi = MetaPopulation
   { epJSONatoms = map (fromAmpersand multi) (allConcepts grindedFSpec)
   , epJSONlinks = map (fromAmpersand multi) (vrels       grindedFSpec)
   }
  where 
   grindedFSpec = fromMaybe ftl (metaFSpec multi)
     where ftl = fatal 71 "There is no grinded fSpec."
instance JSON A_Concept AtomValuesOfConcept where
 fromAmpersand multi cpt = AtomValuesOfConcept
   { avcJSONconcept = Text.pack (name cpt)
   , avcJSONatoms   = map (Text.pack . showValADL) (atomsBySmallestConcept grindedFSpec cpt)
   }
  where 
   grindedFSpec = fromMaybe ftl (metaFSpec multi)
     where ftl = fatal 79 "There is no grinded fSpec."

instance JSON Declaration PairsOfRelation where
 fromAmpersand multi dcl = PairsOfRelation
   { porJSONrelation = Text.pack $ showDcl False dcl
   , porJSONlinks = map (fromAmpersand multi) . pairsInExpr grindedFSpec $ EDcD dcl
   }
  where 
   grindedFSpec = fromMaybe ftl (metaFSpec multi)
     where ftl = fatal 88 "There is no grinded fSpec."
instance JSON AAtomPair JPair where
  fromAmpersand _ p = JPair
    { prJSONsrc = Text.pack . showValADL . apLeft $ p 
    , prJSONtgt = Text.pack . showValADL . apRight $ p
    }


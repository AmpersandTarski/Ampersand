{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE RecordWildCards #-} 
module Ampersand.Output.ToJSON.Populations 
  (Populations)
where
import           Ampersand.ADL1
import           Ampersand.Basics
import           Ampersand.Output.ToJSON.JSONutils
import qualified RIO.Set as Set

data Populations = Populations
   { epJSONatoms :: [AtomValuesOfConcept]
   , epJSONlinks :: [PairsOfRelation]
   } deriving (Generic, Show)
data AtomValuesOfConcept = AtomValuesOfConcept
   { avcJSONconcept :: Text
   , avcJSONatoms :: [Text]
   } deriving (Generic, Show)
data PairsOfRelation = PairsOfRelation
   { porJSONrelation :: Text
   , porJSONlinks :: [JPair]
   } deriving (Generic, Show)
data JPair = JPair
   { prJSONsrc :: Text
   , prJSONtgt :: Text
   } deriving (Generic, Show)
instance ToJSON Populations where
  toJSON = amp2Jason
instance ToJSON AtomValuesOfConcept where
  toJSON = amp2Jason
instance ToJSON PairsOfRelation where
  toJSON = amp2Jason
instance ToJSON JPair where
  toJSON = amp2Jason
instance JSON FSpec Populations where
 fromAmpersand env _ fSpec = Populations
   { epJSONatoms = map (fromAmpersand env fSpec) (Set.elems $ allConcepts fSpec)
   , epJSONlinks = map (fromAmpersand env fSpec) (Set.elems $ vrels       fSpec)
   }

-- instance JSON (MultiFSpecs,Bool) Populations where
--  fromAmpersand env _ (multi,doMeta) = Populations
--    { epJSONatoms = map (fromAmpersand env multi) (zip (Set.elems $ allConcepts theFSpec) (L.repeat doMeta))
--    , epJSONlinks = map (fromAmpersand env multi) (zip (Set.elems $ vrels       theFSpec) (L.repeat doMeta))
--    }
--   where 
--    theFSpec 
--     | doMeta    = fromMaybe ftl (metaFSpec multi)
--     | otherwise = userFSpec multi
--      where ftl = fatal "There is no grinded fSpec."
instance JSON A_Concept AtomValuesOfConcept where
 fromAmpersand _ fSpec cpt = AtomValuesOfConcept
   { avcJSONconcept = idWithoutType cpt
   , avcJSONatoms   = map showValADL (Set.elems $ atomsBySmallestConcept fSpec cpt)
   }

instance JSON Relation PairsOfRelation where
 fromAmpersand env fSpec dcl = PairsOfRelation
   { porJSONrelation = showRel $ dcl
   , porJSONlinks = map (fromAmpersand env fSpec) . Set.elems . pairsInExpr fSpec $ EDcD dcl
   }

instance JSON AAtomPair JPair where
  fromAmpersand _ _ p = JPair
    { prJSONsrc = showValADL . apLeft $ p 
    , prJSONtgt = showValADL . apRight $ p
    }


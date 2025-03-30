{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ampersand.Output.ToJSON.Populations (Populations) where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Output.ToJSON.JSONutils

data Populations = Populations
  { epJSONatoms :: [AtomValuesOfConcept],
    epJSONlinks :: [PairsOfRelation]
  }
  deriving (Generic, Show)

data AtomValuesOfConcept = AtomValuesOfConcept
  { avcJSONconcept :: Text,
    avcJSONatoms :: [Text]
  }
  deriving (Generic, Show)

data PairsOfRelation = PairsOfRelation
  { porJSONrelation :: Text,
    porJSONlinks :: [JPair]
  }
  deriving (Generic, Show)

data JPair = JPair
  { prJSONsrc :: Text,
    prJSONtgt :: Text
  }
  deriving (Generic, Show)

instance ToJSON Populations where
  toJSON = amp2Jason

instance ToJSON AtomValuesOfConcept where
  toJSON = amp2Jason

instance ToJSON PairsOfRelation where
  toJSON = amp2Jason

instance ToJSON JPair where
  toJSON = amp2Jason

instance JSON FSpec Populations where
  fromAmpersand env _ fSpec =
    Populations
      { epJSONatoms = map (fromAmpersand env fSpec) ((toList . concs) fSpec),
        epJSONlinks = map (fromAmpersand env fSpec) ((toList . vrels) fSpec)
      }

-- instance JSON (MultiFSpecs,Bool) Populations where
--  fromAmpersand env _ (multi,doMeta) = Populations
--    { epJSONatoms = map (fromAmpersand env multi) (zip (toList . concs) theFSpec) (L.repeat doMeta))
--    , epJSONlinks = map (fromAmpersand env multi) (zip (toList . vrels) theFSpec) (L.repeat doMeta))
--    }
--   where
--    theFSpec
--     | doMeta    = fromMaybe ftl (metaFSpec multi)
--     | otherwise = userFSpec multi
--      where ftl = fatal "There is no grinded fSpec."
instance JSON A_Concept AtomValuesOfConcept where
  fromAmpersand _ fSpec cpt =
    AtomValuesOfConcept
      { avcJSONconcept = text1ToText . idWithoutType' $ cpt,
        avcJSONatoms = map showValADL (toList $ atomsBySmallestConcept fSpec cpt)
      }

instance JSON Relation PairsOfRelation where
  fromAmpersand env fSpec dcl =
    PairsOfRelation
      { porJSONrelation = tshow dcl,
        porJSONlinks = map (fromAmpersand env fSpec) . toList . pairsInExpr fSpec $ EDcD dcl
      }

instance JSON AAtomPair JPair where
  fromAmpersand _ _ p =
    JPair
      { prJSONsrc = showValADL . apLeft $ p,
        prJSONtgt = showValADL . apRight $ p
      }

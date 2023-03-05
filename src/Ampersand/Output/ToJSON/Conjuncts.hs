{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ampersand.Output.ToJSON.Conjuncts (Conjuncts) where

import Ampersand.ADL1
import Ampersand.FSpec.ToFSpec.NormalForms (conjNF)
import Ampersand.Output.ToJSON.JSONutils
import qualified RIO.NonEmpty as NE

newtype Conjuncts = Conjuncts [JSONConjunct]
  deriving (Generic, Show)

data JSONConjunct = JSONConjunct
  { cnjJSONid :: Text,
    cnjJSONsignalRuleNames :: [Text],
    cnjJSONinvariantRuleNames :: [Text],
    cnjJSONviolationsSQL :: Text
  }
  deriving (Generic, Show)

instance ToJSON JSONConjunct where
  toJSON = amp2Jason

instance ToJSON Conjuncts where
  toJSON = amp2Jason

instance JSON FSpec Conjuncts where
  fromAmpersand env fSpec _ = Conjuncts . map (fromAmpersand env fSpec) . allConjuncts $ fSpec

instance JSON Conjunct JSONConjunct where
  fromAmpersand env fSpec conj =
    JSONConjunct
      { cnjJSONid = text1ToText . rc_id $ conj,
        cnjJSONsignalRuleNames = map (text1ToText . tName) . filter (isSignal fSpec) . NE.toList . rc_orgRules $ conj,
        cnjJSONinvariantRuleNames = map (text1ToText . tName) . filter (not . isSignal fSpec) . NE.toList . rc_orgRules $ conj,
        cnjJSONviolationsSQL = sqlQuery fSpec . conjNF env . notCpl . rcConjunct $ conj
      }

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE RecordWildCards #-} 
module Ampersand.Output.ToJSON.Conjuncts 
  (Conjuncts)
where
import           Ampersand.ADL1
import           Ampersand.FSpec.ToFSpec.NormalForms (conjNF)
import           Ampersand.Output.ToJSON.JSONutils 
import qualified RIO.NonEmpty as NE

data Conjuncts = Conjuncts [JSONConjunct] deriving (Generic, Show)
data JSONConjunct = JSONConjunct
  { cnjJSONid                 :: Text
  , cnjJSONsignalRuleNames    :: [Text]
  , cnjJSONinvariantRuleNames :: [Text]
  , cnjJSONviolationsSQL      :: Text
  } deriving (Generic, Show)
instance ToJSON JSONConjunct where
  toJSON = amp2Jason
instance ToJSON Conjuncts where
  toJSON = amp2Jason
instance JSON FSpec Conjuncts where
 fromAmpersand env fSpec _ = Conjuncts . map (fromAmpersand env fSpec) . allConjuncts $ fSpec
instance JSON Conjunct JSONConjunct where
 fromAmpersand env fSpec conj = JSONConjunct
  { cnjJSONid                  = rc_id conj
  , cnjJSONsignalRuleNames     = map name . filter        isSignal  . NE.toList . rc_orgRules $ conj
  , cnjJSONinvariantRuleNames  = map name . filter (not . isSignal) . NE.toList . rc_orgRules $ conj
  , cnjJSONviolationsSQL       = sqlQuery fSpec . conjNF env . notCpl . rc_conjunct $ conj
  }

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE RecordWildCards #-} 
module Ampersand.Output.ToJSON.Conjuncts 
  (Conjuncts)
where
import           Ampersand.ADL1
import           Ampersand.FSpec.ToFSpec.NormalForms (conjNF)
import           Ampersand.Output.ToJSON.JSONutils 
import qualified Data.List.NonEmpty as NEL

data Conjuncts = Conjuncts [JSONConjunct] deriving (Generic, Show)
data JSONConjunct = JSONConjunct
  { cnjJSONid                 :: String
  , cnjJSONsignalRuleNames    :: [String]
  , cnjJSONinvariantRuleNames :: [String]
  , cnjJSONviolationsSQL      :: String
  } deriving (Generic, Show)
instance ToJSON JSONConjunct where
  toJSON = amp2Jason
instance ToJSON Conjuncts where
  toJSON = amp2Jason
instance JSON FSpec Conjuncts where
 fromAmpersand env multi _ = Conjuncts . map (fromAmpersand env multi) . allConjuncts . userFSpec $ multi
instance JSON Conjunct JSONConjunct where
 fromAmpersand env fSpec conj = JSONConjunct
  { cnjJSONid                  = rc_id conj
  , cnjJSONsignalRuleNames     = map name . filter        isSignal  . NEL.toList . rc_orgRules $ conj
  , cnjJSONinvariantRuleNames  = map name . filter (not . isSignal) . NEL.toList . rc_orgRules $ conj
  , cnjJSONviolationsSQL       = sqlQuery fSpec . conjNF env . notCpl . rc_conjunct $ conj
  }

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
module Ampersand.Output.ToJSON.Conjuncts 
  (Conjuncts)
where
import Ampersand.FSpec.ToFSpec.NormalForms (conjNF)
import Ampersand.Output.ToJSON.JSONutils 
import Ampersand.Core.AbstractSyntaxTree 

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
instance JSON MultiFSpecs Conjuncts where
 fromAmpersand multi _ = Conjuncts . map (fromAmpersand multi) . allConjuncts . userFSpec $ multi
instance JSON Conjunct JSONConjunct where
 fromAmpersand multi conj = JSONConjunct
  { cnjJSONid                  = rc_id conj
  , cnjJSONsignalRuleNames     = map name . filter        isSignal  . rc_orgRules $ conj
  , cnjJSONinvariantRuleNames  = map name . filter (not . isSignal) . rc_orgRules $ conj
  , cnjJSONviolationsSQL       = sqlQuery fSpec . conjNF (getOpts fSpec) . notCpl . rc_conjunct $ conj
  }
   where 
    fSpec = userFSpec multi
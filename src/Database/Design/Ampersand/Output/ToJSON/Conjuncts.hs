{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
module Database.Design.Ampersand.Output.ToJSON.Conjuncts 
  (Conjuncts)
where
import Database.Design.Ampersand.FSpec.ToFSpec.Calc (showPrf)
import Database.Design.Ampersand.FSpec.ToFSpec.NormalForms (cfProof,conjNF)
import Database.Design.Ampersand.FSpec.ShowADL (showADL)
import Database.Design.Ampersand.Output.ToJSON.JSONutils 
import Database.Design.Ampersand.Core.AbstractSyntaxTree 

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
 fromAmpersand fSpec _ = Conjuncts (map (fromAmpersand fSpec) (allConjuncts fSpec))
instance JSON Conjunct JSONConjunct where
 fromAmpersand fSpec conj = JSONConjunct
  { cnjJSONid                  = rc_id conj
  , cnjJSONsignalRuleNames     = map name . filter isSignal . rc_orgRules $ conj
  , cnjJSONinvariantRuleNames  = map name . filter (not . isSignal) . filter (not . ruleIsInvariantUniOrInj) . rc_orgRules $ conj
  , cnjJSONviolationsSQL       = sqlQuery fSpec . conjNF (getOpts fSpec) . notCpl . rc_conjunct $ conj
  }
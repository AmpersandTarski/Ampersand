{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
module Database.Design.Ampersand.Output.ToJSON.Conjuncts 
  (Conjuncts)
where
import Database.Design.Ampersand.FSpec.ToFSpec.Calc (showPrf)
import Database.Design.Ampersand.FSpec.ToFSpec.NormalForms (cfProof,conjNF)
import Database.Design.Ampersand.FSpec.ShowADL (showADL)
import Database.Design.Ampersand.FSpec.SQL
import Database.Design.Ampersand.Output.ToJSON.JSONutils 
import Database.Design.Ampersand.Core.AbstractSyntaxTree 
import Database.Design.Ampersand.Basics

data Conjuncts = Conjuncts [JSONConjunct] deriving (Generic, Show)
data JSONConjunct = JSONConjunct
  { cnjJSONId                 :: String
  , cnjJSONsignalRuleNames    :: [String]
  , cnjJSONinvariantRuleNames :: [String]
  , cnjJSONviolationsSQL      :: String
  , cnjJSONNormalizationSteps :: Maybe [String] -- Not used in frontend. Just informative for analisys
  } deriving (Generic, Show)
instance ToJSON JSONConjunct where
  toJSON = amp2Jason
instance ToJSON Conjuncts where
  toJSON = amp2Jason
instance JSON FSpec Conjuncts where
 fromAmpersand fSpec _ = Conjuncts (map (fromAmpersand fSpec) (vconjs fSpec))
instance JSON Conjunct JSONConjunct where
 fromAmpersand fSpec conj = JSONConjunct
  { cnjJSONId                  = rc_id conj
  , cnjJSONsignalRuleNames     = map name . filter isSignal . rc_orgRules $ conj
  , cnjJSONinvariantRuleNames  = map name . filter (not . ruleIsInvariantUniOrInj) . rc_orgRules $ conj
  , cnjJSONviolationsSQL       = prettySQLQuery fSpec 0 . conjNF (getOpts fSpec) . notCpl . rc_conjunct $ conj
  , cnjJSONNormalizationSteps  
     = if verboseP (getOpts fSpec)
       then Just . showPrf showADL . cfProof (getOpts fSpec) . notCpl . rc_conjunct $ conj
       else Nothing 
  }
  
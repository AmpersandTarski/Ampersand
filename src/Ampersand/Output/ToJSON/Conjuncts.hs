{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ampersand.Output.ToJSON.Conjuncts (Conjuncts) where

import Ampersand.ADL1
import Ampersand.FSpec.Incremental.DeltaTerms (deltaQueriesFor, deltaTableName, withDeltaPlugs)
import Ampersand.FSpec.ToFSpec.NormalForms (conjNF)
import Ampersand.Output.ToJSON.JSONutils
import qualified RIO.NonEmpty as NE

newtype Conjuncts = Conjuncts [JSONConjunct]
  deriving (Generic, Show)

data JSONConjunct = JSONConjunct
  { cnjJSONid :: Text,
    cnjJSONsignalRuleNames :: [Text],
    cnjJSONinvariantRuleNames :: [Text],
    cnjJSONviolationsSQL :: Text,
    -- | Optional (issue #1684): per relation occurring in this conjunct, the
    --   candidate query for delta-scoped re-evaluation. Nothing when the term
    --   falls outside the supported class; runtimes that do not know this
    --   field keep full re-evaluation.
    cnjJSONdeltaQueries :: Maybe [JSONDeltaQuery]
  }
  deriving (Generic, Show)

data JSONDeltaQuery = JSONDeltaQuery
  { dltJSONrelation :: Text,
    dltJSONdeltaTable :: Text,
    dltJSONcandidateSQL :: Text
  }
  deriving (Generic, Show)

instance ToJSON JSONDeltaQuery where
  toJSON = amp2Jason

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
        cnjJSONsignalRuleNames = map fullName . filter (isSignal fSpec) . NE.toList . rc_orgRules $ conj,
        cnjJSONinvariantRuleNames = map fullName . filter (not . isSignal fSpec) . NE.toList . rc_orgRules $ conj,
        cnjJSONviolationsSQL = sqlQuery fSpec violTerm,
        cnjJSONdeltaQueries = map (fromAmpersand env fSpec) <$> deltaQueriesFor violTerm
      }
    where
      violTerm = conjNF env . notCpl . rcConjunct $ conj

instance JSON (Relation, Expression) JSONDeltaQuery where
  fromAmpersand _env fSpec (rel, candTerm) =
    JSONDeltaQuery
      { dltJSONrelation = fullName rel <> tshow (sign rel),
        dltJSONdeltaTable = deltaTableName rel,
        dltJSONcandidateSQL = sqlQuery (withDeltaPlugs fSpec) candTerm
      }

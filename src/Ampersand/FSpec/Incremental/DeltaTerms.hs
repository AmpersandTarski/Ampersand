{-# LANGUAGE ScopedTypeVariables #-}

-- | Candidate-term derivation for delta SQL (issue #1684, Phase 3 of the
--   incremental-evaluation plan).
--
--   The design is delta-scoped re-evaluation: the full current state lives in
--   MariaDB, so the violation cache is maintained by re-running the /existing/
--   violation predicate on a small set of candidate pairs — the pairs whose
--   violation status may have changed given the transaction's touched pairs.
--   No weight columns, no intermediate state: the SQL side only needs
--   candidate COMPLETENESS (obligation series C, delta-calculus.md §7).
--
--   For every populatable relation r there is a delta table (a two-column
--   'BinSQL' plug) holding the pairs touched by the current transaction —
--   empty outside transactions, so the widened\/narrowed envelopes below
--   coincide with the base relation for untouched relations at no cost.
module Ampersand.FSpec.Incremental.DeltaTerms
  ( deltaRelationOf,
    deltaPlugFor,
    deltaPlugs,
    deltaTableName,
    withDeltaPlugs,
    deltaSupported,
    candidateTerms,
    deltaQueriesFor,
  )
where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Classes
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.FSpecAux (getRelationTableInfo)
import Data.Char (isAlphaNum)
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T

-- | The fabricated delta relation Δr (design choice OK-2): same signature as
--   r, a name outside the user namespace, @decusr = False@.
deltaRelationOf :: Relation -> Relation
deltaRelationOf r =
  Relation
    { decnm = mkName RelationName (NamePart (toText1Unsafe (deltaTableName r)) NE.:| []),
      decsgn = decsgn r,
      declabel = Nothing,
      decprps = Set.empty,
      decDefaults = Set.empty,
      decpr = Nothing,
      decMean = [],
      decfpos = OriginUnknown,
      decusr = False,
      decpat = Nothing,
      dechash = hash ("delta" :: Text, fullName r, tshow (sign r))
    }

-- | The delta-table name of a relation: derived from the relation's own name,
--   made collision-free with a hash over (name, signature).
deltaTableName :: Relation -> Text
deltaTableName r =
  "delta_"
    <> sanitize (fullName r)
    <> "_"
    <> tshow (abs (hash (fullName r, tshow (sign r))) `mod` 1000000)
  where
    sanitize = T.map (\c -> if isAlphaNum c then c else '_')

-- | A two-column plug for Δr. Column types are copied from the relation's
--   existing storage, so values move between the tables without coercion.
deltaPlugFor :: FSpec -> Relation -> PlugSQL
deltaPlugFor fSpec r =
  BinSQL
    { sqlname = text1ToSqlName (toText1Unsafe (deltaTableName r)),
      cLkpTbl = [],
      dLkpTbl = [store],
      mainItem = Right dr
    }
  where
    dr = deltaRelationOf r
    (_, baseStore) = getRelationTableInfo fSpec r
    store =
      RelStore
        { rsDcl = dr,
          rsStoredFlipped = False,
          rsSrcAtt = mkAtt "src" (EDcI (source r)) (attType (rsSrcAtt baseStore)),
          rsTrgAtt = mkAtt "tgt" (EDcD dr) (attType (rsTrgAtt baseStore))
        }
    mkAtt nm expr tt =
      Att
        { attSQLColName = text1ToSqlName (toText1Unsafe nm),
          attExpr = expr,
          attType = tt,
          attUse = PlainAttr,
          attNull = False,
          attDBNull = False,
          attUniq = False,
          attFlipped = False
        }

-- | One delta plug per relation that occurs in some conjunct's violation term.
deltaPlugs :: FSpec -> [PlugSQL]
deltaPlugs fSpec =
  [ deltaPlugFor fSpec r
    | r <- Set.toList . Set.unions . map (bindedRelationsIn . rcConjunct) . allConjuncts $ fSpec
  ]

-- | An fSpec whose plug administration also resolves the delta relations, for
--   compiling candidate terms with the unchanged SQL generator. Use this copy
--   for query generation only; the real fSpec stays pristine.
withDeltaPlugs :: FSpec -> FSpec
withDeltaPlugs fSpec =
  fSpec {plugInfos = plugInfos fSpec <> map InternalPlug (deltaPlugs fSpec)}

-- | The term class the candidate calculus covers. Anything else keeps full
--   re-evaluation for its conjunct (coverage grows per construct, OK-7).
deltaSupported :: Expression -> Bool
deltaSupported e = case e of
  EUni (a, b) -> deltaSupported a && deltaSupported b
  EIsc (a, b) -> deltaSupported a && deltaSupported b
  EDif (a, b) -> deltaSupported a && deltaSupported b
  ECps (a, b) -> deltaSupported a && deltaSupported b
  EFlp a -> deltaSupported a
  ECpl a -> deltaSupported a
  EBrk a -> deltaSupported a
  EDcD _ -> True
  EDcI _ -> True
  EDcV _ -> True
  EMp1 {} -> True
  EBin {} -> True
  _ -> False

-- | The widened envelope W: an upper bound of the relation's old and new
--   state, per position polarity (delta-calculus.md §7). Delta tables of
--   untouched relations are empty, so W collapses to the base term for them.
widen :: Expression -> Expression
widen e = case e of
  EUni (a, b) -> widen a .\/. widen b
  EIsc (a, b) -> widen a ./\. widen b
  EDif (a, b) -> widen a .-. narrow b
  ECps (a, b) -> widen a .:. widen b
  EFlp a -> EFlp (widen a)
  ECpl a -> ECpl (narrow a)
  EBrk a -> widen a
  EDcD r -> EDcD r .\/. EDcD (deltaRelationOf r)
  _ -> e

-- | The narrowed envelope N: a lower bound, dual to 'widen'.
narrow :: Expression -> Expression
narrow e = case e of
  EUni (a, b) -> narrow a .\/. narrow b
  EIsc (a, b) -> narrow a ./\. narrow b
  EDif (a, b) -> narrow a .-. widen b
  ECps (a, b) -> narrow a .:. narrow b
  EFlp a -> EFlp (narrow a)
  ECpl a -> ECpl (widen a)
  EBrk a -> narrow a
  EDcD r -> EDcD r .-. EDcD (deltaRelationOf r)
  _ -> e

-- | The candidate terms for a change in relation r (the D-rules of
--   delta-calculus.md §7): each term evaluates, over the current database plus
--   the delta tables, to a superset of the pairs whose membership in the
--   violation term may differ between the old and the new state, via one
--   occurrence of r. The union over the terms (and over the transaction's
--   touched relations) is the candidate set the runtime rechecks.
candidateTerms :: Expression -> Relation -> [Expression]
candidateTerms term r = go term
  where
    go e = case e of
      EDcD s
        | s == r -> [EDcD (deltaRelationOf r)]
        | otherwise -> []
      EUni (a, b) -> go a <> go b
      EIsc (a, b) ->
        [d ./\. widen b | d <- go a] <> [widen a ./\. d | d <- go b]
      EDif (a, b) ->
        [d .-. narrow b | d <- go a] <> [widen a ./\. d | d <- go b]
      ECps (a, b) ->
        [d .:. widen b | d <- go a] <> [widen a .:. d | d <- go b]
      EFlp a -> map EFlp (go a)
      ECpl a -> go a
      EBrk a -> go a
      _ -> []

-- | Per relation occurring in the (normalized) violation term: the union of
--   its candidate terms, as one Expression ready for the SQL generator.
--   Nothing when the term falls outside the supported class.
deltaQueriesFor :: Expression -> Maybe [(Relation, Expression)]
deltaQueriesFor term
  | not (deltaSupported term) = Nothing
  | otherwise =
      Just
        [ (r, foldr1Uni cands)
          | r <- Set.toList (bindedRelationsIn term),
            let cands = candidateTerms term r,
            not (null cands)
        ]
  where
    foldr1Uni :: [Expression] -> Expression
    foldr1Uni xs = case xs of
      [] -> fatal "foldr1Uni: empty candidate list"
      [x] -> x
      (x : rest) -> x .\/. foldr1Uni rest

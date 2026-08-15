{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The code-to-model bridge of the candidate calculus (issue #1684).
--
-- The Isabelle theory @proofs\/incremental\/Candidates.thy@ (branch
-- @incremental-evaluation@, register claim PRF-7) proves the K-obligations:
-- the candidate sets of delta-scoped re-evaluation are complete — no pair
-- changes its membership in a rule term without being named by a candidate
-- query.  This module binds the __actual__ functions of
-- "Ampersand.FSpec.Incremental.DeltaTerms" (@widen@, @narrow@,
-- @candidateTerms@, @deltaQueriesFor@) to that model with QuickCheck
-- properties, re-checked on every @stack test@ run.  The reference evaluator
-- below is written naively and independently (no reuse of the code under
-- test), mirroring the @sem@ function of the Isabelle theory.
--
-- Property → lemma (Candidates.thy):
--
-- * 'propWidenUpper', 'propNarrowLower' → @WN_envelope@ (@W_upper@,
--   @N_lower@): the widened term bounds the old and new denotation from
--   above, the narrowed term from below.
-- * 'propCandidatesComplete' → @K_complete@ + @K_per_relation@: the union of
--   the per-relation candidate terms covers every pair whose membership
--   changed — exercised exactly in the per-relation shape the runtime uses.
-- * 'propK1Uni' .. 'propK6Cpl' → K1..K6: the same completeness statement
--   with the operator under test forced at the root, so every induction
--   case of @K_complete@ is exercised on the real code.
-- * 'propDeltaQueriesAgree' → the @foldr1Uni@ packaging of
--   'deltaQueriesFor' equals the union of 'candidateTerms'.
-- * 'propUnsupportedIsNothing' → the fallback boundary: a term outside the
--   supported class yields no delta queries at all.
--
-- Boundary, matching the theory: the concept populations are held constant
-- (one concept, a fixed atom universe) — the concept-affected fallback
-- (OK-9) discharges that assumption at run time.  The delta sets are
-- generated as supersets of the symmetric difference, mirroring the
-- delta-table contract (a touched-but-unchanged pair may be present).
module Ampersand.Test.Incremental.CandidateProperties
  ( doAllCandidatePropertyTests,
  )
where

import Ampersand.ADL1
import Ampersand.ADL1.P2A_Converters (pCtx2aCtx)
import Ampersand.Basics
import Ampersand.Classes
import Ampersand.FSpec.Incremental.DeltaTerms
import Ampersand.Input.ADL1.CtxError (Guarded (..))
import Ampersand.Input.Parsing (parseCtx)
import Ampersand.Options.FSpecGenOptsParser (defFSpecGenOpts)
import Ampersand.Types.Config (HasRunner, extendWith)
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import Test.QuickCheck

-- * Test harness (same shape as "Ampersand.Test.Incremental.Properties")

doAllCandidatePropertyTests :: (HasRunner env) => RIO env Bool
doAllCandidatePropertyTests = do
  built <- buildKContext
  case built of
    Left err -> do
      logError . display $ "❗❗❗ Failed: candidate property tests could not build their context: " <> err
      pure False
    Right rels -> whileSuccess (kTests rels)
  where
    whileSuccess :: (HasLogFunc env) => [(Text, Property)] -> RIO env Bool
    whileSuccess [] = pure True
    whileSuccess ((nm, prop) : tl) = do
      res <- liftIO $ quickCheckWithResult checkArgs prop
      if isSuccess res
        then do
          logInfo . display $ "✅ Passed: " <> nm
          whileSuccess tl
        else do
          logError . display $ "❗❗❗ Failed: " <> nm
          logError . display . T.pack $ output res
          pure False
    checkArgs =
      stdArgs
        { maxSuccess = 300,
          maxSize = 25,
          chatty = False
        }

kTests :: [Relation] -> [(Text, Property)]
kTests rels =
  [ ("DeltaTerms: widen bounds old and new from above (WN_envelope/W_upper)", propWidenUpper rels),
    ("DeltaTerms: narrow bounds old and new from below (WN_envelope/N_lower)", propNarrowLower rels),
    ("DeltaTerms: candidate terms cover every changed pair (K_complete/K_per_relation)", propCandidatesComplete rels),
    ("DeltaTerms: completeness at a union root (K1)", propK1Uni rels),
    ("DeltaTerms: completeness at an intersection root (K2)", propK2Isc rels),
    ("DeltaTerms: completeness at a difference root (K3)", propK3Dif rels),
    ("DeltaTerms: completeness at a composition root (K4)", propK4Cps rels),
    ("DeltaTerms: completeness at a converse root (K5)", propK5Flp rels),
    ("DeltaTerms: completeness at a complement root (K6)", propK6Cpl rels),
    ("DeltaTerms: deltaQueriesFor packages the candidate-term union", propDeltaQueriesAgree rels),
    ("DeltaTerms: an unsupported construct yields no delta queries", propUnsupportedIsNothing rels)
  ]

-- | One concept, three relations on it: every operator combination is
--   well-typed, which is all the calculus needs (the theory has one
--   universe type).
kModel :: Text
kModel =
  T.unlines
    [ "CONTEXT KTest IN ENGLISH",
      "RELATION kr[P*P]",
      "RELATION ks[P*P]",
      "RELATION kt[P*P]",
      "ENDCONTEXT"
    ]

buildKContext :: (HasRunner env) => RIO env (Either Text [Relation])
buildKContext = do
  let fSpecGenOpts = defFSpecGenOpts ("KTest.adl" NE.:| [])
  extendWith fSpecGenOpts $ case parseCtx "KTest.adl" kModel of
    Errors es -> pure (Left (T.intercalate "; " (map tshow (NE.toList es))))
    Checked (pCtx, _) _ -> do
      env <- ask
      case pCtx2aCtx env pCtx of
        Errors es -> pure (Left (T.intercalate "; " (map tshow (NE.toList es))))
        Checked aCtx _ ->
          pure (Right (L.sortOn fullName (Set.toList (relsDefdIn aCtx))))

-- * The reference evaluator (mirroring @sem@ of Candidates.thy)

-- | Pairs over a small fixed universe of synthetic atoms.
type PairSet = Set.Set (Int, Int)

-- | A database state: contents per relation (delta relations included).
type State = Map.Map Relation PairSet

univ :: [Int]
univ = [0 .. 5]

allPairs :: PairSet
allPairs = Set.fromList [(x, y) | x <- univ, y <- univ]

-- | Naive evaluation of the supported term class.  @EDcI@\/@EDcV@ range over
--   the fixed universe (constant concept populations, the OK-9 assumption).
refEval :: State -> Expression -> PairSet
refEval st e = case e of
  EDcD d -> Map.findWithDefault Set.empty d st
  EDcI _ -> Set.fromList [(x, x) | x <- univ]
  EDcV _ -> allPairs
  EUni (a, b) -> refEval st a `Set.union` refEval st b
  EIsc (a, b) -> refEval st a `Set.intersection` refEval st b
  EDif (a, b) -> refEval st a `Set.difference` refEval st b
  ECps (a, b) ->
    Set.fromList
      [ (x, z)
        | (x, y) <- Set.toList (refEval st a),
          (y', z) <- Set.toList (refEval st b),
          y == y'
      ]
  EFlp a -> Set.map (\(x, y) -> (y, x)) (refEval st a)
  ECpl a -> allPairs `Set.difference` refEval st a
  EBrk a -> refEval st a
  _ -> fatal ("candidate property tests: term outside the supported class: " <> tshow e)

symdiff :: PairSet -> PairSet -> PairSet
symdiff a b = (a `Set.difference` b) `Set.union` (b `Set.difference` a)

-- * Generators

genPairs :: Gen PairSet
genPairs = Set.fromList <$> resize 12 (listOf ((,) <$> genA <*> genA))
  where
    genA = chooseInt (0, 5)

-- | A term of the supported class over the given relations.
genTerm :: [Relation] -> Gen Expression
genTerm rels = sized go
  where
    leaf =
      frequency
        [ (6, EDcD <$> elements rels),
          (1, pure (EDcI p)),
          (1, pure (EDcV (Sign p p)))
        ]
    p = source (headRel rels)
    go n
      | n <= 0 = leaf
      | otherwise =
          frequency
            [ (1, leaf),
              (2, bin EUni),
              (2, bin EIsc),
              (2, bin EDif),
              (2, bin ECps),
              (1, EFlp <$> go (n - 1)),
              (1, ECpl <$> go (n - 1)),
              (1, EBrk <$> go (n - 1))
            ]
      where
        bin c = curry c <$> go (n `div` 2) <*> go (n `div` 2)

headRel :: [Relation] -> Relation
headRel (d : _) = d
headRel [] = fatal "candidate property tests: no relations in the K context"

-- | A full scenario: a term, an old and a new state for the base relations,
--   and a delta per relation — a superset of the symmetric difference, as
--   the delta-table contract allows.
data Scenario = Scenario
  { scTerm :: Expression,
    scOld :: State,
    scNew :: State,
    scDelta :: Map.Map Relation PairSet
  }

instance Show Scenario where
  show sc =
    T.unpack
      $ "term: "
      <> tshow (scTerm sc)
      <> "\nold: "
      <> tshow (Map.toList (scOld sc))
      <> "\nnew: "
      <> tshow (Map.toList (scNew sc))
      <> "\ndelta: "
      <> tshow (Map.toList (scDelta sc))

genScenarioWith :: [Relation] -> Gen Expression -> Gen Scenario
genScenarioWith rels genTm = do
  tm <- genTm
  states <- forM rels $ \d -> do
    old <- genPairs
    new <- genPairs
    extra <- genPairs
    pure (d, old, new, symdiff old new `Set.union` extra)
  pure
    Scenario
      { scTerm = tm,
        scOld = Map.fromList [(d, old) | (d, old, _, _) <- states],
        scNew = Map.fromList [(d, new) | (d, _, new, _) <- states],
        scDelta = Map.fromList [(d, dl) | (d, _, _, dl) <- states]
      }

genScenario :: [Relation] -> Gen Scenario
genScenario rels = genScenarioWith rels (genTerm rels)

-- | The state the generated SQL sees at commit time: the new base relations
--   plus the filled delta tables.
commitState :: Scenario -> State
commitState sc =
  scNew sc
    `Map.union` Map.fromList
      [(deltaRelationOf d, dl) | (d, dl) <- Map.toList (scDelta sc)]

-- * The properties

propWidenUpper :: [Relation] -> Property
propWidenUpper rels = forAll (genScenario rels) $ \sc ->
  let old = refEval (scOld sc) (scTerm sc)
      new = refEval (scNew sc) (scTerm sc)
      w = refEval (commitState sc) (widen (scTerm sc))
   in property ((old `Set.union` new) `Set.isSubsetOf` w)

propNarrowLower :: [Relation] -> Property
propNarrowLower rels = forAll (genScenario rels) $ \sc ->
  let old = refEval (scOld sc) (scTerm sc)
      new = refEval (scNew sc) (scTerm sc)
      n = refEval (commitState sc) (narrow (scTerm sc))
   in property (n `Set.isSubsetOf` (old `Set.intersection` new))

-- | The completeness check shared by the whole-term property and the
--   per-operator properties: every pair whose membership changed is named
--   by some candidate term of some relation of the term.
completeOn :: Scenario -> Property
completeOn sc =
  let old = refEval (scOld sc) (scTerm sc)
      new = refEval (scNew sc) (scTerm sc)
      env = commitState sc
      cands =
        Set.unions
          [ refEval env c
            | d <- Set.toList (bindedRelationsIn (scTerm sc)),
              c <- candidateTerms (scTerm sc) d
          ]
      changed = symdiff old new
   in counterexample
        (T.unpack ("changed: " <> tshow (Set.toList (changed `Set.difference` cands))))
        (changed `Set.isSubsetOf` cands)

propCandidatesComplete :: [Relation] -> Property
propCandidatesComplete rels = forAll (genScenario rels) completeOn

rootedProp :: [Relation] -> (Expression -> Expression -> Expression) -> Property
rootedProp rels root =
  forAll (genScenarioWith rels (root <$> sub <*> sub)) completeOn
  where
    sub = resize 4 (genTerm rels)

propK1Uni, propK2Isc, propK3Dif, propK4Cps, propK5Flp, propK6Cpl :: [Relation] -> Property
propK1Uni rels = rootedProp rels (curry EUni)
propK2Isc rels = rootedProp rels (curry EIsc)
propK3Dif rels = rootedProp rels (curry EDif)
propK4Cps rels = rootedProp rels (curry ECps)
propK5Flp rels = rootedProp rels (\a _ -> EFlp a)
propK6Cpl rels = rootedProp rels (\a _ -> ECpl a)

propDeltaQueriesAgree :: [Relation] -> Property
propDeltaQueriesAgree rels = forAll (genScenario rels) $ \sc ->
  let env = commitState sc
   in case deltaQueriesFor (scTerm sc) of
        Nothing -> counterexample "supported term reported as unsupported" False
        Just qs ->
          conjoin
            [ refEval env q
                === Set.unions (map (refEval env) (candidateTerms (scTerm sc) d))
              | (d, q) <- qs
            ]

propUnsupportedIsNothing :: [Relation] -> Property
propUnsupportedIsNothing rels =
  once (isNothing (deltaQueriesFor (EKl0 (EDcD (headRel rels)))))

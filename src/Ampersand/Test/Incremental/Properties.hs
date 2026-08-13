{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The code-to-model bridge of the incremental evaluator (issue #1683, R2).
--
-- The Isabelle session @Incremental_Delta@ (proofs\/incremental\/) proves the
-- delta calculus on a mathematical model of Z-sets; this module binds the
-- __actual__ functions of "Ampersand.FSpec.Incremental.ZSet" (and the engine
-- of "Ampersand.FSpec.Incremental") to that model with QuickCheck properties,
-- one per proven lemma, re-checked on every @stack test@ run.  The reference
-- implementations below are written naively and independently (no reuse of
-- the functions under test), mirroring the Isabelle definitions.
--
-- Property → lemma:
--
-- * 'propBagApplyIsZplus', 'propRelApplyIsZplus' → Z1 (group addition),
--   plus the storage invariant (no zero weights, no empty rows).
-- * 'propFlipInvolution', 'propFlipAdditive', 'propFlipIsConverse' →
--   Z1 flip linearity, B6.
-- * 'propBagHCrossing', 'propBagHSupport', 'propBagHMaintainsDistinct',
--   'propRelHMaintainsDistinct' → Z5 (the zero-crossing delta of distinct).
-- * 'propPointwiseIsProduct', 'propPointwiseZ2' → Z2, B3.
-- * 'propComposeDeltaOld', 'propComposeFlipDelta', 'propComposeZ3' → Z3, B4.
-- * 'propBagProdIsProduct', 'propBagProdZ4' → Z4, B5.
-- * 'propRelDiffIsDifference' → B2 (on set-valued inputs).
-- * 'propRowWeights', 'propRowWeightsLinear', 'propRowWeightsDomain',
--   'propColWeightsRange' → the Prd projections (Circuit.thy: zrow\/zcol
--   lemmas) and their D4 linearity.
-- * 'propDiagRel' → the I-node embedding (Circuit.thy: zdiag).
-- * 'doEngineOracleTest' → the whole-circuit theorems C1-C5 of Circuit.thy
--   and the population mirror P1-P3 of Population.thy, exercised on a
--   miniature context with an ISA hierarchy and ONE-typed terms: a random
--   set-disciplined transaction stream must keep every circuit equal to the
--   'fullContents' oracle, backfill included.  The I[ONE]\/V[..*ONE] terms
--   pin the base-case fix of 'engineInit' (ONE travels through tx0).
module Ampersand.Test.Incremental.Properties
  ( doAllIncrementalPropertyTests,
  )
where

import Ampersand.ADL1
import Ampersand.ADL1.P2A_Converters (pCtx2aCtx)
import Ampersand.Basics
import Ampersand.Classes
import Ampersand.FSpec (FSpec, fcontextInfo, originalContext)
import Ampersand.FSpec.Incremental
import Ampersand.FSpec.Incremental.ZSet
import Ampersand.FSpec.ToFSpec.ADL2FSpec (makeFSpec)
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

-- * Test harness (same shape as "Ampersand.Test.Parser.QuickChecks")

doAllIncrementalPropertyTests :: (HasRunner env) => RIO env Bool
doAllIncrementalPropertyTests = do
  ok <- whileSuccess zsetTests
  if ok then doEngineOracleTest else pure False
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
        { maxSuccess = 200,
          maxSize = 30,
          chatty = False
        }

zsetTests :: [(Text, Property)]
zsetTests =
  [ ("ZSet: bagApply is pointwise addition (Z1) + invariant", propBagApplyIsZplus),
    ("ZSet: relApply is pointwise addition (Z1) + invariant", propRelApplyIsZplus),
    ("ZSet: relFlip is an involution (Z1)", propFlipInvolution),
    ("ZSet: relFlip commutes with addition (Z1)", propFlipAdditive),
    ("ZSet: relFlip transposes weights (B6)", propFlipIsConverse),
    ("ZSet: bagH is the zero-crossing case form (Z5a)", propBagHCrossing),
    ("ZSet: bagH support within the delta (Z5b)", propBagHSupport),
    ("ZSet: bagH maintains distinct (Z5)", propBagHMaintainsDistinct),
    ("ZSet: relH maintains distinct (Z5)", propRelHMaintainsDistinct),
    ("ZSet: pointwiseDelta is the pointwise product (Z2/B3)", propPointwiseIsProduct),
    ("ZSet: pointwise bilinear expansion is exact (Z2)", propPointwiseZ2),
    ("ZSet: composeDeltaOld is composition (Z3)", propComposeDeltaOld),
    ("ZSet: composeFlipDelta is composition via the flipped index (Z3)", propComposeFlipDelta),
    ("ZSet: composition bilinear expansion is exact (Z3/B4)", propComposeZ3),
    ("ZSet: bagProd is the weighted cartesian product (Z4/B5)", propBagProdIsProduct),
    ("ZSet: product bilinear expansion is exact (Z4)", propBagProdZ4),
    ("ZSet: relDiff clips to set difference (B2)", propRelDiffIsDifference),
    ("ZSet: rowWeights sums rows (Prd projection)", propRowWeights),
    ("ZSet: rowWeights is linear (D4)", propRowWeightsLinear),
    ("ZSet: rowWeights carries the domain on sets", propRowWeightsDomain),
    ("ZSet: colWeights carries the range on sets", propColWeightsRange),
    ("ZSet: diagRel embeds the diagonal", propDiagRel)
  ]

-- * Generators

-- | Atoms from a small pool, so collisions (and thus weight arithmetic) are common.
genAtom :: Gen Int
genAtom = chooseInt (0, 6)

genWeight :: Gen Int
genWeight = elements ([-3 .. -1] <> [1 .. 3])

-- | A normalized unary Z-set: weights combined per key, zeros dropped.
genBag :: Gen (ZBag Int)
genBag = normB <$> listOf ((,) <$> genAtom <*> genWeight)

-- | A normalized binary Z-set.
genRel :: Gen (ZRel Int)
genRel = normR <$> listOf ((,,) <$> genAtom <*> genAtom <*> genWeight)

-- | A set-valued binary Z-set (all weights 1; duplicates collapse).
genSetRel :: Gen (ZRel Int)
genSetRel = refClipR <$> genRel

-- * Independent reference implementations (mirroring the Isabelle model)

normB :: [(Int, Int)] -> ZBag Int
normB = Map.filter (/= 0) . Map.fromListWith (+)

normR :: [(Int, Int, Int)] -> ZRel Int
normR ps =
  Map.filter (not . Map.null)
    . Map.map (Map.filter (/= 0))
    $ Map.fromListWith (Map.unionWith (+)) [(x, Map.singleton y w) | (x, y, w) <- ps]

refB :: ZBag Int -> Int -> Int
refB = flip (Map.findWithDefault 0)

refR :: ZRel Int -> (Int, Int) -> Int
refR m (x, y) = maybe 0 (Map.findWithDefault 0 y) (Map.lookup x m)

relAsList :: ZRel Int -> [((Int, Int), Int)]
relAsList m = [((x, y), w) | (x, row) <- Map.toList m, (y, w) <- Map.toList row]

refPlusB :: ZBag Int -> ZBag Int -> ZBag Int
refPlusB a b = normB (Map.toList a <> Map.toList b)

refPlusR :: ZRel Int -> ZRel Int -> ZRel Int
refPlusR a b = normR [(x, y, w) | ((x, y), w) <- relAsList a <> relAsList b]

refNegR :: ZRel Int -> ZRel Int
refNegR a = normR [(x, y, negate w) | ((x, y), w) <- relAsList a]

refFlip :: ZRel Int -> ZRel Int
refFlip a = normR [(y, x, w) | ((x, y), w) <- relAsList a]

refMul :: ZRel Int -> ZRel Int -> ZRel Int
refMul a b = normR [(x, y, w * refR b (x, y)) | ((x, y), w) <- relAsList a]

refComp :: ZRel Int -> ZRel Int -> ZRel Int
refComp a b =
  normR
    [ (x, z, wa * wb)
      | ((x, y), wa) <- relAsList a,
        ((y', z), wb) <- relAsList b,
        y == y'
    ]

refProd :: ZBag Int -> ZBag Int -> ZRel Int
refProd u v = normR [(x, y, wu * wv) | (x, wu) <- Map.toList u, (y, wv) <- Map.toList v]

refRow :: ZRel Int -> ZBag Int
refRow a = normB [(x, w) | ((x, _), w) <- relAsList a]

refCol :: ZRel Int -> ZBag Int
refCol a = normB [(y, w) | ((_, y), w) <- relAsList a]

-- | The clip @distinct@ of the model, on binary Z-sets.
refClipR :: ZRel Int -> ZRel Int
refClipR a = normR [(x, y, 1) | ((x, y), w) <- relAsList a, w > 0]

refClipB :: ZBag Int -> ZBag Int
refClipB a = normB [(x, 1) | (x, w) <- Map.toList a, w > 0]

-- | The zero-crossing function of Z5, written independently.
refCrossing :: Int -> Int -> Int
refCrossing old new
  | old <= 0 && new > 0 = 1
  | old > 0 && new <= 0 = -1
  | otherwise = 0

-- | Storage invariant: no key maps to 0, no source maps to an empty row.
invB :: ZBag Int -> Bool
invB = notElem 0 . Map.elems

invR :: ZRel Int -> Bool
invR m = all (\row -> not (Map.null row) && 0 `notElem` Map.elems row) (Map.elems m)

-- * The properties

propBagApplyIsZplus :: Property
propBagApplyIsZplus = forAll ((,) <$> genBag <*> genBag) $ \(a, d) ->
  let r = bagApply a d
   in r === refPlusB a d .&&. property (invB r)

propRelApplyIsZplus :: Property
propRelApplyIsZplus = forAll ((,) <$> genRel <*> genRel) $ \(a, d) ->
  let r = relApply a d
   in r === refPlusR a d .&&. property (invR r)

propFlipInvolution :: Property
propFlipInvolution = forAll genRel $ \a -> relFlip (relFlip a) === a

propFlipAdditive :: Property
propFlipAdditive = forAll ((,) <$> genRel <*> genRel) $ \(a, b) ->
  relFlip (relApply a b) === relApply (relFlip a) (relFlip b)

propFlipIsConverse :: Property
propFlipIsConverse = forAll genRel $ \a ->
  relFlip a === refFlip a .&&. property (invR (relFlip a))

propBagHCrossing :: Property
propBagHCrossing = forAll ((,) <$> genBag <*> genBag) $ \(z, d) ->
  bagH z d === normB [(x, refCrossing (refB z x) (refB z x + w)) | (x, w) <- Map.toList d]

propBagHSupport :: Property
propBagHSupport = forAll ((,) <$> genBag <*> genBag) $ \(z, d) ->
  property (Map.keysSet (bagH z d) `Set.isSubsetOf` Map.keysSet d)

propBagHMaintainsDistinct :: Property
propBagHMaintainsDistinct = forAll ((,) <$> genBag <*> genBag) $ \(z, d) ->
  bagApply (refClipB z) (bagH z d) === refClipB (bagApply z d)

propRelHMaintainsDistinct :: Property
propRelHMaintainsDistinct = forAll ((,) <$> genRel <*> genRel) $ \(z, d) ->
  relApply (refClipR z) (relH z d) === refClipR (relApply z d)
    .&&. property (invR (relH z d))

propPointwiseIsProduct :: Property
propPointwiseIsProduct = forAll ((,) <$> genRel <*> genRel) $ \(d, o) ->
  pointwiseDelta d o === refMul d o .&&. property (invR (pointwiseDelta d o))

propPointwiseZ2 :: Property
propPointwiseZ2 = forAll ((,,,) <$> genRel <*> genRel <*> genRel <*> genRel)
  $ \(a, da, b, db) ->
    let aNew = relApply a da
        bNew = relApply b db
        emitted = relApply (pointwiseDelta da b) (pointwiseDelta db aNew)
     in emitted === refPlusR (refMul aNew bNew) (refNegR (refMul a b))

propComposeDeltaOld :: Property
propComposeDeltaOld = forAll ((,) <$> genRel <*> genRel) $ \(da, b) ->
  composeDeltaOld da b === refComp da b .&&. property (invR (composeDeltaOld da b))

propComposeFlipDelta :: Property
propComposeFlipDelta = forAll ((,) <$> genRel <*> genRel) $ \(a, db) ->
  composeFlipDelta (relFlip a) db === refComp a db

propComposeZ3 :: Property
propComposeZ3 = forAll ((,,,) <$> genRel <*> genRel <*> genRel <*> genRel)
  $ \(a, da, b, db) ->
    let aNew = relApply a da
        bNew = relApply b db
        emitted = relApply (composeDeltaOld da b) (composeFlipDelta (relFlip aNew) db)
     in emitted === refPlusR (refComp aNew bNew) (refNegR (refComp a b))

propBagProdIsProduct :: Property
propBagProdIsProduct = forAll ((,) <$> genBag <*> genBag) $ \(u, v) ->
  bagProd u v === refProd u v .&&. property (invR (bagProd u v))

propBagProdZ4 :: Property
propBagProdZ4 = forAll ((,,,) <$> genBag <*> genBag <*> genBag <*> genBag)
  $ \(u, du, v, dv) ->
    let uNew = bagApply u du
        vNew = bagApply v dv
        emitted = relApply (bagProd du v) (bagProd uNew dv)
     in emitted === refPlusR (refProd uNew vNew) (refNegR (refProd u v))

propRelDiffIsDifference :: Property
propRelDiffIsDifference = forAll ((,) <$> genSetRel <*> genSetRel) $ \(a, b) ->
  refClipR (relDiff a b)
    === normR [(x, y, 1) | ((x, y), _) <- relAsList a, refR b (x, y) <= 0]

propRowWeights :: Property
propRowWeights = forAll genRel $ \a ->
  rowWeights a === refRow a .&&. colWeights a === refCol a

propRowWeightsLinear :: Property
propRowWeightsLinear = forAll ((,) <$> genRel <*> genRel) $ \(a, b) ->
  rowWeights (relApply a b) === bagApply (rowWeights a) (rowWeights b)

propRowWeightsDomain :: Property
propRowWeightsDomain = forAll genSetRel $ \a ->
  Map.keysSet (refClipB (rowWeights a)) === Map.keysSet a

propColWeightsRange :: Property
propColWeightsRange = forAll genSetRel $ \a ->
  Map.keysSet (refClipB (colWeights a))
    === Set.fromList [y | ((_, y), _) <- relAsList a]

propDiagRel :: Property
propDiagRel = forAll genBag $ \u ->
  diagRel u === normR [(x, x, w) | (x, w) <- Map.toList u]

-- * The engine-level oracle property

-- | The miniature context: an ISA hierarchy (cone bookkeeping), several
--   relations, and a ONE-typed relation.
miniModel :: Text
miniModel =
  T.unlines
    [ "CONTEXT PropTest IN ENGLISH",
      "CLASSIFY Student ISA Person",
      "CLASSIFY Teacher ISA Person",
      "RELATION likes[Person*Person]",
      "RELATION enrolled[Student*Course]",
      "RELATION teaches[Teacher*Course]",
      "RELATION current[ONE*Course]",
      "ENDCONTEXT"
    ]

-- | Build the FSpec of the miniature context through the real pipeline.
buildMiniFSpec :: (HasRunner env) => RIO env (Either Text FSpec)
buildMiniFSpec = do
  let fSpecGenOpts = defFSpecGenOpts ("PropTest.adl" NE.:| [])
  extendWith fSpecGenOpts $ case parseCtx "PropTest.adl" miniModel of
    Errors es -> pure (Left (T.intercalate "; " (map tshow (NE.toList es))))
    Checked (pCtx, _) _ -> do
      env <- ask
      case pCtx2aCtx env pCtx of
        Errors es -> pure (Left (T.intercalate "; " (map tshow (NE.toList es))))
        Checked aCtx _ -> pure (Right (makeFSpec env aCtx))

doEngineOracleTest :: (HasRunner env) => RIO env Bool
doEngineOracleTest = do
  built <- buildMiniFSpec
  case built of
    Left err -> do
      logError . display $ "❗❗❗ Failed: engine oracle test could not build its context: " <> err
      pure False
    Right fSpec -> do
      let ci = fcontextInfo fSpec
          ctx = fromMaybe (fatal "PropTest has no original context") (originalContext fSpec)
          engine1 = engineInit (mkEngine ci (Set.toList (concs ctx)) (Set.toList (relsDefdIn ctx)) (terms ctx)) (initialPops ci ctx)
      case verifyAgainstOracle engine1 of
        bad@(_ : _) -> do
          logError "❗❗❗ Failed: engine disagrees with the oracle at backfill:"
          forM_ bad $ \(nm, term, i, o) ->
            logError . display $ "  " <> nm <> " (" <> tshow term <> "): incremental " <> tshow i <> " vs oracle " <> tshow o
          pure False
        [] -> do
          res <- liftIO $ quickCheckWithResult stdArgs {maxSuccess = 30, chatty = False} (propStream ci ctx engine1)
          if isSuccess res
            then do
              logInfo "✅ Passed: engine keeps every circuit equal to the fullContents oracle (random transaction streams)."
              pure True
            else do
              logError "❗❗❗ Failed: engine diverged from the fullContents oracle."
              logError . display . T.pack $ output res
              pure False

-- | The circuits under test: one term per node kind, population mirrors per
--   concept (@I[c]@), and the ONE-typed shapes that pin the backfill fix.
terms :: A_Context -> [(Text, Expression)]
terms ctx =
  [ ("i_" <> fullName c, EDcI c) | c <- Set.toList (concs ctx)
  ]
    <> [ ("i_ONE", EDcI ONE),
         ("v_one_one", EDcV (Sign ONE ONE)),
         ("v_person_course", EDcV (Sign person course)),
         ("likes", EDcD likes),
         ("likes_flp", EFlp (EDcD likes)),
         ("likes_likes", ECps (EDcD likes, EDcD likes)),
         ("likes_isc", EIsc (EDcD likes, EFlp (EDcD likes))),
         ("likes_uni", EUni (EDcD likes, EFlp (EDcD likes))),
         ("v_min_likes", EDif (EDcV (Sign person person), EDcD likes)),
         ("classmates", ECps (EDcD enrolled, EFlp (EDcD enrolled))),
         ("taught_by", ECps (EDcD enrolled, EFlp (EDcD teaches))),
         ("prd", EPrd (EDcD likes, EDcD enrolled)),
         ("kl0", EKl0 (EDcD likes)),
         ("kl1", EKl1 (EDcD likes)),
         ("cpl_likes", ECpl (EDcD likes)),
         ("residual", ELrs (EDcD enrolled, EDcD teaches)),
         ("current_pair", ECps (EDcD current, EFlp (EDcD current))),
         ("mp1", EMp1 (ScriptString OriginUnknown "p0") person)
       ]
  where
    person = cptNamed ctx "Person"
    course = cptNamed ctx "Course"
    likes = relNamed ctx "likes"
    enrolled = relNamed ctx "enrolled"
    teaches = relNamed ctx "teaches"
    current = relNamed ctx "current"

cptNamed :: A_Context -> Text -> A_Concept
cptNamed ctx nm =
  fromMaybe (fatal ("PropTest lacks concept " <> nm))
    $ L.find (\c -> fullName c == nm) (Set.toList (concs ctx))

relNamed :: A_Context -> Text -> Relation
relNamed ctx nm =
  fromMaybe (fatal ("PropTest lacks relation " <> nm))
    $ L.find (\d -> fullName d == nm) (Set.toList (relsDefdIn ctx))

-- | A deterministic synthetic atom, honouring the concept's representation
--   (same construction as the incremental-bench command).
mkAtom :: ContextInfo -> A_Concept -> Int -> AAtomValue
mkAtom ci c i =
  case unsafePAtomVal2AtomValue tt (Just c) pav of
    Right v -> v
    Left msg -> fatal ("PropTest cannot synthesize an atom for " <> tshow c <> ": " <> msg)
  where
    tt = reprType ci c
    pav = case tt of
      Integer -> ScriptInt OriginUnknown (fromIntegral i)
      Float -> ScriptFloat OriginUnknown (fromIntegral i)
      Boolean -> ComnBool OriginUnknown (even i)
      _ -> ScriptString OriginUnknown ("a" <> tshow i)

-- | Initial population: a few pairs and explicit atoms, so the backfill is
--   more than the empty transaction.
initialPops :: ContextInfo -> A_Context -> [Population]
initialPops ci ctx =
  [ relPop likes [(pAtom 0, pAtom 1), (pAtom 1, pAtom 2)],
    relPop enrolled [(sAtom 0, cAtom 0)],
    ACptPopu {popcpt = person, popas = [pAtom 3]}
  ]
  where
    person = cptNamed ctx "Person"
    likes = relNamed ctx "likes"
    enrolled = relNamed ctx "enrolled"
    pAtom = mkAtom ci (cptNamed ctx "Person")
    sAtom = mkAtom ci (cptNamed ctx "Student")
    cAtom = mkAtom ci (cptNamed ctx "Course")
    relPop d prs =
      ARelPopu
        { popsrc = source d,
          poptgt = target d,
          popdcl = d,
          popps = Set.fromList (map (uncurry mkAtomPair) prs)
        }

-- | Interpret a list of QuickCheck-drawn numbers as a set-disciplined
--   transaction stream (insert only absent pairs\/atoms, delete only present
--   ones — the lockstep contract of the runtime), verifying against the
--   oracle after every transaction.
propStream :: ContextInfo -> A_Context -> IncEngine -> Property
propStream ci ctx engine1 =
  forAll (listOf ((,,,) <$> chooseInt (0, 99) <*> chooseInt (0, 99) <*> chooseInt (0, 5) <*> chooseInt (0, 5)))
    $ \ops -> case L.foldl' stepOp (Right (engine1, 1 :: Int)) ops of
      Right _ -> property True
      Left err -> counterexample (T.unpack err) False
  where
    rels = [relNamed ctx nm | nm <- ["likes", "enrolled", "teaches", "current"]]
    cpts = [cptNamed ctx nm | nm <- ["Person", "Student", "Teacher", "Course"]]
    atomFor c i
      | c == ONE = AtomValueOfONE
      | otherwise = mkAtom ci c (i `mod` poolSize c)
    -- students/teachers draw from a sub-pool so ISA cones get shared atoms
    poolSize c = case T.unpack (fullName c) of
      "Course" -> 4
      "Student" -> 3
      "Teacher" -> 3
      _ -> 6
    stepOp acc@(Left _) _ = acc
    stepOp (Right (eng, n)) (kind, choice, i, j)
      | kind < 70 = -- relation edit
          let d = nth (choice `mod` length rels) rels
              p = (atomFor (source d) i, atomFor (target d) j)
              existing = Map.findWithDefault Set.empty d (ieRelPairs eng)
              w = if p `Set.member` existing then -1 else 1
              tx =
                TxDelta
                  { txRel = Map.singleton d (relFromPairs [(p, w)]),
                    txCpt = Map.empty
                  }
           in runTx eng n tx
      | otherwise = -- explicit concept-atom edit
          let c = nth (choice `mod` length cpts) cpts
              a = atomFor c i
              existing = Map.findWithDefault Set.empty c (ieCptAtoms eng)
              w = if a `Set.member` existing then -1 else 1
              tx =
                TxDelta
                  { txRel = Map.empty,
                    txCpt = Map.singleton c (Map.singleton a w)
                  }
           in runTx eng n tx
    nth :: Int -> [a] -> a
    nth i xs = case L.drop i xs of
      (x : _) -> x
      [] -> fatal "PropTest: index out of range"
    runTx eng n tx =
      let (eng', _) = applyTx eng tx
       in case verifyAgainstOracle eng' of
            [] -> Right (eng', n + 1)
            bad ->
              Left
                $ "oracle mismatch after tx "
                <> tshow n
                <> ": "
                <> T.intercalate
                  "; "
                  [ nm <> " incremental " <> tshow i' <> " vs oracle " <> tshow o
                    | (nm, _, i', o) <- bad
                  ]

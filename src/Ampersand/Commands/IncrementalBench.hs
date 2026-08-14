{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Benchmark (and verify) incremental evaluation of rule violations.
--
--   For each requested scale N the command populates the model's relations
--   with N synthetic pairs each, then runs a stream of single-pair
--   transactions. Every transaction is evaluated twice: incrementally
--   (a circuit step, cost proportional to the change — see
--   "Ampersand.FSpec.Incremental") and the way generated prototypes evaluate
--   today (full re-evaluation of the affected conjuncts' violation terms over
--   the whole population). With @--verify@ every transaction is additionally
--   checked against the oracle ('fullContents'): the maintained violation
--   sets must be identical to full re-evaluation, for every conjunct.
module Ampersand.Commands.IncrementalBench
  ( incrementalBench,
  )
where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Classes
import Ampersand.Core.ShowAStruct (showA)
import Ampersand.FSpec
import Ampersand.FSpec.Incremental
import Ampersand.FSpec.Incremental.ZSet (relSizeF)
import Ampersand.FSpec.ToFSpec.Populated (fullContents)
import Ampersand.Misc.HasClasses
import Data.Bits (shiftR, xor)
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Text as T

-- | One measured transaction.
data Row = Row
  { rowScale :: !Int,
    rowTx :: !Int,
    rowOp :: !Text,
    rowIncMicros :: !Double,
    rowFullMicros :: !Double,
    rowAffected :: !Int
  }

incrementalBench ::
  forall env.
  (HasIncrementalBenchOpts env, HasLogFunc env) =>
  FSpec ->
  RIO env ()
incrementalBench fSpec = do
  scalesTxt <- view incBenchScalesL
  txCount <- view incBenchTxCountL
  seed <- view incBenchSeedL
  verify <- view incBenchVerifyL
  mCsv <- view incBenchCsvL
  let scales = parseScales scalesTxt
  let ci = fcontextInfo fSpec
      ctx = fromMaybe (fatal "incremental-bench needs the original context") (originalContext fSpec)
      allConcepts = Set.toList (concs ctx)
      terms =
        [ (text1ToText (rc_id conj), notCpl (rcConjunct conj))
          | conj <- allConjuncts fSpec
        ]
      termRels = Set.toList . Set.unions $ map (bindedRelationsIn . snd) terms
      synthRels = [r | r <- termRels, benchable (source r), benchable (target r)]
      affectedTermsOf r = [(nm, term) | (nm, term) <- terms, r `Set.member` bindedRelationsIn term]
  when (null terms) $ do
    logError "This model has no conjuncts; nothing to benchmark."
    exitFailure
  when (null synthRels) $ do
    logError "No relation in the conjuncts is suitable for synthetic population."
    exitFailure
  logInfo . display $ "incremental-bench: " <> tshow (length terms) <> " conjuncts, " <> tshow (length synthRels) <> " relations to populate."
  let engine0 = mkEngine ci allConcepts (Set.toList (relsDefdIn ctx)) terms
  reportFallbacks engine0
  let scriptPops = ctxpopus ctx <> concatMap ptups (patterns ctx)
  allRows <- forM scales $ \n -> do
    logInfo . display $ "--- scale " <> tshow n <> " pairs per relation ---"
    let (synthPops, rng1) = synthesize ci synthRels n (mix (fromIntegral seed))
        pops = scriptPops <> synthPops
    t0 <- getMonotonicTime
    let engine1 = engineInit engine0 pops
        !initForce = sum [relSizeF (cOut c) | (_, _, c) <- ieCircuits engine1]
    t1 <- initForce `seq` getMonotonicTime
    logInfo . display $ "backfill: " <> tshow (length pops) <> " populations in " <> showMs (nsToSecs (t1 - t0)) <> " (violations: " <> tshow initForce <> " weights)"
    when verify $ do
      forM_ (Map.toList (ieCptSet engine1)) $ \(c, atoms) ->
        logDebug . display $ "pop " <> tshow c <> ": " <> tshow (Map.size atoms) <> " atoms"
      checkOracle "backfill" engine1
    let loop :: Int -> IncEngine -> Word64 -> [Row] -> RIO env [Row]
        loop i eng rng acc
          | i > txCount = pure (reverse acc)
          | otherwise = do
              let (mtx, opName, rng') = genTx eng synthRels ci n rng
              case mtx of
                Nothing -> loop i eng rng' acc
                Just tx -> do
                  ta <- getMonotonicTime
                  let (eng', deltas) = applyTx eng tx
                      !dForce = sum (map (relSizeF . snd) deltas)
                  tb <- dForce `seq` getMonotonicTime
                  let dirtyRel = Map.keys (txRel tx)
                      affected = L.nubBy (\a b -> fst a == fst b) (concatMap affectedTermsOf dirtyRel)
                      popsNow = popsView eng'
                  tc <- getMonotonicTime
                  let !fullForce = sum [Set.size (fullContents ci popsNow term) | (_, term) <- affected]
                  td <- fullForce `seq` getMonotonicTime
                  when verify $ checkOracle ("tx " <> tshow i) eng'
                  let row = Row n i opName (nsToMicros (tb - ta)) (nsToMicros (td - tc)) (length affected)
                  loop (i + 1) eng' rng' (row : acc)
    rows <- loop 1 engine1 rng1 []
    summarize n rows
    pure rows
  forM_ mCsv $ \path -> do
    writeFileUtf8 path (csvOf (concat allRows))
    logInfo . display $ "measurements written to " <> T.pack path
  where
    benchable c = case c of
      PlainConcept {} -> True
      _ -> False
    reportFallbacks eng = do
      let fbs = circuitFallbacks eng
          total = sum [n | (_, n, _) <- fbs]
          fb = sum [n | (_, _, n) <- fbs]
      logInfo . display $ "circuit coverage: " <> tshow (total - fb) <> " of " <> tshow total <> " nodes incremental, " <> tshow fb <> " fallback (recompute) nodes."
      forM_ [(nm, n) | (nm, _, n) <- fbs, n > 0]
        $ \(nm, n) -> logInfo . display $ "  fallback nodes in " <> nm <> ": " <> tshow n
    checkOracle lbl eng =
      case verifyDetails eng of
        [] -> pure ()
        bad -> do
          forM_ bad $ \(nm, term, incN, oraN, missing, extra) -> do
            logError . display $ "ORACLE MISMATCH after " <> lbl <> " in conjunct " <> nm <> " (term " <> showA term <> "): incremental " <> tshow incN <> " pairs, oracle " <> tshow oraN <> " pairs."
            forM_ missing $ \p -> logError . display $ "  missing: " <> tshow p
            forM_ extra $ \p -> logError . display $ "  extra:   " <> tshow p
          exitFailure
    summarize n rows = do
      let incs = L.sort (map rowIncMicros rows)
          fulls = L.sort (map rowFullMicros rows)
      logInfo . display $ summaryLine n incs fulls
    summaryLine :: Int -> [Double] -> [Double] -> Text
    summaryLine n incs fulls =
      "scale "
        <> tshow n
        <> ": incremental median "
        <> showMicros (median incs)
        <> " (p95 "
        <> showMicros (percentile 95 incs)
        <> "), full-affected median "
        <> showMicros (median fulls)
        <> " (p95 "
        <> showMicros (percentile 95 fulls)
        <> "), speedup ×"
        <> tshow (roundTo 1 (median fulls / max 0.001 (median incs)))

parseScales :: Text -> [Int]
parseScales t =
  case mapM (readMaybe . T.unpack . T.strip) (T.split (== ',') t) of
    Just ns | not (null ns), all (> 0) ns -> ns
    _ -> fatal "--scales must be a comma-separated list of positive numbers, e.g. 1000,2000,4000"

-- deterministic splitmix64
mix :: Word64 -> Word64
mix s =
  let z0 = s + 0x9E3779B97F4A7C15
      z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xBF58476D1CE4E5B9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
   in z2 `xor` (z2 `shiftR` 31)

randBelow :: Int -> Word64 -> (Int, Word64)
randBelow n rng =
  let rng' = mix rng
   in (fromIntegral (rng' `mod` fromIntegral (max 1 n)), rng')

-- | Synthetic initial population: per relation, @n@ distinct pairs drawn from
--   pools of @n@ atoms per concept.
synthesize :: ContextInfo -> [Relation] -> Int -> Word64 -> ([Population], Word64)
synthesize ci rels n rng0 = go rels rng0 []
  where
    go [] rng acc = (acc, rng)
    go (r : rs) rng acc =
      let (prs, rng') = drawPairs r n rng Set.empty
       in go rs rng'
            $ ARelPopu
              { popsrc = source r,
                poptgt = target r,
                popdcl = r,
                popps = Set.map (uncurry mkAtomPair) prs
              }
            : acc
    drawPairs r k rng acc
      | Set.size acc >= k = (acc, rng)
      | otherwise =
          let (i, rng1) = randBelow n rng
              (j, rng2) = randBelow n rng1
              p = (benchAtom ci (source r) i, benchAtom ci (target r) j)
           in drawPairs r k rng2 (Set.insert p acc)

-- | One random single-pair transaction: insert a fresh pair or delete an
--   existing one in a randomly chosen relation.
genTx :: IncEngine -> [Relation] -> ContextInfo -> Int -> Word64 -> (Maybe TxDelta, Text, Word64)
genTx eng rels ci n rng0 =
  let (ri, rng1) = randBelow (length rels) rng0
      r = nth ri rels
      existing = Map.findWithDefault Set.empty r (ieRelPairs eng)
      (coin, rng2) = randBelow 2 rng1
      doInsert = coin == 0 || Set.null existing
   in if doInsert
        then
          let tryPair 0 rng = (Nothing, "skip", rng)
              tryPair (k :: Int) rng =
                let (i, rngA) = randBelow n rng
                    (j, rngB) = randBelow n rngA
                    p = (benchAtom ci (source r) i, benchAtom ci (target r) j)
                 in if p `Set.member` existing
                      then tryPair (k - 1) rngB
                      else (Just (txOf r p 1), "ins", rngB)
           in tryPair (8 :: Int) rng2
        else
          let (i, rng3) = randBelow (Set.size existing) rng2
              p = nth i (Set.toAscList existing)
           in (Just (txOf r p (-1)), "del", rng3)
  where
    txOf r (x, y) w =
      TxDelta
        { txRel = Map.singleton r (Map.singleton x (Map.singleton y w)),
          txCpt = Map.empty
        }

-- | A deterministic synthetic atom for a concept, honouring its representation.
benchAtom :: ContextInfo -> A_Concept -> Int -> AAtomValue
benchAtom ci c i =
  case unsafePAtomVal2AtomValue tt (Just c) pav of
    Right v -> v
    Left msg -> fatal ("incremental-bench cannot synthesize an atom for " <> tshow c <> ": " <> msg)
  where
    tt = reprType ci c
    pav = case tt of
      Integer -> ScriptInt OriginUnknown (fromIntegral i)
      Float -> ScriptFloat OriginUnknown (fromIntegral i)
      Boolean -> ComnBool OriginUnknown (even i)
      _ -> ScriptString OriginUnknown ("bench_" <> tshow i)

-- 'getMonotonicTime' returns nanoseconds (verified empirically against a
-- 100 ms threadDelay).
nsToMicros :: Double -> Double
nsToMicros = (/ 1e3)

nsToSecs :: Double -> Double
nsToSecs = (/ 1e9)

nth :: Int -> [a] -> a
nth i xs = case L.drop i xs of
  (x : _) -> x
  [] -> fatal "nth: index out of range"

median :: [Double] -> Double
median [] = 0
median xs = nth (length xs `div` 2) (L.sort xs)

percentile :: Int -> [Double] -> Double
percentile _ [] = 0
percentile p xs = let s = L.sort xs in nth (min (length s - 1) (length s * p `div` 100)) s

roundTo :: Int -> Double -> Double
roundTo d x = fromIntegral (round (x * f) :: Integer) / f
  where
    f = 10 ^^ d

showMicros :: Double -> Text
showMicros x
  | x >= 1000 = tshow (roundTo 2 (x / 1000)) <> " ms"
  | otherwise = tshow (roundTo 1 x) <> " µs"

showMs :: Double -> Text
showMs secs = tshow (roundTo 1 (secs * 1000)) <> " ms"

csvOf :: [Row] -> Text
csvOf rows =
  T.unlines
    $ "scale,tx,op,incremental_us,full_affected_us,affected_conjuncts"
    : [ T.intercalate
          ","
          [ tshow (rowScale r),
            tshow (rowTx r),
            rowOp r,
            tshow (rowIncMicros r),
            tshow (rowFullMicros r),
            tshow (rowAffected r)
          ]
        | r <- rows
      ]

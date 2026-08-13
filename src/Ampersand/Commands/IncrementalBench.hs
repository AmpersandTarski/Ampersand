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
import Ampersand.FSpec.Incremental.Synthetic
import Ampersand.FSpec.Incremental.ZSet (relSizeF)
import Ampersand.FSpec.ToFSpec.Populated (fullContents)
import Ampersand.Misc.HasClasses
import Ampersand.Prototype.DeltaSQLHarness (runDeltaSqlHarness)
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
  sqlMode <- view incBenchSqlL
  replay <- view incBenchReplayL
  refereeEvery <- view incBenchRefereeEveryL
  let scales = parseScales scalesTxt
  when sqlMode $ do
    results <- forM (if replay then take 1 scales else scales) $ \n -> do
      logInfo . display $ "--- delta-sql harness at scale " <> tshow n <> (if replay then " (replay: scale ignored)" else " pairs per relation") <> " ---"
      runDeltaSqlHarness replay n txCount refereeEvery seed fSpec
    if and results
      then logInfo "delta-sql harness: all scales green."
      else do
        logError "delta-sql harness: differences found; see log."
        exitFailure
    exitSuccess
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
  let engine0 = mkEngine ci allConcepts terms
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
              let (mpick, opName, rng') = pickTx (ieRelPairs eng) synthRels ci n rng
              case mpick of
                Nothing -> loop i eng rng' acc
                Just (r, (x, y), w) -> do
                  let tx =
                        TxDelta
                          { txRel = Map.singleton r (Map.singleton x (Map.singleton y w)),
                            txCpt = Map.empty
                          }
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

-- 'getMonotonicTime' returns nanoseconds (verified empirically against a
-- 100 ms threadDelay).
nsToMicros :: Double -> Double
nsToMicros = (/ 1e3)

nsToSecs :: Double -> Double
nsToSecs = (/ 1e9)

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

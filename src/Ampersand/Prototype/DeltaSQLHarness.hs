{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The referee harness for delta SQL (issue #1684, Phase 3 exit criterion).
--
--   Against a real (temporary) MariaDB database: install the model, prepare a
--   population, backfill a violation cache from the full queries, then stream
--   single-pair transactions maintained by the delta protocol — fill the
--   delta tables, DELETE the cache rows in the candidate set, re-INSERT the
--   violation rows restricted to the candidate set, clear the delta tables —
--   and compare the maintained caches against fresh full-query evaluation:
--   after every transaction for the conjuncts the transaction touched, and
--   integrally at the end. Zero differences is the verdict.
--
--   Two transaction modes:
--
--   * synthetic (default): pools of generated atoms, insert/delete mix.
--     Requires @--sql-bin-tables@, because inserting a fresh pair into a wide
--     table would need row creation.
--   * replay (@--replay@): delete existing pairs from the real population and
--     re-insert previously deleted ones. Type-safe on any model and layout;
--     wide-table mutations are UPDATEs on the existing rows.
module Ampersand.Prototype.DeltaSQLHarness
  ( runDeltaSqlHarness,
  )
where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Classes
import Ampersand.FSpec
import Ampersand.FSpec.FSpecAux (getRelationTableInfo)
import Ampersand.FSpec.Incremental.DeltaTerms
import Ampersand.FSpec.Incremental.Synthetic
import Ampersand.FSpec.SQL (sqlQuery)
import Ampersand.Prototype.PHP
import Ampersand.Prototype.TableSpec (createTableSql, doubleQuote, plug2TableSpec, queryAsSQL)
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Text as T

cacheTable :: Text
cacheTable = "delta_cache_harness"

type Pair = (AAtomValue, AAtomValue)

type Shadow = Map Relation (Set Pair)

-- | Run the harness at one scale. Returns True when every check found the
--   maintained caches identical to full re-evaluation.
runDeltaSqlHarness ::
  (HasLogFunc env) =>
  Bool -> -- replay mode?
  Int -> -- scale: synthetic pairs per relation (ignored in replay mode)
  Int -> -- number of transactions
  Int -> -- referee cadence: check affected conjuncts every k-th transaction
  Int -> -- seed
  FSpec ->
  RIO env Bool
runDeltaSqlHarness replay scale txCount refEvery seed fSpec = do
  env <- ask
  let ci = fcontextInfo fSpec
      ctx = fromMaybe (fatal "delta-sql harness needs the original context") (originalContext fSpec)
      conjTerms =
        [ (text1ToText (rc_id conj), conjNF env (notCpl (rcConjunct conj)))
          | conj <- allConjuncts fSpec
        ]
      supported =
        [ (nm, term, dqs)
          | (nm, term) <- conjTerms,
            Just dqs <- [deltaQueriesFor term]
        ]
      termRels = Set.toList . Set.unions $ map (bindedRelationsIn . snd) conjTerms
      scriptPops = ctxpopus ctx <> concatMap ptups (patterns ctx)
      scriptShadow :: Shadow
      scriptShadow =
        Map.fromListWith
          Set.union
          [ (popdcl p, Set.map (\pr -> (apLeft pr, apRight pr)) (popps p))
            | p@ARelPopu {} <- scriptPops,
              popdcl p `elem` termRels
          ]
      benchable c = case c of
        PlainConcept {} -> True
        _ -> False
      synthRels = [r | r <- termRels, benchable (source r), benchable (target r)]
      isBinStored r = case fst (getRelationTableInfo fSpec r) of
        BinSQL {} -> True
        _ -> False
  logInfo . display $ "delta-sql harness: " <> tshow (length conjTerms) <> " conjuncts, " <> tshow (length supported) <> " with delta queries" <> (if replay then " (replay mode)" else "") <> "."
  forM_ [nm | (nm, term) <- conjTerms, isNothing (deltaQueriesFor term)]
    $ \nm -> logInfo . display $ "  full re-evaluation stays for " <> nm <> " (term outside the supported class)."
  if null supported
    then do
      logInfo "Nothing to test: no conjunct has delta queries."
      pure True
    else do
      let nonBin = [r | r <- synthRels, not (isBinStored r)]
      if not replay && not (null nonBin)
        then do
          logError . display $ "Synthetic mode needs every relation in its own binary table; not so for: " <> T.intercalate ", " (map fullName nonBin) <> ". Run with --sql-bin-tables, or use --replay."
          pure False
        else do
          logDebug "Initializing temporary database"
          ok <- createTempDatabase fSpec
          if not ok
            then do
              logError "Database creation failed; no harness run."
              pure False
            else do
              let dbNm = tempDbName fSpec
                  (synthPops, rng1) =
                    if replay
                      then ([], mix (fromIntegral seed))
                      else synthesize ci synthRels scale (mix (fromIntegral seed))
                  shadow0 :: Shadow
                  shadow0 =
                    Map.unionWith
                      Set.union
                      scriptShadow
                      ( Map.fromListWith
                          Set.union
                          [ (popdcl p, Set.map (\pr -> (apLeft pr, apRight pr)) (popps p))
                            | p@ARelPopu {} <- synthPops
                          ]
                      )
                  mutableRels =
                    if replay
                      then [r | r <- termRels, not (Set.null (fromMaybe Set.empty (Map.lookup r shadow0)))]
                      else synthRels
              when (replay && null mutableRels)
                $ logWarn "replay mode: the population holds no pairs for any conjunct relation; the transaction stream will be empty."
              executeRawSQL dbNm
                $ [queryAsSQL (createTableSql False (plug2TableSpec p)) | p <- deltaPlugs fSpec]
                <> [ "CREATE TABLE "
                       <> doubleQuote cacheTable
                       <> " ("
                       <> doubleQuote "conj"
                       <> " VARCHAR(255) NOT NULL, "
                       <> doubleQuote "src"
                       <> " VARCHAR(750) NOT NULL, "
                       <> doubleQuote "tgt"
                       <> " VARCHAR(750) NOT NULL) ENGINE = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN"
                   ]
                <> [ insertPairsStmt r (Set.toList prs)
                     | not replay,
                       (r, prs) <-
                         Map.toList
                           ( Map.fromListWith
                               Set.union
                               [ (popdcl p, Set.map (\pr -> (apLeft pr, apRight pr)) (popps p))
                                 | p@ARelPopu {} <- synthPops
                               ]
                           ),
                       not (Set.null prs)
                   ]
                <> [backfillStmt nm term | (nm, term, _) <- supported]
              okBackfill <- referee dbNm [(nm, term) | (nm, term, _) <- supported] "backfill"
              if not okBackfill
                then pure False
                else do
                  (okLoop, timings) <- loop dbNm supported mutableRels ci shadow0 [] rng1 1 []
                  if not okLoop
                    then pure False
                    else do
                      okFinal <- referee dbNm [(nm, term) | (nm, term, _) <- supported] "final full check"
                      when okFinal $ do
                        logInfo
                          . display
                          $ "delta-sql harness: "
                          <> tshow txCount
                          <> " transactions, all caches identical to full evaluation."
                        reportTimings timings
                      pure okFinal
  where
    loop dbNm supported mutableRels ci shadow pool rng i timings
      | i > txCount = pure (True, timings)
      | otherwise = do
          let (mtx, pool', rng') =
                if replay
                  then pickReplayTx shadow pool mutableRels rng
                  else
                    let (m, _, rg) = pickTx shadow mutableRels ci scale rng
                     in (m, pool, rg)
          case mtx of
            Nothing -> do
              -- no pair to mutate (e.g. every replayable pair deleted):
              -- stop early rather than spin.
              logInfo . display $ "no further mutable pairs after " <> tshow (i - 1) <> " transactions; stopping the stream early."
              pure (True, timings)
            Just (r, p@(a, b), w) -> do
              let shadow' = Map.adjust (\s -> if w > 0 then Set.insert p s else Set.delete p s) r shadow
                  dTbl = doubleQuote (deltaTableName r)
                  deltaStmt = "INSERT INTO " <> dTbl <> " (" <> doubleQuote "src" <> ", " <> doubleQuote "tgt" <> ") VALUES (" <> showValSQL a <> ", " <> showValSQL b <> ")"
                  maintStmts =
                    concat
                      [ maintain nm term candTerm
                        | (nm, term, dqs) <- supported,
                          (r', candTerm) <- dqs,
                          r' == r
                      ]
                  affected = [(nm, term) | (nm, term, dqs) <- supported, any ((== r) . fst) dqs]
              t0 <- getMonotonicTime
              executeRawSQL dbNm (mutationStmts r (a, b) w <> [deltaStmt] <> maintStmts <> ["DELETE FROM " <> dTbl])
              t1 <- getMonotonicTime
              okTx <-
                if i `mod` max 1 refEvery == 0
                  then referee dbNm affected ("tx " <> tshow i)
                  else pure True
              t2 <- getMonotonicTime
              if okTx
                then loop dbNm supported mutableRels ci shadow' pool' rng' (i + 1) ((t1 - t0, t2 - t1) : timings)
                else pure (False, timings)

    -- getMonotonicTime returns nanoseconds here (verified against a sleep).
    -- The two measured phases run over the same PHP round-trip mechanism, so
    -- the process overhead is comparable; the delta-protocol side is ONE
    -- round trip, the referee side is two per affected conjunct (cache pull
    -- plus full-query evaluation). The reported ratio therefore understates
    -- the query-level difference.
    reportTimings timings =
      case timings of
        [] -> pure ()
        _ -> do
          let ms xs = tshow (roundMs (medianD xs)) <> " ms"
              protMs = [x / 1e6 | (x, _) <- timings]
              refMs = [y / 1e6 | (_, y) <- timings, y / 1e6 > 1]
          logInfo
            . display
            $ "per transaction (median): delta protocol "
            <> ms protMs
            <> ", referee (cache pull + full re-evaluation of affected conjuncts) "
            <> ms refMs
            <> " over "
            <> tshow (length refMs)
            <> " refereed transactions; both include PHP round-trip overhead."
    medianD xs = case L.sort xs of
      [] -> 0
      s -> nth (length s `div` 2) s
    roundMs :: Double -> Double
    roundMs x = fromIntegral (round (x * 10) :: Integer) / 10

    -- pair mutation per storage layout; in replay mode the wide-table rows
    -- exist (the pairs come from the installed population), so UPDATE suffices.
    mutationStmts r (a, b) w =
      case getRelationTableInfo fSpec r of
        (plug@BinSQL {}, store) ->
          let (tbl, srcCol, tgtCol) = binCols plug store
           in if w > 0
                then ["INSERT INTO " <> tbl <> " (" <> srcCol <> ", " <> tgtCol <> ") VALUES (" <> showValSQL a <> ", " <> showValSQL b <> ")"]
                else ["DELETE FROM " <> tbl <> " WHERE " <> srcCol <> " = " <> showValSQL a <> " AND " <> tgtCol <> " = " <> showValSQL b]
        (plug@TblSQL {}, store) ->
          let tbl = doubleQuote (text1ToText (showUnique plug))
              colNm = doubleQuote . text1ToText . sqlColumNameToText1 . attSQLColName
              srcCol = colNm (rsSrcAtt store)
              tgtCol = colNm (rsTrgAtt store)
              -- the key side is the concept whose table stores the relation
              ((keyCol, keyVal), (attCol, attVal)) =
                if rsStoredFlipped store
                  then ((tgtCol, showValSQL b), (srcCol, showValSQL a))
                  else ((srcCol, showValSQL a), (tgtCol, showValSQL b))
           in if w > 0
                then ["UPDATE " <> tbl <> " SET " <> attCol <> " = " <> attVal <> " WHERE " <> keyCol <> " = " <> keyVal]
                else ["UPDATE " <> tbl <> " SET " <> attCol <> " = NULL WHERE " <> keyCol <> " = " <> keyVal <> " AND " <> attCol <> " = " <> attVal]

    binCols plug store =
      let colNm = doubleQuote . text1ToText . sqlColumNameToText1 . attSQLColName
       in ( doubleQuote (text1ToText (showUnique plug)),
            colNm (rsSrcAtt store),
            colNm (rsTrgAtt store)
          )

    insertPairsStmt r prs =
      case getRelationTableInfo fSpec r of
        (plug@BinSQL {}, store) ->
          let (tbl, srcCol, tgtCol) = binCols plug store
           in "INSERT IGNORE INTO "
                <> tbl
                <> " ("
                <> srcCol
                <> ", "
                <> tgtCol
                <> ") VALUES "
                <> T.intercalate ", " ["(" <> showValSQL a <> ", " <> showValSQL b <> ")" | (a, b) <- prs]
        _ -> fatal "synthetic population needs binary tables (guarded above)"

    -- the delta protocol for one (conjunct, changed relation)
    maintain nm term candTerm =
      let candSQL = sqlQuery (withDeltaPlugs fSpec) candTerm
          violSQL = sqlQuery fSpec term
          inCands col =
            "(" <> col <> ".\"src\", " <> col <> ".\"tgt\") IN (SELECT \"src\", \"tgt\" FROM (" <> candSQL <> ") AS cand)"
       in [ "DELETE c FROM " <> doubleQuote cacheTable <> " AS c WHERE c.\"conj\" = " <> sqlStr nm <> " AND " <> inCands "c",
            "INSERT INTO "
              <> doubleQuote cacheTable
              <> " SELECT "
              <> sqlStr nm
              <> ", v.\"src\", v.\"tgt\" FROM ("
              <> violSQL
              <> ") AS v WHERE "
              <> inCands "v"
          ]

    backfillStmt nm term =
      "INSERT INTO "
        <> doubleQuote cacheTable
        <> " SELECT "
        <> sqlStr nm
        <> ", v.\"src\", v.\"tgt\" FROM ("
        <> sqlQuery fSpec term
        <> ") AS v"

    referee dbNm pairsToCheck lbl = do
      results <- forM pairsToCheck $ \(nm, term) -> do
        cacheRows <-
          performRawQuery dbNm
            $ "SELECT \"src\", \"tgt\" FROM "
            <> doubleQuote cacheTable
            <> " WHERE \"conj\" = "
            <> sqlStr nm
        fullRows <- evaluateExpSQL fSpec dbNm term
        let same = L.sort cacheRows == L.sort fullRows
        unless same $ do
          logError . display $ "DELTA-SQL MISMATCH after " <> lbl <> " in conjunct " <> nm <> ": cache " <> tshow (length cacheRows) <> " rows, full query " <> tshow (length fullRows) <> " rows."
          logError . display $ "  cache-only: " <> tshow (take 3 (L.sort cacheRows L.\\ L.sort fullRows))
          logError . display $ "  full-only:  " <> tshow (take 3 (L.sort fullRows L.\\ L.sort cacheRows))
        pure same
      pure (and results)

    sqlStr :: Text -> Text
    sqlStr t = "'" <> T.intercalate "''" (T.split (== '\'') t) <> "'"

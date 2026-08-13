{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The referee harness for delta SQL (issue #1684, Phase 3 exit criterion).
--
--   Against a real (temporary) MariaDB database: install the model, add a
--   synthetic population, backfill a violation cache from the full queries,
--   then stream single-pair transactions maintained by the delta protocol —
--   fill the delta tables, DELETE the cache rows in the candidate set,
--   re-INSERT the violation rows restricted to the candidate set, clear the
--   delta tables — and after every transaction compare each maintained cache
--   against a fresh full-query evaluation. Zero differences is the verdict.
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

-- | Run the harness at one scale. Returns True when every transaction kept
--   every maintained cache identical to full re-evaluation.
runDeltaSqlHarness ::
  (HasLogFunc env) =>
  Int -> -- scale: synthetic pairs per relation
  Int -> -- number of transactions
  Int -> -- seed
  FSpec ->
  RIO env Bool
runDeltaSqlHarness scale txCount seed fSpec = do
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
      synthRels = [r | r <- termRels, benchable (source r), benchable (target r)]
      nonBinRels = [r | r <- synthRels, not (isBinStored r)]
  logInfo . display $ "delta-sql harness: " <> tshow (length conjTerms) <> " conjuncts, " <> tshow (length supported) <> " with delta queries."
  forM_ [nm | (nm, term) <- conjTerms, isNothing (deltaQueriesFor term)]
    $ \nm -> logInfo . display $ "  full re-evaluation stays for " <> nm <> " (term outside the supported class)."
  if null supported
    then do
      logInfo "Nothing to test: no conjunct has delta queries."
      pure True
    else
      if not (null nonBinRels)
        then do
          logError . display $ "These relations are not stored in their own binary table: " <> T.intercalate ", " (map fullName nonBinRels) <> ". Run with --sql-bin-tables."
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
                  scriptPops = ctxpopus ctx <> concatMap ptups (patterns ctx)
                  (synthPops, rng1) = synthesize ci synthRels scale (mix (fromIntegral seed))
                  shadow0 =
                    Map.fromListWith Set.union
                      $ [ (popdcl p, Set.map (\pr -> (apLeft pr, apRight pr)) (popps p))
                          | p@ARelPopu {} <- scriptPops <> synthPops,
                            popdcl p `elem` synthRels
                        ]
              -- delta tables, cache table, synthetic population, backfill
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
                <> [ insertPairs fSpec r (Set.toList prs)
                     | (r, prs) <- Map.toList shadow0,
                       not (Set.null prs)
                   ]
                <> [backfillStmt fSpec nm term | (nm, term, _) <- supported]
              okBackfill <- refereeAll fSpec dbNm supported "backfill"
              if not okBackfill
                then pure False
                else loop fSpec dbNm ci supported synthRels shadow0 rng1 1
  where
    benchable c = case c of
      PlainConcept {} -> True
      _ -> False
    isBinStored r = case fst (getRelationTableInfo fSpec r) of
      BinSQL {} -> True
      _ -> False
    loop fSpc dbNm ci supported synthRels shadow rng i
      | i > txCount = do
          logInfo . display $ "delta-sql harness: " <> tshow txCount <> " transactions, all caches identical to full evaluation."
          pure True
      | otherwise = do
          let (mtx, _, rng') = pickTx shadow synthRels ci scale rng
          case mtx of
            Nothing -> loop fSpc dbNm ci supported synthRels shadow rng' i
            Just (r, p@(a, b), w) -> do
              let shadow' = Map.adjust (\s -> if w > 0 then Set.insert p s else Set.delete p s) r shadow
                  (relTbl, srcCol, tgtCol) = relCols fSpc r
                  dTbl = doubleQuote (deltaTableName r)
                  mutStmt =
                    if w > 0
                      then "INSERT INTO " <> relTbl <> " (" <> srcCol <> ", " <> tgtCol <> ") VALUES (" <> showValSQL a <> ", " <> showValSQL b <> ")"
                      else "DELETE FROM " <> relTbl <> " WHERE " <> srcCol <> " = " <> showValSQL a <> " AND " <> tgtCol <> " = " <> showValSQL b
                  deltaStmt = "INSERT INTO " <> dTbl <> " (" <> doubleQuote "src" <> ", " <> doubleQuote "tgt" <> ") VALUES (" <> showValSQL a <> ", " <> showValSQL b <> ")"
                  maintStmts =
                    concat
                      [ maintain fSpc nm term candTerm
                        | (nm, term, dqs) <- supported,
                          (r', candTerm) <- dqs,
                          r' == r
                      ]
                  clearStmt = "DELETE FROM " <> dTbl
              executeRawSQL dbNm ([mutStmt, deltaStmt] <> maintStmts <> [clearStmt])
              okTx <- refereeAll fSpc dbNm supported ("tx " <> tshow i)
              if okTx
                then loop fSpc dbNm ci supported synthRels shadow' rng' (i + 1)
                else pure False

    -- the delta protocol for one (conjunct, changed relation)
    maintain fSpc nm term candTerm =
      let candSQL = sqlQuery (withDeltaPlugs fSpc) candTerm
          violSQL = sqlQuery fSpc term
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

    backfillStmt fSpc nm term =
      "INSERT INTO "
        <> doubleQuote cacheTable
        <> " SELECT "
        <> sqlStr nm
        <> ", v.\"src\", v.\"tgt\" FROM ("
        <> sqlQuery fSpc term
        <> ") AS v"

    insertPairs fSpc r prs =
      let (relTbl, srcCol, tgtCol) = relCols fSpc r
       in "INSERT IGNORE INTO "
            <> relTbl
            <> " ("
            <> srcCol
            <> ", "
            <> tgtCol
            <> ") VALUES "
            <> T.intercalate ", " ["(" <> showValSQL a <> ", " <> showValSQL b <> ")" | (a, b) <- prs]

    relCols fSpc r =
      let (plug, store) = getRelationTableInfo fSpc r
          colNm = doubleQuote . text1ToText . sqlColumNameToText1 . attSQLColName
       in ( doubleQuote (text1ToText (showUnique plug)),
            colNm (rsSrcAtt store),
            colNm (rsTrgAtt store)
          )

    refereeAll fSpc dbNm supported lbl = do
      results <- forM supported $ \(nm, term, _) -> do
        cacheRows <-
          performRawQuery dbNm
            $ "SELECT \"src\", \"tgt\" FROM "
            <> doubleQuote cacheTable
            <> " WHERE \"conj\" = "
            <> sqlStr nm
        fullRows <- evaluateExpSQL fSpc dbNm term
        let same = L.sort cacheRows == L.sort fullRows
        unless same $ do
          logError . display $ "DELTA-SQL MISMATCH after " <> lbl <> " in conjunct " <> nm <> ": cache " <> tshow (length cacheRows) <> " rows, full query " <> tshow (length fullRows) <> " rows."
          logError . display $ "  cache-only: " <> tshow (take 3 (L.sort cacheRows L.\\ L.sort fullRows))
          logError . display $ "  full-only:  " <> tshow (take 3 (L.sort fullRows L.\\ L.sort cacheRows))
        pure same
      pure (and results)

    sqlStr :: Text -> Text
    sqlStr t = "'" <> T.intercalate "''" (T.split (== '\'') t) <> "'"

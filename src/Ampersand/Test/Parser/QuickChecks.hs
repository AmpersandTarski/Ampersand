{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Ampersand.Test.Parser.QuickChecks
  ( doAllQuickCheckPropertyTests,
    TestResult (..),
  )
where

import Ampersand.ADL1.PrettyPrinters (prettyPrint)
import Ampersand.Basics
import Ampersand.Core.ParseTree (Inclusion (..), InclusionItem (..), P_Context)
import Ampersand.Input.ADL1.CtxError (Guarded (..))
import Ampersand.Input.Parsing (parseCtx, parseScriptFile)
import Ampersand.Test.Parser.ArbitraryTree ()
import RIO.Char (isLower, isUpper)
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import Test.QuickCheck

-- | An application specific data type that enables nice error
--   messages in the output.
data TestResult = TestResult
  { qcPropName :: !Text,
    qcIsSuccess :: !Bool,
    qcMessage :: ![Text],
    qcQuickCheckResult :: !Result
  }

doAllQuickCheckPropertyTests :: (HasLogFunc env) => RIO env Bool
doAllQuickCheckPropertyTests =
  whileSuccess
    [ doRoundtripTest,
      doInclusionRoundtripTest,
      doObjectIdShorteningTest
      -- More tests can be inserted here
    ]
  where
    whileSuccess :: (HasLogFunc env) => [RIO env TestResult] -> RIO env Bool
    whileSuccess [] = do
      logInfo "All tests succeeded."
      pure True
    whileSuccess (h : tl) = do
      res <- h
      if qcIsSuccess res
        then do
          logInfo . display $ "✅ Passed: " <> qcPropName res
          whileSuccess tl
        else do
          logInfo . display $ "❗❗❗Failed: " <> qcPropName res
          pure False

doRoundtripTest :: RIO env TestResult
doRoundtripTest = do
  qcResult <- liftIO . quickCheckWithResult checkArgs $ prop_parserRoundtrip
  pure
    TestResult
      { qcPropName = "Prettyprint/Parser roundtrip.",
        qcIsSuccess = isSuccess qcResult,
        qcMessage = ["---Some message---"],
        qcQuickCheckResult = qcResult
      }
  where
    checkArgs :: Args
    checkArgs =
      Args
        { replay = Nothing,
          maxSuccess = 100,
          maxDiscardRatio = 8,
          maxSize = 15, -- otherwise there's nothing quick about it.
          maxShrinks = 50,
          chatty = True
        }

-- | The inclusion statements of a file (@CONTEXT A INCLUDES B@) stand outside every CONTEXT block,
--   so the roundtrip of a P_Context never meets them. This property prints generated statements,
--   parses them with the file parser, and compares what comes back with what went in.
--   QuickCheck enforces that each variant of the syntax occurs in a run (issue #1736).
doInclusionRoundtripTest :: RIO env TestResult
doInclusionRoundtripTest = do
  qcResult <- liftIO . quickCheckWithResult stdArgs {maxSize = 10, chatty = True} $ prop_inclusionRoundtrip
  pure
    TestResult
      { qcPropName = "Prettyprint/Parser roundtrip of inclusion statements, NAMED included.",
        qcIsSuccess = isSuccess qcResult,
        qcMessage = ["The printed inclusion statements must parse back to the same statements."],
        qcQuickCheckResult = qcResult
      }

-- | What an inclusion statement says, without its source position: the contexts before INCLUDES with
--   their database names, and for every included context its name, file, parameters, alias and database name.
type InclusionData = ([(Text, Maybe Text)], [(Text, Maybe FilePath, [Text], Maybe Text, Maybe Text)])

inclusionData :: Inclusion -> InclusionData
inclusionData (Inclusion _ includers items) =
  ( [(fullName nm, db) | (nm, db) <- NE.toList includers],
    [(fullName (itName i), itFrom i, itDefs i, namePartToText <$> itAlias i, itDatabase i) | i <- NE.toList items]
  )

prop_inclusionRoundtrip :: NonEmptyList Inclusion -> Property
prop_inclusionRoundtrip (NonEmpty stmts) =
  checkCoverage
    . coverage
    . counterexample ("The printed statements:\n" <> T.unpack printed)
    $ case parseScriptFile "inclusion roundtrip" printed of
      Errors err -> counterexample (show err) False
      Checked (parsed, blocks) _ ->
        counterexample "The parsed statements differ from the generated ones." (map inclusionData parsed === map inclusionData stmts)
          .&&. counterexample "The file parser found a context in statements only." (null blocks)
  where
    printed = T.unlines (map prettyPrint stmts)
    -- Every variant of the syntax has to occur in a run, or a green run would prove nothing about it.
    coverage =
      cover 10 (any (\s -> null (namesBefore s) && null (namesAfter s)) stmts) "a statement without NAMED"
        . cover 5 (any (\s -> not (null (namesBefore s)) && null (namesAfter s)) stmts) "NAMED before INCLUDES only"
        . cover 5 (any (\s -> null (namesBefore s) && not (null (namesAfter s))) stmts) "NAMED after INCLUDES only"
        . cover 10 (any (\s -> not (null (namesBefore s)) && not (null (namesAfter s))) stmts) "NAMED on both sides"
        . cover 5 (any (\s -> any (`elem` namesAfter s) (namesBefore s)) stmts) "the same database name on both sides"
        . cover 5 (any (\s -> length (L.nub (namesBefore s <> namesAfter s)) > 1) stmts) "different database names in one statement"
        . cover 10 (any (T.any isUpper . T.take 1) allNames) "a database name that starts with an upper-case letter"
        . cover 10 (any (T.any isLower . T.take 1) allNames) "a database name that starts with a lower-case letter"
        . cover 10 (any ((> 1) . length . incIncluders) stmts) "several contexts before INCLUDES"
        . cover 10 (any ((> 1) . length . incItems) stmts) "several included contexts"
        . cover 10 (any (isJust . itFrom) allItems) "FROM"
        . cover 10 (any (isNothing . itFrom) allItems) "no FROM"
        . cover 5 (not (all (null . itDefs) allItems)) "preprocessor parameters"
        . cover 10 (any (isJust . itAlias) allItems) "AS"
        . cover 10 (any (isNothing . itAlias) allItems) "no AS"
        . cover 5 (any (\i -> isJust (itFrom i) && isJust (itAlias i) && isJust (itDatabase i)) allItems) "FROM, AS and NAMED on one included context"
    allItems = concatMap (NE.toList . incItems) stmts
    allNames = concatMap (\s -> namesBefore s <> namesAfter s) stmts
    namesBefore s = [db | (_, Just db) <- NE.toList (incIncluders s)]
    namesAfter s = mapMaybe itDatabase (NE.toList (incItems s))

-- | Known-answer test for OBJECT atom-identity shortening. This pins 'shortenObjectId' to the
--   exact same algorithm as the runtime framework (PrototypeFramework, Atom::setId, OBJECT case),
--   using the shared vector "a" x 300 -> "a" x 243 <> "_003ef1ba9e". If either side diverges, the
--   test suite fails. ('ampersand validate' only checks Haskell-vs-SQL agreement within the
--   compiler, so it cannot catch a hash-digit mismatch against the PHP side; this test does.)
doObjectIdShorteningTest :: RIO env TestResult
doObjectIdShorteningTest = do
  qcResult <- liftIO . quickCheckWithResult stdArgs $ once (conjoin checks)
  pure
    TestResult
      { qcPropName = "Object-id shortening matches the cross-repo contract.",
        qcIsSuccess = isSuccess qcResult,
        qcMessage = ["shortenObjectId must stay identical to PrototypeFramework Atom::setId (OBJECT case)."],
        qcQuickCheckResult = qcResult
      }
  where
    -- Shared known-answer vector with the runtime framework's PHP unit test.
    knownInput = T.replicate 300 "a"
    knownExpected = T.replicate 243 "a" <> "_003ef1ba9e"
    checks :: [Property]
    checks =
      [ counterexample "known-answer vector" (shortenObjectId knownInput === knownExpected),
        counterexample "short id kept verbatim" (shortenObjectId "plain-id" === "plain-id"),
        counterexample "254 chars kept verbatim" (shortenObjectId (T.replicate 254 "a") === T.replicate 254 "a"),
        counterexample "result within 254 chars" (T.length (shortenObjectId knownInput) === 254),
        counterexample
          "idempotent on shortened result"
          (shortenObjectId (shortenObjectId knownInput) === shortenObjectId knownInput),
        counterexample
          "distinct long ids stay distinct"
          (shortenObjectId (T.replicate 260 "x" <> "AAA") =/= shortenObjectId (T.replicate 260 "x" <> "BBB"))
      ]

prop_parserRoundtrip :: P_Context -> Bool
prop_parserRoundtrip pCtx =
  case roundtrip pCtx of
    Checked _ _ -> True
    Errors err ->
      exitWith
        . SomeTestsFailed
        $ T.lines (tshow err)
        <> T.lines (prettyCtx pCtx)

roundtrip :: P_Context -> Guarded P_Context
roundtrip pCtx =
  fst
    <$> parseCtx
      ( "❗❗❗ ERROR: There is something wrong with the parser and/or with the\n"
          <> "           way an arbitrary P_Context is defined. (See ArbitraryTree.hs)\n"
          <> "           Below file at position"
      )
      (prettyCtx pCtx)

prettyCtx :: P_Context -> Text
prettyCtx =
  T.unlines
    . zipWith (curry includeLineNr) [1 ..]
    . T.lines
    . prettyPrint
  where
    includeLineNr :: (Int, Text) -> Text
    includeLineNr (nr, str) = "{-" <> T.replicate (4 - T.length (tshow nr)) "0" <> tshow nr <> "-} " <> str

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
import Ampersand.Core.ParseTree (P_Context)
import Ampersand.Input.ADL1.CtxError (Guarded (..))
import Ampersand.Input.Parsing (parseCtx)
import Ampersand.Test.Parser.ArbitraryTree ()
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
    [ doRoundtripTest
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

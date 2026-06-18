{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generate a prototype from a project.
module Ampersand.Commands.Test
  ( test,
    HasTestOpts (..),
  )
where

import Ampersand.Basics
import Ampersand.Misc.HasClasses (HasTestOpts (..))
import Ampersand.Test.Parser.QuickChecks
import Ampersand.Test.Regression (regressionTest)
import Ampersand.Test.RuleTest.SelfCheck (ruleTestSelfCheck)
import Ampersand.Types.Config (HasRunner)

test :: (HasTestOpts env, HasRunner env) => RIO env ()
test = do
  parserRoundtripTest
  ruleTestEngineTest
  regressionTest

ruleTestEngineTest :: (HasRunner env) => RIO env ()
ruleTestEngineTest = do
  success <- ruleTestSelfCheck
  unless success
    $ exitWith (SomeTestsFailed ["Rule-test engine self-check failed!"])

parserRoundtripTest :: (HasRunner env) => RIO env ()
parserRoundtripTest = do
  logInfo "Starting Quickcheck tests."
  success <- doAllQuickCheckPropertyTests
  if success
    then logInfo "✅ Passed."
    else do
      logError "❗❗❗ Failed. Quickcheck tests."
      exitWith (SomeTestsFailed ["Quickcheck test failed!"])

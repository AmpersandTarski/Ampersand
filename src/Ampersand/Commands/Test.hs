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
import Ampersand.Test.Express.ExpressParserTest (expressParserTest)
import Ampersand.Test.Parser.QuickChecks
import Ampersand.Test.Regression (regressionTest)
import Ampersand.Test.Step.StepParserTest (stepParserTest)
import Ampersand.Types.Config (HasRunner)

test :: (HasTestOpts env, HasRunner env) => RIO env ()
test = do
  parserRoundtripTest
  stepReaderTest
  expressTest
  regressionTest

stepReaderTest :: (HasRunner env) => RIO env ()
stepReaderTest = do
  success <- stepParserTest
  unless success $
    exitWith (SomeTestsFailed ["STEP/Part-21 reader test failed!"])

expressTest :: (HasRunner env) => RIO env ()
expressTest = do
  success <- expressParserTest
  unless success $
    exitWith (SomeTestsFailed ["EXPRESS schema reader test failed!"])

parserRoundtripTest :: (HasRunner env) => RIO env ()
parserRoundtripTest = do
  logInfo "Starting Quickcheck tests."
  success <- doAllQuickCheckPropertyTests
  if success
    then logInfo "✅ Passed."
    else do
      logError "❗❗❗ Failed. Quickcheck tests."
      exitWith (SomeTestsFailed ["Quickcheck test failed!"])

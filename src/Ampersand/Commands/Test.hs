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
import Ampersand.Test.IFC.IFCBinderTest (ifcBinderTest)
import Ampersand.Test.IFC.IFCRegressionTest (ifcRegressionTest)
import Ampersand.Test.IFC.IFCWiringTest (ifcWiringTest)
import Ampersand.Test.Parser.QuickChecks
import Ampersand.Test.Regression (regressionTest)
import Ampersand.Test.Step.StepParserTest (stepParserTest)
import Ampersand.Types.Config (HasRunner)

test :: (HasTestOpts env, HasRunner env) => RIO env ()
test = do
  parserRoundtripTest
  stepReaderTest
  expressTest
  ifcBinderTest'
  ifcWiringTest'
  ifcRegressionTest'
  regressionTest

ifcRegressionTest' :: (HasRunner env) => RIO env ()
ifcRegressionTest' = do
  success <- ifcRegressionTest
  unless success
    $ exitWith (SomeTestsFailed ["IFC end-to-end regression test failed!"])

ifcBinderTest' :: (HasRunner env) => RIO env ()
ifcBinderTest' = do
  success <- ifcBinderTest
  unless success
    $ exitWith (SomeTestsFailed ["IFC binder test failed!"])

ifcWiringTest' :: (HasRunner env) => RIO env ()
ifcWiringTest' = do
  success <- ifcWiringTest
  unless success
    $ exitWith (SomeTestsFailed ["IFC wiring test failed!"])

stepReaderTest :: (HasRunner env) => RIO env ()
stepReaderTest = do
  success <- stepParserTest
  unless success
    $ exitWith (SomeTestsFailed ["STEP/Part-21 reader test failed!"])

expressTest :: (HasRunner env) => RIO env ()
expressTest = do
  success <- expressParserTest
  unless success
    $ exitWith (SomeTestsFailed ["EXPRESS schema reader test failed!"])

parserRoundtripTest :: (HasRunner env) => RIO env ()
parserRoundtripTest = do
  logInfo "Starting Quickcheck tests."
  success <- doAllQuickCheckPropertyTests
  if success
    then logInfo "✅ Passed."
    else do
      logError "❗❗❗ Failed. Quickcheck tests."
      exitWith (SomeTestsFailed ["Quickcheck test failed!"])

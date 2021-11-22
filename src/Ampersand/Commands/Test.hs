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
import Ampersand.Types.Config (HasRunner)

test :: (HasTestOpts env, HasRunner env) => RIO env ()
test = do
  parserRoundtripTest
  regressionTest

parserRoundtripTest :: (HasRunner env) => RIO env ()
parserRoundtripTest = do
  logInfo "Starting Quickcheck tests."
  success <- doAllQuickCheckPropertyTests
  if success
    then logInfo "✅ Passed."
    else do
            logError "❗❗❗ Failed. Quickcheck tests."
            exitWith (SomeTestsFailed ["Quickcheck test failed!"])
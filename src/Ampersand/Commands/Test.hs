{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Generate a prototype from a project.
module Ampersand.Commands.Test
  ( test,
    HasTestOpts (..),
  )
where

import Ampersand.Basics (RIO, logInfo)
import Ampersand.Misc.HasClasses (HasTestOpts (..))
import Ampersand.Test.Parser.QuickChecks
  ( doAllQuickCheckPropertyTests,
  )
import Ampersand.Test.Regression (regressionTest)
import Ampersand.Types.Config (HasRunner)

test :: (HasTestOpts env, HasRunner env) => RIO env ()
test = do
  parserRoundtripTest
  regressionTest

parserRoundtripTest :: (HasRunner env) => RIO env ()
parserRoundtripTest = do
  logInfo "Starting Quickcheck tests."
  doAllQuickCheckPropertyTests
  logInfo "Roundtrip checks are OK"
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
import Ampersand.Test.Incremental.CandidateProperties (doAllCandidatePropertyTests)
import Ampersand.Test.Incremental.CostProfileTests (doAllCostProfileTests)
import Ampersand.Test.Incremental.Properties (doAllIncrementalPropertyTests)
import Ampersand.Test.MultiContext.QualifyProperties (doAllQualifyPropertyTests)
import Ampersand.Test.Parser.QuickChecks
import Ampersand.Test.Regression (regressionTest)
import Ampersand.Test.Step.StepParserTest (stepParserTest)
import Ampersand.Types.Config (HasRunner)

test :: (HasTestOpts env, HasRunner env) => RIO env ()
test = do
  parserRoundtripTest
  incrementalPropertyTest
  candidatePropertyTest
  costProfileTest
  qualifyPropertyTest
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

-- | The code-to-model bridge of the incremental evaluator (issue #1683):
--   QuickCheck properties binding the ZSet functions and the engine to the
--   lemmas of proofs/incremental/.
incrementalPropertyTest :: (HasRunner env) => RIO env ()
incrementalPropertyTest = do
  logInfo "Starting incremental-evaluator property tests."
  success <- doAllIncrementalPropertyTests
  if success
    then logInfo "✅ Passed."
    else do
      logError "❗❗❗ Failed. Incremental-evaluator property tests."
      exitWith (SomeTestsFailed ["Incremental-evaluator property test failed!"])

-- | The code-to-model bridge of the candidate calculus (issue #1684):
--   QuickCheck properties binding widen\/narrow\/candidateTerms to the
--   K-lemmas of proofs\/incremental\/Candidates.thy (claim PRF-7).
candidatePropertyTest :: (HasRunner env) => RIO env ()
candidatePropertyTest = do
  logInfo "Starting candidate-calculus property tests."
  success <- doAllCandidatePropertyTests
  if success
    then logInfo "✅ Passed."
    else do
      logError "❗❗❗ Failed. Candidate-calculus property tests."
      exitWith (SomeTestsFailed ["Candidate-calculus property test failed!"])

-- | The route classification of the cost gate (issue #1692): every DC-17
--   route class on a miniature context, plus the structural counterexample.
costProfileTest :: (HasRunner env) => RIO env ()
costProfileTest = do
  logInfo "Starting cost-profile classification tests."
  success <- doAllCostProfileTests
  if success
    then logInfo "\x2705 Passed."
    else do
      logError "\x2757\x2757\x2757 Failed. Cost-profile classification tests."
      exitWith (SomeTestsFailed ["Cost-profile classification test failed!"])

-- | Joining the contexts of a system: an included context contributes its names with a prefix,
--   and a context that is reached along two paths contributes them once.
qualifyPropertyTest :: (HasRunner env) => RIO env ()
qualifyPropertyTest = do
  logInfo "Starting property tests of systems of contexts (CONTEXT ... INCLUDES ...)."
  success <- doAllQualifyPropertyTests
  if success
    then logInfo "\x2705 Passed."
    else do
      logError "\x2757\x2757\x2757 Failed. Property tests of systems of contexts."
      exitWith (SomeTestsFailed ["Property test of systems of contexts failed!"])

parserRoundtripTest :: (HasRunner env) => RIO env ()
parserRoundtripTest = do
  logInfo "Starting Quickcheck tests."
  success <- doAllQuickCheckPropertyTests
  if success
    then logInfo "✅ Passed."
    else do
      logError "❗❗❗ Failed. Quickcheck tests."
      exitWith (SomeTestsFailed ["Quickcheck test failed!"])

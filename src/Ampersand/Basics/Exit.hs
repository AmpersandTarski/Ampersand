{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains a datastructure for exceptions that
--   can be thrown by Ampersand.
module Ampersand.Basics.Exit
  ( exitWith,
    AmpersandExit (..),
  )
where

import Control.Exception hiding (catch)
-- import           Ampersand.Basics.Prelude
import RIO hiding (exitWith, zipWith)
import qualified RIO.Text as T
import qualified System.Exit as SE
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE exitWith #-}

-- | Computation 'exitWith' @code@ throws 'AmpersandExit' @code@.
-- Normally this terminates the program, returning @code@ to the
-- program's caller.
exitWith :: AmpersandExit -> a
exitWith x = unsafePerformIO $ do
  runSimpleApp (mapM_ (logInfo . display) message)
  SE.exitWith exitcode
  where
    (exitcode, message) = info x

-- | Datastructure that contains all kind of exitcodes that
--   are specific for Ampersand
data AmpersandExit
  = -- | An attempt to install the prototype framework failed.
    FailedToInstallPrototypeFramework [Text]
  | -- | An attempt to generate prototype backend failed.
    FailedToGeneratePrototypeBackend [Text]
  | -- | These specific errors are due to some bug in the Ampersand code. Please report such bugs!
    Fatal [Text]
  | -- | An attempt to run some SQL code failed.
    InvalidSQLExpression [Text]
  | -- | There are violations of one or more invariants
    InvariantRuleViolated [Text]
  | -- | There is no configuration file.
    NoConfigurationFile [Text]
  | -- | While running the daemon, somehow the list of files to watch is empty.
    NoFilesToWatch
  | -- | Thrown whenever a ctxError occurs.
    NoValidFSpec [Text]
  | -- | An attempt to run some PHP code failed.
    PHPExecutionFailed [Text]
  | -- | The user is unclear about what chapters she likes in the generated document
    PosAndNegChaptersSpecified [Text]
  | -- | An attempt to read a file failed.
    ReadFileError [Text]
  | -- | Running some test yealded failed tests.
    SomeTestsFailed [Text]
  | -- | The population in the script is not the same as the population in the generated database
    ViolationsInDatabase [(Text, [Text])]
  | -- | Graphviz is not properly installed.
    GraphVizNotInstalled

instance Exception AmpersandExit

instance Show AmpersandExit where
  show x =
    T.unpack $
      "["
        <> tshow exitcode
        <> "] "
        <> T.concat (fmap ("    " <>) message)
    where
      (exitcode, message) = info x

info :: AmpersandExit -> (SE.ExitCode, [Text])
info x =
  case x of
    Fatal msg -> (SE.ExitFailure 2, msg)
    NoValidFSpec msg ->
      ( SE.ExitFailure 10,
        if null msg
          then ["ERROR Something is wrong with your script. See https://github.com/AmpersandTarski/Ampersand/issues/751"]
          else msg
      )
    ViolationsInDatabase viols ->
      (SE.ExitFailure 20, "ERROR: The population would violate invariants. Could not generate your database." : concatMap showViolatedRule viols)
    InvalidSQLExpression msg ->
      (SE.ExitFailure 30, "ERROR: Invalid SQL Expression" : map ("  " <>) msg)
    InvariantRuleViolated msg ->
      (SE.ExitFailure 40, "ERROR: No prototype generated because of rule violations." : map ("  " <>) msg)
    -- Exit code 50 can be reused. Formerly used for FailedToInstallComposer (php dependencies for prototype framework)
    PHPExecutionFailed msg ->
      (SE.ExitFailure 60, msg)
    FailedToInstallPrototypeFramework msg ->
      (SE.ExitFailure 80, msg)
    FailedToGeneratePrototypeBackend msg ->
      (SE.ExitFailure 81, msg)
    NoFilesToWatch ->
      (SE.ExitFailure 100, ["ERROR: No files loaded, nothing to wait for. Fix the last error and restart."])
    NoConfigurationFile msg ->
      (SE.ExitFailure 110, msg)
    SomeTestsFailed msg ->
      (SE.ExitFailure 120, msg)
    ReadFileError msg ->
      (SE.ExitFailure 130, msg) -- Same magic number as used in Setup.hs
    PosAndNegChaptersSpecified msg ->
      (SE.ExitFailure 150, msg)
    GraphVizNotInstalled ->
      ( SE.ExitFailure 160,
        [ "ERROR: Graphviz is not properly installed. Graphviz is required to generated images.",
          "   Please make sure you have Graphviz installed. See http://www.graphviz.org/ for instructions."
        ]
      )
  where
    showViolatedRule :: (Text, [Text]) -> [Text]
    showViolatedRule (rule, pairs) =
      [ "Rule: " <> rule,
        "   violations: " <> T.intercalate ", " pairs
      ]

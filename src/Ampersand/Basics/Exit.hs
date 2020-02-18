{-# LANGUAGE ScopedTypeVariables #-}
-- | This module contains a datastructure for exceptions that
--   can be thrown by Ampersand.
module Ampersand.Basics.Exit 
         ( exitWith
         , AmpersandExit(..)
         ) where

import           Control.Exception hiding (catch)
import           Ampersand.Basics.Prelude
import qualified RIO.List as L
import qualified RIO.Text as T
import qualified System.Exit as SE
import           System.IO.Unsafe(unsafePerformIO)

{-# NOINLINE exitWith #-}
-- | Computation 'exitWith' @code@ throws 'AmpersandExit' @code@.
-- Normally this terminates the program, returning @code@ to the
-- program's caller.
exitWith :: AmpersandExit -> a
exitWith x = unsafePerformIO $ do
  runSimpleApp (mapM_ (logInfo . display . T.pack) message)
  SE.exitWith exitcode
 where (exitcode,message) = info x

-- | Datastructure that contains all kind of exitcodes that
--   are specific for Ampersand
data AmpersandExit 
  = FailedToInstallComposer [String]
  -- ^ An attempt to install Composer failed.
  | FailedToInstallPrototypeFramework [String]
  -- ^ An attempt to install the prototype framework failed.
  | Fatal [String]
  -- ^ These specific errors are due to some bug in the Ampersand code. Please report such bugs!
  | InvalidSQLExpression [String]
  -- ^ An attempt to run some SQL code failed.
  | InvariantRuleViolated [String]
  -- ^ There are violations of one or more invariants
  | NoConfigurationFile [String]
  -- ^ There is no configuration file.
  | NoFilesToWatch
  -- ^ While running the daemon, somehow the list of files to watch is empty.
  | NoValidFSpec [String]
  -- ^ Thrown whenever a ctxError occurs.
  | PHPExecutionFailed [String]
  -- ^ An attempt to run some PHP code failed.
  | PosAndNegChaptersSpecified [String]
  -- ^ The user is unclear about what chapters she likes in the generated document
  | ReadFileError [String]
  -- ^ An attempt to read a file failed.
  | SomeTestsFailed [String]
  -- ^ Running some test yealded failed tests.
  | ViolationsInDatabase [(String,[String])]
  -- ^ The population in the script is not the same as the population in the generated database

instance Exception AmpersandExit

instance Show AmpersandExit where
  show x = "["++show exitcode++"] "
         ++concatMap ("    "++) message
     where (exitcode, message) = info x

info :: AmpersandExit -> (SE.ExitCode, [String])
info x = 
  case x of
    Fatal msg -> (SE.ExitFailure   2 , msg) 
    NoValidFSpec msg
              -> (SE.ExitFailure  10 , case msg of
                                         [] -> ["ERROR Something is wrong with your script. See https://github.com/AmpersandTarski/Ampersand/issues/751"]
                                         _  -> msg
                 ) 
    ViolationsInDatabase viols
              -> (SE.ExitFailure  20 , "ERROR: The population would violate invariants. Could not generate your database." : concatMap showViolatedRule viols)
    InvalidSQLExpression msg
              -> (SE.ExitFailure  30 , "ERROR: Invalid SQL Expression" : map ("  "++) msg)
    InvariantRuleViolated msg
              -> (SE.ExitFailure  40 , "ERROR: No prototype generated because of rule violations." : map ("  "++) msg)
    FailedToInstallComposer msg
              -> (SE.ExitFailure  50 , msg)
    PHPExecutionFailed msg
              -> (SE.ExitFailure  60 , msg)
    FailedToInstallPrototypeFramework msg
              -> (SE.ExitFailure  80 , msg)
    NoFilesToWatch
              -> (SE.ExitFailure 100 , ["ERROR: No files loaded, nothing to wait for. Fix the last error and restart."])
    NoConfigurationFile msg
              -> (SE.ExitFailure 110 , msg)
    SomeTestsFailed msg
              -> (SE.ExitFailure 120 , msg)
    ReadFileError msg
              -> (SE.ExitFailure 130 , msg)
    PosAndNegChaptersSpecified msg 
              -> (SE.ExitFailure 150 , msg)
  where
    showViolatedRule :: (String,[String]) -> [String]
    showViolatedRule (rule,pairs) = 
         [ "Rule: "++rule
         , "   violations: "++L.intercalate ", " pairs
         ]

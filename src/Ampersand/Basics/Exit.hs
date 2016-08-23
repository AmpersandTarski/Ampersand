{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.Basics.Exit 
         ( exitWith
         , AmpersandExit(..)
         ) where

import qualified System.Exit as SE
import System.IO.Unsafe

{-# NOINLINE exitWith #-}
exitWith :: AmpersandExit -> a
exitWith x = unsafePerformIO $ do
  exitIO message
  SE.exitWith exitcode
 where (exitcode,message) = info x

exitIO :: [String] -> IO()
exitIO = mapM_ putStrLn

data AmpersandExit 
  = Succes
  | Fatal [String]
  | NoValidFSpec [String]
  | ViolationsInDatabase
  | InvalidSQLExpression
  | NoPrototypeBecauseOfRuleViolations
  | FailedToInstallComposer [String]
  | PHPExecutionFailed [String]

info :: AmpersandExit -> (SE.ExitCode, [String])
info x = 
  case x of
    Succes    -> (SE.ExitSuccess     , [])
    Fatal msg -> (SE.ExitFailure   1 , msg) -- These specific errors are due to some bug in the Ampersand code. Please report such bugs!
    NoValidFSpec msg
              -> (SE.ExitFailure  10 , msg) 
    ViolationsInDatabase
              -> (SE.ExitFailure  10 , ["ERROR: The population would violate invariants. Could not generate your database."])
    InvalidSQLExpression
              -> (SE.ExitFailure  30 , ["ERROR: Invalid SQL Expression"])
    NoPrototypeBecauseOfRuleViolations
              -> (SE.ExitFailure  40 , ["ERROR: No prototype generated because of rule violations."])
    FailedToInstallComposer msg
              -> (SE.ExitFailure  50 , msg)
    PHPExecutionFailed msg
              -> (SE.ExitFailure  60 , msg)


{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.Basics.Exit 
         ( exitWith
         , AmpersandExit(..)
         ) where

import           Control.Exception hiding (catch)
import           Ampersand.Basics.Prelude
import           Data.List
import qualified System.Exit as SE
import           System.IO.Unsafe(unsafePerformIO)

{-# NOINLINE exitWith #-}
exitWith :: AmpersandExit -> a
exitWith x = unsafePerformIO $ do
  mapM_ putStrLn message
  SE.exitWith exitcode
 where (exitcode,message) = info x

data AmpersandExit 
  = --Succes [String]
 -- | 
    Fatal [String]
  | NoValidFSpec [String]
  | ViolationsInDatabase [(String,[String])]
  | InvalidSQLExpression [String]
  | NoPrototypeBecauseOfRuleViolations
  | FailedToInstallComposer [String]
  | PHPExecutionFailed [String]
  | WrongArgumentsGiven [String]
  | FailedToInstallPrototypeFramework [String]
  | NoAmpersandScript [String]
  | NoFilesToWatch
  | NoConfigurationFile [String]

instance Exception AmpersandExit

instance Show AmpersandExit where
  show x = "["++show code++"] "
         ++concatMap ("    "++) msg
     where (code, msg) = info x
    
info :: AmpersandExit -> (SE.ExitCode, [String])
info x = 
  case x of
  --  Succes msg -> (SE.ExitSuccess    , msg)
    Fatal msg -> (SE.ExitFailure   2 , msg) -- These specific errors are due to some bug in the Ampersand code. Please report such bugs!
    NoValidFSpec msg
              -> (SE.ExitFailure  10 , case msg of
                                         [] -> ["ERROR Something is wrong with your script. See https://github.com/AmpersandTarski/Ampersand/issues/751"]
                                         _  -> msg
                 ) 
    ViolationsInDatabase viols
              -> (SE.ExitFailure  20 , "ERROR: The population would violate invariants. Could not generate your database." : concatMap showViolatedRule viols)
    InvalidSQLExpression msg
              -> (SE.ExitFailure  30 , "ERROR: Invalid SQL Expression" : map ("  "++) msg)
    NoPrototypeBecauseOfRuleViolations
              -> (SE.ExitFailure  40 , ["ERROR: No prototype generated because of rule violations."])
    FailedToInstallComposer msg
              -> (SE.ExitFailure  50 , msg)
    PHPExecutionFailed msg
              -> (SE.ExitFailure  60 , msg)
    WrongArgumentsGiven msg 
              -> (SE.ExitFailure  70 , msg)
    FailedToInstallPrototypeFramework msg
              -> (SE.ExitFailure  80 , msg)
    NoAmpersandScript msg
              -> (SE.ExitFailure  90 , msg)
    NoFilesToWatch
              -> (SE.ExitFailure 100 , ["ERROR: No files loaded, nothing to wait for. Fix the last error and restart."])
    NoConfigurationFile msg
              -> (SE.ExitFailure 110 , msg)
  where
    showViolatedRule :: (String,[String]) -> [String]
    showViolatedRule (rule,pairs) = 
         [ "Rule: "++rule
         , "   violations: "++intercalate ", " pairs
         ]

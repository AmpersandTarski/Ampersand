{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.Basics.Exit 
         ( exitWith
         , AmpersandExit(..)
         ) where

import           Ampersand.Basics.Prelude
import           Ampersand.Basics.UTF8
import           Data.List
import qualified System.Exit as SE
import           System.IO.Unsafe(unsafePerformIO)

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
  | ViolationsInDatabase [(String,[String])]
  | InvalidSQLExpression [String]
  | NoPrototypeBecauseOfRuleViolations
  | FailedToInstallComposer [String]
  | PHPExecutionFailed [String]
  | WrongArgumentsGiven [String]
  | FailedToInstallPrototypeFramework [String]

info :: AmpersandExit -> (SE.ExitCode, [String])
info x = 
  case x of
    Succes    -> (SE.ExitSuccess     , [])
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
  where
    showViolatedRule :: (String,[String]) -> [String]
    showViolatedRule (rule,pairs) = 
         [ "Rule: "++rule
         , "   violations: "++intercalate ", " pairs
         ]

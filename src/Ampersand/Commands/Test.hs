{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generate a prototype from a project.
module Ampersand.Commands.Test
    (test
    ,HasTestOpts(..)
    ) where

import           Ampersand.Basics
import           Ampersand.Misc
import           Ampersand.Types.Config
import           Ampersand.Test.Parser.QuickChecks
import           Ampersand.Test.Regression

test :: (HasTestOpts env, HasRunner env) => RIO env ()
test = do
  parserRoundtripTest
  regressionTest


parserRoundtripTest :: (HasRunner env) => RIO env ()
parserRoundtripTest = do 
    sayLn "Starting Quickcheck tests."
    funcs <- testFunctions
 --   testAmpersandScripts
    tests funcs
  where 
      tests :: (HasLogFunc env) => [([String], RIO env Bool)] -> RIO env ()
      tests [] = pure ()
      tests ((msg,tst):xs) = do
          mapM_ sayLn msg
          success <- tst
          if success then tests xs
          else exitWith (SomeTestsFailed ["*** Some tests failed***"])
      testFunctions :: RIO env [([String], RIO env Bool)]
      testFunctions = do
          (parserCheckResult, msg) <- parserQuickChecks
          return [ ( if parserCheckResult  
                     then ["Parser & prettyprinter test PASSED."]
                     else (  ["QuickCheck found errors in the roundtrip in parsing/prettyprinting for the following case:"]
                           ++map ("\n   "++) (lines msg)
                          )
                   , return parserCheckResult
                   )
                 ]

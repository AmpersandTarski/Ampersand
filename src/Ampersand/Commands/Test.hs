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

-- regressionTestOLD :: (HasTestOpts env, HasRunner env) => RIO env ()
-- regressionTestOLD = do 
--     testOpts <- view testOptsL
--     sayLn $ "Starting regression test."
--     baseDir <- liftIO . makeAbsolute $ rootTestDir $ testOpts
--     totalfails <- runConduit $ walk 1 baseDir .| doTestsInDir .| sumarize
--     if totalfails == 0
--     then sayLn $ "Regression test of all scripts succeeded."
--     else exitWith (SomeTestsFailed ["Regression test failed! ("++show totalfails++" tests failed.)"])
--   where   

--     -- Produces directory data. Recursively walks over the
--     -- directory tree and yields one Dirdata at a time.
--     walk :: Int -> FilePath -> ConduitT () DirData (RIO env) ()
--     walk nr path = do 
--         result <- liftIO $ tryIOError (liftIO listdir)
--         case result of
--           Right dl
--               -> case dl of 
--                    DirList subdirs _
--                     -> do
--                         yield (DirData path dl)
--                         forM_ subdirs (walk . (path </>))
--                    DirError err 
--                     -> yield (DirData path (DirError err))
--           Left err
--               -> yield (DirData path (DirError err))

                
--     -- Processes a DirData, runs the tests in that directory and 
--     -- returns the number of failed tests as an Int.
--     doTestsInDir :: (HasLogFunc env) => ConduitT DirData Int (RIO env) ()
--     doTestsInDir = loop 1
--       where
--         loop :: (HasLogFunc env) => Int -> ConduitT DirData Int (RIO env) ()
--         loop testSet = awaitForever $
--             (\dird -> do 
--                 lift $ sayLn $ ">> " ++ show testSet ++ ". "
--                 doSingleDirTests testSet dird     
--                 loop (testSet + 1)
--             ) 

--     sumarize :: ConduitT Int Void (RIO env) Int
--     sumarize = loop 0 
--       where
--         loop :: Int -> ConduitT Int Void (RIO env) Int
--         loop i = 
--           await >>= maybe (return i) 
--                           (\x -> loop $! (i+x))

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

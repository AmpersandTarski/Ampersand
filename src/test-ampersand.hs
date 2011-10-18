{-# OPTIONS_GHC -Wall #-}
module Main where

import Test.Framework -- (defaultMain, testGroup,Test(..))
--import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.QuickCheck
import Test.HUnit
import Prelude hiding (putStr,readFile,writeFile)
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Misc -- hiding (test)
import DatabaseDesign.Ampersand.Components
import DatabaseDesign.Ampersand.Basics
import Paths_Ampersand
import System.FilePath

fatal :: Int -> String -> a
fatal = fatalMsg "test-ampersand"



tests :: [Test.Framework.Test]
tests = [testGroup "Dummy tests"
             [testCase "aap" dummyTest]
        ,testGroup "Files that should be OK."
--             (fmap (testAmpersandScript  defaultFlags True) [])     
 --              [testCase "EenFile" (testAmpersandScript defaultFlags True (getDataDir </> "pass" </> "Aanvraag.adl"))
              [ ]
        ,testGroup "HUnit" (hUnitTestToTests test2) 
        ]
dummyTest :: Assertion
dummyTest = sort[8,7,4,5,3] @?= [3,4,5,7,8]

test2 =         "OneFile" ~: do dataDir <- getDataDir 
                                scriptText <- readFile (dataDir </> scriptName)
                                verboseLn flags $ "Found file : "++ scriptName
                                pCtx <- parseCtxM_ scriptText flags scriptName
                                assertEqual "Cannot parse this file" (parsedOk pCtx) shouldPass
                             
           where 
            scriptName = "testData" </> "pass" </> "Aanvraag.adl"
            flags = defaultFlags
            shouldPass = True
            parsedOk (Left _ ) = True
            parsedOk (Right _) = False




main :: IO ()
main = defaultMain tests

-- Test insParentheses. It should insert parentheses such that the parser would return the same expression.
--test :: [Bool]
--test =  
--       [ insParentheses (EEqu (EImp (t,t),t))                      == EEqu (EBrk (EImp (t,t)),t)                     
--       , insParentheses (EImp (EEqu (t,t),t))                      == EImp (EBrk (EEqu (t,t)),t)                     
--       , insParentheses (EImp (EImp (t,t),t))                      == EImp (EBrk (EImp (t,t)),t)                     
--       , insParentheses (EIsc [EUni [t,t], EIsc [t,t], EIsc [t,t], t]) == EIsc [EBrk (EUni [t,t]), EBrk (EIsc [t,t]), EBrk (EIsc [t,t]), t]
--       ]
--   where t = ERel (I ONE)
   
-- | monad to test if an ampersand script can be parsed and typechecked
testAmpersandScript :: Options   -- ^ flags to use during testing of the script
                    -> Bool      -- ^ Do we expect that the script is OK (e.g. should it pass)? 
                    -> FilePath  -- ^ path to the file to be tested
                    -> IO Assertion   -- ^ The monad reads the file, does parsing and type checking
                                 -- (maybe do some logging?) and returns True iff the script 
                                 -- is handled as expected.
testAmpersandScript flags shouldPass scriptName = 
       do scriptText <- readFile scriptName
          verboseLn flags $ "Found file : "++ scriptName
          verboseLn flags  $"Start parsing...." ++ "(NOT!)"
          pCtx <- parseCtxM_ scriptText flags scriptName
          return (parsedOk pCtx @?= shouldPass)
     where
       parsedOk (Left _ ) = True
       parsedOk (Right _) = False
--          verboseLn flags "Type checking..."
 --         return (case pCtx of
 --            Left ctx   -> typeCheck ctx pPops
 --            Right msgs -> (fatal 38 "There are errors that should have been presented!",PE msgs)
 --                )   
   

 
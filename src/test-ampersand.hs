{-# OPTIONS_GHC -Wall #-}

module Main where

import Test.Framework
import DatabaseDesign.Ampersand.ADL1
--import DatabaseDesign.Ampersand.Fspec.ShowADL

tests :: [Test]
tests = [ ]

main :: IO ()
main = defaultMain tests

-- Test insParentheses. It should insert parentheses such that the parser would return the same expression.
test :: [Bool]
test =  
       [ insParentheses (EEqu (EImp (t,t),t))                      == EEqu (EBrk (EImp (t,t)),t)                     
       , insParentheses (EImp (EEqu (t,t),t))                      == EImp (EBrk (EEqu (t,t)),t)                     
       , insParentheses (EImp (EImp (t,t),t))                      == EImp (EBrk (EImp (t,t)),t)                     
       , insParentheses (EIsc [EUni [t,t], EIsc [t,t], EIsc [t,t], t]) == EIsc [EBrk (EUni [t,t]), EBrk (EIsc [t,t]), EBrk (EIsc [t,t]), t]
       ]
   where t = ERel (I ONE)
   
-- | monad to test if an ampersand script can be parsed and typechecked
testScript :: FilePath  -- ^ path to the file to be tested
           -> Bool      -- ^ Do we expect that the script is OK (e.g. should it pass)? 
           -> IO Bool   -- ^ The monad reads the file, does parsing and type checking
                        -- (maybe do some logging?) and returns True iff the script 
                        -- is handled as expected.
testScript fn shouldPass = return False   -- TODO: make the parser return an error message, instead of just failing. 
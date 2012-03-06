{-# OPTIONS_GHC -Wall #-}
module Main where

import Test.Framework -- (defaultMain, testGroup,Test(..))
import Test.Framework.Providers.HUnit
import Test.HUnit
import Prelude hiding (putStr,readFile,writeFile,putStrLn)
import DatabaseDesign.Ampersand.Misc -- hiding (test)
import DatabaseDesign.Ampersand.Components
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Input.ADL1.CtxError
import Paths_ampersand
import System.FilePath
import Directory

fatal :: Int -> String -> a
fatal = fatalMsg "test-ampersand"

main :: IO ()
main = do
  tests <- testSet
  defaultMain (testADLFiles defaultFlags tests)

testADLFiles :: Options -> [( FilePath , Bool )] -> [Test.Framework.Test]
testADLFiles flags xs 
        = (if (not . null) passFiles
           then [ testGroup "Files that should pass parsing and typechecking..."
                    (map (testSingleFile True) passFiles)
                ]
           else []
          )++ 
          (if (not . null) failFiles
           then [ testGroup "Files that should fail parsing or typechecking..."
                    (map (testSingleFile False) failFiles)
                ]
           else []
          )
   where
     passFiles = fst ( unzip (filter (f True ) xs))
     failFiles = fst ( unzip (filter (f False) xs))
     f :: Bool -> (FilePath,Bool) -> Bool
     f x (_,y) = x == y
     testSingleFile :: Bool -> FilePath -> Test.Framework.Test
     testSingleFile b fp = testCase (takeFileName fp) (testAmpersandScript b fp)

     -- | monad to test if an ampersand script can be parsed and typechecked
     testAmpersandScript :: Bool      -- ^ Do we expect that the script is OK (e.g. should it pass)? 
                         -> FilePath  -- ^ path to the file to be tested
                         -> Assertion -- ^ The monad reads the file, does parsing and type checking
                                      -- (maybe do some logging?) and returns True iff the script 
                                      -- is handled as expected.
     testAmpersandScript shouldPass scriptName = 
         do pCtx <- parseCtxM_ flags scriptName
            parsedAndTypesOk (check pCtx) @?= shouldPass
       where
         check pCtx = case pCtx of
                       Right ctx -> typeCheck ctx []
                       Left msg  -> (fatal 38 "There are errors that should have been presented!",PE [msg])
         parsedAndTypesOk (_,errs) = errs == CxeNone
                     
   
testSet :: IO [( FilePath , Bool )]
testSet = do 
   dataDir <- getDataDir
 --  verboseLn flags dataDir
   passFiles <- myGetDirectoryContents (dataDir </> "testData" </> "pass")
   failFiles <- myGetDirectoryContents (dataDir </> "testData" </> "fail")
   return (zip passFiles (repeat True) ++ zip failFiles (repeat False))
  where
   myGetDirectoryContents :: FilePath -> IO [FilePath]
   myGetDirectoryContents fp = do
        files <- getDirectoryContents fp
        return (map (fp </>) (filter nonDot files))
     where
       nonDot []      = False
       nonDot ('.':_) = False
       nonDot _       = True  


-- Test insParentheses. It should insert parentheses such that the parser would return the same expression.
--test :: [Bool]
--test =  
--       [ insParentheses (EEqu (EImp (t,t),t))                      == EEqu (EBrk (EImp (t,t)),t)                     
--       , insParentheses (EImp (EEqu (t,t),t))                      == EImp (EBrk (EEqu (t,t)),t)                     
--       , insParentheses (EImp (EImp (t,t),t))                      == EImp (EBrk (EImp (t,t)),t)                     
--       , insParentheses (EIsc [EUni [t,t], EIsc [t,t], EIsc [t,t], t]) == EIsc [EBrk (EUni [t,t]), EBrk (EIsc [t,t]), EBrk (EIsc [t,t]), t]
--       ]
--   where t = ERel (I ONE)
   

 
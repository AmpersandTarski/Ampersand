{-# LANGUAGE DeriveGeneric #-}
module Database.Design.Ampersand.Test.Regression 
  ( DirContent(..)
  , DirData(..)
  , process
  )
where 
import Conduit
import qualified Data.Conduit.List as CL
--import qualified Data.Conduit.Binary as CB

import Data.Char
import System.FilePath ((</>),takeExtension)
--import Control.Monad --(filterM, forM_, foldM,when)
--import System.IO.Error (tryIOError)
--import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
--import Control.Monad.Trans.Class (lift)
--import Data.Conduit
--import System.Exit --(ExitCode, exitFailure, exitSuccess)
import System.Process

import Data.Yaml
import GHC.Generics

data DirContent = DirList [FilePath] [FilePath]  -- files and directories in a directory
                | DirError IOError               
data DirData = DirData FilePath DirContent       -- path and content of a directory
--data DirInfo = DirInfo FilePath [FilePath] TestInfo       -- list of testscripts and information on how to test them

-- This data structure is directy available in .yaml files. Be aware that modification will have consequences for the 
-- yaml files in the test suite.
data TestInfo = TestInfo 
   { command  :: String
   , shouldSucceed :: Bool 
   }deriving Generic
instance FromJSON TestInfo


-- | process does the tests for a specific DirData. Currently, 
--   only the amount of failed tests is returned. 
process :: Int -> DirData -> IO Int 
process indent (DirData path dirContent) =
  case dirContent of
    DirError err     -> do
        putStrLn $ "I've tried to look in " ++ path ++ "."
        putStrLn $ "    There was an error: "
        putStrLn $ "       " ++ show err
        return 1
    DirList dirs files -> do
        putStrLn $ path ++" : "
        doTestSet indent path files
 
yaml = "testinfo.yaml"  -- the required name of the file that contains the test info for this directory.
doTestSet :: Int -> FilePath -> [FilePath] -> IO Int
doTestSet indent dir fs 
  | yaml `elem` fs = 
       do res <- parseYaml
          case res of 
              Left err -> do putStrLni $ dir </> yaml ++" could not be parsed."
                             putStrLni $ prettyPrintParseException err
                             return 1
              Right ti -> do results <- runTests ti $$ getResults
                             putStrLni $ "results: "++show results
                             return results
  | otherwise =
       do putStrLni $ "Nothing to do. ("++yaml++" not present)"
          return 0

  where
    parseYaml ::  IO (Either ParseException TestInfo) 
    parseYaml = decodeFileEither $ dir </> yaml
    runTests :: TestInfo -> Source IO Int
    runTests ti = testsSource ti =$= doATest
      where 
        isRelevant f = map toUpper (takeExtension f) `elem` [".ADL"]
        testsSource :: TestInfo -> Source IO (TestInfo, FilePath, FilePath)
        testsSource ti = CL.sourceList $ 
              zip3 (repeat ti) (repeat dir) (filter isRelevant fs)
    doATest :: Conduit (TestInfo, FilePath, FilePath) IO Int
    doATest = awaitForever dotheTest
      where 
         dotheTest (ti,dir,file) = 
            do res <- liftIO $ testAdlfile (indent + 2) dir file ti
               if res then yield 0 else yield 1 
    getResults :: Sink Int IO Int
    getResults = loop 0 
     where
       loop :: Int -> Sink Int IO Int
       loop i = 
         await >>= maybe (return i) 
                         (\x -> loop $! (i+x))
    putStrLni str = putStrLn $ (replicate indent ' ') ++ str
    
testAdlfile :: Int       -- Number of spaces to indent (for output during testing)
             -> FilePath -- the filepath of the directory where the test should be done
             -> FilePath -- the script that is undergoing the test
             -> TestInfo --The testinfo, so it is known how to test the script
             -> IO Bool  -- Indicator telling if the test passed or not
testAdlfile indent path adl tinfo = 
    do putStrLni $ "<fakeing test of `"++path</>adl++"`>"
       return (True) 
   where
     putStrLni str = putStrLn $ (replicate indent ' ') ++ str 
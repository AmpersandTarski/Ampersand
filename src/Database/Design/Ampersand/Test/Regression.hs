{-# LANGUAGE DeriveGeneric #-}
module Database.Design.Ampersand.Test.Regression 
  ( DirContent(..)
  , DirData(..)
  , process
  )
where 
import Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB

import System.FilePath ((</>))
import Control.Monad --(filterM, forM_, foldM,when)
import System.IO.Error (tryIOError)
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import Control.Monad.Trans.Class (lift)
import Data.Conduit
import System.Exit --(ExitCode, exitFailure, exitSuccess)

import Data.Yaml
import GHC.Generics

data DirContent = DirList [FilePath] [FilePath]  -- files and directories in a directory
                | DirError IOError               
data DirData = DirData FilePath DirContent       -- path and content of a directory
data DirInfo = DirInfo FilePath [FilePath] TestInfo       -- list of testscripts and information on how to test them

-- This data structure is directy available in .yaml files. Be aware that modification will have consequences for the 
-- yaml files in the test suite.
data TestInfo = TestInfo 
   { command  :: String
   , shouldSucceed :: Bool 
   }deriving Generic
data ExpectedResult = Pass | Fail
   deriving (Generic, Show)
instance FromJSON TestInfo
instance FromJSON ExpectedResult


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
 

doTestSet :: Int  -- the number of the testset during the traversal.
          -> FilePath -- the filepath of the directory of the testset
          -> [FilePath] -- the filepaths in the directory
          -> IO Int  -- the number of failed tests.
doTestSet indent dir fs 
  | yaml `elem` fs = 
       do putStrLni $ "yaml file present."
          parseAndShowYaml
  | otherwise =
       do putStrLni $ "Nothing to do. ("++yaml++" not present)"
          return 0

  where
    putStrLni str = putStrLn $ (replicate indent ' ')++str
    yaml = "testinfo.yaml"  -- the required name of the file that contains the test info for this directory.
    parseAndShowYaml :: IO Int
    parseAndShowYaml = do
        mt <- decodeFileEither $ dir </> yaml
        case mt of 
            Left err    -> do putStrLni $ dir </> yaml ++" could not be parsed."
                              putStrLni $ prettyPrintParseException err
                              return 1
            Right tInfo -> do putStrLni $ "command      : "++command tInfo 
                              putStrLni $ "shouldSucceed: "++(show . shouldSucceed) tInfo
                              return 0

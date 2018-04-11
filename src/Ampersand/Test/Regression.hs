{-# LANGUAGE DeriveGeneric #-}
module Ampersand.Test.Regression 
  ( DirContent(..)
  , DirData(..)
  , process
  )
where 
import Ampersand.Basics
import Conduit
import qualified Data.Conduit.List as CL
import Data.Char
import System.FilePath ((</>),takeExtension)
import System.Exit --(ExitCode, exitFailure, exitSuccess)
import System.Process

import Data.Yaml
import GHC.Generics

data DirContent = DirList [FilePath] [FilePath]  -- files and directories in a directory
                | DirError IOError               
data DirData = DirData FilePath DirContent       -- path and content of a directory
--data DirInfo = DirInfo FilePath [FilePath] TestInfo       -- list of testscripts and information on how to test them


-- | process does the tests for a specific DirData. Currently, 
--   only the amount of failed tests is returned. 
process :: Int -> DirData -> IO Int 
process indnt (DirData path dirContent) =
  case dirContent of
    DirError err     -> do
        putStrLn $ "I've tried to look in " ++ path ++ "."
        putStrLn   "    There was an error: "
        putStrLn $ "       " ++ show err
        return 1
    DirList _ files -> do
        putStrLn $ path ++" : "
        doTestSet indnt path files
 
yaml :: String
yaml = "testinfo.yaml"  -- the required name of the file that contains the test info for this directory.
doTestSet :: Int -> FilePath -> [FilePath] -> IO Int
doTestSet indnt dir fs 
  | yaml `elem` fs = 
       do res <- parseYaml
          case res of 
              Left err -> do putStrLni $ dir </> yaml ++" could not be parsed."
                             putStrLni $ prettyPrintParseException err
                             return 1
              Right ti -> do putStrLni $ "Command: "++command ti++if shouldSucceed ti then " (should succeed)." else " (should fail)."
                             runConduit $ runTests ti .| getResults
  | otherwise =
       do putStrLni $ "Nothing to do. ("++yaml++" not present)"
          return 0

  where
    parseYaml ::  IO (Either ParseException TestInfo) 
    parseYaml = decodeFileEither $ dir </> yaml
    runTests :: TestInfo -> ConduitM () Int IO ()
    runTests ti = testsSource .| doATest
      where 
        isRelevant f = map toUpper (takeExtension f) `elem` [".ADL"]
        testsSource :: ConduitT () FilePath IO ()
        testsSource = CL.sourceList $ filter isRelevant fs
        doATest :: ConduitT FilePath Int IO ()
        doATest = awaitForever dotheTest
          where 
             dotheTest file = 
                do liftIO $ putStri $ "Start testing of `"++file++"`: "
                   res <- liftIO $ testAdlfile (indnt + 2) dir file ti
                   yield (if res then 0 else 1) 
    getResults :: ConduitT Int Void IO Int
    getResults = loop 0 
     where
       loop :: Int -> ConduitT Int Void IO Int
       loop i = 
         await >>= maybe (return i) 
                         (\x -> loop $! (i+x))
    putStrLni str = putStrLn $ replicate indnt ' ' ++ str
    putStri   str = putStr   $ replicate indnt ' ' ++ str
    

-- This data structure is directy available in .yaml files. Be aware that modification will have consequences for the 
-- yaml files in the test suite.
data TestInfo = TestInfo 
   { command  :: String
   , shouldSucceed :: Bool 
   }deriving Generic
instance FromJSON TestInfo

testAdlfile :: Int       -- Number of spaces to indent (for output during testing)
             -> FilePath -- the filepath of the directory where the test should be done
             -> FilePath -- the script that is undergoing the test
             -> TestInfo --The testinfo, so it is known how to test the script
             -> IO Bool  -- Indicator telling if the test passed or not
testAdlfile indnt path adl tinfo = runMyProc myProc
   where
     myProc :: CreateProcess
     myProc = CreateProcess { cmdspec = ShellCommand (command tinfo ++" "++adl)
                            , cwd = Just path
                            , env = Nothing
                            , std_in = Inherit
                            , std_out = Inherit
                            , std_err = Inherit
                            , close_fds = False
                            , create_group = False
                            , delegate_ctlc = True
                            , detach_console = False
                            , create_new_console = False
                            , new_session = False
                            , child_group = Nothing
                            , child_user = Nothing
                            , use_process_jobs = False
                            }
     runMyProc :: CreateProcess -> IO Bool
     runMyProc x = do 
                     
        (exit_code, out, err) <- readCreateProcessWithExitCode x ""
        case (shouldSucceed tinfo, exit_code) of
          (True  , ExitSuccess  ) -> passOutput
          (True  , ExitFailure _) -> failOutput (exit_code, out, err)
          (False , ExitSuccess  ) -> failOutput (exit_code, out, err)
          (False , ExitFailure _) -> passOutput
     passOutput = do -- putStrLni out
                     putStrLn "***Pass***"
                     return True 
     failOutput (exit_code, out, err) =
                  do putStrLn $ "\n*FAIL*. Exit code: "++show exit_code++". "
                     case exit_code of
                         ExitSuccess -> return()
                         _           -> do putStrLni out
                                           putStrLni err
                     return False

     putStrLni  = mapM_ (putStrLn . (replicate indnt ' ' ++)) . lines 

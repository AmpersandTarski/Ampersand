{-# LANGUAGE DeriveGeneric #-}
module Ampersand.Test.Regression 
  ( DirContent(..)
  , DirData(..)
  , process
  )
where 
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
process indent (DirData path dirContent) =
  case dirContent of
    DirError err     -> do
        putStrLn $ "I've tried to look in " ++ path ++ "."
        putStrLn $ "    There was an error: "
        putStrLn $ "       " ++ show err
        return 1
    DirList _ files -> do
        putStrLn $ path ++" : "
        doTestSet indent path files
 
yaml :: [Char]
yaml = "testinfo.yaml"  -- the required name of the file that contains the test info for this directory.
doTestSet :: Int -> FilePath -> [FilePath] -> IO Int
doTestSet indent dir fs 
  | yaml `elem` fs = 
       do res <- parseYaml
          case res of 
              Left err -> do putStrLni $ dir </> yaml ++" could not be parsed."
                             putStrLni $ prettyPrintParseException err
                             return 1
              Right ti -> runTests ti $$ getResults
  | otherwise =
       do putStrLni $ "Nothing to do. ("++yaml++" not present)"
          return 0

  where
    parseYaml ::  IO (Either ParseException TestInfo) 
    parseYaml = decodeFileEither $ dir </> yaml
    runTests :: TestInfo -> Source IO Int
    runTests ti = testsSource =$= doATest
      where 
        isRelevant f = map toUpper (takeExtension f) `elem` [".ADL"]
        testsSource :: Source IO FilePath
        testsSource = CL.sourceList $ (filter isRelevant fs)
        doATest :: Conduit FilePath IO Int
        doATest = awaitForever dotheTest
          where 
             dotheTest file = 
                do liftIO $ putStri $ "Start testing of `"++file++"`: "
                   res <- liftIO $ testAdlfile (indent + 2) dir file ti
                   yield (if res then 0 else 1) 
    getResults :: Sink Int IO Int
    getResults = loop 0 
     where
       loop :: Int -> Sink Int IO Int
       loop i = 
         await >>= maybe (return i) 
                         (\x -> loop $! (i+x))
    putStrLni str = putStrLn $ (replicate indent ' ') ++ str
    putStri   str = putStr   $ (replicate indent ' ') ++ str
    

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
testAdlfile indent path adl tinfo = runMyProc myProc
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
                      --      , detach_console = False
                      --      , create_new_console = False
                      --      , new_session = False
                      --      , child_group = Nothing
                      --      , child_user = Nothing
                            }
     runMyProc :: CreateProcess -> IO (Bool)
     runMyProc x = do 
                     
        (exit_code, stdout, stderr) <- readCreateProcessWithExitCode x ""
        case (shouldSucceed tinfo, exit_code) of
          (True  , ExitSuccess  ) -> passOutput
          (True  , ExitFailure _) -> failOutput (exit_code, stdout, stderr)
          (False , ExitSuccess  ) -> failOutput (exit_code, stdout, stderr)
          (False , ExitFailure _) -> passOutput
     passOutput = do -- putStrLni stdout
                     putStrLn $ "***Pass***"
                     return True 
     failOutput (exit_code, stdout, stderr) =
                  do putStrLn $ "\n*FAIL*. Exit code: "++show exit_code++". "
                     case exit_code of
                         ExitSuccess -> return()
                         _           -> do putStrLni stdout
                                           putStrLni stderr
                     return False

     putStrLni  = mapM_ putStrLn . map ((replicate indent ' ')  ++) . lines 

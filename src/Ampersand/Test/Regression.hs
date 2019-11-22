{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Test.Regression 
  ( regressionTest
  )
where 
import           Ampersand.Basics
import           Ampersand.Misc
import           Ampersand.Types.Config
import           Conduit
import qualified Data.Conduit.List as CL
import           Data.Yaml
import           RIO.Char
import qualified RIO.Text as T
import           System.Directory
import           System.FilePath
import           System.IO.Error (tryIOError)
import           System.Process

data DirList = 
        DirList 
          { filesOf   :: [FilePath] -- ^ files in the directory
          , subdirsOf :: [FilePath] -- ^ subdirectories
          }
        | DirError IOException               
data DirData = 
        DirData 
          { traversalNr :: Int  -- ^ the sequencenumber in the traversal 
          , path        :: FilePath -- ^ the full path of this directory
          , dirContent  :: DirList -- ^ the result of reading the content in the directory
          }
data TestResults = TestResults
      { successes :: Int
      , failures  :: Int
      }
add :: TestResults -> TestResults -> TestResults
add a b = TestResults 
            { successes = successes a + successes b
            , failures  = failures a + failures b
            }
regressionTest :: (HasTestOpts env, HasRunner env) => RIO env ()
regressionTest = do 
    testOpts <- view testOptsL
    baseDir <- liftIO . makeAbsolute $ rootTestDir $ testOpts
    runConduit $ walkDirTree baseDir .| numberIt .| doTestsInDir .| sumarize
  
walkDirTree :: FilePath -> ConduitT () (Int -> DirData) (RIO env) ()
walkDirTree fp = do
    result <- liftIO $ tryIOError (liftIO listdir)
    case result of
      Right dl
          -> case dl of 
               DirList{}
                -> do
                    yield (foo dl)
                    forM_ (subdirsOf dl) (walkDirTree . (fp </>))
               DirError err 
                -> yield (foo (DirError err))
      Left err
          -> yield (foo (DirError err))
  where
    foo :: DirList -> (Int -> DirData)
    foo dl i = DirData { traversalNr = i
                       , path        = fp
                       , dirContent  = dl
                       }
    listdir = do
        entries <- getDirectoryContents fp >>= filterHidden
        subdirs <- filterM isDir entries
        files <- filterM isFile entries
        return $ DirList { filesOf = files
                         , subdirsOf = subdirs
                         }
        where 
            isFile entry = doesFileExist (fp </> entry)
            isDir entry = doesDirectoryExist (fp </> entry)
            filterHidden paths = return $ filter (not.isHidden) paths
            isHidden ('.':_) = True
            isHidden _       = False

numberIt :: ConduitT (Int -> DirData) DirData (RIO env) ()
numberIt = loop 1
  where
    loop nr = do
      mx <- await
      case mx of
         Nothing -> return ()
         Just f  -> do 
            yield (f nr)
            loop (nr + 1)
             
doTestsInDir :: (HasLogFunc env) => ConduitT DirData TestResults (RIO env) ()
doTestsInDir = awaitForever once 
   where
    once x = do
      lift $ logInfo $ ">> " <> displayShow (traversalNr x) <> ". "
      lift $ logInfo $ " Handling " <> (display . T.pack $ path x) <> ". "
      let candidates = filter isCandidate (filesOf . dirContent $ x)
            where
              isCandidate :: FilePath -> Bool
              isCandidate fp = "adl" `isExtensionOf` fp            
      if yaml `elem` (filesOf . dirContent $ x)
      then do
        res <- lift parseYaml
        case res of 
          Left err -> do 
            lift . sayLn $ indent <> path x </> yaml <>" could not be parsed."
            lift . sayLn $ indent <> prettyPrintParseException err
            yield TestResults
                    { successes = 0
                    , failures  = length candidates
                    }
          Right ti -> do 
            lift . sayLn $ indent <> "Instructions: "
            lift $ mapM_ sayInstruction (testCmds ti)
            yield TestResults
                    { successes = 0
                    , failures  = length candidates
                    }
                         -- <>command ti<>if shouldSucceed ti then " (should succeed)." else " (should fail)."
                         --   runConduit $ runTests ti .| getResults
      else do
        lift . logInfo $ indent<>"Nothing to do. ("<>display (T.pack yaml)<>" not present)"
        yield TestResults
                 { successes = 0
                 , failures  = 0
                 }
      where
        parseYaml ::  RIO env (Either ParseException TestInfo) 
        parseYaml = liftIO $ decodeFileEither $ path x </> yaml
    sayInstruction :: HasLogFunc env => TestInstruction -> RIO env ()
    sayInstruction x = sayLn $ indent <> "  Command: "<>command x<>if shouldSucceed x then " (should succeed)." else " (should fail)."
    indent :: IsString a => a
    indent = "    "
sumarize :: (HasLogFunc env) => ConduitT TestResults Void (RIO env) ()
sumarize = do
   lift . sayLn $ "Starting regression test."
   loop (TestResults 0 0)
  where
   loop :: (HasLogFunc env) => TestResults -> ConduitT TestResults Void (RIO env) ()
   loop sofar = 
     await >>= maybe finalize (\x -> loop $! (add sofar x)) 
     where finalize = do
             logInfo . display $ tshow (successes sofar) <>" regression tests succeeded." 
             logError . display $ tshow (failures sofar) <>" regression tests failed."
             if failures sofar == 0
             then logInfo $ "Regression test of all scripts succeeded."
             else exitWith (SomeTestsFailed ["Regression test failed!"])



-- doSingleDirTests :: (HasLogFunc env) => ConduitT DirData Int (RIO env) ()
-- doSingleDirTests = do 
--   (DirData path dirContent)
--   case dirContent of
--     DirError err     -> do
--         lift . sayLn $ "ERROR:"
--         lift . sayLn $ "I've tried to look in " <> path <> "."
--         lift . sayLn $ "    There was an error: "
--         lift . sayLn $ "       " <> show err
--         yield 1
--     DirList{} -> do
--         lift . sayLn $ path <>" : "
--         yield 0 -- doSingleTestSet path files


--doSingleTest :: ConduitT 
--doSingleTest = undefined
yaml :: FilePath
yaml = "testinfo.yaml"  -- the required name of the file that contains the test info for this directory.
doSingleTestSet :: HasLogFunc env => FilePath -> [FilePath] -> RIO env Int
doSingleTestSet dir fs 
  | yaml `elem` fs = 
       do res <- parseYaml
          case res of 
              Left err -> do sayLn $ indent <> dir </> yaml <>" could not be parsed."
                             sayLn $ indent <> prettyPrintParseException err
                             return 1
              Right ti -> do sayLn $ indent <> "Instructions: "
                             mapM_ sayInstruction (testCmds ti)
                             -- <>command ti<>if shouldSucceed ti then " (should succeed)." else " (should fail)."
                             runConduit $ runTests ti .| getResults
  | otherwise =
       do sayLn $ indent <> "Nothing to do. ("<>yaml<>" not present)"
          return 0

  where
    indnt = 4
    sayInstruction :: HasLogFunc env => TestInstruction -> RIO env ()
    sayInstruction x = sayLn $ indent <> "  Command: "<>command x<>if shouldSucceed x then " (should succeed)." else " (should fail)."
    parseYaml ::  RIO env (Either ParseException TestInfo) 
    parseYaml = liftIO $ decodeFileEither $ dir </> yaml
    runTests :: TestInfo -> ConduitM () Int (RIO env) ()
    runTests ti = testsSource .| doATest
      where 
        isRelevant f = map toUpper (takeExtension f) `elem` [".ADL"]
        testsSource :: ConduitT () FilePath (RIO env) ()
        testsSource = CL.sourceList $ filter isRelevant fs
        doATest :: ConduitT FilePath Int (RIO env) ()
        doATest = awaitForever dotheTest
          where 
            aap :: (HasLogFunc env) => FilePath -> TestInstruction -> RIO env Bool
            aap = testAdlfile (indnt + 2) dir 
            dotheTest :: FilePath -> ConduitT FilePath Int (RIO env) ()
            dotheTest file = do 
              noot
              res' <- mapM mies (testCmds ti) -- res' <- mapM (liftIO . testAdlfile (indnt + 2) dir file) (testCmds ti)
              let res = [True, False]
              yield (length . filter not $ res) -- return the number of failed testcommands
             where
               noot :: ConduitT FilePath Int (RIO env) ()
               noot = liftIO $ runSimpleApp $ sayLn $ indent<>"Start testing of `"<>file<>"`: "
               mies :: TestInstruction -> ConduitT FilePath Int (RIO env) ()
               mies x = undefined
    getResults :: ConduitT Int Void (RIO env) Int
    getResults = loop 0 
     where
       loop :: Int -> ConduitT Int Void (RIO env) Int
       loop i = 
         await >>= maybe (return i) 
                         (\x -> loop $! (i+x))
    indent = replicate indnt ' '

-- This data structure is directy available in .yaml files. Be aware that modification will have consequences for the 
-- yaml files in the test suite.
data TestInfo = TestInfo 
   { testCmds :: [TestInstruction]
   }deriving Generic
instance FromJSON TestInfo
data TestInstruction = TestInstruction 
   { command :: String
   , shouldSucceed :: Bool
   } deriving Generic
instance FromJSON TestInstruction
testAdlfile :: (HasLogFunc env) =>
                Int      -- Number of spaces to indent (for output during testing)
             -> FilePath -- the filepath of the directory where the test should be done
             -> FilePath -- the script that is undergoing the test
             -> TestInstruction --The instruction to test, so it is known how to test the script
             -> RIO env Bool  -- Indicator telling if the test passed or not
testAdlfile indnt path adl tinfo = do
  (exit_code, out, err) <- liftIO $ readCreateProcessWithExitCode myProc ""
  case (shouldSucceed tinfo, exit_code) of
    (True  , ExitSuccess  ) -> passOutput
    (True  , ExitFailure _) -> failOutput (exit_code, out, err)
    (False , ExitSuccess  ) -> failOutput (exit_code, out, err)
    (False , ExitFailure _) -> passOutput

   where
     myProc :: CreateProcess
     myProc = CreateProcess { cmdspec = ShellCommand (command tinfo <>" "<>adl)
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
     passOutput :: (HasLogFunc env) => RIO env Bool
     passOutput = do sayLn "***Pass***"
                     return True 
     failOutput :: (HasLogFunc env) => (ExitCode, String, String) -> RIO env Bool
     failOutput (exit_code, out, err) = do
          sayLn $ "\n*FAIL*. Exit code: "<>show exit_code<>". "
          case exit_code of
             ExitSuccess -> return True
             _           -> do sayLnI out
                               sayLnI err
                               return False

     sayLnI :: (HasLogFunc env) => String -> RIO env ()
     sayLnI  = mapM_ (sayLn . (replicate indnt ' ' <>)) . lines 

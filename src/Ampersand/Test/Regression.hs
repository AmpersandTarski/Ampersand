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
import           Data.Yaml
import qualified RIO.Text as T
import           System.Directory
import           System.FilePath
import           System.IO.Error (tryIOError)
import           System.Process(shell, CreateProcess(..),readCreateProcessWithExitCode)

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
      lift $ logInfo $ ">> " <> displayShow (traversalNr x) <> ": "<> (display . T.pack $ path x)
      let candidates = filter isCandidate (filesOf . dirContent $ x)
            where
              isCandidate :: FilePath -> Bool
              isCandidate fp = "adl" `isExtensionOf` fp            
      if yaml `elem` (filesOf . dirContent $ x)
      then do
        res <- lift parseYaml
        case res of 
          Left err -> do 
            lift . logError $ indent <> (display . T.pack $ path x </> yaml) <>" could not be parsed."
            lift . logError $ indent <> (display . T.pack . prettyPrintParseException $ err)
            yield TestResults
                    { successes = 0
                    , failures  = 1 -- the yaml file could not be parsed
                    }
          Right ti -> do 
            lift . logDebug $ indent <> "Instructions: "
            lift $ mapM_ sayInstruction (testCmds ti)
            result <- lift . runConduit $
                         doAll candidates (testCmds ti)
                      .| doTestCase 
                      .| sumarizeTestCases
            yield result
--            doAll candidates (testCmds ti) .| doTestCase .| sumarizeTestCases
                         -- <>command ti<>if shouldSucceed ti then " (should succeed)." else " (should fail)."
                         --   runConduit $ runTests ti .| getResults
      else do
        lift . logDebug $ indent<>"Nothing to do. ("<>display (T.pack yaml)<>" not present)"
        yield TestResults
                 { successes = 0
                 , failures  = 0
                 }
      where
        doAll :: [FilePath] -> [TestInstruction] -> ConduitT () TestCase (RIO env) ()
        doAll cs tis = yieldMany $
              foo [\nr -> TestCase (traversalNr x,nr) f ti | f <- cs, ti <- tis] 1
          where foo :: [Int -> TestCase] -> Int -> [TestCase] 
                foo [] _ = []
                foo (f:fs) i = f i : foo fs (i+1) 
        doTestCase :: (HasLogFunc env) => ConduitT TestCase TestResults (RIO env) ()
        doTestCase = awaitForever doOne
           where doOne :: (HasLogFunc env) => TestCase -> ConduitT a TestResults (RIO env) ()
                 doOne tc = do
                         let (a,b) = testNr tc
                             instr = instruction tc
                         lift . logDebug $ " >> "<>display a<>"."<>display b<>": Now starting."
                         lift . logDebug $ "Runing "<>display (command instr)
                                        <>" on "<>display (T.pack $ testFile tc)
                                        <>" should "
                                        <>(case shouldSucceed instr of
                                            True -> "succeed."
                                            False -> "fail."
                                          )
                         res <- lift $ testAdlfile (path x) (testFile tc) instr
                         if res == shouldSucceed instr
                           then do
                              lift . logInfo $ " >> "<>display a<>"."<>display b<>":"<>" *** Pass ***"
                              yield TestResults {successes = 1, failures  = 0}
                           else do
                              lift . logInfo $ " >> "<>display a<>"."<>display b<>":"<>" *** Fail ***"
                              yield TestResults {successes = 0, failures  = 1}

        sumarizeTestCases :: (HasLogFunc env) => ConduitT TestResults Void (RIO env) TestResults
        sumarizeTestCases = loop (TestResults 0 0)
          where
            loop :: (HasLogFunc env) => TestResults -> ConduitT TestResults Void (RIO env) TestResults
            loop sofar = await >>= maybe (return sofar)
                                         (\result -> loop $! (add sofar result)) 
        parseYaml ::  RIO env (Either ParseException TestInfo) 
        parseYaml = liftIO $ decodeFileEither $ path x </> yaml
    sayInstruction :: HasLogFunc env => TestInstruction -> RIO env ()
    sayInstruction x = logDebug $ indent <> "  Command: "<>(display $ command x)<>if shouldSucceed x then " (should succeed)." else " (should fail)."
    indent :: IsString a => a
    indent = "    "

data TestCase = TestCase { testNr :: (Int,Int)
                         , testFile :: FilePath
                         , instruction :: TestInstruction
                         }
sumarize :: (HasLogFunc env) => ConduitT TestResults Void (RIO env) ()
sumarize = do
   lift . logInfo $ "Starting regression test."
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



yaml :: FilePath
yaml = "testinfo.yaml"  -- the required name of the file that contains the test info for this directory.
-- This data structure is directy available in .yaml files. Be aware that modification will have consequences for the 
-- yaml files in the test suite.
data TestInfo = TestInfo 
   { testCmds :: [TestInstruction]
   }deriving Generic
instance FromJSON TestInfo
data TestInstruction = TestInstruction 
   { command :: T.Text
   , shouldSucceed :: Bool
   } deriving Generic
instance FromJSON TestInstruction
testAdlfile :: (HasLogFunc env) =>
                FilePath -- the filepath of the directory where the test should be done
             -> FilePath -- the script that is undergoing the test
             -> TestInstruction --The instruction to test, so it is known how to test the script
             -> RIO env Bool  -- Indicator telling if the test passed or not
testAdlfile dir adl tinfo = do
  logInfo $ "  Start: "<> (display . T.pack $ adl)
  (exit_code, out, err) <- liftIO $ readCreateProcessWithExitCode myProc ""
  logInfo $ "  Ready: "<> (display . T.pack $ adl) <>" ("<>displayShow exit_code<>")"
  case (shouldSucceed tinfo, exit_code) of
    (True  , ExitSuccess  ) -> passOutput
    (True  , ExitFailure _) -> failOutput (exit_code, display . T.pack $ out, display . T.pack $ err)
    (False , ExitSuccess  ) -> failOutput (exit_code, display . T.pack $ out, display . T.pack $ err)
    (False , ExitFailure _) -> passOutput

   where
     myProc :: CreateProcess
     myProc = (shell $ (T.unpack (command tinfo) <>" "<>adl)) {cwd = Just dir}
      
     passOutput :: RIO env Bool
     passOutput = pure True 
     failOutput :: (HasLogFunc env) => (ExitCode, Utf8Builder, Utf8Builder) -> RIO env Bool
     failOutput (exit_code, out, err) = do
          logError $ "*FAIL*. Exit code: "<>(display $ tshow exit_code)<>". "
          case exit_code of
             ExitSuccess -> pure False
             _           -> do logWarn . indnt $ out
                               logError . indnt $ err
                               pure True
      where indnt :: Utf8Builder -> Utf8Builder
            indnt = (display (T.pack . replicate 4 $ ' ') <>)

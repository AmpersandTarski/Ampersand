{-# LANGUAGE DeriveGeneric #-}

module Ampersand.Test.Regression
  ( regressionTest,
  )
where

import Ampersand.Basics
import Ampersand.Misc.HasClasses
import Ampersand.Types.Config
import Conduit
import Data.Yaml
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.ByteString.Lazy.Partial as BLPartial
import RIO.List as L
import RIO.Process
import qualified RIO.Text as T
import System.Directory
import System.FilePath
import System.IO.Error (tryIOError)

data DirList
  = DirList
      { -- | files in the directory
        filesOf :: [FilePath],
        -- | subdirectories
        subdirsOf :: [FilePath]
      }
  | DirError IOException

data DirData = DirData
  { -- | the sequencenumber in the traversal
    traversalNr :: Int,
    -- | the full path of this directory
    path :: FilePath,
    -- | the result of reading the content in the directory
    dirContent :: DirList
  }

data TestResults = TestResults
  { successes :: Int,
    failures :: Int
  }

add :: TestResults -> TestResults -> TestResults
add a b =
  TestResults
    { successes = successes a + successes b,
      failures = failures a + failures b
    }

regressionTest :: (HasTestOpts env, HasRunner env) => RIO env ()
regressionTest = do
  testOpts <- view testOptsL
  baseDir <- liftIO . makeAbsolute $ rootTestDir testOpts
  runConduit $ walkDirTree baseDir .| numberIt .| doTestsInDir .| sumarize

walkDirTree :: FilePath -> ConduitT () (Int -> DirData) (RIO env) ()
walkDirTree fp = do
  result <- liftIO $ tryIOError (liftIO listdir)
  case result of
    Right dl ->
      case dl of
        DirList {} ->
          do
            yield (foo dl)
            forM_ (subdirsOf dl) (walkDirTree . (fp </>))
        DirError err ->
          yield (foo (DirError err))
    Left err ->
      yield (foo (DirError err))
  where
    foo :: DirList -> (Int -> DirData)
    foo dl i =
      DirData
        { traversalNr = i,
          path = fp,
          dirContent = dl
        }
    listdir = do
      entries <- getDirectoryContents fp >>= filterHidden
      subdirs <- filterM isDir entries
      files <- filterM isFile entries
      return
        $ DirList
          { filesOf = files,
            subdirsOf = subdirs
          }
      where
        isFile entry = doesFileExist (fp </> entry)
        isDir entry = doesDirectoryExist (fp </> entry)
        filterHidden paths = return $ filter (not . isHidden) paths
        isHidden ('.' : _) = True
        isHidden _ = False

numberIt :: ConduitT (Int -> DirData) DirData (RIO env) ()
numberIt = loop 1
  where
    loop nr = do
      mx <- await
      case mx of
        Nothing -> return ()
        Just f -> do
          yield (f nr)
          loop (nr + 1)

doTestsInDir :: (HasProcessContext env, HasLogFunc env) => ConduitT DirData TestResults (RIO env) ()
doTestsInDir = awaitForever once
  where
    once x = do
      lift . logInfo $ ">> " <> displayShow (traversalNr x) <> ": " <> (display . T.pack $ path x)
      let candidates = L.sort . filter isCandidate . filesOf . dirContent $ x
            where
              isCandidate :: FilePath -> Bool
              isCandidate fp =
                takeExtensions fp
                  `elem` [ ".adl", -- .adl files only, so we can use .ifc files etc. in INCLUDE statements, without having to check them separately
                           ".archimate" -- this is to test the archichecker
                         ]
      if yaml `elem` (filesOf . dirContent $ x)
        then do
          res <- lift parseYaml
          case res of
            Left err -> do
              lift . logError $ indent <> (display . T.pack $ path x </> yaml) <> " could not be parsed."
              lift . logError $ indent <> (display . T.pack . prettyPrintParseException $ err)
              yield
                TestResults
                  { successes = 0,
                    failures = 1 -- the yaml file could not be parsed
                  }
            Right ti -> do
              lift . logDebug $ indent <> "Instructions: "
              lift $ mapM_ sayInstruction (testCmds ti)
              result <- lift . doFilesWithCommand candidates $ ti
              yield result
        else do
          lift . logDebug $ indent <> "Nothing to do. (" <> display (T.pack yaml) <> " not present)"
          yield
            TestResults
              { successes = 0,
                failures = 0
              }
      where
        doFilesWithCommand :: (HasProcessContext env, HasLogFunc env) => [FilePath] -> TestSpec -> RIO env TestResults
        doFilesWithCommand candidates ti =
          runConduit
            $ doAll candidates (testCmds ti)
            .| mapMC runTestcase
            .| sumarizeTestCases
          where
            doAll :: [FilePath] -> [TestInstruction] -> ConduitT () TestCase (RIO env) ()
            doAll cs tis =
              yieldMany
                $ foo [\nr -> TestCase (traversalNr x, nr) f ti' | f <- cs, ti' <- tis] 1
              where
                foo :: [Int -> TestCase] -> Int -> [TestCase]
                foo [] _ = []
                foo (f : fs) i = f i : foo fs (i + 1)
            runTestcase :: (HasProcessContext env, HasLogFunc env) => TestCase -> RIO env TestResults
            runTestcase tc = do
              let instr = instruction tc
                  indnt = ">> " <> (display . fst . testNr $ tc) <> "." <> (display . snd . testNr $ tc) <> ": "
              res <- testAdlfile indnt (path x) (testFile tc) instr
              return
                $ if res
                  then TestResults {successes = 1, failures = 0}
                  else TestResults {successes = 0, failures = 1}

            sumarizeTestCases :: (HasLogFunc env) => ConduitT TestResults Void (RIO env) TestResults
            sumarizeTestCases = loop $ TestResults {successes = 0, failures = 0}
              where
                loop :: (HasLogFunc env) => TestResults -> ConduitT TestResults Void (RIO env) TestResults
                loop sofar =
                  await
                    >>= maybe
                      (return sofar)
                      (\result -> loop $! add sofar result)
        parseYaml :: RIO env (Either ParseException TestSpec)
        parseYaml = liftIO . decodeFileEither $ path x </> yaml
    sayInstruction :: (HasLogFunc env) => TestInstruction -> RIO env ()
    sayInstruction x = logDebug $ indent <> "  Command: " <> display (command x) <> if exitcode x == 0 then " (should succeed)." else " (should fail with exitcode " <> display (exitcode x) <> ")."
    indent :: (IsString a) => a
    indent = "    "

data TestCase = TestCase
  { testNr :: (Int, Int),
    testFile :: FilePath,
    instruction :: TestInstruction
  }

sumarize :: (HasLogFunc env) => ConduitT TestResults Void (RIO env) ()
sumarize = do
  lift . logInfo $ "Starting regression test."
  loop (TestResults 0 0)
  where
    loop :: (HasLogFunc env) => TestResults -> ConduitT TestResults Void (RIO env) ()
    loop sofar =
      await >>= maybe finalize (\x -> loop $! add sofar x)
      where
        finalize = do
          logInfo $ displayShow (successes sofar) <> " regression tests succeeded."
          logError $ displayShow (failures sofar) <> " regression tests failed."
          if failures sofar == 0
            then logInfo "Regression test of all scripts succeeded."
            else exitWith (SomeTestsFailed ["Regression test failed!"])

yaml :: FilePath
yaml = "testinfo.yaml" -- the required name of the file that contains the test info for this directory.
-- This data structure is directy available in .yaml files. Be aware that modification will have consequences for the
-- yaml files in the test suite.

newtype TestSpec = TestSpec
  { testCmds :: [TestInstruction]
  }
  deriving (Generic)

instance FromJSON TestSpec

data TestInstruction = TestInstruction
  { command :: Text,
    exitcode :: Int
  }
  deriving (Generic)

instance FromJSON TestInstruction

testAdlfile ::
  (HasProcessContext env, HasLogFunc env) =>
  Utf8Builder -> -- prefix to use in all output
  FilePath -> -- the filepath of the directory where the test should be done
  FilePath -> -- the script that is undergoing the test
  TestInstruction -> -- The instruction to test, so it is known how to test the script
  RIO env Bool -- Indicator telling if the test passed or not
testAdlfile indent dir adl tinfo = do
  logInfo $ indent <> "Start: " <> (display . command $ tinfo) <> " " <> (display . T.pack $ adl)
  (exit_code, out, err) <- withWorkingDir dir
    $ case words . T.unpack . command $ tinfo of
      [] -> fatal "No command given!"
      h : tl -> do
        proc h (tl <> [adl]) readProcess

  let testPassed = case exit_code of
        ExitSuccess -> exitcode tinfo == 0
        (ExitFailure x) -> exitcode tinfo == x
  (if testPassed then passHandler else failHandler) (exit_code, out, err)
  return testPassed
  where
    passHandler :: (HasLogFunc env) => (ExitCode, BL.ByteString, BL.ByteString) -> RIO env ()
    passHandler (_, _, _) = do
      logInfo $ indent <> "✅ Passed."
    failHandler :: (HasLogFunc env) => (ExitCode, BL.ByteString, BL.ByteString) -> RIO env ()
    failHandler (exit_code, out, err) = do
      logError
        $ "❗❗❗ Failed. "
        <> indent
        <> (display . T.pack $ adl)
        <> " "
        <> "(Expected: "
        <> ( if exitcode tinfo == 0
               then "ExitSuccess"
               else "ExitFailure " <> display (exitcode tinfo)
           )
        <> ", Actual: "
        <> display (tshow exit_code)
        <> ")"
      mapM_ (logError . indnt) . toUtf8Builders $ out

      logError . indnt $ "------------------- "
      mapM_ (logError . indnt) . toUtf8Builders $ err
      logError . indnt . display $ "out: " <> tshow (nrOfLines out) <> " lines."
      logError . indnt . display $ "err: " <> tshow (nrOfLines err) <> " lines."
      logError "❗❗❗ -------------------"
      where
        nrOfLines = length . BL.split (BLPartial.head "\n")
    indnt :: Utf8Builder -> Utf8Builder
    indnt = ("    " <>)
    toUtf8Builders :: BL.ByteString -> [Utf8Builder]
    toUtf8Builders = map display . T.lines . decodeUtf8With lenientDecode . BL.toStrict

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module MainApps(
    IO
  , ampersand
  , preProcessor
  , mainTest
  , regressionTest
) where

import           Ampersand
import           Ampersand.Input.PreProcessor
import           Ampersand.Options.GlobalParser
import           Ampersand.Runners
import           Ampersand.Types.Config
import           Conduit
import qualified RIO.Set as Set
import qualified RIO.Text as T
import           System.Directory
import           System.Environment    (getArgs, getProgName)
import           System.FilePath ((</>))
import           System.IO.Error (tryIOError)
  
ampersand :: IO ()
ampersand = do
  progName <- getProgName
  args <- getArgs
  work <- ampersandOptionsHandler progName args
  ampersandWorker work

ampersandOptionsHandler :: String -> [String] -> IO (Either ExitCode (GlobalOptsMonoid, RIO Runner ()))
ampersandOptionsHandler progName args = do
  currentDir <- getCurrentDirectory
  runSimpleApp $ logDebug . display $ "args: "<>(T.pack $ show args)
  try $ commandLineHandler currentDir progName args

ampersandWorker :: Either ExitCode (GlobalOptsMonoid, RIO Runner ()) -> IO ()
ampersandWorker eGlobalRun = do
  let defaultOuptutDir = "."
  isTerminal <- hIsTerminalDevice stdout
  case eGlobalRun of
    Left (exitCode :: ExitCode) ->
      throwIO exitCode
    Right (globalMonoid,run) -> do
      global <- globalOptsFromMonoid isTerminal defaultOuptutDir globalMonoid
      -- when (globalLogLevel global == LevelDebug) $ hPutStrLn stderr versionString'
      withRunnerGlobal global $ run `catch` \e ->
          -- This special handler stops "stack: " from being printed before the
          -- exception
          case fromException e of
              Just ec -> exitWith ec
              Nothing -> do
                  logError $ fromString $ displayException e
                  exitWith $ RunnerAborted $ lines $ displayException e



-- ampersandOld :: IO ()
-- ampersandOld = do
--   env <- defEnv
--   runRIO env ampersand'

-- ampersand' :: () 
--   => RIO App ()
-- ampersand' = do
--     env <- ask
--     rootFile <- view rootFileL
-- --    sequence_ . map snd . filter fst $ actionsWithoutScript env-- There are commands that do not need a single filename to be speciied
--     case rootFile of
--       Just _ -> do -- An Ampersand script is provided that can be processed
--             sayLn "Processing your model..."
--             gFSpec <- createFspec []
--             case gFSpec of
--               Errors err    -> 
--                  exitWith . NoValidFSpec . L.intersperse  (replicate 30 '=') 
--                . fmap show . NEL.toList $ err
--               Checked _fSpec ws -> do
--                  mapM_  sayLn . concatMap (lines . show) $ ws
--               --   generateAmpersandOutput fSpec
--                  sayLn "THIS IS THE OLD STUFF. Nothing happend."
--                  sayLn "Finished processing your model"
--                  sayLn . ("Your script has no errors " ++) $
--                     case ws of
--                       []  -> "and no warnings"
--                       [_] -> ", but one warning was found"
--                       _   -> ", but "++show (length ws)++" warnings were found"
           
--       Nothing -> -- No Ampersand script is provided 
--          if or (map fst $ actionsWithoutScript env)
--          then sayWhenLoudLn "No further actions, because no ampersand script is provided"
--          else liftIO $ do 
--             args     <- getArgs
--             progName <- getProgName
--             exitWith . NoAmpersandScript $
--                  [ "No ampersand script provided. Use --help for usage information"
--                  , "   " <> progName <> (concat $ fmap (" " <>) args) ]

--  where
-- --   actionsWithoutScript :: (HasVersion env, HasEnvironment env, HasLogFunc env, HasCommands env) 
-- --      => env -> [(Bool, RIO App ())]
-- --   actionsWithoutScript env = 
-- --      [ ( (view showVersionL env)   || view verbosityL env == Loud , sayLn $ versionText env)
-- --      , ( (view genSampleConfigFileL env)      , liftIO writeConfigFile)
-- --      , ( (view showHelpL env)                 , sayLn $ usageInfo' env)
-- --      , ( (view runAsDaemonL env)              , runDaemon)
-- --      ]
   
--    versionText :: (HasVersion env) => env -> String
--    versionText env = view preVersionL env ++ ampersandVersionStr ++ view postVersionL env

preProcessor :: IO()
preProcessor = 
   runSimpleApp preProcessor' 

preProcessor' :: (HasLogFunc env) => RIO env ()
preProcessor' =
  do
    args <- liftIO getArgs;
    case args of 
      []  -> fatal "No arguments given"
      filename:defs -> do
        result  <- readUTF8File filename
        content <- do
             case result of
               Left err   -> exitWith $ ReadFileError $ "Error while reading input file.\n" : err
               Right cont -> return cont
        sayLn $ either show id (preProcess' filename (Set.fromList defs) (T.unpack content))

mainTest :: IO ()
mainTest = do
  progName <- getProgName
  let args = ["test"]
  work <- ampersandOptionsHandler progName args
  ampersandWorker work
--   env <- defEnv
--   runRIO env mainTest'

mainTest' :: (HasRunner env) => RIO env ()
mainTest' = do 
    sayLn "Starting Quickcheck tests."
    funcs <- testFunctions
    testAmpersandScripts
    tests funcs
  where 
      tests :: (HasLogFunc env) => [([String], RIO env Bool)] -> RIO env ()
      tests [] = pure ()
      tests ((msg,tst):xs) = do
          mapM_ sayLn msg
          success <- tst
          if success then tests xs
          else exitWith (SomeTestsFailed ["*** Some tests failed***"])
               

      testFunctions :: (HasRunner env) => RIO env [([String], RIO env Bool)]
      testFunctions = do
          scr <- getTestScripts
          (parserCheckResult, msg) <- parserQuickChecks
          return [ (["Parsing " ++ show (length scr) ++ " scripts."], parseScripts scr)
                 , ( if parserCheckResult  
                     then ["Parser & prettyprinter test PASSED."]
                     else (  ["QuickCheck found errors in the roundtrip in parsing/prettyprinting for the following case:"]
                           ++map ("\n   "++) (lines msg)
                          )
                   , return parserCheckResult
                   )
                 ]

regressionTest :: IO ()
regressionTest = do
  progName <- getProgName
  let args = ["test"]
  work <- ampersandOptionsHandler progName args
  ampersandWorker work
--   env <- defEnv
--   runRIO env regressionTest'


regressionTest' :: (HasRunner env) => RIO env ()
regressionTest' = do 
    sayLn $ "Starting regression test."
    baseDir <- liftIO . makeAbsolute $ "." </> "testing"
    totalfails <- runConduit $ walk baseDir .| myVisitor .| sumarize
    if totalfails == 0
    then sayLn $ "Regression test of all scripts succeeded."
    else exitWith (SomeTestsFailed ["Regression test failed! ("++show totalfails++" tests failed.)"])
  where   

    -- Produces directory data
    walk :: FilePath -> ConduitT () DirData (RIO env) ()
    walk path = do 
        result <- liftIO $ tryIOError (liftIO listdir)
        case result of
          Right dl
              -> case dl of 
                   DirList subdirs _
                    -> do
                        yield (DirData path dl)
                        forM_ subdirs (walk . (path </>))
                   DirError err 
                    -> yield (DirData path (DirError err))
          Left err
              -> yield (DirData path (DirError err))

      where
        listdir = do
            entries <- getDirectoryContents path >>= filterHidden
            subdirs <- filterM isDir entries
            files <- filterM isFile entries
            return $ DirList subdirs files
            where 
                isFile entry = doesFileExist (path </> entry)
                isDir entry = doesDirectoryExist (path </> entry)
                filterHidden paths = return $ filter (not.isHidden) paths
                isHidden ('.':_) = True
                isHidden _       = False
                
    -- Convert a DirData into an Int that contains the number of failed tests
    myVisitor :: (HasLogFunc env) => ConduitT DirData Int (RIO env) ()
    myVisitor = loop 1
      where
        loop :: (HasLogFunc env) => Int -> ConduitT DirData Int (RIO env) ()
        loop n = awaitForever $
            (\dird -> do 
                lift $ sayLn $ ">> " ++ show n ++ ". "
                x <- lift $ process 4 dird     
                yield x
                loop (n + 1)
            ) 
                    

    sumarize :: ConduitT Int Void (RIO env) Int
    sumarize = loop 0 
      where
        loop :: Int -> ConduitT Int Void (RIO env) Int
        loop i = 
          await >>= maybe (return i) 
                          (\x -> loop $! (i+x))



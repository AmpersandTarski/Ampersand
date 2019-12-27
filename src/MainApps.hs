{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module MainApps(
    IO
  , ampersand
  , preProcessor
  , mainTest
) where

import           Ampersand
import           Ampersand.Input.PreProcessor
import           Ampersand.Options.GlobalParser
import           Ampersand.Runners
import           Ampersand.Types.Config
import qualified RIO.Set as Set
import qualified RIO.Text as T
import           System.Directory
import           System.Environment    (getArgs, getProgName)
  
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
                  throwIO e -- exitWith $ RunnerAborted $ lines $ displayException e



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
--             logInfo "Processing your model..."
--             gFSpec <- createFspec []
--             case gFSpec of
--               Errors err    -> 
--                  exitWith . NoValidFSpec . L.intersperse  (replicate 30 '=') 
--                . fmap show . NE.toList $ err
--               Checked _fSpec ws -> do
--                  mapM_  logInfo . concatMap (lines . show) $ ws
--               --   generateAmpersandOutput fSpec
--                  logInfo "THIS IS THE OLD STUFF. Nothing happend."
--                  logInfo "Finished processing your model"
--                  logInfo . ("Your script has no errors " ++) $
--                     case ws of
--                       []  -> "and no warnings"
--                       [_] -> ", but one warning was found"
--                       _   -> ", but "++show (length ws)++" warnings were found"
           
--       Nothing -> -- No Ampersand script is provided 
--          if or (map fst $ actionsWithoutScript env)
--          then logDebug "No further actions, because no ampersand script is provided"
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
-- --      [ ( (view showVersionL env)   || view verbosityL env == Loud , logInfo $ versionText env)
-- --      , ( (view genSampleConfigFileL env)      , liftIO writeConfigFile)
-- --      , ( (view showHelpL env)                 , logInfo $ usageInfo' env)
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
        logInfo $ either displayShow (display . T.pack) (preProcess' filename (Set.fromList defs) (T.unpack content))

mainTest :: IO ()
mainTest = do
  progName <- getProgName
  let args = ["test", "testing"]
  work <- ampersandOptionsHandler progName args
  ampersandWorker work
--   env <- defEnv
--   runRIO env mainTest'


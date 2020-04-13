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
  progName <- T.pack <$> getProgName
  args <- map T.pack <$> getArgs
  work <- ampersandOptionsHandler progName args
  ampersandWorker work

ampersandOptionsHandler :: Text -> [Text] -> IO (Either ExitCode (GlobalOptsMonoid, RIO Runner ()))
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
                  throwIO e



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
  progName <- T.pack <$> getProgName
  let args = ["test", "testing"]
  work <- ampersandOptionsHandler progName args
  ampersandWorker work
--   env <- defEnv
--   runRIO env mainTest'


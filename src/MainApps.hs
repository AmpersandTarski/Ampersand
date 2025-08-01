{-# LANGUAGE ScopedTypeVariables #-}

module MainApps
  ( IO,
    ampersand,
    preProcessor,
    mainTest,
  )
where

-- The purpose of this module is to call "commandLineHandler" with the correct directory, program name, and arguments.
-- Or, in case of the preprocessor or test engine, call them.

import Ampersand
import Ampersand.Input.PreProcessor
import Ampersand.Options.GlobalParser
import Ampersand.Runners
import Ampersand.Types.Config
import RIO.Directory
import qualified RIO.Set as Set
import qualified RIO.Text as T
import System.Environment (getArgs, getProgName)

ampersand :: IO ()
ampersand = do
  progName <- T.pack <$> getProgName
  args <- map T.pack <$> getArgs
  work <- ampersandOptionsHandler progName args
  ampersandWorker work

ampersandOptionsHandler :: Text -> [Text] -> IO (Either ExitCode (GlobalOptsMonoid, RIO Runner ()))
ampersandOptionsHandler progName args = do
  currentDir <- getCurrentDirectory
  runSimpleApp $ logDebug . display $ "args: " <> T.pack (show args)
  try $ commandLineHandler currentDir progName args

ampersandWorker :: Either ExitCode (GlobalOptsMonoid, RIO Runner ()) -> IO ()
ampersandWorker eGlobalRun = do
  let defaultOuptutDir = "."
  isTerminal <- hIsTerminalDevice stdout
  case eGlobalRun of
    Left (exitCode :: ExitCode) ->
      throwIO exitCode
    Right (globalMonoid, run) -> do
      global <- globalOptsFromMonoid isTerminal defaultOuptutDir globalMonoid
      -- when (globalLogLevel global == LevelDebug) $ hPutStrLn stderr versionString'

      withRunnerGlobal global
        $ run
        `catch` \e ->
          -- This special handler stops "ampersand: " from being printed before the
          -- exception
          case fromException e of
            Just ec -> exitWith ec
            Nothing -> do
              logError $ fromString $ displayException e
              throwIO e

preProcessor :: IO ()
preProcessor =
  runSimpleApp preProcessor'

preProcessor' :: (HasLogFunc env) => RIO env ()
preProcessor' =
  do
    args <- liftIO getArgs
    case args of
      [] -> fatal "No arguments given"
      filename : defs -> do
        result <- readFileUtf8 filename
        content <- do
          case result of
            Left err -> exitWith $ ReadFileError $ "Error while reading input file.\n" : err
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

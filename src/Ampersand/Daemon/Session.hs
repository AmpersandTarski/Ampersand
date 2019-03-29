{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | A persistent version of the Ghci session, encoding lots of semantics on top.
--   Not suitable for calling multithreaded.
-- _Acknoledgements_: This is mainly copied from Neil Mitchells ghcid.
module Ampersand.Daemon.Session(
      Session
    , withSession
    , sessionStart
    ) where

import Ampersand.Daemon.Daemon.Daemon
import Ampersand.Daemon.Daemon.Util
import Data.IORef
import System.Directory
import System.FilePath
import Control.Exception.Extra
import Control.Concurrent.Extra
import Control.Monad.Extra
import Data.Maybe
import Data.List.Extra
import Ampersand.Basics.Prelude
import Ampersand.Misc

data Session = Session
    {ghci :: IORef (Maybe AmpersandDaemon) -- ^ The Ghci session, or Nothing if there is none
    ,command :: IORef (Maybe (String, [String])) -- ^ The last command passed to sessionStart, setup operations
    ,warnings :: IORef [Load] -- ^ The warnings from the last load
    ,curdir :: IORef FilePath -- ^ The current working directory
    ,running :: Var Bool -- ^ Am I actively running an async command
    ,withThread :: ThreadId -- ^ Thread that called withSession
    }

debugging :: Bool
debugging = True

debugShutdown :: Show b => b -> IO ()
debugShutdown x = when debugging $ print ("DEBUG SHUTDOWN", x)

-- | The function 'withSession' expects to be run on the main thread,
--   but the inner function will not. This ensures Ctrl-C is handled
--   properly and any spawned Ghci processes will be aborted.
withSession :: (Session -> IO a) -> IO a
withSession f = do
    ghci <- newIORef Nothing
    command <- newIORef Nothing
    warnings <- newIORef []
    curdir <- newIORef "."
    running <- newVar False
    debugShutdown "Starting session"
    withThread <- myThreadId
    f Session{..} `finally` do
        debugShutdown "Start finally"
        modifyVar_ running $ const $ return False
        whenJustM (readIORef ghci) $ \v -> do
            writeIORef ghci Nothing
--            debugShutdown "Calling kill"
--            kill
        debugShutdown "Finish finally"


-- | Kill. Wait just long enough to ensure you've done the job, but not to see the results.
kill :: IO ()
kill = ignored $ do
    debugShutdown "Before quit"
    ignored $ quit
    

-- | Spawn a new Ghci process at a given command line. Returns the load messages, plus
--   the list of files that were observed (both those loaded and those that failed to load).
sessionStart :: Options -> Session -> String -> [String] -> IO (AmpersandDaemon)
sessionStart opts Session{..} cmd setup = do
    modifyVar_ running $ const $ return True
    writeIORef command $ Just (cmd, setup)

    -- cleanup any old instances
    whenJustM (readIORef ghci) $ \v -> do
        writeIORef ghci Nothing
        void $ forkIO $ kill
    currentDirectory <- readIORef curdir >>= makeAbsolute
    
    -- start the new
    outStrLn $ "Loading " ++ cmd ++ " ..."
    aDaemon <- mask $ \unmask -> do
        daemonState <- unmask $ startAmpersandDaemon opts currentDirectory
        writeIORef ghci $ Just (daemonState)
        return daemonState
    mapM_ outStrLn $ lines . show $ aDaemon  -- for debugging
    dir <- getCurrentDirectory 
    writeIORef curdir dir

    return (aDaemon)


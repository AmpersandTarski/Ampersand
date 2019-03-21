{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Library for spawning and working with Ghci sessions.
-- _Acknoledgements_: This is mainly copied from Neil Mitchells ghcid.
module Ampersand.Daemon.Daemon.Daemon(
    AmpersandDaemon,
    
--    Ghci, 
--    GhciError(..), 
    Stream(..),
    Load(..), load,messages,loaded,
    Severity(..),
--    startGhci, 
    startAmpersandDaemon,
    interrupt, 
    process,
    execStream, 
--    showModules, 
--    showPaths, 
--    reload, 
    exec,
--    quitGhci,
    quit
    ) where

import System.IO
import System.IO.Error
import System.Process
--import System.Time.Extra
import Control.Concurrent.Extra
import Control.Exception.Extra
import Control.Monad.Extra
import Data.Function
import Data.List.Extra
import Data.Maybe
import Data.IORef
import Control.Applicative
import Data.Unique

import System.Console.CmdArgs.Verbosity

import Ampersand.Daemon.Daemon.Parser
import Ampersand.Daemon.Daemon.Types as T
import Ampersand.Daemon.Daemon.Util
import Ampersand.Basics hiding (Unique, hPutStrLn)
import Ampersand.Misc

-- | An AmpersandDaemon session. Created with 'startAmpersandDaemon', closed with 'stopAmpersandDaemon'.
--
--   The interactions with a 'Ghci' session must all occur single-threaded,
--   or an error will be raised. The only exception is 'interrupt', which aborts
--   a running computation, or does nothing if no computation is running.
data Ghci = Ghci
    {ghciProcess :: ProcessHandle
    ,ghciInterrupt :: IO ()
    ,ghciExec :: String -> (Stream -> String -> IO ()) -> IO ()
    ,ghciUnique :: Unique
    }
instance Eq Ghci where
    a == b = ghciUnique a == ghciUnique b

load :: AmpersandDaemon -> [Load]
load = loads . adState
messages :: AmpersandDaemon -> [Load]
messages = filter isMessage . load
loaded :: AmpersandDaemon -> [FilePath]
loaded = map fst . loadResults . adState
data AmpersandDaemon = AmpersandDaemon
    {adExec :: String -> (Stream -> String -> IO ()) -> IO ()
    ,adUnique :: Unique
    ,adState :: DaemonState
    }
instance Eq AmpersandDaemon where
    a == b = adUnique a == adUnique b
instance Show AmpersandDaemon where
  show = show . adState     
data DaemonState = DaemonState
   { filesToLoad :: [FilePath]
   , loads :: [Load]
   , loadResults :: [(FilePath, P_Context)]
   }
instance Show DaemonState where
  showsPrec _ x
   = showString ("DaemonState: "++show (filesToLoad x) ++ " " ++(show .length . loads $ x))

withCreateProc :: CreateProcess
                        -> (Maybe Handle
                            -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO c)
                        -> IO c
withCreateProc proc f = do
    let undo (_, _, _, proc) = ignored $ terminateProcess proc
    bracketOnError (createProcess proc) undo $ \(a,b,c,d) -> f a b c d

-- | Start GHCi by running the described process, returning  the result of the initial loading.
--   If you do not call 'stopGhci' then the underlying process may be leaked.
--   The callback will be given the messages produced while loading, useful if invoking something like "cabal repl"
--   which might compile dependent packages before really loading.
--
--   To create a 'CreateProcess' use the functions in "System.Process", particularly
--   'System.Process.shell' and 'System.Process.proc'.
--
--   @since 0.6.11
{- startGhciProcess :: CreateProcess -> (Stream -> String -> IO ()) -> IO (Ghci, [Load])
startGhciProcess process echo0 = do
    let proc = process{std_in=CreatePipe, std_out=CreatePipe, std_err=CreatePipe, create_group=True}
    withCreateProc proc $ \(Just inp) (Just out) (Just err) ghciProcess -> do

        hSetBuffering out LineBuffering
        hSetBuffering err LineBuffering
        hSetBuffering inp LineBuffering
        let writeInp x = do
                whenLoud $ outStrLn $ "%STDIN: " ++ x
                hPutStrLn inp x

        -- Some programs (e.g. stack) might use stdin before starting ghci (see #57)
        -- Send them an empty line
        hPutStrLn inp ""

        -- I'd like the GHCi prompt to go away, but that's not possible, so I set it to a special
        -- string and filter that out.
        let ghcid_prefix = "#~GHCID-START~#"
        let removePrefix = dropPrefixRepeatedly ghcid_prefix

        -- At various points I need to ensure everything the user is waiting for has completed
        -- So I send messages on stdout/stderr and wait for them to arrive
        syncCount <- newVar 0
        let syncReplay = do
                i <- readVar syncCount
                -- useful to avoid overloaded strings by showing the ['a','b','c'] form, see #109
                let showStr xs = "[" ++ intercalate "," (map show xs) ++ "]"
                let msg = "#~GHCID-FINISH-" ++ show i ++ "~#"
                writeInp $ "INTERNAL_GHCID.putStrLn " ++ showStr msg ++ "\n" ++
                        "INTERNAL_GHCID.hPutStrLn INTERNAL_GHCID.stderr " ++ showStr msg
                return $ isInfixOf msg
        let syncFresh = do
                modifyVar_ syncCount $ return . succ
                syncReplay

        -- Consume from a stream until EOF (return Nothing) or some predicate returns Just
        let consume :: Stream -> (String -> IO (Maybe a)) -> IO (Maybe a)
            consume name finish = do
                let h = if name == Stdout then out else err
                fix $ \rec -> do
                    el <- tryBool isEOFError $ hGetLine h
                    case el of
                        Left _ -> return Nothing
                        Right l -> do
                            whenLoud $ outStrLn $ "%" ++ upper (show name) ++ ": " ++ l
                            res <- finish $ removePrefix l
                            case res of
                                Nothing -> rec
                                Just a -> return $ Just a

        let consume2 :: String -> (Stream -> String -> IO (Maybe a)) -> IO (a,a)
            consume2 msg finish = do
                -- fetch the operations in different threads as hGetLine may block
                -- and can't be aborted by async exceptions, see #154
                res1 <- onceFork $ consume Stdout (finish Stdout)
                res2 <- onceFork $ consume Stderr (finish Stderr)
                res1 <- res1
                res2 <- res2
                case liftM2 (,) res1 res2 of
                    Nothing -> case cmdspec process of
                        ShellCommand cmd -> throwIO $ UnexpectedExit cmd msg
                        RawCommand exe args -> throwIO $ UnexpectedExit (unwords (exe:args)) msg
                    Just v -> return v

        -- held while interrupting, and briefly held when starting an exec
        -- ensures exec values queue up behind an ongoing interrupt and no two interrupts run at once
        isInterrupting <- newLock

        -- is anyone running running an exec statement, ensure only one person talks to ghci at a time
        isRunning <- newLock

        let ghciExec command echo = do
                withLock isInterrupting $ return ()
                res <- withLockTry isRunning $ do
                    writeInp command
                    stop <- syncFresh
                    void $ consume2 command $ \strm s ->
                        if stop s then return $ Just () else do _ <- echo strm s; return Nothing
                when (isNothing res) $
                    fail "Ghcid.exec, computation is already running, must be used single-threaded"

        let ghciInterrupt = withLock isInterrupting $
                whenM (fmap isNothing $ withLockTry isRunning $ return ()) $ do
                    whenLoud $ outStrLn "%INTERRUPT"
                    interruptProcessGroupOf ghciProcess
                    -- let the person running ghciExec finish, since their sync messages
                    -- may have been the ones that got interrupted
                    _ <- syncReplay
                    -- now wait for the person doing ghciExec to have actually left the lock
                    withLock isRunning $ return ()
                    -- there may have been two syncs sent, so now do a fresh sync to clear everything
                    stop <- syncFresh
                    void $ consume2 "Interrupt" $ \_ s -> return $ if stop s then Just () else Nothing

        ghciUnique <- newUnique
        let ghci = Ghci{..}

        -- Now wait for 'GHCi, version' to appear before sending anything real, required for #57
        stdout <- newIORef []
        stderr <- newIORef []
        sync <- newIORef $ const False
        _ <- consume2 "" $ \strm s -> do
            stop <- readIORef sync
            if stop s then
                return $ Just ()
            else do
                -- there may be some initial prompts on stdout before I set the prompt properly
                s <- return $ maybe s (removePrefix . snd) $ stripInfix ghcid_prefix s
                whenLoud $ outStrLn $ "%STDOUT2: " ++ s
                modifyIORef (if strm == Stdout then stdout else stderr) (s:)
                when (any (`isPrefixOf` s) ["GHCi, version ","GHCJSi, version "]) $ do
                    -- the thing before me may have done its own Haskell compiling
                    writeIORef stdout []
                    writeIORef stderr []
                    writeInp "import qualified System.IO as INTERNAL_GHCID"
                    writeInp ":unset +t +s" -- see https://github.com/ndmitchell/ghcid/issues/162
                    writeInp $ ":set prompt " ++ ghcid_prefix

                    writeIORef sync =<< syncFresh
                echo0 strm s
                return Nothing
        r1 <- parseLoad . reverse <$> ((++) <$> readIORef stderr <*> readIORef stdout)
        -- see #132, if hide-source-paths was turned on the modules didn't get printed out properly
        -- so try a showModules to capture the information again
        r2 <- if any isLoading r1 then return [] else map (uncurry Loading) <$> showModules ghci
        execStream ghci "" echo0
        return (ghci, r1 ++ r2)
 -}

{- -- | Start GHCi by running the given shell command, a helper around 'startGhciProcess'.
startGhci
    :: String -- ^ Shell command
    -> Maybe FilePath -- ^ Working directory
    -> (Stream -> String -> IO ()) -- ^ Output callback
    -> IO (Ghci, [Load])
startGhci cmd directory callback = startGhciProcess (shell cmd){cwd=directory} callback 
 -}
startAmpersandDaemon 
     :: Options  -- Ampersand options
     -> FilePath  -- ^ Working directory
     -> (Stream -> String -> IO ()) -- ^ Output callback
     -> IO AmpersandDaemon
startAmpersandDaemon opts directory echo' = do
    unique <- newUnique

    -- At various points I need to ensure everything the user is waiting for has completed
    -- So I send messages on stdout/stderr and wait for them to arrive
    syncCount <- newVar 0
    let writeInp x = do
            whenLoud $ outStrLn $ "%STDIN: " ++ x


    let syncReplay = do
            i <- readVar syncCount
            -- useful to avoid overloaded strings by showing the ['a','b','c'] form, see #109
            let showStr xs = "[" ++ intercalate "," (map show xs) ++ "]"
            let msg = "#~GHCID-FINISH-" ++ show i ++ "~#"
            writeInp $ "INTERNAL_GHCID.putStrLn " ++ showStr msg ++ "\n" ++
                    "INTERNAL_GHCID.hPutStrLn INTERNAL_GHCID.stderr " ++ showStr msg
            return $ isInfixOf msg
    let syncFresh :: IO (String -> Bool)
        syncFresh = do
            modifyVar_ syncCount $ return . succ
            syncReplay
    state <- initialState opts directory
    let ad = AmpersandDaemon
                    {adExec = execCommand -- \cmd echo -> outStrLn $ "Daemon executes command: "++cmd 
                    ,adUnique = unique
                    ,adState = state}
         where execCommand command echo = do
                    writeInp $ "Weer een rondje: "++command
                    stop <- syncFresh
                    void $ consume2 command $ \strm s ->
                        if stop s then return $ Just () else do _ <- echo strm s; return Nothing
    return ad            
-- parseProject :: Options -> FilePath -> IO [Load] 
stopAmpersandDaemon :: AmpersandDaemon -> IO ()
stopAmpersandDaemon _ = Ampersand.Basics.putStrLn "Daemon stopped."
   
initialState :: Options -> FilePath -> IO DaemonState
initialState opts directory = do
   let root = directory -- TODO: Read contents of .ampersand file. Fail if not present.
   ls <- parseProject opts root 
   return DaemonState
     { filesToLoad = [directory]
     , loads = ls
     , loadResults = []
     }
-- | Execute a command, calling a callback on each response.
--   The callback will be called single threaded.
execStream :: AmpersandDaemon -> String -> (Stream -> String -> IO ()) -> IO ()
execStream = adExec

-- | Interrupt Ghci, stopping the current computation (if any),
--   but leaving the process open to new input.
interrupt :: Ghci -> IO ()
interrupt = ghciInterrupt

-- | Obtain the progress handle behind a GHCi instance.
process :: Ghci -> ProcessHandle
process = ghciProcess


---------------------------------------------------------------------
-- SUGAR HELPERS

-- | Execute a command, calling a callback on each response.
--   The callback will be called single threaded.
execBuffer :: AmpersandDaemon -> String -> IO (AmpersandDaemon,[String])
execBuffer ghci cmd = do
    stdout <- newIORef []
    stderr <- newIORef []
    execStream ghci cmd $ \strm s -> do
        modifyIORef (if strm == Stdout then stdout else stderr) (s:)
    let dummyLoad = newLoad  cmd
    output <- reverse <$> ((++) <$> readIORef stderr <*> readIORef stdout)
    return (addLoad dummyLoad ghci,output)
-- | Send a command, get lines of result. Must be called single-threaded.
exec :: AmpersandDaemon -> String -> IO (AmpersandDaemon,[String])
exec ad cmd = execBuffer ad cmd 

addLoad :: Load -> AmpersandDaemon -> AmpersandDaemon
addLoad = fatal "this fatal is at `addLoad`."
newLoad :: String -> Load
newLoad s = Message
   {loadSeverity = Error
   ,loadFile = ".ampersand"
   ,loadFilePos = (0,0) -- ^ The position in the file, @(line,col)@, both 1-based. Uses @(0,0)@ for no position information.
   ,loadFilePosEnd = (0,0) -- ^ The end position in the file, @(line,col)@, both 1-based. If not present will be the same as 'loadFilePos'.
   ,loadMessage = ["Commando: "++s] -- ^ The message, split into separate lines, may contain ANSI Escape codes.
   }

{- -- | List the modules currently loaded, with module name and source file.
showModules :: AmpersandDaemon -> IO [(String,FilePath)]
showModules ghci = parseShowModules <$> execGhci ghci ":show modules"
 -}
{- -- | Return the current working directory, and a list of module import paths
showPaths :: AmpersandDaemon -> IO (FilePath, [FilePath])
showPaths ghci = parseShowPaths <$> execGhci ghci ":show paths"
 -}
{- -- | Perform a reload, list the messages that reload generated.
reload :: Ghci -> IO [Load]
reload ghci = parseLoad <$> execGhci ghci ":reload"
 -}
{- -- | Send @:quit@ and wait for the process to quit.
quitGhci :: Ghci -> IO ()
quitGhci ghci =  do
    interrupt ghci
    handle (\UnexpectedExit{} -> return ()) $ void $ execGhci ghci ":quit"
    -- Be aware that waitForProcess has a race condition, see https://github.com/haskell/process/issues/46.
    -- Therefore just ignore the exception anyway, its probably already terminated.
    ignored $ void $ waitForProcess $ process ghci
 -}
quit :: IO ()
quit = return ()

{- -- | Stop GHCi. Attempts to interrupt and execute @:quit:@, but if that doesn't complete
--   within 5 seconds it just terminates the process.
stopGhci :: Ghci -> IO ()
stopGhci ghci = do
    forkIO $ do
        -- if nicely doesn't work, kill ghci as the process level
        sleep 5
        terminateProcess $ process ghci
    quit ghci
 -}


out = stdout
err = stderr
removePrefix = id
-- Consume from a stream until EOF (return Nothing) or some predicate returns Just
consume :: Stream -> (String -> IO (Maybe a)) -> IO (Maybe a)
consume name finish = do
                let h = if name == Stdout then out else err
                fix $ \rec -> do
                    el <- tryBool isEOFError $ hGetLine h
                    case el of
                        Left _ -> return Nothing
                        Right l -> do
                            whenLoud $ outStrLn $ "%" ++ upper (show name) ++ ": " ++ l
                            res <- finish $ removePrefix l
                            case res of
                                Nothing -> rec
                                Just a -> return $ Just a

consume2 :: String -> (Stream -> String -> IO (Maybe a)) -> IO (a,a)
consume2 msg finish = do
                -- fetch the operations in different threads as hGetLine may block
                -- and can't be aborted by async exceptions, see #154
                res1 <- onceFork $ consume Stdout (finish Stdout)
                res2 <- onceFork $ consume Stderr (finish Stderr)
                res1 <- res1
                res2 <- res2
                case liftM2 (,) res1 res2 of
                    Nothing -> throwIO $ UnexpectedExit "ampersand --daemon" msg
                    Just v -> return v



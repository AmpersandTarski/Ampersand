{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Library for spawning and working with Ghci sessions.
-- _Acknoledgements_: This is mainly copied from Neil Mitchells ghcid.
module Ampersand.Daemon.Daemon.Daemon(
    AmpersandDaemon,
    Stream(..),
    Load(..), load,messages,loaded,
    Severity(..),
    startAmpersandDaemon,
    execStream, 
    exec,
    quit
    ) where

import System.Exit
import System.IO
import System.IO.Error
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
import System.Directory
import System.FilePath
import Ampersand.Daemon.Daemon.Parser
import Ampersand.Daemon.Daemon.Types as T
import Ampersand.Daemon.Daemon.Util
import Ampersand.Basics hiding (Unique, hPutStrLn,readFile,putStrLn)
import Ampersand.Misc

-- | An AmpersandDaemon session. Created with 'startAmpersandDaemon', closed with 'stopAmpersandDaemon'.
--
--   The interactions with a 'Ghci' session must all occur single-threaded,
--   or an error will be raised. The only exception is 'interrupt', which aborts
--   a running computation, or does nothing if no computation is running.

load :: AmpersandDaemon -> [Load]
load = loads . adState
messages :: AmpersandDaemon -> [Load]
messages = filter isMessage . load
loaded :: AmpersandDaemon -> [FilePath]
loaded = loadResults . adState
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
   , loadResults :: [FilePath]
   }
instance Show DaemonState where
  showsPrec _ x
   = showString ("DaemonState: "++show (filesToLoad x) ++ " " ++(show .length . loads $ x))

startAmpersandDaemon 
     :: Options  -- Ampersand options
     -> FilePath  -- ^ Working directory
     -> IO AmpersandDaemon
startAmpersandDaemon opts directory = do
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
    state <- do 
       init <- initialState opts directory
       case init of
         Left msg -> do
           mapM_ putStrLn msg
           exitFailure
         Right s -> pure s  
    
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

stopAmpersandDaemon :: AmpersandDaemon -> IO ()
stopAmpersandDaemon _ = putStrLn "Daemon stopped."
   
initialState :: Options -> FilePath -> IO (Either [String] DaemonState)
initialState opts directory = do
   x <- findRoot directory -- TODO: Read contents of .ampersand file. Fail if not present.
   case x of 
     Left msg   -> return $ Left msg
     Right root -> do 
       (ls,loadedFiles) <- parseProject opts root 
       return $ Right DaemonState
           { filesToLoad = [directory]
           , loads = ls
           , loadResults = loadedFiles
           }
 where findRoot :: FilePath -> IO (Either [String] FilePath)
       findRoot dir = do
         dotAmpersand <- makeAbsolute $ dir </> ".ampersand"
         exists <- doesFileExist dotAmpersand
         if exists 
         then do
             root <- readFile dotAmpersand
             exists' <- doesFileExist root
             return (Right root)  
         else return (Left $ [ "File not found: "++dotAmpersand
                             , "  Your workspace should contain a file called .ampersand. However,"
                             , "  it could not be found. Please provide that file, containing the "
                             , "  name of the top file of your Ampersand project. "
                             ]) 
-- | Execute a command, calling a callback on each response.
--   The callback will be called single threaded.
execStream :: AmpersandDaemon -> String -> (Stream -> String -> IO ()) -> IO ()
execStream = adExec



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



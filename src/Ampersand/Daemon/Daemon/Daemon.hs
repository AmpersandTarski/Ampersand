{-# LANGUAGE RecordWildCards #-}

-- | Library for spawning and working with Ghci sessions.
-- _Acknoledgements_: This is mainly copied from Neil Mitchells ghcid.
module Ampersand.Daemon.Daemon.Daemon(
    AmpersandDaemon(adState),
    DaemonState(loadResults),
    Severity,
    Load,
    load,
    messages,
    loaded,
    startAmpersandDaemon
    ) where

import System.Exit
import System.IO 
import Control.Monad.Extra
import Data.Function
import Data.List.Extra hiding (init)
import Control.Applicative
import System.Directory
import System.FilePath
import Ampersand.Daemon.Daemon.Parser
import Ampersand.Daemon.Daemon.Types as T
import Ampersand.Basics hiding (putStrLn, readFile, init)
import Ampersand.Misc


load :: AmpersandDaemon -> [Load]
load = loads . adState
messages :: AmpersandDaemon -> [Load]
messages = filter isMessage . load
loaded :: AmpersandDaemon -> [FilePath]
loaded = loadResults . adState
data AmpersandDaemon = AmpersandDaemon
    {adState :: DaemonState
    }
--instance Show AmpersandDaemon where
--  show = show . adState     
data DaemonState = DaemonState
   { loads :: [Load]
   , loadResults :: [FilePath]
   }
instance Show DaemonState where
  showsPrec _ x
   = showString $ "DaemonState: #loads = "++(show .length . loads $ x)++" #loadResults = "++(show .length . loadResults $ x)

startAmpersandDaemon 
     :: Options  -- Ampersand options
     -> IO AmpersandDaemon
startAmpersandDaemon opts = do
    state <- do 
       init <- initialState opts
       case init of
         Left msg -> do
           mapM_ putStrLn msg
           exitFailure
         Right s -> pure s  
    
    return AmpersandDaemon
             {adState = state
             }

initialState :: Options -> IO (Either [String] DaemonState)
initialState opts = do
   curDir <- getCurrentDirectory
   x <- findRoot curDir -- TODO: Read contents of .ampersand file. Fail if not present.
   case x of 
     Left msg   -> return $ Left msg
     Right root -> do 
       (ls,loadedFiles) <- parseProject opts root 
       return $ Right DaemonState
           { loads = ls
           , loadResults = nub $ [curDir </> ".ampersand"] ++ loadedFiles
           }
 where findRoot :: FilePath -> IO (Either [String] FilePath)
       findRoot dir = do
         dotAmpersand <- makeAbsolute $ dir </> ".ampersand"
         exists <- doesFileExist dotAmpersand
         if exists 
         then do
             root <- readFile dotAmpersand
             return (Right root)  
         else return (Left $ [ "File not found: "++dotAmpersand
                             , "  Your workspace should contain a file called .ampersand. However,"
                             , "  it could not be found. Please provide that file, containing the "
                             , "  name of the top file of your Ampersand project. "
                             ]) 


{-# LANGUAGE RecordWildCards #-}

-- | Library for spawning and working with Ghci sessions.
-- _Acknoledgements_: This is mainly copied from Neil Mitchells ghcid.
module Ampersand.Daemon.Daemon.Daemon(
    DaemonState(loadResults),
    loads,
    messages,
    loaded,
    startAmpersandDaemon
    ) where

import           Ampersand.Basics
import           Ampersand.Daemon.Daemon.Parser
import           Ampersand.Daemon.Daemon.Types
import           Ampersand.Misc
import qualified RIO.List as L
import qualified RIO.Text as T
import           System.Directory
import           System.FilePath

messages :: DaemonState -> [Load]
messages = filter isMessage . loads
loaded :: DaemonState -> [FilePath]
loaded = loadResults
data DaemonState = DaemonState
    { loads :: [Load]
    , loadResults :: [FilePath]
    }
instance Show DaemonState where
  show x
   = "DaemonState: #loads = "++(show .length . loads $ x)++" #loadResults = "++(show .length . loadResults $ x)

startAmpersandDaemon 
     :: (HasOptions env, HasVerbosity env, HasHandle env) =>
        RIO env DaemonState
startAmpersandDaemon = do
    init <- initialState
    case init of
        Left msg -> exitWith . NoConfigurationFile $ msg
        Right s -> pure s  

initialState :: (HasOptions env, HasVerbosity env, HasHandle env) =>
                RIO env (Either [String] DaemonState)
initialState = do
    env <- ask
    let opts = getOptions env
    curDir <- liftIO $ getCurrentDirectory
    dotAmpersand <- liftIO $ makeAbsolute $ curDir </> daemonConfig opts
    exists <- liftIO $ doesFileExist dotAmpersand
    if exists 
    then do
      content <- readFileUtf8 dotAmpersand
      let files = map T.unpack
                . filter (\fn -> T.length fn > 0) --discard empty lines
                . L.nub                           --discard doubles
                . T.lines $ content
      (ls,loadedFiles) <- do
           xs <- mapM parseProject files
           return ( L.nub . concatMap fst $ xs
                  , L.nub . concatMap snd $ xs
                  )
      return $ Right DaemonState
        { loads = ls
        , loadResults = L.nub $ [dotAmpersand] ++ loadedFiles
        }
    else return . Left $ 
      [ "File not found: "++dotAmpersand
      , "  Your workspace should contain a file called .ampersand. However,"
      , "  it could not be found. Please provide that file, containing the "
      , "  name of the top file(s) of your Ampersand project. One name per line."
      ] 
 
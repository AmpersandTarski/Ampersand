{-# LANGUAGE RecordWildCards #-}

-- | Library for spawning and working with Ghci sessions.
-- _Acknoledgements_: This is mainly copied from Neil Mitchells ghcid.
module Ampersand.Daemon.Daemon.Daemon(
    DaemonState(loadResults),
    Severity,
    Load,
    loads,
    messages,
    loaded,
    startAmpersandDaemon
    ) where

import System.IO.Extra(readFile)
import Data.Function
import Data.List.Extra(nub)
import Control.Applicative
import System.Directory.Extra(getCurrentDirectory,makeAbsolute,doesFileExist)
import System.FilePath
import Ampersand.Daemon.Daemon.Parser
import Ampersand.Daemon.Daemon.Types as T
import Ampersand.Basics hiding (putStrLn)
import Ampersand.Misc


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
     :: Options  -- Ampersand options
     -> IO DaemonState
startAmpersandDaemon opts = do
    state <- do 
       init <- initialState opts
       case init of
         Left msg -> exitWith . NoConfigurationFile $ msg
         Right s -> pure s  
    
    return state

initialState :: Options -> IO (Either [String] DaemonState)
initialState opts = do
   curDir <- getCurrentDirectory
   dotAmpersand <- makeAbsolute $ curDir </> ".ampersand"
   exists <- doesFileExist dotAmpersand
   if exists 
   then do
      content <- readFile dotAmpersand
      let files = filter (\fn -> length fn > 0) --discard empty lines
                . nub                           --discard doubles
                . lines $ content
      (ls,loadedFiles) <- do
           xs <- mapM (parseProject opts) files
           return ( nub . concatMap fst $ xs
                  , nub . concatMap snd $ xs
                  )
      return $ Right DaemonState
        { loads = ls
        , loadResults = nub $ [dotAmpersand] ++ loadedFiles
        }
   else return . Left $ 
      [ "File not found: "++dotAmpersand
      , "  Your workspace should contain a file called .ampersand. However,"
      , "  it could not be found. Please provide that file, containing the "
      , "  name of the top file(s) of your Ampersand project. One name per line."
      ] 
 
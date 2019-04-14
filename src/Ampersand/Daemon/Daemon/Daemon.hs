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
         Left msg -> exitWith . NoConfigurationFile $ msg
         Right s -> pure s  
    
    return AmpersandDaemon
             {adState = state
             }

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
 
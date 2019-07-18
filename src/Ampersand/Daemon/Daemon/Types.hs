{-# LANGUAGE DeriveDataTypeable #-}

-- | The types types that we use in Ghcid
-- _Acknoledgements_: This is mainly copied from Neil Mitchells ghcid.
module Ampersand.Daemon.Daemon.Types(
    Load(..), Severity(..),
    isMessage, isLoading, isLoadConfig
    ) where

import Ampersand.Basics

-- | Severity of messages
data Severity = Warning | Error
    deriving (Show,Eq,Ord,Bounded,Enum,Read,Typeable,Data)

-- | Load messages
data Load
    = -- | A module/file was being loaded.
      Loading
        {loadModule :: String -- ^ The module that was being loaded, @Foo.Bar@.
        ,loadFile :: FilePath -- ^ The file that was being loaded, @Foo/Bar.hs@.
        }
    | -- | An error/warning was emitted.
      Message
        {loadSeverity :: Severity -- ^ The severity of the message, either 'Warning' or 'Error'.
        ,loadFile :: FilePath -- ^ The file the error relates to, @Foo/Bar.hs@.
        ,loadFilePos :: (Int,Int) -- ^ The position in the file, @(line,col)@, both 1-based. Uses @(0,0)@ for no position information.
        ,loadFilePosEnd :: (Int, Int) -- ^ The end position in the file, @(line,col)@, both 1-based. If not present will be the same as 'loadFilePos'.
        ,loadMessage :: [String] -- ^ The message, split into separate lines, may contain ANSI Escape codes.
        }
    | -- | A config file was loaded, usually a .ghci file (GHC 8.2 and above only)
      LoadConfig
        {loadFile :: FilePath -- ^ The file that was being loaded, @.ghci@.
        }
    deriving (Show, Eq, Ord)

-- | Is a 'Load' a 'Message'?
isMessage :: Load -> Bool
isMessage Message{} = True
isMessage _ = False

-- | Is a 'Load' a 'Loading'?
isLoading :: Load -> Bool
isLoading Loading{} = True
isLoading _ = False

-- | Is a 'Load' a 'LoadConfig'?
isLoadConfig :: Load -> Bool
isLoadConfig LoadConfig{} = True
isLoadConfig _ = False

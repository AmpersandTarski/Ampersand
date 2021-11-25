{-# LANGUAGE DeriveDataTypeable #-}

-- | The types types that we use in Ghcid
-- _Acknoledgements_: This is mainly copied from Neil Mitchells ghcid.
module Ampersand.Daemon.Types
  ( Load (..),
    Severity (..),
    isMessage,
    isLoading,
    isLoadConfig,
  )
where

import Ampersand.Basics

-- | Severity of messages
data Severity = Warning | Error
  deriving (Show, Eq, Ord, Bounded, Enum, Read, Typeable, Data)

-- | Load messages
data Load
  = -- | A module/file was being loaded.
    Loading
      { -- | The module that was being loaded, @Foo.Bar@.
        loadModule :: String,
        -- | The file that was being loaded, @Foo/Bar.hs@.
        loadFile :: FilePath
      }
  | -- | An error/warning was emitted.
    Message
      { -- | The severity of the message, either 'Warning' or 'Error'.
        loadSeverity :: Severity,
        -- | The file the error relates to, @Foo/Bar.hs@.
        loadFile :: FilePath,
        -- | The position in the file, @(line,col)@, both 1-based. Uses @(0,0)@ for no position information.
        loadFilePos :: (Int, Int),
        -- | The end position in the file, @(line,col)@, both 1-based. If not present will be the same as 'loadFilePos'.
        loadFilePosEnd :: (Int, Int),
        -- | The message, split into separate lines, may contain ANSI Escape codes.
        loadMessage :: [String]
      }
  | -- | A config file was loaded, usually a .ghci file (GHC 8.2 and above only)
    LoadConfig
      { -- | The file that was being loaded, @.ghci@.
        loadFile :: FilePath
      }
  deriving (Show, Eq, Ord)

-- | Is a 'Load' a 'Message'?
isMessage :: Load -> Bool
isMessage Message {} = True
isMessage _ = False

-- | Is a 'Load' a 'Loading'?
isLoading :: Load -> Bool
isLoading Loading {} = True
isLoading _ = False

-- | Is a 'Load' a 'LoadConfig'?
isLoadConfig :: Load -> Bool
isLoadConfig LoadConfig {} = True
isLoadConfig _ = False

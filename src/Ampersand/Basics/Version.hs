-- | This module contains Version of Ampersand
module Ampersand.Basics.Version
  ( VersionInfo (..),
    appVersion,
    fatal,
  )
where

import Ampersand.Basics.BuildInfo_Generated
import Ampersand.Basics.Exit
import Ampersand.Basics.Prelude
import GHC.Stack
import qualified RIO.Text as T

maxLen :: Int
maxLen = 1500000 -- This trick is to make sure the process is terminated after the error.
-- If the string is too long, it seems that the sentinel `hangs`.
-- But what is too long???

-- | a function to create error message in a structured way, containing the version of Ampersand.
--   It throws an error, showing a (module)name and a number. This makes debugging pretty easy.
fatal :: (HasCallStack) => Text -> a
fatal msg =
  exitWith
    . Fatal
    . T.lines
    $ ( "!             "
          <> shortVersion appVersion
          <> "\n"
          <> lazyCutoff maxLen msg
          <> "\n"
          <> T.pack (prettyCallStack callStack)
      )
  where
    lazyCutoff n txt = case T.uncons txt of
      Nothing -> mempty
      Just (h, tl)
        | T.null tl -> T.cons h mempty
        | n == 0 -> "\n<Ampersand's fatal-mechanism has removed the rest of this error message.>"
        | otherwise -> T.cons h (lazyCutoff (n - 1) tl)
{-# NOINLINE fatal #-}

data VersionInfo = VersionInfo
  { -- | The Ampersand version, including the build timestamp.
    longVersion :: !Text,
    -- | The Ampersand version. The part unto the first space is used as name of the release (appVeyor)
    shortVersion :: !Text,
    -- | Numeric only, including dots as seperation
    numericVersion :: !Text
  }

appVersion :: VersionInfo
appVersion =
  VersionInfo
    { longVersion = short <> ", build time: " <> buildTimeStr,
      shortVersion = short,
      numericVersion = cabalVersionStr
    }
  where
    short = "Ampersand-v" <> cabalVersionStr <> " [" <> gitInfoStr <> "]"

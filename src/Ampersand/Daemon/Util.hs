-- | Utility functions
-- _Acknoledgements_: This is mainly copied from Neil Mitchells ghcid.
module Ampersand.Daemon.Util
  ( takeRemainder,
    allGoodMessage,
    getModTime,
    getShortTime,
  )
where

import Ampersand.Basics
import RIO.Directory
import RIO.Time
import System.Console.ANSI
import System.IO.Error (isDoesNotExistError)

-- | The message to show when no errors have been reported
allGoodMessage :: String
allGoodMessage = setSGRCode [SetColor Foreground Dull Green] ++ "All good" ++ setSGRCode []

-- | Given a 'FilePath' return either 'Nothing' (file does not exist) or 'Just' (the modification time)
getModTime :: FilePath -> IO (Maybe UTCTime)
getModTime file =
  handleJust
    (\e -> if isDoesNotExistError e then Just () else Nothing)
    (\_ -> return Nothing)
    (Just <$> getModificationTime file)

-- | Returns both the amount left (could have been taken more) and the list
takeRemainder :: Int -> [a] -> (Int, [a])
takeRemainder n xs = let ys = take n xs in (n - length ys, ys)

-- | Get the current time in the current timezone in HH:MM:SS format
getShortTime :: IO String
getShortTime = formatTime defaultTimeLocale "%H:%M:%S" <$> getZonedTime

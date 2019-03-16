
-- | Utility functions
-- _Acknoledgements_: This is mainly copied from Neil Mitchells ghcid.
module Ampersand.Daemon.ADaemon.Util(
    dropPrefixRepeatedly,
    takeRemainder,
    outStr, outStrLn,
    ignored,
    allGoodMessage,
    getModTime, getModTimeResolution, getShortTime
    ) where

import Ampersand.Basics hiding (writeFile,putStr,putStrLn)
import Control.Concurrent.Extra
import System.Time.Extra
import System.IO.Unsafe
import System.IO.Extra
import System.FilePath
import System.Console.ANSI
import Data.List.Extra
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import System.IO.Error
import System.Directory
import Control.Exception
import Control.Monad.Extra


-- | Drop a prefix from a list, no matter how many times that prefix is present
dropPrefixRepeatedly :: Eq a => [a] -> [a] -> [a]
dropPrefixRepeatedly []  s = s
dropPrefixRepeatedly pre s = maybe s (dropPrefixRepeatedly pre) $ stripPrefix pre s


{-# NOINLINE lock #-}
lock :: Lock
lock = unsafePerformIO newLock

-- | Output a string with some level of locking
outStr :: String -> IO ()
outStr msg = do
    _ <- evaluate $ length $ show msg
    withLock lock $ putStr msg

outStrLn :: String -> IO ()
outStrLn xs = outStr $ xs ++ "\n"

-- | Ignore all exceptions coming from an action
ignored :: IO () -> IO ()
ignored act = do
    bar <- newBarrier
    _ <- forkFinally act $ const $ signalBarrier bar ()
    waitBarrier bar

-- | The message to show when no errors have been reported
allGoodMessage :: String
allGoodMessage = setSGRCode [SetColor Foreground Dull Green] ++  "All good" ++ setSGRCode []

-- | Given a 'FilePath' return either 'Nothing' (file does not exist) or 'Just' (the modification time)
getModTime :: FilePath -> IO (Maybe UTCTime)
getModTime file = handleJust
    (\e -> if isDoesNotExistError e then Just () else Nothing)
    (\_ -> return Nothing)
    (Just <$> getModificationTime file)

-- | Returns both the amount left (could have been taken more) and the list
takeRemainder :: Int -> [a] -> (Int, [a])
takeRemainder n xs = let ys = take n xs in (n - length ys, ys)

-- | Get the current time in the current timezone in HH:MM:SS format
getShortTime :: IO String
getShortTime = formatTime defaultTimeLocale "%H:%M:%S" <$> getZonedTime


-- | Get the smallest difference that can be reported by two modification times
getModTimeResolution :: IO Seconds
getModTimeResolution = return getModTimeResolutionCache

{-# NOINLINE getModTimeResolutionCache #-}
-- Cache the result so only computed once per run
getModTimeResolutionCache :: Seconds
getModTimeResolutionCache = unsafePerformIO $ withTempDir $ \dir -> do
    let file = dir </> "calibrate.txt"

    -- with 10 measurements can get a bit slow, see Shake issue tracker #451
    -- if it rounds to a second then 1st will be a fraction, but 2nd will be full second
    mtime <- fmap maximum $ forM [1::Int ..3] $ \i -> fmap fst $ duration $ do
        writeFile file $ show i
        t1 <- getModificationTime file
        flip loopM 0 $ \j -> do
            writeFile file $ show (i,j::Int)
            t2 <- getModificationTime file
            return $ if t1 == t2 then Left $ j+1 else Right ()

    putStrLn $ "Longest file modification time lag was " ++ show (ceiling (mtime * 1000::Seconds)::Integer) ++ "ms"
    -- add a little bit of safety, but if it's really quick, don't make it that much slower
    return $ mtime + min 0.1 mtime

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The application entry point
-- _Acknoledgements_: This is mainly copied from Neil Mitchells ghcid.
module Ampersand.Daemon.Daemon(runDaemon) where

import Control.Monad.Extra(forever,when,unless)
import Data.Data
import Data.List.Extra(nubOrd,nubOrdOn,sortOn,nub,partition)
import Data.Maybe
import Data.Ord
import Data.Tuple.Extra(both)
import qualified System.Console.Terminal.Size as Term
import System.Console.ANSI
import System.Environment
import System.Directory.Extra(getCurrentDirectory,withCurrentDirectory)
import System.FilePath
import System.Info
import System.IO.Extra( hSetBuffering, BufferMode(LineBuffering,NoBuffering)
                      , stdout,stderr
                      , putStrLn, putStr
                      , hFlush
                      )

import Ampersand.Basics (ampersandVersionWithoutBuildTimeStr)
import Ampersand.Basics.Exit
import Ampersand.Basics.Prelude
import Ampersand.Daemon.Daemon.Daemon
import Ampersand.Daemon.Daemon.Escape
import Ampersand.Daemon.Daemon.Terminal
import Ampersand.Daemon.Daemon.Types
import Ampersand.Daemon.Daemon.Util
import Ampersand.Daemon.Wait
import Ampersand.Misc


-- | When to colour terminal output.
data ColorMode
    = Never  -- ^ Terminal output will never be coloured.
    | Always -- ^ Terminal output will always be coloured.
    | Auto   -- ^ Terminal output will be coloured if $TERM and stdout appear to support it.
      deriving (Show, Typeable, Data)

data TermSize = TermSize
    {termWidth :: Int
    ,termHeight :: Int
    ,termWrap :: WordWrap
    }

-- | Like 'main', but run with a fake terminal for testing
mainWithTerminal :: Options -> IO TermSize -> ([String] -> IO ()) -> IO ()
mainWithTerminal opts@Options{..} termSize termOutput = goForever
  where goForever = work `catch` errorHandler
        work = forever $ withWindowIcon $ do
            
            -- On certain Cygwin terminals stdout defaults to BlockBuffering
            hSetBuffering stdout LineBuffering
            hSetBuffering stderr NoBuffering
            curDir <- getCurrentDirectory
            verboseLn $ "%OS: " ++ os
            verboseLn $ "%ARCH: " ++ arch
            verboseLn $ "%VERSION: " ++ ampersandVersionWithoutBuildTimeStr
            withCurrentDirectory curDir $ do
                termSize' <- return $ do
                        term <- termSize
                        -- if we write to the final column of the window then it wraps automatically
                        -- so putStrLn width 'x' uses up two lines
                        return $ TermSize
                            (termWidth term - 1)
                            (termHeight term)
                            (termWrap term)

                restyle <- do
                    useStyle <- case Auto of
                        Always -> return True
                        Never -> return False
                        Auto -> hSupportsANSI stdout
                    when useStyle $ do
                        h <- lookupEnv "HSPEC_OPTIONS"
                        when (isNothing h) $ setEnv "HSPEC_OPTIONS" "--color" -- see #87
                    return $ if useStyle then id else map unescape

                maybe withWaiterNotify withWaiterPoll (Nothing) $ \waiter ->
                    runAmpersand opts waiter termSize' (termOutput . restyle)
        errorHandler :: AmpersandExit -> IO()
        errorHandler err = do putStrLn (show err)
                              goForever


runDaemon :: Options -> IO ()
runDaemon opts = mainWithTerminal opts termSize termOutput
    where
        termSize = do
            x <- Term.size
            return $ case x of
                Nothing -> TermSize 80 8 WrapHard
                Just t -> TermSize (Term.width t) (Term.height t) WrapSoft

        termOutput xs = do
            putStr $ concatMap ('\n':) xs
            hFlush stdout -- must flush, since we don't finish with a newline


data Continue = Continue

-- If we return successfully, we restart the whole process
-- Use Continue not () so that inadvertant exits don't restart
runAmpersand :: Options -> Waiter -> IO TermSize -> ([String] -> IO ()) -> IO Continue
runAmpersand opts@Options{..} waiter termSize termOutput = do
    let outputFill :: String -> Maybe (Int, [Load]) -> [String] -> IO ()
        outputFill currTime load' msg' = do
            load'' <- return $ case load' of
                Nothing -> []
                Just (loadedCount, msgs) -> prettyOutput currTime loadedCount $ filter isMessage msgs
            TermSize{..} <- termSize
            let wrap = concatMap (wordWrapE termWidth (termWidth `div` 5) . Esc)
            (termHeight1, msg) <- return $ takeRemainder termHeight $ wrap msg'
            (termHeight2, load''') <- return $ takeRemainder termHeight1 $ wrap load''
            let pad = replicate termHeight2 ""
            let mergeSoft ((Esc x,WrapSoft):(Esc y,q):xs) = mergeSoft $ (Esc (x++y), q) : xs
                mergeSoft ((x,_):xs) = x : mergeSoft xs
                mergeSoft [] = []
            termOutput $ map fromEsc ((if termWrap == WrapSoft then mergeSoft else map fst) $ load''' ++ msg) ++ pad

    nextWait <- waitFiles opts waiter
    aDaemon <- startAmpersandDaemon opts

    when (null . loadResults $ aDaemon) $ do
        exitWith NoFilesToWatch 

    project <- takeFileName <$> getCurrentDirectory

    -- fire, given a waiter, the messages/loaded
    let fire :: ([FilePath] -> IO [String]) -> DaemonState -> IO Continue
        fire nextWait' ad = do
            currTime <- getShortTime
            let no_title = False
            let loadedCount = length (loaded ad)
            verboseLn $ "%MESSAGES: " ++ (show . messages $ ad)
            verboseLn $ "%LOADED: " ++ (show . loaded $ ad)

            let (countErrors, countWarnings) = both sum $ unzip
                    [if loadSeverity == Error then (1::Int,0::Int) else (0,1) | Message{..} <- messages ad, loadMessage /= []]

            unless no_title $ setWindowIcon $
                if countErrors > 0 then IconError else if countWarnings > 0 then IconWarning else IconOK

            let updateTitle extra = unless no_title $ setTitle $ unescape $
                    let f n msg = if n == 0 then "" else show n ++ " " ++ msg ++ ['s' | n > 1]
                    in (if countErrors == 0 && countWarnings == 0 then allGoodMessage ++ ", at " ++ currTime else f countErrors "error" ++
                       (if countErrors >  0 && countWarnings >  0 then ", " else "") ++ f countWarnings "warning") ++
                       " " ++ extra ++ [' ' | extra /= ""] ++ "- " ++ project

            updateTitle ""

            -- order and restrict the messages
            -- nubOrdOn loadMessage because module cycles generate the same message at several different locations
            ordMessages <- do
                let (msgError, msgWarn) = partition ((==) Error . loadSeverity) $ nubOrdOn loadMessage $ messages ad
                -- sort error messages by modtime, so newer edits cause the errors to float to the top - see #153
                errTimes <- sequence [(x,) <$> getModTime x | x <- nubOrd $ map loadFile msgError]
                let f x = lookup (loadFile x) errTimes
                return $ sortOn (Down . f) msgError ++ msgWarn

            outputFill currTime (Just (loadedCount, ordMessages)) []
            when (null . loadResults $ ad) $ exitWith NoFilesToWatch
            
            reason <- nextWait' . nub $ loaded ad ++ (map loadFile . loads $ ad)
            verboseLn $ "%RELOADING: " ++ unwords reason
            return Continue
    fire nextWait aDaemon

-- | Given an available height, and a set of messages to display, show them as best you can.
prettyOutput :: String -> Int -> [Load] -> [String]
prettyOutput currTime loadedCount [] =
    [allGoodMessage ++ " (" ++ show loadedCount ++ " file" ++ ['s' | loadedCount /= 1] ++ ", at " ++ currTime ++ ")"]
prettyOutput _ _ xs = concatMap loadMessage xs


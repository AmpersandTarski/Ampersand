{-# LANGUAGE RecordWildCards, DeriveDataTypeable, TupleSections #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | The application entry point
-- _Acknoledgements_: This is mainly copied from Neil Mitchells ghcid.
module Ampersand.Daemon.Daemon(runDaemon) where

import Control.Exception
import Control.Monad.Extra
import Data.List.Extra
import Data.Maybe
import Data.Ord
import Data.Tuple.Extra
import Ampersand.Daemon.Session
import qualified System.Console.Terminal.Size as Term
import System.Console.CmdArgs
import System.Console.ANSI
import System.Environment
import System.Directory.Extra
import System.Time.Extra
import System.Exit
import System.FilePath
import System.Info
import System.IO.Extra

import Ampersand.Daemon.Daemon.Escape
import Ampersand.Daemon.Daemon.Terminal
import Ampersand.Daemon.Daemon.Util
import Ampersand.Daemon.Daemon.Types
import Ampersand.Daemon.Daemon.Daemon
import Ampersand.Daemon.Wait
import Ampersand.Misc hiding (test)

import Data.Functor
import Prelude
import Ampersand.Basics (fatal,ampersandVersionWithoutBuildTimeStr)

-- | Command line options
data DaemonOptions = DaemonOptions
    {height :: Maybe Int
    ,width :: Maybe Int
    ,topmost :: Bool
    ,no_title :: Bool
    ,project :: String
    ,reload :: [FilePath]
    ,restart :: [FilePath]
    ,directory :: FilePath
    ,outputfile :: [FilePath]
    ,ignoreLoaded :: Bool
    ,poll :: Maybe Seconds
    ,max_messages :: Maybe Int
    ,color :: ColorMode
    ,setup :: [String]
    ,daemon :: Bool
    }
    deriving (Data,Typeable,Show)

-- | When to colour terminal output.
data ColorMode
    = Never  -- ^ Terminal output will never be coloured.
    | Always -- ^ Terminal output will always be coloured.
    | Auto   -- ^ Terminal output will be coloured if $TERM and stdout appear to support it.
      deriving (Show, Typeable, Data)

options :: Mode (CmdArgs DaemonOptions)
options = cmdArgsMode $ DaemonOptions
    {height = Nothing &= help "Number of lines to use (defaults to console height)"
    ,width = Nothing &= name "w" &= help "Number of columns to use (defaults to console width)"
    ,topmost = False &= name "t" &= help "Set window topmost (Windows only)"
    ,no_title = False &= help "Don't update the shell title/icon"
    ,project = "" &= typ "NAME" &= help "Name of the project, defaults to current directory"
    ,restart = [] &= typ "PATH" &= help "Restart the command when the given file or directory contents change (defaults to .ghci and any .cabal file)"
    ,reload = ["."] &= typ "PATH" &= help "Reload when the given file or directory contents change (defaults to none)"
    --,reload = [] &= typ "PATH" &= help "Reload when the given file or directory contents change (defaults to none)"
    ,directory = "." &= typDir &= name "C" &= help "Set the current directory"
    ,outputfile = [] &= typFile &= name "o" &= help "File to write the full output to"
    --,ignoreLoaded = False &= explicit &= name "ignore-loaded" &= help "Keep going if no files are loaded. Requires --reload to be set."
    ,ignoreLoaded = True &= explicit &= name "ignore-loaded" &= help "Keep going if no files are loaded. Requires --reload to be set."
    ,poll = Nothing &= typ "SECONDS" &= opt "0.1" &= explicit &= name "poll" &= help "Use polling every N seconds (defaults to using notifiers)"
    ,max_messages = Nothing &= name "n" &= help "Maximum number of messages to print"
    ,color = Auto &= name "colour" &= name "color" &= opt Always &= typ "always/never/auto" &= help "Color output (defaults to when the terminal supports it)"
    ,setup = [] &= name "setup" &= typ "COMMAND" &= help "Setup commands to pass to ghci on stdin, usually :set <something>"
    ,daemon = False
    } &= verbosity &=
    program "ampersand" &= summary ("Auto reloading ampersand daemon " ++ ampersandVersionWithoutBuildTimeStr)


{-
What happens on various command lines:

Hlint with no .ghci file:
- cabal repl - prompt with Language.Haskell.HLint loaded
- cabal exec ghci Sample.hs - prompt with Sample.hs loaded
- ghci - prompt with nothing loaded
- ghci Sample.hs - prompt with Sample.hs loaded
- stack ghci - prompt with all libraries and Main loaded

Hlint with a .ghci file:
- cabal repl - loads everything twice, prompt with Language.Haskell.HLint loaded
- cabal exec ghci Sample.hs - loads everything first, then prompt with Sample.hs loaded
- ghci - prompt with everything
- ghci Sample.hs - loads everything first, then prompt with Sample.hs loaded
- stack ghci - loads everything first, then prompt with libraries and Main loaded

Warnings:
- cabal repl won't pull in any C files (e.g. hoogle)
- cabal exec ghci won't work with modules that import an autogen Paths module

As a result, we prefer to give users full control with a .ghci file, if available
-}
-- autoOptions :: DaemonOptions -> IO DaemonOptions
-- autoOptions o@DaemonOptions{..}
--     | command /= "" = return $ f [command] []
--     | otherwise = do
--         curdir <- getCurrentDirectory
--         files <- getDirectoryContents "."

--         -- use unsafePerformIO to get nicer pattern matching for logic (read-only operations)
--         let findStack dir = flip catchIOError (const $ return Nothing) $ do
--                 let yaml = dir </> "stack.yaml"
--                 b <- doesFileExist yaml &&^ doesDirectoryExist (dir </> ".stack-work")
--                 return $ if b then Just yaml else Nothing
--         stack <- firstJustM findStack [".",".."] -- stack file might be parent, see #62

--         let cabal = map (curdir </>) $ filter ((==) ".cabal" . takeExtension) files
--         let opts = [] 
--         return $ case () of
--             _ | Just stack <- stack ->
--                 let flags = if null arguments then
--                                 "stack ghci --test --bench" :
--                                 ["--no-load" | ".ghci" `elem` files] ++
--                                 map ("--ghci-options=" ++) opts
--                             else
--                                 "stack exec --test --bench -- ghci" : opts
--                 in f flags $ stack:cabal
--               | ".ghci" `elem` files -> f ("ghci":opts) [curdir </> ".ghci"]
--               | cabal /= [] -> f (if null arguments then "cabal repl":map ("--ghc-options=" ++) opts else "cabal exec -- ghci":opts) cabal
--               | otherwise -> f ("ghci":opts) []
--     where
--         f c r = o{command = unwords $ c ++ map escape arguments, arguments = [], restart = restart ++ r, run = [], test = run ++ test}

--         -- in practice we're not expecting many arguments to have anything funky in them
--         escape x | ' ' `elem` x = "\"" ++ x ++ "\""
--                  | otherwise = x

data TermSize = TermSize
    {termWidth :: Int
    ,termHeight :: Int
    ,termWrap :: WordWrap
    }

-- | Like 'main', but run with a fake terminal for testing
mainWithTerminal :: Options -> IO TermSize -> ([String] -> IO ()) -> IO ()
mainWithTerminal opts termSize termOutput =
    handle (\(UnexpectedExit cmd _) -> do putStrLn $ "Command \"" ++ cmd ++ "\" exited unexpectedly"; exitFailure) $
        forever $ withWindowIcon $ withSession $ \session -> do
            setVerbosity Normal -- undo any --verbose flags

            -- On certain Cygwin terminals stdout defaults to BlockBuffering
            hSetBuffering stdout LineBuffering
            hSetBuffering stderr NoBuffering
            origDir <- getCurrentDirectory
            dOpts <- cmdArgsRun options
            whenLoud $ do
                outStrLn $ "%OS: " ++ os
                outStrLn $ "%ARCH: " ++ arch
                outStrLn $ "%VERSION: " ++ ampersandVersionWithoutBuildTimeStr
            withCurrentDirectory (directory dOpts) $ do
         --       dOpts <- autoOptions dOpts
                dOpts <- return $ dOpts{restart = nubOrd $ (origDir </> ".ampersand") : restart dOpts, reload = nubOrd $ reload dOpts}
                when (topmost dOpts) terminalTopmost

                termSize <- return $ case (width dOpts, height dOpts) of
                    (Just w, Just h) -> return $ TermSize w h WrapHard
                    (w, h) -> do
                        term <- termSize
                        -- if we write to the final column of the window then it wraps automatically
                        -- so putStrLn width 'x' uses up two lines
                        return $ TermSize
                            (fromMaybe (pred $ termWidth term) w)
                            (fromMaybe (termHeight term) h)
                            (if isJust w then WrapHard else termWrap term)

                restyle <- do
                    useStyle <- case color dOpts of
                        Always -> return True
                        Never -> return False
                        Auto -> hSupportsANSI stdout
                    when useStyle $ do
                        h <- lookupEnv "HSPEC_OPTIONS"
                        when (isNothing h) $ setEnv "HSPEC_OPTIONS" "--color" -- see #87
                    return $ if useStyle then id else map unescape

                maybe withWaiterNotify withWaiterPoll (poll dOpts) $ \waiter ->
                    runAmpersand opts session waiter termSize (termOutput . restyle) dOpts



runDaemon :: Options -> IO ()
runDaemon opts = mainWithTerminal opts termSize termOutput
    where
        termSize = do
            x <- Term.size
            return $ case x of
                Nothing -> TermSize 80 8 WrapHard
                Just t -> TermSize (Term.width t) (Term.height t) WrapSoft

        termOutput xs = do
            outStr $ concatMap ('\n':) xs
            hFlush stdout -- must flush, since we don't finish with a newline


data Continue = Continue

-- If we return successfully, we restart the whole process
-- Use Continue not () so that inadvertant exits don't restart
runAmpersand :: Options -> Session -> Waiter -> IO TermSize -> ([String] -> IO ()) -> DaemonOptions -> IO Continue
runAmpersand opts session waiter termSize termOutput dopts@DaemonOptions{..} = do
    let limitMessages = maybe id (take . max 1) max_messages

    let outputFill :: String -> Maybe (Int, [Load]) -> [String] -> IO ()
        outputFill currTime load' msg' = do
            load <- return $ case load' of
                Nothing -> []
                Just (loadedCount, msgs) -> prettyOutput currTime loadedCount $ filter isMessage msgs
            TermSize{..} <- termSize
            let wrap = concatMap (wordWrapE termWidth (termWidth `div` 5) . Esc)
            (termHeight, msg) <- return $ takeRemainder termHeight $ wrap msg'
            (termHeight, load) <- return $ takeRemainder termHeight $ wrap load
            let pad = replicate termHeight ""
            let mergeSoft ((Esc x,WrapSoft):(Esc y,q):xs) = mergeSoft $ (Esc (x++y), q) : xs
                mergeSoft ((x,_):xs) = x : mergeSoft xs
                mergeSoft [] = []
            termOutput $ map fromEsc ((if termWrap == WrapSoft then mergeSoft else map fst) $ load ++ msg) ++ pad

    when (ignoreLoaded && null reload) $ do
        putStrLn "--reload must be set when using --ignore-loaded"
        exitFailure

    nextWait <- waitFiles waiter
    aDaemon <- sessionStart opts session "cmd ampersand --daemon" $
        setup

    when (null (load aDaemon) && not ignoreLoaded) $ do
        putStrLn $ "\nNo files loaded, Ampersand daemon is not working properly.\n"
        exitFailure

    restart <- return $ nubOrd $ restart ++ [x | LoadConfig x <- load aDaemon]
    -- Note that we capture restarting items at this point, not before invoking the command
    -- The reason is some restart items may be generated by the command itself
    restartTimes <- mapM getModTime restart

    project <- if project /= "" then return project else takeFileName <$> getCurrentDirectory

    -- fire, given a waiter, the messages/loaded
    let fire :: ([FilePath] -> IO [String]) -> AmpersandDaemon -> IO Continue
        fire nextWait ad = do
            currTime <- getShortTime
            let loadedCount = length (loaded ad)
            whenLoud $ do
                outStrLn $ "%MESSAGES: " ++ (show . messages $ ad)
                outStrLn $ "%LOADED: " ++ (show . loaded $ ad)
         --   outStrLn $ "Fall asleep..."
         --   sleep 5
         --   outStrLn $ "... Woke up"
            let (countErrors, countWarnings) = both sum $ unzip
                    [if loadSeverity == Error then (1,0) else (0,1) | m@Message{..} <- messages ad, loadMessage /= []]
                

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
                let (msgError, msgWarn) = partition ((==) Error . loadSeverity) $ nubOrdOn loadMessage $ filter isMessage (messages ad)
                -- sort error messages by modtime, so newer edits cause the errors to float to the top - see #153
                errTimes <- sequence [(x,) <$> getModTime x | x <- nubOrd $ map loadFile msgError]
                let f x = lookup (loadFile x) errTimes
                return $ sortOn (Down . f) msgError ++ msgWarn

            outputFill currTime (Just (loadedCount, ordMessages)) []
            forM_ outputfile $ \file ->
                writeFile file $
                    if takeExtension file == ".json" then
                        showJSON [("loaded",map jString (loaded ad)),("messages",map jMessage $ filter isMessage (messages ad))]
                    else
                        unlines $ map unescape $ prettyOutput currTime loadedCount $ limitMessages ordMessages
            when (null (loaded ad) && not ignoreLoaded) $ do
                putStrLn "No files loaded, nothing to wait for. Fix the last error and restart."
                exitFailure
            
            reason <- nextWait $ restart ++ reload ++ loaded ad
            whenLoud $ outStrLn $ "%RELOADING: " ++ unwords reason
            restartTimes2 <- mapM getModTime restart
            let restartChanged = [s | (False, s) <- zip (zipWith (==) restartTimes restartTimes2) restart]
            currTime <- getShortTime
            return Continue
    fire nextWait aDaemon


-- | Given an available height, and a set of messages to display, show them as best you can.
prettyOutput :: String -> Int -> [Load] -> [String]
prettyOutput currTime loadedCount [] =
    [allGoodMessage ++ " (" ++ show loadedCount ++ " file" ++ ['s' | loadedCount /= 1] ++ ", at " ++ currTime ++ ")"]
prettyOutput _ _ xs = concatMap loadMessage xs


showJSON :: [(String, [String])] -> String
showJSON xs = unlines $ concat $
    [ ((if i == 0 then "{" else ",") ++ jString a ++ ":") :
      ["  " ++ (if j == 0 then "[" else ",") ++ b | (j,b) <- zipFrom 0 bs] ++
      [if null bs then "  []" else "  ]"]
    | (i,(a,bs)) <- zipFrom 0 xs] ++
    [["}"]]

jString :: String -> String
jString x = "\"" ++ escapeJSON x ++ "\""

jMessage :: Load -> String
jMessage Message{..} = jDict $
    [("severity",jString $ show loadSeverity)
    ,("file",jString loadFile)] ++
    [("start",pair loadFilePos) | loadFilePos /= (0,0)] ++
    [("end", pair loadFilePosEnd) | loadFilePos /= loadFilePosEnd] ++
    [("message", jString $ intercalate "\n" loadMessage)]
    where pair (a,b) = "[" ++ show a ++ "," ++ show b ++ "]"
jMessage _ = fatal "Not a message: "
jDict :: [(String, String)] -> String
jDict xs = "{" ++ intercalate ", " [jString a ++ ":" ++ b | (a,b) <- xs] ++ "}"

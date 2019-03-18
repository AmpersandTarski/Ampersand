{-# LANGUAGE RecordWildCards, DeriveDataTypeable, TupleSections #-}
{-# OPTIONS_GHC -fno-cse #-}

-- | The application entry point
-- _Acknoledgements_: This is mainly copied from Neil Mitchells ghcid.
module Ampersand.Daemon.Daemon(runDaemon) where
--module Ampersand.Daemon.Daemon(main, mainWithTerminal, TermSize(..), WordWrap(..)) where

import Control.Exception
import System.IO.Error
import Control.Monad.Extra
import Data.List.Extra
import Data.Maybe
import Data.Ord
import Data.Tuple.Extra
import Data.Version
import Ampersand.Daemon.Session
import qualified System.Console.Terminal.Size as Term
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit
import System.Console.ANSI
import System.Environment
import System.Directory.Extra
import System.Time.Extra
import System.Exit
import System.FilePath
import System.Info
import System.IO.Extra

import Ampersand.Daemon.Paths
import Ampersand.Daemon.Daemon.Escape
import Ampersand.Daemon.Daemon.Terminal
import Ampersand.Daemon.Daemon.Util
import Ampersand.Daemon.Daemon.Types
import Ampersand.Daemon.Wait

import Data.Functor
import Prelude


-- | Command line options
data Options = Options
    {command :: String
    ,arguments :: [String]
    ,test :: [String]
    ,run :: [String]
    ,warnings :: Bool
    ,no_status :: Bool
    ,height :: Maybe Int
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
    }
    deriving (Data,Typeable,Show)

-- | When to colour terminal output.
data ColorMode
    = Never  -- ^ Terminal output will never be coloured.
    | Always -- ^ Terminal output will always be coloured.
    | Auto   -- ^ Terminal output will be coloured if $TERM and stdout appear to support it.
      deriving (Show, Typeable, Data)

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {command = "" &= name "c" &= typ "COMMAND" &= help "Command to run (defaults to ghci or cabal repl)"
    ,arguments = [] &= args &= typ "MODULE"
    ,test = [] &= name "T" &= typ "EXPR" &= help "Command to run after successful loading"
    ,run = [] &= name "r" &= typ "EXPR" &= opt "main" &= help "Command to run after successful loading (defaults to main)"
    ,warnings = False &= name "W" &= help "Allow tests to run even with warnings"
    ,no_status = False &= name "S" &= help "Suppress status messages"
    ,height = Nothing &= help "Number of lines to use (defaults to console height)"
    ,width = Nothing &= name "w" &= help "Number of columns to use (defaults to console width)"
    ,topmost = False &= name "t" &= help "Set window topmost (Windows only)"
    ,no_title = False &= help "Don't update the shell title/icon"
    ,project = "" &= typ "NAME" &= help "Name of the project, defaults to current directory"
    ,restart = [] &= typ "PATH" &= help "Restart the command when the given file or directory contents change (defaults to .ghci and any .cabal file)"
    ,reload = [] &= typ "PATH" &= help "Reload when the given file or directory contents change (defaults to none)"
    ,directory = "." &= typDir &= name "C" &= help "Set the current directory"
    ,outputfile = [] &= typFile &= name "o" &= help "File to write the full output to"
    ,ignoreLoaded = False &= explicit &= name "ignore-loaded" &= help "Keep going if no files are loaded. Requires --reload to be set."
    ,poll = Nothing &= typ "SECONDS" &= opt "0.1" &= explicit &= name "poll" &= help "Use polling every N seconds (defaults to using notifiers)"
    ,max_messages = Nothing &= name "n" &= help "Maximum number of messages to print"
    ,color = Auto &= name "colour" &= name "color" &= opt Always &= typ "always/never/auto" &= help "Color output (defaults to when the terminal supports it)"
    ,setup = [] &= name "setup" &= typ "COMMAND" &= help "Setup commands to pass to ghci on stdin, usually :set <something>"
    } &= verbosity &=
    program "ghcid" &= summary ("Auto reloading GHCi daemon v" ++ showVersion version)


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
autoOptions :: Options -> IO Options
autoOptions o@Options{..}
    | command /= "" = return $ f [command] []
    | otherwise = do
        curdir <- getCurrentDirectory
        files <- getDirectoryContents "."

        -- use unsafePerformIO to get nicer pattern matching for logic (read-only operations)
        let findStack dir = flip catchIOError (const $ return Nothing) $ do
                let yaml = dir </> "stack.yaml"
                b <- doesFileExist yaml &&^ doesDirectoryExist (dir </> ".stack-work")
                return $ if b then Just yaml else Nothing
        stack <- firstJustM findStack [".",".."] -- stack file might be parent, see #62

        let cabal = map (curdir </>) $ filter ((==) ".cabal" . takeExtension) files
        let opts = [] 
        return $ case () of
            _ | Just stack <- stack ->
                let flags = if null arguments then
                                "stack ghci --test --bench" :
                                ["--no-load" | ".ghci" `elem` files] ++
                                map ("--ghci-options=" ++) opts
                            else
                                "stack exec --test --bench -- ghci" : opts
                in f flags $ stack:cabal
              | ".ghci" `elem` files -> f ("ghci":opts) [curdir </> ".ghci"]
              | cabal /= [] -> f (if null arguments then "cabal repl":map ("--ghc-options=" ++) opts else "cabal exec -- ghci":opts) cabal
              | otherwise -> f ("ghci":opts) []
    where
        f c r = o{command = unwords $ c ++ map escape arguments, arguments = [], restart = restart ++ r, run = [], test = run ++ test}

        -- in practice we're not expecting many arguments to have anything funky in them
        escape x | ' ' `elem` x = "\"" ++ x ++ "\""
                 | otherwise = x

-- | Use arguments from .ghcid if present
withGhcidArgs :: IO a -> IO a
withGhcidArgs act = do
    b <- doesFileExist ".ghcid"
    if not b then act else do
        extra <- concatMap splitArgs . lines <$> readFile' ".ghcid"
        orig <- getArgs
        withArgs (extra ++ orig) act


data TermSize = TermSize
    {termWidth :: Int
    ,termHeight :: Int
    ,termWrap :: WordWrap
    }

-- | Like 'main', but run with a fake terminal for testing
mainWithTerminal :: IO TermSize -> ([String] -> IO ()) -> IO ()
mainWithTerminal termSize termOutput =
    handle (\(UnexpectedExit cmd _) -> do putStrLn $ "Command \"" ++ cmd ++ "\" exited unexpectedly"; exitFailure) $
        forever $ withWindowIcon $ withSession $ \session -> do
            setVerbosity Normal -- undo any --verbose flags

            -- On certain Cygwin terminals stdout defaults to BlockBuffering
            hSetBuffering stdout LineBuffering
            hSetBuffering stderr NoBuffering
            origDir <- getCurrentDirectory
            opts <- withGhcidArgs $ cmdArgsRun options
            whenLoud $ do
                outStrLn $ "%OS: " ++ os
                outStrLn $ "%ARCH: " ++ arch
                outStrLn $ "%VERSION: " ++ showVersion version
            withCurrentDirectory (directory opts) $ do
                opts <- autoOptions opts
                opts <- return $ opts{restart = nubOrd $ (origDir </> ".ghcid") : restart opts, reload = nubOrd $ reload opts}
                when (topmost opts) terminalTopmost

                termSize <- return $ case (width opts, height opts) of
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
                    useStyle <- case color opts of
                        Always -> return True
                        Never -> return False
                        Auto -> hSupportsANSI stdout
                    when useStyle $ do
                        h <- lookupEnv "HSPEC_OPTIONS"
                        when (isNothing h) $ setEnv "HSPEC_OPTIONS" "--color" -- see #87
                    return $ if useStyle then id else map unescape

                maybe withWaiterNotify withWaiterPoll (poll opts) $ \waiter ->
                    runGhcid session waiter termSize (termOutput . restyle) opts



runDaemon :: IO ()
runDaemon = mainWithTerminal termSize termOutput
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
runGhcid :: Session -> Waiter -> IO TermSize -> ([String] -> IO ()) -> Options -> IO Continue
runGhcid session waiter termSize termOutput opts@Options{..} = do
    let limitMessages = maybe id (take . max 1) max_messages

    let outputFill :: String -> Maybe (Int, [Load]) -> [String] -> IO ()
        outputFill currTime load msg = do
            load <- return $ case load of
                Nothing -> []
                Just (loadedCount, msgs) -> prettyOutput currTime loadedCount $ filter isMessage msgs
            TermSize{..} <- termSize
            let wrap = concatMap (wordWrapE termWidth (termWidth `div` 5) . Esc)
            (termHeight, msg) <- return $ takeRemainder termHeight $ wrap msg
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
    (messages, loaded) <- sessionStart session command $
        setup

    when (null loaded && not ignoreLoaded) $ do
        putStrLn $ "\nNo files loaded, GHCi is not working properly.\nCommand: " ++ command
        exitFailure

    restart <- return $ nubOrd $ restart ++ [x | LoadConfig x <- messages]
    -- Note that we capture restarting items at this point, not before invoking the command
    -- The reason is some restart items may be generated by the command itself
    restartTimes <- mapM getModTime restart

    project <- if project /= "" then return project else takeFileName <$> getCurrentDirectory

    -- fire, given a waiter, the messages/loaded
    let fire nextWait (messages, loaded) = do
            currTime <- getShortTime
            let loadedCount = length loaded
            whenLoud $ do
                outStrLn $ "%MESSAGES: " ++ show messages
                outStrLn $ "%LOADED: " ++ show loaded

            let (countErrors, countWarnings) = both sum $ unzip
                    [if loadSeverity == Error then (1,0) else (0,1) | m@Message{..} <- messages, loadMessage /= []]
            test <- return $
                if null test || countErrors /= 0 || (countWarnings /= 0 && not warnings) then Nothing
                else Just $ intercalate "\n" test

            unless no_title $ setWindowIcon $
                if countErrors > 0 then IconError else if countWarnings > 0 then IconWarning else IconOK

            let updateTitle extra = unless no_title $ setTitle $ unescape $
                    let f n msg = if n == 0 then "" else show n ++ " " ++ msg ++ ['s' | n > 1]
                    in (if countErrors == 0 && countWarnings == 0 then allGoodMessage ++ ", at " ++ currTime else f countErrors "error" ++
                       (if countErrors >  0 && countWarnings >  0 then ", " else "") ++ f countWarnings "warning") ++
                       " " ++ extra ++ [' ' | extra /= ""] ++ "- " ++ project

            updateTitle $ if isJust test then "(running test)" else ""

            -- order and restrict the messages
            -- nubOrdOn loadMessage because module cycles generate the same message at several different locations
            ordMessages <- do
                let (msgError, msgWarn) = partition ((==) Error . loadSeverity) $ nubOrdOn loadMessage $ filter isMessage messages
                -- sort error messages by modtime, so newer edits cause the errors to float to the top - see #153
                errTimes <- sequence [(x,) <$> getModTime x | x <- nubOrd $ map loadFile msgError]
                let f x = lookup (loadFile x) errTimes
                return $ sortOn (Down . f) msgError ++ msgWarn

            outputFill currTime (Just (loadedCount, ordMessages)) ["Running test..." | isJust test]
            forM_ outputfile $ \file ->
                writeFile file $
                    if takeExtension file == ".json" then
                        showJSON [("loaded",map jString loaded),("messages",map jMessage $ filter isMessage messages)]
                    else
                        unlines $ map unescape $ prettyOutput currTime loadedCount $ limitMessages ordMessages
            when (null loaded && not ignoreLoaded) $ do
                putStrLn "No files loaded, nothing to wait for. Fix the last error and restart."
                exitFailure
            whenJust test $ \t -> do
                whenLoud $ outStrLn $ "%TESTING: " ++ t
                sessionExecAsync session t $ \stderr -> do
                    whenLoud $ outStrLn "%TESTING: Completed"
                    hFlush stdout -- may not have been a terminating newline from test output
                    if "*** Exception: " `isPrefixOf` stderr then do
                        updateTitle "(test failed)"
                        setWindowIcon IconError
                     else do
                        updateTitle "(test done)"
                        whenNormal $ outStrLn "\n...done"

            reason <- nextWait $ restart ++ reload ++ loaded
            whenLoud $ outStrLn $ "%RELOADING: " ++ unwords reason
            restartTimes2 <- mapM getModTime restart
            let restartChanged = [s | (False, s) <- zip (zipWith (==) restartTimes restartTimes2) restart]
            currTime <- getShortTime
            if not $ null restartChanged then do
                -- exit cleanly, since the whole thing is wrapped in a forever
                unless no_status $ outputFill currTime Nothing $ "Restarting..." : map ("  " ++) restartChanged
                return Continue
            else do
                unless no_status $ outputFill currTime Nothing $ "Reloading..." : map ("  " ++) reason
                nextWait <- waitFiles waiter
                fire nextWait =<< sessionReload session

    fire nextWait (messages, loaded)


-- | Given an available height, and a set of messages to display, show them as best you can.
prettyOutput :: String -> Int -> [Load] -> [String]
prettyOutput currTime loadedCount [] =
    [allGoodMessage ++ " (" ++ show loadedCount ++ " module" ++ ['s' | loadedCount /= 1] ++ ", at " ++ currTime ++ ")"]
prettyOutput _ _ xs = concatMap loadMessage xs


showJSON :: [(String, [String])] -> String
showJSON xs = unlines $ concat $
    [ ((if i == 0 then "{" else ",") ++ jString a ++ ":") :
      ["  " ++ (if j == 0 then "[" else ",") ++ b | (j,b) <- zipFrom 0 bs] ++
      [if null bs then "  []" else "  ]"]
    | (i,(a,bs)) <- zipFrom 0 xs] ++
    [["}"]]

jString x = "\"" ++ escapeJSON x ++ "\""

jMessage Message{..} = jDict $
    [("severity",jString $ show loadSeverity)
    ,("file",jString loadFile)] ++
    [("start",pair loadFilePos) | loadFilePos /= (0,0)] ++
    [("end", pair loadFilePosEnd) | loadFilePos /= loadFilePosEnd] ++
    [("message", jString $ intercalate "\n" loadMessage)]
    where pair (a,b) = "[" ++ show a ++ "," ++ show b ++ "]"

jDict xs = "{" ++ intercalate ", " [jString a ++ ":" ++ b | (a,b) <- xs] ++ "}"

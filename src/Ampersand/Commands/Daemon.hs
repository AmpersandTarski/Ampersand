{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- | The application entry point
-- _Acknoledgements_: This is mainly copied from Neil Mitchells ghcid.
module Ampersand.Commands.Daemon(runDaemon) where

import           Ampersand.Basics
import           Ampersand.Daemon.Daemon
import           Ampersand.Daemon.Escape
import           Ampersand.Daemon.Terminal
import           Ampersand.Daemon.Types
import           Ampersand.Daemon.Util
import           Ampersand.Daemon.Wait
import           Ampersand.Misc.HasClasses
import           Ampersand.Types.Config
import           Data.Tuple.Extra(both)
import qualified RIO.List as L
import qualified RIO.Text as T
import           System.Console.ANSI (hSupportsANSI,setTitle)
import qualified System.Console.Terminal.Size as Term
import           System.Directory(getCurrentDirectory,setCurrentDirectory)
import           System.Environment
import           System.FilePath
import           System.Info

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
mainWithTerminal :: IO TermSize -> ([String] -> RIO (ExtendedRunner DaemonOpts) ()) -> RIO (ExtendedRunner DaemonOpts) ()
mainWithTerminal termSize termOutput = goForever
  where goForever = work `catch` errorHandler
        work = forever $ withWindowIcon $ do
            
            -- On certain Cygwin terminals stdout defaults to BlockBuffering
            hSetBuffering stdout LineBuffering
            hSetBuffering stderr NoBuffering
            curDir <- liftIO $ getCurrentDirectory
            logDebug $ "%OS: " <> display (T.pack os)
            logDebug $ "%ARCH: " <> display (T.pack arch)
            logDebug $ "%VERSION: " <> display ampersandVersionWithoutBuildTimeStr
            env <- ask
            withCurrentDirectory curDir $ do
                termSize' <- liftIO $ return $ do
                        term <- termSize
                        -- if we write to the final column of the window then it wraps automatically
                        -- so logInfo width 'x' uses up two lines.
                        -- Logging *always* ends the line, so we need to substract 1 of the hight as well.
                        return $ TermSize
                            (termWidth term - 1)
                            (termHeight term -1)
                            (termWrap term)

                restyle <- liftIO $ do
                    useStyle <- case Auto of
                        Always -> return True
                        Never -> return False
                        Auto -> liftIO $ hSupportsANSI stdout
                    when useStyle $ liftIO $ do
                        h <- lookupEnv "HSPEC_OPTIONS"
                        when (isNothing h) $ setEnv "HSPEC_OPTIONS" "--color" -- see #87
                    return $ if useStyle then id else map unescape

                withWaiterNotify env $ \waiter ->
                    runRIO env $ do 
                       runAmpersand env waiter termSize' (termOutput . restyle)

        errorHandler :: AmpersandExit -> RIO (ExtendedRunner DaemonOpts) ()
        errorHandler (err :: AmpersandExit) = do 
              logError (displayShow err)
              goForever

runDaemon :: RIO (ExtendedRunner DaemonOpts) ()
runDaemon = mainWithTerminal termSize termOutput
    where
        termSize = do
            x <- Term.size
            return $ case x of
                Nothing -> TermSize 80 8 WrapHard
                Just t -> TermSize (Term.width t) (Term.height t) WrapSoft

        termOutput :: (HasLogFunc env) => [String] -> RIO env ()
        termOutput xs = do
            mapM_ logInfo $ map (display . T.pack) xs


data Continue = Continue

-- If we return successfully, we restart the whole process
-- Use Continue not () so that inadvertant exits don't restart
runAmpersand :: (HasRunner env, HasDaemonOpts env) 
   => env -> Waiter -> IO TermSize -> ([String] -> RIO env ()) -> RIO env Continue
runAmpersand app waiter termSize termOutput = do
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
            runRIO app $ do 
               termOutput $ map fromEsc ((if termWrap == WrapSoft then mergeSoft else map fst) $ load''' ++ msg) ++ pad


    nextWait <- waitFiles waiter
    aDaemon <- startAmpersandDaemon

    when (null . loadResults $ aDaemon) $ do
        exitWith NoFilesToWatch 

    project <- takeFileName <$> (liftIO $ getCurrentDirectory)

    -- fire, given a waiter, the messages/loaded
    let fire :: (HasLogFunc env) =>
                ([FilePath] -> RIO env [String]) -> DaemonState -> RIO env Continue
        fire nextWait' ad = do
            currTime <- liftIO $ getShortTime
            let no_title = False
            let loadedCount = length (loaded ad)
            logDebug $ "%MESSAGES: " <> (displayShow . messages $ ad)
            logDebug $ "%LOADED: " <> (displayShow . loaded $ ad)

            let (countErrors, countWarnings) = both sum $ L.unzip
                    [if loadSeverity == Error then (1::Int,0::Int) else (0,1) | Message{..} <- messages ad, loadMessage /= []]

            liftIO $ unless no_title $ setWindowIcon $
                if countErrors > 0 then IconError else if countWarnings > 0 then IconWarning else IconOK

            let updateTitle extra = unless no_title $ setTitle $ unescape $
                    let f n msg = if n == 0 then "" else show n ++ " " ++ msg ++ ['s' | n > 1]
                    in (if countErrors == 0 && countWarnings == 0 then allGoodMessage ++ ", at " ++ currTime else f countErrors "error" ++
                       (if countErrors >  0 && countWarnings >  0 then ", " else "") ++ f countWarnings "warning") ++
                       " " ++ extra ++ [' ' | extra /= ""] ++ "- " ++ project

            liftIO $ updateTitle ""

            -- order and restrict the messages
            -- nubOrdOn loadMessage because module cycles generate the same message at several different locations
            ordMessages <- do
                let (msgError, msgWarn) = L.partition ((==) Error . loadSeverity) $ nubOrdOn loadMessage $ messages ad
                -- sort error messages by modtime, so newer edits cause the errors to float to the top - see #153
                errTimes <- liftIO $ sequence [(x,) <$> getModTime x | x <- nubOrd $ map loadFile msgError]
                let f x = lookup (loadFile x) errTimes
                return $ L.sortOn (Down . f) msgError ++ msgWarn

            liftIO $ outputFill currTime (Just (loadedCount, ordMessages)) []
            when (null . loadResults $ ad) $ exitWith NoFilesToWatch
            
            reason <- nextWait' . L.nub $ loaded ad ++ (map loadFile . loads $ ad)
            logDebug $ "%RELOADING: " <> display (T.pack $ unwords reason)
            return Continue
    runRIO app $ fire nextWait aDaemon

-- | Given an available height, and a set of messages to display, show them as best you can.
prettyOutput :: String -> Int -> [Load] -> [String]
prettyOutput currTime loadedCount [] =
    [allGoodMessage ++ " (" ++ show loadedCount ++ " file" ++ ['s' | loadedCount /= 1] ++ ", at " ++ currTime ++ ")"]
prettyOutput _ _ xs = concatMap loadMessage xs


-- below are some functions taken from Data.List.Extra (extra package)
-- | A version of 'nubOrd' which operates on a portion of the value.
--
-- > nubOrdOn length ["a","test","of","this"] == ["a","test","of"]
nubOrdOn :: Ord b => (a -> b) -> [a] -> [a]
nubOrdOn f = map snd . nubOrdBy (compare `on` fst) . map (f &&& id)

-- | A version of 'nubOrd' with a custom predicate.
--
-- > nubOrdBy (compare `on` length) ["a","test","of","this"] == ["a","test","of"]
nubOrdBy :: (a -> a -> Ordering) -> [a] -> [a]
nubOrdBy cmp xs = f E xs
    where f seen rest = 
            case rest of 
              [] -> []
              h:tl | memberRB cmp h seen -> f seen tl
                   | otherwise -> h : f (insertRB cmp h seen) tl

---------------------------------------------------------------------
-- OKASAKI RED BLACK TREE
-- Taken from https://www.cs.kent.ac.uk/people/staff/smk/redblack/Untyped.hs

data Color = R | B deriving Show
data RB a = E | T Color (RB a) a (RB a) deriving Show

{- Insertion and membership test as by Okasaki -}
insertRB :: (a -> a -> Ordering) -> a -> RB a -> RB a
insertRB cmp x s =
    T B a z b
    where
    T _ a z b = ins s
    ins E = T R E x E
    ins s'@(T B a' y b') = case cmp x y of
        LT -> balance (ins a') y b'
        GT -> balance a' y (ins b')
        EQ -> s'
    ins s'@(T R a' y b') = case cmp x y of
        LT -> T R (ins a') y b'
        GT -> T R a' y (ins b')
        EQ -> s'

memberRB :: (a -> a -> Ordering) -> a -> RB a -> Bool
memberRB cmp x rb = 
   case rb of
     E           -> False
     (T _ a y b) -> case cmp x y of
                        LT -> memberRB cmp x a
                        GT -> memberRB cmp x b
                        EQ -> True

{- balance: first equation is new,
   to make it work with a weaker invariant -}
balance :: RB a -> a -> RB a -> RB a
balance (T R a x b) y (T R c z d) = T R (T B a x b) y (T B c z d)
balance (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance a x b = T B a x b


-- Copied from 

-- | Set the current directory, perform an operation, then change back.
--   Remember that the current directory is a global variable, so calling this function
--   multithreaded is almost certain to go wrong. Avoid changing the current directory if you can.
--
-- > withTempDir $ \dir -> do writeFile (dir </> "foo.txt") ""; withCurrentDirectory dir $ doesFileExist "foo.txt"
withCurrentDirectory :: FilePath -> RIO env a -> RIO env a
withCurrentDirectory dir act =
    bracket' getCurrentDirectory setCurrentDirectory $ const ( do
        liftIO $ setCurrentDirectory dir
        act)
  where 
    bracket'
            :: IO a         -- ^ computation to run first (\"acquire resource\")
            -> (a -> IO b)  -- ^ computation to run last (\"release resource\")
            -> (a -> RIO env c)  -- ^ computation to run in-between
            -> RIO env c         -- returns the value from the in-between computation
    bracket' before after thing =
        mask $ \restore -> do
            a <- liftIO before
            r <- restore (thing a) `onException` (liftIO $ after a)
            _ <- liftIO $ after a
            return r
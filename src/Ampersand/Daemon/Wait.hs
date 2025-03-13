{-# LANGUAGE ScopedTypeVariables #-}

-- | Use 'withWaiterPoll' or 'withWaiterNotify' to create a 'Waiter' object,
--   then access it (single-threaded) by using 'waitFiles'.
-- _Acknoledgements_: This is mainly copied from Neil Mitchells ghcid.
module Ampersand.Daemon.Wait
  ( Waiter,
    withWaiterPoll,
    withWaiterNotify,
    waitFiles,
  )
where

import Ampersand.Basics
import Ampersand.Daemon.Util
import Control.Concurrent.Extra (Var, modifyVar_, newVar)
import Control.Monad.Extra (concatMapM, firstJustM, ifM, partitionM)
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Text as T
import RIO.Time
import System.Directory.Extra (canonicalizePath, doesDirectoryExist, listContents)
import System.FSNotify
import System.FilePath
import System.Time.Extra (Seconds, sleep)

data Waiter
  = WaiterPoll Seconds
  | WaiterNotify WatchManager (MVar ()) (Var (Map.Map FilePath StopListening))

withWaiterPoll :: Seconds -> (Waiter -> IO a) -> IO a
withWaiterPoll x f = f $ WaiterPoll x

withWaiterNotify :: env -> (Waiter -> IO a) -> RIO env a
withWaiterNotify env f =
  runRIO env $ do liftIO (withWaiterNotify' f)

withWaiterNotify' :: (Waiter -> IO a) -> IO a
withWaiterNotify' f = withManagerConf defaultConfig $ \manager -> do
  mvar <- liftIO newEmptyMVar
  var <- liftIO $ newVar Map.empty
  f $ WaiterNotify manager mvar var

-- `listContentsInside test dir` will list files and directories inside `dir`,
-- recursing into those subdirectories which pass `test`.
-- Note that `dir` and files it directly contains are always listed, regardless of `test`.
-- Subdirectories will have a trailing path separator, and are only listed if we recurse into them.
listContentsInside :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
listContentsInside test dir = do
  (dirs, files) <- partitionM doesDirectoryExist =<< listContents dir
  recurse <- filterM test dirs
  rest <- concatMapM (listContentsInside test) recurse
  return $ addTrailingPathSeparator dir : files ++ rest

-- | Given the pattern:
--
-- > wait <- waitFiles waiter
-- > ...
-- > wait ["File1.hs","File2.hs"]
--
--   This continues as soon as either @File1.hs@ or @File2.hs@ changes,
--   starting from when 'waitFiles' was initially called.
--
--   Returns a message about why you are continuing (usually a file name).
waitFiles ::
  (HasLogFunc env) =>
  Waiter ->
  RIO env ([FilePath] -> RIO env [String])
waitFiles waiter = do
  base <- liftIO getCurrentTime
  return $ \files -> do
    -- handle (\(e :: IOException) -> do sleep 1.0; return ["Error when waiting, if this happens repeatedly, raise an ampersand bug.",show e]) $ do
    logDebug $ "%WAITING: " <> display (T.pack $ unwords files)
    -- As listContentsInside returns directories, we are waiting on them explicitly and so
    -- will pick up new files, as creating a new file changes the containing directory's modtime.
    files' <- liftIO $
      fmap concat $
        forM files $
          \file ->
            ifM (doesDirectoryExist file) (listContentsInside (return . not . L.isPrefixOf "." . takeFileName) file) (return [file])
    case waiter of
      WaiterPoll _ -> pure ()
      WaiterNotify manager kick mp -> do
        dirs <- liftIO $ fmap Set.fromList $ mapM canonicalizePathSafe $ nubOrd $ map takeDirectory files'
        env <- ask
        liftIO $
          modifyVar_ mp $
            \mp' -> do
              let keep, del :: Map FilePath StopListening
                  (keep, del) = Map.partitionWithKey (\k _ -> k `Set.member` dirs) mp'
              liftIO $ sequence_ $ Map.elems del
              new <- forM (Set.toList $ dirs `Set.difference` Map.keysSet keep) $ \dir -> do
                can <- liftIO $
                  watchDir manager (fromString dir) (const True) $
                    \event -> do
                      runRIO env $ logDebug $ "%NOTIFY: " <> displayShow event
                      void $ tryPutMVar kick ()
                return (dir, can)
              let mp2 :: Map FilePath StopListening
                  mp2 = keep `Map.union` Map.fromList new
              runRIO env $ logDebug $ "%WAITING: " <> display (T.pack $ unwords (Map.keys mp2))
              return mp2
        void $ tryTakeMVar kick
    new <- liftIO $ mapM getModTime files'
    case [x | (x, Just t) <- zip files' new, t > base] of
      [] -> recheck files' new
      xs -> return xs
  where
    recheck ::
      (HasLogFunc env) =>
      [FilePath] ->
      [Maybe UTCTime] ->
      RIO env [FilePath]
    recheck files old = do
      liftIO $ sleep 0.1
      case waiter of
        WaiterPoll t -> liftIO $ sleep $ max 0 $ t - 0.1 -- subtract the initial 0.1 sleep from above
        WaiterNotify _ kick _ -> do
          takeMVar kick
          logDebug "%WAITING: Notify signaled"
      new <- liftIO $ mapM getModTime files
      case [x | (x, t1, t2) <- L.zip3 files old new, t1 /= t2] of
        [] -> recheck files new
        xs -> do
          let disappeared = [x | (x, Just _, Nothing) <- L.zip3 files old new]
          when (disappeared /= []) $ do
            -- if someone is deleting a needed file, give them some space to put the file back
            -- typically caused by VIM
            -- but try not to
            logDebug $ "%WAITING: Waiting max of 1s due to file removal, " <> display (T.pack $ unwords disappeared)
            -- at most 20 iterations, but stop as soon as the file returns
            void $
              flip firstJustM (replicate 20 ()) $
                \_ -> do
                  liftIO $ sleep 0.05
                  new' <- liftIO $ mapM getModTime files
                  return $ if null [x | (x, Just _, Nothing) <- L.zip3 files old new'] then Just () else Nothing
          return xs

canonicalizePathSafe :: FilePath -> IO FilePath
canonicalizePathSafe x = canonicalizePath x `catch` \(_ :: IOException) -> return x

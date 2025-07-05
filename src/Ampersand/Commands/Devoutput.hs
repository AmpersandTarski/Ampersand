{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generate a prototype from a project.
module Ampersand.Commands.Devoutput
  ( devoutput,
    --    ,DevoutputOpts(..)
    HasDevoutputOpts (..),
  )
where

import Ampersand.Basics
import Ampersand.FSpec
import Ampersand.Misc.HasClasses
import Ampersand.Output.FSpec2SQL
import Ampersand.Output.FSpec2Turtle
import RIO.Directory
import RIO.FilePath
import qualified RIO.Text as T
import RIO.Time

-- | Dumps diagnostic output for development purposes
devoutput ::
  (HasDirOutput env, HasFSpecGenOpts env, HasLogFunc env) =>
  FSpec ->
  RIO env ()
devoutput fSpec = do
  -- For the time being, do not output Haskell and SQL dump files.
  -- This is done in a way that there is no dead code. Feel free to
  -- move the doGen... functions down if you want to generate these files.
  case originalContext fSpec of
    Nothing -> do
      doGenHaskell fSpec
      doGenSQLdump fSpec
    Just _ -> mempty
  writeTurtle fSpec

doGenHaskell ::
  (HasDirOutput env, HasFSpecGenOpts env, HasLogFunc env) =>
  FSpec ->
  RIO env ()
doGenHaskell fSpec = do
  env <- ask
  now <- getCurrentTime
  outputFile <- outputFile' <$> ask
  logDebug $ "Generating Haskell source code for " <> (display . fullName) fSpec <> "..."
  liftIO $ createDirectoryIfMissing True (takeDirectory outputFile)
  writeFileUtf8 outputFile (fSpec2Haskell env now fSpec)
  logInfo $ "Haskell written into " <> display (T.pack outputFile)
  where
    outputFile' env = view dirOutputL env </> baseName env -<.> ".hs"

doGenSQLdump ::
  (HasDirOutput env, HasFSpecGenOpts env, HasLogFunc env) =>
  FSpec ->
  RIO env ()
doGenSQLdump fSpec = do
  env <- ask
  outputFile <- outputFile' <$> ask
  logDebug $ "Generating SQL queries dumpfile for " <> (display . fullName) fSpec <> "..."
  liftIO $ createDirectoryIfMissing True (takeDirectory outputFile)
  writeFileUtf8 outputFile (dumpSQLqueries env fSpec)
  logInfo $ "SQL queries dumpfile written into " <> display (T.pack outputFile)
  where
    outputFile' env = view dirOutputL env </> baseName env <> "_dump" -<.> ".sql"

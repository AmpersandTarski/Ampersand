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
import qualified RIO.Text as T
import RIO.Time
import System.Directory
import System.FilePath

-- | Dumps diagnostic output for development purposes
devoutput ::
  (HasDirOutput env, HasFSpecGenOpts env, HasLogFunc env) =>
  FSpec ->
  RIO env ()
devoutput fSpec = do
  doGenHaskell fSpec
  doGenSQLdump fSpec
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

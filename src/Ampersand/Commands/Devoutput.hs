{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generate a prototype from a project.
module Ampersand.Commands.Devoutput
    (devoutput
--    ,DevoutputOpts(..)
    ,HasDevoutputOpts(..)
    ) where

import           Ampersand.Basics
import           Ampersand.FSpec
import           Ampersand.Misc.HasClasses
import           Ampersand.Output.FSpec2SQL
import qualified RIO.Text as T
import           RIO.Time
import           System.FilePath ((</>), (-<.>))
-- | Dumps diagnostic output for development purposes
--
devoutput :: (HasDirOutput env, HasRootFile env, HasLogFunc env) 
       => FSpec -> RIO env ()
devoutput fSpec = do
    doGenHaskell fSpec
    doGenSQLdump fSpec

doGenHaskell :: (HasDirOutput env, HasRootFile env, HasLogFunc env) 
       => FSpec -> RIO env ()
doGenHaskell fSpec = do
    env <- ask
    now <- getCurrentTime
    outputFile <- outputFile' <$> ask
    logDebug $ "Generating Haskell source code for " <> display (T.pack $ name fSpec) <> "..."
    writeFileUtf8 outputFile (T.pack $ fSpec2Haskell env now fSpec)
    logInfo $ "Haskell written into " <> display (T.pack outputFile)
  where 
    outputFile' env = view dirOutputL env </> baseName env -<.> ".hs"

doGenSQLdump :: (HasDirOutput env, HasRootFile env, HasLogFunc env) 
       => FSpec -> RIO env ()
doGenSQLdump fSpec = do
    env <- ask
    outputFile <- outputFile' <$> ask
    logDebug $ "Generating SQL queries dumpfile for " <> display (T.pack $ name fSpec) <> "..."
    writeFileUtf8 outputFile (dumpSQLqueries env fSpec)
    logInfo $ "SQL queries dumpfile written into " <> display (T.pack outputFile)
  where 
    outputFile' env = view dirOutputL env </> baseName env <> "_dump" -<.> ".sql"


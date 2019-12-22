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
import           Ampersand.Misc.HasClasses
import           Ampersand.FSpec
import qualified RIO.Text as T
import           RIO.Time (getCurrentTime)
import           System.FilePath ((</>), (-<.>))
import           Ampersand.Output.FSpec2SQL
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
    sayLn $ "Generating Haskell source code for " ++ name fSpec ++ "..."
    writeFileUtf8 outputFile (T.pack $ fSpec2Haskell env now fSpec)
    sayWhenLoudLn ("Haskell written into " ++ outputFile ++ ".")
  where 
    outputFile' env = view dirOutputL env </> baseName env -<.> ".hs"

doGenSQLdump :: (HasDirOutput env, HasRootFile env, HasLogFunc env) 
       => FSpec -> RIO env ()
doGenSQLdump fSpec = do
    env <- ask
    outputFile <- outputFile' <$> ask
    sayLn $ "Generating SQL queries dumpfile for " ++ name fSpec ++ "..."
    writeFileUtf8 outputFile (dumpSQLqueries env fSpec)
    sayWhenLoudLn ("SQL queries dumpfile written into " ++ outputFile ++ ".")
  where 
    outputFile' env = view dirOutputL env </> baseName env ++ "_dump" -<.> ".sql"


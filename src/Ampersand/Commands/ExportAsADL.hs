{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Clean a project.
module Ampersand.Commands.ExportAsADL
    (exportAsAdl
    ) where

import           Ampersand.Basics
import           Ampersand.Core.ShowAStruct
import           Ampersand.FSpec
import           Ampersand.Misc.HasClasses
import qualified RIO.Text as T
import           System.Directory
import           System.FilePath

-- | For importing and analysing data, Ampersand allows you to annotate an Excel spreadsheet (.xlsx) and turn it into an Ampersand model.
-- By default 'doGenADL' exports the model to Export.adl, ready to be picked up by the user and refined by adding rules.
-- 1. To analyze data in a spreadsheet, prepare your spreadsheet, foo.xlsx,  and run "Ampersand --dataAnalysis foo.xlsx".
--    Expect to find a file "MetaModel.adl" in your working directory upon successful termination.
-- 2. To perform a round-trip test, use an Ampersand-script foo.adl and run and run "Ampersand --export foo.adl".
--    Expect to find a file "Export.adl" in your working directory which should be semantically equivalent to foo.adl.
exportAsAdl :: (HasOutputFile env, HasDirOutput env, HasLogFunc env) => FSpec -> RIO env ()
exportAsAdl fSpec = do
    env <- ask
    logDebug $ "Generating data analysis script (ADL) for " <> display (name fSpec) <> "..."
    liftIO $ createDirectoryIfMissing True (takeDirectory (outputFile' env))
    writeFileUtf8 (outputFile' env) (showA ctx) 
    logInfo $ ".adl-file written to: " <> display (T.pack $ outputFile' env)
  where outputFile' env = view dirOutputL env </> view outputfileL env
        ctx = originalContext fSpec



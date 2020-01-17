{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generate an .xlsx file containing the population from a project.
module Ampersand.Commands.Population
    (population
    ,PopulationOpts(..)
    ,HasPopulationOpts(..)
    ) where

import           Ampersand.Basics
import           Ampersand.Misc.HasClasses
import           Ampersand.FSpec(FSpec(..))
import           Ampersand.Output.Population2Xlsx (fSpec2PopulationXlsx)
import           Ampersand.Output.ToJSON.ToJson
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Text as T
import           System.Directory
import           System.FilePath
import           Text.Pandoc.Class(runIO,getPOSIXTime) --TODO: Replace by RIO's getCurrentTime
import           Text.Pandoc.Error(handleError)

-- | Builds a file containing the population of the current project.
--   depending on a switch this could be an .xlsx file or a json file. 
population :: (HasPopulationOpts env, HasDirOutput env, HasRootFile env,HasLogFunc env)
       => FSpec -> RIO env ()
population fSpec = do
        env <- ask
        format <- view outputFormatL
        logDebug $ "Generating population output in "<>displayShow format
        case format of
          XLSX -> do let outputFile = view dirOutputL env </> baseName env <> "_generated_pop" -<.> ".xlsx"
                     ct <- liftIO $ runIO getPOSIXTime >>= handleError
                     liftIO $ createDirectoryIfMissing True (takeDirectory outputFile)
                     BL.writeFile outputFile $ fSpec2PopulationXlsx ct fSpec
                     logInfo $ "Generated file: " <> display (T.pack outputFile)
          JSON -> do let dir = view dirOutputL env
                     generatePopJSONfile dir fSpec
                     logInfo $ "JSON population file is generated"




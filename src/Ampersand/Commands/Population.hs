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
import           System.FilePath
import qualified RIO.ByteString.Lazy as BL
import           Text.Pandoc.Class(runIO,getPOSIXTime) --TODO: Replace by RIO's getCurrentTime
import           Text.Pandoc.Error(handleError)

-- | Builds an .xlsx file containing the population of the current project.
--
population :: (HasDirOutput env, HasRootFile env ,HasLogFunc env)
       => FSpec -> RIO env ()
population fSpec = do
        outputFile <- outputFile' <$> ask
        sayLn "Generating .xlsx file containing the population..."
        ct <- liftIO $ runIO getPOSIXTime >>= handleError
        BL.writeFile outputFile $ fSpec2PopulationXlsx ct fSpec
        sayWhenLoudLn ("Generated file: " <> outputFile)
      where outputFile' env = view dirOutputL env </> baseName env <> "_generated_pop" -<.> ".xlsx"




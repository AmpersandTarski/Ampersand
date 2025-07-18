{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generate an .xlsx file containing the population from a project.
module Ampersand.Commands.Population
  ( population,
    PopulationOpts (..),
    HasPopulationOpts (..),
  )
where

import Ampersand.Basics
import Ampersand.FSpec (FSpec (..))
import Ampersand.Misc.HasClasses
import Ampersand.Output.Population2Xlsx (fSpec2PopulationXlsx)
import Ampersand.Output.ToJSON.ToJson (populationToJSON)
import qualified RIO.ByteString.Lazy as BL
import RIO.Directory
import RIO.FilePath
import qualified RIO.Text as T
import Text.Pandoc.Class (getPOSIXTime, runIO)
import Text.Pandoc.Error (handleError)

-- | Builds a file containing the population of the current project.
--   depending on a switch this could be an .xlsx file or a json file.
population ::
  (HasPopulationOpts env, HasDirOutput env, HasFSpecGenOpts env, HasLogFunc env) =>
  FSpec ->
  RIO env ()
population fSpec = do
  env <- ask
  format <- view outputFormatL
  logDebug $ "Generating population output in " <> displayShow format
  case format of
    XLSX -> do
      let outputFile = view dirOutputL env </> baseName env <> "_generated_pop" -<.> ".xlsx"
      ct <- liftIO $ runIO getPOSIXTime >>= handleError
      liftIO $ createDirectoryIfMissing True (takeDirectory outputFile)
      BL.writeFile outputFile $ fSpec2PopulationXlsx ct fSpec
      logInfo $ "Generated file: " <> display (T.pack outputFile)
    JSON -> do
      let outputFile = view dirOutputL env </> baseName env <> "_generated_pop" -<.> ".json"
      liftIO $ createDirectoryIfMissing True (takeDirectory outputFile)
      BL.writeFile outputFile $ populationToJSON env fSpec
      logInfo $ "Generated file: " <> display (T.pack outputFile)

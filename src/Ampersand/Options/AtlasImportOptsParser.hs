module Ampersand.Options.AtlasImportOptsParser (atlasimportOptsParser) where

import Ampersand.Basics
import Ampersand.Misc.HasClasses (AtlasImportOpts (..))
import Options.Applicative

-- | Command-line parser for DevOutputOpts.
atlasimportOptsParser :: Parser AtlasImportOpts
atlasimportOptsParser =
  AtlasImportOpts True
    <$> importFileP
    <*> exportFileP
  where
    importFileP :: Parser FilePath
    importFileP =
      strArgument
        ( metavar "IMPORTFILE"
            <> help "The name of the file that contains the (json) population of an atlas."
        )
    exportFileP :: Parser FilePath
    exportFileP =
      strArgument
        ( metavar "OUTPUTFILE"
            <> help "The name of the script to be written."
        )

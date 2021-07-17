module Ampersand.Options.InputOutputOpts (outputFileOptsParser) where

import Ampersand.Basics
import Ampersand.Misc.HasClasses (InputOutputOpts (..))
import Ampersand.Options.FSpecGenOptsParser
import Options.Applicative

-- | Command-line parser for OutputFileOpts.
outputFileOptsParser :: FilePath -> Parser InputOutputOpts
outputFileOptsParser defOutputFile =
  InputOutputOpts
    <$> fSpecGenOptsParser False
    <*> outputfileP
  where
    outputfileP :: Parser FilePath
    outputfileP =
      strArgument
        ( metavar "OUTPUTFILE"
            <> value defOutputFile
            <> showDefault
            <> help "The name of the output file."
        )

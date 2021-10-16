module Ampersand.Options.DevoutputOptsParser (devoutputOptsParser) where

import Ampersand.Basics
import Ampersand.Misc.HasClasses (DevOutputOpts (..))
import Ampersand.Options.FSpecGenOptsParser
import Options.Applicative

-- | Command-line parser for DevOutputOpts.
devoutputOptsParser :: FilePath -> Parser DevOutputOpts
devoutputOptsParser defOutputdir =
  DevOutputOpts
    <$> fSpecGenOptsParser False
    <*> outputdirP
  where
    outputdirP :: Parser FilePath
    outputdirP =
      strArgument
        ( metavar "OUTPUTDIRECTORY"
            <> value defOutputdir
            <> showDefault
            <> help "The name of the directory where the diagnostic files will be written to."
        )

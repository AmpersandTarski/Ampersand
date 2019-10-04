{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.DevoutputOptsParser 
   (devoutputOptsParser)
where

import           Ampersand.Basics
import           Ampersand.Misc.HasClasses (DevOutputOpts (..))
import           Options.Applicative
import           Ampersand.Options.FSpecGenOptsParser

-- | Command-line parser for DevOutputOpts.
devoutputOptsParser :: FilePath -> Parser DevOutputOpts
devoutputOptsParser defOutputdir = 
     ( \fSpecGenOpts outputfile -> DevOutputOpts
       { x8fSpecGenOpts = fSpecGenOpts
       , x5outputFile = outputfile
       }
     ) <$> fSpecGenOptsParser False
       <*> outputdirP defOutputdir
  where
outputdirP :: FilePath -> Parser FilePath
outputdirP defOutputdir = strArgument 
      (metavar "OUTPUTDIRECTORY" 
    <> value defOutputdir
    <> showDefault
    <> help "The name of the directory where the diagnostic files will be written to."
    )




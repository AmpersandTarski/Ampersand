{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.InputOutputOpts 
   (outputFileOptsParser)
where

import           Ampersand.Basics
import           Ampersand.Misc.HasClasses (InputOutputOpts (..))
import           Options.Applicative
import           Ampersand.Options.FSpecGenOptsParser

-- | Command-line parser for OutputFileOpts.
outputFileOptsParser :: FilePath -> Parser InputOutputOpts
outputFileOptsParser defOutputFile = 
     ( \fSpecGenOpts outputfile -> InputOutputOpts
       { x4fSpecGenOpts = fSpecGenOpts
       , x4outputFile = outputfile
       }
     ) <$> fSpecGenOptsParser False
       <*> outputfileP defOutputFile

outputfileP :: FilePath -> Parser FilePath
outputfileP defOutputFile = strArgument 
      (metavar "OUTPUTFILE" 
    <> value defOutputFile
    <> showDefault
    <> help "The name of the output file."
    )




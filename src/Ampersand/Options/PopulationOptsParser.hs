{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.PopulationOptsParser 
   (populationOptsParser)
where

import           Ampersand.Basics
import           Ampersand.Misc.HasClasses (PopulationOpts (..))
import           Options.Applicative
import           Ampersand.Options.FSpecGenOptsParser

-- | Command-line parser for ProofOpts.
populationOptsParser :: Parser PopulationOpts
populationOptsParser = 
     ( \fSpecGenOpts -> PopulationOpts
       { x5fSpecGenOpts = fSpecGenOpts
       }
     ) <$> fSpecGenOptsParser False




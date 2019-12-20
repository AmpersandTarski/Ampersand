{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.ProofOptsParser 
   (proofOptsParser)
where

import           Ampersand.Basics
import           Ampersand.Misc.HasClasses (ProofOpts (..))
import           Options.Applicative
import           Ampersand.Options.FSpecGenOptsParser

-- | Command-line parser for ProofOpts.
proofOptsParser :: Parser ProofOpts
proofOptsParser = 
     ( \fSpecGenOpts -> ProofOpts
       { x6fSpecGenOpts = fSpecGenOpts
       }
     ) <$> fSpecGenOptsParser False




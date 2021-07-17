{-# LANGUAGE NoImplicitPrelude #-}

module Ampersand.Options.ProofOptsParser (proofOptsParser) where

import Ampersand.Basics
import Ampersand.Misc.HasClasses (ProofOpts (..))
import Ampersand.Options.FSpecGenOptsParser
import Options.Applicative

-- | Command-line parser for ProofOpts.
proofOptsParser :: Parser ProofOpts
proofOptsParser =
  ProofOpts
    <$> fSpecGenOptsParser False

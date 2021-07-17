{-# LANGUAGE NoImplicitPrelude #-}

module Ampersand.Options.UmlOptsParser (umlOptsParser) where

import Ampersand.Basics
import Ampersand.Misc.HasClasses (UmlOpts (..))
import Ampersand.Options.FSpecGenOptsParser
import Ampersand.Options.Utils
import Options.Applicative

-- | Command-line parser for the Uml command.
umlOptsParser :: Parser UmlOpts
umlOptsParser =
  UmlOpts
    <$> fSpecGenOptsParser False
      <*> outputLanguageP

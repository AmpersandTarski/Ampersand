{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.UmlOptsParser 
   (umlOptsParser)
where

import           Options.Applicative
import           Ampersand.Misc.HasClasses (UmlOpts (..))
import           Ampersand.Basics
import           Ampersand.Options.FSpecGenOptsParser
import           Ampersand.Options.Utils

-- | Command-line parser for the Uml command.
umlOptsParser :: Parser UmlOpts
umlOptsParser =
      ( \fSpecGenOpts outputLanguage -> UmlOpts
                { x7fSpecGenOpts = fSpecGenOpts
                , x4OutputLanguage = outputLanguage
                }
      ) <$> fSpecGenOptsParser False
        <*> outputLanguageP


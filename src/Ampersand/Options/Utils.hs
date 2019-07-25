{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.Utils where

import           Options.Applicative
import           Ampersand.Basics
import qualified RIO.Char as C

-- | If argument is True, hides the option from usage and help
hideMods :: Bool -> Mod f a
hideMods hide = if hide then internal <> hidden else idm



-- Common parsers:

outputLanguage :: Parser (Maybe Lang)
outputLanguage = 
   f <$> strOption
        ( long "language"
        <> metavar "OUTPUTLANGUAGE"
        <> help ("Pick 'NL' for Dutch or 'EN' for English, as the "<>
                 "language to be used in your output. Without this "<>
                 "option, output is written in the language of your "<>
                 "context." )
        )
   where
      f :: String -> Maybe Lang
      f l = case map C.toUpper l of
              "NL"  -> Just Dutch
              "UK"  -> Just English
              "US"  -> Just English
              "EN"  -> Just English
              _     -> Nothing

rootFile :: Parser FilePath
rootFile = strArgument 
          (metavar "AMPERSAND_SCRIPT" 
          <> help "The root file of your Ampersand model.")
module Ampersand.Options.DaemonParser where

import Ampersand.Basics
import Ampersand.Misc.HasClasses
import Ampersand.Options.FSpecGenOptsParser
import Options.Applicative
import Options.Applicative.Builder.Extra (boolFlags)

-- | Command-line parser for the daemon command.
daemonOptsParser :: Parser DaemonOpts
daemonOptsParser =
  DaemonOpts
    <$> strOption
      ( long "daemonconfig"
          <> metavar "CONFIGFILE"
          <> value ".ampersand"
          <> showDefault
          <> help "The config file contains the list of files to be monitored."
      )
    <*> fSpecGenOptsParser True
    <*> boolFlags
      True
      "warnings"
      "show warnings in the output, if any. "
      mempty

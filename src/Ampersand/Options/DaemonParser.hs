{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.DaemonParser where

import           Options.Applicative
--import           Ampersand.Commands.Proto (ProtoCommand(..), ProtoOpts (..))
import           Ampersand.Misc.HasClasses
import           Ampersand.Basics
import           Ampersand.Options.Utils
import           Ampersand.Options.FSpecGenOptsParser

-- | Command-line parser for the daemon command.
daemonOptsParser :: Parser DaemonOpts
daemonOptsParser = 
   ( \outputLanguage daemonConfig fSpecGenOpts -> DaemonOpts
            { x2OutputLanguage = outputLanguage
            , xdaemonConfig = daemonConfig
            , x2fSpecGenOpts = fSpecGenOpts
            }) 
  <$> outputLanguageP
  <*> strOption
        ( long "daemonconfig"
        <> metavar "CONFIGFILE"
        <> value (".ampersand")
        <> showDefault
        <> help ("The config file contains the list of files to be monitored.")
        )
  <*> fSpecGenOptsParser True



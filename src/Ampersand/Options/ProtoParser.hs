{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.ProtoParser where

import           Options.Applicative
import           Ampersand.Commands.Proto (ProtoCommand(..), ProtoOpts (..))
import           Ampersand.Basics
import           Ampersand.Options.Utils
import           Ampersand.Options.FSpecGenOptsParser
import           Data.List.Split (splitWhen)

-- | Command-line parser for the proto command.
protoOptsParser :: ProtoCommand -> String -> Parser ProtoOpts
protoOptsParser Proto defDBName = 
   ( \dbName host login password forceReinstall 
        outputLanguage trimXLSXCells fSpecGenOpts 
        skipComposer dirPrototype dirCustomizations 
        zwolleVersion allowInvariantViolations -> ProtoOpts
            { xdbName = dbName
            , xsqlHost = host
            , xsqlLogin = login
            , xsqlPwd = password
            , xforceReinstallFramework = forceReinstall
            , xoutputLangugage = outputLanguage
            , x1trimXLSXCells = trimXLSXCells
            , x1fSpecGenOpts = fSpecGenOpts
            , xskipComposer = skipComposer
            , xdirPrototype = dirPrototype
            , xdirCustomizations = dirCustomizations
            , xzwolleVersion = zwolleVersion
            , xallowInvariantViolations = allowInvariantViolations
            }) 
  <$> strOption
        ( long "dbName"
        <> short 'd'
        <> metavar "DATABASENAME"
        <> value defDBName
        <> showDefault
        <> help "Name of the schema of the database that is generated as part of the prototype." )
  <*> strOption
        ( long "sqlHost"
        <> metavar "HOSTNAME"
        <> value "localhost"
        <> showDefault
        <> help "Name of the host of the database." )
  <*> strOption
        ( long "sqlLogin"
        <> metavar "USER"
        <> value "ampersand"
        <> showDefault
        <> help "Name of the database user. (Defaults to `ampersand`)" )
  <*> strOption
        ( long "sqlPwd"
        <> metavar "PASSWORD"
        <> value "ampersand"
        <> showDefault
        <> help "Password for the database user. (Defaults to `ampersand`)" )
  <*> switch
        ( long "force-reinstall-framework"
        <> help ("Re-install the prototype framework. This discards any previously "<>
                "installed version.")
        )
  <*> outputLanguageP
  <*> trimXLSXCellsP
  <*> fSpecGenOptsParser
  <*> switch
        ( long "skip-composer"
        <> help ("Skip installing php dependencies (using Composer) "
                <>"for prototype framework.")
        )
  <*> strOption
        ( long "output-directory"
        <> metavar "DIRECTORY"
        <> value (defDBName<>".proto")
        <> showDefault
        <> help ("Specify the directory where the prototype will be generated.")
        )
  <*> (splitWhen (== ';') <$> strOption
        ( long "customizations"
        <> metavar "DIRECTORY"
        <> value ""
        <> help ("Copy a directory into the generated prototype, "
               <>"overriding the default directory called 'customizations'." )
        ))
  <*> strOption
        ( long "prototype-framework-version"
        <> metavar "VERSION"
        <> value "1.3.1"
        <> showDefault
        <> help "Tag, branch or SHA of the prototype framework on Github." )
  <*> flag True False
        ( long "ignore-invariant-violations"
        <> help ("Allow to build a prototype, even if there are invariants "
               <>"that are being violated. (See "
               <>"https://github.com/AmpersandTarski/Ampersand/issues/728)")
        )



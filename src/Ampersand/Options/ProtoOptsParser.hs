{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.ProtoOptsParser 
   (protoOptsParser)
where

import           Options.Applicative
import           Ampersand.Commands.Proto (ProtoOpts (..))
import           Ampersand.Basics
import           Ampersand.Options.Utils
import           Ampersand.Options.FSpecGenOptsParser
import           Data.List.Split (splitWhen)

-- | Command-line parser for the proto command.
protoOptsParser :: Parser ProtoOpts
protoOptsParser = 
   ( \dbName sqlHost sqlLogin sqlPwd forceReinstall 
        outputLanguage fSpecGenOpts 
        skipComposer dirPrototype dirCustomizations 
        zwolleVersion allowInvariantViolations -> ProtoOpts
            { xdbName = dbName
            , xsqlHost = sqlHost
            , xsqlLogin = sqlLogin
            , xsqlPwd = sqlPwd
            , xforceReinstallFramework = forceReinstall
            , x1OutputLanguage = outputLanguage
            , x1fSpecGenOpts = fSpecGenOpts
            , xskipComposer = skipComposer
            , xdirPrototype = dirPrototype 
            , xdirCustomizations = dirCustomizations
            , xzwolleVersion = zwolleVersion
            , xallowInvariantViolations = allowInvariantViolations
            }) 
  <$> optional dbNameP <*> sqlHostP <*> sqlLoginP <*> sqlPwdP <*> forceReinstallP
  <*> outputLanguageP <*> fSpecGenOptsParser False
  <*> skipComposerP <*> optional dirPrototypeP <*> dirCustomizationsP
  <*> zwolleVersionP <*> allowInvariantViolationsP

dbNameP :: Parser String
dbNameP = strOption
        ( long "dbName"
        <> short 'd'
        <> metavar "DATABASENAME"
        <> help "Name of the schema of the database that is generated as part of the prototype. (defaults to name of your context)" )
sqlHostP :: Parser String
sqlHostP =  strOption
        ( long "sqlHost"
        <> metavar "HOSTNAME"
        <> value "localhost"
        <> showDefault
        <> help "Name of the host of the database." )
sqlLoginP :: Parser String
sqlLoginP = strOption
        ( long "sqlLogin"
        <> metavar "USER"
        <> value "ampersand"
        <> showDefault
        <> help "Name of the database user." )
sqlPwdP :: Parser String
sqlPwdP = strOption
        ( long "sqlPwd"
        <> metavar "PASSWORD"
        <> value "ampersand"
        <> showDefault
        <> help "Password for the database user." )
forceReinstallP :: Parser Bool
forceReinstallP = switch
        ( long "force-reinstall-framework"
        <> help ("Re-install the prototype framework. This discards any previously "<>
                "installed version.")
        )
skipComposerP :: Parser Bool
skipComposerP = switch
        ( long "skip-composer"
        <> help ("Skip installing php dependencies (using Composer) "
                <>"for prototype framework.")
        )
dirPrototypeP :: Parser String
dirPrototypeP = strOption
        ( long "output-directory"
        <> metavar "DIRECTORY"
        <> help ("Specify the directory where the prototype will be generated. (defaults to ??)") --TODO: Fill in the ?? part.
        )
dirCustomizationsP :: Parser [String]
dirCustomizationsP = (splitWhen (== ';') <$> strOption
        ( long "customizations"
        <> metavar "DIRECTORY"
        <> value "customizations"
        <> showDefault
        <> help ("Copy one or more directories into the generated prototype. "
                )
        ))
zwolleVersionP :: Parser String
zwolleVersionP = strOption
        ( long "prototype-framework-version"
        <> metavar "VERSION"
        <> value "v1.3.1"
        <> showDefault
        <> help ( "Tag, branch or SHA of the prototype framework on Github. "
                <>"Normally you shouldn't need to use anohter version "
                <>"than the default. Only a developer of the framework "
                <>"can make good use of it. ")
        )
allowInvariantViolationsP :: Parser Bool
allowInvariantViolationsP = switch
        ( long "ignore-invariant-violations"
        <> help ("Allow to build a prototype, even if there are invariants "
               <>"that are being violated. (See "
               <>"https://github.com/AmpersandTarski/Ampersand/issues/728)")
        )



{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.ProtoParser 
   (protoOptsParser)
where

import           Options.Applicative
import           Ampersand.Commands.Proto (ProtoCommand(..), ProtoOpts (..))
import           Ampersand.Basics
import           Ampersand.Options.Utils
import           Ampersand.Options.FSpecGenOptsParser
import           Data.List.Split (splitWhen)

-- | Command-line parser for the proto command.
protoOptsParser :: ProtoCommand -> String -> Parser ProtoOpts
protoOptsParser Proto defDBName = 
   ( \dbName sqlHost sqlLogin sqlPwd forceReinstall 
        outputLanguage fSpecGenOpts 
        skipComposer dirPrototype dirCustomizations 
        zwolleVersion allowInvariantViolations -> ProtoOpts
            { xdbName = dbName
            , xsqlHost = sqlHost
            , xsqlLogin = sqlLogin
            , xsqlPwd = sqlPwd
            , xforceReinstallFramework = forceReinstall
            , xoutputLangugage = outputLanguage
            , x1fSpecGenOpts = fSpecGenOpts
            , xskipComposer = skipComposer
            , xdirPrototype = dirPrototype 
            , xdirCustomizations = dirCustomizations
            , xzwolleVersion = zwolleVersion
            , xallowInvariantViolations = allowInvariantViolations
            }) 
  <$> dbNameP defDBName<*> sqlHostP <*> sqlLoginP <*> sqlPwdP <*> forceReinstallP
  <*> outputLanguageP <*> fSpecGenOptsParser False
  <*> skipComposerP <*> dirPrototypeP defDBName <*> dirCustomizationsP
  <*> zwolleVersionP <*> allowInvariantViolationsP

dbNameP :: String -> Parser String
dbNameP defDBName= strOption
        ( long "dbName"
        <> short 'd'
        <> metavar "DATABASENAME"
        <> value defDBName
        <> showDefault
        <> help "Name of the schema of the database that is generated as part of the prototype." )
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
        <> help "Name of the database user. (Defaults to `ampersand`)" )
sqlPwdP :: Parser String
sqlPwdP = strOption
        ( long "sqlPwd"
        <> metavar "PASSWORD"
        <> value "ampersand"
        <> showDefault
        <> help "Password for the database user. (Defaults to `ampersand`)" )
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
dirPrototypeP :: String -> Parser String
dirPrototypeP defDBName = strOption
        ( long "output-directory"
        <> metavar "DIRECTORY"
        <> value (defDBName<>".proto")
        <> showDefault
        <> help ("Specify the directory where the prototype will be generated.")
        )
dirCustomizationsP :: Parser [String]
dirCustomizationsP = (splitWhen (== ';') <$> strOption
        ( long "customizations"
        <> metavar "DIRECTORY"
        <> value ""
        <> help ("Copy a directory into the generated prototype, "
               <>"overriding the default directory called 'customizations'." )
        ))
zwolleVersionP :: Parser String
zwolleVersionP = strOption
        ( long "prototype-framework-version"
        <> metavar "VERSION"
        <> value "1.3.1"
        <> showDefault
        <> help "Tag, branch or SHA of the prototype framework on Github." )
allowInvariantViolationsP :: Parser Bool
allowInvariantViolationsP = not <$> switch
        ( long "ignore-invariant-violations"
        <> help ("Allow to build a prototype, even if there are invariants "
               <>"that are being violated. (See "
               <>"https://github.com/AmpersandTarski/Ampersand/issues/728)")
        )



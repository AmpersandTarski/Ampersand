{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.ProtoOptsParser 
   (protoOptsParser)
where

import           Options.Applicative.Builder.Extra
import           Ampersand.Commands.Proto (ProtoOpts (..))
import           Ampersand.Basics
import           Ampersand.Misc.Defaults (defaultDirPrototype)
import           Ampersand.Options.Utils
import           Ampersand.Options.FSpecGenOptsParser
import           Data.List.Split (splitWhen)
import           Options.Applicative
import qualified RIO.Text as T

-- | Command-line parser for the proto command.
protoOptsParser :: Parser ProtoOpts
protoOptsParser = 
   ( \dbName sqlHost sqlLogin sqlPwd forceReinstall 
        outputLanguage fSpecGenOpts 
        dirPrototype dirCustomizations 
        
        zwolleVersion generateFrontend generateBackend -> ProtoOpts
            { xdbName = dbName
            , xsqlHost = sqlHost
            , xsqlLogin = sqlLogin
            , xsqlPwd = sqlPwd
            , xforceReinstallFramework = forceReinstall
            , x1OutputLanguage = outputLanguage
            , x1fSpecGenOpts = fSpecGenOpts
            , xdirPrototype = dirPrototype 
            , xdirCustomizations = dirCustomizations
            , xzwolleVersion = zwolleVersion
            , xgenerateFrontend = generateFrontend
            , xgenerateBackend = generateBackend
            }) 
  <$> optional dbNameP <*> sqlHostP <*> sqlLoginP <*> sqlPwdP <*> forceReinstallP
  <*> outputLanguageP <*> fSpecGenOptsParser False
  <*> optional dirPrototypeP <*> optional dirCustomizationsP
  <*> zwolleVersionP 
  <*> generateFrontendP <*> generateBackendP

dbNameP :: Parser Text
dbNameP = T.pack <$> strOption
        ( long "dbName"
        <> short 'd'
        <> metavar "DATABASENAME"
        <> help "Name of the schema of the database that is generated as part of the prototype. (defaults to name of your context)" )
sqlHostP :: Parser Text
sqlHostP = T.pack <$> strOption
        ( long "sqlHost"
        <> metavar "HOSTNAME"
        <> value "localhost"
        <> showDefault
        <> help "Name of the host of the database." )
sqlLoginP :: Parser Text
sqlLoginP = T.pack <$> strOption
        ( long "sqlLogin"
        <> metavar "USER"
        <> value "ampersand"
        <> showDefault
        <> help "Name of the database user." )
sqlPwdP :: Parser Text
sqlPwdP = T.pack <$> strOption
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
dirPrototypeP :: Parser String
dirPrototypeP = strOption
        ( long "proto-dir"
        <> metavar "DIRECTORY"
        <> value defaultDirPrototype
        <> showDefault
        <> help ("Specify the directory where the prototype will be generated")
        )
dirCustomizationsP :: Parser [String]
dirCustomizationsP = (splitWhen (== ';') <$> strOption
        ( long "customizations"
        <> metavar "DIR;DIR;.."
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
generateFrontendP :: Parser Bool
generateFrontendP = boolFlags True "frontend"
        ( "Generate prototype frontend files (Angular application)")
        mempty

generateBackendP :: Parser Bool
generateBackendP = boolFlags True "backend"
        ( "Generate backend files (PHP application)")
        mempty


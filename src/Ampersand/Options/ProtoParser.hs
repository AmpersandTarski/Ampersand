{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.ProtoParser where

import           Options.Applicative
import           Ampersand.Commands.Proto (ProtoCommand(..), ProtoOpts (..))
import           Ampersand.Basics
import           Ampersand.Options.Utils

-- | Command-line parser for the proto command.
protoOptsParser :: ProtoCommand -> String -> Parser ProtoOpts
protoOptsParser Proto defDBName = 
    ProtoOpts <$> dbName <*> host <*> login 
              <*> password <*> forceReinstall <*> outputLanguage
              <*> rootFile <*> sqlBinTables
  where
    dbName = strOption
        ( long "dbName"
        <> short 'd'
        <> metavar "DATABASENAME"
        <> value defDBName
        <> showDefault
        <> help "Name of the schema of the database that is generated as part of the prototype." )
    host = strOption
        ( long "sqlHost"
        <> metavar "HOSTNAME"
        <> value "localhost"
        <> showDefault
        <> help "Name of the host of the database." )
    login = strOption
        ( long "sqlLogin"
        <> metavar "USER"
        <> value "ampersand"
        <> showDefault
        <> help "Name of the database user. (Defaults to `ampersand`)" )
    password = strOption
        ( long "sqlPwd"
        <> metavar "PASSWORD"
        <> value "ampersand"
        <> showDefault
        <> help "Password for the database user. (Defaults to `ampersand`)" )
    forceReinstall = switch
        ( long "force-reinstall-framework"
        <> help ("Re-install the prototype framework. This discards any previously "<>
                "installed version.")
        )
    sqlBinTables = switch
        ( long "sql-bin-tables"
        <> help "Generate binary tables instead of broad tables in SQL database, for testing purposes." 
        )

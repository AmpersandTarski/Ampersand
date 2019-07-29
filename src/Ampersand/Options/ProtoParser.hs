{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.ProtoParser where

import           Options.Applicative
import           Ampersand.Commands.Proto (ProtoCommand(..), ProtoOpts (..))
import           Ampersand.Basics
import           Ampersand.Options.Utils
import           Ampersand.Options.FSpecGenOptsParser

-- | Command-line parser for the proto command.
protoOptsParser :: ProtoCommand -> String -> Parser ProtoOpts
protoOptsParser Proto defDBName = 
   ( \dbName host login password forceReinstall outputLanguage
        rootFile sqlBinTables trimXLSXCells fSpecGenOpts -> ProtoOpts
            { xdbName = dbName
            , xsqlHost = host
            , xsqlLogin = login
            , xsqlPwd = password
            , xforceReinstallFramework = forceReinstall
            , xoutputLangugage = outputLanguage
         --   , xrootFile = rootFile
         --   , xsqlBinTables = sqlBinTables
            , x1trimXLSXCells = trimXLSXCells
            , x1fSpecGenOpts = fSpecGenOpts
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

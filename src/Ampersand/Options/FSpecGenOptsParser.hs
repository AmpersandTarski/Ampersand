{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.FSpecGenOptsParser 
   (fSpecGenOptsParser, defFSpecGenOpts)
where

import           Options.Applicative
import           Ampersand.Misc.HasClasses (FSpecGenOpts (..))
import           Ampersand.Basics

-- | Command-line parser for the proto command.
fSpecGenOptsParser :: 
     Bool -- When for the daemon command, the rootfile will eventually come from
          -- the daemon config file. 
  -> Parser FSpecGenOpts
fSpecGenOptsParser isForDaemon =
      ( \rootFile sqlBinTables genInterfaces namespace defaultCrud trimXLSXCells
        -> FSpecGenOpts
                { xrootFile = rootFile
                , xsqlBinTables = sqlBinTables
                , xgenInterfaces = genInterfaces
                , xnamespace = namespace
                , xdefaultCrud = defaultCrud
                , xtrimXLSXCells = trimXLSXCells
                }
      ) <$> (if isForDaemon 
              then pure Nothing -- The rootfile should come from the daemon config file.
              else Just <$> rootFileP )
        <*> sqlBinTablesP
        <*> genInterfacesP
        <*> namespaceP
        <*> crudP
        <*> trimXLSXCellsP
defFSpecGenOpts :: FilePath -> FSpecGenOpts
defFSpecGenOpts rootAdl = FSpecGenOpts
     { xrootFile = Just rootAdl
     , xsqlBinTables = False
     , xgenInterfaces = False
     , xnamespace = ""
     , xdefaultCrud = (True,True,True,True)
     , xtrimXLSXCells = True
     } 
rootFileP :: Parser FilePath
rootFileP = strArgument 
          (metavar "AMPERSAND_SCRIPT" 
          <> help "The root file of your Ampersand model.")
sqlBinTablesP :: Parser Bool
sqlBinTablesP = switch
        ( long "sql-bin-tables"
        <> help ("Generate binary tables instead of broad tables in SQL "
               <>"database, for testing purposes." )
        )
genInterfacesP :: Parser Bool
genInterfacesP = switch
        ( long "interfaces"
        <> help "Generate interfaces, which currently does not work."
        )

namespaceP :: Parser String
namespaceP = strOption
        ( long "namespace"
        <> metavar "NAMESPACE"
        <> value ""
        <> help ("Prefix database identifiers with this namespace, to "
               <>"isolate namespaces within the same database." )
        )
  
crudP :: Parser (Bool,Bool,Bool,Bool)
crudP = toCruds <$> strOption 
          (long "crud-defaults"
          <> value "CRUD"
          )
  where
    toCruds :: String -> (Bool,Bool,Bool,Bool)
    toCruds crudString = 
      ( 'c' `notElem` crudString
      , 'r' `notElem` crudString
      , 'u' `notElem` crudString
      , 'd' `notElem` crudString
      )

trimXLSXCellsP :: Parser Bool
trimXLSXCellsP = switch
        ( long "do-not-trim-cellvalues"
        <> help ("Do not ignore leading and trailing spaces in .xlsx files "<>
                 "that are INCLUDED in the script.")
        )

{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.FSpecGenOptsParser where

import           Options.Applicative
import           Ampersand.Misc.HasClasses (FSpecGenOpts (..))
import           Ampersand.Basics
import           Ampersand.Options.Utils

-- | Command-line parser for the proto command.
fSpecGenOptsParser :: Parser FSpecGenOpts
fSpecGenOptsParser = 
   ( \rootFile sqlBinTables genInterfaces namespace defaultCrud
     -> FSpecGenOpts
            { xrootFile = rootFile
            , xsqlBinTables = sqlBinTables
            , xgenInterfaces = genInterfaces
            , xnamespace = namespace
            , xdefaultCrud = defaultCrud
            }
   ) 
  <$> rootFileP
  <*> switch
        ( long "sql-bin-tables"
        <> help ("Generate binary tables instead of broad tables in SQL "
               <>"database, for testing purposes." )
        )
  <*> switch
        ( long "interfaces"
        <> help "Generate interfaces, which currently does not work."
        )
  <*> strOption
        ( long "namespace"
        <> metavar "NAMESPACE"
        <> help ("Prefix database identifiers with this namespace, to "
               <>"isolate namespaces within the same database." )
        )
  <*> defaultCrudP


  where
    defaultCrudP :: Parser (Bool,Bool,Bool,Bool)
    defaultCrudP = option aap noot 
      where
        aap :: ReadM (Bool, Bool, Bool, Bool)
        aap = ( maybeReader $ \crudString -> do
            let c = 'c' `notElem` crudString
                r = 'r' `notElem` crudString
                u = 'u' `notElem` crudString
                d = 'd' `notElem` crudString
            return (c,r,u,d)
            )
        noot :: Mod OptionFields (Bool, Bool, Bool, Bool) 
        noot = (long "crud-defaults")
        
    
{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.FSpecGenOptsParser 
   (fSpecGenOptsParser, defFSpecGenOpts)
where

import           Ampersand.Misc.HasClasses (FSpecGenOpts (..))
import           Ampersand.Basics
import           Ampersand.FSpec.ShowMeatGrinder (MetaModel(..))
import           Options.Applicative
import           Options.Applicative.Builder.Extra
import           RIO.Char (toLower)
import qualified RIO.List as L

-- | Command-line parser for the proto command.
fSpecGenOptsParser :: 
     Bool -- When for the daemon command, the rootfile will eventually come from
          -- the daemon config file. 
  -> Parser FSpecGenOpts
fSpecGenOptsParser isForDaemon =
      ( \rootFile sqlBinTables genInterfaces namespace 
         defaultCrud trimXLSXCells metaModels
        -> FSpecGenOpts
                { xrootFile = rootFile
                , xsqlBinTables = sqlBinTables
                , xgenInterfaces = genInterfaces
                , xnamespace = namespace
                , xdefaultCrud = defaultCrud
                , xtrimXLSXCells = trimXLSXCells
                , xmetaModelsToAdd = metaModels
                }
      ) <$> (if isForDaemon 
              then pure Nothing -- The rootfile should come from the daemon config file.
              else Just <$> rootFileP )
        <*> sqlBinTablesP
        <*> genInterfacesP
        <*> namespaceP
        <*> crudP
        <*> trimXLSXCellsP
        <*> metaModelsP
defFSpecGenOpts :: FilePath -> FSpecGenOpts
defFSpecGenOpts rootAdl = FSpecGenOpts
     { xrootFile = Just rootAdl
     , xsqlBinTables = False
     , xgenInterfaces = False
     , xnamespace = ""
     , xdefaultCrud = (True,True,True,True)
     , xtrimXLSXCells = True
     , xmetaModelsToAdd = []
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
        <> showDefault
        <> help ("Prefix database identifiers with this namespace, to "
               <>"isolate namespaces within the same database." )
        )
  
crudP :: Parser (Bool,Bool,Bool,Bool)
crudP = toCruds <$> strOption 
          (long "crud-defaults"
          <> value "CRUD"
          <> showDefault
          <> metavar "CRUD"
          <> help ("Temporary switch to learn about the semantics of crud in "
               <>"interface expressions." )
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
trimXLSXCellsP = boolFlags True "trim-cellvalues"
        ( "ignoring the leading and trailing spaces in .xlsx files "<>
                 "that are INCLUDED in the script.")
         mempty
metaModelsP :: Parser [MetaModel]
metaModelsP = L.nub <$> many metaModelP -- (zero or more)
  where
    metaModelP :: Parser MetaModel
    metaModelP = toMetaModel <$> strOption
         (  long "addMetaModel"
         <> metavar "METAMODEL"
         <> completeWith (map show allMetaModels)
         <> help "add a metamodel to your model."
         )
      where 
         allMetaModels :: [MetaModel]
         allMetaModels = [minBound .. maxBound]
         toMetaModel :: String -> MetaModel
         toMetaModel s = case filter matches allMetaModels of
            -- FIXME: The fatals here should be plain parse errors. Not sure yet how that should be done.
            --        See https://hackage.haskell.org/package/optparse-applicative
                   [] -> fatal $ unlines
                        ["No matching metamodels found. Possible metamodels are:"
                        , "  "<>L.intercalate ", " (map show allMetaModels)
                        , "  You specified: `"<>s<>"`"
                        ]
                   [f] -> f
                   xs -> fatal $ unlines 
                        [ "Ambiguous metamodel specified. Possible matches are:"
                        , "  "<>L.intercalate ", " (map show xs)
                        ]
            where
              matches :: MetaModel -> Bool
              matches x = map toLower s `L.isPrefixOf` (map toLower $ show x)
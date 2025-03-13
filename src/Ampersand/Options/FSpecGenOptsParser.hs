module Ampersand.Options.FSpecGenOptsParser (fSpecGenOptsParser, defFSpecGenOpts) where

import Ampersand.Basics
import Ampersand.Misc.HasClasses (FSpecGenOpts (..), Recipe (..), Roots (Roots))
import Options.Applicative
import Options.Applicative.Builder.Extra
import qualified RIO.Text as T

-- | Command-line parser for the proto command.
fSpecGenOptsParser ::
  Bool -> -- When for the daemon command, the rootfile will eventually come from
  -- the daemon config file.
  Parser FSpecGenOpts
fSpecGenOptsParser isForDaemon =
  FSpecGenOpts
    <$> rootsP
    <*> sqlBinTablesP
    <*> genInterfacesP
    <*> namespaceP
    <*> crudP
    <*> trimXLSXCellsP
    <*> knownRecipeP
    <*> allowInvariantViolationsP
  where
    rootsP :: Parser Roots
    rootsP =
      if isForDaemon
        then pure $ Roots [] -- The rootfile should come from the daemon config file.
        else Roots <$> some rootFileP

    rootFileP :: Parser FilePath
    rootFileP =
      strArgument
        ( metavar "AMPERSAND_SCRIPT"
            <> help "The root file of your Ampersand model."
        )

    sqlBinTablesP :: Parser Bool
    sqlBinTablesP =
      switch
        ( long "sql-bin-tables"
            <> help
              ( "Generate binary tables instead of wide tables in SQL "
                  <> "database, for testing purposes."
              )
        )

    genInterfacesP :: Parser Bool
    genInterfacesP =
      switch
        ( long "interfaces"
            <> help "Generate interfaces, which currently does not work."
        )

    namespaceP :: Parser Text
    namespaceP =
      strOption
        ( long "namespace"
            <> metavar "NAMESPACE"
            <> value ""
            <> showDefault
            <> help
              ( "Prefix database identifiers with this namespace, to "
                  <> "isolate namespaces within the same database."
              )
        )

    crudP :: Parser (Bool, Bool, Bool, Bool)
    crudP =
      toCruds
        <$> strOption
          ( long "crud-defaults"
              <> value "CRUD"
              <> showDefault
              <> metavar "CRUD"
              <> help
                ( "Temporary switch to learn about the semantics of crud in "
                    <> "interface terms."
                )
          )
      where
        toCruds :: String -> (Bool, Bool, Bool, Bool)
        toCruds crudString =
          ( 'c' `notElem` crudString,
            'r' `notElem` crudString,
            'u' `notElem` crudString,
            'd' `notElem` crudString
          )

    trimXLSXCellsP :: Parser Bool
    trimXLSXCellsP =
      boolFlags
        True
        "trim-cellvalues"
        ( "ignoring the leading and trailing spaces in .xlsx files "
            <> "that are INCLUDED in the script."
        )
        mempty

    knownRecipeP :: Parser Recipe
    knownRecipeP =
      toKnownRecipe
        . T.pack
        <$> strOption
          ( long "build-recipe"
              <> metavar "RECIPE"
              <> value (show Standard)
              <> showDefault
              <> completeWith (map show allKnownRecipes)
              <> help
                ( "Build the internal FSpec with a predefined recipe. Allowd values are: "
                    <> show allKnownRecipes
                )
          )
      where
        allKnownRecipes :: [Recipe]
        allKnownRecipes = [minBound ..]
        toKnownRecipe :: Text -> Recipe
        toKnownRecipe s = case filter matches allKnownRecipes of
          -- TODO: The fatals here should be plain parse errors. Not sure yet how that should be done.
          --       See https://hackage.haskell.org/package/optparse-applicative
          [] ->
            fatal $
              T.unlines
                [ "No matching recipe found. Possible recipes are:",
                  "  " <> T.intercalate ", " (map tshow allKnownRecipes),
                  "  You specified: `" <> s <> "`"
                ]
          [f] -> f
          xs ->
            fatal $
              T.unlines
                [ "Ambiguous recipe specified. Possible matches are:",
                  "  " <> T.intercalate ", " (map tshow xs)
                ]
          where
            matches :: (Show a) => a -> Bool
            matches x = T.toLower s `T.isPrefixOf` T.toLower (tshow x)

    allowInvariantViolationsP :: Parser Bool
    allowInvariantViolationsP =
      switch
        ( long "ignore-invariant-violations"
            <> help
              ( "ignore invariant violations. In case of the prototype command, the "
                  <> "generated prototype might not behave as you expect. "
                  <> "Documentation is not affected. This means that invariant violations "
                  <> "are reported anyway. "
                  <> "(See https://github.com/AmpersandTarski/Ampersand/issues/728)"
              )
        )

defFSpecGenOpts :: [FilePath] -> FSpecGenOpts
defFSpecGenOpts rootAdl =
  FSpecGenOpts
    { xrootFile = Roots rootAdl,
      xsqlBinTables = False,
      xgenInterfaces = False,
      xnamespace = "",
      xdefaultCrud = (True, True, True, True),
      xtrimXLSXCells = True,
      xrecipe = Standard,
      xallowInvariantViolations = False
    }

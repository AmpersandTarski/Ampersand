module Ampersand.Options.FSpecGenOptsParser (fSpecGenOptsParser, defFSpecGenOpts) where

import Ampersand.Basics
import Ampersand.Misc.HasClasses (FSpecGenOpts (..), Recipe (..), Roots (Roots))
import Options.Applicative
import Options.Applicative.Builder.Extra
import qualified RIO.NonEmpty as NE
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
    <*> allConceptTablesP
    <*> compiledContextP
    <*> genInterfacesP
    <*> namespaceP
    <*> crudP
    <*> trimXLSXCellsP
    <*> knownRecipeP
    <*> allowInvariantViolationsP
    <*> failOnOscillationP
    <*> failOnCartesianProductP
    <*> pure False
  where
    rootsP :: Parser Roots
    rootsP =
      if isForDaemon
        then pure $ Roots (".ampersand" NE.:| []) -- The rootfile should come from the daemon config file.
        else Roots . (NE.:| []) <$> rootFileP

    rootFileP :: Parser FilePath
    rootFileP =
      strArgument
        ( metavar "AMPERSAND_SCRIPT"
            <> help "The root file of your Ampersand model."
        )

    sqlBinTablesP :: Parser Bool
    sqlBinTablesP =
      boolFlags
        False
        "sql-bin-tables"
        ( "Generate binary tables instead of wide tables in SQL "
            <> "database, for testing purposes."
        )
        mempty

    allConceptTablesP :: Parser Bool
    allConceptTablesP =
      boolFlags
        False
        "all-concept-tables"
        ( "Give every concept a table, also if no query of this context reads it. "
            <> "Use this for a context that other contexts include."
        )
        mempty

    compiledContextP :: Parser Text
    compiledContextP =
      strOption
        ( long "context"
            <> metavar "CONTEXT"
            <> value ""
            <> help
              ( "The context to compile, if the script is a system of contexts: "
                  <> "the name of a context that the first context in the root file reaches, or its alias. "
                  <> "By default the first context in the root file is compiled."
              )
        )

    genInterfacesP :: Parser Bool
    genInterfacesP =
      boolFlags
        False
        "interfaces"
        "Generate interfaces, which currently does not work (see https://github.com/AmpersandTarski/Ampersand/issues/125)."
        mempty

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
            fatal
              $ T.unlines
                [ "No matching recipe found. Possible recipes are:",
                  "  " <> T.intercalate ", " (map tshow allKnownRecipes),
                  "  You specified: `" <> s <> "`"
                ]
          [f] -> f
          xs ->
            fatal
              $ T.unlines
                [ "Ambiguous recipe specified. Possible matches are:",
                  "  " <> T.intercalate ", " (map tshow xs)
                ]
          where
            matches :: (Show a) => a -> Bool
            matches x = T.toLower s `T.isPrefixOf` T.toLower (tshow x)

    allowInvariantViolationsP :: Parser Bool
    allowInvariantViolationsP =
      boolFlags
        False
        "ignore-invariant-violations"
        ( "ignore invariant violations. In case of the prototype command, the "
            <> "generated prototype might not behave as you expect. "
            <> "Documentation is not affected. This means that invariant violations "
            <> "are reported anyway. "
            <> "(See https://github.com/AmpersandTarski/Ampersand/issues/728)"
        )
        mempty

    failOnOscillationP :: Parser Bool
    failOnOscillationP =
      boolFlags
        False
        "fail-on-oscillation"
        ( "fail (exit code 45) when the ExecEngine rules carry an oscillation "
            <> "risk. Off by default (the risk is reported as a warning). Intended "
            <> "for regression tests that assert the oscillation verdict via the "
            <> "exit code."
        )
        mempty

    failOnCartesianProductP :: Parser Bool
    failOnCartesianProductP =
      boolFlags
        False
        "fail-on-cartesian-product"
        ( "fail (exit code 46) when the SQL of a rule's violation query "
            <> "computes a Cartesian product of two concept tables. Off by "
            <> "default (the product is reported as a warning). Intended for "
            <> "regression tests that assert via the exit code that all "
            <> "complements in violation queries are anchored."
        )
        mempty

defFSpecGenOpts :: NonEmpty FilePath -> FSpecGenOpts
defFSpecGenOpts rootAdl =
  FSpecGenOpts
    { xrootFile = Roots rootAdl,
      xsqlBinTables = False,
      xallConceptTables = False,
      xcompiledContext = "",
      xgenInterfaces = False,
      xnamespace = "",
      xdefaultCrud = (True, True, True, True),
      xtrimXLSXCells = True,
      xrecipe = Standard,
      xallowInvariantViolations = False,
      xfailOnOscillation = False,
      xfailOnCartesianProduct = False,
      xconfineIncludes = False
    }

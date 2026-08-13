module Ampersand.Options.IncrementalBenchOptsParser (incrementalBenchOptsParser) where

import Ampersand.Basics
import Ampersand.Misc.HasClasses (IncrementalBenchOpts (..))
import Ampersand.Options.FSpecGenOptsParser
import Options.Applicative
import qualified RIO.Text as T

-- | Command-line parser for IncrementalBenchOpts.
incrementalBenchOptsParser :: Parser IncrementalBenchOpts
incrementalBenchOptsParser =
  IncrementalBenchOpts
    <$> fSpecGenOptsParser False
    <*> ( T.pack
            <$> strOption
              ( long "scales"
                  <> metavar "N,N,..."
                  <> value "500,1000,2000,4000"
                  <> showDefault
                  <> help "Population sizes (pairs per relation) to measure at."
              )
        )
    <*> option
      auto
      ( long "transactions"
          <> metavar "COUNT"
          <> value 200
          <> showDefault
          <> help "Number of single-pair transactions per scale."
      )
    <*> option
      auto
      ( long "seed"
          <> metavar "SEED"
          <> value 42
          <> showDefault
          <> help "Seed of the deterministic transaction stream."
      )
    <*> switch
      ( long "verify"
          <> help "Check every transaction against full re-evaluation (the oracle). Slow, but proves observational equality on this run."
      )
    <*> optional
      ( strOption
          ( long "csv"
              <> metavar "FILE"
              <> help "Write per-transaction measurements to FILE as CSV."
          )
      )
    <*> switch
      ( long "sql"
          <> help "Run the delta-SQL referee harness against MariaDB instead of the in-memory benchmark: maintain a violation cache with the generated delta queries and compare it with full-query evaluation after every transaction. Requires command line php with MySQL support, and --sql-bin-tables."
      )

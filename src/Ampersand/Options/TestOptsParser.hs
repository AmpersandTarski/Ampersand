module Ampersand.Options.TestOptsParser (testOptsParser) where

import Ampersand.Basics
import Ampersand.Misc.HasClasses (TestOpts (..))
import Options.Applicative

-- | Command-line parser for TestOpts.
testOptsParser :: FilePath -> Parser TestOpts
testOptsParser dir =
  TestOpts
    <$> strArgument
      ( metavar "TESTDIRECTORY"
          <> value dir
          <> showDefault
          <> help "The root of the directory tree where the regression test cases can be found."
      )

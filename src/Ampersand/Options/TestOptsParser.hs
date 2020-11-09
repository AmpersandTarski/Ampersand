{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.TestOptsParser 
   (testOptsParser)
where

import           Ampersand.Basics
import           Ampersand.Misc.HasClasses (TestOpts (..))
import           Options.Applicative

-- | Command-line parser for DevOutputOpts.
testOptsParser :: FilePath -> Parser TestOpts
testOptsParser dir = 
     ( \root -> TestOpts
       { rootTestDir = root
       }
     ) <$> rootTestDirP dir

rootTestDirP :: FilePath -> Parser FilePath
rootTestDirP fp = strArgument 
      (metavar "TESTDIRECTORY" 
    <> value fp
    <> showDefault
    <> help "The root of the directory tree where the regression test cases can be found."
    )




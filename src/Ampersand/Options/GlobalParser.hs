{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Ampersand.Options.GlobalParser where

import Ampersand.Basics
import Ampersand.Options.LogLevelParser
import Ampersand.Options.Utils
import Ampersand.Types.Config
-- import           Control.Monad.Trans.Except
-- import           Control.Monad.Trans.Writer

import Data.Monoid (First (..)) -- , Any (..), Sum (..), Endo (..))
-- import           Options.Applicative.Builder.Internal
-- import           Options.Applicative.Help (errorHelp, stringChunk, vcatChunks)
-- import           Options.Applicative.Types
-- import qualified RIO.List as L
-- import qualified RIO.Directory as D
-- import           System.Environment (getProgName, getArgs, withArgs)
-- import           RIO.FilePath (isValid, pathSeparator, takeDirectory)
import Generics.Deriving.Monoid (mappenddefault, memptydefault)
import Options.Applicative
import Options.Applicative.Builder.Extra

-- | Parsed global command-line options monoid.
data GlobalOptsMonoid = GlobalOptsMonoid
  { -- | Log level
    globalMonoidLogLevel :: !(First LogLevel),
    -- | Whether to include timings in logs.
    globalMonoidTimeInLog :: !FirstTrue,
    -- | We're in a terminal?
    globalMonoidTerminal :: !(First Bool),
    -- | Terminal width override
    globalMonoidTermWidth :: !(First Int),
    -- | Override project output directory
    globalMonoidOutputDir :: !(First FilePath)
  }
  deriving (Generic)

instance Semigroup GlobalOptsMonoid where
  (<>) = mappenddefault

instance Monoid GlobalOptsMonoid where
  mempty = memptydefault
  mappend = (<>)

-- | Parser for global command-line options.
globalOptsParser :: FilePath -> Maybe LogLevel -> Parser GlobalOptsMonoid
globalOptsParser _currentDir defLogLevel =
  let build loglevel timeInLog terminal termWidth outputDir =
        GlobalOptsMonoid
          { globalMonoidLogLevel = loglevel,
            globalMonoidTimeInLog = timeInLog,
            globalMonoidTerminal = terminal,
            globalMonoidTermWidth = termWidth,
            globalMonoidOutputDir = outputDir
          }
   in (build . First <$> logLevelOptsParser hide0 defLogLevel)
        <*> firstBoolFlagsTrue
          "time-in-log"
          "inclusion of timings in logs, for the purposes of using diff with logs"
          hide
        <*> firstBoolFlagsNoDefault
          "terminal"
          "overriding terminal detection in the case of running in a false terminal"
          hide
        <*> optionalFirst
          ( option
              auto
              ( long "terminal-width"
                  <> metavar "INT"
                  <> help "Specify the width of the terminal, used for pretty-print messages"
                  <> hide
              )
          )
        <*> optionalFirst
          ( strOption
              ( long "output-dir"
                  <> metavar "DIR"
                  <> help "Specify the directory where your output will be written to"
                  <> hide
              )
          )
  where
    hide = hideMods hide0
    hide0 = False

-- | Create GlobalOpts from GlobalOptsMonoid.
globalOptsFromMonoid :: (MonadIO m) => Bool -> FilePath -> GlobalOptsMonoid -> m GlobalOpts
globalOptsFromMonoid defaultTerminal defaultOutputDir GlobalOptsMonoid {..} = do
  --  resolver <- for (getFirst globalMonoidResolver) $ \ur -> do
  --    root <-
  --      case globalMonoidResolverRoot of
  --        First Nothing -> getCurrentDir
  --        First (Just dir) -> resolveDir' dir
  --    resolvePaths (Just root) ur
  --  ampersandYaml <-
  --    case getFirst globalMonoidAmpersandYaml of
  --      Nothing -> pure SYLDefault
  --      Just fp -> SYLOverride <$> resolveFile' fp
  pure
    GlobalOpts
      { globalLogLevel = fromFirst defaultLogLevel globalMonoidLogLevel,
        globalTimeInLog = fromFirstTrue globalMonoidTimeInLog,
        globalTerminal = fromFirst defaultTerminal globalMonoidTerminal,
        globalTermWidth = getFirst globalMonoidTermWidth,
        globalOutputDir = fromFirst defaultOutputDir globalMonoidOutputDir
      }
  where
    defaultLogLevel = LevelInfo

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Ampersand.Options.GlobalParser

where

import           Ampersand.Basics
import           Ampersand.Types.Config
import           Ampersand.Options.LogLevelParser
import           Ampersand.Options.Utils
--import           Control.Monad.Trans.Except
--import           Control.Monad.Trans.Writer
import           Generics.Deriving.Monoid (memptydefault, mappenddefault)
import           Options.Applicative
import           Options.Applicative.Builder.Extra
import           Data.Monoid          (First (..)) -- , Any (..), Sum (..), Endo (..))
--import           Options.Applicative.Builder.Internal
--import           Options.Applicative.Help (errorHelp, stringChunk, vcatChunks)
--import           Options.Applicative.Types
--import qualified RIO.List as L
--import qualified System.Directory as D
--import           System.Environment (getProgName, getArgs, withArgs)
--import           System.FilePath (isValid, pathSeparator, takeDirectory)

-- | Parsed global command-line options monoid.
data GlobalOptsMonoid = GlobalOptsMonoid
    { globalMonoidLogLevel     :: !(First LogLevel) -- ^ Log level
    , globalMonoidTimeInLog    :: !FirstTrue -- ^ Whether to include timings in logs.
    , globalMonoidTerminal     :: !(First Bool) -- ^ We're in a terminal?
    , globalMonoidTermWidth    :: !(First Int) -- ^ Terminal width override
    , globalMonoidOutputDir    :: !(First FilePath) -- ^ Override project output directory
    } deriving Generic

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
          { globalMonoidLogLevel     = loglevel
          , globalMonoidTimeInLog    = timeInLog
          , globalMonoidTerminal     = terminal
          , globalMonoidTermWidth    = termWidth
          , globalMonoidOutputDir    = outputDir
          }

    in build <$>
    (First <$> logLevelOptsParser hide0 defLogLevel) <*>
    firstBoolFlagsTrue
        "time-in-log"
        "inclusion of timings in logs, for the purposes of using diff with logs"
        hide <*>
    firstBoolFlagsNoDefault
        "terminal"
        "overriding terminal detection in the case of running in a false terminal"
        hide <*>
    optionalFirst (option auto
        (long "terminal-width" <>
         metavar "INT" <>
         help "Specify the width of the terminal, used for pretty-print messages" <>
         hide)) <*>
    optionalFirst
        (strOption
            (long "output-dir" <>
            metavar "DIR" <>
            help "Specify the directory where your output will be written to" <>
            hide))
  where
    hide = hideMods hide0
    hide0 = False

-- | Create GlobalOpts from GlobalOptsMonoid.
globalOptsFromMonoid :: MonadIO m => Bool -> FilePath -> GlobalOptsMonoid -> m GlobalOpts
globalOptsFromMonoid defaultTerminal defaultOutputDir GlobalOptsMonoid{..} = do
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
  pure GlobalOpts
    { globalLogLevel = fromFirst defaultLogLevel globalMonoidLogLevel
    , globalTimeInLog = fromFirstTrue globalMonoidTimeInLog
    , globalTerminal = fromFirst defaultTerminal globalMonoidTerminal
    , globalTermWidth = getFirst globalMonoidTermWidth
    , globalOutputDir = fromFirst defaultOutputDir globalMonoidOutputDir}
    where defaultLogLevel = LevelInfo


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
module Ampersand.Misc.Config
  ( 
  -- * Main configuration types and classes
  -- ** Runner
    HasRunner(..)
  , Runner(..)
  , GlobalOpts(..)
  -- ** Config & HasConfig
  , Config(..)
--  ,HasConfig(..)
  , loadConfig
)
where
import           Ampersand.Basics
import           RIO.Process (ProcessContext, HasProcessContext (..))
import           Data.Yaml as Yaml
import           Ampersand.Misc.Options(HasEnvironment(..),EnvironmentOptions) -- TODO: Replace by Config mechanism. This removes the dependency of the old Ampersand.Misc.Options structure.

-- | Load the configuration, using current directory, environment variables,
-- and defaults as necessary.
loadConfig :: HasRunner env => (Config -> RIO env a) -> RIO env a
loadConfig inner = do
--    mstackYaml <- view $ globalOptsL.to globalStackYaml
--    configArgs <- view $ globalOptsL.to globalConfigMonoid
--    (stackRoot, userOwnsStackRoot) <- determineStackRootAndOwnership configArgs

--    userConfigPath <- getDefaultUserConfigPath stackRoot
--    extraConfigs0 <- getExtraConfigs userConfigPath >>=
--        mapM (\file -> loadConfigYaml (parseConfigMonoid (parent file)) file)
    let extraConfigs = []
    let configDummy = fatal "TODO: work with config."
    let --withConfig :: RIO env a
        withConfig = inner configDummy
--          configFromConfigMonoid
--            stackRoot
--            userConfigPath
--            mresolver
--            mproject'
--            (mconcat $ configArgs : addConfigMonoid extraConfigs)

--    withConfig $ \config -> do
--      inner config
    inner configDummy

-- | The base environment that almost everything in Ampersand runs in,
-- based off of parsing command line options in 'GlobalOpts'. Provides
-- logging and process execution.
data Runner = Runner
  { runnerGlobalOpts :: !GlobalOpts
  , runnerUseColor   :: !Bool
  , runnerLogFunc    :: !LogFunc
  , runnerTermWidth  :: !Int
  , runnerProcessContext :: !ProcessContext
  , tmpRunnerEnvOptions :: !EnvironmentOptions} deriving Show
instance Show LogFunc where show _ = "<LogFunc>"
instance Show ProcessContext where show _ = "<ProcessContext>"  

-- | Class for environment values which have a 'Runner'.
class (HasProcessContext env, HasLogFunc env) => HasRunner env where
  runnerL :: Lens' env Runner
instance HasLogFunc Runner where
  logFuncL = lens runnerLogFunc (\x y -> x { runnerLogFunc = y })
instance HasRunner Runner where
  runnerL = id
instance HasProcessContext Runner where
  processContextL = lens runnerProcessContext (\x y -> x { runnerProcessContext = y })
instance HasEnvironment Runner where
-- | Parsed global command-line options.
data GlobalOpts = GlobalOpts
    { globalLogLevel     :: !LogLevel -- ^ Log level
    , globalTimeInLog    :: !Bool -- ^ Whether to include timings in logs.
    , globalTerminal     :: !Bool -- ^ We're in a terminal?
    , globalTermWidth    :: !(Maybe Int) -- ^ Terminal width override
    } deriving (Show)

data ColorWhen = ColorNever | ColorAlways | ColorAuto
    deriving (Eq, Show, Generic)

instance Yaml.FromJSON ColorWhen where
    parseJSON v = do
        s <- parseJSON v
        case s of
            "never"  -> return ColorNever
            "always" -> return ColorAlways
            "auto"   -> return ColorAuto
            _ -> fail ("Unknown color use: " <> s <> ". Expected values of " <>
                       "option are 'never', 'always', or 'auto'.")

-- | The top-level Stackage configuration.
data Config =
  Config {configWorkDir             :: !FilePath --Dummy, to make sure Config has some stuff in it.
         -- ^ this allows to override .stack-work directory
         }

-- An uninterpreted representation of configuration options.
-- Configurations may be "cascaded" using mappend (left-biased).
data ConfigMonoid =
  ConfigMonoid
    {configMonoidWorkDir :: !(First FilePath)
    -- ^ See: 'configWorkDir'.
    }
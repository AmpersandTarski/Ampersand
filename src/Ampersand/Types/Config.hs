{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ampersand.Types.Config
  ( -- * Main configuration types and classes

    -- ** Runner
    HasRunner (..),
    Runner (..),

    -- ** Extensions of Runner with command-specific options
    extendWith,
    ExtendedRunner,
    GlobalOpts (..),

    -- ** Config & HasConfig
    Config (..),
    --  ,HasConfig(..)
    loadConfig,
  )
where

import Ampersand.Basics
import Ampersand.Misc.HasClasses
import Data.Yaml as Yaml
import RIO.Process
  ( HasProcessContext (..),
    ProcessContext,
  )

-- | Load the configuration, using current directory, environment variables,
-- and defaults as necessary.
loadConfig :: () => (Config -> RIO env a) -> RIO env a
loadConfig inner = do
  --    mstackYaml <- view $ globalOptsL.to globalStackYaml
  --    configArgs <- view $ globalOptsL.to globalConfigMonoid
  --    (stackRoot, userOwnsStackRoot) <- determineStackRootAndOwnership configArgs

  --    userConfigPath <- getDefaultUserConfigPath stackRoot
  --    extraConfigs0 <- getExtraConfigs userConfigPath >>=
  --        mapM (\file -> loadConfigYaml (parseConfigMonoid (parent file)) file)
  --    let extraConfigs = []
  let configDummy = fatal "TODO: work with config."
  --    let --withConfig :: RIO env a
  --        withConfig = inner configDummy
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
  { runnerGlobalOpts :: !GlobalOpts,
    runnerUseColor :: !Bool,
    runnerLogFunc :: !LogFunc,
    runnerTermWidth :: !Int,
    runnerProcessContext :: !ProcessContext
  }
  deriving (Show)

instance Show LogFunc where show _ = "<LogFunc>"

instance Show ProcessContext where show _ = "<ProcessContext>"

instance HasOptions Runner where
  optsList = optsList . runnerGlobalOpts

-- | Class for environment values which have a 'Runner'.
class (HasProcessContext env, HasLogFunc env) => HasRunner env where
  runnerL :: Lens' env Runner

instance HasLogFunc Runner where
  logFuncL = lens runnerLogFunc (\x y -> x {runnerLogFunc = y})

instance HasRunner Runner where
  runnerL = id
  {-# INLINE runnerL #-}

instance HasProcessContext Runner where
  processContextL = lens runnerProcessContext (\x y -> x {runnerProcessContext = y})

-- | Parsed global command-line options.
data GlobalOpts = GlobalOpts
  { -- | Log level
    globalLogLevel :: !LogLevel,
    -- | Whether to include timings in logs.
    globalTimeInLog :: !Bool,
    -- | We're in a terminal?
    globalTerminal :: !Bool,
    -- | Terminal width override
    globalTermWidth :: !(Maybe Int),
    -- | Relative path where output should be written to
    globalOutputDir :: !FilePath
  }
  deriving (Show)

instance HasOptions GlobalOpts where
  optsList opts =
    [ ( "--verbosity",
        case globalLogLevel opts of
          LevelDebug -> "debug"
          LevelInfo -> "info"
          LevelWarn -> "warn"
          LevelError -> "error"
          LevelOther x -> x
      ),
      ("--[no-]time-in-log", tshow $ globalTimeInLog opts),
      ("--[no-]terminal", tshow $ globalTerminal opts),
      ("--terminal-width", tshow $ globalTermWidth opts),
      ("--output-dir", tshow $ globalOutputDir opts)
    ]

instance HasDirOutput GlobalOpts where
  dirOutputL = lens globalOutputDir (\x y -> x {globalOutputDir = y})

instance HasDirOutput Runner where
  dirOutputL = lens runnerGlobalOpts (\x y -> x {runnerGlobalOpts = y}) . dirOutputL

data ColorWhen = ColorNever | ColorAlways | ColorAuto
  deriving (Eq, Show, Generic)

instance Yaml.FromJSON ColorWhen where
  parseJSON v = do
    s <- parseJSON v
    case s of
      "never" -> return ColorNever
      "always" -> return ColorAlways
      "auto" -> return ColorAuto
      _ ->
        fail
          ( "Unknown color use: "
              <> s
              <> ". Expected values of "
              <> "option are 'never', 'always', or 'auto'."
          )

-- | The top-level Stackage configuration.
newtype Config = Config
  { -- | this allows to override .stack-work directory
    configWorkDir :: FilePath -- Dummy, to make sure Config has some stuff in it.
  }

-- An uninterpreted representation of configuration options.
-- Configurations may be "cascaded" using mappend (left-biased).
-- data ConfigMonoid =
--  ConfigMonoid
--    {configMonoidWorkDir :: !(First FilePath)
--    -- ^ See: 'configWorkDir'.
--    }

extendWith ::
  (MonadReader s m, HasRunner s, MonadIO m) =>
  a ->
  RIO (ExtendedRunner a) b ->
  m b
extendWith ext inner = do
  env1 <- view runnerL
  runRIO env1 $ do
    -- extendWith ext $ inner
    env2 <- ask
    runRIO (ExtendedRunner env2 ext) inner

data ExtendedRunner a = ExtendedRunner
  { eRunner :: !Runner,
    eCmdOpts :: a
  }
  deriving (Show)

cmdOptsL :: Lens' (ExtendedRunner a) a
cmdOptsL = lens eCmdOpts (\x y -> x {eCmdOpts = y})

instance (HasOutputLanguage a) => HasOutputLanguage (ExtendedRunner a) where
  languageL = cmdOptsL . languageL

instance (HasFSpecGenOpts a) => HasFSpecGenOpts (ExtendedRunner a) where
  fSpecGenOptsL = cmdOptsL . fSpecGenOptsL

instance (HasDocumentOpts a) => HasDocumentOpts (ExtendedRunner a) where
  documentOptsL = cmdOptsL . documentOptsL

instance (HasDaemonOpts a) => HasDaemonOpts (ExtendedRunner a) where
  daemonOptsL = cmdOptsL . daemonOptsL

instance (HasTestOpts a) => HasTestOpts (ExtendedRunner a) where
  testOptsL = cmdOptsL . testOptsL

instance (HasGenerateFrontend a) => HasGenerateFrontend (ExtendedRunner a) where
  generateFrontendL = cmdOptsL . generateFrontendL

instance (HasGenerateBackend a) => HasGenerateBackend (ExtendedRunner a) where
  generateBackendL = cmdOptsL . generateBackendL

instance (HasGenerateMetamodel a) => HasGenerateMetamodel (ExtendedRunner a) where
  generateMetamodelL = cmdOptsL . generateMetamodelL

instance (HasFSpecGenOpts a, HasDirPrototype a) => HasDirPrototype (ExtendedRunner a) where
  dirPrototypeL = cmdOptsL . dirPrototypeL

instance (HasProtoOpts a) => HasProtoOpts (ExtendedRunner a) where
  protoOptsL = cmdOptsL . protoOptsL

instance (HasPopulationOpts a) => HasPopulationOpts (ExtendedRunner a) where
  populationOptsL = cmdOptsL . populationOptsL

instance (HasOutputFile a) => HasOutputFile (ExtendedRunner a) where
  outputfileL = cmdOptsL . outputfileL

instance (HasImportFile a) => HasImportFile (ExtendedRunner a) where
  importFileL = cmdOptsL . importFileL

instance HasRunner (ExtendedRunner a) where
  runnerL = lens eRunner (\x y -> x {eRunner = y})

instance HasLogFunc (ExtendedRunner a) where
  logFuncL = runnerL . logFuncL

instance HasProcessContext (ExtendedRunner a) where
  processContextL = runnerL . processContextL

instance HasDirOutput (ExtendedRunner a) where
  dirOutputL = runnerL . dirOutputL

instance (HasBlackWhite a) => HasBlackWhite (ExtendedRunner a) where
  blackWhiteL = cmdOptsL . blackWhiteL

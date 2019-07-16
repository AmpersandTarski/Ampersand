{-# LANGUAGE DeriveGeneric #-}
module Ampersand.Misc.Config
  ( 
    HasRunner(..)
  , Runner(..)
  , GlobalOpts(..)
--  , ColorWhen(..)   
)
where
import           Ampersand.Basics
import           RIO.Process (ProcessContext, HasProcessContext (..))
import Data.Yaml

-- | The base environment that almost everything in Ampersand runs in,
-- based off of parsing command line options in 'GlobalOpts'. Provides
-- logging and process execution.
data Runner = Runner
  { runnerGlobalOpts :: !GlobalOpts
  , runnerUseColor   :: !Bool
  , runnerLogFunc    :: !LogFunc
  , runnerTermWidth  :: !Int
  , runnerProcessContext :: !ProcessContext
  }

-- | Class for environment values which have a 'Runner'.
class (HasProcessContext env, HasLogFunc env) => HasRunner env where
  runnerL :: Lens' env Runner
instance HasLogFunc Runner where
  logFuncL = lens runnerLogFunc (\x y -> x { runnerLogFunc = y })

-- | Parsed global command-line options.
data GlobalOpts = GlobalOpts
    { globalLogLevel     :: !LogLevel -- ^ Log level
    , globalTimeInLog    :: !Bool -- ^ Whether to include timings in logs.
    , globalTerminal     :: !Bool -- ^ We're in a terminal?
    , globalTermWidth    :: !(Maybe Int) -- ^ Terminal width override
    } deriving (Show)

data ColorWhen = ColorNever | ColorAlways | ColorAuto
    deriving (Eq, Show, Generic)

instance FromJSON ColorWhen where
    parseJSON v = do
        s <- parseJSON v
        case s of
            "never"  -> return ColorNever
            "always" -> return ColorAlways
            "auto"   -> return ColorAuto
            _ -> fail ("Unknown color use: " <> s <> ". Expected values of " <>
                       "option are 'never', 'always', or 'auto'.")


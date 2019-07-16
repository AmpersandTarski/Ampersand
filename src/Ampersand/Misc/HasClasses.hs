module Ampersand.Misc.HasClasses

where
import RIO
--import System.FilePath

class HasDaemonConfig a where
  daemonConfigL :: Lens' a FilePath
class HasDirPrototype a where
  dirPrototypeL :: Lens' a FilePath
class HasAllowInvariantViolations a where
  allowInvariantViolationsL :: Lens' a Bool

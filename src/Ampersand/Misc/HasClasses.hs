module Ampersand.Misc.HasClasses

where
import Ampersand.Basics
import RIO.Time
import RIO.FilePath
--import System.FilePath

class HasDaemonConfig a where
  daemonConfigL :: Lens' a FilePath
class HasDirPrototype a where
  dirPrototypeL :: Lens' a FilePath
  getTemplateDir :: a -> String
  getTemplateDir x = 
    view dirPrototypeL x </> "templates"
  getAppDir :: a -> String
  getAppDir x =
    view dirPrototypeL x </> "public" </> "app" </> "project"

class HasAllowInvariantViolations a where
  allowInvariantViolationsL :: Lens' a Bool
class HasExcellOutputOptions a where
  trimXLSXCellsL :: Lens' a Bool
class HasGenTime a where
  genTimeL :: Lens' a LocalTime
class HasRootFile a where
  fileNameL :: Lens' a (Maybe FilePath)
  baseName :: a -> String
  baseName = takeBaseName . fromMaybe "unknown" . view fileNameL
  dirSource :: a -> FilePath -- the directory of the script that is being compiled
  dirSource = takeDirectory . fromMaybe "/" . view fileNameL
class HasOutputLanguage a where
  languageL :: Lens' a (Maybe Lang)  -- The language in which the user wants the documentation to be printed.
class HasDefaultCrud a where
  defaultCrudL :: Lens' a (Bool,Bool,Bool,Bool) -- Default values for CRUD functionality in interfaces
class HasRunComposer a where
  runComposerL :: Lens' a Bool -- if True, runs Composer (php package manager) when generating prototype. Requires PHP and Composer on the machine. Added as switch to disable when building with Docker.
class HasDirCustomizations a where
  dirCustomizationsL :: Lens' a [FilePath] -- the directory that is copied after generating the prototype
class HasZwolleVersion a where
  zwolleVersionL :: Lens' a String -- the version in github of the prototypeFramework. can be a tagname, a branchname or a SHA








                       


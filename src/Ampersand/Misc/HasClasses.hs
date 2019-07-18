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
class HasSqlBinTables a where
  sqlBinTablesL :: Lens' a Bool -- generate binary tables (no 'brede tabellen')
class HasGenInterfaces a where
  genInterfacesL :: Lens' a Bool -- 
class HasNamespace a where
  namespaceL :: Lens' a String -- prefix database identifiers with this namespace, to isolate namespaces within the same database.
class HasMetaOptions a where
  genMetaFileL :: Lens' a Bool
  addSemanticMetamodelL :: Lens' a Bool
class HasDirOutput a where
  dirOutputL :: Lens' a String -- the directory to generate the output in.
class HasGenFuncSpec a where
  genFSpecL :: Lens' a Bool   -- if True, generate a functional design
  diagnosisOnlyL :: Lens' a Bool   -- give a diagnosis only (by omitting the rest of the functional design document)
  fspecFormatL :: Lens' a FSpecFormat -- the format of the generated (pandoc) document(s)
  noDiagnosisL :: Lens' a Bool -- omit the diagnosis chapter from the functional design document.
  genLegalRefsL :: Lens' a Bool   -- Generate a table of legal references in Natural Language chapter
  noGraphicsL :: Lens' a Bool -- Omit generation of graphics during generation of functional design document.
class HasBlackWhite a where
  blackWhiteL :: Lens' a Bool    -- only use black/white in graphics
class HasCommands a where
  genUMLL :: Lens' a Bool -- Generate a UML 2.0 data model
  genHaskellL :: Lens' a Bool -- if True, generate the F-structure as a Haskell source file
  sqlDumpL :: Lens' a Bool -- if True, generate a dump of SQL statements (for debugging)
  export2adlL :: Lens' a Bool 
  genFPAExcelL :: Lens' a Bool 
  genPOPExcelL :: Lens' a Bool 
  proofsL :: Lens' a Bool 
  validateSQLL :: Lens' a Bool 
  genPrototypeL :: Lens' a Bool 
  genRapPopulationL :: Lens' a Bool 
  dataAnalysisL :: Lens' a Bool -- "export a data model as plain Ampersand script, for analysing Excel-data."
  showVersionL :: Lens' a Bool
  genSampleConfigFileL :: Lens' a Bool
  showHelpL :: Lens' a Bool
  runAsDaemonL :: Lens' a Bool
class HasCommands a => HasOutputFile a where
  outputfileAdlL :: Lens' a FilePath
  outputfileDataAnalisysL :: Lens' a FilePath
class HasVersion a where
  preVersionL :: Lens' a String 
  postVersionL :: Lens' a String 
data FSpecFormat = 
         FPandoc
       | Fasciidoc
       | Fcontext
       | Fdocbook
       | Fdocx 
       | Fhtml
       | Fman
       | Fmarkdown
       | Fmediawiki
       | Fopendocument
       | Forg
       | Fpdf
       | Fplain
       | Frst
       | Frtf
       | Flatex
       | Ftexinfo
       | Ftextile
       deriving (Show, Eq, Enum, Bounded)




                       


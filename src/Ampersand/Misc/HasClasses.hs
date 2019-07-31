module Ampersand.Misc.HasClasses

where
import Ampersand.Basics
import RIO.FilePath
--import System.FilePath

class HasFSpecGenOpts a where
  fSpecGenOptsL :: Lens' a FSpecGenOpts
  sqlBinTablesL :: Lens' a Bool
  sqlBinTablesL = fSpecGenOptsL . (lens xsqlBinTables (\x y -> x { xsqlBinTables = y }))
  genInterfacesL :: Lens' a Bool -- 
  genInterfacesL = fSpecGenOptsL . (lens xgenInterfaces (\x y -> x { xgenInterfaces = y }))
  namespaceL :: Lens' a String -- prefix database identifiers with this namespace, to isolate namespaces within the same database.
  namespaceL = fSpecGenOptsL . (lens xnamespace (\x y -> x { xnamespace = y }))
  defaultCrudL :: Lens' a (Bool,Bool,Bool,Bool) -- Default values for CRUD functionality in interfaces
  defaultCrudL = fSpecGenOptsL . lens xdefaultCrud (\x y -> x { xdefaultCrud = y })
  trimXLSXCellsL :: Lens' a Bool
  trimXLSXCellsL = fSpecGenOptsL . lens xtrimXLSXCells (\x y -> x { xtrimXLSXCells = y })
instance HasFSpecGenOpts FSpecGenOpts where
  fSpecGenOptsL = id
  {-# INLINE fSpecGenOptsL #-}
instance HasFSpecGenOpts DaemonOpts where
  fSpecGenOptsL = lens x2fSpecGenOpts (\x y -> x { x2fSpecGenOpts = y })
instance HasFSpecGenOpts ProtoOpts where
  fSpecGenOptsL = lens x1fSpecGenOpts (\x y -> x { x1fSpecGenOpts = y })


class HasDirPrototype a where
  dirPrototypeL :: Lens' a FilePath
  getTemplateDir :: a -> String
  getTemplateDir x = 
    view dirPrototypeL x </> "templates"
  getAppDir :: a -> String
  getAppDir x =
    view dirPrototypeL x </> "public" </> "app" </> "project"
instance HasDirPrototype ProtoOpts where
  dirPrototypeL = lens xdirPrototype (\x y -> x { xdirPrototype = y })

class HasAllowInvariantViolations a where
  allowInvariantViolationsL :: Lens' a Bool
instance HasAllowInvariantViolations ProtoOpts where
  allowInvariantViolationsL = lens xallowInvariantViolations (\x y -> x { xallowInvariantViolations = y })

class HasRootFile a where
  rootFileL :: Lens' a FilePath
  baseName :: a -> String
  baseName = takeBaseName . view rootFileL
  dirSource :: a -> FilePath -- the directory of the script that is being compiled
  dirSource = takeDirectory . view rootFileL
instance HasRootFile ProtoOpts where
  rootFileL = fSpecGenOptsL . rootFileL
instance HasRootFile FSpecGenOpts where
  rootFileL = lens xrootFile (\x y -> x { xrootFile = y })

class HasOutputLanguage a where
  languageL :: Lens' a (Maybe Lang)  -- The language in which the user wants the documentation to be printed.
instance HasOutputLanguage DaemonOpts where
  languageL = lens xOutputLangugage (\x y -> x { xOutputLangugage = y })
instance HasOutputLanguage ProtoOpts where
  languageL = lens xoutputLangugage (\x y -> x { xoutputLangugage = y })

class HasRunComposer a where
  skipComposerL :: Lens' a Bool -- if True, runs Composer (php package manager) when generating prototype. Requires PHP and Composer on the machine. Added as switch to disable when building with Docker.
instance HasRunComposer ProtoOpts where
  skipComposerL = lens xskipComposer (\x y -> x { xskipComposer = y })


class HasDirCustomizations a where
  dirCustomizationsL :: Lens' a [FilePath] -- the directories that are copied after generating the prototype
instance HasDirCustomizations ProtoOpts where
  dirCustomizationsL = lens xdirCustomizations (\x y -> x { xdirCustomizations = y })

class HasZwolleVersion a where
  zwolleVersionL :: Lens' a String -- the version in github of the prototypeFramework. can be a tagname, a branchname or a SHA
instance HasZwolleVersion ProtoOpts where
  zwolleVersionL = lens xzwolleVersion (\x y -> x { xzwolleVersion = y })

class HasDirOutput a where
  dirOutputL :: Lens' a String -- the directory to generate the output in.

class HasOutputLanguage a => HasGenFuncSpec a where
  genFSpecL :: Lens' a Bool   -- if True, generate a functional design
  diagnosisOnlyL :: Lens' a Bool   -- give a diagnosis only (by omitting the rest of the functional design document)
  fspecFormatL :: Lens' a FSpecFormat -- the format of the generated (pandoc) document(s)
  noDiagnosisL :: Lens' a Bool -- omit the diagnosis chapter from the functional design document.
  genLegalRefsL :: Lens' a Bool   -- Generate a table of legal references in Natural Language chapter
  noGraphicsL :: Lens' a Bool -- Omit generation of graphics during generation of functional design document.

class HasBlackWhite a where
  blackWhiteL :: Lens' a Bool    -- only use black/white in graphics

class HasOutputFile a where
  outputfileAdlL :: Lens' a FilePath
  outputfileDataAnalisysL :: Lens' a FilePath
class HasVersion a where
  preVersionL :: Lens' a String 
  postVersionL :: Lens' a String 

class HasProtoOpts env where
   protoOptsL :: Lens' env ProtoOpts
   dbNameL   :: Lens' env String
   sqlHostL  :: Lens' env String
   sqlLoginL :: Lens' env String
   sqlPwdL   :: Lens' env String
   forceReinstallFrameworkL :: Lens' env Bool
   dbNameL   = protoOptsL . lens xdbName (\x y -> x { xdbName = y })
   sqlHostL  = protoOptsL . lens xsqlHost (\x y -> x { xsqlHost = y })
   sqlLoginL = protoOptsL . lens xsqlLogin (\x y -> x { xsqlLogin = y })
   sqlPwdL   = protoOptsL . lens xsqlPwd (\x y -> x { xsqlPwd = y })
   forceReinstallFrameworkL
             = protoOptsL . lens xforceReinstallFramework (\x y -> x { xforceReinstallFramework = y })
instance HasProtoOpts ProtoOpts where
   protoOptsL = id
   {-# INLINE protoOptsL #-}

-- | Options for @ampersand daemon@.
data DaemonOpts = DaemonOpts
  { xOutputLangugage :: !(Maybe Lang)
  , xdaemonConfig :: !FilePath
  , x2fSpecGenOpts :: !FSpecGenOpts
   -- ^ The path (relative from current directory OR absolute) and filename of a file that contains the root file(s) to be watched by the daemon.
  }
class (HasFSpecGenOpts a, HasOutputLanguage a) => HasDaemonOpts a where
  daemonOptsL :: Lens' a DaemonOpts
  daemonConfigL :: Lens' a FilePath
  daemonConfigL = daemonOptsL . (lens xdaemonConfig (\x y -> x { xdaemonConfig = y }))
instance HasDaemonOpts DaemonOpts where
  daemonOptsL = id
  {-# INLINE daemonOptsL #-}


data FSpecGenOpts = FSpecGenOpts
  { xrootFile :: !FilePath  --relative path
  , xsqlBinTables :: !Bool
  , xgenInterfaces :: !Bool -- 
  , xnamespace :: !String -- prefix database identifiers with this namespace, to isolate namespaces within the same database.
  , xdefaultCrud :: !(Bool,Bool,Bool,Bool)
  , xtrimXLSXCells :: !Bool
  -- ^ Should leading and trailing spaces of text values in .XLSX files be ignored? 
  } deriving Show

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

-- | Options for @ampersand export@.
data ExportOpts = ExportOpts
   { xexport2adl :: !FilePath  --relative path
   }
-- | Options for @ampersand dataAnalisys@.
data DataAnalisysOpts = DataAnalisysOpts
   { xdataAnalysis :: !FilePath  --relative path
   }

-- | Options for @ampersand proto@.
data ProtoOpts = ProtoOpts
   { xdbName :: !String
   -- ^ Name of the database that is generated as part of the prototype
   , xsqlHost ::  !String
   -- ^ do database queries to the specified host
   , xsqlLogin :: !String
   -- ^ pass login name to the database server
   , xsqlPwd :: !String
   -- ^ pass password on to the database server
   , xforceReinstallFramework :: !Bool
   -- ^ when true, an existing prototype directory will be destroyed and re-installed
   , xoutputLangugage :: !(Maybe Lang)
   , x1fSpecGenOpts :: !FSpecGenOpts
   , xskipComposer :: !Bool
   , xdirPrototype :: !FilePath
   , xdirCustomizations :: ![FilePath]
   , xzwolleVersion :: !String
   , xallowInvariantViolations :: !Bool
  } deriving Show

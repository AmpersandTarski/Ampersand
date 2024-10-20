{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Ampersand.Misc.HasClasses where

import Ampersand.Basics
import Ampersand.Misc.Defaults (defaultDirPrototype)
import RIO.FilePath
import qualified RIO.List as L
import qualified RIO.Text as T

class HasOptions a where
  showOptions :: (HasLogFunc env) => a -> RIO env ()
  showOptions = mapM_ showOpt . L.sortOn fst . optsList
    where
      showOpt :: (HasLogFunc env) => (Text, Text) -> RIO env ()
      showOpt (key, value) =
        logDebug . display $ key <> " " <> value
  optsList :: a -> [(Text, Text)] -- A tuple containing the 'key' and the value of the options.
  {-# MINIMAL optsList #-}

instance (HasOptions a, HasOptions b) => HasOptions (a, b) where
  optsList (a, b) = optsList a <> optsList b

-- instance (HasOptions a, Foldable f, Functor f) => HasOptions (f a) where
--  optsList xs = concat . toList . fmap optsList $ xs
class HasTrimXLSXOpts a where
  trimXLSXCellsL :: Lens' a Bool

instance HasTrimXLSXOpts AtlasImportOpts where
  trimXLSXCellsL :: Lens' AtlasImportOpts Bool
  trimXLSXCellsL = lens x1trimXLSXCells (\x y -> x {x1trimXLSXCells = y})

instance (HasFSpecGenOpts a) => HasTrimXLSXOpts a where
  trimXLSXCellsL :: Lens' a Bool
  trimXLSXCellsL = fSpecGenOptsL . lens xtrimXLSXCells (\x y -> x {xtrimXLSXCells = y})

class HasImportFile a where
  importFileL :: Lens' a FilePath

instance HasImportFile AtlasImportOpts where
  importFileL :: Lens' AtlasImportOpts FilePath
  importFileL = lens inputFile (\x y -> x {inputFile = y})

instance HasOutputFile AtlasImportOpts where
  outputfileL = lens xoutputFile (\x y -> x {xoutputFile = y})

class HasFSpecGenOpts a where
  fSpecGenOptsL :: Lens' a FSpecGenOpts
  sqlBinTablesL :: Lens' a Bool
  sqlBinTablesL = fSpecGenOptsL . lens xsqlBinTables (\x y -> x {xsqlBinTables = y})
  genInterfacesL :: Lens' a Bool --
  genInterfacesL = fSpecGenOptsL . lens xgenInterfaces (\x y -> x {xgenInterfaces = y})
  namespaceL :: Lens' a Text -- prefix database identifiers with this namespace, to isolate namespaces within the same database.
  namespaceL = fSpecGenOptsL . lens xnamespace (\x y -> x {xnamespace = y})
  defaultCrudL :: Lens' a (Bool, Bool, Bool, Bool) -- Default values for CRUD functionality in interfaces
  defaultCrudL = fSpecGenOptsL . lens xdefaultCrud (\x y -> x {xdefaultCrud = y})
  recipeL :: Lens' a Recipe
  recipeL = fSpecGenOptsL . lens xrecipe (\x y -> x {xrecipe = y})
  allowInvariantViolationsL :: Lens' a Bool
  allowInvariantViolationsL = fSpecGenOptsL . lens xallowInvariantViolations (\x y -> x {xallowInvariantViolations = y})

instance HasFSpecGenOpts FSpecGenOpts where
  fSpecGenOptsL = id
  {-# INLINE fSpecGenOptsL #-}

instance HasFSpecGenOpts DevOutputOpts where
  fSpecGenOptsL = lens x8fSpecGenOpts (\x y -> x {x8fSpecGenOpts = y})

instance HasFSpecGenOpts ValidateOpts where
  fSpecGenOptsL = protoOptsL . fSpecGenOptsL

instance HasFSpecGenOpts ProofOpts where
  fSpecGenOptsL = lens x6fSpecGenOpts (\x y -> x {x6fSpecGenOpts = y})

instance HasFSpecGenOpts PopulationOpts where
  fSpecGenOptsL = lens x5fSpecGenOpts (\x y -> x {x5fSpecGenOpts = y})

instance HasFSpecGenOpts InputOutputOpts where
  fSpecGenOptsL = lens x4fSpecGenOpts (\x y -> x {x4fSpecGenOpts = y})

instance HasFSpecGenOpts DocOpts where
  fSpecGenOptsL = lens x3fSpecGenOpts (\x y -> x {x3fSpecGenOpts = y})

instance HasFSpecGenOpts DaemonOpts where
  fSpecGenOptsL = lens x2fSpecGenOpts (\x y -> x {x2fSpecGenOpts = y})

instance HasFSpecGenOpts ProtoOpts where
  fSpecGenOptsL = lens x1fSpecGenOpts (\x y -> x {x1fSpecGenOpts = y})

class (HasProtoOpts a) => HasDirPrototype a where
  dirPrototypeL :: Lens' a (Maybe FilePath)
  getTemplateDir :: a -> FilePath
  getTemplateDir x =
    getDirPrototype x </> ".templates"
  getAppDir :: a -> FilePath
  getAppDir = getDirPrototype
  getGenericsDir :: a -> FilePath
  getGenericsDir x =
    getDirPrototype x </> "generics"
  getMetamodelDir :: a -> FilePath
  getMetamodelDir x =
    getDirPrototype x </> "metamodel"
  getDirPrototype :: a -> FilePath
  getDirPrototype = fromMaybe defaultDirPrototype . view dirPrototypeL

instance HasDirPrototype ProtoOpts where
  dirPrototypeL = lens xdirPrototype (\x y -> x {xdirPrototype = y})

class HasGenerateFrontend a where
  generateFrontendL :: Lens' a Bool

instance HasGenerateFrontend ProtoOpts where
  generateFrontendL = lens xgenerateFrontend (\x y -> x {xgenerateFrontend = y})

class HasGenerateBackend a where
  generateBackendL :: Lens' a Bool

instance HasGenerateBackend ProtoOpts where
  generateBackendL = lens xgenerateBackend (\x y -> x {xgenerateBackend = y})

class HasGenerateMetamodel a where
  generateMetamodelL :: Lens' a Bool

instance HasGenerateMetamodel ProtoOpts where
  generateMetamodelL = lens xgenerateMetamodel (\x y -> x {xgenerateMetamodel = y})

-- | A type to denote the root file(s) to be parsed for the creation of an Fspec
newtype Roots = Roots
  { -- | Normally this should be a non-empty list. However, the daemon command is an exception to
    --   this. The command `ampersand daemon` expects no script name. The script name(s) will be
    --   configured by means of the `.ampersand` configuration file.
    getRoots :: [FilePath]
  }

instance Show Roots where
  show = L.intercalate ", " . getRoots

class HasRootFile a where
  rootFileL :: Lens' a Roots
  baseName :: a -> FilePath
  baseName x = case getRoots . view rootFileL $ x of
    [] -> fatal "Cannot determine the basename of the script that is being compiled"
    (h : _) -> takeBaseName h
  dirSource :: a -> FilePath -- the directory of the script that is being compiled
  dirSource = takeDirectory . baseName

instance (HasFSpecGenOpts a) => HasRootFile a where
  rootFileL = fSpecGenOptsL . lens xrootFile (\x y -> x {xrootFile = y})

class HasOutputLanguage a where
  languageL :: Lens' a (Maybe Lang) -- The language in which the user wants the documentation to be printed.

instance HasOutputLanguage ProtoOpts where
  languageL = lens x1OutputLanguage (\x y -> x {x1OutputLanguage = y})

instance HasOutputLanguage DocOpts where
  languageL = lens x3OutputLanguage (\x y -> x {x3OutputLanguage = y})

class HasShowWarnings a where
  showWarningsL :: Lens' a Bool -- Should warnings be given to the output?

instance (HasDaemonOpts a) => HasShowWarnings a where
  showWarningsL = daemonOptsL . lens xshowWarnings (\x y -> x {xshowWarnings = y})

class HasDirOutput a where
  dirOutputL :: Lens' a FilePath -- the directory to generate the output in.

class (HasOutputLanguage a) => HasDocumentOpts a where
  documentOptsL :: Lens' a DocOpts
  chaptersL :: Lens' a [Chapter]
  chaptersL = documentOptsL . lens xchapters (\x y -> x {xchapters = y})
  fspecFormatL :: Lens' a FSpecFormat -- the format of the generated (pandoc) document(s)
  fspecFormatL = documentOptsL . lens xfspecFormat (\x y -> x {xfspecFormat = y})
  genLegalRefsL :: Lens' a Bool -- Generate a table of legal references in Natural Language chapter
  genLegalRefsL = documentOptsL . lens xgenLegalRefs (\x y -> x {xgenLegalRefs = y})
  genGraphicsL :: Lens' a Bool -- Generate graphics. Useful for generating text and graphics separately.
  genGraphicsL = documentOptsL . lens xgenGraphics (\x y -> x {xgenGraphics = y})
  genTextL :: Lens' a Bool -- Generate text. Useful for generating text and graphics separately.
  genTextL = documentOptsL . lens xgenText (\x y -> x {xgenText = y})
  genDatamodelOnlyL :: Lens' a Bool -- Generate only the datamodel images. This overrides genGraphicsL and genTextL
  genDatamodelOnlyL = documentOptsL . lens xgenDatamodelImagesOnly (\x y -> x {xgenDatamodelImagesOnly = y})

instance HasDocumentOpts DocOpts where
  documentOptsL = id

class HasBlackWhite a where
  blackWhiteL :: Lens' a Bool -- only use black/white in graphics

instance HasBlackWhite DocOpts where
  blackWhiteL = lens xblackWhite (\x y -> x {xblackWhite = y})

class HasOutputFile a where
  outputfileL :: Lens' a FilePath

instance HasOutputFile InputOutputOpts where
  outputfileL = lens x4outputFile (\x y -> x {x4outputFile = y})

class HasVersion a where
  preVersionL :: Lens' a Text
  postVersionL :: Lens' a Text

class HasProtoOpts env where
  protoOptsL :: Lens' env ProtoOpts

instance HasProtoOpts ProtoOpts where
  protoOptsL = id
  {-# INLINE protoOptsL #-}

instance HasProtoOpts ValidateOpts where
  protoOptsL = lens protoOpts (\x y -> x {protoOpts = y})

class HasDevoutputOpts env where
  devoutputOptsL :: Lens' env DevOutputOpts

class HasInitOpts env where
  initOptsL :: Lens' env InitOpts

class HasProofOpts env where
  proofOptsL :: Lens' env ProofOpts

class HasPopulationOpts env where
  populationOptsL :: Lens' env PopulationOpts
  outputFormatL :: Lens' env PopulationOutputFormat
  outputFormatL = populationOptsL . lens xoutputFormat (\x y -> x {xoutputFormat = y})

class HasValidateOpts env where
  validateOptsL :: Lens' env ValidateOpts

class HasTestOpts env where
  testOptsL :: Lens' env TestOpts

instance HasTestOpts TestOpts where
  testOptsL = id
  {-# INLINE testOptsL #-}

-- | Options for @ampersand daemon@.
data DaemonOpts = DaemonOpts
  { -- | The path (relative from current directory OR absolute) and filename of a file that contains the root file(s) to be watched by the daemon.
    xdaemonConfig :: !FilePath,
    x2fSpecGenOpts :: !FSpecGenOpts,
    -- | Enable/disable show of warnings (if any).
    xshowWarnings :: !Bool
  }

instance HasOptions DaemonOpts where
  optsList opts =
    [ ("--daemonconfig", tshow $ xdaemonConfig opts)
    ]
      <> optsList (x2fSpecGenOpts opts)
      <> [ ("--[no-]warnings", tshow $ xshowWarnings opts)
         ]

class (HasFSpecGenOpts a) => HasDaemonOpts a where
  daemonOptsL :: Lens' a DaemonOpts
  daemonConfigL :: Lens' a FilePath
  daemonConfigL = daemonOptsL . lens xdaemonConfig (\x y -> x {xdaemonConfig = y})

instance HasDaemonOpts DaemonOpts where
  daemonOptsL = id
  {-# INLINE daemonOptsL #-}

-- | An enumeration type for building an FSpec in some common way
data Recipe
  = -- | Plain way of building. No fancy stuff.
    Standard
  | -- | Generates population for an atlas.
    --   It assumes that the database is fit to receive that population, as RAP does.
    Grind
  | -- | A recipe to build a prototyping environment.
    Prototype
  | -- | A recipe to build a Repository for Ampersand Projects (RAP)
    --   The option 'RAP' generates a database that is fit to receive metamodels, so an Atlas is possible.
    --   The 'makeAtlas' button in RAP uses the 'Grind' option to populate the metamodel.
    RAP
  deriving (Show, Enum, Bounded)

data FSpecGenOpts = FSpecGenOpts
  { xrootFile :: !Roots, -- relative paths. Must be set the first time it is read.
    xsqlBinTables :: !Bool,
    xgenInterfaces :: !Bool, --
    xnamespace :: !Text, -- prefix database identifiers with this namespace, to isolate namespaces within the same database.
    xdefaultCrud :: !(Bool, Bool, Bool, Bool),
    xtrimXLSXCells :: !Bool,
    -- | Which recipe for generating code?
    xrecipe :: !Recipe,
    -- | Should invariant violations be ignored?
    xallowInvariantViolations :: !Bool
  }
  deriving (Show)

instance HasOptions FSpecGenOpts where
  optsList opts =
    [ ("AMPERSAND_SCRIPT", tshow $ xrootFile opts),
      ("--sql-bin-tables", tshow $ xsqlBinTables opts),
      ("--interfaces", tshow $ xgenInterfaces opts),
      ("--namespace", tshow $ xnamespace opts),
      ( "--crud-defaults",
        let (c, r, u, d) = xdefaultCrud opts
            f :: Bool -> Text -> Text
            f b = (if b then T.toUpper else T.toLower)
         in mconcat [f c "c", f r "r", f u "u", f d "d"]
      ),
      ("--[no-]trim-cellvalues", tshow $ xtrimXLSXCells opts),
      ("--build-recipe", tshow $ xrecipe opts),
      ("--ignore-invariant-violations", tshow $ xallowInvariantViolations opts)
    ]

data FSpecFormat
  = FPandoc
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
newtype ExportOpts = ExportOpts
  { xexport2adl :: FilePath -- relative path
  }

-- | Options for @ampersand dataAnalysis@ and @ampersand export@.
data InputOutputOpts = InputOutputOpts
  { x4fSpecGenOpts :: !FSpecGenOpts,
    x4outputFile :: !FilePath -- relative path
  }

instance HasOptions InputOutputOpts where
  optsList opts =
    optsList (x4fSpecGenOpts opts)
      <> [ ("OUTPUTDIRECTORY", tshow $ x4outputFile opts)
         ]

-- | Options for @ampersand proto@.
data ProtoOpts = ProtoOpts
  { x1OutputLanguage :: !(Maybe Lang),
    x1fSpecGenOpts :: !FSpecGenOpts,
    xdirPrototype :: !(Maybe FilePath),
    xgenerateFrontend :: !Bool,
    xgenerateBackend :: !Bool,
    xgenerateMetamodel :: !Bool
  }
  deriving (Show)

instance HasOptions ProtoOpts where
  optsList opts =
    [ ("--language", tshow $ x1OutputLanguage opts)
    ]
      <> optsList (x1fSpecGenOpts opts)
      <> [ ("--proto-dir", maybe "" tshow $ xdirPrototype opts),
           ("--[no-]frontend", tshow $ xgenerateFrontend opts),
           ("--[no-]backend", tshow $ xgenerateBackend opts),
           ("--[no-]metamodel", tshow $ xgenerateMetamodel opts)
         ]

-- | Options for @ampersand documentation@.
data DocOpts = DocOpts
  { -- | avoid coloring conventions to facilitate readable pictures in black and white.
    xblackWhite :: !Bool,
    -- | a list containing all chapters that are required to be in the generated documentation
    xchapters :: ![Chapter],
    -- | Only generate datamodel images.
    xgenDatamodelImagesOnly :: !Bool,
    -- | enable/disable generation of graphics. Used to generate text and graphics in separation.
    xgenGraphics :: !Bool,
    -- | enable/disable generation of text. Used to generate text and graphics in separation.
    xgenText :: !Bool,
    -- | the format of the documentation
    xfspecFormat :: !FSpecFormat,
    -- | Options required to build the fSpec
    x3fSpecGenOpts :: !FSpecGenOpts,
    -- | Language of the output document
    x3OutputLanguage :: !(Maybe Lang),
    -- | enable/disable generation of legal references in the documentation
    xgenLegalRefs :: !Bool
  }
  deriving (Show)

instance HasOptions DocOpts where
  optsList opts =
    [ ("--blackWhite", tshow $ xblackWhite opts)
    ]
      <> fmap chapters [minBound ..]
      <> [ ("--datamodelOnly", tshow $ xgenDatamodelImagesOnly opts),
           ("--[no-]graphics", tshow $ xgenGraphics opts),
           ("--[no-]text", tshow $ xgenText opts),
           ("--format", tshow $ xfspecFormat opts)
         ]
      <> optsList (x3fSpecGenOpts opts)
      <> [ ("--language", tshow $ x3OutputLanguage opts),
           ("--[no-]legal-refs", tshow $ xgenLegalRefs opts)
         ]
    where
      chapters :: Chapter -> (Text, Text)
      chapters chp = ("--[no-]" <> tshow chp, tshow $ chp `elem` xchapters opts)

data PopulationOutputFormat
  = XLSX
  | JSON
  deriving (Show, Enum, Bounded)

-- | Options for @ampersand population@
data PopulationOpts = PopulationOpts
  { -- | Options required to build the fSpec
    x5fSpecGenOpts :: !FSpecGenOpts,
    xoutputFormat :: !PopulationOutputFormat
  }
  deriving (Show)

instance HasOptions PopulationOpts where
  optsList opts =
    optsList (x5fSpecGenOpts opts)
      <> [ ("--output-format", tshow $ xoutputFormat opts)
         ]

instance HasPopulationOpts PopulationOpts where
  populationOptsL = id
  outputFormatL = populationOptsL . lens xoutputFormat (\x y -> x {xoutputFormat = y})
  {-# INLINE populationOptsL #-}

-- | Options for @ampersand proofs@
newtype ProofOpts = ProofOpts
  { -- | Options required to build the fSpec
    x6fSpecGenOpts :: FSpecGenOpts
  }
  deriving (Show)

instance HasOptions ProofOpts where
  optsList opts =
    optsList (x6fSpecGenOpts opts)

-- | Options for @ampersand init@
data InitOpts = InitOpts
  deriving (Show)

-- | Options for @ampersand validate@
newtype ValidateOpts = ValidateOpts
  { -- | Options required to build the fSpec
    protoOpts :: ProtoOpts
  }
  deriving (Show)

instance HasOptions ValidateOpts where
  optsList opts =
    optsList (protoOpts opts)

-- | Options for @ampersand devoutput@
data DevOutputOpts = DevOutputOpts
  { -- | Options required to build the fSpec
    x8fSpecGenOpts :: !FSpecGenOpts,
    x5outputFile :: !FilePath -- relative path
  }
  deriving (Show)

instance HasOptions DevOutputOpts where
  optsList opts =
    optsList (x8fSpecGenOpts opts)
      <> [ ("OUTPUTDIRECTORY", tshow $ x5outputFile opts)
         ]

newtype TestOpts = TestOpts
  { rootTestDir :: FilePath -- relative path to directory containing test scripts
  }
  deriving (Show)

instance HasOptions TestOpts where
  optsList opts =
    [ ("TESTDIRECTORY", tshow $ rootTestDir opts)
    ]

data AtlasImportOpts = AtlasImportOpts
  { x1trimXLSXCells :: !Bool,
    inputFile :: !FilePath, -- relative path to file containing the population of the Atlas
    xoutputFile :: !FilePath
  }
  deriving (Show)

instance HasOptions AtlasImportOpts where
  optsList opts =
    [ ("ATLASPOPULATIONFILE", tshow $ inputFile opts)
    ]

data Chapter
  = Intro
  | SharedLang
  | Diagnosis
  | ConceptualAnalysis
  | DataAnalysis
  deriving (Eq, Show, Enum, Bounded)

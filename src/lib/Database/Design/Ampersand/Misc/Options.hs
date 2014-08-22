{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -Wall #-}
module Database.Design.Ampersand.Misc.Options
        (Options(..),getOptions,usageInfo'
        ,ParserVersion(..)
        ,verboseLn,verbose,FspecFormat(..),FileFormat(..)
        ,DocTheme(..),allFspecFormats,helpNVersionTexts)
where
import System.Environment    (getArgs, getProgName,getEnvironment,getExecutablePath )
import Database.Design.Ampersand.Misc.Languages (Lang(..))
import Data.Char (toUpper)
import System.Console.GetOpt
import System.FilePath
import System.Directory
import Data.Time.Clock
import Data.Time.LocalTime
import Control.Monad
import Data.Maybe
import Database.Design.Ampersand.Basics
import Paths_ampersand (getDataDir)
import Prelude hiding (writeFile,readFile,getContents,putStr,putStrLn)
import Data.List

fatal :: Int -> String -> a
fatal = fatalMsg "Misc.Options"

data ParserVersion = Current | Legacy deriving Eq

instance Show ParserVersion where
  show Current = "syntax since Ampersand 2.1.1."
  show Legacy = "syntax664"

-- | This data constructor is able to hold all kind of information that is useful to
--   express what the user would like Ampersand to do.
data Options = Options { showVersion :: Bool
                       , preVersion :: String
                       , postVersion :: String  --built in to aid DOS scripting... 8-(( Bummer.
                       , showHelp :: Bool
                       , verboseP :: Bool
                       , development :: Bool
                       , validateSQL :: Bool
                       , genPrototype :: Bool
                       , dirPrototype :: String  -- the directory to generate the prototype in.
                       , allInterfaces :: Bool
                       , dbName :: String
                       , genAtlas :: Bool
                       , namespace :: String
                       , autoRefresh :: Maybe Int
                       , testRule :: Maybe String
                       , customCssFile :: Maybe FilePath
                       , importfile :: FilePath --a file with content to populate some (Populated a)
                                                   --class Populated a where populate::a->b->a
                       , fileformat :: Maybe FileFormat --file format e.g. of importfile or export2adl
                       , theme :: DocTheme --the theme of some generated output. (style, content differentiation etc.)
                       , genXML :: Bool
                       , genFspec :: Bool   -- if True, generate a functional specification
                       , diag :: Bool   -- if True, generate a diagnosis only
                       , fspecFormat :: FspecFormat
                       , genGraphics :: Bool   -- if True, graphics will be generated for use in Ampersand products like the Atlas or Functional Spec
                       , genEcaDoc :: Bool   -- if True, generate ECA rules in the Functional Spec
                       , proofs :: Bool
                       , haskell :: Bool   -- if True, generate the F-structure as a Haskell source file
                       , dirOutput :: String -- the directory to generate the output in.
                       , outputfile :: String -- the file to generate the output in.
                       , crowfoot :: Bool   -- if True, generate conceptual models and data models in crowfoot notation
                       , blackWhite :: Bool   -- only use black/white in graphics
                       , doubleEdges :: Bool   -- Graphics are generated with hinge nodes on edges.
                       , showPredExpr :: Bool   -- for generated output, show predicate logic?
                       , noDiagnosis :: Bool   -- omit the diagnosis chapter from the functional specification document
                       , diagnosisOnly :: Bool   -- give a diagnosis only (by omitting the rest of the functional specification document)
                       , genLegalRefs :: Bool   -- Generate a table of legal references in Natural Language chapter
                       , genUML :: Bool   -- Generate a UML 2.0 data model
                       , genFPAExcel :: Bool   -- Generate an Excel workbook containing Function Point Analisys
                       , genStaticFiles :: Bool-- Generate the static files into the prototype
                       , genBericht :: Bool
                       , genMeat :: Bool  -- Generate the meta-population and output it to an .adl file
                       , language :: Maybe Lang  -- The language in which the user wants the documentation to be printed.
                       , dirExec :: String --the base for relative paths to input files
                       , ampersandDataDir :: FilePath -- the directory where Ampersand data files are.
                       , progrName :: String --The name of the adl executable
                       , fileName :: FilePath --the file with the Ampersand context
                       , baseName :: String
                       , logName :: FilePath
                       , genTime :: LocalTime
                       , export2adl :: Bool
                       , test :: Bool
                       , includeRap :: Bool  -- When set, the standard RAP is 'merged' into the generated prototype.(experimental)
                       , pangoFont :: String  -- use specified font in PanDoc. May be used to avoid pango-warnings.
                       , sqlHost ::  String  -- do database queries to the specified host
                       , sqlLogin :: String  -- pass login name to the database server
                       , sqlPwd :: String  -- pass password on to the database server
                       , parserVersion :: ParserVersion
                       , oldNormalizer :: Bool
                       }

getOptions :: IO Options
getOptions =
   do args     <- getArgs
      progName <- getProgName
      exePath  <- getExecutablePath -- findExecutable progName
      dataPath <- getDataDir
      let haskellInstallationDirectory = dataPath </> "AmpersandData"
      existshaskellInstallationDirectory <- doesDirectoryExist  haskellInstallationDirectory
--      putStrLn  $ "haskellInstallationDirectory: "  ++haskellInstallationDirectory

      let ampersandInstallationDirectory = takeDirectory exePath </> ".." </> "AmpersandData"
      existsampersandInstallationDirectory <- doesDirectoryExist  ampersandInstallationDirectory
--      putStrLn  $ "ampersandInstallationDirectory: "++ampersandInstallationDirectory

      let dataDir
            | existshaskellInstallationDirectory   = haskellInstallationDirectory
            | existsampersandInstallationDirectory = ampersandInstallationDirectory
            | otherwise                            = exePath
      localTime <-  do utcTime <- getCurrentTime
                       timeZone <- getCurrentTimeZone
                       return (utcToLocalTime timeZone utcTime)
      env <- getEnvironment
      let usage = "\nType '"++ progName++" --help' for usage info."
      let (actions, fNames, errors) = getOpt Permute (each options) args
      when ((not.null) errors) (error $ concat errors ++ usage)
      let fName = case fNames of
                   []  -> error ("Please supply the name of an ampersand file" ++ usage)
                   [x] -> x
                   _   -> error ("too many files: "++ intercalate ", " fNames  ++ usage)

      let startOptions =
               Options {genTime       = localTime
                      , dirOutput     = fromMaybe "."       (lookup envdirOutput    env)
                      , outputfile    = fatal 83 "No monadic options available."
                      , dirPrototype  = fromMaybe ("." </> (addExtension (takeBaseName fName) ".proto"))
                                                  (lookup envdirPrototype env) </> (addExtension (takeBaseName fName) ".proto")
                      , dbName        = fromMaybe (takeBaseName fName) (lookup envdbName env)
                      , logName       = fromMaybe "Ampersand.log" (lookup envlogName      env)
                      , dirExec       = takeDirectory exePath
                      , ampersandDataDir = dataDir
                      , preVersion    = fromMaybe ""        (lookup "CCPreVersion"  env)
                      , postVersion   = fromMaybe ""        (lookup "CCPostVersion" env)
                      , theme         = DefaultTheme
                      , showVersion   = False
                      , showHelp      = False
                      , verboseP      = False
                      , development   = False
                      , validateSQL   = False
                      , genPrototype  = False
                      , allInterfaces = False
                      , genAtlas      = False
                      , namespace     = []
                      , autoRefresh   = Nothing
                      , testRule      = Nothing
                      , customCssFile = Nothing
                      , importfile    = []
                      , fileformat    = Nothing
                      , genXML        = False
                      , genFspec      = False
                      , diag          = False
                      , fspecFormat   = fatal 105 $ "Unknown fspec format. Currently supported formats are "++allFspecFormats++"."
                      , genGraphics   = True
                      , genEcaDoc     = False
                      , proofs        = False
                      , haskell       = False
                      , crowfoot      = False
                      , blackWhite    = False
                      , doubleEdges   = False
                      , showPredExpr  = False
                      , noDiagnosis   = False
                      , diagnosisOnly = False
                      , genLegalRefs  = False
                      , genUML        = False
                      , genFPAExcel   = False
                      , genStaticFiles= True
                      , genBericht    = False
                      , genMeat       = False
                      , language      = Nothing
                      , progrName     = progName
                      , fileName      = if hasExtension fName
                                        then fName
                                        else addExtension fName "adl"
                      , baseName      = takeBaseName fName
                      , export2adl    = False
                      , test          = False
                      , includeRap    = False
                      , pangoFont     = "Sans"
                      , sqlHost       = "localhost"
                      , sqlLogin      = "ampersand"
                      , sqlPwd        = "ampersand"
                      , parserVersion = Current
                      , oldNormalizer = False
                      }
      -- Here we thread startOptions through all supplied option actions
      opts <- foldl (>>=) (return startOptions) actions
      -- Now we do some checks on the options:
      when (development opts && validateSQL opts)
           (error "--dev and --validate must not be used at the same time.") --(Reason: see ticket #378))
      createDirectoryIfMissing True (takeDirectory (logName opts))
      createDirectoryIfMissing True (dirOutput opts)
      when (genPrototype opts)
           (createDirectoryIfMissing True (dirPrototype opts))
      return opts

data DisplayMode = Public | Hidden

data FspecFormat = FPandoc| Fasciidoc| Fcontext| Fdocbook| Fhtml| FLatex| Fman| Fmarkdown| Fmediawiki| Fopendocument| Forg| Fplain| Frst| Frtf| Ftexinfo| Ftextile deriving (Show, Eq)
allFspecFormats :: String
allFspecFormats = show (map (tail . show) [FPandoc, Fasciidoc, Fcontext, Fdocbook, Fhtml, FLatex, Fman, Fmarkdown, Fmediawiki, Fopendocument, Forg, Fplain, Frst, Frtf, Ftexinfo, Ftextile])

data FileFormat = Adl1Format | Adl1PopFormat  deriving (Show, Eq) --file format that can be parsed to some b to populate some Populated a
data DocTheme = DefaultTheme   -- Just the functional specification
              | ProofTheme     -- A document with type inference proofs
              | StudentTheme   -- Output for normal students of the business rules course
              | StudentDesignerTheme   -- Output for advanced students of the business rules course
              | DesignerTheme   -- Output for non-students
                 deriving (Show, Eq)

usageInfo' :: Options -> String
-- When the user asks --help, then the public options are listed. However, if also --verbose is requested, the hidden ones are listed too.
usageInfo' opts = usageInfo (infoHeader (progrName opts)) (if verboseP opts then each options else publics options)

infoHeader :: String -> String
infoHeader progName = "\nUsage info:\n " ++ progName ++ " options file ...\n\nList of options:"

publics :: [(a, DisplayMode) ] -> [a]
publics opts = [o | (o,Public)<-opts]
each :: [(a, DisplayMode) ] -> [a]
each opts = [o |(o,_) <- opts]

type OptionDef = OptDescr (Options -> IO Options)
options :: [(OptionDef, DisplayMode) ]
options = map pp
          [ (Option "v"     ["version"]
               (NoArg (\opts -> return opts{showVersion = True}))
               "show version and exit."
            , Public)
          , (Option "h?"    ["help"]
               (NoArg (\opts -> return opts{showHelp = True}))
               "get (this) usage information."
            , Public)
          , (Option ""      ["verbose"]
               (NoArg (\opts -> return opts{verboseP = True}))
               "verbose error message format."
            , Public)
          , (Option ""      ["dev"]
               (NoArg (\opts -> return opts{development = True}))
               "Report and generate extra development information"
            , Hidden)
          , (Option ""      ["validate"]
               (NoArg (\opts -> return opts{validateSQL = True}))
               "Compare results of rule evaluation in Haskell and SQL (requires command line php with MySQL support)"
            , Hidden)
          , (Option "p"     ["proto"]
               (OptArg (\nm opts -> return opts {dirPrototype = fromMaybe (dirPrototype opts) nm
                                                  ,genPrototype = True}
                       ) "DIRECTORY")
               ("generate a functional prototype (overwrites environment variable "++ envdirPrototype ++ ").")
            , Public)
          , (Option "d"     ["dbName"]
               (ReqArg (\nm opts -> return opts{dbName = if nm == ""
                                                           then baseName opts
                                                           else nm}
                       ) "NAME")
               ("database name (overwrites environment variable "++ envdbName ++ ", defaults to filename)")
            , Public)
          , (Option []      ["theme"]
               (ReqArg (\t opts -> return opts{theme = case map toUpper t of
                                                          "STUDENT"  -> StudentTheme
                                                          "STUDENTDESIGNER" -> StudentDesignerTheme
                                                          "DESIGNER" -> DesignerTheme
                                                          "PROOF"    -> ProofTheme
                                                          _          -> DefaultTheme}
                        ) "THEME")
               "differentiate between certain outputs e.g. student"
            , Public)
          , (Option "x"     ["interfaces"]
               (NoArg (\opts -> return opts{allInterfaces  = True}))
               "generate interfaces."
            , Public)
          , (Option "e"     ["export"]
               (OptArg (\mbnm opts -> return opts{export2adl = True
                                                   ,outputfile = fromMaybe "Export.adl" mbnm}) "file")
               "export as plain Ampersand script."
            , Public)
          , (Option "o"     ["outputDir"]
               (ReqArg (\nm opts -> return opts{dirOutput = nm}
                       ) "DIR")
               ("output directory (dir overwrites environment variable "++ envdirOutput ++ ").")
            , Public)
          , (Option []      ["log"]
               (ReqArg (\nm opts -> return opts{logName = nm}
                       ) "NAME")
               ("log file name (name overwrites environment variable "++ envlogName  ++ ").")
            , Hidden)
          , (Option []      ["import"]
               (ReqArg (\nm opts -> return opts{importfile = nm}
                       ) "FILE")
               "import this file as the population of the context."
            , Public)
          , (Option []      ["fileformat"]
               (ReqArg (\f opts -> return
                             opts{fileformat = case map toUpper f of
                                                 "ADL" -> Just Adl1Format
                                                 "ADL1"-> Just Adl1Format
                                                 "POP" -> Just Adl1PopFormat
                                                 "POP1"-> Just Adl1PopFormat
                                                 _     -> fileformat opts
                                  }
                       ) "FORMAT")
               ("format of import file (format=ADL (.adl), ADL1 (.adl), POP (.pop), POP1 (.pop)).")
            , Public)
          , (Option []      ["namespace"]
               (ReqArg (\nm opts -> return opts{namespace = nm}
                       ) "NAMESPACE")
               "places the population in this namespace within the context."
            , Public)
          , (Option "f"     ["fspec"]
               (ReqArg (\w opts -> return opts
                                { genFspec=True
                                , fspecFormat= case map toUpper w of
                                    ('A': _ )             -> Fasciidoc
                                    ('C': _ )             -> Fcontext
                                    ('D': _ )             -> Fdocbook
                                    ('H': _ )             -> Fhtml
                                    ('L': _ )             -> FLatex
                                    ('M':'A':'N': _ )     -> Fman
                                    ('M':'A': _ )         -> Fmarkdown
                                    ('M':'E': _ )         -> Fmediawiki
                                    ('O':'P': _ )         -> Fopendocument
                                    ('O':'R': _ )         -> Forg
                                    ('P':'A': _ )         -> FPandoc
                                    ('P':'L': _ )         -> Fplain
                                    ('R':'S': _ )         -> Frst
                                    ('R':'T': _ )         -> Frtf
                                    ('T':'E':'X':'I': _ ) -> Ftexinfo
                                    ('T':'E':'X':'T': _ ) -> Ftextile
                                    _                     -> fspecFormat opts}
                       ) "FORMAT")
               ("generate a functional specification document in specified format (format="++allFspecFormats++").")
            , Public)
          , (Option []        ["refresh"]
               (OptArg (\r opts -> return
                            opts{autoRefresh = Just (case r of
                                                       Just str | [(i,"")] <- reads str -> i
                                                       _                                -> 5
                                                     )}
                       ) "INTERVAL")
               "Experimental auto-refresh feature"
            , Hidden)
          , (Option []        ["testRule"]
               (ReqArg (\ruleName opts -> return opts{ testRule = Just ruleName }
                       ) "RULE")
               "Show contents and violations of specified rule."
            , Hidden)
          , (Option []        ["css"]
               (ReqArg (\pth opts -> return opts{ customCssFile = Just pth }) "file")
               "Custom.css file to customize the style of the prototype."
            , Public)
          , (Option []        ["noGraphics"]
               (NoArg (\opts -> return opts{genGraphics = False}))
               "save compilation time by not generating any graphics."
            , Public)
          , (Option []        ["ECA"]
               (NoArg (\opts -> return opts{genEcaDoc = True}))
               "generate documentation with ECA rules."
            , Public)
          , (Option []        ["proofs"]
               (NoArg (\opts -> return opts{proofs = True}))
               "generate derivations."
            , Public)
          , (Option []        ["XML"]
               (NoArg (\opts -> return opts{genXML = True}))
               "generate internal data structure, written in XML (for debugging)."
            , Public)
          , (Option []        ["haskell"]
               (NoArg (\opts -> return opts{haskell = True}))
               "generate internal data structure, written in Haskell (for debugging)."
            , Public)
          , (Option []        ["crowfoot"]
               (NoArg (\opts -> return opts{crowfoot = True}))
               "generate crowfoot notation in graphics."
            , Public)
          , (Option []        ["blackWhite"]
               (NoArg (\opts -> return opts{blackWhite = True}))
               "do not use colours in generated graphics"
            , Public)
          , (Option []        ["doubleEdges"]
               (NoArg (\opts -> return opts{doubleEdges = not (doubleEdges opts)}))
               "generate graphics in an alternate way. (you may experiment with this option to see the differences for yourself)"
            , Public)
          , (Option []        ["predLogic"]
               (NoArg (\opts -> return opts{showPredExpr = True}))
               "show logical expressions in the form of predicate logic."
            , Public)
          , (Option []        ["noDiagnosis"]
               (NoArg (\opts -> return opts{noDiagnosis = True}))
               "omit the diagnosis chapter from the functional specification document."
            , Public)
          , (Option []        ["diagnosis"]
               (NoArg (\opts -> return opts{diagnosisOnly = True}))
               "diagnose your Ampersand script (generates a .pdf file)."
            , Public)
          , (Option []        ["legalrefs"]
               (NoArg (\opts -> return opts{genLegalRefs = True}))
               "generate a table of legal references in Natural Language chapter."
            , Public)
          , (Option []        ["uml"]
               (NoArg (\opts -> return opts{genUML = True}))
               "Generate a UML 2.0 data model."
            , Hidden)
          , (Option []        ["FPA"]
               (NoArg (\opts -> return opts{genFPAExcel = True}))
               "Generate a Excel workbook (.xls)."
            , Hidden)
          , (Option []        ["bericht"]
               (NoArg (\opts -> return opts{genBericht = True}))
               "Generate definitions for 'berichten' (specific to INDOORS project)."
            , Hidden)
          , (Option []        ["language"]
               (ReqArg (\l opts-> return opts{language = case map toUpper l of
                                                       "NL"  -> Just Dutch
                                                       "UK"  -> Just English
                                                       "US"  -> Just English
                                                       "EN"  -> Just English
                                                       _     -> Nothing}
                       ) "LANG")
               "Pick 'NL' for Dutch or 'EN' for English, as the language to be used in your output. Without this option, output is written in the language of your context."
            , Public)
          , (Option []        ["test"]
               (NoArg (\opts -> return opts{test = True}))
               "Used for test purposes only."
            , Hidden)
          , (Option []        ["rap"]
               (NoArg (\opts -> return opts{includeRap = True}))
               "Include RAP into the generated artifacts (experimental)"
            , Hidden)
          , (Option []        ["meta"]
               (NoArg (\opts -> return opts{genMeat = True}))
               "Generate meta-population in an .adl file (experimental)"
            , Hidden)
          , (Option []        ["pango"]
               (ReqArg (\nm opts -> return opts{pangoFont = nm}
                       ) "FONTNAME")
               "specify font name for Pango in graphics."
            , Hidden)
          , (Option []   ["no-static-files"]
               (NoArg  (\opts -> return opts{genStaticFiles = False}))
               "Do not generate static files into the prototype directory"
            , Public)
          , (Option []        ["sqlHost"]
               (ReqArg (\nm opts -> return opts{sqlHost = nm}
                       ) "HOSTNAME")
               "specify database host name."
            , Hidden)
          , (Option []        ["sqlLogin"]
               (ReqArg (\nm opts -> return opts{sqlLogin = nm}
                       ) "NAME")
               "specify database login name."
            , Hidden)
          , (Option []        ["sqlPwd"]
               (ReqArg (\nm opts -> return opts{sqlPwd = nm}
                       ) "STR")
               "specify database password."
            , Hidden)
          , (Option "on"     ["oldNormalizer"]
               (NoArg (\opts -> return opts{oldNormalizer = True}))
               "use the old normalizer at your own risk."
            , Hidden)
          ]
     where pp :: (OptionDef, DisplayMode) -> (OptionDef, DisplayMode)
           pp (Option a b' c d,e) = (Option a b' c d',e)
              where d' =  afkappen [] [] (words d) 40
                    afkappen :: [[String]] -> [String] -> [String] -> Int -> String
                    afkappen regels []    []   _ = intercalate "\n" (map unwords regels)
                    afkappen regels totnu []   b = afkappen (regels++[totnu]) [] [] b
                    afkappen regels totnu (w:ws) b
                          | length (unwords totnu) < b - length w = afkappen regels (totnu++[w]) ws b
                          | otherwise                             = afkappen (regels++[totnu]) [w] ws b

envdirPrototype :: String
envdirPrototype = "CCdirPrototype"
envdirOutput :: String
envdirOutput="CCdirOutput"
envdbName :: String
envdbName="CCdbName"
envlogName :: String
envlogName="CClogName"

verbose :: Options -> String -> IO ()
verbose opts x
   | verboseP opts = putStr x
   | otherwise     = return ()

verboseLn :: Options -> String -> IO ()
verboseLn opts x
   | verboseP opts = -- each line is handled separately, so the buffer will be flushed in time. (see ticket #179)
                     mapM_ putStrLn (lines x)
   | otherwise     = return ()
helpNVersionTexts :: String -> Options -> [String]
helpNVersionTexts vs opts = ["Executable: "++show (dirExec opts)++"\n"   | test opts       ]++
                            [preVersion opts++vs++postVersion opts++"\n" | showVersion opts]++
                            [usageInfo' opts                             | showHelp    opts]


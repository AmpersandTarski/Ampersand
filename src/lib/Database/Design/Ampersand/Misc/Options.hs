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
                      }
      -- Here we thread startOptions through all supplied option actions
      flags <- foldl (>>=) (return startOptions) actions
      -- Now we do some checks on the flags:
      when (development flags && validateSQL flags) 
           (error "--dev and --validate must not be used at the same time.") --(Reason: see ticket #378)) 
      createDirectoryIfMissing True (takeDirectory (logName flags)) 
      createDirectoryIfMissing True (dirOutput flags)
      when (genPrototype flags)
           (createDirectoryIfMissing True (dirPrototype flags))
      return flags
 
            
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
usageInfo' flags = usageInfo (infoHeader (progrName flags)) (if verboseP flags then each options else publics options)
          
infoHeader :: String -> String
infoHeader progName = "\nUsage info:\n " ++ progName ++ " options file ...\n\nList of options:"

publics :: [(a, DisplayMode) ] -> [a]
publics flags = [o | (o,Public)<-flags]
each :: [(a, DisplayMode) ] -> [a]
each flags = [o |(o,_) <- flags]

type OptionDef = OptDescr (Options -> IO Options)
options :: [(OptionDef, DisplayMode) ]
options = map pp
          [ (Option "v"     ["version"]
               (NoArg (\flags -> return flags{showVersion = True}))
               "show version and exit."
            , Public)
          , (Option "h?"    ["help"]
               (NoArg (\flags -> return flags{showHelp = True}))
               "get (this) usage information."
            , Public)
          , (Option ""      ["verbose"]
               (NoArg (\flags -> return flags{verboseP = True}))
               "verbose error message format."
            , Public)
          , (Option ""      ["dev"]
               (NoArg (\flags -> return flags{development = True}))
               "Report and generate extra development information"
            , Hidden)
          , (Option ""      ["validate"]
               (NoArg (\flags -> return flags{validateSQL = True}))
               "Compare results of rule evaluation in Haskell and SQL (requires command line php with MySQL support)"
            , Hidden)
          , (Option "p"     ["proto"]
               (OptArg (\nm flags -> return flags {dirPrototype = fromMaybe (dirPrototype flags) nm
                                                  ,genPrototype = True}
                       ) "DIRECTORY")
               ("generate a functional prototype (overwrites environment variable "++ envdirPrototype ++ ").")
            , Public)
          , (Option "d"     ["dbName"]
               (ReqArg (\nm flags -> return flags{dbName = if nm == "" 
                                                           then baseName flags
                                                           else nm}                          
                       ) "NAME")
               ("database name (overwrites environment variable "++ envdbName ++ ", defaults to filename)")
            , Public)
          , (Option []      ["theme"]
               (ReqArg (\t flags -> return flags{theme = case map toUpper t of 
                                                          "STUDENT"  -> StudentTheme
                                                          "STUDENTDESIGNER" -> StudentDesignerTheme
                                                          "DESIGNER" -> DesignerTheme
                                                          "PROOF"    -> ProofTheme
                                                          _          -> DefaultTheme}
                        ) "THEME")
               "differentiate between certain outputs e.g. student"
            , Public)
          , (Option "x"     ["interfaces"]
               (NoArg (\flags -> return flags{allInterfaces  = True}))
               "generate interfaces."
            , Public)
          , (Option "e"     ["export"]
               (OptArg (\mbnm flags -> return flags{export2adl = True
                                                   ,outputfile = fromMaybe "Export.adl" mbnm}) "file")
               "export as plain Ampersand script."
            , Public)
          , (Option "o"     ["outputDir"]
               (ReqArg (\nm flags -> return flags{dirOutput = nm}
                       ) "DIR")
               ("output directory (dir overwrites environment variable "++ envdirOutput ++ ").")
            , Public)
          , (Option []      ["log"]
               (ReqArg (\nm flags -> return flags{logName = nm}
                       ) "NAME")
               ("log file name (name overwrites environment variable "++ envlogName  ++ ").")
            , Hidden)
          , (Option []      ["import"]
               (ReqArg (\nm flags -> return flags{importfile = nm}
                       ) "FILE")
               "import this file as the population of the context."
            , Public)
          , (Option []      ["fileformat"]
               (ReqArg (\f flags -> return 
                             flags{fileformat = case map toUpper f of
                                                 "ADL" -> Just Adl1Format
                                                 "ADL1"-> Just Adl1Format
                                                 "POP" -> Just Adl1PopFormat
                                                 "POP1"-> Just Adl1PopFormat
                                                 _     -> fileformat flags
                                  }
                       ) "FORMAT")
               ("format of import file (format=ADL (.adl), ADL1 (.adl), POP (.pop), POP1 (.pop)).")
            , Public)
          , (Option []      ["namespace"]
               (ReqArg (\nm flags -> return flags{namespace = nm}
                       ) "NAMESPACE")
               "places the population in this namespace within the context."
            , Public)
          , (Option "f"     ["fspec"]
               (ReqArg (\w flags -> return flags
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
                                    _                     -> fspecFormat flags}
                       ) "FORMAT")  
               ("generate a functional specification document in specified format (format="++allFspecFormats++").")
            , Public)
          , (Option []        ["refresh"]
               (OptArg (\r flags -> return 
                            flags{autoRefresh = Just (case r of
                                                       Just str | [(i,"")] <- reads str -> i
                                                       _                                -> 5
                                                     )}
                       ) "INTERVAL")
               "Experimental auto-refresh feature"
            , Hidden)
          , (Option []        ["testRule"]
               (ReqArg (\ruleName flags -> return flags{ testRule = Just ruleName }
                       ) "RULE")
               "Show contents and violations of specified rule."
            , Hidden)
          , (Option []        ["css"]
               (ReqArg (\pth flags -> return flags{ customCssFile = Just pth }) "file")
               "Custom.css file to customize the style of the prototype."
            , Public)
          , (Option []        ["noGraphics"]
               (NoArg (\flags -> return flags{genGraphics = False}))
               "save compilation time by not generating any graphics."
            , Public)
          , (Option []        ["ECA"]
               (NoArg (\flags -> return flags{genEcaDoc = True}))
               "generate documentation with ECA rules."
            , Public)
          , (Option []        ["proofs"]
               (NoArg (\flags -> return flags{proofs = True}))
               "generate derivations."
            , Public)
          , (Option []        ["XML"]
               (NoArg (\flags -> return flags{genXML = True}))
               "generate internal data structure, written in XML (for debugging)."
            , Public)
          , (Option []        ["haskell"]
               (NoArg (\flags -> return flags{haskell = True}))
               "generate internal data structure, written in Haskell (for debugging)."
            , Public)
          , (Option []        ["crowfoot"]
               (NoArg (\flags -> return flags{crowfoot = True}))
               "generate crowfoot notation in graphics."
            , Public)
          , (Option []        ["blackWhite"]
               (NoArg (\flags -> return flags{blackWhite = True}))
               "do not use colours in generated graphics"
            , Public)
          , (Option []        ["doubleEdges"]
               (NoArg (\flags -> return flags{doubleEdges = not (doubleEdges flags)}))
               "generate graphics in an alternate way. (you may experiment with this option to see the differences for yourself)"
            , Public)
          , (Option []        ["predLogic"]
               (NoArg (\flags -> return flags{showPredExpr = True}))
               "show logical expressions in the form of predicate logic."
            , Public)
          , (Option []        ["noDiagnosis"]
               (NoArg (\flags -> return flags{noDiagnosis = True}))
               "omit the diagnosis chapter from the functional specification document."
            , Public)
          , (Option []        ["diagnosis"]
               (NoArg (\flags -> return flags{diagnosisOnly = True}))
               "diagnose your Ampersand script (generates a .pdf file)."
            , Public)
          , (Option []        ["legalrefs"]
               (NoArg (\flags -> return flags{genLegalRefs = True}))
               "generate a table of legal references in Natural Language chapter."
            , Public)
          , (Option []        ["uml"]
               (NoArg (\flags -> return flags{genUML = True}))
               "Generate a UML 2.0 data model."
            , Hidden)
          , (Option []        ["FPA"]
               (NoArg (\flags -> return flags{genFPAExcel = True}))
               "Generate a Excel workbook (.xls)."
            , Hidden)
          , (Option []        ["bericht"]
               (NoArg (\flags -> return flags{genBericht = True}))
               "Generate definitions for 'berichten' (specific to INDOORS project)."
            , Hidden)
          , (Option []        ["language"]
               (ReqArg (\l flags-> return flags{language = case map toUpper l of
                                                       "NL"  -> Just Dutch
                                                       "UK"  -> Just English
                                                       "US"  -> Just English
                                                       "EN"  -> Just English
                                                       _     -> Nothing}
                       ) "LANG")
               "Pick 'NL' for Dutch or 'EN' for English, as the language to be used in your output. Without this option, output is written in the language of your context."
            , Public)
          , (Option []        ["test"]
               (NoArg (\flags -> return flags{test = True}))
               "Used for test purposes only."
            , Hidden)
          , (Option []        ["rap"]
               (NoArg (\flags -> return flags{includeRap = True}))
               "Include RAP into the generated artifacts (experimental)"
            , Hidden)
          , (Option []        ["meta"]
               (NoArg (\flags -> return flags{genMeat = True}))
               "Generate meta-population in an .adl file (experimental)"
            , Hidden)
          , (Option []        ["pango"]
               (ReqArg (\nm flags -> return flags{pangoFont = nm}
                       ) "FONTNAME")
               "specify font name for Pango in graphics."
            , Hidden)
          , (Option []   ["no-static-files"]
               (NoArg  (\flags -> return flags{genStaticFiles = False}))
               "Do not generate static files into the prototype directory"
            , Public)
          , (Option []        ["sqlHost"]
               (ReqArg (\nm flags -> return flags{sqlHost = nm}
                       ) "HOSTNAME")
               "specify database host name."
            , Hidden)
          , (Option []        ["sqlLogin"]
               (ReqArg (\nm flags -> return flags{sqlLogin = nm}
                       ) "NAME")
               "specify database login name."
            , Hidden)
          , (Option []        ["sqlPwd"]
               (ReqArg (\nm flags -> return flags{sqlPwd = nm}
                       ) "STR")
               "specify database password."
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
verbose flags x
   | verboseP flags = putStr x
   | otherwise      = return ()
   
verboseLn :: Options -> String -> IO ()
verboseLn flags x
   | verboseP flags = -- each line is handled separately, so the buffer will be flushed in time. (see ticket #179)
                      mapM_ putStrLn (lines x)
   | otherwise      = return ()
helpNVersionTexts :: String -> Options -> [String]
helpNVersionTexts vs flags          = ["Executable: "++show (dirExec flags)++"\n"    | test flags       ]++
                                      [preVersion flags++vs++postVersion flags++"\n" | showVersion flags]++
                                      [usageInfo' flags                              | showHelp    flags]


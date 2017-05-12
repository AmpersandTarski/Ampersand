{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.Misc.Options
        ( Options(..)
        , FSpecFormat(..)
        , getOptions
        , verboseLn
        , verbose
        , showFormat
        , helpNVersionTexts
        , writeConfigFile
        )
where
import System.Environment    (getArgs, getProgName,getEnvironment,getExecutablePath )
import System.Console.GetOpt
import System.FilePath
import System.Directory
import Data.Time.Clock
import Data.Time.LocalTime
import Control.Monad
import Data.Maybe
import Ampersand.Basics
import Prelude hiding (writeFile,readFile,getContents,putStr,putStrLn)
import Data.List as DL
import Data.Char
import "yaml-config" Data.Yaml.Config as YC 

-- | This data constructor is able to hold all kind of information that is useful to
--   express what the user would like Ampersand to do.
data Options = Options { environment :: EnvironmentOptions 
                       , showVersion :: Bool
                       , preVersion :: String
                       , postVersion :: String  --built in to aid DOS scripting... 8-(( Bummer.
                       , showHelp :: Bool
                       , verboseP :: Bool
                       , development :: Bool
                       , validateSQL :: Bool
                       , genSampleConfigFile :: Bool -- generate a sample configuration file (yaml)
                       , genPrototype :: Bool
                       , dirPrototype :: String  -- the directory to generate the prototype in.
                       , dirInclude :: String -- the directory that is included in the generated prototype
                       , allInterfaces :: Bool
                       , dbName :: String
                       , namespace :: String
                       , testRule :: Maybe String
               --        , customCssFile :: Maybe FilePath
                       , genFSpec :: Bool   -- if True, generate a functional design
                       , diag :: Bool   -- if True, generate a diagnosis only
                       , fspecFormat :: FSpecFormat -- the format of the generated (pandoc) document(s)
                       , genEcaDoc :: Bool   -- if True, generate ECA rules in the functional design
                       , proofs :: Bool
                       , haskell :: Bool   -- if True, generate the F-structure as a Haskell source file
                       , sqlDump :: Bool   -- if True, generate a dump of SQL statements (for debugging)
                       , dirOutput :: String -- the directory to generate the output in.
                       , outputfile :: String -- the file to generate the output in.
                       , crowfoot :: Bool   -- if True, generate conceptual models and data models in crowfoot notation
                       , blackWhite :: Bool   -- only use black/white in graphics
                       , doubleEdges :: Bool   -- Graphics are generated with hinge nodes on edges.
                       , noDiagnosis :: Bool   -- omit the diagnosis chapter from the functional design document
                       , diagnosisOnly :: Bool   -- give a diagnosis only (by omitting the rest of the functional design document)
                       , genLegalRefs :: Bool   -- Generate a table of legal references in Natural Language chapter
                       , genUML :: Bool   -- Generate a UML 2.0 data model
                       , genFPAChap :: Bool   -- Generate Function Point Analysis chapter
                       , genFPAExcel :: Bool   -- Generate an Excel workbook containing Function Point Analysis
                       , genPOPExcel :: Bool   -- Generate an .xmlx file containing the populations 
                       , genStaticFiles :: Bool-- Generate the static files into the prototype
                       , genBericht :: Bool
                       , language :: Maybe Lang  -- The language in which the user wants the documentation to be printed.
                       , dirExec :: String --the base for relative paths to input files
                       , progrName :: String --The name of the adl executable
                       , fileName :: FilePath --the file with the Ampersand context
                       , baseName :: String
                       , genTime :: LocalTime
                       , export2adl :: Bool
                       , test :: Bool
                       , genMetaTables :: Bool -- When set, generate the meta-tables of AST into the prototype
                       , genMetaFile :: Bool  -- When set, output the meta-population as a file
                       , addSemanticMetaModel :: Bool -- When set, the user can use all relations defined in Formal Ampersand, without the need to specify them explicitly
                       , genRapPopulationOnly :: Bool -- This switch is to tell Ampersand that the model is being used in RAP3 as student's model
                       , sqlHost ::  String  -- do database queries to the specified host
                       , sqlLogin :: String  -- pass login name to the database server
                       , sqlPwd :: String  -- pass password on to the database server
                       , sqlBinTables :: Bool -- generate binary tables (no 'brede tabellen')
                       , defaultCrud :: (Bool,Bool,Bool,Bool) -- Default values for CRUD functionality in interfaces
                       , oldNormalizer :: Bool
                       , trimXLSXCells :: Bool -- Should leading and trailing spaces of text values in .XLSX files be ignored? 
                       } deriving Show
data EnvironmentOptions = EnvironmentOptions
      { envArgs               :: [String]
      , envArgsCommandLine    :: [String]
      , envArgsFromConfigFile :: [String]
      , envProgName           :: String
      , envExePath            :: FilePath
      , envLocalTime          :: LocalTime
      , envDirOutput          :: Maybe FilePath
      , envDirPrototype       :: Maybe FilePath
      , envDbName             :: Maybe String
      , envPreVersion         :: Maybe String
      , envPostVersion        :: Maybe String  
      } deriving Show

dirPrototypeVarName :: String
dirPrototypeVarName = "CCdirPrototype"
dirOutputVarName :: String
dirOutputVarName = "CCdirOutput"
dbNameVarName :: String
dbNameVarName = "CCdbName"

getEnvironmentOptions :: IO EnvironmentOptions
getEnvironmentOptions = 
   do args     <- getArgs
      let (configSwitches,otherArgs) = partition isConfigSwitch args
      argsFromConfigFile <- readConfigFileArgs (mConfigFile configSwitches)
      progName <- getProgName
      execPth  <- getExecutablePath -- on some operating systems, `getExecutablePath` gives a relative path. That may lead to a runtime error.
      exePath  <- makeAbsolute execPth -- see https://github.com/haskell/cabal/issues/3512 for details
      localTime <-  do utcTime <- getCurrentTime
                       timeZone <- getCurrentTimeZone
                       return (utcToLocalTime timeZone utcTime)
      env <- getEnvironment
      return EnvironmentOptions
        { envArgs               = args
        , envArgsCommandLine    = otherArgs
        , envArgsFromConfigFile = argsFromConfigFile
        , envProgName           = progName
        , envExePath            = exePath
        , envLocalTime          = localTime
        , envDirOutput          = DL.lookup dirOutputVarName    env
        , envDirPrototype       = DL.lookup dirPrototypeVarName env  
        , envDbName             = DL.lookup dbNameVarName       env
        , envPreVersion         = DL.lookup "CCPreVersion"      env
        , envPostVersion        = DL.lookup "CCPostVersion"     env
        }
  where
    isConfigSwitch :: String -> Bool
    isConfigSwitch = isPrefixOf configSwitch
    configSwitch :: String
    configSwitch = "--config"    
    mConfigFile :: [String] -> Maybe FilePath
    mConfigFile switches = case mapMaybe (stripPrefix configSwitch) switches of
                    []  -> Nothing
                    ['=':x] -> Just x
                    [err]   -> exitWith . WrongArgumentsGiven $ ["No file specified in `"++configSwitch++err++"`"]
                    xs  -> exitWith . WrongArgumentsGiven $ "Too many config files specified: " : map ("   " ++) xs
    readConfigFileArgs :: Maybe FilePath -> IO [String]
    readConfigFileArgs mFp
     = case mFp of
         Nothing    -> -- If no config file is given, there might exist one with the same name
                       -- as the script name. If that is the case, we use that config file. 
                       do args     <- getArgs
                          let (_, xs, _) = getOpt Permute (map fst options) args
                          case xs of 
                            [script] -> do let yaml = script -<.> "yaml"
                                           exist <- doesFileExist yaml
                                           if exist then readConfigFile yaml else return []
                            _  -> return []
         Just fName -> readConfigFile fName
        where 
           readConfigFile yaml = do
              putStrLn $ "Reading config file: "++yaml
              config <- load yaml
              case keys config \\ ["switches"] of
                []  -> do let switches :: [String] = YC.lookupDefault "switches" [] config
                          case filter (not . isValidSwitch) switches of
                            []  -> return $ map ("--"++) switches
                            [x] -> configFail $ "Invalid switch: "++x
                            xs  -> configFail $ "Invalid switches: "++intercalate ", " xs
                           
                [x] -> configFail $ "Unknown key: "++show x 
                xs  -> configFail $ "Unknown keys: "++intercalate ", " (map show xs)
             where
              configFail :: String -> a
              configFail msg
                  = exitWith . WrongArgumentsGiven $
                     [ "Error in "++yaml++":"
                     , "  "++msg
                     ] 


getOptions :: IO Options
getOptions = 
   do envOpts <- getEnvironmentOptions
      return (getOptions' envOpts)

getOptions' :: EnvironmentOptions -> Options
getOptions' envOpts =  
   case errors of
     []  | development opts && validateSQL opts 
                     -> exitWith . WrongArgumentsGiven $ ["--dev and --validate must not be used at the same time."] --(Reason: see ticket #378))
         | otherwise -> opts
     _  -> exitWith . WrongArgumentsGiven $ errors ++ [usage]
         
 where
    opts :: Options
    -- Here we thread startOptions through all supplied option actions
    opts = foldl f startOptions actions
      where f a b = b a
    (actions, fNames, errors) = getOpt Permute (map fst options) $ envArgsFromConfigFile envOpts ++ envArgsCommandLine envOpts 
    fName = case fNames of
             []   -> exitWith . WrongArgumentsGiven $ "Please supply the name of an ampersand file" : [usage]
             [n]  -> n
             _    -> exitWith . WrongArgumentsGiven $ ("Too many files: "++ intercalate ", " fNames) : [usage]
    usage = "Type '"++envProgName envOpts++" --help' for usage info."
    startOptions :: Options
    startOptions =
               Options {environment      = envOpts
                      , genTime          = envLocalTime envOpts
                      , dirOutput        = fromMaybe "." $ envDirOutput envOpts
                      , outputfile       = fatal 83 "No monadic options available."
                      , dirPrototype     = fromMaybe "." (envDirPrototype envOpts) </> takeBaseName fName <.> ".proto"
                      , dirInclude       = "include"
                      , dbName           = fmap toLower . fromMaybe ("ampersand_"++takeBaseName fName) $ envDbName envOpts
                      , dirExec          = takeDirectory (envExePath envOpts)
                      , preVersion       = fromMaybe "" $ envPreVersion envOpts
                      , postVersion      = fromMaybe "" $ envPostVersion envOpts
                      , showVersion      = False
                      , showHelp         = False
                      , verboseP         = False
                      , development      = False
                      , validateSQL      = False
                      , genSampleConfigFile = False
                      , genPrototype     = False
                      , allInterfaces    = False
                      , namespace        = ""
                      , testRule         = Nothing
              --        , customCssFile    = Nothing
                      , genFSpec         = False
                      , diag             = False
                      , fspecFormat      = fatal 105 $ "Unknown fspec format. Currently supported formats are "++allFSpecFormats++"."
                      , genEcaDoc        = False
                      , proofs           = False
                      , haskell          = False
                      , sqlDump          = False
                      , crowfoot         = False
                      , blackWhite       = False
                      , doubleEdges      = True
                      , noDiagnosis      = False
                      , diagnosisOnly    = False
                      , genLegalRefs     = False
                      , genUML           = False
                      , genFPAChap       = False
                      , genFPAExcel      = False
                      , genStaticFiles   = True
                      , genPOPExcel      = False
                      , genBericht       = False
                      , language         = Nothing
                      , progrName        = envProgName envOpts
                      , fileName         = if hasExtension fName
                                           then fName
                                           else addExtension fName "adl"
                      , baseName         = takeBaseName fName
                      , export2adl       = False
                      , test             = False
                      , genMetaTables    = False
                      , genMetaFile      = False
                      , addSemanticMetaModel = False
                      , genRapPopulationOnly = False
                      , sqlHost          = "localhost"
                      , sqlLogin         = "ampersand"
                      , sqlPwd           = "ampersand"
                      , sqlBinTables       = False
                      , defaultCrud      = (True,True,True,True) 
                      , oldNormalizer    = True -- The new normalizer still has a few bugs, so until it is fixed we use the old one as the default
                      , trimXLSXCells    = True
                      }
writeConfigFile :: IO ()
writeConfigFile = do
    writeFile sampleConfigFileName (unlines sampleConfigFile)
    putStrLn (sampleConfigFileName++" written.")
    
sampleConfigFileName :: FilePath
sampleConfigFileName = "sampleconfig.yaml"
sampleConfigFile :: [String]
sampleConfigFile =
  [ " # Sample config file for Ampersand"
  , " # This file contains a list of all command line options that can be set using a config file"
  , " # It can be used by specifying:  ampersand.exe --config=myConfig.yaml myModel.adl"
  , " # remove the comment character (`#`) in front of the switches you want to activate."
  , " # Note: Make sure that the minus (`-`) characters are in exactly the same column. Yaml format is picky about that."
  , ""
  , "switches:"
  ]++concatMap (yamlItem . fst) options
  where
    yamlItem :: OptionDef -> [String]
    yamlItem (Option _ label kind info ) 
      = if head label `elem` validSwitches
        then
         [ "  ### "++info++":"
         , "  # - "++head label++case kind of
                                   NoArg _ -> "" 
                                   ReqArg _ str -> "="++str
                                   OptArg _ str -> "[="++str++"]"
         , ""
         ]
        else []   
isValidSwitch :: String -> Bool
isValidSwitch str = 
  case mapMaybe (matches . fst) options of
    [Option _ _ kind _]
        -> case (dropWhile (/= '=') str, kind) of
             ([]    , NoArg  _  ) -> True 
             ([]    , OptArg _ _) -> True
             ('=':_ , OptArg _ _) -> True
             ('=':_ , ReqArg _ _) -> True
             _  -> False
    _  -> False
  where 
    matches :: OptionDef -> Maybe OptionDef
    matches x@(Option _ labels _ _) 
     = if takeWhile (/= '=') str `elem` labels \\ ["version","help","config","sampleConfigFile"]
       then Just x
       else Nothing
validSwitches :: [String]
validSwitches =  filter canBeYamlSwitch [head label | Option _ label _ _ <- map fst options]
canBeYamlSwitch :: String -> Bool
canBeYamlSwitch str =
   takeWhile (/= '=') str `notElem` ["version","help","config","sampleConfigFile"]  
data DisplayMode = Public | Hidden deriving Eq

data FSpecFormat = 
         FPandoc
       | Fasciidoc
       | Fcontext
       | Fdocbook
       | Fdocx 
       | Fhtml
       | FLatex
       | Fman
       | Fmarkdown
       | Fmediawiki
       | Fopendocument
       | Forg
       | Fplain
       | Frst
       | Frtf
       | Ftexinfo
       | Ftextile
       deriving (Show, Eq)
allFSpecFormats :: String
allFSpecFormats = "["++intercalate ", " 
    ((sort . map showFormat) 
        [FPandoc, Fasciidoc, Fcontext, Fdocbook, Fdocx, Fhtml, 
                FLatex, Fman, Fmarkdown, Fmediawiki, Fopendocument
                , Forg, Fplain, Frst, Frtf, Ftexinfo, Ftextile]) ++"]"
showFormat :: FSpecFormat -> String
showFormat fmt = case show fmt of
                  _:h:t -> toUpper h : map toLower t
                  x     -> x 

type OptionDef = OptDescr (Options -> Options)
options :: [(OptionDef, DisplayMode) ]
options = [ (Option ['v']   ["version"]
               (NoArg (\opts -> opts{showVersion = True}))
               "show version and exit."
            , Public)
          , (Option ['h','?'] ["help"]
               (NoArg (\opts -> opts{showHelp = True}))
               "get (this) usage information."
            , Public)
          , (Option ['V']   ["verbose"]
               (NoArg (\opts -> opts{verboseP = True}))
               "verbose error message format."
            , Public)
          , (Option []   ["sampleConfigFile"]
               (NoArg (\opts -> opts{genSampleConfigFile = True}))
               ("write a sample configuration file ("++sampleConfigFileName++")")
            , Public)
          , (Option []      ["config"]
               (ReqArg (\nm _ -> fatal 194 $ "config file ("++nm++")should not be treated as a regular option."
                       ) "config.yaml")
               "config file (*.yaml)"
            , Public)
          , (Option []      ["dev"]
               (NoArg (\opts -> opts{development = True}))
               "Report and generate extra development information (for Martijn)"
            , Hidden)
          , (Option []      ["validate"]
               (NoArg (\opts -> opts{validateSQL = True}))
               "Compare results of rule evaluation in Haskell and SQL (requires command line php with MySQL support)"
            , Hidden)
          , (Option ['p']     ["proto"]
               (OptArg (\nm opts -> opts {dirPrototype = fromMaybe (dirPrototype opts) nm
                                                  ,genPrototype = True}
                       ) "DIRECTORY")
               ("generate a functional prototype (This overrules environment variable "++ dirPrototypeVarName ++ ").")
            , Public)
          , (Option []     ["include"]
               (ReqArg (\nm opts -> opts {dirInclude = nm
                                                ,genPrototype = True}
                       ) "DIRECTORY")
               "include a directory into the generated prototype, instead of the default."
            , Public)
          , (Option ['d']  ["dbName"]
               (ReqArg (\nm opts -> opts{dbName = if nm == ""
                                                         then dbName opts
                                                         else map toLower nm}
                       ) "NAME")
               ("database name (This overrules environment variable "++ dbNameVarName ++ ", defaults to filename)")
            , Public)
          , (Option []  ["sqlHost"]
               (ReqArg (\nm opts -> opts{sqlHost = if nm == ""
                                                          then sqlHost opts
                                                          else nm}
                       ) "HOSTNAME")
               "set SQL host name (Defaults to `localhost`)"
            , Public)
          , (Option []  ["sqlLogin"]
               (ReqArg (\nm opts -> opts{sqlLogin = if nm == ""
                                                          then sqlLogin opts
                                                          else nm}
                       ) "USER")
               "set SQL user name (Defaults to `ampersand`)"
            , Public)
          , (Option []  ["sqlPwd"]
               (ReqArg (\nm opts -> opts{sqlPwd = nm}
                       ) "PASSWORD")
               "set SQL password (Defaults to `ampersand`)"
            , Public)
          , (Option []        ["sql-bin-tables"]
               (NoArg (\opts -> opts{sqlBinTables = True}))
               "generate binary tables only in SQL database."
            , Hidden)
          , (Option ['x']     ["interfaces"]
               (NoArg (\opts -> opts{allInterfaces  = True}))
               "generate interfaces."
            , Public)
          , (Option ['e']     ["export"]
               (OptArg (\mbnm opts -> opts{export2adl = True
                                                   ,outputfile = fromMaybe "Export.adl" mbnm}) "file")
               "export as plain Ampersand script."
            , Public)
          , (Option ['o']     ["outputDir"]
               (ReqArg (\nm opts -> opts{dirOutput = nm}
                       ) "DIR")
               ("output directory (This overrules environment variable "++ dirOutputVarName ++ ").")
            , Public)
          , (Option []      ["namespace"]
               (ReqArg (\nm opts -> opts{namespace = nm}
                       ) "NAMESPACE")
               "prefix database identifiers with this namespace, in order to isolate namspaces."
            , Public)
          , (Option ['f']   ["fspec"]
               (ReqArg (\w opts -> opts
                                { genFSpec=True
                                , fspecFormat= case map toUpper w of
                                    ('A': _ )             -> Fasciidoc
                                    ('C': _ )             -> Fcontext
                                    ('D':'O':'C':'B': _ ) -> Fdocbook
                                    ('D':'O':'C':'X': _ ) -> Fdocx
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
               ("generate a functional design document in specified format (FORMAT="++allFSpecFormats++").")
            , Public)
          , (Option []        ["testRule"]
               (ReqArg (\ruleName opts -> opts{ testRule = Just ruleName }
                       ) "RULE")
               "Show contents and violations of specified rule."
            , Hidden)
     --     , (Option []        ["css"]
     --          (ReqArg (\pth opts -> opts{ customCssFile = Just pth }) "file")
     --          "Custom.css file to customize the style of the prototype."
     --       , Public)
          , (Option []        ["ECA"]
               (NoArg (\opts -> opts{genEcaDoc = True}))
               "generate documentation with ECA rules."
            , Hidden)
          , (Option []        ["proofs"]
               (NoArg (\opts -> opts{proofs = True}))
               "generate derivations."
            , Hidden)
          , (Option []        ["haskell"]
               (NoArg (\opts -> opts{haskell = True}))
               "generate internal data structure, written in Haskell (for debugging)."
            , Hidden)
          , (Option []        ["sqldump"]
               (NoArg (\opts -> opts{sqlDump = True}))
               "generate a dump of SQL queries (for debugging)."
            , Public)
          , (Option []        ["crowfoot"]
               (NoArg (\opts -> opts{crowfoot = True}))
               "generate crowfoot notation in graphics."
            , Public)
          , (Option []        ["blackWhite"]
               (NoArg (\opts -> opts{blackWhite = True}))
               "do not use colours in generated graphics"
            , Public)
          , (Option []        ["altGraphics"]
               (NoArg (\opts -> opts{doubleEdges = not (doubleEdges opts)}))
               "generate graphics in an alternate way. (you may experiment with this option to see the differences for yourself)"
            , Public)
          , (Option []        ["noDiagnosis"]
               (NoArg (\opts -> opts{noDiagnosis = True}))
               "omit the diagnosis chapter from the functional design document."
            , Public)
          , (Option []        ["diagnosis"]
               (NoArg (\opts -> opts{diagnosisOnly = True}))
               "diagnose your Ampersand script (generates a .pdf file)."
            , Public)
          , (Option []        ["reference-table"]
               (NoArg (\opts -> opts{genLegalRefs = True}))
               "generate a table of references in the Natural Language chapter, for instance for legal traceability."
            , Public)
          , (Option []        ["uml"]
               (NoArg (\opts -> opts{genUML = True}))
               "Generate a UML 2.0 data model."
            , Hidden)
          , (Option []        ["fpa"]
               (NoArg (\opts -> opts{genFPAChap = True}))
               "Generate Function Point Analysis chapter."
            , Hidden)
          , (Option []        ["fpa-excel"]
               (NoArg (\opts -> opts{genFPAExcel = True}))
               "Generate an Excel workbook (FPA_<filename>.xml)."
            , Hidden)
          , (Option []        ["pop-xlsx"]
               (NoArg (\opts -> opts{genPOPExcel = True}))
               "Generate an .xmlx file containing the populations of your script."
            , Public) 
          , (Option []        ["ebc"]
               (NoArg (\opts -> opts{genBericht = True}))
               "Generate specifications of interfaces in EBV-format (http://www.justid.nl/ebv/)."
            , Hidden)
          , (Option []        ["language"]
               (ReqArg (\l opts-> opts{language = case map toUpper l of
                                                       "NL"  -> Just Dutch
                                                       "UK"  -> Just English
                                                       "US"  -> Just English
                                                       "EN"  -> Just English
                                                       _     -> Nothing}
                       ) "LANG")
               "Pick 'NL' for Dutch or 'EN' for English, as the language to be used in your output. Without this option, output is written in the language of your context."
            , Public)
          , (Option []        ["test"]
               (NoArg (\opts -> opts{test = True}))
               "Used for test purposes only."
            , Hidden)
          , (Option []        ["meta-tables"]
               (NoArg (\opts -> opts{genMetaTables = True}))
               "When set, generate the meta-tables of ampersand into the prototype"
            , Hidden)
          , (Option []        ["meta-file"]
               (NoArg (\opts -> opts{genMetaFile = True}))
               "Generate the meta-population in AST format and output it to an .adl file"
            , Hidden)
          , (Option []        ["add-semantic-metamodel"]
               (NoArg (\opts -> opts{addSemanticMetaModel = True}))
               "Add all relations, concepts and generalisation relations of formal ampersand into your script"
            , Hidden)
          , (Option []        ["gen-as-rap-model"]
               (NoArg (\opts -> opts{genRapPopulationOnly = True}))
               "Generate populations for use in RAP3."
            , Hidden)
          , (Option []        ["no-static-files"]
               (NoArg  (\opts -> opts{genStaticFiles = False}))
               "Do not generate static files into the prototype directory"
            , Public)
          , (Option []        ["crud-defaults"]
               (ReqArg (\crudString opts -> let c = 'c' `notElem` crudString
                                                r = 'r' `notElem` crudString
                                                u = 'u' `notElem` crudString
                                                d = 'd' `notElem` crudString
                                            in opts{defaultCrud = (c,r,u,d)}
                       ) "CRUD"
               )
               "Temporary switch to learn about the semantics of crud in interface expressions."
            , Hidden)
          , (Option []        ["oldNormalizer"]
               (NoArg (\opts -> opts{oldNormalizer = True}))
               "Use the old normalizer at your own risk."
            , Hidden)
          , (Option []        ["newNormalizer"]
               (NoArg (\opts -> opts{oldNormalizer = False}))
               "Use the new normalizer at your own risk." -- :-)
            , Hidden)
          , (Option []        ["do-not-trim-cellvalues"]
               (NoArg (\opts -> opts{trimXLSXCells = False}))
               "Do not ignore leading and trailing spaces in .xlsx files that are INCLUDED in the script." -- :-)
            , Public)
          ]

usageInfo' :: Options -> String
-- When the user asks --help, then the public options are listed. However, if also --verbose is requested, the hidden ones are listed too.
usageInfo' opts = 
  infoHeader (progrName opts) ++"\n"++
    (concat . sort . map publishOption) [od | (od,x) <- options, verboseP opts || x == Public] 

infoHeader :: String -> String
infoHeader progName = "\nUsage info:\n " ++ progName ++ " options file ...\n\nList of options:"


publishOption:: OptDescr a -> String
publishOption (Option shorts longs args expl) 
  = unlines (
    ( "  "++intercalate ", " ["--"++l | l <-longs] 
      ++case args of
         NoArg _      -> "" 
         ReqArg _ str -> "="++str
         OptArg _ str -> "[="++str++"]"
      ++case intercalate ", " [ "-"++[c] | c <- shorts] of
          []  -> []
          xs  -> " ("++xs++")"
    ): 
     map (replicate 10 ' '++) (lines (limit 65 expl)))
  where
   limit :: Int -> String -> String
   limit i = intercalate "\n" . map (singleLine i . words) . lines
   singleLine :: Int -> [String] -> String 
   singleLine i wrds = 
     case fillUpto i "" wrds of
       (str, []) -> str
       (str, rest) -> str ++ "\n"++ singleLine i rest
   fillUpto :: Int -> String -> [String] -> (String, [String])
   fillUpto i "" (w:ws) = fillUpto i w ws
   fillUpto _ str [] = (str, [])
   fillUpto i str (w:ws) = let nstr = str++" "++w
                           in if length nstr > i 
                           then (str, w:ws)
                           else fillUpto i nstr ws 
     

verbose :: Options -> String -> IO ()
verbose opts x
   | verboseP opts = putStr x
   | otherwise     = return ()

verboseLn :: Options -> String -> IO ()
verboseLn opts x
   | verboseP opts = -- Since verbose is for debugging purposes in general, we want no buffering, because it is confusing while debugging.
                     do hSetBuffering stdout NoBuffering
                        mapM_ putStrLn (lines x)
   | otherwise     = return ()
helpNVersionTexts :: String -> Options -> [String]
helpNVersionTexts vs opts = ["Executable: "++show (dirExec opts)++"\n"   | test opts       ]++
                            [preVersion opts++vs++postVersion opts++"\n" | showVersion opts]++
                            [usageInfo' opts                             | showHelp    opts]


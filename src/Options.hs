{-# OPTIONS_GHC -Wall #-}
module Options (Options(..),getOptions,usageInfo',verboseLn,verbose)
where
--import List                  (isSuffixOf)
import System                (getArgs, getProgName)
import System.Environment    (getEnvironment)
import Languages (Lang(..))
import Char (toUpper)
import System.Console.GetOpt
import System.FilePath
import System.Directory
import Time          
--import System.FilePath.Posix
-- | This data constructor is able to hold all kind of information that is useful to 
--   express what the user would like ADL to do. 
data Options = Options { contextName   :: Maybe String
                       , showVersion   :: Bool
                       , showHelp      :: Bool
        --             , verbose       :: String -> IO ()  -- vervangt putStr -- Voor de fijnproevers: Een functie kan hier ook! ;-)) 
		--			   , verboseLn     :: String -> IO ()  -- vervangt putStrLn
					   , verboseP      :: Bool
					   , genPrototype  :: Bool 
					   , uncheckedDirPrototype  :: Maybe String
					   , dirPrototype  :: String
					   , allServices   :: Bool
					   , dbName        :: Maybe String
					   , genAtlas      :: Bool
					   , uncheckedDirAtlas      :: Maybe String
					   , dirAtlas      :: String
					   , genXML        :: Bool
					   , fspecLaTeX    :: Bool
					   , fspecHtml     :: Bool
					   , fspecWord     :: Bool
					   , fspecPandoc   :: Bool
					   , proofs        :: Bool
					   , haskell       :: Bool
					   , uncheckedDirOutput     :: Maybe String
					   , dirOutput     :: String     
					   , beeper        :: Bool
					   , crowfoot      :: Bool
					   , language      :: Lang
                       , progrName     :: String
                       , adlFileName   :: String
                       , baseName      :: String
                       , logName       :: String
                       , genTime       :: ClockTime
                       , uncheckedLogName :: Maybe String
                       , services      :: Bool
                       , skipTypechecker :: Bool  -- tijdelijke optie, totdat typechecker werkt.... 
                       } deriving Show
 
getOptions :: IO Options
getOptions = 
   do args     <- getArgs
      progName <- getProgName
      env      <- getEnvironment
      genTime'  <- getClockTime
      flags    <- case getOpt Permute (each options) args of
                      (o,[n],[])    -> return (foldl (flip id) (defaultOptions genTime' env n progName) o )
                      (_,[],[] )    -> ioError (userError ("no file to parse" ++usageInfo' progName))
                      (_,x:xs,[])   -> ioError (userError ("too many files: "++ show [x:xs] ++usageInfo' progName))
                      (_,_,errs)    -> ioError (userError (concat errs ++ usageInfo' progName))
      flags'   <- checkOptions flags
      return flags'

checkOptions :: Options -> IO Options
checkOptions flags = 
        do flags0  <- case uncheckedLogName flags of
                          Nothing -> return flags { logName = "ADL.log"}
                          Just s  -> return flags { logName = s } 
           verboseLn flags0 ("Checking output directories...")
           currDir <- getCurrentDirectory
           flags1  <- case uncheckedDirOutput flags0 of
                          Nothing -> return flags0 { dirOutput = currDir }
                          Just s  -> do exists <- doesDirectoryExist s
                                        if exists
                                          then return flags0 { dirOutput =  s}
                                          else ioError (userError ("Directory does not exist: "++s))  
           flags2 <- if genPrototype flags1
                        then case uncheckedDirPrototype flags1 of
                             Nothing -> return flags1 { dirPrototype = dirOutput flags1 }
                             Just s  -> do exists <- doesDirectoryExist s
                                           if exists
                                             then return flags1 { dirPrototype = s }
                                             else ioError (userError ("Directory does not exist: "++s))
                        else return flags1  {- No need to check if no prototype will be generated. -}
           flags3 <- if genAtlas flags2
                        then case uncheckedDirAtlas flags2 of
                             Nothing -> return flags2 { dirAtlas = dirOutput flags2 }
                             Just s  -> do exists <- doesDirectoryExist s
                                           if exists
                                             then return flags2 { dirAtlas = s }
                                             else ioError (userError ("Directory does not exist: "++s))
                        else return flags2  {- No need to check if no atlas will be generated. -}
           return flags3                  
             
data DisplayMode = Public | Hidden
    
usageInfo' :: String -> String
usageInfo' progName = usageInfo (infoHeader progName) (publics options)
          
infoHeader :: String -> String
infoHeader progName = "\nUsage info:\n " ++ progName ++ " options file ...\n\nList of options:"

publics :: [(a, DisplayMode) ] -> [a]
publics opts = [o| (o,Public)<-opts]
each :: [(a, DisplayMode) ] -> [a]
each opts = [o|(o,_) <- opts]

options :: [(OptDescr (Options -> Options), DisplayMode) ]
options = [ ((Option ['C']     ["context"]          (OptArg contextOpt "name")  "use context with name"), Public)
          , ((Option ['v']     ["version"]          (NoArg versionOpt)          "show version and exit"), Public)
          , ((Option ['h','?'] ["help"]             (NoArg helpOpt)             "get (this) usage information"), Public)
          , ((Option []        ["verbose"]          (NoArg verboseOpt)          "verbose error message format"), Public)
          , ((Option ['p']     ["proto"]            (OptArg prototypeOpt "dir") ("generate a functional prototype with services defined in the ADL file (dir overrides "++
                                                                                   envdirPrototype ++ " )") ), Public)
          , ((Option ['x']     ["maxServices"]      (NoArg maxServicesOpt)      "if specified, generate all services in the prototype"), Public)
          , ((Option ['d']     ["dbName"]           (OptArg dbNameOpt "name")   ("use database with name (name overrides "++
                                                                                   envdbName ++ " )")), Public)
          , ((Option ['s']     ["services"]         (NoArg servicesOpt)         "generate service specifications in ADL format"), Public)
          , ((Option ['a']     ["atlas"]            (OptArg atlasOpt "dir" )    ("generate atlas (optional an output directory, defaults to current directory) (dir overrides "++
                                                                                   envdirAtlas ++ " )")), Public)
          , ((Option []        ["XML"]              (NoArg xmlOpt)              "generate XML output"), Public)
          , ((Option ['L']     ["fspecLaTeX"]       (NoArg fspecLaTeXOpt)       "generate a functional specification document in LaTeX format"), Public)
          , ((Option ['H']     ["fspecHtml"]        (NoArg fspecHtmlOpt)        "generate a functional specification document in Html format"), Public)
          , ((Option ['W']     ["fspecWord"]        (NoArg fspecWordOpt)       "generate a functional specification document for microsoft's Word"), Public)
          , ((Option ['P']     ["fspecPandoc"]      (NoArg fspecPandocOpt)     "generate a functional specification document in Pandoc format"), Public)
          , ((Option []        ["proofs"]           (NoArg proofsOpt)           "generate correctness proofs"), Public)
          , ((Option []        ["haskell"]          (NoArg haskellOpt)          "generate internal data structure, written in Haskell source code (for debugging)"), Public)
          , ((Option ['o']     ["outputDir"]        (ReqArg outputDirOpt "dir") ("default directory for generated files (dir overrides "++
                                                                                   envdirOutput ++ " )")        ), Public)
          , ((Option []        ["beeper"]           (NoArg beeperOpt)           "generate beeper instead of checker"), Public)
          , ((Option []        ["crowfoot"]         (NoArg crowfootOpt)         "generate crowfoot notation in graphics"), Public)
          , ((Option []        ["language"]         (ReqArg languageOpt "lang") "language to be used, ('NL' or 'UK')"), Public)
          , ((Option []        ["log"]              (ReqArg logOpt "name")       ("log to file with name (name overrides "++
                                                                                   envlogName  ++ " )")), Hidden)
          , ((Option []        ["skipTypechecker"]  (NoArg skipTCOpt)           "skip Typechecking" ), Public) -- Tijdelijk, zolang de TC nog onderhanden is. 
          ]

defaultOptions :: ClockTime -> [(String, String)] -> String -> String -> Options
defaultOptions clocktime env fName pName 
               = Options { contextName   = Nothing
                         , showVersion   = False
                         , showHelp      = False
                   --      , verbose       = donothing
			       --      , verboseLn     = donothing
			             , verboseP      = False
			             , genPrototype  = False
					     , uncheckedDirPrototype  = lookup envdirPrototype env
            		     , dirPrototype  = unchecked
            		     , allServices   = False
		                 , dbName        = lookup "CCdbName" env
		                 , genAtlas      = False   
            		     , uncheckedDirAtlas      = lookup envdirAtlas env
            		     , dirAtlas      = unchecked
            		     , genXML        = False 
	            	     , fspecLaTeX    = False
	            	     , fspecHtml     = False
	            	     , fspecWord     = False
	            	     , fspecPandoc   = False
	            	     , proofs        = False
	            	     , haskell       = False
	            	     , uncheckedDirOutput     = lookup envdirOutput env
                         , dirOutput     = unchecked
                         , beeper        = False
                         , crowfoot      = False
                         , language      = Dutch
                         , progrName     = pName
                         , adlFileName   = replaceExtension fName ".adl"
                         , baseName      = dropExtension  fName
                         , uncheckedLogName = lookup envlogName env
                         , logName       = "ADL.log"
                         , services      = False
                         , genTime       = clocktime
                         , skipTypechecker = False
                         }
                    
envdirPrototype :: String
envdirPrototype = "CCdirPrototype"
envdirAtlas :: String
envdirAtlas="CCdirAtlas"
envdirOutput :: String
envdirOutput="CCdirOutput"
envdbName :: String
envdbName="CCdbName"
envlogName :: String
envlogName="CClogName"

contextOpt :: Maybe String -> Options -> Options
contextOpt  nm  opts = opts{contextName  = nm}            
versionOpt :: Options -> Options
versionOpt      opts = opts{showVersion  = True}            
helpOpt :: Options -> Options
helpOpt         opts = opts{showHelp     = True}            
verboseOpt :: Options -> Options
verboseOpt      opts = opts{ -- verbose      = putStr
                            --,verboseLn    = putStrLn
                            verboseP     = True}            
prototypeOpt :: Maybe String -> Options -> Options
prototypeOpt nm opts = opts{uncheckedDirPrototype = nm
                           ,genPrototype = True}
maxServicesOpt :: Options -> Options
maxServicesOpt  opts = opts{genPrototype = True
                           ,allServices  = True}                            
dbNameOpt :: Maybe String -> Options -> Options
dbNameOpt nm    opts = opts{dbName       = nm}
atlasOpt :: Maybe String -> Options -> Options
atlasOpt nm     opts = opts{uncheckedDirAtlas     =  nm
                           ,genAtlas     = True}
xmlOpt :: Options -> Options
xmlOpt          opts = opts{genXML       = True}
fspecLaTeXOpt :: Options -> Options
fspecLaTeXOpt   opts = opts{fspecLaTeX   = True}
fspecHtmlOpt  :: Options -> Options
fspecHtmlOpt   opts = opts{fspecHtml     = True}
fspecWordOpt   :: Options -> Options
fspecWordOpt   opts = opts{fspecWord     = True}
fspecPandocOpt   :: Options -> Options
fspecPandocOpt   opts = opts{fspecPandoc = True}
proofsOpt :: Options -> Options
proofsOpt       opts = opts{proofs       = True}
servicesOpt :: Options -> Options
servicesOpt     opts = opts{services     = True}
haskellOpt :: Options -> Options
haskellOpt      opts = opts{haskell      = True}
outputDirOpt :: String -> Options -> Options
outputDirOpt nm opts = opts{uncheckedDirOutput    = Just nm}
beeperOpt :: Options -> Options
beeperOpt       opts = opts{beeper       = True}
crowfootOpt :: Options -> Options
crowfootOpt     opts = opts{crowfoot     = True}
languageOpt :: String -> Options -> Options
languageOpt l   opts = opts{language     = case map toUpper l of
                                             "NL"  -> Dutch
                                             "UK"  -> English
                                             _     -> Dutch}
logOpt :: String -> Options -> Options
logOpt nm       opts = opts{uncheckedLogName = Just nm}
skipTCOpt :: Options -> Options
skipTCOpt       opts = opts{skipTypechecker = True} 
verbose :: Options -> String -> IO ()
verbose flags x
    | verboseP flags = putStr x
    | otherwise      = donothing
   
verboseLn :: Options -> String -> IO ()
verboseLn flags x
    | verboseP flags = putStrLn x
    | otherwise      = donothing
    
donothing :: IO()
donothing = putStr ""   -- Ik weet zo gauw niet hoe dit anders moet....
unchecked :: String
unchecked = "."
                             

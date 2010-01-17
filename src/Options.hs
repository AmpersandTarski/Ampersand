{-# OPTIONS_GHC -Wall #-}
module Options (Options(..),getOptions,usageInfo',verboseLn,verbose,FspecFormat(..),allFspecFormats)
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
import Control.Monad
import Strings               (chain)

-- | This data constructor is able to hold all kind of information that is useful to 
--   express what the user would like ADL to do. 
data Options = Options { contextName   :: Maybe String
                       , showVersion   :: Bool
                       , showHelp      :: Bool
        --             , verbose       :: String -> IO ()  -- vervangt putStr -- Voor de fijnproevers: Een functie kan hier ook! ;-)) 
                --       , verboseLn     :: String -> IO ()  -- vervangt putStrLn
                       , verboseP      :: Bool
                       , genPrototype  :: Bool 
                       , uncheckedDirPrototype  :: Maybe String
                       , dirPrototype  :: String
                       , allServices   :: Bool
                       , dbName        :: String
                       , genAtlas      :: Bool
                       , uncheckedDirAtlas      :: Maybe String
                       , dirAtlas      :: String
                       , userAtlas     :: String
                       , genXML        :: Bool
                       , genFspec      :: Bool
                       , fspecFormat   :: FspecFormat
                       , graphics      :: Bool   -- if True, graphics will be generated in the functional spec.
                       , flgSwitchboard:: Bool   -- if True, switchboard graphics will be generated in the functional spec.
                       , proofs        :: Bool
                       , haskell       :: Bool
                       , uncheckedDirOutput     :: Maybe String
                       , dirOutput     :: String     
                       , beeper        :: Bool
                       , crowfoot      :: Bool
                       , dotStyle      :: Int
                       , language      :: Lang
                       , dirExec       :: String --the base for relative paths to input files
                       , texHdrFile    :: Maybe String -- the string represents a FilePath to customheader.tex
                       , progrName     :: String --The name of the adl executable
                       , adlFileName   :: String
                       , baseName      :: String
                       , logName       :: String
                       , genTime       :: ClockTime
                       , uncheckedLogName :: Maybe String
                       , services      :: Bool
                       , test          :: Bool
                       , sqlLogPwdDefd :: Bool
                       , sqlHost       :: String
                       , sqlLogin      :: String
                       , sqlPwd        :: String
                       , verbosephp    :: Bool
                       } deriving Show
    
data FspecFormat = FPandoc | FRtf | FOpenDocument | FLatex | FHtml  deriving (Show, Eq)
allFspecFormats :: String
allFspecFormats = "Pandoc, Rtf, OpenDocument, Latex, Html"
getOptions :: IO Options
getOptions = 
   do args     <- getArgs
      progName <- getProgName
      env      <- getEnvironment
      genTime' <- getClockTime
      flags    <- case getOpt Permute (each options) args of
                      (o,n,[])    -> return (foldl (flip id) (defaultOptions genTime' env n progName) o )
                      (_,_,errs)  -> ioError (userError (concat errs ++ usageInfo'' progName))
      checkMonadicOptions flags

checkMonadicOptions :: Options -> IO(Options)
checkMonadicOptions  = checkLogName 
                   >=> checkDirOutput
                   >=> checkExecOpts
                   >=> checkProtoOpts
                   >=> checkAtlasOpts 

checkLogName :: Options -> IO Options
checkLogName f   = return f{ logName   = case uncheckedLogName   f of 
                                          Nothing -> "ADL.log"
                                          Just s  -> s } 
                                                 
checkDirOutput :: Options -> IO Options
checkDirOutput f = do candidateDir 
                         <- canonicalizePath (case uncheckedDirOutput f of
                                                Nothing -> "."
                                                Just s  -> s )
                      verboseLn f ("Checking output directories...")
                      createDirectoryIfMissing True candidateDir 
                      return f{ dirOutput = candidateDir}

checkExecOpts :: Options -> IO Options
checkExecOpts f = do exePath <- findExecutable (progrName f)
                     return (case exePath of
                               Nothing -> f{dirExec=error ("!Fatal (module Options 126): Specify the path location of "++(progrName f)++" in your system PATH variable.")
                                           ,texHdrFile=error ("!Fatal (module Options 127): Specify the path location of "++(progrName f)++" in your system PATH variable.")
                                           }
                               Just s ->  f{dirExec=takeDirectory s}
                            ) 

checkProtoOpts :: Options -> IO Options
checkProtoOpts f = if genPrototype f
                    then do candidateDir
                              <- canonicalizePath (case uncheckedDirPrototype f of
                                                     Nothing -> "."
                                                     Just s  -> s )
                            createDirectoryIfMissing True candidateDir 
                            return f{ dirPrototype = candidateDir}
                    else return f {- No need for prototype options if no prototype will be generated. -}
                   
                   
checkAtlasOpts :: Options -> IO Options
checkAtlasOpts f = if genAtlas f
                    then do candidateDir
                              <- canonicalizePath (case uncheckedDirAtlas f of
                                                     Nothing -> "."
                                                     Just s  -> s )
                            createDirectoryIfMissing True candidateDir 
                            return f{ dirAtlas = candidateDir}
                    else return f {- No need for Atlas options if no prototype will be generated. -}


            
data DisplayMode = Public | Hidden 
    
usageInfo' :: Options -> String
-- When the user asks --help, then the public options are listed. However, if also --verbose is requested, the hidden ones are listed too.  
usageInfo' opts = usageInfo (infoHeader (progrName opts)) (if (verboseP opts) then each options else publics options)
          
usageInfo'' :: String -> String 
usageInfo'' progName = usageInfo (infoHeader progName) (publics options)

infoHeader :: String -> String
infoHeader progName = "\nUsage info:\n " ++ progName ++ " options file ...\n\nList of options:"

publics :: [(a, DisplayMode) ] -> [a]
publics opts = [o| (o,Public)<-opts]
each :: [(a, DisplayMode) ] -> [a]
each opts = [o|(o,_) <- opts]

options :: [(OptDescr (Options -> Options), DisplayMode) ]
options = map pp
          [ ((Option ['v']     ["version"]     (NoArg versionOpt)          "show version and exit."), Public)
          , ((Option ['h','?'] ["help"]        (NoArg helpOpt)             "get (this) usage information."), Public)
          , ((Option []        ["verbose"]     (NoArg verboseOpt)          "verbose error message format."), Public)
          , ((Option ['C']     ["context"]     (OptArg contextOpt "name")  "use context with name."), Public)

          , ((Option ['p']     ["proto"]       (OptArg prototypeOpt "dir") ("generate a functional prototype with services defined in the ADL file or generated services (specify -x) (dir overrides "++ envdirPrototype ++ ").") ), Public)
          , ((Option ['x']     ["maxServices"] (NoArg maxServicesOpt)      "if specified in combination with -p -f or -s then it uses generated services to generate a prototype, functional spec, or adl file respectively."), Public)
          , ((Option ['s']     ["services"]    (NoArg servicesOpt)         "generate service specifications in ADL format. Specify -x to generate services."), Public)

          , ((Option ['o']     ["outputDir"]   (ReqArg outputDirOpt "dir") ("default directory for generated files (dir overrides environment variable "++ envdirOutput ++ ").")), Public)
          , ((Option []        ["log"]         (ReqArg logOpt "name")      ("log to file with name (name overrides environment variable "++ envlogName  ++ ").")), Hidden)
          , ((Option ['d']     ["dbName"]      (ReqArg dbNameOpt "name")   ("use database with name (name overrides environment variable "++ envdbName ++ ").")), Public)

          , ((Option ['a']     ["atlas"]       (OptArg atlasOpt "dir")     ("generate atlas (optional an output directory, defaults to current directory) (dir overrides  environment variable"++ envdirAtlas ++ ").")), Public)
          , ((Option []        ["user"]       (ReqArg userOpt "user")     ("generate atlas content for this user.")), Public)
          , ((Option ['f']     ["fspec"]       (ReqArg fspecRenderOpt "format")  
                                                                           ("generate a functional specification document in specified format ("++allFspecFormats++").")), Public)
          , ((Option []        ["headerfile"]  (ReqArg languageOpt "filename") "use your own custom header file to prefix to the text before rendering."), Public)
          , ((Option []        ["noGraphics"]  (NoArg noGraphicsOpt)       "save compilation time by not generating any graphics."), Public)
          , ((Option []        ["Switchboard"] (NoArg switchboardOpt)      "generate switchboard graphics in services documentation for diagnostic purposes."), Hidden)
          , ((Option []        ["altGraphics"] (NoArg (altGraphicsOpt 2))  "generate alternative style pictures."), Public)
          , ((Option []        ["proofs"]      (NoArg proofsOpt)           "generate correctness proofs."), Public)
          , ((Option []        ["XML"]         (NoArg xmlOpt)              "generate internal data structure, written in XML (for debugging)."), Public)
          , ((Option []        ["haskell"]     (NoArg haskellOpt)          "generate internal data structure, written in Haskell source code (for debugging)."), Public)

          , ((Option []        ["beeper"]      (NoArg beeperOpt)           "generate beeper instead of checker."), Public)
          , ((Option []        ["crowfoot"]    (NoArg crowfootOpt)         "generate crowfoot notation in graphics."), Public)
          , ((Option []        ["language"]    (ReqArg languageOpt "lang") "language to be used, ('NL' or 'UK')."), Public)
          , ((Option []        ["test"]        (NoArg testOpt)             "Used for test purposes only."), Hidden)

          , ((Option []        ["sqlHost"]     (OptArg sqlHostOpt "hostname") "specify database host name."), Hidden)
          , ((Option []        ["sqlLogin"]    (OptArg sqlLoginOpt "login")   "specify database login name."), Hidden)
          , ((Option []        ["sqlPwd"]      (OptArg sqlPwdOpt "password")  "specify database password."), Hidden)
          , ((Option []        ["verbosePhp"]  (NoArg verbosephpOpt)       "generates loads of comments in PHP-code. Useful for debugging."), Public)
          ]
     where pp :: (OptDescr (Options -> Options), DisplayMode) -> (OptDescr (Options -> Options), DisplayMode)
           pp (Option a b' c d,e) = (Option a b' c d',e)
              where d' =  afkappen [] [] (words d) 40
                    afkappen :: [[String]] -> [String] -> [String] -> Int -> String
                    afkappen regels []    []   _ = chain ("\n") (map unwords regels)
                    afkappen regels totnu []   b = afkappen (regels++[totnu]) [] [] b
                    afkappen regels totnu (w:ws) b 
                          | length (unwords totnu) < b - length w = afkappen regels (totnu++[w]) ws b
                          | otherwise                             = afkappen (regels++[totnu]) [w] ws b     
           
defaultOptions :: ClockTime -> [(String, String)] -> [String] -> String -> Options
defaultOptions clocktime env fNames pName 
               = Options { contextName   = Nothing
                         , showVersion   = False
                         , showHelp      = False
                                     , verboseP      = False
                                     , verbosephp    = False
                                     , genPrototype  = False
                                             , uncheckedDirPrototype  = lookup envdirPrototype env
                                 , dirPrototype  = unchecked
                                 , allServices   = False
                                 , dbName        = case lookup envdbName env of
                                                           Just str -> str
                                                           Nothing  -> ""
                                 , genAtlas      = False   
                                 , uncheckedDirAtlas      = lookup envdirAtlas env
                                 , dirAtlas      = unchecked
                                 , userAtlas      = []
                                 , genXML        = False
                                 , genFspec      = False 
                                 , fspecFormat   = error ("Unknown fspec format. Currently supported formats are "++allFspecFormats++".")
                                 , graphics      = True
                                 , flgSwitchboard= False
                                 , proofs        = False
                                 , haskell       = False
                                 , uncheckedDirOutput     = lookup envdirOutput env
                         , dirExec       = unchecked
                         , texHdrFile    = Nothing
                         , dirOutput     = unchecked
                         , beeper        = False
                         , crowfoot      = False
                         , dotStyle      = 1
                         , language      = Dutch
                         , progrName     = pName
                         , adlFileName   = case fNames of
                                              []      -> error ("no file to parse" ++usageInfo'' pName)
                                              [fName] -> replaceExtension fName ".adl"
                                              x:xs    -> error ("too many files: "++ show (x:xs) ++usageInfo'' pName)
                         , baseName      = case fNames of
                                              []      -> error ("no file to parse" ++usageInfo'' pName)
                                              [fName] -> takeBaseName fName -- was: dropExtension fName, changed because the file_path/filename is no valid databasename
                                              x:xs    -> error ("too many files: "++ show (x:xs) ++usageInfo'' pName)
                         , uncheckedLogName = lookup envlogName env
                         , logName       = "ADL.log"
                         , services      = False
                         , genTime       = clocktime
                         , test          = False
                         , sqlLogPwdDefd = False
                         , sqlHost       = "localhost"
                         , sqlLogin      = "root"
                         , sqlPwd        = ""
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
verboseOpt      opts = opts{ verboseP     = True} 
verbosephpOpt  :: Options -> Options
verbosephpOpt opts = opts{verbosephp  = True}          
prototypeOpt :: Maybe String -> Options -> Options
prototypeOpt nm opts 
  = opts { dirPrototype = 
            case nm of
              Just s  -> s
              Nothing -> case uncheckedDirPrototype opts of
                           Just s -> s
                           Nothing -> "."
         ,genPrototype = True}
maxServicesOpt :: Options -> Options
maxServicesOpt  opts = opts{allServices  = True}                            
dbNameOpt :: String -> Options -> Options
dbNameOpt nm    opts = opts{dbName = if nm == "" 
                                       then baseName opts
                                       else nm
                           }                          
userOpt :: String -> Options -> Options
userOpt x opts = opts{userAtlas = x}
atlasOpt :: Maybe String -> Options -> Options
atlasOpt nm opts 
  = opts { dirAtlas = 
            case nm of
              Just s  -> s
              Nothing -> case uncheckedDirAtlas opts of
                           Just s -> s
                           Nothing -> "."
         ,genAtlas = True}
xmlOpt :: Options -> Options
xmlOpt          opts = opts{genXML       = True}
fspecRenderOpt :: String -> Options -> Options
fspecRenderOpt w opts = opts{ genFspec=True
                            , fspecFormat= case (map toUpper w) of
                                                 ('R': _ ) -> FRtf
                                                 ('L': _ ) -> FLatex
                                                 ('H': _ ) -> FHtml
                                                 ('P': _ ) -> FPandoc
                                                 ('O': _ ) -> FOpenDocument
                                                 _         -> fspecFormat opts
                                                
                            }
switchboardOpt :: Options -> Options
switchboardOpt opts = opts{flgSwitchboard = True}
noGraphicsOpt :: Options -> Options
noGraphicsOpt  opts = opts{graphics      = False}
altGraphicsOpt :: Int -> Options -> Options
altGraphicsOpt  n opts = opts{dotStyle   = n}
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
                                             "US"  -> English
                                             "EN"  -> English
                                             _     -> Dutch}
logOpt :: String -> Options -> Options
logOpt nm       opts = opts{uncheckedLogName = Just nm}
sqlHostOpt  :: Maybe String -> Options -> Options
sqlHostOpt  (Just nm) opts = opts{sqlHost  = nm}
sqlHostOpt   Nothing  opts = opts
sqlLoginOpt :: Maybe String -> Options -> Options
sqlLoginOpt (Just nm) opts = opts{sqlLogin = nm, sqlLogPwdDefd=True}
sqlLoginOpt  Nothing  opts = opts
sqlPwdOpt   :: Maybe String -> Options -> Options
sqlPwdOpt   (Just nm) opts = opts{sqlPwd   = nm, sqlLogPwdDefd=True}
sqlPwdOpt    Nothing  opts = opts
testOpt :: Options -> Options
testOpt opts = opts{test = True}
verbose :: Options -> String -> IO ()
verbose flags x
    | verboseP flags = putStr x
    | otherwise      = doNothing
   
verboseLn :: Options -> String -> IO ()
verboseLn flags x
    | verboseP flags = putStrLn x
    | otherwise      = doNothing
    
doNothing :: IO()
doNothing = putStr ""   -- Ik weet zo gauw niet hoe dit anders moet....
unchecked :: String
unchecked = "."
                             

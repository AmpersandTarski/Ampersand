{-# OPTIONS_GHC -Wall #-}
module Options (Options(..),getOptions,usageInfo',verboseLn,verbose,FspecFormat(..),DocTheme(..),allFspecFormats,defaultFlags)
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
import Version(versionNumber)
import Maybe
-- | This data constructor is able to hold all kind of information that is useful to 
--   express what the user would like ADL to do. 
data Options = Options { contextName   :: Maybe String
                       , showVersion   :: Bool
                       , preVersion    :: String
                       , postVersion   :: String  --built in to aid DOS scripting... 8-(( Bummer. 
                       , showHelp      :: Bool
                       , verboseP      :: Bool
                       , genPrototype  :: Bool 
                       , dirPrototype  :: String  -- the directory to generate the prototype in.
                       , allServices   :: Bool
                       , dbName        :: String
                       , genAtlas      :: Bool
                       , dirAtlas      :: String  -- the directory to generate the atlas in.
                       , userAtlas     :: String
                       , theme         :: DocTheme --the theme of some generated output. (style, content differentiation etc.)
                       , genXML        :: Bool
                       , genFspec      :: Bool
                       , diag          :: Bool
                       , fspecFormat   :: FspecFormat
                       , genGraphics   :: Bool   -- if True, graphics will be generated for use in Ampersand products like the Atlas or Functional Spec
                       , useGraphics   :: Bool   -- if True, graphics will be used in the generated products. 
                       , flgSwitchboard:: Bool   -- if True, switchboard graphics will be generated in the functional spec.
                       , proofs        :: Bool
                       , haskell       :: Bool
                       , dirOutput     :: String -- the directory to generate the output in.
                       , beeper        :: Bool
                       , crowfoot      :: Bool
                       , language      :: Lang
                       , dirExec       :: String --the base for relative paths to input files
                       , texHdrFile    :: Maybe String --the string represents a FilePath to some .tex containing just tex header instructions
                       , progrName     :: String --The name of the adl executable
                       , fileName      :: String
                       , baseName      :: String
                       , logName       :: String
                       , genTime       :: ClockTime
                       , services      :: Bool
                       , test          :: Bool
                       , sqlLogPwdDefd :: Bool
                       , sqlHost       :: String
                       , sqlLogin      :: String
                       , sqlPwd        :: String
                       , verbosephp    :: Bool
                       , helpNVersionTexts :: [String] 
                       } deriving Show
    
defaultFlags :: Options 
defaultFlags = Options { language = Dutch} --TODO Verder invullen. 

getOptions :: IO Options
getOptions =
   do args     <- getArgs
      progName <- getProgName
      defaultOpts <- defaultOptionsM
      (flags,fNames)  <- case getOpt Permute (each options) args of
                         (o,n,[])    -> return ((foldl (flip id) (defaultOpts) o ),n)
                         (_,_,errs)  -> ioError (userError (concat errs ++ usageInfo'' progName))
      checkNSetOptionsAndFileNameM (flags,fNames)
  where 
     defaultOptionsM :: IO(Options)
     defaultOptionsM  =
           do clocktime <- getClockTime
              progName <- getProgName
              exePath <- findExecutable progName
              env <- getEnvironment
              return
               Options{ genTime                = clocktime
                      , dirAtlas      = fromMaybe "."       (lookup envdirAtlas     env)
                      , dirOutput     = fromMaybe "."       (lookup envdirOutput    env)
                      , dirPrototype  = fromMaybe "."       (lookup envdirPrototype env)
                      , dbName        = fromMaybe ""        (lookup envdbName       env)
                      , logName       = fromMaybe "ADL.log" (lookup envlogName      env)
                      , dirExec       = case exePath of
                                          Nothing -> error ("!Fatal (module Options 126): Specify the path location of "++progName++" in your system PATH variable.")
                                          Just s  -> takeDirectory s
                      , preVersion    = fromMaybe ""        (lookup "CCPreVersion"  env)
                      , postVersion   = fromMaybe ""        (lookup "CCPostVersion" env)
                      , theme         = DefaultTheme
                      , contextName   = Nothing
                      , showVersion   = False
                      , showHelp      = False
                      , verboseP      = False
                      , verbosephp    = False
                      , genPrototype  = False
                      , allServices   = False
                      , genAtlas      = False   
                      , userAtlas     = []
                      , genXML        = False
                      , genFspec      = False 
                      , diag          = False 
                      , fspecFormat   = error ("Unknown fspec format. Currently supported formats are "++allFspecFormats++".")
                      , genGraphics   = True
                      , useGraphics   = True
                      , flgSwitchboard= False
                      , proofs        = False
                      , haskell       = False
                      , texHdrFile    = Nothing
                  --GMI -> ik zeg Maybe String, omdat ik de mogelijkheid wil hebben om geen PATH variabele te hoeven gebruiken, en omdat er geen fatsoenlijke standaardwaarde ingevuld kan worden, zoals met een directory.
                  --    , texHdrFile    = error ("!Fatal (module Options 120): Specify the path location of "++progName)++" in your system PATH variable."
                      , beeper        = False
                      , crowfoot      = False
                      , language      = Dutch
                      , progrName     = progName
                      , fileName      = error ("!Fatal (module Options 123): no default value for fileName.")
                      , baseName      = error ("!Fatal (module Options 124): no default value for baseName.")
                      , services      = False
                      , test          = False
                      , sqlLogPwdDefd = False
                      , sqlHost       = "localhost"
                      , sqlLogin      = "root"
                      , sqlPwd        = ""
                      , helpNVersionTexts = []
                      }



     checkNSetOptionsAndFileNameM :: (Options,[String]) -> IO(Options)
     checkNSetOptionsAndFileNameM (flags,fNames) = 
          if or [showVersion flags, showHelp flags] 
          then return flags {helpNVersionTexts = [preVersion flags++"ADLvs" ++ versionNumber++postVersion flags| showVersion flags]
                                               ++[usageInfo' flags                                   | showHelp    flags]
                            }
          else case fNames of
                []      -> error ("no file to parse" ++useHelp)
                [fName] -> verboseLn flags ("Checking output directories...")
                        >> checkLogName flags
                        >> checkDirOutput flags
                        >> checkExecOpts flags
                        >> checkProtoOpts flags
                        >> checkAtlasOpts flags
                        >> return flags { fileName    = if hasExtension fName
                                                         then fName
                                                         else addExtension fName "adl" 
                                        , baseName    = basename fName
                                        , dbName      = case dbName flags of
                                                            ""  -> basename fName
                                                            str -> str
                                        }
                x:xs    -> error ("too many files: "++ (chain ", " (x:xs)) ++useHelp)
       
       where
          basename :: FilePath -> String
          basename n= takeBaseName n
          useHelp :: String
          useHelp = " (use --help for help) "
          checkLogName :: Options -> IO ()
          checkLogName   f = createDirectoryIfMissing True (takeDirectory (logName f))
          checkDirOutput :: Options -> IO ()
          checkDirOutput f = createDirectoryIfMissing True (dirOutput f)

          checkExecOpts :: Options -> IO ()
          checkExecOpts f = do execPath <- findExecutable (progrName f) 
                               when (execPath == Nothing) 
                                    (error ("!Fatal (module Options 162): Specify the path location of "++(progrName f)++" in your system PATH variable."))
          checkProtoOpts :: Options -> IO ()
          checkProtoOpts f = when (genPrototype f) (createDirectoryIfMissing True (dirPrototype f))
          checkAtlasOpts :: Options -> IO ()
          checkAtlasOpts f = when (genAtlas f)     (createDirectoryIfMissing True (dirAtlas     f))

            
data DisplayMode = Public | Hidden 
data FspecFormat = FPandoc | FRtf | FOpenDocument | FLatex | FHtml  deriving (Show, Eq)
data DocTheme = DefaultTheme   -- Just the functional specification
              | ProofTheme     -- A document with type inference proofs
              | StudentTheme   -- An adjusted func spec for students of the business rules course
                 deriving (Show, Eq)
    
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
          , ((Option ['d']     ["dbName"]      (ReqArg dbNameOpt "name")   ("the prototype will use database with name (name overrides environment variable "++ envdbName ++ "). when both are't set, defaults to filename (without '.adl')")), Public)
           , ((Option ['t']       ["theme"]      (ReqArg themeOpt "theme")   ("p.e. student")), Public)
          , ((Option ['x']     ["maxServices"] (NoArg maxServicesOpt)      "if specified in combination with -p -f or -s then it uses generated services to generate a prototype, functional spec, or adl file respectively."), Public)
          , ((Option ['s']     ["services"]    (NoArg servicesOpt)         "generate service specifications in ADL format. Specify -x to generate services."), Public)

          , ((Option ['o']     ["outputDir"]   (ReqArg outputDirOpt "dir") ("default directory for generated files (dir overrides environment variable "++ envdirOutput ++ ").")), Public)
          , ((Option []        ["log"]         (ReqArg logOpt "name")      ("log to file with name (name overrides environment variable "++ envlogName  ++ ").")), Hidden)

          , ((Option ['a']     ["atlas"]       (OptArg atlasOpt "dir")     ("generate atlas (optional an output directory, defaults to current directory) (dir overrides  environment variable"++ envdirAtlas ++ ").")), Public)
          , ((Option []        ["user"]        (ReqArg userOpt "user")     ("generate atlas content for this user.")), Public)
          , ((Option ['f']     ["fspec"]       (ReqArg fspecRenderOpt "format")  
                                                                           ("generate a functional specification document in specified format ("++allFspecFormats++").")), Public)
          , ((Option []        ["headerfile"]  (ReqArg languageOpt "filename") "use your own custom header file to prefix to the text before rendering."), Public)
          , ((Option []        ["noGraphics"]  (NoArg noGraphicsOpt)       "save compilation time by not generating any graphics."), Public)
          , ((Option []        ["Switchboard"] (NoArg switchboardOpt)      "generate switchboard graphics in services documentation for diagnostic purposes."), Hidden)
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
  = opts { dirPrototype = fromMaybe (dirPrototype opts) nm
         , genPrototype = True}
atlasOpt     :: Maybe String -> Options -> Options
atlasOpt nm opts 
  = opts { dirAtlas     = fromMaybe (dirAtlas opts) nm
         , dirOutput    = fromMaybe (dirAtlas opts) nm
         , genAtlas     = True}
maxServicesOpt :: Options -> Options
maxServicesOpt  opts = opts{allServices  = True}                            
themeOpt :: String -> Options -> Options
themeOpt t opts = opts{theme = case (map toUpper t) of 
                                    "STUDENT" -> StudentTheme
                                    "PROOF"   -> ProofTheme
                                    _         -> DefaultTheme}
dbNameOpt :: String -> Options -> Options
dbNameOpt nm opts = opts{dbName = if nm == "" 
                                    then baseName opts
                                    else nm
                        }                          
userOpt :: String -> Options -> Options
userOpt x opts = opts{userAtlas = x}
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
allFspecFormats :: String
allFspecFormats = "Pandoc, Rtf, OpenDocument, Latex, Html"
switchboardOpt :: Options -> Options
switchboardOpt opts = opts{flgSwitchboard = True}
noGraphicsOpt :: Options -> Options
noGraphicsOpt  opts = opts{genGraphics    = False}
proofsOpt :: Options -> Options
proofsOpt       opts = opts{proofs       = True}
servicesOpt :: Options -> Options
servicesOpt     opts = opts{services     = True}
haskellOpt :: Options -> Options
haskellOpt      opts = opts{haskell      = True}
outputDirOpt :: String -> Options -> Options
outputDirOpt nm opts = opts{dirOutput    = nm}
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
logOpt nm       opts = opts{logName = nm}
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
    | otherwise      = return ()
   
verboseLn :: Options -> String -> IO ()
verboseLn flags x
    | verboseP flags = putStrLn x
    | otherwise      = return ()
                             

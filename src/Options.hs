
module Options (Options(..),getOptions,usageInfo')
where

import System.Console.GetOpt
-- | This data constructor is able to hold all kind of information that is usefull to 
--   express what the user would like adl to do. 
data Options = Options { contextName   :: OptionalString
                       , showVersion   :: Bool
                       , showHelp      :: Bool
                       , verbose       :: Bool
					   , genPrototype  :: Bool
					   , dirPrototype  :: OptionalString
					   , allServices   :: Bool
					   , genAtlas      :: Bool
					   , dirAtlas      :: OptionalString
					   , xml           :: Bool
					   , fspec         :: Bool
					   , proofs        :: Bool
					   , haskell       :: Bool
					   , dirOutput     :: OptionalString
					   , beeper        :: Bool
                       } deriving Show 

-- | parse the command line options and return an 'Options' object and a string containing the .adl file  
getOptions :: [String] -> String -> [(String, String)] -> IO(Options,String) 
getOptions args progName environment = 
      case getOpt Permute options args of
          (o,[n],[])    -> return (foldl (flip id) (defaultOptions environment) o , n)
          (_,[],[] )    -> ioError (userError ("no file to parse" ++usageInfo' progName))
          (_,x:xs,[])   -> ioError (userError ("too many files: "++ show [x:xs] ++usageInfo' progName))
          (_,_,errs)    -> ioError (userError (concat errs ++ usageInfo' progName))

usageInfo' :: String -> String
usageInfo' progName = usageInfo (infoHeader progName) options
          
infoHeader :: String -> String
infoHeader progName = "\nUsage info:\n " ++ progName ++ " options file ...\n\nList of options:"
data OptionalString  =  Name String
                     | Default deriving Show





options     :: [OptDescr (Options -> Options)]
options  = [ Option ['C']     ["context"]      (OptArg contextOpt "name")  "use context with name"
           , Option ['v']     ["version"]      (NoArg versionOpt)          "show version and exit"
           , Option ['h','?'] ["help"]         (NoArg helpOpt)             "get (this) usage information"
           , Option []        ["verbose"]      (NoArg verboseOpt)          "verbose error message format"
           , Option ['p']     ["proto"]        (OptArg prototypeOpt "dir") "generate a functional prototype with services defined in ADL file" 
           , Option ['P']     ["maxServices"]  (NoArg maxServicesOpt)      "if specified, generate all services in the prototype"
           , Option ['a']     ["atlas"]        (OptArg atlasOpt "dir" )    "generate atlas (optional an output directory, defaults to current directory)"
           , Option []        ["XML"]          (NoArg xmlOpt)              "generate XML output"
           , Option []        ["fspec"]        (NoArg fspecOpt)            "generate a functional specification document"
           , Option []        ["proofs"]       (NoArg proofsOpt)           "generate correctness proofs"
           , Option []        ["haskell"]      (NoArg haskellOpt)          "generate internal data structure, written in Haskell source code (for debugging)"
           , Option ['o']     ["outputDir"]    (ReqArg outputDirOpt "dir") "default directory for generated files"        
           , Option []        ["beeper"]       (NoArg beeperOpt)           "generate beeper instead of checker"
           ]

defaultOptions :: [(String, String)] -> Options
defaultOptions environment = Options { contextName   = Default
                                     , showVersion   = False
                                     , showHelp      = False
                                     , verbose       = False
						             , genPrototype  = False
            					     , dirPrototype  = lookupdir "CCdirPrototype" environment
			            		     , allServices   = False
					                 , genAtlas      = False   
			            		     , dirAtlas      = lookupdir "CCdirAtlas" environment
			            		     , xml           = False 
				            	     , fspec         = False
				            	     , proofs        = False
				            	     , haskell       = False
				            	     , dirOutput     = lookupdir "CCdirOutput" environment
                                     , beeper        = False
                                     }
lookupdir :: String -> [(String, String)] -> OptionalString
lookupdir envvariable env = 
   case lookup envvariable env of
       Nothing -> Default
       Just s  -> Name s


contextOpt  nm  opts = opts{contextName   = maybe Default Name nm}            
versionOpt      opts = opts{showVersion  = True}            
helpOpt         opts = opts{showHelp     = True}            
verboseOpt      opts = opts{verbose      = True}            
prototypeOpt nm opts = opts{dirPrototype = maybe Default Name nm
                           ,genPrototype = True}
maxServicesOpt  opts = opts{genPrototype = True
                           ,allServices  = True}                            
atlasOpt nm     opts = opts{dirAtlas     = maybe Default Name nm
                           ,genAtlas     = True}
xmlOpt          opts = opts{xml          = True}
fspecOpt        opts = opts{fspec        = True}
proofsOpt       opts = opts{proofs       = True}
haskellOpt      opts = opts{haskell      = True}
outputDirOpt nm opts = opts{dirOutput    = Name nm}
beeperOpt       opts = opts{beeper       = True}



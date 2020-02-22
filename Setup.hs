{-# LANGUAGE OverloadedStrings #-}
-- | Before each build, generate a BuildInfo_Generated module that exports the project version from cabal,
-- the current revision number and the build time. Also generate a file that contains files that 
-- are being included into the ampersand.exe file
-- Note that in order for this Setup.hs to be used by cabal, the build-type should be Custom.
module Main 
where
import qualified Codec.Compression.GZip as GZip  --TODO replace by Codec.Archive.Zip from package zip-archive. This reduces the amount of packages. (We now use two for zipping/unzipping)
import qualified Data.ByteString.Lazy.Char8 as BLC
import           RIO.Char
import qualified RIO.List as L
import           RIO.Prelude
import           RIO
import qualified RIO.Text as T
import           Prelude
import           RIO.Time
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import           Distribution.PackageDescription
import           Distribution.Pretty (prettyShow)
import           System.Directory
import           System.Environment (getEnvironment)
import qualified System.Exit as SE
import           System.FilePath
import           System.IO(IOMode(ReadMode),hGetContents)
import           System.Process(readProcessWithExitCode)
main :: IO ()
main = defaultMainWithHooks (simpleUserHooks { buildHook = generateHook } )

generateHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
-- | Generate Haskell modules that are required for the build and start the build
generateHook pd lbi uh bf = do
    generateBuildInfoModule (T.pack . prettyShow . pkgVersion . package $ pd)
    generateStaticFileModule
    (buildHook simpleUserHooks) pd lbi uh bf -- start the build
 
generateBuildInfoModule :: Text -> IO ()
-- | Generate a Haskell module that contains information that is available
--   only during build time.
generateBuildInfoModule cabalVersionStr = do 
    content <- buildInfoModule cabalVersionStr 
                   <$> getGitInfoStr
                   <*> (T.pack . formatTime defaultTimeLocale "%d-%b-%y %H:%M:%S %Z" 
                           <$> (getCurrentTime >>= utcToLocalZonedTime)
                       )
    writeFileUtf8 (pathFromModuleName buildInfoModuleName) content
  where
    buildInfoModuleName :: Text
    buildInfoModuleName = "Ampersand.Basics.BuildInfo_Generated"

    buildInfoModule :: Text -> Text -> Text -> Text
    buildInfoModule cabalVersion gitInfo time = T.unlines
      [ "-- | This module is generated automatically by Setup.hs before building. Do not edit!"
      , "--   It contains some functions that are supposed to grab information at the time of"
      , "--   building the ampersand executable."
      , "module "<>buildInfoModuleName<>"("
      , ""
      , "      cabalVersionStr"
      , "    , gitInfoStr"
      , "    , buildTimeStr"
      , "    ) where"
      , "import Ampersand.Basics.Prelude"
      , ""
      , "{-"<>"# NOINLINE cabalVersionStr #-}" -- disable inlining to prevent recompilation of dependent modules on each build
      , "-- | The version of Ampersand as it is stated in the package.yaml file."
      , "cabalVersionStr :: Text"
      , "cabalVersionStr = \"" <> cabalVersion <> "\""
      , ""
      , "{-"<>"# NOINLINE gitInfoStr #-}"
      , "-- | The version of Ampersand as seen by Git."
      , "gitInfoStr :: Text"
      , "gitInfoStr = \"" <> gitInfo <> "\""
      , ""
      , "{-"<>"# NOINLINE buildTimeStr #-}"
      , "-- | The time of the build."
      , "buildTimeStr :: Text"
      , "buildTimeStr = \"" <> time <> "\""
      , ""
      ]

    getGitInfoStr :: IO Text
    getGitInfoStr = getInfoStr `catch` warnGracefully
      where 
       getInfoStr = do
         eSHA <- readProcessEither "git" ["rev-parse", "--short", "HEAD"] ""
         eBranch <- readProcessEither "git" ["rev-parse", "--abbrev-ref", "HEAD"] ""
         (exitCode, _, _) <- readProcessWithExitCode "git" ["diff", "--quiet"] ""
         let isDirty = exitCode /= SE.ExitSuccess -- exit code signals whether branch is dirty
         case (eSHA, eBranch) of
           (Right sha, Right branch) ->
               return $ gitInfoStr sha branch isDirty
           _ -> do 
               -- ci/cd will create some custom environment variables.
               -- This is required in case of usage of cabal 2.4 or greater.
               -- (See https://github.com/haskell/cabal/issues/5934 for 
               -- the discussion)
               env <- getEnvironment
               case ( lookup "GIT_SHA" env
                    , lookup "GIT_Branch" env
                    ) of
                 (Just sha, Just branch) ->
                   return $ gitInfoStr branch sha False
                 _ -> do
                   mapM_ print $ lefts [eSHA, eBranch] -- errors during git execution
                   warnNoCommitInfo
           
       warnGracefully err = do
         print (err :: IOException)
         warnNoCommitInfo
       gitInfoStr sha branch isDirty =
          strip branch <> ":" <> strip sha <> (if isDirty then "*" else "")   
       strip str = reverse . dropWhile isSpace . reverse $ str
    
       readProcessEither :: Text -> [Text] -> Text -> IO (Either Text Text)
       readProcessEither cmd args stdinStr = do
         (exitCode,stdoutStr,stderrStr) <- readProcessWithExitCode cmd args stdinStr
         case exitCode of
           SE.ExitSuccess   -> return $ Right stdoutStr
           SE.ExitFailure _ -> return $ Left stderrStr

    warnNoCommitInfo :: IO Text
    warnNoCommitInfo = do
      putStrLn ""
      putStrLn ""
      putStrLn "WARNING: Execution of 'git' command failed."
      putStrLn "BuildInfo_Generated.hs will not contain revision information, and"
      putStrLn "   therefore neither will fatal error messages."
      putStrLn "   Please check your installation."
      return "no git info"


-- | datatype for several kinds of files to be included into the ampersand executable
data FileKind = 
    PandocTemplates
  -- ^ Pandoc template files for the generation of documents in different pandoc formats.
  | FormalAmpersand
  -- ^ The adl script files for formal ampersand
  | PrototypeContext
  -- ^ The adl script files for the prototype context
   deriving (Show, Eq)

generateStaticFileModule :: IO ()
-- | For each file that should be in the ampersand executable, we generate a StaticFile value,
--   which contains the information necessary for Ampersand to create the file at run-time.
--
--   To prevent compiling the generated module (which can get rather big) on each build, we compare the contents
--   the file we are about to generate with the contents of the already generated file and only write if there is a difference.
--   This complicates the build process, but seems the only way to handle large amounts of diverse static
--   files, until Cabal's data-files mechanism is updated to allow fully recursive patterns.
generateStaticFileModule = do
    previousModuleContents <- getPreviousModuleContents
    currentModuleContents <- readAllStaticFiles
    let updateRequired = previousModuleContents == currentModuleContents
    if updateRequired 
      then
        putStrLn $ "Static files unchanged, no need to update "<>sfModulePath
      else do
        putStrLn $ "Static files have changed, updating "<>sfModulePath
        writeFile sfModulePath currentModuleContents
  where 
    staticFileModuleName :: Text
    staticFileModuleName = "Ampersand.Prototype.StaticFiles_Generated"

    sfModulePath = pathFromModuleName staticFileModuleName
    
    getPreviousModuleContents :: IO Text
    getPreviousModuleContents = reader `catch` errorHandler
      where
        reader = withFile sfModulePath ReadMode $ \h -> do
            str <- hGetContents h
            length str `seq` return () -- lazy IO is :-(
            return str
        errorHandler err = do  -- old generated module exists, but we can't read the file or read the contents
          putStrLn $ unlines 
             [ ""
             , "Warning: Cannot read previously generated " <> sfModulePath <> ":"
             , show (err :: SomeException)
             , "This warning should disappear the next time you build Ampersand. If the error persists, please report this as a bug."
             , ""
             ]
          return []
         
    
    -- | Collect all files required to be inside the ampersand.exe 
    readAllStaticFiles :: IO Text
    readAllStaticFiles = do
        pandocTemplatesFiles <- readStaticFiles PandocTemplates  "outputTemplates" "." -- templates for several PANDOC output types
        formalAmpersandFiles <- readStaticFiles FormalAmpersand  "AmpersandData/FormalAmpersand"  "."  --meta information about Ampersand
        systemContextFiles   <- readStaticFiles PrototypeContext "AmpersandData/PrototypeContext" "."  --Special system context for Ampersand
        return $ mkStaticFileModule $ pandocTemplatesFiles <> formalAmpersandFiles <> systemContextFiles
        

    readStaticFiles :: FileKind -> FilePath -> FilePath -> IO [Text]
    readStaticFiles fkind base fileOrDirPth = do
        let path = combine base fileOrDirPth
        isDir <- doesDirectoryExist path
        if isDir 
           then do
             fOrDs <- getProperDirectoryContents path
             fmap concat $ mapM (\fOrD -> readStaticFiles fkind base (combine fileOrDirPth fOrD)) fOrDs
           else do
             timeStamp <- getModificationTime path
             fileContents <- BLC.readFile path
             return [ "SF "<>show fkind<>" "<>show fileOrDirPth<>" "<>utcToEpochTime timeStamp <>
                                 " {-"<>show timeStamp<>" -} (BLC.unpack$ GZip.decompress "<>show (GZip.compress fileContents)<>")"
                        ]
      where utcToEpochTime :: UTCTime -> Text
            utcToEpochTime utcTime = formatTime defaultTimeLocale "%s" utcTime


    mkStaticFileModule :: [Text] -> Text
    mkStaticFileModule sfDeclStrs =
      unlines staticFileModuleHeader <>
      "  [ " <> L.intercalate "\n  , " sfDeclStrs <> "\n" <>
      "  ]\n"

    staticFileModuleHeader :: [Text]
    staticFileModuleHeader =
      [ "{-# LANGUAGE OverloadedStrings #-}"
      , "module "<>staticFileModuleName
      , "   ( StaticFile(..),FileKind(..)"
      , "   , allStaticFiles, getStaticFileContent"
      , "   )"
      , "where"
      , "import           Ampersand.Basics"
      , "import qualified Data.ByteString.Lazy.Char8 as BLC"
      , "import qualified Codec.Compression.GZip as GZip"
      , "import           System.FilePath"
      , ""
      , "data FileKind = PandocTemplates | FormalAmpersand | PrototypeContext deriving (Show, Eq)"
      , "data StaticFile = SF { fileKind      :: FileKind"
      , "                     , filePath      :: FilePath -- relative path, including extension"
      , "                     , timeStamp     :: Integer  -- unix epoch time"
      , "                     , contentString :: Text"
      , "                     }"
      , ""
      , "getStaticFileContent :: FileKind -> FilePath -> Maybe Text"
      , "getStaticFileContent fk fp ="
      , "     case filter isRightFile allStaticFiles of"
      , "        [x] -> Just (contentString x)"
      , "        _   -> Nothing"
      , "  where"
      , "    isRightFile :: StaticFile -> Bool"
      , "    isRightFile (SF fKind path _ _ ) = fKind == fk && equalFilePath path (\".\" </> fp)"
      , ""
      , "{-"<>"# NOINLINE allStaticFiles #-}" -- Workaround: break pragma start { - #, since it upsets Eclipse :-(
      , "allStaticFiles :: [StaticFile]"
      , "allStaticFiles ="
      ]

    getProperDirectoryContents :: FilePath -> IO [Text]
    getProperDirectoryContents fp = (fmap T.pack . filter (`notElem` [".","..",".svn"])) <$> getDirectoryContents fp

pathFromModuleName :: Text -> FilePath
pathFromModuleName m = T.unpack $ "src/" <> T.map (\c -> if c == '.' then '/' else c) m <> ".hs"

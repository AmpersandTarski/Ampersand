{-# LANGUAGE OverloadedStrings #-}
-- | Before each build, generate a BuildInfo_Generated module that exports the project version from cabal,
-- the current revision number and the build time. Also generate a file that contains files that 
-- are being included into the ampersand.exe file
-- Note that in order for this Setup.hs to be used by cabal, the build-type should be Custom.
module Main 
where
--import qualified Codec.Compression.GZip as GZip  --TODO replace by Codec.Archive.Zip from package zip-archive. This reduces the amount of packages. (We now use two for zipping/unzipping)
import           Codec.Archive.Zip
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import           Distribution.PackageDescription
import           Distribution.Pretty (prettyShow)
import           Prelude(print,putStrLn)
import           RIO
import           RIO.Char
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import           RIO.Time
import           System.Directory
import           System.Environment (getEnvironment)
import qualified System.Exit as SE
import           System.FilePath
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
      [ "{-# LANGUAGE OverloadedStrings #-}"
      , "-- | This module is generated automatically by Setup.hs before building. Do not edit!"
      , "--   It contains some functions that are supposed to grab information at the time of"
      , "--   building the ampersand executable."
      , "module "<>buildInfoModuleName<>"("
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
       getInfoStr :: IO Text
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
                   return $ gitInfoStr (T.pack branch) (T.pack sha) False
                 _ -> do
                   mapM_ print $ lefts [eSHA, eBranch] -- errors during git execution
                   warnNoCommitInfo
           
       warnGracefully :: IOException -> IO Text
       warnGracefully err = do
         print (err :: IOException)
         warnNoCommitInfo
       gitInfoStr :: Text -> Text -> Bool -> Text
       gitInfoStr sha branch isDirty =
          strip branch <> ":" <> strip sha <> (if isDirty then "*" else "")   
       strip :: Text -> Text
       strip = T.reverse . T.dropWhile isSpace . T.reverse
    
       readProcessEither :: FilePath -> [Text] -> Text -> IO (Either Text Text)
       readProcessEither cmd args stdinStr = do
         (exitCode,stdoutStr,stderrStr) <- readProcessWithExitCode cmd (map T.unpack args) (T.unpack stdinStr)
         case exitCode of
           SE.ExitSuccess   -> return . Right . T.pack $ stdoutStr
           SE.ExitFailure _ -> return . Left  . T.pack $ stderrStr

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
   deriving (Show, Eq, Bounded, Enum)

generateStaticFileModule :: IO ()
-- | For each set of files (by FileKind), we generate an Archive value,
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
        writeFileUtf8 sfModulePath currentModuleContents
  where 
    staticFileModuleName :: Text
    staticFileModuleName = "Ampersand.Prototype.StaticFiles_Generated"

    sfModulePath = pathFromModuleName staticFileModuleName
    
    getPreviousModuleContents :: IO Text
    getPreviousModuleContents = reader `catch` errorHandler
      where
        reader :: IO Text
        reader = readFileUtf8 sfModulePath  
        errorHandler err = do  -- old generated module exists, but we can't read the file or read the contents
          putStrLn $ unlines 
             [ ""
             , "Warning: Cannot read previously generated " <> sfModulePath <> ":"
             , show (err :: SomeException)
             , "This warning should disappear the next time you build Ampersand. If the error persists, please report this as a bug."
             , ""
             ]
          return mempty
         
    
    -- | Collect all files required to be inside the ampersand.exe 
    readAllStaticFiles :: IO Text
    readAllStaticFiles = do
        pandocTemplatesFiles <- readStaticFiles PandocTemplates  "." -- templates for several PANDOC output types
        formalAmpersandFiles <- readStaticFiles FormalAmpersand  "." -- meta information about Ampersand
        systemContextFiles   <- readStaticFiles PrototypeContext "." -- Special system context for Ampersand
        return $ mkStaticFileModule $ pandocTemplatesFiles <> formalAmpersandFiles <> systemContextFiles

    readStaticFiles :: FileKind -> FilePath -> IO [(FileKind,Entry)]
    readStaticFiles fkind fileOrDirPth = do 
        let path = base </> fileOrDirPth
        isDir <- doesDirectoryExist path
        if isDir 
           then do
             fOrDs <- getProperDirectoryContents path
             fmap concat $ mapM (\fOrD -> readStaticFiles fkind (fileOrDirPth </> fOrD)) fOrDs
           else do
             entry <- removeBase <$> readEntry [OptVerbose] (base</>fileOrDirPth)
             return [(fkind,entry)]
      where removeBase :: Entry -> Entry
            removeBase entry = entry{eRelativePath = rpWithoutBase}
                where rpWithoutBase = stripbase (eRelativePath entry)
                      stripbase :: FilePath -> FilePath
                      stripbase fp = case L.stripPrefix (base++"/") fp of
                                       Just stripped -> stripped
                                       Nothing -> error . L.intercalate "\n" $
                                          ["ERROR: Reading static files failed:"
                                          ,"  base: "<>base
                                          ,"  fp  : "<>fp
                                          ]
            base = case fkind of
               PandocTemplates  -> "outputTemplates"
               FormalAmpersand  -> "AmpersandData/FormalAmpersand"
               PrototypeContext -> "AmpersandData/PrototypeContext"

    mkStaticFileModule :: [(FileKind,Entry)] -> Text
    mkStaticFileModule xs =
      T.unlines staticFileModuleHeader <>
      "  [ " <> T.intercalate "\n  , " (map toText archives) <> "\n" <>
      "  ]\n"
      where toText :: (FileKind,Archive) -> Text
            toText (fk, archive) = "SF "<>tshow fk<>" "<>tshow archive
            archives :: [(FileKind,Archive)]
            archives = map mkArchive $ NE.groupBy tst xs
              where tst :: (FileKind,a) -> (FileKind,a) -> Bool
                    tst a b = fst a == fst b
                    mkArchive :: NE.NonEmpty (FileKind,Entry) -> (FileKind,Archive)
                    mkArchive entries = 
                        ( fst . NE.head $ entries
                        , foldr addEntryToArchive emptyArchive $ snd <$> NE.toList entries
                        )
    staticFileModuleHeader :: [Text]
    staticFileModuleHeader =
      [ "{-# LANGUAGE OverloadedStrings #-}"
      , "module "<>staticFileModuleName
      , "   ( FileKind(..)"
      , "   , getStaticFileContent"
      , "   )"
      , "where"
      , "import           Ampersand.Basics"
      , "import           Codec.Archive.Zip"
      , "import qualified RIO.ByteString as B"
      , "import qualified RIO.ByteString.Lazy as BL"
      , "import qualified RIO.Text as T"
      , ""
      , "data FileKind = PandocTemplates | FormalAmpersand | PrototypeContext deriving (Show, Eq)"
      , "data StaticFile = SF FileKind Archive"
      , ""
      , "getStaticFileContent :: FileKind -> FilePath -> Maybe B.ByteString"
      , "getStaticFileContent fk fp = BL.toStrict <$>"
      , "     case filter isRightArchive allStaticFiles of"
      , "        [SF _ a] -> case findEntryByPath fp a of"
      , "                      Just entry -> Just $ fromEntry entry"
      , "                      Nothing    -> fatal . T.intercalate \"\\n\" $ "
      , "                       [ \"Looking for file: \"<>tshow fp"
      , "                       , \"in archive: \"<>tshow fk"
      , "                       , \"Archive found. it contains:\""
      , "                       ]++map ((\"  \" <>) . showEntry) (zEntries a)"
      , "        xs       -> fatal . T.intercalate \"\\n\" $"
      , "                       [ \"Looking for file: \"<>tshow fp"
      , "                       , \"in archive: \"<>tshow fk"
      , "                       ]++showArchives xs"
      , "  where"
      , "    isRightArchive :: StaticFile -> Bool"
      , "    isRightArchive (SF fKind _) = fKind == fk"
      , "    showEntry :: Entry -> Text"
      , "    showEntry = tshow . eRelativePath  "
      , "    showArchives :: [StaticFile] -> [Text]"
      , "    showArchives xs = "
      , "       [ \"Number of archives: \"<>tshow (length xs)"
      , "       ]++"
      , "       concatMap showSF xs"
      , "       where"
      , "         showSF :: StaticFile -> [Text]"
      , "         showSF (SF fKind archive) = "
      , "           [ \"  Archive: \"<>tshow fKind<>\" (\"<>(tshow . length . zEntries $ archive)<>\" entries)\""
      , "           ]"
      , ""
      , "{-"<>"# NOINLINE allStaticFiles #-}" -- Workaround: break pragma start { - #, since it upsets Eclipse :-(
      , "allStaticFiles :: [StaticFile]"
      , "allStaticFiles = "
      ]

    getProperDirectoryContents :: FilePath -> IO [FilePath]
    getProperDirectoryContents fp = (filter (`notElem` [".","..",".git"])) <$> getDirectoryContents fp

pathFromModuleName :: Text -> FilePath
pathFromModuleName m = T.unpack $ "src/" <> T.map (\c -> if c == '.' then '/' else c) m <> ".hs"

{-# OPTIONS_GHC -Wall #-}  
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription
import System.Process
import System.Exit
import Control.Exception
import Data.List
import Data.Either
import Data.Char
import qualified Data.ByteString.Char8 as BS
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import System.Directory
import System.FilePath
import System.IO
import System.Locale

main :: IO ()
main = defaultMainWithHooks (simpleUserHooks { buildHook = generateBuildInfoHook } )

-- Before each build, generate a BuildInfo_Generated module that exports the project version from cabal,
-- the current svn revision number and the build time.
--
-- Note that in order for this Setup.hs to be used by cabal, the build-type should be Custom.

generateBuildInfoHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
generateBuildInfoHook pd  lbi uh bf = 
 do { let cabalVersionStr = intercalate "." (map show . versionBranch . pkgVersion . package $ pd)

    ; gitInfoStr <- getGitInfoStr
    ; clockTime <- getCurrentTime >>= utcToLocalZonedTime 
    ; let buildTimeStr = formatTime defaultTimeLocale "%d-%b-%y %H:%M:%S %Z" clockTime
    ; writeFile (pathFromModuleName buildInfoModuleName) $
        buildInfoModule cabalVersionStr gitInfoStr buildTimeStr
    
    ; generateStaticFileModule 

    ; putStrLn ""
    ; (buildHook simpleUserHooks) pd lbi uh bf -- start the build
    }

buildInfoModuleName :: String
buildInfoModuleName = "Database.Design.Ampersand.Basics.BuildInfo_Generated"

buildInfoModule :: String -> String -> String -> String
buildInfoModule cabalVersion gitInfo time = unlines
  [ "module "++buildInfoModuleName++"(cabalVersionStr, gitInfoStr, buildTimeStr) where" 
  , ""
  , "-- This module is generated automatically by Setup.hs before building. Do not edit!"
  , ""
  -- Workaround: break pragma start { - #, since it upsets Eclipse :-( 
  , "{-"++"# NOINLINE cabalVersionStr #-}" -- disable inlining to prevent recompilation of dependent modules on each build
  , "cabalVersionStr :: String"
  , "cabalVersionStr = \"" ++ cabalVersion ++ "\""
  , ""
  , "{-"++"# NOINLINE gitInfoStr #-}"
  , "gitInfoStr :: String"
  , "gitInfoStr = \"" ++ gitInfo ++ "\""
  , ""
  , "{-"++"# NOINLINE buildTimeStr #-}"
  , "buildTimeStr :: String"
  , "buildTimeStr = \"" ++ time ++ "\""
  , ""
  ]
    
getGitInfoStr :: IO String
getGitInfoStr = 
 do { eSHA <- readProcessEither "git" ["rev-parse", "--short", "HEAD"] ""
    ; eBranch <- readProcessEither "git" ["rev-parse", "--abbrev-ref", "HEAD"] ""
    ; (exitCode, _, _) <- readProcessWithExitCode "git" ["diff", "--quiet"] ""
    ; let isDirty = exitCode /= ExitSuccess -- exit code signals whether branch is dirty
    ; case (eSHA, eBranch) of
        (Right sha, Right branch) -> 
         return $ strip branch ++ ":" ++ strip sha ++ (if isDirty then "*" else "")
        _ ->
         do { mapM_ print $ lefts [eSHA, eBranch] -- errors during git execution
            ; warnNoCommitInfo
            }
    } `catch` \err ->  -- git failed to execute
         do { print (err :: IOException)
            ; warnNoCommitInfo
            }
 where strip str = reverse . dropWhile isSpace . reverse $ str

       readProcessEither :: String -> [String] -> String -> IO (Either String String)
       readProcessEither cmd args stdinStr = 
        do { (exitCode,stdoutStr,stderrStr) <- readProcessWithExitCode cmd args stdinStr
           ; case exitCode of
               ExitSuccess   -> return $ Right stdoutStr
               ExitFailure _ -> return $ Left stderrStr
           }
 
warnNoCommitInfo :: IO String
warnNoCommitInfo =
 do { putStrLn "\n\n\nWARNING: Execution of 'git' command failed."
    ; putStrLn $ "BuildInfo_Generated.hs will not contain revision information, and therefore\nneither will fatal error messages.\n"++
                 "Please check your installation\n"
    ; return "no git info"
    }


{- For each file in the directory ampersand/static, we generate a StaticFile value,
   which contains the information necessary for Ampersand to create the file at run-time.  
   
   To prevent compiling the generated module (which can get rather big) on each build, we first try to read the
   contents of the previously generated static file module and only update it if there are changes to timestamps
   or filenames. This complicates the build process, but seems the only way to handle large amounts of diverse static
   files, until Cabal's data-files mechanism is updated to allow fully recursive patterns.
-}

staticFileModuleName :: String
staticFileModuleName = "Database.Design.Ampersand.Prototype.StaticFiles_Generated"

generateStaticFileModule :: IO ()
generateStaticFileModule =
 do { previousSFs <- readPreviousStaticFiles sfModulePath -- StaticFiles from previously generated module
    ; let prevFilesAndTimestamps = getFilesAndTimestamps previousSFs
    ; currentStaticFileContents <- readAllStaticFiles
    ; let currentFilesAndTimestamps = map (\(isNew,pth,ts,_,_) -> (isNew,pth,ts)) currentStaticFileContents
    ; if prevFilesAndTimestamps == currentFilesAndTimestamps 
      then
        putStrLn $ "Static files unchanged, no need to update "++sfModulePath
      else
       do { putStrLn $ "Static files have changed, updating "++sfModulePath
          ; let staticFileModuleContents = mkStaticFileModuleContents currentStaticFileContents
          ; writeFile sfModulePath staticFileModuleContents
          }
    }
 where sfModulePath = pathFromModuleName staticFileModuleName
       
       getFilesAndTimestamps :: [StaticFile] -> [(Bool,FilePath,Integer)] -- For correctness, we also include isNew
       getFilesAndTimestamps sfs = map (\(SF isNew pth ts _ _) -> (isNew,pth,ts)) sfs
       
 
readPreviousStaticFiles :: String -> IO [StaticFile]
readPreviousStaticFiles sfModulePath =
 do { exists <- doesFileExist sfModulePath
    ; if not exists then return [] else
        (withFile sfModulePath ReadMode $ \h ->
          do { str <- hGetContents h
             ; let sfsStr = (unlines . drop (length staticFileModuleHeader) . lines $ str)
                   sfs = read sfsStr :: [StaticFile]
                   
             ; length sfs `seq` return () -- length & read will evaluate entire file. Lazy IO is :-(
             ; return sfs
             }) `catch` \err ->  -- old generated module exists, but we can't read the file or read the contents
                do { putStrLn $ "\n\n\nERROR: Cannot read or parse previously generated " ++ sfModulePath ++ ":\n" ++
                                show (err :: SomeException) ++ "\nTry to rebuild Ampersand, and if the error persist, please report this as a bug.\n"
                   ; return []
                   }
    }

-- Scan static file directory and collect all files from oldFrontend and newFrontend
readAllStaticFiles :: IO [(Bool, FilePath, Integer, String, BS.ByteString)]
readAllStaticFiles = 
  do { oldStaticFiles <- readStaticFiles "static/oldFrontend" "."
     ; newStaticFiles <- readStaticFiles "frontend" "."
     ; return $ map (addIsNew False) oldStaticFiles ++ map (addIsNew True) newStaticFiles
     }
  where addIsNew isNew (pth, ts, rts, cstr) = (isNew, pth, ts, rts, cstr)
  
readStaticFiles :: FilePath -> FilePath -> IO [(FilePath, Integer, String, BS.ByteString)]
readStaticFiles base fileOrDir = 
  do { let path = combine base fileOrDir
     ; isDir <- doesDirectoryExist path
     ; if isDir then 
        do { fOrDs <- getProperDirectoryContents path
           ; fmap concat $ mapM (\fOrD -> readStaticFiles base (combine fileOrDir fOrD)) fOrDs
           }
       else
        do { timeStamp <- getModificationTime path
           ; fileContents <- BS.readFile path 
           ; return [(fileOrDir, utcToEpochTime timeStamp, show timeStamp, fileContents)]
           }
     }
  where utcToEpochTime :: UTCTime -> Integer
        utcToEpochTime utcTime = read $ formatTime defaultTimeLocale "%s" utcTime

-- We need the declaration StaticFile to read the module without importing it (which fails if it's absent)
data StaticFile = SF Bool FilePath Integer String String deriving Read
-- NOTE: this declaration cannot use record syntax, as this causes 'read' to work on record syntax (which is not how the Static values are encoded)

staticFileModuleHeader :: [String]
staticFileModuleHeader =
  [ "module "++staticFileModuleName++" where"
  , ""
  , "data StaticFile = SF { isNewFrontend     :: Bool"
  , "                     , filePath          :: FilePath -- relative path, including extension"
  , "                     , timeStamp         :: Integer  -- unix epoch time"
  , "                     , readableTimeStamp :: String   -- not used, only here for clarity"
  , "                     , contentString     :: String"
  , "                     }"
  , ""
  , "{-"++"# NOINLINE allStaticFiles #-}" -- Workaround: break pragma start { - #, since it upsets Eclipse :-( 
  , "allStaticFiles :: [StaticFile]"
  , "allStaticFiles ="
  ]

mkStaticFileModuleContents :: [(Bool, FilePath, Integer, String, BS.ByteString)] -> String
mkStaticFileModuleContents sfTuples =
  unlines staticFileModuleHeader ++ 
  "  [ " ++ intercalate "\n  , " (map  mkStaticFile sfTuples) ++ "\n" ++
  "  ]\n"
  where mkStaticFile :: (Bool, FilePath, Integer, String, BS.ByteString) -> String
        mkStaticFile (isNew, pth, ts, rts, cstr) =
          "SF "++show isNew++" "++show pth++" "++ show ts ++" "++show rts++" "++show (BS.unpack cstr) 
          
          
getProperDirectoryContents :: FilePath -> IO [String]
getProperDirectoryContents pth = fmap (filter (`notElem` [".","..",".svn"])) $ getDirectoryContents pth

pathFromModuleName :: String -> String
pathFromModuleName m = "src/" ++ [if c == '.' then '/' else c | c <- m] ++ ".hs"

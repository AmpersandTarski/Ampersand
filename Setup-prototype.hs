{-# OPTIONS_GHC -Wall #-}  
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription
import System.Process
import System.Exit
import Data.Either
import Data.List
import Data.Char
import System.Directory
import System.FilePath
import qualified Data.ByteString as BS
import Control.Exception
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
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
    ; writeFile "src/Database/Design/Ampersand_Prototype/BuildInfo_Generated.hs" $
        buildInfoModule cabalVersionStr gitInfoStr buildTimeStr

    ; staticFilesGeneratedContents <- getStaticFilesModuleContents 
    ; writeFile (pathFromModule staticFileModuleName) staticFilesGeneratedContents 

    ; (buildHook simpleUserHooks) pd lbi uh bf -- start the build
    }
 where pathFromModule m = "src/" ++ [if c == '.' then '/' else c | c <- m] ++ ".hs"

buildInfoModule :: String -> String -> String -> String
buildInfoModule cabalVersion gitInfo time = unlines
  [ "module Database.Design.Ampersand_Prototype.BuildInfo_Generated (cabalVersionStr, gitInfoStr, buildTimeStr) where" 
  , ""
  , "-- This module is generated automatically by Setup.hs before building. Do not edit!"
  , ""
  , "{-# NOINLINE cabalVersionStr #-}" -- disable inlining to prevent recompilation of dependent modules on each build
  , "cabalVersionStr :: String"
  , "cabalVersionStr = \"" ++ cabalVersion ++ "\""
  , ""
  , "{-# NOINLINE gitInfoStr #-}"
  , "gitInfoStr :: String"
  , "gitInfoStr = \"" ++ gitInfo ++ "\""
  , ""
  , "{-# NOINLINE buildTimeStr #-}"
  , "buildTimeStr :: String"
  , "buildTimeStr = \"" ++ time ++ "\""
  , ""
  ]

getGitInfoStr :: IO String
getGitInfoStr = 
 do { eSHA <- readProcessEither "git" ["rev-parse", "--short", "HEAD"] ""
    ; eBranch <- readProcessEither "git" ["rev-parse", "--abbrev-ref", "HEAD"] ""
    ; (exitCode, _, _) <- readProcessWithExitCode "git" ["diff", "--quiet"] ""
    ; print exitCode
    ; let isDirty = exitCode /= ExitSuccess -- for eDirty, exit status is used to signal dirtyness
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
                 "Please find check your installation\n"
    ; return "no git info"
    }

{- In order to handle static files, we generate a module StaticFiles_Generated.

   For each file in the directory trees static and staticBinary, we generate a StaticFile value,
   which contains the information needed to have Ampersand create the file at run-time.  
   
-}

staticFileModuleName :: String
staticFileModuleName = "Database.Design.Ampersand_Prototype.StaticFiles_Generated"

getStaticFilesModuleContents :: IO String
getStaticFilesModuleContents =
 do { staticFiles       <- readStaticFiles False "static" ""
    ; staticFilesBinary <- readStaticFiles True "staticBinary" ""
    ; return $ "module "++staticFileModuleName++" where\n"++
               "\n"++
               "data StaticFile = SF { filePath      :: FilePath -- relative path, including extension\n"++
               "                     , timeStamp     :: Integer -- unix epoch time\n"++
               "                     , isBinary      :: Bool\n"++
               "                     , contentString :: String\n"++
               "                     }\n"++
               "\n"++
               "{-# NOINLINE allStaticFiles #-}\n" ++
               "allStaticFiles =\n  [ "++ 
               intercalate "\n  , " (staticFiles ++ staticFilesBinary) ++
               "\n  ]\n"
    }
    
readStaticFiles :: Bool -> FilePath -> FilePath -> IO [String]
readStaticFiles isBin base fileOrDir = 
  do { let path = combine base fileOrDir
     ; isDir <- doesDirectoryExist path
     ; if isDir then 
        do { fOrDs <- getProperDirectoryContents path
           ; fmap concat $ mapM (\fOrD -> readStaticFiles isBin base (combine fileOrDir fOrD)) fOrDs
           }
       else
        do { timeStamp <- getModificationTime path
           ; fileContents <- if isBin then fmap show $ BS.readFile path 
                                      else readFile path
           ; return ["SF "++show fileOrDir++" "++utcToEpochTime timeStamp++" {- "++show timeStamp++" -} "++
                            show isBin++" "++show fileContents]
           }
     }
  where utcToEpochTime :: UTCTime -> String
        utcToEpochTime utcTime = formatTime defaultTimeLocale "%s" utcTime
          
getProperDirectoryContents :: FilePath -> IO [String]
getProperDirectoryContents pth = fmap (filter (`notElem` [".","..",".svn"])) $ getDirectoryContents pth
 
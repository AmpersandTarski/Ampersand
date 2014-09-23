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
    ; writeFile "src/lib/Database/Design/Ampersand/Basics/BuildInfo_Generated.hs" $
        buildInfoModule cabalVersionStr gitInfoStr buildTimeStr

    ; (buildHook simpleUserHooks) pd lbi uh bf -- start the build
    }

buildInfoModule :: String -> String -> String -> String
buildInfoModule cabalVersion gitInfo time = unlines
  [ "module Database.Design.Ampersand.Basics.BuildInfo_Generated (cabalVersionStr, gitInfoStr, buildTimeStr) where" 
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

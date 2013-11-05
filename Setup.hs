{-# OPTIONS_GHC -Wall #-}  
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription
--import System.Time
import System.Process
import System.IO
import Control.Exception
import Data.List
import Data.Time.Clock
import Data.Time.Format
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

    ; svnRevisionStr <- do { r <- catch getSVNRevisionStr myHandler 
                           ; if r == "" 
                             then noSVNRevisionStr
                             else return r
                           }
    ; clockTime <- getCurrentTime 
    ; let buildTimeStr = formatTime defaultTimeLocale "%d-%b-%y %H:%M:%S %Z" clockTime
    ; writeFile "src/lib/DatabaseDesign/Ampersand/Basics/BuildInfo_Generated.hs" $
        buildInfoModule cabalVersionStr svnRevisionStr buildTimeStr

    ; (buildHook simpleUserHooks) pd lbi uh bf -- start the build
    }
 where myHandler :: IOException -> IO String
       myHandler err = do { print err
                          ; noSVNRevisionStr
                          }

buildInfoModule :: String -> String -> String -> String
buildInfoModule cabalVersion revision time = unlines
 [ "module DatabaseDesign.Ampersand.Basics.BuildInfo_Generated (cabalVersionStr, svnRevisionStr, buildTimeStr) where"
 , ""
 , "-- This module is generated automatically by Setup.hs before building. Do not edit!"
 , ""
 , "{-# NOINLINE cabalVersionStr #-}" -- disable inlining to prevent recompilation of dependent modules on each build
 , "cabalVersionStr :: String"
 , "cabalVersionStr = \"" ++ cabalVersion ++ "\""
 , ""
 , "{-# NOINLINE svnRevisionStr #-}"
 , "svnRevisionStr :: String"
 , "svnRevisionStr = \"" ++ revision ++ "\""
 , ""
 , "{-# NOINLINE buildTimeStr #-}"
 , "buildTimeStr :: String"
 , "buildTimeStr = \"" ++ time ++ "\""
 ]

getSVNRevisionStr :: IO String
getSVNRevisionStr = 
 do { (inh,outh,errh,proch) <- runInteractiveProcess "svnversion" ["."] Nothing Nothing
    ; hClose inh
    ; hClose errh
    ; version <- hGetContents outh
    ; _ <- seq version $ waitForProcess proch
    ; return (unwords . lines $ version)
    }

noSVNRevisionStr :: IO String
noSVNRevisionStr =
 do { putStrLn "\n\n\nWARNING: Execution of 'svnversion' command failed."
    ; putStrLn $ "BuildInfo_Generated.hs will not contain revision information, and therefore\nneither will fatal error messages.\n"++
                 "Please install a subversion client that supports the command-line 'svnversion'\ncommand.\n"
    ; return "??"
    }

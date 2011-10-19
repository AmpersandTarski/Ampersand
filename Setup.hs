import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription
import System.Time
import System.Process
import System.IO
import Data.List

main = defaultMainWithHooks (simpleUserHooks { buildHook = generateBuildInfoHook } )

-- Before each build, generate a BuildInfo_Generated module that exports the project version from cabal,
-- the current svn revision number and the build time.
--
-- Note that in order for this Setup.hs to be used by cabal, the build-type should be Custom.

generateBuildInfoHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
generateBuildInfoHook pd  lbi uh bf = 
 do { let version = intercalate "." (map show . versionBranch . pkgVersion . package $ pd)

    ; svnRevision <- do { r <- catch getSVNRevisionStr $ \err -> 
                                do { print err
                                   ; noSVNRevisionStr
                                   }
                        ; if r == "" 
                          then noSVNRevisionStr
                          else return r
                        }

    ; time <- getClockTime

    ; writeFile "src/DatabaseDesign/Ampersand_Prototype/BuildInfo_Generated.hs" $
        buildInfoModule version svnRevision (show time)

    ; (buildHook simpleUserHooks) pd lbi uh bf -- start the build
    }

buildInfoModule version revision time =
  "module DatabaseDesign.Ampersand_Prototype.BuildInfo_Generated (versionStr, svnRevisionStr, buildTimeStr) where\n" ++ 
  "\n" ++
  "-- This module is generated automatically by Setup.hs before building. Do not edit!\n" ++
  "\n" ++
  "versionStr = \"" ++ version ++ "\"\n" ++
  "\n" ++
  "svnRevisionStr = \"" ++ revision ++ "\"\n" ++
  "\n" ++
  "buildTimeStr = \"" ++ time ++ "\"\n"

getSVNRevisionStr = 
 do { (inh,outh,errh,proch) <- runInteractiveProcess "svnversion" ["."] Nothing Nothing
    ; hClose inh
    ; hClose errh
    ; version <- hGetContents outh
    ; seq version $ waitForProcess proch
    ; return (unwords . lines $ version)
    }

noSVNRevisionStr =
 do { putStrLn "\n\n\nWARNING: Execution of 'svnversion' command failed."
    ; putStrLn $ "BuildInfo_Generated.hs will not contain revision information, and therefore\nneither will fatal error messages.\n"++
                 "Please install a subversion client that supports the command-line 'svnversion'\ncommand.\n"
    ; return "??"
    }

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
 do { let cabalVersionStr = intercalate "." (map show . versionBranch . pkgVersion . package $ pd)

    ; svnRevisionStr <- do { r <- catch getSVNRevisionStr $ \err -> 
                                do { print err
                                   ; noSVNRevisionStr
                                   }
                        ; if r == "" 
                          then noSVNRevisionStr
                          else return r
                        }

    ; clockTime <- getClockTime
    ; calendarTime <- toCalendarTime clockTime
    ; let buildTimeStr = show (ctDay calendarTime) ++ "-" ++ take 3 (show  $ ctMonth calendarTime) ++ "-" ++ show (ctYear calendarTime `mod` 100) ++ " " ++
                         show (ctHour calendarTime) ++ ":" ++ showPadded (ctMin calendarTime) ++ "." ++ showPadded (ctSec calendarTime) 
    
    ; writeFile "src/DatabaseDesign/Ampersand_Prototype/BuildInfo_Generated.hs" $
        buildInfoModule cabalVersionStr svnRevisionStr buildTimeStr

    ; (buildHook simpleUserHooks) pd lbi uh bf -- start the build
    }
 where showPadded n = (if n<10 then "0" else "") ++ show n

buildInfoModule cabalVersion revision time =
  "module DatabaseDesign.Ampersand_Prototype.BuildInfo_Generated (cabalVersionStr, svnRevisionStr, buildTimeStr) where\n" ++ 
  "\n" ++
  "-- This module is generated automatically by Setup.hs before building. Do not edit!\n" ++
  "\n" ++
  "{-# NOINLINE cabalVersionStr #-}\n" ++ -- disable inlining to prevent recompilation of dependent modules on each build
  "cabalVersionStr = \"" ++ cabalVersion ++ "\"\n" ++
  "\n" ++
  "{-# NOINLINE svnRevisionStr #-}\n" ++
  "svnRevisionStr = \"" ++ revision ++ "\"\n" ++
  "\n" ++
  "{-# NOINLINE buildTimeStr #-}\n" ++
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

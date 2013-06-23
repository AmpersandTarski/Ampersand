{-# OPTIONS_GHC -Wall #-}  
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription
import System.Process
import System.IO
import Data.List
import System.Directory
import System.FilePath
import qualified Data.ByteString as BS
import Control.Exception
import Data.Time.Clock
import Data.Time.Calendar
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
    ; writeFile "src/DatabaseDesign/Ampersand_Prototype/BuildInfo_Generated.hs" $
        buildInfoModule cabalVersionStr svnRevisionStr buildTimeStr

    ; staticFilesGeneratedContents <- getStaticFilesModuleContents 
    ; writeFile (pathFromModule staticFileModuleName) staticFilesGeneratedContents 

    ; (buildHook simpleUserHooks) pd lbi uh bf -- start the build
    }
 where pathFromModule m = "src/" ++ [if c == '.' then '/' else c | c <- m] ++ ".hs"
       myHandler :: IOException -> IO String
       myHandler err = do { print err
                          ; noSVNRevisionStr
                          }

buildInfoModule :: String -> String -> String -> String
buildInfoModule cabalVersion revision time = unlines
  [ "module DatabaseDesign.Ampersand_Prototype.BuildInfo_Generated (cabalVersionStr, svnRevisionStr, buildTimeStr) where" 
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
  , ""
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


{- In order to handle static files, we generate a module StaticFiles_Generated.

   For each file in the directory trees static and staticBinary, we generate a StaticFile value,
   which contains the information needed to have Ampersand create the file at run-time.  
   
-}

staticFileModuleName :: String
staticFileModuleName = "DatabaseDesign.Ampersand_Prototype.StaticFiles_Generated"

getStaticFilesModuleContents :: IO String
getStaticFilesModuleContents =
 do { staticFiles       <- readStaticFiles False "static" ""
    ; staticFilesBinary <- readStaticFiles True "staticBinary" ""
    ; return $ "module "++staticFileModuleName++" where\n"++
               "\n"++
               "import Data.Time.Calendar\n"++
               "import Data.Time.Clock\n"++
               "\n"++
               "data StaticFile = SF { filePath ::      FilePath -- relative path, including extension\n"++
               "                     , timeStamp ::     UTCTime\n"++
               "                     , isBinary ::      Bool\n"++
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
           ; return ["SF "++show fileOrDir++" ("++showTimestamp timeStamp++") {- "++show timeStamp++" -} "++
                            show isBin++" "++show fileContents]
           }
     }
  where showTimestamp :: UTCTime -> String
        showTimestamp ts = "UTCTime (ModifiedJulianDay "++(show.toModifiedJulianDay.utctDay) ts++
                                 ") "++(removeS.show.utctDayTime) ts
        removeS :: String -> String
        removeS s = case reverse s of
                        's':theInt -> theInt
                        other -> other  
getProperDirectoryContents :: FilePath -> IO [String]
getProperDirectoryContents pth = fmap (filter (`notElem` [".","..",".svn"])) $ getDirectoryContents pth 
{-# OPTIONS_GHC -Wall #-}
-- | This module contains Version of Ampersand
module DatabaseDesign.Ampersand.Basics.Version (ampersandVersionStr, ampersandVersionWithoutBuildTimeStr, fatalMsg) where

import DatabaseDesign.Ampersand.Basics.BuildInfo_Generated

-- | a function to create error message in a structured way, containing the version of Ampersand. 
--   It throws an error, showing a (module)name and a number. This makes debugging pretty easy. 
fatalMsg :: String -> Int -> String -> a
fatalMsg haskellModuleName lineNr msg
 = error ("!fatal error "++show lineNr++" (module "++haskellModuleName++") "++ampersandVersionWithoutBuildTimeStr++"\n  "++
            let maxLen = 1500 -- This trick is to make sure the process is terminated after the error. 
                              -- If the string is too long, it seems that the sentinell `hangs`.
                              -- But what is too long???
            in case drop maxLen msg of
                [] -> msg
                _  -> take maxLen msg ++"\n<The rest of error message has been cut off.>"
           )

-- | String, containing the Ampersand version, including the build timestamp.
ampersandVersionStr :: String
ampersandVersionStr = ampersandVersionWithoutBuildTimeStr ++", build time: "++buildTimeStr

-- | String, containing the Ampersand version
ampersandVersionWithoutBuildTimeStr :: String
ampersandVersionWithoutBuildTimeStr = "Ampersand v"++cabalVersionStr++"."++svnRevisionStr
{- 
   #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); 
   #4 SVN revision number: 
      - may be a range separated by ':' if the working copy contains mixed revisions (e.g. "163:165")
      - ends with an 'M' if the working copy was modified (e.g. "163M")
      - for other (rare) values, see the output of the command 'svnversion --help' 
-}

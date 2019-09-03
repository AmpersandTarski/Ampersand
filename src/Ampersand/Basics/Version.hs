{-# LANGUAGE ImplicitParams #-}
-- | This module contains Version of Ampersand
module Ampersand.Basics.Version 
       ( ampersandVersionStr
       , ampersandVersionWithoutBuildTimeStr
       , fatal
       ) where
import Ampersand.Basics.BuildInfo_Generated
import Ampersand.Basics.Exit
import Ampersand.Basics.Prelude hiding (exitWith)
import GHC.Stack
maxLen :: Int
maxLen = 1500000 -- This trick is to make sure the process is terminated after the error.
                  -- If the string is too long, it seems that the sentinel `hangs`.
                  -- But what is too long???

-- | a function to create error message in a structured way, containing the version of Ampersand.
--   It throws an error, showing a (module)name and a number. This makes debugging pretty easy.
fatal :: (HasCallStack) => String -> a
fatal msg
 = exitWith . Fatal . lines $
        ("!             "++ampersandVersionWithoutBuildTimeStr++"\n"++
          lazyCutoff maxLen msg++"\n"++
          prettyCallStack callStack 
        )
 where lazyCutoff _   []   = ""
       lazyCutoff 0   _    = "\n<Ampersand's fatal-mechanism has removed the rest of this error message.>"
       lazyCutoff n (c:cs) = c: lazyCutoff (n-1) cs

{-# NOINLINE fatal #-}

-- | String, containing the Ampersand version, including the build timestamp.
ampersandVersionStr :: String
ampersandVersionStr = ampersandVersionWithoutBuildTimeStr ++", build time: "++buildTimeStr

-- | String, containing the Ampersand version. The part unto the first space is used as name of the release (appVeyor)
ampersandVersionWithoutBuildTimeStr :: String
ampersandVersionWithoutBuildTimeStr = "Ampersand-v"++cabalVersionStr++" ["++gitInfoStr++"]"
{-
   #1.#2.#3[$gitInfo] : #1 major version; #2 student release version; #3 production fix version (normally 0 );
   $gitInfo: "branch:SHA", followed by a '*' if the working copy was dirty: e.g. "master:0eea5e3*" 
-}

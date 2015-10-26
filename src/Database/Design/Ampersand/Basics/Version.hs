{-# LANGUAGE ImplicitParams, CPP #-}
-- | This module contains Version of Ampersand
module Database.Design.Ampersand.Basics.Version (ampersandVersionStr, ampersandVersionWithoutBuildTimeStr, fatal) where
#if MIN_VERSION_base(4,8,1)
import GHC.Stack
import GHC.SrcLoc
#endif
import Database.Design.Ampersand.Basics.BuildInfo_Generated

maxLen :: Int
maxLen = 1500 -- This trick is to make sure the process is terminated after the error.
                  -- If the string is too long, it seems that the sentinel `hangs`.
                  -- But what is too long???

#if MIN_VERSION_base(4,8,1)
-- | a function to create error message in a structured way, containing the version of Ampersand.
--   It throws an error, showing a (module)name and a number. This makes debugging pretty easy.
fatal :: (?loc :: CallStack) => Int -> String -> a
fatal lineNr msg
 = error ("!"++ showCS (tail (getCallStack ?loc)) ++
          "             "++ampersandVersionWithoutBuildTimeStr++"\n"++
          "             error nr: "++show lineNr++"\n  "++
            case drop maxLen msg of
                [] -> msg
                _  -> take maxLen msg ++"\n<The rest of error message has been cut off.>"
           )
 where showCS (root:rest) = unlines (showCallSite root : map (indent . showCallSite) rest)
       showCS [] = "fatal without a call site (check Version.hs to add a call site)\n"
       indent l = "             " ++ l
       showCallSite (f, loc) = f ++ ", called at " ++ showSrcLoc loc
#else
fatal :: Int -> String -> a
fatal i msg
   = error ("!fatal "++show i++"\n  Please recompile Ampersand with ghc 7.10.2 or newer to find out where this error occured.\n  "++
            case drop maxLen msg of
                [] -> msg
                _  -> take maxLen msg ++"\n<The rest of error message has been cut off.>")
#endif

-- | String, containing the Ampersand version, including the build timestamp.
ampersandVersionStr :: String
ampersandVersionStr = ampersandVersionWithoutBuildTimeStr ++", build time: "++buildTimeStr

-- | String, containing the Ampersand version
ampersandVersionWithoutBuildTimeStr :: String
ampersandVersionWithoutBuildTimeStr = "Ampersand v"++cabalVersionStr++"["++gitInfoStr++"]"
{-
   #1.#2.#3[$gitInfo] : #1 major version; #2 student release version; #3 production fix version (normally 0 );
   $gitInfo: "branch:SHA", followed by a '*' if the working copy was dirty: e.g. "master:0eea5e3*" 
-}

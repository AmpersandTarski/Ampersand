{-# OPTIONS_GHC -Wall #-}
module Database.Design.Ampersand_Prototype.Version (prototypeVersionStr, fatalMsg) where

import Database.Design.Ampersand_Prototype.BuildInfo_Generated
import Database.Design.Ampersand_Prototype.CoreImporter (ampersandVersionStr, ampersandVersionWithoutBuildTimeStr)

fatalMsg :: String -> Int -> String -> a
fatalMsg haskellModuleName lineNr msg
 = error ("!fatal "++show lineNr++" (module "++haskellModuleName++", "++prototypeVersionWithoutBuildtimeStr++")\n  "++msg)
 -- Please do not print anything in between "!fatal " and the line number that follows. So do not turn this into "!fatal error " for example.
 -- Why? The error message says for instance "fatal 384", which can be ^C'ed and used for searching within the editor.

prototypeVersionStr :: String
prototypeVersionStr = prototypeOnlyVersionStr++", build time: "++buildTimeStr++ " (lib: "++ampersandVersionStr++")"

prototypeVersionWithoutBuildtimeStr :: String
prototypeVersionWithoutBuildtimeStr = prototypeOnlyVersionStr ++ " (lib: "++ampersandVersionWithoutBuildTimeStr++")"

prototypeOnlyVersionStr :: String
prototypeOnlyVersionStr = "Prototype v"++cabalVersionStr++"["++gitInfoStr++"]"
{-
   #1.#2.#3[$gitInfo] : #1 major version; #2 student release version; #3 production fix version (normally 0 );
   $gitInfo: "branch:SHA", followed by a '*' if the working copy was dirty: e.g. "master:0eea5e3*" 
-}

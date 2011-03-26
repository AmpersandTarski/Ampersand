{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand_Prototype.Version
  (ampersandPrototypeVersionBanner, versionNumberPrototype, fatalMsg)
where
import DatabaseDesign.Ampersand (versionNumber)
ampersandPrototypeVersionBanner :: String
ampersandPrototypeVersionBanner = "Prototype vs. "++versionNumberPrototype++ "(core vs. "++versionNumber++")"

fatalMsg :: String -> Int -> String -> String
fatalMsg haskellModuleName lineNr msg
 = "!fatal "++show lineNr++" (module "++haskellModuleName++", "++ampersandPrototypeVersionBanner++")\n  "++msg

versionNumberPrototype :: String
versionNumberPrototype = "2.0.1.909" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
TICKET #52: fixed
This version is compilable with Ampersand vs. 2.0.1.37
-}

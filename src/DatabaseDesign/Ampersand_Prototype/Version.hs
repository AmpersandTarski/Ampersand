{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand_Prototype.Version
  (ampersandPrototypeVersionBanner, versionNumberPrototype, fatalMsg)
where
import DatabaseDesign.Ampersand_Prototype.CoreImporter (versionNumber)
ampersandPrototypeVersionBanner :: String
ampersandPrototypeVersionBanner = "Prototype vs. "++versionNumberPrototype++ "(core vs. "++versionNumber++")"

fatalMsg :: String -> Int -> String -> a
fatalMsg haskellModuleName lineNr msg
 = error ("!fatal "++show lineNr++" (module "++haskellModuleName++", "++ampersandPrototypeVersionBanner++")\n  "++msg)

versionNumberPrototype :: String
versionNumberPrototype = "2.0.1.938" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
Ticket #104:
Han and I are busy transforming the code to a separated A-structure and P-structure (ticket #85).
In order to collaborate on this issue, the code I am checking in is code in progress.
It is NOT compilable. The coming days will show more of such check-ins.
This ticket will be finalized once the code compiles again. This goes for the prototype as well as the core Ampersand repositories.
Please use earlier releases or wait for a while to check out compilable code.
-}

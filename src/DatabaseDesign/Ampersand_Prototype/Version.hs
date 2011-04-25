{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand_Prototype.Version
  (ampersandPrototypeVersionBanner, versionNumberPrototype, fatalMsg)
where
import DatabaseDesign.Ampersand (versionNumber)
ampersandPrototypeVersionBanner :: String
ampersandPrototypeVersionBanner = "Prototype vs. "++versionNumberPrototype++ "(core vs. "++versionNumber++")"

fatalMsg :: String -> Int -> String -> a
fatalMsg haskellModuleName lineNr msg
 = error ("!fatal "++show lineNr++" (module "++haskellModuleName++", "++ampersandPrototypeVersionBanner++")\n  "++msg)

versionNumberPrototype :: String
versionNumberPrototype = "2.0.1.918" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
Fixed ticket #63: The FilePos data structure has been split.
I have also made a new structure for Context.
Context has been split into P_context and A_context.
A P_context is the parse tree.
The A_context is the enriched parse tree, with which all other things work.
This affects both core and prototype.
This version is compilable with Ampersand vs. 2.1.0.54
-}

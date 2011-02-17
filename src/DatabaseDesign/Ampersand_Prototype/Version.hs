{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand_Prototype.Version
  (ampersandPrototypeVersionBanner,versionNumberPrototype)
where
import DatabaseDesign.Ampersand (versionNumber)
ampersandPrototypeVersionBanner :: String
ampersandPrototypeVersionBanner = "Prototype vs. "++versionNumberPrototype++ "(core vs. "++versionNumber++")"

versionNumberPrototype :: String
versionNumberPrototype = "1.1.0.874" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
Slight restructuring of class Explainable caused minor changes in the prototype generator.
Ticket 11 set to 'fixed'.
-}

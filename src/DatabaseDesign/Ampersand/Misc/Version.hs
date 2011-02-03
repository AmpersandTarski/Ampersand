{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Misc.Version (ampersandCoreVersionBanner,versionNumber)
where

ampersandCoreVersionBanner :: String
ampersandCoreVersionBanner = "Ampersand vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.1.0.856" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
enriching and type checking relations in RoleRelation, svParams, pExplanation
-}

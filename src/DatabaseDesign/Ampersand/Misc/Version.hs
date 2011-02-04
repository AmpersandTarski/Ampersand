{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Misc.Version (ampersandCoreVersionBanner,versionNumber)
where

ampersandCoreVersionBanner :: String
ampersandCoreVersionBanner = "Ampersand vs. "++versionNumber 

versionNumber :: String
versionNumber = "1.2.0.864" -- #1.#2.#3.#4 : #1 major version; #2 student release version; #3 production fix version (normally 0 ); #4 SVN revision number.
{-
SVN Version text:
ready to implement populating atlas.adl with any.adl

-}

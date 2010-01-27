{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-562" -- the number behind the last digit is the SVN revision number.

{-
SVN Version text:
Duidelijker onderscheid gemaakt tussen de URL naar het .png bestand en het bestand waar een navigeerbaar object staat. @Gerard: Kan jij de juiste URLs plaatsen in de class Navigatable? Die moeten wijzen naar een pagina in de atlas waar het desbetreffende object centraal staat.
-}

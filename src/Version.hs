{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-462" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
Aanpassingen aan Fspc:
 - de ECA-regels zijn opgenomen in Fspc
 - de showHS voldoet aan de eis dat de gegenereerde output (het Haskell bestand) foutloos door de haskell-compiler komt
 - normalisatie van PAclauses toegevoegd en getest
 - warnings geschoond
 - Fatal-foutmeldingen enigszins geuniformeerd
-}

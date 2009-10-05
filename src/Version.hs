{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-433" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
Opties opgeschoond. De help is nu beter leesbaar. De opties --version en --help geven nu geen fout meer, maar werken zoals je mag verwachten. De 'hidden' opties komen tevoorschijn al de gebruiker --help tegelijk met --verbose vraagt. 
Daarnaast heb ik ook enkele testspeeltjes voor HaXml aangebracht. 
-}

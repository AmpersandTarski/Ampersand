{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-540" -- the number behind the last digit is the SVN revision number.

{-
SVN Version text:
Jawel, ik heb het nu voor elkaar dat pdfLatex ook werkt met output in een andere directory. 
pdfLatex wordt zo vaak aangeroepen als nodig, en niet vaker. Echter gemaximaliseerd op 5, om ongelukken te voorkomen.
Dit heeft me wel wat ervaring met Monads opgeleverd 8-))
-}

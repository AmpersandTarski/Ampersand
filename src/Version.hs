{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-269" -- the number behind the last digit is the SVN revision number.

{- SVN Version text:
De loop die nog steeds aanwezig is heb ik herleid naar ergens in ADL2Fspec. Daar is Stef nog mee bezig, voor zover ik weet. 
Ik heb in ADL2Fspec wat warnings verwijderd, en loop tegen dingen aan die ik niet begrijp. 
ShowHS aangepast, zodat die nu de Fspc goed kan tonen, op [Themes] na, maar dat moet toch anders.
-}
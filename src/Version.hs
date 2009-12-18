{-# OPTIONS_GHC -Wall #-}
module Version (versionbanner)
where

versionbanner :: String
versionbanner = "ADL vs. 0.8.10-501" -- the number behind the last digit is the SVN revision number.

{-
SVN Version text:
Hoera! 500 bereikt, schreef Stef.
Maar verder lette hij even niet op!
Versie 500 kost je taart, Stef...
Een Declaration is nou eenmaal geen Morphism....:

[71 of 73] Compiling Fspec2Pandoc     ( Fspec2Pandoc.hs, C:\data\eclipseworkspace\ampersand\out\Fspec2Pandoc.o )

Fspec2Pandoc.hs:510:74:
    Couldn't match expected type `Morphism'
           against inferred type `Declaration'
    In the first argument of `isProperty', namely `d'
    In the first argument of `not', namely `(isProperty d)'
    In the expression: not (isProperty d)
-}

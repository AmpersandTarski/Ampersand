{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS -XTypeSynonymInstances #-}
module ShowXMLhaxml (showXML)
where
   import Text.XML.HaXml
--     --Als de compiler hierover struikelt, dan moet je xml installeren. Dat is overigens in de volgende 3 stappen:
--                             -- 1) Eerst installeer je Cabal (zie http://www.haskell.org/cabal/) en dan roep je op je command line: 
--                             -- 2) cabal-install HaXml  (onder windows: cabal install HaXml)
--                             -- 3) er is geen stap 3!
--                             
--     -- Motivatie voor keuze van XML Light. (HJO, 6 feb 2009)
--     -- Oorspronkelijk heb ik gemeend om de Haskell XML Toolbox (hxt) te gebruiken. Die is uitgebreid en 
--     --  maakt het mogelijk om in de toekomst allerlei leuke dingen te gaan doen met de gegenereerde XML. 
--     -- Helaas was het niet mogelijk om dit te gebruiken: Onder Windows kreeg ik het niet aan de praat, omdat
--     --  het gebruik maakt van packages die nog niet standaard beschikbaar zijn.
--     -- Onder debian kreeg ik het ook niet voor elkaar met de standaard installatie manier, omdat in de standaard
--     -- distributie voor debian nog gebruik wordt gemaakt van ghc 6.6 
--     -- Wie weet komt dit later dus nog wel een keer....


-- TODO: Als het ADL bestand strings bevat met speciale characters als '&' en '"', dan wordt nu nog foute XML-code gegenereerd...

   import CommonClasses(Identified(..))
   import Auxiliaries(showL)
   import Strings(chain)
   import Data.Fspec
   import Adl
   import ShowADL
   import Time(ClockTime)
   import Version(versionbanner)
   import Data.Plug

   showXML :: Fspc -> ClockTime -> Document i
   showXML fSpec now = Document prolog -- Prolog
                                (buildsymtab  -- (SymTab EntityDef)
                                elm    -- (Element i)
                                []     -- [Misc]	
      where prolog = Prolog (Just xmlDecl) [] Nothing []
            where xmlDecl :: XMLDecl
                  xmlDecl = XMLDecl "1.0"   -- VersionInfo
                                    Nothing -- (Maybe EncodingDecl)
                                    Nothing -- (Maybe SDDecl)	

{-# OPTIONS_GHC -Wall #-}
module Adl.Gen (Gen(..),Gens)
where
   import Adl.FilePos   (FilePos)
   import Adl.Concept   (Concept,Association(..))
   
   type Gens      = [Gen]
   data Gen       = G { genfp  :: FilePos         -- ^ the position of the GEN-rule
                      , gengen :: Concept         -- ^ generic concept
                      , genspc :: Concept         -- ^ specific concept
                      , genpat :: String          -- ^ pattern of declaration
                      }

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Gen                           ***
-- \***********************************************************************
   instance Eq Gen where
       g == g' = gengen g == gengen g' &&
                 genspc g == genspc g'

   instance Show Gen where
    -- This show is used in error messages. It should therefore not display the term's type
    showsPrec _ g = showString ("GEN "++show (genspc g)++" ISA "++show (gengen g))
   
                      
   instance Association Gen where
    source g = genspc g
    target g = gengen g

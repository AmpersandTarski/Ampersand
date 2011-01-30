{-# OPTIONS_GHC -Wall -XFlexibleInstances -XMultiParamTypeClasses #-}
module DatabaseDesign.Ampersand.ADL1.Gen (Gen(..),Gens)
where
   import DatabaseDesign.Ampersand.Input.ADL1.FilePos                  (FilePos)
   import DatabaseDesign.Ampersand.ADL1.MorphismAndDeclaration   (Association(..))
   
   type Gens c      = [Gen c]
   data Gen concept = G { genfp  :: FilePos         -- ^ the position of the GEN-rule
                        , gengen :: concept         -- ^ generic concept
                        , genspc :: concept         -- ^ specific concept
                        , genpat :: String          -- ^ pattern of declaration
                        }

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Gen                           ***
-- \***********************************************************************
   instance Eq c => Eq (Gen c) where
       g == g' = gengen g == gengen g' &&
                 genspc g == genspc g'

   instance Show c => Show (Gen c) where
    -- This show is used in error messages. It should therefore not display the term's type
    showsPrec _ g = showString ("GEN "++show (genspc g)++" ISA "++show (gengen g))
   
                      
   instance Eq c => Association (Gen c) c where
    source g = genspc g
    target g = gengen g

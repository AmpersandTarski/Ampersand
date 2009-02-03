
module Adl.Gen where
   import Adl.FilePos
   import Adl.Concept
   
   type Gens      = [Gen]
   data Gen       = G { genfp  :: FilePos          -- ^ the position of the GEN-rule
                      , gengen :: Concept          -- ^ generic concept
                      , genspc :: Concept          -- ^ specific concept
                      }deriving (Eq)

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Gen                           ***
-- \***********************************************************************
--   instance Eq Gen where -- WAAROM :TODO Stef, deze Eq mistte zijn where clause. Wil jij dit valideren? 
--       g == g' = gengen g == gengen g' &&
--                 genspc g == genspc g'
                 
   instance Show Gen where
    -- This show is used in error messages. It should therefore not display the term's type
    showsPrec p (G pos g s) = showString ("GEN "++show s++" ISA "++show g)
   
                      
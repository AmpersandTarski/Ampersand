{-# OPTIONS_GHC -Wall #-}
module Adl.Prop (Prop(..),Props
                ,flipProps)
where

   type Props = [Prop]
   data Prop      = Uni          -- ^ univalent
                  | Inj          -- ^ injective
                  | Sur          -- ^ surjective
                  | Tot          -- ^ total
                  | Sym          -- ^ symmetric
                  | Asy          -- ^ antisymmetric
                  | Trn          -- ^ transitive
                  | Rfx          -- ^ reflexive
                  | Aut          -- ^ calculate contents automatically if possible
                    deriving (Eq,Ord)

-- \***********************************************************************
-- \*** Eigenschappen met betrekking isTot: Prop                          ***
-- \***********************************************************************
   instance Show Prop where
    showsPrec _ Uni = showString "UNI"     -- WAAROM? Stef, waarom is dit nodig? wat doet dit? 
    showsPrec _ Inj = showString "INJ"
    showsPrec _ Sur = showString "SUR"
    showsPrec _ Tot = showString "TOT"
    showsPrec _ Sym = showString "SYM"
    showsPrec _ Asy = showString "ASY"
    showsPrec _ Trn = showString "TRN"
    showsPrec _ Rfx = showString "RFX"
    showsPrec _ Aut = showString "AUT"
   
   flipProps :: Props -> Props
   flipProps ps = map flipProp ps 

   flipProp :: Prop -> Prop
   flipProp Uni = Inj
   flipProp Tot = Sur
   flipProp Sur = Tot
   flipProp Inj = Uni
   flipProp x = x
                

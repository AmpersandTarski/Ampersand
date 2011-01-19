{-# OPTIONS_GHC -Wall #-}
module ADL.Prop (Prop(..),Props
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
                    deriving (Eq,Ord)

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Prop                          ***
-- \***********************************************************************
   instance Show Prop where
    showsPrec _ Uni = showString "UNI"
    showsPrec _ Inj = showString "INJ"
    showsPrec _ Sur = showString "SUR"
    showsPrec _ Tot = showString "TOT"
    showsPrec _ Sym = showString "SYM"
    showsPrec _ Asy = showString "ASY"
    showsPrec _ Trn = showString "TRN"
    showsPrec _ Rfx = showString "RFX"
   
   flipProps :: Props -> Props
   flipProps ps = map flipProp ps 

   flipProp :: Prop -> Prop
   flipProp Uni = Inj
   flipProp Tot = Sur
   flipProp Sur = Tot
   flipProp Inj = Uni
   flipProp x = x

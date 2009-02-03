
module Adl.Prop where

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
-- \*** Eigenschappen met betrekking tot: Prop                          ***
-- \***********************************************************************
   instance Show Prop where
    showsPrec p Uni = showString "UNI"
    showsPrec p Inj = showString "INJ"
    showsPrec p Sur = showString "SUR"
    showsPrec p Tot = showString "TOT"
    showsPrec p Sym = showString "SYM"
    showsPrec p Asy = showString "ASY"
    showsPrec p Trn = showString "TRN"
    showsPrec p Rfx = showString "RFX"
    showsPrec p Aut = showString "AUT"
   
   flipProps :: [Prop] -> [Prop]
   flipProps ps = [flipProp p| p<-ps]

   flipProp :: Prop -> Prop
   flipProp Uni = Inj
   flipProp Tot = Sur
   flipProp Sur = Tot
   flipProp Inj = Uni
   flipProp x = x

                 
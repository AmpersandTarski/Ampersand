{-# OPTIONS_GHC -Wall -XFlexibleInstances -XMultiParamTypeClasses -XUndecidableInstances #-}
module DatabaseDesign.Ampersand.ADL1.Population (Population(..),Populations) 
where
   import DatabaseDesign.Ampersand.ADL1.MorphismAndDeclaration   (Relation(..), Association(..))
   import DatabaseDesign.Ampersand.ADL1.Pair                     (Pairs)
   
   type Populations concept = [Population concept]
   data Population concept = Popu
              { popm  :: Relation concept
              , popps :: Pairs
              }

   instance Eq c => Association (Population c) c where
     source pop = source (popm pop)
     target pop = target (popm pop)

 

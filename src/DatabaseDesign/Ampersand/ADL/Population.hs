{-# OPTIONS_GHC -Wall -XFlexibleInstances -XMultiParamTypeClasses -XUndecidableInstances #-}
module DatabaseDesign.Ampersand.ADL.Population (Population(..),Populations) 
where
   import DatabaseDesign.Ampersand.ADL.MorphismAndDeclaration   (Relation(..), Association(..))
   import DatabaseDesign.Ampersand.ADL.Pair                     (Pairs)
   
   type Populations concept = [Population concept]
   data Population concept = Popu
              { popm  :: Relation concept
              , popps :: Pairs
              }

   instance Eq c => Association (Population c) c where
     source pop = mphsrc (popm pop)
     target pop = mphtrg (popm pop)

 

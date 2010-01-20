{-# OPTIONS_GHC -Wall #-}
module Adl.Population (Population(..),Populations) 
where
   import Adl.MorphismAndDeclaration   (Morphism())
   import CommonClasses                (Identified(..))
   import Adl.Pair                     (Pairs)
   import Adl.Concept                  (Association(..))
   
   type Populations = [Population]
   data Population = Popu
              { popm  :: Morphism
              , popps :: Pairs
              }

   instance Association Population where
     source pop = source (popm pop)
     target pop = target (popm pop)

 

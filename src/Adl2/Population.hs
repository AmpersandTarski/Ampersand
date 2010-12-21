
module Adl2.Population (Population(..),Populations) 
where
   import Adl2.Morphism   (Morphism())
   import Adl.Pair       (Pairs)
   import Adl2.Concept    (Association(..))
   
   data Population r = Popu
              { popm  :: Morphism r
              , popps :: Pairs
              }

   instance Association (Population r) where
     source pop = source (popm pop)
     target pop = target (popm pop)

 

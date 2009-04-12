{-# OPTIONS_GHC -Wall #-}
module Adl.Population (Population(..),Populations) 
where
   import Adl.MorphismAndDeclaration   (Morphism)
   import Adl.Pair                     (Pairs)
   
   type Populations = [Population]
   data Population = Popu
              { popm  :: Morphism
              , popps :: Pairs
              }

 
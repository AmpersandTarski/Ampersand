{-# OPTIONS_GHC -Wall #-}
module Adl.Population where
   import Adl.MorphismAndDeclaration
   import Adl.Pair
   
   type Populations = [Population]
   data Population = Popu
              { popm  :: Morphism
              , popps :: Pairs
              }         --deriving (Show) -- voor debugging

 
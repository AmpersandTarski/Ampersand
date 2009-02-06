
module Adl.Population where
   import Adl.MorphismAndDeclaration
   import Adl.Pair
   import Adl.Expression
   import Adl.Concept
   
   type Populations = [Population]
   data Population = Popu
              { popm  :: Morphism
              , popps :: Pairs
              }         --deriving (Show) -- voor debugging

 
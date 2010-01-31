{-# OPTIONS_GHC -Wall #-}
module Adl.Explanation (Explanation(..))
where
   import Languages                    (Lang)
   import Adl.MorphismAndDeclaration   (Morphism)
   

--                       Constructor     ObjRef        RefID  Explanation
   data Explanation    = ExplConcept     String   Lang String String
                       | ExplDeclaration Morphism Lang String String
                       | ExplRule        String   Lang String String
                       | ExplKeyDef      String   Lang String String
                       | ExplObjectDef   String   Lang String String
                       | ExplPattern     String   Lang String String
                       | ExplPopulation  Morphism Lang String String
                       | ExplSQLPlug     String   Lang String String
                       | ExplPHPPlug     String   Lang String String

            
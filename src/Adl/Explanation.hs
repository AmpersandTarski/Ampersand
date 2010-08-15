{-# OPTIONS_GHC -Wall #-}
module Adl.Explanation (Explanation(..),PExplanation(..),Explanations,PExplanations)
where
   import Languages                    (Lang)
   import Adl.MorphismAndDeclaration   (Morphism,Declaration)
   import Adl.ConceptDef               (ConceptDef)
   import Adl.Rule                     (Rule)
   import Adl.KeyDef                   (KeyDef)
   import Adl.ObjectDef                (ObjectDef)
   import CommonClasses                (Identified(..))
   import Data.Explain
   
-- PExplanation is a parse-time constructor. It contains the name of the object it explains.
-- It is a pre-explanation in the sense that it contains a reference to something that is not yet built by the compiler.
--                       Constructor      name          RefID  Explanation
   data PExplanation   = PExplConcept     String   Lang String String
                       | PExplDeclaration Morphism Lang String String
                       | PExplRule        String   Lang String String
                       | PExplKeyDef      String   Lang String String
                       | PExplObjectDef   String   Lang String String
                       | PExplPattern     String   Lang String String

   instance Identified PExplanation where
    name (PExplConcept nm _ _ _) = nm
    name (PExplDeclaration mph _ _ _) = name mph
    name (PExplRule nm _ _ _) = nm
    name (PExplKeyDef nm _ _ _) = nm
    name (PExplObjectDef nm _ _ _) = nm
    name (PExplPattern nm _ _ _) = nm

-- Explanation is the intended constructor. It contains the object it explains.
-- The enrichment process of the parser must map the names (from PExplanation) to the actual objects
--                       Constructor     Object          RefID  Explanation
   data Explanation    = ExplConcept{explObjCD  :: ConceptDef      --The object that is explained.
                                    ,explLang :: Lang            --The language of the explaination
                                    ,explRefId :: String         --The reference of the explaination
                                    ,explCont  :: ExplainContent -- The actual explanaition.
                                    }
                       | ExplDeclaration {explObjD :: Declaration  --The object that is explained.
                                    ,explLang :: Lang            --The language of the explaination
                                    ,explRefId :: String         --The reference of the explaination
                                    ,explCont  :: ExplainContent -- The actual explanaition.
                                    }
                       | ExplRule   {explObjR   :: Rule         --The object that is explained.
                                    ,explLang  :: Lang            --The language of the explaination
                                    ,explRefId :: String         --The reference of the explaination
                                    ,explCont  :: ExplainContent -- The actual explanaition.
                                    }
                       | ExplKeyDef {explObjKD ::     KeyDef       --The object that is explained.
                                    ,explLang :: Lang            --The language of the explaination
                                    ,explRefId :: String         --The reference of the explaination
                                    ,explCont  :: ExplainContent -- The actual explanaition.
                                    }
                       | ExplObjectDef{explObjOD ::   ObjectDef    --The object that is explained.
                                    ,explLang :: Lang            --The language of the explaination
                                    ,explRefId :: String         --The reference of the explaination
                                    ,explCont  :: ExplainContent -- The actual explanaition.
                                    }
                       | ExplPattern {explObjP ::    String        -- SJ: To avoid a compile time loop, the name of the pattern is used rather than the entire pattern. Hence, for patterns the PExplPattern is identical to the ExplPattern
                                    ,explLang :: Lang            --The language of the explaination
                                    ,explRefId :: String         --The reference of the explaination
                                    ,explCont  :: ExplainContent -- The actual explanaition.
                                    }
   type Explanations  = [Explanation]
   type PExplanations = [PExplanation]

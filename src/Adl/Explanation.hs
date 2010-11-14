{-# OPTIONS_GHC -Wall #-}
module Adl.Explanation (Explanation(..),PExplanation(..),PExplObj(..),Explanations,PExplanations,ExplObj(..))
where
   import Languages                    (Lang)
   import Adl.MorphismAndDeclaration   (Morphism,Declaration)
   import Adl.Concept                  (Association(..))
   import Adl.ConceptDef               (ConceptDef)
   import Adl.Rule                     (Rule)
   import Adl.KeyDef                   (KeyDef)
   import Adl.ObjectDef                (ObjectDef)
   import CommonClasses                (Identified(..))
   import Data.Explain

-- PExplanation is a parse-time constructor. It contains the name of the object it explains.
-- It is a pre-explanation in the sense that it contains a reference to something that is not yet built by the compiler.
--                       Constructor      name          RefID  Explanation
   data PExplObj = PExplConceptDef String
                 | PExplDeclaration Morphism
                 | PExplRule String
                 | PExplKeyDef String
                 | PExplObjectDef String
                 | PExplPattern String
                 | PExplContext String
   instance Identified PExplObj where
     name pe = case pe of 
        PExplConceptDef str -> str
        PExplDeclaration mph -> name mph
        PExplRule str -> str
        PExplKeyDef str -> str
        PExplObjectDef str -> str
        PExplPattern str -> str
        PExplContext str -> str
        
   data PExplanation = PExpl {pexObj  :: PExplObj
                             ,pexLang :: Lang
                             ,pexRefID:: String
                             ,pexExpl :: String
                             }

   instance Identified PExplanation where
    name pe = name (pexObj pe)

   data ExplObj = ExplConceptDef ConceptDef
                | ExplDeclaration Declaration
                | ExplRule Rule
                | ExplKeyDef KeyDef
                | ExplObjectDef ObjectDef
                | ExplPattern String   -- SJ: To avoid a compile time loop, the name of the pattern is used rather than the entire pattern. Hence, for patterns the PExplPattern is identical to the ExplPattern
                | ExplContext String   -- SJ: To avoid a compile time loop, the name of the context is used rather than the entire context. Hence, for contexts the PExplContext is identical to the ExplContext
                  deriving Show        --handy for XML creation
                  
   instance Identified ExplObj where    -- Not really the identifier, but the name of the object it references...
    name e = case e of
       ExplConceptDef cd -> name cd
       ExplDeclaration d -> name d ++name (source d)++name (target d)
       ExplRule r        -> name r
       ExplKeyDef kd     -> name kd
       ExplObjectDef od  -> name od
       ExplPattern str   -> str
       ExplContext str   -> str


-- Explanation is the intended constructor. It contains the object it explains.
-- The enrichment process of the parser must map the names (from PExplanation) to the actual objects
   data Explanation  = Expl {explObj   :: ExplObj        -- The object that is explained.
                            ,explLang  :: Lang           -- The language of the explaination
                            ,explRefId :: String         -- The reference of the explaination
                            ,explCont  :: ExplainContent -- The actual explanaition.
                            }deriving Show  --handy for XML creation

   instance Identified Explanation where    -- Not really the identifier, but the name of the object it references...
    name e = name (explObj e)
 
   type Explanations  = [Explanation]
   type PExplanations = [PExplanation]
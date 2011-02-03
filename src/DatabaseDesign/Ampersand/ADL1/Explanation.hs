{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.ADL1.Explanation (Explanation(..),PExplanation(..),PExplObj(..),Explanations,PExplanations,ExplObj(..))
where
   import DatabaseDesign.Ampersand.Basics                        (Identified(..))
   import DatabaseDesign.Ampersand.Misc                          (Lang)
   import DatabaseDesign.Ampersand.ADL1.MorphismAndDeclaration   (Relation,Declaration,Association(..))
   import DatabaseDesign.Ampersand.ADL1.Concept                  (Concept(..))
   import DatabaseDesign.Ampersand.ADL1.ConceptDef               (ConceptDef)
   import DatabaseDesign.Ampersand.ADL1.Rule                     (Rule)
   import DatabaseDesign.Ampersand.ADL1.KeyDef                   (KeyDef)
   import DatabaseDesign.Ampersand.Misc.Explain
   import DatabaseDesign.Ampersand.Input.ADL1.FilePos            (FilePos)

-- PExplanation is a parse-time constructor. It contains the name of the object it explains.
-- It is a pre-explanation in the sense that it contains a reference to something that is not yet built by the compiler.
--                       Constructor      name          RefID  Explanation
   data PExplObj = PExplConceptDef String
                 | PExplDeclaration (Relation Concept)
                 | PExplRule String
                 | PExplKeyDef String
                 | PExplPattern String
                 | PExplService String
                 | PExplContext String
                 deriving Show -- only for fatal error messages

   instance Identified PExplObj where
     name pe = case pe of 
        PExplConceptDef str -> str
        PExplDeclaration rel -> name rel
        PExplRule str -> str
        PExplKeyDef str -> str
        PExplPattern str -> str
        PExplService str -> str
        PExplContext str -> str

   data PExplanation = PExpl { pexPos   :: FilePos   -- the position in the Ampersand script of this purpose definition
                             , pexObj   :: PExplObj  -- the object whose purpose is explained
                             , pexLang  :: Lang      -- the natural language in which the text is stated
                             , pexRefID :: String    -- the reference (for traceability)
                             , pexExpl  :: String    -- the text of this purpose definition
                             }

--   instance Numbered PExplanation where
--    pos = pexPos

   instance Identified PExplanation where
    name pe = name (pexObj pe)

   data ExplObj = ExplConceptDef ConceptDef
                | ExplDeclaration (Declaration Concept)
                | ExplRule (Rule (Relation Concept))
                | ExplKeyDef KeyDef
                | ExplPattern String   -- SJ: To avoid a compile time loop, the name of the pattern is used rather than the entire pattern. Hence, for patterns the PExplPattern is identical to the ExplPattern
                | ExplService String   -- SJ: To avoid a compile time loop, the name of the service is used rather than the entire service. Hence, for services the PExplService is identical to the ExplService
                | ExplContext String   -- SJ: To avoid a compile time loop, the name of the context is used rather than the entire context. Hence, for contexts the PExplContext is identical to the ExplContext
                  deriving Show        --handy for XML creation
                  
   instance Identified ExplObj where    -- Not really the identifier, but the name of the object it references...
    name e = case e of
       ExplConceptDef cd -> name cd
       ExplDeclaration d -> name d ++name (source d)++name (target d)
       ExplRule r        -> name r
       ExplKeyDef kd     -> name kd
       ExplPattern str   -> str
       ExplService str   -> str
       ExplContext str   -> str


-- Explanation is the intended constructor. It contains the object it explains.
-- The enrichment process of the parser must map the names (from PExplanation) to the actual objects
   data Explanation  = Expl {explObj   :: ExplObj        -- The object that is explained.
                            ,explLang  :: Lang           -- The language of the explaination
                            ,explRefId :: String         -- The reference of the explaination
                            ,explCont  :: ExplainContent -- The actual explanation.
                            }deriving Show  --handy for XML creation

   instance Identified Explanation where    -- Not really the identifier, but the name of the object it references...
    name e = name (explObj e)
 
   type Explanations  = [Explanation]
   type PExplanations = [PExplanation]
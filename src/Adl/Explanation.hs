{-# OPTIONS_GHC -Wall #-}
module Adl.Explanation (Explanation(..),PExplanation(..),Explanations,PExplanations)
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
   data PExplanation   = PExplConcept     {pexName :: String
                                          ,pexLang :: Lang
                                          ,pexRefID:: String
                                          ,pexExpl :: String
                                          }
                       | PExplDeclaration {pedMph  :: Morphism
                                          ,pexLang :: Lang
                                          ,pexRefID:: String
                                          ,pexExpl :: String
                                          }
                       | PExplRule        {pexName :: String
                                          ,pexLang :: Lang
                                          ,pexRefID:: String
                                          ,pexExpl :: String
                                          }
                       | PExplKeyDef      {pexName :: String
                                          ,pexLang :: Lang
                                          ,pexRefID:: String
                                          ,pexExpl :: String
                                          }
                       | PExplObjectDef   {pexName :: String
                                          ,pexLang :: Lang
                                          ,pexRefID:: String
                                          ,pexExpl :: String
                                          }
                       | PExplPattern     {pexName :: String
                                          ,pexLang :: Lang
                                          ,pexRefID:: String
                                          ,pexExpl :: String
                                          }
                       | PExplContext     {pexName :: String
                                          ,pexLang :: Lang
                                          ,pexRefID:: String
                                          ,pexExpl :: String
                                          }

   instance Identified PExplanation where
    name pe = case pe of
       PExplDeclaration{} -> name (pedMph pe)
       _                  -> pexName pe

-- Explanation is the intended constructor. It contains the object it explains.
-- The enrichment process of the parser must map the names (from PExplanation) to the actual objects
--                       Constructor     Object          RefID  Explanation
   data Explanation    = ExplConcept{explObjCD :: ConceptDef     -- The object that is explained.  Han, WAAROM hebben alle explObj<X> een suffix <X>?
                                    ,explLang  :: Lang           -- The language of the explaination
                                    ,explRefId :: String         -- The reference of the explaination
                                    ,explCont  :: ExplainContent -- The actual explanaition.
                                    }
                       | ExplDeclaration
                                    {explObjD  :: Declaration    -- The object that is explained.
                                    ,explLang  :: Lang           -- The language of the explaination
                                    ,explRefId :: String         -- The reference of the explaination
                                    ,explCont  :: ExplainContent -- The actual explanaition.
                                    }
                       | ExplRule   {explObjR  :: Rule           -- The object that is explained.
                                    ,explLang  :: Lang           -- The language of the explaination
                                    ,explRefId :: String         -- The reference of the explaination
                                    ,explCont  :: ExplainContent -- The actual explanaition.
                                    }
                       | ExplKeyDef {explObjKD :: KeyDef         -- The object that is explained.
                                    ,explLang  :: Lang           -- The language of the explaination
                                    ,explRefId :: String         -- The reference of the explaination
                                    ,explCont  :: ExplainContent -- The actual explanaition.
                                    }
                       | ExplObjectDef
                                    {explObjOD :: ObjectDef      -- The object that is explained.
                                    ,explLang  :: Lang           -- The language of the explaination
                                    ,explRefId :: String         -- The reference of the explaination
                                    ,explCont  :: ExplainContent -- The actual explanaition.
                                    }
                       | ExplPattern{explObjP :: String          -- SJ: To avoid a compile time loop, the name of the pattern is used rather than the entire pattern. Hence, for patterns the PExplPattern is identical to the ExplPattern
                                    ,explLang  :: Lang           -- The language of the explaination
                                    ,explRefId :: String         -- The reference of the explaination
                                    ,explCont  :: ExplainContent -- The actual explanaition.
                                    }
                       | ExplContext{explObjC  :: String         -- SJ: To avoid a compile time loop, the name of the pattern is used rather than the entire pattern. Hence, for patterns the PExplPattern is identical to the ExplPattern
                                    ,explLang  :: Lang           -- The language of the explaination
                                    ,explRefId :: String         -- The reference of the explaination
                                    ,explCont  :: ExplainContent -- The actual explanaition.
                                    }deriving Show  --handy for XML creation

   instance Identified Explanation where    -- Not really the identifier, but the name of the object it references...
    name e = case e of
       ExplConcept{}     -> name (explObjCD e)
       ExplDeclaration{} -> name (explObjD e) ++name (source (explObjD e))++name (target(explObjD e))
       ExplRule{}        -> name (explObjR e)
       ExplKeyDef{}      -> name (explObjKD e)
       ExplObjectDef{}   -> name (explObjOD e)
       ExplPattern{}     ->      (explObjP e)
       ExplContext{}     ->      (explObjC e)
 
 
 
   type Explanations  = [Explanation]
   type PExplanations = [PExplanation]

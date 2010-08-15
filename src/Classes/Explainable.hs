{-# OPTIONS_GHC -Wall #-}
module Classes.Explainable (UserExplainable(..)) 
where
  import Adl.Explanation             (Explanation(..),PExplanation(..))
  import Adl.Context                 (Context(..))
  import Adl.Pattern                 (Pattern(..))
  import Adl.MorphismAndDeclaration  (makeDeclaration)
  import CommonClasses               (Identified(..))
  import Data.Explain
  
-- For parser data structures (which are: Concept, Declaration, Population, Rule, Gen, KeyDef, ObjectDef, Pattern and Context)
-- the function <explanations :: a -> [Explanation]> gives all explanations that are declared directly in <a>, but not in possible components of <a>.
-- So if <a> is a context, it gives the explanations declared in <a>, but not those declared in patterns in <a>
  class UserExplainable a where
    explanationDeclarations :: a -> [Explanation] -- all explanations declared in <a>. An explanation should answer the question "Why does <a> exist?"

  instance UserExplainable Context where
    explanationDeclarations context
     = [ExplConcept    cd l ref (string2ExplainContent expla)| PExplConcept     nm  l ref expla<-ctxpes context, cd<-ctxcs context, name cd==nm] ++
       [ExplDeclaration d l ref (string2ExplainContent expla)| PExplDeclaration mph l ref expla<-ctxpes context,  d<-ctxds context, makeDeclaration mph==d] ++
       [ExplRule        r l ref (string2ExplainContent expla)| PExplRule        nm  l ref expla<-ctxpes context,  r<-ctxrs context, name r==nm] ++
       [ExplKeyDef      k l ref (string2ExplainContent expla)| PExplKeyDef      nm  l ref expla<-ctxpes context,  k<-ctxks context, name k==nm] ++
       [ExplObjectDef   o l ref (string2ExplainContent expla)| PExplObjectDef   nm  l ref expla<-ctxpes context,  o<-ctxos context, name o==nm] ++
       [ExplPattern    pn l ref (string2ExplainContent expla)| PExplPattern     pn  l ref expla<-ctxpes context,  p<-ctxpats context, name p==pn]
 
  instance UserExplainable Pattern where
    explanationDeclarations pat
     = [ExplConcept    cd l ref (string2ExplainContent expla)| PExplConcept     nm  l ref expla<-ptxps pat, cd<-ptcds pat, name cd==nm] ++
       [ExplDeclaration d l ref (string2ExplainContent expla)| PExplDeclaration mph l ref expla<-ptxps pat,  d<-ptdcs pat, makeDeclaration mph==d] ++
       [ExplRule        r l ref (string2ExplainContent expla)| PExplRule        nm  l ref expla<-ptxps pat,  r<-ptrls pat, name r==nm] ++
       [ExplKeyDef      k l ref (string2ExplainContent expla)| PExplKeyDef      nm  l ref expla<-ptxps pat,  k<-ptkds pat, name k==nm]
  
--  instance Explainable Rule where
--    explanations r = [ ExplRule r "" (rrxpl r) ]   
--    filterExplanations r expls flags = [e|e<-expls , isLang (language flags) e , isforRule r e  ] 
--  instance Explainable Concept where
--  --TODO: Invullen
--    
--  instance Explainable Declaration where
--  --TODO: Invullen
--    
---- Populations and Gen were once to be Explainable, but they do not belong to the class Identified. => Needs rethink
----  instance Explainable Population where
----  --TODO: Invullen
----    
----  instance Explainable Gen where
----   --TODO: Invullen
--    
--  instance Explainable KeyDef where
--   --TODO: Invullen
--    
--  instance Explainable ObjectDef where
--   --TODO: Invullen
--    
--
--  isLang :: Lang -> Explanation -> Bool
--  isLang l expl = True 
--
--  isforRule :: Rule -> Explanation -> Bool
--  isforRule r (ExplRule a _ _) =  name r == name a 
--  isforRule _ _ = False
  
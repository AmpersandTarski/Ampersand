{-# OPTIONS_GHC -Wall #-}
module Classes.Explainable (Explainable(..)) 
where
  import Adl.Explanation             (Explanation(..),PExplanation(..))
  import Adl.Context                 (Context(..))
  import Adl.Pattern                 (Pattern(..))
  import Adl.Gen                     (Gen(..))
  import Adl.Rule                    (Rule(..))
  import Adl.ObjectDef               (ObjectDef(..))
  import Adl.KeyDef                  (KeyDef)
  import Adl.MorphismAndDeclaration  (Declaration,makeDeclaration)
  import Adl.Concept                 (Concept(..))
  import Adl.Population                 (Population(..))
  import Languages
  import CommonClasses               (Identified(..))

-- For parser data structures (which are: Concept, Declaration, Population, Rule, Gen, KeyDef, ObjectDef, Pattern and Context)
-- the function <explanations :: a -> [Explanation]> gives all explanations that are declared directly in <a>, but not in possible components of <a>.
-- So if <a> is a context, it gives the explanations declared in <a>, but not those declared in patterns in <a>
  class Explainable a where
    explanations :: a -> [Explanation] -- all explanations declared in <a>. An explanation should answer the question "Why does <a> exist?"
     
  instance Explainable a => Explainable [a] where
    explanations xs  = (concat. map explanations) xs

  instance Explainable Context where
    explanations context
     = [ExplConcept    cd l ref expla| PExplConcept     nm  l ref expla<-ctxpes context, cd<-ctxcs context, name cd==nm] ++
       [ExplDeclaration d l ref expla| PExplDeclaration mph l ref expla<-ctxpes context,  d<-ctxds context, makeDeclaration mph==d] ++
       [ExplRule        r l ref expla| PExplRule        nm  l ref expla<-ctxpes context,  r<-ctxrs context, name r==nm] ++
       [ExplKeyDef      k l ref expla| PExplKeyDef      nm  l ref expla<-ctxpes context,  k<-ctxks context, name k==nm] ++
       [ExplObjectDef   o l ref expla| PExplObjectDef   nm  l ref expla<-ctxpes context,  o<-ctxos context, name o==nm] ++
       [ExplPattern    pn l ref expla| PExplPattern     pn  l ref expla<-ctxpes context,  p<-ctxpats context, name p==pn]

  instance Explainable Pattern where
    explanations pat
     = [ExplConcept    cd l ref expla| PExplConcept     nm  l ref expla<-ptxps pat, cd<-ptcds pat, name cd==nm] ++
       [ExplDeclaration d l ref expla| PExplDeclaration mph l ref expla<-ptxps pat,  d<-ptdcs pat, makeDeclaration mph==d] ++
       [ExplRule        r l ref expla| PExplRule        nm  l ref expla<-ptxps pat,  r<-ptrls pat, name r==nm] ++
       [ExplKeyDef      k l ref expla| PExplKeyDef      nm  l ref expla<-ptxps pat,  k<-ptkds pat, name k==nm]
  
  instance Explainable Rule where
    explanations r = [ ExplRule r English "" (rrxpl r) ]   
    
  instance Explainable Concept where
  --TODO: Invullen
    
  instance Explainable Declaration where
  --TODO: Invullen
    
  instance Explainable Population where
  --TODO: Invullen
    
  instance Explainable Gen where
   --TODO: Invullen
    
  instance Explainable KeyDef where
   --TODO: Invullen
    
  instance Explainable ObjectDef where
   --TODO: Invullen
    



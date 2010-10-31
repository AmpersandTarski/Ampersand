{-# OPTIONS_GHC -Wall #-}
module Classes.Explainable (UserExplainable(..)) 
where
  import Adl.Explanation             (Explanation(..),PExplanation(..))
  import Adl.Context                 (Context(..))
  import Adl.Pattern                 (Pattern(..))
  import Adl.MorphismAndDeclaration  (makeDeclaration)
  import CommonClasses               (Identified(..))
  import Data.Explain
  import Options 
-- For parser data structures (which are: ConceptDef, Declaration, Population, Rule, Gen, KeyDef, ObjectDef, Pattern and Context)
-- the function <explanations :: a -> [Explanation]> gives all explanations that are declared directly in <a>, but not in possible components of <a>.
-- So if <a> is a context, it gives the explanations declared in <a>, but not those declared in patterns in <a>
  class UserExplainable a where
    explanationDeclarations :: Options -> a -> [Explanation] -- all explanations declared in <a>. An explanation should answer the question "Why does <a> exist?"

  instance UserExplainable Context where
    explanationDeclarations flags context
     = [ExplConceptDef cd l ref (string2ExplainContent flags expla)| PExplConceptDef  nm  l ref expla<-ctxpes context, cd<-ctxcs context, name cd==nm] ++
       [ExplDeclaration d l ref (string2ExplainContent flags expla)| PExplDeclaration mph l ref expla<-ctxpes context,  d<-ctxds context, makeDeclaration mph==d] ++
       [ExplRule        r l ref (string2ExplainContent flags expla)| PExplRule        nm  l ref expla<-ctxpes context,  r<-ctxrs context, name r==nm] ++
       [ExplKeyDef      k l ref (string2ExplainContent flags expla)| PExplKeyDef      nm  l ref expla<-ctxpes context,  k<-ctxks context, name k==nm] ++
       [ExplObjectDef   o l ref (string2ExplainContent flags expla)| PExplObjectDef   nm  l ref expla<-ctxpes context,  o<-ctxos context, name o==nm] ++
       [ExplPattern    pn l ref (string2ExplainContent flags expla)| PExplPattern     pn  l ref expla<-ctxpes context,  p<-ctxpats context, name p==pn]
 
  instance UserExplainable Pattern where
    explanationDeclarations flags pat
     = [ExplConceptDef cd l ref (string2ExplainContent flags expla)| PExplConceptDef  nm  l ref expla<-ptxps pat, cd<-ptcds pat, name cd==nm] ++
       [ExplDeclaration d l ref (string2ExplainContent flags expla)| PExplDeclaration mph l ref expla<-ptxps pat,  d<-ptdcs pat, makeDeclaration mph==d] ++
       [ExplRule        r l ref (string2ExplainContent flags expla)| PExplRule        nm  l ref expla<-ptxps pat,  r<-ptrls pat, name r==nm] ++
       [ExplKeyDef      k l ref (string2ExplainContent flags expla)| PExplKeyDef      nm  l ref expla<-ptxps pat,  k<-ptkds pat, name k==nm] ++
  --     [ExplObjectDef   o l ref (string2ExplainContent flags expla)| PExplObjectDef   nm  l ref expla<-ptxps pat,  o<-ctxos context, name o==nm] ++
       [ExplPattern    pn l ref (string2ExplainContent flags expla)| PExplPattern     pn  l ref expla<-ptxps pat  ]
  

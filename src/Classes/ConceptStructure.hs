{-# OPTIONS_GHC -Wall -XFunctionalDependencies -XMultiParamTypeClasses -XFlexibleContexts -XUndecidableInstances -XFlexibleInstances #-}
module Classes.ConceptStructure          (ConceptStructure(concs
                                                          ,mors
                                                          ,morlist
                                                          ,genE
                                           --             ,closExprs
                                                          )
                                         )
where
   import DatabaseDesign.Ampersand.ADL.Concept                    (Concept(..),Conceptual(..),GenR,SpecHierarchy(..))
   import DatabaseDesign.Ampersand.ADL.Context                    (Context(..))
   import DatabaseDesign.Ampersand.ADL.MorphismAndDeclaration     (Relation(..),Declaration(..),Association(..))
   import DatabaseDesign.Ampersand.ADL.Gen                        (Gen(..))
   import DatabaseDesign.Ampersand.ADL.Expression                 (Expression(..),mapExpression,foldrExpression)
   import DatabaseDesign.Ampersand.ADL.ObjectDef                  (ObjectDef(..),Service(..))
   import DatabaseDesign.Ampersand.ADL.KeyDef                     (KeyDef(..))
   import DatabaseDesign.Ampersand.ADL.Pattern                    (Pattern(..))
   import DatabaseDesign.Ampersand.ADL.Rule                       (Rule(..),RuleType(..))
   import Classification                 (Classification,preCl)
   import Collection                     (Collection(..))
   import Typology                       (genEq,typology)

   class (Conceptual c, SpecHierarchy c) => ConceptStructure a c |a->c where
    concs   :: a -> [c]                 -- ^ the set of all concepts used in data structure a
    mors    :: a -> [Relation c]        -- ^ the set of all relations used within data structure a,
    mors     = rd . morlist
    morlist :: a -> [Relation c]        -- ^ the list of all relations used within data structure a (the difference with mors is that morlist is not unique)
    genE    :: a -> GenR c
    genE x   = if null cx then (==) else head cx where cx = [order c|c<-concs x]
    
-- class KleeneClos where
--  closExprs    :: a -> Expressions (Relation c)  -- no double occurrences in the resulting list of expressions

   instance ConceptStructure a c => ConceptStructure [a] c where
    concs     = rd . concat . map concs
    mors      = rd . concat . map mors
    morlist   =      concat . map morlist
--   closExprs = rd . concat . map closExprs

   instance ConceptStructure (Classification Concept) Concept where
    concs     = rd . concat . map concs     . preCl
    mors      = rd . concat . map mors      . preCl
    morlist   =      concat . map morlist   . preCl
--  closExprs = rd . concat . map closExprs . preCl


   instance ConceptStructure Context Concept where
    concs     c = concs (ctxds c) `uni` concs (ctxpats c)
    mors      c = mors (ctxpats c) `uni` mors [svObj s| s<-ctxsvcs c]
    morlist   c = morlist (ctxpats c)++morlist [svObj s| s<-ctxsvcs c]
    genE      c = genEq (typology (ctxisa c))
--  closExprs c = closExprs (ctxpats c) `uni` closExprs [svObj s| s<-ctxsvcs c]

   instance ConceptStructure KeyDef Concept where
    concs     kd = [kdcpt kd] `uni` concs (kdats kd)
    mors      kd = mors (kdats kd)
    morlist   kd = morlist (kdats kd)
    genE      kd = genE (kdats kd)
--  closExprs kd = closExprs (kdats kd)

   instance ConceptStructure r c => ConceptStructure (Expression r) c where
    concs e             = foldrExpression uni [] (mapExpression concs e)
    mors e              = foldrExpression uni [] (mapExpression mors e)
    morlist e           = foldrExpression (++) [] (mapExpression morlist e)
    genE e              = if null cs then error ("!Fatal (module ConceptStructure 66): not defined: 'genE e'") else order (head cs)
                          where cs = concs e
--  closExprs e         = closExps e

{- closExps :: (Show r, Identified r, Eq r) => Expression r -> [Expression r]
   closExps (Tc f)   = closExps f
   closExps (F ts)   = (rd.concat.map closExps) ts
   closExps (Fdx ts) = (rd.concat.map closExps) ts
   closExps (Fux fs) = (rd.concat.map closExps) fs
   closExps (Fix fs) = (rd.concat.map closExps) fs
   closExps (K0x e') = [K0x e'] `uni` closExps e'
   closExps (K1x e') = [K1x e'] `uni` closExps e'
   closExps (Cpx e') = closExps e'
   closExps _        = []
-}

   instance ConceptStructure Concept Concept where
    concs c     = [c]
    mors      c = [I [] c c True]
    morlist   c = [I [] c c True]
--  closExprs _ = []
    genE _      = (==)   -- was: genE c = order c

   instance ConceptStructure ObjectDef Concept where
    concs     obj = [target (objctx obj)] `uni` concs (objats obj)
    mors      obj = mors (objctx obj) `uni` mors (objats obj) `uni` mors (target (objctx obj))  -- opletten: de expressie (objctx obj) hoort hier ook bij.
    morlist   obj = morlist (objctx obj)++morlist (objats obj)
--  closExprs obj = closExprs (objctx obj) `uni` closExprs (objats obj)

   instance (Conceptual c, SpecHierarchy c) => ConceptStructure (Relation c) c where
    concs mph   = rd [source mph,target mph]
    mors mph    = [mph]
    morlist mph = [mph]
    genE mph    = case mph of
                       Mp1{} -> error ("!Fatal (module ConceptStructure 101): not defined: 'genE Mp1{}'")
                       _     -> order (source mph)
--  closExprs _ = []

   instance (Conceptual c, SpecHierarchy c) => ConceptStructure (Declaration c) c where
    concs d = case d of
               Sgn{}     -> rd [desrc d,detrg d]
               Isn{}     -> rd [degen d,despc d]
               Iscompl{} -> [despc d]
               Vs{}      -> rd [desrc d,detrg d]
    mors _    = []
    morlist _ = []
    genE _    = (==)   -- was: genE d = genE (desrc d)
--  closExprs _ = []

   instance ConceptStructure Pattern Concept where
    concs     pat = concs (ptrls pat) `uni` concs (ptgns pat) `uni` concs (ptdcs pat)
    mors      pat = mors (ptrls pat) `uni` mors (ptkds pat)
    morlist   pat = morlist (ptrls pat)++morlist (ptkds pat)
    genE      pat = genE (ptdcs pat) 
--  closExprs pat = closExprs (ptrls pat)

   -- WHY??? wordt bij Truth de antecedent niet meegenomen?
   --           Er kunnen toch andere concepten en/of morphismen in de expressies aanwezig zijn in de lhs dan in de rhs??
   -- BECAUSE!!! een implicatie is antc |- cons, ofwel -antc\/cons
   --           een truth is      expr        , ofwel -V   \/expr,   ofwel  V |- expr
   --           Daarom laten we de antecedent helemaal weg.
   --           Het systeem genereert zelfs een !Fatal wanneer je naar de antecedent van een Truth zou refereren.
   instance (ConceptStructure (Expression r) c) =>
            ConceptStructure (Rule r) c where
    concs r = case r of
                Ru{rrsrt = Truth } -> concs (rrcon r)
                Ru{}               -> concs (rrant r) `uni` concs (rrcon r)
    mors r = case r of
                Ru{rrsrt = Truth } -> mors (rrcon r)
                Ru{}               -> mors (rrant r) `uni` mors (rrcon r)
    morlist r = case r of
                Ru{rrsrt = Truth } -> morlist (rrcon r)
                Ru{}               -> morlist (rrant r) ++ morlist (rrcon r)
    genE r = genE (rrcon r)
{-    closExprs r = case r of
                Ru{rrsrt = Truth } -> closExprs (rrcon r)
                Ru{}               -> closExprs (rrant r) `uni` closExprs (rrcon r)
-}

   instance (Conceptual c, SpecHierarchy c) => ConceptStructure (Gen c) c where
    concs g     = rd [gengen g,genspc g]  
    mors g      = [I{ mphats=[]
                    , mphgen = gengen g
                    , mphspc = genspc g
                    , mphyin = True
                    }]                         
    morlist g   = mors g
    genE g      = order (genspc g)
--  closExprs _ = []

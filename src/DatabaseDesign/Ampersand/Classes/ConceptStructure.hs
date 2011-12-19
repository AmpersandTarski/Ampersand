{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
module DatabaseDesign.Ampersand.Classes.ConceptStructure          (ConceptStructure(concs
                                                                                   ,mors
                                                                                   ,morlist
                                                                                   ,genE
                                                                    --             ,closExprs
                                                                                   )
                                                                   )
where
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree       
   import DatabaseDesign.Ampersand.Basics                          (fatalMsg,Collection(..),Classification,preCl)
   import Data.List
   import DatabaseDesign.Ampersand.ADL1.Expression
   import DatabaseDesign.Ampersand.ADL1.MorphismAndDeclaration
   import DatabaseDesign.Ampersand.Core.Poset(Ordering(..))
   import Prelude hiding (Ordering(..))
   fatal :: Int -> String -> a
   fatal = fatalMsg "Classes.ConceptStructure"

   class ConceptStructure a where
    concs   :: a -> [A_Concept]                 -- ^ the set of all concepts used in data structure a
    mors    :: a -> [Relation]        -- ^ the set of all relations used within data structure a,
    mors     = nub . morlist
    morlist :: a -> [Relation]        -- ^ the list of all relations used within data structure a (the difference with mors is that morlist is not unique)
    genE    :: a -> GenR
    genE cstruc = if null cs then ((\x y -> if x==y then EQ else NC),[]) else head cs where cs = [order c |c<-concs cstruc]
    
-- class KleeneClos where
--  closExprs    :: a -> [Expression] Relation  -- no double occurrences in the resulting list of expressions

   instance (ConceptStructure a,ConceptStructure b) => ConceptStructure (a, b)  where
    concs   (a,b) = concs a `uni` concs b
    mors    (a,b) =  mors a `uni` mors b
    morlist (a,b) = morlist a ++ morlist b
 
   instance ConceptStructure a => ConceptStructure [a] where
    concs     = nub . concatMap concs
    mors      = nub . concatMap mors
    morlist   =       concatMap morlist
--   closExprs = nub . concatMap closExprs
   instance ConceptStructure (Classification A_Concept) where
    concs     = nub . concatMap concs     . preCl
    mors      = nub . concatMap mors      . preCl
    morlist   =       concatMap morlist   . preCl
--  closExprs = nub . concatMap closExprs . preCl

   instance ConceptStructure A_Context where
    concs     c =       concs ( ctxds c ++ concatMap ptdcs (ctxpats c)  ++ concatMap prcDcls (ctxprocs c) ) 
                  `uni` concs ( ctxgs c ++ concatMap ptgns (ctxpats c)  ++ concatMap prcGens (ctxprocs c) )
                  `uni` [ONE]
    mors      c = mors (ctxpats c) `uni` mors (ctxprocs c) `uni` mors [ifcObj s | s<-ctxifcs c]
    morlist   c = morlist (ctxpats c)++morlist (ctxprocs c)++morlist [ifcObj s | s<-ctxifcs c]
--  closExprs c = closExprs (ctxpats c) `uni` closExprs [ifcObj s | s<-ctxifcs c]
    genE      c = ctxpo c
   instance ConceptStructure KeyDef where
    concs     kd = [kdcpt kd] `uni` concs (kdats kd)
    mors      kd = mors (kdats kd)
    morlist   kd = morlist (kdats kd)
--  closExprs kd = closExprs (kdats kd)

   instance ConceptStructure Expression where
    concs          = foldrMapExpression uni concs []
    mors           = foldrMapExpression uni mors  []
    morlist        = foldrMapExpression (++) morlist []
--  closExprs e         = closExps e



{- closExps :: Expression -> [Expression]
   closExps (EBrk f)   = closExps f
   closExps (ECps ts)   = (rd.concat.map closExps) ts
   closExps (ERad ts) = (rd.concat.map closExps) ts
   closExps (EUni fs) = (rd.concat.map closExps) fs
   closExps (EIsc fs) = (rd.concat.map closExps) fs
   closExps (EKl0 e') = [EKl0 e'] `uni` closExps e'
   closExps (EKl1 e') = [EKl1 e'] `uni` closExps e'
   closExps (ECpl e') = closExps e'
   closExps _        = []
-}


   instance ConceptStructure A_Concept where
    concs c     = [c]
--    mors      c = [I c]
    morlist   c = [I c]
--  closExprs _ = []

   instance ConceptStructure Sign where
    concs (Sign s t) = nub [s,t]
    mors      _      = []
    morlist   _      = []
--  closExprs _ = []

   instance ConceptStructure ObjectDef where
    concs     obj = [target (objctx obj)] `uni` concs (objats obj)
    mors      obj = mors (objctx obj) `uni` mors (objats obj) `uni` mors (target (objctx obj))  -- opletten: de expressie (objctx obj) hoort hier ook bij.
    morlist   obj = morlist (objctx obj)++morlist (objats obj)
--  closExprs obj = closExprs (objctx obj) `uni` closExprs (objats obj)

   instance ConceptStructure Pattern where
    concs     pat = concs (ptgns pat) `uni` concs (ptdcs pat) `uni` concs (ptrls pat) `uni` concs (ptkds pat)
    mors      pat = mors (ptrls pat) `uni` mors (ptkds pat) `uni` mors [makeRelation d | d<-ptdcs pat, (not.null) (multiplicities d)]
    morlist   pat = morlist (ptrls pat)++morlist (ptkds pat)
--  closExprs pat = closExprs (ptrls pat)

   instance ConceptStructure Process where
    concs     proc = concs (prcRules proc)
    mors      proc = mors (prcRules proc)
    morlist   proc = morlist (prcRules proc)
--  closExprs proc = closExprs (prcRules proc)


   instance ConceptStructure Interface where
    concs     ifc = concs (ifcObj ifc)
    mors      ifc = mors (ifcObj ifc)
    morlist   ifc = morlist (ifcObj ifc)
--  closExprs ifc = closExprs (ifcObj ifc)

   instance ConceptStructure Relation where
    concs rel   = nub [source rel,target rel]
    mors rel    = [rel]
    morlist rel = [rel]
--  closExprs _ = []
   instance ConceptStructure Declaration where
    concs d   = concs (sign d)
    mors _    = []
    morlist _ = []
--  closExprs _ = []

   instance ConceptStructure Rule where
    concs r   = concs (rrexp r)
    mors r    = mors (rrexp r)
    morlist r = morlist (rrexp r)

   instance ConceptStructure A_Gen where
    concs g     = nub [gengen g,genspc g]  
--    mors g      = []                         
    morlist _   = []
--  closExprs _ = []

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
module DatabaseDesign.Ampersand.Classes.ConceptStructure          (ConceptStructure(..)
                                                                   )
where
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree       
   import DatabaseDesign.Ampersand.Basics
   import Data.List
   import Data.Maybe
   import DatabaseDesign.Ampersand.ADL1.Expression
   import DatabaseDesign.Ampersand.Classes.Relational(multiplicities,makeRelation)
   import Prelude hiding (Ordering(..))
   fatal :: Int -> String -> a
   fatal = fatalMsg "Classes.ConceptStructure"

   class ConceptStructure a where
    concs ::    a -> [A_Concept]       -- ^ the set of all concepts used in data structure a
    mors ::     a -> [Relation]        -- ^ the set of all relations used within data structure a,
    mors = nub . morlist
    morlist ::  a -> [Relation]        -- ^ the set of all relations used within data structure a
                                       --   (the difference with mors is that morlist is not unique wrt name and signature)
    mp1Rels ::  a -> [(Relation,A_Concept)]        -- ^ the set of all Mp1 relations within data structure a (needed to get the atoms of these relations into the populationtable)
    genE ::     a -> GenR
    genE cstruc = case concs cstruc of
                   [] -> fatal 25 "No concepts???"
                   cs -> genE (head cs)

-- class KleeneClos where
--  closExprs :: a -> [Expression] Relation  -- no double occurrences in the resulting list of expressions

   instance (ConceptStructure a,ConceptStructure b) => ConceptStructure (a, b)  where
    concs    (a,b) = concs a `uni` concs b
    mors     (a,b) = mors a  `uni` mors b
    morlist  (a,b) = morlist a ++ morlist b
    mp1Rels  (a,b) = mp1Rels  a `uni` mp1Rels  b

   instance ConceptStructure a => ConceptStructure (Maybe a) where
    concs    ma = maybe [] concs ma
    mors     ma = maybe [] mors ma
    morlist  ma = maybe [] morlist ma
    mp1Rels  ma = maybe [] mp1Rels  ma
 
   instance ConceptStructure a => ConceptStructure [a] where
    concs     = nub . concatMap concs
    mors      = nub . concatMap mors
    morlist   =       concatMap morlist
    mp1Rels   = nub . concatMap mp1Rels 
    
   instance ConceptStructure (Classification A_Concept) where
    concs     = nub . concatMap concs     . preCl
    mors      = nub . concatMap mors      . preCl
    morlist   =       concatMap morlist   . preCl
    mp1Rels   = nub . concatMap mp1Rels   . preCl
    
   instance ConceptStructure A_Context where
    concs     c =       concs ( ctxds c ++ concatMap ptdcs (ctxpats c)  ++ concatMap prcDcls (ctxprocs c) ) 
                  `uni` concs ( ctxgs c ++ concatMap ptgns (ctxpats c)  ++ concatMap prcGens (ctxprocs c) )
                  `uni` [ONE]
    mors      c = mors (ctxpats c) `uni` mors (ctxprocs c) `uni` mors [ifcObj s | s<-ctxifcs c]
    morlist   c = morlist (ctxpats c)++morlist (ctxprocs c)++morlist [ifcObj s | s<-ctxifcs c]
    mp1Rels   _ = fatal 64 "do not use this from a context"
    genE      c = ctxpo c

   instance ConceptStructure KeyDef where
    concs     kd = [kdcpt kd] `uni` concs [objDef | KeyExp objDef <- kdats kd]
    mors      kd = mors                   [objDef | KeyExp objDef <- kdats kd]
    morlist   kd = morlist                [objDef | KeyExp objDef <- kdats kd]
    mp1Rels   kd = mp1Rels                [objDef | KeyExp objDef <- kdats kd]

   instance ConceptStructure Expression where
    concs          = foldrMapExpression uni concs []
    mors           = foldrMapExpression uni mors  []
    morlist        = foldrMapExpression (++) morlist []
    mp1Rels (EEqu (l,r) _) = mp1Rels l `uni` mp1Rels r
    mp1Rels (EImp (l,r) _) = mp1Rels l `uni` mp1Rels r
    mp1Rels (EIsc (l,r) _) = mp1Rels l `uni` mp1Rels r
    mp1Rels (EUni (l,r) _) = mp1Rels l `uni` mp1Rels r
    mp1Rels (EDif (l,r) _) = mp1Rels l `uni` mp1Rels r
    mp1Rels (ELrs (l,r) _) = mp1Rels l `uni` mp1Rels r
    mp1Rels (ERrs (l,r) _) = mp1Rels l `uni` mp1Rels r
    mp1Rels (ECps (l,r) _) = mp1Rels l `uni` mp1Rels r
    mp1Rels (ERad (l,r) _) = mp1Rels l `uni` mp1Rels r
    mp1Rels (EPrd (l,r) _) = mp1Rels l `uni` mp1Rels r
    mp1Rels (EKl0 e _)     = mp1Rels e
    mp1Rels (EKl1 e _)     = mp1Rels e
    mp1Rels (EFlp e _)     = mp1Rels e
    mp1Rels (ECpl e _)     = mp1Rels e
    mp1Rels (EBrk e)       = mp1Rels e
    mp1Rels (ETyp e _)     = mp1Rels e
    mp1Rels e@(ERel rel@Mp1{} _) = [(rel,source e)]
    mp1Rels (ERel{})       = []


   instance ConceptStructure A_Concept where
    concs   c     = [c]
--    mors    c = [I c]
    morlist c = [I c]
    mp1Rels _ = []
    genE      = cptgE 

   instance ConceptStructure Sign where
    concs (Sign s t) = nub [s,t]
    mors      _      = []
    morlist   _      = []
    mp1Rels   _      = []

   instance ConceptStructure ObjectDef where
    concs     obj = [target (objctx obj)] `uni` concs (objmsub obj)
    mors      obj = mors (objctx obj) `uni` mors (objmsub obj) `uni` mors (target (objctx obj))  -- opletten: de expressie (objctx obj) hoort hier ook bij.
    morlist   obj = morlist (objctx obj)++morlist (objmsub obj)
    mp1Rels   obj = mp1Rels (objctx obj) `uni` mp1Rels (objmsub obj)

   -- Note that these functions are not recursive in the case of InterfaceRefs (which is of course obvious from their types)
   instance ConceptStructure SubInterface where
    concs (Box objs)         = concs objs 
    concs (InterfaceRef _)   = [] 
    mors (Box objs)          = mors objs 
    mors (InterfaceRef _)    = [] 
    morlist (Box objs)       = morlist objs 
    morlist (InterfaceRef _) = [] 
    mp1Rels (Box objs)       = mp1Rels objs 
    mp1Rels (InterfaceRef _) = [] 
          
   instance ConceptStructure Pattern where
    concs     p = concs (ptgns p)   `uni` concs (ptdcs p)   `uni` concs (ptrls p)    `uni` concs (ptkds p)
    mors      p = mors  (ptrls p) `uni` mors (ptkds p) `uni` mors [makeRelation d | d<-ptdcs p, (not.null) (multiplicities d)]
    morlist   p = morlist (ptrls p)++morlist (ptkds p)
    mp1Rels   p = mp1Rels (ptrls p) `uni` mp1Rels (ptkds p)

   instance ConceptStructure Process where
    concs     p = concs (prcGens p) `uni` concs (prcDcls p) `uni` concs (prcRules p) `uni` concs (prcKds p)
    mors      p = mors  (prcRules p) `uni` mors (prcKds p) `uni` mors [makeRelation d | d<-prcDcls p, (not.null) (multiplicities d)]
    morlist   p = morlist (prcRules p)++morlist (prcKds p)
    mp1Rels   p = mp1Rels (prcRules p) `uni` mp1Rels (prcKds p)


   instance ConceptStructure Interface where
    concs     ifc = concs (ifcObj ifc)
    mors      ifc = mors (ifcObj ifc)
    morlist   ifc = morlist (ifcObj ifc)
    mp1Rels   ifc = mp1Rels (ifcObj ifc)

   instance ConceptStructure Relation where
    concs rel   = nub [source rel,target rel]
    mors rel    = [rel]
    morlist rel = [rel]
    mp1Rels _   = fatal 146 "mp1Rels not allowed on Relation"
                    
   instance ConceptStructure Declaration where
    concs d   = concs (sign d)
    mors _    = []
    morlist _ = []
    mp1Rels _ = []

   instance ConceptStructure Rule where
    concs r   = concs (rrexp r)
    mors r    = mors (rrexp r)
    morlist r = morlist (rrexp r)
    mp1Rels r = mp1Rels (rrexp r)
    
   instance ConceptStructure A_Gen where
    concs g     = nub [gengen g,genspc g]  
--    mors g      = []                         
    morlist _   = []
    mp1Rels _ = []

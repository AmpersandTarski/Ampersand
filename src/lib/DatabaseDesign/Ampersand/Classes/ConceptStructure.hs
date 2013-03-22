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
   -- import DatabaseDesign.Ampersand.Classes.Relational(multiplicities,makeRelation)
   import Prelude hiding (Ordering(..))
   fatal :: Int -> String -> a
   fatal = fatalMsg "Classes.ConceptStructure"

   class ConceptStructure a where
    concs ::    a -> [A_Concept]       -- ^ the set of all concepts used in data structure a
    declsUsedIn ::     a -> [Declaration]        -- ^ the set of all relations used within data structure a,
    declsUsedIn        a = nub (map makeDeclaration (morlist a))
    morlist ::  a -> [Relation]        -- ^ the set of all relations used within data structure a
                                       --   (the difference with mors is that morlist is not unique wrt name and signature)
    mp1Exprs :: a -> [Expression]     -- ^ the set of all EMp1 expressions within data structure a (needed to get the atoms of these relations into the populationtable)
    genE ::     a -> GenR
    genE cstruc = case concs cstruc of
                   [] -> fatal 25 "No concepts???"
                   cs -> genE (head cs)

-- class KleeneClos where
--  closExprs :: a -> [Expression] Relation  -- no double occurrences in the resulting list of expressions

   instance (ConceptStructure a,ConceptStructure b) => ConceptStructure (a, b)  where
    concs    (a,b) = concs a `uni` concs b
--    mors     (a,b) = mors a  `uni` mors b
    morlist  (a,b) = morlist a ++ morlist b
    mp1Exprs (a,b) = mp1Exprs a `uni` mp1Exprs b

   instance ConceptStructure a => ConceptStructure (Maybe a) where
    concs    ma = maybe [] concs ma
--    mors     ma = maybe [] mors ma
    morlist  ma = maybe [] morlist ma
    mp1Exprs ma = maybe [] mp1Exprs ma
 
   instance ConceptStructure a => ConceptStructure [a] where
    concs     = nub . concatMap concs
--    mors      = nub . concatMap mors
    morlist   =       concatMap morlist
    mp1Exprs  = nub . concatMap mp1Exprs 
    
   instance ConceptStructure A_Context where
    concs     c =       concs ( ctxds c ++ concatMap ptdcs (ctxpats c)  ++ concatMap prcDcls (ctxprocs c) ) 
                  `uni` concs ( ctxgs c ++ concatMap ptgns (ctxpats c)  ++ concatMap prcGens (ctxprocs c) )
                  `uni` [ONE]
--    mors      c = mors (ctxpats c) 
--            `uni` mors (ctxprocs c)
--            `uni` mors [ifcObj s | s<-ctxifcs c]
--            `uni` mors (ctxrs c)
--            `uni` mors (      
    morlist   c = morlist (ctxpats c)
                ++morlist (ctxprocs c)
                ++morlist (map ifcObj (ctxifcs c))
                ++morlist  (ctxrs c)
                ++morlist (ctxks c)
                ++morlist (ctxsql c)
                ++morlist (ctxphp c)
    mp1Exprs  _ = fatal 64 "do not use this from a context"
    genE      c = ctxpo c

   instance ConceptStructure KeyDef where
    concs     kd = [kdcpt kd] `uni` concs [objDef | KeyExp objDef <- kdats kd]
--    mors      kd = mors                   [objDef | KeyExp objDef <- kdats kd]
    morlist   kd = morlist                [objDef | KeyExp objDef <- kdats kd]
    mp1Exprs  kd = mp1Exprs               [objDef | KeyExp objDef <- kdats kd]

   instance ConceptStructure Expression where
    concs          = foldrMapExpression uni concs []
--    mors           = foldrMapExpression uni mors  []
    morlist        = foldrMapExpression (++) morlist []
    mp1Exprs (EEqu (l,r) _) = mp1Exprs l `uni` mp1Exprs r
    mp1Exprs (EImp (l,r) _) = mp1Exprs l `uni` mp1Exprs r
    mp1Exprs (EIsc (l,r) _) = mp1Exprs l `uni` mp1Exprs r
    mp1Exprs (EUni (l,r) _) = mp1Exprs l `uni` mp1Exprs r
    mp1Exprs (EDif (l,r) _) = mp1Exprs l `uni` mp1Exprs r
    mp1Exprs (ELrs (l,r) _) = mp1Exprs l `uni` mp1Exprs r
    mp1Exprs (ERrs (l,r) _) = mp1Exprs l `uni` mp1Exprs r
    mp1Exprs (ECps (l,r) _) = mp1Exprs l `uni` mp1Exprs r
    mp1Exprs (ERad (l,r) _) = mp1Exprs l `uni` mp1Exprs r
    mp1Exprs (EPrd (l,r) _) = mp1Exprs l `uni` mp1Exprs r
    mp1Exprs (EKl0 e _)     = mp1Exprs e
    mp1Exprs (EKl1 e _)     = mp1Exprs e
    mp1Exprs (EFlp e _)     = mp1Exprs e
    mp1Exprs (ECpl e _)     = mp1Exprs e
    mp1Exprs (EBrk e)       = mp1Exprs e
    mp1Exprs (ETyp e _)     = mp1Exprs e
    mp1Exprs ERel{}         = []
    mp1Exprs e@EMp1{}       = [e]


   instance ConceptStructure A_Concept where
    concs   c     = [c]
--    mors    c = [I c]
    morlist c = [I c]
    mp1Exprs _ = []
    genE C{cptgE = a}  = a
    genE _ = fatal 100 "A_Concept without cptgE"

   instance ConceptStructure Sign where
    concs (Sign s t) = nub [s,t]
--    mors      _      = []
    morlist   _      = []
    mp1Exprs  _      = []

   instance ConceptStructure ObjectDef where
    concs     obj = [target (objctx obj)] `uni` concs (objmsub obj)
--    mors      obj = mors (objctx obj) `uni` mors (objmsub obj) `uni` mors (target (objctx obj))  -- opletten: de expressie (objctx obj) hoort hier ook bij.
    morlist   obj = morlist (objctx obj)++morlist (objmsub obj)++morlist (target (objctx obj))  -- opletten: de expressie (objctx obj) hoort hier ook bij.
    mp1Exprs  obj = mp1Exprs (objctx obj) `uni` mp1Exprs (objmsub obj)

   -- Note that these functions are not recursive in the case of InterfaceRefs (which is of course obvious from their types)
   instance ConceptStructure SubInterface where
    concs (Box objs)         = concs objs 
    concs (InterfaceRef _)   = [] 
--    mors (Box objs)          = mors objs 
--    mors (InterfaceRef _)    = [] 
    morlist (Box objs)       = morlist objs 
    morlist (InterfaceRef _) = [] 
    mp1Exprs (Box objs)       = mp1Exprs objs 
    mp1Exprs (InterfaceRef _) = [] 
          
   instance ConceptStructure Pattern where
    concs     p = concs (ptgns p)   `uni` concs (ptdcs p)   `uni` concs (ptrls p)    `uni` concs (ptkds p)
--    mors      p = mors  (ptrls p) `uni` mors (ptkds p) `uni` mors [makeRelation d | d<-ptdcs p, (not.null) (multiplicities d)]
    morlist   p = morlist (ptrls p)++morlist (ptkds p)
    mp1Exprs  p = mp1Exprs (ptrls p) `uni` mp1Exprs (ptkds p)

   instance ConceptStructure Process where
    concs     p = concs (prcGens p) `uni` concs (prcDcls p) `uni` concs (prcRules p) `uni` concs (prcKds p)
 --   mors      p = mors  (prcRules p) `uni` mors (prcKds p) `uni` mors [makeRelation d | d<-prcDcls p, (not.null) (multiplicities d)]
    morlist   p = morlist (prcRules p)++morlist (prcKds p)++[r | (_,r) <- prcRRels p]
    mp1Exprs  p = mp1Exprs (prcRules p) `uni` mp1Exprs (prcKds p)


   instance ConceptStructure Interface where
    concs     ifc = concs (ifcObj ifc)
--    mors      ifc = mors (ifcObj ifc)
    morlist   ifc = morlist (ifcObj ifc)
    mp1Exprs  ifc = mp1Exprs (ifcObj ifc)

   instance ConceptStructure Relation where
    concs rel   = let d=makeDeclaration rel in nub [source d,target d]
--    mors rel    = [rel]
    morlist rel = [rel]
    mp1Exprs _   = fatal 142 "mp1Exprs not allowed on Relation"
                    
   instance ConceptStructure Declaration where
    concs d   = concs (sign d)
--    mors _    = []
    morlist _ = []
    mp1Exprs _ = fatal 148 "mp1Exprs not allowed on Declaration"

   instance ConceptStructure Rule where
    concs r   = concs (rrexp r) ++ concs (rrviol r)
--    mors r    = mors (rrexp r) `uni` mors (rrviol r)
    morlist r = morlist (rrexp r) ++ morlist (rrviol r)
    mp1Exprs r = mp1Exprs (rrexp r)
   
   instance ConceptStructure PairView where
    concs (PairView ps) = concs ps
    morlist (PairView ps) = morlist ps
    mp1Exprs (PairView ps) = mp1Exprs ps
     
   instance ConceptStructure PairViewSegment where
    concs    (PairViewText _)  = []
    concs    (PairViewExp _ x) = concs x
    morlist  (PairViewText _)  = []
    morlist  (PairViewExp _ x) = morlist x
    mp1Exprs (PairViewText _)  = []
    mp1Exprs (PairViewExp _ x) = mp1Exprs x
     
   instance ConceptStructure A_Gen where
    concs g     = nub [gengen g,genspc g]  
--    mors g      = []                         
    morlist _   = []
    mp1Exprs _ = fatal 160 "mp1Exprs not allowed on A_Gen"

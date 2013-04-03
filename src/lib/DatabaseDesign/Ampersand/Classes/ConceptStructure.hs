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
    declsUsedIn :: a -> [Declaration]        -- ^ the set of all declaratons used within data structure a. `used within` means that there is a relation that refers to that declaration.
    declsUsedIn a = nub (map makeDeclaration (relationsIn a))
    relationsIn ::  a -> [Relation]        -- ^ the set of all relations used within data structure a
    mp1Exprs :: a -> [Expression]     -- ^ the set of all EMp1 expressions within data structure a (needed to get the atoms of these relations into the populationtable)
    genE ::     a -> GenR
    genE cstruc = case concs cstruc of
                   [] -> fatal 25 "No concepts???"
                   x:_ -> genE x

-- class KleeneClos where
--  closExprs :: a -> [Expression] Relation  -- no double occurrences in the resulting list of expressions

   instance (ConceptStructure a,ConceptStructure b) => ConceptStructure (a, b)  where
    concs    (a,b) = concs a `uni` concs b
    relationsIn  (a,b) = relationsIn a `uni` relationsIn b
    mp1Exprs (a,b) = mp1Exprs a `uni` mp1Exprs b

   instance ConceptStructure a => ConceptStructure (Maybe a) where
    concs    ma = maybe [] concs ma
    relationsIn  ma = maybe [] relationsIn ma
    mp1Exprs ma = maybe [] mp1Exprs ma
 
   instance ConceptStructure a => ConceptStructure [a] where
    concs     = nub . concatMap concs
    relationsIn = foldr ((uni) . relationsIn) [] 
    mp1Exprs  = nub . concatMap mp1Exprs 
    
   instance ConceptStructure A_Context where
    concs     c =       concs ( ctxds c ++ concatMap ptdcs (ctxpats c)  ++ concatMap prcDcls (ctxprocs c) ) 
                  `uni` concs ( ctxgs c ++ concatMap ptgns (ctxpats c)  ++ concatMap prcGens (ctxprocs c) )
                  `uni` [ONE]
    relationsIn c = foldr (uni) []
                      [ (relationsIn.ctxpats) c
                      , (relationsIn.ctxprocs) c
                      , (relationsIn.ctxifcs) c
                      , (relationsIn.ctxrs) c
                      , (relationsIn.ctxks) c
                      , (relationsIn.ctxsql) c
                      , (relationsIn.ctxphp) c
                      ]
    mp1Exprs  _ = fatal 64 "do not use this from a context"
    genE      c = ctxpo c

   instance ConceptStructure KeyDef where
    concs       kd = [kdcpt kd] `uni` concs [objDef | KeyExp objDef <- kdats kd]
    relationsIn kd = relationsIn            [objDef | KeyExp objDef <- kdats kd]
    mp1Exprs    kd = mp1Exprs               [objDef | KeyExp objDef <- kdats kd]

   instance ConceptStructure Expression where
    concs          = foldrMapExpression uni concs []
    relationsIn    = foldrMapExpression (uni) relationsIn []
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
    mp1Exprs EDcD{}         = []
    mp1Exprs EDcI{}         = []
    mp1Exprs EDcV{}         = []
    mp1Exprs e@EMp1{}       = [e]


   instance ConceptStructure A_Concept where
    concs   c     = [c]
    relationsIn c = [I c]
    mp1Exprs _    = []
    genE C{cptgE = a}  = a
    genE _ = fatal 100 "A_Concept without cptgE"

   instance ConceptStructure Sign where
    concs (Sign s t) = nub [s,t]
    relationsIn   _  = []
    mp1Exprs  _      = []

   instance ConceptStructure ObjectDef where
    concs     obj = [target (objctx obj)] `uni` concs (objmsub obj)
    relationsIn obj = foldr (uni) []
                       [ (relationsIn.objctx) obj
                       , (relationsIn.objmsub) obj
                       , (relationsIn.target.objctx) obj
                       ]
    mp1Exprs  obj = mp1Exprs (objctx obj) `uni` mp1Exprs (objmsub obj)

   -- Note that these functions are not recursive in the case of InterfaceRefs (which is of course obvious from their types)
   instance ConceptStructure SubInterface where
    concs (Box objs)         = concs objs 
    concs (InterfaceRef _)   = [] 
    relationsIn (Box objs)       = relationsIn objs 
    relationsIn (InterfaceRef _) = [] 
    mp1Exprs (Box objs)       = mp1Exprs objs 
    mp1Exprs (InterfaceRef _) = [] 
          
   instance ConceptStructure Pattern where
    concs       p = concs (ptgns p)   `uni` concs (ptdcs p)   `uni` concs (ptrls p)    `uni` concs (ptkds p)
    relationsIn p = foldr (uni) []
                       [ (relationsIn.ptrls) p
                       , (relationsIn.ptkds) p
                       ]
    mp1Exprs  p = mp1Exprs (ptrls p) `uni` mp1Exprs (ptkds p)

   instance ConceptStructure Process where
    concs     p = concs (prcGens p) `uni` concs (prcDcls p) `uni` concs (prcRules p) `uni` concs (prcKds p)
    relationsIn p = foldr (uni) []
                       [ (relationsIn.prcRules) p
                       , (relationsIn.prcKds) p
                       , (relationsIn.prcrrels) p
                       ]
           where prcrrels x = map snd (prcRRels x)
    mp1Exprs  p = mp1Exprs (prcRules p) `uni` mp1Exprs (prcKds p)

   instance ConceptStructure Interface where
    concs       ifc = concs       (ifcObj ifc)
    relationsIn ifc = foldr (uni) []
                       [ (relationsIn.ifcObj) ifc
                       , (relationsIn.ifcParams) ifc
                       , (relationsIn.ifcViols) ifc
                       ]
    mp1Exprs    ifc = mp1Exprs    (ifcObj ifc)

   instance ConceptStructure Relation where
    concs       rel = let d=makeDeclaration rel in nub [source d,target d]
    relationsIn rel = [rel]
    mp1Exprs    _   = fatal 142 "mp1Exprs not allowed on Relation"
                    
   instance ConceptStructure Declaration where
    concs       d = concs (sign d)
    relationsIn _ = []
    mp1Exprs    _ = fatal 148 "mp1Exprs not allowed on Declaration"

   instance ConceptStructure Rule where
    concs r   = concs (rrexp r) ++ concs (rrviol r)
    relationsIn r = foldr (uni) []
                     [ (relationsIn.rrexp ) r
                     , (relationsIn.rrviol) r
                     ]
    mp1Exprs r = mp1Exprs (rrexp r)
   
   instance ConceptStructure PairView where
    concs       (PairView ps) = concs       ps
    relationsIn (PairView ps) = relationsIn ps
    mp1Exprs    (PairView ps) = mp1Exprs    ps
     
   instance ConceptStructure PairViewSegment where
    concs       (PairViewText _)  = []
    concs       (PairViewExp _ x) = concs x
    relationsIn (PairViewText _)  = []
    relationsIn (PairViewExp _ x) = relationsIn x
    mp1Exprs    (PairViewText _)  = []
    mp1Exprs    (PairViewExp _ x) = mp1Exprs x
     
   instance ConceptStructure A_Gen where
    concs g        = nub [gengen g,genspc g]  
    relationsIn _  = []
    mp1Exprs _ = fatal 160 "mp1Exprs not allowed on A_Gen"

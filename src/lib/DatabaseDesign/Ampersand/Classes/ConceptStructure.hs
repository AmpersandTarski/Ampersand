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
   import Prelude hiding (Ordering(..))
   fatal :: Int -> String -> a
   fatal = fatalMsg "Classes.ConceptStructure"

   class ConceptStructure a where
    concs ::    a -> [A_Concept]       -- ^ the set of all concepts used in data structure a
    declsUsedIn :: a -> [Declaration]        -- ^ the set of all declaratons used within data structure a. `used within` means that there is a relation that refers to that declaration.
    declsUsedIn a = map prim2dcl ((filter (not.isMp1).nub.concatMap primitives.expressionsIn) a)
      where prim2dcl expr =
             case expr of
               EDcD d@Sgn{} -> d
               EDcD{}       -> fatal 23 "invalid declaration in EDcD{}" 
               EDcI c       -> Isn c
               EDcV sgn     -> Vs sgn
               EMp1{}  -> fatal 25 "EMp1 should be filtered out from primitives. use `filter (not isMp1)`"
               _       -> fatal 26 "prim2dcl is not supposed to be calleed on a non-primitive expression."
    expressionsIn :: a -> [Expression] -- ^The set of all expressions within data structure a 
    mp1Exprs :: a -> [Expression]     -- ^ the set of all EMp1 expressions within data structure a (needed to get the atoms of these relations into the populationtable)
    mp1Exprs = filter isMp1.nub.concatMap primitives.expressionsIn

   instance (ConceptStructure a,ConceptStructure b) => ConceptStructure (a, b)  where
    concs    (a,b) = concs a `uni` concs b
    expressionsIn (a,b) = expressionsIn a `uni` expressionsIn b

   instance ConceptStructure a => ConceptStructure (Maybe a) where
    concs    ma = maybe [] concs ma
    expressionsIn ma = maybe [] expressionsIn ma
 
   instance ConceptStructure a => ConceptStructure [a] where
    concs     = nub . concatMap concs
    expressionsIn = foldr ((uni) . expressionsIn) [] 
    
   instance ConceptStructure A_Context where
    concs     c =       concs ( ctxds c ++ concatMap ptdcs (ctxpats c)  ++ concatMap prcDcls (ctxprocs c) ) 
                  `uni` concs ( ctxgs c ++ concatMap ptgns (ctxpats c)  ++ concatMap prcGens (ctxprocs c) )
                  `uni` [ONE]
    expressionsIn c = foldr (uni) []
                      [ (expressionsIn.ctxpats) c
                      , (expressionsIn.ctxprocs) c
                      , (expressionsIn.ctxifcs) c
                      , (expressionsIn.ctxrs) c
                      , (expressionsIn.ctxks) c
                      , (expressionsIn.ctxvs) c
                      , (expressionsIn.ctxsql) c
                      , (expressionsIn.ctxphp) c
                      ]

   instance ConceptStructure IdentityDef where
    concs       identity   = [idCpt identity] `uni` concs [objDef | IdentityExp objDef <- identityAts identity]
    expressionsIn identity = expressionsIn             [objDef | IdentityExp objDef <- identityAts identity]

   instance ConceptStructure ViewDef where
    concs       vd = [vdcpt vd] `uni` concs [objDef | ViewExp objDef <- vdats vd]
    expressionsIn vd = expressionsIn        [objDef | ViewExp objDef <- vdats vd]

   instance ConceptStructure Expression where
    concs          = foldrMapExpression uni concs []
    expressionsIn e = [e]


   instance ConceptStructure A_Concept where
    concs   c     = [c]
    expressionsIn _ = []

   instance ConceptStructure Sign where
    concs (Sign s t) = nub [s,t]
    expressionsIn _  = []

   instance ConceptStructure ObjectDef where
    concs     obj = [target (objctx obj)] `uni` concs (objmsub obj)
    expressionsIn obj = foldr (uni) []
                       [ (expressionsIn.objctx) obj
                       , (expressionsIn.objmsub) obj
                       ]

   -- Note that these functions are not recursive in the case of InterfaceRefs (which is of course obvious from their types)
   instance ConceptStructure SubInterface where
    concs (Box _ objs)         = concs objs 
    concs (InterfaceRef _)   = [] 
    expressionsIn (Box _ objs)       = expressionsIn objs 
    expressionsIn (InterfaceRef _) = [] 
          
   instance ConceptStructure Pattern where
    concs       p = concs (ptgns p)   `uni` concs (ptdcs p)   `uni` concs (ptrls p)    `uni` concs (ptids p)
    expressionsIn p = foldr (uni) []
                       [ (expressionsIn.ptrls) p
                       , (expressionsIn.ptids) p
                       , (expressionsIn.ptvds) p
                       ]

   instance ConceptStructure Process where
    concs     p = concs (prcGens p) `uni` concs (prcDcls p) `uni` concs (prcRules p) `uni` concs (prcIds p)
    expressionsIn p = foldr (uni) []
                       [ (expressionsIn.prcRules) p
                       , (expressionsIn.prcIds) p
                       , (expressionsIn.prcVds) p
                       ]

   instance ConceptStructure Interface where
    concs       ifc = concs       (ifcObj ifc)
    expressionsIn ifc = foldr (uni) []
                       [ (expressionsIn.ifcObj) ifc
                       , (expressionsIn.ifcParams) ifc
                       ]

   instance ConceptStructure Declaration where
    concs         d = concs (sign d)
    expressionsIn _ = fatal 148 "expressionsIn not allowed on Declaration"

   instance ConceptStructure Rule where
    concs r   = concs (rrexp r) ++ concs (rrviol r)
    expressionsIn r = foldr (uni) []
                     [ (expressionsIn.rrexp ) r
                     , (expressionsIn.rrviol) r
                     ]
   
   instance ConceptStructure (PairView Expression) where
    concs         (PairView ps) = concs         ps
    expressionsIn (PairView ps) = expressionsIn ps
     
   instance ConceptStructure (PairViewSegment Expression) where
    concs       (PairViewText _)  = []
    concs       (PairViewExp _ x) = concs x
    expressionsIn    (PairViewText _)  = []
    expressionsIn    (PairViewExp _ x) = expressionsIn x
     
   instance ConceptStructure A_Gen where
    concs g@Isa{}  = nub [gengen g,genspc g]  
    concs g@IsE{}  = nub (genspc g: genrhs g)
    expressionsIn _ = fatal 160 "expressionsIn not allowed on A_Gen"
    
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
module DatabaseDesign.Ampersand.Classes.ConceptStructure          (ConceptStructure(..), prim2rel)
where
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree       
   import DatabaseDesign.Ampersand.Core.ParseTree (ConceptDef(..))
   import DatabaseDesign.Ampersand.Basics
   import Data.List
   import Data.Maybe
   import DatabaseDesign.Ampersand.ADL1.Expression
   import Prelude hiding (Ordering(..))
   fatal :: Int -> String -> a
   fatal = fatalMsg "Classes.ConceptStructure"

   class ConceptStructure a where
    concs ::    a -> [A_Concept]       -- ^ the set of all concepts used in data structure a
    relsUsedIn :: a -> [Declaration]        -- ^ the set of all declaratons used within data structure a. `used within` means that there is a relation that refers to that declaration.
    relsUsedIn a = [ d | d@Sgn{}<-relsMentionedIn a]++[Isn c | c<-concs a]
    relsMentionedIn :: a -> [Declaration]        -- ^ the set of all declaratons used within data structure a. `used within` means that there is a relation that refers to that declaration.
    relsMentionedIn = nub . map prim2rel . primsMentionedIn
    primsMentionedIn :: a -> [Expression]
    primsMentionedIn = nub . concatMap primitives . expressionsIn
    expressionsIn :: a -> [Expression] -- ^The set of all expressions within data structure a 
    mp1Exprs :: a -> [Expression]     -- ^ the set of all EMp1 expressions within data structure a (needed to get the atoms of these relations into the populationtable)
    mp1Exprs = filter isMp1.primsMentionedIn
    -- | mp1Pops draws the population from singleton expressions.
    mp1Pops :: a -> [Population]
    mp1Pops struc
     = [ PCptPopu{ popcpt = cpt (head cl), popas = map atm cl } | cl<-eqCl cpt (mp1Exprs struc)]
       where cpt (EMp1 _ c) = c
             cpt _          = fatal 31 "cpt error"
             atm (EMp1 a _) = a
             atm _          = fatal 31 "atm error"

   prim2rel :: Expression -> Declaration
   prim2rel e
    = case e of
       EDcD d@Sgn{} -> d
       EDcD{}       -> fatal 23 "invalid declaration in EDcD{}" 
       EDcI c       -> Isn c
       EDcV sgn     -> Vs sgn
       EMp1 _ c     -> Isn c
       _            -> fatal 40 $ "only primitive expressions should be found here.\nHere we see: " ++ show e

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
    concs ctx = foldr uni []
                [ (concs.ctxpats) ctx
                , (concs.ctxprocs) ctx
                , (concs.ctxrs) ctx
                , (concs.ctxds) ctx
                , (concs.ctxpopus) ctx
                , (concs.ctxcds) ctx
                , (concs.ctxks) ctx
                , (concs.ctxvs) ctx
                , (concs.ctxgs) ctx
                , (concs.ctxifcs) ctx
                , (concs.ctxps) ctx
                , (concs.ctxsql) ctx
                , (concs.ctxphp) ctx
                ]
    expressionsIn ctx = foldr uni []
                        [ (expressionsIn.ctxpats) ctx
                        , (expressionsIn.ctxprocs) ctx
                        , (expressionsIn.ctxifcs) ctx
                        , (expressionsIn.ctxrs) ctx
                        , (expressionsIn.ctxks) ctx
                        , (expressionsIn.ctxvs) ctx
                        , (expressionsIn.ctxsql) ctx
                        , (expressionsIn.ctxphp) ctx
                        ]

   instance ConceptStructure IdentityDef where
    concs       identity   = [idCpt identity] `uni` concs [objDef | IdentityExp objDef <- identityAts identity]
    expressionsIn identity = expressionsIn             [objDef | IdentityExp objDef <- identityAts identity]

   instance ConceptStructure ViewDef where
    concs       vd = [vdcpt vd] `uni` concs [objDef | ViewExp objDef <- vdats vd]
    expressionsIn vd = expressionsIn        [objDef | ViewExp objDef <- vdats vd]

   instance ConceptStructure Expression where
    concs (EDcI c    ) = [c]
    concs (EEps i sgn) = nub (i:concs sgn)
    concs (EDcV   sgn) = concs sgn
    concs (EMp1 _ c  ) = [c]
    concs e            = foldrMapExpression uni concs [] e
    expressionsIn e = [e]


   instance ConceptStructure A_Concept where
    concs         c = [c]
    expressionsIn _ = []

   instance ConceptStructure ConceptDef where
    concs        cd = [PlainConcept (cdcpt cd)]
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
    concs pat = foldr uni []
                [ (concs.ptrls) pat
                , (concs.ptgns) pat
                , (concs.ptdcs) pat
                , (concs.ptups) pat
                , (concs.ptids) pat
                , (concs.ptxps) pat
                ]
    expressionsIn p = foldr (uni) []
                       [ (expressionsIn.ptrls) p
                       , (expressionsIn.ptids) p
                       , (expressionsIn.ptvds) p
                       ]


   instance ConceptStructure Process where
    concs prc = foldr uni []
                [ (concs.prcRules) prc
                , (concs.prcGens) prc
                , (concs.prcDcls) prc
                , (concs.prcUps) prc
                , (concs.prcIds) prc
                , (concs.prcVds) prc
                , (concs.prcXps) prc
                ]
    expressionsIn p = foldr (uni) []
                       [ (expressionsIn.prcRules) p
                       , (expressionsIn.prcIds) p
                       , (expressionsIn.prcVds) p
                       ]

   instance ConceptStructure Interface where
    concs         ifc = concs (ifcObj ifc)
    expressionsIn ifc = foldr (uni) []
                       [ (expressionsIn.ifcObj) ifc
                       , (expressionsIn.ifcParams) ifc
                       ]

   instance ConceptStructure Declaration where
    concs         d = concs (sign d)
    expressionsIn _ = fatal 148 "expressionsIn not allowed on Declaration"

   instance ConceptStructure Rule where
    concs r   = concs (rrexp r) `uni` concs (rrviol r)
    expressionsIn r = foldr (uni) []
                     [ (expressionsIn.rrexp ) r
                     , (expressionsIn.rrviol) r
                     ]
   
   instance ConceptStructure (PairView Expression) where
    concs         (PairView ps) = concs         ps
    expressionsIn (PairView ps) = expressionsIn ps

   instance ConceptStructure Population where
    concs pop@PRelPopu{} = concs (popsgn pop) -- The concepts of the specific (not the generic) signature are the concepts of this population.
    concs pop@PCptPopu{} = concs (popcpt pop)
    expressionsIn _    = []

   instance ConceptStructure Purpose where
    concs pop@Expl{} = concs (explObj pop)
    expressionsIn _ = []

   instance ConceptStructure ExplObj where
    concs (ExplConceptDef cd) = concs cd
    concs (ExplDeclaration d) = concs d
    concs (ExplRule _)        = [{-beware of loops...-}] 
    concs (ExplIdentityDef _) = [{-beware of loops...-}]
    concs (ExplViewDef _)     = [{-beware of loops...-}]
    concs (ExplPattern _)     = [{-beware of loops...-}]
    concs (ExplProcess _)     = [{-beware of loops...-}]
    concs (ExplInterface _)   = [{-beware of loops...-}]
    concs (ExplContext _)     = [{-beware of loops...-}]
    
    expressionsIn _ = []

   instance ConceptStructure (PairViewSegment Expression) where
    concs       (PairViewText _)  = []
    concs       (PairViewExp _ x) = concs x
    expressionsIn    (PairViewText _)  = []
    expressionsIn    (PairViewExp _ x) = expressionsIn x
     
   instance ConceptStructure A_Gen where
    concs g@Isa{}  = nub [gengen g,genspc g]  
    concs g@IsE{}  = nub (genspc g: genrhs g)
    expressionsIn _ = fatal 160 "expressionsIn not allowed on A_Gen"
    
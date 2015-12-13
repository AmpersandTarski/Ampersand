{-# LANGUAGE FlexibleInstances #-}
module Database.Design.Ampersand.Classes.ConceptStructure (ConceptStructure(..)) where      

import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.Basics
import Data.List
import Data.Maybe
import Database.Design.Ampersand.ADL1.Expression(primitives,isMp1,foldrMapExpression)
import Database.Design.Ampersand.Classes.ViewPoint
import Prelude hiding (Ordering(..))

{- TODO: Interface parameters (of type Declaration) are returned as Expressions by expressionsIn, to preserve the meaning of relsMentionedIn
   (implemented using primsMentionedIn, which calls expressionsIn). A more correct way to do this would be to not use expressionsIn, but
   define relsMentionedIn directly.
   
   Another improvement would be to factorize the prim constructors from the Expression data type, so expressionsIn won't need to be partial
   anymore.
-}

class ConceptStructure a where
  concs ::      a -> [A_Concept]               -- ^ the set of all concepts used in data structure a
  relsUsedIn :: a -> [Declaration]             -- ^ the set of all declaratons used within data structure a. `used within` means that there is a relation that refers to that declaration.
  relsUsedIn a = [ d | d@Sgn{}<-relsMentionedIn a]++[Isn c | c<-concs a]
  relsMentionedIn :: a -> [Declaration]        -- ^ the set of all declaratons used within data structure a. `used within` means that there is a relation that refers to that declaration.
  relsMentionedIn = nub . map prim2rel . primsMentionedIn
  primsMentionedIn :: a -> [Expression]
  primsMentionedIn = nub . concatMap primitives . expressionsIn
  expressionsIn :: a -> [Expression] -- ^ The set of all expressions within data structure a
  
  -- | mp1Pops draws the population from singleton expressions.
  mp1Pops :: ContextInfo -> a -> [Population]
  mp1Pops ci struc
   = [ ACptPopu{ popcpt = cpt (head cl)
               , popas = map atm cl } 
     | cl<-eqCl cpt ((filter isMp1.primsMentionedIn) struc)]
     where cpt (EMp1 _ c)   = c
           cpt _            = fatal 31 "cpt error"
           atm (EMp1 val c) = safePSingleton2AAtomVal ci c val
           atm _            = fatal 31 "atm error"
           
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
  concs ctx = foldr uni [ONE, PlainConcept "SESSION"]  -- ONE and [SESSION] are allways in any context. (see https://github.com/AmpersandTarski/ampersand/issues/70)
              [ (concs.ctxpats) ctx
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
                      , (expressionsIn.ctxifcs) ctx
                      , (expressionsIn.ctxrs) ctx
                      , (expressionsIn.ctxks) ctx
                      , (expressionsIn.ctxvs) ctx
                      , (expressionsIn.ctxsql) ctx
                      , (expressionsIn.ctxphp) ctx
                      , (expressionsIn.multrules) ctx
                      , (expressionsIn.identityRules) ctx
                      ]

instance ConceptStructure IdentityDef where
  concs       identity   = [idCpt identity] `uni` concs [objDef | IdentityExp objDef <- identityAts identity]
  expressionsIn identity = expressionsIn             [objDef | IdentityExp objDef <- identityAts identity]

instance ConceptStructure ViewDef where
  concs       vd = [vdcpt vd] `uni` concs [objDef | ViewExp _ objDef <- vdats vd]
  expressionsIn vd = expressionsIn        [objDef | ViewExp _ objDef <- vdats vd]

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
  concs        cd = [PlainConcept { cptnm = name cd
                                  }
                    ]
  expressionsIn _ = []

instance ConceptStructure Signature where
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
  concs (Box _ _ objs)         = concs objs
  concs InterfaceRef{}         = []
  expressionsIn (Box _ _ objs) = expressionsIn objs
  expressionsIn InterfaceRef{} = []

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

instance ConceptStructure Interface where
  concs         ifc = concs (ifcObj ifc)
  expressionsIn ifc = foldr (uni) []
                     [ (expressionsIn.ifcObj) ifc
                     , map EDcD $ ifcParams ifc -- Return param declarations as expressions
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
  concs pop@ARelPopu{} = concs (popdcl pop)
  concs pop@ACptPopu{} = concs (popcpt pop)
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
  concs (ExplInterface _)   = [{-beware of loops...-}]
  concs (ExplContext _)     = [{-beware of loops...-}]
  
  expressionsIn _ = []

instance ConceptStructure (PairViewSegment Expression) where
  concs pvs = case pvs of
      PairViewText{} -> []
      PairViewExp{}  -> concs (pvsExp pvs)
  expressionsIn pvs = case pvs of
      PairViewText{} -> []
      PairViewExp{}  -> expressionsIn (pvsExp pvs)

instance ConceptStructure A_Gen where
  concs g@Isa{}  = nub [gengen g,genspc g]
  concs g@IsE{}  = nub (genspc g: genrhs g)
  expressionsIn _ = fatal 160 "expressionsIn not allowed on A_Gen"

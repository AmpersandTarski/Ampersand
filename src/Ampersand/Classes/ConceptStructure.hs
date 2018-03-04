{-# LANGUAGE FlexibleInstances #-}
module Ampersand.Classes.ConceptStructure (ConceptStructure(..)) where      

import           Ampersand.ADL1.Expression(primitives,subExpressions,Expressions)
import           Ampersand.Basics hiding (Ordering(..))
import           Ampersand.Core.AbstractSyntaxTree
import           Ampersand.Core.ParseTree(ConceptDef)
import           Ampersand.Classes.ViewPoint
import           Data.Maybe
import qualified Data.Set as Set

class ConceptStructure a where
  concs                 :: a -> A_Concepts -- ^ the set of all concepts used in data structure a
  expressionsIn         :: a -> Expressions -- ^ The set of all expressions within data structure a
  bindedRelationsIn     :: a -> [Relation]  -- ^ the set of all declaratons used within data structure a. `used within` means that there is a relation that refers to that relation.
  bindedRelationsIn = Set.toList . Set.map fromJust . Set.filter isJust . Set.map bindedRelation . primsMentionedIn
    where 
      bindedRelation :: Expression -> Maybe Relation
      bindedRelation primExpr =
        case primExpr of
         EDcD d -> Just d
         _      -> Nothing
  primsMentionedIn      :: a -> Expressions
  primsMentionedIn = Set.unions . Set.toList . Set.map primitives . expressionsIn
  modifyablesByInsOrDel :: a -> Expressions -- ^ the set of expressions of which population could be modified directy by Insert or Delete
  modifyablesByInsOrDel = Set.filter affectedByInsOrDel . primsMentionedIn -- if primsMentionedIn contains no duplicates, neither does modifyablesByInsOrDel.
    where affectedByInsOrDel e
            = case e of
                EDcD{} -> True
                EDcI{} -> True
                EDcV{} -> True
                _      -> False

instance (ConceptStructure a,ConceptStructure b) => ConceptStructure (a, b)  where
  concs    (a,b) = concs a `uni` concs b
  expressionsIn (a,b) = expressionsIn a `uni` expressionsIn b

instance ConceptStructure a => ConceptStructure (Maybe a) where
  concs = maybe empty concs
  expressionsIn = maybe empty expressionsIn

instance ConceptStructure a => ConceptStructure [a] where
  concs     = Set.unions . map concs
  expressionsIn = foldr (uni . expressionsIn) empty
--instance ConceptStructure a => ConceptStructure (Set.Set a) where
--  concs     = Set.toList . Set.union . concs

instance ConceptStructure A_Context where
  concs ctx = Set.unions -- ONE and [SESSION] are allways in any context. (see https://github.com/AmpersandTarski/ampersand/issues/70)
              [ singleton ONE
              , singleton (makeConcept "SESSION")
              , (concs . ctxcds) ctx
              , (concs . ctxds) ctx
              , (concs . ctxgs) ctx
              , (concs . ctxifcs) ctx
              , (concs . ctxks) ctx
              , (concs . ctxpats) ctx
              , (concs . ctxphp) ctx
              , (concs . ctxpopus) ctx
              , (concs . ctxps) ctx
              , (concs . ctxrs) ctx
              , (concs . ctxsql) ctx
              , (concs . ctxvs) ctx
              ]
  expressionsIn ctx = foldr uni empty
                      [ (expressionsIn . ctxifcs) ctx
                      , (expressionsIn . ctxks) ctx
                      , (expressionsIn . ctxpats) ctx
                      , (expressionsIn . ctxphp) ctx
                      , (expressionsIn . ctxrs) ctx
                      , (expressionsIn . ctxsql) ctx
                      , (expressionsIn . ctxvs) ctx
                      , (expressionsIn . identityRules) ctx
                      , (expressionsIn . multrules) ctx
                      ]

instance ConceptStructure IdentityDef where
  concs       identity   = singleton (idCpt identity) `uni` concs [objDef | IdentityExp objDef <- identityAts identity]
  expressionsIn identity = expressionsIn             [objDef | IdentityExp objDef <- identityAts identity]

instance ConceptStructure ViewDef where
  concs         vd = singleton (vdcpt vd) `uni` concs (vdats vd)
  expressionsIn vd = expressionsIn (vdats vd)

instance ConceptStructure ViewSegment where
  concs  = concs . vsmLoad
  expressionsIn = expressionsIn . vsmLoad

instance ConceptStructure ViewSegmentPayLoad where
  concs  (ViewExp e)  = concs e
  concs  ViewText{} = empty
  expressionsIn (ViewExp e) = expressionsIn e
  expressionsIn ViewText{}  = empty
instance ConceptStructure Expression where
  concs (EDcD d    ) = concs d
  concs (EDcI c    ) = singleton c
  concs (EEps i sgn) = singleton i `uni` concs sgn
  concs (EDcV   sgn) = concs sgn
  concs (EMp1 _ c  ) = singleton c
  concs e            = concs . Set.toList . primitives $ e
  expressionsIn = subExpressions

instance ConceptStructure A_Concept where
  concs         c = singleton c
  expressionsIn _ = empty

instance ConceptStructure ConceptDef where
  concs           = singleton . makeConcept . name
  expressionsIn _ = empty

instance ConceptStructure Signature where
  concs (Sign s t) = singleton s `uni` singleton t
  expressionsIn _  = empty

instance ConceptStructure ObjectDef where
  concs     obj = (singleton . target . objExpression $ obj) `uni` concs (objmsub obj)
  expressionsIn obj = foldr uni empty
                     [ (expressionsIn . objExpression) obj
                     , (expressionsIn . objmsub) obj
                     ]

-- Note that these functions are not recursive in the case of InterfaceRefs (which is of course obvious from their types)
instance ConceptStructure SubInterface where
  concs si = case si of
              Box{} -> concs (siObjs si)
              InterfaceRef{} -> empty
  expressionsIn si = case si of
              Box{} -> expressionsIn (siObjs si)
              InterfaceRef{} -> empty

instance ConceptStructure Pattern where
  concs pat = Set.unions
              [ (concs . ptrls) pat
              , (concs . ptgns) pat
              , (concs . ptdcs) pat
              , (concs . ptups) pat
              , (concs . ptids) pat
              , (concs . ptxps) pat
              ]
  expressionsIn p = Set.unions
                     [ (expressionsIn . ptrls) p
                     , (expressionsIn . ptids) p
                     , (expressionsIn . ptvds) p
                     ]

instance ConceptStructure Interface where
  concs         = concs         . ifcObj
  expressionsIn = expressionsIn . ifcObj

instance ConceptStructure Relation where
  concs         d = concs (sign d)
  expressionsIn d = fatal ("expressionsIn not allowed on Relation of "++show d)

instance ConceptStructure Rule where
  concs r   = concs (formalExpression r) `uni` concs (rrviol r)
  expressionsIn r = foldr uni empty
                   [ (expressionsIn . formalExpression ) r
                   , (expressionsIn . rrviol) r
                   ]

instance ConceptStructure (PairView Expression) where
  concs         (PairView ps) = concs         ps
  expressionsIn (PairView ps) = expressionsIn ps

instance ConceptStructure Population where
  concs pop@ARelPopu{} = concs (popdcl pop)
  concs pop@ACptPopu{} = concs (popcpt pop)
  expressionsIn _    = empty

instance ConceptStructure Purpose where
  concs pop@Expl{} = concs (explObj pop)
  expressionsIn _ = empty

instance ConceptStructure ExplObj where
  concs (ExplConceptDef cd) = concs cd
  concs (ExplRelation d)    = concs d
  concs (ExplRule _)        = empty {-beware of loops...-}
  concs (ExplIdentityDef _) = empty {-beware of loops...-}
  concs (ExplViewDef _)     = empty {-beware of loops...-}
  concs (ExplPattern _)     = empty {-beware of loops...-}
  concs (ExplInterface _)   = empty {-beware of loops...-}
  concs (ExplContext _)     = empty {-beware of loops...-}
  
  expressionsIn _ = empty

instance ConceptStructure (PairViewSegment Expression) where
  concs pvs = case pvs of
      PairViewText{} -> empty
      PairViewExp{}  -> concs (pvsExp pvs)
  expressionsIn pvs = case pvs of
      PairViewText{} -> empty
      PairViewExp{}  -> expressionsIn (pvsExp pvs)

instance ConceptStructure A_Gen where
  concs g@Isa{}  = Set.fromList [gengen g,genspc g]
  concs g@IsE{}  = singleton (genspc g) `uni` Set.fromList (genrhs g)
  expressionsIn g = fatal ("expressionsIn not allowed on A_Gen:\n"++show g)

instance ConceptStructure Conjunct where
  concs         = concs . rc_conjunct
  expressionsIn = expressionsIn . rc_conjunct

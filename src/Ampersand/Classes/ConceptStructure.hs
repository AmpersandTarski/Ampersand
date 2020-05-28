{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Classes.ConceptStructure (ConceptStructure(..)) where      

import           Ampersand.ADL1
import           Ampersand.Basics hiding (Ordering(..))
import           Ampersand.Classes.ViewPoint
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set

class ConceptStructure a where
  concs ::             a -> A_Concepts  -- ^ the set of all concepts used in data structure a
  expressionsIn ::     a -> Expressions -- ^ The set of all expressions within data structure a
  bindedRelationsIn :: a -> Relations   -- ^ the set of all declaratons used within data structure a. `used within` means that there is a relation that refers to that relation.
  bindedRelationsIn = Set.map theBindedRel . Set.filter isBindedRelation . primsMentionedIn
    where 
      isBindedRelation :: Expression -> Bool
      isBindedRelation expr =
        case expr of
         EDcD _ -> True
         _      -> False
      theBindedRel :: Expression -> Relation
      theBindedRel expr =
        case expr of
         EDcD d -> d
         _      -> fatal $ "This function is only implemented partially, and must be called with an expression of the form BindedRelation only." <>tshow expr
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
  concs    (a,b) = concs a `Set.union` concs b
  expressionsIn (a,b) = expressionsIn a `Set.union` expressionsIn b

instance ConceptStructure a => ConceptStructure (Maybe a) where
  concs = maybe Set.empty concs
  expressionsIn = maybe Set.empty expressionsIn

instance ConceptStructure a => ConceptStructure [a] where
  concs         = Set.unions . map concs
  expressionsIn = Set.unions . map expressionsIn
instance ConceptStructure a => ConceptStructure (NE.NonEmpty a) where
  concs         = Set.unions . fmap concs
  expressionsIn = Set.unions . fmap expressionsIn
instance (Eq a ,ConceptStructure a) => ConceptStructure (Set.Set a) where
  concs         = Set.unions . map concs         . Set.elems
  expressionsIn = Set.unions . map expressionsIn . Set.elems

instance ConceptStructure A_Context where
  concs ctx = Set.unions -- ONE and [SESSION] are allways in any context. (see https://github.com/AmpersandTarski/ampersand/issues/70)
              [ Set.singleton ONE
            --  , Set.singleton (makeConcept "SESSION") --SESSION is in PrototypeContext.adl
              , (concs . ctxcds) ctx
              , (concs . ctxds) ctx
              , (concs . ctxgs) ctx
              , (concs . ctxifcs) ctx
              , (concs . ctxks) ctx
              , (concs . ctxpats) ctx
              , (concs . ctxpopus) ctx
              , (concs . ctxps) ctx
              , (concs . ctxrs) ctx
              , (concs . ctxvs) ctx
              ]
  expressionsIn ctx = Set.unions
                      [ (expressionsIn . ctxifcs) ctx
                      , (expressionsIn . ctxks) ctx
                      , (expressionsIn . ctxpats) ctx
                      , (expressionsIn . ctxrs) ctx
                      , (expressionsIn . ctxvs) ctx
                      , (expressionsIn . identityRules) ctx
                      , (expressionsIn . multrules) ctx
                      ]

instance ConceptStructure IdentityDef where
  concs identity = Set.singleton (idCpt identity) 
                    `Set.union` 
                   (concs . fmap segment . identityAts $ identity)
  expressionsIn = expressionsIn . fmap segment . identityAts
 
instance ConceptStructure ViewDef where
  concs         vd = Set.singleton (vdcpt vd) `Set.union` concs (vdats vd)
  expressionsIn vd = expressionsIn (vdats vd)

instance ConceptStructure ViewSegment where
  concs  = concs . vsmLoad
  expressionsIn = expressionsIn . vsmLoad

instance ConceptStructure ViewSegmentPayLoad where
  concs  (ViewExp e)  = concs e
  concs  ViewText{} = Set.empty
  expressionsIn (ViewExp e) = expressionsIn e
  expressionsIn ViewText{}  = Set.empty
instance ConceptStructure Expression where
  concs (EDcD d    ) = concs d
  concs (EDcI c    ) = Set.singleton c
  concs (EEps i sgn) = Set.singleton i `Set.union` concs sgn
  concs (EDcV   sgn) = concs sgn
  concs (EMp1 _ c  ) = Set.singleton c
  concs e            = concs . Set.toList . primitives $ e
  expressionsIn = subExpressions

instance ConceptStructure A_Concept where
  concs         c = Set.singleton c
  expressionsIn _ = Set.empty

instance ConceptStructure ConceptDef where
  concs _         = Set.empty -- singleton . makeConcept . name -- TODO: To do this properly, we need to separate Conceptdef into P_ConceptDef and A_ConceptDef
  expressionsIn _ = Set.empty

instance ConceptStructure Signature where
  concs (Sign s t) = Set.singleton s `Set.union` Set.singleton t
  expressionsIn _  = Set.empty

instance ConceptStructure BoxItem where
  concs (BxExpr obj) = concs obj
  concs (BxTxt _  ) = Set.empty
  expressionsIn (BxExpr obj) = expressionsIn obj
  expressionsIn (BxTxt _  ) = Set.empty
instance ConceptStructure ObjectDef where
  concs obj = (Set.singleton . target . objExpression $ obj) `Set.union` concs (objmsub obj)
  expressionsIn obj = Set.unions
                     [ (expressionsIn . objExpression) obj
                     , (expressionsIn . objmsub) obj
                     ]
-- Note that these functions are not recursive in the case of InterfaceRefs (which is of course obvious from their types)
instance ConceptStructure SubInterface where
  concs si = case si of
              Box{} -> concs (siObjs si)
              InterfaceRef{} -> Set.empty
  expressionsIn si = case si of
              Box{} -> expressionsIn (siObjs si)
              InterfaceRef{} -> Set.empty

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
  concs         = concs . expressionsIn 
  expressionsIn ifc = Set.unions
       [expressionsIn . BxExpr        . ifcObj $ ifc
       ,expressionsIn . objExpression . ifcObj $ ifc]

instance ConceptStructure Relation where
  concs         d = concs (sign d)
  expressionsIn d = fatal ("expressionsIn not allowed on Relation of "<>tshow d)

instance ConceptStructure Rule where
  concs r   = concs (formalExpression r) `Set.union` concs (rrviol r)
  expressionsIn r = Set.unions
                   [ (expressionsIn . formalExpression ) r
                   , (expressionsIn . rrviol) r
                   ]

instance ConceptStructure (PairView Expression) where
  concs         (PairView ps) = concs         . NE.toList $ ps
  expressionsIn (PairView ps) = expressionsIn . NE.toList $ ps

instance ConceptStructure Population where
  concs pop@ARelPopu{} = concs (popdcl pop)
  concs pop@ACptPopu{} = concs (popcpt pop)
  expressionsIn _    = Set.empty

instance ConceptStructure Purpose where
  concs pop@Expl{} = concs (explObj pop)
  expressionsIn _ = Set.empty

instance ConceptStructure ExplObj where
  concs (ExplConcept cpt)   = Set.singleton cpt
  concs (ExplRelation d)    = concs d
  concs (ExplRule _)        = Set.empty {-beware of loops...-}
  concs (ExplIdentityDef _) = Set.empty {-beware of loops...-}
  concs (ExplViewDef _)     = Set.empty {-beware of loops...-}
  concs (ExplPattern _)     = Set.empty {-beware of loops...-}
  concs (ExplInterface _)   = Set.empty {-beware of loops...-}
  concs (ExplContext _)     = Set.empty {-beware of loops...-}
  
  expressionsIn _ = Set.empty

instance ConceptStructure (PairViewSegment Expression) where
  concs pvs = case pvs of
      PairViewText{} -> Set.empty
      PairViewExp{}  -> concs (pvsExp pvs)
  expressionsIn pvs = case pvs of
      PairViewText{} -> Set.empty
      PairViewExp{}  -> expressionsIn (pvsExp pvs)

instance ConceptStructure AClassify where
  concs g@Isa{}  = Set.fromList [gengen g,genspc g]
  concs g@IsE{}  = Set.singleton (genspc g) `Set.union` (Set.fromList . NE.toList $ genrhs g)
  expressionsIn g = fatal ("expressionsIn not allowed on AClassify:\n"<>tshow g)

instance ConceptStructure Conjunct where
  concs         = concs . rc_conjunct
  expressionsIn = expressionsIn . rc_conjunct

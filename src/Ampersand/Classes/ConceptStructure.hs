{-# LANGUAGE FlexibleInstances #-}

module Ampersand.Classes.ConceptStructure (ConceptStructure (..), PConceptStructure (..)) where

import Ampersand.ADL1
import Ampersand.Basics hiding (Ordering (..))
import Ampersand.Classes.ViewPoint
import Ampersand.Core.ParseTree (mkPConcept)
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set

-- | Class for collecting P_Concepts from P-level structures (before type checking)
class PConceptStructure a where
  pConcs :: a -> Set.Set P_Concept

instance (PConceptStructure a, PConceptStructure b) => PConceptStructure (a, b) where
  pConcs (a, b) = pConcs a `Set.union` pConcs b

instance (PConceptStructure a) => PConceptStructure (Maybe a) where
  pConcs = maybe Set.empty pConcs

instance (PConceptStructure a) => PConceptStructure [a] where
  pConcs = Set.unions . map pConcs

instance (PConceptStructure a) => PConceptStructure (NE.NonEmpty a) where
  pConcs = Set.unions . fmap pConcs

instance (PConceptStructure a) => PConceptStructure (Set.Set a) where
  pConcs = Set.unions . map pConcs . toList

instance PConceptStructure (Term TermPrim) where
  pConcs (Prim (Pid _ c)) = Set.singleton c
  pConcs (Prim (Patm _ _ (Just c))) = Set.singleton c
  pConcs (Prim (Patm _ _ Nothing)) = Set.empty
  pConcs (Prim (Pfull _ src tgt)) = Set.fromList [src, tgt]
  pConcs (Prim (PBind _ _ c)) = Set.singleton c
  pConcs (Prim (PBin _ _)) = Set.empty  -- PBin will become PBind during type checking
  pConcs (Prim PI{}) = Set.empty
  pConcs (Prim PVee{}) = Set.empty
  pConcs (Prim PNamedR{}) = Set.empty
  pConcs (Prim PFlipped{}) = Set.empty
  pConcs (PEqu _ a b) = pConcs a `Set.union` pConcs b
  pConcs (PInc _ a b) = pConcs a `Set.union` pConcs b
  pConcs (PIsc _ a b) = pConcs a `Set.union` pConcs b
  pConcs (PUni _ a b) = pConcs a `Set.union` pConcs b
  pConcs (PDif _ a b) = pConcs a `Set.union` pConcs b
  pConcs (PLrs _ a b) = pConcs a `Set.union` pConcs b
  pConcs (PRrs _ a b) = pConcs a `Set.union` pConcs b
  pConcs (PDia _ a b) = pConcs a `Set.union` pConcs b
  pConcs (PCps _ a b) = pConcs a `Set.union` pConcs b
  pConcs (PRad _ a b) = pConcs a `Set.union` pConcs b
  pConcs (PPrd _ a b) = pConcs a `Set.union` pConcs b
  pConcs (PKl0 _ e) = pConcs e
  pConcs (PKl1 _ e) = pConcs e
  pConcs (PFlp _ e) = pConcs e
  pConcs (PCpl _ e) = pConcs e
  pConcs (PBrk _ e) = pConcs e

instance PConceptStructure (P_BoxItem TermPrim) where
  pConcs P_BoxItemTerm{obj_term = term, obj_msub = mSub} = 
    pConcs term `Set.union` pConcs mSub
  pConcs P_BxTxt{} = Set.empty

instance PConceptStructure (P_SubIfc TermPrim) where
  pConcs P_Box{si_box = boxItems} = pConcs boxItems
  pConcs P_InterfaceRef{} = Set.empty  -- Interface refs will be checked when that interface is processed

instance PConceptStructure P_Interface where
  pConcs pIfc = pConcs (ifc_Obj pIfc)

instance PConceptStructure P_Sign where
  pConcs (P_Sign src tgt) = Set.fromList [src, tgt]

instance PConceptStructure PConceptDef where
  pConcs = Set.singleton . PCpt . cdname

instance PConceptStructure PClassify where
  pConcs pCls = Set.fromList ([specific pCls] <> NE.toList (generics pCls))

-- P_Relation extracts the source and target concepts from signature
instance PConceptStructure P_Relation where
  pConcs rel = pConcs (dec_sign rel)

instance PConceptStructure (P_Rule TermPrim) where
  pConcs rule = pConcs (rr_exp rule)

-- P_ViewD extracts its view-concept and the concepts from all view segments
instance PConceptStructure (P_ViewD TermPrim) where
  pConcs vd = Set.insert (vd_cpt vd) (Set.unions (map pConcs (vd_ats vd)))

-- P_ViewSegment extracts concepts from the payload (concrete instance for TermPrim)
instance PConceptStructure (P_ViewSegment TermPrim) where
  pConcs seg = pConcs (vsm_load seg)

-- P_ViewSegmtPayLoad extracts concepts from expressions (concrete instance for TermPrim)
instance PConceptStructure (P_ViewSegmtPayLoad TermPrim) where
  pConcs (P_ViewExp term) = pConcs term
  pConcs (P_ViewText _) = Set.empty

-- P_IdentDf extracts concept and concepts from identity attributes (concrete instance for TermPrim)
instance PConceptStructure (P_IdentDf TermPrim) where
  pConcs ident = Set.insert (ix_cpt ident) (Set.unions (map pConcs (NE.toList (ix_ats ident))))

-- P_Population extracts concept from P_CptPopu case
instance PConceptStructure P_Population where
  pConcs (P_CptPopu _ cpt _) = Set.singleton cpt
  pConcs pop@P_RelPopu{} = (pConcs . p_nmdr) pop  -- Relations are validated separately

instance PConceptStructure P_NamedRel where
  pConcs pnRel = pConcs (p_mbSign pnRel)

-- PRef2Obj extracts concept from PRef2ConceptDef
instance PConceptStructure PRef2Obj where
  pConcs (PRef2ConceptDef nm) = Set.singleton (mkPConcept nm)
  pConcs _ = Set.empty

-- PPurpose extracts concepts from the reference object
instance PConceptStructure PPurpose where
  pConcs purp = pConcs (pexObj purp)


class ConceptStructure a where
  concs ::
    a ->
    -- | the set of all concepts used in data structure a
    A_Concepts
  expressionsIn ::
    a ->
    -- | The set of all expressions within data structure a
    Expressions
  bindedRelationsIn ::
    a ->
    -- | the set of all declaratons used within data structure a. `used within` means that there is a relation that refers to that relation.
    Relations
  bindedRelationsIn = Set.map theBindedRel . Set.filter isBindedRelation . primsMentionedIn
    where
      isBindedRelation :: Expression -> Bool
      isBindedRelation expr =
        case expr of
          EDcD _ -> True
          _ -> False
      theBindedRel :: Expression -> Relation
      theBindedRel expr =
        case expr of
          EDcD d -> d
          _ -> fatal $ "This function is only implemented partially, and must be called with an expression of the form BindedRelation only." <> tshow expr
  primsMentionedIn :: a -> Expressions
  primsMentionedIn = Set.unions . Set.toList . Set.map primitives . expressionsIn
  modifyablesByInsOrDel ::
    a ->
    -- | the set of expressions of which population could be modified directy by Insert or Delete
    Expressions
  modifyablesByInsOrDel = Set.filter affectedByInsOrDel . primsMentionedIn -- if primsMentionedIn contains no duplicates, neither does modifyablesByInsOrDel.
    where
      affectedByInsOrDel e =
        case e of
          EDcD {} -> True
          EDcI {} -> True
          EDcV {} -> True
          EBin {} -> True
          EEqu {} -> False
          EInc {} -> False
          EIsc {} -> False
          EUni {} -> False
          EDif {} -> False
          ELrs {} -> False
          ERrs {} -> False
          EDia {} -> False
          ECps {} -> False
          ERad {} -> False
          EPrd {} -> False
          EKl0 {} -> False
          EKl1 {} -> False
          EFlp {} -> False
          ECpl {} -> False
          EBrk {} -> False
          EMp1 {} -> False

instance (ConceptStructure a, ConceptStructure b) => ConceptStructure (a, b) where
  concs (a, b) = concs a `Set.union` concs b
  expressionsIn (a, b) = expressionsIn a `Set.union` expressionsIn b

instance (ConceptStructure a) => ConceptStructure (Maybe a) where
  concs = maybe Set.empty concs
  expressionsIn = maybe Set.empty expressionsIn

instance (ConceptStructure a) => ConceptStructure [a] where
  concs = Set.unions . map concs
  expressionsIn = Set.unions . map expressionsIn

instance (ConceptStructure a) => ConceptStructure (NE.NonEmpty a) where
  concs = Set.unions . fmap concs
  expressionsIn = Set.unions . fmap expressionsIn

instance (Eq a, ConceptStructure a) => ConceptStructure (Set.Set a) where
  concs = Set.unions . map concs . toList
  expressionsIn = Set.unions . map expressionsIn . toList

instance ConceptStructure A_Context where
  concs ctx =
    Set.unions -- ONE and [SESSION] are always in any context. (see https://github.com/AmpersandTarski/ampersand/issues/70)
      [ --  Set.singleton ONE,
        --  , Set.singleton (makeConcept "SESSION") --SESSION is in PrototypeContext.adl
        (concs . ctxcdsOutPats) ctx,
        (concs . ctxds) ctx,
        (concs . ctxgs) ctx,
        (concs . ctxifcs) ctx,
        (concs . ctxks) ctx,
        (concs . ctxpats) ctx,
        (concs . ctxpopus) ctx,
        (concs . ctxps) ctx,
        (concs . ctxrs) ctx,
        (concs . ctxvs) ctx,
        (concs . ctxEnforces) ctx
      ]
  expressionsIn ctx =
    Set.unions
      [ (expressionsIn . ctxifcs) ctx,
        (expressionsIn . ctxks) ctx,
        (expressionsIn . ctxpats) ctx,
        (expressionsIn . ctxrs) ctx,
        (expressionsIn . ctxvs) ctx,
        (expressionsIn . identityRules) ctx,
        (expressionsIn . proprules) ctx,
        (expressionsIn . enforceRules) ctx
      ]

instance ConceptStructure IdentityRule where
  concs identity =
    Set.singleton (idCpt identity)
      `Set.union` concs (identityAts identity)
  expressionsIn = Set.fromList . NE.toList . identityAts

instance ConceptStructure ViewDef where
  concs vd = Set.singleton (vdcpt vd) `Set.union` concs (vdats vd)
  expressionsIn vd = expressionsIn (vdats vd)

instance ConceptStructure AEnforce where
  concs enf = concs (enfRel enf) `Set.union` concs (enfExpr enf)
  expressionsIn enf = expressionsIn (enfExpr enf)

instance ConceptStructure ViewSegment where
  concs = concs . vsmLoad
  expressionsIn = expressionsIn . vsmLoad

instance ConceptStructure ViewSegmentPayLoad where
  concs (ViewExp e) = concs e
  concs ViewText {} = Set.empty
  expressionsIn (ViewExp e) = expressionsIn e
  expressionsIn ViewText {} = Set.empty

instance ConceptStructure Expression where
  concs (EDcD d) = concs d
  concs (EDcI c) = Set.singleton c
  concs (EBin _ sgn) = concs sgn
  concs (EDcV sgn) = concs sgn
  concs (EMp1 _ c) = Set.singleton c
  concs e = concs . Set.toList . primitives $ e
  expressionsIn = subExpressions

instance ConceptStructure A_Concept where
  concs = Set.singleton
  expressionsIn _ = Set.empty

instance ConceptStructure AConceptDef where
  concs = Set.singleton . acdcpt
  expressionsIn _ = Set.empty

instance ConceptStructure Signature where
  concs (Sign s t) = Set.singleton s `Set.union` Set.singleton t
  concs (ISgn c)   = Set.singleton c
  expressionsIn _  = Set.empty

instance ConceptStructure BoxItem where
  concs (BxExpr obj) = concs obj
  concs BxText {} = Set.empty
  expressionsIn (BxExpr obj) = expressionsIn obj
  expressionsIn BxText {} = Set.empty

instance ConceptStructure ObjectDef where
  concs obj = (Set.singleton . target . objExpression $ obj) `Set.union` concs (objmsub obj)
  expressionsIn obj =
    Set.unions
      [ (expressionsIn . objExpression) obj,
        (expressionsIn . objmsub) obj
      ]

-- Note that these functions are not recursive in the case of InterfaceRefs (which is of course obvious from their types)
instance ConceptStructure SubInterface where
  concs si = case si of
    Box {} -> concs (siObjs si)
    InterfaceRef {} -> Set.singleton (siConcept si)
  expressionsIn si = case si of
    Box {} -> expressionsIn (siObjs si)
    InterfaceRef {} -> Set.empty

instance ConceptStructure Pattern where
  concs pat =
    Set.unions
      [ (concs . ptrls) pat,
        (concs . ptgns) pat,
        (concs . ptdcs) pat,
        (concs . ptcds) pat,
        (concs . ptups) pat,
        (concs . ptids) pat,
        (concs . ptxps) pat,
        (concs . ptenfs) pat,
        (concs . ptvds) pat
      ]
  expressionsIn p =
    Set.unions
      [ (expressionsIn . ptrls) p,
        (expressionsIn . ptids) p,
        (expressionsIn . ptvds) p,
        (expressionsIn . ptenfs) p
      ]

instance ConceptStructure Interface where
  concs = concs . expressionsIn
  expressionsIn ifc =
    Set.unions
      [ expressionsIn . BxExpr . ifcObj $ ifc,
        expressionsIn . objExpression . ifcObj $ ifc
      ]

instance ConceptStructure Relation where
  concs d = concs (sign d)
  expressionsIn d = fatal ("expressionsIn not allowed on Relation of " <> tshow d)

instance ConceptStructure Rule where
  concs r = concs (formalExpression r) `Set.union` concs (rrviol r)
  expressionsIn r =
    Set.unions
      [ (expressionsIn . formalExpression) r,
        (expressionsIn . rrviol) r
      ]

instance ConceptStructure (PairView Expression) where
  concs (PairView ps) = concs . NE.toList $ ps
  expressionsIn (PairView ps) = expressionsIn . NE.toList $ ps

instance ConceptStructure Population where
  concs pop@ARelPopu {} = concs (popdcl pop)
  concs pop@ACptPopu {} = concs (popcpt pop)
  expressionsIn _ = Set.empty

instance ConceptStructure Purpose where
  concs pop@Expl {} = concs (explObj pop)
  expressionsIn _ = Set.empty

instance ConceptStructure ExplObj where
  concs (ExplConcept cpt) = Set.singleton cpt
  concs (ExplRelation d) = concs d
  concs (ExplRule _) = Set.empty {-beware of loops...-}
  concs (ExplIdentityDef _) = Set.empty {-beware of loops...-}
  concs (ExplViewDef _) = Set.empty {-beware of loops...-}
  concs (ExplPattern _) = Set.empty {-beware of loops...-}
  concs (ExplInterface _) = Set.empty {-beware of loops...-}
  concs (ExplContext _) = Set.empty {-beware of loops...-}

  expressionsIn _ = Set.empty

instance ConceptStructure (PairViewSegment Expression) where
  concs pvs = case pvs of
    PairViewText {} -> Set.empty
    PairViewExp {} -> concs (pvsExp pvs)
  expressionsIn pvs = case pvs of
    PairViewText {} -> Set.empty
    PairViewExp {} -> expressionsIn (pvsExp pvs)

instance ConceptStructure AClassify where
  concs g@Isa {} = Set.fromList [gengen g, genspc g]
  concs g@IsE {} = Set.singleton (genspc g) `Set.union` (Set.fromList . NE.toList $ genrhs g)
  expressionsIn g = fatal ("expressionsIn not allowed on AClassify:\n" <> tshow g)

instance ConceptStructure Conjunct where
  concs = concs . rcConjunct
  expressionsIn = expressionsIn . rcConjunct

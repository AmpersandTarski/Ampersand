{-# LANGUAGE DuplicateRecordFields #-}

module Ampersand.Core.A2P_Converters
  ( aAtomValue2pAtomValue,
    aConcept2pConcept,
    aCtx2pCtx,
    aExpression2pTermPrim,
    aExplObj2PRef2Obj,
    aClassify2pClassify,
    aIdentityDef2pIdentityDef,
    aObjectDef2pObjectDef,
    aRelation2pRelation,
    aProps2Pprops,
    aPopulation2pPopulation,
    aRule2pRule,
    aSign2pSign,
    aViewDef2pViewDef,
    aPattern2pPattern,
    aRoleRule2pRoleRule,
    aInterface2pInterface,
    aPairView2pPairView,
  )
where

import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Classes
import RIO.Char
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T

aCtx2pCtx :: A_Context -> P_Context
aCtx2pCtx ctx =
  PCtx
    { ctx_nm = ctxnm ctx,
      ctx_lbl = ctxlbl ctx,
      ctx_pos = ctxpos ctx,
      ctx_lang = Just $ ctxlang ctx,
      ctx_markup = Just $ ctxmarkup ctx,
      ctx_pats = map aPattern2pPattern . ctxpats $ ctx,
      ctx_rs = map aRule2pRule . toList . ctxrs $ ctx,
      ctx_ds = map aRelation2pRelation . toList . ctxds $ ctx,
      ctx_cs = map aConcDef2pConcDef $ ctxcdsOutPats ctx,
      ctx_ks = map aIdentityDef2pIdentityDef . ctxks $ ctx,
      ctx_rrules = map aRoleRule2pRoleRule . ctxrrules $ ctx,
      ctx_reprs = reprList (ctxInfo ctx),
      ctx_vs = map aViewDef2pViewDef . ctxvs $ ctx,
      ctx_gs = map aClassify2pClassify . ctxgs $ ctx,
      ctx_ifcs = map aInterface2pInterface . ctxifcs $ ctx,
      ctx_ps = map aPurpose2pPurpose . ctxps $ ctx,
      ctx_pops = map aPopulation2pPopulation . ctxpopus $ ctx,
      ctx_metas = ctxmetas ctx,
      ctx_enfs = map aEnforce2pEnforce . ctxEnforces $ ctx
    }

aEnforce2pEnforce :: AEnforce -> P_Enforce TermPrim
aEnforce2pEnforce (AEnforce orig rel op expr _ _) =
  P_Enforce
    { pos = orig,
      penfRel = PNamedR . aRelation2pNamedRel $ rel,
      penfOp = op,
      penfExpr = aExpression2pTermPrim expr
    }

aConcDef2pConcDef :: AConceptDef -> PConceptDef
aConcDef2pConcDef aCd =
  PConceptDef
    { pos = origin aCd,
      cdname = name aCd,
      cdlbl = acdlabel aCd,
      cddef2 = PCDDefNew (aMeaning2pMeaning $ acddef2 aCd),
      cdmean = map aMeaning2pMeaning $ acdmean aCd,
      cdfrom = acdfrom aCd
    }

aPattern2pPattern :: Pattern -> P_Pattern
aPattern2pPattern pat =
  P_Pat
    { pos = ptpos pat,
      pt_nm = ptnm pat,
      pt_lbl = ptlbl pat,
      pt_rls = map aRule2pRule . toList . ptrls $ pat,
      pt_gns = map aClassify2pClassify . ptgns $ pat,
      pt_dcs = map aRelation2pRelation . toList . ptdcs $ pat,
      pt_RRuls = map aRoleRule2pRoleRule . udefRoleRules $ pat,
      pt_cds = map aConcDef2pConcDef (ptcds pat),
      pt_Reprs = ptrps pat,
      pt_ids = map aIdentityDef2pIdentityDef . ptids $ pat,
      pt_vds = map aViewDef2pViewDef . ptvds $ pat,
      pt_xps = map aPurpose2pPurpose . ptxps $ pat,
      pt_pop = map aPopulation2pPopulation . ptups $ pat,
      pt_end = ptend pat,
      pt_enfs = map aEnforce2pEnforce . ptenfs $ pat
    }

aRule2pRule :: Rule -> P_Rule TermPrim
aRule2pRule rul =
  P_Rule
    { pos = rrfps rul,
      rr_nm = name rul,
      rr_lbl = rrlbl rul,
      rr_exp = aExpression2pTermPrim (formalExpression rul),
      rr_mean = map aMeaning2pMeaning (rrmean rul),
      rr_msg = map aMarkup2pMessage (rrmsg rul),
      rr_viol = fmap aPairView2pPairView (rrviol rul)
    }

aRelation2pRelation :: Relation -> P_Relation
aRelation2pRelation dcl =
  P_Relation
    { dec_nm = decnm dcl,
      dec_sign = aSign2pSign (decsgn dcl),
      dec_label = declabel dcl,
      dec_prps = aProps2Pprops $ decprps dcl,
      dec_defaults = aRelDefaults2pRelDefaults $ decDefaults dcl,
      dec_pragma = decpr dcl,
      dec_Mean = map aMeaning2pMeaning (decMean dcl),
      dec_pos = decfpos dcl
    }

aRelDefaults2pRelDefaults :: ARelDefaults -> [PRelationDefault]
aRelDefaults2pRelDefaults = map aRelDefaults2pRelDefault . toList

aRelDefaults2pRelDefault :: ARelDefault -> PRelationDefault
aRelDefaults2pRelDefault x = case x of
  ARelDefaultAtom st vals -> PDefAtom st (fmap aAtomValue2pAtomValue vals)
  ARelDefaultEvalPHP st txt -> PDefEvalPHP st txt

aProps2Pprops :: AProps -> Set PProp
aProps2Pprops aps
  | P_Sym `elem` xs && P_Asy `elem` xs =
      Set.singleton P_Prop `Set.union` (xs Set.\\ Set.fromList [P_Sym, P_Asy])
  | P_Uni `elem` xs && P_Tot `elem` xs =
      Set.singleton P_Map `Set.union` (xs Set.\\ Set.fromList [P_Uni, P_Tot])
  | P_Inj `elem` xs && P_Sur `elem` xs =
      Set.singleton P_Bij `Set.union` (xs Set.\\ Set.fromList [P_Inj, P_Sur])
  | otherwise = xs
  where
    xs = Set.map aProp2pProp aps
    aProp2pProp :: AProp -> PProp
    aProp2pProp p = case p of
      Uni -> P_Uni
      Inj -> P_Inj
      Sur -> P_Sur
      Tot -> P_Tot
      Sym -> P_Sym
      Asy -> P_Asy
      Trn -> P_Trn
      Rfx -> P_Rfx
      Irf -> P_Irf

aRelation2pNamedRel :: Relation -> P_NamedRel
aRelation2pNamedRel dcl =
  PNamedRel
    { pos = decfpos dcl,
      p_nrnm = decnm dcl,
      p_mbSign = Just . aSign2pSign $ decsgn dcl
    }

aIdentityDef2pIdentityDef :: IdentityRule -> P_IdentDf TermPrim -- P_IdentDef
aIdentityDef2pIdentityDef iDef =
  P_Id
    { pos = idPos iDef,
      ix_name = idName iDef,
      ix_label = idlabel iDef,
      ix_cpt = aConcept2pConcept (idCpt iDef),
      ix_ats = fmap aExpression2pTermPrim (identityAts iDef)
    }

aRoleRule2pRoleRule :: A_RoleRule -> P_RoleRule
aRoleRule2pRoleRule rr =
  Maintain
    { pos = arPos rr,
      mRoles = arRoles rr,
      mRules = arRules rr
    }

aViewDef2pViewDef :: ViewDef -> P_ViewDef
aViewDef2pViewDef vDef =
  P_Vd
    { pos = vdpos vDef,
      vd_nm = name vDef,
      vd_label = vdlabel vDef,
      vd_cpt = aConcept2pConcept (vdcpt vDef),
      vd_isDefault = vdIsDefault vDef,
      vd_html = vdhtml vDef,
      vd_ats = fmap aViewSegment2pP_ViewSegment (vdats vDef)
    }

aClassify2pClassify :: AClassify -> PClassify
aClassify2pClassify gen =
  case gen of
    Isa {} ->
      PClassify
        { pos = genpos gen,
          specific = aConcept2pConcept (genspc gen),
          generics = aConcept2pConcept (gengen gen) NE.:| []
        }
    IsE {} ->
      PClassify
        { pos = genpos gen,
          specific = aConcept2pConcept (genspc gen),
          generics = fmap aConcept2pConcept . genrhs $ gen
        }

aInterface2pInterface :: Interface -> P_Interface
aInterface2pInterface ifc =
  P_Ifc
    { ifc_IsAPI = ifcIsAPI ifc,
      ifc_Name = name ifc,
      ifc_lbl = ifclbl ifc,
      ifc_Roles = ifcRoles ifc,
      ifc_Obj = aObjectDef2pObjectDef (BxExpr (ifcObj ifc)),
      pos = origin ifc,
      ifc_Prp = ifcPurpose ifc
    }

aSign2pSign :: Signature -> P_Sign
aSign2pSign sgn =
  P_Sign
    { pSrc = aConcept2pConcept (source sgn),
      pTgt = aConcept2pConcept (target sgn)
    }

aConcept2pConcept :: A_Concept -> P_Concept
aConcept2pConcept cpt =
  case cpt of
    ONE             -> P_ONE
    UNION cs        -> toPcpt "\\/" cs
    ISECT cs        -> toPcpt "/\\" cs
    PlainConcept {} -> PCpt { p_cptnm = name cpt }
  where
    toPcpt sep cs
     = PCpt {p_cptnm = Name { nameParts = (toNamePartText1 . toText1Unsafe . T.intercalate sep . map tshow . toList) cs :| [],
                              nameType  = ConceptName
                            }
            }

aPurpose2pPurpose :: Purpose -> PPurpose
aPurpose2pPurpose p =
  PPurpose
    { pos = explPos p,
      pexObj = aExplObj2PRef2Obj (explObj p),
      pexMarkup = aMarkup2pMarkup (explMarkup p),
      pexRefIDs = explRefIds p
    }

aPopulation2pPopulation :: Population -> P_Population
aPopulation2pPopulation p =
  case p of
    ARelPopu {} ->
      P_RelPopu
        { pos = Origin $ "Origin is not present in Population(" <> fullName pDcl <> ") from A-Structure",
          p_nmdr = pDcl,
          p_popps = map aAtomPair2pAtomPair (toList $ popps p),
          p_src = Nothing,
          p_tgt = Nothing
        }
      where
        pDcl = aRelation2pNamedRel (popdcl p)
    ACptPopu {} ->
      P_CptPopu
        { pos = Origin $ "Origin is not present in Population(" <> fullName (popcpt p) <> ") from A-Structure",
          p_cpt = aConcept2pConcept (popcpt p),
          p_popas = map aAtomValue2pAtomValue (popas p)
        }

aObjectDef2pObjectDef :: BoxItem -> P_BoxBodyElement
aObjectDef2pObjectDef x =
  case x of
    BxExpr oDef ->
      P_BoxItemTerm
        { obj_PlainName = objPlainName oDef,
          pos = origin oDef,
          obj_lbl = objlbl oDef,
          obj_term = aExpression2pTermPrim (objExpression oDef),
          obj_crud = case objmsub oDef of
            Just (InterfaceRef _ False _) -> Nothing -- Crud specification is not allowed in combination with a reference to an interface.
            _ -> Just $ aCruds2pCruds (objcrud oDef),
          obj_mView = objmView oDef,
          obj_msub = fmap aSubIfc2pSubIfc (objmsub oDef)
        }
    BxText {} ->
      P_BxTxt
        { obj_PlainName = boxPlainName x,
          pos = origin x,
          box_txt = boxtxt x
        }

aExpression2pTermPrim :: Expression -> Term TermPrim
aExpression2pTermPrim expr =
  case expr of
    EEqu (l, r) -> PEqu o (aExpression2pTermPrim l) (aExpression2pTermPrim r)
    EInc (l, r) -> PInc o (aExpression2pTermPrim l) (aExpression2pTermPrim r)
    EIsc (l, r) -> PIsc o (aExpression2pTermPrim l) (aExpression2pTermPrim r)
    EUni (l, r) -> PUni o (aExpression2pTermPrim l) (aExpression2pTermPrim r)
    EDif (l, r) -> PDif o (aExpression2pTermPrim l) (aExpression2pTermPrim r)
    ELrs (l, r) -> PLrs o (aExpression2pTermPrim l) (aExpression2pTermPrim r)
    ERrs (l, r) -> PRrs o (aExpression2pTermPrim l) (aExpression2pTermPrim r)
    EDia (l, r) -> PDia o (aExpression2pTermPrim l) (aExpression2pTermPrim r)
    ECps (l, r)
      | isEEps l -> aExpression2pTermPrim r
      | isEEps r -> aExpression2pTermPrim l
      | otherwise -> PCps o (aExpression2pTermPrim l) (aExpression2pTermPrim r)
    ERad (l, r) -> PRad o (aExpression2pTermPrim l) (aExpression2pTermPrim r)
    EPrd (l, r) -> PPrd o (aExpression2pTermPrim l) (aExpression2pTermPrim r)
    EEps a _ -> aExpression2pTermPrim $ EDcI a
    EKl0 e -> PKl0 o (aExpression2pTermPrim e)
    EKl1 e -> PKl1 o (aExpression2pTermPrim e)
    EFlp e -> PFlp o (aExpression2pTermPrim e)
    ECpl e -> PCpl o (aExpression2pTermPrim e)
    EBrk e -> PBrk o (aExpression2pTermPrim e)
    EDcD dcl -> Prim . PNamedR . PNamedRel (origin dcl) (name dcl) . Just . aSign2pSign . sign $ dcl
    EDcI cpt -> Prim . Pid o . aConcept2pConcept $ cpt
    EBin oper cpt -> Prim . PBind o oper . aConcept2pConcept $ cpt
    EDcV sgn -> Prim . Pfull o (aConcept2pConcept . source $ sgn) . aConcept2pConcept . target $ sgn
    EMp1 val cpt -> Prim . Patm o val . Just . aConcept2pConcept $ cpt
  where
    o = Origin $ "Origin is not present in Expression: " <> tshow expr

aMeaning2pMeaning :: Meaning -> PMeaning
aMeaning2pMeaning = PMeaning . aMarkup2pMarkup . ameaMrk

aMarkup2pMessage :: Markup -> PMessage
aMarkup2pMessage = PMessage . aMarkup2pMarkup

aMarkup2pMarkup :: Markup -> P_Markup
aMarkup2pMarkup markup =
  P_Markup
    { mLang = Just $ amLang markup,
      mFormat = Just ReST,
      mString = markup2Markdown markup
    }

aPairView2pPairView :: PairView Expression -> PairView (Term TermPrim)
aPairView2pPairView pv =
  PairView
    { ppv_segs = NE.map aPairViewSegment2pPairViewSegment (ppv_segs pv)
    }

aViewSegment2pP_ViewSegment :: ViewSegment -> P_ViewSegment TermPrim
aViewSegment2pP_ViewSegment vs =
  P_ViewSegment
    { vsm_labl = vsmlabel vs,
      pos = origin vs,
      vsm_load = aViewSegmentPayLoad2pViewSegmentPayLoad (vsmLoad vs)
    }

aViewSegmentPayLoad2pViewSegmentPayLoad :: ViewSegmentPayLoad -> P_ViewSegmtPayLoad TermPrim
aViewSegmentPayLoad2pViewSegmentPayLoad vsp =
  case vsp of
    ViewExp {} -> P_ViewExp (aExpression2pTermPrim $ vsgmExpr vsp)
    ViewText {} -> P_ViewText (vsgmTxt vsp)

aPairViewSegment2pPairViewSegment :: PairViewSegment Expression -> PairViewSegment (Term TermPrim)
aPairViewSegment2pPairViewSegment x =
  case x of
    PairViewText {} ->
      PairViewText
        { pos = origin x,
          pvsStr = pvsStr x
        }
    PairViewExp {} ->
      PairViewExp
        { pos = origin x,
          pvsSoT = pvsSoT x,
          pvsExp = aExpression2pTermPrim (pvsExp x)
        }

aExplObj2PRef2Obj :: ExplObj -> PRef2Obj
aExplObj2PRef2Obj obj =
  case obj of
    ExplConcept cpt -> PRef2ConceptDef (name cpt)
    ExplRelation rel -> PRef2Relation (aRelation2pNamedRel rel)
    ExplRule str -> PRef2Rule str
    ExplIdentityDef str -> PRef2IdentityDef str
    ExplViewDef str -> PRef2ViewDef str
    ExplPattern str -> PRef2Pattern str
    ExplInterface str -> PRef2Interface str
    ExplContext str -> PRef2Context str

aAtomPair2pAtomPair :: AAtomPair -> PAtomPair
aAtomPair2pAtomPair pr =
  PPair
    { pos = Origin "Unknown, but likely due to a meatgrinded population.",
      ppLeft = aAtomValue2pAtomValue (apLeft pr),
      ppRight = aAtomValue2pAtomValue (apRight pr)
    }

aAtomValue2pAtomValue :: AAtomValue -> PAtomValue
aAtomValue2pAtomValue AtomValueOfONE = fatal "Unexpected AtomValueOfONE in convertion to P-structure"
aAtomValue2pAtomValue val =
  case aavtyp val of
    Alphanumeric -> case val of
      AAVString {} -> ScriptString o (aavtxt val)
      _ -> fatal "Unexpected combination of value types"
    BigAlphanumeric -> case val of
      AAVString {} -> ScriptString o (aavtxt val)
      _ -> fatal "Unexpected combination of value types"
    HugeAlphanumeric -> case val of
      AAVString {} -> ScriptString o (aavtxt val)
      _ -> fatal "Unexpected combination of value types"
    Password -> case val of
      AAVString {} -> ScriptString o (aavtxt val)
      _ -> fatal "Unexpected combination of value types"
    Binary -> fatal $ tshow (aavtyp val) <> " cannot be represented in P-structure currently."
    BigBinary -> fatal $ tshow (aavtyp val) <> " cannot be represented in P-structure currently."
    HugeBinary -> fatal $ tshow (aavtyp val) <> " cannot be represented in P-structure currently."
    Date -> case val of
      AAVDate {} ->
        -- TODO: Needs rethinking. A string or a double?
        ScriptString o (showValADL val)
      _ -> fatal "Unexpected combination of value types"
    DateTime -> case val of
      AAVDateTime {} ->
        -- TODO: Needs rethinking. A string or a double?
        ScriptString o (showValADL val)
      _ -> fatal "Unexpected combination of value types"
    Integer -> case val of
      AAVInteger {} -> XlsxDouble o (fromInteger (aavint val))
      _ -> fatal "Unexpected combination of value types"
    Float -> case val of
      AAVFloat {} -> XlsxDouble o (aavflt val)
      _ -> fatal "Unexpected combination of value types"
    Boolean -> case val of
      AAVBoolean {} -> ComnBool o (aavbool val)
      _ -> fatal "Unexpected combination of value types"
    Object -> case val of
      AAVString {} -> ScriptString o (aavtxt val)
      _ -> fatal "Unexpected combination of value types"
    TypeOfOne -> fatal "Unexpected combination of value types"
  where
    o = Origin $ "Origin is not present in AAtomValue: " <> tshow val

aSubIfc2pSubIfc :: SubInterface -> P_SubInterface
aSubIfc2pSubIfc sub =
  case sub of
    Box orig _ heading objs ->
      P_Box
        { pos = orig,
          si_header = heading,
          si_box = map aObjectDef2pObjectDef objs
        }
    InterfaceRef orig isLinkto str ->
      P_InterfaceRef
        { pos = orig,
          si_isLink = isLinkto,
          si_str = str
        }

aCruds2pCruds :: Cruds -> P_Cruds
aCruds2pCruds x =
  P_Cruds
    (crudOrig x)
    ( toText1Unsafe
        . T.pack
        $ zipWith (curry f) [crudC x, crudR x, crudU x, crudD x] "crud"
    )
  where
    f :: (Bool, Char) -> Char
    f (b, c) = (if b then toUpper else toLower) c

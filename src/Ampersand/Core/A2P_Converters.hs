{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Core.A2P_Converters (
    aAtomValue2pAtomValue
  , aConcept2pConcept
  , aCtx2pCtx
  , aCpt2pCpt
  , aExpression2pTermPrim
  , aExplObj2PRef2Obj
  , aClassify2pClassify
  , aIdentityDef2pIdentityDef
  , aObjectDef2pObjectDef
  , aRelation2pRelation
  , aPopulation2pPopulation
  , aRule2pRule
  , aSign2pSign
  , aViewDef2pViewDef
  , aPattern2pPattern
  , aRoleRule2pRoleRule
  , aInterface2pInterface
) 
where
import           Ampersand.ADL1
import           Ampersand.Basics
import           RIO.Char
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T

aCtx2pCtx :: A_Context -> P_Context
aCtx2pCtx ctx = 
 PCtx { ctx_nm     = ctxnm ctx
      , ctx_pos    = ctxpos ctx
      , ctx_lang   = Just $ ctxlang ctx
      , ctx_markup = Just $ ctxmarkup ctx
      , ctx_pats   = map aPattern2pPattern . ctxpats $ ctx
      , ctx_rs     = map aRule2pRule . Set.elems . ctxrs $ ctx
      , ctx_ds     = map aRelation2pRelation . Set.elems . ctxds $ ctx
      , ctx_cs     = ctxcds ctx
      , ctx_ks     = map aIdentityDef2pIdentityDef . ctxks $ ctx
      , ctx_rrules = map aRoleRule2pRoleRule  .ctxrrules $ ctx
      , ctx_reprs  = reprList (ctxInfo ctx)
      , ctx_vs     = map aViewDef2pViewDef . ctxvs $ ctx
      , ctx_gs     = map aClassify2pClassify . ctxgs $ ctx
      , ctx_ifcs   = map aInterface2pInterface . ctxifcs $ ctx
      , ctx_ps     = mapMaybe aPurpose2pPurpose . ctxps $ ctx
      , ctx_pops   = map aPopulation2pPopulation . ctxpopus $ ctx
      , ctx_metas  = ctxmetas ctx
      }
  
aPattern2pPattern :: Pattern -> P_Pattern
aPattern2pPattern pat = 
 P_Pat { pos   = ptpos pat
       , pt_nm    = ptnm pat
       , pt_rls   = map aRule2pRule . Set.elems . ptrls $ pat
       , pt_gns   = map aClassify2pClassify . ptgns $ pat
       , pt_dcs   = map aRelation2pRelation . Set.elems . ptdcs $ pat
       , pt_RRuls = [] --TODO: should this be empty? There is nothing in the A-structure
       , pt_cds   = [] --TODO: should this be empty? There is nothing in the A-structure
       , pt_Reprs = [] --TODO: should this be empty? There is nothing in the A-structure
       , pt_ids   = map aIdentityDef2pIdentityDef . ptids $ pat
       , pt_vds   = map aViewDef2pViewDef . ptvds $ pat
       , pt_xps   = mapMaybe aPurpose2pPurpose . ptxps $ pat
       , pt_pop   = map aPopulation2pPopulation . ptups $ pat
       , pt_end   = ptend pat
       }

aRule2pRule :: Rule -> P_Rule TermPrim
aRule2pRule rul =
 P_Ru { pos  = rrfps rul
      , rr_nm   = rrnm rul
      , rr_exp  = aExpression2pTermPrim (formalExpression rul)
      , rr_mean = map aMeaning2pMeaning (rrmean rul)
      , rr_msg  = map aMarkup2pMessage (rrmsg rul)
      , rr_viol = fmap aPairView2pPairView (rrviol rul)
      }

aRelation2pRelation :: Relation -> P_Relation
aRelation2pRelation dcl = 
 P_Sgn { dec_nm     = decnm dcl
       , dec_sign   = aSign2pSign (decsgn dcl)
       , dec_prps   = decprps dcl
       , dec_pragma = [decprL dcl, decprM dcl, decprR dcl]
       , dec_Mean   = map aMeaning2pMeaning (decMean dcl)
       , pos   = decfpos dcl
       }

aRelation2pNamedRel :: Relation -> P_NamedRel
aRelation2pNamedRel dcl = PNamedRel
  { pos      = decfpos dcl
  , p_nrnm   = decnm dcl
  , p_mbSign = Just . aSign2pSign $ decsgn dcl
  }
 
aIdentityDef2pIdentityDef :: IdentityDef -> P_IdentDf TermPrim -- P_IdentDef
aIdentityDef2pIdentityDef iDef =
 P_Id { pos    = idPos iDef
      , ix_lbl = idLbl iDef
      , ix_cpt = aConcept2pConcept (idCpt iDef)
      , ix_ats = fmap aIdentitySegment2pIdentSegmnt (identityAts iDef)
      }

aRoleRule2pRoleRule :: A_RoleRule -> P_RoleRule
aRoleRule2pRoleRule rr =
 Maintain { pos    = arPos rr
          , mRoles = arRoles rr
          , mRules = arRules rr
          }

aViewDef2pViewDef :: ViewDef -> P_ViewDef
aViewDef2pViewDef vDef =
 P_Vd { pos          = vdpos vDef
      , vd_lbl       = vdlbl vDef
      , vd_cpt       = aConcept2pConcept (vdcpt vDef)
      , vd_isDefault = vdIsDefault vDef
      , vd_html      = vdhtml vDef
      , vd_ats       = fmap aViewSegment2pP_ViewSegment (vdats vDef)
      }

aClassify2pClassify :: AClassify -> PClassify
aClassify2pClassify gen =
 case gen of
  Isa{} -> PClassify 
                { pos       = genpos gen
                , specific  = aConcept2pConcept (genspc gen) 
                , generics  = aConcept2pConcept (gengen gen) NE.:| []
                }
  IsE{} -> PClassify 
                { pos      = genpos gen
                , specific = aConcept2pConcept (genspc gen) 
                , generics = fmap aConcept2pConcept . genrhs $ gen
                }

aInterface2pInterface :: Interface -> P_Interface
aInterface2pInterface ifc =
 P_Ifc { ifc_IsAPI  = ifcIsAPI ifc
       , ifc_Name   = name ifc
       , ifc_Roles  = ifcRoles ifc
       , ifc_Obj    = aObjectDef2pObjectDef (BxExpr (ifcObj ifc))
       , pos        = origin ifc
       , ifc_Prp    = ifcPrp ifc
       }


aSign2pSign :: Signature -> P_Sign
aSign2pSign sgn =
 P_Sign { pSrc = aConcept2pConcept (source sgn)
        , pTgt = aConcept2pConcept (target sgn)
        }

aConcept2pConcept :: A_Concept -> P_Concept
aConcept2pConcept cpt =
 case cpt of
   ONE            -> P_ONE
   PlainConcept{} -> PCpt { p_cptnm = name cpt
                          }

aPurpose2pPurpose :: Purpose -> Maybe PPurpose 
aPurpose2pPurpose p = 
 if explUserdefd p
 then Just
   PRef2 { pos    = explPos p
         , pexObj    = aExplObj2PRef2Obj (explObj p)
         , pexMarkup = aMarkup2pMarkup (explMarkup p)
         , pexRefIDs = explRefIds p
         }
 else Nothing 

aPopulation2pPopulation :: Population -> P_Population
aPopulation2pPopulation p =
 case p of 
  ARelPopu{} -> P_RelPopu { pos  = Origin $ "Origin is not present in Population("<>name pDcl<>") from A-Structure"
                          , p_nmdr  = pDcl
                          , p_popps = map aAtomPair2pAtomPair (Set.elems $ popps p)
                          , p_src = Nothing 
                          , p_tgt = Nothing
                          }
      where pDcl = aRelation2pNamedRel (popdcl p)
  ACptPopu{} -> P_CptPopu { pos  = Origin $ "Origin is not present in Population("<>name (popcpt p)<>") from A-Structure"
                          , p_cpt  = aCpt2pCpt (popcpt p)
                          , p_popas = map aAtomValue2pAtomValue (popas p)
                          }
aCpt2pCpt :: A_Concept -> P_Concept
aCpt2pCpt cpt = case cpt of
  PlainConcept{} -> PCpt{ p_cptnm = name cpt }
  ONE          -> P_ONE

aObjectDef2pObjectDef :: BoxItem -> P_BoxItemTermPrim
aObjectDef2pObjectDef x =
  case x of
    BxExpr oDef ->
      P_BxExpr { obj_nm    = name oDef
               , pos       = origin oDef
               , obj_ctx   = aExpression2pTermPrim (objExpression oDef)
               , obj_crud  = case objmsub oDef of 
                               Just (InterfaceRef False _) -> Nothing  -- Crud specification is not allowed in combination with a reference to an interface.
                               _ -> Just $ aCruds2pCruds (objcrud oDef)
               , obj_mView = objmView oDef
               , obj_msub  = fmap aSubIfc2pSubIfc (objmsub oDef)
               }
    BxTxt oDef ->
      P_BxTxt  { obj_nm    = name oDef
            , pos       = origin oDef
            , obj_txt   = objtxt oDef
            }
aExpression2pTermPrim :: Expression -> Term TermPrim
aExpression2pTermPrim expr = 
  case expr of
    EEqu (l,r)   -> PEqu o (aExpression2pTermPrim l) (aExpression2pTermPrim r)  
    EInc (l,r)   -> PInc o (aExpression2pTermPrim l) (aExpression2pTermPrim r)
    EIsc (l,r)   -> PIsc o (aExpression2pTermPrim l) (aExpression2pTermPrim r)
    EUni (l,r)   -> PUni o (aExpression2pTermPrim l) (aExpression2pTermPrim r)
    EDif (l,r)   -> PDif o (aExpression2pTermPrim l) (aExpression2pTermPrim r)
    ELrs (l,r)   -> PLrs o (aExpression2pTermPrim l) (aExpression2pTermPrim r)
    ERrs (l,r)   -> PRrs o (aExpression2pTermPrim l) (aExpression2pTermPrim r)
    EDia (l,r)   -> PDia o (aExpression2pTermPrim l) (aExpression2pTermPrim r)
    ECps (l,r) 
      | isEEps l  -> aExpression2pTermPrim r
      | isEEps r  -> aExpression2pTermPrim l
      | otherwise -> PCps o (aExpression2pTermPrim l) (aExpression2pTermPrim r)
    ERad (l,r)   -> PRad o (aExpression2pTermPrim l) (aExpression2pTermPrim r)
    EPrd (l,r)   -> PPrd o (aExpression2pTermPrim l) (aExpression2pTermPrim r)
    EEps a _     -> aExpression2pTermPrim $ EDcI a
    EKl0 e       -> PKl0 o (aExpression2pTermPrim e)
    EKl1 e       -> PKl1 o (aExpression2pTermPrim e)
    EFlp e       -> PFlp o (aExpression2pTermPrim e)
    ECpl e       -> PCpl o (aExpression2pTermPrim e)
    EBrk e       -> PBrk o (aExpression2pTermPrim e)
    EDcD dcl     -> Prim . PNamedR . PNamedRel (origin dcl) (name dcl) . Just . aSign2pSign . sign $ dcl
    EDcI cpt     -> Prim . Pid o . aConcept2pConcept $ cpt
    EDcV sgn     -> Prim . Pfull o (aConcept2pConcept . source $ sgn) . aConcept2pConcept . target $ sgn
    EMp1 val cpt -> Prim . Patm o val . Just . aConcept2pConcept $ cpt
  where 
   o = fatal "Origin is not present in Expression"


aMeaning2pMeaning :: Meaning -> PMeaning
aMeaning2pMeaning = PMeaning . aMarkup2pMarkup . ameaMrk

aMarkup2pMessage :: Markup -> PMessage
aMarkup2pMessage = PMessage . aMarkup2pMarkup

aMarkup2pMarkup :: Markup -> P_Markup
aMarkup2pMarkup markup =
 P_Markup  { mLang   = Just $ amLang markup
           , mFormat = Just ReST
           , mString = aMarkup2String markup
           } 

aPairView2pPairView :: PairView Expression -> PairView (Term TermPrim)
aPairView2pPairView pv =
 PairView { ppv_segs = NE.map aPairViewSegment2pPairViewSegment (ppv_segs pv)
          }

aViewSegment2pP_ViewSegment :: ViewSegment -> P_ViewSegment TermPrim
aViewSegment2pP_ViewSegment vs = P_ViewSegment
          { vsm_labl = vsmlabel vs 
          , pos      = origin vs
          , vsm_load = aViewSegmentPayLoad2pViewSegmentPayLoad (vsmLoad vs)
          }
aViewSegmentPayLoad2pViewSegmentPayLoad :: ViewSegmentPayLoad -> P_ViewSegmtPayLoad TermPrim
aViewSegmentPayLoad2pViewSegmentPayLoad vsp = 
  case vsp of 
     ViewExp{}  -> P_ViewExp (aExpression2pTermPrim $ vsgmExpr vsp)
     ViewText{} -> P_ViewText (vsgmTxt vsp)

aPairViewSegment2pPairViewSegment :: PairViewSegment Expression -> PairViewSegment (Term TermPrim)
aPairViewSegment2pPairViewSegment x =
 case x of 
  PairViewText{} -> PairViewText{ pos = origin x
                                , pvsStr = pvsStr x
                                }
  PairViewExp{}  -> PairViewExp { pos = origin x
                                , pvsSoT = pvsSoT x
                                , pvsExp = aExpression2pTermPrim (pvsExp x)
                                }

aIdentitySegment2pIdentSegmnt :: IdentitySegment -> P_IdentSegmnt TermPrim
aIdentitySegment2pIdentSegmnt (IdentityExp oDef) =
  P_IdentExp { ks_obj = aObjectDef2pObjectDef (BxExpr oDef)
             }

aExplObj2PRef2Obj :: ExplObj -> PRef2Obj
aExplObj2PRef2Obj obj =
 case obj of
  ExplConcept cpt     -> PRef2ConceptDef (name cpt)
  ExplRelation rel    -> PRef2Relation (aRelation2pNamedRel rel)
  ExplRule str        -> PRef2Rule str
  ExplIdentityDef str -> PRef2IdentityDef str
  ExplViewDef str     -> PRef2ViewDef str
  ExplPattern str     -> PRef2Pattern str
  ExplInterface str   -> PRef2Interface str
  ExplContext str     -> PRef2Context str

aAtomPair2pAtomPair :: AAtomPair -> PAtomPair
aAtomPair2pAtomPair pr =
 PPair { pos   = Origin "Unknown, but likely due to a meatgrinded population."
       , ppLeft  = aAtomValue2pAtomValue (apLeft pr)
       , ppRight = aAtomValue2pAtomValue (apRight pr)
       }

aAtomValue2pAtomValue :: AAtomValue -> PAtomValue
aAtomValue2pAtomValue AtomValueOfONE = fatal "Unexpected AtomValueOfONE in convertion to P-structure"
aAtomValue2pAtomValue val =
  case aavtyp val of
    Alphanumeric     -> case val of 
                          AAVString{} -> ScriptString o (aavtxt val)
                          _         -> fatal "Unexpected combination of value types"
    BigAlphanumeric  -> case val of 
                          AAVString{} -> ScriptString o (aavtxt val)
                          _         -> fatal "Unexpected combination of value types"
    HugeAlphanumeric -> case val of 
                          AAVString{} -> ScriptString o (aavtxt val)
                          _         -> fatal "Unexpected combination of value types"
    Password         -> case val of 
                          AAVString{} -> ScriptString o (aavtxt val)
                          _         -> fatal  "Unexpected combination of value types"
    Binary           -> fatal $ tshow (aavtyp val) <> " cannot be represented in P-structure currently."
    BigBinary        -> fatal $ tshow (aavtyp val) <> " cannot be represented in P-structure currently."
    HugeBinary       -> fatal $ tshow (aavtyp val) <> " cannot be represented in P-structure currently."
    Date             -> case val of
                          AAVDate{} -> --TODO: Needs rethinking. A string or a double?
                                       ScriptString o (showValADL val)
                          _         -> fatal "Unexpected combination of value types"
    DateTime         -> case val of
                          AAVDateTime{} -> --TODO: Needs rethinking. A string or a double?
                                       ScriptString o (showValADL val)
                          _         -> fatal "Unexpected combination of value types"
    Integer          -> case val of
                          AAVInteger{} -> XlsxDouble o (fromInteger (aavint val))
                          _            -> fatal "Unexpected combination of value types"
    Float            -> case val of
                          AAVFloat{}   -> XlsxDouble o (aavflt val)
                          _            -> fatal "Unexpected combination of value types"
    Boolean          -> case val of 
                          AAVBoolean{} -> ComnBool o (aavbool val)
                          _            -> fatal "Unexpected combination of value types"
    Object           -> case val of 
                          AAVString{} -> ScriptString o (aavtxt val)
                          _         -> fatal "Unexpected combination of value types"
    TypeOfOne        -> fatal "Unexpected combination of value types"
  where
   o = fatal "Origin is not present in AAtomValue"

aSubIfc2pSubIfc :: SubInterface -> P_SubIfc TermPrim
aSubIfc2pSubIfc sub =
 case sub of
  Box _ mStr objs  
    -> P_Box          { pos   = fatal "Origin is not present in SubInterface"
                      , si_class = mStr
                      , si_box   = map aObjectDef2pObjectDef objs
                      }
  InterfaceRef isLinkto str
    -> P_InterfaceRef { pos    = fatal "Origin is not present in SubInterface"
                      , si_isLink = isLinkto
                      , si_str    = str
                      }

aCruds2pCruds :: Cruds -> P_Cruds
aCruds2pCruds x = P_Cruds (crudOrig x) (T.pack $ zipWith (curry f) [crudC x, crudR x, crudU x, crudD x] "crud")
   where f :: (Bool,Char) -> Char
         f (b,c) = (if b then toUpper else toLower) c

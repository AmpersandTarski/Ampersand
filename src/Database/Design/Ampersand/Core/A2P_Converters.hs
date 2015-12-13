module Database.Design.Ampersand.Core.A2P_Converters (
  aCtx2pCtx
  , aAtomValue2pAtomValue
) 
where
import Database.Design.Ampersand.ADL1.Expression
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Core.ParseTree
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Data.Maybe

aCtx2pCtx :: A_Context -> P_Context
aCtx2pCtx ctx = 
 PCtx { ctx_nm     = ctxnm ctx
      , ctx_pos    = ctxpos ctx
      , ctx_lang   = ctxlang ctx
      , ctx_markup = Just $ ctxmarkup ctx
      , ctx_thms   = ctxthms ctx 
      , ctx_pats   = map aPattern2pPattern . ctxpats $ ctx
      , ctx_rs     = map aRule2pRule . ctxrs $ ctx
      , ctx_ds     = map aDeclaration2pDeclaration . ctxds $ ctx
      , ctx_cs     = ctxcds ctx
      , ctx_ks     = map aIdentityDef2pIdentityDef . ctxks $ ctx
      , ctx_rrules = map aRoleRule2pRoleRule  .ctxrrules $ ctx
      , ctx_rrels  = map aRoleRelation2pRoleRelation . ctxRRels $ ctx
      , ctx_reprs  = ctxreprs ctx
      , ctx_vs     = map aViewDef2pViewDef . ctxvs $ ctx
      , ctx_gs     = map aGen2pGen . ctxgs $ ctx
      , ctx_ifcs   = map aInterface2pInterface . ctxifcs $ ctx
      , ctx_ps     = catMaybes . map aPurpose2pPurpose . ctxps $ ctx
      , ctx_pops   = map aPopulation2pPopulation . ctxpopus $ ctx
      , ctx_sql    = map aObjectDef2pObjectDef . ctxsql $ ctx
      , ctx_php    = map aObjectDef2pObjectDef . ctxphp $ ctx
      , ctx_metas  = ctxmetas ctx
      }

aPattern2pPattern :: Pattern -> P_Pattern
aPattern2pPattern pat = 
 P_Pat { pt_pos   = ptpos pat
       , pt_nm    = ptnm pat
       , pt_rls   = map aRule2pRule (ptrls pat)
       , pt_gns   = map aGen2pGen (ptgns pat)
       , pt_dcs   = map aDeclaration2pDeclaration (ptdcs pat)
       , pt_RRuls = [] --TODO: should this be empty? There is nothing in the A-structure
       , pt_RRels = [] --TODO: should this be empty? There is nothing in the A-structure
       , pt_cds   = [] --TODO: should this be empty? There is nothing in the A-structure
       , pt_Reprs = [] --TODO: should this be empty? There is nothing in the A-structure
       , pt_ids   = map aIdentityDef2pIdentityDef (ptids pat)
       , pt_vds   = map aViewDef2pViewDef (ptvds pat)
       , pt_xps   = catMaybes . map aPurpose2pPurpose . ptxps $ pat
       , pt_pop   = map aPopulation2pPopulation . ptups $ pat
       , pt_end   = ptend pat
       }

aRule2pRule :: Rule -> P_Rule TermPrim
aRule2pRule rul =
 P_Ru { rr_fps  = rrfps rul
      , rr_nm   = rrnm rul
      , rr_exp  = aExpression2pTermPrim (rrexp rul)
      , rr_mean = aMeaning2pMeaning (rrmean rul)
      , rr_msg  = map aMarkup2pMessage (rrmsg rul)
      , rr_viol = fmap aPairView2pPairView (rrviol rul)
      }

aDeclaration2pDeclaration :: Declaration -> P_Declaration
aDeclaration2pDeclaration dcl = 
 P_Sgn { dec_nm     = decnm dcl
       , dec_sign   = aSign2pSign (decsgn dcl)
       , dec_prps   = decprps dcl
       , dec_pragma = [decprL dcl, decprM dcl, decprR dcl]
       , dec_Mean   = aMeaning2pMeaning (decMean dcl)
       , dec_popu   = [] --TODO: should this be empty? There is nothing in the A-structure
       , dec_fpos   = decfpos dcl
       , dec_plug   = decplug dcl
       }

aDeclaration2pNamedRel :: Declaration -> P_NamedRel
aDeclaration2pNamedRel dcl =
 PNamedRel (decfpos dcl) (decnm dcl) (Just (aSign2pSign (decsgn dcl)))
 
aIdentityDef2pIdentityDef :: IdentityDef -> P_IdentDf TermPrim -- P_IdentDef
aIdentityDef2pIdentityDef iDef =
 P_Id { ix_pos = idPos iDef
      , ix_lbl = idLbl iDef
      , ix_cpt = aConcept2pConcept (idCpt iDef)
      , ix_ats = map aIdentitySegment2pIdentSegmnt (identityAts iDef)
      }

aRoleRule2pRoleRule :: A_RoleRule -> P_RoleRule
aRoleRule2pRoleRule rr =
 Maintain { mPos   = arPos rr
          , mRoles = arRoles rr
          , mRules = arRules rr
          }

aRoleRelation2pRoleRelation :: A_RoleRelation -> P_RoleRelation
aRoleRelation2pRoleRelation rr =
 P_RR { rr_Pos   = rrPos rr
      , rr_Roles = rrRoles rr
      , rr_Rels  = map aDeclaration2pNamedRel (rrRels rr)
      }

aViewDef2pViewDef :: ViewDef -> P_ViewDef
aViewDef2pViewDef vDef =
 P_Vd { vd_pos       = vdpos vDef
      , vd_lbl       = vdlbl vDef
      , vd_cpt       = aConcept2pConcept (vdcpt vDef)
      , vd_isDefault = vdIsDefault vDef
      , vd_html      = vdhtml vDef
      , vd_ats       = map aViewSegment2pViewSegmt (vdats vDef)
      }

aGen2pGen :: A_Gen -> P_Gen
aGen2pGen gen =
 case gen of
  Isa{} -> PGen { gen_fp  = fatal 115 "Origin is not present in A_Gen"
                , gen_spc = aConcept2pConcept (genspc gen)
                , gen_gen = aConcept2pConcept (gengen gen)
                }
  IsE{} -> P_Cy { gen_fp  = fatal 119 "Origin is not present in A_Gen"
                , gen_spc = aConcept2pConcept (genspc gen)
                , gen_rhs = map aConcept2pConcept (genrhs gen)
                }

aInterface2pInterface :: Interface -> P_Interface
aInterface2pInterface ifc =
 P_Ifc { ifc_Name   = name ifc
       , ifc_Class  = ifcClass ifc
       , ifc_Params = map aDeclaration2pNamedRel (ifcParams ifc)
       , ifc_Args   = ifcArgs ifc
       , ifc_Roles  = ifcRoles ifc
       , ifc_Obj    = aObjectDef2pObjectDef (ifcObj ifc)
       , ifc_Pos    = ifcPos ifc
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
   ONE            -> P_Singleton
   PlainConcept{} -> PCpt { p_cptnm = cptnm cpt
                          }

aPurpose2pPurpose :: Purpose -> Maybe PPurpose 
aPurpose2pPurpose p = 
 if explUserdefd p
 then Just $
   PRef2 { pexPos    = explPos p
         , pexObj    = aExplObj2PRef2Obj (explObj p)
         , pexMarkup = aMarkup2pMarkup (explMarkup p)
         , pexRefIDs = explRefIds p
         }
 else Nothing 

aPopulation2pPopulation :: Population -> P_Population
aPopulation2pPopulation p =
 case p of 
  ARelPopu{} -> P_RelPopu { p_orig  = fatal 174 "Origin is not present in Population"
                          , p_nmdr  = aDeclaration2pNamedRel (popdcl p)
                          , p_popps = map aAtomPair2pAtomPair (popps p)
                          }
  ACptPopu{} -> P_CptPopu { p_orig  = fatal 178 "Origin is not present in Population"
                          , p_cnme  = name (popcpt p)
                          , p_popas = map aAtomValue2pAtomValue (popas p)
                          }

aObjectDef2pObjectDef :: ObjectDef -> P_ObjectDef
aObjectDef2pObjectDef oDef =
 P_Obj { obj_nm    = objnm oDef
       , obj_pos   = objpos oDef
       , obj_ctx   = aExpression2pTermPrim (objctx oDef)
       , obj_mView = objmView oDef
       , obj_msub  = fmap aSubIfc2pSubIfc (objmsub oDef)
       , obj_strs  = objstrs oDef
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
    EEps _ _     -> fatal 217 $ "EEps is only expected inside an ECps."
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
   o = fatal 199 "Origin is not present in Expression"


aMeaning2pMeaning :: AMeaning -> [PMeaning]
aMeaning2pMeaning m = map (PMeaning . aMarkup2pMarkup) (ameaMrk m)

aMarkup2pMessage :: A_Markup -> PMessage
aMarkup2pMessage m = (PMessage . aMarkup2pMarkup) m

aMarkup2pMarkup :: A_Markup -> P_Markup
aMarkup2pMarkup markup =
 P_Markup  { mLang   = Just $ amLang markup
           , mFormat = Just ReST
           , mString = aMarkup2String  ReST markup
           } 

aPairView2pPairView :: PairView Expression -> PairView (Term TermPrim)
aPairView2pPairView pv =
 PairView { ppv_segs = map aPairViewSegment2pPairViewSegment (ppv_segs pv)
          }

aPairViewSegment2pPairViewSegment :: PairViewSegment Expression -> PairViewSegment (Term TermPrim)
aPairViewSegment2pPairViewSegment segment =
 case segment of 
  PairViewText{} -> PairViewText{ pvsOrg = pvsOrg segment
                                , pvsStr = pvsStr segment
                                }
  PairViewExp{}  -> PairViewExp { pvsOrg = pvsOrg segment
                                , pvsSoT = pvsSoT segment
                                , pvsExp = aExpression2pTermPrim (pvsExp segment)
                                }

aIdentitySegment2pIdentSegmnt :: IdentitySegment -> P_IdentSegmnt TermPrim
aIdentitySegment2pIdentSegmnt (IdentityExp oDef) =
 P_IdentExp  { ks_obj = aObjectDef2pObjectDef oDef
             }

aViewSegment2pViewSegmt :: ViewSegment -> P_ViewSegmt TermPrim
aViewSegment2pViewSegmt segment =
 case segment of
  ViewExp{}  -> P_ViewExp  { vs_nr  = vsgmNr segment
                           , vs_obj = aObjectDef2pObjectDef (vsgmObj segment)
                           }
  ViewText{} -> P_ViewText { vs_nr  = vsgmNr segment
                           , vs_txt = vsgmTxt segment
                           }
  ViewHtml{} -> P_ViewHtml { vs_nr  = vsgmNr segment
                           , vs_htm = vsgmHtml segment
                           }

aExplObj2PRef2Obj :: ExplObj -> PRef2Obj
aExplObj2PRef2Obj obj =
 case obj of
  ExplConceptDef cd   -> PRef2ConceptDef (name cd)
  ExplDeclaration d   -> PRef2Declaration (aDeclaration2pNamedRel d)
  ExplRule str        -> PRef2Rule str
  ExplIdentityDef str -> PRef2IdentityDef str
  ExplViewDef str     -> PRef2ViewDef str
  ExplPattern str     -> PRef2Pattern str
  ExplInterface str   -> PRef2Interface str
  ExplContext str     -> PRef2Context str

aAtomPair2pAtomPair :: AAtomPair -> PAtomPair
aAtomPair2pAtomPair pr =
 PPair { pppos   = fatal 280 "Origin is not present in AAtomPair"
       , ppLeft  = aAtomValue2pAtomValue (apLeft pr)
       , ppRight = aAtomValue2pAtomValue (apRight pr)
       }

aAtomValue2pAtomValue :: AAtomValue -> PAtomValue
aAtomValue2pAtomValue AtomValueOfONE = fatal 286 "Unexpected AtomValueOfONE in convertion to P-structure"
aAtomValue2pAtomValue val =
  case aavtyp val of
    Alphanumeric     -> case val of 
                          AAVString{} -> ScriptString o (aavstr val)
                          _         -> fatal 291 "Unexpected combination of value types"
    BigAlphanumeric  -> case val of 
                          AAVString{} -> ScriptString o (aavstr val)
                          _         -> fatal 294 "Unexpected combination of value types"
    HugeAlphanumeric -> case val of 
                          AAVString{} -> ScriptString o (aavstr val)
                          _         -> fatal 297 "Unexpected combination of value types"
    Password         -> case val of 
                          AAVString{} -> ScriptString o (aavstr val)
                          _         -> fatal 300 "Unexpected combination of value types"
    Binary           -> fatal 293 $ show (aavtyp val) ++ " cannot be represented in P-structure currently."
    BigBinary        -> fatal 294 $ show (aavtyp val) ++ " cannot be represented in P-structure currently."
    HugeBinary       -> fatal 295 $ show (aavtyp val) ++ " cannot be represented in P-structure currently."
    Date             -> case val of
                          AAVDate{} -> --TODO: Needs rethinking. A string or a double?
                                       ScriptString o (showValADL val)
                          _         -> fatal 307 "Unexpected combination of value types"
    DateTime         -> case val of
                          AAVDateTime{} -> --TODO: Needs rethinking. A string or a double?
                                       ScriptString o (showValADL val)
                          _         -> fatal 311 "Unexpected combination of value types"
    Integer          -> case val of
                          AAVInteger{} -> XlsxDouble o (fromInteger (aavint val))
                          _            -> fatal 314 "Unexpected combination of value types"
    Float            -> case val of
                          AAVFloat{}   -> XlsxDouble o (aavflt val)
                          _            -> fatal 317 "Unexpected combination of value types"
    Boolean          -> case val of 
                          AAVBoolean{} -> ComnBool o (aavbool val)
                          _            -> fatal 320 "Unexpected combination of value types"
    Object           -> case val of 
                          AAVString{} -> ScriptString o (aavstr val)
                          _         -> fatal 323 "Unexpected combination of value types"
    TypeOfOne        -> fatal 324 "Unexpected combination of value types"
  where
   o = fatal 289 "Origin is not present in AAtomValue"

aSubIfc2pSubIfc :: SubInterface -> P_SubIfc TermPrim
aSubIfc2pSubIfc sub =
 case sub of
  Box _ mStr objs  
    -> P_Box          { si_ori   = fatal 295 "Origin is not present in SubInterface"
                      , si_class = mStr
                      , si_box   = map aObjectDef2pObjectDef objs
                      }
  InterfaceRef isLinkto str 
    -> P_InterfaceRef { si_ori    = fatal 295 "Origin is not present in SubInterface"
                      , si_isLink = isLinkto
                      , si_str    = str
                      }






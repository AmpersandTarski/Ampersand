{-# LANGUAGE FlexibleInstances #-}
module Database.Design.Ampersand.Core.ToMeta 
  (toMeta)
where
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.Core.ParseTree

-- | When dealing with meta-stuff for Ampersand, (Like makeGenerics, makeRAP), 
--   the names of Concepts should be different than 'normal', user-defined Concepts. 
--   This function modifies everything in the context to reflect that.  
toMeta :: MakeMeta a => Options -> (a -> a)
toMeta opts = 
  if metaTablesHaveUnderscore opts then makeMeta (string2Meta opts) else id

string2Meta :: Options -> String -> String
string2Meta opts str 
              = if metaTablesHaveUnderscore opts 
                then show ("__"++unquoted++"__")
                else str
  where
    unquoted
     | length str < 2 = str
     | head str == '"' && last str == '"' = reverse . tail . reverse . tail $ str
     | otherwise = str 

class MakeMeta a where
  makeMeta :: (String -> String) -> a -> a

instance MakeMeta a => MakeMeta (Maybe a) where
  makeMeta _ Nothing = Nothing
  makeMeta f (Just a) = Just (makeMeta f a) 
instance MakeMeta a => MakeMeta [a] where
  makeMeta _ [] = []
  makeMeta f xs = map (makeMeta f) xs
    
instance MakeMeta P_Context where
  makeMeta f ctx
   = PCtx{ ctx_nm    =            (ctx_nm ctx)  
         , ctx_pos   = makeMeta f (ctx_pos ctx)
         , ctx_lang  = makeMeta f (ctx_lang ctx)
         , ctx_markup= makeMeta f (ctx_markup ctx)
         , ctx_thms  =            (ctx_thms ctx)
         , ctx_pats  = makeMeta f (ctx_pats ctx)
         , ctx_rs    = makeMeta f (ctx_rs ctx)
         , ctx_ds    = makeMeta f (ctx_ds ctx)
         , ctx_cs    = makeMeta f (ctx_cs ctx)
         , ctx_ks    = makeMeta f (ctx_ks ctx)
         , ctx_rrules= makeMeta f (ctx_rrules ctx)
         , ctx_rrels = makeMeta f (ctx_rrels ctx)
         , ctx_reprs = makeMeta f (ctx_reprs ctx)
         , ctx_vs    = makeMeta f (ctx_vs ctx)
         , ctx_gs    = makeMeta f (ctx_gs ctx)
         , ctx_ifcs  = makeMeta f (ctx_ifcs ctx)
         , ctx_ps    = makeMeta f (ctx_ps ctx)
         , ctx_pops  = makeMeta f (ctx_pops ctx)
         , ctx_sql   = makeMeta f (ctx_sql ctx)
         , ctx_php   = makeMeta f (ctx_php ctx)
         , ctx_metas = makeMeta f (ctx_metas ctx)
         } 

instance MakeMeta P_Pattern where
  makeMeta f p
   = P_Pat { pt_pos = makeMeta f (pt_pos p)
           , pt_nm  =            (pt_nm p)
           , pt_rls = makeMeta f (pt_rls p)
           , pt_gns = makeMeta f (pt_gns p)
           , pt_dcs = makeMeta f (pt_dcs p)
           , pt_RRuls = makeMeta f (pt_RRuls p)
           , pt_RRels = makeMeta f (pt_RRels p)
           , pt_Reprs = makeMeta f (pt_Reprs p)
           , pt_cds = makeMeta f (pt_cds p)
           , pt_ids = makeMeta f (pt_ids p)
           , pt_vds = makeMeta f (pt_vds p)
           , pt_xps = makeMeta f (pt_xps p)
           , pt_pop = makeMeta f (pt_pop p)
           , pt_end = makeMeta f (pt_end p)
           }

instance MakeMeta Representation where
  makeMeta f rep
   = Repr { reprpos  = makeMeta f (reprpos rep)
          , reprcpts =      map f (reprcpts rep)
          , reprdom  =            (reprdom rep)
          } 
instance MakeMeta ConceptDef where
  makeMeta f cd
   = Cd  { cdpos  = makeMeta f (cdpos cd)
         , cdcpt  =          f (cdcpt cd)
         , cdplug =            (cdplug cd)
         , cddef  =            (cddef cd)
         , cdref  =            (cdref cd)
         , cdfrom =            (cdfrom cd)
         }

instance MakeMeta P_Declaration where
  makeMeta f d
   = P_Sgn { dec_nm     =          f (dec_nm d)
           , dec_sign   = makeMeta f (dec_sign d)
           , dec_prps   = makeMeta f (dec_prps d)
           , dec_pragma =          (dec_pragma d)
           , dec_Mean   = makeMeta f (dec_Mean d)
           , dec_popu   = makeMeta f (dec_popu d)
           , dec_fpos   = makeMeta f (dec_fpos d)
           , dec_plug   =            (dec_plug d)
           }

instance MakeMeta P_Sign where
  makeMeta f sgn
   = P_Sign { pSrc = makeMeta f (pSrc sgn)
            , pTgt = makeMeta f (pTgt sgn)
            }

instance MakeMeta P_Concept where
  makeMeta f cpt 
    = case cpt of
        PCpt{} -> PCpt{p_cptnm = f(p_cptnm cpt)}
        P_Singleton -> cpt
        
instance MakeMeta a => MakeMeta (P_Rule a) where
  makeMeta f rul
   = P_Ru { rr_fps  = makeMeta f (rr_fps rul)
          , rr_nm   =            (rr_nm rul)
          , rr_exp  = makeMeta f (rr_exp rul)
          , rr_mean = makeMeta f (rr_mean rul)
          , rr_msg  = makeMeta f (rr_msg rul)
          , rr_viol = makeMeta f (rr_viol rul)
          }

instance MakeMeta P_RoleRule where
  makeMeta f rr
   = Maintain
          { mPos   = makeMeta f (mPos rr)
          , mRoles =            (mRoles rr)
          , mRules =            (mRules rr)
          }
instance MakeMeta P_RoleRelation where
  makeMeta f rr
   = P_RR { rr_Pos   = makeMeta f (rr_Pos rr)
          , rr_Roles =            (rr_Roles rr)
          , rr_Rels  = makeMeta f (rr_Rels rr)
          }

instance MakeMeta P_IdentDef where
  makeMeta f ident
   = P_Id { ix_pos = makeMeta f (ix_pos ident) 
          , ix_lbl =          f (ix_lbl ident) -- TODO: HJO20150209: Not sure if it is wise to 'f' the label. Check with Michiel.
          , ix_cpt = makeMeta f (ix_cpt ident)
          , ix_ats = makeMeta f (ix_ats ident)
          }
instance MakeMeta P_IdentSegment where
  makeMeta f sgmt
   = P_IdentExp
          { ks_obj = makeMeta f (ks_obj sgmt)
          }
instance MakeMeta a => MakeMeta (P_ViewD a) where
  makeMeta f vd
   = P_Vd { vd_pos = makeMeta f (vd_pos vd) 
          , vd_lbl =            (vd_lbl vd) -- No need to meta these labels (only used as template variable names)
          , vd_cpt = makeMeta f (vd_cpt vd)
          , vd_isDefault =      (vd_isDefault vd)
          , vd_html =           (vd_html vd) -- No need to meta the html template filename or inline html code
          , vd_ats = makeMeta f (vd_ats vd)
          }

instance MakeMeta a => MakeMeta (P_ViewSegmt a) where
  makeMeta f vs
   = case vs of 
      P_ViewExp{}  -> P_ViewExp { vs_nr  = vs_nr vs
                                , vs_obj = makeMeta f (vs_obj vs)
                                }
      P_ViewText{} -> vs
      P_ViewHtml{} -> vs

instance MakeMeta P_Gen where
  makeMeta f gen
   = case gen of 
      P_Cy{} -> P_Cy { gen_fp  = makeMeta f (gen_fp gen)
                     , gen_spc = makeMeta f (gen_spc gen)
                     , gen_rhs = makeMeta f (gen_rhs gen)
                     }
      PGen{} -> PGen { gen_fp  = makeMeta f (gen_fp gen)
                     , gen_spc = makeMeta f (gen_spc gen)
                     , gen_gen = makeMeta f (gen_gen gen)
                     }
instance MakeMeta P_Interface where
  makeMeta f ifc
   = P_Ifc { ifc_Name   =            (ifc_Name ifc)
           , ifc_Class  =            (ifc_Class ifc)
           , ifc_Params = makeMeta f (ifc_Params ifc)
           , ifc_Args   =            (ifc_Args ifc)
           , ifc_Roles  =            (ifc_Roles ifc)
           , ifc_Obj    = makeMeta f (ifc_Obj ifc)
           , ifc_Pos    = makeMeta f (ifc_Pos ifc)
           , ifc_Prp    =            (ifc_Prp ifc)
           }
instance MakeMeta PPurpose where
  makeMeta f prp
   = PRef2 { pexPos    = makeMeta f (pexPos prp)
           , pexObj    = makeMeta f (pexObj prp)
           , pexMarkup = makeMeta f (pexMarkup prp)
           , pexRefIDs =            (pexRefIDs prp)
           }

instance MakeMeta P_Markup where
  makeMeta f m
   = P_Markup 
          { mLang   = makeMeta f (mLang m)
          , mFormat = makeMeta f (mFormat m)
          , mString =            (mString m)
          }
instance MakeMeta PRef2Obj where
  makeMeta f ref
   = case ref of
      PRef2ConceptDef s  -> PRef2ConceptDef (f s)
      PRef2Declaration t -> PRef2Declaration (makeMeta f t)
      PRef2Rule _        -> ref 
      PRef2IdentityDef _ -> ref 
      PRef2ViewDef _     -> ref 
      PRef2Pattern _     -> ref 
      PRef2Interface _   -> ref 
      PRef2Context _     -> ref 

instance MakeMeta PMeaning where
  makeMeta f (PMeaning m) = PMeaning (makeMeta f m)
instance MakeMeta PMessage where
  makeMeta f (PMessage m) = PMessage (makeMeta f m)
instance MakeMeta P_Population where
  makeMeta f pop 
   = case pop of
      P_RelPopu{} -> P_RelPopu { p_src   =            (p_src pop)
                               , p_tgt   =            (p_tgt pop)
                               , p_nmdr  = makeMeta f (p_nmdr pop)
                               , p_orig  = makeMeta f (p_orig pop)
                               , p_popps = makeMeta f (p_popps pop)
                               }
      P_CptPopu{} -> P_CptPopu { p_cnme  =          f (p_cnme pop)
                               , p_orig  = makeMeta f (p_orig pop)
                               , p_popas =            (p_popas pop)
                               }

instance MakeMeta Meta where
  makeMeta f m
   = Meta { mtPos  = makeMeta f (mtPos m)
          , mtObj  =            (mtObj m)
          , mtName =            (mtName m)
          , mtVal  =            (mtVal m)
          }
instance MakeMeta a => MakeMeta (P_ObjDef a) where
  makeMeta f obj
   = P_Obj { obj_nm   =          f (obj_nm obj)
           , obj_pos  = makeMeta f (obj_pos obj)
           , obj_ctx  = makeMeta f (obj_ctx obj)
           , obj_crud =            (obj_crud obj)
           , obj_mView =           (obj_mView obj)
           , obj_msub = makeMeta f (obj_msub obj)
           , obj_strs =            (obj_strs obj)
           }

instance MakeMeta a => MakeMeta (P_SubIfc a) where
  makeMeta f sub
   = case sub of
      P_Box{}          -> P_Box         { si_ori   = makeMeta f (si_ori sub)
                                        , si_class =            (si_class sub)
                                        , si_box   = makeMeta f (si_box sub)
                                        }
      P_InterfaceRef{} -> P_InterfaceRef{ si_ori   = makeMeta f (si_ori sub)
                                        , si_isLink =            si_isLink sub
                                        , si_str   =            (si_str sub)
                                        , si_crud  =            (si_crud sub)
                                        }

instance MakeMeta a => MakeMeta (PairView a) where
  makeMeta f pv
    = PairView {ppv_segs = makeMeta f (ppv_segs pv)}
instance MakeMeta a => MakeMeta (PairViewSegment a) where
  makeMeta f sgmt
    = case sgmt of
       PairViewText{} -> sgmt
       PairViewExp{}  -> sgmt{pvsExp = makeMeta f (pvsExp sgmt)}

instance MakeMeta a => MakeMeta (Term a) where
  makeMeta f t
   = case t of
       Prim a     -> Prim (makeMeta f a)
       PEqu o a b -> PEqu o (makeMeta f a) (makeMeta f b)
       PInc o a b -> PInc o (makeMeta f a) (makeMeta f b)
       PIsc o a b -> PIsc o (makeMeta f a) (makeMeta f b)
       PUni o a b -> PUni o (makeMeta f a) (makeMeta f b)
       PDif o a b -> PDif o (makeMeta f a) (makeMeta f b)
       PLrs o a b -> PLrs o (makeMeta f a) (makeMeta f b)
       PRrs o a b -> PRrs o (makeMeta f a) (makeMeta f b)
       PDia o a b -> PDia o (makeMeta f a) (makeMeta f b)
       PCps o a b -> PCps o (makeMeta f a) (makeMeta f b)
       PRad o a b -> PRad o (makeMeta f a) (makeMeta f b)
       PPrd o a b -> PPrd o (makeMeta f a) (makeMeta f b)
       PKl0 o a   -> PKl0 o (makeMeta f a)
       PKl1 o a   -> PKl1 o (makeMeta f a)
       PFlp o a   -> PFlp o (makeMeta f a)
       PCpl o a   -> PCpl o (makeMeta f a)
       PBrk o a   -> PBrk o (makeMeta f a)


instance MakeMeta TermPrim where
  makeMeta f t
   = case t of
      PI    _         -> t 
      Pid   o c       -> Pid o (makeMeta f c)
      Patm  o a c     -> Patm o a (makeMeta f c)
      PVee  _         -> t
      Pfull o src tgt -> Pfull o (makeMeta f src)(makeMeta f tgt)
      PNamedR nr      -> PNamedR (makeMeta f nr)

instance MakeMeta P_NamedRel where
  makeMeta f (PNamedRel o nm      sgn)
            = PNamedRel o (f nm) (makeMeta f sgn)
   
instance MakeMeta PAtomPair where
  makeMeta _ = id
instance MakeMeta Origin where
  makeMeta _ = id
instance MakeMeta Lang where
  makeMeta _ = id
instance MakeMeta PandocFormat where
  makeMeta _ = id
instance MakeMeta Prop where
  makeMeta _ = id






















  
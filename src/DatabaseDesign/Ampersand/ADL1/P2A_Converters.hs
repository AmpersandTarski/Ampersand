{-# OPTIONS_GHC -Wall -XRelaxedPolyRec#-}
-- -XRelaxedPolyRec required for OpenSuse, for as long as we@OpenUniversityNL use an older GHC
module DatabaseDesign.Ampersand.ADL1.P2A_Converters 
     ( pGen2aGen
     , pCpt2aCpt
     , pSign2aSign
     , pExpr2aExpr
     , pDecl2aDecl
     , pRel2aRel
     , pCtx2aCtx
     , pPat2aPat
     , pRul2aRul
     , pKDef2aKDef
     , pIFC2aIFC
     , pProc2aProc
     , pODef2aODef
     , pExpl2aExpl
     , disambiguate
     )
where

import DatabaseDesign.Ampersand.Core.ParseTree
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import Data.List(nub,intercalate)
import Text.Pandoc
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec.ShowADL
import DatabaseDesign.Ampersand.Input.ADL1.CtxError

fatal :: Int -> String -> a
fatal = fatalMsg "P2A_Converter"

pCtx2aCtx :: P_Context -> (A_Context,CtxError)
pCtx2aCtx pctx
 = (actx
   ,cxelist ( cxerrs++if nocxe(cxelist cxerrs) then nmchks else []))
   where
    actx = 
         ACtx{ ctxnm    = name pctx    -- The name of this context
             , ctxpo    = partOrder    -- the generalization relation between concepts
             , ctxpats  = pats         -- The patterns defined in this context
                                       -- Each pattern contains all user defined rules inside its scope
             , ctxprocs = procs        -- The processes defined in this context
             , ctxrs    = ctxrules
             , ctxds    = adecs        -- The declarations defined in this context, outside the scope of patterns
             , ctxdecls = uniteRels (concatMap declarations pats ++ concatMap declarations procs ++ adecs)   -- All declarations
             , ctxcs    = ctx_cs pctx  -- The concept definitions defined in this context, outside the scope of patterns
             , ctxks    = keys         -- The key definitions defined in this context, outside the scope of patterns
             , ctxgs    = agens        -- The gen definitions defined in this context, outside the scope of patterns
             , ctxifcs  = ifcs         -- The interfaces defined in this context, outside the scope of patterns
             , ctxps    = aexps        -- The pre-explanations defined in this context, outside the scope of patterns
             , ctxsql   = sqlPlugs     -- user defined sqlplugs, taken from the Ampersand script
             , ctxphp   = phpPlugs     -- user defined phpplugs, taken from the Ampersand script
             , ctxenv   = (ERel(V (Sign ONE ONE)) ,[])
             }
    cxerrs = patcxes++rulecxes++keycxes++interfacecxes++proccxes++sPlugcxes++pPlugcxes++popcxes++xplcxes
    nmchks = rulenmchk
    (partOrder,_,_) = makePartialOrder hierarchy
    hierarchy = 
       let ctx_gens = ctx_gs pctx `uni` concatMap pt_gns (ctx_pats pctx) `uni` concatMap procGens (ctx_PPrcs pctx)
       in [(a (gen_gen g),a (gen_spc g)) | g<-ctx_gens]
       where a pc = C {cptnm = p_cptnm pc
                      ,cptgE = fatal 63 "do not refer to this concept"
                      ,cptos = fatal 64 "do not refer to this concept"
                      }
    adecs = map (pDecl2aDecl actx allpops "NoPattern") (ctx_ds pctx)
    agens = map (pGen2aGen actx "NoPattern") (ctx_gs pctx)
    (aexps,   xplcxes)   = (unzip . map (pExpl2aExpl actx)             . ctx_ps   ) pctx
    (pats,    patcxes)   = (unzip . map (pPat2aPat   actx allpops)     . ctx_pats ) pctx
    (procs,   proccxes)  = (unzip . map (pProc2aProc actx allpops)     . ctx_PPrcs) pctx
    (ctxrules,rulecxes)  = (unzip . map (pRul2aRul   actx "NoPattern") . ctx_rs   ) pctx
    (keys,    keycxes)   = (unzip . map (pKDef2aKDef actx)             . ctx_ks   ) pctx
    (ifcs,interfacecxes) = (unzip . map (pIFC2aIFC   actx)             . ctx_ifcs ) pctx
    (sqlPlugs,sPlugcxes) = (unzip . map (pODef2aODef actx NoCast)      . ctx_sql  ) pctx
    (phpPlugs,pPlugcxes) = (unzip . map (pODef2aODef actx NoCast)      . ctx_php  ) pctx
    (allpops, popcxes)   = (unzip . map (pPop2aPop   actx)             . pops ) pctx
    uniteRels ds = [ d | cl<-eqClass (==) ds
                       , let d=(head cl){ decprps      = (foldr1 uni.map decprps) cl
                                        , decprps_calc = (foldr1 uni.map decprps_calc) cl
                                        , decpopu      = (foldr1 uni.map decpopu) cl
                                        }
                       ]
    pops pc
     = ctx_pops pc ++
       [ pop | pat<-ctx_pats pc,  pop<-pt_pop pat] ++
       [ pop | prc<-ctx_PPrcs pc, pop<-procPop prc]
    rulenmchk = nub [newcxe ("Rules with identical names at positions "++show(map origin rs))
                    |r<-rules actx, let rs=[r'|r'<-rules actx,name r==name r'],length rs>1]

pPat2aPat :: A_Context -> [Population] -> P_Pattern -> (Pattern, CtxError)
pPat2aPat actx pops ppat 
 = (A_Pat { ptnm  = name ppat    -- Name of this pattern
          , ptpos = pt_pos ppat  -- the position in the file in which this pattern was declared.
          , ptrls = prules       -- The user defined rules in this pattern
          , ptgns = agens        -- The generalizations defined in this pattern
          , ptdcs = adecs        -- The declarations declared in this pattern
          , ptcds = pt_cds ppat  -- The concept definitions defined in this pattern
          , ptkds = keys         -- The key definitions defined in this pattern
          , ptxps = xpls         -- The explanations of elements defined in this pattern
          }
   ,CxeOrig (cxelist (rulecxes++keycxes++xplcxes)) "pattern" (name ppat) (origin ppat) )
   where
    (prules,rulecxes) = unzip arls
    arls  = map (pRul2aRul actx (name ppat)) (pt_rls ppat)
    agens = map (pGen2aGen actx (name ppat)) (pt_gns ppat)
    adecs = map (pDecl2aDecl actx pops (name ppat)) (pt_dcs ppat)
    (keys,keycxes) = unzip akds
    akds  = map (pKDef2aKDef actx) (pt_kds ppat)
    (xpls,xplcxes) = (unzip . map (pExpl2aExpl actx) . pt_xps) ppat

pProc2aProc :: A_Context -> [Population] -> P_Process -> (Process,CtxError)
pProc2aProc actx pops pproc
 = (Proc { prcNm    = procNm pproc
         , prcPos   = procPos pproc
         , prcRules = prules
         , prcGens  = agens          -- The generalizations defined in this pattern
         , prcDcls  = adecs          -- The declarations declared in this pattern
         , prcRRuls = arruls         -- The assignment of roles to rules.
         , prcRRels = arrels         -- The assignment of roles to Relations.
         , prcCds   = procCds pproc  -- The concept definitions defined in this process
         , prcKds   = keys           -- The key definitions defined in this process
         , prcXps   = expls          -- The pre-explanations of elements defined in this process
         }
   ,CxeOrig (cxelist (rulecxes++keycxes++rrcxes++editcxes++explcxes)) "process" (name pproc) (origin pproc) )
   where
    (prules,rulecxes) = (unzip . map (pRul2aRul actx (name pproc)) . procRules) pproc
    arrels = [(rol,rel) |rr<-rrels, rol<-rrRoles rr, rel<-rrRels rr]
    (rrels,editcxes)  = (unzip . map (pRRel2aRRel actx)            . procRRels) pproc
    agens  = map (pGen2aGen actx (name pproc)) (procGens pproc)
    adecs  = map (pDecl2aDecl actx pops (name pproc)) (procDcls pproc)
    arruls = [(rol,rul) |rul<-rules actx, rr<-rruls, name rul `elem` mRules rr, rol<-mRoles rr]
    (rruls,rrcxes)    = (unzip . map (pRRul2aRRul actx)            . procRRuls) pproc
    (keys,keycxes)    = (unzip . map (pKDef2aKDef actx)            . procKds) pproc
    (expls,explcxes)  = (unzip . map (pExpl2aExpl actx)            . procXps) pproc

pRRul2aRRul :: (Language l, ConceptStructure l, Identified l) => l -> RoleRule -> (RoleRule,CtxError)
pRRul2aRRul actx prrul
 = ( prrul, CxeOrig (cxelist rrcxes) "role rule" "" (origin prrul))
   where
     rrcxes = [ newcxe ("Rule '"++r++" does not exist.")
              | r<-mRules prrul, null [rul | rul<-rules actx, name rul==r]]
     
pRRel2aRRel :: (Language l, ConceptStructure l, Identified l) => l -> P_RoleRelation -> (RoleRelation,CtxError)
pRRel2aRRel actx prrel
 = ( RR { rrRoles = rr_Roles prrel
        , rrRels  = rels
        , rrPos   = rr_Pos prrel
        }
   , CxeOrig (cxelist editcxes) "role relation" "" (origin prrel))
   where
     (rels,editcxes) = unzip [pRel2aRel actx (psign t) r | (r,t)<-rr_Rels prrel]
 
pRul2aRul :: (Language l, ConceptStructure l, Identified l) => l -> String -> P_Rule -> (Rule,CtxError)
pRul2aRul actx patname prul        -- for debugging the parser, this is a good place to put     error (show (rr_exp prul))
 = (Ru { rrnm  = rr_nm prul                 -- Name of this rule
       , rrexp = aexpr                      -- The rule expression
       , rrfps = rr_fps prul                -- Position in the Ampersand file
       , rrxpl = meanings (rr_mean prul)  -- Ampersand generated explanations (for all known languages)
       , rrtyp = sign aexpr                 -- Allocated type
       , rrdcl = Nothing                    -- The property, if this rule originates from a property on a Declaration
       , r_env = patname                    -- Name of pattern in which it was defined.
       , r_usr = True                       -- True if this rule was specified explicitly as a rule in the Ampersand script;
                                            -- False if it follows implicitly from the Ampersand script and generated by a computer
       , r_sgl = False --or [rr_nm prul `elem` map (name.snd) (prcRRuls p) | p<-ctxprocs actx]  -- True if this is a signal; False if it is an ALWAYS rule
       , srrel = -- the signal relation
                 Sgn { decnm = rr_nm prul
                     , decsgn = sign aexpr
                     , decprps = []
                     , decprps_calc = []
                     , decprL = ""
                     , decprM = ""
                     , decprR = ""
                     , decMean = ""
                     , decpopu = []
                     , decfpos = rr_fps prul
                     , deciss = True
                     , decusr = False
                     , decpat = ""
                     , decplug = True
                     }
       }
   , CxeOrig exprcxe "rule" "" (origin prul)
   )
   where (aexpr,exprcxe) = pExpr2aExpr actx NoCast (rr_exp prul)
         meanings (lang,expl) = [ Means lang (string2Blocks (defaultFlags {language=lang}) expl) | not (null expl)] --  TODO: Fix with meaning/ explanation. (related to #106)
   

-- | pKDef2aKDef checks compatibility of composition with key concept on equality
pKDef2aKDef :: (Language l, ConceptStructure l, Identified l) => l -> P_KeyDef -> (KeyDef, CtxError)
pKDef2aKDef actx pkdef
 = (Kd { kdpos = kd_pos pkdef
       , kdlbl = kd_lbl pkdef
       , kdcpt = c
       , kdats = ats
                    }
   , CxeOrig (cxelist (kdcxe:atscxes)) "key definition" "" (origin pkdef) )
   where
    (ats,atscxes)  = (unzip . map (pODef2aODef actx (SourceCast c)) . kd_ats) pkdef
    c  = pCpt2aCpt actx (kd_cpt pkdef)
    kdcxe = newcxeif (nocxe (cxelist atscxes) && length (nub (c:map (source.objctx) ats))/=1)
                     (intercalate "\n" ["The source of expression " ++ showADL (objctx x) 
                                        ++" ("++showADL (source (objctx x))++") is not equal to the key concept ("++ showADL c ++ ")."
                                       |x<-ats,source (objctx x)/=c])

-- TODO -> Does pIFC2aIFC require more checks? What is the intention of params, viols, args i.e. the interface data type?
pIFC2aIFC :: (Language l, ConceptStructure l, Identified l) => l -> P_Interface -> (Interface,CtxError)
pIFC2aIFC actx pifc 
 = (Ifc { ifcName = ifc_Name pifc
        , ifcParams = prms
        , ifcViols = fatal 206 "not implemented ifcViols"
        , ifcArgs = ifc_Args pifc
        , ifcObj = obj
        , ifcPos = ifc_Pos pifc
        , ifcExpl = ifc_Expl pifc
                    }
   , CxeOrig (cxelist (objcxe:prmcxes)) "interface" (name pifc) (origin pifc) )
   where
    (obj,objcxe)  = pODef2aODef actx NoCast (ifc_Obj pifc)
    (prms,prmcxes)  = unzip [pRel2aRel actx (psign sgn) r | (r,sgn)<-ifc_Params pifc]
-- | pODef2aODef checks compatibility of composition of expressions on equality
pODef2aODef :: (Language l, ConceptStructure l, Identified l) => l -> AutoCast -> P_ObjectDef -> (ObjectDef,CtxError)
pODef2aODef actx cast podef 
 = (Obj { objnm = obj_nm podef
        , objpos = obj_pos podef
        , objctx = expr
        , objats = ats
        , objstrs = obj_strs podef
                    }
   , CxeOrig (cxelist (odcxe:exprcxe:atscxes)) "object definition" "" (origin podef) )
   where
    (ats,atscxes)  = unzip [pODef2aODef actx (SourceCast (target expr)) at | at<-obj_ats podef]
    (expr,exprcxe)  = pExpr2aExpr actx cast (obj_ctx podef)
    odcxe = newcxeif (nocxe (cxelist (exprcxe:atscxes)) && length (nub (target expr:map (source.objctx) ats))/=1)
                     (intercalate "\n" ["The source of expression " ++ showADL (objctx x) 
                                        ++" ("++showADL (source (objctx x))++") is not compatible with the target of expression "
                                        ++ showADL expr ++ " ("++ showADL (target expr) ++ ")."
                                       |x<-ats,source (objctx x)/=target expr])

pExpl2aExpl :: A_Context -> PExplanation -> (Explanation, CtxError)
pExpl2aExpl actx pexpl
 = ( Expl { explPos   = pexPos   pexpl
          , explObj   = explobs
          , explLang  = pexLang  pexpl
          , explRefId = pexRefID pexpl
          , explCont  = [Plain [Str $ pexExpl  pexpl]]
          }
   , CxeOrig xplcxe "explanation" "" (origin pexpl))
   where (explobs,xplcxe) = pExOb2aExOb actx (pexObj   pexpl)

pExOb2aExOb :: A_Context -> PExplObj -> (ExplObj, CtxError)
pExOb2aExOb actx (PExplConceptDef str  ) = (ExplConceptDef (head cds), newcxeif(null cds)("No concept definition for '"++str++"'"))
                                            where cds = [cd | cd<-conceptDefs actx, name cd==str ]
pExOb2aExOb actx (PExplDeclaration pr t) = (ExplDeclaration (makeDeclaration rel), relcxe)
                                            where (rel,relcxe) = pRel2aRel actx (psign t) pr
pExOb2aExOb actx (PExplRule str        ) = (ExplRule (head ruls), newcxeif(null ruls)("No rule named '"++str++"'") )
                                            where ruls = [rul | rul<-rules actx, name rul==str ]
pExOb2aExOb actx (PExplKeyDef str      ) = (ExplKeyDef (head kds), newcxeif(null kds)("No key definition named '"++str++"'") )
                                            where kds = [kd | kd<-keyDefs actx, name kd==str]
pExOb2aExOb actx (PExplPattern str     ) = (ExplPattern str,   newcxeif(null[pat |pat<-patterns   actx,   name pat==str])("No pattern named '"++str++"'") )
pExOb2aExOb actx (PExplProcess str     ) = (ExplProcess str,   newcxeif(null[prc |prc<-processes  actx,  name prc==str]) ("No process named '"++str++"'") )
pExOb2aExOb actx (PExplInterface str   ) = (ExplInterface str, newcxeif(null[ifc |ifc<-interfaces actx, name ifc==str])  ("No interface named '"++str++"'") )
pExOb2aExOb actx (PExplContext str     ) = (ExplContext str,   newcxeif(name actx/=str) ("No context named '"++str++"'") )  
pExOb2aExOb actx (PExplFspc str        ) = (ExplFspc str,      newcxeif( name actx/=str)("No specification named '"++str++"'") )


pPop2aPop :: (Language l, ConceptStructure l, Identified l) => l -> P_Population -> (Population,CtxError)
pPop2aPop contxt p
 = ( Popu { popm  = prel
          , popps = p_popps p
          }
   , relcxe)
   where (prel,relcxe) = pRel2aRel contxt (p_type p) (p_popm p)

pGen2aGen :: (Language l, ConceptStructure l, Identified l) => l -> String -> P_Gen -> A_Gen
pGen2aGen contxt patNm pg
   = Gen{genfp  = gen_fp  pg
        ,gengen = pCpt2aCpt contxt (gen_gen pg)
        ,genspc = pCpt2aCpt contxt (gen_spc pg)
        ,genpat = patNm
        }
          
pSign2aSign :: (Language l, ConceptStructure l, Identified l) => l -> P_Sign -> Sign
pSign2aSign contxt (P_Sign cs) = Sign (head ts) (last ts)
  where ts = map (pCpt2aCpt contxt) cs
        
pCpt2aCpt :: (Language l, ConceptStructure l, Identified l) => l -> P_Concept -> A_Concept
pCpt2aCpt contxt pc
    = case pc of
        PCpt{} -> c 
        P_Singleton -> ONE
      where 
      c = C {cptnm = p_cptnm pc
            ,cptgE = genE contxt
            ,cptos = nub$[srcPaire p |d<-declarations contxt,p<-contents d,c <= source d]
                       ++[trgPaire p |d<-declarations contxt,p<-contents d,c <= target d]
            }

pDecl2aDecl :: (Language l, ConceptStructure l, Identified l) => l -> [Population] -> String -> P_Declaration -> Declaration
pDecl2aDecl contxt pops patname pd
 = Sgn { decnm   = dec_nm pd
       , decsgn  = pSign2aSign contxt (dec_sign pd)
       , decprps = dec_prps pd
       , decprps_calc = [] --TODO -> calculate (see ticket 119)
       , decprL  = dec_prL pd
       , decprM  = dec_prM pd
       , decprR  = dec_prR pd
       , decMean = dec_Mean pd
       , decpopu = nub$    -- All populations from the P_structure will be assembled in the decpopu field of the corresponding declaratio
                   dec_popu pd ++ 
                   concat [popps pop | pop<-pops, let ad=popm pop
                                     , name ad==name pd
                                     , relsgn ad==pSign2aSign contxt (dec_sign pd)
                                     ]
       , decfpos = dec_fpos pd 
       , deciss  = True
       , decusr  = True
       , decpat  = patname
       , decplug = dec_plug pd
       }

-- | p2a for isolated references to relations. Use pExpr2aExpr instead if relation is used in an expression.
pRel2aRel :: (Language l, ConceptStructure l, Identified l) => l -> [P_Concept] -> P_Relation -> (Relation,CtxError)
pRel2aRel contxt sgn P_V 
 = case sgn of
    [] -> (fatal 326 "Ambiguous universal relation."
                          , CxeOrig (newcxe
                                    "Ambiguous universal relation.") 
                                    "relation" "" OriginUnknown )
    [c] -> (V (Sign (pCpt2aCpt contxt c) (pCpt2aCpt contxt c)), CxeOrig cxenone "relation" "" OriginUnknown)
    [s,t] -> (V (Sign (pCpt2aCpt contxt s) (pCpt2aCpt contxt t)), CxeOrig cxenone "relation" "" OriginUnknown)
    _   -> fatal 328 "Encountered a Sign with more than two elements. This should be impossible."
pRel2aRel contxt sgn P_I 
 = case sgn of
    [] -> (fatal 331 "Ambiguous identity relation."
                          , CxeOrig (newcxe
                                    "Ambiguous identity relation.") 
                                    "relation" "" OriginUnknown )
    [c] -> (I (pCpt2aCpt contxt c), CxeOrig cxenone "relation" "" OriginUnknown)
    [s,t] -> if s==t then (I (pCpt2aCpt contxt s), CxeOrig cxenone "relation" "" OriginUnknown)
             else (fatal 337 "The identity relation must be homogeneous."
                          , CxeOrig (newcxe
                                    "The identity relation must be homogeneous.") 
                                    "relation" "" OriginUnknown )
    _   -> fatal 341 "Encountered a Sign with more than two elements. This should be impossible."
pRel2aRel contxt sgn (P_Mp1 x) 
 = case sgn of
    [] -> (fatal 343 "Ambiguous value."
                          , CxeOrig (newcxe
                                    "Ambiguous value.") 
                                    "relation" "" OriginUnknown )
    [c] -> (Mp1 x (pCpt2aCpt contxt c), CxeOrig cxenone "relation" "" OriginUnknown)
    [s,t] -> if s==t then (Mp1 x (pCpt2aCpt contxt s), CxeOrig cxenone "relation" "" OriginUnknown)
             else (fatal 349 "Ambiguous value."
                          , CxeOrig (newcxe
                                    "Ambiguous value.") 
                                    "relation" "" OriginUnknown )
    _   -> fatal 354 "Encountered a Sign with more than two elements. This should be impossible."
pRel2aRel contxt sgn prel
 = case (ds,dts,sgn,unknowncpts) of
    ( _ , _ , _ ,c:cs) -> (fatal 324 ("Unknown concept in a relation named '"++name prel++".")
                          , CxeOrig (cxelist
                                    [newcxeif(null cs)     ("Unknown concept: '"++name c++"'.")
                                    ,newcxeif(not(null cs))("Unknown concepts: '"++name c++"' and '"++name (head cs)++"'." )])
                                    "relation" "" (origin prel) )
    ([] , _ , _ , _  ) -> (fatal 329 ("Relation undeclared: '"++name prel++".")
                          , CxeOrig (newcxe
                                    ("Relation undeclared: '"++name prel++"'.")) 
                                    "relation" "" (origin prel) )
    ([d],[] ,[] , _  ) -> (makeRelation d, CxeOrig cxenone "relation" "" (origin prel))
    ([d],[] , _ , _  ) -> (fatal 334 ("Relation undeclared: '"++name prel++".")
                          , CxeOrig (newcxe
                                    ("Relation undeclared: '"++name prel++show sgn++"'."
                                     ++".\nDo you intend the one with type "++(show.sign) d++"?")) 
                                    "relation" "" (origin prel) )
    ( _ ,[d], _ , _  ) -> (makeRelation d, CxeOrig cxenone "relation" "" (origin prel))
    ( _ ,[] ,[] , _  ) -> (fatal 340 ("Ambiguous reference to a relation named: '"++name prel++".")
                          , CxeOrig (newcxe
                                    ("Ambiguous relation: '"++name prel++"'.\nUse the full relation signature."
                                     ++"\nPossible types are "++concatMap (show.sign) ds++"."))
                                    "relation" "" (origin prel) )
    ( _ ,[] , _ , _  ) -> (fatal 345 ("Illegal reference to a relation named '"++name prel++".")
                          , CxeOrig (newcxe
                                    ("Relation undeclared: '"++name prel++show sgn++"'."
                                    ++"\nPossible types are "++concatMap (show.sign) ds++".")) 
                                    "relation" "" (origin prel) )
    (_ : (_ : _), _ : (_ : _), [], []) -> fatal 350 "dts should be empty because dts=[..|.., not(null sgn), ..]"
    (_ : (_ : _), _ : (_ : _), _ : _, []) -> fatal 351 ("length dts should be at most 1 when not(null sgn)\n"++show dts)
    ([_], _ : (_ : _), _, []) -> fatal 352 "More ds than dts should be impossible due to implementation of dts i.e. dts=[d |d<-ds,condition]"

   where
    unknowncpts = nub[c |c<-sgn, pCpt2aCpt contxt c `notElem` concs contxt]
    ds  = [d | d<-declarations contxt, name d==name prel]
    dts = [d | d<-ds, not(null sgn)
                    , name (head sgn)==name (source d) &&
                      name (last sgn)==name (target d)   ]

-- | An InfExpression yields a list of alternatives that are type correct (type: [Expression]) and a list of error messages (type: [TErr]).
type InfExpression  = AutoCast -> ([Expression],[TErr])
-- | internal type to push down the type as far as known on the ERel, thus possibly with wild cards on source or target
data AutoCast = NoCast | SourceCast A_Concept | TargetCast A_Concept | Cast A_Concept A_Concept deriving (Show,Eq)
-- | AutoCast is not of class Association, but it should be flippable

-- The ordering of AutoCast is from less determined to more determined.
instance Ord AutoCast where
 Cast s t     <= Cast s' t'    = s<=s' && t<=t'
 SourceCast s <= Cast s' _     = s<=s'
 TargetCast t <= Cast _ t'     = t<=t'
 NoCast       <= _             = True
 SourceCast s <= SourceCast s' = s<=s'
 TargetCast t <= TargetCast t' = t<=t'
 _            <= _             = False

flpcast :: AutoCast -> AutoCast
flpcast NoCast = NoCast
flpcast (SourceCast x) = TargetCast x
flpcast (TargetCast x) = SourceCast x
flpcast (Cast x y) = Cast y x
--abbreviations

type TErr = String

--the type checker always returns an expression with sufficient type casts, it should remove redundant ones.
--applying the type checker on an complete, explicit typed expression is equivalent to disambiguating the expression
disambiguate :: (Language l, ConceptStructure l, Identified l) => l -> Expression -> Expression
disambiguate contxt x 
 | nocxe errs = expr 
 | otherwise = fatal 428 ("an expression must be type correct, but this one is not:\n" ++ show errs)
 where
 (expr,errs) = pExpr2aExpr contxt NoCast (f x)
 -- f transforms x to a P_Expression using full relation signatures
 f (EEqu (l,r)) = Pequ (f l,f r)
 f (EImp (l,r)) = Pimp (f l,f r)
 f (EIsc es)    = Pisc (map f es)
 f (EUni es)    = PUni (map f es)
 f (EDif (l,r)) = PDif (f l,f r)
 f (ELrs (l,r)) = PLrs (f l,f r)
 f (ERrs (l,r)) = PRrs (f l,f r)
 f (ECps es)    = PCps (map f es)
 f (ERad es)    = PRad (map f es)
 f (EKl0 e)     = PKl0 (f e)
 f (EKl1 e)     = PKl1 (f e)
 f (EFlp e)     = PFlp (f e)
 f (ECpl e)     = PCpl (f e)
 f (EBrk e)     = PBrk (f e)
 f (ETyp e _) = f e
 f (ERel rel@(Rel{})) = PTyp (Prel (P_Rel (name rel) (origin rel))) (P_Sign [g (source rel),g (target rel)])
 f (ERel rel@(I{}))   = PTyp (Prel  P_I                           ) (P_Sign [g (source rel)])
 f (ERel rel@(V{}))   = PTyp (Prel  P_V                           ) (P_Sign [g (source rel),g (target rel)])
 f (ERel rel@(Mp1{})) = PTyp (Prel (P_Mp1 (relval rel)           )) (P_Sign [g (source rel)])
 g c@(C{}) = PCpt (name c)
 g ONE     = P_Singleton

{- The story of type checking an expression.
Invariants:
 1. inference yields either one solution or else one or more error messages.

Step 1:

---------
the type checker always returns an expression with sufficient type casts.
it removes some of the redundant ones i.e. (PTyp e sgn) for which the isolated e would have been inferred to sgn anyway.
-}
pExpr2aExpr :: (Language l, ConceptStructure l, Identified l) => l -> AutoCast -> P_Expression -> (Expression, CtxError)
pExpr2aExpr contxt cast pexpr = case infer contxt pexpr cast of
   ([] ,[])   -> ( fatal 389 ("Illegal reference to expression '"++showADL pexpr++".")
                 , newcxe "Unknown type error") --should not be possible
   ([x],[])   -> if isTypeable x
                 then (x,cxenone)
                 else fatal 393 ("expression "++show x++" contains untypeable elements.")
   (xs ,[])   -> ( fatal 394 ("Illegal reference to expression '"++showADL pexpr++".")
                 , newcxe ("Ambiguous expression: "++showADL pexpr++"\nPossible types are: "++ show (map sign xs)++"."))
   (_  ,errs) -> ( fatal 396 ("Illegal reference to expression '"++showADL pexpr++".")
                 , cxelist (map newcxe errs))

-- | p2a for p_relations in a p_expression. Returns all (ERel arel) expressions matching the p_relation and AutoCast within a certain context.
pRel2aExpr :: (Language l, ConceptStructure l, Identified l) => P_Relation -> l -> InfExpression 
pRel2aExpr P_V contxt ac
 = (alts, ["The context has no concepts" |null alts])
   where
   alts = case ac of
     NoCast       -> [ERel(V (Sign a b)) |a<-concs contxt,b<-concs contxt]
     SourceCast s -> [ERel(V (Sign s b)) |b<-concs contxt]
     TargetCast t -> [ERel(V (Sign a t)) |a<-concs contxt]
     Cast s t     -> [ERel(V (Sign s t)) ]
pRel2aExpr P_I contxt ac
 = (alts, ["The context has no concepts" |null alts])
   where
   alts = case ac of
     NoCast       -> [ERel(I c) |c<-concs contxt]
     SourceCast s -> [ERel(I s)]
     TargetCast t -> [ERel(I t)]
     Cast s t     -> [ERel(I s) |s==t]
pRel2aExpr (P_Mp1 x) contxt ac
 = (alts, ["The context has no concepts" |null alts])
   where
   alts = case ac of
     NoCast       -> [ERel(Mp1 x c) |c<-concs contxt]
     SourceCast s -> [ERel(Mp1 x s)]
     TargetCast t -> [ERel(Mp1 x t)]
     Cast s t     -> [ERel(Mp1 x s) |s==t]
pRel2aExpr prel contxt ac
 = (alts, [ "Relation not declared: " ++ showADL prel |null alts]
        ++[ "Relation declaration " ++ show (name d) ++ " cannot be cast to "++show (cast d)++", because it has properties " ++ show (endomults d) ++ ", which are defined on endorelations only."
          | d<-declarations contxt, name prel==name d,let Sign s t = cast d
          , source d == target d          -- the declaration is endo (DOUBT: could this be: source d `comparable` target d ?)
          , s/=t                          -- but the enforced type is not endo
          , (not.null) (endomults d)]     -- and it has properties reserved for endorelations.
        ++[ "Relation declaration " ++ show (name d) ++ " has endoproperties " ++ show (endomults d) ++ ", which are defined on endorelations only."
          | d<-declarations contxt, name prel==name d
          , source d /= target d          -- the declaration is not endo (DOUBT: could this be: source d `comparable` target d ?)
          , (not.null) (endomults d)])    -- but it has properties reserved for endorelations.
   where
    cast d = case ac of         -- make sure the declaration satisfies the desired genericity.
     NoCast       -> Sign (source d) (target d)
     SourceCast s -> Sign s (target d)
     TargetCast t -> Sign (source d) t
     Cast s t     -> Sign s t
    alts = [ERel (arel d) |d<-declarations contxt,name d==name prel, sign d `comparable` cast d, endocheck d]
    endocheck d = null (endomults d) || source d==target d
    endomults d     = [x |x<-multiplicities d,  x `elem` endoprops]
    arel d = Rel{ relnm  = name prel
                , relpos = origin prel
                , relsgn = sign d
                , reldcl = d
                }


infer :: (Language l, ConceptStructure l, Identified l) => l
         -> P_Expression              -- ^ the expression e to be analyzed
         -> AutoCast                  -- ^ a type in which expression e is cast. (possible values: NoCast, SourceCast s, TargetCast t, Cast s t)
         -> ( [Expression], [TErr])   -- ^ all interpretations of e that are possible in this context AND
                                      --   the error messages. If there are error messages, the result may be undefined. If there are no error messages, there is precisely one interpretation, which is the result.

infer contxt (Pequ (p_l,p_r)) ac = inferEquImp contxt Pequ EEqu (p_l,p_r) ac
infer contxt (Pimp (p_l,p_r)) ac = inferEquImp contxt Pimp EImp (p_l,p_r) ac
infer contxt (PUni p_rs) ac      = inferUniIsc contxt PUni EUni True  p_rs ac -- True is used to infer contxt the most specific type.
infer contxt (Pisc p_rs) ac      = inferUniIsc contxt Pisc EIsc False p_rs ac -- False is used to infer contxt the most generic type.
infer contxt (PCps p_es) ac      = inferCpsRad contxt PCps ECps p_es ac
infer contxt (PRad p_es) ac      = inferCpsRad contxt PRad ERad p_es ac   
infer contxt (Prel rel) ac       = (nub alts, msgs) where (alts,msgs) = pRel2aExpr rel contxt ac
infer contxt (PTyp p_r psgn) _   = (alts, take 1 msgs)
    where uc = pSign2aSign contxt psgn
          (candidates,messages) = infer contxt p_r (Cast (source uc) (target uc))
          alts = {- Possibly useful for debugging:
                 if p_r==Prel P_I && psgn==P_Sign [PCpt "Bericht"]
                 then error (show e++
                             "\nuc: "++show uc++
                             "\ncandidates: "++show candidates++
                             "\nalts: "++show [ ETyp e uc | e <- candidates, sign e <= uc]) else -}
                 [ if 1==length (fst (infer contxt p_r  NoCast))
                      && uc==sign e -- would the type be the same without cast PTyp i.e. one candidate with uc==sign e?
                   then e           -- then remove this redundant PTyp
                   else ETyp e uc   -- else keep it
                 | e <- candidates, sign e <= uc]
          unknowncs = nub[c |c<-concs uc, c `notElem` concs contxt]
          msgs = ["Unknown concept: '"++name (head unknowncs)++"'." |length unknowncs == 1] ++
                 ["Unknown concepts: '"++name (head unknowncs)++"' and '"++name (last unknowncs)++"'." |length unknowncs > 1] ++
                 messages++
                 ["Ambiguous types of "++showADL p_r++" : "++(show.map sign) alts++"." | length alts>1]++
                 ["No types of "++showADL p_r++" match "++show uc++".\n Possibilities: "++(show.map sign) alts++"." | null alts]
infer contxt (PBrk r) ac      = ([EBrk e |e<-alts], messages) where (alts,messages) = infer contxt r ac
infer contxt (PFlp r) ac      = ([EFlp e |e<-alts], messages) where (alts,messages) = infer contxt r (flpcast ac)
infer contxt (PCpl r) ac      = ([ECpl e |e<-alts], messages) where (alts,messages) = infer contxt r ac
infer contxt (PKl0 r) ac      = ([EKl0 e |e<-alts], messages) where (alts,messages) = infer contxt r ac
infer contxt (PKl1 r) ac      = ([EKl1 e |e<-alts], messages) where (alts,messages) = infer contxt r ac
infer contxt (PRrs (p_l,p_r)) ac = (alts, if null deepMsgs then combMsgs else deepMsgs)
    where -- Step 1: infer contxt types of left hand side and right hand sides
           lc = case ac of
              NoCast       -> NoCast
              SourceCast s -> TargetCast s
              TargetCast _ -> NoCast
              Cast s _     -> TargetCast s
           rc = case ac of
              NoCast       -> NoCast
              SourceCast _ -> NoCast
              TargetCast t -> TargetCast t
              Cast _ t     -> TargetCast t
           (lAlts,lMsgs)=infer contxt p_l lc
           (rAlts,rMsgs)=infer contxt p_r rc
          -- Step 2: compute the viable alternatives 
           alts = nub [ERrs (l,r) |l<-lAlts,r<-rAlts,source l `comparable` source r]
          -- Step 3: compute messages
           deepMsgs = lMsgs++rMsgs
           combMsgs = [ "Types at the left of "++showADL p_l++" and "++showADL p_l++" do not match:\n"++
                        "\n  Possible types of "++showADL p_l++": "++ (show.nub) (map source lAlts)++"."++
                        "\n  Possible types of "++showADL p_r++": "++ (show.nub) (map source rAlts)++"."
                      | null alts]
infer contxt (PLrs (p_l,p_r)) ac = (alts, if null deepMsgs then combMsgs else deepMsgs)
    where -- Step 1: infer contxt types of left hand side and right hand sides
           lc = case ac of
              NoCast       -> NoCast
              SourceCast _ -> NoCast
              TargetCast t -> SourceCast t
              Cast _ t     -> SourceCast t
           rc = case ac of
              NoCast       -> NoCast
              SourceCast s -> SourceCast s
              TargetCast _ -> NoCast
              Cast s _     -> SourceCast s
           (lAlts,lMsgs)=infer contxt p_l lc
           (rAlts,rMsgs)=infer contxt p_r rc
          -- Step 2: compute the viable alternatives 
           alts = nub [ELrs (l,r) |l<-lAlts,r<-rAlts,target l `comparable` target r]
          -- Step 3: compute messages
           deepMsgs = lMsgs++rMsgs
           combMsgs = [ "Types at the right of "++showADL p_l++" and "++showADL p_l++" do not match:\n"++
                        "\n  Possible types of "++showADL p_l++": "++ (show.nub) (map target lAlts)++"."++
                        "\n  Possible types of "++showADL p_r++": "++ (show.nub) (map target rAlts)++"."
                      | null alts]
infer contxt e@(PDif (p_l,p_r)) ac = (alts, if null deepMsgs then combMsgs else deepMsgs)
    where -- Step 1: infer contxt types of left hand side and right hand sides
           terms     = [infer contxt p_e ac | p_e<-[p_l,p_r]]
          -- Step 2: find the most general type that is determined.
           detS   = sort [s |SourceCast s<-ds] where ds = [detSrc es | (es,_)<-terms]
           detT   = sort [t |TargetCast t<-ds] where ds = [detTrg es | (es,_)<-terms]
           uc     = case (detS,detT) of
                     (s:_,t:_) -> Cast s t
                     (s:_, []) -> SourceCast s
                     ( [],t:_) -> TargetCast t
                     ( [], []) -> NoCast
          -- Step 3: redo inference with tightened types
           (lAlts',lMsgs)=infer contxt p_l uc
           (rAlts',rMsgs)=infer contxt p_r uc
          -- Step 4: compute the viable alternatives 
           alts = nub [EDif (l,r) |l<-lAlts',r<-rAlts',sign r <= sign l]
          -- Step 5: compute messages
           deepMsgs = lMsgs++rMsgs
           combMsgs = [ "Incompatible types in: "++showADL e++"."++
                        "\n  Possible types of "++showADL p_l++": "++ show (map sign lAlts')++"."++
                        "\n  Possible types of "++showADL p_r++": "++ show (map sign rAlts')++"."
                      | null alts]++
                      [ "Ambiguous types in "++showADL e
                      | not (srcTypeable p_l)&&not (srcTypeable p_r) || not (trgTypeable p_r)&&not (trgTypeable p_l) ]

-- | the inference procedure for \/ and /\  (i.e. union  and  intersect)
inferUniIsc :: (ShowADL a,Language l, ConceptStructure l, Identified l) =>
               l
               -> ([P_Expression] -> a)
               -> ([Expression] -> Expression)
               -> Bool
               -> [P_Expression]
               -> AutoCast
               -> ([Expression], [TErr])
inferUniIsc _      _            _           _       [] _     = fatal 610 "Type checking (PUni []) or (Pisc []) should never occur."
inferUniIsc contxt _            _           _       [p_e] ac = infer contxt p_e ac
inferUniIsc contxt pconstructor constructor specific p_rs ac = (solutions,messages)
    where -- Step 1: do inference on all subexpressions,-- example: e = hoofdplaats[Gerecht*Plaats]~\/neven[Plaats*Rechtbank]
          terms     = [infer contxt p_e ac | p_e<-p_rs]        -- example: terms = [[hoofdplaats[Gerecht*Plaats]~],[neven[Plaats*Rechtbank]]]
          -- Step 2: find the most generic type that is determined.
          detS   = (if specific then sort else reverse.sort)                         -- example: detS=[Plaats,Plaats]
                   [s |SourceCast s<-ds] where ds = [detSrc es | (es,_)<-terms]
          detT   = (if specific then sort else reverse.sort)                         -- example: detT=[Gerecht,Rechtbank], because Rechtbank ISA Gerecht
                   [t |TargetCast t<-ds] where ds = [detTrg es | (es,_)<-terms]
          uc     = case (detS,detT) of                                               -- example: uc  =Cast Plaats Gerecht, i.e. the most generic
                    (s:_,t:_) -> Cast s t
                    (s:_, []) -> SourceCast s
                    ( [],t:_) -> TargetCast t
                    ( [], []) -> NoCast
          -- Step 3: redo inference with tightened types
          terms'    = [infer contxt p_e uc | p_e<-p_rs]
          -- Step 4: get the terms with fewest alternatives up front (for reducing the number of comparisons).
          altss     = sort' length [es | (es,_)<-terms']
          -- Step 5: determine candidate combinations
          solutions = {- Possibly useful for debugging:
                      case (p_rs, ac) of
                       ( [_, Prel (P_Rel "neven" _)] , _ ) ->
                           error (show (pconstructor p_rs)++
                                  "\nac="++show ac++
                                  "\ndetS="++show detS++
                                  "\ndetT="++show detT++
                                  "\nuc  ="++show uc  ++
                                  "\nterms'="++show [es | (es,_)<-terms']++
                                  "\naltss="++show altss)
                       _ -> -}
                              nub
                              [ constructor (x:cand)                            -- Assemble x with the remaining candidates
                              | x<-head altss                          -- use subexpression  x  to compare all other subexpressions with
                              , let cands=[ [a | a<-alts, a `comparable` x] -- reduce the number of candidate combinations by ensuring that all subexpressions are comparable.
                                          | alts<-tail altss]
                              , cand<-combinations cands               -- assemble usable combinations
                              ]
{- More insightful, but combinatorially explosive, would be the following:
          combs     = sort' (not.null.snd)                     -- The alternatives without errors will be up front
                      [ (rs,typeErrors rs)
                      | rs<-combinations candidats             -- example: combinations [[1,2,3],[10,20],[4]] = [[1,10,4],[1,20,4],[2,10,4],[2,20,4],[3,10,4],[3,20,4]]
                      ]
          solutions = [pconstructor p_rs | (rs,ms)<-combs, null ms]        -- a combination without error messages is a potential solution
-}
          -- Step 6: compute messages
          deepMsgs  = [m | (_,msgs)<-terms, m<-msgs]           -- messages from within the terms
          messages  = if null deepMsgs
                      then (if null solutions
                            then typeErrors                    -- the messages found by combining terms, from which an arbitrary wrong combination is taken
                            else [])                           -- we have solutions without deep messages. 
                      else deepMsgs                            -- get deep messages before combination-messages.
          typeErrors
           = [ "Incomparable types in expression "++showADL (pconstructor p_rs)
                                                               -- ++ " between\n   "++intercalate "\n   " [show (sign r)++ " (in "++show r++")" | r<-types]
             | length types>1 ]                                -- if there is more than one class, not all types are comparable, so we have an error.
             where types = [ head (sort cl)                    -- from each class, pick the expression with the most specific type.
                           | cl<-(eqClass comparable.nub.concat) altss ]       -- make equivalence classes of subexpressions with comparable type

-- | the inference procedure for = and |-  (i.e. equivalence  and  implication/subset)
inferEquImp :: (ShowADL a1, Eq a,Language l, ConceptStructure l, Identified l) =>
               l
               -> ((P_Expression, P_Expression) -> a1)
               -> ((Expression, Expression) -> a)
               -> (P_Expression, P_Expression)
               -> AutoCast
               -> ([a], [String])
inferEquImp contxt pconstructor constructor (p_l,p_r) ac = (alts, if null deepMsgs then combMsgs else deepMsgs)
    where -- Step 1: infer contxt types of left hand side and right hand sides   -- example: Pimp (PCps [Prel beslissing,Prel van,Prel jurisdictie],Prel bevoegd)
           terms     = [infer contxt p_e ac | p_e<-[p_l,p_r]]                    -- example: [[beslissing[Zaak*Beslissing];van[Beslissing*Orgaan];jurisdictie[Orgaan*Rechtbank]],[bevoegd[Zaak*Gerecht]]]
          -- Step 2: find the most general type that is determined.
           detS   = sort [s |SourceCast s<-ds] where ds = [detSrc es | (es,_)<-terms]  -- example: [Zaak,Zaak]
           detT   = sort [t |TargetCast t<-ds] where ds = [detTrg es | (es,_)<-terms]  -- example: [Gerecht,Rechtbank]
           uc     = case (detS,detT) of                                                -- example: Cast Zaak Gerecht
                     (s:_,t:_) -> Cast s t
                     (s:_, []) -> SourceCast s
                     ( [],t:_) -> TargetCast t
                     ( [], []) -> NoCast
          -- Step 3: redo inference with tightened types
           (lAlts',lMsgs)=infer contxt p_l uc
           (rAlts',rMsgs)=infer contxt p_r uc
          -- Step 4: compute the viable alternatives 
           alts = {- Possibly useful for debugging: 
                  if  "nodig" `elem` map name (p_mors p_l)  -- p_l==PTyp (Prel P_I) (P_Sign [PCpt "Bericht"])
                  then error (show (pconstructor (p_l,p_r))++
                              "\nterms: "++show [es | (es,_)<-terms]++
                              "\nlAlts': "++show lAlts'++
                              "\nRAlts': "++show rAlts'++
                              "\ndetS, detT, uc: "++show detS++"    "++show detT++"    "++show uc++
                              "\nalts: "++show (nub [EImp (l,r) |l<-lAlts',r<-rAlts',sign r <= sign l])) else -}
                  nub [constructor (l,r) |l<-lAlts',r<-rAlts',sign r <= sign l]
          -- Step 5: compute messages
           deepMsgs = lMsgs++rMsgs
           combMsgs = [ "Left and right types must be equal in: "++showADL (pconstructor (p_l,p_r))++"."++
                        "\n  Possible types of "++showADL p_l++": "++ show (map sign lAlts')++"."++
                        "\n  Possible types of "++showADL p_r++": "++ show (map sign rAlts')++"."
                      | null alts]++
                      [ "expression "++showADL (pconstructor (p_l,p_r))++" cannot be typed."
                      | not (srcTypeable p_l)&&not (srcTypeable p_r) || not (trgTypeable p_r)&&not (trgTypeable p_l) ]

-- | the inference procedure for ; and ! (i.e. composition and relational addition)
inferCpsRad :: (Show a,Language l, ConceptStructure l, Identified l) =>
               l                       -- ^ The context, from which the declarations and concepts are used.
               -> ([P_Expression] -> a)        -- ^ The constructor, which is either PCps or Rad
               -> ([Expression] -> Expression)
               -> [P_Expression]
               -> AutoCast
               -> ([Expression], [TErr])
inferCpsRad _      _            _          []    _  = fatal 469 "Type checking (PRad []) or (PCps []) should never occur."
inferCpsRad contxt _            _          [p_e] ac = infer contxt p_e ac
inferCpsRad contxt pconstructor constructor p_rs ac = (solutions,messages)
    where -- Step 1: do inference on all subexpressions  -- example: PCps [Prel in,Prel zaak]
          castVector= case ac of                                         -- example: castVector=[SourceCast Document,TargetCast Zaak]
                       Cast s t     -> SourceCast s : [NoCast | _<-(init.tail) p_rs] ++ [TargetCast t]
                       SourceCast s -> SourceCast s : [NoCast | _<-tail p_rs]
                       TargetCast t -> [NoCast | _<-init p_rs] ++ [TargetCast t]
                       NoCast       -> [NoCast | _<-p_rs]
          terms     = [ infer contxt p_e ec | (p_e,ec)<-zip p_rs castVector ]   -- example: terms=[[in[Document*Dossier]],[zaak[Dossier*Zaak],zaak[Beslissing*Zaak]]]
          -- Step 2: determine the intermediate types, if determined
          inter                                                          -- example: inter=[[Dossier]]
           = [ case (detTrg lAlts, detSrc rAlts) of
                (TargetCast t, SourceCast s) -> [s `lub` t| s `comparable` t]
                (NoCast,       SourceCast s) -> [s]
                (TargetCast t, NoCast      ) -> [t]
                (NoCast      , NoCast      ) -> []
                (_           , _           ) -> fatal 433 "inspect code of detTrg or detSrc"
             | ((lAlts,_),(rAlts,_)) <- zip (init terms) (tail terms)
             ]
          -- Step 3: determine the tightened cast vector
          inter'                                                         -- example: inter'=[[Document],[Dossier],[Zaak]]
           = case ac of
              NoCast       -> [[]] ++inter++[[]]
              SourceCast s -> [[s]]++inter++[[]]
              TargetCast t -> [[]] ++inter++[[t]]
              Cast s t     -> [[s]]++inter++[[t]]
          castVector'                                                    -- example: castVector'=[Cast Document Dossier,Cast Dossier Zaak]
           = [ case (ls,rs) of
                ([s], [t]) -> Cast s t
                ([s],  _ ) -> SourceCast s
                ( _ , [t]) -> TargetCast t
                ( _ ,  _ ) -> NoCast      
             | (ls,rs)<-zip (init inter') (tail inter') ]
          -- Step 4: redo inference on all subexpressions
          terms'    = [ infer contxt p_e ec | (p_e,ec)<-zip p_rs castVector' ]  -- example: terms'=[[in[Document*Dossier]],[zaak[Dossier*Zaak]]]
          -- Step 5: combine all possibilities                           -- example: combs=[([in[Document*Dossier],zaak[Dossier*Zaak]],[])]
          combs     = sort' (not.null.snd)                     -- The alternatives without errors will be up front
                      [ (rs,makeMessages rs)
                      | rs<-combinations [es | (es,_)<-terms'] -- example: combinations [[1,2,3],[10,20],[4]] = [[1,10,4],[1,20,4],[2,10,4],[2,20,4],[3,10,4],[3,20,4]]
                      , ac <= Cast (source (head rs)) (target (last rs))
                      ]
          -- Step 6: determine solutions                       --  example: solutions=[in[Document*Dossier];zaak[Dossier*Zaak]]
          solutions = {- Possibly useful for debugging:
                      case (p_rs, ac) of
                       ( [_, PFlp (Prel (P_Rel "type" _)) , _] , _ ) ->
                           error (show (pconstructor p_rs)++
                                  "\ncastVector="++show castVector++
                                  "\nterms="++show [es | (es,_)<-terms]++
                                  "\ninter="++show inter++
                                  "\ninter'="++show inter'++
                                  "\ncastVector'="++show castVector'++
                                  "\nterms'="++show [es | (es,_)<-terms']++
                                  "\ncombs="++show combs++
                                  "\nsolutions="++show [ECps rs | (rs,ms)<-combs, null ms])
                       _ -> -} nub [constructor rs | (rs,ms)<-combs, null ms]        -- a combination without error messages is a potential solution
          -- Step 7: compute messages
          deepMsgs  = [m | (_,msgs)<-terms, m<-msgs]           -- messages from within the terms
          combMsgs  = [ms | (_,ms)<-combs, not (null ms)]      -- messages from combinations that are wrong (i.e. that have messages)
          interMsgs = [ "The type between "++show (pconstructor lft)++" and "++show (pconstructor rht)++"is ambiguous,\nbecause it may be one of "++show is
                      | (is,(lft,rht))<-zip inter (splits p_rs), length is>1]   -- example: splits [1,2,3,4] = [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]
          messages  = if null deepMsgs
                      then (case solutions of
                             []  -> (concat.take 1) combMsgs     -- the messages found by combining terms, from which an arbitrary wrong combination is taken
                             [_] -> []                           -- we have solutions without deep messages. 
                             _  -> interMsgs)                   -- we have multiple solutions, so something is wrong in between terms.
                      else deepMsgs                            -- get deep messages before combination-messages.
          makeMessages rs
           = [ "incomparable types between "++showADL l++showCast ltyp++" and "++showADL r++showCast rtyp++":\n   "++showADL (target l)++" does not match "++showADL (source r)
             | (l,r,ltyp,rtyp) <- zip4 (init rs) (tail rs) castVector' castVector', not (target l `comparable` source r)]
          showCast (Cast s t) = "["++show s++"*"++show t++"]"
          showCast _ = ""


-- The following function can be used to determine how much of a set of alternative expression is already determined
detSrc :: [Expression] -> AutoCast
detSrc alts = case (alts, nub (map source alts)) of
      ( _ ,[s])     -> SourceCast s                 -- if the alternatives have the same source, the type is source-determined.
      _             -> NoCast                       -- in other cases the type is not yet determined.
detTrg :: [Expression] -> AutoCast
detTrg alts = case (alts, nub (map target alts)) of
      ( _ ,[t])     -> TargetCast t                 -- if the alternatives have the same target, the type is target-determined.
      _             -> NoCast                       -- in other cases the type is not yet determined.

-- The purpose of "typeable" is to know whether a type has to be provided from the environment (as in I, V, and Mp1), or the type can be enumerated from the content
srcTypeable :: P_Expression -> Bool
srcTypeable (Pequ (l,_))   = ls||rs where ls = srcTypeable l; rs = srcTypeable l
srcTypeable (Pimp (l,_))   = ls||rs where ls = srcTypeable l; rs = srcTypeable l
srcTypeable (PUni es)      = any srcTypeable es
srcTypeable (Pisc es)      = any srcTypeable es
srcTypeable (PDif (l,_))   = ls||rs where ls = srcTypeable l; rs = srcTypeable l
srcTypeable (PLrs (l,_))   = srcTypeable l
srcTypeable (PRrs (l,_))   = trgTypeable l
srcTypeable (PRad [])      = False
srcTypeable (PRad es)      = srcTypeable (head es)
srcTypeable (PCps [])      = False
srcTypeable (PCps es)      = srcTypeable (head es)
srcTypeable (PKl0 e)       = srcTypeable e
srcTypeable (PKl1 e)       = srcTypeable e
srcTypeable (PFlp e)       = trgTypeable e
srcTypeable (PCpl e)       = srcTypeable e
srcTypeable (PBrk e)       = srcTypeable e
srcTypeable (PTyp _ _)     = True
srcTypeable (Prel P_Rel{}) = True
srcTypeable (Prel _      ) = False


trgTypeable :: P_Expression -> Bool
trgTypeable (Pequ (_,r))   = lt||rt where lt = trgTypeable r; rt = trgTypeable r
trgTypeable (Pimp (_,r))   = lt||rt where lt = trgTypeable r; rt = trgTypeable r
trgTypeable (PUni es)      = any trgTypeable es                            
trgTypeable (Pisc es)      = any trgTypeable es                            
trgTypeable (PDif (_,r))   = lt||rt where lt = trgTypeable r; rt = trgTypeable r
trgTypeable (PLrs (_,r))   = srcTypeable r
trgTypeable (PRrs (_,r))   = trgTypeable r
trgTypeable (PRad [])      = False
trgTypeable (PRad es)      = trgTypeable (last es)
trgTypeable (PCps [])      = False
trgTypeable (PCps es)      = trgTypeable (last es)
trgTypeable (PKl0 e)       = trgTypeable e
trgTypeable (PKl1 e)       = trgTypeable e
trgTypeable (PFlp e)       = srcTypeable e
trgTypeable (PCpl e)       = trgTypeable e
trgTypeable (PBrk e)       = trgTypeable e
trgTypeable (PTyp _ _)     = True
trgTypeable (Prel P_Rel{}) = True
trgTypeable (Prel _      ) = False

{- for debugging only:
p_mors :: P_Expression -> [P_Relation]
p_mors expr = case expr of 
       Pequ (e,e')  ->  p_mors e `uni` p_mors e'     -- rs  ^ equivalence              =
       Pimp (e,e')  ->  p_mors e `uni` p_mors e'     -- rs  ^ implication              |-
       Pisc es      ->  foldr uni [] (map p_mors es) -- bs  ^ intersection             /\
       PUni es      ->  foldr uni [] (map p_mors es) -- bs  ^ union                    \/
       PDif (e,e')  ->  p_mors e `uni` p_mors e'     -- rs  ^ difference               -
       PLrs (e,e')  ->  p_mors e `uni` p_mors e'     -- rs  ^ left residual            /
       PRrs (e,e')  ->  p_mors e `uni` p_mors e'     -- rs  ^ right residual           \
       PCps es      ->  foldr uni [] (map p_mors es) -- ts  ^ composition              ;
       PRad es      ->  foldr uni [] (map p_mors es) -- ts  ^ relative addition        !
       PKl0 e       ->  p_mors e                     -- e   ^ Rfx.Trn closure          *
       PKl1 e       ->  p_mors e                     -- e   ^ Transitive closure       +
       PFlp e       ->  p_mors e                     -- e   ^ conversion               ~
       PCpl e       ->  p_mors e                     -- e   ^ complement               ~
       PBrk e       ->  p_mors e                     -- e   ^ bracketed expression  ( ... )
       PTyp e _     ->  p_mors e                     -- e   ^ type cast expression  ... [c] (defined tuple instead of list because ETyp only exists for actual casts)
       Prel r       ->  [r]                     -- rel ^ simple relation
-}

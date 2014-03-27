{-# OPTIONS_GHC -Wall -XFlexibleInstances -XDataKinds #-}
{-# LANGUAGE RelaxedPolyRec #-}
module DatabaseDesign.Ampersand.ADL1.P2A_Converters ( pCtx2aCtx, showErr, Guarded(..) )
where
import DatabaseDesign.Ampersand.ADL1.Disambiguate
import DatabaseDesign.Ampersand.Core.ParseTree -- (P_Context(..), A_Context(..))
import DatabaseDesign.Ampersand.Input.ADL1.CtxError
import DatabaseDesign.Ampersand.ADL1.Lattices
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree hiding (sortWith, maxima, greatest)
import DatabaseDesign.Ampersand.Classes.ConceptStructure
import DatabaseDesign.Ampersand.Basics (Identified(name), fatalMsg,eqCl)
import DatabaseDesign.Ampersand.Misc
import Prelude hiding (head, sequence, mapM)
import Control.Applicative
import Data.Traversable
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Data.List(nub)

head :: [a] -> a
head [] = fatal 30 "head must not be used on an empty list!"
head (a:_) = a

fatal :: Int -> String -> a
fatal = fatalMsg "ADL1.P2A_Converters"

newtype SignOrd = SignOrd Sign
instance Ord SignOrd where
  compare (SignOrd (Sign a b)) (SignOrd (Sign c d)) = compare (name a,name b) (name c,name d)
instance Eq SignOrd where
  (==) (SignOrd (Sign a b)) (SignOrd (Sign c d)) = (name a,name b) == (name c,name d)

pCtx2aCtx :: P_Context -> Guarded A_Context
pCtx2aCtx 
 PCtx { ctx_nm     = n1
      , ctx_pos    = n2
      , ctx_lang   = lang
      , ctx_markup = pandocf
      , ctx_thms   = p_themes       --  The themes that are specified by the user to be documented.
      , ctx_pats   = p_patterns     --  The patterns defined in this context
      , ctx_PPrcs  = p_processes    --  The processes as defined by the parser
      , ctx_rs     = p_rules        --  All user defined rules in this context, but outside patterns and outside processes
      , ctx_ds     = p_declarations --  The relations declared in this context, outside the scope of patterns
      , ctx_cs     = p_conceptdefs  --  The concept definitions defined in this context, outside the scope of patterns
      , ctx_ks     = p_identdefs    --  The identity definitions defined in this context, outside the scope of patterns
      , ctx_vs     = p_viewdefs     --  The view definitions defined in this context, outside the scope of patterns
      , ctx_gs     = p_gens         --  The gen definitions defined in this context, outside the scope of patterns
      , ctx_ifcs   = p_interfaces   --  The interfaces defined in this context, outside the scope of patterns
      , ctx_ps     = p_purposes     --  The purposes defined in this context, outside the scope of patterns
      , ctx_pops   = p_pops         --  The populations defined in this context, but outside patterns and outside processes
      , ctx_sql    = p_sqldefs      --  user defined sqlplugs, taken from the Ampersand script
      , ctx_php    = p_phpdefs      --  user defined phpplugs, taken from the Ampersand script
      , ctx_metas  = p_metas        --  generic meta information (name/value pairs) that can be used for experimenting without having to modify the adl syntax
      }
 = (\pats procs rules identdefs viewdefs interfaces purposes udpops sqldefs phpdefs
     -> ACtx{ ctxnm = n1
            , ctxpos = n2
            , ctxlang = deflangCtxt
            , ctxmarkup = deffrmtCtxt
            , ctxthms = p_themes
            , ctxpats = pats
            , ctxprocs = procs
            , ctxrs = rules
            , ctxds = ctxDecls
            , ctxpopus = nub (udpops++dclPops++mp1Pops rules++mp1Pops pats++mp1Pops procs++mp1Pops identdefs++mp1Pops viewdefs++mp1Pops interfaces)
            , ctxcds = allConceptDefs
            , ctxks = identdefs
            , ctxvs = viewdefs
            , ctxgs = map pGen2aGen p_gens
            , ctxgenconcs = map (map castConcept) (concGroups ++ map (:[]) soloConcs)
            , ctxifcs = interfaces
            , ctxps = purposes
            , ctxsql = sqldefs
            , ctxphp = phpdefs
            , ctxmetas = p_metas
            }
    ) <$> traverse pPat2aPat p_patterns            --  The patterns defined in this context
      <*> traverse pProc2aProc p_processes         --  The processes defined in this context
      <*> traverse (pRul2aRul [] n1) p_rules       --  All user defined rules in this context, but outside patterns and outside processes
      <*> traverse pIdentity2aIdentity p_identdefs --  The identity definitions defined in this context, outside the scope of patterns
      <*> traverse pViewDef2aViewDef p_viewdefs    --  The view definitions defined in this context, outside the scope of patterns
      <*> traverse pIfc2aIfc p_interfaces          --  The interfaces defined in this context, outside the scope of patterns
      <*> traverse pPurp2aPurp p_purposes          --  The purposes of objects defined in this context, outside the scope of patterns
      <*> traverse pPop2aPop p_pops                --  [Population]
      <*> traverse pObjDef2aObjDef p_sqldefs       --  user defined sqlplugs, taken from the Ampersand script
      <*> traverse pObjDef2aObjDef p_phpdefs       --  user defined phpplugs, taken from the Ampersand script
  where
    -- story about genRules and genLattice
    -- the genRules is a list of equalities between concept sets, in which every set is interpreted as a conjunction of concepts
    -- the genLattice is the resulting optimized structure
    genRules = [ ( Set.singleton (name (gen_spc x)), Set.fromList (map name (gen_concs x)))
               | x <- p_gens ++ concatMap pt_gns p_patterns ++ concatMap procGens p_processes
               ]
    genLattice :: Op1EqualitySystem String
    genLattice = optimize1 (foldr addEquality emptySystem genRules)
    
    concGroups :: [[String]]
    concGroups = getGroups genLattice
    allConcs :: Set.Set String
    allConcs = Set.fromList (map (name . source) decls ++ map (name . target) decls)
    soloConcs :: [String]
    soloConcs = filter (not . isInSystem genLattice) (Set.toList allConcs)
    
    deflangCtxt = fromMaybe English lang  -- explanation: if lang==Nothing, then English, if lang==Just l then l
    deffrmtCtxt = fromMaybe HTML pandocf

    (decls,dclPops)= unzip dps
    (ctxDecls,_ ) = unzip ctxDecls'
    dps = ctxDecls'++patDecls++patProcs
    ctxDecls' = [ pDecl2aDecl n1         deflangCtxt deffrmtCtxt pDecl | pDecl<-p_declarations ] --  The relations declared in this context, outside the scope of patterns
    patDecls  = [ pDecl2aDecl (name pat) deflangCtxt deffrmtCtxt pDecl | pat<-p_patterns, pDecl<-pt_dcs pat ] --  The relations declared in all patterns within this context.
    patProcs  = [ pDecl2aDecl (name prc) deflangCtxt deffrmtCtxt pDecl | prc<-p_processes, pDecl<-procDcls prc ] --  The relations declared in all processes within this context.
      
    declMap = Map.map groupOnTp (Map.fromListWith (++) [(name d,[d]) | d <- decls])
      where groupOnTp lst = Map.fromListWith accumDecl [(SignOrd$ sign d,d) | d <- lst]
    findDecls x = Map.findWithDefault Map.empty x declMap
    findDecl o x = getOneExactly o . Map.elems $ findDecls x
    findDeclsTyped x tp = Map.findWithDefault [] (SignOrd tp) (Map.map (:[]) (findDecls x))
    findDeclTyped o x tp = getOneExactly o (findDeclsTyped x tp)
    -- accumDecl is the function that combines two relations into one
    -- meanings, for instance, two should get combined into a list of meanings, et cetera
    -- positions are combined
    -- TODO
    accumDecl :: Declaration -> Declaration -> Declaration
    accumDecl a _ = a
    
    pDecl2aDecl :: 
         String         -- The name of the pattern
      -> Lang           -- The default language 
      -> PandocFormat   -- The default pandocFormat
      -> P_Declaration -> (Declaration, Population)
    pDecl2aDecl patNm defLanguage defFormat pd
     = let dcl = Sgn { decnm   = dec_nm pd
                     , decsgn  = pSign2aSign (dec_sign pd)
                     , decprps = dec_prps pd
                     , decprps_calc = Nothing  --decprps_calc in an A_Context are still the user-defined only. prps are calculated in adl2fspec.
                     , decprL  = dec_prL pd
                     , decprM  = dec_prM pd
                     , decprR  = dec_prR pd
                     , decMean = pMean2aMean defLanguage defFormat (dec_Mean pd)
                     , decConceptDef = dec_conceptDef pd
                     , decfpos = dec_fpos pd 
                     , deciss  = True
                     , decusr  = True
                     , decISA  = False
                     , decpat  = patNm
                     , decplug = dec_plug pd
                     }
       in (dcl, PRelPopu { popdcl = dcl, popps = dec_popu pd})
    
    pSign2aSign :: P_Sign -> Sign
    pSign2aSign (P_Sign src tgt) = Sign (pCpt2aCpt src) (pCpt2aCpt tgt)

    pGen2aGen :: P_Gen -> A_Gen
    pGen2aGen pg@PGen{}
       = Isa{gengen = pCpt2aCpt (gen_gen pg)
            ,genspc = pCpt2aCpt (gen_spc pg)
            }
    pGen2aGen pg@P_Cy{}
       = IsE { genrhs = map pCpt2aCpt (gen_rhs pg)
             , genspc = pCpt2aCpt (gen_spc pg)
             }

    castSign :: String -> String -> Sign
    castSign a b = Sign (castConcept a) (castConcept b)

{- SJ20140216: The function castConcept is used in the type system, but looks similar to pCpt2aCpt.
   However, castConcept makes an erroneous concept, which we should prevent in the first place.
   So it seems castConcept should be removed if possible, and pCpt2aCpt should be doing all the work. 
-}
    leastConcept :: A_Concept -> String -> A_Concept
    leastConcept c str
     = case (name c `elem` leastConcepts, str `elem` leastConcepts) of
         (True, _) -> c
         (_, True) -> castConcept str
         (_, _)    -> fatal 178 ("Either "++name c++" or "++str++" should be a subset of the other." )
       where
         leastConcepts = findExact genLattice (Atom (name c) `Meet` (Atom str))
    castConcept :: String -> A_Concept
    castConcept "ONE" = ONE
    castConcept x
     = PlainConcept {cptnm = x}

    pCpt2aCpt :: P_Concept -> A_Concept
    pCpt2aCpt pc
        = case pc of
            PCpt{} -> PlainConcept { cptnm = p_cptnm pc}
            P_Singleton -> ONE   

    pPop2aPop :: P_Population -> Guarded Population
    pPop2aPop P_CptPopu { p_cnme = cnm, p_popas = ps }
     = pure PCptPopu{ popcpt = castConcept cnm, popas = ps }
    pPop2aPop orig@(P_RelPopu { p_rnme = rnm, p_popps = ps })
     = fmap (\dcl -> PRelPopu { popdcl = dcl, popps = ps})
            (findDecl orig rnm)
    pPop2aPop orig@(P_TRelPop { p_rnme = rnm, p_type = tp, p_popps = ps })
     = fmap (\dcl -> PRelPopu { popdcl = dcl, popps = ps})
            (findDeclTyped orig rnm (pSign2aSign tp))
    
    pObjDef2aObjDef :: P_ObjectDef -> Guarded ObjectDef
    pObjDef2aObjDef x = fmap fst (typecheckObjDef tpda)
     where tpda = disambiguate termPrimDisAmb x
     
    pViewDef2aViewDef :: P_ViewDef -> Guarded ViewDef
    pViewDef2aViewDef x = typecheckViewDef tpda
     where tpda = disambiguate termPrimDisAmb x
     
    typecheckViewDef :: P_ViewD (TermPrim, DisambPrim) -> Guarded ViewDef
    typecheckViewDef
       o@(P_Vd { vd_pos = orig
            , vd_lbl = lbl -- String
            , vd_cpt = cpt -- Concept
            , vd_ats = pvs -- view segment
            })
     = (\vdts
        -> Vd { vdpos = orig
              , vdlbl = lbl
              , vdcpt = pCpt2aCpt cpt
              , vdats = vdts
              })
       <$> traverse (typeCheckViewSegment o) pvs
    
    typeCheckViewSegment :: (P_ViewD a) -> (P_ViewSegmt (TermPrim, DisambPrim)) -> Guarded ViewSegment
    typeCheckViewSegment o P_ViewExp{ vs_obj = ojd }
     = (\(obj,b) -> case findExact genLattice (mIsc c (name (source (objctx obj)))) of
                      [] -> mustBeOrdered o o (Src,(source (objctx obj)),obj)
                      r  -> if b || c `elem` r then pure (ViewExp obj{objctx = addEpsilonLeft c r (name (source (objctx obj))) (objctx obj)})
                            else mustBeBound (origin obj) [(Tgt,objctx obj)]
       ) <?> typecheckObjDef ojd
     where c = name (vd_cpt o)
    typeCheckViewSegment _ P_ViewText { vs_txt = txt } = pure$ ViewText txt
    typeCheckViewSegment _ P_ViewHtml { vs_htm = htm } = pure$ ViewHtml htm
    
    typecheckObjDef :: (P_ObjDef (TermPrim, DisambPrim)) -> Guarded (ObjectDef, Bool)
    typecheckObjDef o@(P_Obj { obj_nm = nm
                             , obj_pos = orig
                             , obj_ctx = ctx
                             , obj_msub = subs
                             , obj_strs = ostrs
                             })
     = (\(expr,subi)
        -> case subi of
            Nothing -> pure (obj expr Nothing)
            Just (InterfaceRef s) -> pure (obj expr (Just$InterfaceRef s)) --TODO: check type!
            Just b@(Box c _)
              -> case findExact genLattice (mjoin (name c) (gc Tgt (fst expr))) of
                    [] -> mustBeOrdered o (Src,c,((\(Just x)->x) subs)) (Tgt,target (fst expr),(fst expr))
                    r  -> if (name c) `elem` r
                          then pure (obj (addEpsilonRight' (name c) (fst expr), snd expr) (Just$ b))
                          else mustBeBound (origin o) [(Tgt,fst expr)]
       ) <?> ((,) <$> typecheckTerm ctx <*> maybeOverGuarded pSubi2aSubi subs)
     where
      obj (e,(sr,_)) s
       = ( Obj { objnm = nm
               , objpos = orig
               , objctx = e
               , objmsub = s
               , objstrs = ostrs
               }, sr)
    addEpsilonLeft :: String -> [String] -> String -> Expression -> Expression
    addEpsilonLeft a b c e
     = if a==c then (if c `elem` b then e else fatal 200 "b == c must hold: the concept of the epsilon relation should be equal to the intersection of its source and target")
               else if c/=name (source e) then fatal 202 ("addEpsilonLeft glues erroneously: c="++show c++"  and e="++show e++".")
                    else EEps (castConcept (head b)) (castSign a c) .:. e
    addEpsilonLeft',addEpsilonRight' :: String -> Expression -> Expression
    addEpsilonLeft' a e
     = if a==name (source e) then e else EEps (leastConcept (source e) a) (castSign a (name (source e))) .:. e
    addEpsilonRight' a e
     = if a==name (target e) then e else e .:. EEps (leastConcept (target e) a) (castSign (name (target e)) a)
    addEpsilon :: String -> String -> Expression -> Expression
    addEpsilon s t e
     = (if s==name (source e) then id else (EEps (leastConcept (source e) s) (castSign s (name (source e))) .:.)) $
       (if t==name (target e) then id else (.:. EEps (leastConcept (target e) t) (castSign (name (target e)) t))) e
    
    pSubi2aSubi :: (P_SubIfc (TermPrim, DisambPrim)) -> Guarded SubInterface
    pSubi2aSubi (P_InterfaceRef _ s) = pure (InterfaceRef s)
    pSubi2aSubi o@(P_Box _ []) = hasNone [] o
    pSubi2aSubi o@(P_Box _ l)
     = (\lst -> case findExact genLattice (foldr1 Join (map (Atom . name . source . objctx . fst) lst)) of
                  [] -> mustBeOrderedLst o [(source (objctx a),Src, a) | (a,_) <- lst]
                  r -> case [ objctx a
                            | (a,False) <- lst
                            , not ((name . source . objctx $ a) `elem` r)
                            ] of
                            [] -> pure (Box (castConcept (head r)) (map fst lst))
                            lst' -> mustBeBound (origin o) [(Src,expr)| expr<-lst']
       ) <?> (traverse typecheckObjDef l <* uniqueNames l)
    
    
    typecheckTerm :: Term (TermPrim, DisambPrim) -> Guarded (Expression, (Bool, Bool))
    typecheckTerm tct
     = case tct of
         Prim (t,v) -> (\x -> (x, case t of
                                   PVee _ -> (False,False)
                                   _ -> (True,True)
                                   )) <$> pDisAmb2Expr (t,v)
         Pequ _ a b -> binary  (.==.) (MBE (Src,fst) (Src,snd), MBE (Tgt,fst) (Tgt,snd)) <?> ((,)<$>tt a<*>tt b) 
         Pimp _ a b -> binary  (.|-.) (MBG (Src,snd) (Src,fst), MBG (Tgt,snd) (Tgt,fst)) <?> ((,)<$>tt a<*>tt b)
         PIsc _ a b -> binary  (./\.) (ISC (Src,fst) (Src,snd), ISC (Tgt,fst) (Tgt,snd)) <?> ((,)<$>tt a<*>tt b)
         PUni _ a b -> binary  (.\/.) (UNI (Src,fst) (Src,snd), UNI (Tgt,fst) (Tgt,snd)) <?> ((,)<$>tt a<*>tt b)
         PDif _ a b -> binary  (.-.)  (MBG (Src,fst) (Src,snd), MBG (Tgt,fst) (Tgt,snd)) <?> ((,)<$>tt a<*>tt b)
         PLrs _ a b -> binary' (./.)  (MBG (Tgt,snd) (Tgt,fst)) ((Src,fst),(Src,snd))    <?> ((,)<$>tt a<*>tt b)
         PRrs _ a b -> binary' (.\.)  (MBG (Src,fst) (Src,snd)) ((Tgt,fst),(Tgt,snd))    <?> ((,)<$>tt a<*>tt b)
         PCps _ a b -> binary' (.:.)  (ISC (Tgt,fst) (Src,snd)) ((Src,fst),(Tgt,snd))    <?> ((,)<$>tt a<*>tt b)
         PRad _ a b -> binary' (.!.)  (MBE (Tgt,fst) (Src,snd)) ((Src,fst),(Tgt,snd))    <?> ((,)<$>tt a<*>tt b)
         PPrd _ a b -> (\((x,(s,_)),(y,(_,t))) -> (x .*. y, (s,t))) <$> ((,)<$>tt a<*>tt b)
         PKl0 _ a   -> unary   EKl0   (UNI (Src, id) (Tgt, id), UNI (Src, id) (Tgt, id)) <?> tt a
         PKl1 _ a   -> unary   EKl1   (UNI (Src, id) (Tgt, id), UNI (Src, id) (Tgt, id)) <?> tt a
         PFlp _ a   -> (\(x,(s,t)) -> ((EFlp x), (t,s))) <$> tt a
         PCpl _ a   -> (\(x,_) -> (ECpl x,(False,False))) <$> tt a
         PBrk _ e   -> (\(x,t) -> (EBrk x,t)) <$> tt e
     where
      o = origin (fmap fst tct)
      tt = typecheckTerm
      -- SJC: Here is what binary, binary' and unary do:
      -- (1) Create an expression, the combinator for this is given by its first argument
      -- (2) Fill in the corresponding type-checked terms to that expression
      -- (3) For binary' only: fill in the intermediate concept too
      -- (4) Fill in the type of the new expression
      -- For steps (3) and (4), you can use the `TT' data type to specify the new type, and what checks should occur:
      -- If you don't know what to use, try MBE: it is the strictest form.
      -- In the steps (3) and (4), different type errors may arise:
      -- If the type does not exist, this yields a type error.
      -- Some types may be generalized, while others may not.
      -- When a type may be generalized, that means that the value of the expression does not change if the type becomes larger
      -- When a type may not be generalized:
      --   the type so far is actually just an estimate
      --   it must be bound by the context to something smaller, or something as big
      --   a way to do this, is by using (V[type] /\ thingToBeBound)
      -- More details about generalizable types can be found by looking at "deriv1".
      binary :: (Expression -> Expression->Expression) -- combinator
             -> ( TT ( SrcOrTgt
                     , ( (Expression, (Bool, Bool))
                       , (Expression, (Bool, Bool))
                       ) -> (Expression, (Bool, Bool))
                     )
                , TT ( SrcOrTgt
                     , ( (Expression, (Bool, Bool))
                       , (Expression, (Bool, Bool))
                       ) -> (Expression, (Bool, Bool))
                     )
                ) -- simple instruction on how to derive the type
             -> ((Expression,(Bool,Bool)), (Expression,(Bool,Bool))) -- expressions to feed into the combinator after translation
             -> Guarded (Expression,(Bool,Bool))
      binary  cbn     tp (e1,e2) = wrap'' cbn (fst e1,fst e2) <$> deriv tp (e1,e2)
      unary   cbn     tp e1      = wrap   cbn (fst e1       ) <$> deriv tp e1
      binary' cbn cpt tp (e1,e2) = wrap'  cbn (fst e1,fst e2) <$> deriv1 o (fmap (resolve (e1,e2)) cpt) <*> deriv' tp (e1,e2)
      wrap'' f (e1,e2) ((src,b1), (tgt,b2)) = (f (addEpsilon src tgt e1) (addEpsilon src tgt e2), (b1, b2))
      wrap   f expr  ((src,b1), (tgt,b2))  = (f (addEpsilon src tgt expr), (b1, b2))
      wrap'  f (e1,e2) (cpt,_) ((_,b1), (_,b2))  = (f (addEpsilonRight' cpt e1) (addEpsilonLeft' cpt e2), (b1, b2))
      deriv (t1,t2) es = (,) <$> deriv1 o (fmap (resolve es) t1) <*> deriv1 o (fmap (resolve es) t2)
    
    deriv1 o x'
     = case x' of
        (MBE a@(p1,(e1,b1)) b@(p2,(e2,b2))) ->
             if (b1 && b2) || (gc p1 e1 == gc p2 e2) then (\x -> (x,b1||b2)) <$> getExactType mjoin (p1, e1) (p2, e2)
             else mustBeBound o [(p,e) | (p,(e,False))<-[a,b]]
        (MBG (p1,(e1,b1)) (p2,(e2,b2))) ->
             (\x -> (x,b1)) <$> getAndCheckType mjoin (p1, True, e1) (p2, b2, e2)
        (UNI (p1,(e1,b1)) (p2,(e2,b2))) ->
             (\x -> (x,b1 && b2)) <$> getAndCheckType mjoin (p1, b1, e1) (p2, b2, e2)
        (ISC (p1,(e1,b1)) (p2,(e2,b2))) ->
             (\x -> (x,b1 || b2)) <$> getAndCheckType mIsc  (p1, b1, e1) (p2, b2, e2)
     where
      getExactType flf (p1,e1) (p2,e2)
       = case findExact genLattice (flf (gc p1 e1) (gc p2 e2)) of
          [] -> mustBeOrdered o (p1,e1) (p2,e2)
          r  -> pure$ head r
      getAndCheckType flf (p1,b1,e1) (p2,b2,e2)
       = case findSubsets genLattice (flf (gc p1 e1) (gc p2 e2)) of -- note: we could have used GetOneGuarded, but this is more specific
          []  -> mustBeOrdered o (p1,e1) (p2,e2)
          [r] -> case (b1 || Set.member (gc p1 e1) r,b2 || Set.member (gc p2 e2) r ) of
                   (True,True) -> pure (head' (Set.toList r))
                   (a,b) -> mustBeBound o [(p,e) | (False,p,e)<-[(a,p1,e1),(b,p2,e2)]]
          lst -> mustBeOrderedConcLst o (p1,e1) (p2,e2) (map (map castConcept . Set.toList) lst)
       where head' [] =fatal 321 ("empty list on expressions "++show ((p1,b1,e1),(p2,b2,e2)))
             head' (a:_) = a
    termPrimDisAmb :: TermPrim -> (TermPrim, DisambPrim)
    termPrimDisAmb x
     = (x, case x of
           PI _        -> Ident
           Pid _ conspt-> Known (EDcI (pCpt2aCpt conspt))
           Patm _ s Nothing -> Mp1 s
           Patm _ s (Just conspt) -> Known (EMp1 s (pCpt2aCpt conspt))
           PVee _      -> Vee
           Pfull _ a b -> Known (EDcV (Sign (pCpt2aCpt a) (pCpt2aCpt b)))
           Prel _ r    -> Rel [EDcD dc | dc <- (Map.elems $ findDecls r)]
           PTrel _ r s -> Rel [EDcD dc | dc <- (findDeclsTyped r (pSign2aSign s))]
        )
    
    termPrim2Decl :: TermPrim -> Guarded Declaration
    termPrim2Decl o@(Prel _ r   ) = getOneExactly o [ dc | dc <- (Map.elems $ findDecls r)]
    termPrim2Decl o@(PTrel _ r s) = getOneExactly o [ dc | dc <- (findDeclsTyped r (pSign2aSign s))]
    termPrim2Decl _ = fatal 231 "Expecting Declaration"
    termPrim2Expr :: TermPrim -> Guarded Expression
    termPrim2Expr = pDisAmb2Expr . termPrimDisAmb
    
    pIfc2aIfc :: P_Interface -> Guarded Interface
    pIfc2aIfc P_Ifc { ifc_Params = tps
                    , ifc_Args = args
                    , ifc_Roles = rols
                    , ifc_Obj = obj
                    , ifc_Pos = orig
                    -- , ifc_Name = nm
                    , ifc_Prp = prp
                    }
        = (\ tps' obj'
             -> Ifc { ifcParams = tps'
                    , ifcArgs = args
                    , ifcRoles = rols
                    , ifcObj = obj'
                    , ifcPos = orig
                    , ifcPrp = prp
                    }) <$> traverse termPrim2Expr tps
                       <*> pObjDef2aObjDef obj
    pProc2aProc :: P_Process -> Guarded Process
    pProc2aProc P_Prc { procNm = nm
                      , procPos = orig
                      , procEnd = posEnd
                      , procRules = ruls
                      , procGens = gens
                      , procDcls = dcls
                      , procRRuls = rolruls
                      , procRRels = rolrels
                      , procCds = _cdefs -- SJ2013: the underscore means that this argument is not used.
                      , procIds = idefs
                      , procVds = viewdefs
                      , procXps = purposes
                      , procPop = pops
                      }
     = (\ ruls' rels' pops' idefs' viewdefs' purposes'
         ->  let (decls',dPops) = unzip [ pDecl2aDecl nm deflangCtxt deffrmtCtxt pDecl | pDecl<-dcls ]
             in Proc { prcNm = nm
                     , prcPos = orig
                     , prcEnd = posEnd
                     , prcRules = map snd ruls'
                     , prcGens = map pGen2aGen gens
                     , prcDcls = decls'
                     , prcUps = pops' ++ [ dp | dp@PRelPopu{}<-dPops, (not.null.popps) dp ] ++ [ cp | cp@PCptPopu{}<-dPops, (not.null.popas) cp ]
                     , prcRRuls = [(rol,r)|(rols,r)<-ruls',rol<-rols]
                     , prcRRels = [(rol,r)|(rols,rs)<-rels',rol<-rols,r<-rs]
                     , prcIds = idefs'
                     , prcVds = viewdefs'
                     , prcXps = purposes'
                     }
       ) <$> traverse (\x -> pRul2aRul' [rol | rr <- rolruls, rul <- mRules rr, name x == rul, rol <- mRoles rr] nm x) ruls
         <*> sequenceA [(\x -> (rr_Roles prr,x)) <$> (traverse termPrim2Decl $ rr_Rels prr) | prr <- rolrels]
         <*> traverse pPop2aPop pops
         <*> traverse pIdentity2aIdentity idefs
         <*> traverse pViewDef2aViewDef viewdefs
         <*> traverse pPurp2aPurp purposes
    
    pPat2aPat :: P_Pattern -> Guarded Pattern
    pPat2aPat ppat
     = f <$> parRuls ppat <*> parKeys ppat <*> parPops ppat <*> parViews ppat <*> parPrps ppat <*> sequenceA rrels
       where
        f prules keys' pops' views' xpls rrels'
           = let (decls',dPops) = unzip [ pDecl2aDecl (name ppat) deflangCtxt deffrmtCtxt pDecl | pDecl<-pt_dcs ppat ]
             in A_Pat { ptnm  = name ppat
                      , ptpos = pt_pos ppat
                      , ptend = pt_end ppat
                      , ptrls = map snd prules
                      , ptgns = agens'
                      , ptdcs = decls'
                      , ptups = pops' ++ [ dp | dp@PRelPopu{}<-dPops, (not.null.popps) dp ] ++ [ cp | cp@PCptPopu{}<-dPops, (not.null.popas) cp ]
                      , ptrruls = [(rol,r)|(rols,r)<-prules,rol<-rols]
                      , ptrrels = [(rol,dcl)|rr<-rrels', rol<-rrRoles rr, dcl<-rrRels rr]  -- The assignment of roles to Relations.
                      , ptids = keys'
                      , ptvds = views'
                      , ptxps = xpls
                      }
        agens'   = map pGen2aGen (pt_gns ppat)
        parRuls  = traverse (\x -> pRul2aRul' [rol | prr <- pt_rus ppat, rul<-mRules prr, name x == rul, rol<-mRoles prr] (name ppat) x) . pt_rls
        rrels :: [Guarded RoleRelation]
        rrels =  [(\x -> RR (rr_Roles prr) x (origin prr)) <$> (traverse termPrim2Decl $ rr_Rels prr) | prr <- pt_res ppat]
        parKeys  = traverse pIdentity2aIdentity . pt_ids
        parPops  = traverse pPop2aPop . pt_pop
        parViews = traverse pViewDef2aViewDef . pt_vds
        parPrps  = traverse pPurp2aPurp . pt_xps
    
    pRul2aRul':: [String] -- list of roles for this rule
              -> String -- environment name (pattern / proc name)
              -> (P_Rule TermPrim) -> Guarded ([String],Rule)  -- roles in the lhs
    pRul2aRul' a b c = fmap ((,) a) (pRul2aRul a b c)
    pRul2aRul :: [String] -- list of roles for this rule
              -> String -- environment name (pattern / proc name)
              -> (P_Rule TermPrim) -> Guarded Rule
    pRul2aRul rol env = typeCheckRul rol env . disambiguate termPrimDisAmb
    typeCheckRul :: [String] -- list of roles for this rule
              -> String -- environment name (pattern / proc name)
              -> (P_Rule (TermPrim, DisambPrim)) -> Guarded Rule
    typeCheckRul sgl env P_Ru { rr_nm = nm
                       , rr_exp = expr
                       , rr_fps = orig
                       , rr_mean = meanings
                       , rr_msg = msgs
                       , rr_viol = viols
                       }
     = (\ (exp',_)
       -> (\ vls ->
          Ru { rrnm = nm
             , rrexp = exp'
             , rrfps = orig
             , rrmean = pMean2aMean deflangCtxt deffrmtCtxt meanings
             , rrmsg = map (pMess2aMess deflangCtxt deffrmtCtxt) msgs
             , rrviol = vls
             , rrtyp = sign exp'
             , rrdcl = Nothing
             , r_env = env
             , r_usr = UserDefined
             , r_sgl = not (null sgl)
             , srrel = Sgn{ decnm   = nm
                          , decsgn  = (sign exp')
                          , decprps = []
                          , decprps_calc = Nothing
                          , decprL  = ""
                          , decprM  = ""
                          , decprR  = ""
                          , decMean = pMean2aMean deflangCtxt deffrmtCtxt meanings
                          , decConceptDef = Nothing
                          , decfpos = orig
                          , deciss  = True
                          , decusr  = False
                          , decISA  = False
                          , decpat  = env
                          , decplug = False
                          }
             }
             ) <$> maybeOverGuarded (typeCheckPairView orig exp') viols
          ) <?> typecheckTerm expr
    pIdentity2aIdentity :: P_IdentDef -> Guarded IdentityDef
    pIdentity2aIdentity
            P_Id { ix_pos = orig
                 , ix_lbl = lbl
                 , ix_cpt = pconc
                 , ix_ats = isegs
                 }
     = (\isegs' ->
       Id { idPos = orig
          , idLbl = lbl
          , idCpt = pCpt2aCpt pconc
          , identityAts = isegs'
          }) <$> traverse pIdentSegment2IdentSegment isegs
    pIdentSegment2IdentSegment :: P_IdentSegment -> Guarded IdentitySegment
    pIdentSegment2IdentSegment (P_IdentExp ojd)
     = IdentityExp <$> pObjDef2aObjDef ojd
    
    typeCheckPairView :: Origin -> Expression -> PairView (Term (TermPrim, DisambPrim)) -> Guarded (PairView Expression)
    typeCheckPairView o x (PairView lst)
     = PairView <$> traverse (typeCheckPairViewSeg o x) lst
    typeCheckPairViewSeg :: Origin -> Expression -> (PairViewSegment (Term (TermPrim, DisambPrim))) -> Guarded (PairViewSegment Expression)
    typeCheckPairViewSeg _ _ (PairViewText x) = pure (PairViewText x)
    typeCheckPairViewSeg o t (PairViewExp s x)
     = (\(e,(b,_)) -> case (findSubsets genLattice (mjoin (name (source e)) (gc s t))) of
                        [] -> mustBeOrdered o (Src,e) (s,t)
                        lst -> if b || and (map (name (source e) `elem`) lst)
                               then pure (PairViewExp s e)
                               else mustBeBound o [(Src, e)]
                        ) <?> typecheckTerm x

    pPurp2aPurp :: PPurpose -> Guarded Purpose
    pPurp2aPurp PRef2 { pexPos    = orig     -- :: Origin
                      , pexObj    = objref   -- :: PRefObj
                      , pexMarkup = pmarkup  -- :: P_Markup
                      , pexRefIDs  = refIds  -- :: [String]
                      }
     = (\ obj -> Expl { explPos      = orig
                      , explObj      = obj
                      , explMarkup   = pMarkup2aMarkup deflangCtxt deffrmtCtxt pmarkup
                      , explUserdefd = True
                      , explRefIds   = refIds
                      })
       <$> pRefObj2aRefObj objref
    pRefObj2aRefObj :: PRef2Obj -> Guarded ExplObj
    pRefObj2aRefObj (PRef2ConceptDef  s ) = pure$ ExplConceptDef (lookupConceptDef s)
    pRefObj2aRefObj (PRef2Declaration tm) = ExplDeclaration <$> (termPrim2Decl tm)
    pRefObj2aRefObj (PRef2Rule        s ) = pure$ ExplRule s
    pRefObj2aRefObj (PRef2IdentityDef s ) = pure$ ExplIdentityDef s
    pRefObj2aRefObj (PRef2ViewDef     s ) = pure$ ExplViewDef s
    pRefObj2aRefObj (PRef2Pattern     s ) = pure$ ExplPattern s
    pRefObj2aRefObj (PRef2Process     s ) = pure$ ExplProcess s
    pRefObj2aRefObj (PRef2Interface   s ) = pure$ ExplInterface s
    pRefObj2aRefObj (PRef2Context     s ) = pure$ ExplContext s
    pRefObj2aRefObj (PRef2Fspc        s ) = pure$ ExplContext s
    lookupConceptDef :: String -> ConceptDef
    lookupConceptDef s
     = if null cs
       then Cd{cdpos=OriginUnknown, cdcpt=s, cdplug=True, cddef="", cdtyp="", cdref="", cdfrom=n1}
       else head cs
       where cs = [cd | cd<-allConceptDefs, name cd==s]
    allConceptDefs :: [ConceptDef]
    allConceptDefs = p_conceptdefs++concatMap pt_cds p_patterns++concatMap procCds p_processes
    
pDisAmb2Expr :: (TermPrim, DisambPrim) -> Guarded Expression
-- SJ 20140211 @SJC: TODO graag een typefout genereren voor een SESSION atoom anders dan _SESSION.
pDisAmb2Expr (_,Known x) = pure x
pDisAmb2Expr (_,Rel [x]) = pure x
pDisAmb2Expr (o,Rel rs)  = cannotDisambRel o rs
pDisAmb2Expr (o,_)       = cannotDisamb o

pMean2aMean :: Lang           -- The default language 
            -> PandocFormat   -- The default pandocFormat
            -> [PMeaning] -> AMeaning
pMean2aMean defLanguage defFormat pmeanings
 = AMeaning [ pMarkup2aMarkup defLanguage defFormat pmarkup | PMeaning pmarkup <-pmeanings ]
pMess2aMess :: Lang           -- The default language 
            -> PandocFormat   -- The default pandocFormat
            -> PMessage -> A_Markup
pMess2aMess defLanguage defFormat (PMessage x) = pMarkup2aMarkup defLanguage defFormat x
pMarkup2aMarkup :: Lang           -- The default language 
                -> PandocFormat   -- The default pandocFormat
                -> P_Markup -> A_Markup 
pMarkup2aMarkup defLanguage defFormat
   P_Markup  { mLang   = ml
             , mFormat = mpdf
             , mString = str
             }
 = A_Markup { amLang = fromMaybe defLanguage ml -- The language is always defined; if not by the user, then by default.
            , amFormat = fmt
            , amPandoc = string2Blocks fmt str
            }
     where
       fmt = fromMaybe defFormat mpdf           -- The pandoc format is always defined; if not by the user, then by default.

-- helpers for generating a lattice, not having to write `Atom' all the time
mjoin,mIsc :: a -> a -> FreeLattice a
mjoin a b = Join (Atom a) (Atom b)
mIsc  a b = Meet (Atom a) (Atom b)
-- intended for finding the right expression on terms like (Src,fst)
resolve :: t -> (SrcOrTgt, t -> (t1, (t2, t2))) -> (SrcOrTgt, (t1, t2))
resolve es (p,f)
 = case (p,f es) of
  (Src,(e,(b,_))) -> (Src,(e,b))
  (Tgt,(e,(_,b))) -> (Tgt,(e,b))

maybeOverGuarded :: Applicative f => (t -> f a) -> Maybe t -> f (Maybe a)
maybeOverGuarded _ Nothing = pure Nothing
maybeOverGuarded f (Just x) = Just <$> f x

data TT a  -- (In order of increasing strictness. If you are unsure which to pick: just use MBE, it'll usually work fine)
 = UNI a a -- find the union of these types, return it.
 | ISC a a -- find the intersection of these types, return it.
 | MBE a a -- must be equal: must be (made) of equal type. If these types are comparable, it returns the greatest.
 | MBG a a -- The first of these types must be the greatest, if so, return it (error otherwise)
 -- SJC: difference between UNI and MBE
 -- in general, UNI is less strict than MBE:
 --   suppose A ≤ C, B ≤ C, and C is the least such concept (e.g. if A≤D and B≤D then C≤D)
 --   in this case UNI A B will yield C (if both A and B are generalizable), while MBE A B will give an error
 --   note that in case of A ≤ C, B ≤ C, A ≤ D, B ≤ D (and there is no order between C and D), both will give an error
 --   the error message, however, should be different:
 --     for MBE it says that A and B must be of the same type, and suggests adding an order between A and B
 --     for UNI it says that it cannot decide whether A \/ B is of type C or D, and suggests adding an order between C and D
 --   In addition, MBE requires that both sides are not generalizable. UNI does not, and simply propagates this property.
 -- MBG is like MBE, but will only try to generalize the right hand side (when allowed)

deriv' :: (Applicative f)
       => ((SrcOrTgt, t -> (Expression, (Bool, Bool))), (SrcOrTgt, t -> (Expression, (Bool, Bool))))
       -> t
       -> f ((String, Bool), (String, Bool))
deriv' (a,b) es = let (sourceOrTarget1, (e1, t1)) = resolve es a
                      (sourceOrTarget2, (e2, t2)) = resolve es b
                  in pure ((gc sourceOrTarget1 e1, t1), (gc sourceOrTarget2 e2, t2))
instance Functor TT where
  fmap f (UNI a b) = UNI (f a) (f b)
  fmap f (ISC a b) = ISC (f a) (f b)
  fmap f (MBE a b) = MBE (f a) (f b)
  fmap f (MBG a b) = MBG (f a) (f b)

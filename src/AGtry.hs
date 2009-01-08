

-- UUAGC 0.9.7 (AGtry.ag)
module AGtry where

import Char (isDigit) -- more from this module: Alpha,chr,ord,digitToInt,intToDigit,isAlphaNum,toUpper,toLower,isUpper)
import CommonClasses ( Identified(name))
import Collection ( Collection(isc,(>-),uni, rd))
import UU_Scanner
import Auxiliaries (chain, eqClass, commaEng, sord, sort', eqCl)
import Classification
         ( Classification(Cl)
         ,locatesF,makeClassificationsF,preCl,mapCl)
import Typology (Inheritance(Isa),genEq,typology)
import ADLdef
import ShowHS(showHS)
import ShowADL
import CC_aux ( order,makeConceptSpace, put_gE
              , renumberRules,pMeaning,shSigns,anything
              , gEtabG)

diagl = 31
diagc = 0

chop [x]    = []
chop (x:xs) = ([x],xs): [(x:l, r)| (l,r)<-chop xs]
chop []     = []

-- (l,r) `elem` chop xs => not (null l) && not (null r)
-- Proof:
-- a) The theorem holds for all xs of length <= 1.
-- b) Suppose the theorem holds for all xs of length <= n, with xs::[C]
--    The induction hypothesis is therefore:
--    b.0) Assume (induction hypothesis): (l,r) `elem` chop xs && length xs<=n => not (null l) && not (null r)
--    b.1) let x::C
--    b.2)      (l,r)<-chop xs
--         implies    {semantics of list comprehension}
--              (l,r) `elem` chop xs
--         implies    {let length xs<=n and induction hypothesis}
--              not (null l) && not (null r)
--    b.3)
--              chop (x:xs) = ([x],xs): [(x:l, r)| (l,r)<-chop xs]
--         implies    {assume length xs<=n, then by b.2}
--              not (null l) && not (null r)
--         implies    {The first clause in the definition of chop ensures: not (null xs)}
--              not (null (x:l)) && not (null r) && not (null [x]) && not (null xs)
--    b.4) TODO: finish proof.

mkCtxAG :: [(Context,(Gens,Declarations))] -> String -> Classification (Context,(Gens,Declarations))
mkCtxAG ctxs ctxName
 = if null ctTrees
   then error ("!Error of type 0: unknown context "++ctxName
               ++".\ntesting: length ctxs="++show (length ctxs)
               ++if length ctxs>1
                 then ".\nPick one of "++chain ", " (map (name.fst) (tail ctxs))++" or "++(name.fst.head) ctxs
                 else if null ctxs then "No context available" else ".\nPick "++(name.fst.head) ctxs
              ) else
   let ecl = [head cl|cl<-eqClass (==) (map (name.fst) ctxs), length cl>1] in
   if null ecl then head ctTrees else
   error ("!Error of type 0: multiple contexts carrying the name "++commaEng "and" ecl++".")
   where
    ctTrees = [t|tree<-trees, t<-locatesF ((==ctxName).name.fst) tree]
-- multiple ctTrees may occur if contexts are used multiply in different ctTrees.
    trees = makeClassificationsF (name.fst) tuples++[Cl s []| s<-singles]
            where tuples = [ ((spc,(mG,mD)),(gen,(mG',mD')))
                           | (spc,(mG,mD))<-ctxs, o<-extends spc
                           , (gen,(mG',mD'))<-ctxs,name gen==o]
                  singles= [ (spc,(mG,mD))
                           | (spc,(mG,mD))<-ctxs, not (name spc `elem` rd [name c|(s,g)<-tuples, (c,m)<-[s,g]])]

lubb gE a b = if a `gE` b then b else a
ordd gE a b = a `gE` b || b `gE` a

-- irred removes redundancy from a list of declarations.
-- Intended for use in the type checker AGtry only.
-- Precondition: a `gEq` a' && b `gEq` b' || a' `gEq` a && b' `gEq` b

irredC :: GenR -> [Concept] -> [Concept]
irredC gE cs = map (foldr1 lub) (eqClass order cs)
               where a `lub` b   = if a `gE` b then b else a
                     a `order` b = a `gE` b || b `gE` a
irredT :: GenR -> [(Concept,Concept)] -> [(Concept,Concept)]
irredT gE ccs = map (foldr1 lub) (eqClass order ccs)
                where (a,a') `lub` (b,b')   = if a `gE` b && a' `gE` b' then (b,b') else (a,a')
                      (a,a') `order` (b,b') = (a `gE` b && a' `gE` b') || (b `gE` a && b' `gE` a')
irredT' :: GenR -> [(Concept,Concept)] -> [(Concept,Concept)]
irredT' gE ccs = map (foldr1 glb) (eqClass order ccs)
                 where (a,a') `glb` (b,b')   = if a `gE` b && a' `gE` b' then (a,a') else (b,b')
                       (a,a') `order` (b,b') = (a `gE` b && a' `gE` b') || (b `gE` a && b' `gE` a')
irredD :: GenR -> [Declaration] -> [Declaration]
irredD gE ds = map (foldr1 lub) (eqClass order ds)
               where m `lub` m'   = if a `gE` b && a' `gE` b' then m else m'
                                    where (a,a') = sign m; (b,b') = sign m'
                     m `order` m' = (a `gE` b && a' `gE` b') || (b `gE` a && b' `gE` a')
                                    where (a,a') = sign m; (b,b') = sign m'
irredM :: Morphic a => GenR -> [a] -> [a]
irredM gE as = map (foldr1 lub) (eqClass order as)
               where m `lub` m'   = if a `gE` b && a' `gE` b' then m else m'
                                    where (a,a') = sign m; (b,b') = sign m'
                     m `order` m' = (a `gE` b && a' `gE` b') || (b `gE` a && b' `gE` a')
                                    where (a,a') = sign m; (b,b') = sign m'


renumber ss = [ Sgn nm a b props prL prM prR cs expla pos nr sig | (nr,Sgn nm a b props prL prM prR cs expla pos _ sig)<-zip [1..] ss]
mergecontents ss = [ Sgn nm a b (rd [p|s<-cl,p<-multiplicities s])
                                prL prM prR
                                (sord [l|s<-cl,l<-contents s])
                                expla p nr sig
                   | (nr,cl)<-zip [1..] (eqClass (==) ss)
                   , Sgn nm a b props prL prM prR cs expla p _ sig <-
                       take 1 ([s|s@(Sgn _ _ _ _ prL prM prR _ _ _ _ _)<-cl
                                 , not (null (prL++prM++prR))]++cl)
                   ]

deriveMults ks sgs
 = error "TODO DeriveMults moet ook multipliciteiten van relaties afleiden van expressies."++
   [Sgn nm a b (props `uni` derived s) prL prM prR cs expla pos nr sig| s@(Sgn nm a b props prL prM prR cs expla pos nr sig)<-sgs]
   where derived s = []


subExprCheck r@(Ru c antc p cons cpu expla sgn nr pn)
 = (Ru c antc p cons matches expla sgn nr pn
   , if not (null cerrs) then [showADL r++"\n"++"COMPUTING not allowed for closures (neither * nor +)."] else
     [ 
       if length es>1
       then "cannot decide which occurrence of ("++showADL c++") you mean:\n"++chain "\n" [showADL e++"["++show (source e)++"*"++show (target e)++"]"| e<-es]
       else "No match found for " ++showADL c++" in this rule."
     | (c,es)<-cpu']
   )
   where
    (matches,str) = recur ([antc|c/=AlwaysExpr]++[cons])
    cerrs = [] -- obsolete? map (drop 5) ([ name m| m<-mors r] `isc` map (("Clos_"++).name.head.mors.fst) cpu')
    cpu'  = [ (c,es) | c<-cpu, es<-[rd[m| m<-matches, c `match` m]], length es/=1]
    recur  []    = ([],"")
    recur (e:es) = (subexps++matches , "   recur "++showADL (e:es)++" = "++showADL (subexps++matches)++"\nbecause\n"++
                   "("++showADL subexps++" = subexprs ("++showADL e++") and\n"++
                   "("++showADL matches++" = recur ("++showADL es++")\n--------\n"++str'++str'')
     where (subexps,str')  = subexprs e
           (matches,str'') = recur es
    subexprs e@(Tm m)    = ([e| c<-cpu, e `match` c] , "")
    subexprs e@(Tc e')   = subexprs e'
    subexprs e@(F ts)    = ([e| c<-cpu, e `match` c]++matches ,  showADL matches++" = recur ("++showADL ts++")\n--------\n"++str)
                           where (matches,str) = recur ts
    subexprs e@(Fd ts)   = ([e| c<-cpu, e `match` c]++matches ,  showADL matches++" = recur ("++showADL ts++")\n--------\n"++str)
                           where (matches,str) = recur ts
    subexprs e@(Fi fs)   = ([e| c<-cpu, e `match` c]++matches ,  showADL matches++" = recur ("++showADL fs++")\n--------\n"++str)
                           where (matches,str) = recur fs
    subexprs e@(Fu fs)   = ([e| c<-cpu, e `match` c]++matches ,  showADL matches++" = recur ("++showADL fs++")\n--------\n"++str)
                           where (matches,str) = recur fs
    subexprs e@(K0 e')   = ([e| c<-cpu, e `match` c]++matches ,  showADL matches++" = subexprs ("++showADL e'++")\n--------\n"++str)
                           where (matches,str) = subexprs e'
    subexprs e@(K1 e')   = ([e| c<-cpu, e `match` c]++matches ,  showADL matches++" = subexprs ("++showADL e'++")\n--------\n"++str)
                           where (matches,str) = subexprs e'
    subexprs e@(Cp e')   = ([e| c<-cpu, e `match` c]++matches ,  showADL matches++" = subexprs ("++showADL e'++")\n--------\n"++str)
                           where (matches,str) = subexprs e'
    match  (Tm m) (Tm m')  = name m == name m' && (sign m `order` sign m' || sign (flp m) `order` sign m')
    match  (F ts) (F ts')  = length ms==length ms' && and [f `match` f'| (f,f')<-zip ms ms']
                             where ms = filter isMph ts; ms' = filter isMph ts'
    match (Fd ts) (Fd ts') = length ms==length ms' && and [f `match` f'| (f,f')<-zip ms ms']
                             where ms = filter isMph ts; ms' = filter isMph ts'
    match (Fi fs) (Fi fs') = and [f `match` f'|(f,f')<-zip fs fs'] -- or [ and [f `match` f'|(f,f')<-zip fs prm] | prm<-perms fs']
    match (Fu fs) (Fu fs') = and [f `match` f'|(f,f')<-zip fs fs'] -- or [ and [f `match` f'|(f,f')<-zip fs prm] | prm<-perms fs']
    match  (K0 e) (K0 e')  = e `match` e'
    match  (K1 e) (K1 e')  = e `match` e'
    match  (Cp e) (Cp e')  = e `match` e'
    match  (Tc e) e'       = match e e'
    match  e (Tc e')       = match e e'
    match _ _              = False


llub gE ss ss'
 = if null ss'
   then if null ss
        then []
        else ss
   else if null ss
        then ss'
        else rd [ (if a `gE` a' then a' else a, if b `gE` b' then b' else b)
                | (a,b)<-ss, (a',b')<-ss'
                , (a `gE` a' || a' `gE` a) && (b `gE` b' || b' `gE` b)
                ]
tlub gE cs [] = []
tlub gE [] cs = []
tlub gE as bs = (map (foldr1 lub) . eqClass order) [lub a b| a<-as, b<-bs, a `order` b]
                where lub a b = if a `gE` b then b else a
                      order a b = a `gE` b || b `gE` a
-- comp is bedoeld voor de typering van alle mogelijkheden bij een ;. Alle mogelijke types van l worden gematcht met alle mogelijke types van r, om alle mogelijke types van l;r te verkrijgen.
comp gE ls rs = [(f a a' b,f b' a' b)|(a,b)<-ls, (a',b')<-rs, a' `gE` b || b `gE` a']
                where f Anything x y | x `gE` y = y
                                     | y `gE` x = x
                      f a _ _ = a
comps gE tss  = if null ss then [] else foldr1 (comp gE) ss
                where ss=[s|s<-tss, not (null s)]
distr pos (a,b) gE css [t] = map Tm (ids gE a (source t))++[t]++map Tm (ids gE (target t) b)
distr pos (a,b) gE css ts
 = if or [length cs>1|cs<-css]
   then error ("Fatal: ambiguities "++show css)
   else concat [map Tm (ids gE x (source t))++[t|not (eq t)]| (x,y,t)<-zip3 ([a]++cs) (cs++[b]) ts]++map Tm (ids gE (target (last ts)) b)
   where cs  = [c|[c]<-css]
         eq :: Expression -> Bool
         eq t = idsOnly t && source t==target t
-- ids creates an identity with source x and target y, but only if necessary (that is: when x and y differ)
ids gE x y | x==y      = []
           | x `gE` y  = [I [] x y False]
           | otherwise = [I [] y x True]
-- Architecture ------------------------------------------------
-- cata
sem_Architecture (Arch _cs )  =
    (sem_Architecture_Arch (sem_Contexts _cs ) )
sem_Architecture_Arch cs_  =
    (let _lhsOcs =
             _csIcontexts
         _csOctxs =
             _csIover
         _lhsOsErr =
             _csIsErr
         ( _csIcontexts,_csIover,_csIsErr) =
             (cs_ _csOctxs )
     in  ( _lhsOcs,_lhsOsErr))
-- Concept -----------------------------------------------------
-- cata
sem_Concept (Anything )  =
    (sem_Concept_Anything )
sem_Concept (C _c _gE _os )  =
    (sem_Concept_C _c _gE _os )
sem_Concept (NOthing )  =
    (sem_Concept_NOthing )
sem_Concept (S )  =
    (sem_Concept_S )
sem_Concept_Anything  =
    (\ _lhsIgE ->
         (let _lhsOconcept =
                  Anything
              _lhsOnm =
                  "Anything"
          in  ( _lhsOconcept,_lhsOnm)))
sem_Concept_C c_ gE_ os_  =
    (\ _lhsIgE ->
         (let _lhsOconcept =
                  C c_ _lhsIgE []
              _lhsOnm =
                  c_
          in  ( _lhsOconcept,_lhsOnm)))
sem_Concept_NOthing  =
    (\ _lhsIgE ->
         (let _lhsOconcept =
                  NOthing
              _lhsOnm =
                  "NOthing"
          in  ( _lhsOconcept,_lhsOnm)))
sem_Concept_S  =
    (\ _lhsIgE ->
         (let _lhsOconcept =
                  S
              _lhsOnm =
                  "ONE"
          in  ( _lhsOconcept,_lhsOnm)))
-- ConceptDef --------------------------------------------------
-- cata
sem_ConceptDef (Cd _pos _nm _def _ref )  =
    (sem_ConceptDef_Cd _pos _nm _def _ref )
sem_ConceptDef_Cd pos_ nm_ def_ ref_  =
    (let _lhsOc =
             Cd pos_ nm_ def_ ref_
     in  ( _lhsOc))
-- ConceptDefs -------------------------------------------------
-- cata
sem_ConceptDefs list  =
    (Prelude.foldr sem_ConceptDefs_Cons sem_ConceptDefs_Nil (Prelude.map sem_ConceptDef list) )
sem_ConceptDefs_Cons hd_ tl_  =
    (let _lhsOconDefs =
             _hdIc : _tlIconDefs
         ( _hdIc) =
             (hd_ )
         ( _tlIconDefs) =
             (tl_ )
     in  ( _lhsOconDefs))
sem_ConceptDefs_Nil  =
    (let _lhsOconDefs =
             []
     in  ( _lhsOconDefs))
-- Concepts ----------------------------------------------------
-- cata
sem_Concepts list  =
    (Prelude.foldr sem_Concepts_Cons sem_Concepts_Nil (Prelude.map sem_Concept list) )
sem_Concepts_Cons hd_ tl_  =
    (\ _lhsIgE ->
         (let _hdOgE =
                  _lhsIgE
              _tlOgE =
                  _lhsIgE
              ( _hdIconcept,_hdInm) =
                  (hd_ _hdOgE )
          in  ( )))
sem_Concepts_Nil  =
    (\ _lhsIgE ->
         (let 
          in  ( )))
-- Context -----------------------------------------------------
-- cata
sem_Context (Ctx _nm _on _isa _world _pats _rs _ds _cs _ks _os _pops )  =
    (sem_Context_Ctx _nm _on _isa _world (sem_Patterns _pats ) (sem_Rules _rs ) (sem_Declarations _ds ) (sem_ConceptDefs _cs ) (sem_KeyDefs _ks ) (sem_ObjDefs _os ) (sem_Populations _pops ) )
sem_Context_Ctx nm_ on_ isa_ world_ pats_ rs_ ds_ cs_ ks_ os_ pops_  =
    (\ _lhsIctxTree
       _lhsIctxs ->
         (let _mD =
                  (                       renumber.mergecontents.concat) [mD| (context,(mG,mD)) <- preCl _lhsIctxTree]
              _mC =
                  mergecontents(_dsIrawDecls ++ _patsIrawDecls)
              _keys =
                  rd (_ksIkeyDefs ++ _patsIkeyDefs)
              _mGen =
                  (rd.concat) [mG| (context,(mG,mD)) <- preCl _lhsIctxTree]
              _inh =
                  Isa [(g,s)|G pos g s<- _mGen] (concs _mD>-rd [c|G pos g s<- _mGen, c<-[g,s]])
              _genE =
                  cmp where cmp Anything b = True
                            cmp a Anything = False
                            cmp NOthing b  = False
                            cmp a NOthing  = True
                            cmp a b        = if a==b then True else genEq (typology _inh) a b
              _cD =
                  makeConceptSpace _genE _patsImorphisms
              _patsOsDef =
                  _mD
              _rsOsDef =
                  _mD
              _dsOsDef =
                  _mD
              _ksOsDef =
                  _mD
              _osOsDef =
                  _mD
              _popsOsDef =
                  _mD
              _patsOgE =
                  _genE
              _rsOgE =
                  _genE
              _dsOgE =
                  _genE
              _ksOgE =
                  _genE
              _osOgE =
                  _genE
              _rsOpn =
                  ""
              _popsOgE =
                  _genE
              _osOiConcs =
                  []
              _osOiConc =
                  Anything
              _lhsOover =
                  [( put_gE _genE _cD
                     (Ctx nm_
                          on_
                          _inh
                          []
                          _patsIpatterns
                          (_patsIrules ++ _osIrules ++ _dsIrules ++ _ksIrules)
                          _mC
                          _csIconDefs
                          _keys
                          _osIobjDefs
                          _popsIpopus)
                  , (_patsImGen, _mC))]
              _lhsOcontext =
                  _ctx
              _ctx =
                  put_gE _genE _cD
                  ( Ctx nm_
                        on_
                        _inh
                        [cl|Cl r cls<-[mapCl fst _lhsIctxTree], cl<-cls]
                        _patsIpatterns
                        (_patsIrules ++ _osIrules ++ _dsIrules ++ _ksIrules)
                        (declarations _patsIpatterns)
                        (sort' name (rd (_csIconDefs ++ _patsIconDefs)))
                        _keys
                        _osIobjDefs
                        _popsIpopus)
              _lhsOsErr =
                  _patsIsErr ++ _ksIsErr ++ _osIsErr ++ _popsIsErr
              _patsOrnr =
                  1
              _osOrnr =
                  _patsIrnr
              _dsOrnr =
                  _osIrnr
              _ksOrnr =
                  _dsIrnr
              _lhsOnRules =
                  _ksIrnr-1
              _lhsOrules =
                  _patsIrules ++ _osIrules ++ _dsIrules ++ _ksIrules
              _rsOrnr =
                  _patsIrnr
              ( _patsIconDefs,_patsIkeyDefs,_patsImGen,_patsImorphisms,_patsIpatterns,_patsIrawDecls,_patsIrnr,_patsIrules,_patsIsErr,_patsIusedDecls) =
                  (pats_ _patsOgE _patsOrnr _patsOsDef )
              ( _rsIdeclarations,_rsImGen,_rsImorphisms,_rsIrnr,_rsIrules,_rsIsErr,_rsIusedDecls) =
                  (rs_ _rsOgE _rsOpn _rsOrnr _rsOsDef )
              ( _dsIdeclarations,_dsIrawDecls,_dsIrnr,_dsIrules,_dsIsErr) =
                  (ds_ _dsOgE _dsOrnr _dsOsDef )
              ( _csIconDefs) =
                  (cs_ )
              ( _ksIexprs,_ksIkeyDefs,_ksIrnr,_ksIrules,_ksIsErr) =
                  (ks_ _ksOgE _ksOrnr _ksOsDef )
              ( _osIobjDefs,_osIrnr,_osIrules,_osIsErr,_osIsources) =
                  (os_ _osOgE _osOiConc _osOiConcs _osOrnr _osOsDef )
              ( _popsIpopus,_popsIsErr) =
                  (pops_ _popsOgE _popsOsDef )
          in  ( _lhsOcontext,_lhsOnRules,_lhsOover,_lhsOrules,_lhsOsErr)))
-- Contexts ----------------------------------------------------
-- cata
sem_Contexts list  =
    (Prelude.foldr sem_Contexts_Cons sem_Contexts_Nil (Prelude.map sem_Context list) )
sem_Contexts_Cons hd_ tl_  =
    (\ _lhsIctxs ->
         (let _lhsOcontexts =
                  _hdIcontext : _tlIcontexts
              _lhsOover =
                  _hdIover ++ _tlIover
              _hdOctxTree =
                  mkCtxAG _lhsIctxs (name _hdIcontext)
              _lhsOsErr =
                  _hdIsErr ++ _tlIsErr
              _hdOctxs =
                  _lhsIctxs
              _tlOctxs =
                  _lhsIctxs
              ( _hdIcontext,_hdInRules,_hdIover,_hdIrules,_hdIsErr) =
                  (hd_ _hdOctxTree _hdOctxs )
              ( _tlIcontexts,_tlIover,_tlIsErr) =
                  (tl_ _tlOctxs )
          in  ( _lhsOcontexts,_lhsOover,_lhsOsErr)))
sem_Contexts_Nil  =
    (\ _lhsIctxs ->
         (let _lhsOover =
                  []
              _lhsOcontexts =
                  []
              _lhsOsErr =
                  []
          in  ( _lhsOcontexts,_lhsOover,_lhsOsErr)))
-- Declaration -------------------------------------------------
-- cata
sem_Declaration (Iscompl _g _s )  =
    (sem_Declaration_Iscompl (sem_Concept _g ) (sem_Concept _s ) )
sem_Declaration (Isn _g _s )  =
    (sem_Declaration_Isn (sem_Concept _g ) (sem_Concept _s ) )
sem_Declaration (Sgn _nm _a _b _props _prL _prM _prR _content _expla _morPos _nr _sig )  =
    (sem_Declaration_Sgn _nm (sem_Concept _a ) (sem_Concept _b ) _props _prL _prM _prR _content _expla _morPos _nr _sig )
sem_Declaration (Vs _g _s )  =
    (sem_Declaration_Vs (sem_Concept _g ) (sem_Concept _s ) )
sem_Declaration_Iscompl g_ s_  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr =
                  []
              _lhsOrnr =
                  _lhsIrnr
              _lhsOrules =
                  []
              _lhsOdeclaration =
                  Iscompl _gIconcept _sIconcept
              _lhsOrawDecl =
                  Iscompl _gIconcept _sIconcept
              _lhsOnm =
                  "I"
              _gOgE =
                  _lhsIgE
              _sOgE =
                  _lhsIgE
              ( _gIconcept,_gInm) =
                  (g_ _gOgE )
              ( _sIconcept,_sInm) =
                  (s_ _sOgE )
          in  ( _lhsOdeclaration,_lhsOnm,_lhsOrawDecl,_lhsOrnr,_lhsOrules,_lhsOsErr)))
sem_Declaration_Isn g_ s_  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr =
                  []
              _lhsOrnr =
                  _lhsIrnr
              _lhsOrules =
                  []
              _lhsOdeclaration =
                  Isn _gIconcept _sIconcept
              _lhsOrawDecl =
                  Isn _gIconcept _sIconcept
              _lhsOnm =
                  "I"
              _gOgE =
                  _lhsIgE
              _sOgE =
                  _lhsIgE
              ( _gIconcept,_gInm) =
                  (g_ _gOgE )
              ( _sIconcept,_sInm) =
                  (s_ _sOgE )
          in  ( _lhsOdeclaration,_lhsOnm,_lhsOrawDecl,_lhsOrnr,_lhsOrules,_lhsOsErr)))
sem_Declaration_Sgn nm_ a_ b_ props_ prL_ prM_ prR_ content_ expla_ morPos_ nr_ sig_  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr =
                  if _aInm == _bInm then [] else
                  let ps = props_ `isc` [Trn,Rfx,Sym,Asy] in
                  if null ps then []
                  else ["7 in "++show morPos_++"\n   Heterogeneous relation "++
                        nm_++"["++ _aInm ++"*"++ _bInm ++
                        "]\n   may not be declared "++commaEng "and" [pMeaning p| p<-ps]++
                        ".\n   (This relation is heterogeneous because "++ _aInm ++" and "++ _bInm ++
                        "\n   are different concepts).\n"]
              _lhsOrnr =
                  _lhsIrnr + length (multRules _msignat)
              _lhsOrules =
                  renumberRules _lhsIrnr (multRules _msignat)
              _lhsOdeclaration =
                  _msignat
              _msignat =
                  head ([ s
                        | s <- _lhsIsDef, nm_==name s, _aIconcept==source s, _bIconcept==target s]++
                        [error ("Missing "++showHS "" (Sgn nm_ _aIconcept _bIconcept props_ prL_ prM_ prR_ content_ expla_ morPos_ 0 sig_)++" in AGtry.ag\n"++ show _lhsIsDef)])
              _lhsOrawDecl =
                  Sgn nm_ _aIconcept _bIconcept props_ prL_ prM_ prR_ content_ expla_ morPos_ 0 sig_
              _lhsOnm =
                  nm_
              _aOgE =
                  _lhsIgE
              _bOgE =
                  _lhsIgE
              ( _aIconcept,_aInm) =
                  (a_ _aOgE )
              ( _bIconcept,_bInm) =
                  (b_ _bOgE )
          in  ( _lhsOdeclaration,_lhsOnm,_lhsOrawDecl,_lhsOrnr,_lhsOrules,_lhsOsErr)))
sem_Declaration_Vs g_ s_  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr =
                  []
              _lhsOrnr =
                  _lhsIrnr
              _lhsOrules =
                  []
              _lhsOdeclaration =
                  Vs _gIconcept _sIconcept
              _lhsOrawDecl =
                  Vs _gIconcept _sIconcept
              _lhsOnm =
                  "V"
              _gOgE =
                  _lhsIgE
              _sOgE =
                  _lhsIgE
              ( _gIconcept,_gInm) =
                  (g_ _gOgE )
              ( _sIconcept,_sInm) =
                  (s_ _sOgE )
          in  ( _lhsOdeclaration,_lhsOnm,_lhsOrawDecl,_lhsOrnr,_lhsOrules,_lhsOsErr)))
-- Declarations ------------------------------------------------
-- cata
sem_Declarations list  =
    (Prelude.foldr sem_Declarations_Cons sem_Declarations_Nil (Prelude.map sem_Declaration list) )
sem_Declarations_Cons hd_ tl_  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr =
                  _hdIsErr ++ _tlIsErr
              _hdOrnr =
                  _lhsIrnr
              _tlOrnr =
                  _hdIrnr
              _lhsOrnr =
                  _tlIrnr
              _lhsOrules =
                  _hdIrules ++ _tlIrules
              _lhsOdeclarations =
                  _hdIdeclaration: _tlIdeclarations
              _lhsOrawDecls =
                  _hdIrawDecl: _tlIrawDecls
              _hdOgE =
                  _lhsIgE
              _hdOsDef =
                  _lhsIsDef
              _tlOgE =
                  _lhsIgE
              _tlOsDef =
                  _lhsIsDef
              ( _hdIdeclaration,_hdInm,_hdIrawDecl,_hdIrnr,_hdIrules,_hdIsErr) =
                  (hd_ _hdOgE _hdOrnr _hdOsDef )
              ( _tlIdeclarations,_tlIrawDecls,_tlIrnr,_tlIrules,_tlIsErr) =
                  (tl_ _tlOgE _tlOrnr _tlOsDef )
          in  ( _lhsOdeclarations,_lhsOrawDecls,_lhsOrnr,_lhsOrules,_lhsOsErr)))
sem_Declarations_Nil  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr =
                  []
              _lhsOrnr =
                  _lhsIrnr
              _lhsOrules =
                  []
              _lhsOdeclarations =
                  []
              _lhsOrawDecls =
                  []
          in  ( _lhsOdeclarations,_lhsOrawDecls,_lhsOrnr,_lhsOrules,_lhsOsErr)))
-- Expression --------------------------------------------------
-- cata
sem_Expression (Cp _e )  =
    (sem_Expression_Cp (sem_Expression _e ) )
sem_Expression (F _ts )  =
    (sem_Expression_F (sem_Expressions _ts ) )
sem_Expression (Fd _ts )  =
    (sem_Expression_Fd (sem_Expressions _ts ) )
sem_Expression (Fi _fs )  =
    (sem_Expression_Fi (sem_Expressions _fs ) )
sem_Expression (Fu _fs )  =
    (sem_Expression_Fu (sem_Expressions _fs ) )
sem_Expression (K0 _e )  =
    (sem_Expression_K0 (sem_Expression _e ) )
sem_Expression (K1 _e )  =
    (sem_Expression_K1 (sem_Expression _e ) )
sem_Expression (Tc _c )  =
    (sem_Expression_Tc (sem_Expression _c ) )
sem_Expression (Tm _m )  =
    (sem_Expression_Tm (sem_Morphism _m ) )
sem_Expression_Cp e_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsigns =
                  _eIsigns
              _lhsOexpr =
                  Cp _eIexpr
              _lhsOraw =
                  Cp _eIraw
              _lhsOmorphisms =
                  _eImorphisms
              _lhsOsErr =
                  _eIsErr
              _eOrnr =
                  _lhsIrnr
              _lhsOrnr =
                  _eIrnr
              _lhsOrules =
                  _eIrules
              _eOgE =
                  _lhsIgE
              _eOisign =
                  _lhsIisign
              _eOpn =
                  _lhsIpn
              _eOpos =
                  _lhsIpos
              _eOsDef =
                  _lhsIsDef
              ( _eIexpr,_eImorphisms,_eIraw,_eIrnr,_eIrules,_eIsErr,_eIsigns) =
                  (e_ _eOgE _eOisign _eOpn _eOpos _eOrnr _eOsDef )
          in  ( _lhsOexpr,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsigns)))
sem_Expression_F ts_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsigns =
                  _sgns
              _sgns =
                  comps _lhsIgE _tsIsignss
              _trpls =
                  if null _sgns
                  then [ (head (irredC _lhsIgE [s| (s,t)<-sl]),l,sl,tlub _lhsIgE (rd(map snd sl)) (rd(map fst sr)),sr,r,head (irredC _lhsIgE [t| (s,t)<-sr]))
                       | ((l,r),(sls,srs))<-zip (chop _tsIexprs) (chop _tsIsignss)
                       , sl <- [comps _lhsIgE sls], not (null sl)
                       , sr <- [comps _lhsIgE srs], not (null sr)
                       ]
                  else [ (a,l,sl,tlub _lhsIgE (rd(map snd sl)) (rd(map fst sr)),sr,r,b)
                       | (a,b) <- take 1 _lhsIisign
                       , ((l,r),(sls,srs))<-zip (chop _tsIexprs) (chop _tsIsignss)
                       , sl <- [[(s,t)| (s,t)<-comps _lhsIgE sls,if a/=Anything then s `_lhsIgE` a else True]]
                       , sr <- [[(s,t)| (s,t)<-comps _lhsIgE srs,if b/=Anything then t `_lhsIgE` b else True]]
                       ]
              _lhsOexpr =
                  _dis
              _dis =
                  if null _lhsIisign then F (_tsIexprs) else
                  let (s,t)= head _lhsIisign
                  in F (
                        distr _lhsIpos (s,t) _lhsIgE [cs|(a,l,sl,cs,sr,r,b)<- _trpls, s `_lhsIgE` a, t `_lhsIgE` b] _tsIexprs)
              _tsOseptets =
                  _trpls
              _lhsOraw =
                  F _tsIraw
              _lhsOmorphisms =
                  _tsImorphisms
              _lhsOsErr =
                  _tsIsErr++
                  [ "5 in "++show _lhsIpos++"\n   Type error in composing\n   "++
                    chain " and\n   " [show l++" :: "++shSigns sgnsl++" with "++show r++" :: "++shSigns sgnsr
                                  | (l,sgnsl,r,sgnsr)<-errs]++"\n"
                  | errs<- [[(l,sgnsl,r,sgnsr) | ((sgnsl,sgnsr),(l,r))<- zip (zip _tsIsignss (tail _tsIsignss)) (zip _tsIraw (tail _tsIraw))
                                               , correct<-[[(sl,tl,sr,tr)|(sl,tl)<-sgnsl, (sr,tr)<-sgnsr, tl `order` sr || anything tl || anything sr]]
                                               , null correct]]
                  , not (null errs)
                  ]++                      [ "6 in "++show _lhsIpos++"\n   Ambiguous composition over concepts "++show cs++" of\n   "++
                    show (F ls)++" :: "++shSigns sl++
                    " and\n   "++
                    show (F rs)++" :: "++shSigns sr++"\n"
                  |(a,ls,sl,cs,sr,rs,b)<- _trpls, length cs>1]
              _tsOrnr =
                  _lhsIrnr
              _lhsOrnr =
                  _tsIrnr
              _lhsOrules =
                  _tsIrules
              _tsOgE =
                  _lhsIgE
              _tsOisign =
                  _lhsIisign
              _tsOpn =
                  _lhsIpn
              _tsOpos =
                  _lhsIpos
              _tsOsDef =
                  _lhsIsDef
              ( _tsIexprs,_tsImorphisms,_tsIraw,_tsIrnr,_tsIrules,_tsIsErr,_tsIsignss) =
                  (ts_ _tsOgE _tsOisign _tsOpn _tsOpos _tsOrnr _tsOsDef _tsOseptets )
          in  ( _lhsOexpr,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsigns)))
sem_Expression_Fd ts_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsigns =
                  _sgns
              _sgns =
                  comps _lhsIgE _tsIsignss
              _trpls =
                  if null _sgns
                  then [ (head (irredC _lhsIgE [s| (s,t)<-sl]),l,sl,tlub _lhsIgE (rd(map snd sl)) (rd(map fst sr)),sr,r,head (irredC _lhsIgE [t| (s,t)<-sr]))
                       | ((l,r),(sls,srs))<-zip (chop _tsIexprs) (chop _tsIsignss)
                       , sl <- [comps _lhsIgE sls], not (null sl)
                       , sr <- [comps _lhsIgE srs], not (null sr)
                       ]
                  else [ (a,l,sl,tlub _lhsIgE (rd(map snd sl)) (rd(map fst sr)),sr,r,b)
                       | (a,b) <- take 1 _lhsIisign
                       , ((l,r),(sls,srs))<-zip (chop _tsIexprs) (chop _tsIsignss)
                       , sl <- [[(s,t)| (s,t)<-comps _lhsIgE sls,if a/=Anything then s `_lhsIgE` a else True]]
                       , sr <- [[(s,t)| (s,t)<-comps _lhsIgE srs,if b/=Anything then t `_lhsIgE` b else True]]
                       ]
              _lhsOexpr =
                  _dis
              _dis =
                  if null _lhsIisign then Fd (_tsIexprs) else
                  let (s,t)= head _lhsIisign
                  in Fd (
                         distr _lhsIpos (s,t) _lhsIgE [cs|(a,l,sl,cs,sr,r,b)<- _trpls, a==s, b==t] _tsIexprs)
              _tsOseptets =
                  _trpls
              _lhsOraw =
                  Fd _tsIraw
              _lhsOmorphisms =
                  _tsImorphisms
              _lhsOsErr =
                  _tsIsErr++
                  [ "5 in "++show _lhsIpos++"\n   Type error in composing\n   "++
                    chain " and\n   " [show l++" :: "++shSigns sgnsl++" with "++show r++" :: "++shSigns sgnsr
                                  | (l,sgnsl,r,sgnsr)<-errs]
                  | errs<- [[(l,sgnsl,r,sgnsr) | ((sgnsl,sgnsr),(l,r))<- zip (zip _tsIsignss (tail _tsIsignss)) (zip _tsIraw (tail _tsIraw))
                                               , correct<-[[(sl,tl,sr,tr)|(sl,tl)<-sgnsl, (sr,tr)<-sgnsr, tl `order` sr || anything tl || anything sr]]
                                               , null correct]]
                  , not (null errs)
                  ]++
                  [ "6 in "++show _lhsIpos++"\n   Ambiguous relative addition over concepts "++show cs++" of\n   "++
                    show (Fd ls)++" :: "++shSigns sl++
                    " and\n   "++
                    show (Fd rs)++" :: "++shSigns sr++"\n"
                  |(a,ls,sl,cs,sr,rs,b)<- _trpls, length cs>1]
              _tsOrnr =
                  _lhsIrnr
              _lhsOrnr =
                  _tsIrnr
              _lhsOrules =
                  _tsIrules
              _tsOgE =
                  _lhsIgE
              _tsOisign =
                  _lhsIisign
              _tsOpn =
                  _lhsIpn
              _tsOpos =
                  _lhsIpos
              _tsOsDef =
                  _lhsIsDef
              ( _tsIexprs,_tsImorphisms,_tsIraw,_tsIrnr,_tsIrules,_tsIsErr,_tsIsignss) =
                  (ts_ _tsOgE _tsOisign _tsOpn _tsOpos _tsOrnr _tsOsDef _tsOseptets )
          in  ( _lhsOexpr,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsigns)))
sem_Expression_Fi fs_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsigns =
                  if null _sgns then error("Fatal: empty @sgns in Fi-expression "++showADL (Fi _fsIraw)) else _sgns
              _sgns =
                  if null _fsIsignss then error("Fatal: empty @fs.signss in expression "++showHS "" (Fi _fsIexprs)) else
                  foldr1 (llub _lhsIgE) _fsIsignss
              _fsOseptets =
                  []
              _lhsOexpr =
                  Fi _fsIexprs
              _dis =
                  let [(a,b)] = take 1 _lhsIisign
                      (a',b') = (source (head _fsIexprs), target (last _fsIexprs))
                  in Fi (                                    _fsIexprs                                    )
              _lhsOraw =
                  Fi _fsIraw
              _lhsOmorphisms =
                  _fsImorphisms
              _lhsOsErr =
                  if null _sgns
                  then ["4 in "++show _lhsIpos++"\n   Incompatible types in comparing\n   "++
                        commaEng "with\n  " [show f++" :: "++shSigns s| (f,s)<-zip _fsIraw _fsIsignss]++"\n"]
                  else _fsIsErr
              _fsOrnr =
                  _lhsIrnr
              _lhsOrnr =
                  _fsIrnr
              _lhsOrules =
                  _fsIrules
              _fsOgE =
                  _lhsIgE
              _fsOisign =
                  _lhsIisign
              _fsOpn =
                  _lhsIpn
              _fsOpos =
                  _lhsIpos
              _fsOsDef =
                  _lhsIsDef
              ( _fsIexprs,_fsImorphisms,_fsIraw,_fsIrnr,_fsIrules,_fsIsErr,_fsIsignss) =
                  (fs_ _fsOgE _fsOisign _fsOpn _fsOpos _fsOrnr _fsOsDef _fsOseptets )
          in  ( _lhsOexpr,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsigns)))
sem_Expression_Fu fs_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsigns =
                  if null _sgns then error("Fatal: empty @sgns in Fu-expression "++showADL (Fi _fsIraw)) else _sgns
              _sgns =
                  if null _fsIsignss then error("Fatal: empty @fs.signss in expression "++showHS "" (Fu _fsIexprs)) else
                  foldr1 (llub _lhsIgE) _fsIsignss
              _fsOseptets =
                  []
              _lhsOexpr =
                  Fu _fsIexprs
              _dis =
                  let [(a,b)] = take 1 _lhsIisign
                      (a',b') = (source (head _fsIexprs), target (last _fsIexprs))
                  in Fu (                                    _fsIexprs                                    )
              _lhsOraw =
                  Fu _fsIraw
              _lhsOmorphisms =
                  _fsImorphisms
              _lhsOsErr =
                  if null _sgns
                  then ["4 in "++show _lhsIpos++"\n   Incompatible types in comparing\n   "++
                        commaEng "with\n  " [show f++" :: "++shSigns s| (f,s)<-zip _fsIraw _fsIsignss]++"\n"]
                  else _fsIsErr
              _fsOrnr =
                  _lhsIrnr
              _lhsOrnr =
                  _fsIrnr
              _lhsOrules =
                  _fsIrules
              _fsOgE =
                  _lhsIgE
              _fsOisign =
                  _lhsIisign
              _fsOpn =
                  _lhsIpn
              _fsOpos =
                  _lhsIpos
              _fsOsDef =
                  _lhsIsDef
              ( _fsIexprs,_fsImorphisms,_fsIraw,_fsIrnr,_fsIrules,_fsIsErr,_fsIsignss) =
                  (fs_ _fsOgE _fsOisign _fsOpn _fsOpos _fsOrnr _fsOsDef _fsOseptets )
          in  ( _lhsOexpr,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsigns)))
sem_Expression_K0 e_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsigns =
                  _eIsigns
              _lhsOexpr =
                  Tm _closmor
              _closmor =
                  let [(a,b)] = take 1 _lhsIisign
                  in Mph (             (name.head.declarations) _eIexpr) _lhsIpos [] (a,b) True (Sgn (             (name.head.declarations) _eIexpr) a b [Trn,Rfx] "" "" "" [] "Closure" _lhsIpos 0 True)
              _lhsOraw =
                  K0 _eIraw
              _lhsOmorphisms =
                  _eImorphisms
              _lhsOsErr =
                  _eIsErr
              _eOrnr =
                  _lhsIrnr+1
              _lhsOrnr =
                  _eIrnr
              _lhsOrules =
                  let [(a,b)] = take 1 _lhsIisign
                  in Ru Equivalence
                        (Tm _closmor)
                        _lhsIpos
                        (K0 _eIexpr)
                        [Tm _closmor]
                        ""
                        (a,b)
                        _lhsIrnr
                        _lhsIpn
                     : _eIrules
              _eOgE =
                  _lhsIgE
              _eOisign =
                  _lhsIisign
              _eOpn =
                  _lhsIpn
              _eOpos =
                  _lhsIpos
              _eOsDef =
                  _lhsIsDef
              ( _eIexpr,_eImorphisms,_eIraw,_eIrnr,_eIrules,_eIsErr,_eIsigns) =
                  (e_ _eOgE _eOisign _eOpn _eOpos _eOrnr _eOsDef )
          in  ( _lhsOexpr,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsigns)))
sem_Expression_K1 e_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsigns =
                  _eIsigns
              _lhsOexpr =
                  Tm _closmor
              _closmor =
                  let [(a,b)] = take 1 _lhsIisign
                  in Mph (             (name.head.declarations) _eIexpr) _lhsIpos [] (a,b) True (Sgn (             (name.head.declarations) _eIexpr) a b [Trn] "" "" "" [] "Closure" _lhsIpos 0 False)
              _lhsOraw =
                  K1 _eIraw
              _lhsOmorphisms =
                  _eImorphisms
              _lhsOsErr =
                  _eIsErr
              _eOrnr =
                  _lhsIrnr+1
              _lhsOrnr =
                  _eIrnr
              _lhsOrules =
                  let [(a,b)] = take 1 _lhsIisign
                  in Ru Equivalence
                        (Tm _closmor)
                        _lhsIpos
                        (K1 _eIexpr)
                        [Tm _closmor]
                        ""
                        (a,b)
                        _lhsIrnr
                        _lhsIpn
                     : _eIrules
              _eOgE =
                  _lhsIgE
              _eOisign =
                  _lhsIisign
              _eOpn =
                  _lhsIpn
              _eOpos =
                  _lhsIpos
              _eOsDef =
                  _lhsIsDef
              ( _eIexpr,_eImorphisms,_eIraw,_eIrnr,_eIrules,_eIsErr,_eIsigns) =
                  (e_ _eOgE _eOisign _eOpn _eOpos _eOrnr _eOsDef )
          in  ( _lhsOexpr,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsigns)))
sem_Expression_Tc c_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsigns =
                  _cIsigns
              _lhsOexpr =
                  Tc _cIexpr
              _lhsOraw =
                  Tc _cIraw
              _lhsOmorphisms =
                  _cImorphisms
              _lhsOsErr =
                  _cIsErr
              _cOrnr =
                  _lhsIrnr
              _lhsOrnr =
                  _lhsIrnr
              _lhsOrules =
                  _cIrules
              _cOgE =
                  _lhsIgE
              _cOisign =
                  _lhsIisign
              _cOpn =
                  _lhsIpn
              _cOpos =
                  _lhsIpos
              _cOsDef =
                  _lhsIsDef
              ( _cIexpr,_cImorphisms,_cIraw,_cIrnr,_cIrules,_cIsErr,_cIsigns) =
                  (c_ _cOgE _cOisign _cOpn _cOpos _cOrnr _cOsDef )
          in  ( _lhsOexpr,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsigns)))
sem_Expression_Tm m_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsigns =
                  let (m,ss) = _mIraw in if isMph m then (if inline m then map sign ss else map (sign.flp) ss) else [sign m]
              _lhsOexpr =
                  Tm _mImorphism
              _lhsOraw =
                  let (m,ss) = _mIraw in Tm m
              _lhsOmorphisms =
                  [_mImorphism]
              _lhsOsErr =
                  _mIsErr
              _lhsOrnr =
                  _lhsIrnr
              _lhsOrules =
                  []
              _mOgE =
                  _lhsIgE
              _mOisign =
                  _lhsIisign
              _mOsDef =
                  _lhsIsDef
              ( _mIatts,_mIid,_mImorphism,_mInm,_mIpos,_mIraw,_mIsErr,_mIusedDecls,_mIyin) =
                  (m_ _mOgE _mOisign _mOsDef )
          in  ( _lhsOexpr,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsigns)))
-- Expressions -------------------------------------------------
-- cata
sem_Expressions list  =
    (Prelude.foldr sem_Expressions_Cons sem_Expressions_Nil (Prelude.map sem_Expression list) )
sem_Expressions_Cons hd_ tl_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef
       _lhsIseptets ->
         (let _lhsOsignss =
                  _hdIsigns: _tlIsignss
              _hdOisign =
                  if null _lhsIseptets then _lhsIisign else
                  _hsgn
              _hsgn =
                  [(a,c)| (a,l,sl,cs,sr,r,b)<- _lhsIseptets, length l==1, c<-cs]
              _tlOisign =
                  if null _lhsIseptets then _lhsIisign else
                  [(c,b)| (a,l,sl,cs,sr,r,b)<- _lhsIseptets, length l==1, c<-cs]
              _tlOseptets =
                  _septs
              _septs =
                  [ (c',tail l,rd[(c',c)|(a,c)<-sl],cs,sr,r,b)
                  | (a,l,sl,cs,sr,r,b)<- _lhsIseptets,length l>1,(a',c')<- _hsgn]
              _lhsOraw =
                  _hdIraw: _tlIraw
              _lhsOmorphisms =
                  _hdImorphisms ++ _tlImorphisms
              _lhsOsErr =
                  _hdIsErr ++ _tlIsErr
              _hdOrnr =
                  _lhsIrnr
              _tlOrnr =
                  _hdIrnr
              _lhsOrnr =
                  _tlIrnr
              _lhsOrules =
                  _hdIrules ++ _tlIrules
              _lhsOexprs =
                  _hdIexpr : _tlIexprs
              _hdOgE =
                  _lhsIgE
              _hdOpn =
                  _lhsIpn
              _hdOpos =
                  _lhsIpos
              _hdOsDef =
                  _lhsIsDef
              _tlOgE =
                  _lhsIgE
              _tlOpn =
                  _lhsIpn
              _tlOpos =
                  _lhsIpos
              _tlOsDef =
                  _lhsIsDef
              ( _hdIexpr,_hdImorphisms,_hdIraw,_hdIrnr,_hdIrules,_hdIsErr,_hdIsigns) =
                  (hd_ _hdOgE _hdOisign _hdOpn _hdOpos _hdOrnr _hdOsDef )
              ( _tlIexprs,_tlImorphisms,_tlIraw,_tlIrnr,_tlIrules,_tlIsErr,_tlIsignss) =
                  (tl_ _tlOgE _tlOisign _tlOpn _tlOpos _tlOrnr _tlOsDef _tlOseptets )
          in  ( _lhsOexprs,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsignss)))
sem_Expressions_Nil  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef
       _lhsIseptets ->
         (let _lhsOsignss =
                  []
              _lhsOraw =
                  []
              _lhsOmorphisms =
                  []
              _lhsOsErr =
                  []
              _lhsOrnr =
                  _lhsIrnr
              _lhsOrules =
                  []
              _lhsOexprs =
                  []
          in  ( _lhsOexprs,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsignss)))
-- Gen ---------------------------------------------------------
-- cata
sem_Gen (G _pos _genus _spec )  =
    (sem_Gen_G _pos (sem_Concept _genus ) (sem_Concept _spec ) )
sem_Gen_G pos_ genus_ spec_  =
    (\ _lhsIgE
       _lhsIpn
       _lhsIrnr ->
         (let _lhsOrnr =
                  _lhsIrnr+1
              _lhsOrules =
                  [ Ru Implication
                        (Tm (mIs _specIconcept))
                        pos_
                        (Tm (mIs _genusIconcept))
                        [Tm (mIs _specIconcept), Tm (mIs _genusIconcept)]
                        ""
                        (_specIconcept, _genusIconcept)
                        _lhsIrnr
                        _lhsIpn
                  ]
              _lhsOgen =
                  G pos_ _genusIconcept _specIconcept
              _genusOgE =
                  _lhsIgE
              _specOgE =
                  _lhsIgE
              ( _genusIconcept,_genusInm) =
                  (genus_ _genusOgE )
              ( _specIconcept,_specInm) =
                  (spec_ _specOgE )
          in  ( _lhsOgen,_lhsOrnr,_lhsOrules)))
-- Gens --------------------------------------------------------
-- cata
sem_Gens list  =
    (Prelude.foldr sem_Gens_Cons sem_Gens_Nil (Prelude.map sem_Gen list) )
sem_Gens_Cons hd_ tl_  =
    (\ _lhsIgE
       _lhsIpn
       _lhsIrnr ->
         (let _hdOrnr =
                  _lhsIrnr
              _tlOrnr =
                  _hdIrnr
              _lhsOrnr =
                  _tlIrnr
              _lhsOrules =
                  _hdIrules ++ _tlIrules
              _lhsOlist =
                  _hdIgen : _tlIlist
              _hdOgE =
                  _lhsIgE
              _hdOpn =
                  _lhsIpn
              _tlOgE =
                  _lhsIgE
              _tlOpn =
                  _lhsIpn
              ( _hdIgen,_hdIrnr,_hdIrules) =
                  (hd_ _hdOgE _hdOpn _hdOrnr )
              ( _tlIlist,_tlIrnr,_tlIrules) =
                  (tl_ _tlOgE _tlOpn _tlOrnr )
          in  ( _lhsOlist,_lhsOrnr,_lhsOrules)))
sem_Gens_Nil  =
    (\ _lhsIgE
       _lhsIpn
       _lhsIrnr ->
         (let _lhsOrnr =
                  _lhsIrnr
              _lhsOrules =
                  []
              _lhsOlist =
                  []
          in  ( _lhsOlist,_lhsOrnr,_lhsOrules)))
-- KeyDef ------------------------------------------------------
-- cata
sem_KeyDef (Kd _pos _lbl _ctx _ats )  =
    (sem_KeyDef_Kd _pos _lbl (sem_Expression _ctx ) (sem_ObjDefs _ats ) )
sem_KeyDef_Kd pos_ lbl_ ctx_ ats_  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr =
                  _atsIsErr
              _atsOrnr =
                  _lhsIrnr
              _ctxOrnr =
                  _atsIrnr
              _lhsOrnr =
                  _ctxIrnr
              _lhsOrules =
                  _atsIrules ++ _ctxIrules
              _lhsOkd =
                  Kd pos_ lbl_ _ctxIexpr _atsIobjDefs
              _lhsOexprs =
                  [ expr | Obj nm pos expr ats <- _atsIobjDefs]
              _ctxOpn =
                  ""
              _ctxOisign =
                  _ctxIsigns
              _atsOiConcs =
                  rd (map snd _ctxIsigns ++ _atsIsources)>-[Anything]
              _atsOiConc =
                  head (rd (map snd _ctxIsigns))
              _ctxOgE =
                  _lhsIgE
              _ctxOpos =
                  pos_
              _ctxOsDef =
                  _lhsIsDef
              _atsOgE =
                  _lhsIgE
              _atsOsDef =
                  _lhsIsDef
              ( _ctxIexpr,_ctxImorphisms,_ctxIraw,_ctxIrnr,_ctxIrules,_ctxIsErr,_ctxIsigns) =
                  (ctx_ _ctxOgE _ctxOisign _ctxOpn _ctxOpos _ctxOrnr _ctxOsDef )
              ( _atsIobjDefs,_atsIrnr,_atsIrules,_atsIsErr,_atsIsources) =
                  (ats_ _atsOgE _atsOiConc _atsOiConcs _atsOrnr _atsOsDef )
          in  ( _lhsOexprs,_lhsOkd,_lhsOrnr,_lhsOrules,_lhsOsErr)))
-- KeyDefs -----------------------------------------------------
-- cata
sem_KeyDefs list  =
    (Prelude.foldr sem_KeyDefs_Cons sem_KeyDefs_Nil (Prelude.map sem_KeyDef list) )
sem_KeyDefs_Cons hd_ tl_  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr =
                  _hdIsErr ++ _tlIsErr
              _hdOrnr =
                  _lhsIrnr
              _tlOrnr =
                  _hdIrnr
              _lhsOrnr =
                  _tlIrnr
              _lhsOrules =
                  _hdIrules ++ _tlIrules
              _lhsOkeyDefs =
                  _hdIkd : _tlIkeyDefs
              _lhsOexprs =
                  _hdIexprs ++ _tlIexprs
              _hdOgE =
                  _lhsIgE
              _hdOsDef =
                  _lhsIsDef
              _tlOgE =
                  _lhsIgE
              _tlOsDef =
                  _lhsIsDef
              ( _hdIexprs,_hdIkd,_hdIrnr,_hdIrules,_hdIsErr) =
                  (hd_ _hdOgE _hdOrnr _hdOsDef )
              ( _tlIexprs,_tlIkeyDefs,_tlIrnr,_tlIrules,_tlIsErr) =
                  (tl_ _tlOgE _tlOrnr _tlOsDef )
          in  ( _lhsOexprs,_lhsOkeyDefs,_lhsOrnr,_lhsOrules,_lhsOsErr)))
sem_KeyDefs_Nil  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr =
                  []
              _lhsOrnr =
                  _lhsIrnr
              _lhsOrules =
                  []
              _lhsOkeyDefs =
                  []
              _lhsOexprs =
                  []
          in  ( _lhsOexprs,_lhsOkeyDefs,_lhsOrnr,_lhsOrules,_lhsOsErr)))
-- Morphism ----------------------------------------------------
-- cata
sem_Morphism (I _atts _g _s _yin )  =
    (sem_Morphism_I _atts (sem_Concept _g ) (sem_Concept _s ) _yin )
sem_Morphism (Mph _nm _pos _atts _sgn _yin _decl )  =
    (sem_Morphism_Mph _nm _pos _atts _sgn _yin (sem_Declaration _decl ) )
sem_Morphism (V _atts _sgn )  =
    (sem_Morphism_V _atts _sgn )
sem_Morphism_I atts_ g_ s_ yin_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIsDef ->
         (let _lhsOsErr =
                  []
              _lhsOusedDecls =
                  declarations [ _rraw ]
              _lhsOraw =
                  ( _rraw, [])
              _rraw =
                  if null _ats      then I [] Anything Anything True  else
                  if length _ats==1 then I _ats (head _ats) (head _ats) True              else
                  error ("Contact your dealer:\nADL allows only one concept in I["++show atts_++"].")
              _lhsOmorphism =
                  let (s,t) = if null _lhsIisign && null _ats then error ("Fatal: null @lhs.isign in I lhs.morphism!") else
                              head (_lhsIisign++[(head _ats,last _ats)])
                      s' = if s==Anything then t else s; t'= if t==Anything then s else t
                      is = ids _lhsIgE s' t'
                  in if null is then I _ats s' t' True else head is
              _lhsOnm =
                  "I"
              _lhsOpos =
                  posNone
              _lhsOatts =
                  _ats
              _ats =
                  rd ([C a _lhsIgE as|C a _ as<- atts_]++[S|S<- atts_])
              _lhsOyin =
                  let I ats g s yin = _rraw in yin
              _lhsOid =
                  True
              _gOgE =
                  _lhsIgE
              _sOgE =
                  _lhsIgE
              ( _gIconcept,_gInm) =
                  (g_ _gOgE )
              ( _sIconcept,_sInm) =
                  (s_ _sOgE )
          in  ( _lhsOatts,_lhsOid,_lhsOmorphism,_lhsOnm,_lhsOpos,_lhsOraw,_lhsOsErr,_lhsOusedDecls,_lhsOyin)))
sem_Morphism_Mph nm_ pos_ atts_ sgn_ yin_ decl_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIsDef ->
         (let _lhsOsErr =
                  [ "3 in "++show pos_++
                    "\n   Relation " ++ show (Mph nm_ pos_ atts_ (Anything, Anything) yin_ (Isn Anything Anything)) ++
                    (if null atts_ then "" else show (rd atts_)) ++
                    " is not declared.\n"
                  | null _ss]
              _declOrnr =
                  -999999
              _lhsOmorphism =
                  Mph nm_ pos_ _ats _s yin_
                   (if null _rel then error("Fatal (module AGtry): null @rel in "++ nm_++" on "++show pos_++"\n@lhs.isign = "++ show _lhsIisign++"\n@ss       = "++ show _ss++"\n@s          = "++ show _s++"\n"++ gEtabG _lhsIgE (rd ([fst _s,snd _s]++concs _ss))) else
                    head _rel)
              _s =
                  if null _lhsIisign
                  then error("Fatal 2: Empty declaration allocation for "++ nm_ ++" on "++show pos_++":\n  @ss="++chain "\n       " (map show _ss))
                  else head _lhsIisign
              _m =
                  if null _rel
                  then error("Fatal 3: Empty declaration allocation for "++show nm_++" on "++show pos_++":\n  @lhs.isign="++show _lhsIisign++":\n  @ss="++chain "\n       " (map show _ss))
                  else if length _rel>1
                  then error("Fatal 6: Ambiguous declaration allocation for "++show nm_++" on "++show pos_++":\n   "++
                                                       chain "\n   " (map show _rel))
                  else head _rel
              _rel =
                  irredD _lhsIgE [s| s <- _ss, nm_==name s, (a,b) <- _lhsIisign
                                   , if yin_ then test s (a,b) else test s (b,a)]
                  where test s (a,b) = (source s `_lhsIgE` a) &&
                                       (target s `_lhsIgE` b)
              _lhsOusedDecls =
                  _ss
              _ss =
                  let ss = [ s | s <- _lhsIsDef, name s == nm_] in
                  if null atts_ then rd ss else
                  [ s | s <- ss, if yin_ then source s == head atts_ && target s == last atts_ else source s == last atts_ && target s == head atts_]
              _lhsOraw =
                  let err=error "illegal reference to 'raw' in semantics of Mph of Morphism" in
                  if null _ats
                  then (Mph nm_ pos_ _ats (Anything,Anything) yin_ (Sgn nm_ Anything Anything [] "" "" "" [] "" err 0 True), _ss)
                  else (Mph nm_ pos_ _ats (if yin_ then (head _ats,last _ats) else (last _ats,head _ats)) yin_ (Sgn nm_ (head _ats) (last _ats) [] "" "" "" [] "" err 0 True), _ss)
              _ats =
                  rd ([C a _lhsIgE as|C a _ as<- atts_]++[S|S<- atts_])
              _lhsOnm =
                  nm_
              _lhsOpos =
                  pos_
              _lhsOatts =
                  _ats
              _lhsOyin =
                  yin_
              _lhsOid =
                  False
              _declOgE =
                  _lhsIgE
              _declOsDef =
                  _lhsIsDef
              ( _declIdeclaration,_declInm,_declIrawDecl,_declIrnr,_declIrules,_declIsErr) =
                  (decl_ _declOgE _declOrnr _declOsDef )
          in  ( _lhsOatts,_lhsOid,_lhsOmorphism,_lhsOnm,_lhsOpos,_lhsOraw,_lhsOsErr,_lhsOusedDecls,_lhsOyin)))
sem_Morphism_V atts_ sgn_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIsDef ->
         (let _lhsOsErr =
                  []
              _lhsOusedDecls =
                  declarations [ _rraw ]
              _lhsOraw =
                  ( _rraw, [])
              _rraw =
                  V atts_ sgn_
              _lhsOmorphism =
                  let (s,t) = if null _lhsIisign then error ("Fatal: null @lhs.isign in V lhs.morphism! "++(show atts_)) else
                              head _lhsIisign
                  in V _ats (s,t)
              _lhsOnm =
                  "V"
              _lhsOpos =
                  posNone
              _lhsOatts =
                  _ats
              _ats =
                  rd ([C a _lhsIgE as|C a _ as<- atts_]++[S|S<- atts_])
              _lhsOid =
                  True
              _lhsOyin =
                  True
          in  ( _lhsOatts,_lhsOid,_lhsOmorphism,_lhsOnm,_lhsOpos,_lhsOraw,_lhsOsErr,_lhsOusedDecls,_lhsOyin)))
-- Morphisms ---------------------------------------------------
-- cata
sem_Morphisms list  =
    (Prelude.foldr sem_Morphisms_Cons sem_Morphisms_Nil (Prelude.map sem_Morphism list) )
sem_Morphisms_Cons hd_ tl_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIsDef ->
         (let _lhsOusedDecls =
                  _hdIusedDecls ++ _tlIusedDecls
              _lhsOmorphisms =
                  _hdImorphism : _tlImorphisms
              _lhsOraw =
                  _hdIraw : _tlIraw
              _hdOisign =
                  take 1 (
                   [ (src, if yin then target s else source s)
                   | (src,_)<- _lhsIisign, (Mph nm pos ats _ yin _,ss) <- [_hdIraw], s<-ss, (if yin then source s else target s) `_lhsIgE` src]++
                   _lhsIisign)
              _hdOgE =
                  _lhsIgE
              _hdOsDef =
                  _lhsIsDef
              _tlOgE =
                  _lhsIgE
              _tlOisign =
                  _lhsIisign
              _tlOsDef =
                  _lhsIsDef
              ( _hdIatts,_hdIid,_hdImorphism,_hdInm,_hdIpos,_hdIraw,_hdIsErr,_hdIusedDecls,_hdIyin) =
                  (hd_ _hdOgE _hdOisign _hdOsDef )
              ( _tlImorphisms,_tlIraw,_tlIusedDecls) =
                  (tl_ _tlOgE _tlOisign _tlOsDef )
          in  ( _lhsOmorphisms,_lhsOraw,_lhsOusedDecls)))
sem_Morphisms_Nil  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIsDef ->
         (let _lhsOusedDecls =
                  []
              _lhsOraw =
                  []
              _lhsOmorphisms =
                  []
          in  ( _lhsOmorphisms,_lhsOraw,_lhsOusedDecls)))
-- ObjDefs -----------------------------------------------------
-- cata
sem_ObjDefs list  =
    (Prelude.foldr sem_ObjDefs_Cons sem_ObjDefs_Nil (Prelude.map sem_ObjectDef list) )
sem_ObjDefs_Cons hd_ tl_  =
    (\ _lhsIgE
       _lhsIiConc
       _lhsIiConcs
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr =
                  _hdIsErr ++ _tlIsErr
              _hdOrnr =
                  _lhsIrnr
              _tlOrnr =
                  _hdIrnr
              _lhsOrnr =
                  _tlIrnr
              _lhsOrules =
                  _hdIrules ++ _tlIrules
              _hdOiConcs =
                  _lhsIiConcs
              _hdOiConc =
                  _lhsIiConc
              _lhsOobjDefs =
                  _hdIodef : _tlIobjDefs
              _lhsOsources =
                  _signs
              _signs =
                  rd [lubb _lhsIgE src' src''|src''<- _tlIsources, src'<- _hdIsConcs, src' `order` src'']
              _hdOgE =
                  _lhsIgE
              _hdOsDef =
                  _lhsIsDef
              _tlOgE =
                  _lhsIgE
              _tlOiConc =
                  _lhsIiConc
              _tlOiConcs =
                  _lhsIiConcs
              _tlOsDef =
                  _lhsIsDef
              ( _hdIats,_hdInm,_hdIodef,_hdIpos,_hdIrnr,_hdIrules,_hdIsConcs,_hdIsErr) =
                  (hd_ _hdOgE _hdOiConc _hdOiConcs _hdOrnr _hdOsDef )
              ( _tlIobjDefs,_tlIrnr,_tlIrules,_tlIsErr,_tlIsources) =
                  (tl_ _tlOgE _tlOiConc _tlOiConcs _tlOrnr _tlOsDef )
          in  ( _lhsOobjDefs,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsources)))
sem_ObjDefs_Nil  =
    (\ _lhsIgE
       _lhsIiConc
       _lhsIiConcs
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr =
                  []
              _lhsOrnr =
                  _lhsIrnr
              _lhsOrules =
                  []
              _lhsOobjDefs =
                  []
              _lhsOsources =
                  [Anything]
          in  ( _lhsOobjDefs,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsources)))
-- ObjectDef ---------------------------------------------------
-- cata
sem_ObjectDef (Obj _nm _pos _ctx _ats )  =
    (sem_ObjectDef_Obj _nm _pos (sem_Expression _ctx ) (sem_ObjDefs _ats ) )
sem_ObjectDef_Obj nm_ pos_ ctx_ ats_  =
    (\ _lhsIgE
       _lhsIiConc
       _lhsIiConcs
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr =
                  let ls  = (concs _atsIobjDefs)
                      ls' c = [g| g<-ls, g `_lhsIgE` c || c `_lhsIgE` g]
                      ground c = if null (ls' c)
                                 then Nothing
                                 else Just (minimum (ls' c))
                  in
                  take 1
                  (_ctxIsErr++
                   _atsIsErr++
                   [ "18 on \n"++show pos_ ++" in the definition of '"++ nm_ ++
                     "'\n   Undefined type, because there is no relation '"++showADL _ctxIexpr
                     ++"' with source '"++showADL _lhsIiConc++"'.\n"
                   | null _signs]++
                   [ "11 on \n"++show pos_ ++" in the definition of '"++ nm_ ++
                     "'\n   Ambiguous type for '"++showADL _ctxIexpr++"', because the target can be any of "
                     ++commaEng "or" [name t|(s,t)<- _signs]++".\n"
                   | length _signs>1]++
                   [ "19 on \n"++show pos_ ++" in the definition of '"++ nm_ ++
                     "'\n   the label '"++nm
                     ++"' may be used only once in each SERVICE, but it occurs on lines "++commaEng "and" [show l| o<- _atsIobjDefs, name o==nm, FilePos (fn,Pos l c,sym)<-[ADLdef.pos o]]++".\n"
                   | nm<-map name _atsIobjDefs, length [o| o<- _atsIobjDefs, name o==nm]>1 ]++
                   [ "9 on \n"++show pos_ ++" in the definition of SERVICE "++ nm_ ++ "\n   Cannot match the right hand side of "++showADL _ctxIexpr
                     ++"\n   with the left hand side of "
                     ++commaEng "and" [ showADL (ctx o) | o <- nqos ]
                     ++"\nDetails of this mistake:"
                     ++"\n   target("++showADL _ctxIexpr++ ") is "++name t++","
                     ++"\n   "++(if null eqcs
                                 then "which does not match " ++ commaEng "or" (rd [name c| c<-nqcs])
                                 else "but "++commaEng "\n   and" [ "source("++showADL (ctx o)++ ") is "++(name.source.ctx) o | o<-nqos ]
                                )
                     ++".\n"
                   | null _signs, t<-map snd _ctxIsigns
                   , eqcs<-[[(source.ctx) o| o<- _atsIobjDefs, (source.ctx) o==t ]]
                   , nqos<-[[o| o<- _atsIobjDefs, (source.ctx) o/=t ]]
                   , nqcs<-[[(source.ctx) c| cl<-eqCl (source.ctx) nqos, c<-cl]]
                   , not (null nqos)]++
                   [ "10 on \n"++show pos_ ++" in the definition of SERVICE "++ nm_ ++ "\n   Cannot match the left hand side of "++showADL _ctxIexpr
                     ++"\n   with "++show t
                     ++".\n"
                   | null _signs, t<-map snd _ctxIsigns ]++
                   [ "17 on "++show pos_ ++"\n"++show [ [concept o|o<-cl] | cl<-eqcls]++"\n"++": Cannot match "
                     ++(showADL.ctx) e++ " on " ++(show . ADLdef.pos . ctx) e++ " with " ++(showADL.ctx) e'++ " on " ++(show . ADLdef.pos . ctx) e'
                     ++ "\n because source(" ++(showADL.ctx) e++ ")=" ++(name.source.ctx) e++ " and source(" ++(showADL.ctx) e'++ ")=" ++(name.source.ctx) e'++"."
                     ++ "\n (" ++(showADL.ctx) e++ " and " ++(showADL.ctx) e'++ " do not match."
                     ++ "\n"
                   | eqcls<-[[cl| cl<-eqCl (ground.source.ctx) _atsIobjDefs]], length eqcls>1
                   , [e,e']<-[[o| cl<-take 2 eqcls, o<-take 1 (sort' (ground.source.ctx) cl)]]
                   ])
              _atsOrnr =
                  _lhsIrnr
              _ctxOrnr =
                  _atsIrnr
              _lhsOrnr =
                  _ctxIrnr
              _lhsOrules =
                  _atsIrules ++ _ctxIrules
              _lhsOodef =
                  if and [sgn==(Anything,Anything)|sgn<- _ctxIsigns]
                  then Obj nm_ pos_ (Tm (mIs (C nm_ _lhsIgE []))) _atsIobjDefs
                  else Obj nm_ pos_ _ctxIexpr _atsIobjDefs
              _lhsOats =
                  _atsIobjDefs
              _lhsOsConcs =
                  rd (map fst _ctxIsigns)
              _lhsOpos =
                  pos_
              _lhsOnm =
                  nm_
              _atsOiConcs =
                  irredC _lhsIgE (map snd _signs)
              _atsOiConc =
                  if null _ctxIsigns then C nm_ _lhsIgE [] else _concpt
              _ctxOisign =
                  _signs
              _concpt =
                  if null _signs
                  then error ("Fatal (module AGtry): no target for '"++showADL _ctxIexpr++"'."++
                              "\n@pos       : "++show pos_++
                              "\n@nm        : "++show nm_++
                              "\n@ctx.signs : "++show _ctxIsigns
                             )
                  else head (map snd _signs)
              _signs =
                  irredT' _lhsIgE
                     ([( lubb _lhsIgE _lhsIiConc s, t)
                      | (s,t) <- _ctxIsigns, not (s==Anything && t==Anything)
                      , ordd _lhsIgE _lhsIiConc s]++
                      [(C nm_ _lhsIgE [], C nm_ _lhsIgE [])| and [sgn==(Anything,Anything)|sgn<- _ctxIsigns]])
              _ctxOpn =
                  ""
              _ctxOgE =
                  _lhsIgE
              _ctxOpos =
                  pos_
              _ctxOsDef =
                  _lhsIsDef
              _atsOgE =
                  _lhsIgE
              _atsOsDef =
                  _lhsIsDef
              ( _ctxIexpr,_ctxImorphisms,_ctxIraw,_ctxIrnr,_ctxIrules,_ctxIsErr,_ctxIsigns) =
                  (ctx_ _ctxOgE _ctxOisign _ctxOpn _ctxOpos _ctxOrnr _ctxOsDef )
              ( _atsIobjDefs,_atsIrnr,_atsIrules,_atsIsErr,_atsIsources) =
                  (ats_ _atsOgE _atsOiConc _atsOiConcs _atsOrnr _atsOsDef )
          in  ( _lhsOats,_lhsOnm,_lhsOodef,_lhsOpos,_lhsOrnr,_lhsOrules,_lhsOsConcs,_lhsOsErr)))
-- Pairs -------------------------------------------------------
-- cata
sem_Pairs list  =
    (Prelude.foldr sem_Pairs_Cons sem_Pairs_Nil list )
sem_Pairs_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_Pairs_Nil  =
    (let 
     in  ( ))
-- Pattern -----------------------------------------------------
-- cata
sem_Pattern (Pat _nm _rules _gen _pms _cs _ks )  =
    (sem_Pattern_Pat _nm (sem_Rules _rules ) (sem_Gens _gen ) (sem_Declarations _pms ) (sem_ConceptDefs _cs ) (sem_KeyDefs _ks ) )
sem_Pattern_Pat nm_ rules_ gen_ pms_ cs_ ks_  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOmGen =
                  rd [G pos g s| G pos g s <- _rulesImGen ++ _genIlist, g/=s]
              _lhsOkeyDefs =
                  _ksIkeyDefs
              _lhsOmorphisms =
                  _rulesImorphisms
              _lhsOsErr =
                  _rulesIsErr ++ _pmsIsErr ++ _ksIsErr
              _genOrnr =
                  _lhsIrnr
              _rulesOrnr =
                  _genIrnr
              _pmsOrnr =
                  _rulesIrnr
              _ksOrnr =
                  _pmsIrnr
              _lhsOrnr =
                  _ksIrnr
              _lhsOrules =
                  _genIrules ++ _rulesIrules ++ _pmsIrules ++ _ksIrules
              _lhsOpatterns =
                  [Pat nm_ _rulesIrules ( _rulesImGen ++ _genIlist)
                       ( _pmsIdeclarations ++ _rulesIdeclarations )
                       _csIconDefs _ksIkeyDefs]
              _lhsOusedDecls =
                  _rulesIusedDecls ++declarations _ksIexprs
              _lhsOrawDecls =
                  _pmsIrawDecls
              _lhsOconDefs =
                  _csIconDefs
              _rulesOpn =
                  nm_
              _genOpn =
                  nm_
              _ksOgE =
                  _lhsIgE
              _rulesOgE =
                  _lhsIgE
              _rulesOsDef =
                  _lhsIsDef
              _genOgE =
                  _lhsIgE
              _pmsOgE =
                  _lhsIgE
              _pmsOsDef =
                  _lhsIsDef
              _ksOsDef =
                  _lhsIsDef
              ( _rulesIdeclarations,_rulesImGen,_rulesImorphisms,_rulesIrnr,_rulesIrules,_rulesIsErr,_rulesIusedDecls) =
                  (rules_ _rulesOgE _rulesOpn _rulesOrnr _rulesOsDef )
              ( _genIlist,_genIrnr,_genIrules) =
                  (gen_ _genOgE _genOpn _genOrnr )
              ( _pmsIdeclarations,_pmsIrawDecls,_pmsIrnr,_pmsIrules,_pmsIsErr) =
                  (pms_ _pmsOgE _pmsOrnr _pmsOsDef )
              ( _csIconDefs) =
                  (cs_ )
              ( _ksIexprs,_ksIkeyDefs,_ksIrnr,_ksIrules,_ksIsErr) =
                  (ks_ _ksOgE _ksOrnr _ksOsDef )
          in  ( _lhsOconDefs,_lhsOkeyDefs,_lhsOmGen,_lhsOmorphisms,_lhsOpatterns,_lhsOrawDecls,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOusedDecls)))
-- Patterns ----------------------------------------------------
-- cata
sem_Patterns list  =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list) )
sem_Patterns_Cons hd_ tl_  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOmGen =
                  _hdImGen ++ _tlImGen
              _lhsOkeyDefs =
                  _hdIkeyDefs ++ _tlIkeyDefs
              _lhsOmorphisms =
                  _hdImorphisms ++ _tlImorphisms
              _lhsOsErr =
                  _hdIsErr ++ _tlIsErr
              _hdOrnr =
                  _lhsIrnr
              _tlOrnr =
                  _hdIrnr
              _lhsOrnr =
                  _tlIrnr
              _lhsOrules =
                  _hdIrules ++ _tlIrules
              _lhsOpatterns =
                  _hdIpatterns ++ _tlIpatterns
              _lhsOusedDecls =
                  rd(_hdIusedDecls ++ _tlIusedDecls)
              _lhsOrawDecls =
                  _hdIrawDecls ++ _tlIrawDecls
              _lhsOconDefs =
                  _hdIconDefs ++ _tlIconDefs
              _hdOgE =
                  _lhsIgE
              _hdOsDef =
                  _lhsIsDef
              _tlOgE =
                  _lhsIgE
              _tlOsDef =
                  _lhsIsDef
              ( _hdIconDefs,_hdIkeyDefs,_hdImGen,_hdImorphisms,_hdIpatterns,_hdIrawDecls,_hdIrnr,_hdIrules,_hdIsErr,_hdIusedDecls) =
                  (hd_ _hdOgE _hdOrnr _hdOsDef )
              ( _tlIconDefs,_tlIkeyDefs,_tlImGen,_tlImorphisms,_tlIpatterns,_tlIrawDecls,_tlIrnr,_tlIrules,_tlIsErr,_tlIusedDecls) =
                  (tl_ _tlOgE _tlOrnr _tlOsDef )
          in  ( _lhsOconDefs,_lhsOkeyDefs,_lhsOmGen,_lhsOmorphisms,_lhsOpatterns,_lhsOrawDecls,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOusedDecls)))
sem_Patterns_Nil  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOmGen =
                  []
              _lhsOkeyDefs =
                  []
              _lhsOmorphisms =
                  []
              _lhsOsErr =
                  []
              _lhsOrnr =
                  _lhsIrnr
              _lhsOrules =
                  []
              _lhsOpatterns =
                  []
              _lhsOusedDecls =
                  []
              _lhsOrawDecls =
                  []
              _lhsOconDefs =
                  []
          in  ( _lhsOconDefs,_lhsOkeyDefs,_lhsOmGen,_lhsOmorphisms,_lhsOpatterns,_lhsOrawDecls,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOusedDecls)))
-- Population --------------------------------------------------
-- cata
sem_Population (Popu _m _ps )  =
    (sem_Population_Popu (sem_Morphism _m ) _ps )
sem_Population_Popu m_ ps_  =
    (\ _lhsIgE
       _lhsIsDef ->
         (let _lhsOsErr =
                  _mIsErr
              _lhsOpop =
                  Popu _mImorphism ps_
              _mOisign =
                  let (m,ss) = _mIraw in if isMph m then (if inline m then map sign ss else map (sign.flp) ss) else [sign m]
              _mOgE =
                  _lhsIgE
              _mOsDef =
                  _lhsIsDef
              ( _mIatts,_mIid,_mImorphism,_mInm,_mIpos,_mIraw,_mIsErr,_mIusedDecls,_mIyin) =
                  (m_ _mOgE _mOisign _mOsDef )
          in  ( _lhsOpop,_lhsOsErr)))
-- Populations -------------------------------------------------
-- cata
sem_Populations list  =
    (Prelude.foldr sem_Populations_Cons sem_Populations_Nil (Prelude.map sem_Population list) )
sem_Populations_Cons hd_ tl_  =
    (\ _lhsIgE
       _lhsIsDef ->
         (let _lhsOsErr =
                  _hdIsErr ++ _tlIsErr
              _lhsOpopus =
                  _hdIpop : _tlIpopus
              _hdOgE =
                  _lhsIgE
              _hdOsDef =
                  _lhsIsDef
              _tlOgE =
                  _lhsIgE
              _tlOsDef =
                  _lhsIsDef
              ( _hdIpop,_hdIsErr) =
                  (hd_ _hdOgE _hdOsDef )
              ( _tlIpopus,_tlIsErr) =
                  (tl_ _tlOgE _tlOsDef )
          in  ( _lhsOpopus,_lhsOsErr)))
sem_Populations_Nil  =
    (\ _lhsIgE
       _lhsIsDef ->
         (let _lhsOsErr =
                  []
              _lhsOpopus =
                  []
          in  ( _lhsOpopus,_lhsOsErr)))
-- Prop --------------------------------------------------------
-- cata
sem_Prop (Asy )  =
    (sem_Prop_Asy )
sem_Prop (Inj )  =
    (sem_Prop_Inj )
sem_Prop (Sur )  =
    (sem_Prop_Sur )
sem_Prop (Sym )  =
    (sem_Prop_Sym )
sem_Prop (Tot )  =
    (sem_Prop_Tot )
sem_Prop (Trn )  =
    (sem_Prop_Trn )
sem_Prop (Uni )  =
    (sem_Prop_Uni )
sem_Prop_Asy  =
    (let 
     in  ( ))
sem_Prop_Inj  =
    (let 
     in  ( ))
sem_Prop_Sur  =
    (let 
     in  ( ))
sem_Prop_Sym  =
    (let 
     in  ( ))
sem_Prop_Tot  =
    (let 
     in  ( ))
sem_Prop_Trn  =
    (let 
     in  ( ))
sem_Prop_Uni  =
    (let 
     in  ( ))
-- Rule --------------------------------------------------------
-- cata
sem_Rule (Gc _gluePos _m _expr _cpu _sgn _nr _pn )  =
    (sem_Rule_Gc _gluePos (sem_Morphism _m ) (sem_Expression _expr ) (sem_Expressions _cpu ) _sgn _nr _pn )
sem_Rule (Ru _c _antc _rulePos _cons _cpu _expl _sgn _nr _pn )  =
    (sem_Rule_Ru _c (sem_Expression _antc ) _rulePos (sem_Expression _cons ) (sem_Expressions _cpu ) _expl _sgn _nr _pn )
sem_Rule (Sg _pos _rule _expl _sgn _nr _pn _signal )  =
    (sem_Rule_Sg _pos (sem_Rule _rule ) _expl _sgn _nr _pn (sem_Declaration _signal ) )
sem_Rule_Gc gluePos_ m_ expr_ cpu_ sgn_ nr_ pn_  =
    (\ _lhsIgE
       _lhsIpn
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOmGen =
                  rd [G pos g s| (d,c) <- _exprIsigns, m <- _mIusedDecls, G pos g s<-[G gluePos_ d (source m), G gluePos_ c (target m)], g/=s]
              _cpuOisign =
                  []
              _cpuOseptets =
                  []
              _cpuOpos =
                  gluePos_
              _mOisign =
                  _ms
              _exprOisign =
                  _esign
              _exprOpos =
                  gluePos_
              _rs =
                  [ Ru c antc pos expr cpu expla (a,b) (i + _lhsIrnr) _lhsIpn
                  | ((Ru c antc pos expr cpu expla (a,b) _ _,cpuErrs), i)<-zip _ruls [0..]] ++ _exprIrules
              _lhsOmorphisms =
                  _mors
              _mors =
                  _mImorphism : _exprImorphisms
              _ruls =
                  [ subExprCheck (Ru Implication (Tm _mImorphism) gluePos_ _exprIexpr _cpuIraw "" (a,b) nr_ _lhsIpn)
                  | (d,c) <- _exprIsigns, s <- _mIusedDecls
                  , if d `_lhsIgE` source _mImorphism && c `_lhsIgE` target _mImorphism then True else
                    error ("Fatal: (please report this as an error in ADL)\nFalse assumption in alternative Gc of SEM Rule, that "++show (d,c)++" in @expr.signs be more general than "++ show (source _mImorphism,target _mImorphism)++".")
                  , (a,b)<-[if null _mIatts then (source _mImorphism,target _mImorphism) else (head _mIatts,last _mIatts)]]
              _lhsOusedDecls =
                  rd (declarations _mors ++ _closdecls)
              _lhsOdeclarations =
                  []
              _closdecls =
                  rd [s| Ru c antc p cons cpu expla (a,b) n pn <- _exprIrules, s<-declarations antc]
              _ms =
                  irredT _lhsIgE
                   (if null _exprIsigns
                    then [(source m,target m)| m <- _mIusedDecls]
                    else [(source m,target m)| m <- _mIusedDecls
                                             , or[s `_lhsIgE` source m && t `_lhsIgE` target m
                                                 | (s,t) <- _exprIsigns ]
                         ])
              _esign =
                  irredT _lhsIgE _exprIsigns
              _lhsOsErr =
                  take 1
                   (  [ "12 in "++show gluePos_++
                        "\n   I not allowed as left hand side of GLUE\n"
                      | null _ms && _mIid]
                   ++ ["13 in "++show gluePos_++
                        "\n   I not allowed as right hand side of GLUE\n"
                      | anything _exprIsigns]
                   ++ _mIsErr ++ _exprIsErr
                   )
              _exprOrnr =
                  _lhsIrnr+length _rs
              _lhsOrnr =
                  _exprIrnr
              _lhsOrules =
                  _rs
              _mOgE =
                  _lhsIgE
              _mOsDef =
                  _lhsIsDef
              _exprOgE =
                  _lhsIgE
              _exprOpn =
                  _lhsIpn
              _exprOsDef =
                  _lhsIsDef
              _cpuOgE =
                  _lhsIgE
              _cpuOpn =
                  _lhsIpn
              _cpuOrnr =
                  _exprIrnr
              _cpuOsDef =
                  _lhsIsDef
              ( _mIatts,_mIid,_mImorphism,_mInm,_mIpos,_mIraw,_mIsErr,_mIusedDecls,_mIyin) =
                  (m_ _mOgE _mOisign _mOsDef )
              ( _exprIexpr,_exprImorphisms,_exprIraw,_exprIrnr,_exprIrules,_exprIsErr,_exprIsigns) =
                  (expr_ _exprOgE _exprOisign _exprOpn _exprOpos _exprOrnr _exprOsDef )
              ( _cpuIexprs,_cpuImorphisms,_cpuIraw,_cpuIrnr,_cpuIrules,_cpuIsErr,_cpuIsignss) =
                  (cpu_ _cpuOgE _cpuOisign _cpuOpn _cpuOpos _cpuOrnr _cpuOsDef _cpuOseptets )
          in  ( _lhsOdeclarations,_lhsOmGen,_lhsOmorphisms,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOusedDecls)))
sem_Rule_Ru c_ antc_ rulePos_ cons_ cpu_ expl_ sgn_ nr_ pn_  =
    (\ _lhsIgE
       _lhsIpn
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOmGen =
                  []
              _cpuOisign =
                  _is
              _cpuOseptets =
                  []
              _cpuOpos =
                  rulePos_
              _antcOisign =
                  if null _is then _antcIsigns else _is
              _consOisign =
                  if null _is then _consIsigns else _is
              _antcOpos =
                  rulePos_
              _consOpos =
                  rulePos_
              _r =
                  fst _rcpu
              _rs =
                  [ _r ] ++ _clrs
              _errcpu =
                  snd _rcpu
              _rcpu =
                  subExprCheck
                  (if c_==AlwaysExpr
                  then Ru c_ (error ("Reference to antecedent of AlwaysExpr Rule "++showHS "" _r)) rulePos_ _consIexpr _cpuIraw expl_
                          (if null _is
                           then error("Fatal: null @is on "++show rulePos_++"\n@cons.expr = "++show _consIexpr++"\n@cons.signs = "++shSigns _consIsigns)
                           else head _is)
                           _lhsIrnr _lhsIpn
                  else Ru c_ _antcIexpr rulePos_ _consIexpr _cpuIraw expl_
                          (if null _is
                           then error("Fatal: null @is on "++show rulePos_++"\n@antc.expr = "++show _antcIexpr++"\n@antc.signs = "++shSigns _antcIsigns++"\n@cons.expr = "++show _consIexpr++"\n@cons.signs = "++shSigns _consIsigns)
                           else head _is)
                          _lhsIrnr _lhsIpn)
              _lhsOusedDecls =
                  rd (declarations (_antcImorphisms ++ _consImorphisms) ++ _closdecls)
              _lhsOdeclarations =
                  []
              _closdecls =
                  rd [s| Ru c antc p cons cpu expla (a,b) n pn <- _clrs, s<-declarations antc]
              _clrs =
                  if c_==AlwaysExpr
                  then _consIrules
                  else _antcIrules ++ _consIrules
              _lhsOmorphisms =
                  (if c_==AlwaysExpr then [] else _antcImorphisms)++ _consImorphisms
              _is =
                  if c_==AlwaysExpr then _consIsigns else
                  if null _antcIsigns
                  then (if null _consIsigns
                        then []
                        else _consIsigns)
                  else (if null _consIsigns
                        then _antcIsigns
                        else _s)
              _s =
                  if c_==AlwaysExpr then _consIsigns else
                  irredT _lhsIgE [ (if a `_lhsIgE` a' then a' else a, if b `_lhsIgE` b' then b' else b)
                                 | (a,b)<- _consIsigns, (a',b')<- _antcIsigns
                                 , (a `_lhsIgE` a' || a' `_lhsIgE` a) && (b `_lhsIgE` b' || b' `_lhsIgE` b)
                                 ]
              _lhsOsErr =
                  take 1
                  (_consIsErr ++
                    (if c_==AlwaysExpr then [] else _antcIsErr ++
                     [ "8 in "++show rulePos_++"\n   Meaningless rule.\n"
                     | null _consIsigns&&null _antcIsigns]++
                     [ "1 in "++show rulePos_++"\n   Mismatch in rule:\n   "++
                       show _antcIexpr++ " :: "++shSigns _antcIsigns ++ "\n   does not match\n   "++
                       show _consIexpr++ " :: "++shSigns _consIsigns++"\n"
                     | null _is && not (null _antcIsigns) && not (null _consIsigns)]++
                     [ "2 in "++show rulePos_++"\n   Ambiguous types: "++shSigns _is++
                       "\n   in right hand side:  "++ show _consIexpr++
                       "\n   and left hand side:  "++ show _antcIexpr++"\n"
                     | length _is>1]++
                     [ "15 in "++show rulePos_++
                       "\n   the right hand side: "++ show _consIexpr++" has type "++shSigns [(s,t)| (s,t) <- _is, not(s `order` t)]++
                       "\n   and left hand side:  "++ show _antcIexpr++" requires that source and target are the same.\n"
                     | isIdent _antcIexpr && or [not(s `order` t)| (s,t) <- _is] ]++
                     [ "16 in "++show rulePos_++
                       "\n   the left hand side:  "++ show _consIexpr++" has type "++shSigns [(s,t)| (s,t) <- _is, not(s `order` t)]++
                       "\n   and right hand side: "++ show _antcIexpr++" requires that source and target are the same.\n"
                     | isIdent _consIexpr && or [not(s `order` t)| (s,t) <- _is] ]++
                     [ "14 in "++show rulePos_++" rule "++show (nr _r)++":\n   "++showADL _r++"\n   "++chain "\n   " _errcpu++"\n"
                     | not (null _errcpu)]))
              _antcOrnr =
                  _lhsIrnr + 1
              _consOrnr =
                  if c_==AlwaysExpr then _lhsIrnr + 1 else _antcIrnr
              _lhsOrnr =
                  _consIrnr
              _lhsOrules =
                  _rs
              _antcOgE =
                  _lhsIgE
              _antcOpn =
                  _lhsIpn
              _antcOsDef =
                  _lhsIsDef
              _consOgE =
                  _lhsIgE
              _consOpn =
                  _lhsIpn
              _consOsDef =
                  _lhsIsDef
              _cpuOgE =
                  _lhsIgE
              _cpuOpn =
                  _lhsIpn
              _cpuOrnr =
                  _consIrnr
              _cpuOsDef =
                  _lhsIsDef
              ( _antcIexpr,_antcImorphisms,_antcIraw,_antcIrnr,_antcIrules,_antcIsErr,_antcIsigns) =
                  (antc_ _antcOgE _antcOisign _antcOpn _antcOpos _antcOrnr _antcOsDef )
              ( _consIexpr,_consImorphisms,_consIraw,_consIrnr,_consIrules,_consIsErr,_consIsigns) =
                  (cons_ _consOgE _consOisign _consOpn _consOpos _consOrnr _consOsDef )
              ( _cpuIexprs,_cpuImorphisms,_cpuIraw,_cpuIrnr,_cpuIrules,_cpuIsErr,_cpuIsignss) =
                  (cpu_ _cpuOgE _cpuOisign _cpuOpn _cpuOpos _cpuOrnr _cpuOsDef _cpuOseptets )
          in  ( _lhsOdeclarations,_lhsOmGen,_lhsOmorphisms,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOusedDecls)))
sem_Rule_Sg pos_ rule_ expl_ sgn_ nr_ pn_ signal_  =
    (\ _lhsIgE
       _lhsIpn
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOmGen =
                  []
              _lhsOmorphisms =
                  _ruleImorphisms
              _lhsOusedDecls =
                  rd (declarations _ruleImorphisms)
              _lhsOdeclarations =
                  declarations _rs
              _signalname =
                  if null _signalInm || take 6 _signalInm == "Signal" && and [isDigit c| c<-drop 6 _signalInm ]
                  then "Signal"++show _lhsIrnr else _signalInm
              _rs =
                  [ Sg p r expla sgn n _lhsIpn (Sgn _signalname a b [] "" "" "" [] "" pos_ _lhsIrnr True)
                  | r@(Ru c antc p cons cpu expla sgn n _lhsIpn)<-take 1 _ruleIrules
                  , (a,b)<-[sign r] ] ++ drop 1 _ruleIrules
              _lhsOsErr =
                  _ruleIsErr
              _ruleOrnr =
                  _lhsIrnr
              _lhsOrnr =
                  if null _signalInm || take 6 _signalInm == "Signal" && and [isDigit c| c<-drop 6 _signalInm ]
                  then _lhsIrnr + 1 else _lhsIrnr
              _lhsOrules =
                  _rs
              _ruleOgE =
                  _lhsIgE
              _ruleOpn =
                  _lhsIpn
              _ruleOsDef =
                  _lhsIsDef
              _signalOgE =
                  _lhsIgE
              _signalOrnr =
                  _ruleIrnr
              _signalOsDef =
                  _lhsIsDef
              ( _ruleIdeclarations,_ruleImGen,_ruleImorphisms,_ruleIrnr,_ruleIrules,_ruleIsErr,_ruleIusedDecls) =
                  (rule_ _ruleOgE _ruleOpn _ruleOrnr _ruleOsDef )
              ( _signalIdeclaration,_signalInm,_signalIrawDecl,_signalIrnr,_signalIrules,_signalIsErr) =
                  (signal_ _signalOgE _signalOrnr _signalOsDef )
          in  ( _lhsOdeclarations,_lhsOmGen,_lhsOmorphisms,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOusedDecls)))
-- RuleType ----------------------------------------------------
-- cata
sem_RuleType (AlwaysExpr )  =
    (sem_RuleType_AlwaysExpr )
sem_RuleType (Automatic )  =
    (sem_RuleType_Automatic )
sem_RuleType (Equivalence )  =
    (sem_RuleType_Equivalence )
sem_RuleType (Generalization )  =
    (sem_RuleType_Generalization )
sem_RuleType (Implication )  =
    (sem_RuleType_Implication )
sem_RuleType_AlwaysExpr  =
    (let 
     in  ( ))
sem_RuleType_Automatic  =
    (let 
     in  ( ))
sem_RuleType_Equivalence  =
    (let 
     in  ( ))
sem_RuleType_Generalization  =
    (let 
     in  ( ))
sem_RuleType_Implication  =
    (let 
     in  ( ))
-- Rules -------------------------------------------------------
-- cata
sem_Rules list  =
    (Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list) )
sem_Rules_Cons hd_ tl_  =
    (\ _lhsIgE
       _lhsIpn
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOmGen =
                  _hdImGen ++ _tlImGen
              _lhsOmorphisms =
                  _hdImorphisms ++ _tlImorphisms
              _lhsOsErr =
                  _hdIsErr ++ _tlIsErr
              _lhsOdeclarations =
                  _hdIdeclarations ++ _tlIdeclarations
              _lhsOusedDecls =
                  _hdIusedDecls ++ _tlIusedDecls
              _hdOrnr =
                  _lhsIrnr
              _tlOrnr =
                  _hdIrnr
              _lhsOrnr =
                  _tlIrnr
              _lhsOrules =
                  _hdIrules ++ _tlIrules
              _hdOgE =
                  _lhsIgE
              _hdOpn =
                  _lhsIpn
              _hdOsDef =
                  _lhsIsDef
              _tlOgE =
                  _lhsIgE
              _tlOpn =
                  _lhsIpn
              _tlOsDef =
                  _lhsIsDef
              ( _hdIdeclarations,_hdImGen,_hdImorphisms,_hdIrnr,_hdIrules,_hdIsErr,_hdIusedDecls) =
                  (hd_ _hdOgE _hdOpn _hdOrnr _hdOsDef )
              ( _tlIdeclarations,_tlImGen,_tlImorphisms,_tlIrnr,_tlIrules,_tlIsErr,_tlIusedDecls) =
                  (tl_ _tlOgE _tlOpn _tlOrnr _tlOsDef )
          in  ( _lhsOdeclarations,_lhsOmGen,_lhsOmorphisms,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOusedDecls)))
sem_Rules_Nil  =
    (\ _lhsIgE
       _lhsIpn
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOmGen =
                  []
              _lhsOmorphisms =
                  []
              _lhsOsErr =
                  []
              _lhsOdeclarations =
                  []
              _lhsOusedDecls =
                  []
              _lhsOrnr =
                  _lhsIrnr
              _lhsOrules =
                  []
          in  ( _lhsOdeclarations,_lhsOmGen,_lhsOmorphisms,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOusedDecls)))
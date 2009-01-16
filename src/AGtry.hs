

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
    cpu'  = [ (c,es) | c<-r_cpu r, es<-[rd[m| m<-matches, c `match` m]], length es/=1]
    recur  []    = ([],"")
    recur (e:es) = (subexps++matches , "   recur "++showADL (e:es)++" = "++showADL (subexps++matches)++"\nbecause\n"++
                   "("++showADL subexps++" = subexprs ("++showADL e++") and\n"++
                   "("++showADL matches++" = recur ("++showADL es++")\n--------\n"++str'++str'')
     where (subexps,str')  = subexprs e
           (matches,str'') = recur es
    subexprs e@(Tm m)    = ([e| c<-r_cpu r, e `match` c] , "")
    subexprs e@(Tc e')   = subexprs e'
    subexprs e@(F ts)    = ([e| c<-r_cpu r, e `match` c]++matches ,  showADL matches++" = recur ("++showADL ts++")\n--------\n"++str)
                           where (matches,str) = recur ts
    subexprs e@(Fd ts)   = ([e| c<-r_cpu r, e `match` c]++matches ,  showADL matches++" = recur ("++showADL ts++")\n--------\n"++str)
                           where (matches,str) = recur ts
    subexprs e@(Fi fs)   = ([e| c<-r_cpu r, e `match` c]++matches ,  showADL matches++" = recur ("++showADL fs++")\n--------\n"++str)
                           where (matches,str) = recur fs
    subexprs e@(Fu fs)   = ([e| c<-r_cpu r, e `match` c]++matches ,  showADL matches++" = recur ("++showADL fs++")\n--------\n"++str)
                           where (matches,str) = recur fs
    subexprs e@(K0 e')   = ([e| c<-r_cpu r, e `match` c]++matches ,  showADL matches++" = subexprs ("++showADL e'++")\n--------\n"++str)
                           where (matches,str) = subexprs e'
    subexprs e@(K1 e')   = ([e| c<-r_cpu r, e `match` c]++matches ,  showADL matches++" = subexprs ("++showADL e'++")\n--------\n"++str)
                           where (matches,str) = subexprs e'
    subexprs e@(Cp e')   = ([e| c<-r_cpu r, e `match` c]++matches ,  showADL matches++" = subexprs ("++showADL e'++")\n--------\n"++str)
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
{-
   visit 0:
      synthesized attributes:
         cs                   : Contexts 
         sErr                 : [String]
   alternatives:
      alternative Arch:
         child cs             : Contexts 
-}
-- cata
sem_Architecture :: Architecture  ->
                    T_Architecture 
sem_Architecture (Arch _cs )  =
    (sem_Architecture_Arch (sem_Contexts _cs ) )
-- semantic domain
type T_Architecture  = ( Contexts,([String]))
sem_Architecture_Arch :: T_Contexts  ->
                         T_Architecture 
sem_Architecture_Arch cs_  =
    (let _lhsOcs :: Contexts
         _csOctxs :: ([(Context,(Gens,Declarations))])
         _lhsOsErr :: ([String])
         _csIcontexts :: Contexts
         _csIover :: ([(Context,(Gens,Declarations))])
         _csIsErr :: ([String])
         -- "AGtry.ag"(line 160, column 10)
         _lhsOcs =
             _csIcontexts
         -- "AGtry.ag"(line 161, column 10)
         _csOctxs =
             _csIover
         -- "AGtry.ag"(line 603, column 10)
         _lhsOsErr =
             _csIsErr
         ( _csIcontexts,_csIover,_csIsErr) =
             (cs_ _csOctxs )
     in  ( _lhsOcs,_lhsOsErr))
-- Concept -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         gE                   : GenR
      synthesized attributes:
         concept              : Concept 
         nm                   : String
   alternatives:
      alternative Anything:
      alternative C:
         child c              : {String}
         child gE             : {GenR}
         child os             : {[String]}
      alternative NOthing:
      alternative S:
-}
-- cata
sem_Concept :: Concept  ->
               T_Concept 
sem_Concept (Anything )  =
    (sem_Concept_Anything )
sem_Concept (C _c _gE _os )  =
    (sem_Concept_C _c _gE _os )
sem_Concept (NOthing )  =
    (sem_Concept_NOthing )
sem_Concept (S )  =
    (sem_Concept_S )
-- semantic domain
type T_Concept  = GenR ->
                  ( Concept,String)
sem_Concept_Anything :: T_Concept 
sem_Concept_Anything  =
    (\ _lhsIgE ->
         (let _lhsOconcept :: Concept
              _lhsOnm :: String
              -- "AGtry.ag"(line 1105, column 7)
              _lhsOconcept =
                  Anything
              -- "AGtry.ag"(line 1106, column 7)
              _lhsOnm =
                  "Anything"
          in  ( _lhsOconcept,_lhsOnm)))
sem_Concept_C :: String ->
                 GenR ->
                 ([String]) ->
                 T_Concept 
sem_Concept_C c_ gE_ os_  =
    (\ _lhsIgE ->
         (let _lhsOconcept :: Concept
              _lhsOnm :: String
              -- "AGtry.ag"(line 1100, column 7)
              _lhsOconcept =
                  C c_ _lhsIgE []
              -- "AGtry.ag"(line 1101, column 7)
              _lhsOnm =
                  c_
          in  ( _lhsOconcept,_lhsOnm)))
sem_Concept_NOthing :: T_Concept 
sem_Concept_NOthing  =
    (\ _lhsIgE ->
         (let _lhsOconcept :: Concept
              _lhsOnm :: String
              -- "AGtry.ag"(line 1108, column 7)
              _lhsOconcept =
                  NOthing
              -- "AGtry.ag"(line 1109, column 7)
              _lhsOnm =
                  "NOthing"
          in  ( _lhsOconcept,_lhsOnm)))
sem_Concept_S :: T_Concept 
sem_Concept_S  =
    (\ _lhsIgE ->
         (let _lhsOconcept :: Concept
              _lhsOnm :: String
              -- "AGtry.ag"(line 1102, column 7)
              _lhsOconcept =
                  S
              -- "AGtry.ag"(line 1103, column 7)
              _lhsOnm =
                  "ONE"
          in  ( _lhsOconcept,_lhsOnm)))
-- ConceptDef --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         c                    : ConceptDef
   alternatives:
      alternative Cd:
         child pos            : {FilePos}
         child nm             : {String}
         child def            : {String}
         child ref            : {String}
-}
-- cata
sem_ConceptDef :: ConceptDef  ->
                  T_ConceptDef 
sem_ConceptDef (Cd _pos _nm _def _ref )  =
    (sem_ConceptDef_Cd _pos _nm _def _ref )
-- semantic domain
type T_ConceptDef  = ( ConceptDef)
sem_ConceptDef_Cd :: FilePos ->
                     String ->
                     String ->
                     String ->
                     T_ConceptDef 
sem_ConceptDef_Cd pos_ nm_ def_ ref_  =
    (let _lhsOc :: ConceptDef
         -- "AGtry.ag"(line 1113, column 8)
         _lhsOc =
             Cd pos_ nm_ def_ ref_
     in  ( _lhsOc))
-- ConceptDefs -------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         conDefs              : [ConceptDef]
   alternatives:
      alternative Cons:
         child hd             : ConceptDef 
         child tl             : ConceptDefs 
      alternative Nil:
-}
-- cata
sem_ConceptDefs :: ConceptDefs  ->
                   T_ConceptDefs 
sem_ConceptDefs list  =
    (Prelude.foldr sem_ConceptDefs_Cons sem_ConceptDefs_Nil (Prelude.map sem_ConceptDef list) )
-- semantic domain
type T_ConceptDefs  = ( ([ConceptDef]))
sem_ConceptDefs_Cons :: T_ConceptDef  ->
                        T_ConceptDefs  ->
                        T_ConceptDefs 
sem_ConceptDefs_Cons hd_ tl_  =
    (let _lhsOconDefs :: ([ConceptDef])
         _hdIc :: ConceptDef
         _tlIconDefs :: ([ConceptDef])
         -- "AGtry.ag"(line 1117, column 10)
         _lhsOconDefs =
             _hdIc : _tlIconDefs
         ( _hdIc) =
             (hd_ )
         ( _tlIconDefs) =
             (tl_ )
     in  ( _lhsOconDefs))
sem_ConceptDefs_Nil :: T_ConceptDefs 
sem_ConceptDefs_Nil  =
    (let _lhsOconDefs :: ([ConceptDef])
         -- "AGtry.ag"(line 1118, column 10)
         _lhsOconDefs =
             []
     in  ( _lhsOconDefs))
-- Concepts ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         gE                   : GenR
   alternatives:
      alternative Cons:
         child hd             : Concept 
         child tl             : Concepts 
      alternative Nil:
-}
-- cata
sem_Concepts :: Concepts  ->
                T_Concepts 
sem_Concepts list  =
    (Prelude.foldr sem_Concepts_Cons sem_Concepts_Nil (Prelude.map sem_Concept list) )
-- semantic domain
type T_Concepts  = GenR ->
                   ( )
sem_Concepts_Cons :: T_Concept  ->
                     T_Concepts  ->
                     T_Concepts 
sem_Concepts_Cons hd_ tl_  =
    (\ _lhsIgE ->
         (let _hdOgE :: GenR
              _tlOgE :: GenR
              _hdIconcept :: Concept
              _hdInm :: String
              -- copy rule (down)
              _hdOgE =
                  _lhsIgE
              -- copy rule (down)
              _tlOgE =
                  _lhsIgE
              ( _hdIconcept,_hdInm) =
                  (hd_ _hdOgE )
          in  ( )))
sem_Concepts_Nil :: T_Concepts 
sem_Concepts_Nil  =
    (\ _lhsIgE ->
         (let 
          in  ( )))
-- Context -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ctxTree              : Classification (Context,(Gens,Declarations))
         ctxs                 : [(Context,(Gens,Declarations))]
      synthesized attributes:
         context              : Context 
         nRules               : Int
         over                 : [(Context,(Gens,Declarations))]
         rules                : Rules 
         sErr                 : [String]
   alternatives:
      alternative Ctx:
         child nm             : {String}
         child on             : {[String]}
         child isa            : {Inheritance Concept}
         child world          : {[Classification Context]}
         child pats           : Patterns 
         child rs             : Rules 
         child ds             : Declarations 
         child cs             : ConceptDefs 
         child ks             : KeyDefs 
         child os             : ObjectDefs 
         child pops           : Populations 
         visit 0:
            local mD          : _
            local mC          : _
            local keys        : _
            local mGen        : _
            local inh         : _
            local genE        : _
            local cD          : _
            local ctx         : _
-}
-- cata
sem_Context :: Context  ->
               T_Context 
sem_Context (Ctx _nm _on _isa _world _pats _rs _ds _cs _ks _os _pops )  =
    (sem_Context_Ctx _nm _on _isa _world (sem_Patterns _pats ) (sem_Rules _rs ) (sem_Declarations _ds ) (sem_ConceptDefs _cs ) (sem_KeyDefs _ks ) (sem_ObjectDefs _os ) (sem_Populations _pops ) )
-- semantic domain
type T_Context  = (Classification (Context,(Gens,Declarations))) ->
                  ([(Context,(Gens,Declarations))]) ->
                  ( Context,Int,([(Context,(Gens,Declarations))]),Rules,([String]))
sem_Context_Ctx :: String ->
                   ([String]) ->
                   (Inheritance Concept) ->
                   ([Classification Context]) ->
                   T_Patterns  ->
                   T_Rules  ->
                   T_Declarations  ->
                   T_ConceptDefs  ->
                   T_KeyDefs  ->
                   T_ObjectDefs  ->
                   T_Populations  ->
                   T_Context 
sem_Context_Ctx nm_ on_ isa_ world_ pats_ rs_ ds_ cs_ ks_ os_ pops_  =
    (\ _lhsIctxTree
       _lhsIctxs ->
         (let _patsOsDef :: Declarations
              _rsOsDef :: Declarations
              _dsOsDef :: Declarations
              _ksOsDef :: Declarations
              _osOsDef :: Declarations
              _popsOsDef :: Declarations
              _patsOgE :: GenR
              _rsOgE :: GenR
              _dsOgE :: GenR
              _ksOgE :: GenR
              _osOgE :: GenR
              _rsOpn :: String
              _popsOgE :: GenR
              _osOiConcs :: ([Concept])
              _osOiConc :: Concept
              _lhsOover :: ([(Context,(Gens,Declarations))])
              _lhsOcontext :: Context
              _lhsOsErr :: ([String])
              _patsOrnr :: Int
              _osOrnr :: Int
              _dsOrnr :: Int
              _ksOrnr :: Int
              _lhsOnRules :: Int
              _lhsOrules :: Rules
              _rsOrnr :: Int
              _patsIconDefs :: ([ConceptDef])
              _patsIkeyDefs :: KeyDefs
              _patsImGen :: Gens
              _patsImorphisms :: Morphisms
              _patsIpatterns :: Patterns
              _patsIrawDecls :: Declarations
              _patsIrnr :: Int
              _patsIrules :: Rules
              _patsIsErr :: ([String])
              _patsIusedDecls :: Declarations
              _rsIdeclarations :: Declarations
              _rsImGen :: Gens
              _rsImorphisms :: Morphisms
              _rsIrnr :: Int
              _rsIrules :: Rules
              _rsIsErr :: ([String])
              _rsIusedDecls :: Declarations
              _dsIdeclarations :: Declarations
              _dsIrawDecls :: Declarations
              _dsIrnr :: Int
              _dsIrules :: Rules
              _dsIsErr :: ([String])
              _csIconDefs :: ([ConceptDef])
              _ksIexprs :: Expressions
              _ksIkeyDefs :: KeyDefs
              _ksIrnr :: Int
              _ksIrules :: Rules
              _ksIsErr :: ([String])
              _osIobjDefs :: ObjectDefs
              _osIrnr :: Int
              _osIrules :: Rules
              _osIsErr :: ([String])
              _osIsources :: ([Concept])
              _popsIpopus :: Populations
              _popsIsErr :: ([String])
              -- "AGtry.ag"(line 186, column 9)
              _mD =
                  (                       renumber.mergecontents.concat) [mD| (context,(mG,mD)) <- preCl _lhsIctxTree]
              -- "AGtry.ag"(line 187, column 9)
              _mC =
                  mergecontents(_dsIrawDecls ++ _patsIrawDecls)
              -- "AGtry.ag"(line 188, column 9)
              _keys =
                  rd (_ksIkeyDefs ++ _patsIkeyDefs)
              -- "AGtry.ag"(line 189, column 9)
              _mGen =
                  (rd.concat) [mG| (context,(mG,mD)) <- preCl _lhsIctxTree]
              -- "AGtry.ag"(line 190, column 9)
              _inh =
                  Isa [(g,s)|G pos g s<- _mGen] (concs _mD>-rd [c|G pos g s<- _mGen, c<-[g,s]])
              -- "AGtry.ag"(line 191, column 9)
              _genE =
                  cmp where cmp Anything b = True
                            cmp a Anything = False
                            cmp NOthing b  = False
                            cmp a NOthing  = True
                            cmp a b        = if a==b then True else genEq (typology _inh) a b
              -- "AGtry.ag"(line 196, column 9)
              _cD =
                  makeConceptSpace _genE _patsImorphisms
              -- "AGtry.ag"(line 197, column 9)
              _patsOsDef =
                  _mD
              -- "AGtry.ag"(line 198, column 9)
              _rsOsDef =
                  _mD
              -- "AGtry.ag"(line 199, column 9)
              _dsOsDef =
                  _mD
              -- "AGtry.ag"(line 200, column 9)
              _ksOsDef =
                  _mD
              -- "AGtry.ag"(line 201, column 9)
              _osOsDef =
                  _mD
              -- "AGtry.ag"(line 202, column 9)
              _popsOsDef =
                  _mD
              -- "AGtry.ag"(line 203, column 9)
              _patsOgE =
                  _genE
              -- "AGtry.ag"(line 204, column 9)
              _rsOgE =
                  _genE
              -- "AGtry.ag"(line 205, column 9)
              _dsOgE =
                  _genE
              -- "AGtry.ag"(line 206, column 9)
              _ksOgE =
                  _genE
              -- "AGtry.ag"(line 207, column 9)
              _osOgE =
                  _genE
              -- "AGtry.ag"(line 208, column 9)
              _rsOpn =
                  ""
              -- "AGtry.ag"(line 209, column 9)
              _popsOgE =
                  _genE
              -- "AGtry.ag"(line 210, column 9)
              _osOiConcs =
                  []
              -- "AGtry.ag"(line 211, column 9)
              _osOiConc =
                  Anything
              -- "AGtry.ag"(line 212, column 9)
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
              -- "AGtry.ag"(line 226, column 9)
              _lhsOcontext =
                  _ctx
              -- "AGtry.ag"(line 227, column 9)
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
                        _popsIpopus
                   )
              -- "AGtry.ag"(line 608, column 10)
              _lhsOsErr =
                  _patsIsErr ++ _ksIsErr ++ _osIsErr ++ _popsIsErr
              -- "AGtry.ag"(line 903, column 10)
              _patsOrnr =
                  1
              -- "AGtry.ag"(line 904, column 10)
              _osOrnr =
                  _patsIrnr
              -- "AGtry.ag"(line 905, column 10)
              _dsOrnr =
                  _osIrnr
              -- "AGtry.ag"(line 906, column 10)
              _ksOrnr =
                  _dsIrnr
              -- "AGtry.ag"(line 907, column 10)
              _lhsOnRules =
                  _ksIrnr-1
              -- "AGtry.ag"(line 908, column 10)
              _lhsOrules =
                  _patsIrules ++ _osIrules ++ _dsIrules ++ _ksIrules
              -- copy rule (chain)
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
{-
   visit 0:
      inherited attribute:
         ctxs                 : [(Context,(Gens,Declarations))]
      synthesized attributes:
         contexts             : Contexts 
         over                 : [(Context,(Gens,Declarations))]
         sErr                 : [String]
   alternatives:
      alternative Cons:
         child hd             : Context 
         child tl             : Contexts 
      alternative Nil:
-}
-- cata
sem_Contexts :: Contexts  ->
                T_Contexts 
sem_Contexts list  =
    (Prelude.foldr sem_Contexts_Cons sem_Contexts_Nil (Prelude.map sem_Context list) )
-- semantic domain
type T_Contexts  = ([(Context,(Gens,Declarations))]) ->
                   ( Contexts,([(Context,(Gens,Declarations))]),([String]))
sem_Contexts_Cons :: T_Context  ->
                     T_Contexts  ->
                     T_Contexts 
sem_Contexts_Cons hd_ tl_  =
    (\ _lhsIctxs ->
         (let _lhsOcontexts :: Contexts
              _lhsOover :: ([(Context,(Gens,Declarations))])
              _hdOctxTree :: (Classification (Context,(Gens,Declarations)))
              _lhsOsErr :: ([String])
              _hdOctxs :: ([(Context,(Gens,Declarations))])
              _tlOctxs :: ([(Context,(Gens,Declarations))])
              _hdIcontext :: Context
              _hdInRules :: Int
              _hdIover :: ([(Context,(Gens,Declarations))])
              _hdIrules :: Rules
              _hdIsErr :: ([String])
              _tlIcontexts :: Contexts
              _tlIover :: ([(Context,(Gens,Declarations))])
              _tlIsErr :: ([String])
              -- "AGtry.ag"(line 163, column 10)
              _lhsOcontexts =
                  _hdIcontext : _tlIcontexts
              -- "AGtry.ag"(line 164, column 10)
              _lhsOover =
                  _hdIover ++ _tlIover
              -- "AGtry.ag"(line 165, column 10)
              _hdOctxTree =
                  mkCtxAG _lhsIctxs (name _hdIcontext)
              -- "AGtry.ag"(line 605, column 10)
              _lhsOsErr =
                  _hdIsErr ++ _tlIsErr
              -- copy rule (down)
              _hdOctxs =
                  _lhsIctxs
              -- copy rule (down)
              _tlOctxs =
                  _lhsIctxs
              ( _hdIcontext,_hdInRules,_hdIover,_hdIrules,_hdIsErr) =
                  (hd_ _hdOctxTree _hdOctxs )
              ( _tlIcontexts,_tlIover,_tlIsErr) =
                  (tl_ _tlOctxs )
          in  ( _lhsOcontexts,_lhsOover,_lhsOsErr)))
sem_Contexts_Nil :: T_Contexts 
sem_Contexts_Nil  =
    (\ _lhsIctxs ->
         (let _lhsOover :: ([(Context,(Gens,Declarations))])
              _lhsOcontexts :: Contexts
              _lhsOsErr :: ([String])
              -- "AGtry.ag"(line 166, column 10)
              _lhsOover =
                  []
              -- "AGtry.ag"(line 167, column 10)
              _lhsOcontexts =
                  []
              -- "AGtry.ag"(line 606, column 10)
              _lhsOsErr =
                  []
          in  ( _lhsOcontexts,_lhsOover,_lhsOsErr)))
-- Declaration -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         gE                   : GenR
         sDef                 : Declarations 
      chained attribute:
         rnr                  : Int
      synthesized attributes:
         declaration          : Declaration 
         nm                   : String
         rawDecl              : Declaration 
         rules                : Rules 
         sErr                 : [String]
   alternatives:
      alternative Iscompl:
         child g              : Concept 
         child s              : Concept 
      alternative Isn:
         child g              : Concept 
         child s              : Concept 
      alternative Sgn:
         child nm             : {String}
         child a              : Concept 
         child b              : Concept 
         child props          : {[Prop]}
         child prL            : {String}
         child prM            : {String}
         child prR            : {String}
         child content        : {[Paire]}
         child expla          : {String}
         child morPos         : {FilePos}
         child nr             : {Int}
         child sig            : {Bool}
         visit 0:
            local msignat     : _
      alternative Vs:
         child g              : Concept 
         child s              : Concept 
-}
-- cata
sem_Declaration :: Declaration  ->
                   T_Declaration 
sem_Declaration (Iscompl _g _s )  =
    (sem_Declaration_Iscompl (sem_Concept _g ) (sem_Concept _s ) )
sem_Declaration (Isn _g _s )  =
    (sem_Declaration_Isn (sem_Concept _g ) (sem_Concept _s ) )
sem_Declaration (Sgn _nm _a _b _props _prL _prM _prR _content _expla _morPos _nr _sig )  =
    (sem_Declaration_Sgn _nm (sem_Concept _a ) (sem_Concept _b ) _props _prL _prM _prR _content _expla _morPos _nr _sig )
sem_Declaration (Vs _g _s )  =
    (sem_Declaration_Vs (sem_Concept _g ) (sem_Concept _s ) )
-- semantic domain
type T_Declaration  = GenR ->
                      Int ->
                      Declarations ->
                      ( Declaration,String,Declaration,Int,Rules,([String]))
sem_Declaration_Iscompl :: T_Concept  ->
                           T_Concept  ->
                           T_Declaration 
sem_Declaration_Iscompl g_ s_  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr :: ([String])
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _lhsOdeclaration :: Declaration
              _lhsOrawDecl :: Declaration
              _lhsOnm :: String
              _gOgE :: GenR
              _sOgE :: GenR
              _gIconcept :: Concept
              _gInm :: String
              _sIconcept :: Concept
              _sInm :: String
              -- "AGtry.ag"(line 790, column 13)
              _lhsOsErr =
                  []
              -- "AGtry.ag"(line 1036, column 13)
              _lhsOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 1037, column 10)
              _lhsOrules =
                  []
              -- "AGtry.ag"(line 1298, column 13)
              _lhsOdeclaration =
                  Iscompl _gIconcept _sIconcept
              -- "AGtry.ag"(line 1299, column 9)
              _lhsOrawDecl =
                  Iscompl _gIconcept _sIconcept
              -- "AGtry.ag"(line 1300, column 9)
              _lhsOnm =
                  "I"
              -- copy rule (down)
              _gOgE =
                  _lhsIgE
              -- copy rule (down)
              _sOgE =
                  _lhsIgE
              ( _gIconcept,_gInm) =
                  (g_ _gOgE )
              ( _sIconcept,_sInm) =
                  (s_ _sOgE )
          in  ( _lhsOdeclaration,_lhsOnm,_lhsOrawDecl,_lhsOrnr,_lhsOrules,_lhsOsErr)))
sem_Declaration_Isn :: T_Concept  ->
                       T_Concept  ->
                       T_Declaration 
sem_Declaration_Isn g_ s_  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr :: ([String])
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _lhsOdeclaration :: Declaration
              _lhsOrawDecl :: Declaration
              _lhsOnm :: String
              _gOgE :: GenR
              _sOgE :: GenR
              _gIconcept :: Concept
              _gInm :: String
              _sIconcept :: Concept
              _sInm :: String
              -- "AGtry.ag"(line 789, column 9)
              _lhsOsErr =
                  []
              -- "AGtry.ag"(line 1034, column 10)
              _lhsOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 1035, column 10)
              _lhsOrules =
                  []
              -- "AGtry.ag"(line 1295, column 9)
              _lhsOdeclaration =
                  Isn _gIconcept _sIconcept
              -- "AGtry.ag"(line 1296, column 9)
              _lhsOrawDecl =
                  Isn _gIconcept _sIconcept
              -- "AGtry.ag"(line 1297, column 9)
              _lhsOnm =
                  "I"
              -- copy rule (down)
              _gOgE =
                  _lhsIgE
              -- copy rule (down)
              _sOgE =
                  _lhsIgE
              ( _gIconcept,_gInm) =
                  (g_ _gOgE )
              ( _sIconcept,_sInm) =
                  (s_ _sOgE )
          in  ( _lhsOdeclaration,_lhsOnm,_lhsOrawDecl,_lhsOrnr,_lhsOrules,_lhsOsErr)))
sem_Declaration_Sgn :: String ->
                       T_Concept  ->
                       T_Concept  ->
                       ([Prop]) ->
                       String ->
                       String ->
                       String ->
                       ([Paire]) ->
                       String ->
                       FilePos ->
                       Int ->
                       Bool ->
                       T_Declaration 
sem_Declaration_Sgn nm_ a_ b_ props_ prL_ prM_ prR_ content_ expla_ morPos_ nr_ sig_  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr :: ([String])
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _lhsOdeclaration :: Declaration
              _lhsOrawDecl :: Declaration
              _lhsOnm :: String
              _aOgE :: GenR
              _bOgE :: GenR
              _aIconcept :: Concept
              _aInm :: String
              _bIconcept :: Concept
              _bInm :: String
              -- "AGtry.ag"(line 781, column 9)
              _lhsOsErr =
                  if _aInm == _bInm then [] else
                  let ps = props_ `isc` [Trn,Rfx,Sym,Asy] in
                  if null ps then []
                  else ["7 in "++show morPos_++"\n   Heterogeneous relation "++
                        nm_++"["++ _aInm ++"*"++ _bInm ++
                        "]\n   may not be declared "++commaEng "and" [pMeaning p| p<-ps]++
                        ".\n   (This relation is heterogeneous because "++ _aInm ++" and "++ _bInm ++
                        "\n   are different concepts).\n"]
              -- "AGtry.ag"(line 1032, column 10)
              _lhsOrnr =
                  _lhsIrnr + length (multRules _msignat)
              -- "AGtry.ag"(line 1033, column 10)
              _lhsOrules =
                  renumberRules _lhsIrnr (multRules _msignat)
              -- "AGtry.ag"(line 1289, column 9)
              _lhsOdeclaration =
                  _msignat
              -- "AGtry.ag"(line 1290, column 9)
              _msignat =
                  head ([ s
                        | s <- _lhsIsDef, nm_==name s, _aIconcept==source s, _bIconcept==target s]++
                        [error ("Missing "++showHS "" (Sgn nm_ _aIconcept _bIconcept props_ prL_ prM_ prR_ content_ expla_ morPos_ 0 sig_)++" in AGtry.ag\n"++ show _lhsIsDef)])
              -- "AGtry.ag"(line 1293, column 9)
              _lhsOrawDecl =
                  Sgn nm_ _aIconcept _bIconcept props_ prL_ prM_ prR_ content_ expla_ morPos_ 0 sig_
              -- "AGtry.ag"(line 1294, column 9)
              _lhsOnm =
                  nm_
              -- copy rule (down)
              _aOgE =
                  _lhsIgE
              -- copy rule (down)
              _bOgE =
                  _lhsIgE
              ( _aIconcept,_aInm) =
                  (a_ _aOgE )
              ( _bIconcept,_bInm) =
                  (b_ _bOgE )
          in  ( _lhsOdeclaration,_lhsOnm,_lhsOrawDecl,_lhsOrnr,_lhsOrules,_lhsOsErr)))
sem_Declaration_Vs :: T_Concept  ->
                      T_Concept  ->
                      T_Declaration 
sem_Declaration_Vs g_ s_  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr :: ([String])
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _lhsOdeclaration :: Declaration
              _lhsOrawDecl :: Declaration
              _lhsOnm :: String
              _gOgE :: GenR
              _sOgE :: GenR
              _gIconcept :: Concept
              _gInm :: String
              _sIconcept :: Concept
              _sInm :: String
              -- "AGtry.ag"(line 791, column 8)
              _lhsOsErr =
                  []
              -- "AGtry.ag"(line 1038, column 10)
              _lhsOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 1039, column 10)
              _lhsOrules =
                  []
              -- "AGtry.ag"(line 1301, column 9)
              _lhsOdeclaration =
                  Vs _gIconcept _sIconcept
              -- "AGtry.ag"(line 1302, column 9)
              _lhsOrawDecl =
                  Vs _gIconcept _sIconcept
              -- "AGtry.ag"(line 1303, column 9)
              _lhsOnm =
                  "V"
              -- copy rule (down)
              _gOgE =
                  _lhsIgE
              -- copy rule (down)
              _sOgE =
                  _lhsIgE
              ( _gIconcept,_gInm) =
                  (g_ _gOgE )
              ( _sIconcept,_sInm) =
                  (s_ _sOgE )
          in  ( _lhsOdeclaration,_lhsOnm,_lhsOrawDecl,_lhsOrnr,_lhsOrules,_lhsOsErr)))
-- Declarations ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         gE                   : GenR
         sDef                 : Declarations 
      chained attribute:
         rnr                  : Int
      synthesized attributes:
         declarations         : Declarations 
         rawDecls             : Declarations 
         rules                : Rules 
         sErr                 : [String]
   alternatives:
      alternative Cons:
         child hd             : Declaration 
         child tl             : Declarations 
      alternative Nil:
-}
-- cata
sem_Declarations :: Declarations  ->
                    T_Declarations 
sem_Declarations list  =
    (Prelude.foldr sem_Declarations_Cons sem_Declarations_Nil (Prelude.map sem_Declaration list) )
-- semantic domain
type T_Declarations  = GenR ->
                       Int ->
                       Declarations ->
                       ( Declarations,Declarations,Int,Rules,([String]))
sem_Declarations_Cons :: T_Declaration  ->
                         T_Declarations  ->
                         T_Declarations 
sem_Declarations_Cons hd_ tl_  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr :: ([String])
              _hdOrnr :: Int
              _tlOrnr :: Int
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _lhsOdeclarations :: Declarations
              _lhsOrawDecls :: Declarations
              _hdOgE :: GenR
              _hdOsDef :: Declarations
              _tlOgE :: GenR
              _tlOsDef :: Declarations
              _hdIdeclaration :: Declaration
              _hdInm :: String
              _hdIrawDecl :: Declaration
              _hdIrnr :: Int
              _hdIrules :: Rules
              _hdIsErr :: ([String])
              _tlIdeclarations :: Declarations
              _tlIrawDecls :: Declarations
              _tlIrnr :: Int
              _tlIrules :: Rules
              _tlIsErr :: ([String])
              -- "AGtry.ag"(line 793, column 10)
              _lhsOsErr =
                  _hdIsErr ++ _tlIsErr
              -- "AGtry.ag"(line 1042, column 10)
              _hdOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 1043, column 10)
              _tlOrnr =
                  _hdIrnr
              -- "AGtry.ag"(line 1044, column 10)
              _lhsOrnr =
                  _tlIrnr
              -- "AGtry.ag"(line 1045, column 10)
              _lhsOrules =
                  _hdIrules ++ _tlIrules
              -- "AGtry.ag"(line 1207, column 10)
              _lhsOdeclarations =
                  _hdIdeclaration: _tlIdeclarations
              -- "AGtry.ag"(line 1208, column 10)
              _lhsOrawDecls =
                  _hdIrawDecl: _tlIrawDecls
              -- copy rule (down)
              _hdOgE =
                  _lhsIgE
              -- copy rule (down)
              _hdOsDef =
                  _lhsIsDef
              -- copy rule (down)
              _tlOgE =
                  _lhsIgE
              -- copy rule (down)
              _tlOsDef =
                  _lhsIsDef
              ( _hdIdeclaration,_hdInm,_hdIrawDecl,_hdIrnr,_hdIrules,_hdIsErr) =
                  (hd_ _hdOgE _hdOrnr _hdOsDef )
              ( _tlIdeclarations,_tlIrawDecls,_tlIrnr,_tlIrules,_tlIsErr) =
                  (tl_ _tlOgE _tlOrnr _tlOsDef )
          in  ( _lhsOdeclarations,_lhsOrawDecls,_lhsOrnr,_lhsOrules,_lhsOsErr)))
sem_Declarations_Nil :: T_Declarations 
sem_Declarations_Nil  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr :: ([String])
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _lhsOdeclarations :: Declarations
              _lhsOrawDecls :: Declarations
              -- "AGtry.ag"(line 794, column 10)
              _lhsOsErr =
                  []
              -- "AGtry.ag"(line 1046, column 10)
              _lhsOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 1047, column 10)
              _lhsOrules =
                  []
              -- "AGtry.ag"(line 1209, column 10)
              _lhsOdeclarations =
                  []
              -- "AGtry.ag"(line 1210, column 10)
              _lhsOrawDecls =
                  []
          in  ( _lhsOdeclarations,_lhsOrawDecls,_lhsOrnr,_lhsOrules,_lhsOsErr)))
-- Expression --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         gE                   : GenR
         isign                : [(Concept,Concept)]
         pn                   : String
         pos                  : FilePos
         sDef                 : Declarations 
      chained attribute:
         rnr                  : Int
      synthesized attributes:
         expr                 : Expression 
         morphisms            : Morphisms
         raw                  : Expression 
         rules                : Rules 
         sErr                 : [String]
         signs                : [(Concept,Concept)]
   alternatives:
      alternative Cp:
         child e              : Expression 
      alternative F:
         child ts             : Expressions 
         visit 0:
            local sgns        : _
            local trpls       : _
            local dis         : _
      alternative Fd:
         child ts             : Expressions 
         visit 0:
            local sgns        : _
            local trpls       : _
            local dis         : _
      alternative Fi:
         child fs             : Expressions 
         visit 0:
            local sgns        : _
            local dis         : _
      alternative Fu:
         child fs             : Expressions 
         visit 0:
            local sgns        : _
            local dis         : _
      alternative K0:
         child e              : Expression 
         visit 0:
            local closmor     : _
      alternative K1:
         child e              : Expression 
         visit 0:
            local closmor     : _
      alternative Tc:
         child c              : Expression 
      alternative Tm:
         child m              : Morphism 
-}
-- cata
sem_Expression :: Expression  ->
                  T_Expression 
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
-- semantic domain
type T_Expression  = GenR ->
                     ([(Concept,Concept)]) ->
                     String ->
                     FilePos ->
                     Int ->
                     Declarations ->
                     ( Expression,Morphisms,Expression,Int,Rules,([String]),([(Concept,Concept)]))
sem_Expression_Cp :: T_Expression  ->
                     T_Expression 
sem_Expression_Cp e_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsigns :: ([(Concept,Concept)])
              _lhsOexpr :: Expression
              _lhsOraw :: Expression
              _lhsOmorphisms :: Morphisms
              _lhsOsErr :: ([String])
              _eOrnr :: Int
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _eOgE :: GenR
              _eOisign :: ([(Concept,Concept)])
              _eOpn :: String
              _eOpos :: FilePos
              _eOsDef :: Declarations
              _eIexpr :: Expression
              _eImorphisms :: Morphisms
              _eIraw :: Expression
              _eIrnr :: Int
              _eIrules :: Rules
              _eIsErr :: ([String])
              _eIsigns :: ([(Concept,Concept)])
              -- "AGtry.ag"(line 543, column 8)
              _lhsOsigns =
                  _eIsigns
              -- "AGtry.ag"(line 544, column 8)
              _lhsOexpr =
                  Cp _eIexpr
              -- "AGtry.ag"(line 545, column 8)
              _lhsOraw =
                  Cp _eIraw
              -- "AGtry.ag"(line 591, column 8)
              _lhsOmorphisms =
                  _eImorphisms
              -- "AGtry.ag"(line 762, column 8)
              _lhsOsErr =
                  _eIsErr
              -- "AGtry.ag"(line 1019, column 10)
              _eOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 1020, column 10)
              _lhsOrnr =
                  _eIrnr
              -- "AGtry.ag"(line 1021, column 10)
              _lhsOrules =
                  _eIrules
              -- copy rule (down)
              _eOgE =
                  _lhsIgE
              -- copy rule (down)
              _eOisign =
                  _lhsIisign
              -- copy rule (down)
              _eOpn =
                  _lhsIpn
              -- copy rule (down)
              _eOpos =
                  _lhsIpos
              -- copy rule (down)
              _eOsDef =
                  _lhsIsDef
              ( _eIexpr,_eImorphisms,_eIraw,_eIrnr,_eIrules,_eIsErr,_eIsigns) =
                  (e_ _eOgE _eOisign _eOpn _eOpos _eOrnr _eOsDef )
          in  ( _lhsOexpr,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsigns)))
sem_Expression_F :: T_Expressions  ->
                    T_Expression 
sem_Expression_F ts_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsigns :: ([(Concept,Concept)])
              _lhsOexpr :: Expression
              _tsOseptets :: ([(Concept, Expressions, [(Concept,Concept)], [Concept], [(Concept,Concept)], Expressions, Concept)])
              _lhsOraw :: Expression
              _lhsOmorphisms :: Morphisms
              _lhsOsErr :: ([String])
              _tsOrnr :: Int
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _tsOgE :: GenR
              _tsOisign :: ([(Concept,Concept)])
              _tsOpn :: String
              _tsOpos :: FilePos
              _tsOsDef :: Declarations
              _tsIexprs :: Expressions
              _tsImorphisms :: Morphisms
              _tsIraw :: Expressions
              _tsIrnr :: Int
              _tsIrules :: Rules
              _tsIsErr :: ([String])
              _tsIsignss :: ([[(Concept,Concept)]])
              -- "AGtry.ag"(line 473, column 8)
              _lhsOsigns =
                  _sgns
              -- "AGtry.ag"(line 474, column 8)
              _sgns =
                  comps _lhsIgE _tsIsignss
              -- "AGtry.ag"(line 475, column 8)
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
              -- "AGtry.ag"(line 487, column 8)
              _lhsOexpr =
                  _dis
              -- "AGtry.ag"(line 488, column 8)
              _dis =
                  if null _lhsIisign then F (_tsIexprs) else
                  let (s,t)= head _lhsIisign
                  in F (
                        distr _lhsIpos (s,t) _lhsIgE [cs|(a,l,sl,cs,sr,r,b)<- _trpls, s `_lhsIgE` a, t `_lhsIgE` b] _tsIexprs)
              -- "AGtry.ag"(line 492, column 8)
              _tsOseptets =
                  _trpls
              -- "AGtry.ag"(line 493, column 8)
              _lhsOraw =
                  F _tsIraw
              -- "AGtry.ag"(line 585, column 8)
              _lhsOmorphisms =
                  _tsImorphisms
              -- "AGtry.ag"(line 670, column 8)
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
              -- "AGtry.ag"(line 981, column 10)
              _tsOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 982, column 10)
              _lhsOrnr =
                  _tsIrnr
              -- "AGtry.ag"(line 983, column 10)
              _lhsOrules =
                  _tsIrules
              -- copy rule (down)
              _tsOgE =
                  _lhsIgE
              -- copy rule (down)
              _tsOisign =
                  _lhsIisign
              -- copy rule (down)
              _tsOpn =
                  _lhsIpn
              -- copy rule (down)
              _tsOpos =
                  _lhsIpos
              -- copy rule (down)
              _tsOsDef =
                  _lhsIsDef
              ( _tsIexprs,_tsImorphisms,_tsIraw,_tsIrnr,_tsIrules,_tsIsErr,_tsIsignss) =
                  (ts_ _tsOgE _tsOisign _tsOpn _tsOpos _tsOrnr _tsOsDef _tsOseptets )
          in  ( _lhsOexpr,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsigns)))
sem_Expression_Fd :: T_Expressions  ->
                     T_Expression 
sem_Expression_Fd ts_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsigns :: ([(Concept,Concept)])
              _lhsOexpr :: Expression
              _tsOseptets :: ([(Concept, Expressions, [(Concept,Concept)], [Concept], [(Concept,Concept)], Expressions, Concept)])
              _lhsOraw :: Expression
              _lhsOmorphisms :: Morphisms
              _lhsOsErr :: ([String])
              _tsOrnr :: Int
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _tsOgE :: GenR
              _tsOisign :: ([(Concept,Concept)])
              _tsOpn :: String
              _tsOpos :: FilePos
              _tsOsDef :: Declarations
              _tsIexprs :: Expressions
              _tsImorphisms :: Morphisms
              _tsIraw :: Expressions
              _tsIrnr :: Int
              _tsIrules :: Rules
              _tsIsErr :: ([String])
              _tsIsignss :: ([[(Concept,Concept)]])
              -- "AGtry.ag"(line 494, column 8)
              _lhsOsigns =
                  _sgns
              -- "AGtry.ag"(line 495, column 8)
              _sgns =
                  comps _lhsIgE _tsIsignss
              -- "AGtry.ag"(line 496, column 8)
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
              -- "AGtry.ag"(line 508, column 8)
              _lhsOexpr =
                  _dis
              -- "AGtry.ag"(line 509, column 8)
              _dis =
                  if null _lhsIisign then Fd (_tsIexprs) else
                  let (s,t)= head _lhsIisign
                  in Fd (
                         distr _lhsIpos (s,t) _lhsIgE [cs|(a,l,sl,cs,sr,r,b)<- _trpls, a==s, b==t] _tsIexprs)
              -- "AGtry.ag"(line 513, column 8)
              _tsOseptets =
                  _trpls
              -- "AGtry.ag"(line 514, column 8)
              _lhsOraw =
                  Fd _tsIraw
              -- "AGtry.ag"(line 586, column 8)
              _lhsOmorphisms =
                  _tsImorphisms
              -- "AGtry.ag"(line 701, column 8)
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
              -- "AGtry.ag"(line 984, column 10)
              _tsOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 985, column 10)
              _lhsOrnr =
                  _tsIrnr
              -- "AGtry.ag"(line 986, column 10)
              _lhsOrules =
                  _tsIrules
              -- copy rule (down)
              _tsOgE =
                  _lhsIgE
              -- copy rule (down)
              _tsOisign =
                  _lhsIisign
              -- copy rule (down)
              _tsOpn =
                  _lhsIpn
              -- copy rule (down)
              _tsOpos =
                  _lhsIpos
              -- copy rule (down)
              _tsOsDef =
                  _lhsIsDef
              ( _tsIexprs,_tsImorphisms,_tsIraw,_tsIrnr,_tsIrules,_tsIsErr,_tsIsignss) =
                  (ts_ _tsOgE _tsOisign _tsOpn _tsOpos _tsOrnr _tsOsDef _tsOseptets )
          in  ( _lhsOexpr,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsigns)))
sem_Expression_Fi :: T_Expressions  ->
                     T_Expression 
sem_Expression_Fi fs_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsigns :: ([(Concept,Concept)])
              _fsOseptets :: ([(Concept, Expressions, [(Concept,Concept)], [Concept], [(Concept,Concept)], Expressions, Concept)])
              _lhsOexpr :: Expression
              _lhsOraw :: Expression
              _lhsOmorphisms :: Morphisms
              _lhsOsErr :: ([String])
              _fsOrnr :: Int
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _fsOgE :: GenR
              _fsOisign :: ([(Concept,Concept)])
              _fsOpn :: String
              _fsOpos :: FilePos
              _fsOsDef :: Declarations
              _fsIexprs :: Expressions
              _fsImorphisms :: Morphisms
              _fsIraw :: Expressions
              _fsIrnr :: Int
              _fsIrules :: Rules
              _fsIsErr :: ([String])
              _fsIsignss :: ([[(Concept,Concept)]])
              -- "AGtry.ag"(line 524, column 8)
              _lhsOsigns =
                  if null _sgns then error("Fatal: empty @sgns in Fi-expression "++showADL (Fi _fsIraw)) else _sgns
              -- "AGtry.ag"(line 525, column 8)
              _sgns =
                  if null _fsIsignss then error("Fatal: empty @fs.signss in expression "++showHS "" (Fi _fsIexprs)) else
                  foldr1 (llub _lhsIgE) _fsIsignss
              -- "AGtry.ag"(line 527, column 8)
              _fsOseptets =
                  []
              -- "AGtry.ag"(line 528, column 8)
              _lhsOexpr =
                  Fi _fsIexprs
              -- "AGtry.ag"(line 529, column 8)
              _dis =
                  let [(a,b)] = take 1 _lhsIisign
                      (a',b') = (source (head _fsIexprs), target (last _fsIexprs))
                  in Fi (                                    _fsIexprs                                    )
              -- "AGtry.ag"(line 532, column 8)
              _lhsOraw =
                  Fi _fsIraw
              -- "AGtry.ag"(line 587, column 8)
              _lhsOmorphisms =
                  _fsImorphisms
              -- "AGtry.ag"(line 745, column 8)
              _lhsOsErr =
                  if null _sgns
                  then ["4 in "++show _lhsIpos++"\n   Incompatible types in comparing\n   "++
                        commaEng "with\n  " [show f++" :: "++shSigns s| (f,s)<-zip _fsIraw _fsIsignss]++"\n"]
                  else _fsIsErr
              -- "AGtry.ag"(line 990, column 10)
              _fsOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 991, column 10)
              _lhsOrnr =
                  _fsIrnr
              -- "AGtry.ag"(line 992, column 10)
              _lhsOrules =
                  _fsIrules
              -- copy rule (down)
              _fsOgE =
                  _lhsIgE
              -- copy rule (down)
              _fsOisign =
                  _lhsIisign
              -- copy rule (down)
              _fsOpn =
                  _lhsIpn
              -- copy rule (down)
              _fsOpos =
                  _lhsIpos
              -- copy rule (down)
              _fsOsDef =
                  _lhsIsDef
              ( _fsIexprs,_fsImorphisms,_fsIraw,_fsIrnr,_fsIrules,_fsIsErr,_fsIsignss) =
                  (fs_ _fsOgE _fsOisign _fsOpn _fsOpos _fsOrnr _fsOsDef _fsOseptets )
          in  ( _lhsOexpr,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsigns)))
sem_Expression_Fu :: T_Expressions  ->
                     T_Expression 
sem_Expression_Fu fs_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsigns :: ([(Concept,Concept)])
              _fsOseptets :: ([(Concept, Expressions, [(Concept,Concept)], [Concept], [(Concept,Concept)], Expressions, Concept)])
              _lhsOexpr :: Expression
              _lhsOraw :: Expression
              _lhsOmorphisms :: Morphisms
              _lhsOsErr :: ([String])
              _fsOrnr :: Int
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _fsOgE :: GenR
              _fsOisign :: ([(Concept,Concept)])
              _fsOpn :: String
              _fsOpos :: FilePos
              _fsOsDef :: Declarations
              _fsIexprs :: Expressions
              _fsImorphisms :: Morphisms
              _fsIraw :: Expressions
              _fsIrnr :: Int
              _fsIrules :: Rules
              _fsIsErr :: ([String])
              _fsIsignss :: ([[(Concept,Concept)]])
              -- "AGtry.ag"(line 515, column 8)
              _lhsOsigns =
                  if null _sgns then error("Fatal: empty @sgns in Fu-expression "++showADL (Fi _fsIraw)) else _sgns
              -- "AGtry.ag"(line 516, column 8)
              _sgns =
                  if null _fsIsignss then error("Fatal: empty @fs.signss in expression "++showHS "" (Fu _fsIexprs)) else
                  foldr1 (llub _lhsIgE) _fsIsignss
              -- "AGtry.ag"(line 518, column 8)
              _fsOseptets =
                  []
              -- "AGtry.ag"(line 519, column 8)
              _lhsOexpr =
                  Fu _fsIexprs
              -- "AGtry.ag"(line 520, column 8)
              _dis =
                  let [(a,b)] = take 1 _lhsIisign
                      (a',b') = (source (head _fsIexprs), target (last _fsIexprs))
                  in Fu (                                    _fsIexprs                                    )
              -- "AGtry.ag"(line 523, column 8)
              _lhsOraw =
                  Fu _fsIraw
              -- "AGtry.ag"(line 588, column 8)
              _lhsOmorphisms =
                  _fsImorphisms
              -- "AGtry.ag"(line 730, column 8)
              _lhsOsErr =
                  if null _sgns
                  then ["4 in "++show _lhsIpos++"\n   Incompatible types in comparing\n   "++
                        commaEng "with\n  " [show f++" :: "++shSigns s| (f,s)<-zip _fsIraw _fsIsignss]++"\n"]
                  else _fsIsErr
              -- "AGtry.ag"(line 987, column 10)
              _fsOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 988, column 10)
              _lhsOrnr =
                  _fsIrnr
              -- "AGtry.ag"(line 989, column 10)
              _lhsOrules =
                  _fsIrules
              -- copy rule (down)
              _fsOgE =
                  _lhsIgE
              -- copy rule (down)
              _fsOisign =
                  _lhsIisign
              -- copy rule (down)
              _fsOpn =
                  _lhsIpn
              -- copy rule (down)
              _fsOpos =
                  _lhsIpos
              -- copy rule (down)
              _fsOsDef =
                  _lhsIsDef
              ( _fsIexprs,_fsImorphisms,_fsIraw,_fsIrnr,_fsIrules,_fsIsErr,_fsIsignss) =
                  (fs_ _fsOgE _fsOisign _fsOpn _fsOpos _fsOrnr _fsOsDef _fsOseptets )
          in  ( _lhsOexpr,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsigns)))
sem_Expression_K0 :: T_Expression  ->
                     T_Expression 
sem_Expression_K0 e_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsigns :: ([(Concept,Concept)])
              _lhsOexpr :: Expression
              _lhsOraw :: Expression
              _lhsOmorphisms :: Morphisms
              _lhsOsErr :: ([String])
              _eOrnr :: Int
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _eOgE :: GenR
              _eOisign :: ([(Concept,Concept)])
              _eOpn :: String
              _eOpos :: FilePos
              _eOsDef :: Declarations
              _eIexpr :: Expression
              _eImorphisms :: Morphisms
              _eIraw :: Expression
              _eIrnr :: Int
              _eIrules :: Rules
              _eIsErr :: ([String])
              _eIsigns :: ([(Concept,Concept)])
              -- "AGtry.ag"(line 533, column 8)
              _lhsOsigns =
                  _eIsigns
              -- "AGtry.ag"(line 534, column 8)
              _lhsOexpr =
                  Tm _closmor
              -- "AGtry.ag"(line 535, column 8)
              _closmor =
                  let [(a,b)] = take 1 _lhsIisign
                  in Mph (             (name.head.declarations) _eIexpr) _lhsIpos [] (a,b) True (Sgn (             (name.head.declarations) _eIexpr) a b [Trn,Rfx] "" "" "" [] "Closure" _lhsIpos 0 True)
              -- "AGtry.ag"(line 537, column 8)
              _lhsOraw =
                  K0 _eIraw
              -- "AGtry.ag"(line 589, column 8)
              _lhsOmorphisms =
                  _eImorphisms
              -- "AGtry.ag"(line 760, column 8)
              _lhsOsErr =
                  _eIsErr
              -- "AGtry.ag"(line 993, column 10)
              _eOrnr =
                  _lhsIrnr+1
              -- "AGtry.ag"(line 994, column 10)
              _lhsOrnr =
                  _eIrnr
              -- "AGtry.ag"(line 995, column 10)
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
              -- copy rule (down)
              _eOgE =
                  _lhsIgE
              -- copy rule (down)
              _eOisign =
                  _lhsIisign
              -- copy rule (down)
              _eOpn =
                  _lhsIpn
              -- copy rule (down)
              _eOpos =
                  _lhsIpos
              -- copy rule (down)
              _eOsDef =
                  _lhsIsDef
              ( _eIexpr,_eImorphisms,_eIraw,_eIrnr,_eIrules,_eIsErr,_eIsigns) =
                  (e_ _eOgE _eOisign _eOpn _eOpos _eOrnr _eOsDef )
          in  ( _lhsOexpr,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsigns)))
sem_Expression_K1 :: T_Expression  ->
                     T_Expression 
sem_Expression_K1 e_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsigns :: ([(Concept,Concept)])
              _lhsOexpr :: Expression
              _lhsOraw :: Expression
              _lhsOmorphisms :: Morphisms
              _lhsOsErr :: ([String])
              _eOrnr :: Int
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _eOgE :: GenR
              _eOisign :: ([(Concept,Concept)])
              _eOpn :: String
              _eOpos :: FilePos
              _eOsDef :: Declarations
              _eIexpr :: Expression
              _eImorphisms :: Morphisms
              _eIraw :: Expression
              _eIrnr :: Int
              _eIrules :: Rules
              _eIsErr :: ([String])
              _eIsigns :: ([(Concept,Concept)])
              -- "AGtry.ag"(line 538, column 8)
              _lhsOsigns =
                  _eIsigns
              -- "AGtry.ag"(line 539, column 8)
              _lhsOexpr =
                  Tm _closmor
              -- "AGtry.ag"(line 540, column 8)
              _closmor =
                  let [(a,b)] = take 1 _lhsIisign
                  in Mph (             (name.head.declarations) _eIexpr) _lhsIpos [] (a,b) True (Sgn (             (name.head.declarations) _eIexpr) a b [Trn] "" "" "" [] "Closure" _lhsIpos 0 False)
              -- "AGtry.ag"(line 542, column 8)
              _lhsOraw =
                  K1 _eIraw
              -- "AGtry.ag"(line 590, column 8)
              _lhsOmorphisms =
                  _eImorphisms
              -- "AGtry.ag"(line 761, column 8)
              _lhsOsErr =
                  _eIsErr
              -- "AGtry.ag"(line 1006, column 10)
              _eOrnr =
                  _lhsIrnr+1
              -- "AGtry.ag"(line 1007, column 10)
              _lhsOrnr =
                  _eIrnr
              -- "AGtry.ag"(line 1008, column 10)
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
              -- copy rule (down)
              _eOgE =
                  _lhsIgE
              -- copy rule (down)
              _eOisign =
                  _lhsIisign
              -- copy rule (down)
              _eOpn =
                  _lhsIpn
              -- copy rule (down)
              _eOpos =
                  _lhsIpos
              -- copy rule (down)
              _eOsDef =
                  _lhsIsDef
              ( _eIexpr,_eImorphisms,_eIraw,_eIrnr,_eIrules,_eIsErr,_eIsigns) =
                  (e_ _eOgE _eOisign _eOpn _eOpos _eOrnr _eOsDef )
          in  ( _lhsOexpr,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsigns)))
sem_Expression_Tc :: T_Expression  ->
                     T_Expression 
sem_Expression_Tc c_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsigns :: ([(Concept,Concept)])
              _lhsOexpr :: Expression
              _lhsOraw :: Expression
              _lhsOmorphisms :: Morphisms
              _lhsOsErr :: ([String])
              _cOrnr :: Int
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _cOgE :: GenR
              _cOisign :: ([(Concept,Concept)])
              _cOpn :: String
              _cOpos :: FilePos
              _cOsDef :: Declarations
              _cIexpr :: Expression
              _cImorphisms :: Morphisms
              _cIraw :: Expression
              _cIrnr :: Int
              _cIrules :: Rules
              _cIsErr :: ([String])
              _cIsigns :: ([(Concept,Concept)])
              -- "AGtry.ag"(line 470, column 8)
              _lhsOsigns =
                  _cIsigns
              -- "AGtry.ag"(line 471, column 8)
              _lhsOexpr =
                  Tc _cIexpr
              -- "AGtry.ag"(line 472, column 8)
              _lhsOraw =
                  Tc _cIraw
              -- "AGtry.ag"(line 584, column 8)
              _lhsOmorphisms =
                  _cImorphisms
              -- "AGtry.ag"(line 669, column 8)
              _lhsOsErr =
                  _cIsErr
              -- "AGtry.ag"(line 978, column 10)
              _cOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 979, column 10)
              _lhsOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 980, column 10)
              _lhsOrules =
                  _cIrules
              -- copy rule (down)
              _cOgE =
                  _lhsIgE
              -- copy rule (down)
              _cOisign =
                  _lhsIisign
              -- copy rule (down)
              _cOpn =
                  _lhsIpn
              -- copy rule (down)
              _cOpos =
                  _lhsIpos
              -- copy rule (down)
              _cOsDef =
                  _lhsIsDef
              ( _cIexpr,_cImorphisms,_cIraw,_cIrnr,_cIrules,_cIsErr,_cIsigns) =
                  (c_ _cOgE _cOisign _cOpn _cOpos _cOrnr _cOsDef )
          in  ( _lhsOexpr,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsigns)))
sem_Expression_Tm :: T_Morphism  ->
                     T_Expression 
sem_Expression_Tm m_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsigns :: ([(Concept,Concept)])
              _lhsOexpr :: Expression
              _lhsOraw :: Expression
              _lhsOmorphisms :: Morphisms
              _lhsOsErr :: ([String])
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _mOgE :: GenR
              _mOisign :: ([(Concept,Concept)])
              _mOsDef :: Declarations
              _mIatts :: ([Concept])
              _mIid :: Bool
              _mImorphism :: Morphism
              _mInm :: String
              _mIpos :: FilePos
              _mIraw :: ((Morphism,Declarations))
              _mIsErr :: ([String])
              _mIusedDecls :: Declarations
              _mIyin :: Bool
              -- "AGtry.ag"(line 467, column 8)
              _lhsOsigns =
                  let (m,ss) = _mIraw in if isMph m then (if inline m then map sign ss else map (sign.flp) ss) else [sign m]
              -- "AGtry.ag"(line 468, column 8)
              _lhsOexpr =
                  Tm _mImorphism
              -- "AGtry.ag"(line 469, column 8)
              _lhsOraw =
                  let (m,ss) = _mIraw in Tm m
              -- "AGtry.ag"(line 583, column 8)
              _lhsOmorphisms =
                  [_mImorphism]
              -- "AGtry.ag"(line 668, column 8)
              _lhsOsErr =
                  _mIsErr
              -- "AGtry.ag"(line 976, column 10)
              _lhsOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 977, column 10)
              _lhsOrules =
                  []
              -- copy rule (down)
              _mOgE =
                  _lhsIgE
              -- copy rule (down)
              _mOisign =
                  _lhsIisign
              -- copy rule (down)
              _mOsDef =
                  _lhsIsDef
              ( _mIatts,_mIid,_mImorphism,_mInm,_mIpos,_mIraw,_mIsErr,_mIusedDecls,_mIyin) =
                  (m_ _mOgE _mOisign _mOsDef )
          in  ( _lhsOexpr,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsigns)))
-- Expressions -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         gE                   : GenR
         isign                : [(Concept,Concept)]
         pn                   : String
         pos                  : FilePos
         sDef                 : Declarations 
         septets              : [(Concept, Expressions, [(Concept,Concept)], [Concept], [(Concept,Concept)], Expressions, Concept)]
      chained attribute:
         rnr                  : Int
      synthesized attributes:
         exprs                : Expressions 
         morphisms            : Morphisms
         raw                  : Expressions 
         rules                : Rules 
         sErr                 : [String]
         signss               : [[(Concept,Concept)]]
   alternatives:
      alternative Cons:
         child hd             : Expression 
         child tl             : Expressions 
         visit 0:
            local hsgn        : _
            local septs       : _
      alternative Nil:
-}
-- cata
sem_Expressions :: Expressions  ->
                   T_Expressions 
sem_Expressions list  =
    (Prelude.foldr sem_Expressions_Cons sem_Expressions_Nil (Prelude.map sem_Expression list) )
-- semantic domain
type T_Expressions  = GenR ->
                      ([(Concept,Concept)]) ->
                      String ->
                      FilePos ->
                      Int ->
                      Declarations ->
                      ([(Concept, Expressions, [(Concept,Concept)], [Concept], [(Concept,Concept)], Expressions, Concept)]) ->
                      ( Expressions,Morphisms,Expressions,Int,Rules,([String]),([[(Concept,Concept)]]))
sem_Expressions_Cons :: T_Expression  ->
                        T_Expressions  ->
                        T_Expressions 
sem_Expressions_Cons hd_ tl_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef
       _lhsIseptets ->
         (let _lhsOsignss :: ([[(Concept,Concept)]])
              _hdOisign :: ([(Concept,Concept)])
              _tlOisign :: ([(Concept,Concept)])
              _tlOseptets :: ([(Concept, Expressions, [(Concept,Concept)], [Concept], [(Concept,Concept)], Expressions, Concept)])
              _lhsOraw :: Expressions
              _lhsOmorphisms :: Morphisms
              _lhsOsErr :: ([String])
              _hdOrnr :: Int
              _tlOrnr :: Int
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _lhsOexprs :: Expressions
              _hdOgE :: GenR
              _hdOpn :: String
              _hdOpos :: FilePos
              _hdOsDef :: Declarations
              _tlOgE :: GenR
              _tlOpn :: String
              _tlOpos :: FilePos
              _tlOsDef :: Declarations
              _hdIexpr :: Expression
              _hdImorphisms :: Morphisms
              _hdIraw :: Expression
              _hdIrnr :: Int
              _hdIrules :: Rules
              _hdIsErr :: ([String])
              _hdIsigns :: ([(Concept,Concept)])
              _tlIexprs :: Expressions
              _tlImorphisms :: Morphisms
              _tlIraw :: Expressions
              _tlIrnr :: Int
              _tlIrules :: Rules
              _tlIsErr :: ([String])
              _tlIsignss :: ([[(Concept,Concept)]])
              -- "AGtry.ag"(line 549, column 10)
              _lhsOsignss =
                  _hdIsigns: _tlIsignss
              -- "AGtry.ag"(line 550, column 10)
              _hdOisign =
                  if null _lhsIseptets then _lhsIisign else
                  _hsgn
              -- "AGtry.ag"(line 553, column 10)
              _hsgn =
                  [(a,c)| (a,l,sl,cs,sr,r,b)<- _lhsIseptets, length l==1, c<-cs]
              -- "AGtry.ag"(line 554, column 10)
              _tlOisign =
                  if null _lhsIseptets then _lhsIisign else
                  [(c,b)| (a,l,sl,cs,sr,r,b)<- _lhsIseptets, length l==1, c<-cs]
              -- "AGtry.ag"(line 556, column 10)
              _tlOseptets =
                  _septs
              -- "AGtry.ag"(line 557, column 10)
              _septs =
                  [ (c',tail l,rd[(c',c)|(a,c)<-sl],cs,sr,r,b)
                  | (a,l,sl,cs,sr,r,b)<- _lhsIseptets,length l>1,(a',c')<- _hsgn]
              -- "AGtry.ag"(line 559, column 10)
              _lhsOraw =
                  _hdIraw: _tlIraw
              -- "AGtry.ag"(line 593, column 10)
              _lhsOmorphisms =
                  _hdImorphisms ++ _tlImorphisms
              -- "AGtry.ag"(line 773, column 10)
              _lhsOsErr =
                  _hdIsErr ++ _tlIsErr
              -- "AGtry.ag"(line 1024, column 10)
              _hdOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 1025, column 10)
              _tlOrnr =
                  _hdIrnr
              -- "AGtry.ag"(line 1026, column 10)
              _lhsOrnr =
                  _tlIrnr
              -- "AGtry.ag"(line 1027, column 10)
              _lhsOrules =
                  _hdIrules ++ _tlIrules
              -- "AGtry.ag"(line 1192, column 10)
              _lhsOexprs =
                  _hdIexpr : _tlIexprs
              -- copy rule (down)
              _hdOgE =
                  _lhsIgE
              -- copy rule (down)
              _hdOpn =
                  _lhsIpn
              -- copy rule (down)
              _hdOpos =
                  _lhsIpos
              -- copy rule (down)
              _hdOsDef =
                  _lhsIsDef
              -- copy rule (down)
              _tlOgE =
                  _lhsIgE
              -- copy rule (down)
              _tlOpn =
                  _lhsIpn
              -- copy rule (down)
              _tlOpos =
                  _lhsIpos
              -- copy rule (down)
              _tlOsDef =
                  _lhsIsDef
              ( _hdIexpr,_hdImorphisms,_hdIraw,_hdIrnr,_hdIrules,_hdIsErr,_hdIsigns) =
                  (hd_ _hdOgE _hdOisign _hdOpn _hdOpos _hdOrnr _hdOsDef )
              ( _tlIexprs,_tlImorphisms,_tlIraw,_tlIrnr,_tlIrules,_tlIsErr,_tlIsignss) =
                  (tl_ _tlOgE _tlOisign _tlOpn _tlOpos _tlOrnr _tlOsDef _tlOseptets )
          in  ( _lhsOexprs,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsignss)))
sem_Expressions_Nil :: T_Expressions 
sem_Expressions_Nil  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIpn
       _lhsIpos
       _lhsIrnr
       _lhsIsDef
       _lhsIseptets ->
         (let _lhsOsignss :: ([[(Concept,Concept)]])
              _lhsOraw :: Expressions
              _lhsOmorphisms :: Morphisms
              _lhsOsErr :: ([String])
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _lhsOexprs :: Expressions
              -- "AGtry.ag"(line 560, column 10)
              _lhsOsignss =
                  []
              -- "AGtry.ag"(line 561, column 10)
              _lhsOraw =
                  []
              -- "AGtry.ag"(line 594, column 10)
              _lhsOmorphisms =
                  []
              -- "AGtry.ag"(line 779, column 10)
              _lhsOsErr =
                  []
              -- "AGtry.ag"(line 1028, column 10)
              _lhsOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 1029, column 10)
              _lhsOrules =
                  []
              -- "AGtry.ag"(line 1193, column 10)
              _lhsOexprs =
                  []
          in  ( _lhsOexprs,_lhsOmorphisms,_lhsOraw,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsignss)))
-- Gen ---------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         gE                   : GenR
         pn                   : String
      chained attribute:
         rnr                  : Int
      synthesized attributes:
         gen                  : Gen 
         rules                : Rules 
   alternatives:
      alternative G:
         child pos            : {FilePos}
         child genus          : Concept 
         child spec           : Concept 
-}
-- cata
sem_Gen :: Gen  ->
           T_Gen 
sem_Gen (G _pos _genus _spec )  =
    (sem_Gen_G _pos (sem_Concept _genus ) (sem_Concept _spec ) )
-- semantic domain
type T_Gen  = GenR ->
              String ->
              Int ->
              ( Gen,Int,Rules)
sem_Gen_G :: FilePos ->
             T_Concept  ->
             T_Concept  ->
             T_Gen 
sem_Gen_G pos_ genus_ spec_  =
    (\ _lhsIgE
       _lhsIpn
       _lhsIrnr ->
         (let _lhsOrnr :: Int
              _lhsOrules :: Rules
              _lhsOgen :: Gen
              _genusOgE :: GenR
              _specOgE :: GenR
              _genusIconcept :: Concept
              _genusInm :: String
              _specIconcept :: Concept
              _specInm :: String
              -- "AGtry.ag"(line 1058, column 10)
              _lhsOrnr =
                  _lhsIrnr+1
              -- "AGtry.ag"(line 1059, column 10)
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
              -- "AGtry.ag"(line 1203, column 7)
              _lhsOgen =
                  G pos_ _genusIconcept _specIconcept
              -- copy rule (down)
              _genusOgE =
                  _lhsIgE
              -- copy rule (down)
              _specOgE =
                  _lhsIgE
              ( _genusIconcept,_genusInm) =
                  (genus_ _genusOgE )
              ( _specIconcept,_specInm) =
                  (spec_ _specOgE )
          in  ( _lhsOgen,_lhsOrnr,_lhsOrules)))
-- Gens --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         gE                   : GenR
         pn                   : String
      chained attribute:
         rnr                  : Int
      synthesized attributes:
         list                 : Gens 
         rules                : Rules 
   alternatives:
      alternative Cons:
         child hd             : Gen 
         child tl             : Gens 
      alternative Nil:
-}
-- cata
sem_Gens :: Gens  ->
            T_Gens 
sem_Gens list  =
    (Prelude.foldr sem_Gens_Cons sem_Gens_Nil (Prelude.map sem_Gen list) )
-- semantic domain
type T_Gens  = GenR ->
               String ->
               Int ->
               ( Gens,Int,Rules)
sem_Gens_Cons :: T_Gen  ->
                 T_Gens  ->
                 T_Gens 
sem_Gens_Cons hd_ tl_  =
    (\ _lhsIgE
       _lhsIpn
       _lhsIrnr ->
         (let _hdOrnr :: Int
              _tlOrnr :: Int
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _lhsOlist :: Gens
              _hdOgE :: GenR
              _hdOpn :: String
              _tlOgE :: GenR
              _tlOpn :: String
              _hdIgen :: Gen
              _hdIrnr :: Int
              _hdIrules :: Rules
              _tlIlist :: Gens
              _tlIrnr :: Int
              _tlIrules :: Rules
              -- "AGtry.ag"(line 1050, column 10)
              _hdOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 1051, column 10)
              _tlOrnr =
                  _hdIrnr
              -- "AGtry.ag"(line 1052, column 10)
              _lhsOrnr =
                  _tlIrnr
              -- "AGtry.ag"(line 1053, column 10)
              _lhsOrules =
                  _hdIrules ++ _tlIrules
              -- "AGtry.ag"(line 1198, column 10)
              _lhsOlist =
                  _hdIgen : _tlIlist
              -- copy rule (down)
              _hdOgE =
                  _lhsIgE
              -- copy rule (down)
              _hdOpn =
                  _lhsIpn
              -- copy rule (down)
              _tlOgE =
                  _lhsIgE
              -- copy rule (down)
              _tlOpn =
                  _lhsIpn
              ( _hdIgen,_hdIrnr,_hdIrules) =
                  (hd_ _hdOgE _hdOpn _hdOrnr )
              ( _tlIlist,_tlIrnr,_tlIrules) =
                  (tl_ _tlOgE _tlOpn _tlOrnr )
          in  ( _lhsOlist,_lhsOrnr,_lhsOrules)))
sem_Gens_Nil :: T_Gens 
sem_Gens_Nil  =
    (\ _lhsIgE
       _lhsIpn
       _lhsIrnr ->
         (let _lhsOrnr :: Int
              _lhsOrules :: Rules
              _lhsOlist :: Gens
              -- "AGtry.ag"(line 1054, column 10)
              _lhsOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 1055, column 10)
              _lhsOrules =
                  []
              -- "AGtry.ag"(line 1199, column 10)
              _lhsOlist =
                  []
          in  ( _lhsOlist,_lhsOrnr,_lhsOrules)))
-- KeyDef ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         gE                   : GenR
         sDef                 : Declarations 
      chained attribute:
         rnr                  : Int
      synthesized attributes:
         exprs                : Expressions 
         kd                   : KeyDef 
         rules                : Rules 
         sErr                 : [String]
   alternatives:
      alternative Kd:
         child pos            : {FilePos}
         child lbl            : {String}
         child ctx            : Expression 
         child ats            : ObjectDefs 
-}
-- cata
sem_KeyDef :: KeyDef  ->
              T_KeyDef 
sem_KeyDef (Kd _pos _lbl _ctx _ats )  =
    (sem_KeyDef_Kd _pos _lbl (sem_Expression _ctx ) (sem_ObjectDefs _ats ) )
-- semantic domain
type T_KeyDef  = GenR ->
                 Int ->
                 Declarations ->
                 ( Expressions,KeyDef,Int,Rules,([String]))
sem_KeyDef_Kd :: FilePos ->
                 String ->
                 T_Expression  ->
                 T_ObjectDefs  ->
                 T_KeyDef 
sem_KeyDef_Kd pos_ lbl_ ctx_ ats_  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr :: ([String])
              _atsOrnr :: Int
              _ctxOrnr :: Int
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _lhsOkd :: KeyDef
              _lhsOexprs :: Expressions
              _ctxOpn :: String
              _ctxOisign :: ([(Concept,Concept)])
              _ctxOpos :: FilePos
              _atsOiConcs :: ([Concept])
              _atsOiConc :: Concept
              _ctxOgE :: GenR
              _ctxOsDef :: Declarations
              _atsOgE :: GenR
              _atsOsDef :: Declarations
              _ctxIexpr :: Expression
              _ctxImorphisms :: Morphisms
              _ctxIraw :: Expression
              _ctxIrnr :: Int
              _ctxIrules :: Rules
              _ctxIsErr :: ([String])
              _ctxIsigns :: ([(Concept,Concept)])
              _atsIobjDefs :: ObjectDefs
              _atsIrnr :: Int
              _atsIrules :: Rules
              _atsIsErr :: ([String])
              _atsIsources :: ([Concept])
              -- "AGtry.ag"(line 807, column 8)
              _lhsOsErr =
                  _atsIsErr
              -- "AGtry.ag"(line 949, column 10)
              _atsOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 950, column 10)
              _ctxOrnr =
                  _atsIrnr
              -- "AGtry.ag"(line 951, column 10)
              _lhsOrnr =
                  _ctxIrnr
              -- "AGtry.ag"(line 952, column 10)
              _lhsOrules =
                  _atsIrules ++ _ctxIrules
              -- "AGtry.ag"(line 1122, column 10)
              _lhsOkd =
                  Kd pos_ lbl_ _ctxIexpr _atsIobjDefs
              -- "AGtry.ag"(line 1123, column 10)
              _lhsOexprs =
                  [ expr | Obj nm pos expr ats <- _atsIobjDefs]
              -- "AGtry.ag"(line 1124, column 10)
              _ctxOpn =
                  ""
              -- "AGtry.ag"(line 1125, column 10)
              _ctxOisign =
                  _ctxIsigns
              -- "AGtry.ag"(line 1126, column 10)
              _ctxOpos =
                  pos_
              -- "AGtry.ag"(line 1140, column 10)
              _atsOiConcs =
                  rd (map snd _ctxIsigns ++ _atsIsources)>-[Anything]
              -- "AGtry.ag"(line 1141, column 10)
              _atsOiConc =
                  head (rd (map snd _ctxIsigns))
              -- copy rule (down)
              _ctxOgE =
                  _lhsIgE
              -- copy rule (down)
              _ctxOsDef =
                  _lhsIsDef
              -- copy rule (down)
              _atsOgE =
                  _lhsIgE
              -- copy rule (down)
              _atsOsDef =
                  _lhsIsDef
              ( _ctxIexpr,_ctxImorphisms,_ctxIraw,_ctxIrnr,_ctxIrules,_ctxIsErr,_ctxIsigns) =
                  (ctx_ _ctxOgE _ctxOisign _ctxOpn _ctxOpos _ctxOrnr _ctxOsDef )
              ( _atsIobjDefs,_atsIrnr,_atsIrules,_atsIsErr,_atsIsources) =
                  (ats_ _atsOgE _atsOiConc _atsOiConcs _atsOrnr _atsOsDef )
          in  ( _lhsOexprs,_lhsOkd,_lhsOrnr,_lhsOrules,_lhsOsErr)))
-- KeyDefs -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         gE                   : GenR
         sDef                 : Declarations 
      chained attribute:
         rnr                  : Int
      synthesized attributes:
         exprs                : Expressions 
         keyDefs              : KeyDefs 
         rules                : Rules 
         sErr                 : [String]
   alternatives:
      alternative Cons:
         child hd             : KeyDef 
         child tl             : KeyDefs 
      alternative Nil:
-}
-- cata
sem_KeyDefs :: KeyDefs  ->
               T_KeyDefs 
sem_KeyDefs list  =
    (Prelude.foldr sem_KeyDefs_Cons sem_KeyDefs_Nil (Prelude.map sem_KeyDef list) )
-- semantic domain
type T_KeyDefs  = GenR ->
                  Int ->
                  Declarations ->
                  ( Expressions,KeyDefs,Int,Rules,([String]))
sem_KeyDefs_Cons :: T_KeyDef  ->
                    T_KeyDefs  ->
                    T_KeyDefs 
sem_KeyDefs_Cons hd_ tl_  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr :: ([String])
              _hdOrnr :: Int
              _tlOrnr :: Int
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _lhsOkeyDefs :: KeyDefs
              _lhsOexprs :: Expressions
              _hdOgE :: GenR
              _hdOsDef :: Declarations
              _tlOgE :: GenR
              _tlOsDef :: Declarations
              _hdIexprs :: Expressions
              _hdIkd :: KeyDef
              _hdIrnr :: Int
              _hdIrules :: Rules
              _hdIsErr :: ([String])
              _tlIexprs :: Expressions
              _tlIkeyDefs :: KeyDefs
              _tlIrnr :: Int
              _tlIrules :: Rules
              _tlIsErr :: ([String])
              -- "AGtry.ag"(line 804, column 10)
              _lhsOsErr =
                  _hdIsErr ++ _tlIsErr
              -- "AGtry.ag"(line 941, column 10)
              _hdOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 942, column 10)
              _tlOrnr =
                  _hdIrnr
              -- "AGtry.ag"(line 943, column 10)
              _lhsOrnr =
                  _tlIrnr
              -- "AGtry.ag"(line 944, column 10)
              _lhsOrules =
                  _hdIrules ++ _tlIrules
              -- "AGtry.ag"(line 1130, column 10)
              _lhsOkeyDefs =
                  _hdIkd : _tlIkeyDefs
              -- "AGtry.ag"(line 1131, column 10)
              _lhsOexprs =
                  _hdIexprs ++ _tlIexprs
              -- copy rule (down)
              _hdOgE =
                  _lhsIgE
              -- copy rule (down)
              _hdOsDef =
                  _lhsIsDef
              -- copy rule (down)
              _tlOgE =
                  _lhsIgE
              -- copy rule (down)
              _tlOsDef =
                  _lhsIsDef
              ( _hdIexprs,_hdIkd,_hdIrnr,_hdIrules,_hdIsErr) =
                  (hd_ _hdOgE _hdOrnr _hdOsDef )
              ( _tlIexprs,_tlIkeyDefs,_tlIrnr,_tlIrules,_tlIsErr) =
                  (tl_ _tlOgE _tlOrnr _tlOsDef )
          in  ( _lhsOexprs,_lhsOkeyDefs,_lhsOrnr,_lhsOrules,_lhsOsErr)))
sem_KeyDefs_Nil :: T_KeyDefs 
sem_KeyDefs_Nil  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr :: ([String])
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _lhsOkeyDefs :: KeyDefs
              _lhsOexprs :: Expressions
              -- "AGtry.ag"(line 805, column 10)
              _lhsOsErr =
                  []
              -- "AGtry.ag"(line 945, column 10)
              _lhsOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 946, column 10)
              _lhsOrules =
                  []
              -- "AGtry.ag"(line 1132, column 10)
              _lhsOkeyDefs =
                  []
              -- "AGtry.ag"(line 1133, column 10)
              _lhsOexprs =
                  []
          in  ( _lhsOexprs,_lhsOkeyDefs,_lhsOrnr,_lhsOrules,_lhsOsErr)))
-- Morphism ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         gE                   : GenR
         isign                : [(Concept,Concept)]
         sDef                 : Declarations 
      synthesized attributes:
         atts                 : [Concept]
         id                   : Bool
         morphism             : Morphism 
         nm                   : String
         pos                  : FilePos
         raw                  : (Morphism,Declarations)
         sErr                 : [String]
         usedDecls            : Declarations 
         yin                  : Bool
   alternatives:
      alternative I:
         child atts           : {[Concept]}
         child g              : Concept 
         child s              : Concept 
         child yin            : {Bool}
         visit 0:
            local rraw        : _
            local ats         : _
      alternative Mph:
         child nm             : {String}
         child pos            : {FilePos}
         child atts           : {[Concept]}
         child sgn            : {(Concept,Concept)}
         child yin            : {Bool}
         child decl           : Declaration 
         visit 0:
            local s           : _
            local m           : _
            local rel         : _
            local ss          : _
            local ats         : _
      alternative V:
         child atts           : {[Concept]}
         child sgn            : {(Concept,Concept)}
         visit 0:
            local rraw        : _
            local ats         : _
-}
-- cata
sem_Morphism :: Morphism  ->
                T_Morphism 
sem_Morphism (I _atts _g _s _yin )  =
    (sem_Morphism_I _atts (sem_Concept _g ) (sem_Concept _s ) _yin )
sem_Morphism (Mph _nm _pos _atts _sgn _yin _decl )  =
    (sem_Morphism_Mph _nm _pos _atts _sgn _yin (sem_Declaration _decl ) )
sem_Morphism (V _atts _sgn )  =
    (sem_Morphism_V _atts _sgn )
-- semantic domain
type T_Morphism  = GenR ->
                   ([(Concept,Concept)]) ->
                   Declarations ->
                   ( ([Concept]),Bool,Morphism,String,FilePos,((Morphism,Declarations)),([String]),Declarations,Bool)
sem_Morphism_I :: ([Concept]) ->
                  T_Concept  ->
                  T_Concept  ->
                  Bool ->
                  T_Morphism 
sem_Morphism_I atts_ g_ s_ yin_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIsDef ->
         (let _lhsOsErr :: ([String])
              _lhsOusedDecls :: Declarations
              _lhsOraw :: ((Morphism,Declarations))
              _lhsOmorphism :: Morphism
              _lhsOnm :: String
              _lhsOpos :: FilePos
              _lhsOatts :: ([Concept])
              _lhsOyin :: Bool
              _lhsOid :: Bool
              _gOgE :: GenR
              _sOgE :: GenR
              _gIconcept :: Concept
              _gInm :: String
              _sIconcept :: Concept
              _sInm :: String
              -- "AGtry.ag"(line 801, column 9)
              _lhsOsErr =
                  []
              -- "AGtry.ag"(line 1258, column 9)
              _lhsOusedDecls =
                  declarations [ _rraw ]
              -- "AGtry.ag"(line 1259, column 9)
              _lhsOraw =
                  ( _rraw, [])
              -- "AGtry.ag"(line 1260, column 9)
              _rraw =
                  if null _ats      then I [] Anything Anything True  else
                  if length _ats==1 then I _ats (head _ats) (head _ats) True              else
                  error ("Contact your dealer:\nADL allows only one concept in I["++show atts_++"].")
              -- "AGtry.ag"(line 1263, column 9)
              _lhsOmorphism =
                  let (s,t) = if null _lhsIisign && null _ats then error ("Fatal: null @lhs.isign in I lhs.morphism!") else
                              head (_lhsIisign++[(head _ats,last _ats)])
                      s' = if s==Anything then t else s; t'= if t==Anything then s else t
                      is = ids _lhsIgE s' t'
                  in if null is then I _ats s' t' True else head is
              -- "AGtry.ag"(line 1268, column 9)
              _lhsOnm =
                  "I"
              -- "AGtry.ag"(line 1269, column 9)
              _lhsOpos =
                  posNone
              -- "AGtry.ag"(line 1270, column 9)
              _lhsOatts =
                  _ats
              -- "AGtry.ag"(line 1271, column 9)
              _ats =
                  rd ([C a _lhsIgE as|C a _ as<- atts_]++[S|S<- atts_])
              -- "AGtry.ag"(line 1272, column 9)
              _lhsOyin =
                  let I ats g s yin = _rraw in yin
              -- "AGtry.ag"(line 1273, column 9)
              _lhsOid =
                  True
              -- copy rule (down)
              _gOgE =
                  _lhsIgE
              -- copy rule (down)
              _sOgE =
                  _lhsIgE
              ( _gIconcept,_gInm) =
                  (g_ _gOgE )
              ( _sIconcept,_sInm) =
                  (s_ _sOgE )
          in  ( _lhsOatts,_lhsOid,_lhsOmorphism,_lhsOnm,_lhsOpos,_lhsOraw,_lhsOsErr,_lhsOusedDecls,_lhsOyin)))
sem_Morphism_Mph :: String ->
                    FilePos ->
                    ([Concept]) ->
                    ((Concept,Concept)) ->
                    Bool ->
                    T_Declaration  ->
                    T_Morphism 
sem_Morphism_Mph nm_ pos_ atts_ sgn_ yin_ decl_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIsDef ->
         (let _lhsOsErr :: ([String])
              _declOrnr :: Int
              _lhsOmorphism :: Morphism
              _lhsOusedDecls :: Declarations
              _lhsOraw :: ((Morphism,Declarations))
              _lhsOnm :: String
              _lhsOpos :: FilePos
              _lhsOatts :: ([Concept])
              _lhsOyin :: Bool
              _lhsOid :: Bool
              _declOgE :: GenR
              _declOsDef :: Declarations
              _declIdeclaration :: Declaration
              _declInm :: String
              _declIrawDecl :: Declaration
              _declIrnr :: Int
              _declIrules :: Rules
              _declIsErr :: ([String])
              -- "AGtry.ag"(line 796, column 9)
              _lhsOsErr =
                  [ "3 in "++show pos_++
                    "\n   Relation " ++ show (Mph nm_ pos_ atts_ (Anything, Anything) yin_ (Isn Anything Anything)) ++
                    (if null atts_ then "" else show (rd atts_)) ++
                    " is not declared.\n"
                  | null _ss]
              -- "AGtry.ag"(line 1071, column 10)
              _declOrnr =
                  -999999
              -- "AGtry.ag"(line 1226, column 9)
              _lhsOmorphism =
                  Mph nm_ pos_ _ats _s yin_
                   (if null _rel then error("Fatal (module AGtry): null @rel in "++ nm_++" on "++show pos_++"\n@lhs.isign = "++ show _lhsIisign++"\n@ss       = "++ show _ss++"\n@s          = "++ show _s++"\n"++ gEtabG _lhsIgE (rd ([fst _s,snd _s]++concs _ss))) else
                    head _rel)
              -- "AGtry.ag"(line 1229, column 9)
              _s =
                  if null _lhsIisign
                  then error("Fatal 2: Empty declaration allocation for "++ nm_ ++" on "++show pos_++":\n  @ss="++chain "\n       " (map show _ss))
                  else head _lhsIisign
              -- "AGtry.ag"(line 1232, column 9)
              _m =
                  if null _rel
                  then error("Fatal 3: Empty declaration allocation for "++show nm_++" on "++show pos_++":\n  @lhs.isign="++show _lhsIisign++":\n  @ss="++chain "\n       " (map show _ss))
                  else if length _rel>1
                  then error("Fatal 6: Ambiguous declaration allocation for "++show nm_++" on "++show pos_++":\n   "++
                                                       chain "\n   " (map show _rel))
                  else head _rel
              -- "AGtry.ag"(line 1239, column 9)
              _rel =
                  irredD _lhsIgE [s| s <- _ss, nm_==name s, (a,b) <- _lhsIisign
                                   , if yin_ then test s (a,b) else test s (b,a)]
                  where test s (a,b) = (source s `_lhsIgE` a) &&
                                       (target s `_lhsIgE` b)
              -- "AGtry.ag"(line 1244, column 9)
              _lhsOusedDecls =
                  _ss
              -- "AGtry.ag"(line 1245, column 9)
              _ss =
                  let ss = [ s | s <- _lhsIsDef, name s == nm_] in
                  if null atts_ then rd ss else
                  [ s | s <- ss, if yin_ then source s == head atts_ && target s == last atts_ else source s == last atts_ && target s == head atts_]
              -- "AGtry.ag"(line 1248, column 9)
              _lhsOraw =
                  let err=error "illegal reference to 'raw' in semantics of Mph of Morphism" in
                  if null _ats
                  then (Mph nm_ pos_ _ats (Anything,Anything) yin_ (Sgn nm_ Anything Anything [] "" "" "" [] "" err 0 True), _ss)
                  else (Mph nm_ pos_ _ats (if yin_ then (head _ats,last _ats) else (last _ats,head _ats)) yin_ (Sgn nm_ (head _ats) (last _ats) [] "" "" "" [] "" err 0 True), _ss)
              -- "AGtry.ag"(line 1252, column 9)
              _ats =
                  rd ([C a _lhsIgE as|C a _ as<- atts_]++[S|S<- atts_])
              -- "AGtry.ag"(line 1253, column 9)
              _lhsOnm =
                  nm_
              -- "AGtry.ag"(line 1254, column 9)
              _lhsOpos =
                  pos_
              -- "AGtry.ag"(line 1255, column 9)
              _lhsOatts =
                  _ats
              -- "AGtry.ag"(line 1256, column 9)
              _lhsOyin =
                  yin_
              -- "AGtry.ag"(line 1257, column 9)
              _lhsOid =
                  False
              -- copy rule (down)
              _declOgE =
                  _lhsIgE
              -- copy rule (down)
              _declOsDef =
                  _lhsIsDef
              ( _declIdeclaration,_declInm,_declIrawDecl,_declIrnr,_declIrules,_declIsErr) =
                  (decl_ _declOgE _declOrnr _declOsDef )
          in  ( _lhsOatts,_lhsOid,_lhsOmorphism,_lhsOnm,_lhsOpos,_lhsOraw,_lhsOsErr,_lhsOusedDecls,_lhsOyin)))
sem_Morphism_V :: ([Concept]) ->
                  ((Concept,Concept)) ->
                  T_Morphism 
sem_Morphism_V atts_ sgn_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIsDef ->
         (let _lhsOsErr :: ([String])
              _lhsOusedDecls :: Declarations
              _lhsOraw :: ((Morphism,Declarations))
              _lhsOmorphism :: Morphism
              _lhsOnm :: String
              _lhsOpos :: FilePos
              _lhsOatts :: ([Concept])
              _lhsOid :: Bool
              _lhsOyin :: Bool
              -- "AGtry.ag"(line 802, column 9)
              _lhsOsErr =
                  []
              -- "AGtry.ag"(line 1274, column 9)
              _lhsOusedDecls =
                  declarations [ _rraw ]
              -- "AGtry.ag"(line 1275, column 9)
              _lhsOraw =
                  ( _rraw, [])
              -- "AGtry.ag"(line 1276, column 9)
              _rraw =
                  V atts_ sgn_
              -- "AGtry.ag"(line 1277, column 9)
              _lhsOmorphism =
                  let (s,t) = if null _lhsIisign then error ("Fatal: null @lhs.isign in V lhs.morphism! "++(show atts_)) else
                              head _lhsIisign
                  in V _ats (s,t)
              -- "AGtry.ag"(line 1280, column 9)
              _lhsOnm =
                  "V"
              -- "AGtry.ag"(line 1281, column 9)
              _lhsOpos =
                  posNone
              -- "AGtry.ag"(line 1282, column 9)
              _lhsOatts =
                  _ats
              -- "AGtry.ag"(line 1283, column 9)
              _ats =
                  rd ([C a _lhsIgE as|C a _ as<- atts_]++[S|S<- atts_])
              -- "AGtry.ag"(line 1284, column 9)
              _lhsOid =
                  True
              -- "AGtry.ag"(line 1285, column 9)
              _lhsOyin =
                  True
          in  ( _lhsOatts,_lhsOid,_lhsOmorphism,_lhsOnm,_lhsOpos,_lhsOraw,_lhsOsErr,_lhsOusedDecls,_lhsOyin)))
-- Morphisms ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         gE                   : GenR
         isign                : [(Concept,Concept)]
         sDef                 : Declarations 
      synthesized attributes:
         morphisms            : Morphisms 
         raw                  : [(Morphism,Declarations)]
         usedDecls            : Declarations 
   alternatives:
      alternative Cons:
         child hd             : Morphism 
         child tl             : Morphisms 
      alternative Nil:
-}
-- cata
sem_Morphisms :: Morphisms  ->
                 T_Morphisms 
sem_Morphisms list  =
    (Prelude.foldr sem_Morphisms_Cons sem_Morphisms_Nil (Prelude.map sem_Morphism list) )
-- semantic domain
type T_Morphisms  = GenR ->
                    ([(Concept,Concept)]) ->
                    Declarations ->
                    ( Morphisms,([(Morphism,Declarations)]),Declarations)
sem_Morphisms_Cons :: T_Morphism  ->
                      T_Morphisms  ->
                      T_Morphisms 
sem_Morphisms_Cons hd_ tl_  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIsDef ->
         (let _lhsOusedDecls :: Declarations
              _lhsOmorphisms :: Morphisms
              _lhsOraw :: ([(Morphism,Declarations)])
              _hdOisign :: ([(Concept,Concept)])
              _hdOgE :: GenR
              _hdOsDef :: Declarations
              _tlOgE :: GenR
              _tlOisign :: ([(Concept,Concept)])
              _tlOsDef :: Declarations
              _hdIatts :: ([Concept])
              _hdIid :: Bool
              _hdImorphism :: Morphism
              _hdInm :: String
              _hdIpos :: FilePos
              _hdIraw :: ((Morphism,Declarations))
              _hdIsErr :: ([String])
              _hdIusedDecls :: Declarations
              _hdIyin :: Bool
              _tlImorphisms :: Morphisms
              _tlIraw :: ([(Morphism,Declarations)])
              _tlIusedDecls :: Declarations
              -- "AGtry.ag"(line 564, column 10)
              _lhsOusedDecls =
                  _hdIusedDecls ++ _tlIusedDecls
              -- "AGtry.ag"(line 1214, column 10)
              _lhsOmorphisms =
                  _hdImorphism : _tlImorphisms
              -- "AGtry.ag"(line 1215, column 10)
              _lhsOraw =
                  _hdIraw : _tlIraw
              -- "AGtry.ag"(line 1216, column 10)
              _hdOisign =
                  take 1 (
                   [ (src, if yin then target s else source s)
                   | (src,_)<- _lhsIisign, (Mph nm pos ats _ yin _,ss) <- [_hdIraw], s<-ss, (if yin then source s else target s) `_lhsIgE` src]++
                   _lhsIisign)
              -- copy rule (down)
              _hdOgE =
                  _lhsIgE
              -- copy rule (down)
              _hdOsDef =
                  _lhsIsDef
              -- copy rule (down)
              _tlOgE =
                  _lhsIgE
              -- copy rule (down)
              _tlOisign =
                  _lhsIisign
              -- copy rule (down)
              _tlOsDef =
                  _lhsIsDef
              ( _hdIatts,_hdIid,_hdImorphism,_hdInm,_hdIpos,_hdIraw,_hdIsErr,_hdIusedDecls,_hdIyin) =
                  (hd_ _hdOgE _hdOisign _hdOsDef )
              ( _tlImorphisms,_tlIraw,_tlIusedDecls) =
                  (tl_ _tlOgE _tlOisign _tlOsDef )
          in  ( _lhsOmorphisms,_lhsOraw,_lhsOusedDecls)))
sem_Morphisms_Nil :: T_Morphisms 
sem_Morphisms_Nil  =
    (\ _lhsIgE
       _lhsIisign
       _lhsIsDef ->
         (let _lhsOusedDecls :: Declarations
              _lhsOraw :: ([(Morphism,Declarations)])
              _lhsOmorphisms :: Morphisms
              -- "AGtry.ag"(line 565, column 10)
              _lhsOusedDecls =
                  []
              -- "AGtry.ag"(line 1220, column 10)
              _lhsOraw =
                  []
              -- "AGtry.ag"(line 1221, column 10)
              _lhsOmorphisms =
                  []
          in  ( _lhsOmorphisms,_lhsOraw,_lhsOusedDecls)))
-- ObjectDef ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         gE                   : GenR
         iConc                : Concept 
         iConcs               : [Concept]
         sDef                 : Declarations 
      chained attribute:
         rnr                  : Int
      synthesized attributes:
         ats                  : ObjectDefs 
         nm                   : String
         odef                 : ObjectDef 
         pos                  : FilePos
         rules                : Rules 
         sConcs               : [Concept]
         sErr                 : [String]
   alternatives:
      alternative Obj:
         child nm             : {String}
         child pos            : {FilePos}
         child ctx            : Expression 
         child ats            : ObjectDefs 
         visit 0:
            local concpt      : _
            local signs       : _
-}
-- cata
sem_ObjectDef :: ObjectDef  ->
                 T_ObjectDef 
sem_ObjectDef (Obj _nm _pos _ctx _ats )  =
    (sem_ObjectDef_Obj _nm _pos (sem_Expression _ctx ) (sem_ObjectDefs _ats ) )
-- semantic domain
type T_ObjectDef  = GenR ->
                    Concept ->
                    ([Concept]) ->
                    Int ->
                    Declarations ->
                    ( ObjectDefs,String,ObjectDef,FilePos,Int,Rules,([Concept]),([String]))
sem_ObjectDef_Obj :: String ->
                     FilePos ->
                     T_Expression  ->
                     T_ObjectDefs  ->
                     T_ObjectDef 
sem_ObjectDef_Obj nm_ pos_ ctx_ ats_  =
    (\ _lhsIgE
       _lhsIiConc
       _lhsIiConcs
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr :: ([String])
              _atsOrnr :: Int
              _ctxOrnr :: Int
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _lhsOodef :: ObjectDef
              _lhsOats :: ObjectDefs
              _lhsOsConcs :: ([Concept])
              _lhsOpos :: FilePos
              _lhsOnm :: String
              _atsOiConcs :: ([Concept])
              _atsOiConc :: Concept
              _ctxOisign :: ([(Concept,Concept)])
              _ctxOpn :: String
              _ctxOpos :: FilePos
              _ctxOgE :: GenR
              _ctxOsDef :: Declarations
              _atsOgE :: GenR
              _atsOsDef :: Declarations
              _ctxIexpr :: Expression
              _ctxImorphisms :: Morphisms
              _ctxIraw :: Expression
              _ctxIrnr :: Int
              _ctxIrules :: Rules
              _ctxIsErr :: ([String])
              _ctxIsigns :: ([(Concept,Concept)])
              _atsIobjDefs :: ObjectDefs
              _atsIrnr :: Int
              _atsIrules :: Rules
              _atsIsErr :: ([String])
              _atsIsources :: ([Concept])
              -- "AGtry.ag"(line 810, column 9)
              _lhsOsErr =
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
              -- "AGtry.ag"(line 935, column 10)
              _atsOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 936, column 10)
              _ctxOrnr =
                  _atsIrnr
              -- "AGtry.ag"(line 937, column 10)
              _lhsOrnr =
                  _ctxIrnr
              -- "AGtry.ag"(line 938, column 10)
              _lhsOrules =
                  _atsIrules ++ _ctxIrules
              -- "AGtry.ag"(line 1144, column 10)
              _lhsOodef =
                  if and [sgn==(Anything,Anything)|sgn<- _ctxIsigns]
                  then Obj nm_ pos_ (Tm (mIs (C nm_ _lhsIgE []))) _atsIobjDefs
                  else Obj nm_ pos_ _ctxIexpr _atsIobjDefs
              -- "AGtry.ag"(line 1147, column 10)
              _lhsOats =
                  _atsIobjDefs
              -- "AGtry.ag"(line 1148, column 10)
              _lhsOsConcs =
                  rd (map fst _ctxIsigns)
              -- "AGtry.ag"(line 1149, column 10)
              _lhsOpos =
                  pos_
              -- "AGtry.ag"(line 1150, column 10)
              _lhsOnm =
                  nm_
              -- "AGtry.ag"(line 1151, column 10)
              _atsOiConcs =
                  irredC _lhsIgE (map snd _signs)
              -- "AGtry.ag"(line 1152, column 10)
              _atsOiConc =
                  if null _ctxIsigns then C nm_ _lhsIgE [] else _concpt
              -- "AGtry.ag"(line 1153, column 10)
              _ctxOisign =
                  _signs
              -- "AGtry.ag"(line 1154, column 10)
              _concpt =
                  if null _signs
                  then error ("Fatal (module AGtry): no target for '"++showADL _ctxIexpr++"'."++
                              "\n@pos       : "++show pos_++
                              "\n@nm        : "++show nm_++
                              "\n@ctx.signs : "++show _ctxIsigns
                             )
                  else head (map snd _signs)
              -- "AGtry.ag"(line 1162, column 10)
              _signs =
                  irredT' _lhsIgE
                     ([( lubb _lhsIgE _lhsIiConc s, t)
                      | (s,t) <- _ctxIsigns, not (s==Anything && t==Anything)
                      , ordd _lhsIgE _lhsIiConc s]++
                      [(C nm_ _lhsIgE [], C nm_ _lhsIgE [])| and [sgn==(Anything,Anything)|sgn<- _ctxIsigns]])
              -- "AGtry.ag"(line 1169, column 10)
              _ctxOpn =
                  ""
              -- "AGtry.ag"(line 1170, column 10)
              _ctxOpos =
                  pos_
              -- copy rule (down)
              _ctxOgE =
                  _lhsIgE
              -- copy rule (down)
              _ctxOsDef =
                  _lhsIsDef
              -- copy rule (down)
              _atsOgE =
                  _lhsIgE
              -- copy rule (down)
              _atsOsDef =
                  _lhsIsDef
              ( _ctxIexpr,_ctxImorphisms,_ctxIraw,_ctxIrnr,_ctxIrules,_ctxIsErr,_ctxIsigns) =
                  (ctx_ _ctxOgE _ctxOisign _ctxOpn _ctxOpos _ctxOrnr _ctxOsDef )
              ( _atsIobjDefs,_atsIrnr,_atsIrules,_atsIsErr,_atsIsources) =
                  (ats_ _atsOgE _atsOiConc _atsOiConcs _atsOrnr _atsOsDef )
          in  ( _lhsOats,_lhsOnm,_lhsOodef,_lhsOpos,_lhsOrnr,_lhsOrules,_lhsOsConcs,_lhsOsErr)))
-- ObjectDefs --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         gE                   : GenR
         iConc                : Concept 
         iConcs               : [Concept]
         sDef                 : Declarations 
      chained attribute:
         rnr                  : Int
      synthesized attributes:
         objDefs              : ObjectDefs 
         rules                : Rules 
         sErr                 : [String]
         sources              : [Concept]
   alternatives:
      alternative Cons:
         child hd             : ObjectDef 
         child tl             : ObjectDefs 
         visit 0:
            local signs       : _
      alternative Nil:
-}
-- cata
sem_ObjectDefs :: ObjectDefs  ->
                  T_ObjectDefs 
sem_ObjectDefs list  =
    (Prelude.foldr sem_ObjectDefs_Cons sem_ObjectDefs_Nil (Prelude.map sem_ObjectDef list) )
-- semantic domain
type T_ObjectDefs  = GenR ->
                     Concept ->
                     ([Concept]) ->
                     Int ->
                     Declarations ->
                     ( ObjectDefs,Int,Rules,([String]),([Concept]))
sem_ObjectDefs_Cons :: T_ObjectDef  ->
                       T_ObjectDefs  ->
                       T_ObjectDefs 
sem_ObjectDefs_Cons hd_ tl_  =
    (\ _lhsIgE
       _lhsIiConc
       _lhsIiConcs
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr :: ([String])
              _hdOrnr :: Int
              _tlOrnr :: Int
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _hdOiConcs :: ([Concept])
              _hdOiConc :: Concept
              _lhsOobjDefs :: ObjectDefs
              _lhsOsources :: ([Concept])
              _hdOgE :: GenR
              _hdOsDef :: Declarations
              _tlOgE :: GenR
              _tlOiConc :: Concept
              _tlOiConcs :: ([Concept])
              _tlOsDef :: Declarations
              _hdIats :: ObjectDefs
              _hdInm :: String
              _hdIodef :: ObjectDef
              _hdIpos :: FilePos
              _hdIrnr :: Int
              _hdIrules :: Rules
              _hdIsConcs :: ([Concept])
              _hdIsErr :: ([String])
              _tlIobjDefs :: ObjectDefs
              _tlIrnr :: Int
              _tlIrules :: Rules
              _tlIsErr :: ([String])
              _tlIsources :: ([Concept])
              -- "AGtry.ag"(line 874, column 10)
              _lhsOsErr =
                  _hdIsErr ++ _tlIsErr
              -- "AGtry.ag"(line 927, column 10)
              _hdOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 928, column 10)
              _tlOrnr =
                  _hdIrnr
              -- "AGtry.ag"(line 929, column 10)
              _lhsOrnr =
                  _tlIrnr
              -- "AGtry.ag"(line 930, column 10)
              _lhsOrules =
                  _hdIrules ++ _tlIrules
              -- "AGtry.ag"(line 1137, column 10)
              _hdOiConcs =
                  _lhsIiConcs
              -- "AGtry.ag"(line 1138, column 10)
              _hdOiConc =
                  _lhsIiConc
              -- "AGtry.ag"(line 1184, column 10)
              _lhsOobjDefs =
                  _hdIodef : _tlIobjDefs
              -- "AGtry.ag"(line 1185, column 10)
              _lhsOsources =
                  _signs
              -- "AGtry.ag"(line 1186, column 10)
              _signs =
                  rd [lubb _lhsIgE src' src''|src''<- _tlIsources, src'<- _hdIsConcs, src' `order` src'']
              -- copy rule (down)
              _hdOgE =
                  _lhsIgE
              -- copy rule (down)
              _hdOsDef =
                  _lhsIsDef
              -- copy rule (down)
              _tlOgE =
                  _lhsIgE
              -- copy rule (down)
              _tlOiConc =
                  _lhsIiConc
              -- copy rule (down)
              _tlOiConcs =
                  _lhsIiConcs
              -- copy rule (down)
              _tlOsDef =
                  _lhsIsDef
              ( _hdIats,_hdInm,_hdIodef,_hdIpos,_hdIrnr,_hdIrules,_hdIsConcs,_hdIsErr) =
                  (hd_ _hdOgE _hdOiConc _hdOiConcs _hdOrnr _hdOsDef )
              ( _tlIobjDefs,_tlIrnr,_tlIrules,_tlIsErr,_tlIsources) =
                  (tl_ _tlOgE _tlOiConc _tlOiConcs _tlOrnr _tlOsDef )
          in  ( _lhsOobjDefs,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsources)))
sem_ObjectDefs_Nil :: T_ObjectDefs 
sem_ObjectDefs_Nil  =
    (\ _lhsIgE
       _lhsIiConc
       _lhsIiConcs
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOsErr :: ([String])
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _lhsOobjDefs :: ObjectDefs
              _lhsOsources :: ([Concept])
              -- "AGtry.ag"(line 875, column 10)
              _lhsOsErr =
                  []
              -- "AGtry.ag"(line 931, column 10)
              _lhsOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 932, column 10)
              _lhsOrules =
                  []
              -- "AGtry.ag"(line 1187, column 10)
              _lhsOobjDefs =
                  []
              -- "AGtry.ag"(line 1188, column 10)
              _lhsOsources =
                  [Anything]
          in  ( _lhsOobjDefs,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOsources)))
-- Pairs -------------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : {Paire}
         child tl             : Pairs 
      alternative Nil:
-}
-- cata
sem_Pairs :: Pairs  ->
             T_Pairs 
sem_Pairs list  =
    (Prelude.foldr sem_Pairs_Cons sem_Pairs_Nil list )
-- semantic domain
type T_Pairs  = ( )
sem_Pairs_Cons :: Paire ->
                  T_Pairs  ->
                  T_Pairs 
sem_Pairs_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_Pairs_Nil :: T_Pairs 
sem_Pairs_Nil  =
    (let 
     in  ( ))
-- Pattern -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         gE                   : GenR
         sDef                 : Declarations 
      chained attribute:
         rnr                  : Int
      synthesized attributes:
         conDefs              : [ConceptDef]
         keyDefs              : KeyDefs 
         mGen                 : Gens 
         morphisms            : Morphisms
         patterns             : Patterns 
         rawDecls             : Declarations 
         rules                : Rules 
         sErr                 : [String]
         usedDecls            : Declarations 
   alternatives:
      alternative Pat:
         child nm             : {String}
         child rules          : Rules 
         child gen            : Gens 
         child pms            : Declarations 
         child cs             : ConceptDefs 
         child ks             : KeyDefs 
-}
-- cata
sem_Pattern :: Pattern  ->
               T_Pattern 
sem_Pattern (Pat _nm _rules _gen _pms _cs _ks )  =
    (sem_Pattern_Pat _nm (sem_Rules _rules ) (sem_Gens _gen ) (sem_Declarations _pms ) (sem_ConceptDefs _cs ) (sem_KeyDefs _ks ) )
-- semantic domain
type T_Pattern  = GenR ->
                  Int ->
                  Declarations ->
                  ( ([ConceptDef]),KeyDefs,Gens,Morphisms,Patterns,Declarations,Int,Rules,([String]),Declarations)
sem_Pattern_Pat :: String ->
                   T_Rules  ->
                   T_Gens  ->
                   T_Declarations  ->
                   T_ConceptDefs  ->
                   T_KeyDefs  ->
                   T_Pattern 
sem_Pattern_Pat nm_ rules_ gen_ pms_ cs_ ks_  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOmGen :: Gens
              _lhsOkeyDefs :: KeyDefs
              _lhsOmorphisms :: Morphisms
              _lhsOsErr :: ([String])
              _genOrnr :: Int
              _rulesOrnr :: Int
              _pmsOrnr :: Int
              _ksOrnr :: Int
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _lhsOpatterns :: Patterns
              _lhsOusedDecls :: Declarations
              _lhsOrawDecls :: Declarations
              _lhsOconDefs :: ([ConceptDef])
              _rulesOpn :: String
              _genOpn :: String
              _ksOgE :: GenR
              _rulesOgE :: GenR
              _rulesOsDef :: Declarations
              _genOgE :: GenR
              _pmsOgE :: GenR
              _pmsOsDef :: Declarations
              _ksOsDef :: Declarations
              _rulesIdeclarations :: Declarations
              _rulesImGen :: Gens
              _rulesImorphisms :: Morphisms
              _rulesIrnr :: Int
              _rulesIrules :: Rules
              _rulesIsErr :: ([String])
              _rulesIusedDecls :: Declarations
              _genIlist :: Gens
              _genIrnr :: Int
              _genIrules :: Rules
              _pmsIdeclarations :: Declarations
              _pmsIrawDecls :: Declarations
              _pmsIrnr :: Int
              _pmsIrules :: Rules
              _pmsIsErr :: ([String])
              _csIconDefs :: ([ConceptDef])
              _ksIexprs :: Expressions
              _ksIkeyDefs :: KeyDefs
              _ksIrnr :: Int
              _ksIrules :: Rules
              _ksIsErr :: ([String])
              -- "AGtry.ag"(line 247, column 9)
              _lhsOmGen =
                  rd [G pos g s| G pos g s <- _rulesImGen ++ _genIlist, g/=s]
              -- "AGtry.ag"(line 572, column 9)
              _lhsOkeyDefs =
                  _ksIkeyDefs
              -- "AGtry.ag"(line 578, column 9)
              _lhsOmorphisms =
                  _rulesImorphisms
              -- "AGtry.ag"(line 613, column 9)
              _lhsOsErr =
                  _rulesIsErr ++ _pmsIsErr ++ _ksIsErr
              -- "AGtry.ag"(line 919, column 10)
              _genOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 920, column 10)
              _rulesOrnr =
                  _genIrnr
              -- "AGtry.ag"(line 921, column 10)
              _pmsOrnr =
                  _rulesIrnr
              -- "AGtry.ag"(line 922, column 10)
              _ksOrnr =
                  _pmsIrnr
              -- "AGtry.ag"(line 923, column 10)
              _lhsOrnr =
                  _ksIrnr
              -- "AGtry.ag"(line 924, column 10)
              _lhsOrules =
                  _genIrules ++ _rulesIrules ++ _pmsIrules ++ _ksIrules
              -- "AGtry.ag"(line 1087, column 10)
              _lhsOpatterns =
                  [Pat nm_ _rulesIrules ( _rulesImGen ++ _genIlist)
                       ( _pmsIdeclarations ++ _rulesIdeclarations )
                       _csIconDefs _ksIkeyDefs]
              -- "AGtry.ag"(line 1090, column 10)
              _lhsOusedDecls =
                  _rulesIusedDecls ++declarations _ksIexprs
              -- "AGtry.ag"(line 1091, column 10)
              _lhsOrawDecls =
                  _pmsIrawDecls
              -- "AGtry.ag"(line 1092, column 10)
              _lhsOconDefs =
                  _csIconDefs
              -- "AGtry.ag"(line 1093, column 10)
              _rulesOpn =
                  nm_
              -- "AGtry.ag"(line 1094, column 10)
              _genOpn =
                  nm_
              -- "AGtry.ag"(line 1095, column 10)
              _ksOgE =
                  _lhsIgE
              -- copy rule (down)
              _rulesOgE =
                  _lhsIgE
              -- copy rule (down)
              _rulesOsDef =
                  _lhsIsDef
              -- copy rule (down)
              _genOgE =
                  _lhsIgE
              -- copy rule (down)
              _pmsOgE =
                  _lhsIgE
              -- copy rule (down)
              _pmsOsDef =
                  _lhsIsDef
              -- copy rule (down)
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
{-
   visit 0:
      inherited attributes:
         gE                   : GenR
         sDef                 : Declarations 
      chained attribute:
         rnr                  : Int
      synthesized attributes:
         conDefs              : [ConceptDef]
         keyDefs              : KeyDefs 
         mGen                 : Gens 
         morphisms            : Morphisms
         patterns             : Patterns 
         rawDecls             : Declarations 
         rules                : Rules 
         sErr                 : [String]
         usedDecls            : Declarations 
   alternatives:
      alternative Cons:
         child hd             : Pattern 
         child tl             : Patterns 
      alternative Nil:
-}
-- cata
sem_Patterns :: Patterns  ->
                T_Patterns 
sem_Patterns list  =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list) )
-- semantic domain
type T_Patterns  = GenR ->
                   Int ->
                   Declarations ->
                   ( ([ConceptDef]),KeyDefs,Gens,Morphisms,Patterns,Declarations,Int,Rules,([String]),Declarations)
sem_Patterns_Cons :: T_Pattern  ->
                     T_Patterns  ->
                     T_Patterns 
sem_Patterns_Cons hd_ tl_  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOmGen :: Gens
              _lhsOkeyDefs :: KeyDefs
              _lhsOmorphisms :: Morphisms
              _lhsOsErr :: ([String])
              _hdOrnr :: Int
              _tlOrnr :: Int
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _lhsOpatterns :: Patterns
              _lhsOusedDecls :: Declarations
              _lhsOrawDecls :: Declarations
              _lhsOconDefs :: ([ConceptDef])
              _hdOgE :: GenR
              _hdOsDef :: Declarations
              _tlOgE :: GenR
              _tlOsDef :: Declarations
              _hdIconDefs :: ([ConceptDef])
              _hdIkeyDefs :: KeyDefs
              _hdImGen :: Gens
              _hdImorphisms :: Morphisms
              _hdIpatterns :: Patterns
              _hdIrawDecls :: Declarations
              _hdIrnr :: Int
              _hdIrules :: Rules
              _hdIsErr :: ([String])
              _hdIusedDecls :: Declarations
              _tlIconDefs :: ([ConceptDef])
              _tlIkeyDefs :: KeyDefs
              _tlImGen :: Gens
              _tlImorphisms :: Morphisms
              _tlIpatterns :: Patterns
              _tlIrawDecls :: Declarations
              _tlIrnr :: Int
              _tlIrules :: Rules
              _tlIsErr :: ([String])
              _tlIusedDecls :: Declarations
              -- "AGtry.ag"(line 244, column 10)
              _lhsOmGen =
                  _hdImGen ++ _tlImGen
              -- "AGtry.ag"(line 569, column 10)
              _lhsOkeyDefs =
                  _hdIkeyDefs ++ _tlIkeyDefs
              -- "AGtry.ag"(line 575, column 10)
              _lhsOmorphisms =
                  _hdImorphisms ++ _tlImorphisms
              -- "AGtry.ag"(line 610, column 10)
              _lhsOsErr =
                  _hdIsErr ++ _tlIsErr
              -- "AGtry.ag"(line 911, column 10)
              _hdOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 912, column 10)
              _tlOrnr =
                  _hdIrnr
              -- "AGtry.ag"(line 913, column 10)
              _lhsOrnr =
                  _tlIrnr
              -- "AGtry.ag"(line 914, column 10)
              _lhsOrules =
                  _hdIrules ++ _tlIrules
              -- "AGtry.ag"(line 1077, column 10)
              _lhsOpatterns =
                  _hdIpatterns ++ _tlIpatterns
              -- "AGtry.ag"(line 1078, column 10)
              _lhsOusedDecls =
                  rd(_hdIusedDecls ++ _tlIusedDecls)
              -- "AGtry.ag"(line 1079, column 10)
              _lhsOrawDecls =
                  _hdIrawDecls ++ _tlIrawDecls
              -- "AGtry.ag"(line 1080, column 10)
              _lhsOconDefs =
                  _hdIconDefs ++ _tlIconDefs
              -- copy rule (down)
              _hdOgE =
                  _lhsIgE
              -- copy rule (down)
              _hdOsDef =
                  _lhsIsDef
              -- copy rule (down)
              _tlOgE =
                  _lhsIgE
              -- copy rule (down)
              _tlOsDef =
                  _lhsIsDef
              ( _hdIconDefs,_hdIkeyDefs,_hdImGen,_hdImorphisms,_hdIpatterns,_hdIrawDecls,_hdIrnr,_hdIrules,_hdIsErr,_hdIusedDecls) =
                  (hd_ _hdOgE _hdOrnr _hdOsDef )
              ( _tlIconDefs,_tlIkeyDefs,_tlImGen,_tlImorphisms,_tlIpatterns,_tlIrawDecls,_tlIrnr,_tlIrules,_tlIsErr,_tlIusedDecls) =
                  (tl_ _tlOgE _tlOrnr _tlOsDef )
          in  ( _lhsOconDefs,_lhsOkeyDefs,_lhsOmGen,_lhsOmorphisms,_lhsOpatterns,_lhsOrawDecls,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOusedDecls)))
sem_Patterns_Nil :: T_Patterns 
sem_Patterns_Nil  =
    (\ _lhsIgE
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOmGen :: Gens
              _lhsOkeyDefs :: KeyDefs
              _lhsOmorphisms :: Morphisms
              _lhsOsErr :: ([String])
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _lhsOpatterns :: Patterns
              _lhsOusedDecls :: Declarations
              _lhsOrawDecls :: Declarations
              _lhsOconDefs :: ([ConceptDef])
              -- "AGtry.ag"(line 245, column 10)
              _lhsOmGen =
                  []
              -- "AGtry.ag"(line 570, column 10)
              _lhsOkeyDefs =
                  []
              -- "AGtry.ag"(line 576, column 10)
              _lhsOmorphisms =
                  []
              -- "AGtry.ag"(line 611, column 10)
              _lhsOsErr =
                  []
              -- "AGtry.ag"(line 915, column 10)
              _lhsOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 916, column 10)
              _lhsOrules =
                  []
              -- "AGtry.ag"(line 1081, column 10)
              _lhsOpatterns =
                  []
              -- "AGtry.ag"(line 1082, column 10)
              _lhsOusedDecls =
                  []
              -- "AGtry.ag"(line 1083, column 10)
              _lhsOrawDecls =
                  []
              -- "AGtry.ag"(line 1084, column 10)
              _lhsOconDefs =
                  []
          in  ( _lhsOconDefs,_lhsOkeyDefs,_lhsOmGen,_lhsOmorphisms,_lhsOpatterns,_lhsOrawDecls,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOusedDecls)))
-- Population --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         gE                   : GenR
         sDef                 : Declarations 
      synthesized attributes:
         pop                  : Population 
         sErr                 : [String]
   alternatives:
      alternative Popu:
         child m              : Morphism 
         child ps             : {[Paire]}
-}
-- cata
sem_Population :: Population  ->
                  T_Population 
sem_Population (Popu _m _ps )  =
    (sem_Population_Popu (sem_Morphism _m ) _ps )
-- semantic domain
type T_Population  = GenR ->
                     Declarations ->
                     ( Population,([String]))
sem_Population_Popu :: T_Morphism  ->
                       ([Paire]) ->
                       T_Population 
sem_Population_Popu m_ ps_  =
    (\ _lhsIgE
       _lhsIsDef ->
         (let _lhsOsErr :: ([String])
              _lhsOpop :: Population
              _mOisign :: ([(Concept,Concept)])
              _mOgE :: GenR
              _mOsDef :: Declarations
              _mIatts :: ([Concept])
              _mIid :: Bool
              _mImorphism :: Morphism
              _mInm :: String
              _mIpos :: FilePos
              _mIraw :: ((Morphism,Declarations))
              _mIsErr :: ([String])
              _mIusedDecls :: Declarations
              _mIyin :: Bool
              -- "AGtry.ag"(line 877, column 10)
              _lhsOsErr =
                  _mIsErr
              -- "AGtry.ag"(line 1174, column 10)
              _lhsOpop =
                  Popu _mImorphism ps_
              -- "AGtry.ag"(line 1175, column 10)
              _mOisign =
                  let (m,ss) = _mIraw in if isMph m then (if inline m then map sign ss else map (sign.flp) ss) else [sign m]
              -- copy rule (down)
              _mOgE =
                  _lhsIgE
              -- copy rule (down)
              _mOsDef =
                  _lhsIsDef
              ( _mIatts,_mIid,_mImorphism,_mInm,_mIpos,_mIraw,_mIsErr,_mIusedDecls,_mIyin) =
                  (m_ _mOgE _mOisign _mOsDef )
          in  ( _lhsOpop,_lhsOsErr)))
-- Populations -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         gE                   : GenR
         sDef                 : Declarations 
      synthesized attributes:
         popus                : Populations 
         sErr                 : [String]
   alternatives:
      alternative Cons:
         child hd             : Population 
         child tl             : Populations 
      alternative Nil:
-}
-- cata
sem_Populations :: Populations  ->
                   T_Populations 
sem_Populations list  =
    (Prelude.foldr sem_Populations_Cons sem_Populations_Nil (Prelude.map sem_Population list) )
-- semantic domain
type T_Populations  = GenR ->
                      Declarations ->
                      ( Populations,([String]))
sem_Populations_Cons :: T_Population  ->
                        T_Populations  ->
                        T_Populations 
sem_Populations_Cons hd_ tl_  =
    (\ _lhsIgE
       _lhsIsDef ->
         (let _lhsOsErr :: ([String])
              _lhsOpopus :: Populations
              _hdOgE :: GenR
              _hdOsDef :: Declarations
              _tlOgE :: GenR
              _tlOsDef :: Declarations
              _hdIpop :: Population
              _hdIsErr :: ([String])
              _tlIpopus :: Populations
              _tlIsErr :: ([String])
              -- "AGtry.ag"(line 879, column 10)
              _lhsOsErr =
                  _hdIsErr ++ _tlIsErr
              -- "AGtry.ag"(line 1179, column 10)
              _lhsOpopus =
                  _hdIpop : _tlIpopus
              -- copy rule (down)
              _hdOgE =
                  _lhsIgE
              -- copy rule (down)
              _hdOsDef =
                  _lhsIsDef
              -- copy rule (down)
              _tlOgE =
                  _lhsIgE
              -- copy rule (down)
              _tlOsDef =
                  _lhsIsDef
              ( _hdIpop,_hdIsErr) =
                  (hd_ _hdOgE _hdOsDef )
              ( _tlIpopus,_tlIsErr) =
                  (tl_ _tlOgE _tlOsDef )
          in  ( _lhsOpopus,_lhsOsErr)))
sem_Populations_Nil :: T_Populations 
sem_Populations_Nil  =
    (\ _lhsIgE
       _lhsIsDef ->
         (let _lhsOsErr :: ([String])
              _lhsOpopus :: Populations
              -- "AGtry.ag"(line 880, column 10)
              _lhsOsErr =
                  []
              -- "AGtry.ag"(line 1180, column 10)
              _lhsOpopus =
                  []
          in  ( _lhsOpopus,_lhsOsErr)))
-- Prop --------------------------------------------------------
{-
   alternatives:
      alternative Asy:
      alternative Inj:
      alternative Sur:
      alternative Sym:
      alternative Tot:
      alternative Trn:
      alternative Uni:
-}
-- cata
sem_Prop :: Prop  ->
            T_Prop 
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
-- semantic domain
type T_Prop  = ( )
sem_Prop_Asy :: T_Prop 
sem_Prop_Asy  =
    (let 
     in  ( ))
sem_Prop_Inj :: T_Prop 
sem_Prop_Inj  =
    (let 
     in  ( ))
sem_Prop_Sur :: T_Prop 
sem_Prop_Sur  =
    (let 
     in  ( ))
sem_Prop_Sym :: T_Prop 
sem_Prop_Sym  =
    (let 
     in  ( ))
sem_Prop_Tot :: T_Prop 
sem_Prop_Tot  =
    (let 
     in  ( ))
sem_Prop_Trn :: T_Prop 
sem_Prop_Trn  =
    (let 
     in  ( ))
sem_Prop_Uni :: T_Prop 
sem_Prop_Uni  =
    (let 
     in  ( ))
-- Rule --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         gE                   : GenR
         pn                   : String
         sDef                 : Declarations 
      chained attribute:
         rnr                  : Int
      synthesized attributes:
         declarations         : Declarations 
         mGen                 : Gens 
         morphisms            : Morphisms
         rules                : Rules 
         sErr                 : [String]
         usedDecls            : Declarations 
   alternatives:
      alternative Gc:
         child gluePos        : {FilePos}
         child m              : Morphism 
         child expr           : Expression 
         child cpu            : Expressions 
         child sgn            : {(Concept,Concept)}
         child nr             : {Int}
         child pn             : {String}
         visit 0:
            local rs          : _
            local mors        : _
            local ruls        : _
            local closdecls   : _
            local ms          : _
            local esign       : _
      alternative Ru:
         child c              : RuleType 
         child antc           : Expression 
         child rulePos        : {FilePos}
         child cons           : Expression 
         child cpu            : Expressions 
         child expl           : {String}
         child sgn            : {(Concept,Concept)}
         child nr             : {Int}
         child pn             : {String}
         visit 0:
            local r           : _
            local rs          : _
            local errcpu      : _
            local rcpu        : _
            local closdecls   : _
            local clrs        : _
            local is          : _
            local s           : _
      alternative Sg:
         child pos            : {FilePos}
         child rule           : Rule 
         child expl           : {String}
         child sgn            : {(Concept,Concept)}
         child nr             : {Int}
         child pn             : {String}
         child signal         : Declaration 
         visit 0:
            local signalname  : _
            local rs          : _
-}
-- cata
sem_Rule :: Rule  ->
            T_Rule 
sem_Rule (Gc _gluePos _m _expr _cpu _sgn _nr _pn )  =
    (sem_Rule_Gc _gluePos (sem_Morphism _m ) (sem_Expression _expr ) (sem_Expressions _cpu ) _sgn _nr _pn )
sem_Rule (Ru _c _antc _rulePos _cons _cpu _expl _sgn _nr _pn )  =
    (sem_Rule_Ru (sem_RuleType _c ) (sem_Expression _antc ) _rulePos (sem_Expression _cons ) (sem_Expressions _cpu ) _expl _sgn _nr _pn )
sem_Rule (Sg _pos _rule _expl _sgn _nr _pn _signal )  =
    (sem_Rule_Sg _pos (sem_Rule _rule ) _expl _sgn _nr _pn (sem_Declaration _signal ) )
-- semantic domain
type T_Rule  = GenR ->
               String ->
               Int ->
               Declarations ->
               ( Declarations,Gens,Morphisms,Int,Rules,([String]),Declarations)
sem_Rule_Gc :: FilePos ->
               T_Morphism  ->
               T_Expression  ->
               T_Expressions  ->
               ((Concept,Concept)) ->
               Int ->
               String ->
               T_Rule 
sem_Rule_Gc gluePos_ m_ expr_ cpu_ sgn_ nr_ pn_  =
    (\ _lhsIgE
       _lhsIpn
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOmGen :: Gens
              _cpuOisign :: ([(Concept,Concept)])
              _cpuOseptets :: ([(Concept, Expressions, [(Concept,Concept)], [Concept], [(Concept,Concept)], Expressions, Concept)])
              _cpuOpos :: FilePos
              _mOisign :: ([(Concept,Concept)])
              _exprOisign :: ([(Concept,Concept)])
              _exprOpos :: FilePos
              _lhsOmorphisms :: Morphisms
              _lhsOusedDecls :: Declarations
              _lhsOdeclarations :: Declarations
              _lhsOsErr :: ([String])
              _exprOrnr :: Int
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _mOgE :: GenR
              _mOsDef :: Declarations
              _exprOgE :: GenR
              _exprOpn :: String
              _exprOsDef :: Declarations
              _cpuOgE :: GenR
              _cpuOpn :: String
              _cpuOrnr :: Int
              _cpuOsDef :: Declarations
              _mIatts :: ([Concept])
              _mIid :: Bool
              _mImorphism :: Morphism
              _mInm :: String
              _mIpos :: FilePos
              _mIraw :: ((Morphism,Declarations))
              _mIsErr :: ([String])
              _mIusedDecls :: Declarations
              _mIyin :: Bool
              _exprIexpr :: Expression
              _exprImorphisms :: Morphisms
              _exprIraw :: Expression
              _exprIrnr :: Int
              _exprIrules :: Rules
              _exprIsErr :: ([String])
              _exprIsigns :: ([(Concept,Concept)])
              _cpuIexprs :: Expressions
              _cpuImorphisms :: Morphisms
              _cpuIraw :: Expressions
              _cpuIrnr :: Int
              _cpuIrules :: Rules
              _cpuIsErr :: ([String])
              _cpuIsignss :: ([[(Concept,Concept)]])
              -- "AGtry.ag"(line 254, column 8)
              _lhsOmGen =
                  rd [G pos g s| (d,c) <- _exprIsigns, m <- _mIusedDecls, G pos g s<-[G gluePos_ d (source m), G gluePos_ c (target m)], g/=s]
              -- "AGtry.ag"(line 389, column 8)
              _cpuOisign =
                  []
              -- "AGtry.ag"(line 390, column 8)
              _cpuOseptets =
                  []
              -- "AGtry.ag"(line 391, column 8)
              _cpuOpos =
                  gluePos_
              -- "AGtry.ag"(line 392, column 8)
              _mOisign =
                  _ms
              -- "AGtry.ag"(line 395, column 8)
              _exprOisign =
                  _esign
              -- "AGtry.ag"(line 396, column 8)
              _exprOpos =
                  gluePos_
              -- "AGtry.ag"(line 398, column 8)
              _rs =
                  [ Ru c antc pos expr cpu expla (a,b) (i + _lhsIrnr) _lhsIpn
                  | ((Ru c antc pos expr cpu expla (a,b) _ _,cpuErrs), i)<-zip _ruls [0..]] ++ _exprIrules
              -- "AGtry.ag"(line 400, column 8)
              _lhsOmorphisms =
                  _mors
              -- "AGtry.ag"(line 401, column 8)
              _mors =
                  _mImorphism : _exprImorphisms
              -- "AGtry.ag"(line 402, column 8)
              _ruls =
                  [ subExprCheck (Ru Implication (Tm _mImorphism) gluePos_ _exprIexpr _cpuIraw "" (a,b) nr_ _lhsIpn)
                  | (d,c) <- _exprIsigns, s <- _mIusedDecls
                  , if d `_lhsIgE` source _mImorphism && c `_lhsIgE` target _mImorphism then True else
                    error ("Fatal: (please report this as an error in ADL)\nFalse assumption in alternative Gc of SEM Rule, that "++show (d,c)++" in @expr.signs be more general than "++ show (source _mImorphism,target _mImorphism)++".")
                  , (a,b)<-[if null _mIatts then (source _mImorphism,target _mImorphism) else (head _mIatts,last _mIatts)]]
              -- "AGtry.ag"(line 408, column 8)
              _lhsOusedDecls =
                  rd (declarations _mors ++ _closdecls)
              -- "AGtry.ag"(line 410, column 8)
              _lhsOdeclarations =
                  []
              -- "AGtry.ag"(line 412, column 8)
              _closdecls =
                  rd [s| Ru c antc p cons cpu expla (a,b) n pn <- _exprIrules, s<-declarations antc]
              -- "AGtry.ag"(line 414, column 8)
              _ms =
                  irredT _lhsIgE
                   (if null _exprIsigns
                    then [(source m,target m)| m <- _mIusedDecls]
                    else [(source m,target m)| m <- _mIusedDecls
                                             , or[s `_lhsIgE` source m && t `_lhsIgE` target m
                                                 | (s,t) <- _exprIsigns ]
                         ])
              -- "AGtry.ag"(line 421, column 8)
              _esign =
                  irredT _lhsIgE _exprIsigns
              -- "AGtry.ag"(line 658, column 8)
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
              -- "AGtry.ag"(line 971, column 10)
              _exprOrnr =
                  _lhsIrnr+length _rs
              -- "AGtry.ag"(line 972, column 10)
              _lhsOrnr =
                  _exprIrnr
              -- "AGtry.ag"(line 973, column 10)
              _lhsOrules =
                  _rs
              -- copy rule (down)
              _mOgE =
                  _lhsIgE
              -- copy rule (down)
              _mOsDef =
                  _lhsIsDef
              -- copy rule (down)
              _exprOgE =
                  _lhsIgE
              -- copy rule (down)
              _exprOpn =
                  _lhsIpn
              -- copy rule (down)
              _exprOsDef =
                  _lhsIsDef
              -- copy rule (down)
              _cpuOgE =
                  _lhsIgE
              -- copy rule (down)
              _cpuOpn =
                  _lhsIpn
              -- copy rule (chain)
              _cpuOrnr =
                  _exprIrnr
              -- copy rule (down)
              _cpuOsDef =
                  _lhsIsDef
              ( _mIatts,_mIid,_mImorphism,_mInm,_mIpos,_mIraw,_mIsErr,_mIusedDecls,_mIyin) =
                  (m_ _mOgE _mOisign _mOsDef )
              ( _exprIexpr,_exprImorphisms,_exprIraw,_exprIrnr,_exprIrules,_exprIsErr,_exprIsigns) =
                  (expr_ _exprOgE _exprOisign _exprOpn _exprOpos _exprOrnr _exprOsDef )
              ( _cpuIexprs,_cpuImorphisms,_cpuIraw,_cpuIrnr,_cpuIrules,_cpuIsErr,_cpuIsignss) =
                  (cpu_ _cpuOgE _cpuOisign _cpuOpn _cpuOpos _cpuOrnr _cpuOsDef _cpuOseptets )
          in  ( _lhsOdeclarations,_lhsOmGen,_lhsOmorphisms,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOusedDecls)))
sem_Rule_Ru :: T_RuleType  ->
               T_Expression  ->
               FilePos ->
               T_Expression  ->
               T_Expressions  ->
               String ->
               ((Concept,Concept)) ->
               Int ->
               String ->
               T_Rule 
sem_Rule_Ru c_ antc_ rulePos_ cons_ cpu_ expl_ sgn_ nr_ pn_  =
    (\ _lhsIgE
       _lhsIpn
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOmGen :: Gens
              _cpuOisign :: ([(Concept,Concept)])
              _cpuOseptets :: ([(Concept, Expressions, [(Concept,Concept)], [Concept], [(Concept,Concept)], Expressions, Concept)])
              _cpuOpos :: FilePos
              _antcOisign :: ([(Concept,Concept)])
              _consOisign :: ([(Concept,Concept)])
              _antcOpos :: FilePos
              _consOpos :: FilePos
              _lhsOusedDecls :: Declarations
              _lhsOdeclarations :: Declarations
              _lhsOmorphisms :: Morphisms
              _lhsOsErr :: ([String])
              _antcOrnr :: Int
              _consOrnr :: Int
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _antcOgE :: GenR
              _antcOpn :: String
              _antcOsDef :: Declarations
              _consOgE :: GenR
              _consOpn :: String
              _consOsDef :: Declarations
              _cpuOgE :: GenR
              _cpuOpn :: String
              _cpuOrnr :: Int
              _cpuOsDef :: Declarations
              _cItyp :: RuleType
              _antcIexpr :: Expression
              _antcImorphisms :: Morphisms
              _antcIraw :: Expression
              _antcIrnr :: Int
              _antcIrules :: Rules
              _antcIsErr :: ([String])
              _antcIsigns :: ([(Concept,Concept)])
              _consIexpr :: Expression
              _consImorphisms :: Morphisms
              _consIraw :: Expression
              _consIrnr :: Int
              _consIrules :: Rules
              _consIsErr :: ([String])
              _consIsigns :: ([(Concept,Concept)])
              _cpuIexprs :: Expressions
              _cpuImorphisms :: Morphisms
              _cpuIraw :: Expressions
              _cpuIrnr :: Int
              _cpuIrules :: Rules
              _cpuIsErr :: ([String])
              _cpuIsignss :: ([[(Concept,Concept)]])
              -- "AGtry.ag"(line 252, column 8)
              _lhsOmGen =
                  []
              -- "AGtry.ag"(line 321, column 8)
              _cpuOisign =
                  _is
              -- "AGtry.ag"(line 322, column 8)
              _cpuOseptets =
                  []
              -- "AGtry.ag"(line 323, column 8)
              _cpuOpos =
                  rulePos_
              -- "AGtry.ag"(line 324, column 8)
              _antcOisign =
                  if null _is then _antcIsigns else _is
              -- "AGtry.ag"(line 325, column 8)
              _consOisign =
                  if null _is then _consIsigns else _is
              -- "AGtry.ag"(line 326, column 8)
              _antcOpos =
                  rulePos_
              -- "AGtry.ag"(line 327, column 8)
              _consOpos =
                  rulePos_
              -- "AGtry.ag"(line 331, column 8)
              _r =
                  fst _rcpu
              -- "AGtry.ag"(line 332, column 8)
              _rs =
                  [ _r ] ++ _clrs
              -- "AGtry.ag"(line 334, column 8)
              _errcpu =
                  snd _rcpu
              -- "AGtry.ag"(line 335, column 8)
              _rcpu =
                  subExprCheck
                  (if _cItyp==AlwaysExpr
                  then Ru _cItyp (error ("Reference to antecedent of AlwaysExpr Rule "++"showHS  r")) rulePos_ _consIexpr _cpuIraw expl_
                          (if null _is
                           then error("Fatal: null @is on "++show rulePos_++"\n@cons.expr = "++show _consIexpr++"\n@cons.signs = "++shSigns _consIsigns)
                           else head _is)
                           _lhsIrnr _lhsIpn
                  else Ru _cItyp _antcIexpr rulePos_ _consIexpr _cpuIraw expl_
                          (if null _is
                           then error("Fatal: null @is on "++show rulePos_++"\n@antc.expr = "++show _antcIexpr++"\n@antc.signs = "++shSigns _antcIsigns++"\n@cons.expr = "++show _consIexpr++"\n@cons.signs = "++shSigns _consIsigns)
                           else head _is)
                          _lhsIrnr _lhsIpn)
              -- "AGtry.ag"(line 348, column 8)
              _lhsOusedDecls =
                  rd (declarations (_antcImorphisms ++ _consImorphisms) ++ _closdecls)
              -- "AGtry.ag"(line 350, column 8)
              _lhsOdeclarations =
                  []
              -- "AGtry.ag"(line 352, column 8)
              _closdecls =
                  rd [s| Ru c antc p cons cpu expla (a,b) n pn <- _clrs, s<-declarations antc]
              -- "AGtry.ag"(line 354, column 8)
              _clrs =
                  if _cItyp==AlwaysExpr
                  then _consIrules
                  else _antcIrules ++ _consIrules
              -- "AGtry.ag"(line 357, column 8)
              _lhsOmorphisms =
                  (if _cItyp==AlwaysExpr then [] else _antcImorphisms)++ _consImorphisms
              -- "AGtry.ag"(line 359, column 8)
              _is =
                  if _cItyp==AlwaysExpr then _consIsigns else
                  if null _antcIsigns
                  then (if null _consIsigns
                        then []
                        else _consIsigns)
                  else (if null _consIsigns
                        then _antcIsigns
                        else _s)
              -- "AGtry.ag"(line 368, column 8)
              _s =
                  if _cItyp==AlwaysExpr then _consIsigns else
                  irredT _lhsIgE [ (if a `_lhsIgE` a' then a' else a, if b `_lhsIgE` b' then b' else b)
                                 | (a,b)<- _consIsigns, (a',b')<- _antcIsigns
                                 , (a `_lhsIgE` a' || a' `_lhsIgE` a) && (b `_lhsIgE` b' || b' `_lhsIgE` b)
                                 ]
              -- "AGtry.ag"(line 622, column 8)
              _lhsOsErr =
                  take 1
                  (_consIsErr ++
                    (if _cItyp==AlwaysExpr then [] else _antcIsErr ++
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
              -- "AGtry.ag"(line 963, column 10)
              _antcOrnr =
                  _lhsIrnr + 1
              -- "AGtry.ag"(line 964, column 10)
              _consOrnr =
                  if _cItyp==AlwaysExpr then _lhsIrnr + 1 else _antcIrnr
              -- "AGtry.ag"(line 965, column 10)
              _lhsOrnr =
                  _consIrnr
              -- "AGtry.ag"(line 966, column 10)
              _lhsOrules =
                  _rs
              -- copy rule (down)
              _antcOgE =
                  _lhsIgE
              -- copy rule (down)
              _antcOpn =
                  _lhsIpn
              -- copy rule (down)
              _antcOsDef =
                  _lhsIsDef
              -- copy rule (down)
              _consOgE =
                  _lhsIgE
              -- copy rule (down)
              _consOpn =
                  _lhsIpn
              -- copy rule (down)
              _consOsDef =
                  _lhsIsDef
              -- copy rule (down)
              _cpuOgE =
                  _lhsIgE
              -- copy rule (down)
              _cpuOpn =
                  _lhsIpn
              -- copy rule (chain)
              _cpuOrnr =
                  _consIrnr
              -- copy rule (down)
              _cpuOsDef =
                  _lhsIsDef
              ( _cItyp) =
                  (c_ )
              ( _antcIexpr,_antcImorphisms,_antcIraw,_antcIrnr,_antcIrules,_antcIsErr,_antcIsigns) =
                  (antc_ _antcOgE _antcOisign _antcOpn _antcOpos _antcOrnr _antcOsDef )
              ( _consIexpr,_consImorphisms,_consIraw,_consIrnr,_consIrules,_consIsErr,_consIsigns) =
                  (cons_ _consOgE _consOisign _consOpn _consOpos _consOrnr _consOsDef )
              ( _cpuIexprs,_cpuImorphisms,_cpuIraw,_cpuIrnr,_cpuIrules,_cpuIsErr,_cpuIsignss) =
                  (cpu_ _cpuOgE _cpuOisign _cpuOpn _cpuOpos _cpuOrnr _cpuOsDef _cpuOseptets )
          in  ( _lhsOdeclarations,_lhsOmGen,_lhsOmorphisms,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOusedDecls)))
sem_Rule_Sg :: FilePos ->
               T_Rule  ->
               String ->
               ((Concept,Concept)) ->
               Int ->
               String ->
               T_Declaration  ->
               T_Rule 
sem_Rule_Sg pos_ rule_ expl_ sgn_ nr_ pn_ signal_  =
    (\ _lhsIgE
       _lhsIpn
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOmGen :: Gens
              _lhsOmorphisms :: Morphisms
              _lhsOusedDecls :: Declarations
              _lhsOdeclarations :: Declarations
              _lhsOsErr :: ([String])
              _ruleOrnr :: Int
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _ruleOgE :: GenR
              _ruleOpn :: String
              _ruleOsDef :: Declarations
              _signalOgE :: GenR
              _signalOrnr :: Int
              _signalOsDef :: Declarations
              _ruleIdeclarations :: Declarations
              _ruleImGen :: Gens
              _ruleImorphisms :: Morphisms
              _ruleIrnr :: Int
              _ruleIrules :: Rules
              _ruleIsErr :: ([String])
              _ruleIusedDecls :: Declarations
              _signalIdeclaration :: Declaration
              _signalInm :: String
              _signalIrawDecl :: Declaration
              _signalIrnr :: Int
              _signalIrules :: Rules
              _signalIsErr :: ([String])
              -- "AGtry.ag"(line 253, column 8)
              _lhsOmGen =
                  []
              -- "AGtry.ag"(line 376, column 8)
              _lhsOmorphisms =
                  _ruleImorphisms
              -- "AGtry.ag"(line 377, column 8)
              _lhsOusedDecls =
                  rd (declarations _ruleImorphisms)
              -- "AGtry.ag"(line 379, column 8)
              _lhsOdeclarations =
                  declarations _rs
              -- "AGtry.ag"(line 380, column 8)
              _signalname =
                  if null _signalInm || take 6 _signalInm == "Signal" && and [isDigit c| c<-drop 6 _signalInm ]
                  then "Signal"++show _lhsIrnr else _signalInm
              -- "AGtry.ag"(line 382, column 8)
              _rs =
                  [ Sg p r expla sgn n _lhsIpn (Sgn _signalname a b [] "" "" "" [] "" pos_ _lhsIrnr True)
                  | r@(Ru c antc p cons cpu expla sgn n _lhsIpn)<-take 1 _ruleIrules
                  , (a,b)<-[sign r] ] ++ drop 1 _ruleIrules
              -- "AGtry.ag"(line 657, column 8)
              _lhsOsErr =
                  _ruleIsErr
              -- "AGtry.ag"(line 967, column 10)
              _ruleOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 968, column 10)
              _lhsOrnr =
                  if null _signalInm || take 6 _signalInm == "Signal" && and [isDigit c| c<-drop 6 _signalInm ]
                  then _lhsIrnr + 1 else _lhsIrnr
              -- "AGtry.ag"(line 970, column 10)
              _lhsOrules =
                  _rs
              -- copy rule (down)
              _ruleOgE =
                  _lhsIgE
              -- copy rule (down)
              _ruleOpn =
                  _lhsIpn
              -- copy rule (down)
              _ruleOsDef =
                  _lhsIsDef
              -- copy rule (down)
              _signalOgE =
                  _lhsIgE
              -- copy rule (chain)
              _signalOrnr =
                  _ruleIrnr
              -- copy rule (down)
              _signalOsDef =
                  _lhsIsDef
              ( _ruleIdeclarations,_ruleImGen,_ruleImorphisms,_ruleIrnr,_ruleIrules,_ruleIsErr,_ruleIusedDecls) =
                  (rule_ _ruleOgE _ruleOpn _ruleOrnr _ruleOsDef )
              ( _signalIdeclaration,_signalInm,_signalIrawDecl,_signalIrnr,_signalIrules,_signalIsErr) =
                  (signal_ _signalOgE _signalOrnr _signalOsDef )
          in  ( _lhsOdeclarations,_lhsOmGen,_lhsOmorphisms,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOusedDecls)))
-- RuleType ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         typ                  : RuleType
   alternatives:
      alternative AlwaysExpr:
      alternative Automatic:
      alternative Equivalence:
      alternative Generalization:
      alternative Implication:
-}
-- cata
sem_RuleType :: RuleType  ->
                T_RuleType 
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
-- semantic domain
type T_RuleType  = ( RuleType)
sem_RuleType_AlwaysExpr :: T_RuleType 
sem_RuleType_AlwaysExpr  =
    (let _lhsOtyp :: RuleType
         -- "AGtry.ag"(line 260, column 20)
         _lhsOtyp =
             AlwaysExpr
     in  ( _lhsOtyp))
sem_RuleType_Automatic :: T_RuleType 
sem_RuleType_Automatic  =
    (let _lhsOtyp :: RuleType
         -- "AGtry.ag"(line 262, column 20)
         _lhsOtyp =
             Automatic
     in  ( _lhsOtyp))
sem_RuleType_Equivalence :: T_RuleType 
sem_RuleType_Equivalence  =
    (let _lhsOtyp :: RuleType
         -- "AGtry.ag"(line 259, column 20)
         _lhsOtyp =
             Equivalence
     in  ( _lhsOtyp))
sem_RuleType_Generalization :: T_RuleType 
sem_RuleType_Generalization  =
    (let _lhsOtyp :: RuleType
         -- "AGtry.ag"(line 261, column 20)
         _lhsOtyp =
             Generalization
     in  ( _lhsOtyp))
sem_RuleType_Implication :: T_RuleType 
sem_RuleType_Implication  =
    (let _lhsOtyp :: RuleType
         -- "AGtry.ag"(line 258, column 20)
         _lhsOtyp =
             Implication
     in  ( _lhsOtyp))
-- Rules -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         gE                   : GenR
         pn                   : String
         sDef                 : Declarations 
      chained attribute:
         rnr                  : Int
      synthesized attributes:
         declarations         : Declarations 
         mGen                 : Gens 
         morphisms            : Morphisms
         rules                : Rules 
         sErr                 : [String]
         usedDecls            : Declarations 
   alternatives:
      alternative Cons:
         child hd             : Rule 
         child tl             : Rules 
      alternative Nil:
-}
-- cata
sem_Rules :: Rules  ->
             T_Rules 
sem_Rules list  =
    (Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list) )
-- semantic domain
type T_Rules  = GenR ->
                String ->
                Int ->
                Declarations ->
                ( Declarations,Gens,Morphisms,Int,Rules,([String]),Declarations)
sem_Rules_Cons :: T_Rule  ->
                  T_Rules  ->
                  T_Rules 
sem_Rules_Cons hd_ tl_  =
    (\ _lhsIgE
       _lhsIpn
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOmGen :: Gens
              _lhsOmorphisms :: Morphisms
              _lhsOsErr :: ([String])
              _lhsOdeclarations :: Declarations
              _lhsOusedDecls :: Declarations
              _hdOrnr :: Int
              _tlOrnr :: Int
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              _hdOgE :: GenR
              _hdOpn :: String
              _hdOsDef :: Declarations
              _tlOgE :: GenR
              _tlOpn :: String
              _tlOsDef :: Declarations
              _hdIdeclarations :: Declarations
              _hdImGen :: Gens
              _hdImorphisms :: Morphisms
              _hdIrnr :: Int
              _hdIrules :: Rules
              _hdIsErr :: ([String])
              _hdIusedDecls :: Declarations
              _tlIdeclarations :: Declarations
              _tlImGen :: Gens
              _tlImorphisms :: Morphisms
              _tlIrnr :: Int
              _tlIrules :: Rules
              _tlIsErr :: ([String])
              _tlIusedDecls :: Declarations
              -- "AGtry.ag"(line 249, column 10)
              _lhsOmGen =
                  _hdImGen ++ _tlImGen
              -- "AGtry.ag"(line 580, column 10)
              _lhsOmorphisms =
                  _hdImorphisms ++ _tlImorphisms
              -- "AGtry.ag"(line 615, column 10)
              _lhsOsErr =
                  _hdIsErr ++ _tlIsErr
              -- "AGtry.ag"(line 616, column 10)
              _lhsOdeclarations =
                  _hdIdeclarations ++ _tlIdeclarations
              -- "AGtry.ag"(line 617, column 10)
              _lhsOusedDecls =
                  _hdIusedDecls ++ _tlIusedDecls
              -- "AGtry.ag"(line 955, column 10)
              _hdOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 956, column 10)
              _tlOrnr =
                  _hdIrnr
              -- "AGtry.ag"(line 957, column 10)
              _lhsOrnr =
                  _tlIrnr
              -- "AGtry.ag"(line 958, column 10)
              _lhsOrules =
                  _hdIrules ++ _tlIrules
              -- copy rule (down)
              _hdOgE =
                  _lhsIgE
              -- copy rule (down)
              _hdOpn =
                  _lhsIpn
              -- copy rule (down)
              _hdOsDef =
                  _lhsIsDef
              -- copy rule (down)
              _tlOgE =
                  _lhsIgE
              -- copy rule (down)
              _tlOpn =
                  _lhsIpn
              -- copy rule (down)
              _tlOsDef =
                  _lhsIsDef
              ( _hdIdeclarations,_hdImGen,_hdImorphisms,_hdIrnr,_hdIrules,_hdIsErr,_hdIusedDecls) =
                  (hd_ _hdOgE _hdOpn _hdOrnr _hdOsDef )
              ( _tlIdeclarations,_tlImGen,_tlImorphisms,_tlIrnr,_tlIrules,_tlIsErr,_tlIusedDecls) =
                  (tl_ _tlOgE _tlOpn _tlOrnr _tlOsDef )
          in  ( _lhsOdeclarations,_lhsOmGen,_lhsOmorphisms,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOusedDecls)))
sem_Rules_Nil :: T_Rules 
sem_Rules_Nil  =
    (\ _lhsIgE
       _lhsIpn
       _lhsIrnr
       _lhsIsDef ->
         (let _lhsOmGen :: Gens
              _lhsOmorphisms :: Morphisms
              _lhsOsErr :: ([String])
              _lhsOdeclarations :: Declarations
              _lhsOusedDecls :: Declarations
              _lhsOrnr :: Int
              _lhsOrules :: Rules
              -- "AGtry.ag"(line 250, column 10)
              _lhsOmGen =
                  []
              -- "AGtry.ag"(line 581, column 10)
              _lhsOmorphisms =
                  []
              -- "AGtry.ag"(line 618, column 10)
              _lhsOsErr =
                  []
              -- "AGtry.ag"(line 619, column 10)
              _lhsOdeclarations =
                  []
              -- "AGtry.ag"(line 620, column 10)
              _lhsOusedDecls =
                  []
              -- "AGtry.ag"(line 959, column 10)
              _lhsOrnr =
                  _lhsIrnr
              -- "AGtry.ag"(line 960, column 10)
              _lhsOrules =
                  []
          in  ( _lhsOdeclarations,_lhsOmGen,_lhsOmorphisms,_lhsOrnr,_lhsOrules,_lhsOsErr,_lhsOusedDecls)))
module TypeInference.InfExpression where
--this module is based on code generated with uuagc
--uuagc is not used because you can't (as far as I know) define DATA PExpression Morphism (Maybe Sign)
--you can define the polymorph DATA PExpression a b, but it does not match our expectations

import Adl
import Data.List (union,nubBy,nub)

--the type containing inh and syn atts
type InfExpression  = 
     [Declaration] ->  --decls
     [(Concept,Concept)] -> --isas
     (Maybe Sign) --autocast
     -> SynAtts
type SynAtts = --synthesized
     (PExpression Declaration Sign --typedexpr
     ,[Sign] --trytype 
                               --if trytype subexpression==ambiguous => autocast if one alternative possible 
                               --else remove impossible alts and return trytype==ambiguous
                               --An ambiguous error contains all alternatives
     ,Either Sign [TErr] --reltype (if reltype subexpression==ambiguous => error)
     ,[ITree] --proof
     ,Bool --homogeneous?
     )
type TErr = String
type ITree = String
------------------------------------------------------------
--uuagc: *_Syn_PExpression functions
typedexpr :: SynAtts -> PExpression Declaration Sign
typedexpr (infex,_,_,_,_) = infex
trytype :: SynAtts -> [Sign]
trytype (_,ttype,_,_,_) = ttype
reltype :: SynAtts -> Either Sign [TErr]
reltype (_,_,rtype,_,_) = rtype
prooftree :: SynAtts -> [ITree]
prooftree (_,_,_,proof,_) = proof
ishomo :: SynAtts -> Bool
ishomo (_,_,_,_,hm) = hm

--all the type-correct exprs given a context of decls and isas
typedexprs :: [Declaration] -> [(Concept,Concept)] -> [PExpression Morphism (Maybe Sign)] -> [PExpression Declaration Sign]
typedexprs ds isas xs = [typedexpr ix|ix<-ixs,(not.inerror)(reltype ix)]
   where ixs = [infer x ds isas Nothing | x<-xs]
--all the type errors given a context of decls and isas
typeerrors :: [Declaration] -> [(Concept,Concept)] -> [PExpression Morphism (Maybe Sign)] -> [[TErr]]
typeerrors ds isas xs = [err|ix <-[infer x ds isas Nothing | x<-xs],inerror(reltype ix),let Right err = reltype ix]
------------------------------------------------------------
--uuagc: infer=sem_PExpression
--stop de attributen van een expr in de infer functie met gelijke structuur
--een attr x van het type PExpression gaat er in als (infer x) = inh atts tuple
infer :: PExpression Morphism (Maybe Sign) -> InfExpression 
infer (MulPExp mop subs usercast )  = infer_MulPExp mop (map infer subs) usercast
infer (UnPExp uop sub usercast )  = infer_UnPExp uop (infer sub) usercast 
infer (TPExp subm usercast )  = infer_TPExp subm usercast 

-- inh: *_  (decls and isa do not change)
-- syn: _* 
infer_UnPExp :: UnOp -> InfExpression  ->  Maybe Sign ->  InfExpression 
infer_UnPExp uop sub usercast     decls_ isa_ autocast_ =
    let {--calculate synthesized--}
        ttype = ptf (trytype (subtuple subac))
        rtype = lmap tf(reltype (subtuple subac))         --no type conditions on unpexp i.e. no errors will be discovered here
        infex = UnPExp uop (typedexpr (subtuple subac)) (thetype rtype)
        proof = []
        hm =ishomo (subtuple usercast)
    in  (infex,ttype,rtype,proof,hm)                      --return synthesized
    where
    {--push inherited on subexpression--}
    subtuple = sub decls_ isa_                            --the synth. atts of the subexpression with free autocast
    subac    = if castcondition usercast autocast_ isa_   --pass cast down to the sub
               then fmap tf_inv (cast usercast autocast_) --use the inverse type
               else error "No match with user cast"
    {--typing functions--}
    ptf = map tf                 --the pre-type function resembles the type function
    tf (a,b) | uop==Co   = (b,a)
             | otherwise = (a,b)
    tf_inv = tf                  --the inverse of tf equals tf,  but you never know which unary operators will be added

infer_MulPExp :: MulOp -> [InfExpression]  ->  (Maybe Sign) ->  InfExpression 
infer_MulPExp mop subs usercast     decls_ isa_ autocast_ 
 |elem mop [Fc,Fd] =
    let {--calculate synthesized--}
        --if ttype contains duplicates, then there must be an ambiguous composition like r::A*B,r::A*C,s::B*D,s::C*D => ttype=[(A,D),(A,D)]
        --thus duplicates are useful for erroranalysis, but will be BOTH/ALL be removed from trytype of this expression
        --because there will be no way to correct this from the context with an autocast_, and thus it is not a potential type
        --in other words, if there is another alternative p.e ttype=[(A,D),(A,D),(X,Y)] 
        --then the ambiguous composition is ignored (trytype=rmdupl [] ttype = [(X,Y)])
        --TODO -> Do I really want amb.comp to be ignored?
        --case if there is some context that autocasts (X,Y) then it is ok -> reltype
        --     if there is no such thing then autocast_==Nothing => reltype=amb.comp
        ttype = ttypemerge tryac subs --ttype respecting autocast_ should be equal to (tf(reltype substuples))
        rtype = case length ttype of
           1 -> Left tf
           _ -> analyseerror
        infex = MulPExp mop (map typedexpr the_subs_tuples) (thetype rtype)
        proof = []
        hm = False --foldr (&&) True (map ishomo (head(alts tryac subs))) --all composed are homogeneous <-> mulpexp is homogeneous, just check the head alt.
        {--push inherited on subexpressions--}  --alternative::[SynAtts] one SynAtts for each subexpression
        tryac =  if castcondition usercast autocast_ isa_ then cast usercast autocast_ else error "No match with user cast"
      --  no_alt (xs,_) = [if null x then [] else x|x<-xs]
        ttypemerge ac ss 
           | length ss<2 = concat(concat[map trytype s_tpls|s_tpls<-fst (alts ac ss)])
           | otherwise    = ptf[concat(map trytype s_tpls)|s_tpls<-fst (alts ac ss)] --types of all alternatives are potential types of this expression
         --alts => list of alternatives for a composition of subexpressions (ss) given an autocast (ac) 
        the_subs_tuples :: [SynAtts] --one SynAtts for each subexpression
        the_subs_tuples                    --autocast is not free anymore i.e. equal to tf
           = [head sub_tplss
             |sub_tplss<-subs_tplss
     --        ,if length sub_tplss==1 then True 
       --       else error (show("there are alternatives i.e. ambiguous reltype"
         --                ,ttype,[concat(map trytype s_tpls)|let Left ss_tplss=alts tryac subs,s_tpls<-ss_tplss],map trytype sub_tplss))
             ]
           where subs_tplss = case alts (Just tf) subs of (x,_) -> x; _ -> error "xxx"
        alts = comp_alts ttypemerge decls_ isa_
        {--typing functions--}
        ptf :: [[Sign]] -> [Sign] --calculate the trytype from the trytypes of the subs 
        ptf [] = error "there must be subs so there must be trytype lists for them"
        ptf (tts:ttss) = concat[map (tp a) (cs [b] ttss)|(a,b)<-tts]
           where
           tp a c = (a,c)
           cs bs [] = bs
           cs bs (x:xs) = cs [c|b<-bs,(b',c)<-x,(b\-\b') isa_] xs
        tf | length ttype==1 = head ttype
           | otherwise = error "tf undefined: tf is defined iff length ttype==1"
        {--error analysis--} 
        analyseerror = {-Right ["xxx"] --}Right (snd( alts tryac subs)) --Right (comp_errors (alts tryac subs))
    in  (infex,rmdupl [] ttype,rtype,proof,hm) --return synthesized (use of nub, see comments@ttype)
 |otherwise =
    let --push inherited on subexpressions
        autocast = Nothing
        substuples = [sub decls_ isa_ autocast | sub<-subs]
        --calculate synthesized
        ttype = []
        rtype = testy usercast
        infex = MulPExp mop (map typedexpr substuples) (thetype rtype)
        proof = []
        hm = foldr (||) False (map ishomo substuples) --one comparable is homogeneous <-> mulpexp is homogeneous
             && elem mop [Re,Ri,Fi,Fu]
    in  (infex,ttype,rtype,proof,hm) --return synthesized 

infer_TPExp :: Morphism  ->  (Maybe Sign) ->  InfExpression 
infer_TPExp subm usercast     decls_ isa_ autocast_ =  --TODO -> I and V must be added to decls_
    let {--calculate synthesized--}
        ttype = ptf (ds ac)
        rtype = case length(ds ac) of
              1 -> Left tf                               --TODO -> check not Anything; check+infer homo
              0 -> analyseerror
              _ -> Right ["AMB declaration",show(ds ac),show autocast_, show ac]             --AMB declaration means that there are at least two ds with the same type and same name
        infex = if null(ds ac)
                then error (show(rmap ("only TPExp infex if there is a declaration\n":) rtype)) --rtype yields type error
                else TPExp (head (ds ac)) (thetype rtype)
        proof = []
        hm = not$foldr (||) False [ isIdent d || null[()|p<-decprps d,elem p [Asy,Rfx,Sym,Trn]] |d<-ds usercast]
        --iff all (ds usercast) are homogeneous, then hm=True i.e. hm applies to ttype, not rtype. implemented: not(d is heterogeneous)
    in  (infex,ttype,rtype,proof,hm)                     --return synthesized
    where
    {--bind subm to decl--}
    ds (Just (a,b)) = [d|d<-decls_, name subm==name d, (sign d \\-\\ (a,b)) isa_] --bind subm to decl with free autocast
    ds Nothing      = [d|d<-decls_, name subm==name d]
    ac = if castcondition usercast autocast_ isa_ 
         then cast usercast autocast_                                   
         else error "No match with user cast"
    {--typing functions--}
    ptf = map sign
    tf | length (ds ac)==1 && ac==Nothing = (sign.head.ds) Nothing
       | length (ds ac)==1                = (\(Just cx) -> cx) ac
       | otherwise = error "tf undefined: tf is defined iff length (ds ac)==1"
    {--error analysis--}
    analyseerror 
       | null(ds Nothing) && chkundeclcpt = (\(Right err)-> Right ("No decl":err)) undeclcpt
       | chkundeclcpt = undeclcpt
       | otherwise = Right ["No decl"]
    undeclcpt = case usercast of
         Just (c1,c2) -> if not(elem c1 cpts) then Right ["No cpt c1"]
                         else if not(elem c2 cpts) then Right ["No cpt c2"]
                         else error "No undecl cpt -> chkundeclcpt first"
         Nothing      -> error "No user type -> chkundeclcpt first"
    chkundeclcpt = case usercast of
         Nothing -> False
         Just (c1,c2) -> not(elem c1 cpts) || not(elem c2 cpts)
    cpts = concs decls_++map fst isa_++map snd isa_
------------------------------------------------------------
------------------------------------------------------------

type OnIsa a = [(Concept,Concept)] -> a

--cptgE of Concept is still (==), so I can't use the functions order,glb,lub
(\-\) :: Concept -> Concept -> OnIsa Bool
(\-\) a b = (\isas -> elem (a,b) isas || elem (b,a) isas)
(\\-\\) :: Sign -> Sign -> OnIsa Bool
(\\-\\) (a,b) (c,d) = (\isas -> (a\-\c) isas && (b\-\d) isas)

lmap :: (a -> a) -> Either a b -> Either a b
lmap f (Left x) = Left (f x)
lmap _ y = y
rmap :: (b -> b) -> Either a b -> Either a b
rmap f (Right x) = Right (f x)
rmap _ y = y

castcondition :: Maybe Sign -> Maybe Sign -> OnIsa Bool
castcondition (Just x) (Just y) = x \\-\\ y
castcondition _ _ = (\_ -> True)
cast :: Maybe Sign -> Maybe Sign -> Maybe Sign
cast usercast@(Just _) _ = usercast
cast _ autocast = autocast

-- (\-/) :: Concept -> Concept -> OnIsa Concept
-- (\-/) a b | elem (
-- (/-\) :: Concept -> Concept -> OnIsa Concept

--TODO -> there exists a prelude function
jst (Just x) = x
jst Nothing = error "check ac/=Nothing"

--TODO-> check if wanted (used in composition on trytype, but not sure if I want this, see comments at ttype of composition)
rmdupl _ [] = []
rmdupl prets (t:ts) 
   | elem t prets || elem t ts =    rmdupl (t:prets) ts
   | otherwise                 = t:(rmdupl (t:prets) ts)

--the alternatives are organised by subexpression
--thus [[SynAtts]] contains lists of equal length <= the number of subexpressions
--TODO -> Haskell type solution  to ensure this (abstract type or something) 
--        Now it could just as well be a list of tuples per alternative <= card.prod.of current choice
--        Thus it is an implementation risk
ss_merge::[[[SynAtts]]]->[[SynAtts]]
ss_merge [] = []
ss_merge (x:[]) = x
ss_merge (x:xs) = foldr concatelems x xs 
concatelems::[[SynAtts]] -> [[SynAtts]] -> [[SynAtts]]
concatelems [] [] = []
concatelems [] _ = error "sdfds"
concatelems _ [] = error "sdfsd"
concatelems (x:xs) (y:ys) = --if length (concatelems xs ys)>3 then error (show ([map trytype ccc|ccc<-(x++y):(concatelems xs ys)])) else 
                            (x `Data.List.union` y):(concatelems xs ys)

testy (Just x) = Left x
testy _ = Right ["Anything"]

thetype :: Either Sign [TErr] -> Sign
thetype (Left t) = t
thetype (Right x) = error ("no type"++show x)
inerror :: Either Sign [TErr] -> Bool
inerror (Left _) = False
inerror (Right _) = True

------------------------------------------------------------
------------------------------------------------------------
--or reltype=Left type + infex exists or Right TErr
--comp_errors :: [[SynAtts]] -> [TErr]
--comp_errors [] = []
--comp_errors (s_tpls:ss_tplss) 
  -- | null ss = [s_tpls]
--  = --["composition"++show (ttypemerge ac (s:ss),length(comp_alts ttypemerge decls_ isa_ ac (s:ss)))]
  --  map reltype s_tpls 

--the type alternatives of some composition of a list of subexpressions
--one alternative is a list of SynAtts, exactly one SynAtt for each subexpression possibly yielding an error in the subexpression
--if there is no alternative yielding a type, then all alternatives yield errors
--if there is an alternative yielding a type, then all alternatives yield types
--if there are more alternatives yielding types, then the composition expression as a whole is ambiguous, 
--but it might be unambiguous within a larger expression (i.e. with a different ac)
--if there are more alternatives yielding errors, then it could be either one i.e. the actual mistake of the user is undecided,
--but the actual mistake might be decided within a larger expression (i.e. with a different ac)
--
--the goal is to return all alternatives: sss_tplss = s_tpls:ss_tplss
--where
--s_tpls => all alternatives for s where source=a and target="some source in ptf(ss) with target(ss)=c"
--ss_tplss (recursion) => all alternatives for each s' in ss
--                     = comp_alts ss 
--                       where ac=("some target of concat(map trytype s_tpls)", c)
--
--if there are no such s_tpls, then this could be caused by that:
--1) ss yields an error i.e. there are no ptf(ss)
--   => returning the tuple of s with source=a and open target is to general if there are target(s)="some source in ptf(take (ok_untill 0) ss)"
--      if there are such sources (b) then return for all b the tuple of s with source=a and target=b
--      otherwise check cause 2 
--      +> if s does compose to some left part of ss then ss_tplss=comp_alts (take (ok_untill 0) ss) ++ comp_alts (drop (ok_untill 0) ss)
--2) there is no s_tpl that can be composed to any ss_tpls 
--   "x can be composed to y" => there is a target(x)=source(y)
--   if there are alternatives for s i.e. trytype (s with source=a and open target) is not null
--   then explicitly cast all trytypes (_,b) to tuples of s with source=a and target=b <= interpretation:one tuple=one alternative
--   otherwise check cause 3
--3) there are no alternatives for s where source=a
--   => return the tuple of s with source=a and open target (yielding an error)
comp_alts :: (Maybe (Concept, Concept)-> [InfExpression]-> [(Concept, Concept)])
             -> [Declaration] -> [(Concept,Concept)] -> (Maybe Sign) -> [InfExpression] -> ([[SynAtts]],[TErr])
comp_alts _ _ _ _ [] = ([],[])
comp_alts ttypemerge decls_ isa_ ac (s:ss) --ttypemerge incorporates ptf; ac could be different for ttype and rtype
 --  | null ss = ([s_tpls],s_err)
   | length sss_tplss==length (s:ss) =(sss_tplss,sss_errs)
   | otherwise = error (show(length sss_tplss,length (s:ss),sss_errs)++"alternative requires at least one tuple for each s:ss")
   where
   --find the error in ss p.e. s=r,ss=s;t;v. s;ss yields error, but maybe r;s;t does not: ok_untill 0 = 2
   --(take (ok_untill 0) ss) yields no errors
   --(drop (ok_untill 0) ss) cannot be composed with (take (ok_untill 0) ss) maybe because head(drop (ok_untill 0) ss) yields an error
   --cast(source(take (ok_untill 0) ss)) must be in ok_bs 
   ok_untill n                         
      | null ss = 0
      | n+1==length ss = n+1 
      | null(ttypemerge Nothing (take (n+1) ss)) = n
      | n>length ss = error "n may not be greater than the length of ss"
      | otherwise = ok_untill (n+1)
   ok_bs 
      | length ss==ok_untill 0 = map fst ptf_ss --respect c in case Just (a,c)=ac
      | otherwise = map fst(ttypemerge Nothing (take (ok_untill 0) ss)) 
   ------------------------------------------------------------------------------------------------------
   ptf_ss::[Sign]   --the cast(source(ss)) is (still) unknown, target(ss) may be restricted by ac 
   ptf_ss          
      | null ss     = []                                                    --s is the last subexpression i.e. no composition i.e. ptf_ss=[]
      | ac==Nothing = ttypemerge Nothing ss                                     
      | otherwise   = let Just (_,c)=ac in ttypemerge (Just (Anything,c)) ss --some ss, cast(target(ss))=c
   ------------------------------------------------------------------------------------------------------
   (s_tpls,s_err)                      --the alternatives (tuples) for s, possibly yielding an error
    = (handle_error_in_s.handle_no_composition.handle_error_in_ss)
      [s  decls_ isa_ (Just(a,b))
      | (a,b)<-trytype trystpl         --if null (trytype trystpl) then s yields error (see handle_error_in_s)
      , if ac==Nothing || (a\-\fst(jst ac)) isa_ then True else error "condition must have been implemented by trystpl"
      , (b',c)<-ptf_ss                 --if null ptf_ss then there is no composition because ss yields error or null ss, 
                                       --if length ss>1 then maybe s could be composed with (take n<length ss);start n=1;n++
      , (b\-\b') isa_                  --if not null ptf_ss and there is no such b and b' then there is no composition because
                                       --the composition of s and ss is incompatible
      , if ac==Nothing || (c\-\snd(jst ac)) isa_ then True else error "condition must have been implemented by ptf_ss"
      ]
      where 
      trystpl                          --the cast(target(s)) is (still) unknown, source(s) may be restricted by ac 
         | ac==Nothing = s decls_ isa_ Nothing
         | null ss     = s decls_ isa_ ac                                      --s is the last subexpression, cast(type(s))=ac
         | otherwise   = let Just (a,_)=ac in s decls_ isa_ (Just(a,Anything)) --some s, cast(source(s))=a is composed to ss
      handle_error_in_ss []            --if ss yields error or null ss, try to reduce the number of s_tpls
 --        | null ptf_ss                 
           = [s  decls_ isa_ (Just(a,b))
             | (a,b)<-trytype trystpl  --if null (trytype trystpl) then s yields error
             , if ac==Nothing || (a\-\fst(jst ac)) isa_ then True else error "condition must have been implemented by trystplx"
             , b'<-ok_bs               --if null ok_bs then (head ss) yields error or null ss
             , (b\-\b') isa_           --if not null ok_bs and there is no such b and b' then there is no composition because
                                       --the composition of s and some (take n<length ss);start n=1;n++ is incompatible
             ]
      handle_error_in_ss xs = xs       --there is no error in ss composition and not null ss
      handle_no_composition []
           = [s  decls_ isa_ (Just(a,b))
             | (a,b)<-trytype trystpl  --if null (trytype trystpl) then s yields error
             , if ac==Nothing || (a\-\fst(jst ac)) isa_ then True else error "condition must have been implemented by trystply"
             ]
      handle_no_composition xs = xs    --there is a composition
      handle_error_in_s [] = case reltype trystpl of
             Right err -> ([],err)
             _ -> error "expecting type error in s" -- ([trystpl],["error in s"]) --s yields error -> return the tuple that yields the error i.e. trystpl
      handle_error_in_s xs = (xs,[])        --there is no error in s
   ------------------------------------------------------------------------------------------------------
   --sss_tplss::[[SynAtts]]          --the alternatives (lists of tuples) per subexpression in s:ss
   (sss_tplss,sss_errs) 
    = (s_tpls:ss_tplss
      ,s_err++ss_errs)             --error analysis rule: report all errors within a subexpression Ri
                                     --comp_alts is recursive so all Ri will be s_tpls possibly yielding s_err at sometime
   (ss_tplss,ss_errs) 
     -- | length ss==2 = error (show (null s_tpls,s_err,ptf_ss,ok_bs,s_ss_bs,s_trytypes))
      | null ss = ([],[])
      | null s_tpls && null s_err = error "there must be an alternative or error for s"
      | not(null s_tpls) && not(null s_err)  = error "there may not be both an alternative and error for s"
      --s yields error -> STEP 1 => no composition error, just error(s) of s and ss
      | null s_tpls && not(null ptf_ss) = --s yields error, ss does not or only amb comp
         (tpls ss_alts_ptf, errs ss_alts_ptf) 
      | null s_tpls && null ptf_ss = --s yields error, ss does too 
         (tpls ss_alts_no_ptf, if not(null(errs ss_alts_no_ptf)) then errs ss_alts_no_ptf else error "ss has no error1?")
      --s yields no error
      | null ptf_ss && ok_untill 0==0 = --head ss yields an error  -> STEP 1 => no composition error, just error(s) of ss
         (tpls ss_alts_no_ptf, if not(null(errs ss_alts_no_ptf)) then errs ss_alts_no_ptf else error "ss has no error2?")
        --ss yields an error, but there are ok_bs -> STEP2 => error(s) of ss + potential composition error of s and split1 --TODO merge multiple amb errors?
      | null ptf_ss && null s_ss_bs = --incompatible composition of s and split1
         (tpls ss_alts_no_ptf, if not(null(errs ss_alts_no_ptf)) then ("incompatible1_"++show (length ss)):(errs ss_alts_no_ptf) else error ("ss has no error3?"++show(ok_untill 0, ok_bs,s_trytypes,length s_ss_alts_no_ptf)))
      | null ptf_ss && length s_ss_bs==1 = -- s and split1 compose
         (tpls s_ss_alts_no_ptf, if not(null(errs s_ss_alts_no_ptf)) then errs s_ss_alts_no_ptf else error "ss has no error4?")
      | null ptf_ss && length s_ss_bs>1 = --ambiguous composition of s and split1 
         (tpls s_ss_alts_no_ptf, if not(null(errs s_ss_alts_no_ptf)) then ("ambiguous1_"++show (length ss)):(errs s_ss_alts_no_ptf) else error "ss has no error5?")
      --both s and ss yield no error or only amb comp -> STEP2 => potential composition error of s and ss + potential amb.comps in ss --TODO merge multiple amb errors?
      | null s_ss_bs = --incompatible composition of s and ss
         (tpls ss_alts_ptf,  ("incompatible2_"++show (length ss)):(show (ok_bs,s_trytypes)):(errs ss_alts_ptf))
      | length s_ss_bs==1 = -- s and ss compose
         (tpls s_ss_alts_ptf, errs s_ss_alts_ptf)
      | length s_ss_bs>1 = --ambiguous composition of s and ss, different alternatives may yield the same error => unduplicate
         (tpls s_ss_alts_ptf, ("ambiguous2_"++show (length ss)):(show (ok_bs,s_trytypes)):(errs s_ss_alts_ptf))
      | otherwise = error "are there other options?"

   tpls xs = ss_merge (map fst xs)--[x |Left x<-xs] --removes duplicate alternatives
   errs xs = (nub.concat) (map snd xs) --[x |Right x<-xs] --errors of all alternatives on a heap
   s_trytypes = [(a,b)| s_tpl<-s_tpls, (a,b)<-trytype s_tpl]
   s_ss_bs = nubBy (\x y->fst x==fst y) [(b,b')|(_,b)<-s_trytypes,b'<-ok_bs,(b\-\b') isa_]
   ss_alts_ptf = --ss_alts independent of s, when there are ptf_ss
      [comp_alts ttypemerge decls_ isa_ (Just (b,c)) ss
      |(b,c)<-ptf_ss]
   ss_alts_no_ptf = --ss_alts indepent of s, when ss yields error
      [comp_alts ttypemerge decls_ isa_ (if ac==Nothing then Nothing else let Just (_,c)=ac in (Just (Anything,c))) ss]
   s_ss_alts_ptf = --ss_alts dependent of s, when there are ptf_ss
      [comp_alts ttypemerge decls_ isa_ (Just (b',c)) ss
      | (_,b)<-s_trytypes
      , (b',c)<-ptf_ss
      , (b\-\b') isa_]
   s_ss_alts_no_ptf = --ss_alts dependent of s, when ss yields error
      [comp_alts ttypemerge decls_ isa_ (if ac==Nothing then (Just (b,Anything)) 
                                         else let Just (_,c)=ac in (Just (b,c))) ss
      | (_,b)<-s_ss_bs]
   ------------------------------------------------------------------------------------------------------
   --end of comp_alts------------------------------------------------------------------------------------
   ------------------------------------------------------------------------------------------------------


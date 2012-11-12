{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms 
  (conjNF,disjNF,normPA,nfProof,cfProof,dfProof,proofPA,simplify,distribute,isI)
where
   import DatabaseDesign.Ampersand.Basics    (fatalMsg,Identified(..),eqCl,eqClass,Flippable(..))
   import DatabaseDesign.Ampersand.Classes
   import DatabaseDesign.Ampersand.ADL1.ECArule
   import DatabaseDesign.Ampersand.ADL1.Expression 
   import DatabaseDesign.Ampersand.ADL1.MorphismAndDeclaration (Relational(..))
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import DatabaseDesign.Ampersand.Fspec.Fspec
   import Data.List (nub)

   fatal :: Int -> String -> a
   fatal = fatalMsg "Fspec.ToFspec.NormalForms"

{- Normalization of process algebra clauses -}

   normPA :: PAclause -> PAclause
   normPA expr = expr' 
       where (expr',_,_) = if null (proofPA expr) then fatal 21 "last: empty list" else last (proofPA expr)

   type Proof a = [(a, [String], String)]

   proofPA :: PAclause -> Proof PAclause
   proofPA = {-reverse.take 3.reverse.-}pPA
    where pPA expr' = case normstepPA expr' of
                       ( _ , []  ,equ) -> [(expr',[]   ,equ)]    -- is dus (expr,[],"<=>")
                       (res,steps,equ) -> (expr',steps,equ):pPA res

   normstepPA :: PAclause -> (PAclause,[String],String)
   normstepPA expr = (res,ss,"<=>")
    where
     (res,ss) = norm expr
     norm :: PAclause -> (PAclause,[String])
     norm (Chc [] ms)  = (Blk ms, ["Run out of options"])
     norm (Chc [r] ms) = (r', ["Flatten ONE"])
                       where r' = case r of
                                    Blk{} -> r
                                    _     -> r{paMotiv = ms} 
-- WHY? Stef, kan je uitleggen wat hier gebeurt? Enig commentaar is hier wel op zijn plaats.
-- Ook zou het helpen om bij de verschillende constructoren van PAclause een beschrijving te geven van het idee er achter. 
-- BECAUSE! Kan ik wel uitleggen, maar het is een heel verhaal. Dat moet tzt in een wetenschappelijk artikel gebeuren, zodat het er goed staat.
-- Het idee is dat een procesalgebra is weergegeven in Haskell combinatoren (gedefinieerd als PAclause(..), zie ADL.ECArule).
-- Die kun je vervolgens normaliseren met herschrijfregels op basis van gelijkheden die gelden in de bewuste procesalgebra.
-- Helaas zijn de herschrijfregels nu nog hard gecodeerd, zodat ik voor PAclause een afzonderlijke Haskell functie moet schrijven.
-- Hierna volgt de normalisator voor relatiealgebra-expressies, genaamd normStep. Die heeft dezelfde structuur,
-- maar gebruikt herschrijfregels vanuit gelijkheden die gelden in relatiealgebra.
     norm (Chc ds ms)  | (not.null) msgs = (Chc ops ms, msgs)
                       | (not.null) [d | d<-ds, isChc d] = (Chc (nub [ d' | d<-ds, d'<-if isChc d then let Chc ops' _ = d in ops' else [d] ]) ms, ["flatten Chc"])  -- flatten
                       | (not.null) [Nop | Nop{}<-ops] = (Nop{paMotiv=ms}, ["Choose to do nothing"])
                       | (not.null) [Blk | Blk{}<-ops] = (Chc [op | op<-ops, not (isBlk op)] ms, ["Choose anything but block"])
                       | otherwise = (Chc ds ms, [])
                       where nds  = map norm ds
                             msgs = concatMap snd nds
                             ops  = map fst nds
     norm (All [] ms)  = (Nop ms, ["All [] = No Operation"])
     norm (All [d] ms) = (d', ["Flatten ONE"])
                       where d' = case d of
                                    Blk{} -> d
                                    _     -> d{paMotiv = ms} 
     norm (All ds ms)  | (not.null) msgs = (All ops ms, msgs)
                       | (not.null) [d | d<-ds, isAll d] = (All (nub [ d' | d<-ds, d'<-if isAll d then let All ops' _ = d in ops' else [d] ]) ms, ["flatten All"])  -- flatten
                       | (not.null) [Blk | Blk{}<-ops] = (Blk{paMotiv = [m | op@Blk{}<-ops,m<-paMotiv op]}, ["Block all"])
                       | (not.null) [Nop | Nop{}<-ops] = (All [op | op<-ops, not (isNop op)] ms, ["Ignore Nop"])
                       | (not.null) long              = (All ds' ms, ["Take the expressions for "++commaEngString "and" [(name.head.(morlist::Expression->[Relation]).paTo.head) cl |cl<-long]++"together"])
                       | otherwise = (All ds ms, [])
                       where ds' = [ let p=head cl in
                                     if length cl==1 then p else p{paTo=disjNF (EUni[paDelta c | c<-cl]), paMotiv=concatMap paMotiv cl}
                                   | cl<-dCls ]
                                   ++[d | d<-ds, not (isDo d)]
                             nds  = map norm ds
                             msgs = concatMap snd nds
                             ops  = map fst nds
                             dCls :: [[PAclause]]
                             dCls = eqCl to [d | d<-ds, isDo d]
                             long :: [[PAclause]]
                             long = [cl | cl<-dCls, length cl>1]
                             to d@(Do{}) = (paSrt d,paTo d)
                             to _        = fatal 74 "illegal call of to(d)"
     norm (New c p ms)        = ( case p' of
                                   Blk{} -> p'{paMotiv = ms}
                                   _     -> New c (\x->let (p'', _) = norm (p x) in p'') ms
                                , msgs)
                                where (p', msgs) = norm (p "x")
     norm (Rmv c p ms)        = ( case p' of
                                   Blk{} -> p'{paMotiv = ms}
                                   _     -> Rmv c (\x->let (p'', _) = norm (p x) in p'') ms
                                , msgs)
                                where (p', msgs) = norm (p "x")
     norm (Sel c e p ms)      = ( case p' of
                                   Blk{} -> p'{paMotiv = ms}
                                   _     -> Sel c e (\x->let (p'', _) = norm (p x) in p'') ms
                                , msgs)
                                where (p', msgs) = norm (p "x")
     norm p                   = (p, [])

     commaEngString :: String -> [String] -> String
     commaEngString str [a,b,c]= a++", "++b++", "++str++" "++c
     commaEngString str [a,b]  = a++" "++str++" "++b
     commaEngString _ [a]    = a
     commaEngString str (a:as) = a++", "++commaEngString str as
     commaEngString _ []     = ""


{- Normalization of expressions -}

   simplify :: Expression -> Expression
   simplify expr = expr' 
       where (expr',_,_) = if null (simpProof shw expr) then fatal 101 "last: empty list" else last (simpProof shw expr)
             shw _ = ""
   
   simpProof :: (Expression -> String) -> Expression -> Proof Expression
   simpProof shw expr    
    = if expr==res
      then [(expr,[],"<=>")]
      else (expr,steps,equ):simpProof shw res
    where (res,steps,equ) = normStep shw True True expr

-- The purpose of "normStep" is to elaborate a single step in a rewrite process,
-- in which the expression is normalized by means of rewrite rules.
-- The form of the expression decides which rewrite rule is applicable.
   normStep :: (Expression -> String) -> Bool -> Bool ->
               Expression -> (Expression,[String],String) -- This might be generalized to "Expression" if it weren't for the fact that flip is embedded in the Relation type.
   normStep shw   -- a function to print an expression. Might be "showADL" or "(showADL . disambiguate fSpec)", or any other...
            eq    -- If eq==True, only equivalences are used. Otherwise, implications are used as well.
            simpl -- If True, only simplification rules are used, which is a subset of all rules. Consequently, simplification is implied by normalization.
            expr = (res,ss,equ)
    where
     (res,ss,equ) = nM expr []
--     nM :: Expression -> [Expression] -> (Expression,[String],String)
     nM (EEqu (l,r))     _      = (EIsc [EImp (l,r), EImp (r,l)], ["remove ="],"<=>")
     nM (EImp (x,ELrs (z,y))) _ = (EImp (ECps [x,y], z), ["remove left residual (/)"],"<=>")
     nM (EImp (y,ERrs (x,z))) _ = (EImp (ECps [x,y], z), ["remove right  residual (\\)"],"<=>")
     nM (EImp (l,r))     _      = (EUni [ECpl l, r], ["remove |-"],"<=>")
     nM (EKl0 e')        _      = (EKl0 res', steps, equ')
                                  where (res',steps,equ') = nM e' []
     nM (EKl1 e')        _      = (EKl1 res', steps, equ')
                                  where (res',steps,equ') = nM e' []
     nM (ECpl (ECpl e')) _      = (e', ["complement of complement"],"<=>")
     nM (ECpl (EIsc fs)) _      = if simpl then (notCpl res',steps,equ') else (EUni (map notCpl fs), ["De Morgan"], "<=>")
                                  where (res',steps,equ') = nM (EIsc fs) []
     nM (ECpl (EUni fs)) _      = if simpl then (notCpl res',steps,equ') else (EIsc (map notCpl fs), ["De Morgan"], "<=>")
                                  where (res',steps,equ') = nM (EUni fs) []
     nM (ECpl (ERad ts)) _ | not simpl && or [isNeg t | t<-ts] = (ECps (map notCpl ts), ["De Morgan"], "<=>")
                           | otherwise                         = let (res',steps,equ') = nM (ERad ts) [] in (notCpl res',steps,equ')
     nM (ECpl e')        _      = (notCpl res',steps,equ')
                                  where (res',steps,equ') = nM e' []
     nM (EBrk f)         _      = nM f []
     nM (ECps [t])       _ = nM t []
     nM (ECps (k:ks))   rs | or [isECps x |x<-k:ks]   = nM (ECps [y | x<-k:ks, y<-if isECps x then unE x else [x]]) rs  -- haakjes verwijderen o.g.v. associativiteit
                           | or [isI  x |x<-k:ks]   = (ECps [x |x<-k:ks,not (isI x)], ["x;I = x"], "<=>")
                   -- If only simplification is required, we are done now.
                           | simpl                  = (if isECps f then ECps (t:unE f) else ECps [t,f], steps++steps', fEqu [equ',equ''])
                           | not eq && length ks>1 && isERad g && length gs>1
                                                    = (ECps (ERad (ECps [k,head gs]:tail gs):ks'), ["Peirce: r;(s!q) => (r;s)!q"],"==>")
                           | not eq && isERad k && length ue>1
                                                    = (ERad (init ue++[ECps (last ue:ks)]), ["Peirce: (r!s);q => r!(s;q)"],"==>")
                           | not eq && isEIsc k     = (distribute ECps EIsc isECps isEIsc (ECps (k:ks)), ["distribute /\\ over ;"], "==>")
                           | isEUni k               = (distribute ECps EUni isECps isEUni (ECps (k:ks)), ["distribute \\/ over ;"], "<=>")
                           | and [isNeg x |x<-k:ks] = (notCpl (ERad [notCpl x | x<-k:ks]), ["De Morgan"], "<=>")
                           | isEUni (last (k:ks))   = (EUni [ECps (init (k:ks)++[t']) |EUni xs<-[last (k:ks)], t'<-xs], ["distr \\/ over ;"], "<=>")
                           | otherwise              = (if isECps f then ECps (t:unE f) else ECps [t,f], steps++steps', fEqu [equ',equ''])
                           where (t,steps, equ')    = nM k []
                                 (f,steps',equ'')   = nM (ECps ks) (k:rs)
                                 ue = unE k
                                 g@(ERad gs):ks' = ks

     nM (ERad [k])    _    = nM k []
     nM (ERad (k:ks)) rs   | or [isERad x |x<-k:ks] = nM (ERad [y | x<-k:ks, y<-if isERad x then unE x else [x]]) rs
                           | or [isImin x |x<-k:ks] = (ECps [x |x<-k:ks,not (isImin x)], ["x!-I = x"], "<=>")
                   -- If only simplification is required, we are done now.
                           | simpl                  = (if isERad f then ERad (t:unE f) else ERad [t,f], steps++steps', fEqu [equ',equ''])
                           | not eq && isEUni k     = (distribute ERad EUni isERad isEUni (ERad (k:ks)), ["distribute \\/ over !"], "==>")
                           | isEIsc k               = (distribute ERad EIsc isERad isEIsc (ERad (k:ks)), ["distribute /\\ over !"], "<=>")
                           | and [isNeg x |x<-k:ks] = (notCpl (ECps [notCpl x | x<-k:ks]), ["De Morgan"], "<=>")
                           | length ks>1 && isNeg k && isPos g && isFunction k
                                                    = (ECps [notCpl k,ERad ks], ["f-!g = f;g if f is a function"], "<=>")
                           | length ks>1 && isPos k && isNeg g && isFunction (flp g)
                                                    = (ERad ( ECps[k,notCpl g]:ks'), ["f!g- = f;g if g~ is a function"], "<=>")
                           | otherwise              = (if isERad f then ERad (t:unE f) else ERad [t,f], steps++steps', fEqu [equ',equ''])
                           where (t,steps, equ')  = nM k []
                                 (f,steps',equ'') = nM (ERad ks) (k:rs)
                                 g:ks' = ks
     nM (EPrd [k])    _    = nM k []
     nM (EPrd (k:ks)) rs   | or [isEPrd x |x<-k:ks]    = nM (EPrd [y | x<-k:ks, y<-if isEPrd x then unE x else [x]]) rs
                           | length (k:ks)/=length ks' = (EPrd ks', ["eliminate cartesian product"], "<=>")
                           | otherwise                 = (if isEPrd f then EPrd (t:unE f) else EPrd [t,f], steps++steps', fEqu [equ',equ''])
                           where ks' = nub [k,last (k:ks)]
                                 (t,steps, equ')  = nM k []
                                 (f,steps',equ'') = nM (EPrd ks) (k:rs)
     nM (EIsc [k]) _   = nM k []
     nM (EUni [k]) _   = nM k []
     nM (EIsc (k:ks)) rs
                     -- Associativity of /\:    (r/\s)/\t  -->  r/\s/\t      (implicit step)
                            | or [isEIsc x |x<-k:ks] = nM (EIsc [y | x<-k:ks, y<-if isEIsc x then unE x else [x]]) rs
                     -- If only simplification is required, we are done now.
                            | simpl                  = (if isEIsc f then EIsc (t:unE f) else EIsc [t,f], steps++steps', fEqu [equ',equ''])
                     -- Absorb equals:    r/\r  -->  r
                            | or [length cl>1 |cl<-absor3]
                                                     = (EIsc [head cl | cl<-absor3], [shw e++" /\\ "++shw e++" = "++shw e | cl<-absor3, length cl>1, let e=head cl], "<=>")
                     -- Inconsistency:    r/\-r   -->  False
                            | not (null incons)      = (EUni [], [shw (notCpl (head incons))++" /\\ "++shw (head incons)++" = V-"], "<=>")
                     -- Inconsistency:    EUni []   -->  False
                            | k==EUni []             = (EUni [], ["inconsistency"], "<=>")
  -- this is unreachable    | k==EIsc []             = (EIsc ks, ["x/\\V = x"], "<=>")
                     -- Inconsistency:    x/\\V-  -->  False
                            | or[x==EUni [] |x<-ks]  = (EUni [], ["x/\\V- = V-"], "<=>")
                     -- Absorb if r is antisymmetric:    r/\r~  -->  I    (note that a reflexive r incurs r/\r~ = I)
                            | or [length cl>1 |cl<-absor2]
                                                     = ( EUni [if length cl>1 then ERel (I (source e))  else e | cl<-absor2, let e=head cl]
                                                       , [shw e++" /\\ "++shw (flp e)++" = I, because"++shw e++" is antisymmetric" | cl<-absor2, let e=head cl]
                                                       , if and [isRfx (head cl) | cl<-absor2, length cl>1] then "<=>" else "==>"
                                                       )
                     -- Absorb:    (x\\/y)/\\y  -->  y
                            | isEUni k && not (null absor0) = let f'=head absor0 in (EIsc ks, ["absorb "++shw k++" because of "++shw f'++" ((x\\/y)/\\y = y))"], "<=>")
                     -- Absorb:    (x\\/-y)/\\y  -->  x/\\y
                            | isEUni k && not (null absor1) = let (ts,f')=head absor1 in (EIsc (ts++ks), ["absorb "++shw f'], "<=>")
                            | otherwise                   = (if isEIsc f then EIsc (t:unE f) else EIsc [t,f], steps++steps', fEqu [equ',equ''])
                            where (t,steps, equ')  = nM k []
                                  (f,steps',equ'') = nM (EIsc ks) (k:rs)
                                  incons = [x |x<-ks,x==notCpl k]
                                  absor0 = [t' | t'<-unE k, f'<-ks++rs, t'==f']
                                  absor1 = [(if length rest<=1 then rest else [EUni rest] , t') | t'<-unE k, f'<-ks++rs, notCpl t'==f', let rest = [x |x<-unE k,x/=t']]
                                  absor2 = eqClass same (rs++k:ks) where e `same` e' = (isRfx e && isAsy e && isRfx e' && isAsy e') && (e == flp e')
                                  absor3 = eqClass (==) (rs++k:ks)
     nM (EUni (k:ks)) rs
                     -- Associativity of \/:    (r\/s)\/t  -->  r\/s\/t      (implicit step)
                            | or [isEUni x |x<-k:ks]       = nM (EUni [y | x<-k:ks, y<-if isEUni x then unE x else [x]]) rs
                     -- If only simplification is required, we are done now.
                            | simpl                        = (if isEUni f then EUni (t:unE f) else EUni [t,f], steps++steps', fEqu [equ',equ''])
                     -- Absorb equals:    r\/r  -->  r
                            | or [length cl>1 |cl<-absor3] = (EUni [head cl | cl<-absor3], [shw e++"\\/"++shw e++" = "++shw e | cl<-absor3, length cl>1, let e=head cl], "<=>")
                     -- Tautology:    r\/-r  -->  V
                            | or [length cl>1 |cl<-absor2] = let ncp (ECpl e) = e; ncp e = e in
                                                            (EIsc [], take 1 ["let "++ shw (ncp e)++" = e. Since -e\\/e = V we get"
                                                                           | cl<-absor2, length cl>1, let e=head cl], "<=>")
                     -- Absorb -V:    r\/-V  --> r
                            | or[x==EUni [] |x<-k:ks]      = (EUni [x |x<-k:ks,x/=EUni []], ["x\\/-V = x"], "<=>")
                     -- Tautology:    r\/V  -->  V
                            | k==EIsc []                   = (EIsc [], ["tautology"],  "<=>")
                            | or[x==EIsc [] |x<-ks]        = (EIsc [], ["x\\/V = V"], "<=>")
                            | isEIsc k && not (null absor0)= let f'=head absor0 in (EUni ks, ["absorb "++shw k++" because of "++shw f'++" ((x/\\y)\\/y = y))"], "<=>")
                            | isEIsc k && not (null absor1)= let (ts,f')=head absor1 in (EUni (ts++ks), ["absorb "++shw f'++" ((x/\\y-)\\/y = x\\/y))"], "<=>")
                            | otherwise                    = (if isEUni f then EUni (t:unE f) else EUni [t,f], steps++steps', fEqu [equ',equ''])
                            where (t,steps, equ')  = nM k []
                                  (f,steps',equ'') = nM (EUni ks) (k:rs)
                                  absor0 = [t' | t'<-unE k, f'<-ks++rs, t'==f']
                                  absor1 = [(if length rest<=1 then rest else [EIsc rest] , t') | t'<-unE k, f'<-ks++rs, notCpl t'==f', rest<-[[x |x<-unE k,x/=t']]]
                                  absor2 = eqClass same (rs++k:ks) where e `same` e' = e==notCpl e'
                                  absor3 = eqClass (==) (rs++k:ks)
     nM (EFlp e) _          | isSym e =  (e,[shw e++" is symmetric"],"<=>")
     -- disabled nm (ERel r) _ because it is buggy for Mp1 relations 
     --nM (ERel r) _          | isEq && not (isIdent r)   = if isRfx r
     --                                                   then (EIsc [ERel (I (source r))], [name r++" is an equivalence relation"], "<=>")
     --                                                   else (EIsc [ERel (I (source r))], [name r++" is transitive and symmetric"], "==>")
     --                                                   where isEq = isTrn r && isSym r
     nM x _               = (x,[],"<=>")


   fEqu :: [String] -> String
   fEqu ss = if and [s=="<=>" | s<-ss] then "<=>" else "==>"

   unE :: Expression -> [Expression]
   unE (EIsc es')  = es'
   unE (EUni es')  = es'
   unE (ECps es')  = es'
   unE (ERad es')  = es'
   unE (EPrd es')  = es'
   unE x           = [x]

{- 
Distribution
To distribute one operator (g) over another (f) results in an expression 
that has g as its root (in all cases).
so  distribute EIsc EUni isEIsc isEUni (EIsc [r, EUni [s,t]]) = EUni [EIsc [r,s], EIsc[r,s]]
and distribute EUni EIsc isEUni isEIsc (EIsc [r, EUni [s,t]]) = EIsc [EUni [r], EUni [s,s]]
-}

   distribute :: ([Expression] -> Expression)
              -> ([Expression] -> Expression)
              -> (Expression -> Bool)
              -> (Expression -> Bool)
              -> Expression
              -> Expression
   distribute f g isf isg = dis
    where
     dis x | isf x && null xs      = g [f []]
           | isg x && null xs      = g []
           | isf x && length xs==1 = dis k
           | isg x && length xs==1 = dis k
           | isg x && isg k        = dis (g (ys++ks))
           | isf x && isf k        = dis (f (ys++ks))
           | isf x && isg k        = g [f [p,q] | p<-unE k, q<-unE (dis (f ks))]
           | isf x && isf k        = g [f [k,q] |           q<-unE (dis (f ks))]
           | isg x                 = g (unE (dis k)++ks)
           | otherwise             = g [x]             
           where xs = unE x
                 k:ks = xs
                 ys = unE k
   
{- klad: De volgende tekst is bedoeld als "uitleg". Ik heb de f vervangen door plus en de g door maal, wat iets minder abstract leest dan f en g.
     dis x | isPlus x && null xs      = maal [0]                                            -- correct want plus [] = maal [0]
           | isMaal x && null xs      = maal []                                             -- correct want maal [] = maal []
           | isPlus x && length xs==1 = dis k                                               -- correct want plus [x] = x
           | isMaal x && length xs==1 = dis k                                               -- correct want maal [x] = x
           | isMaal x && isMaal k     = dis (maal (ys++ks))                                 -- correct want maal is associatief
           | isPlus x && isPlus k     = dis (plus (ys++ks))                                 -- correct want plus is associatief
           | isPlus x && isMaal k     = maal [plus [p,q] | p<-ys , q<-unE (dis (plus ks))]   -- correct want plus distribueert over maal
           | isPlus x                 = maal [plus [k,q] |         q<-unE (dis (plus ks))]   -- correct want plus distribueert over maal
           | isMaal x                 = maal (unE (dis k)++ks)                              -- correct want transformatieloze recursie
           | otherwise                = maal [x]                                            -- correct want x = maal [x]
           where xs = unE x
                 k:ks = xs
                 ys = unE k
-}

   nfProof :: (Expression -> String) -> Expression -> Proof Expression
   nfProof shw = nfPr shw True -- The boolean True means that clauses are derived using <=> derivations.
   nfPr :: (Expression -> String) -> Bool-> Expression -> [(Expression, [String], String)]
   nfPr shw eq expr
    = if expr==res
      then [(expr,[],"<=>")]
      else (expr,steps,equ):nfPr shw eq (simplify res)
    where (res,steps,equ) = normStep shw eq False expr

   cfProof :: (Expression -> String) -> Expression -> Proof Expression
   cfProof shw expr
    = [line | step, line<-init pr]++
      [line | step', line<-init pr']++
      [line | step'', line<-init pr'']++
      [last ([(expr,[],"<=>")]++
             [line | step, line<-pr]++
             [line | step', line<-pr']++
             [line | step'', line<-pr'']
            )]
      where pr            = nfPr shw True (simplify expr)
            (expr',_,_)   = if null pr then fatal 328 "last: empty list" else last pr
            step          = simplify expr/=expr' || and [null s | (_,ss,_)<-pr, s<-ss]
            expr''        = simplify (distribute EUni EIsc isEUni isEIsc expr')   -- Distribute:    (x/\y)\/z  -->  x\/z /\ y\/z
            pr'           = if or [isEIsc f |EUni fs<-[expr'], f<-fs] 
                            then [(expr',["Distribute:    (x/\\y)\\/z  <=>  x\\/z /\\ y\\/z"],"<=>"),(expr'',[],"<=>")]
                            else [(expr',[],"<=>")]
            step'         = expr'/=expr'' || and [null s | (_,ss,_)<-pr', s<-ss]
            pr''          = nfPr shw True expr''
            step''        = expr''/=expr''' || and [null s | (_,ss,_)<-pr'', s<-ss]
            (expr''',_,_) = if null pr'' then fatal 337 "last: empty list" else last pr''

   conjNF :: Expression -> Expression
   conjNF expr = e where (e,_,_) = if null (cfProof (\_->"") expr) then fatal 340 "last: empty list" else last (cfProof (\_->"") expr)

   disjNF :: Expression -> Expression
   disjNF expr = e where (e,_,_) = if null (dfProof (\_->"") expr) then fatal 343 "last: empty list" else last (dfProof (\_->"") expr)

   dfProof :: (Expression -> String) -> Expression -> Proof Expression
   dfProof shw expr
    = [line | step, line<-init pr]++
      [line | step', line<-init pr']++
      [line | step'', line<-init pr'']++
      [last ([(expr,[],"<=>")]++
             [line | step, line<-pr]++
             [line | step', line<-pr']++
             [line | step'', line<-pr'']
            )]
      where pr            = nfPr shw True expr
            (expr',_,_)   = if null pr then fatal 356 "last: empty list" else last pr
            step          = simplify expr/=simplify expr'
            expr''        = distribute EIsc EUni isEIsc isEUni expr'   -- Distribute:    (x\/y)/\z  -->  x/\z \/ y/\z
            pr'           = if or [isEUni f|EIsc fs<-[expr'], f<-fs] 
                            then [(expr',["Distribute:    (x\\/y)/\\z  <=>  x/\\z \\/ y/\\z"],"<=>"),(expr'',[],"<=>")]
                            else [(expr',[],"<=>")]
            step'         = simplify expr'/=simplify expr''
            pr''          = nfPr shw True expr''
            step''        = simplify expr''/=simplify expr'''
            (expr''',_,_) = if null pr'' then fatal 365 "last: empty list" else last pr''
            
   -- TODO: @Stef: Why is this needed? --Why not use isIdent ???  Deal with ticket #147
   isI :: Expression -> Bool
   isI (ERel r ) = isIdent r
   isI (EBrk e ) = isI e
   isI (ECps ts) = all isI ts
   isI (EIsc fs) = all isI fs
   isI (EUni fs) = all isI fs
   isI (EKl0 e ) = isI e
   isI (EKl1 e ) = isI e
   isI (EFlp e ) = isI e
   isI _ = False            

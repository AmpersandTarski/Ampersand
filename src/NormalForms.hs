{-# OPTIONS_GHC -Wall #-}
module NormalForms (conjNF,disjNF,normPA,nfProof,cfProof,dfProof,proofPA,nfPr,simplify,distribute)
where
--   import Adl
   import Strings        (commaEng)
   import Collection     (Collection (..))
   import Auxiliaries    (eqCl,eqClass)
   import ShowADL        (showADL)
   import Adl.ECArule    (PAclause(..),isAll,isChc,isBlk,isNop,isDo)
   import Adl.Expression 
   import Adl.MorphismAndDeclaration (inline,mIs)
   import Adl.Concept
   import CommonClasses
   import Classes.Morphical
{- Normalization of process algebra clauses -}

   normPA :: PAclause -> PAclause
   normPA expr = expr' 
       where (expr',_,_) = last (proofPA expr)

   type Proof a = [(a, [String], String)]

   proofPA :: PAclause -> Proof PAclause
   proofPA expr = ({-reverse.take 3.reverse.-}pPA) expr
    where pPA expr' = case normstepPA expr' of
                       ( _ , []  ,equ) -> [(expr',[]   ,equ)]    -- is dus (expr,[],"<=>")
                       (res,steps,equ) -> [(expr',steps,equ)]++pPA res

   normstepPA :: PAclause -> (PAclause,[String],String)
   normstepPA expr = (res,ss,"<=>")
    where
     (res,ss) = norm expr
     norm :: PAclause -> (PAclause,[String])
     norm (Chc [] ms)  = (Blk ms, ["Run out of options"])
     norm (Chc [d] ms) = (d', ["Flatten Singleton"])
                       where d' = case d of
                                    Blk{} -> d
                                    _     -> d{paMotiv = ms} 
-- DAAROM? Stef, kan je uitleggen wat hier gebeurt? Enig commentaar is hier wel op zijn plaats. Ook zou het helpen om bij de verschillende constructoren van PAclause een beschrijving te geven van het idee er achter. 
-- Kan ik wel uitleggen, maar het is een heel verhaal. Dat moet tzt in een wetenschappelijk artikel gebeuren, zodat het er goed staat.
-- Het idee is dat een procesalgebra is weergegeven in Haskell combinatoren (gedefinieerd als PAclause(..), zie Adl.ECArule).
-- Die kun je vervolgens normaliseren met herschrijfregels op basis van gelijkheden die gelden in de bewuste procesalgebra.
-- Helaas zijn de herschrijfregels nu nog hard gecodeerd, zodat ik voor PAclause een afzonderlijke Haskell functie moet schrijven.
-- Hierna volgt de normalisator voor relatiealgebra-expressies, genaamd normStep. Die heeft dezelfde structuur,
-- maar gebruikt herschrijfregels vanuit gelijkheden die gelden in relatiealgebra.
     norm (Chc ds ms)  | (not.null) msgs = (Chc ops ms, msgs)
                       | (not.null) [d| d<-ds, isChc d] = (Chc (rd [ d' | d<-ds, d'<-if isChc d then let Chc ops' _ = d in ops' else [d] ]) ms, ["flatten Chc"])  -- flatten
                       | (not.null) [Nop| Nop{}<-ops] = (Nop{paMotiv=ms}, ["Choose to do nothing"])
                       | (not.null) [Blk| Blk{}<-ops] = (Chc [op| op<-ops, not (isBlk op)] ms, ["Choose anything but block"])
                       | otherwise = (Chc ds ms, [])
                       where nds  = map norm ds
                             msgs = (concat.map snd) nds
                             ops  = map fst nds
     norm (All [] ms)  = (Nop ms, ["All [] = No Operation"])
     norm (All [d] ms) = (d', ["Flatten Singleton"])
                       where d' = case d of
                                    Blk{} -> d
                                    _     -> d{paMotiv = ms} 
     norm (All ds ms)  | (not.null) msgs = (All ops ms, msgs)
                       | (not.null) [d| d<-ds, isAll d] = (All (rd [ d' | d<-ds, d'<-if isAll d then let All ops' _ = d in ops' else [d] ]) ms, ["flatten All"])  -- flatten
                       | (not.null) [Blk| Blk{}<-ops] = (Blk{paMotiv = [m| op@Blk{}<-ops,m<-paMotiv op]}, ["Block all"])
                       | (not.null) [Nop| Nop{}<-ops] = (All [op| op<-ops, not (isNop op)] ms, ["Ignore Nop"])
                       | (not.null) long              = (All ds' ms, ["Take the expressions for "++commaEng "and" [(name.head.morlist.paTo.head) cl|cl<-long]++"together"])
                       | otherwise = (All ds ms, [])
                       where ds' = [ let p=head cl in
                                     if length cl==1 then p else p{paTo=disjNF (Fu[paDelta c| c<-cl]), paMotiv=concat (map paMotiv cl)}
                                   | cl<-dCls ]
                                   ++[d| d<-ds, not (isDo d)]
                             nds  = map norm ds
                             msgs = (concat.map snd) nds
                             ops  = map fst nds
                             dCls = eqCl to [d| d<-ds, isDo d]
                             long = [cl| cl<-dCls, length cl>1]
                             to d@(Do{}) = (paSrt d,paTo d)
                             to _        = error("!Fatal (module NormalForms 70): illegal call of to(d)")
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

{- Normalization of expressions -}

   simplify :: Expression -> Expression
   simplify expr = expr' 
       where (expr',_,_) = last (simpProof expr)
   
   simpProof :: Expression -> Proof Expression
   simpProof expr    
    = if expr==res
      then [(expr,[],"<=>")]
      else (expr,steps,equ):simpProof res
    where (res,steps,equ) = normStep True True expr

   normStep :: Bool -> Bool -> Expression -> (Expression,[String],String)
   normStep eq    -- If eq==True, only equivalences are used. Otherwise, implications are used as well.
            simpl -- If True, only simplification rules are used, which is a subset of all rules. Consequently, simplification is implied by normalization.
            expr = (res,ss,equ)
    where
     (res,ss,equ) = nM expr []
     nM :: Expression -> Expressions -> (Expression,[String],String)
     nM (K0 e')      _    = (K0 res', steps, equ')
                             where (res',steps,equ') = nM e' []
     nM (K1 e')      _    = (K1 res', steps, equ')
                             where (res',steps,equ') = nM e' []
     nM (Cp (Cp e')) _    = (e', ["compl of compl"],"<=>")
     nM (Cp (Fi fs)) _    = if simpl then (notCp res',steps,equ') else (Fu (map notCp fs), ["De Morgan"], "<=>")
                             where (res',steps,equ') = nM (Fi fs) []
     nM (Cp (Fu fs)) _    = if simpl then (notCp res',steps,equ') else (Fi (map notCp fs), ["De Morgan"], "<=>")
                             where (res',steps,equ') = nM (Fu fs) []
     nM (Cp (Fd ts)) _    = if not simpl && and [isNeg t| t<-ts] 
                               then (F (map notCp ts), ["De Morgan"], "<=>")
                               else (notCp res',steps,equ')
                             where (res',steps,equ') = nM (Fd ts) []
     nM (Cp e')      _    = (notCp res',steps,equ')
                             where (res',steps,equ') = nM e' []
     nM (Tc f)       _    = nM f []
     nM (F [t])      _    = nM t []
     nM (F (k:ks))   rs   | or [isF x|x<-k:ks]      = nM (F [y| x<-k:ks, y<-if isF x then unF x else [x]]) rs  -- haakjes verwijderen o.g.v. associativiteit
                          | or [isIdent x|x<-k:ks]  = (F [x|x<-k:ks,not (isIdent x)], ["x;I = x"], "<=>")
                   -- If only simplification is required, we are done now.
                          | simpl                   = (if isF f then F (t:unF f) else F [t,f], steps++steps', fEqu [equ',equ''])
                          | not eq && length ks>1 && isFd g && length gs>1
                                                    = (F (Fd (F [k,head gs]:tail gs):ks'), ["Peirce: r;(s!q) => (r;s)!q"],"==>")
                          | not eq && isFd k && length ue>1
                                                    = (Fd (init ue++[F (last ue:ks)]), ["Peirce: (r!s);q => r!(s;q)"],"==>")
                          | not eq && isFi k        = (distribute F Fi isF isFi (F (k:ks)), ["distribute /\\ over ;"], "==>")
                          | isFu k                  = (distribute F Fu isF isFu (F (k:ks)), ["distribute \\/ over ;"], "<=>")
                          | and [isNeg x|x<-(k:ks)] = (notCp (Fd [notCp x| x<-(k:ks)]), ["De Morgan"], "<=>")
                          | isFu (last (k:ks))      = (Fu [F (init (k:ks)++[t'])|Fu xs<-[last (k:ks)], t'<-xs], ["distribute \\/ over ;"], "<=>")
                          | otherwise               = (if isF f then F (t:unF f) else F [t,f], steps++steps', fEqu [equ',equ''])
                          where (t,steps, equ')  = nM k []
                                (f,steps',equ'') = nM (F ks) (k:rs)
                                ue = unF k
                                g@(Fd gs):ks' = ks

     nM (Fd [k])    _     = nM k []
     nM (Fd (k:ks)) rs    | or [isFd x|x<-k:ks]     = nM (Fd [y| x<-k:ks, y<-if isFd x then unF x else [x]]) rs
                          | or [isNot x|x<-k:ks]    = (F [x|x<-k:ks,not (isNot x)], ["x!-I = x"], "<=>")
                   -- If only simplification is required, we are done now.
                          | simpl                   = (if isFd f then Fd (t:unF f) else Fd [t,f], steps++steps', fEqu [equ',equ''])
                          | not eq && isFu k        = (distribute Fd Fu isFd isFu (Fd (k:ks)), ["distribute \\/ over !"], "==>")
                          | isFi k                  = (distribute Fd Fi isFd isFi (Fd (k:ks)), ["distribute /\\ over !"], "<=>")
                          | and [isNeg x|x<-(k:ks)] = (notCp (F [notCp x| x<-(k:ks)]), ["De Morgan"], "<=>")
                          | length ks>1 && isNeg k && isPos g && isFunction k
                                                    = (F [notCp k,Fd ks], ["f-!g = f;g if f is a function"], "<=>")
                          | length ks>1 && isPos k && isNeg g && isFunction (flp g)
                                                    = (Fd ( F[k,notCp g]:ks'), ["f!g- = f;g if g~ is a function"], "<=>")
                          | otherwise               = (if isFd f then Fd (t:unF f) else Fd [t,f], steps++steps', fEqu [equ',equ''])
                          where (t,steps, equ')  = nM k []
                                (f,steps',equ'') = nM (Fd ks) (k:rs)
                                g:ks' = ks
     nM (Fi [k]) _   = nM k []
     nM (Fu [k]) _   = nM k []
     nM (Fi (k:ks)) rs
                     -- Associativity of /\:    (r/\s)/\t  -->  r/\s/\t      (implicit step)
                            | or [isFi x|x<-k:ks]         = nM (Fi [y| x<-k:ks, y<-if isFi x then unF x else [x]]) rs
                     -- If only simplification is required, we are done now.
                            | simpl                       = (if isFi f then Fi (t:unF f) else Fi [t,f], steps++steps', fEqu [equ',equ''])
                     -- Absorb equals:    r/\r  -->  r
                            | or [length cl>1|cl<-absor3] = (Fi [head cl| cl<-absor3], [showADL e++"/\\"++showADL e++" = "++showADL e| cl<-absor3, length cl>1, let e=head cl], "<=>")
                     -- Inconsistency:    r/\-r   -->  False
                            | not (null incons)           = (Fu [], [showADL (notCp (head incons))++"/\\"++showADL (head incons)++" = V-"], "<=>")
                     -- Inconsistency:    Fu []   -->  False
                            | k==Fu []                    = (Fu [], ["inconsistency"], "<=>")
  -- this is unreachable    | k==Fi []                    = (Fi ks, ["x/\\V = x"], "<=>")
                     -- Inconsistency:    x/\\V-  -->  False
                            | or[x==Fu []|x<-ks]          = (Fu [], ["x/\\V- = V-"], "<=>")
                     -- Absorb if r is antisymmetric:    r/\r~  -->  I    (note that a reflexive r incurs r/\r~ = I)
                            | or [length cl>1|cl<-absor2] = ( Fu [if length cl>1 then Tm (mIs (source e)) (-1) else e| cl<-absor2, let e=head cl]
                                                            , [showADL e++"/\\"++showADL (flp e)++" = I, because"++showADL e++" is antisymmetric"| cl<-absor2, let e=head cl]
                                                            , if and [isRfx (head cl)| cl<-absor2, length cl>1] then "<=>" else "==>"
                                                            )
                     -- Absorb:    (x\\/y)/\\y  -->  y
                            | isFu k && not (null absor0) = let f'=head absor0 in (Fi ks, ["absorb "++showADL k++" because of "++showADL f'++" ((x\\/y)/\\y = y))"], "<=>")
                     -- Absorb:    (x\\/-y)/\\y  -->  x/\\y
                            | isFu k && not (null absor1) = let (ts,f')=head absor1 in (Fi (ts++ks), ["absorb "++showADL f'], "<=>")
                            | otherwise                   = (if isFi f then Fi (t:unF f) else Fi [t,f], steps++steps', fEqu [equ',equ''])
                            where (t,steps, equ')  = nM k []
                                  (f,steps',equ'') = nM (Fi ks) (k:rs)
                                  incons = [x|x<-ks,x==notCp k]
                                  absor0 = [t'| t'<-unF k, f'<-ks++rs, t'==f']
                                  absor1 = [(if length rest<=1 then rest else [Fu rest] , t')| t'<-unF k, f'<-ks++rs, notCp t'==f', let rest = [x|x<-unF k,x/=t']]
                                  absor2 = eqClass same (rs++k:ks) where e `same` e' = if isRfx e && isAsy e && isRfx e' && isAsy e' then e==flp e' else False
                                  absor3 = eqClass (==) (rs++k:ks)
     nM (Fu (k:ks)) rs
                     -- Associativity of \/:    (r\/s)\/t  -->  r\/s\/t      (implicit step)
                            | or [isFu x|x<-k:ks]         = nM (Fu [y| x<-k:ks, y<-if isFu x then unF x else [x]]) rs
                     -- If only simplification is required, we are done now.
                            | simpl                       = (if isFu f then Fu (t:unF f) else Fu [t,f], steps++steps', fEqu [equ',equ''])
                     -- Absorb equals:    r\/r  -->  r
                            | or [length cl>1|cl<-absor3] = (Fu [head cl| cl<-absor3], [showADL e++"\\/"++showADL e++" = "++showADL e| cl<-absor3, length cl>1, let e=head cl], "<=>")
                     -- Tautology:    r\/-r  -->  V
                            | or [length cl>1|cl<-absor2] = let ncp (Cp e) = e; ncp e = e in
                                                            (Fi [], take 1 [if length (morlist e)>1 then "let "++ showADL (ncp e)++" = e. Since -e\\/e = V we get" else showADL (notCp e)++"\\/"++showADL e++" = V"| cl<-absor2, length cl>1, let e=head cl], "<=>")
                            | k==Fi []                    = (Fi [], ["tautology"], "<=>")
  -- this is unreachable    | k==Fu []                    = (Fu ks, ["x\\/V- = x"], "<=>")
                     -- Tautology:    r\/V  -->  V
                            | or[x==Fi []|x<-ks]          = (Fi [], ["x\\/V = V"], "<=>")
                            | isFi k && not (null absor0) = let f'=head absor0 in (Fu ks, ["absorb "++showADL k++" because of "++showADL f'++" ((x/\\y)\\/y = y))"], "<=>")
                            | isFi k && not (null absor1) = let (ts,f')=head absor1 in (Fu (ts++ks), ["absorb "++showADL f'++" ((x/\\y-)\\/y = x\\/y))"], "<=>")
                            | otherwise                   = (if isFu f then Fu (t:unF f) else Fu [t,f], steps++steps', fEqu [equ',equ''])
                            where (t,steps, equ')  = nM k []
                                  (f,steps',equ'') = nM (Fu ks) (k:rs)
                                  absor0 = [t'| t'<-unF k, f'<-ks++rs, t'==f']
                                  absor1 = [(if length rest<=1 then rest else [Fi rest] , t')| t'<-unF k, f'<-ks++rs, notCp t'==f', rest<-[[x|x<-unF k,x/=t']]]
                                  absor2 = eqClass same (rs++k:ks) where e `same` e' = e==notCp e'
                                  absor3 = eqClass (==) (rs++k:ks)
     nM (Tm m n) _        | isSym m && not (inline m) =  (Tm (flp m) n,[name m++" is symmetric"],"<=>")
                   -- Equivalence relation:    r  -->  I   if r is reflexive, transitive and symmetric.
                          | isEq && not (isIdent m)   = if isRfx m
                                                        then (Fi [Tm (mIs (source m)) n], [showADL m++" is an equivalence relation"], "<=>")
                                                        else (Fi [Tm (mIs (source m)) n], [showADL m++" is transitive and symmetric"], "==>")
                                                        where isEq = isTrn m && isSym m
     nM x _               = (x,[],"<=>")


   fEqu :: [String] -> String
   fEqu ss = if and [s=="<=>" | s<-ss] then "<=>" else "==>"

   unF :: Expression -> Expressions
   unF (Fi es')  = es'
   unF (Fu es')  = es'
   unF (Fd es')  = es'
   unF (F  es')  = es'
   unF x        = [x]

{- 
Distribution
To distribute one operator (g) over another (f) results in an expression 
that has g as its root (in all cases).
so  distribute Fi Fu isFi isFu (Fi [r, Fu [s,t]]) = Fu [Fi [r,s], Fi[r,s]]
and distribute Fu Fi isFu isFi (Fi [r, Fu [s,t]]) = Fi [Fu [r], Fu [s,s]]
-}

   distribute :: (Expressions -> Expression)
              -> (Expressions -> Expression)
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
           | isf x && isg k        = g [f [p,q]| p<-unF k, q<-unF (dis (f ks))]
           | isf x && isf k        = g [f [k,q]|           q<-unF (dis (f ks))]
           | isg x                 = g (unF (dis k)++ks)
           | otherwise             = g [x]             
           where xs = unF x
                 k:ks = xs
                 ys = unF k
   
{- klad: De volgende tekst is bedoeld als "uitleg". Ik heb de f vervangen door plus en de g door maal, wat iets minder abstract leest dan f en g.
     dis x | isPlus x && null xs      = maal [0]                                            -- correct want plus [] = maal [0]
           | isMaal x && null xs      = maal []                                             -- correct want maal [] = maal []
           | isPlus x && length xs==1 = dis k                                               -- correct want plus [x] = x
           | isMaal x && length xs==1 = dis k                                               -- correct want maal [x] = x
           | isMaal x && isMaal k     = dis (maal (ys++ks))                                 -- correct want maal is associatief
           | isPlus x && isPlus k     = dis (plus (ys++ks))                                 -- correct want plus is associatief
           | isPlus x && isMaal k     = maal [plus [p,q]| p<-ys , q<-unF (dis (plus ks))]   -- correct want plus distribueert over maal
           | isPlus x                 = maal [plus [k,q]|         q<-unF (dis (plus ks))]   -- correct want plus distribueert over maal
           | isMaal x                 = maal (unF (dis k)++ks)                              -- correct want transformatieloze recursie
           | otherwise                = maal [x]                                            -- correct want x = maal [x]
           where xs = unF x
                 k:ks = xs
                 ys = unF k
-}

   nfProof :: Expression -> Proof Expression
   nfProof expr = nfPr True expr -- The boolean True means that clauses are derived using <=> derivations.
   nfPr :: Bool-> Expression-> [(Expression, [String], String)]
   nfPr eq expr
    = if expr==res
      then [(expr,[],"<=>")]
      else (expr,steps,equ):nfPr eq (simplify res)
    where (res,steps,equ) = normStep eq False expr

   cfProof :: Expression -> Proof Expression
   cfProof expr
    = [line| step, line<-init pr]++
      [line| step', line<-init pr']++
      [line| step'', line<-init pr'']++
      [last ([(expr,[],"<=>")]++
             [line| step, line<-pr]++
             [line| step', line<-pr']++
             [line| step'', line<-pr'']
            )]
      where pr            = nfPr True (simplify expr)
            (expr',_,_)   = last pr
            step          = simplify expr/=expr' || and [null s| (_,ss,_)<-pr, s<-ss]
            expr''        = simplify (distribute Fu Fi isFu isFi expr')   -- Distribute:    (x/\y)\/z  -->  x\/z /\ y\/z
            pr'           = case or [isFi f|Fu fs<-[expr'], f<-fs] of
                             True -> [(expr',["Distribute:    (x/\\y)\\/z  <=>  x\\/z /\\ y\\/z"],"<=>"),(expr'',[],"<=>")]
                             _    -> [(expr',[],"<=>")]
            step'         = expr'/=expr'' || and [null s| (_,ss,_)<-pr', s<-ss]
            pr''          = nfPr True expr''
            step''        = expr''/=expr''' || and [null s| (_,ss,_)<-pr'', s<-ss]
            (expr''',_,_) = last pr''

   conjNF :: Expression -> Expression
   conjNF  expr = e where (e,_,_) = last (cfProof expr)

   disjNF :: Expression -> Expression
   disjNF  expr = e where (e,_,_) = last (dfProof expr)

   dfProof :: Expression -> Proof Expression
   dfProof expr
    = [line| step, line<-init pr]++
      [line| step', line<-init pr']++
      [line| step'', line<-init pr'']++
      [last ([(expr,[],"<=>")]++
             [line| step, line<-pr]++
             [line| step', line<-pr']++
             [line| step'', line<-pr'']
            )]
      where pr            = nfPr True expr
            (expr',_,_)   = last pr
            step          = simplify expr/=simplify expr'
            expr''        = distribute Fi Fu isFi isFu expr'   -- Distribute:    (x\/y)/\z  -->  x/\z \/ y/\z
            pr'           = case or [isFu f|Fi fs<-[expr'], f<-fs] of
                             True -> [(expr',["Distribute:    (x\\/y)/\\z  <=>  x/\\z \\/ y/\\z"],"<=>"),(expr'',[],"<=>")]
                             _    -> [(expr',[],"<=>")]
            step'         = simplify expr'/=simplify expr''
            pr''          = nfPr True expr''
            step''        = simplify expr''/=simplify expr'''
            (expr''',_,_) = last pr''

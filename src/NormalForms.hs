{-# OPTIONS_GHC -Wall -XFlexibleContexts #-}
module NormalForms (conjNF,disjNF,normPA,nfProof,cfProof,dfProof,proofPA,simplify,distribute)
where
   import DatabaseDesign.Ampersand.Core.Basics    (Collection (..),commaEng,eqCl,eqClass)
   import DatabaseDesign.Ampersand.ADL1
{- Normalization of process algebra clauses -}

   normPA :: PAclause (Relation Concept) -> PAclause (Relation Concept)
   normPA expr = expr' 
       where (expr',_,_) = last (proofPA expr)

   type Proof a = [(a, [String], String)]

   proofPA :: PAclause (Relation Concept) -> Proof (PAclause (Relation Concept))
   proofPA expr = ({-reverse.take 3.reverse.-}pPA) expr
    where pPA expr' = case normstepPA expr' of
                       ( _ , []  ,equ) -> [(expr',[]   ,equ)]    -- is dus (expr,[],"<=>")
                       (res,steps,equ) -> [(expr',steps,equ)]++pPA res

   normstepPA :: PAclause (Relation Concept) -> (PAclause (Relation Concept),[String],String)
   normstepPA expr = (res,ss,"<=>")
    where
     (res,ss) = norm expr
     norm :: PAclause (Relation Concept) -> (PAclause (Relation Concept),[String])
     norm (Chc [] ms)  = (Blk ms, ["Run out of options"])
     norm (Chc [d] ms) = (d', ["Flatten Singleton"])
                       where d' = case d of
                                    Blk{} -> d
                                    _     -> d{paMotiv = ms} 
-- BECAUSE? Stef, kan je uitleggen wat hier gebeurt? Enig commentaar is hier wel op zijn plaats. Ook zou het helpen om bij de verschillende constructoren van PAclause een beschrijving te geven van het idee er achter. 
-- Kan ik wel uitleggen, maar het is een heel verhaal. Dat moet tzt in een wetenschappelijk artikel gebeuren, zodat het er goed staat.
-- Het idee is dat een procesalgebra is weergegeven in Haskell combinatoren (gedefinieerd als PAclause(..), zie ADL.ECArule).
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
                       | (not.null) long              = (All ds' ms, ["Take the expressions for "++commaEng "and" [(name.head.(morlist::Expression (Relation Concept)->[Relation Concept]).paTo.head) cl|cl<-long]++"together"])
                       | otherwise = (All ds ms, [])
                       where ds' = [ let p=head cl in
                                     if length cl==1 then p else p{paTo=disjNF (Fux[paDelta c| c<-cl]), paMotiv=concat (map paMotiv cl)}
                                   | cl<-dCls ]
                                   ++[d| d<-ds, not (isDo d)]
                             nds  = map norm ds
                             msgs = (concat.map snd) nds
                             ops  = map fst nds
                             dCls :: [[PAclause (Relation Concept)]]
                             dCls = eqCl to [d| d<-ds, isDo d]
                             long :: [[PAclause (Relation Concept)]]
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

   simplify :: (Identified c, Eq c, Show c, ConceptStructure c c) => Expression (Relation c) -> Expression (Relation c)
   simplify expr = expr' 
       where (expr',_,_) = last (simpProof shw expr)
             shw _ = ""
   
   simpProof :: (Identified c, Eq c, Show c, SpecHierarchy c, ConceptStructure c c) =>
                (Expression (Relation c) -> String) -> Expression (Relation c) -> Proof (Expression (Relation c))
   simpProof shw expr    
    = if expr==res
      then [(expr,[],"<=>")]
      else (expr,steps,equ):simpProof shw res
    where (res,steps,equ) = normStep shw True True expr

   normStep :: (Identified c, Show c, SpecHierarchy c, ConceptStructure c c) => 
               (Expression (Relation c) -> String) -> Bool -> Bool ->
               Expression (Relation c) -> (Expression (Relation c),[String],String) -- This might be generalized to "Expression r" if it weren't for the fact that flip is embedded in the Relation type.
   normStep shw   -- a function to print an expression. Might be "showADL" or "showADLcode fSpec", or any other...
            eq    -- If eq==True, only equivalences are used. Otherwise, implications are used as well.
            simpl -- If True, only simplification rules are used, which is a subset of all rules. Consequently, simplification is implied by normalization.
            expr = (res,ss,equ)
    where
     (res,ss,equ) = nM expr []
--     nM :: (Identified c, Show c, SpecHierarchy c, ConceptStructure c c) => 
--         Expression (Relation c) -> [Expression (Relation c)] -> (Expression (Relation c),[String],String)
     nM (K0x e')      _    = (K0x res', steps, equ')
                             where (res',steps,equ') = nM e' []
     nM (K1x e')      _    = (K1x res', steps, equ')
                             where (res',steps,equ') = nM e' []
     nM (Cpx (Cpx e')) _    = (e', ["compl of compl"],"<=>")
     nM (Cpx (Fix fs)) _    = if simpl then (notCp res',steps,equ') else (Fux (map notCp fs), ["De Morgan"], "<=>")
                             where (res',steps,equ') = nM (Fix fs) []
     nM (Cpx (Fux fs)) _    = if simpl then (notCp res',steps,equ') else (Fix (map notCp fs), ["De Morgan"], "<=>")
                             where (res',steps,equ') = nM (Fux fs) []
     nM (Cpx (Fdx ts)) _    = if not simpl && and [isNeg t| t<-ts] 
                               then (F (map notCp ts), ["De Morgan"], "<=>")
                               else (notCp res',steps,equ')
                             where (res',steps,equ') = nM (Fdx ts) []
     nM (Cpx e')      _    = (notCp res',steps,equ')
                             where (res',steps,equ') = nM e' []
     nM (Tc f)       _    = nM f []
     nM (F [t])      _    = nM t []
     nM (F (k:ks))   rs   | or [isF x|x<-k:ks]  = nM (F [y| x<-k:ks, y<-if isF x then unF x else [x]]) rs  -- haakjes verwijderen o.g.v. associativiteit
                          | or [isI x|x<-k:ks]  = (F [x|x<-k:ks,not (isI x)], ["x;I = x"], "<=>")
                   -- If only simplification is required, we are done now.
                          | simpl                   = (if isF f then F (t:unF f) else F [t,f], steps++steps', fEqu [equ',equ''])
                          | not eq && length ks>1 && isFd g && length gs>1
                                                    = (F (Fdx (F [k,head gs]:tail gs):ks'), ["Peirce: r;(s!q) => (r;s)!q"],"==>")
                          | not eq && isFd k && length ue>1
                                                    = (Fdx (init ue++[F (last ue:ks)]), ["Peirce: (r!s);q => r!(s;q)"],"==>")
                          | not eq && isFi k        = (distribute F Fix isF isFi (F (k:ks)), ["distribute /\\ over ;"], "==>")
                          | isFu k                  = (distribute F Fux isF isFu (F (k:ks)), ["distribute \\/ over ;"], "<=>")
                          | and [isNeg x|x<-(k:ks)] = (notCp (Fdx [notCp x| x<-(k:ks)]), ["De Morgan"], "<=>")
                          | isFu (last (k:ks))      = (Fux [F (init (k:ks)++[t'])|Fux xs<-[last (k:ks)], t'<-xs], ["distribute \\/ over ;"], "<=>")
                          | otherwise               = (if isF f then F (t:unF f) else F [t,f], steps++steps', fEqu [equ',equ''])
                          where (t,steps, equ')  = nM k []
                                (f,steps',equ'') = nM (F ks) (k:rs)
                                ue = unF k
                                g@(Fdx gs):ks' = ks

     nM (Fdx [k])    _     = nM k []
     nM (Fdx (k:ks)) rs    | or [isFd x|x<-k:ks]     = nM (Fdx [y| x<-k:ks, y<-if isFd x then unF x else [x]]) rs
                          | or [isNot x|x<-k:ks]    = (F [x|x<-k:ks,not (isNot x)], ["x!-I = x"], "<=>")
                   -- If only simplification is required, we are done now.
                          | simpl                   = (if isFd f then Fdx (t:unF f) else Fdx [t,f], steps++steps', fEqu [equ',equ''])
                          | not eq && isFu k        = (distribute Fdx Fux isFd isFu (Fdx (k:ks)), ["distribute \\/ over !"], "==>")
                          | isFi k                  = (distribute Fdx Fix isFd isFi (Fdx (k:ks)), ["distribute /\\ over !"], "<=>")
                          | and [isNeg x|x<-(k:ks)] = (notCp (F [notCp x| x<-(k:ks)]), ["De Morgan"], "<=>")
                          | length ks>1 && isNeg k && isPos g && isFunction k
                                                    = (F [notCp k,Fdx ks], ["f-!g = f;g if f is a function"], "<=>")
                          | length ks>1 && isPos k && isNeg g && isFunction (flp g)
                                                    = (Fdx ( F[k,notCp g]:ks'), ["f!g- = f;g if g~ is a function"], "<=>")
                          | otherwise               = (if isFd f then Fdx (t:unF f) else Fdx [t,f], steps++steps', fEqu [equ',equ''])
                          where (t,steps, equ')  = nM k []
                                (f,steps',equ'') = nM (Fdx ks) (k:rs)
                                g:ks' = ks
     nM (Fix [k]) _   = nM k []
     nM (Fux [k]) _   = nM k []
     nM (Fix (k:ks)) rs
                     -- Associativity of /\:    (r/\s)/\t  -->  r/\s/\t      (implicit step)
                            | or [isFi x|x<-k:ks]         = nM (Fix [y| x<-k:ks, y<-if isFi x then unF x else [x]]) rs
                     -- If only simplification is required, we are done now.
                            | simpl                       = (if isFi f then Fix (t:unF f) else Fix [t,f], steps++steps', fEqu [equ',equ''])
                     -- Absorb equals:    r/\r  -->  r
                            | or [length cl>1|cl<-absor3] = (Fix [head cl| cl<-absor3], [shw e++"/\\"++shw e++" = "++shw e| cl<-absor3, length cl>1, let e=head cl], "<=>")
                     -- Inconsistency:    r/\-r   -->  False
                            | not (null incons)           = (Fux [], [shw (notCp (head incons))++"/\\"++shw (head incons)++" = V-"], "<=>")
                     -- Inconsistency:    Fu []   -->  False
                            | k==Fux []                    = (Fux [], ["inconsistency"], "<=>")
  -- this is unreachable    | k==Fi []                    = (Fi ks, ["x/\\V = x"], "<=>")
                     -- Inconsistency:    x/\\V-  -->  False
                            | or[x==Fux []|x<-ks]          = (Fux [], ["x/\\V- = V-"], "<=>")
                     -- Absorb if r is antisymmetric:    r/\r~  -->  I    (note that a reflexive r incurs r/\r~ = I)
                            | or [length cl>1|cl<-absor2] = ( Fux [if length cl>1 then Tm (mIs (source e)) (-1) else e| cl<-absor2, let e=head cl]
                                                            , [shw e++"/\\"++shw (flp e)++" = I, because"++shw e++" is antisymmetric"| cl<-absor2, let e=head cl]
                                                            , if and [isRfx (head cl)| cl<-absor2, length cl>1] then "<=>" else "==>"
                                                            )
                     -- Absorb:    (x\\/y)/\\y  -->  y
                            | isFu k && not (null absor0) = let f'=head absor0 in (Fix ks, ["absorb "++shw k++" because of "++shw f'++" ((x\\/y)/\\y = y))"], "<=>")
                     -- Absorb:    (x\\/-y)/\\y  -->  x/\\y
                            | isFu k && not (null absor1) = let (ts,f')=head absor1 in (Fix (ts++ks), ["absorb "++shw f'], "<=>")
                            | otherwise                   = (if isFi f then Fix (t:unF f) else Fix [t,f], steps++steps', fEqu [equ',equ''])
                            where (t,steps, equ')  = nM k []
                                  (f,steps',equ'') = nM (Fix ks) (k:rs)
                                  incons = [x|x<-ks,x==notCp k]
                                  absor0 = [t'| t'<-unF k, f'<-ks++rs, t'==f']
                                  absor1 = [(if length rest<=1 then rest else [Fux rest] , t')| t'<-unF k, f'<-ks++rs, notCp t'==f', let rest = [x|x<-unF k,x/=t']]
                                  absor2 = eqClass same (rs++k:ks) where e `same` e' = if isRfx e && isAsy e && isRfx e' && isAsy e' then e==flp e' else False
                                  absor3 = eqClass (==) (rs++k:ks)
     nM (Fux (k:ks)) rs
                     -- Associativity of \/:    (r\/s)\/t  -->  r\/s\/t      (implicit step)
                            | or [isFu x|x<-k:ks]         = nM (Fux [y| x<-k:ks, y<-if isFu x then unF x else [x]]) rs
                     -- If only simplification is required, we are done now.
                            | simpl                       = (if isFu f then Fux (t:unF f) else Fux [t,f], steps++steps', fEqu [equ',equ''])
                     -- Absorb equals:    r\/r  -->  r
                            | or [length cl>1|cl<-absor3] = (Fux [head cl| cl<-absor3], [shw e++"\\/"++shw e++" = "++shw e| cl<-absor3, length cl>1, let e=head cl], "<=>")
                     -- Tautology:    r\/-r  -->  V
                            | or [length cl>1|cl<-absor2] = let ncp (Cpx e) = e; ncp e = e in
                                                            (Fix [], take 1 ["let "++ shw (ncp e)++" = e. Since -e\\/e = V we get"
                                                                            | cl<-absor2, length cl>1, let e=head cl], "<=>")
                            | k==Fix []                    = (Fix [], ["tautology"], "<=>")
  -- this is unreachable    | k==Fu []                    = (Fu ks, ["x\\/V- = x"], "<=>")
                     -- Tautology:    r\/V  -->  V
                            | or[x==Fix []|x<-ks]          = (Fix [], ["x\\/V = V"], "<=>")
                            | isFi k && not (null absor0) = let f'=head absor0 in (Fux ks, ["absorb "++shw k++" because of "++shw f'++" ((x/\\y)\\/y = y))"], "<=>")
                            | isFi k && not (null absor1) = let (ts,f')=head absor1 in (Fux (ts++ks), ["absorb "++shw f'++" ((x/\\y-)\\/y = x\\/y))"], "<=>")
                            | otherwise                   = (if isFu f then Fux (t:unF f) else Fux [t,f], steps++steps', fEqu [equ',equ''])
                            where (t,steps, equ')  = nM k []
                                  (f,steps',equ'') = nM (Fux ks) (k:rs)
                                  absor0 = [t'| t'<-unF k, f'<-ks++rs, t'==f']
                                  absor1 = [(if length rest<=1 then rest else [Fix rest] , t')| t'<-unF k, f'<-ks++rs, notCp t'==f', rest<-[[x|x<-unF k,x/=t']]]
                                  absor2 = eqClass same (rs++k:ks) where e `same` e' = e==notCp e'
                                  absor3 = eqClass (==) (rs++k:ks)
     nM (Tm m n) _        | isSym m && not (inline m) =  (Tm (flp m) n,[name m++" is symmetric"],"<=>")
                   -- Equivalence relation:    r  -->  I   if r is reflexive, transitive and symmetric.
                          | isEq && not (isIdent m)   = if isRfx m
                                                        then (Fix [Tm (mIs (source m)) n], [name m++" is an equivalence relation"], "<=>")
                                                        else (Fix [Tm (mIs (source m)) n], [name m++" is transitive and symmetric"], "==>")
                                                        where isEq = isTrn m && isSym m
     nM x _               = (x,[],"<=>")


   fEqu :: [String] -> String
   fEqu ss = if and [s=="<=>" | s<-ss] then "<=>" else "==>"

   unF :: Expression r -> Expressions r
   unF (Fix es')  = es'
   unF (Fux es')  = es'
   unF (Fdx es')  = es'
   unF (F  es')  = es'
   unF x        = [x]

{- 
Distribution
To distribute one operator (g) over another (f) results in an expression 
that has g as its root (in all cases).
so  distribute Fi Fu isFi isFu (Fi [r, Fu [s,t]]) = Fu [Fi [r,s], Fi[r,s]]
and distribute Fu Fi isFu isFi (Fi [r, Fu [s,t]]) = Fi [Fu [r], Fu [s,s]]
-}

   distribute :: ([Expression r] -> Expression r)
              -> ([Expression r] -> Expression r)
              -> (Expression r -> Bool)
              -> (Expression r -> Bool)
              -> Expression r
              -> Expression r
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

   nfProof :: (Identified c, Show c, ConceptStructure c c) =>
              (Expression (Relation c) -> String) -> Expression (Relation c) -> Proof (Expression (Relation c))
   nfProof shw expr = nfPr shw True expr -- The boolean True means that clauses are derived using <=> derivations.
   nfPr :: (Identified c, Show c, ConceptStructure c c) =>
           (Expression (Relation c) -> String) -> Bool-> Expression (Relation c) -> [(Expression (Relation c), [String], String)]
   nfPr shw eq expr
    = if expr==res
      then [(expr,[],"<=>")]
      else (expr,steps,equ):nfPr shw eq (simplify res)
    where (res,steps,equ) = normStep shw eq False expr

   cfProof :: (Identified c, Eq c, Show c, ConceptStructure c c) =>
              (Expression (Relation c) -> String) -> Expression (Relation c) -> Proof (Expression (Relation c))
   cfProof shw expr
    = [line| step, line<-init pr]++
      [line| step', line<-init pr']++
      [line| step'', line<-init pr'']++
      [last ([(expr,[],"<=>")]++
             [line| step, line<-pr]++
             [line| step', line<-pr']++
             [line| step'', line<-pr'']
            )]
      where pr            = nfPr shw True (simplify expr)
            (expr',_,_)   = last pr
            step          = simplify expr/=expr' || and [null s| (_,ss,_)<-pr, s<-ss]
            expr''        = simplify (distribute Fux Fix isFu isFi expr')   -- Distribute:    (x/\y)\/z  -->  x\/z /\ y\/z
            pr'           = case or [isFi f|Fux fs<-[expr'], f<-fs] of
                             True -> [(expr',["Distribute:    (x/\\y)\\/z  <=>  x\\/z /\\ y\\/z"],"<=>"),(expr'',[],"<=>")]
                             _    -> [(expr',[],"<=>")]
            step'         = expr'/=expr'' || and [null s| (_,ss,_)<-pr', s<-ss]
            pr''          = nfPr shw True expr''
            step''        = expr''/=expr''' || and [null s| (_,ss,_)<-pr'', s<-ss]
            (expr''',_,_) = last pr''

   conjNF :: (Identified c, Eq c, Show c, ConceptStructure c c) =>
             Expression (Relation c) -> Expression (Relation c)
   conjNF expr = e where (e,_,_) = last (cfProof (\_->"") expr)

   disjNF :: (Identified c, Eq c, Show c, ConceptStructure c c) =>
             Expression (Relation c) -> Expression (Relation c)
   disjNF expr = e where (e,_,_) = last (dfProof (\_->"") expr)

   dfProof :: (Identified c, Eq c, Show c, ConceptStructure c c) =>
              (Expression (Relation c) -> String) -> Expression (Relation c) -> Proof (Expression (Relation c))
   dfProof shw expr
    = [line| step, line<-init pr]++
      [line| step', line<-init pr']++
      [line| step'', line<-init pr'']++
      [last ([(expr,[],"<=>")]++
             [line| step, line<-pr]++
             [line| step', line<-pr']++
             [line| step'', line<-pr'']
            )]
      where pr            = nfPr shw True expr
            (expr',_,_)   = last pr
            step          = simplify expr/=simplify expr'
            expr''        = distribute Fix Fux isFi isFu expr'   -- Distribute:    (x\/y)/\z  -->  x/\z \/ y/\z
            pr'           = case or [isFu f|Fix fs<-[expr'], f<-fs] of
                             True -> [(expr',["Distribute:    (x\\/y)/\\z  <=>  x/\\z \\/ y/\\z"],"<=>"),(expr'',[],"<=>")]
                             _    -> [(expr',[],"<=>")]
            step'         = simplify expr'/=simplify expr''
            pr''          = nfPr shw True expr''
            step''        = simplify expr''/=simplify expr'''
            (expr''',_,_) = last pr''

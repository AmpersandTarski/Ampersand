{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms 
  (conjNF,disjNF,normPA,nfProof,cfProof,dfProof,proofPA,simplify,exprIsc2list, exprUni2list, exprCps2list, exprRad2list)
where
   import DatabaseDesign.Ampersand.Basics    (fatalMsg,Identified(..),commaEng,eqCl,eqClass,Flippable(..), (>-))
   import DatabaseDesign.Ampersand.Classes
   import DatabaseDesign.Ampersand.ADL1.ECArule
   import DatabaseDesign.Ampersand.ADL1.Expression 
   import qualified DatabaseDesign.Ampersand.Core.Poset as Poset
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import DatabaseDesign.Ampersand.Fspec.Fspec
   import Data.List (nub)
   import Prelude hiding (head)
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "Fspec.ToFspec.NormalForms"

   head :: [a] -> a
   head [] = fatal 30 "head must not be used on an empty list!"
   head (a:_) = a


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
     norm (CHC [] ms)  = (Blk ms, ["Run out of options"])
     norm (CHC [r] ms) = (r', ["Flatten ONE"])
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
     norm (CHC ds ms)  | (not.null) msgs = (CHC ops ms, msgs)
                       | (not.null) [d | d<-ds, isCHC d] = (CHC (nub [ d' | d<-ds, d'<-if isCHC d then let CHC ops' _ = d in ops' else [d] ]) ms, ["flatten CHC"])  -- flatten
                       | (not.null) [Nop | Nop{}<-ops] = (Nop{paMotiv=ms}, ["Choose to do nothing"])
                       | (not.null) [Blk | Blk{}<-ops] = (CHC [op | op<-ops, not (isBlk op)] ms, ["Choose anything but block"])
                       | otherwise = (CHC ds ms, [])
                       where nds  = map norm ds
                             msgs = concatMap snd nds
                             ops  = map fst nds
     norm (ALL [] ms)  = (Nop ms, ["ALL [] = No Operation"])
     norm (ALL [d] ms) = (d', ["Flatten ONE"])
                       where d' = case d of
                                    Blk{} -> d
                                    _     -> d{paMotiv = ms} 
     norm (ALL ds ms)  | (not.null) msgs = (ALL ops ms, msgs)
                       | (not.null) [d | d<-ds, isAll d] = (ALL (nub [ d' | d<-ds, d'<-if isAll d then let ALL ops' _ = d in ops' else [d] ]) ms, ["flatten ALL"])  -- flatten
                       | (not.null) [Blk | Blk{}<-ops] = (Blk{paMotiv = [m | op@Blk{}<-ops,m<-paMotiv op]}, ["Block all"])
                       | (not.null) [Nop | Nop{}<-ops] = (ALL [op | op<-ops, not (isNop op)] ms, ["Ignore Nop"])
                       | (not.null) long              = (ALL ds' ms, ["Take the expressions for "++commaEng "and" [name (paTo (head cl)) |cl<-long]++"together"])
                       | otherwise = (ALL ds ms, [])
                       where ds' = [ let p=head cl in
                                       if length cl==1 then p else p{paDelta=disjNF (foldr1 (.\/.) [paDelta c | c<-cl]), paMotiv=concatMap paMotiv cl}
                                   | cl<-dCls {- not (null cl) is guaranteed by eqCl -} ]
                                   ++[d | d<-ds, not (isDo d)]
                             nds  = map norm ds
                             msgs = concatMap snd nds
                             ops  = map fst nds
                             dCls :: [[PAclause]]
                             dCls = eqCl to [d | d<-ds, isDo d]
                             long :: [[PAclause]]
                             long = [cl | cl<-dCls, length cl>1]
                             to d@Do{} = (paSrt d,paTo d)
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
    where (res,steps,equ) = normStep shw True True True expr

   -- | This type exists for the sake of normStep only.
   data Cmp = Lte | Gte | Eql   

   cplCmp :: Cmp -> Cmp
   cplCmp Lte = Gte
   cplCmp Gte = Lte
   cplCmp Eql  = Eql

   -- | The purpose of "normStep" is to elaborate a single step in a rewrite process,
   -- in which the expression is normalized by means of rewrite rules.
   -- This function can be used for simplification, which means that an Expression is standardized
   -- using associativity and other 'trivial' rules only.
   -- These 'trivial' rules do not produce a step in the proof.
   -- Use normstep shw eq True expr to do simplification only.
   -- Use normstep shw eq False expr to obtain a single proof step or none when no rule is applicable.
   -- This function returns a resulting expression that is closer to a normal form.
   -- The normal form is not unique. This function simply uses the first rewrite rule it encounters.
   normStep :: (Expression -> String) -> Bool -> Bool -> Bool ->
               Expression -> (Expression,[String],String) -- This might be generalized to "Expression" if it weren't for the fact that flip is embedded in the Relation type.
   normStep shw   -- a function to print an expression. Might be "showADL"
            eq    -- If eq==True, only equivalences are used. Otherwise, implications are used as well.
            dnf   -- If dnf==True, the result is in disjunctive normal form, otherwise in conjunctive normal form
            simpl -- If True, only simplification rules are used, which is a subset of all rules. Consequently, simplification is implied by normalization.
            expr = (res,ss,equ)
    where
     
     (res,ss,equ) = nM (if eq then Eql else Lte) expr []
--     nM :: Expression -> [Expression] -> (Expression,[String],String)
     nM posNeg (EEqu (l,r) _) _     | simpl = (t .==. f, steps++steps', fEqu [equ',equ''])
                                              where (t,steps, equ')  = nM posNeg l []
                                                    (f,steps',equ'') = nM posNeg r []
     nM posNeg (EImp (l,r) _) _     | simpl = (t .|-. f, steps++steps', fEqu [equ',equ''])
                                              where (t,steps, equ')  = nM (cplCmp posNeg) l []
                                                    (f,steps',equ'') = nM posNeg r []
     nM posNeg (ELrs (l,r) _) _     | simpl = (t ./. f, steps++steps', fEqu [equ',equ''])     -- l/r  =  l ! -r~  =  -(-l ; r~)
                                              where (t,steps, equ')  = nM posNeg l []
                                                    (f,steps',equ'') = nM (cplCmp posNeg) r []
     nM posNeg (ERrs (l,r) _) _     | simpl = (t .\. f, steps++steps', fEqu [equ',equ''])
                                              where (t,steps, equ')  = nM (cplCmp posNeg) l []
                                                    (f,steps',equ'') = nM posNeg r []
     nM posNeg (EUni (EUni (l,k) _,r) _) rs = nM posNeg (l .\/. (k .\/. r)) rs  -- standardize, using associativity of .\/.
     nM posNeg (EUni (l,r) _) rs    | simpl = (t .\/. f, steps++steps', fEqu [equ',equ''])
                                              where (t,steps, equ')  = nM posNeg l []
                                                    (f,steps',equ'') = nM posNeg r (l:rs)
     nM posNeg (EIsc (EIsc (l,k) _,r) _) rs = nM posNeg (l ./\. (k ./\. r)) rs  -- standardize, using associativity of ./\.
     nM posNeg (EIsc (l,r) _) rs    | simpl = (t ./\. f, steps++steps', fEqu [equ',equ''])
                                              where (t,steps, equ')  = nM posNeg l []
                                                    (f,steps',equ'') = nM posNeg r (l:rs)
     nM posNeg (ECps (ECps (l,k) _,r) _) rs = nM posNeg (l .:. (k .:. r)) rs  -- standardize, using associativity of .:.
     nM posNeg (ECps (l,r) _) rs    | simpl = (t .:. f, steps++steps', fEqu [equ',equ''])
                                               where (t,steps, equ')  = nM posNeg l []
                                                     (f,steps',equ'') = nM posNeg r (l:rs)
     nM posNeg (ERad (ERad (l,k) _,r) _) rs = nM posNeg (l .!. (k .!. r)) rs  -- standardize, using associativity of .!.
     nM posNeg (ERad (l,r) _) rs    | simpl = (t .!. f, steps++steps', fEqu [equ',equ''])
                                              where (t,steps, equ')    = nM posNeg l []
                                                    (f,steps',equ'')   = nM posNeg r (l:rs)
     nM posNeg (EPrd (EPrd (l,k) _,r) _) rs = nM posNeg (l .*. (k .*. r)) rs  -- standardize, using associativity of .*.
     nM posNeg (EPrd (l,r) _) _     | simpl = (t .*. f, steps++steps', fEqu [equ',equ''])
                                              where (t,steps, equ')  = nM posNeg l []
                                                    (f,steps',equ'') = nM posNeg r []
     nM posNeg (EKl0 e sgn)              _  = (EKl0 res' sgn, steps, equ')
                                              where (res',steps,equ') = nM posNeg e []
     nM posNeg (EKl1 e sgn)              _  = (EKl1 res' sgn, steps, equ')
                                              where (res',steps,equ') = nM posNeg e []
     nM posNeg (ECpl (ECpl e _) _)       rs = nM (cplCmp posNeg) e rs
     nM posNeg (ECpl e sgn) _       | simpl = (notCpl sgn res',steps,equ')
                                              where (res',steps,equ') = nM (cplCmp posNeg) e []
     nM posNeg (EBrk e)                  _  = nM posNeg e []
     nM posNeg (EFlp (ECpl e _) sgn)     rs = nM (cplCmp posNeg) (notCpl sgn (flp e)) rs
     nM _      x _                  | simpl = (x,[],"<=>")
-- up to here, simplification has been treated. The remaining rules can safely assume  simpl==False
     nM _      (EEqu (l,r) _) _                                = ((l .|-. r) ./\. (r .|-. l), ["remove ="],"<=>")
     nM _      (EImp (x,ELrs (z,y) _) _) _                     = (x .:. y .|-. z, ["remove left residual (/)"],"<=>")
     nM _      (EImp (y,ERrs (x,z) _) _) _                     = (x .:. y .|-. z, ["remove right residual (\\)"],"<=>")
     nM _      (EImp (l,r) sgn) _                              = (notCpl sgn l .\/. r, ["remove |-"],"<=>")
     nM _      (ELrs (l,r) sgn) _                              = (l .!. notCpl sgn (flp r), ["remove left residual (/)"],"<=>")
     nM _      (ERrs (l,r) sgn) _                              = (notCpl sgn (flp l) .!. r, ["remove right residual (\\)"],"<=>")
     nM _      (ECpl e@EIsc{} sgn) _                           = (notCpl sgn (deMorgan sgn e), ["De Morgan"], "<=>")
     nM _      (ECpl e@EUni{} sgn) _                           = (notCpl sgn (deMorgan sgn e), ["De Morgan"], "<=>")
     nM _      (ECpl e@(ERad (_,ECpl{}) _) sgn) _              = (notCpl sgn (deMorgan sgn e), ["De Morgan"], "<=>")
     nM _      (ECpl e@(ERad (ECpl{},_) _) sgn) _              = (notCpl sgn (deMorgan sgn e), ["De Morgan"], "<=>")
     nM _      (ECpl e@(ECps (ECpl{},ECpl{}) _) sgn) _         = (notCpl sgn (deMorgan sgn e), ["De Morgan"], "<=>")
     nM posNeg (ECpl e sgn) _                                  = (notCpl sgn res',steps,equ')
                                                                 where (res',steps,equ') = nM (cplCmp posNeg) e []
     nM _      (ECps (l,r) sgn) _ | isIdent l && sign l Poset.>=sgn = (r, ["I;x = x"], "<=>")
     nM _      (ECps (l,r) sgn) _ | isIdent r && sign r Poset.>=sgn = (l, ["x;I = x"], "<=>")
     nM Lte    (ECps (r,ERad (s,q) _) _) _ | not eq            = ((r.:.s).!.q, ["Peirce: r;(s!q) |- (r;s)!q"],"==>")
     nM Lte    (ECps (ERad (r,s) _,q) _) _ | not eq            = (r.!.(s.:.q), ["Peirce: (r!s);q |- r!(s;q)"],"==>")
     nM Lte    (ECps (EIsc (r,s) _,q) _) _ | not eq            = ((r.:.q)./\.(s.:.q), ["distribute ; over /\\"],"==>")
     nM Lte    (ECps (r,EIsc (s,q) _) _) _ | not eq            = ((r.:.s)./\.(r.:.q), ["distribute ; over /\\"],"==>")
     nM _      (ECps (EUni (q,s) _,r) _) _                     = ((q.:.r).\/.(s.:.r), ["distribute ; over \\/"],"<=>")
     nM _      (ECps (l,EUni (q,s) _) _) _                     = ((l.:.q).\/.(l.:.s), ["distribute ; over \\/"],"<=>")
     nM _      x@(ECps (l@EFlp{},r) _) _ | not eq && flp l==r && isInj l   = (iExpr (source x), ["r~;r |- I (r is univalent)"], "==>")
     nM _      x@(ECps (l,       r) _) _ | not eq && l==flp r && isInj l   = (iExpr (source x), ["r;r~ |- I (r is injective)"], "==>")
     nM _      x@(ECps (l@EFlp{},r) _) _ | flp l==r && isInj l && isTot l  = (iExpr (source x), ["r~;r=I because r is univalent and surjective"], "<=>")
     nM _      x@(ECps (l,       r) _) _ | l==flp r && isInj l && isTot l  = (iExpr (source x), ["r;r~=I because r is injective and total"], "<=>")
     nM _      x@(ECps(ECpl{},ECpl{}) _) _                     = (deMorgan (sign x) x, ["De Morgan"], "<=>")
     nM posNeg (ECps (l,r) _)           rs                     = (t .:. f, steps++steps', fEqu [equ',equ''])
                                                                 where (t,steps, equ')  = nM posNeg l []
                                                                       (f,steps',equ'') = nM posNeg r (l:rs)
     nM _      (ERad (l,r) sgn) _ | isImin l && sign l Poset.>=sgn = (r, ["-I;x = x"], "<=>")
     nM _      (ERad (l,r) sgn) _ | isImin r && sign r Poset.>=sgn = (l, ["x;-I = x"], "<=>")
     nM Gte    (ERad (ECps (r,s) _,q) _) _  | not eq           = (r.!.(s.:.q), ["Peirce: r;(s!q) -| (r;s)!q"],"==>")
     nM Gte    (ERad (r,ECps (s,q) _) _) _  | not eq           = ((r.!.s).:.q, ["Peirce: r!(s;q) -| (r!s);q"],"==>")
     nM Gte    (ERad (EUni (r,s) _,q) _) _  | not eq           = ((r.!.q).\/.(s.!.q), ["distribute ! over \\/"],"==>")
     nM Gte    (ERad (r,EUni (s,q) _) _) _  | not eq           = ((r.!.s).\/.(r.!.q), ["distribute ! over \\/"],"==>")
     nM _      (ERad (EIsc (q,s) _,r) _) _                     = ((q.!.r)./\.(s.!.r), ["distribute ! over /\\"],"<=>")
     nM _      (ERad (l,EIsc (q,s) _) _) _                     = ((l.!.q)./\.(l.!.s), ["distribute ! over /\\"],"<=>")
     nM _      x@(ERad(ECpl{},_) _)      _                     = (deMorgan (sign x) x, ["De Morgan"], "<=>")
     nM _      x@(ERad(_,ECpl{}) _)      _                     = (deMorgan (sign x) x, ["De Morgan"], "<=>")
     nM posNeg (ERad (l,r) _)           rs                     = (t .!. f, steps++steps', fEqu [equ',equ''])
                                                                 where (t,steps, equ')  = nM posNeg l []
                                                                       (f,steps',equ'') = nM posNeg r (l:rs)
     nM _      (EPrd (l,EPrd (_,r) _) _) _                     = (l .*. r, ["eliminate middle in cartesian product"], "<=>")
     nM posNeg (EPrd (l,r) _) _                                = (t .*. f, steps++steps', fEqu [equ',equ''])
                                                                 where (t,steps, equ')  = nM posNeg l []
                                                                       (f,steps',equ'') = nM posNeg r []
     nM _      (EIsc (EUni (l,k) _,r) _) _ | dnf               = ((l./\.r) .\/. (k./\.r), ["distribute /\\ over \\/"],"<=>")
     nM _      (EIsc (l,EUni (k,r) _) _) _ | dnf               = ((l./\.k) .\/. (l./\.r), ["distribute /\\ over \\/"],"<=>")
     nM _      (EUni (EIsc (l,k) _,r) _) _ | not dnf           = ((l.\/.r) ./\. (k.\/.r), ["distribute \\/ over /\\"],"<=>")
     nM _      (EUni (l,EIsc (k,r) _) _) _ | not dnf           = ((l.\/.k) ./\. (l.\/.r), ["distribute \\/ over /\\"],"<=>")
     nM posNeg (EIsc (l,r) sgn) rs
-- Absorb equals:    r/\r  -->  r
         | or [length cl>1 |cl<-absorbClasses]
              = ( foldr1 (./\.) [head cl | cl<-absorbClasses]
                , [shw e++" /\\ "++shw e++" = "++shw e | cl<-absorbClasses, length cl>1, let e=head cl]
                , "<=>"
                )
-- Inconsistency:    r/\-r   -->  False
         | not (null incons)
              = let i = head incons in (notCpl sgn (vExpr sgn), [shw (notCpl (sign i) i)++" /\\ "++shw i++" = V-"], "<=>")
-- Inconsistency:    x/\\V-  -->  False
         | (not.null) [() |t'<-exprIsc2list l++exprIsc2list r, isFalse t']
              = let sgn' = sign (foldr1 (./\.) (exprIsc2list l++exprIsc2list r)) in (notCpl sgn' (vExpr sgn'), ["x/\\V- = V-"], "<=>")
-- Absorb if r is antisymmetric:    r/\r~ --> I
         | not eq && or [length cl>1 |cl<-absorbAsy]
              = ( foldr1 (./\.) [if length cl>1 then iExpr (source e) else e | cl<-absorbAsy, let e=head cl] 
                , [shw e++" /\\ "++shw (flp e)++" |- I, because"++shw e++" is antisymmetric" | cl<-absorbAsy, let e=head cl]
                , "==>"
                )
-- Absorb if r is antisymmetric and reflexive:    r/\r~ = I
         | or [length cl>1 |cl<-absorbAsyRfx]
              = ( foldr1 (./\.) [if length cl>1 then iExpr (source e) else e | cl<-absorbAsyRfx, let e=head cl] 
                , [shw e++" /\\ "++shw (flp e)++" = I, because"++shw e++" is antisymmetric and reflexive" | cl<-absorbAsyRfx, let e=head cl]
                , "<=>"
                )
-- Absorb:    (x\\/y)/\\y  =  y
         | isEUni l && not (null absor0)
              = let t'=head absor0  in (r, ["absorb "++shw l++" because of "++shw t'++", using law  (x\\/y)/\\y = y"], "<=>")
         | isEUni r && not (null absor0')
              = let t'=head absor0' in (r, ["absorb "++shw r++" because of "++shw t'++", using law  (x\\/y)/\\x = x"], "<=>")
-- Absorb:    (x\\/-y)/\\y  =  x/\\y
         | isEUni l && not (null absor1)
              = ( case head absor1 of
                    (_,[]) -> r
                    (_,ts) -> foldr1 (.\/.) ts ./\. r
                , ["absorb "++shw t'++", using law (x\\/-y)/\\y  =  x/\\y" | (t',_)<-absor1]
                , "<=>"
                )
         | isEUni r && not (null absor1')
              = ( case head absor1' of
                    (_,[]) -> l
                    (_,ts) -> l ./\. foldr1 (.\/.) ts
                , ["absorb "++shw t'++", using law x/\\(y\\/-x)  =  x/\\y" | (t',_)<-absor1']
                , "<=>"
                )
         | otherwise = (t ./\. f, steps++steps', fEqu [equ',equ''])
         where (t,steps, equ')  = nM posNeg l []
               (f,steps',equ'') = nM posNeg r (l:rs)
               absorbClasses = eqClass (==) (rs++exprIsc2list l++exprIsc2list r)
               incons = [x |x<-exprIsc2list r,x==notCpl (sign l) l]
               absor0  = [t' | t'<-exprUni2list l, f'<-rs++exprIsc2list r, t'==f']
               absor0' = [t' | t'<-exprUni2list r, f'<-rs++exprIsc2list l, t'==f']
               absor1  = [(t', exprUni2list l>-[t']) | t'<-exprUni2list l, ECpl f' _<-rs++exprIsc2list r, t'==f']++[(x, exprUni2list l>-[x]) | x@(ECpl t' _)<-exprUni2list l, f'<-rs++exprIsc2list r, t'==f']
               absor1' = [(t', exprUni2list r>-[t']) | t'<-exprUni2list r, ECpl f' _<-rs++exprIsc2list l, t'==f']++[(x, exprUni2list r>-[x]) | x@(ECpl t' _)<-exprUni2list r, f'<-rs++exprIsc2list l, t'==f']
               absorbAsy = eqClass same eList where e `same` e' = isAsy e && isAsy e' && e == flp e'
               absorbAsyRfx = eqClass same eList where e `same` e' = isRfx e && isAsy e && isRfx e' && isAsy e' && e == flp e'
               eList  = rs++exprIsc2list l++exprIsc2list r
     nM posNeg (EUni (l,r) sgn) rs
-- Absorb equals:    r\/r  -->  r
         | or [length cl>1 |cl<-absorbClasses]
              = ( foldr1 (.\/.) [head cl | cl<-absorbClasses]
                , [shw e++" \\/ "++shw e++" = "++shw e | cl<-absorbClasses, length cl>1, let e=head cl]
                , "<=>"
                )
-- Tautologies:
         | (not.null) tauts               = (vExpr sgn, ["let e = "++ shw (head tauts)++". Since -e\\/e = V we get"], "<=>")   -- r\/-r  -->  V
         | isTrue l                       = (vExpr sgn, ["V\\/x = V"], "<=>")                                                  -- r\/V   -->  V
         | isTrue r                       = (vExpr sgn, ["x\\/V = V"], "<=>")
-- Absorb -V:    r\/-V  --> r
         | isFalse l                      = (r, ["-V\\/x = x"], "<=>")
         | isFalse r                      = (l, ["x\\/-V = x"], "<=>")
-- Absorb:    (x/\\y)\\/y  =  y
         | isEIsc l && not (null absor0)  = let t'=head absor0  in (r, ["absorb "++shw l++" because of "++shw t'++", using law  (x/\\y)\\/y = y"], "<=>")
         | isEIsc r && not (null absor0') = let t'=head absor0' in (r, ["absorb "++shw r++" because of "++shw t'++", using law  (x/\\y)\\/x = x"], "<=>")
-- Absorb:    (x/\\-y)\\/y  =  x\\/y
         | isEIsc l && not (null absor1)
              = ( case head absor1 of
                    (_,[]) -> r
                    (_,ts) -> foldr1 (./\.) ts .\/. r
                , ["absorb "++shw t'++", using law (x/\\-y)\\/y  =  x\\/y" | (t',_)<-absor1]
                , "<=>"
                )
         | isEIsc r && not (null absor1')
              = ( case head absor1' of
                    (_,[]) -> l
                    (_,ts) -> l .\/. foldr1 (./\.) ts
                , ["absorb "++shw t'++", using law x\\/(y/\\-x)  =  x\\/y" | (t',_)<-absor1' ]
                , "<=>"
                )
         | otherwise = (t ./\. f, steps++steps', fEqu [equ',equ''])
         where (t,steps, equ')  = nM posNeg l []
               (f,steps',equ'') = nM posNeg r (l:rs)
               absorbClasses = eqClass (==) (rs++exprUni2list l++exprUni2list r)
               tauts = [x' |x<-exprUni2list r,x==notCpl (sign l) l, ECpl x' _<-[x,l]]
               absor0  = [t' | t'<-exprIsc2list l, f'<-rs++exprUni2list r, t'==f']
               absor0' = [t' | t'<-exprIsc2list r, f'<-rs++exprUni2list l, t'==f']
               absor1  = [(t', exprIsc2list l>-[t']) | t'<-exprIsc2list l, ECpl f' _<-rs++exprUni2list r, t'==f']++[(x, exprIsc2list l>-[x]) | x@(ECpl t' _)<-exprIsc2list l, f'<-rs++exprUni2list r, t'==f']
               absor1' = [(t', exprIsc2list r>-[t']) | t'<-exprIsc2list r, ECpl f' _<-rs++exprUni2list l, t'==f']++[(x, exprIsc2list r>-[x]) | x@(ECpl t' _)<-exprIsc2list r, f'<-rs++exprUni2list l, t'==f']
     nM _ (EFlp e _) _ | isSym e =  (e,[shw e++" is symmetric"],"<=>")
     nM _ x _               = (x,[],"<=>")

   exprIsc2list, exprUni2list, exprCps2list, exprRad2list :: Expression -> [Expression]
   exprIsc2list (EIsc (l,r) _) = exprIsc2list l++exprIsc2list r
   exprIsc2list r              = [r]
   exprUni2list (EUni (l,r) _) = exprUni2list l++exprUni2list r
   exprUni2list r              = [r]
   exprCps2list (ECps (l,r) _) = exprCps2list l++exprCps2list r
   exprCps2list r              = [r]
   exprRad2list (ERad (l,r) _) = exprRad2list l++exprRad2list r
   exprRad2list r              = [r]


   fEqu :: [String] -> String
   fEqu ss = if and [s=="<=>" | s<-ss] then "<=>" else "==>"

   nfProof :: (Expression -> String) -> Expression -> Proof Expression
   nfProof shw = nfPr shw True True -- The first boolean True means that clauses are derived using <=> derivations. The second True means that a disjunctive normal form is produced.
   nfPr :: (Expression -> String) -> Bool -> Bool -> Expression -> [(Expression, [String], String)]
   nfPr shw eq dnf expr
    = if expr==res
      then [(expr,[],"<=>")]
      else (expr,steps,equ):nfPr shw eq dnf (simplify res)
    where (res,steps,equ) = normStep shw eq dnf False expr

   cfProof :: (Expression -> String) -> Expression -> Proof Expression
   cfProof shw expr
    = [line | step, line<-init pr]++
      [line | step', line<-init pr']++
      [last ([(expr,[],"<=>")]++
             [line | step, line<-pr]++
             [line | step', line<-pr']
            )]
      where pr           = nfPr shw True False (simplify expr)
            (expr',_,_)  = if null pr then fatal 328 "last: empty list" else last pr
            step         = simplify expr/=expr' -- obsolete?    || and [null s | (_,ss,_)<-pr, s<-ss]
            pr'          = nfPr shw True False (simplify expr')
            step'        = simplify expr'/=simplify expr'' -- obsolete?    || and [null s | (_,ss,_)<-pr', s<-ss]
            (expr'',_,_) = if null pr' then fatal 337 "last: empty list" else last pr'

   conjNF :: Expression -> Expression
   conjNF expr = e where (e,_,_) = if null (cfProof (\_->"") expr) then fatal 340 "last: empty list" else last (cfProof (\_->"") expr)

   disjNF :: Expression -> Expression
   disjNF expr = e where (e,_,_) = if null (dfProof (\_->"") expr) then fatal 343 "last: empty list" else last (dfProof (\_->"") expr)

   dfProof :: (Expression -> String) -> Expression -> Proof Expression
   dfProof shw expr
    = [line | step, line<-init pr]++
      [line | step', line<-init pr']++
      [last ([(expr,[],"<=>")]++
             [line | step, line<-pr]++
             [line | step', line<-pr']
            )]
      where pr           = nfPr shw True True expr
            (expr',_,_)  = if null pr then fatal 356 "last: empty list" else last pr
            step         = simplify expr/=simplify expr'
            pr'          = nfPr shw True True expr'
            step'        = simplify expr'/=simplify expr''
            (expr'',_,_) = if null pr' then fatal 365 "last: empty list" else last pr'

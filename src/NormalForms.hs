  {-# OPTIONS_GHC -Wall #-}
module NormalForms (conjNF,disjNF,nfProof,nfPr,simplify,negRight)--,isFu)
where
   import Adl
   import Collection (Collection (rd))
   import ShowADL
   
   simplify :: Expression -> Expression
   simplify expr = expr' 
       where (expr',_,_) = last (simpProof expr)
   
   simpProof :: Expression -> [(Expression, [String], String)]
   simpProof expr    
    = if expr==res
      then [(expr,[],"<=>")]
      else (expr,steps,equ):simpProof res
    where (res,steps,equ) = normStep True True True expr

   normStep :: Bool -> Bool -> Bool -> Expression -> (Expression,[String],String)
   normStep eq    -- If eq==True, only equivalences are used. Otherwise, implications are used as well.
            dnf   -- If dnf=True, the result is in disjunctive normal form. Otherwise it is in conjunctive normal form
            simpl -- If True, only simplification rules are used, which is a subset of all rules. Consequently, simplification is implied by normalization.
            expr = (res,ss,equ)
    where
     (res,ss,equ) = norm expr []
     norm :: Expression -> [Expression] -> (Expression,[String],String)
     norm (K0 e')      _    = (K0 res', steps, equ')
                               where (res',steps,equ') = norm e' []
     norm (K1 e')      _    = (K1 res', steps, equ')
                               where (res',steps,equ') = norm e' []
     norm (Cp (Cp e')) _    = (e', ["compl of compl"],"<=>")
     norm (Cp (Fi fs)) _    = if simpl then (notCp res',steps,equ') else (Fu (map notCp fs), ["De Morgan"], "<=>")
                               where (res',steps,equ') = norm (Fi fs) []
     norm (Cp (Fu fs)) _    = if simpl then (notCp res',steps,equ') else (Fi (map notCp fs), ["De Morgan"], "<=>")
                               where (res',steps,equ') = norm (Fu fs) []
     norm (Cp (Fd ts)) _    = if not simpl && and [isNeg t| t<-ts] 
                                 then (F (map notCp ts), ["De Morgan"], "<=>")
                                 else (notCp res',steps,equ')
                               where (res',steps,equ') = norm (Fd ts) []
     norm (Cp e')      _    = (notCp res',steps,equ')
                               where (res',steps,equ') = norm e' []
     norm (Tc f)       _    = norm f []
     norm (F [t])      _   = norm t []
     norm (F (k:ks))   rs   | or [isF x|x<-k:ks]          = norm (F [y| x<-k:ks, y<-if isF x then unF x else [x]]) rs  -- haakjes verwijderen o.g.v. associativiteit
                            | or [isIdent x|x<-k:ks]      = (F [x|x<-k:ks,not (isIdent x)], ["x;I = x"], "<=>")
                            | not simpl && not eq && length ks>1 && isFd g && length gs>1
                                                          = (F (Fd (F [k,head gs]:tail gs):ks'), ["Peirce: r;(s!q) => (r;s)!q"],"==>")
                            | not simpl && not eq && isFd k && length ue>1
                                                          = (Fd (init ue++[F (last ue:ks)]), ["Peirce: (r!s);q => r!(s;q)"],"==>")
                            | not simpl && not eq && isFi k = (distribute F Fi isF isFi (F (k:ks)), ["distribute /\\ over ;"], "==>")
                            | not simpl && isFu k           = (distribute F Fu isF isFu (F (k:ks)), ["distribute \\/ over ;"], "<=>")
                            | not simpl && and [isNeg x|x<-(k:ks)]
                                                          = (notCp (Fd [notCp x| x<-(k:ks)]), ["De Morgan"], "<=>")
                            | not simpl && isFu (last (k:ks)) = (Fu [F (init (k:ks)++[t'])|Fu xs<-[last (k:ks)], t'<-xs], ["distribute \\/ over ;"], "<=>")
                            | otherwise                   = (if isF f then F (t:unF f) else F [t,f], steps++steps', fEqu [equ',equ''])
                            where (t,steps, equ')  = norm k []
                                  (f,steps',equ'') = norm (F ks) (k:rs)
                                  ue = unF k
                                  g@(Fd gs):ks' = ks

     norm (Fd [k])    _     = norm k []
     norm (Fd (k:ks)) rs    | or [isFd x|x<-k:ks]         = norm (Fd [y| x<-k:ks, y<-if isFd x then unF x else [x]]) rs
                            | or [isNot x|x<-k:ks]        = (F [x|x<-k:ks,not (isNot x)], ["x!-I = x"], "<=>")
                            | not simpl && not eq && isFu k = (distribute Fd Fu isFd isFu (Fd (k:ks)), ["distribute \\/ over !"], "==>")
                            | not simpl && isFi k         = (distribute Fd Fi isFd isFi (Fd (k:ks)), ["distribute /\\ over !"], "<=>")
                            | not simpl && and [isNeg x|x<-(k:ks)]
                                                          = (notCp (F [notCp x| x<-(k:ks)]), ["De Morgan"], "<=>")
                            | not simpl && length ks>1 && isNeg k && isPos g && isFunction k
                                                          = (F [notCp k,Fd ks], ["f-!g = f;g if f is a function"], "<=>")
                            | not simpl && length ks>1 && isPos k && isNeg g && isFunction (flp g)
                                                          = (Fd ( F[k,notCp g]:ks'), ["f!g- = f;g if g~ is a function"], "<=>")
                            | otherwise                   = (if isFd f then Fd (t:unF f) else Fd [t,f], steps++steps', fEqu [equ',equ''])
                            where (t,steps, equ')  = norm k []
                                  (f,steps',equ'') = norm (Fd ks) (k:rs)
                                  g:ks' = ks
     norm (Fi [k])  _       = norm k []
     norm (Fi (k:ks)) rs    | or [isFi x|x<-k:ks]         = norm (Fi [y| x<-k:ks, y<-if isFi x then unF x else [x]]) rs
                            | rd(k:ks)/=k:ks              = (Fi (rd (k:ks)), ["x/\\x = x"], "<=>")
                            | not (null incons)           = (Fu [], [showADL (notCp (head incons))++"/\\"++showADL (head incons)++" = V-"], "<=>")
                            | k==Fu []                    = (Fu [], ["inconsistency"], "<=>")
  -- this is unreachable    | k==Fi []                    = (Fi ks, ["x/\\V = x"], "<=>")
                            | or[x==Fu []|x<-ks]          = (Fu [], ["x/\\V- = V-"], "<=>")
                            | isFu k && not (null absor0) = let f'=head absor0 in (Fi ks, ["absorb "++showADL k++" because of "++showADL f'], "<=>")
                            | isFu k && not (null absor1) = let (ts,f')=head absor1 in (Fi (ts++ks), ["absorb "++showADL f'], "<=>")
                            | not simpl && isFu k && dnf  = (distribute Fi Fu isFi isFu (Fi (k:ks)), ["distribute \\/ over /\\"], "<=>")
                            | otherwise                   = (if isFi f then Fi (t:unF f) else Fi [t,f], steps++steps', fEqu [equ',equ''])
                            where (t,steps, equ')  = norm k []
                                  (f,steps',equ'') = norm (Fi ks) (k:rs)
                                  incons = [x|x<-ks,x==notCp k]
                                  absor0 = [t'| t'<-unF k, f'<-ks++rs, t'==f']
                                  absor1 = [(if length rest<=1 then rest else [Fu rest] , t')| t'<-unF k, f'<-ks++rs, notCp t'==f', rest<-[[x|x<-unF k,x/=t']]]
     norm (Fu [k]) _       = norm k []
     norm (Fu (k:ks)) rs    | or [isFu x|x<-k:ks]         = norm (Fu [y| x<-k:ks, y<-if isFu x then unF x else [x]]) rs
                            | rd(k:ks)/=k:ks              = (Fu (rd (k:ks)), ["x\\/x = x"], "<=>")
                            | not (null compl)            = (Fi [], [showADL (notCp (head compl))++"\\/"++showADL (head compl)++" = V"], "<=>")
                            | k==Fi []                    = (Fi [], ["tautology"], "<=>")
  -- this is unreachable    | k==Fu []                    = (Fu ks, ["x\\/V- = x"], "<=>")
                            | or[x==Fi []|x<-ks]          = (Fi [], ["x\\/V = V"], "<=>")
                            | isFi k && not (null absor0) = let f'=head absor0 in (Fu ks, ["absorb "++showADL k++" because of "++showADL f'++" ((x/\\y)\\/y = y))"], "<=>")
                            | isFi k && not (null absor1) = let (ts,f')=head absor1 in (Fu (ts++ks), ["absorb "++showADL f'++" ((x/\\y-)\\/y = x\\/y))"], "<=>")
                            | not simpl && isFi k && not dnf = (distribute Fu Fi isFu isFi (Fu (k:ks)), ["distribute /\\ over \\/"], "<=>")
                            | otherwise                   = (if isFu f then Fu (t:unF f) else Fu [t,f], steps++steps', fEqu [equ',equ''])
                            where (t,steps, equ')  = norm k []
                                  (f,steps',equ'') = norm (Fu ks) (k:rs)
                                  compl  = [x|x<-ks,x==notCp k]
                                  absor0 = [t'| t'<-unF k, f'<-ks++rs, t'==f']
                                  absor1 = [(if length rest<=1 then rest else [Fi rest] , t')| t'<-unF k, f'<-ks++rs, notCp t'==f', rest<-[[x|x<-unF k,x/=t']]]
     norm x _             = (x,[],"<=>")


   fEqu :: [String] -> String
   fEqu ss = if and [s=="<=>" | s<-ss] then "<=>" else "==>"

   isFu :: Expression -> Bool
   isFu Fu{}  = True
   isFu _     = False

   isFi :: Expression -> Bool
   isFi Fi{}  = True
   isFi _     = False
   
   isF :: Expression -> Bool
   isF  F{}   = True
   isF _      = False

   isFd :: Expression -> Bool
   isFd Fd{}  = True
   isFd _     = False
   
   unF :: Expression -> Expressions
   unF (Fi es')  = es'
   unF (Fu es')  = es'
   unF (Fd es')  = es'
   unF (F  es')  = es'
   unF x        = [x]


   distribute :: ([Expression] -> Expression)
              -> ([Expression] -> Expression)
              -> (Expression -> Bool)
              -> (Expression -> Bool)
              -> Expression
              -> Expression
   distribute f g isf isg = dis
    where
     dis x | isf x && null xs = g [f []]
           | isg x && null xs = g []
           | isg x && isg k   = dis (g (ys++ks))
           | isf x && isf k   = dis (f (ys++ks))
           | isf x            = g [f [p,q]| p<-if isg k then unF k else [k], q<-unF (dis (f ks))]
           | isg x            = g (unF (dis k)++ks)
  --       | null ks          = g [k]
           | otherwise        = g [x]             
           where xs = unF x
                 k:ks = xs
                 ys = unF k
   
   conjNF :: Expression -> Expression
   conjNF  expr = negRight (if null proof then expr else expr')
                  where (expr',_,_) = last proof
                        proof = nfPr True False expr

   disjNF :: Expression -> Expression
   disjNF  expr = negRight (if null proof then expr else expr')
                  where (expr',_,_) = last proof
                        proof = nfPr True True expr

   negRight :: Expression -> Expression
   negRight e' = e'

   nfProof :: Expression -> [(Expression, [String], String)]
   nfProof expr = nfPr True False expr -- Clauses are derived by means of the conjunctive form, using <=> derivations.
   nfPr :: Bool-> Bool-> Expression-> [(Expression, [String], String)]
   nfPr eq dnf expr
    = if expr==res
      then [(expr,[],"<=>")]
      else (expr,steps,equ):nfPr eq dnf (simplify res)
    where (res,steps,equ) = normStep eq dnf False expr


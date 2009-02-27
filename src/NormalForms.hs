  {-# TODOOPTIONS_GHC -Wall #-}
module NormalForms (conjNF,disjNF,nfProof,nfPr,simplify,negRight,isFu)
where
   import Adl
   import Collection (Collection (rd))
   import ShowADL
   
   simplify :: Expression -> Expression
   simplify expr = expr' 
       where (expr',_,_) = last (simpProof expr)
   
   simpProof expr    -- dus levert op: [(Expression,[String],String)]
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
     norm (K0 e)       rs   = (K0 res, steps, equ)
                              where (res,steps,equ) = norm e []
     norm (K1 e)       rs   = (K1 res, steps, equ)
                              where (res,steps,equ) = norm e []
     norm (Cp (Cp e))  rs   = (e, ["compl of compl"],"<=>")
     norm (Cp (Fi fs)) rs   = if simpl then (notCp res,steps,equ) else (Fu (map notCp fs), ["De Morgan"], "<=>")
                              where (res,steps,equ) = norm (Fi fs) []
     norm (Cp (Fu fs)) rs   = if simpl then (notCp res,steps,equ) else (Fi (map notCp fs), ["De Morgan"], "<=>")
                              where (res,steps,equ) = norm (Fu fs) []
     norm (Cp (Fd ts)) rs   = if not simpl && and [isNeg t| t<-ts] then (F (map notCp ts), ["De Morgan"], "<=>") else
                              (notCp res,steps,equ)
                              where (res,steps,equ) = norm (Fd ts) []
     norm (Cp e)       rs   = (notCp res,steps,equ)
                              where (res,steps,equ) = norm e []
     norm (Tc f)       rs   = norm f []
     norm (F [t])      rs   = norm t []
     norm (F (e:es))   rs   | or [isF x|x<-e:es]          = norm (F [y| x<-e:es, y<-if isF x then unF x else [x]]) rs  -- haakjes verwijderen o.g.v. associativiteit
                            | or [isIdent x|x<-e:es]      = (F [x|x<-e:es,not (isIdent x)], ["x;I = x"], "<=>")
                            | not simpl && not eq && length es>1 && isFd g && length gs>1
                                                          = (F (Fd (F [e,head gs]:tail gs):es'), ["Peirce: r;(s!q) => (r;s)!q"],"==>")
                            | not simpl && not eq && isFd e && length ue>1
                                                          = (Fd (init ue++[F (last ue:es)]), ["Peirce: (r!s);q => r!(s;q)"],"==>")
                            | not simpl && not eq && isFi e = (distribute F Fi isF isFi (F (e:es)), ["distribute /\\ over ;"], "==>")
                            | not simpl && isFu e           = (distribute F Fu isF isFu (F (e:es)), ["distribute \\/ over ;"], "<=>")
                            | not simpl && and [isNeg x|x<-(e:es)]
                                                          = (notCp (Fd [notCp x| x<-(e:es)]), ["De Morgan"], "<=>")
                            | not simpl && isFu (last (e:es)) = (Fu [F (init (e:es)++[t])|Fu xs<-[last (e:es)], t<-xs], ["distribute \\/ over ;"], "<=>")
                            | otherwise                   = (if isF f then F (t:unF f) else F [t,f], steps++steps', fEqu [equ,equ'])
                            where (t,steps, equ)  = norm e []
                                  (f,steps',equ') = norm (F es) (e:rs)
                                  ue = unF e
                                  g@(Fd gs):es' = es
     norm (Fd [e])    rs    = norm e []
     norm (Fd (e:es)) rs    | or [isFd x|x<-e:es]         = norm (Fd [y| x<-e:es, y<-if isFd x then unF x else [x]]) rs
                            | or [isNot x|x<-e:es]        = (F [x|x<-e:es,not (isNot x)], ["x!-I = x"], "<=>")
                            | not simpl && not eq && isFu e = (distribute Fd Fu isFd isFu (Fd (e:es)), ["distribute \\/ over !"], "==>")
                            | not simpl && isFi e         = (distribute Fd Fi isFd isFi (Fd (e:es)), ["distribute /\\ over !"], "<=>")
                            | not simpl && and [isNeg x|x<-(e:es)]
                                                          = (notCp (F [notCp x| x<-(e:es)]), ["De Morgan"], "<=>")
                            | not simpl && length es>1 && isNeg e && isPos g && isFunction e
                                                          = (F [notCp e,Fd es], ["f-!g = f;g if f is a function"], "<=>")
                            | not simpl && length es>1 && isPos e && isNeg g && isFunction (flp g)
                                                          = (Fd ( F[e,notCp g]:es'), ["f!g- = f;g if g~ is a function"], "<=>")
                            | otherwise                   = (if isFd f then Fd (t:unF f) else Fd [t,f], steps++steps', fEqu [equ,equ'])
                            where (t,steps, equ)  = norm e []
                                  (f,steps',equ') = norm (Fd es) (e:rs)
                                  ue = unF e
                                  g:es' = es
     norm (Fi [e]) rs       = norm e []
     norm (Fi (e:es)) rs    | or [isFi x|x<-e:es]         = norm (Fi [y| x<-e:es, y<-if isFi x then unF x else [x]]) rs
                            | rd(e:es)/=e:es              = (Fi (rd (e:es)), ["x/\\x = x"], "<=>")
                            | not (null incons)           = (Fu [], [showADL (notCp (head incons))++"/\\"++showADL (head incons)++" = V-"], "<=>")
                            | e==Fu []                    = (Fu [], ["inconsistency"], "<=>")
  -- this is unreachable    | e==Fi []                    = (Fi es, ["x/\\V = x"], "<=>")
                            | or[x==Fu []|x<-es]          = (Fu [], ["x/\\V- = V-"], "<=>")
                            | isFu e && not (null absor0) = let f=head absor0 in (Fi es, ["absorb "++showADL e++" because of "++showADL f], "<=>")
                            | isFu e && not (null absor1) = let (ts,f)=head absor1 in (Fi (ts++es), ["absorb "++showADL f], "<=>")
                            | not simpl && isFu e && dnf  = (distribute Fi Fu isFi isFu (Fi (e:es)), ["distribute \\/ over /\\"], "<=>")
                            | otherwise                   = (if isFi f then Fi (t:unF f) else Fi [t,f], steps++steps', fEqu [equ,equ'])
                            where (t,steps, equ)  = norm e []
                                  (f,steps',equ') = norm (Fi es) (e:rs)
                                  incons = [x|x<-es,x==notCp e]
                                  absor0 = [t| t<-unF e, f<-es++rs, t==f]
                                  absor1 = [(if length rest<=1 then rest else [Fu rest] , t)| t<-unF e, f<-es++rs, notCp t==f, rest<-[[x|x<-unF e,x/=t]]]
     norm (Fu [e]) rs       = norm e []
     norm (Fu (e:es)) rs    | or [isFu x|x<-e:es]         = norm (Fu [y| x<-e:es, y<-if isFu x then unF x else [x]]) rs
                            | rd(e:es)/=e:es              = (Fu (rd (e:es)), ["x\\/x = x"], "<=>")
                            | not (null compl)            = (Fi [], [showADL (notCp (head compl))++"\\/"++showADL (head compl)++" = V"], "<=>")
                            | e==Fi []                    = (Fi [], ["tautology"], "<=>")
  -- this is unreachable    | e==Fu []                    = (Fu es, ["x\\/V- = x"], "<=>")
                            | or[x==Fi []|x<-es]          = (Fi [], ["x\\/V = V"], "<=>")
                            | isFi e && not (null absor0) = let f=head absor0 in (Fu es, ["absorb "++showADL e++" because of "++showADL f++" ((x/\\y)\\/y = y))"], "<=>")
                            | isFi e && not (null absor1) = let (ts,f)=head absor1 in (Fu (ts++es), ["absorb "++showADL f++" ((x/\\y-)\\/y = x\\/y))"], "<=>")
                            | not simpl && isFi e && not dnf = (distribute Fu Fi isFu isFi (Fu (e:es)), ["distribute /\\ over \\/"], "<=>")
                            | otherwise                   = (if isFu f then Fu (t:unF f) else Fu [t,f], steps++steps', fEqu [equ,equ'])
                            where (t,steps, equ)  = norm e []
                                  (f,steps',equ') = norm (Fu es) (e:rs)
                                  compl  = [x|x<-es,x==notCp e]
                                  absor0 = [t| t<-unF e, f<-es++rs, t==f]
                                  absor1 = [(if length rest<=1 then rest else [Fi rest] , t)| t<-unF e, f<-es++rs, notCp t==f, rest<-[[x|x<-unF e,x/=t]]]
     norm x           rs    = (x,[],"<=>")


   fEqu ss = if and [s=="<=>" | s<-ss] then "<=>" else "==>"
   isFu (Fu fs) = True
   isFu _       = False
   isFi (Fi fs) = True
   isFi _       = False
   isF  (F _)   = True
   isF _        = False
   isFd (Fd _)  = True
   isFd _       = False
   unF (Fi es)  = es
   unF (Fu es)  = es
   unF (Fd es)  = es
   unF (F  es)  = es
   unF x        = [x]


   distribute f g isf isg = dis
    where
     dis x | isf x && null xs = g [f []]
           | isg x && null xs = g []
           | isg x && isg e   = dis (g (ys++es))
           | isf x && isf e   = dis (f (ys++es))
           | isf x            = g [f [p,q]| p<-if isg e then unF e else [e], q<-unF (dis (f es))]
           | isg x            = g (unF (dis e)++es)
  --       | null es          = g [e]
           | otherwise        = g [x]             
           where xs = unF x
                 e:es = xs
                 ys = unF e

   conjNF  expr = negRight (if null proof then expr else expr')
                  where (expr',motives,equ) = last proof
                        proof = nfPr True  False expr
   conjNF' expr = negRight (if null proof then expr else expr')
                  where (expr',motives,equ) = last proof
                        proof = nfPr False False expr
   disjNF  expr = negRight (if null proof then expr else expr')
                  where (expr',motives,equ) = last proof
                        proof = nfPr True  True expr
   disjNF' expr = negRight (if null proof then expr else expr')
                  where (expr',motives,equ) = last proof
                        proof = nfPr False True expr
   negRight e = e

   nfProof expr = nfPr True False expr -- Clauses are derived by means of the conjunctive form, using <=> derivations.
   nfPr eq dnf expr
    = if expr==res
      then [(expr,[],"<=>")]
      else (expr,steps,equ):nfPr eq dnf (simplify res)
    where (res,steps,equ) = normStep eq dnf False expr


{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms
  (conjNF,disjNF,normPA,nfProof,cfProof,dfProof,proofPA,simplify)
where
   import DatabaseDesign.Ampersand.Basics
   import DatabaseDesign.Ampersand.ADL1.ECArule
   import DatabaseDesign.Ampersand.Classes.Relational 
   import DatabaseDesign.Ampersand.ADL1.Expression 
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import DatabaseDesign.Ampersand.Fspec.Fspec
   import DatabaseDesign.Ampersand.Fspec.ShowADL  -- for debug purposes only
   import Data.List (nub {- , intercalate -} )
--   import Debug.Trace
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
     (res,ss,equ) = nM True expr []
     nM :: Bool -> Expression -> [Expression] -> (Expression,[String],String)
-- posCpl indicates whether the expression is positive under a complement. It is False when expr is inside a complemented expression.
     nM posCpl (EEqu (l,r)) _     | simpl = (t .==. f, steps++steps', fEqu [equ',equ''])
                                            where (t,steps, equ')  = nM posCpl l []  -- TODO: the use of posCpl is erroneous
                                                  (f,steps',equ'') = nM posCpl r []  -- TODO: the use of posCpl is erroneous
     nM posCpl (EImp (l,r)) _     | simpl = (t .|-. f, steps++steps', fEqu [equ',equ''])
                                            where (t,steps, equ')  = nM (not posCpl) l []
                                                  (f,steps',equ'') = nM posCpl r []
     nM posCpl (EUni (EUni (l,k),r)) rs   = nM posCpl (l .\/. (k .\/. r)) rs  -- standardize, using associativity of .\/.
     nM posCpl (EUni (l,r)) rs    | simpl = (t .\/. f, steps++steps', fEqu [equ',equ''])
                                            where (t,steps, equ')  = nM posCpl l []
                                                  (f,steps',equ'') = nM posCpl r (l:rs)
     nM posCpl (EIsc (EIsc (l,k),r)) rs   = nM posCpl (l ./\. (k ./\. r)) rs  -- standardize, using associativity of ./\.
     nM posCpl (EIsc (l,r)) rs    | simpl = (t ./\. f, steps++steps', fEqu [equ',equ''])
                                            where (t,steps, equ')  = nM posCpl l []
                                                  (f,steps',equ'') = nM posCpl r (l:rs)
     nM posCpl (ECps (ECps (l,k),r)) rs   = nM posCpl (l .:. (k .:. r)) rs  -- standardize, using associativity of .:. 
                                                -- Note: function shiftL and shiftR make use of the fact that this normalizes to (l .:. (k .:. r))
     nM posCpl (ECps (l,r)) rs    | simpl = (t .:. f, steps++steps', fEqu [equ',equ''])
                                             where (t,steps, equ')  = nM posCpl l []
                                                   (f,steps',equ'') = nM posCpl r (l:rs)
     nM posCpl (ELrs (l,r)) _     | simpl = (t ./. f, steps++steps', fEqu [equ',equ''])
                                            where (t,steps, equ')  = nM posCpl l []
                                                  (f,steps',equ'') = nM (not posCpl) r []
     nM posCpl (ERrs (l,r)) _     | simpl = (t .\. f, steps++steps', fEqu [equ',equ''])
                                            where (t,steps, equ')  = nM (not posCpl) l []
                                                  (f,steps',equ'') = nM posCpl r []
     nM posCpl (EDia (l,r)) _     | simpl = (t .<>. f, steps++steps', fEqu [equ',equ''])
                                            where (t,steps, equ')  = nM posCpl l []  -- TODO: the use of posCpl is erroneous
                                                  (f,steps',equ'') = nM posCpl r []  -- TODO: the use of posCpl is erroneous
     nM posCpl (ERad (ERad (l,k),r)) rs   = nM posCpl (l .!. (k .!. r)) rs  -- standardize, using associativity of .!.
     nM posCpl (ERad (l,r)) rs    | simpl = (t .!. f, steps++steps', fEqu [equ',equ''])
                                            where (t,steps, equ')    = nM posCpl l []
                                                  (f,steps',equ'')   = nM posCpl r (l:rs)
     nM posCpl (EPrd (EPrd (l,k),r)) rs   = nM posCpl (l .*. (k .*. r)) rs  -- standardize, using associativity of .*.
     nM posCpl (EPrd (l,r)) _     | simpl = (t .*. f, steps++steps', fEqu [equ',equ''])
                                            where (t,steps, equ')  = nM posCpl l []
                                                  (f,steps',equ'') = nM posCpl r []
     nM posCpl (EKl0 e)              _    = (EKl0 res', steps, equ')
                                            where (res',steps,equ') = nM posCpl e []
     nM posCpl (EKl1 e)              _    = (EKl1 res', steps, equ')
                                            where (res',steps,equ') = nM posCpl e []
     nM posCpl (ECpl (ECpl e))         rs = nM posCpl e rs
     nM posCpl (ECpl e) _         | simpl = (notCpl res',steps,equ')
                                            where (res',steps,equ') = nM (not posCpl) e []
     nM posCpl (EBrk e)                _  = nM posCpl e []
     nM posCpl (EFlp (ECpl e))         rs = nM posCpl (notCpl (flp e)) rs
     nM _      x _                | simpl = (x,[],"<=>")
-- up to here, simplification has been treated. The remaining rules can safely assume  simpl==False
     nM _      (EEqu (l,r)) _                            = ((l .|-. r) ./\. (r .|-. l), ["remove ="],"<=>")
     nM _      (EImp (x,ELrs (z,y))) _                   = (x .:. y .|-. z, ["remove left residual (/)"],"<=>")
     nM _      (EImp (y,ERrs (x,z))) _                   = (x .:. y .|-. z, ["remove right residual (\\)"],"<=>")
     nM _      (EImp (l,r)) _                            = (notCpl l .\/. r, ["remove |-"],"<=>")
--   nM posCpl e@(ECpl EIsc{}) _           | posCpl==dnf = (deMorganEIsc e, ["De Morgan"], "<=>")
--   nM posCpl e@(ECpl EUni{}) _           | posCpl/=dnf = (deMorganEUni e, ["De Morgan"], "<=>")
     nM _      e@(ECpl EIsc{}) _                         = (deMorganEIsc e, ["De Morgan"], "<=>")
     nM _      e@(ECpl EUni{}) _                         = (deMorganEUni e, ["De Morgan"], "<=>")
     nM _      e@(ECpl (ERad (_,ECpl{}))) _              = (deMorganERad e, ["De Morgan"], "<=>")
     nM _      e@(ECpl (ERad (ECpl{},_))) _              = (deMorganERad e, ["De Morgan"], "<=>")
     nM _      e@(ECpl (ECps (ECpl{},ECpl{}))) _         = (deMorganECps e, ["De Morgan"], "<=>")
     nM posCpl (ECpl e) _                                = (notCpl res',steps,equ')
                                                           where (res',steps,equ') = nM (not posCpl) e []
     nM _      (ECps (EEps c (Sign s _),EEps c' (Sign _  t'))) _ | c ==c' = (EEps c  (Sign s t'), [], "<=>")
     nM _      (ECps (EEps c (Sign s t),EEps c' (Sign _  t'))) _ | c ==t  = (EEps c' (Sign s t'), [], "<=>")
     nM _      (ECps (EEps c (Sign s _),EEps c' (Sign s' t'))) _ | s'==c' = (EEps c  (Sign s t'), [], "<=>")
     nM _      (ECps (EEps c (Sign s _),ECps(EEps c' (Sign _  t'),r))) _ | c ==c' = (ECps (EEps c  (Sign s t'),r), [], "<=>")
     nM _      (ECps (EEps c (Sign s t),ECps(EEps c' (Sign _  t'),r))) _ | c ==t  = (ECps (EEps c' (Sign s t'),r), [], "<=>")
     nM _      (ECps (EEps c (Sign s _),ECps(EEps c' (Sign s' t'),r))) _ | s'==c' = (ECps (EEps c  (Sign s t'),r), [], "<=>")
     nM _      (ECps (ERrs (x,e),y)) _ | not eq && isIdent e = (ERrs (x,y), ["Jipsen&Tsinakis: (x\\I);y |- x\\y"], "==>")
     nM _      (ECps (x,ELrs (e,y))) _ | not eq && isIdent e = (ELrs (x,y), ["Jipsen&Tsinakis: x;(I/y) |- x/y"], "==>")
     nM _      (ECps (ERrs (x,y),z)) _          | not eq = (ERrs (x,ECps (y,z)), ["Jipsen&Tsinakis: (x\\y);z |- x\\(y;z)"], "==>")
     nM _      (ECps (x,ELrs (y,z))) _          | not eq = (ERrs (x,ECps (y,z)), ["Jipsen&Tsinakis: x;(y/z) |- (x;y)/z"], "==>")
     nM _      (ECps (ERrs (x,y),ERrs (y',z))) _ | not eq && y==y' = (ERrs (x,z), ["Jipsen&Tsinakis: (x\\y);(y\\z) |- x\\z"], "==>")
     nM _      (ECps (ELrs (x,y),ELrs (y',z))) _ | not eq && y==y' = (ERrs (x,z), ["Jipsen&Tsinakis: (x/y);(y/z) |- x/z"], "==>")
     nM _      (ECps (ERrs (x,y),ERrs (y',z))) _ | y==y' && x==y && x==z = (ERrs (x,z), ["Jipsen&Tsinakis: (x\\x);(x\\x) = x\\x"], "<=>")
     nM _      (ECps (ELrs (x,y),ELrs (y',z))) _ | y==y' && x==y && x==z = (ERrs (x,z), ["Jipsen&Tsinakis: (x/x);(x/x) = x/x"], "<=>")
     nM _      (ECps (x,ERrs (y,z))) _    | x==y && x==z = (x, ["Jipsen&Tsinakis: x;(x\\x) = x"], "<=>")
     nM _      (ECps (ELrs (x,y),z)) _    | x==z && y==z = (x, ["Jipsen&Tsinakis: (x/x);x = x"], "<=>")
     nM _      (ECps (l,r)) _                | isIdent l = (r, ["I;x = x"], "<=>")
     nM _      (ECps (l,r)) _                | isIdent r = (l, ["x;I = x"], "<=>")
     nM True   (ECps (r,ERad (s,q))) _          | not eq = ((r.:.s).!.q, ["Peirce: r;(s!q) |- (r;s)!q"],"==>")
     nM True   (ECps (ERad (r,s),q)) _          | not eq = (r.!.(s.:.q), ["Peirce: (r!s);q |- r!(s;q)"],"==>")
     nM True   (ECps (EIsc (r,s),q)) _          | not eq = ((r.:.q)./\.(s.:.q), ["distribute ; over /\\"],"==>")
     nM True   (ECps (r,EIsc (s,q))) _          | not eq = ((r.:.s)./\.(r.:.q), ["distribute ; over /\\"],"==>")
     nM _      (ECps (EUni (q,s),r)) _                   = ((q.:.r).\/.(s.:.r), ["distribute ; over \\/"],"<=>")
     nM _      (ECps (l,EUni (q,s))) _                   = ((l.:.q).\/.(l.:.s), ["distribute ; over \\/"],"<=>")
     nM _      x@(ECps (l@EFlp{},r)) _ | not eq && flp l==r && isInj l   = (EDcI (source x), ["r~;r |- I (r is univalent)"], "==>")
     nM _      x@(ECps (l,       r)) _ | not eq && l==flp r && isInj l   = (EDcI (source x), ["r;r~ |- I (r is injective)"], "==>")
     nM _      x@(ECps (l@EFlp{},r)) _ | flp l==r && isInj l && isTot l  = (EDcI (source x), ["r~;r=I because r is univalent and surjective"], "<=>")
     nM _      x@(ECps (l,       r)) _ | l==flp r && isInj l && isTot l  = (EDcI (source x), ["r;r~=I because r is injective and total"], "<=>")
     nM posCpl (ECps (l,r))           rs                   = (t .:. f, steps++steps', fEqu [equ',equ''])
                                                               where (t,steps, equ')  = nM posCpl l []
                                                                     (f,steps',equ'') = nM posCpl r (l:rs)
     nM _      x@(EEps i sgn) _ | source sgn==i && i==target sgn = (EDcI i, ["source and target are equal to "++name i++", so "++showADL x++"="++showADL (EDcI i)], "<=>")
     nM _      (ELrs (ECps (x,y),z)) _ | not eq && y==z    = (x,     ["(x;y)/y |- x"], "==>")
     nM _      (ELrs (ECps (x,y),z)) _ | not eq && flp x==z= (flp y, [case (x, y) of
                                                                           (EFlp _, EFlp _) -> "(SJ) (x~;y~)/x |- y"
                                                                           (     _, EFlp _) -> "(SJ) (x;y~)/x~ |- y"
                                                                           (EFlp _,      _) -> "(SJ) (x~;y)/x |- y~"
                                                                           (     _,      _) -> "(SJ) (x;y)/x~ |- y~"], "==>")
     nM _      (ELrs (ELrs (x,z),y)) _                     = (ELrs (x,ECps (y,z)), ["Jipsen&Tsinakis: x/yz = (x/z)/y"], "<=>")
     nM posCpl (ELrs (l,r)) _                              = (t ./. f, steps++steps', fEqu [equ',equ''])
                                                             where (t,steps, equ')  = nM posCpl l []
                                                                   (f,steps',equ'') = nM (not posCpl) r []
     nM _      (ERrs (y,ERrs (x,z))) _                     = (ERrs (ECps (x,y),z), ["Jipsen&Tsinakis: xy\\z = y\\(x\\z)"], "<=>")
     nM _      (ERrs (x,ECps (y,z))) _ | not eq && x==y    = (z,     ["x\\(x;y) |- y"], "==>")
     nM posCpl (ERrs (l,r)) _                              = (t .\. f, steps++steps', fEqu [equ',equ''])
                                                             where (t,steps, equ')  = nM (not posCpl) l []
                                                                   (f,steps',equ'') = nM posCpl r []
     nM posCpl (EDia (l,r)) _                              = (t .<>. f, steps++steps', fEqu [equ',equ''])
                                                             where (t,steps, equ')  = nM posCpl l []
                                                                   (f,steps',equ'') = nM posCpl r []
     nM _      (ERad (l,r)) _                   | isImin l = (r, ["-I!x = x"], "<=>")
     nM _      (ERad (l,r)) _                   | isImin r = (l, ["x!-I = x"], "<=>")
--     nM False  (ERad (ECps (r,s),q)) _            | not eq = (r.:.(s.!.q), ["Peirce: (r;s)!q |- r;(s!q)"],"==>")  -- SJ 20131124 TODO: check this rule. It is wrong!
--     nM False  (ERad (r,ECps (s,q))) _            | not eq = ((r.!.s).:.q, ["Peirce: (r!s);q |- r!(s;q)"],"==>")  -- SJ 20131124 TODO: check this rule. It is wrong!
     nM False  (ERad (EUni (r,s),q)) _            | not eq = ((r.!.q).\/.(s.!.q), ["distribute ! over \\/"],"==>")
     nM False  (ERad (r,EUni (s,q))) _            | not eq = ((r.!.s).\/.(r.!.q), ["distribute ! over \\/"],"==>")
     nM _      (ERad (EIsc (q,s),r)) _                     = ((q.!.r)./\.(s.!.r), ["distribute ! over /\\"],"<=>")
     nM _      (ERad (l,EIsc (q,s))) _                     = ((l.!.q)./\.(l.!.s), ["distribute ! over /\\"],"<=>")
     nM _      (ERad(ECpl l,r))      _                     = (flp l .\. r, [case l of EFlp{} -> "-l~!r = l\r"; _ -> "-l!r = l~\r"], "<=>")
     nM _      (ERad(l,ECpl r))      _                     = (l ./. flp r, [case r of EFlp{} -> "l!-r~ = l/r"; _ -> "l!-r = l/r~"], "<=>")
     nM posCpl (ERad (l,r))         rs                     = (t .!. f, steps++steps', fEqu [equ',equ''])
                                                                 where (t,steps, equ')  = nM posCpl l []
                                                                       (f,steps',equ'') = nM posCpl r (l:rs)
     nM _      (EPrd (l,EPrd (_,r))) _                     = (l .*. r, ["eliminate middle in cartesian product"], "<=>")
     nM posCpl (EPrd (l,r)) _                              = (t .*. f, steps++steps', fEqu [equ',equ''])
                                                                 where (t,steps, equ')  = nM posCpl l []
                                                                       (f,steps',equ'') = nM posCpl r []
     nM posCpl (EIsc (EUni (l,k),r)) _       | posCpl==dnf = ((l./\.r) .\/. (k./\.r), ["distribute /\\ over \\/"],"<=>")
     nM posCpl (EIsc (l,EUni (k,r))) _       | posCpl==dnf = ((l./\.k) .\/. (l./\.r), ["distribute /\\ over \\/"],"<=>")
     nM posCpl x@(EIsc (l,r)) rs
-- Absorb equals:    r/\r  -->  r
         | or [length cl>1 |cl<-absorbClasses]
              = ( case absorbClasses of [] -> fatal 243 "Going into foldr1 with empty absorbClasses"; _ -> foldr1 (./\.) [head cl | cl<-absorbClasses]
                , [shw e++" /\\ "++shw e++" = "++shw e | cl<-absorbClasses, length cl>1, let e=head cl]
                , "<=>"
                )
-- Absorb True:    r/\V  --> r
         | isTrue l                      = (r, ["V/\\x = x"], "<=>")
         | isTrue r                      = (l, ["x/\\V = x"], "<=>")
-- Inconsistency:    r/\-r   -->  False
         | not (null incons)
              = let i = head incons in (notCpl (EDcV (sign i)), [shw (notCpl i)++" /\\ "++shw i++" = V-"], "<=>")
-- Inconsistency:    x/\\V-  -->  False
         | isFalse l                     = (notCpl (EDcV (sign x)), ["-V/\\x = -V"], "<=>")
         | isFalse r                     = (notCpl (EDcV (sign x)), ["x/\\-V = -V"], "<=>")
-- Absorb if r is antisymmetric:    r/\r~ --> I
         | not eq && or [length cl>1 |cl<-absorbAsy]
              = ( foldr1 (./\.) [if length cl>1 then EDcI (source e) else e | cl<-absorbAsy, let e=head cl] 
                , [shw e++" /\\ "++shw (flp e)++" |- I, because"++shw e++" is antisymmetric" | cl<-absorbAsy, let e=head cl]
                , "==>"
                )
-- Absorb if r is antisymmetric and reflexive:    r/\r~ = I
         | or [length cl>1 |cl<-absorbAsyRfx]
              = ( foldr1 (./\.) [if length cl>1 then EDcI (source e) else e | cl<-absorbAsyRfx, let e=head cl] 
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
         where (t,steps, equ')  = nM posCpl l []
               (f,steps',equ'') = nM posCpl r (l:rs)
               absorbClasses = eqClass (==) (exprIsc2list l++exprIsc2list r)
               incons = [conjunct |conjunct<-exprIsc2list r,conjunct==notCpl l]
               absor0  = [disjunct | disjunct<-exprUni2list l, f'<-rs++exprIsc2list r, disjunct==f']
               absor0' = [disjunct | disjunct<-exprUni2list r, f'<-rs++exprIsc2list l, disjunct==f']
               absor1  = [(disjunct, exprUni2list l>-[disjunct]) | disjunct<-exprUni2list l, ECpl f'<-rs++exprIsc2list r, disjunct==f']++
                         [(disjunct, exprUni2list l>-[disjunct]) | disjunct@(ECpl t')<-exprUni2list l, f'<-rs++exprIsc2list r, t'==f']
               absor1' = [(disjunct, exprUni2list r>-[disjunct]) | disjunct<-exprUni2list r, ECpl f'<-rs++exprIsc2list l, disjunct==f']++
                         [(disjunct, exprUni2list r>-[disjunct]) | disjunct@(ECpl t')<-exprUni2list r, f'<-rs++exprIsc2list l, t'==f']
               absorbAsy = eqClass same eList where e `same` e' = isAsy e && isAsy e' && e == flp e'
               absorbAsyRfx = eqClass same eList where e `same` e' = isRfx e && isAsy e && isRfx e' && isAsy e' && e == flp e'
               eList  = rs++exprIsc2list l++exprIsc2list r
     nM posCpl (EUni (EIsc (l,k),r)) _  | posCpl/=dnf = ((l.\/.r) ./\. (k.\/.r), ["distribute \\/ over /\\"],"<=>")
     nM posCpl (EUni (l,EIsc (k,r))) _  | posCpl/=dnf = ((l.\/.k) ./\. (l.\/.r), ["distribute \\/ over /\\"],"<=>")
     nM posCpl x@(EUni (l,r)) rs
-- Absorb equals:    r\/r  -->  r
         | or [length cl>1 |cl<-absorbClasses]   -- yields False if absorbClasses is empty
              = ( foldr1 (.\/.) [head cl | cl<-absorbClasses]  -- cl cannot be empty, because it is made by eqClass
                , [shw e++" \\/ "++shw e++" = "++shw e | cl<-absorbClasses, length cl>1, let e=head cl]
                , "<=>"
                )
-- Tautologies:
         | (not.null) tauts               = (EDcV (sign x), ["let e = "++ shw (head tauts)++". Since -e\\/e = V we get"], "<=>")   -- r\/-r  -->  V
         | isTrue l                       = (EDcV (sign x), ["V\\/x = V"], "<=>")                                                  -- r\/V   -->  V
         | isTrue r                       = (EDcV (sign x), ["x\\/V = V"], "<=>")
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
         | otherwise = (t .\/. f, steps++steps', fEqu [equ',equ''])
         where (t,steps, equ')  = nM posCpl l []
               (f,steps',equ'') = nM posCpl r (l:rs)
            -- absorption can take place if two terms are equal. So let us make a list of equal terms: absorbClasses (for substituting r\/r by r)
               absorbClasses = eqClass (==) (exprUni2list l++exprUni2list r)
            -- tautologies occur if -r\/r, so we are looking for pairs, (x,l) such that x== -l
               tauts = [t' |disjunct<-exprUni2list r,disjunct==notCpl l, ECpl t'<-[disjunct,l]]
               absor0  = [t' | t'<-exprIsc2list l, f'<-rs++exprUni2list r, t'==f']
               absor0' = [t' | t'<-exprIsc2list r, f'<-rs++exprUni2list l, t'==f']
               absor1  = [(t', exprIsc2list l>-[t']) | t'<-exprIsc2list l, ECpl f'<-rs++exprUni2list r, t'==f']++[(e, exprIsc2list l>-[e]) | e@(ECpl t')<-exprIsc2list l, f'<-rs++exprUni2list r, t'==f']
               absor1' = [(t', exprIsc2list r>-[t']) | t'<-exprIsc2list r, ECpl f'<-rs++exprUni2list l, t'==f']++[(e, exprIsc2list r>-[e]) | e@(ECpl t')<-exprIsc2list r, f'<-rs++exprUni2list l, t'==f']
     nM _ (EFlp e) _ | isSym e =  (e,[shw e++" is symmetric"],"<=>")
     nM _ x _               = (x,[],"<=>")

   fEqu :: [String] -> String
   fEqu ss = if and [s=="<=>" | s<-ss] then "<=>" else "==>"

   nfProof :: (Expression -> String) -> Expression -> Proof Expression
   nfProof shw = nfPr shw True True -- The first boolean True means that clauses are derived using <=> derivations. The second True means that a disjunctive normal form is produced.
   nfPr :: (Expression -> String) -> Bool -> Bool -> Expression -> [(Expression, [String], String)]
   nfPr shw eq dnf expr
    = {-if showADL expr=="r \\/ s"
      then fatal 360 ("Diagnose expr: "++showADL expr++"\n"++
                      "eq:            "++show eq++"\n"++
                      "dnf:           "++show eq++"\n"++
                      "res:           "++showADL res++"\n"++
                      "expr==res:     "++show (expr==res)
                     ) else-}
      if expr==res
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

   isEUni :: Expression -> Bool
   isEUni EUni{}  = True
   isEUni _       = False

   isEIsc :: Expression -> Bool
   isEIsc EIsc{}  = True
   isEIsc _       = False


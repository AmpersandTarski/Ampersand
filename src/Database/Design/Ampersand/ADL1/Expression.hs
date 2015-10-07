{-# LANGUAGE TypeSynonymInstances , OverlappingInstances #-}
module Database.Design.Ampersand.ADL1.Expression (
                       subst
                      ,foldlMapExpression,foldrMapExpression
                      ,primitives,isMp1, isEEps
                      ,isPos,isNeg, deMorganERad, deMorganECps, deMorganEUni, deMorganEIsc, notCpl, isCpl
                      ,exprIsc2list, exprUni2list, exprCps2list, exprRad2list, exprPrd2list
                      ,insParentheses)
where
import Database.Design.Ampersand.Basics (uni)
import Database.Design.Ampersand.Core.AbstractSyntaxTree
--import Debug.Trace

-- fatal :: Int -> String -> a
-- fatal = fatalMsg "ADL1.Expression"

-- | subst is used to replace each occurrence of a relation
--   with an expression. The parameter expr will therefore be applied to an
--   expression of the form Erel rel.
subst :: (Declaration,Expression) -> Expression -> Expression
subst (decl,expr) = subs
     where
       subs (EEqu (l,r)) = EEqu (subs l,subs r)
       subs (EInc (l,r)) = EInc (subs l,subs r)
       subs (EIsc (l,r)) = EIsc (subs l,subs r)
       subs (EUni (l,r)) = EUni (subs l,subs r)
       subs (EDif (l,r)) = EDif (subs l,subs r)
       subs (ELrs (l,r)) = ELrs (subs l,subs r)
       subs (ERrs (l,r)) = ERrs (subs l,subs r)
       subs (EDia (l,r)) = EDia (subs l,subs r)
       subs (ECps (l,r)) = ECps (subs l,subs r)
       subs (ERad (l,r)) = ERad (subs l,subs r)
       subs (EPrd (l,r)) = EPrd (subs l,subs r)
       subs (EKl0 e    ) = EKl0 (subs e)
       subs (EKl1 e    ) = EKl1 (subs e)
       subs (EFlp e    ) = EFlp (subs e)
       subs (ECpl e    ) = ECpl (subs e)
       subs (EBrk e)     = EBrk (subs e)
       subs e@(EDcD d  ) | d==decl   = expr
                         | otherwise = e
       subs e@EDcI{}     = e
       subs e@EEps{}     = e
       subs e@EDcV{}     = e
       subs e@EMp1{}     = e

foldlMapExpression :: (a -> r -> a) -> (Declaration->r) -> a -> Expression -> a
foldlMapExpression f = foldrMapExpression f' where f' x y = f y x

foldrMapExpression :: (r -> a -> a) -> (Declaration->r) -> a -> Expression -> a
foldrMapExpression f g a (EEqu (l,r)) = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (EInc (l,r)) = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (EIsc (l,r)) = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (EUni (l,r)) = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (EDif (l,r)) = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (ELrs (l,r)) = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (ERrs (l,r)) = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (EDia (l,r)) = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (ECps (l,r)) = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (ERad (l,r)) = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (EPrd (l,r)) = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (EKl0 e)     = foldrMapExpression f g a                         e
foldrMapExpression f g a (EKl1 e)     = foldrMapExpression f g a                         e
foldrMapExpression f g a (EFlp e)     = foldrMapExpression f g a                         e
foldrMapExpression f g a (ECpl e)     = foldrMapExpression f g a                         e
foldrMapExpression f g a (EBrk e)     = foldrMapExpression f g a                         e
foldrMapExpression f g a (EDcD d)     = f (g d) a
foldrMapExpression _ _ a  EDcI{}      = a
foldrMapExpression _ _ a  EEps{}      = a
foldrMapExpression _ _ a  EDcV{}      = a
foldrMapExpression _ _ a  EMp1{}      = a

primitives :: Expression -> [Expression]
primitives expr =
  case expr of
    (EEqu (l,r)) -> primitives l `uni` primitives r
    (EInc (l,r)) -> primitives l `uni` primitives r
    (EIsc (l,r)) -> primitives l `uni` primitives r
    (EUni (l,r)) -> primitives l `uni` primitives r
    (EDif (l,r)) -> primitives l `uni` primitives r
    (ELrs (l,r)) -> primitives l `uni` primitives r
    (ERrs (l,r)) -> primitives l `uni` primitives r
    (EDia (l,r)) -> primitives l `uni` primitives r
    (ECps (l,r)) -> primitives l `uni` primitives r
    (ERad (l,r)) -> primitives l `uni` primitives r
    (EPrd (l,r)) -> primitives l `uni` primitives r
    (EKl0 e)     -> primitives e
    (EKl1 e)     -> primitives e
    (EFlp e)     -> primitives e
    (ECpl e)     -> primitives e
    (EBrk e)     -> primitives e
    EDcD{}       -> [expr]
    EDcI{}       -> [expr]
    EEps{}       -> []  -- Since EEps is inserted for typing reasons only, we do not consider it a primitive..
    EDcV{}       -> [expr]
    EMp1{}       -> [expr]

-- | The rule of De Morgan requires care with respect to the complement.
--   The following function provides a function to manipulate with De Morgan correctly.
deMorganERad :: Expression -> Expression
deMorganERad (ECpl (ERad (l,r)))
  = notCpl (deMorganERad l) .:. notCpl (deMorganERad r)
deMorganERad (ERad (l,r))
  = notCpl (notCpl (deMorganERad l) .:. notCpl (deMorganERad r))
deMorganERad e = e
deMorganECps :: Expression -> Expression
deMorganECps (ECpl (ECps (l,r)))
  = notCpl (deMorganECps l) .!. notCpl (deMorganECps r)
deMorganECps (ECps (l,r))
  = notCpl (notCpl (deMorganECps l) .!. notCpl (deMorganECps r))
deMorganECps e = e
deMorganEUni :: Expression -> Expression
deMorganEUni (ECpl (EUni (l,r)))
  = notCpl (deMorganEUni l) ./\. notCpl (deMorganEUni r)
deMorganEUni (EUni (l,r))
  = notCpl (notCpl (deMorganEUni l) ./\. notCpl (deMorganEUni r))
deMorganEUni e = e
deMorganEIsc :: Expression -> Expression
deMorganEIsc (ECpl (EIsc (l,r)))
  = notCpl (deMorganEIsc l) .\/. notCpl (deMorganEIsc r)
deMorganEIsc (EIsc (l,r))
  = notCpl (notCpl (deMorganEIsc l) .\/. notCpl (deMorganEIsc r))
deMorganEIsc e = e

notCpl :: Expression -> Expression
notCpl (ECpl e) = e
notCpl e = ECpl e

isCpl :: Expression -> Bool
isCpl (ECpl{}) = True
isCpl _ = False

isPos :: Expression -> Bool
isPos  = not . isNeg
isNeg :: Expression -> Bool
isNeg = isCpl

isMp1 :: Expression -> Bool
isMp1 EMp1{} = True
isMp1 _ = False

isEEps :: Expression -> Bool
isEEps EEps{} = True
isEEps _ = False

exprIsc2list, exprUni2list, exprCps2list, exprRad2list, exprPrd2list :: Expression -> [Expression]
exprIsc2list (EIsc (l,r)) = exprIsc2list l++exprIsc2list r
exprIsc2list r            = [r]
exprUni2list (EUni (l,r)) = exprUni2list l++exprUni2list r
exprUni2list r            = [r]
exprCps2list (ECps (l,r)) = exprCps2list l++exprCps2list r
exprCps2list r            = [r]
exprRad2list (ERad (l,r)) = exprRad2list l++exprRad2list r
exprRad2list r            = [r]
exprPrd2list (EPrd (l,r)) = exprPrd2list l++exprPrd2list r
exprPrd2list r            = [r]

insParentheses :: Expression -> Expression
insParentheses expr = insPar 0 expr
   where
     wrap :: Integer -> Integer -> Expression -> Expression
     wrap i j e' = if i<=j then e' else EBrk (insPar 0 e')
     insPar :: Integer -> Expression -> Expression
     insPar i  (EEqu (l,r)) = wrap i 0 (insPar 1 l .==. insPar 1 r)
     insPar i  (EInc (l,r)) = wrap i 0 (insPar 1 l .|-. insPar 1 r)
     insPar i x@EIsc{}      = wrap i 2 (foldr1 (./\.) [insPar 3 e | e<-exprIsc2list x ])
     insPar i x@EUni{}      = wrap i 2 (foldr1 (.\/.) [insPar 3 e | e<-exprUni2list x ])
     insPar i  (EDif (l,r)) = wrap i 4 (insPar 5 l .-. insPar 5 r)
     insPar i  (ELrs (l,r)) = wrap i 6 (insPar 7 l ./. insPar 7 r)
     insPar i  (ERrs (l,r)) = wrap i 6 (insPar 7 l .\. insPar 7 r)
     insPar i  (EDia (l,r)) = wrap i 6 (insPar 7 l .<>. insPar 7 r)
     insPar i x@ECps{}      = wrap i 8 (foldr1 (.:.) [insPar 9 e | e<-exprCps2list x ])
     insPar i x@ERad{}      = wrap i 8 (foldr1 (.!.) [insPar 9 e | e<-exprRad2list x ])
     insPar i x@EPrd{}      = wrap i 8 (foldr1 (.*.) [insPar 9 e | e<-exprPrd2list x ])
     insPar _  (EKl0 e)     = EKl0 (insPar 10 e)
     insPar _  (EKl1 e)     = EKl1 (insPar 10 e)
     insPar _  (EFlp e)     = EFlp (insPar 10 e)
     insPar _  (ECpl e)     = ECpl (insPar 10 e)
     insPar i  (EBrk e)     = insPar i e
     insPar _  x            = x

{-
   insPar 0 (r/\s/\t/\x/\y |- p)
=
   wrap 0 0 (insPar 1 (r/\s/\t/\x/\y) |- insPar 1 p)
=
   insPar 1 (r/\s/\t/\x/\y) |- insPar 1 p
=
   wrap 1 2 (foldr1 f [insPar 3 e | e<-exprIsc2list (r/\s/\t/\x/\y) ]) |- p   where f x y = EIsc (x,y)
=
   foldr1 f [insPar 3 e | e<-exprIsc2list (r/\s/\t/\x/\y) ] |- p   where f x y = EIsc (x,y)
=
   foldr1 f [insPar 3 e | e<-[r,s,t,x,y] ] |- p   where f x y = EIsc (x,y)
=
   foldr1 f [insPar 3 r,insPar 3 s,insPar 3 t,insPar 3 x,insPar 3 y] |- p   where f x y = EIsc (x,y)
=
   foldr1 f [r,s,t,x,y] |- p   where f x y = EIsc (x,y)
=
   r/\s/\t/\x/\y |- p

   insPar 0 (r;s;t;x;y |- p)
=
   wrap 0 0 (insPar 1 (r;s;t;x;y) |- insPar 1 p)
=
   insPar 1 (r;s;t;x;y) |- insPar 1 p
=
   wrap 1 8 (insPar 8 r ; insPar 8 (s;t;x;y)) |- p
=
   r; insPar 8 (s;t;x;y) |- p
=
   r; wrap 8 8 (insPar 8 s; insPar 8 (t;x;y)) |- p
=
   r; insPar 8 s; insPar 8 (t;x;y) |- p
=
   r; s; insPar 8 (t;x;y) |- p
-}


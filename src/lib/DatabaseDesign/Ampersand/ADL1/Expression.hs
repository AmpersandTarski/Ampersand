{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
module DatabaseDesign.Ampersand.ADL1.Expression (
                       isTypeable,subst,subsi
                      ,foldlMapExpression,foldrMapExpression
                      ,isECps,isERad,isEPrd,isEIsc,isEUni -- ,isI
                      ,isPos,isNeg ,notCpl, isCpl)
where
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree

subst :: (Relation,Expression) -> Expression -> Expression
subst (rel,f) = subs
     where
       subs (EEqu (l,r)) = EEqu (subs l,subs r)
       subs (EImp (l,r)) = EImp (subs l,subs r)
       subs (EIsc es)    = EIsc (map subs es)
       subs (EUni es)    = EUni (map subs es)
       subs (EDif (l,r)) = EDif (subs l,subs r)
       subs (ELrs (l,r)) = ELrs (subs l,subs r)
       subs (ERrs (l,r)) = ERrs (subs l,subs r)
       subs (ECps es)    = ECps (map subs es)
       subs (ERad es)    = ERad (map subs es)
       subs (EPrd es)    = EPrd (map subs es)
       subs (EKl0 e)     = EKl0 (subs e)
       subs (EKl1 e)     = EKl1 (subs e)
       subs (EFlp e)     = EFlp (subs e)
       subs (ECpl e)     = ECpl (subs e)
       subs (EBrk e)     = EBrk (subs e)
       subs (ETyp e sgn) = ETyp (subs e) sgn
       subs (ERel r) | rel==r    = f
                     | otherwise = ERel r

-- | This function is used to replace the n-th relation (counting from the left)
--   with an expression. The parameter f will therefore be applied to an
--   expression of the form Erel rel.
subsi :: Int -> (Expression -> Expression) -> Expression -> Expression
subsi n f expr = expr'
       where
         (expr',_) = subs 1 expr
         subs :: Int -> Expression -> (Expression, Int)
         subs i (EEqu (l,r)) = (EEqu (l',r'), i'')
                               where (l',i')  = subs i l
                                     (r',i'') = subs i' r
         subs i (EImp (l,r)) = (EImp (l',r'), i'')
                               where (l',i')  = subs i l
                                     (r',i'') = subs i' r
         subs i (EIsc es)    = (EIsc es', if null es then i else last is)
                               where propagate j (x:xs) = (x',j'): propagate j' xs
                                      where (x',j') = subs j x
                                     propagate _ [] = []
                                     (es',is) = unzip (propagate i es)
         subs i (EUni es)    = (EUni es', if null es then i else last is)
                               where propagate j (x:xs) = (x',j'): propagate j' xs
                                      where (x',j') = subs j x
                                     propagate _ [] = []
                                     (es',is) = unzip (propagate i es)
         subs i (EDif (l,r)) = (EDif (l',r'), i'')
                               where (l',i')  = subs i l
                                     (r',i'') = subs i' r
         subs i (ELrs (l,r)) = (ELrs (l',r'), i'')
                               where (l',i')  = subs i l
                                     (r',i'') = subs i' r
         subs i (ERrs (l,r)) = (ERrs (l',r'), i'')
                               where (l',i')  = subs i l
                                     (r',i'') = subs i' r
         subs i (ECps es)    = (ECps es', if null es then i else last is)
                               where propagate j (x:xs) = (x',j'): propagate j' xs
                                      where (x',j') = subs j x
                                     propagate _ [] = []
                                     (es',is) = unzip (propagate i es)
         subs i (ERad es)    = (ERad es', if null es then i else last is)
                               where propagate j (x:xs) = (x',j'): propagate j' xs
                                      where (x',j') = subs j x
                                     propagate _ [] = []
                                     (es',is) = unzip (propagate i es)
         subs i (EPrd es)    = (EPrd es', if null es then i else last is)
                               where propagate j (x:xs) = (x',j'): propagate j' xs
                                      where (x',j') = subs j x
                                     propagate _ [] = []
                                     (es',is) = unzip (propagate i es)
         subs i (EKl0 x)     = (EKl0 x', i') where (x',i') = subs i x
         subs i (EKl1 x)     = (EKl1 x', i') where (x',i') = subs i x 
         subs i (EFlp x)     = (EFlp x', i') where (x',i') = subs i x 
         subs i (ECpl x)     = (ECpl x', i') where (x',i') = subs i x 
         subs i (EBrk x)     = (EBrk x', i') where (x',i') = subs i x 
         subs i (ETyp x sgn) = (ETyp x' sgn, i') where (x',i') = subs i x
         subs i x@ERel{} | i==n      = (f x, i+1)
                         | otherwise = (x, i+1)

foldlMapExpression :: (a -> r -> a) -> (Relation->r) -> a -> Expression -> a
foldlMapExpression f = foldrMapExpression f' where f' x y = f y x

foldrMapExpression :: (r -> a -> a) -> (Relation->r) -> a -> Expression -> a
foldrMapExpression f g a (EEqu (l,r))  = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (EImp (l,r))  = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression _ _ a (EIsc [])     = a
foldrMapExpression f g a (EIsc (e:es)) = foldrMapExpression f g (foldrMapExpression f g a e) (EIsc es)
foldrMapExpression _ _ a (EUni [])     = a
foldrMapExpression f g a (EUni (e:es)) = foldrMapExpression f g (foldrMapExpression f g a e) (EUni es)
foldrMapExpression f g a (EDif (l,r))  = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (ELrs (l,r))  = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression f g a (ERrs (l,r))  = foldrMapExpression f g (foldrMapExpression f g a l) r
foldrMapExpression _ _ a (ECps [])     = a
foldrMapExpression f g a (ECps (e:es)) = foldrMapExpression f g (foldrMapExpression f g a e) (ECps es)
foldrMapExpression _ _ a (ERad [])     = a
foldrMapExpression f g a (ERad (e:es)) = foldrMapExpression f g (foldrMapExpression f g a e) (ERad es)
foldrMapExpression _ _ a (EPrd [])     = a
foldrMapExpression f g a (EPrd (e:es)) = foldrMapExpression f g (foldrMapExpression f g a e) (EPrd es)
foldrMapExpression f g a (EKl0 e)      = foldrMapExpression f g a                         e
foldrMapExpression f g a (EKl1 e)      = foldrMapExpression f g a                         e
foldrMapExpression f g a (EFlp e)      = foldrMapExpression f g a                         e
foldrMapExpression f g a (ECpl e)      = foldrMapExpression f g a                         e
foldrMapExpression f g a (EBrk e)      = foldrMapExpression f g a                         e
foldrMapExpression f g a (ETyp e _)    = foldrMapExpression f g a e
foldrMapExpression f g a (ERel rel)    = f (g rel) a

isEUni :: Expression -> Bool
isEUni EUni{}  = True
isEUni _       = False

isEIsc :: Expression -> Bool
isEIsc EIsc{}  = True
isEIsc _       = False

isECps :: Expression -> Bool
isECps ECps{}  = True
isECps _       = False

isERad :: Expression -> Bool
isERad ERad{}  = True
isERad _       = False

isEPrd :: Expression -> Bool
isEPrd EPrd{}  = True
isEPrd _       = False


-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Expression                    ***
-- \***********************************************************************


{-
  insPar 0 (EUni [EIsc fs,ERel x]))
= w 0 2 (EUni [insPar 2 (EIsc fs), insPar 2 (ERel x)])
= EUni [w 3 2 (EIsc (map insPar 2 fs)), ERel x]
= EUni [EBrk (EIsc (map insPar 2 fs)), ERel x]

  insPar 0 (EEqu (EImp l r,ERel x))
= wrap 0 0 (EEqu (insPar 1 (EImp l r), insPar 1 (ERel x)))
= EEqu (insPar 1 (EImp l r), insPar 1 (ERel x))
= EEqu (wrap 1 0 (EImp (insPar 1 l, insPar 1 r), ERel x)
= EEqu (EBrk (EImp (insPar 3 l, insPar 3 r), ERel x))

  insPar 0 (EImp (EImp l r,ERel x))
= wrap 0 0 (EImp (insPar 1 (EImp l r), insPar 1 (ERel x)))
= EImp (insPar 1 (EImp l r), insPar 1 (ERel x))
= EImp (wrap 1 0 (EImp (insPar 1 l, insPar 1 r), ERel x))
= EImp (EBrk (EImp (insPar 1 l, insPar 1 r), ERel x))

  insPar 0 (EImp (EEqu l r,ERel x))
= wrap 0 0 (EImp (insPar 1 (EEqu l r), insPar 1 (ERel x)))
= EImp (insPar 1 (EEqu l r), insPar 1 (ERel x))
= EImp (wrap 1 0 (EEqu (insPar 1 l, insPar 1 r), ERel x)
= EImp (EBrk (EEqu (insPar 1 l, insPar 1 r), ERel x)
-}


-- | In the data structure Expression, every subexpression has one signature, which can be computed by the function sign :: Expression -> Sign.
--   However, the data structure must not have empty lists in a subexpression EIsc, EUni, ECps, ERad, or EPrd.
--   For this reason we have the following test function
isTypeable :: Expression -> Bool
isTypeable (EEqu (l,r)) = isTypeable l && isTypeable r
isTypeable (EImp (l,r)) = isTypeable l && isTypeable r
isTypeable (EIsc [])    = False
isTypeable (EIsc es)    = all isTypeable es
isTypeable (EUni [])    = False
isTypeable (EUni es)    = all isTypeable es
isTypeable (EDif (l,r)) = isTypeable l && isTypeable r
isTypeable (ELrs (l,r)) = isTypeable l && isTypeable r
isTypeable (ERrs (l,r)) = isTypeable l && isTypeable r
isTypeable (ECps [])    = False
isTypeable (ECps es)    = all isTypeable es
isTypeable (ERad [])    = False
isTypeable (ERad es)    = all isTypeable es
isTypeable (EPrd [])    = False
isTypeable (EPrd es)    = all isTypeable es
isTypeable (EKl0 e)     = isTypeable e
isTypeable (EKl1 e)     = isTypeable e
isTypeable (EFlp e)     = isTypeable e
isTypeable (ECpl e)     = isTypeable e
isTypeable (EBrk e)     = isTypeable e
isTypeable (ETyp e _)   = isTypeable e
isTypeable (ERel _)     = True



-- The origin of an expression is the origin of one of its relations.
-- Let us just take the first one... 
-- WHY? (SJ) Why does this instance exist? Can we not do without?
-- instance Traced Expression where
--  origin e = origin (head (mors e))

notCpl :: Expression -> Expression
notCpl (ECpl e') = e'
notCpl e' = ECpl e'

isCpl :: Expression -> Bool
isCpl (ECpl _) = True
isCpl _ = False

isPos :: Expression -> Bool
isPos (ECpl _) = False
isPos _ = True
isNeg :: Expression -> Bool
isNeg = not . isPos 


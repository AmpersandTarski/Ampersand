{-# OPTIONS_GHC -Wall -XFlexibleContexts -XUndecidableInstances -XFlexibleInstances -XMultiParamTypeClasses #-}
module DatabaseDesign.Ampersand.ADL.Expression (Expression(..),PExpression(..),UnOp(..),MulOp(..),Expressions,PExpressions
                      ,mapExpression,foldlExpression,foldrExpression
                      ,isF,isFd,isFi,isFu,isI,v
                      ,isPos,isNeg,idsOnly,notCp,insParentheses)
where
import DatabaseDesign.Ampersand.ADL.MorphismAndDeclaration  (Relation(..),Declaration(..),Association(..),Relational(..),Identified(..))
import DatabaseDesign.Ampersand.ADL.FilePos                 (Numbered(..))
import DatabaseDesign.Ampersand.ADL.Concept                 (Concept(..), Conceptual(..), SpecHierarchy(..) ,Sign)
import DatabaseDesign.Ampersand.ADL.Prop                    (Prop(..))
import Collection                  (Collection (..))
import Data.List hiding (union)
import Auxiliaries                 (eqClass, sord')

type Expressions rel = [Expression rel]
data Expression rel
      = Tm rel Int             -- m  ^ simple morphism, possibly conversed     ~
      | Tc (Expression rel)    -- e  ^ bracketed expression                 ( ... )
      | F  (Expressions rel)   -- ts ^ composition                             ;
      | Fdx (Expressions rel)  -- ts ^ relative addition                       !
      | Fix (Expressions rel)  -- fs ^ intersection                            /\
      | Fux (Expressions rel)  -- fs ^ union                                   \/
      | K0x (Expression rel)   -- e  ^ Reflexive and transitive closure        *
      | K1x (Expression rel)   -- e  ^ Transitive closure                      +
      | Cpx (Expression rel)   -- e  ^ Complement                              -
                               
mapExpression :: (a->b) -> Expression a -> Expression b
mapExpression f (Tm m n) = Tm (f m) n
mapExpression f (Tc  e ) = Tc (mapExpression f e )
mapExpression f (F   ts) = F  (map (mapExpression f) ts)
mapExpression f (Fdx ts) = Fdx(map (mapExpression f) ts)
mapExpression f (Fix fs) = Fix(map (mapExpression f) fs)
mapExpression f (Fux fs) = Fux(map (mapExpression f) fs)
mapExpression f (K0x e ) = K0x(mapExpression f e )
mapExpression f (K1x e ) = K1x(mapExpression f e )
mapExpression f (Cpx e ) = Cpx(mapExpression f e )

foldlExpression :: (a -> r -> a) -> a -> Expression r -> a
foldlExpression f a e = foldrExpression f' a e where f' x y = f y x

foldrExpression ::  (r -> a -> a) -> a -> Expression r -> a
foldrExpression f a (Tm m _)     = f m a
foldrExpression f a (Tc  e )     = foldrExpression f a                       e
foldrExpression _ a (F     []  ) = a
foldrExpression f a (F   (t:ts)) = foldrExpression f (foldrExpression f a t) (F ts)
foldrExpression _ a (Fdx   []  ) = a
foldrExpression f a (Fdx (t:ts)) = foldrExpression f (foldrExpression f a t) (Fdx ts)
foldrExpression _ a (Fix   []  ) = a
foldrExpression f a (Fix (t:ts)) = foldrExpression f (foldrExpression f a t) (Fix ts)
foldrExpression _ a (Fux   []  ) = a
foldrExpression f a (Fux (t:ts)) = foldrExpression f (foldrExpression f a t) (Fux ts)
foldrExpression f a (K0x e )     = foldrExpression f a                       e
foldrExpression f a (K1x e )     = foldrExpression f a                       e
foldrExpression f a (Cpx e )     = foldrExpression f a                       e

data UnOp
  = K0 -- ^ Reflexive and transitive closure *
  | K1 -- ^ Transitive closure +
  | Cp -- ^ Complement -
  | Co -- ^ Converse ~
    deriving (Show,Eq)

data MulOp
  = Fc -- ^ composition ;
  | Fd -- ^ relative addition !
  | Fi -- ^ intersection
  | Fu -- ^ union \/
  | Ri -- ^ Rule implication |-  => (r |- s |- t <=> (-r\/s) /\ (-s\/t) )
  | Re -- ^ Rule equivalence =   => (r = s = t   <=> (r |- s |- t) /\ (t |- s |- r)
    deriving (Show,Eq)

data Op = Op1 UnOp | Opn MulOp

--see cc.hs => [Re,Ri,Fu,Fi,Fd,Fc] unop
precedence::Op->Int
precedence (Op1 _) = 0
precedence (Opn Fc) = 1
precedence (Opn Fd) = 2
precedence (Opn Fi) = 3
precedence (Opn Fu) = 4
precedence (Opn Ri) = 5
precedence (Opn Re) = 6

type PExpressions a b = [PExpression a b]
--at parse time: PExpression (Relation Concept) (Maybe Sign)
--after type check: PExpression (Declaration Concept) Sign
data PExpression term tp
  --        Oper. Operands              Type
  = TPExp         term                  tp
  | MulPExp MulOp [PExpression term tp] tp
  | UnPExp  UnOp  (PExpression term tp) tp
    deriving (Eq)

instance Show (PExpression (Relation Concept) (Maybe Sign)) where
 show (TPExp term Nothing)          = name term
 show (TPExp term (Just (a,b)))      = name term++"["++name a++"*"++name b++"]"
 show (MulPExp mop xs Nothing)      = show mop ++ concat [usebrackets (Opn mop) (oper x) <??> show x |x<-xs]
 show (MulPExp mop xs (Just (a,b)))  = show mop ++ concat [True <??> show x |x<-xs]++"["++name a++"*"++name b++"]"
 show (UnPExp uop x Nothing)      = show uop ++ (usebrackets (Op1 uop) (oper x) <??> show x) 
 show (UnPExp uop x (Just (a,b)))  = show uop ++ (True <??> show x)++"["++name a++"*"++name b++"]"

oper :: PExpression term tp -> Maybe Op
oper (TPExp{}) = Nothing
oper (MulPExp mop _ _) = Just (Opn mop)
oper (UnPExp uop _ _) = Just (Op1 uop)

(<??>) :: Bool -> [Char] -> [Char]
(<??>) True x = "("++x++")"
(<??>) False x = x

usebrackets :: Op -> Maybe Op -> Bool
usebrackets _ Nothing = False
usebrackets op1 (Just op2) = precedence op1 <= precedence op2

instance Show (PExpression (Declaration c) Sign) where
 show (TPExp term _)          = name term
 show (MulPExp mop xs _)      = show mop ++ concat [usebrackets (Opn mop) (oper x) <??> show x |x<-xs]
 show (UnPExp uop x _)      = show uop ++ show (usebrackets (Op1 uop) (oper x) <??> show x)


isFu :: Expression r -> Bool
isFu Fux{}  = True
isFu _     = False

isFi :: Expression r -> Bool
isFi Fix{}  = True
isFi _     = False

isF :: Expression r -> Bool
isF  F{}   = True
isF _      = False

isFd :: Expression r -> Bool
isFd Fdx{}  = True
isFd _     = False

isI :: (Conceptual c, Relational r c) => Expression r -> Bool
isI (Tm m _) = isIdent m
isI (Tc  e ) = isI e
isI (F   ts) = and (map isI ts)
isI (Fix fs) = and (map isI fs)
isI (Fux fs) = and (map isI fs)
isI (K0x e ) = isI e
isI (K1x e ) = isI e
isI (Cpx e ) = isI e
isI _ = False


-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Expression                    ***
-- \***********************************************************************
instance (Show r,Identified r,Eq r) => Eq (Expression r) where
 F  ts == F  ts' = ts==ts'
 Fdx ts == Fdx ts' = ts==ts'
 Fux fs == Fux fs' = sord' show fs==sord' show fs'
 Fix fs == Fix fs' = sord' show fs==sord' show fs'
 Cpx e  == Cpx e'  = e==e'
 K0x e  == K0x e'  = e==e'
 K1x e  == K1x e'  = e==e'
 Tm m _  == Tm m' _  = m==m'
 Tc e  == Tc e'  = e==e'
 Tc e  == e'     = e==e'
 e     == Tc e'  = e==e'
 _     == _      = False

instance Show r => Show (Expression r) where
 showsPrec _ expr  = showString (showExpr (" \\/ ", "/\\", "!", ";", "*", "+", "-", "(", ")") expr)
   where
    showExpr (union,inter,rAdd,rMul,clos0,clos1,compl,lpar,rpar) expr' = showchar (insParentheses expr')
      where
   --    wrap i j str = if i<=j then str else lpar++str++rpar
       showchar (Tm mph _) = show mph
       showchar (Fux []) = "-V"
       showchar (Fux fs) = intercalate union [showchar f| f<-fs]
       showchar (Fix []) = "V"
       showchar (Fix fs) = intercalate inter [showchar f| f<-fs]
       showchar (Fdx []) = "-I"
       showchar (Fdx ts) = intercalate rAdd [showchar t| t<-ts]
       showchar (F [])  = "I"
       showchar (F ts)  = intercalate rMul [showchar t| t<-ts]
       showchar (K0x e') = showchar e'++clos0
       showchar (K1x e') = showchar e'++clos1
       showchar (Cpx e') = compl++showchar e'
       showchar (Tc f)  = lpar++showchar f++rpar

insParentheses :: Expression r -> Expression r
insParentheses expr = insPar 0 expr
      where
       wrap :: Integer -> Integer -> Expression r -> Expression r
       wrap i j e' = if i<=j then e' else Tc e'
       insPar :: Integer -> Expression r -> Expression r
       insPar _ (Tm mph i) = Tm mph i
       insPar i (Fux fs)  = wrap i 4 (Fux [insPar 4 f| f<-fs])
       insPar i (Fix fs)  = wrap i 5 (Fix [insPar 5 f| f<-fs])
       insPar i (Fdx ts)  = wrap i 6 (Fdx [insPar 6 t| t<-ts])
       insPar i (F ts)   = wrap i 7 (F  [insPar 7 t| t<-ts])
       insPar _ (K0x e')  = K0x (insPar 8 e')
       insPar _ (K1x e')  = K1x (insPar 8 e')
       insPar _ (Cpx e')  = Cpx (insPar 8 e')
       insPar i (Tc f)   = insPar i f

instance (SpecHierarchy c,Association r c,Show c,Show r) => Association (Expression r) c where
 source (Tm mph _) = source mph
 source (Tc f)     = source f
 source (F  [])    = error ("!Fatal (module Expression 208): source (F [])")
 source (F  ts)    = source (head ts)
 source (Fdx [])   = error ("!Fatal (module Expression 210): source (Fd [])")
 source (Fdx ts)   = source (head ts)
 source (Fux fs)   = if length (eqClass comparable (map source fs))==1 then minimum (map source fs)
                     else error ("(!Fatal (module Expression 213): source ("++show (Fux fs)++")")
 source (Fix fs)   = if length (eqClass comparable (map source fs))==1 then maximum (map source fs)
                     else error ("!Fatal (module Expression 215): source ("++show (Fix fs)++")")
 source (K0x e')   = source e'
 source (K1x e')   = source e'
 source (Cpx e')   = source e'

 target (Tm mph _) = target mph
 target (Tc f)     = target f
 target (F  [])    = error ("!Fatal (module Expression 222): type of target (F []) is Anything")
 target (F  ts)    = target (last ts)
 target (Fdx [])   = error ("!Fatal (module Expression 224): type of target (Fd []) is Anything")
 target (Fdx ts)   = target (last ts)
 target (Fux fs)   = if length (eqClass comparable (map target fs))==1 then minimum (map target fs)
                     else error ("!Fatal (module Expression 227): type of target (Fux "++show fs++")) is Anything")
 target (Fix fs)   = if length (eqClass comparable (map target fs))==1 then maximum (map target fs)
                     else error ("!Fatal (module Expression 229): type of target (Fix "++show fs++")) is Anything")
 target (K0x e')   = target e'
 target (K1x e')   = target e'
 target (Cpx e')   = target e'

 sign (Tm mph _)   = sign mph
 sign (Tc f)       = sign f
 sign (F ts)       = if null ts 
                     then error ("!Fatal (module Expression 237): no terms in sign (F "++show ts++")")
                     else foldr1 jnSign (map sign ts)
                     where (s , _ ) `jnSign` ( _ ,t') = (s,t')
 sign (Fdx ts)     = if null ts 
                     then error ("!Fatal (module Expression 241): no terms in sign (Fd "++show ts++")")
                     else foldr1 jnSign (map sign ts)
                     where (s , _ ) `jnSign` ( _ ,t') = (s,t')
 sign (Fux fs)     = if length (eqClass comparable (map sign fs))>1 then error ("!Fatal (module Expression 244): sign (Fu fs) not defined\nwith map sign fs="++show (map sign fs)) else
                     if null fs then error ("!Fatal (module Expression 204): type of sign (Fux "++show fs++") is (Anything, Anything).")
                     else foldr1 lub (map sign fs)
 sign (Fix fs)     = if length (eqClass comparable (map sign fs))>1 then error ("!Fatal (module Expression 247): sign (Fi fs) not defined\nwith map sign fs="++show (map sign fs)) else
                     if null fs then error ("!Fatal (module Expression 248): type of sign (Fix "++show fs++") is (Anything, Anything).")
                     else foldr1 lub (map sign fs)
 sign (K0x e')     = sign e'
 sign (K1x e')     = sign e'
 sign (Cpx e')     = sign e'

instance (Numbered r) => Numbered (Expression r) where
 pos (Tm mph _)  = pos mph
 pos (Tc f)  = pos f
 pos (F ts)  = if not (null ts) then pos (head ts) else error "!Fatal (module Expression 257): Please submit a complete bug report to your dealer"
 pos (Fdx ts) = if not (null ts) then pos (head ts) else error "!Fatal (module Expression 258): Please submit a complete bug report to your dealer"
 pos (Fux fs) = if not (null fs) then pos (head fs) else error "!Fatal (module Expression 259): Please submit a complete bug report to your dealer"
 pos (Fix fs) = if not (null fs) then pos (head fs) else error "!Fatal (module Expression 260): Please submit a complete bug report to your dealer"
 pos (K0x e')  = pos e'
 pos (K1x e')  = pos e'
 pos (Cpx e')  = pos e'

v :: Sign -> Expression (Relation Concept)
v (a,b) = Tm (V [] (a,b)) (-1)

notCp :: Expression r -> Expression r
notCp (Cpx e') = e'
notCp e' = Cpx e'

isPos :: Expression r -> Bool
isPos (Cpx _) = False
isPos _ = True
isNeg :: Expression r -> Bool
isNeg e' = not (isPos e')

idsOnly :: (Eq r, Conceptual c, Relational r c) => Expression r -> Bool
idsOnly e' = and [isIdent m'| m'<-mors e'] -- > tells whether all the arguments are equivalent to I
             where mors e = foldrExpression rdcons [] e   -- ^ yields a list of relations from e
                   rdcons m ms = if m `elem` ms then ms else m:ms
             

instance (SpecHierarchy c, Show c, Show r, Association r c, Relational r c) => Relational (Expression r) c where
 multiplicities expr = case expr of
     (Tm mph _)-> multiplicities mph
     (Tc f)    -> multiplicities f
     (F ts)    -> foldr isc [Uni,Tot,Sur,Inj] (map multiplicities ts) -- homogene multiplicities can be used and deduced by and from rules: many rules are multiplicities (TODO)
     (Fdx _)   -> [] -- many rules with Fd in it are multiplicities (TODO). Solve perhaps by defining relation a = (Fd ts)
     (Fux _)   -> [] -- expr \/ a is Rfx if a is Rfx, is Tot if a is Tot (TODO), is Uni if both a and expr are uni
     (Fix _)   -> [] -- expr /\ a is Asy if a is Asy, is Uni if a is Uni (TODO), is Tot if both a and expr are tot
     (K0x e')  -> [Rfx,Trn] `uni` (multiplicities e'>-[Uni,Inj])
     (K1x e')  -> [Trn] `uni` (multiplicities e'>-[Uni,Inj])
     (Cpx e')  -> [p|p<-multiplicities e', p==Sym]

 flp expr = case expr of
     (Tm mph i)-> Tm (flp mph) i
     (Tc f)    -> Tc (flp f)
     (F ts)    -> F (map flp (reverse ts))
     (Fdx ts)  -> Fdx (map flp (reverse ts))
     (Fux fs)  -> Fux (map flp fs)
     (Fix fs)  -> Fix (map flp fs)
     (K0x e')  -> K0x (flp e')
     (K1x e')  -> K1x (flp e')
     (Cpx e')  -> Cpx (flp e')

 isNot expr = case expr of        -- > tells whether the argument is equivalent to I-
     (Tm mph _)  -> isNot mph    
     (Tc f)      -> isNot f
     (F [])      -> False
     (F [t])     -> isNot t        
     (F (_:_))   -> False       -- WHY?? Stef, is dit goed? Het ziet er zo raar uit... (Vergelijk ook deze versie met de vorige.)
     (Fdx [])    -> False
     (Fdx [t])   -> isNot t
     (Fdx (_:_)) -> False
     (Fux [])    -> False
     (Fux [t])   -> isNot t
     (Fux (_:_)) -> False
     (Fix [])    -> False
     (Fix [t])   -> isNot t
     (Fix (_:_)) -> False
     (K0x _)     -> False
     (K1x _)     -> False
     (Cpx _)     -> False

 isTrue expr = case expr of
     (F [])    -> False -- > fout voor singletons (TODO)
     (F ts) | isFunction    (head ts) -> (isTrue. F .tail) ts
            | isFlpFunction (last ts) -> (isTrue. F .init) ts
            | otherwise               -> isTrue (head ts) && isTrue (last ts) &&
                                                (not.isFalse. F .drop 1.init) ts  -- niet isFalse tussen head en last
     (Fdx as)   -> isFalse (F (map notCp as))
     (Fux fs)   -> or  [isTrue f| f<-fs] -- isImin \/ isIdent => isTrue (onder andere => TODO)
     (Fix fs)   -> and [isTrue f| f<-fs]
     (K0x e')   -> isTrue (K1x e')
     (K1x e')   -> isTrue e' -- als elk elem van (source e) in een cykel (in e) zit, dan ook is K0 ook True (TODO)
     (Cpx e')   -> isFalse e'
     (Tm mph _) -> isTrue mph
     (Tc f)     -> isTrue f

 isFalse expr = case expr of
     (F [])     -> False -- > helemaal correct
     (F ts)     -> or (map isFalse ts) -- ook True als twee concepten op de ; niet aansluiten: a[A*B];b[C*D] met B/\C=0 (Dit wordt echter door de typechecker uitgesloten)
     (Fdx as)   -> isTrue (F (map notCp as))
     (Fux fs)   -> and [isFalse f| f<-fs]
     (Fix fs)   -> or  [isFalse f| f<-fs] -- isImin /\ isIdent => isFalse (onder andere => TODO)
     (K0x _)    -> False
     (K1x e')   -> isFalse e'
     (Cpx e')   -> isTrue e'
     (Tm mph _) -> isFalse mph
     (Tc f)     -> isFalse f

 isProp expr = case expr of
     (F ts)     -> null ([Asy,Sym]>-multiplicities (F ts)) || and [null ([Asy,Sym]>-multiplicities t)| t<-ts]
     (Fdx [e']) -> isProp e'
     (Fdx as)   -> isProp (F (map notCp as))
     (Fux fs)   -> and [isProp f| f<-fs]
     (Fix fs)   -> or [isProp f| f<-fs]
     (K0x e')   -> isProp e'
     (K1x e')   -> isProp e'
     (Cpx e')   -> isTrue e'
     (Tm mph _) -> isProp mph
     (Tc f)     -> isProp f

 isIdent expr = case expr of
     (F ts)     -> and [isIdent t| t<-ts]   -- > a;a~ = I bij bepaalde multipliciteiten (TODO)
     (Fdx [e']) -> isIdent e'
     (Fdx [])   -> False
     (Fdx as)   -> isImin (F (map Cpx as))
     (Fux fs)   -> and [isIdent f| f<-fs] && not (null fs)   -- > fout voor singletons (TODO)
     (Fix fs)   -> and [isIdent f| f<-fs] && not (null fs)   -- > fout voor singletons (TODO)
     (K0x e')   -> isIdent e' || isFalse e'
     (K1x e')   -> isIdent e'
     (Cpx e')   -> isImin e'
     (Tm mph _) -> isIdent mph
     (Tc f)     -> isIdent f
   where
    isImin expr' = case expr' of
       Fdx ts  -> and [isImin t| t<-ts]
       Fix fs  -> and [isImin f| f<-fs] && not (null fs)
       Fux fs  -> and [isImin f| f<-fs] && not (null fs)
       F  [e'] -> isImin e'
       Cpx v'  -> isIdent v'
       _       -> False 



                      

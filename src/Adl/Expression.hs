{-# OPTIONS_GHC -Wall #-}
module Adl.Expression (Expression(..),Expressionx(..),PExpression(..),UnOp(..),MulOp(..),Expressions,isF,isFd,isFi,isFu
                      ,v
                      ,isPos,isNeg,notCp,insParentheses, uniquemphs)
where
import Adl.MorphismAndDeclaration  (Morphism(..),Declaration(..),inline)
import Adl.FilePos                 (Numbered(..))
import Adl.Concept                 (Concept(..),Association(..)
                                   ,Sign
                                   ,MorphicId(..),Morphic(..))
import Adl.Prop                    (Prop(..))
import Collection                  (Collection (..))
import Strings                     (chain)
import CommonClasses               (Identified(..), ABoolAlg(..))
import Auxiliaries                 (eqClass, sord')

type Expressions = [Expression]
data Expression  = Tm Morphism Int -- m  ^ simple morphism, possibly conversed     ~
                 | Tc Expression   -- e  ^ bracketed expression                 ( ... )
                 | F  Expressions  -- ts ^ composition                             ;
                 | Fdx Expressions  -- ts ^ relative addition                       !
                 | Fix Expressions  -- fs ^ intersection                            /\
                 | Fux Expressions  -- fs ^ union                                   \/
                 | K0x Expression   -- e  ^ Reflexive and transitive closure        *
                 | K1x Expression   -- e  ^ Transitive closure                      +
                 | Cpx Expression   -- e  ^ Complement                              -
                              --               deriving (Show)

data UnOp
  = K0 -- ^ Reflexive and transitive closure *
  | K1 -- ^ Transitive closure +
  | Cp -- ^ Complement -
  | Co -- ^ Converse ~
    deriving (Show)

data MulOp
  = Fc -- ^ composition ;
  | Fd -- ^ relative addition !
  | Fi -- ^ intersection
  | Fu -- ^ union \/
  | Ri -- ^ Rule implication |-  => (r |- s |- t <=> (-r\/s) /\ (-s\/t) )
  | Re -- ^ Rule equivalence =   => (r = s = t   <=> (r |- s |- t) /\ (t |- s |- r)
    deriving (Show)

data PExpression
  --        Oper. Operands      Explicit type / Type cast
  = TPExp         Morphism      (Maybe Sign)
  | MulPExp MulOp [PExpression] (Maybe Sign)
  | UnPExp  UnOp  PExpression   (Maybe Sign)
    deriving (Show)

data Expressionx
  --       Oper. Relations     Type
  = TExp         Declaration   Sign
  | MulExp MulOp [Expressionx] Sign
  | UnExp  UnOp  Expressionx   Sign
    deriving (Show)

isFu :: Expression -> Bool
isFu Fux{}  = True
isFu _     = False

isFi :: Expression -> Bool
isFi Fix{}  = True
isFi _     = False

isF :: Expression -> Bool
isF  F{}   = True
isF _      = False

isFd :: Expression -> Bool
isFd Fdx{}  = True
isFd _     = False

--DESCR -> if you need an identifier for morphisms within the scope of an expression 
uniquemphs :: Int -> Expression -> (Expression,Int)
uniquemphs i (Tm mp _) = (Tm mp (i+1),i+1)
uniquemphs i (F []) = (F [],i)
uniquemphs i (F (ex:rexs)) = (F (lft:rghts),ri)
   where
   (lft,li) = uniquemphs i ex
   (F rghts,ri) = (uniquemphs li (F rexs))
uniquemphs i (Fdx []) = (Fdx [],i)
uniquemphs i (Fdx (ex:rexs)) = (Fdx (lft:rghts),ri)
   where
   (lft,li) = uniquemphs i ex
   (Fdx rghts,ri) = (uniquemphs li (Fdx rexs))
uniquemphs i (Fix []) = (Fix [],i)
uniquemphs i (Fix (ex:rexs)) = (Fix (lft:rghts),ri)
   where
   (lft,li) = uniquemphs i ex
   (Fix rghts,ri) = (uniquemphs li (Fix rexs))
uniquemphs i (Fux []) = (Fux [],i)
uniquemphs i (Fux (ex:rexs)) = (Fux (lft:rghts),ri)
   where
   (lft,li) = uniquemphs i ex
   (Fux rghts,ri) = (uniquemphs li (Fux rexs))
uniquemphs i (Cpx ex) = (Cpx sb, si)
   where (sb,si) = uniquemphs i ex
uniquemphs i (Tc ex) = (Tc sb, si)
   where (sb,si) = uniquemphs i ex
uniquemphs i (K0x ex) = (K0x sb, si)
   where (sb,si) = uniquemphs i ex
uniquemphs i (K1x ex) = (K1x sb, si)
   where (sb,si) = uniquemphs i ex

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Expression                    ***
-- \***********************************************************************
instance Eq Expression where
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

-- {-
instance Show Expression where
 showsPrec _ expr  = showString (showExpr (" \\/ ", "/\\", "!", ";", "*", "+", "-", "(", ")") expr)
   where
    showExpr (union,inter,rAdd,rMul,clos0,clos1,compl,lpar,rpar) expr' = showchar (insParentheses expr')
      where
   --    wrap i j str = if i<=j then str else lpar++str++rpar
       showchar (Tm mph _)  = name mph++if inline mph then "" else "~"
       showchar (Fux []) = "-V"
       showchar (Fux fs) = chain union [showchar f| f<-fs]
       showchar (Fix []) = "V"
       showchar (Fix fs) = chain inter [showchar f| f<-fs]
       showchar (Fdx []) = "-I"
       showchar (Fdx ts) = chain rAdd [showchar t| t<-ts]
       showchar (F [])  = "I"
       showchar (F ts)  = chain rMul [showchar t| t<-ts]
       showchar (K0x e') = showchar e'++clos0
       showchar (K1x e') = showchar e'++clos1
       showchar (Cpx e') = compl++showchar e'
       showchar (Tc f)  = lpar++showchar f++rpar
-- -}
insParentheses :: Expression -> Expression
insParentheses expr = insPar 0 expr
      where
       wrap :: Integer -> Integer -> Expression -> Expression
       wrap i j e' = if i<=j then e' else Tc e'
       insPar :: Integer -> Expression -> Expression
       insPar _ (Tm mph i) = Tm mph i
       insPar i (Fux fs)  = wrap i 4 (Fux [insPar 4 f| f<-fs])
       insPar i (Fix fs)  = wrap i 5 (Fix [insPar 5 f| f<-fs])
       insPar i (Fdx ts)  = wrap i 6 (Fdx [insPar 6 t| t<-ts])
       insPar i (F ts)   = wrap i 7 (F  [insPar 7 t| t<-ts])
       insPar _ (K0x e')  = K0x (insPar 8 e')
       insPar _ (K1x e')  = K1x (insPar 8 e')
       insPar _ (Cpx e')  = Cpx (insPar 8 e')
       insPar i (Tc f)   = insPar i f

instance Association Expression where

 source (Tm mph _)        = source mph
 source (Tc f)          = source f
 source (F  [])         = Anything -- error ("!Fatal (module Expression 103): source (F [])")
 source (F  ts)         = source (head ts)
 source (Fdx [])         = Anything -- error ("!Fatal (module Expression 05): source (Fd [])")
 source (Fdx ts)         = source (head ts)
 source (Fux fs)         = if length (eqClass order (map source fs))==1 then minimum (map source fs)
                          else Anything -- error ("(!Fatal (module Expression 108): source ("++showHS "" (Fu fs)++")")
 source (Fix fs)         = if length (eqClass order (map source fs))==1 then maximum (map source fs)
                          else Anything -- error ("!Fatal (module Expression 110): source ("++showHS "" (Fi fs)++")")
 source (K0x e')         = source e'
 source (K1x e')         = source e'
 source (Cpx e')         = source e'

 target (Tm mph _)        = target mph
 target (Tc f)          = target f
 target (F  [])         = Anything -- error ("!Fatal (module Expression 117): target (F [])")
 target (F  ts)         = target (last ts)
 target (Fdx [])         = Anything -- error ("!Fatal (module Expression 119): target (Fd [])")
 target (Fdx ts)         = target (last ts)
 target (Fux fs)         = if length (eqClass order (map target fs))==1 then minimum (map target fs)
                          else Anything
 target (Fix fs)         = if length (eqClass order (map target fs))==1 then maximum (map target fs)
                          else Anything
 target (K0x e')         = target e'
 target (K1x e')         = target e'
 target (Cpx e')         = target e'

 sign (Tm mph _)          = sign mph
 sign (Tc f)            = sign f
 sign (F ts)            = if null ts 
                           then error ("!Fatal (module Expression 132): no terms in sign (F "++show ts++")")
                           else foldr1 jnSign (map sign ts)
                           where (s , _ ) `jnSign` ( _ ,t') = (s,t')
 sign (Fdx ts)           = if null ts 
                           then error ("!Fatal (module Expression 136): no terms in sign (Fd "++show ts++")")
                           else foldr1 jnSign (map sign ts)
                           where (s , _ ) `jnSign` ( _ ,t') = (s,t')
 sign (Fux fs)           = if length (eqClass order (map sign fs))>1 then error ("!Fatal (module Expression 139): sign (Fu fs) not defined\nwith map sign fs="++show (map sign fs)) else
                          if null fs then (Anything, Anything) else
                          foldr1 lub (map sign fs)
 sign (Fix fs)           = if length (eqClass order (map sign fs))>1 then error ("!Fatal (module Expression 142): sign (Fi fs) not defined\nwith map sign fs="++show (map sign fs)) else
                          if null fs then (Anything, Anything) else
                          foldr1 lub (map sign fs)
 sign (K0x e')           = sign e'
 sign (K1x e')           = sign e'
 sign (Cpx e')           = sign e'

instance Numbered Expression where
 pos (Tm mph _)  = pos mph
 pos (Tc f)  = pos f
 pos (F ts)  = if not (null ts) then pos (head ts) else error "!Fatal (module Expression 152): Please submit a complete bug report to your dealer"
 pos (Fdx ts) = if not (null ts) then pos (head ts) else error "!Fatal (module Expression 153): Please submit a complete bug report to your dealer"
 pos (Fux fs) = if not (null fs) then pos (head fs) else error "!Fatal (module Expression 154): Please submit a complete bug report to your dealer"
 pos (Fix fs) = if not (null fs) then pos (head fs) else error "!Fatal (module Expression 155): Please submit a complete bug report to your dealer"
 pos (K0x e')  = pos e'
 pos (K1x e')  = pos e'
 pos (Cpx e')  = pos e'

instance MorphicId Expression where
 isIdent expr = case expr of
     (F ts)    -> and [isIdent t| t<-ts]   -- > a;a~ = I bij bepaalde multipliciteiten (TODO)
     (Fdx [e']) -> isIdent e'
     (Fdx as)   -> isImin (F (map Cpx as))
     (Fux fs)   -> and [isIdent f| f<-fs] && not (null fs)   -- > fout voor singletons (TODO)
     (Fix fs)   -> and [isIdent f| f<-fs] && not (null fs)   -- > fout voor singletons (TODO)
     (K0x e')   -> isIdent e' || isFalse e'
     (K1x e')   -> isIdent e'
     (Cpx e')   -> isImin e'
     (Tm mph _)  -> isIdent mph
     (Tc f)    -> isIdent f
   where
    isImin :: Expression -> Bool
    isImin expr' = case expr' of
       (Fdx ts)  -> and [isImin t| t<-ts]
       (Fix fs)  -> and [isImin f| f<-fs] && not (null fs)
       (Fux fs)  -> and [isImin f| f<-fs] && not (null fs)
       (F  [e']) -> isImin e'
       (Cpx v')   -> isIdent v'
    --   _        = False -- (TODO)
       (Tm _ _)  -> False
       (Tc _)  -> False
       (K0x _)  -> False
       (K1x _)  -> False
       (F [])  -> False
       (F (_:_))-> False   -- WAAROM?? Stef, is dit goed? Het ziet er zo raar uit... (Vergelijk ook deze versie met de vorige.)

 

v :: Sign -> Expression
v (a,b) = Tm (V [] (a,b)) (-1)

notCp :: Expression -> Expression
notCp (Cpx e') = e'
notCp e' = Cpx e'

isPos :: Expression -> Bool
isPos (Cpx _) = False
isPos _ = True
isNeg :: Expression -> Bool
isNeg e' = not (isPos e')

instance Morphic Expression where
 multiplicities expr = case expr of
     (Tm mph _)-> multiplicities mph
     (Tc f)  -> multiplicities f
     (F ts)  -> foldr isc [Uni,Tot,Sur,Inj] (map multiplicities ts) -- homogene multiplicities can be used and deduced by and from rules: many rules are multiplicities (TODO)
     (Fdx _)  -> [] -- many rules with Fd in it are multiplicities (TODO). Solve perhaps by defining relation a = (Fd ts)
     (Fux _)  -> [] -- expr \/ a is Rfx if a is Rfx, is Tot if a is Tot (TODO), is Uni if both a and expr are uni
     (Fix _)  -> [] -- expr /\ a is Asy if a is Asy, is Uni if a is Uni (TODO), is Uni if both a and expr are uni
     (K0x e') -> [Rfx,Trn] `uni` (multiplicities e'>-[Uni,Inj])
     (K1x e') -> [Trn] `uni` (multiplicities e'>-[Uni,Inj])
     (Cpx e') -> [p|p<-multiplicities e', p==Sym]

 flp expr = case expr of
     (Tm mph i)-> Tm (flp mph) i
     (Tc f)  -> Tc (flp f)
     (F ts)  -> F (map flp (reverse ts))
     (Fdx ts) -> Fdx (map flp (reverse ts))
     (Fux fs) -> Fux (map flp fs)
     (Fix fs) -> Fix (map flp fs)
     (K0x e') -> K0x (flp e')
     (K1x e') -> K1x (flp e')
     (Cpx e') -> Cpx (flp e')

 isNot expr = case expr of        -- > tells whether the argument is equivalent to I-
     (Tm mph _)   -> isNot mph    
     (Tc f)     -> isNot f
     (F [])     -> False
     (F [t])    -> isNot t        
     (F (_:_))  -> False       -- WAAROM?? Stef, is dit goed? Het ziet er zo raar uit... (Vergelijk ook deze versie met de vorige.)
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
     
 typeUniq expr = case expr of
     (Tm mph _) -> typeUniq mph -- I don't understand what typeUniq does - Bas. (TODO) WAAROM? (uitleggen bij de class definition, niet hier)
     (Tc f)   -> typeUniq f
     (F  ts)  -> and (map typeUniq ts)
     (Fdx ts)  -> and (map typeUniq ts)
     (Fux fs)  -> and (map typeUniq fs)
     (Fix fs)  -> and (map typeUniq fs)
     (K0x e')  -> typeUniq e'
     (K1x e')  -> typeUniq e'
     (Cpx e')  -> typeUniq e'

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
     (Tm mph _)  -> isTrue mph
     (Tc f)    -> isTrue f

 isFalse expr = case expr of
     (F [])    -> False -- > helemaal correct
     (F ts)    -> or (map isFalse ts) -- ook True als twee concepten op de ; niet aansluiten: a[A*B];b[C*D] met B/\C=0 (Dit wordt echter door de typechecker uitgesloten)
     (Fdx as)   -> isTrue (F (map notCp as))
     (Fux fs)   -> and [isFalse f| f<-fs]
     (Fix fs)   -> or  [isFalse f| f<-fs] -- isImin /\ isIdent => isFalse (onder andere => TODO)
     (K0x _)    -> False
     (K1x e')   -> isFalse e'
     (Cpx e')   -> isTrue e'
     (Tm mph _)  -> isFalse mph
     (Tc f)    -> isFalse f

 isSignal _ = False

 isProp expr = case expr of
     (F ts)    -> null ([Asy,Sym]>-multiplicities (F ts)) || and [null ([Asy,Sym]>-multiplicities t)| t<-ts]
     (Fdx [e']) -> isProp e'
     (Fdx as)   -> isProp (F (map notCp as))
     (Fux fs)   -> and [isProp f| f<-fs]
     (Fix fs)   -> or [isProp f| f<-fs]
     (K0x e')   -> isProp e'
     (K1x e')   -> isProp e'
     (Cpx e')   -> isTrue e'
     (Tm mph _)  -> isProp mph
     (Tc f)    -> isProp f



                      

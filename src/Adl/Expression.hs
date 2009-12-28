{-# OPTIONS_GHC -Wall #-}
module Adl.Expression (Expression(..),Expressions,isF,isFd,isFi,isFu
                      ,v
                      ,isPos,isNeg,notCp,insParentheses)
where
   import Adl.MorphismAndDeclaration  (Morphism(..),inline)
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
   data Expression  = Tm Morphism     -- m  ^ simple morphism, possibly conversed     ~
                    | Tc Expression   -- e  ^ bracketed expression                 ( ... )
                    | F  Expressions  -- ts ^ composition                             ;
                    | Fd Expressions  -- ts ^ relative addition                       !
                    | Fi Expressions  -- fs ^ intersection                            /\
                    | Fu Expressions  -- fs ^ union                                   \/
                    | K0 Expression   -- e  ^ Reflexive and transitive closure        *
                    | K1 Expression   -- e  ^ Transitive closure                      +
                    | Cp Expression   -- e  ^ Complement                              -
                                 --               deriving (Show)

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
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Expression                    ***
-- \***********************************************************************
   instance Eq Expression where
    F  ts == F  ts' = ts==ts'
    Fd ts == Fd ts' = ts==ts'
    Fu fs == Fu fs' = sord' show fs==sord' show fs'
    Fi fs == Fi fs' = sord' show fs==sord' show fs'
    Cp e  == Cp e'  = e==e'
    K0 e  == K0 e'  = e==e'
    K1 e  == K1 e'  = e==e'
    Tm m  == Tm m'  = m==m'
    Tc e  == Tc e'  = e==e'
    Tc e  == e'     = e==e'
    e     == Tc e'  = e==e'
    _     == _      = False

-- {-
   instance Show Expression where
    showsPrec _ expr  = showString (showExpr ("\\/", "/\\", "!", ";", "*", "+", "-", "(", ")") expr)
      where
       showExpr (union,inter,rAdd,rMul,clos0,clos1,compl,lpar,rpar) expr' = showchar (insParentheses expr')
         where
      --    wrap i j str = if i<=j then str else lpar++str++rpar
          showchar (Tm mph)  = name mph++if inline mph then "" else "~"
          showchar (Fu []) = "-V"
          showchar (Fu fs) = chain union [showchar f| f<-fs]
          showchar (Fi []) = "V"
          showchar (Fi fs) = chain inter [showchar f| f<-fs]
          showchar (Fd []) = "-I"
          showchar (Fd ts) = chain rAdd [showchar t| t<-ts]
          showchar (F [])  = "I"
          showchar (F ts)  = chain rMul [showchar t| t<-ts]
          showchar (K0 e') = showchar e'++clos0
          showchar (K1 e') = showchar e'++clos1
          showchar (Cp e') = compl++showchar e'
          showchar (Tc f)  = lpar++showchar f++rpar
-- -}
   insParentheses :: Expression -> Expression
   insParentheses expr = insPar 0 expr
         where
          wrap :: Integer -> Integer -> Expression -> Expression
          wrap i j e' = if i<=j then e' else Tc e'
          insPar :: Integer -> Expression -> Expression
          insPar _ (Tm mph) = Tm mph
          insPar i (Fu fs)  = wrap i 4 (Fu [insPar 4 f| f<-fs])
          insPar i (Fi fs)  = wrap i 5 (Fi [insPar 5 f| f<-fs])
          insPar i (Fd ts)  = wrap i 6 (Fd [insPar 6 t| t<-ts])
          insPar i (F ts)   = wrap i 7 (F  [insPar 7 t| t<-ts])
          insPar _ (K0 e')  = K0 (insPar 8 e')
          insPar _ (K1 e')  = K1 (insPar 8 e')
          insPar _ (Cp e')  = Cp (insPar 8 e')
          insPar i (Tc f)   = insPar i f

   instance Association Expression where

    source (Tm mph)        = source mph
    source (Tc f)          = source f
    source (F  [])         = Anything -- error ("!Fatal (module Expression 103): source (F [])")
    source (F  ts)         = source (head ts)
    source (Fd [])         = Anything -- error ("!Fatal (module Expression 05): source (Fd [])")
    source (Fd ts)         = source (head ts)
    source (Fu fs)         = if length (eqClass order (map source fs))==1 then minimum (map source fs)
                             else Anything -- error ("(!Fatal (module Expression 108): source ("++showHS "" (Fu fs)++")")
    source (Fi fs)         = if length (eqClass order (map source fs))==1 then maximum (map source fs)
                             else Anything -- error ("!Fatal (module Expression 110): source ("++showHS "" (Fi fs)++")")
    source (K0 e')         = source e'
    source (K1 e')         = source e'
    source (Cp e')         = source e'

    target (Tm mph)        = target mph
    target (Tc f)          = target f
    target (F  [])         = Anything -- error ("!Fatal (module Expression 117): target (F [])")
    target (F  ts)         = target (last ts)
    target (Fd [])         = Anything -- error ("!Fatal (module Expression 119): target (Fd [])")
    target (Fd ts)         = target (last ts)
    target (Fu fs)         = if length (eqClass order (map target fs))==1 then minimum (map target fs)
                             else Anything
    target (Fi fs)         = if length (eqClass order (map target fs))==1 then maximum (map target fs)
                             else Anything
    target (K0 e')         = target e'
    target (K1 e')         = target e'
    target (Cp e')         = target e'

    sign (Tm mph)          = sign mph
    sign (Tc f)            = sign f
    sign (F ts)            = if null ts 
                              then error ("!Fatal (module Expression 132): no terms in sign (F "++show ts++")")
                              else foldr1 jnSign (map sign ts)
                              where (s , _ ) `jnSign` ( _ ,t') = (s,t')
    sign (Fd ts)           = if null ts 
                              then error ("!Fatal (module Expression 136): no terms in sign (Fd "++show ts++")")
                              else foldr1 jnSign (map sign ts)
                              where (s , _ ) `jnSign` ( _ ,t') = (s,t')
    sign (Fu fs)           = if length (eqClass order (map sign fs))>1 then error ("!Fatal (module Expression 139): sign (Fu fs) not defined\nwith map sign fs="++show (map sign fs)) else
                             if null fs then (Anything, Anything) else
                             foldr1 lub (map sign fs)
    sign (Fi fs)           = if length (eqClass order (map sign fs))>1 then error ("!Fatal (module Expression 142): sign (Fi fs) not defined\nwith map sign fs="++show (map sign fs)) else
                             if null fs then (Anything, Anything) else
                             foldr1 lub (map sign fs)
    sign (K0 e')           = sign e'
    sign (K1 e')           = sign e'
    sign (Cp e')           = sign e'

   instance Numbered Expression where
    pos (Tm mph)  = pos mph
    pos (Tc f)  = pos f
    pos (F ts)  = if not (null ts) then pos (head ts) else error "!Fatal (module Expression 152): Please submit a complete bug report to your dealer"
    pos (Fd ts) = if not (null ts) then pos (head ts) else error "!Fatal (module Expression 153): Please submit a complete bug report to your dealer"
    pos (Fu fs) = if not (null fs) then pos (head fs) else error "!Fatal (module Expression 154): Please submit a complete bug report to your dealer"
    pos (Fi fs) = if not (null fs) then pos (head fs) else error "!Fatal (module Expression 155): Please submit a complete bug report to your dealer"
    pos (K0 e')  = pos e'
    pos (K1 e')  = pos e'
    pos (Cp e')  = pos e'

   instance MorphicId Expression where
    isIdent expr = case expr of
        (F ts)    -> and [isIdent t| t<-ts]   -- > a;a~ = I bij bepaalde multipliciteiten (TODO)
        (Fd [e']) -> isIdent e'
        (Fd as)   -> isImin (F (map Cp as))
        (Fu fs)   -> and [isIdent f| f<-fs] && not (null fs)   -- > fout voor singletons (TODO)
        (Fi fs)   -> and [isIdent f| f<-fs] && not (null fs)   -- > fout voor singletons (TODO)
        (K0 e')   -> isIdent e' || isFalse e'
        (K1 e')   -> isIdent e'
        (Cp e')   -> isImin e'
        (Tm mph)  -> isIdent mph
        (Tc f)    -> isIdent f
      where
       isImin :: Expression -> Bool
       isImin expr' = case expr' of
          (Fd ts)  -> and [isImin t| t<-ts]
          (Fi fs)  -> and [isImin f| f<-fs] && not (null fs)
          (Fu fs)  -> and [isImin f| f<-fs] && not (null fs)
          (F  [e']) -> isImin e'
          (Cp v')   -> isIdent v'
       --   _        = False -- (TODO)
          (Tm _)  -> False
          (Tc _)  -> False
          (K0 _)  -> False
          (K1 _)  -> False
          (F [])  -> False
          (F (_:_))-> False   -- WAAROM?? Stef, is dit goed? Het ziet er zo raar uit... (Vergelijk ook deze versie met de vorige.)

    

   v :: Sign -> Expression
   v (a,b) = Tm (V [] (a,b))

   notCp :: Expression -> Expression
   notCp (Cp e') = e'
   notCp e' = Cp e'

   isPos :: Expression -> Bool
   isPos (Cp _) = False
   isPos _ = True
   isNeg :: Expression -> Bool
   isNeg e' = not (isPos e')

   instance Morphic Expression where
    multiplicities expr = case expr of
        (Tm mph)-> multiplicities mph
        (Tc f)  -> multiplicities f
        (F ts)  -> foldr isc [Uni,Tot,Sur,Inj] (map multiplicities ts) -- homogene multiplicities can be used and deduced by and from rules: many rules are multiplicities (TODO)
        (Fd _)  -> [] -- many rules with Fd in it are multiplicities (TODO). Solve perhaps by defining relation a = (Fd ts)
        (Fu _)  -> [] -- expr \/ a is Rfx if a is Rfx, is Tot if a is Tot (TODO), is Uni if both a and expr are uni
        (Fi _)  -> [] -- expr /\ a is Asy if a is Asy, is Uni if a is Uni (TODO), is Uni if both a and expr are uni
        (K0 e') -> [Rfx,Trn] `uni` (multiplicities e'>-[Uni,Inj])
        (K1 e') -> [Trn] `uni` (multiplicities e'>-[Uni,Inj])
        (Cp e') -> [p|p<-multiplicities e', p==Sym]

    flp expr = case expr of
        (Tm mph)-> Tm (flp mph)
        (Tc f)  -> Tc (flp f)
        (F ts)  -> F (map flp (reverse ts))
        (Fd ts) -> Fd (map flp (reverse ts))
        (Fu fs) -> Fu (map flp fs)
        (Fi fs) -> Fi (map flp fs)
        (K0 e') -> K0 (flp e')
        (K1 e') -> K1 (flp e')
        (Cp e') -> Cp (flp e')

    isNot expr = case expr of        -- > tells whether the argument is equivalent to I-
        (Tm mph)   -> isNot mph    
        (Tc f)     -> isNot f
        (F [])     -> False
        (F [t])    -> isNot t        
        (F (_:_))  -> False       -- WAAROM?? Stef, is dit goed? Het ziet er zo raar uit... (Vergelijk ook deze versie met de vorige.)
        (Fd [])    -> False
        (Fd [t])   -> isNot t
        (Fd (_:_)) -> False
        (Fu [])    -> False
        (Fu [t])   -> isNot t
        (Fu (_:_)) -> False
        (Fi [])    -> False
        (Fi [t])   -> isNot t
        (Fi (_:_)) -> False
        (K0 _)     -> False
        (K1 _)     -> False
        (Cp _)     -> False
        
    typeUniq expr = case expr of
        (Tm mph) -> typeUniq mph -- I don't understand what typeUniq does - Bas. (TODO) WAAROM? (uitleggen bij de class definition, niet hier)
        (Tc f)   -> typeUniq f
        (F  ts)  -> and (map typeUniq ts)
        (Fd ts)  -> and (map typeUniq ts)
        (Fu fs)  -> and (map typeUniq fs)
        (Fi fs)  -> and (map typeUniq fs)
        (K0 e')  -> typeUniq e'
        (K1 e')  -> typeUniq e'
        (Cp e')  -> typeUniq e'

    isTrue expr = case expr of
        (F [])    -> False -- > fout voor singletons (TODO)
        (F ts) | isFunction    (head ts) -> (isTrue. F .tail) ts
               | isFlpFunction (last ts) -> (isTrue. F .init) ts
               | otherwise               -> isTrue (head ts) && isTrue (last ts) &&
                                                   (not.isFalse. F .drop 1.init) ts  -- niet isFalse tussen head en last
        (Fd as)   -> isFalse (F (map notCp as))
        (Fu fs)   -> or  [isTrue f| f<-fs] -- isImin \/ isIdent => isTrue (onder andere => TODO)
        (Fi fs)   -> and [isTrue f| f<-fs]
        (K0 e')   -> isTrue (K1 e')
        (K1 e')   -> isTrue e' -- als elk elem van (source e) in een cykel (in e) zit, dan ook is K0 ook True (TODO)
        (Cp e')   -> isFalse e'
        (Tm mph)  -> isTrue mph
        (Tc f)    -> isTrue f

    isFalse expr = case expr of
        (F [])    -> False -- > helemaal correct
        (F ts)    -> or (map isFalse ts) -- ook True als twee concepten op de ; niet aansluiten: a[A*B];b[C*D] met B/\C=0 (Dit wordt echter door de typechecker uitgesloten)
        (Fd as)   -> isTrue (F (map notCp as))
        (Fu fs)   -> and [isFalse f| f<-fs]
        (Fi fs)   -> or  [isFalse f| f<-fs] -- isImin /\ isIdent => isFalse (onder andere => TODO)
        (K0 _)    -> False
        (K1 e')   -> isFalse e'
        (Cp e')   -> isTrue e'
        (Tm mph)  -> isFalse mph
        (Tc f)    -> isFalse f

    isSignal _ = False

    isProp expr = case expr of
        (F ts)    -> null ([Asy,Sym]>-multiplicities (F ts)) || and [null ([Asy,Sym]>-multiplicities t)| t<-ts]
        (Fd [e']) -> isProp e'
        (Fd as)   -> isProp (F (map notCp as))
        (Fu fs)   -> and [isProp f| f<-fs]
        (Fi fs)   -> or [isProp f| f<-fs]
        (K0 e')   -> isProp e'
        (K1 e')   -> isProp e'
        (Cp e')   -> isTrue e'
        (Tm mph)  -> isProp mph
        (Tc f)    -> isProp f


                      

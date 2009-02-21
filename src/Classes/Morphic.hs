
module Classes.Morphic where
   import Adl.Concept
   import Adl.Prop
   import Adl.ConceptDef
   import Adl.Context
   import Adl.MorphismAndDeclaration
   import Adl.Gen
   import Adl.Expression
   import Adl.ObjectDef
   import Adl.KeyDef
   import Adl.Population
   import Adl.Pattern
   import Adl.Rule
   import Collection (Collection (uni,isc,(>-),empty,rd))

   instance MorphicId Concept where
    isIdent c = True    -- > tells whether the argument is equivalent to I
   
   instance Morphic Concept where
    multiplicities c = [Uni,Tot,Sur,Inj,Sym,Trn,Rfx]
    flp c = c
--    isIdent c = True    -- > tells whether the argument is equivalent to I
    isProp c = True    -- > tells whether the argument is equivalent to I
    isNot c   = False   -- > tells whether the argument is equivalent to I-
    isMph c = False
    isTrue c = singleton c
    isFalse c = False
    isSignal c = False
    singleton S = True
    singleton _ = False
    typeUniq Anything = False
    typeUniq _ = True

   instance MorphicId Morphism where
    isIdent m                         = isIdentM m              -- > tells whether the argument is equivalent to I
   
   instance Morphic Morphism where
    multiplicities (Mph nm pos atts (a,b) True  s) = multiplicities s
    multiplicities (Mph nm pos atts (a,b) False s) = flipProps (multiplicities s)
    multiplicities (V atts (a,b)) = [Tot,Sur]++[Inj| singleton a]++[Uni| singleton b]++
                                    [Asy| a==b, singleton b]++[Sym|a==b]++[Rfx|a==b]++[Trn|a==b]
    multiplicities (I _ _ _ _)    = [Inj,Sur,Uni,Tot,Sym,Asy,Trn,Rfx]
    multiplicities (Mp1 _ _)      = [Inj,Uni,Sym,Asy,Trn]
    flp (Mph nm pos atts (a,b) yin s) = Mph nm pos (reverse atts) (b,a) (not yin) s
    flp (V atts (a,b))                = V (reverse atts) (b,a)
    flp i                             = i
--    isIdent m                         = isIdentM m              -- > tells whether the argument is equivalent to I
    isProp (I _ _ _ _)                = True                    -- > tells whether the argument is equivalent to I
    isProp (V _ (a,b))                = a==b && singleton a
    isProp (Mp1 _ _)                  = True
    isProp m                          = null ([Asy,Sym]>-multiplicities m)
    isNot m                           = isNot (makeDeclaration m)   -- > tells whether the argument is equivalent to I-
    isMph (Mph _ _ _ _ _ _)           = True
    isMph _                           = False
    isTrue (V _ _)                    = True
    isTrue (I _ a b _)                = singleton b
    isTrue _                          = False
    isFalse _                         = False
    isSignal m                        = isSignal (makeDeclaration m)
    typeUniq (Mph nm pos  []  (a,b) _ s) = typeUniq a && typeUniq b
    typeUniq (Mph nm pos atts (a,b) _ s) = True
    typeUniq (I  []  g s yin) = typeUniq g && typeUniq s
    typeUniq (I atts g s yin) = True
    typeUniq (V  []  (a,b)) = typeUniq a && typeUniq b
    typeUniq (V atts (a,b)) = True

   instance MorphicId Declaration where 
    isIdent (Isn _ _)                            = True   -- > tells whether the argument is equivalent to I
    isIdent _                                    = False

   instance Morphic Declaration where
    multiplicities (Sgn _ _ _ ps _ _ _ _ _ _ _ _)= ps
    multiplicities (Isn g s)         | g==s      = [Uni,Tot,Sur,Inj,Sym,Trn,Rfx]
                                     | otherwise = [Uni,Tot,    Inj,Sym,Trn,Rfx]
    multiplicities (Iscompl g s)                 = [Sym]
    multiplicities (Vs g s)                      = [Tot,Sur]
    flp(Sgn nm a b props prL prM prR cs expla pos nr sig)
                                                 = Sgn nm b a (flipProps props) "" "" "" (map reverse cs) expla pos nr sig
    flp    i                                     = i
--    isIdent (Isn _ _)                            = True   -- > tells whether the argument is equivalent to I
--    isIdent _                                    = False
    isProp (Isn _ _)                             = True                    -- > tells whether the argument is equivalent to I
    isProp (Iscompl g s)                         = False
    isProp (Vs a b)                              = a==b && singleton a
    isProp d                                     = null ([Asy,Sym]>-multiplicities d)
    isNot (Iscompl _ _)                          = True   -- > tells whether the argument is equivalent to I-
    isNot _                                      = False
    isTrue (Vs _ _)                              = True
    isTrue _                                     = False
    isFalse _                                    = False
    isSignal (Sgn _ _ _ _ _ _ _ _ _ _ _ s)       = s
    isSignal _                                   = False
    isMph (Sgn _ a b _ _ _ _ _ _ _ _ _)          = True
    isMph _                                      = False
    typeUniq (Sgn _ a b _ _ _ _ _ _ _ _ _)       = typeUniq a && typeUniq b
    typeUniq (Isn g s)                           = typeUniq g && typeUniq s
    typeUniq (Iscompl g s)                       = typeUniq g && typeUniq s
    typeUniq (Vs g s)                            = typeUniq g && typeUniq s

   instance MorphicId Expression where
    isIdent (F ts)       = and [isIdent t| t<-ts]   -- > a;a~ = I bij bepaalde multipliciteiten (TODO)
    isIdent (Fd [e])     = isIdent e
    isIdent (Fd as)      = isImin (F (map Cp as))
    isIdent (Fu fs)      = and [isIdent f| f<-fs] && not (null fs)   -- > fout voor singletons (TODO)
    isIdent (Fi fs)      = and [isIdent f| f<-fs] && not (null fs)   -- > fout voor singletons (TODO)
    isIdent (K0 e)       = isIdent e || isFalse e
    isIdent (K1 e)       = isIdent e
    isIdent (Cp e)       = isImin e
    isIdent (Tm m)       = isIdent m
    isIdent (Tc f)       = isIdent f


   instance Morphic Expression where

    multiplicities (Tm m)  = multiplicities m
    multiplicities (Tc f)  = multiplicities f
    multiplicities (F ts)  = foldr isc [Uni,Tot,Sur,Inj] (map multiplicities ts) -- homogene multiplicities can be used and deduced by and from rules: many rules are multiplicities (TODO)
    multiplicities (Fd ts) = [] -- many rules with Fd in it are multiplicities (TODO). Solve perhaps by defining relation a = (Fd ts)
    multiplicities (Fu fs) = [] -- expr \/ a is Rfx if a is Rfx, is Tot if a is Tot (TODO), is Uni if both a and expr are uni
    multiplicities (Fi fs) = [] -- expr /\ a is Asy if a is Asy, is Uni if a is Uni (TODO), is Uni if both a and expr are uni
    multiplicities (K0 e)  = [Rfx,Trn] `uni` (multiplicities e>-[Uni,Inj])
    multiplicities (K1 e)  = [Trn] `uni` (multiplicities e>-[Uni,Inj])
    multiplicities (Cp e)  = [p|p<-multiplicities e, p==Sym]

    flp (Tm m)             = Tm (flp m)
    flp (Tc f)             = Tc (flp f)
    flp (F ts)             = F (map flp (reverse ts))
    flp (Fd ts)            = Fd (map flp (reverse ts))
    flp (Fu fs)            = Fu (map flp fs)
    flp (Fi fs)            = Fi (map flp fs)
    flp (K0 e)             = K0 (flp e)
    flp (K1 e)             = K1 (flp e)
    flp (Cp e)             = Cp (flp e)

    isNot (Tm m)           = isNot m    -- > tells whether the argument is equivalent to I-
    isNot (Tc f)           = isNot f
    isNot (F [t])          = isNot t
    isNot (Fd [t])         = isNot t
    isNot (Fu [f])         = isNot f
    isNot (Fi [f])         = isNot f
    isNot _                = False

    typeUniq (Tm m)        = typeUniq m -- I don't understand what typeUniq does - Bas. (TODO)
    typeUniq (Tc f)        = typeUniq f
    typeUniq (F  ts)       = and (map typeUniq ts)
    typeUniq (Fd ts)       = and (map typeUniq ts)
    typeUniq (Fu fs)       = and (map typeUniq fs)
    typeUniq (Fi fs)       = and (map typeUniq fs)
    typeUniq (K0 e)        = typeUniq e
    typeUniq (K1 e)        = typeUniq e
    typeUniq (Cp e)        = typeUniq e

    isMph (Tm m)           = isMph m
    isMph (Tc f)           = isMph f
    isMph (F [t])          = isMph t
    isMph (Fd [t])         = isMph t
    isMph (Fu [f])         = isMph f
    isMph (Fi [f])         = isMph f
    isMph (K0 e)           = isMph e
    isMph (K1 e)           = isMph e
    isMph _                = False

    isTrue (F [])       = False -- > fout voor singletons (TODO)
    isTrue (F ts)       | isFunction    (head ts) = (isTrue. F .tail) ts
                        | isFlpFunction (last ts) = (isTrue. F .init) ts
                        | otherwise               = isTrue (head ts) && isTrue (last ts) &&
                                                   (not.isFalse. F .drop 1.init) ts  -- niet isFalse tussen head en last
    isTrue (Fd as)      = isFalse (F (map notCp as))
    isTrue (Fu fs)      = or  [isTrue f| f<-fs] -- isImin \/ isIdent => isTrue (onder andere => TODO)
    isTrue (Fi fs)      = and [isTrue f| f<-fs]
    isTrue (K0 e)       = isTrue (K1 e)
    isTrue (K1 e)       = isTrue e -- als elk elem van (source e) in een cykel (in e) zit, dan ook is K0 ook True (TODO)
    isTrue (Cp e)       = isFalse e
    isTrue (Tm m)       = isTrue m
    isTrue (Tc f)       = isTrue f

    isFalse (F [])       = False -- > helemaal correct
    isFalse (F ts)       = or (map isFalse ts) -- ook True als twee concepten op de ; niet aansluiten: a[A*B];b[C*D] met B/\C=0 (Dit wordt echter door de typechecker uitgesloten)
    isFalse (Fd as)      = isTrue (F (map notCp as))
    isFalse (Fu fs)      = and [isFalse f| f<-fs]
    isFalse (Fi fs)      = or  [isFalse f| f<-fs] -- isImin /\ isIdent => isFalse (onder andere => TODO)
    isFalse (K0 _)       = False
    isFalse (K1 e)       = isFalse e
    isFalse (Cp e)       = isTrue e
    isFalse (Tm m)       = isFalse m
    isFalse (Tc f)       = isFalse f

    isSignal e           = False

    isProp (F ts)       = null ([Asy,Sym]>-multiplicities (F ts)) || and [null ([Asy,Sym]>-multiplicities t)| t<-ts]
    isProp (Fd [e])     = isProp e
    isProp (Fd as)      = isProp (F (map notCp as))
    isProp (Fu fs)      = and [isProp f| f<-fs]
    isProp (Fi fs)      = or [isProp f| f<-fs]
    isProp (K0 e)       = isProp e
    isProp (K1 e)       = isProp e
    isProp (Cp e)       = isTrue e
    isProp (Tm m)       = isProp m
    isProp (Tc f)       = isProp f

   instance MorphicId Rule where
    isIdent r = isIdent (normExpr r)

   instance Morphic Rule where
    multiplicities r           = []
    isMph r  | ruleType r==Truth = isMph (consequent r)
             | otherwise       = False
    flp r@(Ru Truth antc pos expr cpu expla (a,b) nr pn) = Ru Truth (error ("(Module CC_aux:) illegal call to antecedent in flp ("++show r++")")) pos (flp expr) cpu expla (b,a) nr pn
    flp (Ru c antc pos cons cpu expla (a,b) nr pn)   = Ru c (flp antc) pos (flp cons) cpu expla (b,a) nr pn
  --  isIdent r = error ("(module CC_aux: isIdent) not applicable to any rule:\n "++showHS "" r)
    typeUniq r | ruleType r==Truth = typeUniq (antecedent r)
               | otherwise       = typeUniq (antecedent r) && typeUniq (consequent r)
--    isIdent r = isIdent (normExpr r)
    isProp r = isProp (normExpr r)
    isTrue r | ruleType r==Truth  = isTrue (consequent r)
             | otherwise        = isTrue (consequent r) || isFalse (consequent r)
    isFalse r| ruleType r==Truth  = isFalse (consequent r)
             | otherwise        = isFalse (consequent r) && isTrue (consequent r)
    isSignal r = isSignaal r
    isNot r  | ruleType r==Truth  = isNot (consequent r)
             | otherwise        = False  -- TODO: check correctness!


   normExpr :: Rule -> Expression
   normExpr rule
    | isSignal rule      = v (sign rule)
    | ruleType rule==Truth = consequent rule
    | ruleType rule==Implication = Fu [Cp (antecedent rule), consequent rule]
    | ruleType rule==Equivalence = Fi [ Fu [antecedent rule, Cp (consequent rule)]
                              , Fu [Cp (antecedent rule), consequent rule]]
    | otherwise          = error("Fatal (module CC_aux): Cannot make an expression of "++show rule)

   isImin :: Expression -> Bool
   isImin (Fd ts)  = and [isImin t| t<-ts]
   isImin (Fi fs)  = and [isImin f| f<-fs] && not (null fs)
   isImin (Fu fs)  = and [isImin f| f<-fs] && not (null fs)
   isImin (F  [e]) = isImin e
   isImin (Cp v)   = isIdent v
   isImin _        = False -- (TODO)

   class Substitutive a where
 -- precondition: sign f `order` sign m
    subst :: (Expression,Expression) -> a -> a
    subst (m,f) x = error "(module CC_aux) Unable to substitute"

   instance (Morphic a,Substitutive a) => Substitutive [a] where
    subst (m,f) xs = map (subst (m,f)) xs


module Classes.Morphic where
   import Adl.Concept(Concept(..),Association(..),Morphic(..))
   import Adl.Prop
   import Adl.MorphismAndDeclaration
   import Adl.Expression(Expression(..),notCp,v)
   import Adl.Rule (Rule(..),RuleType(..),ruleType,consequent,antecedent)
   import Collection (Collection (uni,isc,(>-)))

   class Association a => MorphicId a where
    isIdent        :: a -> Bool  -- > tells whether the argument is equivalent to I

   instance MorphicId Concept where
    isIdent _ = True    -- > tells whether the argument is equivalent to I
   
   instance Morphic Concept where
    multiplicities _ = [Uni,Tot,Sur,Inj,Sym,Trn,Rfx]
    flp c = c
--    isIdent c = True    -- > tells whether the argument is equivalent to I
    isProp _ = True    -- > tells whether the argument is equivalent to I
    isNot _   = False   -- > tells whether the argument is equivalent to I-
    isMph _ = False
    isTrue c = singleton c
    isFalse _ = False
    isSignal _ = False
    singleton S = True
    singleton _ = False
    typeUniq Anything = False
    typeUniq _ = True

   instance MorphicId Morphism where
    isIdent mph = isIdentM mph              -- > tells whether the argument is equivalent to I
   
   instance Morphic Morphism where
    multiplicities mph 
      = case mph of
           Mph{mphyin = True}  -> multiplicities (mphdcl mph)
           Mph{mphyin = False} -> flipProps (multiplicities (mphdcl mph))
           V {}                -> [Tot]
                                ++[Sur]
                                ++[Inj| singleton(source (mphtyp mph))]
                                ++[Uni| singleton(target (mphtyp mph))]
                                ++[Asy| homogeneous(mphtyp mph), singleton(target (mphtyp mph))]
                                ++[Sym| homogeneous(mphtyp mph)]
                                ++[Rfx| homogeneous(mphtyp mph)]
                                ++[Trn| homogeneous(mphtyp mph)]
           I{}                 -> [Inj,Sur,Uni,Tot,Sym,Asy,Trn,Rfx]
           Mp1{}               -> [Inj,Uni,Sym,Asy,Trn]
    flp mph 
      = case mph of
           Mph{mphtyp = (s,t)} -> mph{ mphats = reverse(mphats mph)
                                     , mphtyp = (t,s)
                                     , mphyin = not (mphyin mph)
                                     }
           V{mphtyp = (s,t)}   -> V  { mphats = reverse(mphats mph)
                                     , mphtyp = (t,s)
                                     }
           I{}                 -> mph
           Mp1{}               -> mph
    isProp mph = case mph of
           Mph{}               -> null ([Asy,Sym]>-multiplicities mph)
           V{}                 -> homogeneous(mphtyp mph) && singleton(source (mphtyp mph))
           I{}                 -> True
           Mp1{}               -> True
    isNot mph  = isNot (makeDeclaration mph)   -- > tells whether the argument is equivalent to I-
    isMph mph = case mph of 
           Mph{}               -> True
           _                   -> False
    isTrue mph = case mph of
           Mph{}               -> False
           V{}                 -> True
           I{}                 -> singleton (mphspc mph)
           Mp1{}               -> False
    isFalse _   = False
    isSignal mph = isSignal (makeDeclaration mph)
    typeUniq mph = case mph of
           Mph{mphats = []}    ->  typeUniq (source (mphtyp mph)) && 
                                   typeUniq (target (mphtyp mph))
           Mph{mphats = _:_}   ->  True
           I{mphats = []}      ->  typeUniq (mphgen mph) && 
                                   typeUniq (mphspc mph)
           I{mphats = _:_}     ->  True
           V{mphats = []}      ->  typeUniq (mphgen mph) && 
                                   typeUniq (mphspc mph)
           V{mphats = _:_}     ->  True
           Mp1{}               ->   undefined   --WAAROM? Stef, dit was niet gedefinieerd TODO
    

   instance MorphicId Declaration where 
    isIdent (Isn _ _)                            = True   -- > tells whether the argument is equivalent to I
    isIdent _                                    = False

   instance Morphic Declaration where
    multiplicities d = case d of
           Sgn {}       -> decprps d
           Isn{}        -> [Uni,Tot,Sur,Inj,Sym,Trn,Rfx]         --WAAROM? Stef, waarom is dit niet ook Asy? Is dit niet gewoon FOUT?
                        ++ [Sur | (degen d) == (despc d)]
           Iscompl{}    -> [Sym]
           Vs{}         -> [Tot,Sur]
    flp d = case d of
           Sgn {}       -> d{ decprps = flipProps (decprps d)
                            , decprL  = ""
                            , decprM  = ""
                            , decprR  = ""
                            , decpopu = map reverse (decpopu d)
                            }
           Isn{}        -> d
           Iscompl{}    -> d
           Vs{}         -> d
    isProp d = case d of         -- > tells whether the argument is equivalent to I
           Sgn {}       -> null ([Asy,Sym]>-multiplicities d)
           Isn{}        -> True
           Iscompl{}    -> False
           Vs{}         -> ((degen d) == (despc d)) && singleton (degen d)
    isNot d = case d of          -- > tells whether the argument is equivalent to I-
           Iscompl{}    -> True   
           _            -> False
    isTrue d = case d of 
           Vs{}         -> True
           _            -> False
    isFalse _ = False
    isSignal d = case d of
           Sgn {}       -> deciss d
           _            -> False
    isMph d = case d of
           Sgn {}       -> True
           _            -> False
    typeUniq d = case d of
           Sgn {}       -> typeUniq (desrc d) && typeUniq (detgt d)
           Isn{}        -> typeUniq (degen d) && typeUniq (despc d)
           Iscompl{}    -> typeUniq (degen d) && typeUniq (despc d)
           Vs{}         -> typeUniq (degen d) && typeUniq (despc d)
           
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

    isMph expr = case expr of
        (Tm mph)  -> isMph mph
        (Tc f)    -> isMph f
        (F  [])   -> False
        (F  [f])  -> isMph f
        (F  (_:_))-> False       -- WAAROM?? Stef, is dit goed? Het ziet er zo raar uit... (Vergelijk ook deze versie met de vorige.)
        (Fd [])   -> False
        (Fd [f])  -> isMph f
        (Fd (_:_))-> False
        (Fu [])   -> False
        (Fu [f])  -> isMph f
        (Fu (_:_))-> False
        (Fi [])   -> False
        (Fi [f])  -> isMph f
        (Fi (_:_))-> False
        (K0 e')   -> isMph e'
        (K1 e')   -> isMph e'
        (Cp _)    -> False
        
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

   instance MorphicId Rule where
    isIdent r = isIdent (normExpr r)

   instance Morphic Rule where
    multiplicities _  = []
    isMph r = case r of
                Ru{rrsrt=Truth} -> isMph (rrcon r)
                Ru{}            -> False
                Sg{}            -> isMph (srsig r)
                Gc{}            -> False
                Fr{}            -> False
                
--    isMph r  | ruleType r==Truth = isMph (consequent r)
--             | otherwise       = False
    flp r = case r of
                Ru{} -> r{rrant = if rrsrt r == Truth
                                  then error ("(Module Classes.Morphic:) illegal call to antecedent in flp ("++show r++")")
                                  else flp (rrant r)
                         ,rrcon = flp (rrcon r)
                         ,rrtyp = (target (rrtyp r),source (rrtyp r))
                         }
                Sg{}            -> undefined
                Gc{}            -> undefined
                Fr{}            -> undefined
  --  isIdent r = error ("(module CC_aux: isIdent) not applicable to any rule:\n "++showHS "" r)
    typeUniq r | ruleType r==Truth = typeUniq (antecedent r)
               | otherwise       = typeUniq (antecedent r) && typeUniq (consequent r)
--    isIdent r = isIdent (normExpr r)
    isProp r = isProp (normExpr r)
    isTrue r | ruleType r==Truth  = isTrue (consequent r)
             | otherwise        = isTrue (consequent r) || isFalse (consequent r)
    isFalse r| ruleType r==Truth  = isFalse (consequent r)
             | otherwise        = isFalse (consequent r) && isTrue (consequent r)
    isSignal r = case r of
                   Sg{} -> True
                   _    -> False 
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
   isImin expr = case expr of
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

   class Substitutive a where  --WAAROM? Stef, hier hoort een vette uitleg bij...
 -- precondition: sign f `order` sign m
    subst :: (Expression,Expression) -> a -> a
 --   subst (_,_) _ = error "(module CC_aux) Unable to substitute"

   instance (Morphic a,Substitutive a) => Substitutive [a] where
    subst (m',f) xs = map (subst (m',f)) xs


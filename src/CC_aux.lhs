> module CC_aux
>            ( --Architecture,archContexts
>             Pattern(Pat)
>            , Declaration(Sgn, Vs, Isn, Iscompl)
>            , ConceptDef()
>            , ObjectDef(Obj), ObjDefs, Object(concept, attributes, ctx, populations, extends), objectOfConcept, KeyDef(Kd), KeyDefs
>            , Rule(Ru,Sg,Gc)
>            , Gen(G)
>            , Pairs
>            , Morphism(Mph,I,V,Mp1)
>            , Concept()
>            , Prop(Uni,Tot,Inj,Sur,Sym,Asy,Trn,Rfx,Aut)
>            , Expressions
>            , Expression(Fu,Fi,Fd,Tc,F,Tm,K0,K1,Cp)
>            , v, mIs, sIs
>            , makeMph
>            , posNone
>            , pKey_pos
>            , pVarid_val_pos, pConid_val_pos
>            , Numbered(nr,pos), renumberRules
>            , normExpr
>            , isSgn
>            , fEmpty
>            , oneMorphism
>            , single
>            , Morphic(source, target, sign, multiplicities, flp, isIdent, isProp, isMph, isNot, isTrue, isFalse, isSignal{-, singleton-}, equiv, typeUniq)
>            , Morphical( concs, conceptDefs, mors, morlist, declarations, genE, closExprs, objDefs, keyDefs )
>            , Language( declaredRules, multRules, rules, signals, specs, patterns, objectdefs, isa )
>            , inline,idsOnly,explain , makeInline
>            , applyM, declaration
>            , glb, lub, sur, inj, fun, tot
>            , ruleType, antecedent, mkVar
>            , Calc(calc)
>            , ShowHS(showHSname,showHS)
>            , ShowADL(showADL)
>            , consequent, order, isNeg, isPos, notCp
>            , isProperty 
>            , union
>            , Population(Popu), Populations
>            , Pop(update, put_gE)
>            , Gens, Declarations, GenR, Contexts
>            , contents, makeConceptSpace, pMeaning
>            , FilePos(FilePos), ConceptDefs, Concepts
>            , Paire, Rules, Morphisms, Patterns
>            , anything, shSigns, gEtabG
>            , keys, cpu
>            , isFunction, isFlpFunction

>            , wrld, src, trg, uncomp
>            , conts, cod, clearG, dom, subst

>            , Key, showFullRelName
>  ) where
>  import Char (toLower)
>  import UU_Scanner
>  import UU_Parsing
>  import CommonClasses ( Identified(name))
>  import Collection (Collection (uni,isc,(>-),empty,rd)  )
>  import Languages (Lang,ShowLang,plural)
>  import Auxiliaries  
>          ( sort', chain, rEncode, commaEng, clos1, diag
>           ,eqCl, sord, eqClass, rd', enumerate, unCap, showL, haskellIdentifier)
>  import Classification 
>            ( Classification(),preCl,mapCl
>            )
>  import Typology ( Inheritance(Isa), Typologic, genEq, typology)
>  import ADLdef

TODO:
 - Onderscheid tussen signaal en regel maken in de datastructuur van regel.
 - rules alleen regels op laten leveren, dus geen signalen en w'el multipliciteiten.
 - declarations geeft precies de gedeclareerde relaties
 - toestaan van signalen met namen van declraties en met dubbele namen (grondig testen!).



>  objectOfConcept context cpt = if length os == 0 then Nothing else Just (head os)
>    where os = [o|o<-attributes context,concept o == cpt]

>  class Ord a => ABoolAlg a where
>   glb,lub :: a -> a -> a
>   order :: a -> a -> Bool
>   glb a b | b <= a = b
>           | a <= b = a
>           | otherwise  = error "(module CC_aux) glb undefined"
>   lub a b | a <= b = b
>           | b <= a = a
>           | otherwise = error "(module CC_aux) lub undefined"
>   order a b | a <= b = True
>             | b <= a = True
>             | otherwise = False

>  gEtable cs
>   = chain "\n" ([f l " "     ++"  "++chain " " (map (f 6.name) cs)]++
>                 [f l (name c')++" |"++chain "|"[f 6 (show (c <= c'))|c<-cs]| c'<-cs])
>     where l = maximum (map (length.name) cs); f n str = take n (str++forever ' ')
>           forever c = c:forever c

>  gEtabG gEq cs
>   = chain "\n" ([f l " "     ++"  "++chain " " (map (f 6.name) cs)]++
>                 [f l (name c')++" |"++chain "|"[f 6 (show (c <= c'))|c<-cs]| c'<-cs])
>     where l = maximum (map (length.name) cs); f n str = take n (str++forever ' ')
>           forever c = c:forever c

>  instance ABoolAlg Concept where
>   glb a b | b <= a = b
>           | a <= b = a
>           | otherwise = error ("(module CC_aux) Fatal: (C) glb undefined: a="++show a++", b="++show b)
>   lub a b | a <= b = b
>           | b <= a = a
>           | otherwise = error ("(module CC_aux) Fatal: (C) lub undefined: a="++show a++", b="++show b)

  instance (Ord a,Ord b) => Ord (a,b) where
   (a,a') <= (b,b') = a <= b && a' <= b'

>  instance (Show a,Show b,ABoolAlg a,ABoolAlg b) => ABoolAlg (a,b) where
>   glb a b | a <= b = a
>           | b <= a = b
>           | otherwise = error ("(module CC_aux) Fatal: glb undefined: a="++show a++", b="++show b)
>   lub a b | b <= a = a
>           | a <= b = b
>           | otherwise = error ("(module CC_aux) Fatal: lub undefined: a="++show a++", b="++show b)

  instance Ord Declaration where
   a <= b = source a <= source b && target a <= target b

>  instance Ord Morphism where
>   a <= b = source a <= source b && target a <= target b

>  instance ABoolAlg Morphism  -- SJ  2007/09/14: This is used solely for drawing conceptual graphs.

>  instance Ord Expression where
>   a <= b = source a <= source b && target a <= target b

Keys are for instance: KEY Person(ssn)
                       KEY Person(name, birthDate, cityOfBirth)
which means:
I[Person] = ssn;ssn~
I[Person] = name;name~ /\ birthDate;birthDate~ /\ cityOfBirth;cityOfBirth~



>  v :: (Concept, Concept) -> Expression
>  v (a,b) = Tm (V [] (a,b))
>  mIs :: Concept -> Morphism
>  mIs c = I [] c c True
>  sIs c = Isn c c

>  dom, cod :: Declaration -> [String]
>  dom s = rd [src l| l<-contents s]
>  cod s = rd [trg l| l<-contents s]

>  pMeaning Uni   = "univalent"
>  pMeaning Inj   = "injective"
>  pMeaning Sur   = "surjective"
>  pMeaning Tot   = "total"
>  pMeaning Sym   = "symmetric"
>  pMeaning Asy   = "antisymmetric"
>  pMeaning Trn   = "transitive"
>  pMeaning Rfx   = "reflexive"
>  pMeaning Aut   = "automatic if possible"
>  isFunction m   = null ([Uni,Tot]>-multiplicities m)
>  isFlpFunction m = null ([Sur,Inj]>-multiplicities m)
>  isProperty m   = null([Sym,Asy]>-multiplicities m)

-- Chapter: Rules
There are 4 types of rule: 


renumberRule gives back the rule in the second argument, only numbered by the first
renumberRules gives back an array of rules as specified by the second argument, renumbered from n to n+length(r:rs)

>  renumberRule n r@(Ru 'A' b c d e f g _ h) = Ru 'A' (error ("(Module CC_aux:) illegal call to antecedent in renumberRule ("++showADL r++")")) c d e f g n h
>  renumberRule n (Ru a b c d e f g _ h)     = Ru a b c d e f g n h
>  renumberRule n (Sg p rule c d _ f g)      = Sg p (renumberRule n rule) c d n f g
>  renumberRule n (Gc a b c d e _ f)         = Gc a b c d e n f
>  renumberRule n r                          = r
>  renumberRules n (r:rs) = (renumberRule n r):renumberRules (n+1) rs
>  renumberRules _ [] = []


The following code is used in transforming expressions into clauses

>  isPos (Cp _) = False
>  isPos _ = True
>  isNeg (Cp _) = True
>  isNeg _ = False
>  notCp (Cp e) = e
>  notCp e = Cp e

Transform a rule to an expression:

>  normExpr :: Rule -> Expression
>  normExpr rule
>   | isSignal rule      = v (sign rule)
>   | ruleType rule=='A' = consequent rule
>   | ruleType rule=='I' = Fu [Cp (antecedent rule), consequent rule]
>   | ruleType rule=='E' = Fi [ Fu [antecedent rule, Cp (consequent rule)]
>                             , Fu [Cp (antecedent rule), consequent rule]]
>   | otherwise          = error("Fatal (module CC_aux): Cannot make an expression of "++showHS "" rule)


>  class Explained a where
>   explain :: a -> String

>  instance Explained Rule where
>   explain (Ru _ _ _ _ _ expla _ _ _) = expla
>   explain (Sg _ _ expla _ _ _ _)     = expla
>   explain r                          = ""

>  instance Explained Declaration where
>   explain (Sgn _ _ _ _ _ _ _ _ expla _ _ _) = expla
>   explain d                                 = ""

>  instance Explained Concept where
>   explain c = name c   
>   --explain (C expla _ _) = expla
>   --explain (S)           = "ONE"
>   --explain NOthing       = "Nothing"
>   --explain Anything      = "Anything"

>  class Conceptual a where
>   conts      :: a -> [String]                   -- the set of all instances in a concept

>  instance Conceptual a => Conceptual [a] where
>   conts                                         = rd . concat . map conts

>  instance Conceptual a => Conceptual (Classification a) where
>   conts                                         = rd . concat . map conts . preCl

>  instance Conceptual Concept where
>   conts (C {cptos = os}) = os
>   conts (S)        = error ("(module CC_aux) Fatal: ONE has exactly one concept, but it is not te be referred to")
>   conts Anything   = error ("(module CC_aux) Fatal: Anything is Everything...")
>   conts NOthing    = error ("(module CC_aux) Fatal: NOthing is not very much...")

>  class Morphical a where
>   concs        :: a -> Concepts                  -- the set of all concepts used in data structure a
>   conceptDefs  :: a -> ConceptDefs               -- the set of all concept definitions in the data structure
>   conceptDefs x = []
>   mors         :: a -> Morphisms                 -- the set of all morphisms used within data structure a
>   morlist      :: a -> Morphisms                 -- the list of all morphisms used within data structure a
>   declarations :: a -> Declarations
>--   declarations x  = rd [declaration m|m<-mors x]
>   genE         :: a -> GenR
>   genE x        = if null cx then (==) else head cx where cx = [gE|C {cptgE = gE } <-concs x]
>   closExprs    :: a -> [Expression]               -- no double occurrences in the resulting list of expressions
>   closExprs s   = []
>   objDefs      :: a -> ObjDefs
>   objDefs s     = []
>   keyDefs      :: a -> KeyDefs
>   keyDefs s     = []

>  instance Morphical a => Morphical [a] where
>   concs             = rd . concat . map concs
>   conceptDefs       = rd . concat . map conceptDefs
>   mors              = rd . concat . map mors
>   morlist           =      concat . map morlist
>   declarations      = rd . concat . map declarations
>   closExprs         = rd . concat . map closExprs
>   objDefs           =      concat . map objDefs
>   keyDefs           =      concat . map keyDefs

>  instance Morphical Concept where
>   concs        c                    = [c]
>   mors         c                    = [I [] c c True]
>   morlist      c                    = [I [] c c True]
>   declarations c                    = []
>   genE         (C {cptgE = gE})     = gE
>   genE         (S)                  = (<=)::Concept->Concept->Bool
>   genE         Anything             = (<=)::Concept->Concept->Bool
>   genE         NOthing              = (<=)::Concept->Concept->Bool

>  instance Morphical a => Morphical (Classification a) where
>   concs            = rd . concat . map concs . preCl
>   conceptDefs      = rd . concat . map conceptDefs . preCl
>   mors             = rd . concat . map mors . preCl
>   morlist          =      concat . map morlist . preCl
>   declarations     = rd . concat . map declarations . preCl
>   closExprs        = rd . concat . map closExprs . preCl

>  instance Morphical Declaration where
>   concs (Sgn _ a b _ _ _ _ _ _ _ _ _)           = rd [a,b]
>   concs (Isn g s)                               = rd [g,s]
>   concs (Iscompl g s)                           = [s]
>   concs (Vs g s)                                = [s]
>   mors s                                        = []
>   morlist s                                     = []
>   genE (Sgn nm a b _ _ _ _ _ _ _ _ _)           = genE a
>   genE (Isn g s)                                = genE s
>   genE (Iscompl g s)                            = genE s
>   genE (Vs g s)                                 = genE s
>   declarations s                                = [s]

>  instance Morphical Morphism where
>   concs (Mph nm pos atts (a,b) yin s)           = rd [a,b]
>   concs (I atts g s _)                          = rd [g,s]
>   concs (V atts (a,b))                          = rd [a,b]
>   mors m                                        = [makeInline m]
>   morlist m                                     = [m]
>   genE (Mph nm pos atts (a,b) _ s)              = genE a
>   genE (I atts g s _)                           = genE s
>   genE (V atts (a,b))                           = genE a
>   declarations m                                = [declaration m]

>  instance Morphical Gen where
>   concs (G pos g s)                             = rd [g,s]
>   mors (G pos g s)                              = [I [] g s True]
>   morlist (G pos g s)                           = [I [] g s True]
>   genE (G pos g s)                              = genE s
>   declarations (G pos g s)                      = []

>  instance Morphical Expression where
>   concs (Tm m)                                  = rd (concs m)
>   concs (Tc f)                                  = rd (concs f)
>   concs (F ts)                                  = rd (concs ts)
>   concs (Fd ts)                                 = rd (concs ts)
>   concs (Fu fs)                                 = rd (concs fs)
>   concs (Fi fs)                                 = rd (concs fs)
>   concs (K0 e)                                  = rd (concs e)
>   concs (K1 e)                                  = rd (concs e)
>   concs (Cp e)                                  = rd (concs e)

>   mors (Tm m)                                   = mors (makeInline m)
>   mors (Tc f)                                   = mors f
>   mors (F ts)                                   = mors ts -- voor a;-b;c hoeft geen extra mors rond b nodig te zijn
>   mors (Fd ts@(h:t))                            = rd (mors ts ++ (concat) [mors (source c)|c<-t])
>   mors (Fu fs)                                  = mors fs
>   mors (Fi fs)                                  = mors fs -- voor a /\ -b hoeft geen extra mors rond b nodig te zijn
>   mors (K0 e)                                   = mors e
>   mors (K1 e)                                   = mors e
>   mors (Cp e)                                   = rd (mors e ++ mors (source e)++mors (target e))

>   morlist (Tm m)                                = morlist m
>   morlist (Tc f)                                = morlist f
>   morlist (F ts)                                = morlist ts
>   morlist (Fd ts)                               = morlist ts
>   morlist (Fu fs)                               = morlist fs
>   morlist (Fi fs)                               = morlist fs
>   morlist (K0 e)                                = morlist e
>   morlist (K1 e)                                = morlist e
>   morlist (Cp e)                                = morlist e

>   genE (Tm m)                                   = genE m
>   genE (Tc f)                                   = genE f
>   genE (F ts)                                   = genE ts
>   genE (Fd ts)                                  = genE ts
>   genE (Fu fs)                                  = genE fs
>   genE (Fi fs)                                  = genE fs
>   genE (K0 e)                                   = genE e
>   genE (K1 e)                                   = genE e
>   genE (Cp e)                                   = genE e

>   declarations (Tm m)                           = declarations m
>   declarations (Tc f)                           = declarations f
>   declarations (F ts)                           = declarations ts
>   declarations (Fd ts)                          = declarations ts
>   declarations (Fu fs)                          = declarations fs
>   declarations (Fi fs)                          = declarations fs
>   declarations (K0 e)                           = declarations e
>   declarations (K1 e)                           = declarations e
>   declarations (Cp e)                           = declarations e

>   closExprs (Tc f)                              = closExprs f
>   closExprs (F ts)                              = (rd.concat.map closExprs) ts
>   closExprs (Fd ts)                             = (rd.concat.map closExprs) ts
>   closExprs (Fu fs)                             = (rd.concat.map closExprs) fs
>   closExprs (Fi fs)                             = (rd.concat.map closExprs) fs
>   closExprs (K0 e)                              = [K0 e] `uni` closExprs e
>   closExprs (K1 e)                              = [K1 e] `uni` closExprs e
>   closExprs (Cp e)                              = closExprs e
>   closExprs _                                   = []

>  instance Morphical Rule where
>   concs (Ru c antc _ cons _ _ _ _ _)     = if c=='A' then concs cons else concs antc `uni` concs cons
>   concs (Sg _ rule _ _ _ _ _)            = concs rule
>   concs (Gc _ m expr _ _ _ _)            = concs m `uni` concs expr
>   concs (Fr _ d expr _)                  = concs d `uni` concs expr
>   mors (Ru c antc _ cons _ _ _ _ _)      = if c=='A' then mors cons else mors antc `uni` mors cons
>   mors (Sg _ rule _ _ _ _ _)             = mors rule
>   mors (Gc _ m expr _ _ _ _)             = mors m `uni` mors expr
>   mors (Fr _ d expr _)                   = mors d `uni` mors expr
>   morlist (Ru c antc _ cons _ _ _ _ _)   = if c=='A' then morlist cons else morlist antc++morlist cons
>   morlist (Sg _ rule _ _ _ _ _)          = morlist rule
>   morlist (Gc _ m expr _ _ _ _)          = morlist m++morlist expr
>   morlist (Fr _ _ expr _)                = morlist expr
>   genE (Ru c antc _ cons  _ _ _ _ _)     = if c=='A' then genE cons else genE [antc,cons]
>   genE (Sg _ rule _ _ _ _ _)             = genE rule
>   genE (Gc _ m expr _ _ _ _)             = genE m
>   genE (Fr _ _ expr _)                   = genE expr
>   declarations (Ru c antc _ cons _ _ _ _ _) = if c=='A' then declarations cons else declarations [antc,cons]
>   declarations (Sg _ rule _ _ _ _ d)        = [d] `uni` declarations rule
>   declarations (Gc _ m expr _ _ _ _)        = declarations m
>   declarations (Fr _ _ expr _)              = declarations expr
>   closExprs (Ru c antc _ cons _ _ _ _ _) = if c=='A' then closExprs cons else closExprs antc `uni` closExprs cons
>   closExprs (Sg _ rule _ _ _ _ _)        = closExprs rule
>   closExprs (Gc _ m expr  _ _ _ _)       = closExprs expr
>   closExprs (Fr _ _ expr _)              = [expr]

>  instance Morphical Pattern where
>   concs        (Pat nm rs gen pms cs ks) = concs rs `uni` concs gen `uni` concs pms
>   conceptDefs  (Pat nm rs gen pms cs ks) = cs
>   mors         (Pat nm rs gen pms cs ks) = mors rs `uni` mors ks
>   morlist      (Pat nm rs gen pms cs ks) = morlist rs++morlist ks
>   declarations (Pat nm rs gen pms cs ks) = rd pms
>   genE         (Pat nm rs gen pms cs ks) = genE (pms++declarations (signals rs))
>   closExprs    (Pat nm rs gen pms cs ks) = closExprs rs

>  instance Morphical Context where
>   concs        ctx = concs (ctxds ctx) `uni` concs (ctxpats ctx)
>   conceptDefs  ctx = ctxcs ctx
>   mors         ctx = mors (ctxpats ctx) `uni` mors (ctxos ctx)
>   morlist      ctx = morlist (ctxpats ctx)++morlist (ctxos ctx)
>   declarations ctx = rd (ctxds ctx ++[d| pat<-(ctxpats ctx), d<-declarations pat])
>   genE         ctx = genEq (typology (ctxisa ctx))
>   closExprs    ctx = closExprs (ctxpats ctx) `uni` closExprs (ctxos ctx)
>   objDefs      ctx = ctxos ctx
>   keyDefs      ctx = ctxks ctx

>  instance Morphical KeyDef where
>   concs        (Kd pos lbl ctx ats) = concs ctx `uni` concs ats
>   mors         (Kd pos lbl ctx ats) = mors ctx `uni` mors ats
>   morlist      (Kd pos lbl ctx ats) = morlist ctx ++ morlist ats
>   genE         (Kd pos lbl ctx ats) = genE ats
>   declarations (Kd pos lbl ctx ats) = declarations ctx `uni` declarations ats
>   keyDefs      k                    = [k]

>  instance Morphical ObjectDef where
>   concs        obj = [source (objctx obj)] `uni` concs (objats obj)
>   conceptDefs  obj = []
>   mors         obj = mors (objctx obj) `uni` mors (objats obj) `uni` mors (target (objctx obj))  -- opletten: de expressie (objctx obj) hoort hier ook bij.
>   morlist      obj = morlist (objctx obj)++morlist (objats obj)
>   declarations obj = []
>   closExprs    obj = closExprs (objctx obj) `uni` closExprs (objats obj)
>   objDefs      obj = [obj]

>  class Substitutive a where
>-- precondition: sign f `order` sign m
>   subst :: (Expression,Expression) -> a -> a
>   subst (m,f) x = error "(module CC_aux) Unable to substitute"

>  instance (Morphic a,Substitutive a) => Substitutive [a] where
>   subst (m,f) xs = map (subst (m,f)) xs

>  instance Substitutive Expression where
>   subst (Tm m,f) t@(Tm m') = if     m==m' then f     else
>                              if flp m==m' then flp f else t
>   subst (m,f) t@(Tm m')    = t
>   subst (m,f) f'           = if m `match` f'
>                              then (if m==f' then f else if flp m==f' then flp f else subs f')
>                              else subs f'
>    where
>      subs (Tc f')    = Tc (subst (m,f) f')
>      subs (F ts)     = F  (subst (m,f) ts)
>      subs (Fd ts)    = Fd (subst (m,f) ts)
>      subs (Fu fs)    = Fu (subst (m,f) fs)
>      subs (Fi fs)    = Fi (subst (m,f) fs)
>      subs (K0 e)     = K0 (subst (m,f) e)
>      subs (K1 e)     = K1 (subst (m,f) e)
>      subs (Cp e)     = Cp (subst (m,f) e)
>      subs e          = subst (m,f) e
>      Tm m  `match` Tm m'   = True
>      Tc f  `match` Tc f'   = True
>      F ts  `match` F  ts'  = True
>      Fd ts `match` Fd ts'  = True
>      Fu fs `match` Fu fs'  = True
>      Fi fs `match` Fi fs'  = True
>      K0 e  `match` K0 e'   = True
>      K1 e  `match` K1 e'   = True
>      Cp e  `match` Cp e'   = True
>      _     `match` _       = False

>  instance Substitutive Rule where
>   subst (m,f) r@(Ru 'A' antc pos cons cpu expla sgn nr pn)
>    = Ru 'A' (error ("(Module CC_aux:) illegal call to antecedent in subst ("++showADL m++","++showADL f++") ("++showADL r++")")) pos cons' cpu expla (sign cons') nr pn
>      where cons' = subst (m,f) cons
>   subst (m,f) r@(Ru c antc pos cons cpu expla sgn nr pn)
>    = if sign antc' `order` sign cons'
>      then Ru c antc' pos cons' cpu expla (sign antc' `lub` sign cons') nr pn
>      else r -- error ("(module CC_aux) Fatal: cannot execute:   subst (m,f) r\nwith m="++show m++"\n     f="++show f++"\nand  r="++showADL r++"\n"++showHS "" r++"\nbecause "++show (sign antc')++" `order` "++show (sign cons')++" is False.\n"++gEtabG gEq [c| (a,b)<-[sign antc',sign cons'], c<-[a,b]])
>      where antc' = subst (m,f) antc
>            cons' = subst (m,f) cons
>   subst (m,f) (Sg p rule expla sgn nr pn signal)
>    = Sg p r' expla (sign r') nr pn signal
>      where r'= subst (m,f) rule
>   subst (m,f) (Gc pos m' expr cpu sgn nr pn)
>    = Gc pos m' expr' cpu (sign expr') nr pn
>      where expr' = subst (m,f) expr

>{-  instance Show Context where
>   showsPrec p context  -- (Ctx nm on isa world pats rs ds cs ks os pops)
>    = showString ("CONTEXT "++nm++
>                  (if null (extends context) then "" else " EXTENDS "++chain ", " (extends context))++"\n"++
>                  chain "\n\n" (map show (patterns context))++"\n"++
>                  chain "\n" (map show (rules context))++"\n"++
>                  chain "\n" (map show (declarations context))++"\n"++
>                  chain "\n" (map show (conceptDefs context))++"\n"++
>                  chain "\n" (map show (keyDefs context))++"\n"++
>                  chain "\n" (map show (objectDefs context))++"\n"++
>                  chain "\n" (map show (populations context))++"\nENDCONTEXT" ) -}

The function showHS prints structures as haskell source, which is intended for testing.

>  class ShowADL a where
>   showADL :: a -> String

>  class ShowHS a where
>   showHSname :: a -> String
>   showHS     :: String -> a -> String

>  instance ShowHS Prop where
>   showHSname p = error ("(module CC_aux) should not showHS the name of multiplicities (Prop): "++showHS "" p)
>   showHS indent Uni = "Uni"
>   showHS indent Inj = "Inj"
>   showHS indent Sur = "Sur"
>   showHS indent Tot = "Tot"
>   showHS indent Sym = "Sym"
>   showHS indent Asy = "Asy"
>   showHS indent Trn = "Trn"
>   showHS indent Rfx = "Rfx"
>   showHS indent Aut = "AUT"

>  instance ShowADL Prop where
>   showADL Uni = "UNI"
>   showADL Inj = "INJ"
>   showADL Sur = "SUR"
>   showADL Tot = "TOT"
>   showADL Sym = "SYM"
>   showADL Asy = "ASY"
>   showADL Trn = "TRN"
>   showADL Rfx = "RFX"
>   showADL Aut = "AUT"

>  instance ShowHS a => ShowHS [a] where
>   showHSname _ = error ("(module CC_aux) lists are anonymous with respect to showHS.")
>   showHS indent = chain "\n".map (showHS indent)

>  instance ShowADL a => ShowADL [a] where
>   showADL = chain "\n".map showADL

>  instance ShowHS Architecture where
>   showHSname _ = error ("(module CC_aux) an architecture is anonymous with respect to showHS.")
>   showHS indent arch = concat (map (showHS indent) (archContexts arch))

>  instance ShowADL Architecture where
>   showADL arch = chain "\n" (map showADL (archContexts arch))

>  instance ShowHS Context where
>-- TODO: showHS should generate valid Haskell code for the entire pattern. Right now, it doesn't
>   showHSname context = "ctx_"++haskellIdentifier (name context)
>   showHS indent context
>    = "Ctx "++show (name context)++"   -- (Ctx nm on isa world pats rs ds cs ks os pops)"++
>      indent++"       "++(if null on   then "[]" else showL [show x|x<-on])++
>      (if null on   then " " else indent++"       ")++"isa [ {- world is left empty -} ]"++
>      (if null pats then " []" else indent++"       "++showL [showHSname p++" gE"| p<-pats])++
>      (if null rs   then " []" else indent++"       "++showL [showHSname r       | r<-rs  ])++
>      (if null ds   then " []" else indent++"       "++showL [showHSname d       | d<-ds  ])++
>      (if null cs   then " []" else indent++"       "++showL [showHSname c       | c<-cs  ])++
>      (if null ks   then " []" else indent++"       "++showL ["key_"++name k     | k<-ks  ])++
>      (if null os   then " []" else indent++"       "++showL [showHSname o       | o<-os  ])++
>      (if null pops then " []" else indent++"       "++showL [showHSname p       | p<-pops])++
>      indent++"where"++
>      indent++" isa = "++showHS (indent++"       ") (isa context)++
>      indent++" gE  = genEq (typology isa)"++
>      (if null on   then "" else indent++" on  = "++showL [show x|x<-on]++"\n")++
>      (if null os   then "" else concat [indent++" "++showHSname o++" = "++showHS "" o| o<-os]++"\n")++
>      (if null rs   then "" else concat [indent++" "++showHSname r++" = "++showHS "" r| r<-rs]++"\n")++
>      (if null ds   then "" else concat [indent++" "++showHSname d++" = "++showHS "" d| d<-ds]++"\n")++
>      (if null pops then "" else concat [indent++" "++showHSname p++indent++"  = "++showHS (indent++"    ") p  |p<-populations context]++"\n")++
>      (if null cs   then "" else concat [indent++" "++showHSname c++" = "++showHS "" c| c<-cs]++"\n")++
>      (if null ks   then "" else concat [indent++" "++showHSname k++" = "++showHS "" k| k<-ks]++"\n")
>   -- patterns will be shown in  (showHS indent Fspec)
>      where pats = patterns context
>            rs   = rules context
>            ds   = declarations context>-declarations (patterns context)
>            cs   = conceptDefs context>-conceptDefs (patterns context)
>            ks   = keyDefs context
>            os   = attributes context
>            pops = populations context
>            on   = extends context

>  instance ShowADL Context where
>   showADL context
>    = "CONTEXT " ++name context
>      ++ (if null (extends context)     then "" else "EXTENDS "++chain ", "   (extends context)                   ++ "\n")
>      ++ (if null (attributes context)  then "" else "\n"      ++chain "\n\n" (map showADL (attributes context))  ++ "\n")
>      ++ (if null cdefs                 then "" else "\n"      ++chain "\n"   (map showADL cdefs)                 ++ "\n")
>      ++ (if null decls                 then "" else "\n"      ++chain "\n"   (map showADL decls)                 ++ "\n")
>      ++ (if null (keyDefs context)     then "" else "\n"      ++chain "\n"   (map showADL (keyDefs context))     ++ "\n")
>      ++ (if null (patterns context)    then "" else "\n"      ++chain "\n\n" (map showADL (patterns context))    ++ "\n")
>      ++ (if null (populations context) then "" else "\n"      ++chain "\n\n" (map showADL (populations context)) ++ "\n")
>      ++ "\n\nENDCONTEXT"
>      where decls = declarations context>-declarations (patterns context)
>            cdefs = conceptDefs context>-conceptDefs (patterns context)

>  instance ShowHS Pattern where
>-- TODO: showHS should generate valid Haskell code for the entire pattern. Right now, it doesn't
>   showHSname pat = "pat_"++haskellIdentifier (name pat)
>   showHS indent pat@(Pat nm rs gen pss cs ks)
>    = "Pat "++show (name pat)++
>      (if null (declaredRules pat) then " []" else indent++"    [" ++chain          "    , "  [showHSname r              | r<-declaredRules pat] ++            "]")++
>      (if null gen                 then " []" else indent++"    [ "++chain (indent++"    , ") [showHS (indent++"     ") g| g<-gen              ] ++indent++"    ]")++
>      (if null (declarations pat)  then " []" else indent++"    [" ++chain          "    , "  [showHSname d              | d<-declarations  pat] ++            "]")++
>      (if null (conceptDefs pat)   then " []" else indent++"    [" ++chain          "    , "  [showHSname c              | c<-conceptDefs   pat] ++            "]")++
>      (if null ks                  then " []" else indent++"    [ "++chain (indent++"    , ") [showHS (indent++"     ") k| k<-ks               ] ++indent++"    ]")++
>      indent++"where"++
>      (if null (declarations pat)  then "" else concat [indent++" "++showHSname d ++" = "++ showHS (indent++"   ") d |d <-declarations  pat] )++
>      (if null (declaredRules pat) then "" else concat [indent++" "++showHSname r ++" = "++ showHS (indent++"   ") r |r <-declaredRules pat] )++
>      (if null (conceptDefs pat)   then "" else concat [indent++" "++showHSname cd++" = "++ showHS (indent++"   ") cd|cd<-conceptDefs   pat] )++
>      (if null ks                  then "" else concat [indent++" "++showHSname k ++" = "++ showHS (indent++"   ") k |k <-ks               ] )

>  instance ShowADL Pattern where
>   showADL pat@(Pat nm rs gen pss cs ks)
>    = "PATTERN " ++ name pat 
>      ++ (if null (conceptDefs pat)     then "" else "\n  " ++chain "\n  "   (map showADL (conceptDefs pat))   ++ "\n")
>      ++ (if null gen                   then "" else "\n  " ++chain "\n  "   (map showADL gen)                 ++ "\n")
>      ++ (if null (keyDefs pat)         then "" else "\n  " ++chain "\n  "   (map showADL (keyDefs pat))       ++ "\n")
>      ++ (if null (declarations pat)    then "" else "\n  " ++chain "\n  "   (map showADL (declarations pat))  ++ "\n")
>      ++ (if null (declaredRules pat)   then "" else "\n  " ++chain "\n  "   (map showADL (declaredRules pat)) ++ "\n")
>      ++ "ENDPATTERN"

>  instance ShowHS Population where
>   showHSname (Popu m ps) = "pop_"++haskellIdentifier (name m++name (source m)++name (target m))
>   showHS indent p@(Popu m ps)
>    = "Popu ("++showHS "" m++")"++indent++"     [ "++chain (indent++"     , ") (map show ps)++indent++"     ]"

>  instance ShowADL Population where
>   showADL (Popu m ps)
>    = nlIndent++"pop_"++name m++name (source m)++name (target m)++nlIndent++" = [ "++chain (nlIndent'++"; ") (map show ps)++nlIndent'++"]"
>      where nlIndent = "\n      "; nlIndent' = nlIndent++"    "

>  instance ShowHS Rule where
>   showHSname r = "rule"++show (nr r)
>   showHS indent r@(Ru 'A' _ pos cons cpu expla sgn nr pn)
>    = chain " " ["Ru","'A'",undef,"("++showHS "" pos++")","("++showHS "" (consequent r)++")",showL (map (showHS "") cpu),show(explain r),showSgn sgn,show nr,show pn]
>      where undef = "(let undef = undef in error \"Fatal: antecedent is not defined in an 'A' rule\")"
>   showHS indent r@(Ru c antc pos cons cpu expla sgn nr pn)
>    = chain " " ["Ru","'"++[c]++"'","("++showHS "" antc++")","("++showHS "" pos++")","("++showHS "" (consequent r)++")","["++chain "," (map (showHS "") cpu)++"]",show(explain r),showSgn sgn,show nr,show pn]
>   showHS indent r@(Sg pos rule expla sgn nr pn signal)
>    = chain " " ["Sg","("++showHS "" pos++")","("++showHS "" rule++")",show expla,showSgn sgn,show nr,show pn,show signal]
>   showHS indent r@(Gc pos m expr cpu sgn nr pn)
>    = chain " " ["Gc","("++showHS "" pos++")","("++showHS "" m++")","("++showHS "" (consequent r)++")","["++chain "," (map (showHS "") cpu)++"]",showSgn sgn,show nr,show pn]
>   showHS indent r = ""

>  instance ShowADL Rule where
>   showADL r@(Sg p rule expla sgn nr pn signal) = "SIGNAL "++name signal++" ON "++ showADL rule
>   showADL r@(Fr _ d expr _) = showADL d ++ "\n" ++ show (name d)++" = "++showADL expr
>   showADL r
>    | ruleType r=='A' = "ALWAYS "++showADL (consequent r)++
>                        if null (cpu r) then "" else " COMPUTING " ++ show (cpu r)
>    | ruleType r=='I' = showADL (antecedent r)++" |- "++showADL (consequent r)++
>                        if null (cpu r) then "" else " COMPUTING " ++ show (cpu r)
>    | ruleType r=='E' = showADL (antecedent r)++" = "++showADL (consequent r)++
>                        if null (cpu r) then "" else " COMPUTING " ++ show (cpu r)
>    | otherwise       = "GLUE "++showADL (antecedent r)++" = "++showADL (consequent r)++
>                        if null (cpu r) then "" else " COMPUTING " ++ show (cpu r)

>  instance ShowHS Expression where
>   showHSname e = error ("(module CC_aux) an expression is anonymous with respect to showHS. Detected at: "++ showADL e)
>   showHS indent (Tm m)   = "Tm ("++showHS "" m++") "
>   showHS indent (Tc f)   = showHS indent f
>   showHS indent (F [])   = "F [] <Id>"
>   showHS indent (Fd [])  = "Fd [] <nId>"
>   showHS indent (Fu [])  = "Fu [] <False>"
>   showHS indent (Fi [])  = "Fi [] <True>"
>   showHS indent (F [t])  = "F ["++showHS (indent++"   ") t++"]"
>   showHS indent (F ts)   = "F [ "++chain (indent++"  , ") [showHS (indent++"    ") t| t<-ts]++indent++"  ]"
>   showHS indent (Fd [t]) = "Fd ["++showHS (indent++"    ") t++"]"
>   showHS indent (Fd ts)  = "Fd [ "++chain (indent++"   , ") [showHS (indent++"     ") t| t<-ts]++indent++"   ]"
>   showHS indent (Fu [f]) = "Fu ["++showHS (indent++"    ") f++"]"
>   showHS indent (Fu fs)  = "Fu [ "++chain (indent++"   , ") [showHS (indent++"     ") f| f<-fs]++indent++"   ]"
>   showHS indent (Fi [f]) = "Fi ["++showHS (indent++"    ") f++"]"
>   showHS indent (Fi fs)  = "Fi [ "++chain (indent++"   , ") [showHS (indent++"     ") f| f<-fs]++indent++"   ]"
>   showHS indent (K0 e)   = "K0 ("++showHS (indent++"    ") e++") "
>   showHS indent (K1 e)   = "K1 ("++showHS (indent++"    ") e++") "
>   showHS indent (Cp e)   = "Cp ("++showHS (indent++"    ") e++") "

>  instance ShowADL Expression where
>   showADL e = show e


>  isFactor (Fu fs) = True
>  isFactor (Fi fs) = True
>  isFactor _ = False

>  showS m = name m++"["++show (source m)++","++show (target m)++"]"
>  showSgn (a,b) = "("++showHS "" a++","++showHS "" b++")"
>  showSign cs = "["++chain "*" (map name cs)++"]"
>  showFullRelName m = rEncode (name m++name (source m)++name (target m))

>  instance ShowHS a => ShowHS (Inheritance a) where
>   showHSname i = error ("(module CC_aux) every inheritance is anonymous with respect to showHS. Detected at: "++ showHS "" i)
>   showHS indent (Isa ts cs) = "Isa "++showL ["("++showHS "" g++","++showHS "" s++")"|(g,s)<-ts] ++indent++"    "++ showL (map (showHS "") cs)

>  instance ShowADL a => ShowADL (Inheritance a) where
>   showADL (Isa ts cs) = ""

>  instance ShowHS Concept where
>   showHSname c = error ("(module CC_aux: showHS) Illegal call to showHSname ("++name c++"). A concept gets no definition in Haskell code.")
>   showHS indent c |  isAnything c = name c
>                   |  isNothing  c = name c
>                   |  singleton  c = "S"
>                   |  otherwise    = "C "++show (name c) ++ " gE []"    -- contents not shown. 

> --XXXVERVANGEN  showHS indent Anything = "Anything"
> --  showHS indent NOthing  = "NOthing"
> --  showHS indent  c       = if singleton c
> --                           then "S"
> --                           else "C "++show (name c) ++ " gE []"    -- contents not shown.

>  instance ShowADL Concept where
>   showADL c = show (name c)

>  instance ShowHS ConceptDef where
>   showHSname cd = "cDef_"++haskellIdentifier (name cd)
>   showHS indent cd
>    = " Cd ("++showHS "" (cdpos cd)++") "++show (name cd)++" "++show (cddef cd)++(if null (cdref cd) then "" else " "++show (cdref cd))

>  instance ShowADL ConceptDef where
>   showADL cd
>    = "\n  CONCEPT "++show (name cd)++" "++show (cddef cd)++" "++(if null (cdref cd) then "" else show (cdref cd))

>  instance ShowHS KeyDef where
>   showHSname kd = "kDef_"++haskellIdentifier (name kd)
>   showHS indent kd@(Kd pos lbl ctx ats)
>    = "Kd ("++showHS "" pos++") "++show lbl++" ("++showHS "" ctx++")"
>      ++indent++"[ "++chain (indent++", ") [showHS (indent++"  ") a|a<-ats]++indent++"]"

>  instance ShowADL KeyDef where
>   showADL (Kd pos lbl ctx ats)
>    = "KEY "++lbl++">"++name (target ctx)++"("++chain "," (map showADL ats)++")"

>  instance ShowHS ObjectDef where
>   showHSname obj = "oDef_"++haskellIdentifier (name obj)
>   showHS indent obj 
>    = "Obj "++show (name obj)++" ("++showHS "" (objpos obj)++")"++ctxStr++
>      (if null (objats obj)
>       then " []"
>       else indent++"    [ "++chain (indent++"    , ") (map (showHS (indent++"      ")) (objats obj))++indent++"    ]")
>    where ctxStr | length (morlist (objctx obj)) >1 = indent++"    ("++showHS (indent++"     ") (objctx obj)++indent++"    )"
>                 | otherwise               = indent++"    ("++showHS "" (objctx obj)++")"

>  instance ShowADL ObjectDef where
>   showADL obj = "OBJECT "++name obj++" : "++(showADL (objctx obj))++" = ["++chain ", " (map show (objats obj))++"]"

>  instance ShowHS Declaration where
>   showHSname d = "rel_"++haskellIdentifier (name d++name (source d)++name (target d))
>   showHS indent d@(Sgn nm a b props prL prM prR cs expla pos nr sig)
>    = "Sgn "++show nm++
>              " ("++showHS "" a++") ("++showHS "" b++") "
>              ++showL(map (showHS "") props)++" "
>              ++show prL++" "++show prM++" "++show prR++" "
>              ++(if null cs then "[]" else "[[\"Content not shown\",\"\"]]")
>              ++" "++show expla
>              ++" ("++showHS "" pos++")"
>              ++" "++show nr
>              ++" "++show sig
>   showHS indent (Isn g s)
>    = "Isn ("++showHS "" g++") ("++showHS "" s++")"
>   showHS indent (Iscompl g s)
>    = "Iscompl ("++showHS "" g++") ("++showHS "" s++")"
>   showHS indent (Vs g s)
>    = "Vs ("++showHS "" g++") ("++showHS "" s++")"

>  instance ShowADL Declaration where
>   showADL decl@(Sgn nm a b props prL prM prR cs expla _ _ sig)
>    = if isSignal decl then "SIGNAL "++nm++" ON ("++name a++" * "++name b++")" else
>      nm++" :: "++name a++" * "++name b++
>      (if null props then "" else showL(map showADL props))++
>      (if null(prL++prM++prR) then "" else " PRAGMA "++chain " " (map show [prL,prM,prR]))++
>      (if null expla then "" else " EXPLANATION \""++expla++"\"")
>      ++"."
>   showADL (Isn g s)
>    = "I["++show (name g)++(if g==s then "" else "*"++show (name s))++"]"
>   showADL (Iscompl g s)
>    = "-I["++show (name g)++(if g==s then "" else "*"++show (name s))++"]"
>   showADL (Vs g s)
>    = "V["++show (name g)++(if g==s then "" else "*"++show (name s))++"]"

>  instance ShowHS Morphism where
>   showHSname m = error ("(module CC_aux: showHS) Illegal call to showHSname ("++showADL m++"). A morphism gets no definition in Haskell code.")
>   showHS indent (Mph nm pos atts sgn@(a,b) yin d)
>    = "Mph "++show nm++" ("++showHS "" pos++") "++showL(map (showHS "") atts)++" "++showSgn sgn++" "++show yin++" ("++showHS "" d++")"
>   showHS indent (I atts g s yin)
>    = "I"++" "++showL(map (showHS "") atts)++" ("++showHS "" g++") ("++showHS "" s++") "++show yin
>   showHS indent (V atts sgn)
>    = "V"++" "++showL(map (showHS "") atts)++" ("++showSgn sgn++")"
>   showHS indent (Mp1 str sgn)
>    = "Mp1"++" "++show str++" ("++showHS "" sgn++")"

>  instance ShowADL Morphism where
>   showADL m@(Mph nm pos atts sgn@(a,b) yin s)
>    = ({- if take 5 nm=="Clos_" then drop 5 nm++"*" else -} nm)++
>      (if null atts
>           then (if yin && sign m==sign s || not yin && sign m==sign (flp s) then "" else showSign [a,b])
>           else showSign atts)++
>      if yin then "" else "~"
>   showADL (I atts g s yin)
>    = "I"++if null atts then "" else showSign atts++if g==s then "" else if yin then "" else "~"
>   showADL (V atts (a,b))
>    = "V"++if null atts then "" else showSign atts
>   showADL (Mp1 str sgn)
>    = "'"++str++"'"++(showSign [sgn])

>  instance ShowHS Gen where
>   showHSname g = error ("(module CC_aux: showHS) Illegal call to showHSname ("++showADL g++"). A GEN statement gets no definition in Haskell code.")
>   showHS indent (G pos g s)  = "G ("++showHS "" pos++") ("++showHS "" s++") ("++showHS "" g++")"

>  instance ShowADL Gen where
>   showADL (G pos g s) = "GEN "++showADL s++" ISA "++show g

>  instance ShowHS FilePos where
>   showHSname p = error ("(module CC_aux: showHS) Illegal call to showHSname ("++showHS "" p++"). A position gets no definition in Haskell code.")
>   showHS indent (FilePos (fn,Pos l c,sym))
>     = "FilePos ("++show fn++",Pos "++show l++" "++show c++","++show sym++")"

  instance Show Pattern where
   showsPrec p (Pat nm rs gen pms cs ks)
    = showString ("PATTERN "++nm++"\n"++
                  pr gen++['\n'| not (null gen)]++
                  pr pms++['\n'| not (null pms)]++
                  pr cs++['\n'| not (null cs)]++
                  pr ks++['\n'| not (null ks)]++
                  pr rs++"\nENDPATTERN" )
      where pr xs = chain "\n" [" "++show x| x<-xs]

  instance Show Rule where
   showsPrec p r
    | isSignal r      = let (Sg p rule expla sgn nr pn signal) in
                        "SIGNAL "++name signal++" ON " show rule
    | ruleType r=='A' = showString ("RULE "++show (consequent r)++"\n  EXPLANATION "++explain r)
    | ruleType r=='I' = showString ("RULE "++show (antecedent r)++" |- "++show (consequent r)++"\n  EXPLANATION "++explain r++"\n("++show (pos r)++")")
    | ruleType r=='E' = showString ("RULE "++show (antecedent r)++" = " ++show (consequent r)++"\n  EXPLANATION "++explain r++"\n("++show (pos r)++")")
    | otherwise       = showString ("GLUE "++show (antecedent r)++" = "++show (consequent r)++"\n("++show (pos r)++")")


The following shSigns is intendes for error messages

>  shSigns [(a,b)] = "["++show a++"*"++show b++"]"
>  shSigns ss = commaEng "or" ["["++show a++"*"++show b++"]"|(a,b)<-ss]
   
This show is used in error messages. It should therefore not display the morphism's type



>  class Populated a where
>   contents  :: a -> [Paire]

>  instance Populated Morphism where
>   contents m@(Mph _ _ _ _ False _) = map reverse (contents (flp m))
>   contents m = contents (declaration m)

>  instance Populated Declaration where
>   contents (Sgn _ _ _ _ _ _ _ cs _ _ _ _) = cs
>   contents (Isn g s)                      = [[o,o] | o<-conts s]
>   contents (Iscompl g s)                  = [[o,o']| o<-conts s,o'<-conts s,o/=o']
>   contents (Vs g s)                       = [[o,o']| o<-conts s,o'<-conts s]

>  instance Populated Expression where
>   contents (Tm m)        = contents m
>   contents (Tc f)        = contents f
>   contents f@(F ts)
>    | idsOnly ts
>       = if not (source f `order` target f) then error ("(module CC_aux) Fatal: no order in "++showHS "" f) else
>                            [[e,e]|e<-os]
>    | otherwise
>       = if null css then error ("(module CC_aux) Fatal: no terms in F "++showHS "" ts) else
>                            foldr1 join css
>                            where os = conts (source f `lub` target f)
>                                  css = [contents t|t<-ts, not (idsOnly t)]
>   contents f@(Fd ts)
>     = if null ts then error ("(module CC_aux) Fatal: no terms in Fd "++showHS "" ts) else joinD ts
>   contents (Fu fs) = if null fs then [] else
>                      (foldr1 uni .map contents) fs
>   contents (Fi fs) = if null fs then [] else
>                      (foldr1 isc .map contents) fs
>   contents (K0 e)  = clos1 (contents e) `uni` [[c,c]|c <-conts (source e `lub` target e)]
>   contents (K1 e)  = clos1 (contents e)
>   contents (Cp (Cp e)) = contents e
>   contents (Cp e)  = [[a,b]| [a,b]<-diag [] (conts (source e)) [] (conts (target e)), not ([a,b] `elem` contents e)]

>  join::[Paire]->[Paire]->[Paire]
>  join a b = merge ((sort' (trg.head).eqCl trg) a)
>                   ((sort' (src.head).eqCl src) b)
>             where merge (xs:xss) (ys:yss)
>                    | trg (head xs)<src (head ys) = merge xss (ys:yss)
>                    | trg (head xs)>src (head ys) = merge (xs:xss) yss
>                    | otherwise = [[x,y]|[x,i]<-xs,[j,y]<-ys]++ merge xss yss
>                   merge _ _ = []
>  joinD :: [Expression] -> [Paire]
>  joinD [s]      = contents s
>  joinD (r:s:ts) = [ [head (head rc),last (head sc)]
>                   | rc<-eqCl head (contents r)
>                   , sc<-eqCl last (joinD (s:ts))
>                   , null (conts (target r `glb` source s) >-(map last rc `uni` map head sc))
>                   ]
>  makeConceptSpace :: GenR -> [Morphism] -> Concepts
>  makeConceptSpace gE morphisms
>   = [ upd (fst (head raw)) (sord (concat (map snd raw)))
>     | raw <- eqCl fst [(c,os)| m@(Mph nm pos atts (s,t) yin sgn@(Sgn _ s' t' _ _ _ _ ds _ _ _ _)) <- morphisms
>                              , (c,os) <- [(s',dom sgn),(t',cod sgn)]
>                       ]
>     ] where
>        upd c os | isC c     = c{cptos=os}
>                 | otherwise = c
>  -- XXXVERVANGEN:      upd (C c gEq os) os' = C c gEq os'
>  --                    upd c  os = c

>  class Key a where
>   keys :: a->[(Concept,String,[ObjectDef])]

>  instance Key Context where
>   keys context
>    = ( concat [keys p| p<-patterns context] ++
>        [(target ctx,lbl,ats)|Kd pos lbl ctx ats<-keyDefs context]
>      )

>  instance Key Pattern where
>   keys pat = [(target ctx,lbl,ats)|Kd pos lbl ctx ats<-keyDefs pat]

>  instance Key KeyDef where
>   keys (Kd pos lbl ctx ats) = [(target ctx,lbl,ats)]

   instance Key ObjectDef where
    keys obj = [(target (objctx obj),name obj,objats obj)]

The story with objects:
Each object has a type, which is a concept.
It can contain objects, which are called the attributes of the object (e.g. a Person contains name, address, city)
Each attribute is bound to its object through the context expression of the attribute.
So if p is a Person with name Peter, and the attribute name has context expression ctx, then p ctx Peter.

>  class Object a where
>   concept :: a -> Concept                 -- the type of the object
>   attributes :: a -> [ObjectDef]          -- the objects defined within the object
>   ctx :: a -> Expression                  -- the context expression
>   populations :: a -> [Population]        -- the populations in the object (for now: use for contexts only)
>   extends :: a -> [String]                -- the objects of which this is is extension (for now: use for contexts only)
>   extends _ = []                          -- empty unless specified otherwise.

>  instance Object Context where
>   concept _    = cptAnything
>   attributes ctx = ctxos ctx
>   ctx        _ = error ("Cannot evaluate the context expression of the current context (yet)")
>   populations  ctx = ctxpops ctx
>   extends ctx = ctxon ctx

>  instance Object ObjectDef where
>   concept obj = target (objctx obj)
>   attributes obj = objats obj
>   ctx obj = objctx obj
>   populations  _ = []

The following definition is used to compute whether a concept may display its internal code.
This may be done when there are no keys and no instances for this particular concept.

>  displayInternalCode context c
>   = null[o| o<-attributes context, c==concept o]

TODO: transform makeConceptSpace to makeConceptSpace :: [Declaration] -> Concepts

>  class Pop a where
>   put_gE     :: GenR -> Concepts  -> a -> a
>   specialize :: (Concept,Concept) -> a -> a
>   update     :: [Declaration] -> a -> a
>   update ss c = c

>  instance Pop Concept where
>--XXXisVERVANGEN   put_gE gE cs c     = h (head ([c'|c'<-cs, c==c']++[c]))
>--                               where h (C c gEq os) = C c gE os
>--                                    h x = x
>   put_gE gE cs c = h (head ([c'|c'<-cs, c==c']++[c]))
>           where h x | isC x = x{cptgE = gE}
>                     | otherwise = x

>   specialize (a,b) c = if length (eqClass order [a,b,c])>1 then error ("(module CC_aux) Fatal: specialize 1 ("++show a++","++show b++") "++showHS "" c) else
>                        (a `glb` b) `lub` c

>  instance Pop KeyDef where
>   put_gE gE cs (Kd pos lbl ctx ats) = Kd pos lbl (put_gE gE cs ctx) [put_gE gE cs a| a<-ats]
>   update ss    (Kd pos lbl ctx ats) = Kd pos lbl (update ss    ctx) [update ss    a| a<-ats]
>   specialize t (Kd pos lbl ctx ats) = Kd pos lbl (specialize t ctx) [specialize t a| a<-ats]

>  instance Pop ObjectDef where
>   put_gE gE cs obj = obj { objctx = (put_gE gE cs (objctx obj))
>                          , objats = [put_gE gE cs a| a<-objats obj]
>                          }
>   update ss    obj = obj { objctx = (update ss    (objctx obj))
>                          , objats = [update ss    a| a<-objats obj]
>                          }
>   specialize t obj = obj { objctx = (specialize t (objctx obj))
>                          , objats = [specialize t a| a<-objats obj]
>                          }


>  instance (Pop a,Pop b) => Pop (a,b) where
>   put_gE gE cs (x,y) = (put_gE gE cs x, put_gE gE cs y)
>   update ss    (x,y) = (update ss    x, update ss    y)
>   specialize t (x,y) = (specialize t x, specialize t y)

Om een of andere reden stond hier eerder:
  instance (Show a,Pop a) => Pop (a,a) where
   put_gE gE cs (s,t) = (put_gE gE cs s,put_gE gE cs t)
    specialize (a,b) (s,t) = if not (a `order` s && b `order` t) then error ("(module CC_aux) Fatal: specialize 2 ("++show a++","++show b++") ("++show s++","++show t++")") else
                             (a `lub` s, b `lub` t)

>  instance Pop Gen where
>   put_gE gE cs (G pos g s) = G pos (put_gE gE cs g) (put_gE gE cs s)
>   update ss    (G pos g s) = G pos (update ss g)    (update ss s)
>   specialize t (G pos g s) = G pos (specialize t g) (specialize t s)

>  instance Pop Context where
>--XXXisVERVANGEN   put_gE gE cs (Ctx nm on isa world pats rs dw cs' ks os pops)
>--                       = Ctx nm on isa (map (mapCl (put_gE gE cs)) world) (map (put_gE gE cs) pats) (map (put_gE gE cs) rs) (map (put_gE gE cs) dw) cs' (map (put_gE gE cs) ks) (map (put_gE gE cs) os) pops
>     put_gE gE cs context
>                 = context { wrld    = map (mapCl (put_gE gE cs)) (wrld context)
>                           , ctxpats = map (put_gE gE cs) (ctxpats context)
>                           , ctxrs   = map (put_gE gE cs) (ctxrs context)
>                           , ctxds   = map (put_gE gE cs) (ctxds context)
>                           , ctxks   = map (put_gE gE cs) (ctxks context)
>                           , ctxos   = map (put_gE gE cs) (ctxos context)
>                           }
>--XXXisVERVANGEN   update ss    (Ctx nm on isa world pats rs ds cs  ks os pops)
>--                       = Ctx nm on isa world (map (update ss) pats) (map (update ss) rs) (map (update ss) ds) cs (map (update ss) ks) (map (update ss) os) pops
>     update ss context
>                 = context { ctxpats = map (update ss) (ctxpats context)
>                           , ctxrs   = map (update ss) (ctxrs context)
>                           , ctxds   = map (update ss) (ctxds context)
>                           , ctxks   = map (update ss) (ctxks context)
>                           , ctxos   = map (update ss) (ctxos context)
>                           }
>--XXXisVERVANGEN   specialize t (Ctx nm on isa world pats rs ds cs  ks os pops)
>--                       = Ctx nm on isa world (map (specialize t) pats) (map (specialize t) rs) (map (specialize t) ds) cs (map (specialize t) ks) (map (specialize t) os) pops
>     specialize t context
>                 = context { ctxpats = map (specialize t) (ctxpats context)
>                           , ctxrs   = map (specialize t) (ctxrs context)
>                           , ctxds   = map (specialize t) (ctxds context)
>                           , ctxks   = map (specialize t) (ctxks context)
>                           , ctxos   = map (specialize t) (ctxos context)
>                           }

>  instance Pop Pattern where
>   put_gE gE cs (Pat nm rs gen pms cs' ks) = Pat nm (map (put_gE gE cs) rs) (map (put_gE gE cs) gen) (map (put_gE gE cs) pms) cs' (map (put_gE gE cs) ks)
>   update ss (Pat nm rs gen pms cs ks)    = Pat nm (map (update ss) rs) (map (update ss) gen) (map (update ss) pms) cs (map (update ss) ks)
>   specialize t (Pat nm rs gen pms cs ks) = Pat nm (map (specialize t) rs) (map (specialize t) gen) (map (specialize t) pms) cs (map (specialize t) ks)

>  instance Pop Rule where
>   put_gE gE cs r@(Ru 'A' antc pos expr cpu expla sgn nr pn) = Ru 'A' (error ("(Module CC_aux:) illegal call to antecedent in put_gE cs ("++showADL r++")")) pos (put_gE gE cs expr) (map (put_gE gE cs) cpu) expla (put_gE gE cs sgn) nr pn
>   put_gE gE cs r@(Ru c antc pos cons cpu expla sgn nr pn)   = Ru c (put_gE gE cs antc) pos (put_gE gE cs (consequent r)) (map (put_gE gE cs) cpu) expla (put_gE gE cs sgn) nr pn
>   put_gE gE cs r@(Sg p rule expla sgn nr pn signal)         = Sg p (put_gE gE cs rule) expla (put_gE gE cs sgn) nr pn signal
>   put_gE gE cs r@(Gc pos m expr cpu sgn nr pn)              = Gc pos (put_gE gE cs m) (put_gE gE cs expr) (map (put_gE gE cs) cpu) (put_gE gE cs sgn) nr pn
>   put_gE gE cs r@(Fr t d expr pn)                           = Fr t (put_gE gE cs d) (put_gE gE cs expr) pn
>   update ss r@(Ru 'A' antc pos expr cpu expla sgn nr pn)    = Ru 'A' (error ("(Module CC_aux:) illegal call to antecedent in update ss ("++showADL r++")")) pos (update ss expr) (map (update ss) cpu) expla (update ss sgn) nr pn
>   update ss r@(Ru c antc pos cons cpu expla sgn nr pn)      = Ru c (update ss antc) pos (update ss cons) (map (update ss) cpu) expla (update ss sgn) nr pn
>   update ss r@(Sg p rule expla sgn nr pn signal)            = Sg p (update ss rule) expla (update ss sgn) nr pn signal
>   update ss r@(Gc pos m expr cpu sgn nr pn)                 = Gc pos (update ss m) (update ss expr) (map (update ss) cpu) (update ss sgn) nr pn
>   update ss r@(Fr t d expr pn)                              = Fr t (update ss d) (update ss expr) pn
>   specialize t r@(Ru 'A' antc pos expr cpu expla sgn nr pn) = Ru 'A' (error ("(Module CC_aux:) illegal call to antecedent in specialize t ("++showADL r++")")) pos (specialize t expr) (map (specialize t) cpu) expla (specialize t sgn) nr pn
>   specialize t r@(Ru c antc pos cons cpu expla sgn nr pn)   = Ru c (specialize t antc) pos (specialize t cons) (map (specialize t) cpu) expla (specialize t sgn) nr pn
>   specialize t r@(Sg p rule expla sgn nr pn signal)         = Sg p (specialize t rule) expla (specialize t sgn) nr pn signal
>   specialize t r@(Gc pos m expr cpu sgn nr pn)              = Gc pos (specialize t m) (specialize t expr) (map (specialize t) cpu) (specialize t sgn) nr pn
>   specialize t' r@(Fr t d expr pn)                          = Fr t (specialize t' d) (specialize t' expr) pn

>  single e x = length [m| m<-morlist e, m==x]==1

>  makeInline m | inline m = m
>  makeInline m = flp m

>  instance Pop Expression where
>   put_gE gE cs (Tm m)       = Tm (put_gE gE cs m)
>   put_gE gE cs (Tc f)       = Tc (put_gE gE cs f)
>   put_gE gE cs (F ts)       = F  (map (put_gE gE cs) ts)
>   put_gE gE cs (Fd ts)      = Fd (map (put_gE gE cs) ts)
>   put_gE gE cs (Fu fs)      = Fu (map (put_gE gE cs) fs)
>   put_gE gE cs (Fi fs)      = Fi (map (put_gE gE cs) fs)
>   put_gE gE cs (K0 e)       = K0 (put_gE gE cs e)
>   put_gE gE cs (K1 e)       = K1 (put_gE gE cs e)
>   put_gE gE cs (Cp e)       = Cp (put_gE gE cs e)

>   update ss (Tm m)            = Tm (update ss m)
>   update ss (Tc f)            = Tc (update ss f)
>   update ss (F ts)            = F  (map (update ss) ts)
>   update ss (Fd ts)           = Fd (map (update ss) ts)
>   update ss (Fu fs)           = Fu (map (update ss) fs)
>   update ss (Fi fs)           = Fi (map (update ss) fs)
>   update ss (K0 e)            = K0 (update ss e)
>   update ss (K1 e)            = K1 (update ss e)
>   update ss (Cp e)            = Cp (update ss e)

>   specialize t (Tm m)         = Tm (specialize t m)
>   specialize t (Tc f)         = Tc (specialize t f)
>   specialize t@(a,b) (F [])   = error ("(module CC_aux) specialize t@("++show a++","++show b++") (F [])")
>   specialize t@(a,b) (F [t']) = F [specialize t t']
>   specialize t@(a,b) (F ts)   = F ([specialize (a,target h) h]++init (tail ts)++[specialize (source l,b) l])
>                                 where h=head ts; l=last ts
>   specialize t@(a,b) (Fd [])   = error ("(module CC_aux) specialize t@("++show a++","++show b++") (Fd [])")
>   specialize t@(a,b) (Fd [t']) = Fd [specialize t t']
>   specialize t@(a,b) (Fd ts)   = Fd ([specialize (a,target h) h]++init (tail ts)++[specialize (source l,b) l])
>                                  where h=head ts; l=last ts
>   specialize t@(a,b) (Fu fs)  = Fu (map (specialize t) fs) 
>   specialize t@(a,b) (Fi fs)  = Fi (map (specialize t) fs) 
>   specialize t (K0 e)         = K0 (specialize t e)
>   specialize t (K1 e)         = K1 (specialize t e)
>   specialize t (Cp e)         = Cp (specialize t e)

  instance Pop a => Pop [a] where
   put_gE gE cs xs   = map (put_gE gE cs) xs
   update ss xs      = map (update ss) xs
   specialize t xs   = map (specialize t) xs

>  instance Pop Morphism where
>   put_gE gE cs (Mph nm p atts sgn yin s)       = Mph nm p (map (put_gE gE cs) atts) (put_gE gE cs sgn) yin (put_gE gE cs s)
>   put_gE gE cs (I atts g s yin)                = I (map (put_gE gE cs) atts) (put_gE gE cs g) (put_gE gE cs s) yin
>   put_gE gE cs (V atts (a,b))                  = V (map (put_gE gE cs) atts) (put_gE gE cs a, put_gE gE cs b)
>   update ss (Mph nm p atts sgn yin s)          = Mph nm p atts (update ss sgn) yin (update ss s)
>   update ss (I atts g s yin)                   = I (map (update ss) atts) (update ss g) (update ss s) yin
>   update ss (V atts (a,b))                     = V (map (update ss) atts) (update ss a, update ss b)
>   specialize t@(a,b) (Mph nm p atts sgn yin s) = Mph nm p (if null atts then [] else if yin then [a,b] else [b,a]) t yin (specialize t s)
>   specialize t@(a,b) (I atts g s yin)          = if yin then I atts b a yin else I atts a b yin
>   specialize t@(a,b) (V atts (a',b'))          = V atts (a,b)

>  instance Pop Declaration where
>   put_gE gE cs (Sgn nm a b props prL prM prR cs' expla pos nr sig)
>                                                = Sgn nm (put_gE gE cs a) (put_gE gE cs b) props prL prM prR cs' expla pos nr sig
>   put_gE gE cs (Isn g s)                       = Isn (put_gE gE cs g) (put_gE gE cs s)
>   put_gE gE cs (Iscompl g s)                   = Iscompl (put_gE gE cs g) (put_gE gE cs s)
>   put_gE gE cs (Vs g s)                        = Vs (put_gE gE cs g) (put_gE gE cs s)

>   update ss s@(Sgn _ _ _ _ _ _ _ _ _ _ _ _)    = head ([c|c<-ss, s==c]++[s])
>   update ss s                                  = s
>   specialize (x,y) (Sgn nm a b props prL prM prR ls expla pos nr sig)
>                                                = Sgn nm x y props prL prM prR [[d,e]|[d,e]<-ls,d `elem` conts a, e `elem` conts b] expla pos nr sig
>   specialize (x,y) sg@(Isn g s)                = if x <= y then Isn x y else error ("(module CC_aux) Fatal: specialize 7 "++show (x,y)++showHS "" s)
>   specialize (x,y) sg@(Iscompl g s)            = if x <= y then Iscompl x y else error ("(module CC_aux) Fatal: specialize 7 "++show (x,y)++showHS "" s)

>  idsOnly x = and [isIdent m| m<-mors x]

>  class Morphics a where
>   anything       :: a -> Bool

>  instance Morphics a => Morphics [a] where
>   anything xs = and (map anything xs)
>  instance Morphics Concept where
>   anything c = isAnything c

>  instance (Morphics a,Morphics b) => Morphics (a,b) where
>   anything (x,y) = anything x && anything y

>  instance Morphic Morphism where
>   source (Mph nm pos atts (a,b) _ s) = a
>   source (I atts g s yin)            = if yin then s else g
>   source (V atts (a,b))              = a
>   source (Mp1 _ s) = s
>   target (Mph nm pos atts (a,b) _ s) = b
>   target (I atts g s yin)            = if yin then g else s
>   target (V atts (a,b))              = b
>   target (Mp1 _ t) = t
>   sign   (Mph nm pos atts (a,b) _ s) = (a,b)
>   sign   (I atts g s yin)            = if yin then (s,g) else (g,s)
>   sign   (V atts (a,b))              = (a,b)
>   sign   (Mp1 _ s) = (s,s)
>   multiplicities (Mph nm pos atts (a,b) True  s) = multiplicities s
>   multiplicities (Mph nm pos atts (a,b) False s) = flipProps (multiplicities s)
>   multiplicities (V atts (a,b)) = [Tot,Sur]++[Inj| singleton a]++[Uni| singleton b]++
>                                   [Asy| a==b, singleton b]++[Sym|a==b]++[Rfx|a==b]++[Trn|a==b]
>   multiplicities (I _ _ _ _)    = [Inj,Sur,Uni,Tot,Sym,Asy,Trn,Rfx]
>   multiplicities (Mp1 _ _)      = [Inj,Uni,Sym,Asy,Trn]
>   flp (Mph nm pos atts (a,b) yin s) = Mph nm pos (reverse atts) (b,a) (not yin) s
>   flp (V atts (a,b))                = V (reverse atts) (b,a)
>   flp i                             = i
>   isIdent (I _ _ _ _)               = True                    -- > tells whether the argument is equivalent to I
>   isIdent (V _ (a,b))               = a==b && singleton a
>   isIdent _                         = False
>   isProp (I _ _ _ _)                = True                    -- > tells whether the argument is equivalent to I
>   isProp (V _ (a,b))                = a==b && singleton a
>   isProp (Mp1 _ _)                  = True
>   isProp m                          = null ([Asy,Sym]>-multiplicities m)
>   isNot m                           = isNot (declaration m)   -- > tells whether the argument is equivalent to I-
>   isMph (Mph _ _ _ _ _ _)           = True
>   isMph _                           = False
>   isTrue (V _ _)                    = True
>   isTrue (I _ a b _)                = singleton b
>   isTrue _                          = False
>   isFalse _                         = False
>   isSignal m                        = isSignal (declaration m)
>   typeUniq (Mph nm pos  []  (a,b) _ s) = typeUniq a && typeUniq b
>   typeUniq (Mph nm pos atts (a,b) _ s) = True
>   typeUniq (I  []  g s yin) = typeUniq g && typeUniq s
>   typeUniq (I atts g s yin) = True
>   typeUniq (V  []  (a,b)) = typeUniq a && typeUniq b
>   typeUniq (V atts (a,b)) = True

>  isSgn (Sgn _ _ _ _ _ _ _ _ _ _ _ _) = True
>  isSgn _ = False


>  instance Morphic Expression where
>   source (Tm m)          = source m
>   source (Tc f)          = source f
>   source (F  [])         = error ("(module CC_aux) Fatal: source (F [])")
>   source (F  ts)         = source (head ts)
>   source (Fd [])         = error ("(module CC_aux) Fatal: source (Fd [])")
>   source (Fd ts)         = source (head ts)
>   source (Fu [])         = error ("(module CC_aux) Fatal: source (Fu [])")
>   source (Fu fs)         = source (head fs) -- should be most generic of (map source fs) (TODO)
>   source (Fi [])         = error ("(module CC_aux) Fatal: source (Fi [])")
>   source (Fi fs)         = source (head fs) -- should be most specific of (map source fs) (TODO)
>   source (K0 e)          = source e
>   source (K1 e)          = source e
>   source (Cp e)          = source e

>   target (Tm m)          = target m
>   target (Tc f)          = target f
>   target (F  [])         = error ("(module CC_aux) Fatal: target (F [])")
>   target (F  ts)         = target (last ts)
>   target (Fd [])         = error ("(module CC_aux) Fatal: target (Fd [])")
>   target (Fd ts)         = target (last ts)
>   target (Fu [])         = error ("(module CC_aux) Fatal: target (Fu [])")
>   target (Fu fs)         = target (last fs) -- should be most generic of (map source fs) (TODO)
>   target (Fi [])         = error ("(module CC_aux) Fatal: target (Fi [])")
>   target (Fi fs)         = target (last fs) -- should be most specific of (map source fs) (TODO)
>   target (K0 e)          = target e
>   target (K1 e)          = target e
>   target (Cp e)          = target e

>   sign (Tm m)            = sign m
>   sign (Tc f)            = sign f
>   sign (F ts)            = if null ts then error ("(module CC_aux) Fatal: no terms in sign (F "++showHS "" ts++")") else
>                            foldr1 jnSign (map sign ts)
>                            where (s,t) `jnSign` (s',t') = (s,t')
>   sign (Fd ts)           = if null ts then error ("(module CC_aux) Fatal: no terms in sign (Fd "++showHS "" ts++")") else
>                            foldr1 jnSign (map sign ts)
>                            where (s,t) `jnSign` (s',t') = (s,t')
>   sign (Fu fs)           = if length (eqClass order (map sign fs))>1 then error ("(module CC_aux) Fatal: sign (Fu fs) not defined\nwith map sign fs="++show (map sign fs)) else
>                            if null fs then (cptAnything, cptAnything) else
>                            foldr1 lub (map sign fs)
>   sign (Fi fs)           = if length (eqClass order (map sign fs))>1 then error ("(module CC_aux) Fatal: sign (Fi fs) not defined\nwith map sign fs="++show (map sign fs)) else
>                            if null fs then (cptAnything, cptAnything) else
>                            foldr1 lub (map sign fs)
>   sign (K0 e)            = sign e
>   sign (K1 e)            = sign e
>   sign (Cp e)            = sign e

>   multiplicities (Tm m)  = multiplicities m
>   multiplicities (Tc f)  = multiplicities f
>   multiplicities (F ts)  = foldr isc [Uni,Tot,Sur,Inj] (map multiplicities ts) -- homogene multiplicities can be used and deduced by and from rules: many rules are multiplicities (TODO)
>   multiplicities (Fd ts) = [] -- many rules with Fd in it are multiplicities (TODO). Solve perhaps by defining relation a = (Fd ts)
>   multiplicities (Fu fs) = [] -- expr \/ a is Rfx if a is Rfx, is Tot if a is Tot (TODO), is Uni if both a and expr are uni
>   multiplicities (Fi fs) = [] -- expr /\ a is Asy if a is Asy, is Uni if a is Uni (TODO), is Uni if both a and expr are uni
>   multiplicities (K0 e)  = [Rfx,Trn] `uni` (multiplicities e>-[Uni,Inj])
>   multiplicities (K1 e)  = [Trn] `uni` (multiplicities e>-[Uni,Inj])
>   multiplicities (Cp e)  = [p|p<-multiplicities e, p==Sym]

>   flp (Tm m)             = Tm (flp m)
>   flp (Tc f)             = Tc (flp f)
>   flp (F ts)             = F (map flp (reverse ts))
>   flp (Fd ts)            = Fd (map flp (reverse ts))
>   flp (Fu fs)            = Fu (map flp fs)
>   flp (Fi fs)            = Fi (map flp fs)
>   flp (K0 e)             = K0 (flp e)
>   flp (K1 e)             = K1 (flp e)
>   flp (Cp e)             = Cp (flp e)

>   isNot (Tm m)           = isNot m    -- > tells whether the argument is equivalent to I-
>   isNot (Tc f)           = isNot f
>   isNot (F [t])          = isNot t
>   isNot (Fd [t])         = isNot t
>   isNot (Fu [f])         = isNot f
>   isNot (Fi [f])         = isNot f
>   isNot _                = False

>   typeUniq (Tm m)        = typeUniq m -- I don't understand what typeUniq does - Bas. (TODO)
>   typeUniq (Tc f)        = typeUniq f
>   typeUniq (F  ts)       = and (map typeUniq ts)
>   typeUniq (Fd ts)       = and (map typeUniq ts)
>   typeUniq (Fu fs)       = and (map typeUniq fs)
>   typeUniq (Fi fs)       = and (map typeUniq fs)
>   typeUniq (K0 e)        = typeUniq e
>   typeUniq (K1 e)        = typeUniq e
>   typeUniq (Cp e)        = typeUniq e

>   isMph (Tm m)           = isMph m
>   isMph (Tc f)           = isMph f
>   isMph (F [t])          = isMph t
>   isMph (Fd [t])         = isMph t
>   isMph (Fu [f])         = isMph f
>   isMph (Fi [f])         = isMph f
>   isMph (K0 e)           = isMph e
>   isMph (K1 e)           = isMph e
>   isMph _                = False

>   isTrue (F [])       = False -- > fout voor singletons (TODO)
>   isTrue (F ts)       | isFunction    (head ts) = (isTrue. F .tail) ts
>                       | isFlpFunction (last ts) = (isTrue. F .init) ts
>                       | otherwise               = isTrue (head ts) && isTrue (last ts) &&
>                                                  (not.isFalse. F .drop 1.init) ts  -- niet isFalse tussen head en last
>   isTrue (Fd as)      = isFalse (F (map notCp as))
>   isTrue (Fu fs)      = or  [isTrue f| f<-fs] -- isImin \/ isIdent => isTrue (onder andere => TODO)
>   isTrue (Fi fs)      = and [isTrue f| f<-fs]
>   isTrue (K0 e)       = isTrue (K1 e)
>   isTrue (K1 e)       = isTrue e -- als elk elem van (source e) in een cykel (in e) zit, dan ook is K0 ook True (TODO)
>   isTrue (Cp e)       = isFalse e
>   isTrue (Tm m)       = isTrue m
>   isTrue (Tc f)       = isTrue f

>   isFalse (F [])       = False -- > helemaal correct
>   isFalse (F ts)       = or (map isFalse ts) -- ook True als twee concepten op de ; niet aansluiten: a[A*B];b[C*D] met B/\C=0 (Dit wordt echter door de typechecker uitgesloten)
>   isFalse (Fd as)      = isTrue (F (map notCp as))
>   isFalse (Fu fs)      = and [isFalse f| f<-fs]
>   isFalse (Fi fs)      = or  [isFalse f| f<-fs] -- isImin /\ isIdent => isFalse (onder andere => TODO)
>   isFalse (K0 _)       = False
>   isFalse (K1 e)       = isFalse e
>   isFalse (Cp e)       = isTrue e
>   isFalse (Tm m)       = isFalse m
>   isFalse (Tc f)       = isFalse f

>   isSignal e           = False

-- > isIdent tells whether the argument is equivalent to I

>   isIdent (F ts)       = and [isIdent t| t<-ts]   -- > a;a~ = I bij bepaalde multipliciteiten (TODO)
>   isIdent (Fd [e])     = isIdent e
>   isIdent (Fd as)      = isImin (F (map Cp as))
>   isIdent (Fu fs)      = and [isIdent f| f<-fs] && not (null fs)   -- > fout voor singletons (TODO)
>   isIdent (Fi fs)      = and [isIdent f| f<-fs] && not (null fs)   -- > fout voor singletons (TODO)
>   isIdent (K0 e)       = isIdent e || isFalse e
>   isIdent (K1 e)       = isIdent e
>   isIdent (Cp e)       = isImin e
>   isIdent (Tm m)       = isIdent m
>   isIdent (Tc f)       = isIdent f

>   isProp (F ts)       = null ([Asy,Sym]>-multiplicities (F ts)) || and [null ([Asy,Sym]>-multiplicities t)| t<-ts]
>   isProp (Fd [e])     = isProp e
>   isProp (Fd as)      = isProp (F (map notCp as))
>   isProp (Fu fs)      = and [isProp f| f<-fs]
>   isProp (Fi fs)      = or [isProp f| f<-fs]
>   isProp (K0 e)       = isProp e
>   isProp (K1 e)       = isProp e
>   isProp (Cp e)       = isTrue e
>   isProp (Tm m)       = isProp m
>   isProp (Tc f)       = isProp f

>  fEmpty (F [])  = True
>  fEmpty (Fd []) = True
>  fEmpty (Fu []) = True
>  fEmpty (Fi []) = True
>  fEmpty     _   = False

>  oneMorphism (Tm _)    = True
>  oneMorphism (Tc f)    = oneMorphism f
>  oneMorphism (F [t])   = oneMorphism t
>  oneMorphism (F ts)    = False
>  oneMorphism (Fd [t])  = oneMorphism t
>  oneMorphism (Fd ts)   = False
>  oneMorphism (Fu [f])  = oneMorphism f
>  oneMorphism (Fu fs)   = False
>  oneMorphism (Fi [f])  = oneMorphism f
>  oneMorphism (Fi fs)   = False
>  oneMorphism (K0 e)    = oneMorphism e
>  oneMorphism (K1 e)    = oneMorphism e
>  oneMorphism (Cp e)    = oneMorphism e

>  isImin (Fd ts)  = and [isImin t| t<-ts]
>  isImin (Fi fs)  = and [isImin f| f<-fs] && not (null fs)
>  isImin (Fu fs)  = and [isImin f| f<-fs] && not (null fs)
>  isImin (F  [e]) = isImin e
>  isImin (Cp v)   = isIdent v
>  isImin _        = False -- (TODO)
  
>  instance Morphic Rule where
>   source r | ruleType r=='A' = fst (sign r)
>            | otherwise       = fst (sign r)
>   target r | ruleType r=='A' = snd (sign r)
>            | otherwise       = snd (sign r)
>   sign r   | ruleType r=='A' = sign (consequent r)
>            | otherwise       = if sign (antecedent r) `order` sign (consequent r) then sign (antecedent r) `lub` sign (consequent r) else
>                                error ("(module CC_aux) Fatal: incompatible signs in "++showHS "" r)
>   multiplicities r           = []
>   isMph r  | ruleType r=='A' = isMph (consequent r)
>            | otherwise       = False
>   flp r@(Ru 'A' antc pos expr cpu expla (a,b) nr pn) = Ru 'A' (error ("(Module CC_aux:) illegal call to antecedent in flp ("++showADL r++")")) pos (flp expr) cpu expla (b,a) nr pn
>   flp (Ru c antc pos cons cpu expla (a,b) nr pn)   = Ru c (flp antc) pos (flp cons) cpu expla (b,a) nr pn
> --  isIdent r = error ("(module CC_aux: isIdent) not applicable to any rule:\n "++showHS "" r)
>   typeUniq r | ruleType r=='A' = typeUniq (antecedent r)
>              | otherwise       = typeUniq (antecedent r) && typeUniq (consequent r)
>   isIdent r = isIdent (normExpr r)
>   isProp r = isProp (normExpr r)
>   isTrue r | ruleType r=='A'  = isTrue (consequent r)
>            | otherwise        = isTrue (consequent r) || isFalse (consequent r)
>   isFalse r| ruleType r=='A'  = isFalse (consequent r)
>            | otherwise        = isFalse (consequent r) && isTrue (consequent r)
>   isSignal (Sg _ _ _ _ _ _ _) = True
>   isSignal _                  = False
>   isNot r  | ruleType r=='A'  = isNot (consequent r)
>            | otherwise        = False  -- TODO: check correctness!

>  mkVar ex cs = mknew ex [[(toLower.head.(++"x").name) c]|c<-cs]
>   where
>    mknew ex [] = []
>    mknew ex (x:xs) | x `elem` ex = mknew ex ((x++"'"):xs)
>                    | otherwise = x: mknew (ex++[x]) xs

>  class Calc a where
>   limit     :: (Concept,Concept) -> a -> a
>   calc      :: a -> [Declaration] -> [Paire]

TODO: transform the following into  instance Collection Declaration where

> {-  e `elemSgn` s  = e `elem` contents s

>  jnMph :: Morphism -> Morphism -> Morphism
>  s `jnMph` t   | isIdent s = Mph nm' p' [] (sign signat) True signat
>                | isIdent t = Mph nm  p  [] (sign signat) True signat
>                | source t `order` target s = Mph (name signat) p' [] (sign signat) True signat
>                | otherwise = error ("(module CC_aux) unable to `;` (compose) nonequivalent relations "++show s++" and "++show t++".")
>                where
>                 Mph nm  p  atts  sgn  yin  sg  = s
>                 Mph nm' p' atts' sgn' yin' sg' = t
>                 signat = (if yin then sg else flp sg) `jnSgn` (if yin then sg' else flp sg')

>  jnSgn :: Declaration -> Declaration -> Declaration
>  s `jnSgn` t   | isIdent s = Sgn nm' (a `lub` a') b' props' prL' prM' prR' cs' expla' pos' nr' False
>                | isIdent t = Sgn nm  a' (b `lub` b') props  prL  prM  prR  cs  expla  pos  nr  False
>                | source t `order` target s = Sgn (nm++";"++nm') a b' (h (multiplicities s) `isc` h (multiplicities t)) prL prM prR (cs `join` cs') "" posNone 0 False
>                | otherwise = error ("(module CC_aux) unable to `;` (compose) nonequivalent relations "++show s++" and "++show t++".")
>                where
>                 h ps = ps>-[Sym,Asy,Trn,Rfx]
>                 Sgn nm  a  b  props  prL  prM  prR  cs  expla  pos  nr  False = s
>                 Sgn nm' a' b' props' prL' prM' prR' cs' expla' pos' nr' False = t -}

>  instance Calc Declaration where
>   limit (a,b) s@(Sgn nm a' b' props prL prM prR cs expla pos nr sig)
>    | a `order` a' && b `order` b' = Sgn nm (a `lub` a') ( b `lub` b') props prL prM prR [[x,y]| [x,y]<-contents s, x `elem` conts a, y `elem` conts b] "" pos nr sig
>    | otherwise = error ("(module CC_aux) Cannot limit "++show (a,b)++" with limit (Sgn nm "++show a'++" "++show b'++" props prL prM prR cs pos nr sig)")
>   limit (a,b) (Isn g s)
>    | g <= a && s <= b = Isn a b
>    | otherwise = error ("(module CC_aux) Cannot limit "++show (a,b)++" with limit (Isn "++show g++" "++show s++")")
>   limit (a,b) (Iscompl g s)
>    | g <= a && s <= b = Iscompl a b
>    | otherwise = error ("(module CC_aux) Cannot limit "++show (a,b)++" with limit (Iscompl "++show g++" "++show s++")")
>   calc sg@(Sgn _ _ _ _ _ _ _ _ _ _ _ _) ss
>    = contents (head([x|x<-ss, source x <= source sg && target x <= target sg]++
>                    error ("(module CC_aux) Scope error1 :"++name sg)
>               )    )
>   calc sg ss = contents sg

>  instance Calc Morphism where
>   limit (a,b) (Mph nm pos atts sgn True s)  = Mph nm pos atts (a,b) True (limit (a,b) s)
>   limit (a,b) (Mph nm pos atts sgn False s) = Mph nm pos atts (b,a) False (limit (b,a) s)
>   limit (a,b) (I atts g s yin)              = if a <= b then (if yin then I atts b a yin else I atts a b yin)
>                                               else error ("(module CC_aux) !Fatal error: "++show a++" <= "++show b++" expected.")
>   limit (a,b) (V atts (a',b'))              = V atts (a,b)
>   calc m@(Mph nm pos atts sgn yin s) ss
>     = if null signs then error ("(module CC_aux) Scope error :"++showS s++" "++show (map showS ss)) else
>       if length signs>1 then error ("(module CC_aux) Calculation error : ambiguous "++showS s++" in calc ("++show m++") "++show (map showS ss)) else
>       if yin then contents (head signs) else map reverse (contents (head signs))
>       where signs = [x|x<-ss, x == s]
>   calc i ss  = contents i

>  instance Calc Expression where
>   limit sgn'  (Tm m)  = Tm (limit sgn' m)
>   limit sgn'  (Tc f)  = Tc (limit sgn' f)
>   limit sgn (F ts)    = F (lim sgn ts)
>    where lim sgn  [x] = [limit sgn x]
>          lim sgn   [] = []
>          lim (a,b) (x:xs) = [limit (a,c) x]++lim (c,b) xs
>                             where c = if null xs then target x else
>                                       if target x `order` source (head xs) then target x `lub` source (head xs) else
>                                       error ("(module CC_aux) Fatal: limit sgn ("++showHS "" (F ts)++") has incompatible types inside...")
>   limit sgn (Fd ts)   = Fd (lim sgn ts)
>    where lim sgn  [x] = [limit sgn x]
>          lim sgn   [] = []
>          lim (a,b) (x:xs) = [limit (a,c) x]++lim (c,b) xs
>                             where c = if null xs then target x else
>                                       if target x `order` source (head xs) then target x `lub` source (head xs) else
>                                       error ("(module CC_aux) Fatal: limit sgn ("++showHS "" (Fd ts)++") has incompatible types inside...")
>   limit sgn (Fu fs)   = Fu (map (limit sgn) fs)
>   limit sgn (Fi fs)   = Fi (map (limit sgn) fs)
>   limit sgn' (K0 e)   = K0 (limit sgn' e)
>   limit sgn' (K1 e)   = K1 (limit sgn' e)
>   limit sgn' (Cp e)   = Cp (limit sgn' e)

>   calc (Tm m) ss      = calc m ss
>   calc (Tc f) ss      = calc f ss
>   calc (F  ts) ss     = if null ts then error ("(module CC_aux) Fatal: no terms in calc (F "++showHS "" ts++")") else
>                         foldr1 join [calc t ss| t<-ts ]

   calc (Fd ts) ss     = if null ts then error ("(module CC_aux) Fatal: no terms in calc (Fd "++showHS "" ts++")") else
                         foldr1 joinD [calc t ss| t<-ts ]

>   calc (Fu fs) ss     = foldr uni [] [calc f ss| f<-fs ]
>   calc (Fi fs) ss     = if null fs then error ("(module CC_aux) Fatal: no factors in calc (Fi "++showHS "" fs++")") else
>                         foldr1 isc  [calc f ss| f<-fs ]
>   calc (K0 e) ss      = clos1 (calc e ss) `uni` [[a,a]|a <-conts (source e `lub` target e)]
>   calc (K1 e) ss      = clos1 (calc e ss)
>   calc (Cp e) ss      = --error ("(module CC_aux) Diagnosis:\nsource: "++show (conts (source e)) ++"\ntarget: "++show (conts (target e)))
>                         [[a,b]| [a,b]<-diag [] (conts (source e)) [] (conts (target e)), not ([a,b] `elem` calc e ss)]

>  fun,tot,inj,sur :: [Prop]->Bool
>  fun = elem Uni
>  tot = elem Tot
>  inj = elem Inj
>  sur = elem Sur
>  automatic m = Aut `elem` multiplicities m
>  instance Typologic Concept

>  applyM (Sgn nm _ _ _ prL prM prR _ _ _ _ _) d c = if null (prL++prM++prR) then d++" "++nm++" "++c else prL++(if null prL then d else unCap d)++prM++c++prR
>  applyM (Isn _ _)                            d c = d++" equals "++c
>  applyM (Iscompl _ _)                        d c = d++" differs from "++c
>  applyM (Vs _ _)                             d c = show True


>  instance Identified Pattern where
>   name (Pat nm _ _ _ _ _) = nm

Interpretation of context as a language means to describe the classification tree,
the set of declarations and the rules that apply in that context. Inheritance of
properties is achieved as a result.


>  union :: Pattern -> Pattern -> Pattern
>  union (Pat nm rs parChds pms cs ks) (Pat nm' rs' parChds' pms' cs' ks')
>    = Pat nm' (rs `uni` rs') (parChds `uni` parChds') (pms `uni` pms') (cs `uni` cs') (ks `uni` ks')

>  class Morphical a => Language a where
>    declaredRules  :: a -> [Rule] -- all rules in the language that are specified as a rule in the ADL-model, including the GLUE rules, but excluding the multiplicity rules (multRules).
>    declaredRules x = []
>    multRules      :: a -> [Rule] -- all rules in the language that are specified as declaration properties.
>    multRules       = multRules.declarations
>    rules          :: a -> [Rule] -- all rules in the language that hold within the language
>    rules x         = declaredRules x++multRules x++specs x
>    signals        :: a -> [Rule] -- all SIGNAL rules in the language.
>    signals x       = []
>    specs          :: a -> [Rule] -- all GLUE rules in the language.
>    specs x         = []
>    patterns       :: a -> [Pattern]
>    patterns x      = []
>    objectdefs     :: a -> [ObjectDef]
>    objectdefs x    = []
>    isa            :: a -> Inheritance Concept
>    isa x           = empty

>  instance Language a => Language [a] where
>   declaredRules xs = (concat. map declaredRules) xs
>   multRules xs     = (concat. map multRules) xs
>   signals xs       = (concat. map signals) xs
>   specs xs         = (rd. concat. map specs) xs
>   patterns         = {- rd' name. -} concat.map patterns
>   objectdefs       = {- rd' name. -} concat.map objectdefs
>   isa              = foldr uni empty.map isa

>  instance Language a => Language (Classification a) where
>   declaredRules cl = declaredRules (preCl cl)
>   multRules cl     = multRules (preCl cl)
>   signals cl       = signals (preCl cl)
>   specs cl         = specs (preCl cl)
>   patterns cl      = patterns (preCl cl)
>   objectdefs cl    = objectdefs (preCl cl)
>   isa              = foldr uni empty.map isa.preCl

>  instance Language Rule where
>   declaredRules r@(Gc pos m expr cpu sgn nr pn)
>    = [Ru 'E' (F [Tm m]) pos expr cpu (name m++" is implemented using "++enumerate (map name (mors expr))) sgn nr pn]
>   declaredRules   r@(Ru _ _ _ _ _ _ _ _ _) = [r]
>   declaredRules   r                        = []
>   rules r                                  = []
>   signals r@(Sg _ _ _ _ _ _ _)             = [r]
>   signals r                                = []
>   specs   r@(Gc _ _ _ _ _ _ _)             = [r]
>   specs   r                                = []
>   patterns r                               = [Pat "" [r] [] [] [] []]
>   isa (Gc _ m expr _ _ _ _)
>     = Isa tuples (concs expr>-[e|(a,b)<-tuples,e<-[a,b]])
>       where tuples = clear [(source expr,source m),(target expr,target m)]
>   isa r = empty

>  clear abs = rd [(a,b)| (a,b)<-abs, a/=b]
>  clearG abs = rd [G pos g s| G pos g s<-abs, g/=s]

>  instance Language Pattern where
>   declaredRules (Pat nm rs parChds pms cs ks) = [r|r@(Ru c antc pos cons cpu expla sgn nr pn)<-rs]
>   rules r                                     = []
>   signals (Pat nm rs parChds pms cs ks)       = [r|r@(Sg p rule expla sgn nr pn signal)<-rs]
>   specs (Pat nm rs parChds pms cs ks)         = [r|r@(Gc pos m expr cpu sgn nr pn)<-rs]
>   patterns p                                  = [p]
>   isa   (Pat nm rs parChds pms cs ks)         = Isa ts (singles>-[e| G pos g s<-parChds,e<-[g,s]])
>                                                 where Isa tuples singles = isa rs
>                                                       ts = clear (tuples++[(g,s)| G pos g s<-parChds])

>  instance Language Context where
>   declaredRules context = declaredRules (foldr union (Pat "" [] [] [] [] []) (patterns context))++declaredRules (wrld context)
>   multRules     context = multRules     (foldr union (Pat "" [] [] [] [] []) (patterns context))++multRules     (wrld context)
>   rules         context = [r| r<-(ctxrs context), not (isSignal r)]
>  -- rules         (Ctx nm on i world pats rs ds cs ks os pops) = [r| r<-rs, not (isSignal r)]
>   signals       context = signals       (foldr union (Pat "" [] [] [] [] []) (patterns context))++signals       (wrld context)
>   specs         context = specs         (foldr union (Pat "" [] [] [] [] []) (patterns context))++specs         (wrld context)
>   patterns      context     = ctxpats context
>   objectdefs    context     = ctxos   context
>   isa           context     = ctxisa  context

>  instance Language Declaration where
>   multRules d
>    = [h p| p<-multiplicities d, p `elem` [Uni,Tot,Inj,Sur,Sym,Asy,Trn,Rfx]
>          , if source d==target d || p `elem` [Uni,Tot,Inj,Sur] then True else
>             error ("!Fatal (module CC_aux): Property "++show p++" requires equal source and target domains (you specified "++name (source d)++" and "++name (target d)++").") ]
>     where h Sym = Ru 'E' (F [Tm r]) (ADLdef.pos d) (F [Tm r'])        [] (name d++"["++name (source d)++"*"++name (source d)++"] is symmetric.")     sgn (nr d) ""
>           h Asy = Ru 'I' (Fi [F [Tm r], F [Tm r']]) (ADLdef.pos d) id [] (name d++"["++name (source d)++"*"++name (source d)++"] is antisymmetric.") sgn (nr d) ""
>           h Trn = Ru 'I' (F [Tm r, Tm r]) (ADLdef.pos d) (F [Tm r])   [] (name d++"["++name (source d)++"*"++name (source d)++"] is transitive.")    sgn (nr d) ""
>           h Rfx = Ru 'I' id (ADLdef.pos d) (F [Tm r])                 [] (name d++"["++name (source d)++"*"++name (source d)++"] is reflexive.")     sgn (nr d) ""
>           h Uni = Ru 'I' (F [Tm r',Tm r]) (ADLdef.pos d) id'          [] (name d++"["++name (source d)++"*"++name (target d)++"] is univalent")      sgn (nr d) ""
>           h Sur = Ru 'I' id' (ADLdef.pos d) (F [Tm r',Tm r])          [] (name d++"["++name (source d)++"*"++name (target d)++"] is surjective")     sgn (nr d) ""
>           h Inj = Ru 'I' (F [Tm r,Tm r']) (ADLdef.pos d) id           [] (name d++"["++name (source d)++"*"++name (target d)++"] is injective")      sgn (nr d) ""
>           h Tot = Ru 'I' id (ADLdef.pos d) (F [Tm r,Tm r'])           [] (name d++"["++name (source d)++"*"++name (target d)++"] is total")          sgn (nr d) ""
>           sgn   = (source d,source d)
>           r     = Mph (name d)                (ADLdef.pos d) [] (source d,target d) True d
>           r'    = flp (r ) 
>           r'' t = Mph (t++"["++(name d)++"]") (ADLdef.pos d) [] (source d,target d) True d
>           id    = F [Tm (I [source d] (source d) (source d) True)]
>           id'    = F [Tm (I [target d] (target d) (target d) True)]

  nogE a b = False


Calculation of positions of symbols (both terminal and nonterminal) in the source code, to be performed by the parser


>  gsym_pos :: IsParser p Token => TokenType -> String -> String -> p FilePos
>  gsym_pos kind val val2 = get_tok_pos <$> pSym (Tok kind val val2 noPos "")

>  gsym_val_pos :: IsParser p Token => TokenType -> String -> String -> p (String,FilePos)
>  gsym_val_pos kind val val2 = get_tok_val_pos <$> pSym (Tok kind val val2 noPos "")

>  pOperAny           ::  IsParser p Token => p String
>  pOperAny           =   pOper    ""

>  pOper_pos name     =   gsym_pos TkOp        name      name
>  pKey_pos  keyword  =   gsym_pos TkKeyword   keyword   keyword
>  pSpec_pos s        =   gsym_pos TkSymbol    [s]       [s]

>  pOParen_pos, pString_pos, pChar_pos, pInteger8_pos, pInteger10_pos, pInteger16_pos,
>     pVarid_pos, pConid_pos, pTextnm_pos, pTextln_pos, pInteger_pos
>                     ::  IsParser p Token => p FilePos
>  pOParen_pos        =   pSpec_pos '('

>  pString_pos        =   gsym_pos TkString    ""        "?STR?"
>  pChar_pos          =   gsym_pos TkChar      ""        "'chr'"
>  pInteger8_pos      =   gsym_pos TkInteger8  ""        "1"
>  pInteger10_pos     =   gsym_pos TkInteger10 ""        "1"
>  pInteger16_pos     =   gsym_pos TkInteger16 ""        "1"
>  pVarid_pos         =   gsym_pos TkVarid     ""        "?LC?"
>  pConid_pos         =   gsym_pos TkConid     ""        "?UC?"
>  pTextnm_pos        =   gsym_pos TkTextnm    ""        ""
>  pTextln_pos        =   gsym_pos TkTextln    ""        ""
>  pInteger_pos       =   pInteger10_pos

>  pInteger10_val_pos, pString_val_pos, pChar_val_pos, pVarid_val_pos, pConid_val_pos,
>     pInteger_val_pos
>                     ::  IsParser p Token => p (String,FilePos)
>  pInteger10_val_pos =   gsym_val_pos TkInteger10 ""        "1"
>  pString_val_pos    =   gsym_val_pos TkString    ""        "?STR?"
>  pChar_val_pos      =   gsym_val_pos TkChar      ""        "'chr'"
>  pVarid_val_pos     =   gsym_val_pos TkVarid     ""        "?LC?"
>  pConid_val_pos     =   gsym_val_pos TkConid     ""        "?UC?"
>  pInteger_val_pos   =   pInteger10_val_pos

>  pParens_pos        ::  IsParser p Token => p a -> p (FilePos,a)
>  pParens_pos p      =   (,) <$> pOParen_pos <*> p <* pCParen

  mor2 :: Morphism -> [String]
  mor2 m = [mor2name m,mor2name (source m),mor2name (target m)]

>  mor2filename m = "Atlas"++rEncode (name m)++".html"

  relName [r,d,c] = strip(r++conceptForm d++conceptForm c)

  
  
  
  
  
  
  
  
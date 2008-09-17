-- This module defines that ADL language 

> module ADLdef ( Architecture(..)
>               , Contexts 
>               , Context(..)
>               , Concepts 
>               , Concept(..),cptC,cptS,cptAnything,cptNothing
>                    , cptnew,isAnything,isNothing,isC
>               , ConceptDef(..),ConceptDefs
>               , ObjectDef(..), objdefNew
>               , ObjDefs
>               , KeyDef(..),KeyDefs
>               , Population(..), Populations
>               , Morphism(..),Morphisms,inline,makeDeclaration,makeInline
>               , Declaration(..),makeMph
>               , Declarations
>               , Pattern(..),union
>               , Patterns
>               , Expression(..),isPos,isNeg,notCp
>               , Expressions
>               , Prop(..)
>               , Gen(..), Gens
>               , Rule(..),ruleType,consequent,cpu,antecedent,normExpr,uncomp
>               , Rules
>               , Paire,src, trg,v
>               , Pairs
>               , GenR
>               , Object(..)
>               , Populated(..)
>               , Morphic(..)
>               , Key(..)
>               , Language(..)
>               , Morphical(..)
>               , Numbered(..)
>               , FilePos(..),posNone
>               , Substitutive(..)
>               , mIs
>               , sIs
>               ) 
> where
>
>  import ADLdataDef
>  import CommonClasses ( Identified(name)
>                       , ABoolAlg(glb,lub,order)
>                       , Explained(explain)
>						, Conceptual(conts)
>                       , Morphics(anything)
>                       )
>  import Typology ( Inheritance(Isa), Typologic(typology), genEq)
>  import Classification ( Classification(),preCl,mapCl)
>  import Collection (Collection (uni,isc,(>-),empty,rd))
>  import UU_Scanner (Pos(Pos))
>  import Auxiliaries (chain, eqClass, enumerate, sort', clos1,diag,eqCl) 



>  instance Key Context where
>   keys context
>    = ( concat [keys p| p<-patterns context] ++
>        [(target ctx,lbl,ats)|Kd pos lbl ctx ats<-keyDefs context]
>      )

>  instance Language Context where
>   --Interpretation of context as a language means to describe the classification tree,
>   --the set of declarations and the rules that apply in that context. Inheritance of
>   --properties is achieved as a result.
>   declaredRules context = declaredRules (foldr union (Pat "" [] [] [] [] []) (patterns context))++declaredRules (ctxwrld context)
>   multRules     context = multRules     (foldr union (Pat "" [] [] [] [] []) (patterns context))++multRules     (ctxwrld context)
>   rules         context = [r| r<-(ctxrs context), not (isSignal r)]
>  -- rules         (Ctx nm on i world pats rs ds cs ks os pops) = [r| r<-rs, not (isSignal r)]
>   signals       context = signals       (foldr union (Pat "" [] [] [] [] []) (patterns context))++signals       (ctxwrld context)
>   specs         context = specs         (foldr union (Pat "" [] [] [] [] []) (patterns context))++specs         (ctxwrld context)
>   patterns      context     = ctxpats context
>   objectdefs    context     = ctxos   context
>   isa           context     = ctxisa  context

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

>  instance Object Context where
>   concept _    = cptAnything
>   attributes ctx = ctxos ctx
>   ctx        _ = error ("Cannot evaluate the context expression of the current context (yet)")
>   populations  ctx = ctxpops ctx
>   extends ctx = ctxon ctx


>  mIs :: Concept -> Morphism
>  mIs c = I [] c c True
>  sIs c = Isn c c

>  cptC nm gE os = C nm gE os  -- constructor
>  cptS = S                    -- constructor
>  cptAnything = Anything      -- constructor
>  cptNothing = NOthing        -- constructor
>  cptnew nm = cptC nm (==) []

>  instance ABoolAlg Concept where
>   glb a b | b <= a = b
>           | a <= b = a
>           | otherwise = error ("(module CC_aux) Fatal: (C) glb undefined: a="++show a++", b="++show b)
>   lub a b | a <= b = b
>           | b <= a = a
>           | otherwise = error ("(module CC_aux) Fatal: (C) lub undefined: a="++show a++", b="++show b)

>  instance Conceptual Concept where
>   conts (C {cptos = os}) = os
>   conts (S)        = error ("(module CC_aux) Fatal: ONE has exactly one concept, but it is not te be referred to")
>   conts Anything   = error ("(module CC_aux) Fatal: Anything is Everything...")
>   conts NOthing    = error ("(module CC_aux) Fatal: NOthing is not very much...")

>  instance Explained Concept where
>   explain c = name c   
>   --explain (C expla _ _) = expla
>   --explain (S)           = "ONE"
>   --explain NOthing       = "Nothing"
>   --explain Anything      = "Anything"

>  instance Morphic Concept where
>   source c = c
>   target c = c
>--   sign c = (c,c)
>   multiplicities c = [Uni,Tot,Sur,Inj,Sym,Trn,Rfx]
>   flp c = c
>   isIdent c = True    -- > tells whether the argument is equivalent to I
>   isProp c = True    -- > tells whether the argument is equivalent to I
>   isNot c   = False   -- > tells whether the argument is equivalent to I-
>   isMph c = False
>   isTrue c = singleton c
>   isFalse c = False
>   isSignal c = False
>   singleton S = True
>   singleton _ = False
>   typeUniq Anything = False
>   typeUniq _ = True

>  instance Morphics Concept where
>   anything c = isAnything c

>  instance Morphical Concept where
>   concs        c                    = [c]
>   mors         c                    = [I [] c c True]
>   morlist      c                    = [I [] c c True]
>   declarations c                    = []
>   genE         (C {cptgE = gE})     = gE
>   genE         (S)                  = (<=)::Concept->Concept->Bool
>   genE         Anything             = (<=)::Concept->Concept->Bool
>   genE         NOthing              = (<=)::Concept->Concept->Bool

>  instance Ord Concept where
>   NOthing <= b  = False
>   a <= NOthing  = True
>   Anything <= b = True
>   a <= Anything = False
>   a@(C _ gE _) <= b = a `gE` b
>   a <= b = a==b
>   --TODO?: ORD is niet gedefinieerd op Singelton.... Is dat erg?

>  instance Typologic Concept

>  isNothing, isAnything :: Concept -> Bool
>  isNothing  c | c == cptNothing  = True
>               | otherwise        = False
>  isAnything c | c == cptAnything = True
>               | otherwise        = False
>  isC C{} = True
>  isC c   = False


   instance Key ObjectDef where
    keys obj = [(target (objctx obj),name obj,objats obj)]

>  instance Morphical ObjectDef where
>   concs        obj = [source (objctx obj)] `uni` concs (objats obj)
>   conceptDefs  obj = []
>   mors         obj = mors (objctx obj) `uni` mors (objats obj) `uni` mors (target (objctx obj))  -- opletten: de expressie (objctx obj) hoort hier ook bij.
>   morlist      obj = morlist (objctx obj)++morlist (objats obj)
>   declarations obj = []
>   closExprs    obj = closExprs (objctx obj) `uni` closExprs (objats obj)
>   objDefs      obj = [obj]

>  instance Numbered ObjectDef where
>   pos obj = objpos obj

>  instance Object ObjectDef where
>   concept obj = target (objctx obj)
>   attributes obj = objats obj
>   ctx obj = objctx obj
>   populations  _ = []


>  objdefNew e = Obj "" posNone e []    -- de constructor van een object. Er is geen default waarde voor expression, dus die moeten we dan maar meegeven. 8-((


>  instance Key KeyDef where
>   keys (Kd pos lbl ctx ats) = [(target ctx,lbl,ats)]

>  instance Morphical KeyDef where
>   concs        kd = concs (kdctx kd)`uni` concs (kdats kd)
>   mors         kd = mors (kdctx kd) `uni` mors (kdats kd)
>   morlist      kd = morlist (kdctx kd) ++ morlist (kdats kd)
>   genE         kd = genE (kdats kd)
>   declarations kd = declarations (kdctx kd) `uni` declarations (kdats kd)
>   keyDefs      kd = [kd]



>  instance ABoolAlg Morphism  -- SJ  2007/09/14: This is used solely for drawing conceptual graphs.


A declaration stands for a relation between two concepts. Mathematically, we
interpret a declaration as a relation, i.e. a subset of the cartesian product of two sets.
m::declaration  means interpret m `subsetEq` interpret (source m) x interpret (target m).
Every declaration m has cardinalities, in which
  Uni:    forall x,y,y': x m y & x m y' => y==y'           (~m;m `subsetEq` I[target m])
  Tot:    forall x: exists y: x m y                        (I[source m] `subsetEq` m;~m)
  Inj:    forall x,x',y: x m y & x' m y => x==x'           (m;~m `subsetEq` I[source m])
  Sur:    forall y: exists x: x m y                        (I[target m] `subsetEq` ~m;m)
  Trn:    forall x,y,z: x m y & y m z => x m z             (m;m `subsetEq` m)
  Asy:    forall x,y: x m y & y m x => x==y                (m & ~m `subsetEq` I)
  Sym:    forall x,y: x m y <=> y m x                      (m = ~m)
  Rfx:    forall x: x m x                                  (I `subsetEq` m)

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
>   isNot m                           = isNot (makeDeclaration m)   -- > tells whether the argument is equivalent to I-
>   isMph (Mph _ _ _ _ _ _)           = True
>   isMph _                           = False
>   isTrue (V _ _)                    = True
>   isTrue (I _ a b _)                = singleton b
>   isTrue _                          = False
>   isFalse _                         = False
>   isSignal m                        = isSignal (makeDeclaration m)
>   typeUniq (Mph nm pos  []  (a,b) _ s) = typeUniq a && typeUniq b
>   typeUniq (Mph nm pos atts (a,b) _ s) = True
>   typeUniq (I  []  g s yin) = typeUniq g && typeUniq s
>   typeUniq (I atts g s yin) = True
>   typeUniq (V  []  (a,b)) = typeUniq a && typeUniq b
>   typeUniq (V atts (a,b)) = True

>  instance Morphical Morphism where
>   concs (Mph nm pos atts (a,b) yin s)           = rd [a,b]
>   concs (I atts g s _)                          = rd [g,s]
>   concs (V atts (a,b))                          = rd [a,b]
>   mors m                                        = [makeInline m]
>   morlist m                                     = [m]
>   genE (Mph nm pos atts (a,b) _ s)              = genE a
>   genE (I atts g s _)                           = genE s
>   genE (V atts (a,b))                           = genE a
>   declarations m                                = [makeDeclaration m]

>  instance Ord Morphism where
>   a <= b = source a <= source b && target a <= target b

>  instance Populated Morphism where
>   contents m@(Mph _ _ _ _ False _) = map reverse (contents (flp m))
>   contents m = contents (makeDeclaration m)


>  makeInline :: Morphism -> Morphism
>  makeInline m | inline m = m
>  makeInline m = flp m


>  instance Explained Declaration where
>   explain (Sgn _ _ _ _ _ _ _ _ expla _ _ _) = expla
>   explain d                                 = ""

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

>  instance Morphic Declaration where
>   source (Sgn _ a b _ _ _ _ _ _ _ _ _)         = a
>   source (Isn g s)                             = s
>   source (Iscompl g s)                         = s
>   source (Vs a b)                              = a
>   target (Sgn _ a b _ _ _ _ _ _ _ _ _)         = b
>   target (Isn g s)                             = g
>   target (Iscompl g s)                         = g
>   target (Vs a b)                              = b
>   sign   (Sgn _ a b _ _ _ _ _ _ _ _ _)         = (a,b)
>   sign   (Isn g s)                             = (s,g)
>   sign   (Iscompl g s)                         = (s,g)
>   sign   (Vs g s)                              = (s,g)
>   multiplicities (Sgn _ _ _ ps _ _ _ _ _ _ _ _)= ps
>   multiplicities (Isn g s)         | g==s      = [Uni,Tot,Sur,Inj,Sym,Trn,Rfx]
>                                    | otherwise = [Uni,Tot,    Inj,Sym,Trn,Rfx]
>   multiplicities (Iscompl g s)                 = [Sym]
>   multiplicities (Vs g s)                      = [Tot,Sur]
>   flp(Sgn nm a b props prL prM prR cs expla pos nr sig)
>                                                = Sgn nm b a (flipProps props) "" "" "" (map reverse cs) expla pos nr sig
>   flp    i                                     = i
>   isIdent (Isn _ _)                            = True   -- > tells whether the argument is equivalent to I
>   isIdent _                                    = False
>   isProp (Isn _ _)                             = True                    -- > tells whether the argument is equivalent to I
>   isProp (Iscompl g s)                         = False
>   isProp (Vs a b)                              = a==b && singleton a
>   isProp d                                     = null ([Asy,Sym]>-multiplicities d)
>   isNot (Iscompl _ _)                          = True   -- > tells whether the argument is equivalent to I-
>   isNot _                                      = False
>   isTrue (Vs _ _)                              = True
>   isTrue _                                     = False
>   isFalse _                                    = False
>   isSignal (Sgn _ _ _ _ _ _ _ _ _ _ _ s)       = s
>   isSignal _                                   = False
>   isMph (Sgn _ a b _ _ _ _ _ _ _ _ _)          = True
>   isMph _                                      = False
>   typeUniq (Sgn _ a b _ _ _ _ _ _ _ _ _)       = typeUniq a && typeUniq b    -- XXX Check en dubbel check: Stef vragen of dit een verbetering is. (er stond namelijk :typeUniq (Sgn _ a b _ _ _ _ _ _ _ _ _)       = typeUniq a && typeUniq a  
>   typeUniq (Isn g s)                           = typeUniq g && typeUniq s
>   typeUniq (Iscompl g s)                       = typeUniq g && typeUniq s
>   typeUniq (Vs g s)                            = typeUniq g && typeUniq s

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

>  makeMph :: Declaration -> Morphism
>  makeMph d = Mph (name d) (ADLdef.pos d) [] (sign d) True d


>  instance Populated Declaration where
>   contents (Sgn _ _ _ _ _ _ _ cs _ _ _ _) = cs
>   contents (Isn g s)                      = [[o,o] | o<-conts s]
>   contents (Iscompl g s)                  = [[o,o']| o<-conts s,o'<-conts s,o/=o']
>   contents (Vs g s)                       = [[o,o']| o<-conts s,o'<-conts s]

>  instance Key Pattern where
>   keys pat = [(target ctx,lbl,ats)|Kd pos lbl ctx ats<-keyDefs pat]

>  instance Language Pattern where
>   declaredRules (Pat nm rs parChds pms cs ks) = [r|r@(Ru c antc pos cons cpu expla sgn nr pn)<-rs]
>   rules r                                     = []
>   signals (Pat nm rs parChds pms cs ks)       = [r|r@(Sg p rule expla sgn nr pn signal)<-rs]
>   specs (Pat nm rs parChds pms cs ks)         = [r|r@(Gc pos m expr cpu sgn nr pn)<-rs]
>   patterns p                                  = [p]
>   isa   (Pat nm rs parChds pms cs ks)         = Isa ts (singles>-[e| G pos g s<-parChds,e<-[g,s]])
>                                                 where Isa tuples singles = isa rs
>                                                       ts = clear (tuples++[(g,s)| G pos g s<-parChds])

>  instance Morphical Pattern where
>   concs        (Pat nm rs gen pms cs ks) = concs rs `uni` concs gen `uni` concs pms
>   conceptDefs  (Pat nm rs gen pms cs ks) = cs
>   mors         (Pat nm rs gen pms cs ks) = mors rs `uni` mors ks
>   morlist      (Pat nm rs gen pms cs ks) = morlist rs++morlist ks
>   declarations (Pat nm rs gen pms cs ks) = rd pms
>   genE         (Pat nm rs gen pms cs ks) = genE (pms++declarations (signals rs))
>   closExprs    (Pat nm rs gen pms cs ks) = closExprs rs

>  union :: Pattern -> Pattern -> Pattern
>  union (Pat nm rs parChds pms cs ks) (Pat nm' rs' parChds' pms' cs' ks')
>    = Pat nm' (rs `uni` rs') (parChds `uni` parChds') (pms `uni` pms') (cs `uni` cs') (ks `uni` ks')



>  instance Morphic Expression where
>   source (Tm m)          = source m
>   source (Tc f)          = source f
>   source (F  [])         = Anything -- error ("(module CC_aux) Fatal: source (F [])")
>   source (F  ts)         = source (head ts)
>   source (Fd [])         = Anything -- error ("(module CC_aux) Fatal: source (Fd [])")
>   source (Fd ts)         = source (head ts)
>   source (Fu fs)         = if length (eqClass order (map source fs))==1 then minimum (map source fs)
>                            else Anything -- error ("(module CC_aux) Fatal: source ("++showHS "" (Fu fs)++")")
>   source (Fi fs)         = if length (eqClass order (map source fs))==1 then maximum (map source fs)
>                            else Anything -- error ("(module CC_aux) Fatal: source ("++showHS "" (Fi fs)++")")
>   source (K0 e)          = source e
>   source (K1 e)          = source e
>   source (Cp e)          = source e

>   target (Tm m)          = target m
>   target (Tc f)          = target f
>   target (F  [])         = Anything -- error ("(module CC_aux) Fatal: target (F [])")
>   target (F  ts)         = target (last ts)
>   target (Fd [])         = Anything -- error ("(module CC_aux) Fatal: target (Fd [])")
>   target (Fd ts)         = target (last ts)
>   target (Fu fs)         = if length (eqClass order (map target fs))==1 then minimum (map target fs)
>                            else Anything -- error ("(module CC_aux) Fatal: target ("++showADL (Fu fs)++")")
>   target (Fi fs)         = if length (eqClass order (map target fs))==1 then maximum (map target fs)
>                            else Anything -- error ("(module CC_aux) Fatal: target ("++showADL (Fi fs)++")")
>   target (K0 e)          = target e
>   target (K1 e)          = target e
>   target (Cp e)          = target e

>   sign (Tm m)            = sign m
>   sign (Tc f)            = sign f
>   sign (F ts)            = if null ts 
>                             then error ("(module CC_aux) Fatal: no terms in sign (F "++misbruiktShowHS "" ts++")")
>                             else foldr1 jnSign (map sign ts)
>                             where (s,t) `jnSign` (s',t') = (s,t')
>   sign (Fd ts)           = if null ts 
>                             then error ("(module CC_aux) Fatal: no terms in sign (Fd "++misbruiktShowHS "" ts++")")
>                             else foldr1 jnSign (map sign ts)
>                             where (s,t) `jnSign` (s',t') = (s,t')
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

>  instance Ord Expression where
>   a <= b = source a <= source b && target a <= target b

>  instance Populated Expression where
>   contents (Tm m)        = contents m
>   contents (Tc f)        = contents f
>   contents f@(F ts)
>    | idsOnly ts
>       = if not (source f `order` target f) then error ("(module CC_aux) Fatal: no order in "++misbruiktShowHS "" f) else
>                            [[e,e]|e<-os]
>    | otherwise
>       = if null css then error ("(module CC_aux) Fatal: no terms in F "++misbruiktShowHS "" ts) else
>                            foldr1 join css
>                            where os = conts (source f `lub` target f)
>                                  css = [contents t|t<-ts, not (idsOnly t)]
>   contents f@(Fd ts)
>     = if null ts then error ("(module CC_aux) Fatal: no terms in Fd "++misbruiktShowHS "" ts) else joinD ts
>   contents (Fu fs) = if null fs then [] else
>                      (foldr1 uni .map contents) fs
>   contents (Fi fs) = if null fs then [] else
>                      (foldr1 isc .map contents) fs
>   contents (K0 e)  = clos1 (contents e) `uni` [[c,c]|c <-conts (source e `lub` target e)]
>   contents (K1 e)  = clos1 (contents e)
>   contents (Cp (Cp e)) = contents e
>   contents (Cp e)  = [[a,b]| [a,b]<-diag [] (conts (source e)) [] (conts (target e)), not ([a,b] `elem` contents e)]

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

>  flipProps :: [Prop] -> [Prop]
>  flipProps ps = [flipProp p| p<-ps]

>  flipProp Uni = Inj
>  flipProp Tot = Sur
>  flipProp Sur = Tot
>  flipProp Inj = Uni
>  flipProp x = x


>  instance Morphical Gen where
>   concs (G pos g s)                             = rd [g,s]
>   mors (G pos g s)                              = [I [] g s True]
>   morlist (G pos g s)                           = [I [] g s True]
>   genE (G pos g s)                              = genE s
>   declarations (G pos g s)                      = []

>  instance Explained Rule where
>   explain (Ru _ _ _ _ _ expla _ _ _) = expla
>   explain (Sg _ _ expla _ _ _ _)     = expla
>   explain r                          = ""

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

>  instance Morphic Rule where
>   source r | ruleType r=='A' = fst (sign r)
>            | otherwise       = fst (sign r)
>   target r | ruleType r=='A' = snd (sign r)
>            | otherwise       = snd (sign r)
>   sign r   | ruleType r=='A' = sign (consequent r)
>            | otherwise       = if sign (antecedent r) `order` sign (consequent r) then sign (antecedent r) `lub` sign (consequent r) else
>                                error ("(module CC_aux) Fatal: incompatible signs in "++misbruiktShowHS "" r)
>   multiplicities r           = []
>   isMph r  | ruleType r=='A' = isMph (consequent r)
>            | otherwise       = False
>   flp r@(Ru 'A' antc pos expr cpu expla (a,b) nr pn) = Ru 'A' (error ("(Module CC_aux:) illegal call to antecedent in flp ("++showADLX r++")")) pos (flp expr) cpu expla (b,a) nr pn
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

>  instance Substitutive Rule where
>   subst (m,f) r@(Ru 'A' antc pos cons cpu expla sgn nr pn)
>    = Ru 'A' (error ("(Module CC_aux:) illegal call to antecedent in subst ("++showADLX m++","++showADLX f++") ("++showADLX r++")")) pos cons' cpu expla (sign cons') nr pn
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

>  instance Identified Rule where
>   name r = "Rule"++show (nr r)

>  ruleType    (Ru c _ _ _ _ _ _ _ _) = c
>  ruleType    (Sg _ rule _ _ _ _ _)  = ruleType rule
>  ruleType    (Gc _ _ _ _ _ _ _)     = 'g'
>  ruleType    (Fr _ _ _ _)           = 'f'
>  antecedent r@(Ru 'A' _ _ _ _ _ _ _ _) = error ("(Module ADLdef:) illegal call to antecedent of rule "++showADLX r)
>  antecedent  (Ru _ a _ _ _ _ _ _ _) = a
>  antecedent  (Sg _ rule _ _ _ _ _)  = antecedent rule
>  antecedent  (Gc _ d _ _ _ _ _)     = Tm d
>  antecedent  (Fr _ _ e _)           = e
>  consequent  (Ru _ _ _ c _ _ _ _ _) = c
>  consequent  (Sg _ rule _ _ _ _ _)  = consequent rule
>  consequent  (Gc _ _ e _ _ _ _)     = e
>  consequent  (Fr _ d _ _)           = Tm (makeMph d)
>  cpu         (Ru _ _ _ _ c _ _ _ _) = c
>  cpu         (Sg _ _ _ _ _ _ _)     = [] -- TODO nakijken: Moet dit niet de signaalrelatie zijn?
>  cpu         (Gc _ _ _ c _ _ _)     = c
>  cpu         (Fr _ d _ _)           = [Tm (makeMph d)]
>  patternName (Ru _ _ _ _ _ _ _ _ p) = p
>  patternName (Sg _ _ _ _ _ p _)     = p
>  patternName (Gc _ _ _ _ _ _ p)     = p
>  patternName (Fr _ _ _ p)           = p
>  uncomp (Ru a b c d e f (g,g') h i) = Ru a b c d [] f (g,g') h i
>  uncomp (Gc a b c d e f g)          = Gc a b c [] e f g
>  uncomp s                           = s



>  src, trg      :: Paire -> String
>  src xs         = if null xs then error ("(module ADLdef) Fatal: src []") else head xs
>  trg xs         = if null xs then error ("(module ADLdef) Fatal: trg []") else last xs
>  join::Pairs->Pairs->Pairs
>  join a b = merge ((sort' (trg.head).eqCl trg) a)
>                   ((sort' (src.head).eqCl src) b)
>             where merge (xs:xss) (ys:yss)
>                    | trg (head xs)<src (head ys) = merge xss (ys:yss)
>                    | trg (head xs)>src (head ys) = merge (xs:xss) yss
>                    | otherwise = [[x,y]|[x,i]<-xs,[j,y]<-ys]++ merge xss yss
>                   merge _ _ = []

>  class Populated a where
>   contents  :: a -> Pairs


>  class Numbered a where
>   nr :: a->Int
>   pos :: a->FilePos
>   nr x = nr (ADLdef.pos x)

>  instance Numbered FilePos where
>   nr (FilePos (fn,Pos l c,sym)) = l
>   pos p = p

>  instance Numbered Rule where
>   pos (Ru _ _ p _ _ _ _ _ _) = p
>   pos (Sg p _ _ _ _ _ _)     = p
>   pos (Gc p _ _ _ _ _ _)     = p
>   pos r = posNone
>   nr  (Ru _ _ _ _ _ _ _ n _) = n
>   nr  (Sg _ _ _ _ n _ _)     = n
>   nr  (Gc _ _ _ _ _ n _)     = n
>   nr  r = 0

>  instance Numbered Morphism where
>   pos (Mph _ p _ _ _ _) = p
>   pos m                 = posNone
>   nr m = nr (makeDeclaration m)

>  instance Numbered Declaration where
>   pos (Sgn _ _ _ _ _ _ _ _ _ p _ _) = p
>   pos d                             = posNone
>   nr (Sgn _ _ _ _ _ _ _ _ _ _ n _)  = n
>   nr d                              = 0

>  instance Numbered Expression where
>   pos (Tm m)  = ADLdef.pos m
>   pos (Tc f)  = ADLdef.pos f
>   pos (F ts)  = if not (null ts) then ADLdef.pos (head ts) else error "(module ADLdef) !!Software error 813. Please submit a complete bug report to your dealer"
>   pos (Fd ts) = if not (null ts) then ADLdef.pos (head ts) else error "(module ADLdef) !!Software error 814. Please submit a complete bug report to your dealer"
>   pos (Fu fs) = if not (null fs) then ADLdef.pos (head fs) else error "(module ADLdef) !!Software error 815. Please submit a complete bug report to your dealer"
>   pos (Fi fs) = if not (null fs) then ADLdef.pos (head fs) else error "(module ADLdef) !!Software error 816. Please submit a complete bug report to your dealer"
>   pos (K0 e)  = ADLdef.pos e
>   pos (K1 e)  = ADLdef.pos e
>   pos (Cp e)  = ADLdef.pos e

>  class Key a where
>   keys :: a->[(Concept,String,[ObjectDef])]

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

>  class Morphic a where
>   source, target :: a -> Concept
>   sign           :: a -> (Concept,Concept)
>   sign x = (source x,target x)
>   multiplicities :: a -> [Prop]
>   multiplicities m = []
>   flp            :: a -> a
>   isIdent        :: a -> Bool  -- > tells whether the argument is equivalent to I
>   isProp         :: a -> Bool  -- > tells whether the argument is a property
>   isNot          :: a -> Bool  -- > tells whether the argument is equivalent to I-
>   isMph          :: a -> Bool
>   isTrue         :: a -> Bool  -- > tells whether the argument is equivalent to V
>   isFalse        :: a -> Bool  -- > tells whether the argument is equivalent to V-
>   isSignal       :: a -> Bool  -- > tells whether the argument refers to a signal
>   singleton      :: a -> Bool  -- > tells whether V=I
>   singleton e     = isProp e && isTrue e
>   equiv          :: a -> a -> Bool
>   equiv m m' = source m==source m'&&target m==target m' || source m==target m'&&target m==source m'
>   typeUniq :: a -> Bool -- this says whether the type of 'a' and all of its constituent parts is defined (i.e. not "Anything")
>   isFunction :: a -> Bool     -- XXXHJ: Toegevoegd aan class Morphic (Was een gewone functie)
>   isFlpFunction :: a -> Bool
>   isFunction m   = null ([Uni,Tot]>-multiplicities m) -- XXXHJ: Toegevoegd aan class Morphic (Was een gewone functie)
>   isFlpFunction m = null ([Sur,Inj]>-multiplicities m)


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
>   idsOnly        :: a -> Bool
>   idsOnly e = and [isIdent m| m<-mors e] -- > tells whether all the arguments are equivalent to I

>  instance Morphical a => Morphical [a] where
>   concs             = rd . concat . map concs
>   conceptDefs       = rd . concat . map conceptDefs
>   mors              = rd . concat . map mors
>   morlist           =      concat . map morlist
>   declarations      = rd . concat . map declarations
>   closExprs         = rd . concat . map closExprs
>   objDefs           =      concat . map objDefs
>   keyDefs           =      concat . map keyDefs

>  class Substitutive a where
>-- precondition: sign f `order` sign m
>   subst :: (Expression,Expression) -> a -> a
>   subst (m,f) x = error "(module CC_aux) Unable to substitute"

>  instance (Morphic a,Substitutive a) => Substitutive [a] where
>   subst (m,f) xs = map (subst (m,f)) xs


>  instance Morphical a => Morphical (Classification a) where
>   concs            = rd . concat . map concs . preCl
>   conceptDefs      = rd . concat . map conceptDefs . preCl
>   mors             = rd . concat . map mors . preCl
>   morlist          =      concat . map morlist . preCl
>   declarations     = rd . concat . map declarations . preCl
>   closExprs        = rd . concat . map closExprs . preCl

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


The following code is used in transforming expressions into clauses

>  isPos :: Expression -> Bool
>  isPos (Cp _) = False
>  isPos _ = True
>  isNeg e = not (isPos e)

>  notCp (Cp e) = e
>  notCp e = Cp e

>  isImin :: Expression -> Bool
>  isImin (Fd ts)  = and [isImin t| t<-ts]
>  isImin (Fi fs)  = and [isImin f| f<-fs] && not (null fs)
>  isImin (Fu fs)  = and [isImin f| f<-fs] && not (null fs)
>  isImin (F  [e]) = isImin e
>  isImin (Cp v)   = isIdent v
>  isImin _        = False -- (TODO)

>  clear abs = rd [(a,b)| (a,b)<-abs, a/=b]

Transform a rule to an expression:

>  normExpr :: Rule -> Expression
>  normExpr rule
>   | isSignal rule      = v (sign rule)
>   | ruleType rule=='A' = consequent rule
>   | ruleType rule=='I' = Fu [Cp (antecedent rule), consequent rule]
>   | ruleType rule=='E' = Fi [ Fu [antecedent rule, Cp (consequent rule)]
>                             , Fu [Cp (antecedent rule), consequent rule]]
>   | otherwise          = error("Fatal (module CC_aux): Cannot make an expression of "++misbruiktShowHS "" rule)

>  v :: (Concept, Concept) -> Expression
>  v (a,b) = Tm (V [] (a,b))

>  joinD :: [Expression] -> [Paire]
>  joinD [s]      = contents s
>  joinD (r:s:ts) = [ [head (head rc),last (head sc)]
>                   | rc<-eqCl head (contents r)
>                   , sc<-eqCl last (joinD (s:ts))
>                   , null (conts (target r `glb` source s) >-(map last rc `uni` map head sc))
>                   ]

>  --Onderstaande functies zijn bedoeld voor foutmeldingen:
>  misbruiktShowHS indent e = "showHS \""++indent++show e
>  showADLX x = show x
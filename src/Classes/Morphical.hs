
module Classes.Morphical where
   import Adl.Concept
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
   
   import Classification
   import Collection  (rd,uni)
   import Typology
   import Classes.Object
   import Classes.Morphic

   class Morphical a where
    concs        :: a -> Concepts                  -- the set of all concepts used in data structure a
    conceptDefs  :: a -> ConceptDefs               -- the set of all concept definitions in the data structure
    conceptDefs x = []
    mors         :: a -> Morphisms                 -- the set of all morphisms used within data structure a
    morlist      :: a -> Morphisms                 -- the list of all morphisms used within data structure a
    declarations :: a -> Declarations
 --   declarations x  = rd [declaration m|m<-mors x]
    genE         :: a -> GenR
    genE x        = if null cx then (==) else head cx where cx = [gE|C {cptgE = gE } <-concs x]
    closExprs    :: a -> [Expression]               -- no double occurrences in the resulting list of expressions
    closExprs s   = []
    objDefs      :: a -> ObjectDefs
    objDefs s     = []
    keyDefs      :: a -> KeyDefs
    keyDefs s     = []
    idsOnly        :: a -> Bool
    idsOnly e = and [isIdentM m| m<-mors e] -- > tells whether all the arguments are equivalent to I

   instance Morphical a => Morphical [a] where
    concs             = rd . concat . map concs
    conceptDefs       = rd . concat . map conceptDefs
    mors              = rd . concat . map mors
    morlist           =      concat . map morlist
    declarations      = rd . concat . map declarations
    closExprs         = rd . concat . map closExprs
    objDefs           =      concat . map objDefs
    keyDefs           =      concat . map keyDefs

   instance Morphical a => Morphical (Classification a) where
    concs            = rd . concat . map concs . preCl
    conceptDefs      = rd . concat . map conceptDefs . preCl
    mors             = rd . concat . map mors . preCl
    morlist          =      concat . map morlist . preCl
    declarations     = rd . concat . map declarations . preCl
    closExprs        = rd . concat . map closExprs . preCl


--   instance Morphical a => Morphical (Classification a) where
--    concs            = rd . concat . map concs . preCl
--    conceptDefs      = rd . concat . map conceptDefs . preCl
--    mors             = rd . concat . map mors . preCl
--    morlist          =      concat . map morlist . preCl
--    declarations     = rd . concat . map declarations . preCl
--    closExprs        = rd . concat . map closExprs . preCl


   instance Morphical Context where
    concs        ctx = concs (ctxds ctx) `uni` concs (ctxpats ctx)
    conceptDefs  ctx = ctxcs ctx
    mors         ctx = mors (ctxpats ctx) `uni` mors (ctxos ctx)
    morlist      ctx = morlist (ctxpats ctx)++morlist (ctxos ctx)
    declarations ctx = (map (makeFdecl ctx).rd) (ctxds ctx ++[d| pat<-ctxpats ctx, d<-declarations pat])
     where makeFdecl context d@(Sgn nm a b props prL prM prR cs expla pos nr sig)
            = (Sgn nm a b props prL prM prR cs' expla pos nr sig)
              where cs' = rd ([link| Popu m ps<-populations context, makeDeclaration m==d, link<-ps]++cs)
           makeFdecl context d = d
  -- TODO: is dit wel de juiste plek om makeFdecl aan te roepen? Dat zou eigenlijk in MakeFspec moeten, maar alleen als de populatie voor de generator uit Fspc wordt gegenereerd.
    genE         ctx = genEq (typology (ctxisa ctx))
    closExprs    ctx = closExprs (ctxpats ctx) `uni` closExprs (ctxos ctx)
    objDefs      ctx = ctxos ctx
    keyDefs      ctx = ctxks ctx

   instance Morphical KeyDef where
    concs        kd = concs (kdctx kd)`uni` concs (kdats kd)
    mors         kd = mors (kdctx kd) `uni` mors (kdats kd)
    morlist      kd = morlist (kdctx kd) ++ morlist (kdats kd)
    genE         kd = genE (kdats kd)
    declarations kd = declarations (kdctx kd) `uni` declarations (kdats kd)
    keyDefs      kd = [kd]

   instance Morphical Expression where
    concs (Tm m)                                  = rd (concs m)
    concs (Tc f)                                  = rd (concs f)
    concs (F ts)                                  = rd (concs ts)
    concs (Fd ts)                                 = rd (concs ts)
    concs (Fu fs)                                 = rd (concs fs)
    concs (Fi fs)                                 = rd (concs fs)
    concs (K0 e)                                  = rd (concs e)
    concs (K1 e)                                  = rd (concs e)
    concs (Cp e)                                  = rd (concs e)

    mors (Tm m)                                   = mors (makeInline m)
    mors (Tc f)                                   = mors f
    mors (F ts)                                   = mors ts -- voor a;-b;c hoeft geen extra mors rond b nodig te zijn
    mors (Fd ts@(h:t))                            = rd (mors ts ++ (concat) [mors (source c)|c<-t])
    mors (Fu fs)                                  = mors fs
    mors (Fi fs)                                  = mors fs -- voor a /\ -b hoeft geen extra mors rond b nodig te zijn
    mors (K0 e)                                   = mors e
    mors (K1 e)                                   = mors e
    mors (Cp e)                                   = rd (mors e ++ mors (source e)++mors (target e))

    morlist (Tm m)                                = morlist m
    morlist (Tc f)                                = morlist f
    morlist (F ts)                                = morlist ts
    morlist (Fd ts)                               = morlist ts
    morlist (Fu fs)                               = morlist fs
    morlist (Fi fs)                               = morlist fs
    morlist (K0 e)                                = morlist e
    morlist (K1 e)                                = morlist e
    morlist (Cp e)                                = morlist e

    genE (Tm m)                                   = genE m
    genE (Tc f)                                   = genE f
    genE (F ts)                                   = genE ts
    genE (Fd ts)                                  = genE ts
    genE (Fu fs)                                  = genE fs
    genE (Fi fs)                                  = genE fs
    genE (K0 e)                                   = genE e
    genE (K1 e)                                   = genE e
    genE (Cp e)                                   = genE e

    declarations (Tm m)                           = declarations m
    declarations (Tc f)                           = declarations f
    declarations (F ts)                           = declarations ts
    declarations (Fd ts)                          = declarations ts
    declarations (Fu fs)                          = declarations fs
    declarations (Fi fs)                          = declarations fs
    declarations (K0 e)                           = declarations e
    declarations (K1 e)                           = declarations e
    declarations (Cp e)                           = declarations e

    closExprs (Tc f)                              = closExprs f
    closExprs (F ts)                              = (rd.concat.map closExprs) ts
    closExprs (Fd ts)                             = (rd.concat.map closExprs) ts
    closExprs (Fu fs)                             = (rd.concat.map closExprs) fs
    closExprs (Fi fs)                             = (rd.concat.map closExprs) fs
    closExprs (K0 e)                              = [K0 e] `uni` closExprs e
    closExprs (K1 e)                              = [K1 e] `uni` closExprs e
    closExprs (Cp e)                              = closExprs e
    closExprs _                                   = []

   instance Morphical Concept where
    concs        c                    = [c]
    mors         c                    = [I [] c c True]
    morlist      c                    = [I [] c c True]
    declarations c                    = []
    genE         (C {cptgE = gE})     = gE
    genE         (S)                  = (<=)::Concept->Concept->Bool
    genE         Anything             = (<=)::Concept->Concept->Bool
    genE         NOthing              = (<=)::Concept->Concept->Bool

   instance Morphical ObjectDef where
    concs        obj = [source (objctx obj)] `uni` concs (objats obj)
    conceptDefs  obj = []
    mors         obj = mors (objctx obj) `uni` mors (objats obj) `uni` mors (target (objctx obj))  -- opletten: de expressie (objctx obj) hoort hier ook bij.
    morlist      obj = morlist (objctx obj)++morlist (objats obj)
    declarations obj = []
    closExprs    obj = closExprs (objctx obj) `uni` closExprs (objats obj)
    objDefs      obj = [obj]

   instance Morphical Morphism where
    concs (Mph nm pos atts (a,b) yin s)           = rd [a,b]
    concs (I atts g s _)                          = rd [g,s]
    concs (V atts (a,b))                          = rd [a,b]
    mors m                                        = [makeInline m]
    morlist m                                     = [m]
    genE (Mph nm pos atts (a,b) _ s)              = genE a
    genE (I atts g s _)                           = genE s
    genE (V atts (a,b))                           = genE a
    declarations m                                = [makeDeclaration m]

   instance Morphical Declaration where
    concs (Sgn _ a b _ _ _ _ _ _ _ _ _)           = rd [a,b]
    concs (Isn g s)                               = rd [g,s]
    concs (Iscompl g s)                           = [s]
    concs (Vs g s)                                = [s]
    mors s                                        = []
    morlist s                                     = []
    genE (Sgn nm a b _ _ _ _ _ _ _ _ _)           = genE a
    genE (Isn g s)                                = genE s
    genE (Iscompl g s)                            = genE s
    genE (Vs g s)                                 = genE s
    declarations s                                = [s]

   instance Morphical Pattern where
    concs        pat = concs (ptrls pat) `uni` concs (ptgns pat) `uni` concs (ptdcs pat)
    conceptDefs  pat = ptcds pat
    mors         pat = mors (ptrls pat) `uni` mors (ptkds pat)
    morlist      pat = morlist (ptrls pat)++morlist (ptkds pat)
    declarations pat = ptdcs pat
    genE         pat = genE (ptdcs pat++declarations [r| r<-(ptrls pat),isSignal r ])  
    closExprs    pat = closExprs (ptrls pat)

   instance Morphical Rule where
    concs (Ru c antc _ cons _ _ _ _ _)     = if c==Truth then concs cons else concs antc `uni` concs cons
    concs (Sg _ rule _ _ _ _ _)            = concs rule
    concs (Gc _ m expr _ _ _ _)            = concs m `uni` concs expr
    concs (Fr _ d expr _)                  = concs d `uni` concs expr
    mors (Ru c antc _ cons _ _ _ _ _)      = if c==Truth then mors cons else mors antc `uni` mors cons
    mors (Sg _ rule _ _ _ _ _)             = mors rule
    mors (Gc _ m expr _ _ _ _)             = mors m `uni` mors expr
    mors (Fr _ d expr _)                   = mors d `uni` mors expr
    morlist (Ru c antc _ cons _ _ _ _ _)   = if c==Truth then morlist cons else morlist antc++morlist cons
    morlist (Sg _ rule _ _ _ _ _)          = morlist rule
    morlist (Gc _ m expr _ _ _ _)          = morlist m++morlist expr
    morlist (Fr _ _ expr _)                = morlist expr
    genE (Ru c antc _ cons  _ _ _ _ _)     = if c==Truth then genE cons else genE [antc,cons]
    genE (Sg _ rule _ _ _ _ _)             = genE rule
    genE (Gc _ m expr _ _ _ _)             = genE m
    genE (Fr _ _ expr _)                   = genE expr
    declarations (Ru c antc _ cons _ _ _ _ _) = if c==Truth then declarations cons else declarations [antc,cons]
    declarations (Sg _ rule _ _ _ _ d)        = [d] `uni` declarations rule
    declarations (Gc _ m expr _ _ _ _)        = declarations m
    declarations (Fr _ _ expr _)              = declarations expr
    closExprs (Ru c antc _ cons _ _ _ _ _) = if c==Truth then closExprs cons else closExprs antc `uni` closExprs cons
    closExprs (Sg _ rule _ _ _ _ _)        = closExprs rule
    closExprs (Gc _ m expr  _ _ _ _)       = closExprs expr
    closExprs (Fr _ _ expr _)              = [expr]

   instance Morphical Gen where
    concs (G pos g s)                             = rd [g,s]
    mors (G pos g s)                              = [I [] g s True]
    morlist (G pos g s)                           = [I [] g s True]
    genE (G pos g s)                              = genE s
    declarations (G pos g s)                      = []

    
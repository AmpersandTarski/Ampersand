{-# OPTIONS_GHC -Wall #-}
module Classes.Morphical                 (Morphical(concs
                                                   ,conceptDefs
                                                   ,mors
                                                   ,morlist
                                                   ,declarations
                                                   ,genE
                                                   ,closExprs
                                                   ,objDefs
                                                   ,keyDefs
                                                   ,idsOnly
                                         )         )
where
   import Adl.Concept                    (Concept(..),Concepts,GenR,Association(..),Morphic(isSignal))
   import Adl.ConceptDef                 (ConceptDefs)
   import Adl.Context                    (Context(..))
   import Adl.MorphismAndDeclaration     (Morphism(..),Declaration(..),Morphisms,Declarations
                                         ,isIdentM,makeDeclaration,makeInline)
   import Adl.Gen                        (Gen(..))
   import Adl.Expression                 (Expression(..))
   import Adl.ObjectDef                  (ObjectDef(..),ObjectDefs)
   import Adl.KeyDef                     (KeyDef(..),KeyDefs)
   import Adl.Population                 (Population(..))
   import Adl.Pattern                    (Pattern(..))
   import Adl.Rule                       (Rule(..),RuleType(..))
   
   import Classification                 (Classification,preCl)
   import Collection                     (rd,uni)
   import Typology                       (genEq,typology)
   import Classes.Object                 (populations)

   class Morphical a where
    concs        :: a -> Concepts                  -- the set of all concepts used in data structure a
    conceptDefs  :: a -> ConceptDefs               -- the set of all concept definitions in the data structure
    conceptDefs _ = []
    mors         :: a -> Morphisms                 -- the set of all morphisms used within data structure a
    morlist      :: a -> Morphisms                 -- the list of all morphisms used within data structure a
    declarations :: a -> Declarations
 --   declarations x  = rd [declaration m|m<-mors x]
    genE         :: a -> GenR
    genE x        = if null cx then (==) else head cx where cx = [gE|C {cptgE = gE } <-concs x]
    closExprs    :: a -> [Expression]               -- no double occurrences in the resulting list of expressions
    closExprs _   = []
    objDefs      :: a -> ObjectDefs
    objDefs _     = []
    keyDefs      :: a -> KeyDefs
    keyDefs _     = []
    idsOnly        :: a -> Bool
    idsOnly e' = and [isIdentM m'| m'<-mors e'] -- > tells whether all the arguments are equivalent to I

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
    concs        c  = concs (ctxds c) `uni` concs (ctxpats c)
    conceptDefs  c = ctxcs c
    mors         c = mors (ctxpats c) `uni` mors (ctxos c)
    morlist      c = morlist (ctxpats c)++morlist (ctxos c)
    declarations c = (map (makeFdecl c).rd) (ctxds c ++[d| pat<-ctxpats c, d<-declarations pat])
     where makeFdecl context d 
               = case d of
                   Sgn{}     -> d{decpopu = rd( [link| Popu mph ps<-populations context, makeDeclaration mph==d, link<-ps]
                                          ++(decpopu d))
                                 }
                   Isn{}     -> d
                   Iscompl{} -> d
                   Vs{}      -> d
                    
--           makeFdecl context d@(Sgn nm a b props prL prM prR cs expla pos nr sig)
--                             = (Sgn nm a b props prL prM prR cs' expla pos nr sig)
--              where cs' = rd ([link| Popu m' ps<-populations context, makeDeclaration m'==d, link<-ps]++cs)
--           makeFdecl _ d = d
  -- TODO: is dit wel de juiste plek om makeFdecl aan te roepen? Dat zou eigenlijk in MakeFspec moeten, maar alleen als de populatie voor de generator uit Fspc wordt gegenereerd.
    genE         c = genEq (typology (ctxisa c))
    closExprs    c = closExprs (ctxpats c) `uni` closExprs (ctxos c)
    objDefs      c = ctxos c
    keyDefs      c = ctxks c

   instance Morphical KeyDef where
    concs        kd = concs (kdctx kd)`uni` concs (kdats kd)
    mors         kd = mors (kdctx kd) `uni` mors (kdats kd)
    morlist      kd = morlist (kdctx kd) ++ morlist (kdats kd)
    genE         kd = genE (kdats kd)
    declarations kd = declarations (kdctx kd) `uni` declarations (kdats kd)
    keyDefs      kd = [kd]

   instance Morphical Expression where
    concs (Tm mph)          = rd (concs mph)
    concs (Tc f)            = rd (concs f)
    concs (F ts)            = rd (concs ts)
    concs (Fd ts)           = rd (concs ts)
    concs (Fu fs)           = rd (concs fs)
    concs (Fi fs)           = rd (concs fs)
    concs (K0 e')           = rd (concs e')
    concs (K1 e')           = rd (concs e')
    concs (Cp e')           = rd (concs e')

    mors (Tm mph)           = mors (makeInline mph)
    mors (Tc f)             = mors f
    mors (F ts)             = mors ts -- voor a;-b;c hoeft geen extra mors rond b nodig te zijn
    mors (Fd [])            = undefined                    
    mors (Fd ts@(_:t))      = rd (mors ts ++ (concat) [mors (source c)|c<-t])
    mors (Fu fs)            = mors fs
    mors (Fi fs)            = mors fs -- voor a /\ -b hoeft geen extra mors rond b nodig te zijn
    mors (K0 e')            = mors e'
    mors (K1 e')            = mors e'
    mors (Cp e')            = rd (mors e' ++ mors (source e')++mors (target e'))

    morlist (Tm mph)        = morlist mph
    morlist (Tc f)          = morlist f
    morlist (F ts)          = morlist ts
    morlist (Fd ts)         = morlist ts
    morlist (Fu fs)         = morlist fs
    morlist (Fi fs)         = morlist fs
    morlist (K0 e')         = morlist e'
    morlist (K1 e')         = morlist e'
    morlist (Cp e')         = morlist e'

    genE (Tm mph)           = genE mph
    genE (Tc f)             = genE f
    genE (F ts)             = genE ts
    genE (Fd ts)            = genE ts
    genE (Fu fs)            = genE fs
    genE (Fi fs)            = genE fs
    genE (K0 e')            = genE e'
    genE (K1 e')            = genE e'
    genE (Cp e')            = genE e'

    declarations (Tm mph)   = declarations mph
    declarations (Tc f)     = declarations f
    declarations (F ts)     = declarations ts
    declarations (Fd ts)    = declarations ts
    declarations (Fu fs)    = declarations fs
    declarations (Fi fs)    = declarations fs
    declarations (K0 e')    = declarations e'
    declarations (K1 e')    = declarations e'
    declarations (Cp e')    = declarations e'

    closExprs (Tc f)        = closExprs f
    closExprs (F ts)        = (rd.concat.map closExprs) ts
    closExprs (Fd ts)       = (rd.concat.map closExprs) ts
    closExprs (Fu fs)       = (rd.concat.map closExprs) fs
    closExprs (Fi fs)       = (rd.concat.map closExprs) fs
    closExprs (K0 e')       = [K0 e'] `uni` closExprs e'
    closExprs (K1 e')       = [K1 e'] `uni` closExprs e'
    closExprs (Cp e')       = closExprs e'
    closExprs _             = []

   instance Morphical Concept where
    concs        c          = [c]
    mors         c          = [I [] c c True]
    morlist      c          = [I [] c c True]
    declarations _          = []
    genE c = case c of
                C{}        -> cptgE c
                S{}        -> (<=)  ::Concept->Concept->Bool
                Anything   -> (<=)  ::Concept->Concept->Bool
                NOthing    -> (<=)  ::Concept->Concept->Bool

   instance Morphical ObjectDef where
    concs        obj = [source (objctx obj)] `uni` concs (objats obj)
    conceptDefs  _   = []
    mors         obj = mors (objctx obj) `uni` mors (objats obj) `uni` mors (target (objctx obj))  -- opletten: de expressie (objctx obj) hoort hier ook bij.
    morlist      obj = morlist (objctx obj)++morlist (objats obj)
    declarations _   = []
    closExprs    obj = closExprs (objctx obj) `uni` closExprs (objats obj)
    objDefs      obj = [obj]

   instance Morphical Morphism where
    concs mph = rd [source mph,target mph]
-- was: (Met bovenstaande hebben we ook een definitie voor Mp1)
--    concs (Mph nm pos atts (a,b) yin s)           = rd [a,b]
--    concs (I atts g s _)                          = rd [g,s]
--    concs (V atts (a,b))                          = rd [a,b]
    mors mph         = [makeInline mph]
    morlist mph      = [mph]
    genE mph = case mph of
           Mph{} -> genE (source mph)
           I{}   -> genE (mphspc mph)
           V{}   -> genE (source mph)
           Mp1{} -> undefined
    declarations mph = [makeDeclaration mph]

   instance Morphical Declaration where
    concs d = case d of
               Sgn{}     -> rd [desrc d,detgt d]
               Isn{}     -> rd [degen d,despc d]
               Iscompl{} -> [despc d]
               Vs{}      -> [despc d]        
--    concs (Sgn _ a b _ _ _ _ _ _ _ _ _)           = rd [a,b]
--    concs (Isn g s)                               = rd [g,s]
--    concs (Iscompl g s)                           = [s]
--    concs (Vs g s)                                = [s]
    mors _                                        = []
    morlist _                                     = []
    genE d = case d of
               Sgn{}     -> genE(desrc d)
               Isn{}     -> genE(despc d)
               Iscompl{} -> genE(despc d)
               Vs{}      -> genE(despc d)
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
    concs r = case r of
                Ru{rrsrt = Truth } -> concs (rrant r)   -- WAAROM??? Stef, dit is volgens mij fout: bij Truth kunnen toch andere concepten in de expressies aanwezig zijn in de lhs dan in de rhs??
                Ru{}               -> concs (rrant r) `uni` concs (rrcon r)
                Sg{}               -> concs (srsig r)
                Gc{}               -> concs (grspe r) `uni` concs (grgen r)
                Fr{}               -> concs (frdec r) `uni` concs (frcmp r)
    mors r = case r of
                Ru{rrsrt = Truth } -> mors (rrcon r)   -- WAAROM??? Stef, dit is volgens mij fout: bij Truth kunnen toch andere morphismen in de expressies aanwezig zijn in de lhs dan in de rhs??
                Ru{}               -> mors (rrant r) `uni` mors (rrcon r)
                Sg{}               -> mors (srsig r)
                Gc{}               -> mors (grspe r) `uni` mors (grgen r)
                Fr{}               -> mors (frdec r) `uni` mors (frcmp r)
    morlist r = case r of
                Ru{rrsrt = Truth } -> morlist (rrcon r)   -- WAAROM??? Stef, dit is volgens mij fout: bij Truth kunnen toch andere morphismen in de expressies aanwezig zijn in de lhs dan in de rhs??
                Ru{}               -> morlist (rrant r) ++ morlist (rrcon r)
                Sg{}               -> morlist (srsig r)
                Gc{}               -> morlist (grspe r) ++ morlist (grgen r)
                Fr{}               -> morlist (frcmp r)
    genE r = case r of
                Ru{rrsrt = Truth } -> genE (rrcon r)   -- WAAROM??? Stef, dit is volgens mij fout: bij Truth kunnen toch andere morphismen in de expressies aanwezig zijn in de lhs dan in de rhs??
                Ru{}               -> genE [(rrant r),(rrcon r)]
                Sg{}               -> genE (srsig r)
                Gc{}               -> genE (grspe r) 
                Fr{}               -> genE (frcmp r)
    declarations r = case r of
                Ru{rrsrt = Truth } -> declarations (rrcon r)   -- WAAROM??? Stef, dit is volgens mij fout: bij Truth kunnen toch andere morphismen in de expressies aanwezig zijn in de lhs dan in de rhs??
                Ru{}               -> declarations [(rrant r),(rrcon r)]
                Sg{}               -> [srrel r] `uni` declarations (srsig r)
                Gc{}               -> declarations (grspe r) 
                Fr{}               -> declarations (frcmp r)
    closExprs r = case r of
                Ru{rrsrt = Truth } -> closExprs (rrcon r)   -- WAAROM??? Stef, dit is volgens mij fout: bij Truth kunnen toch andere morphismen in de expressies aanwezig zijn in de lhs dan in de rhs??
                Ru{}               -> closExprs (rrant r) `uni` closExprs (rrcon r)
                Sg{}               -> closExprs (srsig r)
                Gc{}               -> closExprs (grgen r) 
                Fr{}               -> [frcmp r]



   instance Morphical Gen where
    concs g        = rd [gengen g,genspc g]  
    mors g         = [I{ mphats=[]
                      , mphgen = gengen g
                      , mphspc = genspc g
                      , mphyin = True
                      }]                         
    morlist g      = mors g
    genE g         = genE (genspc g)
    declarations _ = []

    
> module CC_aux 
>            ( ShowLang(showLang)
>            , Architecture(Arch),Context(Ctx)
>            , Pattern(Pat)
>            , Declaration(Sgn, Vs, Isn, Iscompl)
>            , ConceptDef(Cd)
>            , ObjectDef(Obj), ObjDefs, Attribute(Att), Attributes, KeyDef(Kd), KeyDefs, Object(objects)
>            , Rule(Ru,Sg,Gc)
>            , makeMph
>            , Gen(G)
>            , Pairs
>            , Morphism(Mph,I,V,Mp1), v, mIs, sIs
>            , Concept(Anything,NOthing,C,S)
>            , Prop(Uni,Tot,Inj,Sur,Sym,Asy,Trn,Rfx,Aut)
>            , Expressions
>            , Expression(Fu,Fi,Fd,Tc,F,Tm,K0,K1,Cp)
>            , posNone
>            , pKey_pos
>            , showS, showADL
>            , flp
>            , pVarid_val_pos
>            , Numbered(nr,pos)
>            , normExpr
>            , isSgn
>            , fEmpty
>            , oneMorphism
>            , single
>            , Morphic(source, target, sign, multiplicities, flp, isIdent, isMph, isNot, isTrue, isFalse, isSignal, singleton, equiv, typeUniq)
>            , Morphical( concs, conceptdefs, mors, morlist, declarations, genE, closExprs, objDefs )
>            , Language( rules, multRules, signals, specs, patterns, isa )
>            , inline,idsOnly,explain 
>            , Lang(English,Dutch)
>            , applyM, declaration, plural, source, target
>            , glb, lub, sur, inj, fun, tot, sign
>            , multiplicities, ruleType, antecedent, mkVar
>            , Calc(calc)
>            , ShowHS(showHS,showADL)
>            , consequent, order, isNeg, isPos, isNeg, notCp
>            , mIs
>            , isMph, isProperty, 
>            , union

>            , Pop(update)
>            , Gens, Declarations, GenR, Contexts
>            , contents, put_gE, makeConceptSpace, pMeaning
>            , FilePos(FilePos), ConceptDefs, Concepts
>            , Paire, Rules, Morphisms, Patterns
>            , anything, shSigns, gEtabG
>            , KeyDefs, keys, cpu
>            , Language
>            , isFunction, isFlpFunction

>            , wrld, src, trg, uncomp
>            , conts, cod, clearG, dom, subst

>            , Key, showFullRelName
>  ) where
>  import Char (toLower)
>  import UU_Scanner
>  import UU_Parsing
>  import CommonClasses ( Identified(name)
>                        ,Collection (uni,isc,(>-),empty)  )
>  import Auxiliaries  
>          ( sort', chain, rd , rEncode, commaEng, clos1, diag
>           ,eqCl, sord, eqClass, rd', enumerate, unCap)
>  import Classification 
>            ( Classification(),preCl,mapCl
>            )
>  import Typology ( Inheritance(Isa), Typologic)

TODO:
 - Onderscheid tussen signaal en regel maken in de datastructuur van regel.
 - rules alleen regels op laten leveren, dus geen signalen.
 - declarations geeft precies de gedeclareerde relaties
 - toestaan van signalen met namen van declraties en met dubbele namen (grondig testen!).

>  data Lang = Dutch | English deriving (Show, Eq)
>  class ShowLang a where
>   showLang :: Lang -> a -> String

>  data ObjectDef = Obj String         -- view name of the object definition (i.e. class name) (The CSL name is in the concept, third argument)
>                       FilePos        -- position of the object definition in the file containing the ADL sourcecode
>                       Concept        -- the concept of which objects will be an instance. (The view name is the string, first argument)
>                       [Attribute]    -- So in its entirety: Obj nm pos c ats
>                   deriving Show
>  concept (Obj nm pos c ats) = c

>  type ObjDefs = [ObjectDef]
>  data Attribute = Att String         -- view name of the attribute definition
>                       FilePos        -- position of the object definition in the file containing the ADL sourcecode
>                       Concept        -- the concept which is the target of the attribute. Must be equal to the target of the expression.
>                       Expression     -- So in its entirety: Att nm pos c e
>                      deriving (Eq,Show)
>  type Attributes = [Attribute]
>  instance Identified Attribute where
>   name (Att nm pos c e) = nm

>  data Architecture = Arch Contexts -- deriving Show
>  data Concept      = C String GenR [String] | -- C nm gE cs represents the set of instances cs by name nm.
>                      S String GenR [String] | -- S nm gE cs is a singleton concept: I[nm]=V[nm]
>                      Anything               |
>                      NOthing

>  type Concepts  = [Concept]
>  instance Eq Concept where
>   C a _ _ == C b _ _ = a==b
>   S a _ _ == S b _ _ = a==b
>   Anything == Anything = True
>   NOthing == NOthing = True
>   _ == _ = False

>  instance Ord Concept where
>   NOthing <= b  = False
>   a <= NOthing  = True
>   Anything <= b = True
>   a <= Anything = False
>   a@(C _ gE _) <= b = a `gE` b
>   a@(S _ gE _) <= b = a `gE` b 

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

>  type GenR = Concept->Concept->Bool

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

>  data KeyDef = Kd FilePos      -- the position of this definition in the text of the ADL source (filename, line number and column number).
>                   String       -- the name (or label) of this Key. The label has no meaning, but is used in the generated user interface if it is not an empty string.
>                   Concept      -- the concept, which is an entity, of which this is a key
>                   [Attribute]  -- the constituent attributes (i.e. name/expression pairs) of this key.
>                deriving (Eq,Show)
>  type KeyDefs = [KeyDef]

>  data ConceptDef = Cd FilePos  -- the position of this definition in the text of the ADL source (filename, line number and column number).
>                       String   -- the name of this concept. If there is no such concept, the conceptdefinition is ignored.
>                       String   -- the textual definition of this concept.
>                       String   -- a label meant to identify the source of the definition. (useful as LaTeX' symbolic reference)
>                    deriving Show
>  instance Eq ConceptDef where
>   Cd _ n _ _ == Cd _ m _ _ = n==m
>  instance Identified ConceptDef where
>   name (Cd _ n _ _) = n

>  type ConceptDefs = [ConceptDef]
>  data Context   = Ctx String                    -- name of this context
>                       [String]                  -- the list of context names of contexts whose rules are imported
>                       (Inheritance Concept)     -- a data structure containing the generalization structure of concepts
>                       [Classification Context]  -- a tree, being the transitive closure of the 'extends' (see formal definition) relation.
>                       Patterns                  -- a list of patterns defined in this context
>                       Declarations              -- a list of declarations defined in this context, outside the scope of patterns
>                       ConceptDefs               -- a list of concept definitions defined in this context, outside the scope of patterns
>                       KeyDefs                   -- a list of key definitions defined in this context, outside the scope of patterns
>                       ObjDefs                   -- a list of key definitions defined in this context, outside the scope of patterns
>               --    deriving Show -- just for testing
>--  instance Eq Context where
>--   Ctx nm _ _ _ _ _ _ _ _ == Ctx nm' _ _ _ _ _ _ _ _ = nm == nm'
>  type Contexts  = [Context]
>  type Paire     = [String]
>  src, trg      :: Paire -> String
>  src xs         = if null xs then error ("(module CC_aux) Fatal: src []") else head xs
>  trg xs         = if null xs then error ("(module CC_aux) Fatal: trg []") else last xs
>  type Pairs     = [Paire]
>  data Gen       = G Concept             -- generic concept
>                     Concept             -- specific concept
>                   deriving Eq
>  type Gens      = [Gen]
>  data Morphism  = Mph String            -- the name of the morphism. This is the same name as
>                                         --     the relation that is bound to the morphism.
>                       FilePos           -- the position of the rule in which the morphism occurs
>                       [Concept]         -- the attributes specified inline
>                       (Concept,Concept) -- the allocated type. Together with the name, this forms the declaration.
>                       Bool              -- the 'yin' factor. If true, a declaration is bound in the same direction as the morphism. If false, binding occurs in the opposite direction.
>                       Declaration       -- the declaration bound to this morphism.
>                 | I   [Concept]         -- the (optional) attribute specified inline. ADL syntax allows at most one concept in this list.
>                       Concept           -- the generic concept
>                       Concept           -- the specific concept
>                       Bool              -- the 'yin' factor. If true, the specific concept is source and the generic concept is target. If false, the other way around.
>                 | V   [Concept]         -- the (optional) attributes specified inline.
>                       (Concept,Concept) -- the allocated type.
>                 | Mp1 String            -- the value of the one morphism
>                       Concept           -- the allocated type.

                   deriving Ord

>  v (a,b) = Tm (V [] (a,b))
>  mIs c = I [] c c True
>  sIs c = Isn c c

>  type Morphisms = [Morphism]
>  data Declaration = Sgn String  -- the name of the declaration
>                       Concept -- the source concept of the declaration
>                       Concept -- the target concept of the declaration
>                       [Prop]  -- the multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx)
>                       String  -- three strings, which form the pragma. E.g. if pragma consists of the three strings: "Person ", " is married to person ", and " in Vegas."
>                       String  --    then a tuple ("Peter","Jane") in the list of links means that Person Peter is married to person Jane in Vegas.
>                       String
>                       [Paire] -- the list of tuples, of which the relation consists.
>                       String  -- the explanation
>                       FilePos -- the position in the ADL source file where this declaration is declared.
>                       Int     -- a unique number that can be used to identify the relation
>                       Bool    -- if true, this is a signal relation; otherwise it is an ordinary relation.
>                 | Isn Concept Concept
>                 | Iscompl Concept Concept
>                 | Vs Concept Concept

                   deriving Ord

>  dom, cod :: Declaration -> [String]
>  dom s = rd [src l| l<-contents s]
>  cod s = rd [trg l| l<-contents s]
>  type Declarations = [Declaration]
>  type Patterns  = [Pattern]
>  data Prop      = Uni          -- univalent
>                 | Inj          -- injective
>                 | Sur          -- surjective
>                 | Tot          -- total
>                 | Sym          -- symmetric
>                 | Asy          -- antisymmetric
>                 | Trn          -- transitive
>                 | Rfx          -- reflexive
>                 | Aut          -- calculate contents automatically if possible
>                   deriving (Eq,Ord)
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

> -- Ru c antc p cons cpu expla sgn nr pn
>  data Rule      = Ru Char              -- 'I' if this is an implication, 'E' if this is an equivalence, 'A' if this is an ALWAYS expression.
>                      Expression        -- antecedent
>                      FilePos           -- position in the ADL file
>                      Expression        -- consequent
>                      Expressions       -- cpu. This is a list of subexpressions, which must be computed.
>                      String            -- explanation
>                      (Concept,Concept) -- type
>                      Int               -- rule number
>                      String            -- name of pattern in which it was defined.
> -- Sg p rule expla sgn nr pn signal
>                 | Sg FilePos           -- position in the ADL file
>                      Rule              -- the rule to be signalled
>                      String            -- explanation
>                      (Concept,Concept) -- type
>                      Int               -- rule number
>                      String            -- name of pattern in which it was defined.
>                      Declaration       -- the signal relation
>                 | Gc FilePos           -- position in the ADL file
>                      Morphism          -- specific
>                      Expression        -- generic
>                      Expressions       -- cpu. This is a list of subexpressions, which must be computed.
>                      (Concept,Concept) -- declaration
>                      Int               -- rule number
>                      String            -- name of pattern in which it was defined.
> -- Fr t d expr pn  -- represents an automatic computation, such as * or +.
>                 | Fr AutType           -- the type of automatic computation
>                      Declaration       -- where the result is to be stored
>                      Expression        -- expression to be computed
>                      String            -- name of pattern in which it was defined.
>                   deriving (Eq,Show)
>  data AutType = Clos0 | Clos1 deriving (Eq,Show)

  instance Eq Rule where
   Ru c antc _ cons _ sgn nr pn == Ru c' antc' _ cons' _ sgn' nr' pn'
    | nr==0 || nr'==0 = c==c' && sgn==sgn' && (if c=='A'&&c'=='A' then True else antc==antc') && cons==cons'
    | otherwise       = nr==nr' && pn==pn
   Gc _ m expr sgn nr pn _ _ == Gc _ m' expr' sgn' nr' pn'
    | nr==0 || nr'==0 = sgn==sgn' && m==m' && expr==expr'
    | otherwise       = nr==nr' && pn==pn
   _ == _ = False

>  ruleType    (Ru c _ _ _ _ _ _ _ _) = c
>  ruleType    (Sg _ rule _ _ _ _ _)  = ruleType rule
>  ruleType    (Gc _ _ _ _ _ _ _)     = 'g'
>  ruleType    (Fr _ _ _ _)           = 'f'
>  antecedent r@(Ru 'A' _ _ _ _ _ _ _ _) = error ("(Module CC_aux:) illegal call to antecedent of rule "++showADL r)
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

>  makeMph d = Mph (name d) (CC_aux.pos d) [] (sign d) True d

renumberRule gives back the rule in the second argument, only numbered by the first
renumberRules gives back an array of rules as specified by the second argument, renumbered from n to n+length(r:rs)

>  renumberRule n r@(Ru 'A' b c d e f g _ h) = Ru 'A' (error ("(Module CC_aux:) illegal call to antecedent in renumberRule ("++showADL r++")")) c d e f g n h
>  renumberRule n (Ru a b c d e f g _ h)   = Ru a b c d e f g n h
>  renumberRule n (Sg p rule c d _ f g)    = Sg p (renumberRule n rule) c d n f g
>  renumberRule n (Gc a b c d e _ f)       = Gc a b c d e n f
>  renumberRule n r                        = r
>  renumberRules n (r:rs) = (renumberRule n r):renumberRules (n+1) rs
>  renumberRules _ [] = []

>  type Rules     = [Rule]
>  data Pattern   = Pat String             -- name of this pattern
>                       Rules              -- list of rules defined in this pattern
>                       Gens               -- list of generalizations defined in this pattern
>                       Declarations       -- list of declarations defined in this pattern
>                       ConceptDefs        -- list of concept definitions defined in this pattern
>                       KeyDefs            -- list of key definitions defined in this pattern
>                   deriving Show
>  data Expression  = Tm Morphism          -- simple morphism, possibly conversed     ~
>                   | Tc Expression        -- bracketed expression                 ( ... )
>                   | F Expressions        -- composition                             ;
>                   | Fd Expressions       -- relative addition                       !
>                   | Fi Expressions       -- intersection                            /\
>                   | Fu Expressions       -- union                                   \/
>                   | K0 Expression        -- Reflexive and transitive closure        *
>                   | K1 Expression        -- Transitive closure                      +
>                   | Cp Expression        -- Complement                              -

>  instance Eq Expression where
>   F  ts == F  ts' = ts==ts'
>   Fd ts == Fd ts' = ts==ts'
>   Fu fs == Fu fs' = rd fs==rd fs'
>   Fi fs == Fi fs' = rd fs==rd fs'
>   Cp e  == Cp e'  = e==e'
>   K0 e  == K0 e'  = e==e'
>   K1 e  == K1 e'  = e==e'
>   Tm m  == Tm m'  = m==m'
>   Tc e  == e'     = e==e'
>   e     == Tc e'  = e==e'
>   _     == _      = False

>  type Expressions = [Expression]

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
>   | otherwise          = error("Fatal (module CC_aux): Cannot make an expression of "++showHS rule)

>  class Numbered a where
>   nr :: a->Int
>   pos :: a->FilePos
>   nr x = nr (CC_aux.pos x)

>  instance Numbered Attribute where
>   pos (Att _ p _ _) = p

>  instance Numbered ObjectDef where
>   pos (Obj _ p _ _) = p

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
>   nr m = nr (declaration m)

>  instance Numbered Declaration where
>   pos (Sgn _ _ _ _ _ _ _ _ _ p _ _) = p
>   pos d                             = posNone
>   nr (Sgn _ _ _ _ _ _ _ _ _ _ n _)  = n
>   nr d                              = 0

>  instance Numbered Expression where
>   pos (Tm m)  = CC_aux.pos m
>   pos (Tc f)  = CC_aux.pos f
>   pos (F ts)  = if not (null ts) then CC_aux.pos (head ts) else error "(module CC_aux) !!Software error 813. Please submit a complete bug report to your dealer"
>   pos (Fd ts) = if not (null ts) then CC_aux.pos (head ts) else error "(module CC_aux) !!Software error 814. Please submit a complete bug report to your dealer"
>   pos (Fu fs) = if not (null fs) then CC_aux.pos (head fs) else error "(module CC_aux) !!Software error 815. Please submit a complete bug report to your dealer"
>   pos (Fi fs) = if not (null fs) then CC_aux.pos (head fs) else error "(module CC_aux) !!Software error 816. Please submit a complete bug report to your dealer"
>   pos (K0 e)  = CC_aux.pos e
>   pos (K1 e)  = CC_aux.pos e
>   pos (Cp e)  = CC_aux.pos e

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
>   explain (C expla _ _) = expla
>   explain (S expla _ _) = expla
>   explain NOthing       = "Nothing"
>   explain Anything      = "Anything"

>  class Conceptual a where
>   conts      :: a -> [String]                   -- the set of all objects in a concept

>  instance Conceptual a => Conceptual [a] where
>   conts                                         = rd . concat . map conts

>  instance Conceptual a => Conceptual (Classification a) where
>   conts                                         = rd . concat . map conts . preCl

>  instance Conceptual Concept where
>   conts (C _ _ os) = os
>   conts (S _ _ os) = os
>   conts Anything   = error ("(module CC_aux) Fatal: Anything is Everything...")
>   conts NOthing    = error ("(module CC_aux) Fatal: NOthing is not very much...")

>  class Morphical a where
>   concs        :: a -> [Concept]                  -- the set of all concepts used in data structure a
>   conceptdefs  :: a -> [ConceptDef]               -- the set of all concept definitions in the data structure
>   conceptdefs x = []
>   mors         :: a -> [Morphism]                 -- the set of all morphisms used within data structure a
>   morlist      :: a -> [Morphism]                 -- the list of all morphisms used within data structure a
>   morlist = mors
>   declarations :: a -> [Declaration]
>--   declarations x  = rd [declaration m|m<-mors x]
>   genE         :: a -> GenR
>   genE x        = if null cx then (==) else head cx where cx = [gE|C _ gE _<-concs x]++[gE|S _ gE _<-concs x]
>   closExprs    :: a -> [Expression]
>   closExprs s   = []
>   objDefs      :: a -> ObjDefs
>   objDefs s     = []

>  instance Morphical a => Morphical [a] where
>   concs                                         = rd . concat . map concs
>   conceptdefs                                   = rd . concat . map conceptdefs
>   mors                                          = rd . concat . map mors
>   morlist                                       =      concat . map morlist
>   declarations                                  = rd . concat . map declarations
>   closExprs                                     = rd . concat . map closExprs
>   objDefs                                       =      concat . map objDefs

>  instance Morphical Concept where
>   concs        c                                = [c]
>   mors         c                                = [I [] c c True]
>   declarations c                                = []
>   genE         (C c gE cs)                      = gE
>   genE         (S c gE cs)                      = gE
>   genE         Anything                         = (<=)::Concept->Concept->Bool
>   genE         NOthing                          = (<=)::Concept->Concept->Bool

>  instance Morphical a => Morphical (Classification a) where
>   concs                                         = rd . concat . map concs . preCl
>   conceptdefs                                   = rd . concat . map conceptdefs . preCl
>   mors                                          = rd . concat . map mors . preCl
>   morlist                                       =      concat . map morlist . preCl
>   declarations                                  = rd . concat . map declarations . preCl
>   closExprs                                     = rd . concat . map closExprs . preCl

>  instance Morphical Declaration where
>   concs (Sgn _ a b _ _ _ _ _ _ _ _ _)             = rd [a,b]
>   concs (Isn g s)                               = rd [g,s]
>   concs (Iscompl g s)                           = [s]
>   concs (Vs g s)                                = [s]
>   mors s                                        = []
>   genE (Sgn nm a b _ _ _ _ _ _ _ _ _)             = genE a
>   genE (Isn g s)                                = genE s
>   genE (Iscompl g s)                            = genE s
>   genE (Vs g s)                                 = genE s
>   declarations s                                = [s]

>  instance Morphical Morphism where
>   concs (Mph nm pos atts (a,b) yin s)           = rd [a,b]
>   concs (I atts g s _)                          = rd [g,s]
>   concs (V atts (a,b))                          = rd [a,b]
>   mors m                                        = [m]
>   genE (Mph nm pos atts (a,b) _ s)              = genE a
>   genE (I atts g s _)                           = genE s
>   genE (V atts (a,b))                           = genE a
>   declarations m                                = [declaration m]

>  declaration :: Morphism -> Declaration
>  declaration (Mph _ _ _ _ _ s) = s
>  declaration (I atts g s yin)  = Isn g s
>  declaration (V atts (a,b))    = Vs a b
>  declaration (Mp1 s c)         = Isn c c

>  instance Morphical Gen where
>   concs (G g s)                                 = rd [g,s]
>   mors (G g s)                                  = [I [] g s True]
>   genE (G g s)                                  = genE s

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

>   mors (Tm m)                                   = mors m
>   mors (Tc f)                                   = mors f
>   mors (F ts)                                   = mors ts
>   mors (Fd ts)                                  = mors ts
>   mors (Fu fs)                                  = mors fs
>   mors (Fi fs)                                  = mors fs
>   mors (K0 e)                                   = mors e
>   mors (K1 e)                                   = mors e
>   mors (Cp e)                                   = mors e

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
>   closExprs (K0 e)                              = K0 e: closExprs e
>   closExprs (K1 e)                              = K1 e: closExprs e
>   closExprs (Cp e)                              = closExprs e
>   closExprs _                                   = []

>  instance Morphical Rule where
>   concs (Ru c antc _ cons _ _ _ _ _)     = if c=='A' then rd (concs cons) else rd (concs antc++concs cons)
>   concs (Sg _ rule _ _ _ _ _)            = concs rule
>   concs (Gc _ m expr _ _ _ _)            = rd (concs m++concs expr)
>   concs (Fr _ d expr _)                  = rd (concs d++concs expr)
>   mors (Ru c antc _ cons _ _ _ _ _)      = if c=='A' then rd (mors cons) else rd (mors antc++mors cons)
>   mors (Sg _ rule _ _ _ _ _)             = mors rule
>   mors (Gc _ m expr _ _ _ _)             = rd (mors m++mors expr)
>   mors (Fr _ d expr _)                   = rd (mors d++mors expr)
>   morlist (Ru c antc _ cons _ _ _ _ _)   = if c=='A' then morlist cons else morlist antc++morlist cons
>   morlist (Sg _ rule _ _ _ _ _)          = morlist rule
>   morlist (Gc _ m expr _ _ _ _)          = morlist m++morlist expr
>   morlist (Fr _ _ expr _)                = morlist expr
>   genE (Ru c antc _ cons  _ _ _ _ _)     = if c=='A' then genE cons else genE [antc,cons]
>   genE (Sg _ rule _ _ _ _ _)             = genE rule
>   genE (Gc _ m expr _ _ _ _)             = genE m
>   genE (Fr _ _ expr _)                   = genE expr
>   declarations (Ru c antc _ cons _ _ _ _ _) = if c=='A' then declarations cons else declarations [antc,cons]
>   declarations (Sg _ rule _ _ _ _ d)        = rd (d: declarations rule)
>   declarations (Gc _ m expr _ _ _ _)        = declarations m
>   declarations (Fr _ _ expr _)              = declarations expr
>   closExprs (Ru c antc _ cons _ _ _ _ _) = if c=='A' then rd (closExprs cons) else rd (closExprs antc++closExprs cons)
>   closExprs (Sg _ rule _ _ _ _ _)        = closExprs rule
>   closExprs (Gc _ m expr  _ _ _ _)       = rd (closExprs expr)
>   closExprs (Fr _ _ expr _)              = [expr]

>  instance Morphical Pattern where
>   concs (Pat nm rs gen pms cs ks)                  = rd (concs rs++concs gen++concs pms)
>   conceptdefs (Pat nm rs gen pms cs ks)            = cs
>   mors (Pat nm rs gen pms cs ks)                   = mors rs
>   morlist (Pat nm rs gen pms cs ks)                = morlist rs
>   declarations (Pat nm rs parChds pms cs ks)       = rd pms
>   genE  (Pat nm rs parChds pms cs ks)              = genE (pms++declarations (signals rs))
>   closExprs (Pat nm rs gen pms cs ks)              = rd (closExprs rs)

>  instance Morphical Context where
>   concs        (Ctx nm on isa world dc ss cs ks os) = rd (concs ss++concs dc)
>   conceptdefs  (Ctx nm on isa world dc ss cs ks os) = cs
>   mors         (Ctx nm on isa world dc ss cs ks os) = rd (mors dc++mors os)
>   morlist      (Ctx nm on isa world dc ss cs ks os) = morlist dc++morlist os
>   declarations (Ctx nm on isa world dc ss cs ks os) = rd ss
>   closExprs    (Ctx nm on isa world dc ss cs ks os) = rd (closExprs dc++closExprs os)
>   objDefs      (Ctx nm on isa world dc ss cs ks os) = os

>  instance Morphical ObjectDef where
>   concs        (Obj nm pos c ats) = rd (c: concs ats)
>   conceptdefs  (Obj nm pos c ats) = conceptdefs ats
>   mors         (Obj nm pos c ats) = rd (mors ats)
>   morlist      (Obj nm pos c ats) = morlist ats
>   declarations (Obj nm pos c ats) = []
>   closExprs    (Obj nm pos c ats) = rd (closExprs ats)
>   objDefs      o              = [o]

>  instance Morphical Attribute where
>   concs        (Att nm pos c e) = rd (c: concs e)
>   conceptdefs  (Att nm pos c e) = []
>   mors         (Att nm pos c e) = mors e
>   morlist      (Att nm pos c e) = morlist e
>   declarations (Att nm pos c e) = []
>   closExprs    (Att nm pos c e) = closExprs e

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
>      else r -- error ("(module CC_aux) Fatal: cannot execute:   subst (m,f) r\nwith m="++show m++"\n     f="++show f++"\nand  r="++showADL r++"\n"++showHS r++"\nbecause "++show (sign antc')++" `order` "++show (sign cons')++" is False.\n"++gEtabG gEq [c| (a,b)<-[sign antc',sign cons'], c<-[a,b]])
>      where antc' = subst (m,f) antc
>            cons' = subst (m,f) cons
>   subst (m,f) (Sg p rule expla sgn nr pn signal)
>    = Sg p r' expla (sign r') nr pn signal
>      where r'= subst (m,f) rule
>   subst (m,f) (Gc pos m' expr cpu sgn nr pn)
>    = Gc pos m' expr' cpu (sign expr') nr pn
>      where expr' = subst (m,f) expr

>  instance Show Prop where
>   showsPrec p Uni = showString "UNI"
>   showsPrec p Inj = showString "INJ"
>   showsPrec p Sur = showString "SUR"
>   showsPrec p Tot = showString "TOT"
>   showsPrec p Sym = showString "SYM"
>   showsPrec p Asy = showString "ASY"
>   showsPrec p Trn = showString "TRN"
>   showsPrec p Rfx = showString "RFX"
>   showsPrec p Aut = showString "AUT"

>{-  instance Show Context where
>   showsPrec p (Ctx nm on isa world dc ss cs ks os)
>    = showString ("CONTEXT "++nm++
>                  (if on==[] then "" else " EXTENDS "++chain ", " on)++"\n"++
>                  chain "\n\n" (map show dc)++"\n"++
>                  chain "\n" (map show ss)++++"\n"++
>                  chain "\n" (map show cs)++++"\n"++
>                  chain "\n" (map show ks)++++"\n"++
>                  chain "\n" (map show os)++"\nENDCONTEXT" ) -}

The function showHS prints structures as haskell source, which is intended for testing.

>  class ShowHS a where
>   showHS, showADL :: a -> String

>  instance ShowHS Prop where
>   showHS Uni  = "Uni"
>   showHS Inj  = "Inj"
>   showHS Sur  = "Sur"
>   showHS Tot  = "Tot"
>   showHS Sym  = "Sym"
>   showHS Asy  = "Asy"
>   showHS Trn  = "Trn"
>   showHS Rfx  = "Rfx"
>   showHS Aut  = "AUT"
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
>   showHS  = chain "\n".map showHS
>   showADL = chain "\n".map showADL

>  instance ShowHS Context where
>-- TODO: showHS should generate valid Haskell code for the entire pattern. Right now, it doesn't
>   showHS (Ctx nm on isa world dc ss cs ks os)
>    = nlHs++"ctx_"++nm++"\n>   = Ctx "++show nm++" "++show on++" isa (genEq (typology isa)) []"++
>      ind++showL ["pat_"++name p|p<-dc]++
>      ind++showL ["mor_"++name s++name(source s)++name(target s)|s<-ss]++
>      init nlHs'++"where"++nlHs'++
>      "isa = "++showHS isa++
>      concat [nlHs'++showHS s|s<-ss]++"\n"++
>      showHS dc++
>      concat ["\n\nDeclarations from "++name pat++"\n"++concat[nlHs'++showHS s|s<-declarations pat]|pat<-dc]
>      where nlHs = "\n>  "; ind = nlHs++"       "; nlHs' = nlHs++"    "
>   showADL (Ctx nm on isa world dc ss cs ks os)
>    = "CONTEXT\n" ++
>      chain "\n\n" (map showADL dc) ++ "\n\n" ++
>      chain "\n" (map showADL ss) ++ "\n\n" ++
>--      chain "\n" (map showADL cs) ++ "\n\n" ++
>--      chain "\n" (map showADL ks) ++
>      "\nENDCONTEXT"

>  instance ShowHS Pattern where
>-- TODO: showHS should generate valid Haskell code for the entire pattern. Right now, it doesn't
>   showHS (Pat nm rs gen pss cs ks)
>    = nlHs++"pat_"++nm++nlHs'++"= Pat "++show nm++
>      (if null rs then " []" else ind++"[ "++chain (ind++", ") [showHS r| r<-rs]++ind++"]")++
>      (if null gen   then " []" else ind++"[ "++chain (ind++", ") [showHS g| g<-gen] ++ind++"]")++
>      (if null gen   then " []" else ind++"[ "++chain (ind++", ") ["mor_"++name s++name(source s)++name(target s)| s<-pss] ++ind++"]")++
>      init nlHs'
>      where nlHs = "\n>      "; ind = nlHs++"       "; nlHs' = nlHs++"    "
>   showADL (Pat nm rs gen pss cs ks)
>    = "PATTERN\n" ++
>      chain "\n" (map showADL pss) ++
>      chain "\n\n" (map showADL rs) ++ "\n\n" ++
>--      chain "\n" (map showADL cs) ++ "\n\n" ++
>--      chain "\n" (map showADL ks) ++
>      "\nENDPATTERN"

>  instance ShowHS Rule where
>   showHS r@(Ru 'A' _ p cons cpu expla sgn nr pn)
>    = chain " " ["Ru","'A'","("++showHS (consequent r)++")","["++chain "," (map showADL cpu)++"]",show(explain r),showSgn sgn,show nr,show pn]
>   showHS r@(Ru c antc p cons cpu expla sgn nr pn)
>    = chain " " ["Ru","'"++[c]++"'","("++showHS antc++")","posNone","("++showHS (consequent r)++")","["++chain "," (map showADL cpu)++"]",show(explain r),showSgn sgn,show nr,show pn]
>   showHS r@(Sg p rule expla sgn nr pn signal)
>    = chain " " ["SIGNAL",name signal,"ON",showHS rule] -- voor later, als rule wijzigt in expr: ,show(explain r),showSgn sgn,show nr,show pn]
>   showHS r@(Gc p m expr cpu sgn nr pn)
>    = chain " " ["Gc","posNone","("++showHS m++")","("++showHS (consequent r)++")","["++chain "," (map showADL cpu)++"]",showSgn sgn,show nr,show pn]
>   showHS r = ""
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
>   showHS (Tm m)   = "Tm ("++showHS m++") "
>   showHS (Tc f)   = "Tc ("++showHS f++") "
>   showHS (F [])   = "F [] <Id>"
>   showHS (Fd [])  = "Fd [] <nId>"
>   showHS (Fu [])  = "Fu [] <False>"
>   showHS (Fi [])  = "Fi [] <True>"
>   showHS (F ts)   = chain " " ["F",showL (map showHS ts)]
>   showHS (Fd ts)  = chain " " ["Fd",showL (map showHS ts)]
>   showHS (Fu fs)  = chain " " ["Fu",showL (map showHS fs)]
>   showHS (Fi fs)  = chain " " ["Fi",showL (map showHS fs)]
>   showHS (K0 e)   = "K0 ("++showHS e++") "
>   showHS (K1 e)   = "K1 ("++showHS e++") "
>   showHS (Cp e)   = "Cp ("++showHS e++") "

>   showADL e = show e

    showADL (Fu [Cp (F [Tm (Mph "r" <...>) ]) ,F [Tm (Mph "s" <...>) ]])
-->
    wrap 0 4 (chain "\\/" [showchar 4 f| f<-[Cp (F [Tm (Mph "r" <...>) ])
                                            ,F [Tm (Mph "s" <...>) ]
                                            ]
                          ]
             )
=
    chain "\\/" [showchar 4 f| f<-[ Cp (F [Tm (Mph "r" <...>) ])
                                  , F [Tm (Mph "s" <...>) ]
                                  ]
                ]
=
    chain "\\/" [showchar 4 (Cp (F [Tm (Mph "r" <...>) ]))
                ,showchar 4 (F [Tm (Mph "s" <...>) ])
                ]
=
    showchar 7 (F [Tm (Mph "r" <...>) ])++"-"
    ++"\\/"++
    showchar 4 (F [Tm (Mph "s" <...>) ])
=
    wrap 7 7 (chain rMul [showchar 7 t| t<-[Tm (Mph "r" <...>) ]])++"-"
    ++"\\/"++
    wrap 4 7 (chain rMul [showchar 7 t| t<-Tm (Mph "s" <...>) ]])
=
    chain rMul [showchar 7 (Tm (Mph "r" <...>))]++"-"
    ++"\\/"++
    chain rMul [showchar 7 (Tm (Mph "s" <...>))]
=
    showADL (Mph "r" <...>)++"-"
    ++"\\/"++
    showADL (Mph "s" <...>)
=
    "r" ++ "-" ++ "\\/" ++ "s"
    

>  showExpr (union,inter,rAdd,rMul,clos0,clos1,compl,lpar,rpar) e = showchar 0 e
>    where
>     wrap i j str = if i<=j then str else lpar++str++rpar
>     showchar i (Tm m)  = name m++if inline m then "" else "~"
>     showchar i (Fu []) = "-V"
>     showchar i (Fu fs) = wrap i 4 (chain union [showchar 4 f| f<-fs])
>     showchar i (Fi []) = "V"
>     showchar i (Fi fs) = wrap i 5 (chain inter [showchar 5 f| f<-fs])
>     showchar i (Fd []) = "-I"
>     showchar i (Fd ts) = wrap i 6 (chain rAdd [showchar 6 t| t<-ts])
>     showchar i (F [])  = "I"
>     showchar i (F ts)  = wrap i 7 (chain rMul [showchar 7 t| t<-ts])
>     showchar i (K0 e)  = showchar 8 e++clos0
>     showchar i (K1 e)  = showchar 8 e++clos1
>     showchar i (Cp e)  = compl++showchar 8 e
>     showchar i (Tc f)  = showchar i f

>  isFactor (Fu fs) = True
>  isFactor (Fi fs) = True
>  isFactor _ = False

>  showS m = name m++"["++show (source m)++","++show (target m)++"]"
>  showSgn (a,b) = "("++showHS a++","++showHS b++")"
>  showSign cs = "["++chain "*" (map name cs)++"]"
>  showL xs = "["++chain "," xs++"]"
>  showFullRelName m = rEncode (name m++name (source m)++name (target m))

>  instance ShowHS a => ShowHS (Inheritance a) where
>   showHS (Isa ts cs) = "Isa "++showL ["("++showHS g++","++showHS s++")"|(g,s)<-ts] ++" "++ showL (map showHS cs)
>   showADL (Isa ts cs) = ""

>  instance ShowHS Concept where
>   showHS Anything = "Anything"
>   showHS NOthing  = "NOthing"
>   showHS  c = if singleton c
>               then "S "++show (name c) -- ++" "++show (conts c)
>               else "C "++show (name c) -- ++" "++show (conts c)
>   showADL c = show (name c)

>  instance ShowHS Declaration where
>   showHS (Sgn nm a b props prL prM prR cs expla pos nr sig)
>    = chain " " ["mor_"++nm++name a++name b,"= Sgn",show nm,"("++showHS a++")","("++showHS b++")",showL(map showHS props),show prL,show prM,show prR,"[]",show expla,show pos,show nr, show sig]
>   showHS (Isn g s)
>    = "Isn ("++showHS g++") ("++showHS s++")"
>   showHS (Iscompl g s)
>    = "Iscompl ("++showHS g++") ("++showHS s++")"
>   showHS (Vs g s)
>    = "Vs ("++showHS g++") ("++showHS s++")"
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
>   showHS (Mph nm pos atts sgn@(a,b) yin s)
>    = chain " " ["Mph",show nm,(\(FilePos (_,Pos l c,_))->show (l,c)) pos,showL(map showHS atts),showSgn sgn,show yin] -- ,"mor_"++nm++name (source s)++name (target s)]
>   showHS (I atts g s yin)
>    = chain " " ["I",showL(map showHS atts),name g, name s, show yin]
>   showHS (V atts sgn)
>    = chain " " ["V",showL(map showHS atts),showSgn sgn]
>   showHS (Mp1 str sgn)
>    = chain " " ["Mp1",show str,showHS sgn]
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
>   showHS (G g s)  = "G ("++show s++") ("++show g++")"
>   showADL (G g s) = "GEN "++showADL s++" ISA "++show g

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


>  instance Show Expression where
>   showsPrec p e  = showString (showExpr ("\\/", "/\\", "!", ";", "*", "+", "-", "(", ")") e)

The following shSigns is intendes for error messages

>  shSigns [(a,b)] = "["++show a++"*"++show b++"]"
>  shSigns ss = commaEng "or" ["["++show a++"*"++show b++"]"|(a,b)<-ss]
   
>  instance Show Concept where
>   showsPrec p c = showString (name c)

This show is used in error messages. It should therefore not display the morphism's type

>  instance Show Morphism where
>   showsPrec p (Mph nm pos  []  sgn yin m) = showString (nm  {- ++"("++show a++"*"++show b++")" where (a,b)=sgn -} ++if yin then "" else "~")
>   showsPrec p (Mph nm pos atts sgn yin m) = showString (nm  {- ++"["++chain "*" (map name (rd atts))++"]" -}      ++if yin then "" else "~")
>   showsPrec p (I atts g s yin)            = showString ("I"++ (if null atts then {- ++"["++name g, (if s/=g then ","++name s else "")++"]" -} "" else show atts))
>   showsPrec p (V atts (a,b))              = showString ("V"++ (if null atts then "" else show atts))

>  instance Show Declaration where
>   showsPrec p (Sgn nm a b props prL prM prR cs expla _ _ False)
>    = showString (chain " " ([nm,"::",name a,"*",name b,show props,"PRAGMA",show prL,show prM,show prR]++if null expla then [] else ["EXPLANATION",show expla]))
>   showsPrec p (Sgn nm a b props prL prM prR cs expla _ _ True)
>    = showString (chain " " ["SIGNAL",nm,"ON (",name a,"*",name b,")"])
>   showsPrec p _
>    = showString ""

This show is used in error messages. It should therefore not display the term's type

>  instance Show Gen where
>   showsPrec p (G g s) = showString ("GEN "++show s++" ISA "++show g)

>  instance Eq Declaration where
>   s == s' = name s==name s' && source s==source s' && target s==target s'
>  instance Eq Morphism where
>--   m == m' = name m==name m' && source m==source m' && target m==target m' && yin==yin'
>   Mph nm _ _ (a,b) yin _ == Mph nm' _ _ (a',b') yin' _ = nm==nm' && yin==yin' && a==a' && b==b'
>   I _ g s yin            == I _ g' s' yin'             =            if yin==yin' then g==g' && s==s' else g==s' && s==g'
>   V _ (a,b)              == V _ (a',b')                = a==a' && b==b'
>   a == b = False

TODO: 3 lines above: V _ (a,b)
V _ _ is a Morphism, Vs _ _ is a Declaration

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
>       = if not (source f `order` target f) then error ("(module CC_aux) Fatal: no order in "++showHS f) else
>                            [[e,e]|e<-os]
>    | otherwise
>       = if null css then error ("(module CC_aux) Fatal: no terms in F "++showHS ts) else
>                            foldr1 join css
>                            where os = conts (source f `lub` target f)
>                                  css = [contents t|t<-ts, not (idsOnly t)]
>   contents f@(Fd ts)
>     = if null ts then error ("(module CC_aux) Fatal: no terms in Fd "++showHS ts) else joinD ts
>   contents (Fu fs) = if null fs then [] else
>                      (foldr1 uni .map contents) fs
>   contents (Fi fs) = if null fs then [] else
>                      (foldr1 isc .map contents) fs
>   contents (K0 e)  = clos1 (contents e) `uni` [[c,c]|c <-conts (source e `lub` target e)]
>   contents (K1 e)  = clos1 (contents e)
>   contents (Cp (Cp e)) = contents e
>   contents (Cp e)  = [[a,b]| [a,b]<-diag [] (conts (source e)) [] (conts (target e)), not ([a,b] `elem` contents e)]

>  join::[Paire]->[Paire]->[Paire]
>  join a b = rd (merge ((sort' (trg.head).eqCl trg) a)
>                       ((sort' (src.head).eqCl src) b))
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
>        upd (C c gEq os) os' = C c gEq os'
>        upd (S c gEq os) os' = S c gEq os'
>        upd c  os = c

>  class Key a where
>   keys :: a->[(Concept,String,[Attribute])]

>  instance Key Context where
>   keys (Ctx nm on isa world dc ss cs ks os) = (concat [keys p| p<-dc] ++ [(c,lbl,ats)|Kd pos lbl c ats<-ks])

>  instance Key Pattern where
>   keys (Pat nm rs gen pms cs ks) = [(c,lbl,ats)|Kd pos lbl c ats<-ks]

>  instance Key KeyDef where
>   keys (Kd pos lbl c ats) = [(c,lbl,ats)]

   instance Key ObjectDef where
    keys (Obj nm pos c ats) = [(c,nm,ats)]

>  class Object a where
>   objects :: a -> [ObjectDef]

>  instance Object Context where
>   objects (Ctx _ _ _ _ _ _ _ _ os) =  os

>  instance Object ObjectDef where
>   objects o = [o]

>  instance Object a => Object [a] where
>   objects os = concat (map objects os)


The following definition is used to compute whether a concept may display its internal code.
This may be done when there are no keys and no objects for this particular concept.

>  displayInternalCode ctx@(Ctx nm on isa world dc ss cs ks os) c
>   = null [e| (e,_,ats)<-keys ctx, e==c, not (null ats)] && null[o| o<-objects ctx, c==concept o]

TODO: transform makeConceptSpace to makeConceptSpace :: [Declaration] -> Concepts

>  class Pop a where
>   put_gE     :: GenR -> Concepts  -> a -> a
>   specialize :: (Concept,Concept) -> a -> a
>   update     :: [Declaration] -> a -> a
>   update ss c = c

>  instance Pop Concept where
>   put_gE gE cs c     = h (head ([c'|c'<-cs, c==c']++[c]))
>                        where h (C c gEq os) = C c gE os
>                              h (S c gEq os) = S c gE os
>                              h x = x
>   specialize (a,b) c = if length (eqClass order [a,b,c])>1 then error ("(module CC_aux) Fatal: specialize 1 ("++show a++","++show b++") "++showHS c) else
>                        (a `glb` b) `lub` c

>  instance Pop KeyDef where
>   put_gE gE cs (Kd pos lbl c ats) = Kd pos lbl (put_gE gE cs c) (map (put_gE gE cs) ats)
>   specialize (a,b) (Kd pos lbl c ats) = Kd pos lbl (specialize (a,b) c) (map (specialize (a,b)) ats)

>  instance Pop ObjectDef where
>   put_gE gE cs (Obj nm pos c ats) = Obj nm pos (put_gE gE cs c) [put_gE gE cs a| a<-ats]
>   update ss    (Obj nm pos c ats) = Obj nm pos c [update ss    a| a<-ats]
>   specialize t (Obj nm pos c ats) = Obj nm pos c [specialize t a| a<-ats]

>  instance Pop Attribute where
>   put_gE gE cs (Att nm pos c e) = Att nm pos (put_gE gE cs c) (put_gE gE cs e)
>   update ss    (Att nm pos c e) = Att nm pos c (update ss    e)
>   specialize t (Att nm pos c e) = Att nm pos c (specialize t e)

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
>   put_gE gE cs (G g s) = G (put_gE gE cs g) (put_gE gE cs s)
>   update ss (G g s)    = G (update ss g)    (update ss s)
>   specialize t (G g s) = G (specialize t g) (specialize t s)

>  instance Pop Context where
>   put_gE gE cs (Ctx nm on isa world dc ss cs' ks os) = Ctx nm on isa (map (mapCl (put_gE gE cs)) world) (map (put_gE gE cs) dc) (map (put_gE gE cs) ss) cs' (map (put_gE gE cs) ks) (map (put_gE gE cs) os)
>   update ss    (Ctx nm on isa world dc ss' cs ks os) = Ctx nm on isa world (map (update ss) dc) (map (update ss) ss') cs (map (update ss) ks) (map (update ss) os)
>   specialize t (Ctx nm on isa world dc ss  cs ks os) = Ctx nm on isa world (map (specialize t) dc) (map (specialize t) ss) cs (map (specialize t) ks) (map (specialize t) os)

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
>  inline (Mph _ _ _ _ yin _) = yin
>  inline m = True

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
>   update ss s@(Sgn _ _ _ _ _ _ _ _ _ _ _ _)    = head ([c|c<-ss, s==c]++[s])
>   update ss s                                  = s
>   specialize (x,y) (Sgn nm a b props prL prM prR ls expla pos nr sig)
>                                                = Sgn nm x y props prL prM prR [[d,e]|[d,e]<-ls,d `elem` conts a, e `elem` conts b] expla pos nr sig
>   specialize (x,y) sg@(Isn g s)                = if x <= y then Isn x y else error ("(module CC_aux) Fatal: specialize 7 "++show (x,y)++showHS s)
>   specialize (x,y) sg@(Iscompl g s)            = if x <= y then Iscompl x y else error ("(module CC_aux) Fatal: specialize 7 "++show (x,y)++showHS s)

>  class Morphic a where
>   source, target :: a -> Concept
>   sign           :: a -> (Concept,Concept)
>   sign x = (source x,target x)
>   multiplicities :: a -> [Prop]
>   multiplicities m = []
>   flp            :: a -> a
>   isIdent        :: a -> Bool  -- > tells whether the argument is equivalent to I
>   isNot          :: a -> Bool  -- > tells whether the argument is equivalent to I-
>   isMph          :: a -> Bool
>   isTrue         :: a -> Bool  -- > tells whether the argument is equivalent to V
>   isFalse        :: a -> Bool  -- > tells whether the argument is equivalent to V-
>   isSignal       :: a -> Bool  -- > tells whether the argument refers to a signal
>   singleton      :: a -> Bool  -- > tells whether V=I
>   singleton e     = isIdent e && isTrue e
>   equiv          :: a -> a -> Bool
>   equiv m m' = source m==source m'&&target m==target m' || source m==target m'&&target m==source m'
>   typeUniq :: a -> Bool -- this says whether the type of 'a' and all of its constituent parts is defined (i.e. not "Anything")

>  instance Morphic Attribute where
>   source (Att nm pos c e) = source e
>   target (Att nm pos c e) = c
>   sign   (Att nm pos c e) = (c, source e)
>   multiplicities (Att nm pos c e) = multiplicities e
>   flp a = error ("Cannot flip an attribute: "++show a)
>   isIdent   (Att nm pos c e) = isIdent   e
>   isNot     (Att nm pos c e) = isNot     e
>   isMph     (Att nm pos c e) = isMph     e
>   isTrue    (Att nm pos c e) = isTrue    e
>   isFalse   (Att nm pos c e) = isFalse   e
>   isSignal a = error ("Cannot test an attribute for being a SIGNAL: "++show a)
>   singleton (Att nm pos c e) = singleton e
>   typeUniq (Att nm pos c e) = typeUniq c && typeUniq e
>  idsOnly x = and [isIdent m| m<-mors x]

>  class Morphics a where
>   anything       :: a -> Bool

>  instance Morphics a => Morphics [a] where
>   anything xs = and (map anything xs)
>  instance Morphics Concept where
>   anything Anything = True
>   anything _ = False
>  instance (Morphics a,Morphics b) => Morphics (a,b) where
>   anything (x,y) = anything x && anything y

>  instance Morphic Concept where
>   source c = c
>   target c = c
>--   sign c = (c,c)
>   multiplicities c = [Uni,Tot,Sur,Inj,Sym,Trn,Rfx]
>   flp c = c
>   isIdent c = True    -- > tells whether the argument is equivalent to I
>   isNot c   = False   -- > tells whether the argument is equivalent to I-
>   isMph c = False
>   isTrue c = singleton c
>   isFalse c = False
>   isSignal c = False
>   singleton (S _ _ _) = True
>   singleton _ = False
>   typeUniq Anything = False
>   typeUniq _ = True

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
>   isNot (Iscompl _ _)                          = True   -- > tells whether the argument is equivalent to I-
>   isNot _                                      = False
>   isTrue (Vs _ _)                              = True
>   isTrue _                                     = False
>   isFalse _                                    = False
>   isSignal (Sgn _ _ _ _ _ _ _ _ _ _ _ s)       = s
>   isSignal _                                   = False
>   isMph (Sgn _ a b _ _ _ _ _ _ _ _ _)          = True
>   isMph _                                      = False
>   typeUniq (Sgn _ a b _ _ _ _ _ _ _ _ _)       = typeUniq a && typeUniq a
>   typeUniq (Isn g s)                           = typeUniq g && typeUniq s
>   typeUniq (Iscompl g s)                       = typeUniq g && typeUniq s
>   typeUniq (Vs g s)                            = typeUniq g && typeUniq s

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
>   sign (F ts)            = if null ts then error ("(module CC_aux) Fatal: no terms in sign (F "++showHS ts++")") else
>                            foldr1 jnSign (map sign ts)
>                            where (s,t) `jnSign` (s',t') = (s,t')
>   sign (Fd ts)           = if null ts then error ("(module CC_aux) Fatal: no terms in sign (Fd "++showHS ts++")") else
>                            foldr1 jnSign (map sign ts)
>                            where (s,t) `jnSign` (s',t') = (s,t')
>   sign (Fu fs)           = if length (eqClass order (map sign fs))>1 then error ("(module CC_aux) Fatal: sign (Fu fs) not defined\nwith map sign fs="++show (map sign fs)) else
>                            if null fs then (Anything, Anything) else
>                            foldr1 lub (map sign fs)
>   sign (Fi fs)           = if length (eqClass order (map sign fs))>1 then error ("(module CC_aux) Fatal: sign (Fi fs) not defined\nwith map sign fs="++show (map sign fs)) else
>                            if null fs then (Anything, Anything) else
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
>                                error ("(module CC_aux) Fatal: incompatible signs in "++showHS r)
>   multiplicities r           = []
>   isMph r  | ruleType r=='A' = isMph (consequent r)
>            | otherwise       = False
>   flp r@(Ru 'A' antc pos expr cpu expla (a,b) nr pn) = Ru 'A' (error ("(Module CC_aux:) illegal call to antecedent in flp ("++showADL r++")")) pos (flp expr) cpu expla (b,a) nr pn
>   flp (Ru c antc pos cons cpu expla (a,b) nr pn)   = Ru c (flp antc) pos (flp cons) cpu expla (b,a) nr pn
> --  isIdent r = error ("(module CC_aux: isIdent) not applicable to any rule:\n "++showHS r)
>   typeUniq r | ruleType r=='A' = typeUniq (antecedent r)
>              | otherwise       = typeUniq (antecedent r) && typeUniq (consequent r)
>   isIdent r = isIdent (normExpr r)
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
>                                       error ("(module CC_aux) Fatal: limit sgn ("++showHS (F ts)++") has incompatible types inside...")
>   limit sgn (Fd ts)   = Fd (lim sgn ts)
>    where lim sgn  [x] = [limit sgn x]
>          lim sgn   [] = []
>          lim (a,b) (x:xs) = [limit (a,c) x]++lim (c,b) xs
>                             where c = if null xs then target x else
>                                       if target x `order` source (head xs) then target x `lub` source (head xs) else
>                                       error ("(module CC_aux) Fatal: limit sgn ("++showHS (Fd ts)++") has incompatible types inside...")
>   limit sgn (Fu fs)   = Fu (map (limit sgn) fs)
>   limit sgn (Fi fs)   = Fi (map (limit sgn) fs)
>   limit sgn' (K0 e)   = K0 (limit sgn' e)
>   limit sgn' (K1 e)   = K1 (limit sgn' e)
>   limit sgn' (Cp e)   = Cp (limit sgn' e)

>   calc (Tm m) ss      = calc m ss
>   calc (Tc f) ss      = calc f ss
>   calc (F  ts) ss     = if null ts then error ("(module CC_aux) Fatal: no terms in calc (F "++showHS ts++")") else
>                         foldr1 join [calc t ss| t<-ts ]

   calc (Fd ts) ss     = if null ts then error ("(module CC_aux) Fatal: no terms in calc (Fd "++showHS ts++")") else
                         foldr1 joinD [calc t ss| t<-ts ]

>   calc (Fu fs) ss     = foldr uni [] [calc f ss| f<-fs ]
>   calc (Fi fs) ss     = if null fs then error ("(module CC_aux) Fatal: no factors in calc (Fi "++showHS fs++")") else
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
>  flipProps :: [Prop] -> [Prop]
>  flipProps ps = [flipProp p| p<-ps]

>  flipProp Uni = Inj
>  flipProp Tot = Sur
>  flipProp Sur = Tot
>  flipProp Inj = Uni
>  flipProp x = x

>  instance Typologic Concept
>  instance Identified Concept where
>   name (C nm _ _) = nm
>   name (S nm _ _) = nm
>   name Anything   = "Anything"
>   name NOthing    = "NOthing"
>  instance Identified Morphism where
>   name (Mph nm _ _ _ _ _) = nm
>   name i = name (declaration i)
>  instance Identified Declaration where
>   name (Sgn nm _ _ _ _ _ _ _ _ _ _ _) = nm
>   name (Isn _ _)                      = "I"
>   name (Iscompl _ _)                  = "I-"
>   name (Vs _ _)                       = "V"

>  applyM (Sgn nm _ _ _ prL prM prR _ _ _ _ _) d c = if null (prL++prM++prR) then d++" "++nm++" "++c else prL++(if null prL then d else unCap d)++prM++c++prR
>  applyM (Isn _ _)                            d c = d++" equals "++c
>  applyM (Iscompl _ _)                        d c = d++" differs from "++c
>  applyM (Vs _ _)                             d c = show True

>  instance Identified Context where
>   name (Ctx nm _ _ _ _ _ _ _ _) = nm

>  instance Identified Pattern where
>   name (Pat nm _ _ _ _ _) = nm

Interpretation of context as a language means to describe the classification tree,
the set of declarations and the rules that apply in that context. Inheritance of
properties is achieved as a result.


>  union :: Pattern -> Pattern -> Pattern
>  union (Pat nm rs parChds pms cs ks) (Pat nm' rs' parChds' pms' cs' ks')
>    = Pat nm' (rd(rs++rs')) (rd(parChds++parChds')) (rd(pms++pms')) (rd(cs++cs')) (ks++ks')

>  class Morphical a => Language a where
>    rules     :: a -> [Rule] -- all rules in the language that are specified as a rule in the ADL-model, including the GLUE rules, but excluding the multiplicity rules (multRules).
>    multRules :: a -> [Rule] -- all rules in the language that are specified as declaration properties.
>    multRules  = rules.rd.declarations
>    signals   :: a -> [Rule] -- all SIGNAL rules in the language.
>    signals x  = []
>    specs     :: a -> [Rule] -- all GLUE rules in the language.
>    specs x    = []
>    patterns  :: a -> [Pattern]
>    patterns x = []
>    isa       :: a -> Inheritance Concept
>    isa x      = empty

>  instance Language a => Language [a] where
>   rules xs   = (concat. map rules) xs
>   signals xs = (concat. map signals) xs
>   specs xs   = (rd. concat. map specs) xs
>   patterns   = rd' name.concat.map patterns
>   isa        = foldr uni empty.map isa

>  instance Language a => Language (Classification a) where
>   rules cl    = rules (preCl cl)
>   signals cl  = signals (preCl cl)
>   specs cl    = specs (preCl cl)
>   patterns cl = patterns (preCl cl)
>   isa         = foldr uni empty.map isa.preCl

>  instance Language Rule where
>   rules   r@(Gc pos m expr cpu sgn nr pn)
>    = [Ru 'E' (F [Tm m]) pos expr cpu (name m++" is implemented using "++enumerate (map name (mors expr))) sgn nr pn]
>   rules   r@(Ru _ _ _ _ _ _ _ _ _) = [r]
>   rules   r                        = []
>   signals r@(Sg _ _ _ _ _ _ _)     = [r]
>   signals r                        = []
>   specs   r@(Gc _ _ _ _ _ _ _)     = [r]
>   specs   r                        = []
>   patterns r                       = [Pat "" [r] [] [] [] []]
>   isa (Gc _ m expr _ _ _ _)
>     = Isa tuples (concs expr>-rd [e|(a,b)<-tuples,e<-[a,b]])
>       where tuples = clear [(source expr,source m),(target expr,target m)]
>   isa r = empty

>  clear abs = rd [(a,b)| (a,b)<-abs, a/=b]
>  clearG abs = rd [G g s| G g s<-abs, g/=s]

>  instance Language Pattern where
>   rules (Pat nm rs parChds pms cs ks)   = [r|r@(Ru c antc pos cons cpu expla sgn nr pn)<-rs]
>   signals (Pat nm rs parChds pms cs ks) = [r|r@(Sg p rule expla sgn nr pn signal)<-rs]
>   specs (Pat nm rs parChds pms cs ks)   = [r|r@(Gc pos m expr cpu sgn nr pn)<-rs]
>   patterns p                            = [p]
>   isa   (Pat nm rs parChds pms cs ks)   = Isa ts (singles>-rd [e| G g s<-parChds,e<-[g,s]])
>                                           where Isa tuples singles = isa rs
>                                                 ts = clear (tuples++[(g,s)| G g s<-parChds])

>  instance Language Context where
>   rules    (Ctx nm on i world dc ss cs ks os) = renumberRules 1 (rules (foldr union (Pat "" [] [] [] [] []) dc)++rules world)
>   signals  (Ctx nm on i world dc ss cs ks os) = signals (foldr union (Pat "" [] [] [] [] []) dc)++signals world
>   specs    (Ctx nm on i world dc ss cs ks os) = specs (foldr union (Pat "" [] [] [] [] []) dc)++specs world
>   patterns (Ctx nm on i world dc ss cs ks os) = dc
>   isa      (Ctx nm on i world dc ss cs ks os) = i
>   multRules context
>    = renumberRules (1 + (length (rules context)))
>                     [  c
>                      | sgn <- declarations context ,
>                        c <- rules sgn
>                     ]
>      

>  instance Language Declaration where
>   rules d = multRules d
>   multRules d
>    = [h p| p<-multiplicities d, p `elem` [Uni,Tot,Inj,Sur,Sym,Asy,Trn,Rfx]
>          , if source d==target d || p `elem` [Uni,Tot,Inj,Sur] then True else
>             error ("(module CC_aux) !Err: Property "++show p++" requires equal source and target domains (you specified "++name (source d)++" and "++name (target d)++").") ]
>     where h Sym = Ru 'E' (F [Tm r]) (CC_aux.pos d) (F [Tm r'])        [] (name d++"["++name (source d)++"*"++name (source d)++"] is symmetric.")     sgn (nr d) ""
>           h Asy = Ru 'I' (Fi [F [Tm r], F [Tm r']]) (CC_aux.pos d) id [] (name d++"["++name (source d)++"*"++name (source d)++"] is antisymmetric.") sgn (nr d) ""
>           h Trn = Ru 'I' (F [Tm r, Tm r]) (CC_aux.pos d) (F [Tm r])   [] (name d++"["++name (source d)++"*"++name (source d)++"] is transitive.")    sgn (nr d) ""
>           h Rfx = Ru 'I' id (CC_aux.pos d) (F [Tm r])                 [] (name d++"["++name (source d)++"*"++name (source d)++"] is reflexive.")     sgn (nr d) ""
>           h Uni = Ru 'I' (F [Tm r',Tm r]) (CC_aux.pos d) id'          [] (name d++"["++name (source d)++"*"++name (target d)++"] is univalent")      sgn (nr d) ""
>           h Sur = Ru 'I' id' (CC_aux.pos d) (F [Tm r',Tm r])          [] (name d++"["++name (source d)++"*"++name (target d)++"] is surjective")     sgn (nr d) ""
>           h Inj = Ru 'I' (F [Tm r,Tm r']) (CC_aux.pos d) id           [] (name d++"["++name (source d)++"*"++name (target d)++"] is injective")      sgn (nr d) ""
>           h Tot = Ru 'I' id (CC_aux.pos d) (F [Tm r,Tm r'])           [] (name d++"["++name (source d)++"*"++name (target d)++"] is total")          sgn (nr d) ""
>           sgn   = (source d,source d)
>           r     = Mph (name d)                (CC_aux.pos d) [] (source d,target d) True d
>           r'    = flp (r ) 
>           r'' t = Mph (t++"["++(name d)++"]") (CC_aux.pos d) [] (source d,target d) True d
>           id    = F [Tm (I [source d] (source d) (source d) True)]
>           id'    = F [Tm (I [target d] (target d) (target d) True)]

  nogE a b = False

>  wrld :: Context -> [Classification Context]
>  wrld (Ctx nm on i world dc ss cs ks os) = world

Language peculiarities

>  plural English str
>   | null str = str
>   | last str=='y' = init str++"ies"
>   | last str=='s' = str++"es"
>   | last str=='x' = str++"es"
>   | last str=='f' = init str++"ves"
>   | otherwise     = head ([p|(s,p) <-exceptions, s==str]++[str++"s"])
>   where exceptions = [("mouse","mice"),("sheep","sheep"),("Mouse","Mice"),("Sheep","Sheep")]
>  plural Dutch str
>   | null str = str
>   | not (null matches)         = head (matches++[str++"en"])
>   | take 2 (reverse str)=="ei" = str++"s"
>   | take 2 (reverse str)=="ji" = str++"en"
>   | take 2 (reverse str)=="oi" = str++"'s"
>   | last str `elem` "aeiou" = str++"s"
>   | (take 2.drop 1.reverse) str `elem` ["aa","oo","ee","uu"] = (reverse.drop 2.reverse) str++mede (drop (length str-1) str)++"en"
>   | otherwise                  = str++"en"
>   where mede "f" = "v"
>         mede "s" = "z"
>         mede x = x
>         matches = [(reverse.drop (length s).reverse) str++p|(s,p) <-exceptions, (map toLower.reverse.take (length s).reverse) str==s]
>         exceptions = [ ("aanbod", "aanbiedingen")
>                      , ("beleg", "belegeringen")
>                      , ("dank", "dankbetuigingen")
>                      , ("gedrag", "gedragingen")
>                      , ("genot", "genietingen")
>                      , ("lof", "loftuitingen")
>                      , ("rede", "redenen")
>                      , ("lende", "lendenen")
>                      , ("onderzoek", "onderzoekingen")
>                      , ("archiefstuk", "archiefbescheiden")
>                      , ("titel", "titels")
>                      ]

Calculation of positions of symbols (both terminal and nonterminal) in the source code, to be performed by the parser

>  newtype FilePos = FilePos (String, Pos, String)                        deriving (Eq,Ord)
>  posNone         = FilePos ("",noPos,"")

>  instance Show Pos where
>    show (Pos l c)
>      = "line " ++ show l
>        ++ ", column " ++ show c
>  instance Ord Pos where
>    a >= b = (show a) >= (show b)
>    a <= b = (show a) <= (show b)

>  instance Show FilePos where
>    show (FilePos (fn,Pos l c,sym))
>      = "line " ++ show l
>--        ++ ", column " ++ show c
>        ++ ", file " ++ show fn

>  get_tok_pos     (Tok _ _ s l f) = FilePos (f,l,s)
>  get_tok_val_pos (Tok _ _ s l f) = (s,FilePos (f,l,s))

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

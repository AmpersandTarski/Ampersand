-- This module defines that ADL language 

> module ADLdef where
>  import CommonClasses ( Identified(name))
>  import Typology ( Inheritance(Isa))--, Typologic, genEq, typology)
>  import Classification 
>            ( Classification(),preCl,mapCl
>            )
>  import Collection (Collection (uni,isc,(>-),empty,rd)  )
>  import UU_Scanner (Pos(Pos),Token(Tok),noPos)
>  import Auxiliaries (chain) 

>  data Architecture = Arch { archContexts :: Contexts} -- deriving Show
>

>  type Contexts  = [Context]
>  data Context   = Ctx { ctxnm         :: String     -- name of this context
>                       , ctxon           :: [String]   -- the list of context names of contexts whose rules are imported
>                       , ctxisa          :: (Inheritance Concept)     -- a data structure containing the generalization structure of concepts
>                       , wrld            :: [Classification Context]  -- a tree, being the transitive closure of the 'extends' (see formal definition) relation.
>                       , ctxpats         :: Patterns                  -- a list of patterns defined in this context
>                       , ctxrs           :: Rules                     -- a list of all rules that are valid within this context
>                       , ctxds           :: Declarations              -- a list of declarations defined in this context, outside the scope of patterns
>                       , ctxcs           :: ConceptDefs               -- a list of concept definitions defined in this context, outside the scope of patterns
>                       , ctxks           :: KeyDefs                   -- a list of key definitions defined in this context, outside the scope of patterns
>                       , ctxos           :: ObjDefs                   -- a list of key definitions defined in this context, outside the scope of patterns
>                       , ctxpops         :: Populations              -- a list of populations defined in this context
>                       } 

>               --    deriving Show -- just for testing. pattern:  Ctx nm on i world pats rs ds cs ks os pops
>--  instance Eq Context where
>--   Ctx nm _ _ _ _ _ _ _ _ _ _ == Ctx nm' _ _ _ _ _ _ _ _ _ _ = nm == nm'
>  instance Identified Context where
>    name c = ctxnm c

>  type Concepts  = [Concept]
>  data Concept      = C { cptnm   :: String
>                        , cptgE   :: GenR 
>                        , cptos   :: [String]  -- atoms
>                        }  -- C nm gE cs represents the set of instances cs by name nm.
>                    | S  -- the universal singleton: I[S]=V[S]
>                    | Anything
>                    | NOthing
>  cptC nm gE os = C nm gE os  -- constructor
>  cptS = S                    -- constructor
>  cptAnything = Anything      -- constructor
>  cptNothing = NOthing        -- constructor
>  cptnew nm = cptC nm (==) []

>  instance Eq Concept where
>   C a _ _ == C b _ _ = a==b
>   S == S = True
>   Anything == Anything = True
>   NOthing == NOthing = True
>   _ == _ = False

>  instance Identified Concept where
>   name (C {cptnm = nm}) = nm
>   name S = "ONE"
>   name Anything   = "Anything"
>   name NOthing    = "NOthing"

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
>   singleton (S) = True
>   singleton _ = False
>   typeUniq Anything = False
>   typeUniq _ = True

>  instance Ord Concept where
>   NOthing <= b  = False
>   a <= NOthing  = True
>   Anything <= b = True
>   a <= Anything = False
>   a@(C _ gE _) <= b = a `gE` b
>   a <= b = a==b
>   --TODO: ORD is niet gedefinieerd op Singelton.... Is dat erg?

>  instance Show Concept where
>   showsPrec p c = showString (name c)

>  isNothing, isAnything :: Concept -> Bool
>  isNothing  c | c == cptNothing  = True
>               | otherwise        = False
>  isAnything c | c == cptAnything = True
>               | otherwise        = False
>  isC C{} = True
>  isC c   = False


>  type ConceptDefs = [ConceptDef]
>  data ConceptDef = Cd { cdpos :: FilePos  -- pos: the position of this definition in the text of the ADL source (filename, line number and column number).
>                       , cdnm  :: String   -- nm:  the name of this concept. If there is no such concept, the conceptdefinition is ignored.
>                       , cddef :: String   -- def: the textual definition of this concept.
>                       , cdref :: String   -- ref: a label meant to identify the source of the definition. (useful as LaTeX' symbolic reference)
>                       } deriving Show    -- so, conventionally we will write: Cd pos nm def ref
>  instance Eq ConceptDef where
>   cd == cd' = cdnm cd == cdnm cd
>  instance Identified ConceptDef where
>   name cd = cdnm cd

----------------- HIERBOVEN ZIJN FIELD LABELS EN FUNCTIES TOEGEVOEGD. ------------
----------------- Hieronder nog niet.....

>  type ObjDefs = [ObjectDef]
>  data ObjectDef = Obj { objnm  :: String         -- nm:   view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
>                       , objpos :: FilePos        -- pos:  position of this definition in the text of the ADL source file (filename, line number and column number)
>                       , objctx :: Expression     -- ctx:  this expression describes the instances of this object, related to their context. 
>                       , objats :: ObjDefs        -- ats:  the attributes, which are object definitions themselves.
>                       } deriving (Eq,Show) -- So in its entirety: Obj nm pos ctx ats

>  instance Identified ObjectDef where
>   name obj = objnm obj

>  instance Numbered ObjectDef where
>   pos obj = objpos obj

>  objdefNew e = Obj "" posNone e []    -- de constructor van een object. Er is geen default waarde voor expression, dus die moeten we dan maar meegeven. 8-((

>  type KeyDefs = [KeyDef]
>  data KeyDef = Kd FilePos      -- pos:  position of this definition in the text of the ADL source file (filename, line number and column number).
>                   String       -- lbl:  the name (or label) of this Key. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
>                   Expression   -- ctx:  this expression describes the instances of this object, related to their context
>                   [ObjectDef]  -- ats:  the constituent attributes (i.e. name/expression pairs) of this key.
>                deriving (Eq,Show) -- So in its entirety: Kd pos lbl ctx ats


>  instance Identified KeyDef where
>   name (Kd _ lbl _ _) = lbl


>  type Populations = [Population]
>  data Population = Popu Morphism Pairs

>  type Morphisms = [Morphism]
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

>  inline::Morphism -> Bool
>  inline (Mph _ _ _ _ yin _) = yin
>  inline (I _ _ _ _ )        = True
>  inline (V _ _)             = True
>  inline (Mp1 _ _)           = True




>  instance Eq Morphism where
>--   m == m' = name m==name m' && source m==source m' && target m==target m' && yin==yin'
>   Mph nm _ _ (a,b) yin _ == Mph nm' _ _ (a',b') yin' _ = nm==nm' && yin==yin' && a==a' && b==b'
>   Mph nm _ _ (a,b) yin _ == _ = False
>   I _ g s yin            == I _ g' s' yin'             =            if yin==yin' then g==g' && s==s' else g==s' && s==g'
>   I _ g s yin            == _ = False
>   V _ (a,b)              == V _ (a',b')                = a==a' && b==b'
>   V _ (a,b)              == _ = False
>   Mp1 s c                == Mp1 s' c'                  = s==s' && c==c'
>   Mp1 s c                == _ = False

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

>  instance Identified Morphism where
>   name (Mph nm _ _ _ _ _) = nm
>   name i = name (declaration i)


>  instance Show Morphism where
>   showsPrec p (Mph nm pos  []  sgn yin m) = showString (nm  {- ++"("++show a++"*"++show b++")" where (a,b)=sgn -} ++if yin then "" else "~")
>   showsPrec p (Mph nm pos atts sgn yin m) = showString (nm  {- ++"["++chain "*" (map name (rd atts))++"]" -}      ++if yin then "" else "~")
>   showsPrec p (I atts g s yin)            = showString ("I"++ (if null atts then {- ++"["++name g, (if s/=g then ","++name s else "")++"]" -} "" else show atts))
>   showsPrec p (V atts (a,b))              = showString ("V"++ (if null atts then "" else show atts))


>  declaration :: Morphism -> Declaration
>  declaration (Mph _ _ _ _ _ s) = s
>  declaration (I atts g s yin)  = Isn g s
>  declaration (V atts (a,b))    = Vs a b
>  declaration (Mp1 s c)         = Isn c c


>  type Declarations = [Declaration]
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

>  instance Eq Declaration where
>   s == s' = name s==name s' && source s==source s' && target s==target s'

>  instance Identified Declaration where
>   name (Sgn nm _ _ _ _ _ _ _ _ _ _ _) = nm
>   name (Isn _ _)                      = "I"
>   name (Iscompl _ _)                  = "I-"
>   name (Vs _ _)                       = "V"

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

>  instance Show Declaration where
>   showsPrec p (Sgn nm a b props prL prM prR cs expla _ _ False)
>    = showString (chain " " ([nm,"::",name a,"*",name b,show props,"PRAGMA",show prL,show prM,show prR]++if null expla then [] else ["EXPLANATION",show expla]))
>   showsPrec p (Sgn nm a b props prL prM prR cs expla _ _ True)
>    = showString (chain " " ["SIGNAL",nm,"ON (",name a,"*",name b,")"])
>   showsPrec p _
>    = showString ""


>  type Patterns  = [Pattern]
>  data Pattern   = Pat String             -- name of this pattern
>                       Rules              -- list of rules declared in this pattern
>                       Gens               -- list of generalizations defined in this pattern
>                       Declarations       -- list of declarations defined in this pattern
>                       ConceptDefs        -- list of concept definitions defined in this pattern
>                       KeyDefs            -- list of key definitions defined in this pattern
>                   deriving Show



>  type Expressions = [Expression]
>  data Expression  = Tm Morphism          -- simple morphism, possibly conversed     ~
>                   | Tc Expression        -- bracketed expression                 ( ... )
>                   | F Expressions        -- composition                             ;
>                   | Fd Expressions       -- relative addition                       !
>                   | Fi Expressions       -- intersection                            /\
>                   | Fu Expressions       -- union                                   \/
>                   | K0 Expression        -- Reflexive and transitive closure        *
>                   | K1 Expression        -- Transitive closure                      +
>                   | Cp Expression        -- Complement                              -

>  instance Show Expression where
>   showsPrec p e  = showString (showExpr ("\\/", "/\\", "!", ";", "*", "+", "-", "(", ")") e)
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

>  type Rules     = [Rule]

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
>  flipProps :: [Prop] -> [Prop]
>  flipProps ps = [flipProp p| p<-ps]

>  flipProp Uni = Inj
>  flipProp Tot = Sur
>  flipProp Sur = Tot
>  flipProp Inj = Uni
>  flipProp x = x



>  data Gen       = G FilePos             -- the position of the GEN-rule
>                     Concept             -- generic concept
>                     Concept             -- specific concept
>                   deriving Eq
>  type Gens      = [Gen]


>  instance Show Gen where
>   -- This show is used in error messages. It should therefore not display the term's type
>   showsPrec p (G pos g s) = showString ("GEN "++show s++" ISA "++show g)


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

>  instance Identified Rule where
>   name r = "Rule"++show (nr r)

>  ruleType    (Ru c _ _ _ _ _ _ _ _) = c
>  ruleType    (Sg _ rule _ _ _ _ _)  = ruleType rule
>  ruleType    (Gc _ _ _ _ _ _ _)     = 'g'
>  ruleType    (Fr _ _ _ _)           = 'f'
> --XXXnog terugzetten in definitie: antecedent r@(Ru 'A' _ _ _ _ _ _ _ _) = error ("(Module ADLdef:) illegal call to antecedent of rule "++showADL r)
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

>  type GenR = Concept->Concept->Bool

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

>  type Paire     = [String]
>  src, trg      :: Paire -> String
>  src xs         = if null xs then error ("(module ADLdef) Fatal: src []") else head xs
>  trg xs         = if null xs then error ("(module ADLdef) Fatal: trg []") else last xs
>  type Pairs     = [Paire]



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
>   nr m = nr (declaration m)

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



>  makeMph :: Declaration -> Morphism
>  makeMph d = Mph (name d) (ADLdef.pos d) [] (sign d) True d



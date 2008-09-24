  Deze module bevat de datadefinitie van ADL. Daarnaast bevat het een minimale
  set aan eigenschappen (Eq, Show , Identified) van deze structuren.
  
> module ADLdataDef 
>               ( Architecture(..)
>               , Contexts
>               , Context(..)
>               , Concepts
>               , Concept(..)
>               , ConceptDef(..),ConceptDefs
>               , ObjectDef(..)
>               , ObjDefs
>               , KeyDef(..),KeyDefs
>               , Population(..), Populations
>               , Morphism(..),Morphisms,inline,makeDeclaration
>               , Declaration(..)
>               , Declarations
>               , Pattern(..)
>               , Patterns
>               , Expression(..)
>               , Expressions
>               , Prop(..)
>               , Gen(..), Gens
>               , Rule(..)
>               , Rules
>               , Paire
>               , Pairs
>               , GenR
>               , FilePos(..),posNone
>               , ShowADL(..)
>               ) where
>  import Typology ( Inheritance(Isa), Typologic(typology), genEq)
>  import Classification ( Classification(),preCl,mapCl)
>  import UU_Scanner (Pos(Pos),Token(Tok),noPos)
>  import Strings(chain)
>  import CommonClasses(Identified(name))
>  import Collection (Collection (uni,isc,(>-),empty,rd))
>  import Auxiliaries(showL)

>  data Architecture = Arch { archContexts :: Contexts} -- deriving Show
>  instance ShowADL Architecture where
>   showADL arch = chain "\n" (map showADL (archContexts arch))

>  type Contexts  = [Context]
>  data Context   
>     = Ctx { ctxnm    :: String     -- name of this context
>           , ctxon    :: [String]   -- the list of extends (= context names of contexts) whose rules are imported
>           , ctxisa   :: (Inheritance Concept)     -- a data structure containing the generalization structure of concepts
>           , ctxwrld  :: [Classification Context]  -- a tree, being the transitive closure of the 'extends' (see formal definition) relation.
>           , ctxpats  :: Patterns                  -- a list of patterns defined in this context
>           , ctxrs    :: Rules                     -- a list of all rules that are valid within this context
>           , ctxds    :: Declarations              -- a list of declarations defined in this context, outside the scope of patterns
>           , ctxcs    :: ConceptDefs               -- a list of concept definitions defined in this context, outside the scope of patterns
>           , ctxks    :: KeyDefs                   -- a list of key definitions defined in this context, outside the scope of patterns
>           , ctxos    :: ObjDefs                   -- a list of attributes defined in this context, outside the scope of patterns
>           , ctxpops  :: Populations               -- a list of populations defined in this context
>           } 
>  instance Identified Context where
>   name ctx = ctxnm ctx
>  instance ShowADL Context where
>   showADL context
>    = "CONTEXT " ++name context
>      ++ (if null (ctxon context)   then "" else "EXTENDS "++chain ", "   (ctxon context)                   ++ "\n")
>      ++ (if null (ctxos context)   then "" else "\n"      ++chain "\n\n" (map showADL (ctxos context))   ++ "\n")
>      ++ (if null (ctxcs context)   then "" else "\n"      ++chain "\n"   (map showADL (ctxcs context))   ++ "\n")
>      ++ (if null (ctxds context)   then "" else "\n"      ++chain "\n"   (map showADL (ctxds context))   ++ "\n")
>      ++ (if null (ctxks context)   then "" else "\n"      ++chain "\n"   (map showADL (ctxks context))   ++ "\n")
>      ++ (if null (ctxpats context) then "" else "\n"      ++chain "\n\n" (map showADL (ctxpats context)) ++ "\n")
>      ++ (if null (ctxpops context) then "" else "\n"      ++chain "\n\n" (map showADL (ctxpops context)) ++ "\n")
>      ++ "\n\nENDCONTEXT"
>  --    where decls = declarations context>-declarations (patterns context)
>  --          cdefs = conceptDefs context>-conceptDefs (patterns context)


>  type Concepts = [Concept]
>  data Concept
>     = C   { cptnm :: String
>           , cptgE :: GenR 
>           , cptos :: [String]  -- atoms
>           }  -- C nm gE cs represents the set of instances cs by name nm.
>     | S  -- the universal singleton: I[Anything]=V[Anything]
>     | Anything
>     | NOthing
>  instance Eq Concept where
>   C a _ _ == C b _ _ = a==b
>   S == S = True
>   Anything == Anything = True
>   NOthing == NOthing = True
>   _ == _ = False
>  instance Show Concept where
>   showsPrec p c = showString (name c)
>  instance ShowADL Concept where
>   showADL c = show (name c)

>  instance Identified Concept where
>   name (C {cptnm = nm}) = nm
>   name S = "ONE"
>   name Anything   = "Anything"
>   name NOthing    = "NOthing"





>  type ConceptDefs = [ConceptDef]
>  data ConceptDef 
>     = Cd  { cdpos :: FilePos  -- pos: the position of this definition in the text of the ADL source (filename, line number and column number).
>           , cdnm  :: String   -- nm:  the name of this concept. If there is no such concept, the conceptdefinition is ignored.
>           , cddef :: String   -- def: the textual definition of this concept.
>           , cdref :: String   -- ref: a label meant to identify the source of the definition. (useful as LaTeX' symbolic reference)
>           } deriving Show    -- so, conventionally we will write: Cd pos nm def ref
>  instance Eq ConceptDef where
>   cd == cd' = cdnm cd == cdnm cd
>  instance Identified ConceptDef where
>   name cd = cdnm cd
>  instance ShowADL ConceptDef where
>   showADL cd
>    = "\n  CONCEPT "++show (name cd)++" "++show (cddef cd)++" "++(if null (cdref cd) then "" else show (cdref cd))



>  type Patterns  = [Pattern]
>  data Pattern 
>     = Pat { ptnm  :: String       -- name of this pattern
>           , ptrls :: Rules        -- list of rules declared in this pattern
>           , ptgns :: Gens         -- list of generalizations defined in this pattern
>           , ptdcs :: Declarations -- list of declarations defined in this pattern
>           , ptcds :: ConceptDefs  -- list of concept definitions defined in this pattern
>           , ptkds :: KeyDefs      -- list of key definitions defined in this pattern
>           } deriving Show

>  instance Identified Pattern where
>   name pat = ptnm pat
>  instance ShowADL Pattern where
>   showADL pat
>    = "PATTERN " ++ name pat 
>      ++ (if null (ptrls pat)  then "" else "\n  " ++chain "\n  " (map showADL (ptrls pat)) ++ "\n")
>      ++ (if null (ptgns pat)  then "" else "\n  " ++chain "\n  " (map showADL (ptgns pat)) ++ "\n")
>      ++ (if null (ptdcs pat)  then "" else "\n  " ++chain "\n  " (map showADL (ptdcs pat)) ++ "\n")
>      ++ (if null (ptcds pat)  then "" else "\n  " ++chain "\n  " (map showADL (ptcds pat)) ++ "\n")
>      ++ (if null (ptkds pat)  then "" else "\n  " ++chain "\n  " (map showADL (ptkds pat)) ++ "\n")
>      ++ "ENDPATTERN"


>  type Rules = [Rule]

>  data Rule =
> -- Ru c antc p cons cpu expla sgn nr pn
>       Ru { rrsrt :: Char              -- 'I' if this is an implication, 'E' if this is an equivalence, 'A' if this is an ALWAYS expression.
>          , rrant :: Expression        -- antecedent
>          , rrfps :: FilePos           -- position in the ADL file
>          , rrcon :: Expression        -- consequent
>          , rrcpu :: Expressions       -- cpu. This is a list of subexpressions, which must be computed.
>          , rrxpl :: String            -- explanation
>          , rrtyp :: (Concept,Concept) -- type
>          , rrnum :: Int               -- rule number
>          , rrpat :: String            -- name of pattern in which it was defined.
>          }
> -- Sg p rule expla sgn nr pn signal
>     | Sg { srfps :: FilePos           -- position in the ADL file
>          , srsig :: Rule              -- the rule to be signalled
>          , srxpl :: String            -- explanation
>          , srtyp :: (Concept,Concept) -- type
>          , srnum :: Int               -- rule number
>          , srpat :: String            -- name of pattern in which it was defined.
>          , srrel :: Declaration       -- the signal relation
>          }
> -- Gc p antc cons cpu _ _ _
>     | Gc { grfps :: FilePos           -- position in the ADL file
>          , grspe :: Morphism          -- specific
>          , grgen :: Expression        -- generic
>          , grcpu :: Expressions       -- cpu. This is a list of subexpressions, which must be computed.
>          , grtyp :: (Concept,Concept) -- declaration
>          , grnum :: Int               -- rule number
>          , grpat :: String            -- name of pattern in which it was defined.
>          }
> -- Fr t d expr pn  -- represents an automatic computation, such as * or +.
>     | Fr { fraut :: AutType           -- the type of automatic computation
>          , frdec :: Declaration       -- where the result is to be stored
>          , frcmp :: Expression        -- expression to be computed
>          , frpat :: String            -- name of pattern in which it was defined.
>          } 
>       deriving (Eq,Show)

>  instance ShowADL Rule where
>   showADL r@(Sg p rule expla sgn nr pn signal) = "SIGNAL "++name signal++" ON "++ showADL rule
>   showADL r@(Fr _ d expr _) = showADL d ++ "\n" ++ show (name d)++" = "++showADL expr
>   showADL r@(Ru c antc p cons cpu expla sgn nr pn)
>    | c=='A' = "ALWAYS "++showADL cons++
>                 if null cpu then "" else " COMPUTING " ++ show cpu 
>    | c=='I' = showADL antc ++" |- "++showADL cons++
>                 if null cpu then "" else " COMPUTING " ++ show cpu 
>    | c=='E' = showADL antc ++" = " ++showADL cons++
>                 if null cpu then "" else " COMPUTING " ++ show cpu 
>   showADL r@(Gc _ antc cons cpu _ _ _)
>             = "GLUE "++showADL antc++" = "++showADL cons++
>                 if null cpu then "" else " COMPUTING " ++ show cpu


  instance Eq Rule where
   Ru c antc _ cons _ sgn nr pn == Ru c' antc' _ cons' _ sgn' nr' pn'
    | nr==0 || nr'==0 = c==c' && sgn==sgn' && (if c=='A'&&c'=='A' then True else antc==antc') && cons==cons'
    | otherwise       = nr==nr' && pn==pn
   Gc _ m expr sgn nr pn _ _ == Gc _ m' expr' sgn' nr' pn'
    | nr==0 || nr'==0 = sgn==sgn' && m==m' && expr==expr'
    | otherwise       = nr==nr' && pn==pn
   _ == _ = False


>  type Declarations = [Declaration]
>  data Declaration = 
>          Sgn { decnm   :: String  -- the name of the declaration
>              , decsrc  :: Concept -- the source concept of the declaration
>              , dectgt  :: Concept -- the target concept of the declaration
>              , decprps :: Props   -- the multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx)
>              , decp1   :: String  -- three strings, which form the pragma. E.g. if pragma consists of the three strings: "Person ", " is married to person ", and " in Vegas."
>              , decp2   :: String  --    then a tuple ("Peter","Jane") in the list of links means that Person Peter is married to person Jane in Vegas.
>              , decp3   :: String
>              , decpr   :: Pairs   -- the list of tuples, of which the relation consists.
>              , decexpl :: String  -- the explanation
>              , decfpos :: FilePos -- the position in the ADL source file where this declaration is declared.
>              , decid   :: Int     -- a unique number that can be used to identify the relation
>              , deciss  :: Bool    -- if true, this is a signal relation; otherwise it is an ordinary relation.
>              }
>         | Isn 
>              { dectgt :: Concept  -- The generic concept
>              , decsrc :: Concept  -- The specific concept
>              }
>         | Iscompl 
>              { dectgt :: Concept
>              , decsrc :: Concept
>              }
>         | Vs 
>              { decsrc :: Concept
>              , dectgt :: Concept
>              }

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

>  instance Eq Declaration where
>     s == s' = name s==name s' && decsrc s==decsrc s' && dectgt s==dectgt s'

>  instance Identified Declaration where
>   name (Sgn nm _ _ _ _ _ _ _ _ _ _ _) = nm
>   name (Isn _ _)                      = "I"
>   name (Iscompl _ _)                  = "I-"
>   name (Vs _ _)                       = "V"


>  instance Show Declaration where
>   showsPrec p (Sgn nm a b props prL prM prR cs expla _ _ False)
>    = showString (chain " " ([nm,"::",name a,"*",name b,show props,"PRAGMA",show prL,show prM,show prR]++if null expla then [] else ["EXPLANATION",show expla]))
>   showsPrec p (Sgn nm a b props prL prM prR cs expla _ _ True)
>    = showString (chain " " ["SIGNAL",nm,"ON (",name a,"*",name b,")"])
>   showsPrec p _
>    = showString ""
>  instance ShowADL Declaration where
>   showADL decl@(Sgn nm a b props prL prM prR cs expla _ _ sig)
>    = if sig then "SIGNAL "++nm++" ON ("++name a++" * "++name b++")" else
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


>  type KeyDefs = [KeyDef]
>  data KeyDef = Kd { kdpos :: FilePos      -- position of this definition in the text of the ADL source file (filename, line number and column number).
>                   , kdlbl :: String       -- the name (or label) of this Key. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
>                   , kdctx :: Expression   -- this expression describes the instances of this object, related to their context
>                   , kdats :: ObjDefs      -- the constituent attributes (i.e. name/expression pairs) of this key.
>                   } deriving (Eq,Show) 

>  instance Identified KeyDef where
>   name kd = kdlbl kd
>  instance ShowADL KeyDef where
>   showADL kd 
>   -- Oorspronkelijk: = "KEY "++kdlbl kd++">"++name (target (kdctx kd))++"("++chain "," (map showADL (kdats kd))++")"
>    = "KEY "++kdlbl kd
>            ++">"++"name (target("++showADL (kdctx kd)++")"
>            ++"("++chain "," (map showADL (kdats kd))++")"


>  type Populations = [Population]
>  data Population = Popu 
>             { popm  :: Morphism
>             , popps :: Pairs
>             }
>  instance ShowADL Population where
>   showADL (Popu m ps)
>    = nlIndent++"pop_"++name m++name (source m)++name (target m)++nlIndent++" = [ "++chain (nlIndent'++"; ") (map show ps)++nlIndent'++"]"
>      where nlIndent = "\n      "; nlIndent' = nlIndent++"    "
>            source (Mph nm pos atts (a,b) yin m) = a
>            target (Mph nm pos atts (a,b) yin m) = b

>  type ObjDefs = [ObjectDef]
>  data ObjectDef = Obj { objnm  :: String         -- nm:   view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
>                       , objpos :: FilePos        -- pos:  position of this definition in the text of the ADL source file (filename, line number and column number)
>                       , objctx :: Expression     -- ctx:  this expression describes the instances of this object, related to their context. 
>                       , objats :: ObjDefs        -- ats:  the attributes, which are object definitions themselves.
>                       } deriving (Eq,Show) -- So in its entirety: Obj nm pos ctx ats

>  instance Identified ObjectDef where
>   name obj = objnm obj

>  instance ShowADL ObjectDef where
>   showADL obj = "OBJECT "++name obj++" : "++(showADL (objctx obj))++" = ["++chain ", " (map show (objats obj))++"]"

>  type Gens      = [Gen]
>  data Gen       = G FilePos             -- the position of the GEN-rule
>                     Concept             -- generic concept
>                     Concept             -- specific concept
>                   deriving Eq
>  instance Show Gen where
>   -- This show is used in error messages. It should therefore not display the term's type
>   showsPrec p (G pos g s) = showString ("GEN "++show s++" ISA "++show g)
>  instance ShowADL Gen where
>   showADL (G pos g s) = "GEN "++showADL s++" ISA "++show g



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
>  instance ShowADL Expression where
>   showADL e = show e


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

>  instance Show Morphism where
>   showsPrec p (Mph nm pos  []  sgn yin m) = showString (nm  {- ++"("++show a++"*"++show b++")" where (a,b)=sgn -} ++if yin then "" else "~")
>   showsPrec p (Mph nm pos atts sgn yin m) = showString (nm  {- ++"["++chain "*" (map name (rd atts))++"]" -}      ++if yin then "" else "~")
>   showsPrec p (I atts g s yin)            = showString ("I"++ (if null atts then {- ++"["++name g, (if s/=g then ","++name s else "")++"]" -} "" else show atts))
>   showsPrec p (V atts (a,b))              = showString ("V"++ (if null atts then "" else show atts))
>  instance ShowADL Morphism where
>   showADL m@(Mph nm pos atts sgn@(a,b) yin s)
>    = ({- if take 5 nm=="Clos_" then drop 5 nm++"*" else -} nm)++
>      (if null atts
>           then (if yin && sgn==(decsrc s,dectgt s) || not yin && sgn==(dectgt s,decsrc s) then "" else showSign [a,b])
>           else showSign atts)++
>      if yin then "" else "~"
>   showADL (I atts g s yin)
>    = "I"++if null atts then "" else showSign atts++if g==s then "" else if yin then "" else "~"
>   showADL (V atts (a,b))
>    = "V"++if null atts then "" else showSign atts
>   showADL (Mp1 str sgn)
>    = "'"++str++"'"++(showSign [sgn])
>  showSign cs = "["++chain "*" (map name cs)++"]"


>  inline::Morphism -> Bool
>  inline (Mph _ _ _ _ yin _) = yin
>  inline (I _ _ _ _ )        = True
>  inline (V _ _)             = True
>  inline (Mp1 _ _)           = True

>  instance Identified Morphism where
>   name (Mph nm _ _ _ _ _) = nm
>   name i = name (makeDeclaration i)

>  makeDeclaration :: Morphism -> Declaration
>  makeDeclaration (Mph _ _ _ _ _ s) = s
>  makeDeclaration (I atts g s yin)  = Isn g s
>  makeDeclaration (V atts (a,b))    = Vs a b
>  makeDeclaration (Mp1 s c)         = Isn c c


>  data AutType = Clos0 | Clos1 deriving (Eq,Show)

>  type Props = [Prop]
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
>  instance ShowADL Prop where
>   showADL p = show p


>  type Pairs     = [Paire]
>  type Paire     = [String]
>  type GenR = Concept->Concept->Bool

>  instance Show Pos where
>    show (Pos l c)
>      = "line " ++ show l
>        ++ ", column " ++ show c
>  instance Ord Pos where
>    a >= b = (show a) >= (show b)
>    a <= b = (show a) <= (show b)

>  newtype FilePos = FilePos (String, Pos, String)                        deriving (Eq,Ord)
>  posNone         = FilePos ("",noPos,"")

>  instance Show FilePos where
>    show (FilePos (fn,Pos l c,sym))
>      = "line " ++ show l
>--        ++ ", column " ++ show c
>        ++ ", file " ++ show fn

>  class ShowADL a where
>   showADL :: a -> String
>  instance ShowADL a => ShowADL [a] where
>   showADL = chain "\n".map showADL




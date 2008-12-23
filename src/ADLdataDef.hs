  module ADLdataDef
                ( module Data.ADL
         --       , Architecture(..)
         --       , Contexts
         --       , Context(..)
         --       , Concepts
         --       , Concept(..)
         --       , ConceptDef(..),ConceptDefs
          --      , ObjectDef(..)
          --      , ObjDefs
         --       , KeyDef(..),KeyDefs
          --      , Population(..), Populations
         --       , Morphism(..),Morphisms
                  ,inline,makeDeclaration
         --       , Declaration(..)
         --       , Declarations
         --       , Pattern(..)
          --      , Patterns
          --      , Expression(..)
          --      , Expressions
          --      , Prop(..)
          --      , Gen(..), Gens
          --      , Rule(..)
           --     , Rules
           --     , Paire
           --     , Pairs
           --     , GenR
            --    , FilePos(..),posNone
                ) where
   import Data.ADL             
   import Typology ( Inheritance())
   import Classification ( Classification())
   import UU_Scanner (Pos(Pos),noPos)
   import Strings(chain)
   import CommonClasses(Identified(name))
   import Collection (Collection (rd))
   import Auxiliaries(showL)



   instance Show ConceptDef    
   instance Show Pattern
   instance Eq Rule
   instance Show Rule
   instance Eq KeyDef
   instance Show KeyDef
   instance Eq ObjectDef
   instance Show ObjectDef
   instance Eq Gen
 
   
   instance Identified Context where
    name ctx = ctxnm ctx

   instance Eq Concept where
    C a _ _ == C b _ _ = a==b
    S == S = True
    Anything == Anything = True
    NOthing == NOthing = True
    _ == _ = False
   instance Show Concept where
    showsPrec p c = showString (name c)

   instance Identified Concept where
    name (C {cptnm = nm}) = nm
    name S = "ONE"
    name Anything   = "Anything"
    name NOthing    = "NOthing"





   instance Eq ConceptDef where
    cd == cd' = cdnm cd == cdnm cd
   instance Identified ConceptDef where
    name cd = cdnm cd

   instance Identified Pattern where
    name pat = ptnm pat













   instance Eq Declaration where
      d == d' = name d==name d' && decsrc d==decsrc d' && dectgt d==dectgt d'

   instance Identified Declaration where
    name (Sgn nm _ _ _ _ _ _ _ _ _ _ _) = nm
    name (Isn _ _)                      = "I"
    name (Iscompl _ _)                  = "-I"
    name (Vs _ _)                       = "V"


   instance Show Declaration where
    showsPrec p (Sgn nm a b props prL prM prR cs expla _ _ False)
     = showString (chain " " ([nm,"::",name a,"*",name b,show props,"PRAGMA",show prL,show prM,show prR]++if null expla then [] else ["EXPLANATION",show expla]))
    showsPrec p (Sgn nm a b props prL prM prR cs expla _ _ True)
     = showString (chain " " ["SIGNAL",nm,"ON (",name a,"*",name b,")"])
    showsPrec p _
     = showString ""

   instance Identified KeyDef where
    name kd = kdlbl kd

   instance Identified ObjectDef where
    name obj = objnm obj

   instance Show Gen where
    -- This show is used in error messages. It should therefore not display the term's type
    showsPrec p (G pos g s) = showString ("GEN "++show s++" ISA "++show g)


   instance Eq Expression where
    F  ts == F  ts' = ts==ts'
    Fd ts == Fd ts' = ts==ts'
    Fu fs == Fu fs' = rd fs==rd fs'
    Fi fs == Fi fs' = rd fs==rd fs'
    Cp e  == Cp e'  = e==e'
    K0 e  == K0 e'  = e==e'
    K1 e  == K1 e'  = e==e'
    Tm m  == Tm m'  = m==m'
    Tc e  == e'     = e==e'
    e     == Tc e'  = e==e'
    _     == _      = False
   instance Show Expression where
    showsPrec p e  = showString (showExpr ("\\/", "/\\", "!", ";", "*", "+", "-", "(", ")") e)
   showExpr (union,inter,rAdd,rMul,clos0,clos1,compl,lpar,rpar) e = showchar 0 e
     where
      wrap i j str = if i<=j then str else lpar++str++rpar
      showchar i (Tm m)  = name m++if inline m then "" else "~"
      showchar i (Fu []) = "-V"
      showchar i (Fu fs) = wrap i 4 (chain union [showchar 4 f| f<-fs])
      showchar i (Fi []) = "V"
      showchar i (Fi fs) = wrap i 5 (chain inter [showchar 5 f| f<-fs])
      showchar i (Fd []) = "-I"
      showchar i (Fd ts) = wrap i 6 (chain rAdd [showchar 6 t| t<-ts])
      showchar i (F [])  = "I"
      showchar i (F ts)  = wrap i 7 (chain rMul [showchar 7 t| t<-ts])
      showchar i (K0 e)  = showchar 8 e++clos0
      showchar i (K1 e)  = showchar 8 e++clos1
      showchar i (Cp e)  = compl++showchar 8 e
      showchar i (Tc f)  = showchar i f

   instance Eq Morphism where
 --   m == m' = name m==name m' && source m==source m' && target m==target m' && yin==yin'
    Mph nm _ _ (a,b) yin _ == Mph nm' _ _ (a',b') yin' _ = nm==nm' && yin==yin' && a==a' && b==b'
    Mph nm _ _ (a,b) yin _ == _ = False
    I _ g s yin            == I _ g' s' yin'             =            if yin==yin' then g==g' && s==s' else g==s' && s==g'
    I _ g s yin            == _ = False
    V _ (a,b)              == V _ (a',b')                = a==a' && b==b'
    V _ (a,b)              == _ = False
    Mp1 s c                == Mp1 s' c'                  = s==s' && c==c'
    Mp1 s c                == _ = False




   instance Show Morphism where
    showsPrec p (Mph nm pos  []  sgn yin m) = showString (nm  {- ++"("++show a++"*"++show b++")" where (a,b)=sgn -} ++if yin then "" else "~")
    showsPrec p (Mph nm pos atts sgn yin m) = showString (nm  {- ++"["++chain "*" (map name (rd atts))++"]" -}      ++if yin then "" else "~")
    showsPrec p (I atts g s yin)            = showString ("I"++ (if null atts then {- ++"["++name g, (if s/=g then ","++name s else "")++"]" -} "" else show atts))
    showsPrec p (V atts (a,b))              = showString ("V"++ (if null atts then "" else show atts))

   inline::Morphism -> Bool
   inline (Mph _ _ _ _ yin _) = yin
   inline (I _ _ _ _ )        = True
   inline (V _ _)             = True
   inline (Mp1 _ _)           = True

   instance Identified Morphism where
    name (Mph nm _ _ _ _ _) = nm
    name i = name (makeDeclaration i)

   makeDeclaration :: Morphism -> Declaration
   makeDeclaration (Mph _ _ _ _ _ s) = s
   makeDeclaration (I atts g s yin)  = Isn g s
   makeDeclaration (V atts (a,b))    = Vs a b
   makeDeclaration (Mp1 s c)         = Isn c c


   instance Show Prop where
    showsPrec p Uni = showString "UNI"
    showsPrec p Inj = showString "INJ"
    showsPrec p Sur = showString "SUR"
    showsPrec p Tot = showString "TOT"
    showsPrec p Sym = showString "SYM"
    showsPrec p Asy = showString "ASY"
    showsPrec p Trn = showString "TRN"
    showsPrec p Rfx = showString "RFX"
    showsPrec p Aut = showString "AUT"



module Adl.MorphismAndDeclaration where
   import Adl.FilePos
   import Adl.Concept
   import Adl.Prop
   import Adl.Pair
   import Strings(chain)
   import CommonClasses(Identified(name,typ)
                        , Conceptual(conts)
                        , Explained(explain)
                        , ABoolAlg)    

   type Morphisms = [Morphism]
   data Morphism  = 
                   Mph { mphnm :: String             -- ^ the name of the morphism. This is the same name as
                                                     --   the declaration that is bound to the morphism.
                                                     --    WAAROM Waarom zou je dit attribuut opnemen? De naam van het morphisme is immers altijd gelijk aan de naam van de Declaration mphdcl ....
                                                     --    ANTWOORD Tijdens het parsen, tot het moment dat de declaration aan het Morphism is gekoppeld, moet de naam van het Morphism bekend zijn. Nadat het morphisme gebonden is aan een declaration moet de naam van het morphisme gelijk zijn aan de naam van zijn mphdcl.
                       , mphpos :: FilePos           -- ^ the position of the rule in which the morphism occurs
                       , mphats :: [Concept]         -- ^ the attributes specified inline
                       , mphtyp :: Sign              -- ^ the allocated type. Together with the name, this forms the declaration.
                       , mphyin :: Bool              -- ^ the 'yin' factor. If true, a declaration is bound in the same direction as the morphism. If false, binding occurs in the opposite direction.
                       , mphdcl :: Declaration       -- ^ the declaration bound to this morphism.
                       }
                  | I  { mphats :: [Concept]         -- ^ the (optional) attribute specified inline. ADL syntax allows at most one concept in this list.
                       , mphgen ::  Concept          -- ^ the generic concept  
                       , mphspc ::  Concept          -- ^ the specific concept
                       , mphyin ::  Bool             -- ^ the 'yin' factor. If true, the specific concept is source and the generic concept is target. If false, the other way around.
                       } 
                  | V  { mphats :: [Concept]         -- ^ the (optional) attributes specified inline.
                       , mphtyp :: Sign              -- ^ the allocated type.
                       }
                  | Mp1 { mph1val :: String          -- ^ the value of the one morphism
                        , mph1typ :: Concept         -- ^ the allocated type.
                        }  

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Morphism                      ***
-- \***********************************************************************
   makeInline :: Morphism -> Morphism
   makeInline m = case m of
                    Mph{mphyin=False} -> m{mphtyp = reverse' (mphtyp m), mphyin=True}
                    _                 -> m
                    where reverse' (a,b) = (b,a)
--   makeInline m | inline m = m
--   makeInline (Mph nm pos atts (a,b) yin s) = Mph nm pos (reverse atts) (b,a) (not yin) s
--   makeInline (V atts (a,b))                = V (reverse atts) (b,a)
--   makeInline i                             = i


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

   instance Ord Morphism where
    a <= b = source a <= source b && target a <= target b

   instance Identified Morphism where
    name m = name (makeDeclaration m)
    typ m = "Morphism_"

   instance Association Morphism where
--    source (Mph nm pos atts (a,b) _ s) = a
--    source (I atts g s yin)            = if yin then s else g
--    source (V atts (a,b))              = a
--    source (Mp1 _ s) = s
--    target (Mph nm pos atts (a,b) _ s) = b
--    target (I atts g s yin)            = if yin then g else s
--    target (V atts (a,b))              = b
--    target (Mp1 _ t) = t
    sign   (Mph nm pos atts (a,b) _ s) = (a,b)
    sign   (I atts g s yin)            = if yin then (s,g) else (g,s)
    sign   (V atts (a,b))              = (a,b)
    sign   (Mp1 _ s) = (s,s)
    source m = source (sign m)
    target m = target (sign m)
   instance Numbered Morphism where
    pos m@(Mph{}) = mphpos m
    pos m         = posNone
    nr m = nr (makeDeclaration m)

   makeDeclaration :: Morphism -> Declaration
   makeDeclaration m = case m of
               Mph{} -> mphdcl m
               I{}   -> Isn{ despc = mphspc  m, degen = mphgen  m}   -- WAAROM?? Stef, waarom wordt de yin hier niet gebruikt?? Is dat niet gewoon FOUT?
               V{}   -> Vs { degen = source  (sign m), despc = target (sign m)}
               Mp1{} -> Isn{ despc = mph1typ m, degen = mph1typ m}

    
   inline::Morphism -> Bool
   inline m =  case m of
                Mph{} -> mphyin m
                I{}   -> True
                V{}   -> True
                Mp1{} -> True

   type Declarations = [Declaration]
   data Declaration = 
           Sgn { decnm   :: String  -- ^ the name of the declaration
               , desrc   :: Concept -- ^ the source concept of the declaration
               , detgt   :: Concept -- ^ the target concept of the declaration
               , decprps :: Props   -- ^ the multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx)
               , decp1   :: String  -- ^ three strings, which form the pragma. E.g. if pragma consists of the three strings: "Person ", " is married to person ", and " in Vegas."
               , decp2   :: String  -- ^    then a tuple ("Peter","Jane") in the list of links means that Person Peter is married to person Jane in Vegas.
               , decp3   :: String
               , decpr   :: Pairs   -- ^ the list of tuples, of which the relation consists.
               , decexpl :: String  -- ^ the explanation
               , decfpos :: FilePos -- ^ the position in the ADL source file where this declaration is declared.
               , decid   :: Int     -- ^ a unique number that can be used to identify the relation
               , deciss  :: Bool    -- ^ if true, this is a signal relation; otherwise it is an ordinary relation.
               }
          | Isn 
               { degen :: Concept  -- ^ The generic concept
               , despc :: Concept  -- ^ The specific concept
               }
          | Iscompl 
               { degen :: Concept
               , despc :: Concept
               }
          | Vs 
               { degen :: Concept
               , despc :: Concept
               }

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Declaration                   ***
-- \***********************************************************************
   instance Eq Declaration where
      d == d' = name d==name d' && source d==source d' && target d==target d'
   instance Show Declaration where
    showsPrec p (Sgn nm a b props prL prM prR cs expla _ _ False)
     = showString (chain " " ([nm,"::",name a,"*",name b,show props,"PRAGMA",show prL,show prM,show prR]++if null expla then [] else ["EXPLANATION",show expla]))
    showsPrec p (Sgn nm a b props prL prM prR cs expla _ _ True)
     = showString (chain " " ["SIGNAL",nm,"ON (",name a,"*",name b,")"])
    showsPrec p _
     = showString ""
   instance Identified Declaration where
    name (Sgn nm _ _ _ _ _ _ _ _ _ _ _) = nm
    name (Isn _ _)                      = "I"
    name (Iscompl _ _)                  = "-I"
    name (Vs _ _)                       = "V"
    typ d = "Declaration_"

   instance Association Declaration where
      source d = case d of
                   Sgn {}     -> desrc d
                   Isn {}     -> despc d
                   Iscompl {} -> despc d
                   Vs {}      -> degen d
      target d = case d of 
                   Sgn {}     -> detgt d
                   Isn {}     -> degen d
                   Iscompl {} -> degen d
                   Vs {}      -> despc d
    --sign is vanzelf al geregeld...

   instance Numbered Declaration where
    pos (Sgn _ _ _ _ _ _ _ _ _ p _ _) = p
    pos d                             = posNone
    nr (Sgn _ _ _ _ _ _ _ _ _ _ n _)  = n
    nr d                              = 0

   -- | Deze declaratie is de reden dat Declaration en Morphism in één module moeten zitten.
   makeMph :: Declaration -> Morphism
   makeMph d = Mph{ mphnm  = name d
                  , mphpos = pos d
                  , mphats = []
                  , mphtyp = sign d
                  , mphyin = True
                  , mphdcl = d
                  }

   isIdentM :: Morphism -> Bool
   isIdentM m = case m of
                   I{}   -> True       -- > tells whether the argument is equivalent to I
                   V{}   -> source m == target m && isSingleton (source m)
                   _     -> False
                   
   mIs :: Concept -> Morphism
   mIs c = I [] c c True

   instance Populated Morphism where
    contents m = contents (makeDeclaration (makeInline m))
-- Dit was in het verleden: (maar flp is hier niet bekend!)
--    contents m@(Mph _ _ _ _ False _) = map reverse (contents (flp m))
--    contents m = contents (makeDeclaration m)

   instance Populated Declaration where
    contents (Sgn _ _ _ _ _ _ _ cs _ _ _ _) = cs
    contents (Isn g s)                      = [[o,o] | o<-conts s]
    contents (Iscompl g s)                  = [[o,o']| o<-conts s,o'<-conts s,o/=o']
    contents (Vs g s)                       = [[o,o']| o<-conts s,o'<-conts s]

   instance ABoolAlg Morphism  -- SJ  2007/09/14: This is used solely for drawing conceptual graphs.
                  
   instance Explained Declaration where
    explain (Sgn _ _ _ _ _ _ _ _ expla _ _ _) = expla
    explain d                                 = ""

                        
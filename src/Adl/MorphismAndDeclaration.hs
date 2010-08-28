{-# OPTIONS_GHC -Wall #-}
module Adl.MorphismAndDeclaration (Morphism(..),Morphisms
                                  ,Declaration(..),Declarations
                                  ,makeDeclaration,makeMph
                                  ,makeInline,inline
                                  ,isIdent
                                  ,isSgn,mIs
                                  ,isProperty
                                  ,applyM)
where
   import Adl.FilePos      (FilePos(..),Numbered(..))
   import Adl.Concept      (Concept,Concepts,Association(..),Sign,MorphicId(..),Morphic(..)
                           ,isSingleton)
   import Adl.Prop         (Prop(..),Props,flipProps)
   import Adl.Pair         (Pairs,flipPair) 
   import Strings          (chain, unCap)
   import CommonClasses    (Identified(..),showSign
                           , ABoolAlg, SelfExplained(..))    
   import Collection       (Collection ((>-)))
   import Data.Explain
   import Languages        (Lang(..),plural)
   import Char             (toLower)
   import Text.Pandoc
   import Options
   import Strings          (preciesEen)
   type Morphisms = [Morphism]
   data Morphism  = 
                   Mph  { mphnm :: String            -- ^ the name of the morphism. This is the same name as
                                                     --   the declaration that is bound to the morphism.
                                                     --    VRAAG: Waarom zou je dit attribuut opnemen? De naam van het morphisme is immers altijd gelijk aan de naam van de Declaration mphdcl ....
                                                     --    ANTWOORD: Tijdens het parsen, tot het moment dat de declaration aan het Morphism is gekoppeld, moet de naam van het Morphism bekend zijn. Nadat het morphisme gebonden is aan een declaration moet de naam van het morphisme gelijk zijn aan de naam van zijn mphdcl.
                        , mphpos :: FilePos          -- ^ the position of the rule in which the morphism occurs
                        , mphats :: Concepts         -- ^ the attributes specified inline
                        , mphtyp :: Sign             -- ^ the allocated type. Together with the name, this forms the declaration.
                        , mphyin :: Bool             -- ^ the 'yin' factor. If true, a declaration is bound in the same direction as the morphism. If false, binding occurs in the opposite direction.
                        , mphdcl :: Declaration      -- ^ the declaration bound to this morphism.
                                                     --   If not mphyin, then target m<=source (mphdcl m) and source m<=target (mphdcl m). In this case, we write m~ (pronounce: m-flip or m-wok)
                                                     --   If mphyin, then source m<=source (mphdcl m) and target m<=target (mphdcl m). In this case, we write m
                        }
                  | I   { mphats :: Concepts         -- ^ the (optional) attribute specified inline. ADL syntax allows at most one concept in this list.
                        , mphgen ::  Concept         -- ^ the generic concept  
                        , mphspc ::  Concept         -- ^ the specific concept
                        , mphyin ::  Bool            -- ^ the 'yin' factor. If true, the specific concept is source and the generic concept is target. If false, the other way around.
                        } 
                  | V   { mphats :: Concepts         -- ^ the (optional) attributes specified inline.
                        , mphtyp :: Sign             -- ^ the allocated type.
                        }
                  -- | Een Mp1 is een deelverzameling van I, zou dus vervangen moeten worden voor I van een Mp1-type
                  | Mp1 { mph1val :: String          -- ^ the value of the one morphism
                        , mphats  :: Concepts        -- ^ the (optional) attribute specified inline. ADL syntax allows at most one concept in this list.
                        , mph1typ :: Concept         -- ^ the allocated type.
                        }  

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Morphism                      ***
-- \***********************************************************************
   makeInline :: Morphism -> Morphism
   makeInline m = case m of
                    Mph{mphyin=False} -> m{mphtyp = rev (mphtyp m), mphyin=True}
                    _                 -> m
                    where rev (a,b) = (b,a)

   instance Eq Morphism where
 --   m == m' = name m==name m' && source m==source m' && target m==target m' && yin==yin'
    Mph nm _ _ (a,b) yin _ == Mph nm' _ _ (a',b') yin' _ = nm==nm' && yin==yin' && a==a' && b==b'
    I _ g s yin            == I _ g' s' yin'             = if yin==yin' then g==g' && s==s' else g==s' && s==g'
    V _ (a,b)              == V _ (a',b')                = a==a' && b==b'
    Mp1 s _ c              == Mp1 s' _ c'                = s==s' && c==c'
    _ == _ = False

   instance Show Morphism where
    showsPrec _ m = case m of
      Mph{} -> showString (name m++
               (if inline m 
                then showSign [source m,target m] 
                else showSign [target m,source m]++"~"))
      I{}   -> showString ("I"++ if null (mphats m) then "" else show (mphats m))
      V{}   -> showString ("V"++ if null (mphats m) then "" else show (mphats m))
      Mp1{} -> showString ("Mp1 "++show (mph1val m)++" "++ if null (mphats m) then "" else show (mphats m))

   instance Ord Morphism where
    a <= b = source a <= source b && target a <= target b

   instance Identified Morphism where
    name m = name (makeDeclaration m)

   instance Association Morphism where
    sign   m@Mph{}               = mphtyp m    -- DAAROM: dit is niet afhankelijk van mphyin. mphtyp geeft het actuele type weer van dit morphisme. Yin regelt de verhouding tussen morfisme m en de bijbehorende declaratie, mphdcl m. Yin heeft dan ook geen invloed op het type van m (zijnde mphtyp m).
    sign   (I _ g s yin)         = if yin then (s,g) else (g,s)
    sign   (V _ (a,b))           = (a,b)
    sign   m@Mp1{}               = if null (mphats m) then (mph1typ m,mph1typ m) else (head (mphats m),last (mphats m))
    source m = source (sign m)
    target m = target (sign m)
   instance Numbered Morphism where
    pos m = case m of
             Mph{} ->  mphpos m
             _     ->  Nowhere
    nr m = nr (makeDeclaration m)

   instance MorphicId Morphism where
    isIdent mph = case mph of
                   I{}   -> True       -- > tells whether the argument is equivalent to I
                   V{}   -> source mph == target mph && isSingleton (source mph)
                   _     -> False
   
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
           I{}                 -> mph{ mphyin = not (mphyin mph)}
           Mp1{}               -> mph
    isProp mph = case mph of
           Mph{}               -> null ([Asy,Sym]>-multiplicities mph)
           V{}                 -> homogeneous(mphtyp mph) && singleton(source (mphtyp mph))
           I{}                 -> True
           Mp1{}               -> True
    isNot mph  = isNot (makeDeclaration mph)   -- > tells whether the argument is equivalent to I-
    isTrue mph = case mph of
           Mph{}               -> False
           V{}                 -> True
           I{}                 -> singleton (mphspc mph)
           Mp1{}               -> False
    isFalse _   = False
    isSignal mph = isSignal (makeDeclaration mph)


   makeDeclaration :: Morphism -> Declaration
   makeDeclaration m = case m of
               Mph{} -> mphdcl m
               I{}   -> Isn{ despc = mphspc  m, degen = mphgen  m}   -- WAAROM?? Stef, waarom wordt de yin hier niet gebruikt?? Is dat niet gewoon FOUT?
               V{}   -> Vs { degen = source  (sign m), despc = target (sign m)}
               Mp1{} -> Isn{ despc = mph1typ m, degen = mph1typ m}

   isProperty :: Morphism -> Bool
   isProperty mph   = null([Sym,Asy]>-multiplicities mph)
    
   inline :: Morphism -> Bool
   inline m =  case m of
                Mph{} -> mphyin m
                I{}   -> True    --WAAROM? Stef, wat is de reden van de mphyin bij I{} ?? Verwijderen of in werking stellen. Nu is het half, en dus fout!
                V{}   -> True
                Mp1{} -> True

   type Declarations = [Declaration]
   data Declaration = 
           Sgn { decnm   :: String  -- ^ the name of the declaration
               , desrc   :: Concept -- ^ the source concept of the declaration
               , detrg   :: Concept -- ^ the target concept of the declaration
                 --multiplicities returns decprps_calc so if you only need the user defined properties do not use multiplicities but decprps
               , decprps :: Props   -- ^ the user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx)
               , decprps_calc :: Props   -- ^ the calculated and user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx)
               , decprL  :: String  -- ^ three strings, which form the pragma. E.g. if pragma consists of the three strings: "Person ", " is married to person ", and " in Vegas."
               , decprM  :: String  -- ^    then a tuple ("Peter","Jane") in the list of links means that Person Peter is married to person Jane in Vegas.
               , decprR  :: String
               , decpopu :: Pairs   -- ^ the list of tuples, of which the relation consists.
               , decfpos :: FilePos -- ^ the position in the ADL source file where this declaration is declared.
               , decid   :: Int     -- ^ a unique number that can be used to identify the relation
               , deciss  :: Bool    -- ^ if true, this is a signal relation; otherwise it is an ordinary relation.
               , decusr  :: Bool    -- ^ if true, this relation is declared in the ADL-script; otherwise it was generated by ADL.
               , decpat  :: String  -- ^ the pattern where this declaration has been declared.
               , decplug :: Bool    -- ^ if true, this relation may not be stored in or retrieved from the standard database (it should be gotten from a Plug of some sort instead)
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
    showsPrec _ d
     = showString (chain " " ([decnm d,"::",name (desrc d),"*",name (detrg d),show (decprps_calc d),"PRAGMA",show (decprL d),show (decprM d),show (decprR d)] {- obsolete: ++if null (decexpl d) then [] else ["EXPLANATION",show (decexpl d)] -} ))
   instance Identified Declaration where
    name d@Sgn{}   = decnm d
    name Isn{}     = "I"
    name Iscompl{} = "-I"
    name Vs{}      = "V"

   instance Association Declaration where
      source d = case d of
                   Sgn {}    -> desrc d
                   Isn {}    -> despc d
                   Iscompl{} -> despc d
                   Vs {}     -> degen d
      target d = case d of
                   Sgn {}    -> detrg d
                   Isn {}    -> degen d
                   Iscompl{} -> degen d
                   Vs {}     -> despc d
    --sign is vanzelf al geregeld...

   instance Numbered Declaration where
    pos d = case d of
              Sgn{} -> decfpos d
              _     -> Nowhere
    nr  d = case d of
              Sgn{} -> decid d
              _     -> 0

   instance MorphicId Declaration where 
    isIdent d = case d of
                 Isn{} -> True   -- > tells whether the argument is equivalent to I
                 _     -> False

   instance Morphic Declaration where
    multiplicities d = case d of
           Sgn {}       -> decprps_calc d
           Isn{}        -> [Uni,Tot,Inj,Sym,Asy,Trn,Rfx]
                        ++ [Sur | degen d == despc d]
           Iscompl{}    -> [Sym]
           Vs{}         -> [Tot,Sur]
    flp d = case d of
           Sgn {}       -> d{ desrc   = detrg d
                            , detrg   = desrc d
                            , decprps = flipProps (decprps d)
                            , decprps_calc = flipProps (decprps_calc d)
                            , decprL  = ""
                            , decprM  = ""
                            , decprR  = ""
                            , decpopu = map flipPair (decpopu d)
                            }
           Isn{}        -> d
           Iscompl{}    -> d
           Vs{}         -> d
    isProp d = case d of         -- > tells whether the argument is a property.
           Sgn {}       -> null ([Asy,Sym]>-multiplicities d)
           Isn{}        -> True
           Iscompl{}    -> False
           Vs{}         -> (degen d == despc d) && singleton (degen d)
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


   -- | Deze declaratie is de reden dat Declaration en Morphism in precies een module moeten zitten.
   makeMph :: Declaration -> Morphism
   makeMph d = Mph (name d)     -- mphnm 
                   (pos d)      -- mphpos
                   []           -- mphats
                   (sign d)     -- mphtyp
                   True         -- mphyin
                   d            -- mphdcl

   mIs :: Concept -> Morphism
   mIs c = I [] c c True

   instance ABoolAlg Morphism  -- SJ  2007/09/14: This is used solely for drawing conceptual graphs.

   instance SelfExplained Declaration where
     autoExplain flags d = [explainParagraph flags{language=Dutch}   dutchInlines] 
                       ++  [explainParagraph flags{language=English} englishInlines]
      where dutchInlines 
                 | null ([Sym,Asy]         >- multiplicities d) = [Emph [Str (name d)]]
                                                                ++[Str " is een eigenschap van "]
                                                                ++[Str ((unCap.plural Dutch .name.source) d)]
                                                                ++[Str "."]
		         | null ([Sym,Rfx,Trn]     >- multiplicities d) = [Emph [Str (name d)]]
                                                                ++[Str " is een equivalentierelatie tussen "]
                                                                ++[Str ((unCap.plural Dutch .name.source) d)]
                                                                ++[Str "."]
		         | null ([Asy,Trn]         >- multiplicities d) = [Emph [Str (name d)]]
                                                                ++[Str " is een ordeningsrelatie tussen "]
                                                                ++[Str ((unCap.plural Dutch .name.source) d)]
                                                                ++[Str "."]
		         | null ([Uni,Tot,Inj,Sur] >- multiplicities d) = applyM'' d [Str ("Elke "++(unCap.name.source) d)] 
		                                                                     [Str ("precies "++preciesEen++" "++(unCap.name.target) d)] 
		                                                        ++[Str " en vice versa."]
		         | null ([Uni,Tot,Inj]     >- multiplicities d) = applyM'' d [Str ("Elke "++(unCap.name.source) d)]
		                                                                    [Str ("precies "++preciesEen++" "++(unCap.name.target) d)]
		                                                        ++[Str ", maar niet voor elke "]
		                                                        ++[Str ((unCap.name.target) d)]
		                                                        ++[Str (" hoeft er een "++(unCap.name.source) d++" te zijn.")]
		         | null ([Uni,Tot,    Sur] >- multiplicities d) = applyM'' d [Str ("Elke "++(unCap.name.source) d)]
		                                                                     [Str ("precies "++preciesEen++" "++(unCap.name.target) d)]
		                                                        ++[Str (", maar elke "++(unCap.name.target) d++" is gerelateerd aan "++preciesEen++" of meer "++(unCap.plural Dutch .name.source) d++".")]
		         | null ([Uni,    Inj,Sur] >- multiplicities d) = [Str ("Er is precies "++preciesEen++" "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: " )]
		                                                        ++applyM'' d [Str "b"] [Str "a"]
		                                                        ++[Str (", maar niet voor elke "++(unCap.name.source) d++" hoeft er een "++(unCap.name.target) d++" te zijn.")]
		         | null ([    Tot,Inj,Sur] >- multiplicities d) = [Str ("Er is precies "++preciesEen++" "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: ")]
		                                                        ++applyM'' d [Str "b"] [Str "a"]
		                                                        ++[Str (", maar elke "++(unCap.name.source) d++" mag gerelateerd zijn aan meerdere "++(unCap.plural Dutch .name.target) d++".")]
		         | null ([Uni,Tot        ] >- multiplicities d) = applyM'' d [Str ("Elke "++(unCap.name.source) d)] 
		                                                                     [Str ("precies "++preciesEen++" "++(unCap.name.target) d)]
		                                                        ++[Str "."]
		         | null ([Uni,    Inj    ] >- multiplicities d) = applyM'' d [Str ("Elke "++(unCap.name.source) d)] 
		                                                                     [Str ("ten hoogste "++preciesEen++" "++(unCap.name.target) d)]
		                                                        ++[Str (" en elke "++(unCap.name.target) d++" is gerelateerd aan ten hoogste "++preciesEen++" "++(unCap.name.source) d++".")]
		         | null ([Uni,        Sur] >- multiplicities d) = applyM'' d [Str ("Elke "++(unCap.name.source) d)]
		                                                                     [Str ("ten hoogste "++preciesEen++" "++(unCap.name.target) d)]
		                                                        ++[Str (", terwijl elke "++(unCap.name.target) d++" is gerelateerd aan tenminste "++preciesEen++" "++(unCap.name.source) d++".")]
		         | null ([    Tot,Inj    ] >- multiplicities d) = applyM'' d [Str ("Elke "++(unCap.name.source) d)]
		                                                                     [Str ("tenminste "++preciesEen++" "++(unCap.name.target) d)]
		                                                        ++[Str (", terwijl elke "++(unCap.name.target) d++" is gerelateerd aan ten hoogste "++preciesEen++" "++(unCap.name.source) d++".")]
		         | null ([    Tot,    Sur] >- multiplicities d) = applyM'' d [Str ("Elke "++(unCap.name.source) d)]
		                                                                     [Str ("tenminste "++preciesEen++" "++(unCap.name.target) d)]
		                                                        ++[Str (" en elke "++(unCap.name.target) d++" is gerelateerd aan tenminste "++preciesEen++" "++(unCap.name.source) d++".")]
		         | null ([        Inj,Sur] >- multiplicities d) = [Str ("Er is precies "++preciesEen++" "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: ")]
		                                                        ++applyM'' d [Str "b"] [Str "a"]
		                                                        ++[Str "."]
		         | null ([            Sur] >- multiplicities d) = [Str ("Er is tenminste "++preciesEen++" "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: ")]
		                                                        ++applyM'' d [Str "b"] [Str "a"]
		                                                        ++[Str "."]
		         | null ([        Inj    ] >- multiplicities d) = [Str ("Er is hooguit "++preciesEen++" "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: ")]
		                                                        ++applyM'' d [Str "b"] [Str "a"]
		                                                        ++[Str "."]
		         | null ([    Tot        ] >- multiplicities d) = applyM'' d [Str ("Elke "++(unCap.name.source) d)] 
		                                                                     [Str ("tenminste "++preciesEen++" "++(unCap.name.target) d)]
		                                                        ++[Str "."]
		         | null ([Uni            ] >- multiplicities d) = applyM'' d [Str ("Elke "++(unCap.name.source) d)]
		                                                                     [Str ("nul of "++preciesEen++" "++(unCap.name.target) d)]
		                                                        ++[Str "."]
		         | otherwise                                    = [Str "De zin: "]
		                                                        ++[Quoted DoubleQuote 
		                                                                (applyM'' d [Str ((var [].source) d)]
		                                                                            [Str ((var [source d].target) d)])
		                                                          ]
		                                                        ++[Str (" heeft betekenis (dus: is waar of niet waar) voor een "++(unCap.name.source) d++" ")]
		                                                        ++[Str ((var [].source) d)]
		                                                        ++[Str (" en een "++(unCap.name.target) d++" ")]
		                                                        ++[Str ((var [source d].target) d)]
		                                                        ++[Str "."]
		                                                          
            englishInlines 
		         | null ([Sym,Asy]         >- multiplicities d) = [Emph [Str (name d)]]
                                                                ++[Str " is a property of "]
                                                                ++[Str ((unCap.plural English .name.source) d)]
                                                                ++[Str "."]
		         | null ([Sym,Rfx,Trn]     >- multiplicities d) = [Emph [Str (name d)]]
                                                                ++[Str " is an equivalence relation on "]
                                                                ++[Str ((unCap.plural English .name.source) d)]
                                                                ++[Str "."]
		         | null ([Asy,Trn]         >- multiplicities d) = [Emph [Str (name d)]]
                                                                ++[Str " is an ordering relation on "]
                                                                ++[Str ((unCap.plural English .name.source) d)]
                                                                ++[Str "."]
		         | null ([Uni,Tot,Inj,Sur] >- multiplicities d) = applyM'' d [Str ("each "++(unCap.name.source) d)]
		                                                                     [Str ("exactly one "++(unCap.name.target) d)]
		                                                        ++[Str " and vice versa."]
		         | null ([Uni,Tot,Inj    ] >- multiplicities d) = applyM'' d [Str ("each "++(unCap.name.source) d)]
		                                                                     [Str ("exactly one "++(unCap.name.target) d)]
		                                                        ++[Str ", but not for each "]
		                                                        ++[Str ((unCap.name.target) d++" there must be a "++(unCap.name.source) d)]
		                                                        ++[Str "."]
		         | null ([Uni,Tot,    Sur] >- multiplicities d) = applyM'' d [Str ("each "++(unCap.name.source) d)]
		                                                                     [Str ("exactly one "++(unCap.name.target) d)]
		                                                        ++[Str ", but each "]
		                                                        ++[Str ((unCap.name.target) d++" is related to one or more "++(unCap.plural English .name.source) d)]
		                                                        ++[Str "."]
		         | null ([Uni,    Inj,Sur] >- multiplicities d) = [Str ("There is exactly one "++(unCap.name.source) d++" (a) for each "++(unCap.name.target) d++" (b), for which: ")]
		                                                        ++applyM'' d [Str "b"] [Str "a"]
		                                                        ++[Str (", but not for each "++(unCap.name.source) d++" there must be a "++(unCap.name.target) d++".")]
		         | null ([    Tot,Inj,Sur] >- multiplicities d) = [Str ("There is exactly one "++(unCap.name.source) d++" (a) for each "++(unCap.name.target) d++" (b), for which: ")]
		                                                        ++applyM'' d [Str "b"] [Str "a"]
		                                                        ++[Str (", but each "++(unCap.name.source) d++" is related to one or more "++(unCap.plural English .name.target) d)]
		                                                        ++[Str "."]
		         | null ([Uni,Tot        ] >- multiplicities d) = applyM'' d [Str ("each "++(unCap.name.source) d)]
		                                                                    [Str ("exactly one "++(unCap.name.target) d)]
		                                                        ++[Str "."]
		         | null ([Uni,    Inj    ] >- multiplicities d) = applyM'' d [Str ("each "++(unCap.name.source) d)]
		                                                                    [Str ("at most one "++(unCap.name.target) d)]
		                                                        ++[Str (" and each "++(unCap.name.target) d++" is related to at most one "++(unCap.name.source) d)]
		                                                        ++[Str "."]
		         | null ([Uni,        Sur] >- multiplicities d) = applyM'' d [Str ("each "++(unCap.name.source) d)]
		                                                                     [Str ("at most one "++(unCap.name.target) d)]
		                                                        ++[Str (", whereas each "++(unCap.name.target) d++" is related to at least one "++(unCap.name.source) d)]
		                                                        ++[Str "."]
		         | null ([    Tot,Inj    ] >- multiplicities d) = applyM'' d [Str ("each "++(unCap.name.source) d)]
		                                                                     [Str ("at least one "++(unCap.name.target) d)]
		                                                        ++[Str (", whereas each "++(unCap.name.target) d++" is related to at most one "++(unCap.name.source) d)]
		                                                        ++[Str "."]
		         | null ([    Tot,    Sur] >- multiplicities d) = applyM'' d [Str ("each "++(unCap.name.source) d)]
		                                                                     [Str ("at least one "++(unCap.name.target) d)]
		                                                        ++[Str (" and each "++(unCap.name.target) d++" is related to at least one "++(unCap.name.source) d)]
		                                                        ++[Str "."]
		         | null ([        Inj,Sur] >- multiplicities d) = [Str ("There is exactly one "++(unCap.name.source) d++" (a) for each "++(unCap.name.target) d++" (b), for which: ")]
		                                                        ++applyM'' d [Str "b"] [Str "a"]
		                                                        ++[Str "."]
		         | null ([            Sur] >- multiplicities d) = [Str ("There is at least one "++(unCap.name.source) d++" (a) for each "++(unCap.name.target) d++" (b), for which: ")]
		                                                        ++applyM'' d [Str "b"] [Str "a"]
		                                                        ++[Str "."]
		         | null ([        Inj    ] >- multiplicities d) = [Str ("There is at most one "++(unCap.name.source) d++" (a) for each "++(unCap.name.target) d++" (b), for which: ")]
		                                                        ++applyM'' d [Str "b"] [Str "a"]
		                                                        ++[Str "."]
		         | null ([    Tot        ] >- multiplicities d) = applyM'' d [Str ("each "++(unCap.name.source) d)]
		                                                                     [Str ("at least one "++(unCap.name.target) d)]
		                                                        ++[Str "."]
		         | null ([Uni            ] >- multiplicities d) = applyM'' d [Str ("each "++(unCap.name.source) d)]
		                                                                     [Str ("zero or one "++(unCap.name.target) d)]
		                                                        ++[Str "."]
		         | otherwise                                    = [Str "The sentence: "]
		                                                        ++[Quoted DoubleQuote
		                                                              (applyM'' d [Str ((var [].source) d)]
		                                                                          [Str ((var [source d].target) d)])]
		                                                        ++[Str (" is meaningful (i.e. it is either true or false) for any "++(unCap.name.source) d++" ")]
		                                                        ++[Str ((var [].source) d)]
		                                                        ++[Str (" and "++(unCap.name.target) d++" ")]
		                                                        ++[Str ((var [source d].target) d)]
		                                                        ++[Str "."]
            

   isSgn :: Declaration -> Bool
   isSgn Sgn{} = True
   isSgn  _    = False

   applyM'' :: Declaration -> [Inline] -> [Inline] -> [Inline]
   applyM'' decl d c =
      case decl of
        Sgn{}     -> if null (prL++prM++prR) 
                       then d++[Str (" "++decnm decl++" ")]++c 
                       else [Str prL]++d++[Str prM]++c++[Str prR]
           where prL = decprL decl
                 prM = decprM decl
                 prR = decprR decl
        Isn{}     -> d++[Str " equals "]++c
        Iscompl{} -> d++[Str " differs from "]++c
        Vs{}      -> [Str (show True)]
   
   
   applyM :: Declaration -> String -> String -> String    --TODO language afhankelijk maken. 
   applyM decl d c =
      case decl of
        Sgn{}     -> if null (prL++prM++prR) 
                       then d++" "++decnm decl++" "++c 
                       else prL++d++prM++c++prR
           where prL = decprL decl
                 prM = decprM decl
                 prR = decprR decl
        Isn{}     -> d++" equals "++c
        Iscompl{} -> d++" differs from "++c
        Vs{}      -> show True

   var :: Identified a => [a] -> a -> String     -- TODO Vervangen door mkvar, uit predLogic.hs
   var seen c = low c ++ ['\''| c'<-seen, low c == low c']
               where low idt= if null (name idt) then "x" else [(toLower.head.name) idt]
                        

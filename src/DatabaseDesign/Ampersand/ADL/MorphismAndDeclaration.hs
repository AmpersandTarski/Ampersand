{-# OPTIONS_GHC -Wall -XFunctionalDependencies -XFlexibleInstances -XFlexibleContexts -XUndecidableInstances -XMultiParamTypeClasses #-}
module DatabaseDesign.Ampersand.ADL.MorphismAndDeclaration (Relation(..),Association(..),Relational(..), mapMorphism
                                  ,Declaration(..),Identified(..),uniqueNames
                                  ,makeDeclaration,makeRelation
                                  ,inline
                                  ,isSgn,mIs
                                  ,showSign,applyM)
where
   import DatabaseDesign.Ampersand.ADL.FilePos      (FilePos(..),Numbered(..))
   import DatabaseDesign.Ampersand.ADL.Concept      (Concept(..),Conceptual(..),Signaling(..))
   import DatabaseDesign.Ampersand.ADL.Prop         (Prop(..),Props,flipProps)
   import DatabaseDesign.Ampersand.ADL.Pair         (Pairs,flipPair) 
   import Data.List
   import Char             (toLower)
   import Collection       (Collection ((>-)))
   import Auxiliaries      (eqCl)

  -- in the following class, variable rel is a relation type and concept is a concept type.
  -- For example: instance Association (Declaration Concept) Concept
   class Eq concept => Association rel concept | rel->concept where
     source, target :: rel -> concept      -- e.g. Declaration Concept -> Concept
     sign           :: rel -> (concept,concept)
     sign x = (source x,target x) 
     homogeneous :: rel  -> Bool
     homogeneous s = source s == target s

   class (Eq c, Association r c) => Relational r c | r->c where
    multiplicities :: Conceptual c => r -> [Prop]
    multiplicities _ = []  --WHY? Stef, is this correct as a default?
                           --BECAUSE! There is noting wrong here. It is as though the user never specified any multiplicity constraints....
    flp            :: r -> r
    isFlp          :: r -> Bool
    isFlp _ = False
--   isIdent        :: r -> Bool  -- > tells whether the argument is equivalent to I
    isProp         :: Conceptual c => r -> Bool  -- > tells whether the argument is r c property
    isNot          :: Conceptual c => r -> Bool  -- > tells whether the argument is equivalent to I-
    isTrue         :: Conceptual c => r -> Bool  -- > tells whether the argument is equivalent to V
    isFalse        :: Conceptual c => r -> Bool  -- > tells whether the argument is equivalent to V-
  --  isSignal       :: r -> Bool  -- > tells whether the argument refers to r  signal
  --  singleton      :: r -> Bool  -- > tells whether V=I
  --  singleton e     = isProp e && isTrue e
    equiv          :: r -> r -> Bool
    equiv m m' = source m==source m'&&target m==target m' || source m==target m'&&target m==source m'
    isFunction :: Conceptual c => r -> Bool     
    isFunction m   = null ([Uni,Tot]>-multiplicities m)
    isFlpFunction :: Conceptual c => r -> Bool
    isFlpFunction m = null ([Sur,Inj]>-multiplicities m)
    isTot :: Conceptual c => r -> Bool  -- 
    isTot d = Tot `elem` multiplicities d
    isUni :: Conceptual c => r -> Bool  -- 
    isUni d = Uni `elem` multiplicities d
    isSur :: Conceptual c => r -> Bool  -- 
    isSur d = Sur `elem` multiplicities d
    isInj :: Conceptual c => r -> Bool  -- 
    isInj d = Inj `elem` multiplicities d
    isRfx :: Conceptual c => r -> Bool  -- 
    isRfx d = Rfx `elem` multiplicities d
    isTrn :: Conceptual c => r -> Bool  -- 
    isTrn d = Trn `elem` multiplicities d
    isSym :: Conceptual c => r -> Bool  -- 
    isSym d = Sym `elem` multiplicities d
    isAsy :: Conceptual c => r -> Bool  -- 
    isAsy d = Asy `elem` multiplicities d
    makeInline :: r -> r
    makeInline m = if isFlp m then flp m else m
    isIdent :: Conceptual c => r -> Bool  -- > tells whether the argument is equivalent to I

   data Relation c = 
                   Rel  { relnm :: String            -- ^ the name of the morphism. This is the same name as
                                                     --   the declaration that is bound to the morphism.
                                                     --    VRAAG: Waarom zou je dit attribuut opnemen? De naam van het morphisme is immers altijd gelijk aan de naam van de Declaration reldcl ....
                                                     --    ANTWOORD: Tijdens het parsen, tot het moment dat de declaration aan het morphism is gekoppeld, moet de naam van het morphism bekend zijn. Nadat het morphisme gebonden is aan een declaration moet de naam van het morphisme gelijk zijn aan de naam van zijn reldcl.
                        , relpos :: FilePos          -- ^ the position of the rule in which the morphism occurs
                        , relats :: [c]              -- ^ the attributes specified inline
                        , relsrc :: c                -- ^ the source. Together with the target, this forms the type.
                        , reltrg :: c                -- ^ the target. Together with the source, this forms the type.
                        , relyin :: Bool             -- ^ the 'yin' factor. If true, a declaration is bound in the same direction as the morphism. If false, binding occurs in the opposite direction.
                        , reldcl :: Declaration c  -- ^ the declaration bound to this morphism.
                                                     --   If not relyin, then target m<=source (reldcl m) and source m<=target (reldcl m). In this case, we write m~ (pronounce: m-flip or m-wok)
                                                     --   If relyin, then source m<=source (reldcl m) and target m<=target (reldcl m). In this case, we write m
                        }
                  | I   { relats :: [c]              -- ^ the (optional) attribute specified inline. Ampersand syntax allows at most one concept in this list.
                        , relgen :: c                -- ^ the generic concept  
                        , relspc :: c                -- ^ the specific concept
                        , relyin :: Bool             -- ^ the 'yin' factor. If true, the specific concept is source and the generic concept is target. If false, the other way around.
                        } 
                  | V   { relats :: [c]              -- ^ the (optional) attributes specified inline.
                        , reltyp :: (c,c)            -- ^ the allocated type.
                        }
                  -- | Een Mp1 is een deelverzameling van I, zou dus vervangen moeten worden voor I van een Mp1-type
                  | Mp1 { rel1val :: String          -- ^ the value of the one morphism
                        , relats  :: [c]             -- ^ the (optional) attribute specified inline. Ampersand syntax allows at most one concept in this list.
                        , rel1typ :: c               -- ^ the allocated type.
                        }  

   mapMorphism :: Eq a => (a->b) -> Relation a -> Relation b
   mapMorphism f m@Rel{} = m{ relats = map f (relats m)
                            , relsrc = f (relsrc m)
                            , reltrg = f (reltrg m)
                            , reldcl = mapDeclaration f (reldcl m)
                            }
   mapMorphism f m@I  {} = m{ relats = map f (relats m)
                            , relspc = f (relspc m)
                            , relgen = f (relgen m)
                            }
   mapMorphism f m@V  {} = m{ relats = map f (relats m)
                            , reltyp = let (s,t)=sign m in (f s, f t)
                            }
   mapMorphism f m@Mp1{} = m{ relats  = map f (relats m)
                            , rel1typ = f (rel1typ m)
                            }

   class Identified a where
    name   :: a->String
    rename :: a->String->a
    rename x _ = error ("!Fatal (module MorphismAndDeclaration 114): some Identified element named " ++ name x ++ " cannot be renamed.")

   --the function uniqueNames ensures case-insensitive unique names like sql plug names
   uniqueNames :: (Identified a) => [String]->[a]->[a]
   uniqueNames taken xs
    = [p | cl<-eqCl (map toLower.name) xs  -- each equivalence class cl contains (identified a) with the same map toLower (name p)
         , p <-if name (head cl) `elem` taken || length cl>1
               then [rename p (name p++show i)| (p,i)<-zip cl [(1::Int)..]]
               else cl
      ]

   instance Identified a => Identified [a] where
    name [] = ""
    name (i:_) = name i

   instance Identified Concept where
    name (C {cptnm = nm}) = nm
    name S = "ONE"
    name Anything   = "Anything"
    name NOthing    = "NOthing"


-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Relation                      ***
-- \***********************************************************************

   
   instance Eq c => Eq (Relation c) where
 --   m == m' = name m==name m' && source m==source m' && target m==target m' && yin==yin'
    Rel nm _ _ a b yin _ == Rel nm' _ _ a' b' yin' _ = nm==nm' && yin==yin' && a==a' && b==b'
    I _ g s yin          == I _ g' s' yin'           = if yin==yin' then g==g' && s==s' else g==s' && s==g'
    V _ (a,b)            == V _ (a',b')              = a==a' && b==b'
    Mp1 s _ c            == Mp1 s' _ c'              = s==s' && c==c'
    _ == _ = False

   showSign :: Identified a => [a] -> String
   showSign cs = "["++(intercalate "*".map name) cs++"]"

   instance (Identified c, Eq c, Show c) => Show (Relation c) where
    showsPrec _ m = case m of
      Rel{} -> showString (relnm m++
               (if inline m 
                then showSign [source m,target m]
                else showSign [target m,source m]++"~"))
      I{}   -> showString ("I"++ if null (relats m) then "" else show (relats m))
      V{}   -> showString ("V"++ if null (relats m) then "" else show (relats m))
      Mp1{} -> showString ("Mp1 "++show (rel1val m)++" "++ if null (relats m) then "" else show (relats m))

   instance (Ord c, Association (Relation c) c) => Ord (Relation c) where
    a <= b = source a <= source b && target a <= target b

   instance (Identified c, Eq c) => Identified (Relation c) where
    name m = name (makeDeclaration m)

   instance (Eq c) => Association (Relation c) c where
    sign   m@Rel{}               = (source m, target m)    -- BECAUSE: (source m, target m) represents the actual type of this morphism. Yin takes care of the consistency with the underlying declaration, reldcl m.
    sign   (I _ g s yin)         = if yin then (s,g) else (g,s)
    sign   (V _ (a,b))           = (a,b)
    sign   m@Mp1{}               = if null (relats m) then (rel1typ m,rel1typ m) else (head (relats m),last (relats m))
    source m@Rel{} = relsrc m
    source m@I{}   = relspc m
    source m@V{}   = let (s,_) = sign m in s
    source m@Mp1{} = rel1typ m
    target m@Rel{} = reltrg m
    target m@I{}   = relgen m
    target m@V{}   = let (_,t) = sign m in t
    target m@Mp1{} = rel1typ m

   instance Eq c => Numbered (Relation c) where
    pos m = case m of
             Rel{} ->  relpos m
             _     ->  Nowhere
    nr m = nr (makeDeclaration m)

   instance Eq c => Signaling (Relation c) where
    isSignal rel = isSignal (makeDeclaration rel)

   instance (Eq c) => Relational (Relation c) c where
    multiplicities rel 
      = case rel of
           Rel{relyin = True}  -> multiplicities (reldcl rel)
           Rel{relyin = False} -> flipProps (multiplicities (reldcl rel))
           V {}                -> [Tot]
                                ++[Sur]
                                ++[Inj| isSingleton (source rel)]
                                ++[Uni| isSingleton (target rel)]
                                ++[Asy| homogeneous rel, isSingleton (source rel)]
                                ++[Sym| homogeneous rel]
                                ++[Rfx| homogeneous rel]
                                ++[Trn| homogeneous rel]
           I{}                 -> [Inj,Sur,Uni,Tot,Sym,Asy,Trn,Rfx]
           Mp1{}               -> [Inj,Uni,Sym,Asy,Trn]
    flp rel 
      = case rel of
           Rel{}               -> rel{ relats = reverse(relats rel)
                                     , relsrc = target rel
                                     , reltrg = source rel
                                     , relyin = not (relyin rel)
                                     }
           V{reltyp = (s,t)}   -> V  { relats = reverse(relats rel)
                                     , reltyp = (t,s)
                                     }
           I{}                 -> rel{ relyin = not (relyin rel)}
           Mp1{}               -> rel
    isProp rel = case rel of
           Rel{}               -> null ([Asy,Sym]>-multiplicities (reldcl rel))
           V{}                 -> homogeneous rel && isSingleton (source rel)
           I{}                 -> True
           Mp1{}               -> True
    isNot rel  = isNot (makeDeclaration rel)   -- > tells whether the argument is equivalent to I-
    isTrue rel = case rel of
           Rel{}               -> False
           V{}                 -> True
           I{}                 -> False
           Mp1{}               -> False
    isFalse _   = False
    isIdent rel = case rel of
                   I{}   -> True       -- > tells whether the argument is equivalent to I
                   V{}   -> source rel == target rel && isSingleton (source rel)
                   _     -> False
   
   mIs :: concept -> Relation concept
   mIs c = I [] c c True

   makeDeclaration :: Eq c => Relation c -> Declaration c
   makeDeclaration m = case m of
               Rel{} -> reldcl m
               I{}   -> Isn{ despc = relspc  m, degen = relgen  m}   -- WHY?? Stef, waarom wordt de yin hier niet gebruikt?? Is dat niet gewoon FOUT?
               V{}   -> let (s,t) = sign m in Vs { desrc = s, detrg = t}
               Mp1{} -> Isn{ despc = rel1typ m, degen = rel1typ m}   -- WHY?? This is weird. Is this correct?
    
   inline :: Relation c -> Bool
   inline m =  case m of
                Rel{} -> relyin m
                I{}   -> True    --WHY? Stef, wat is de reden van de relyin bij I{} ?? Verwijderen of in werking stellen. Nu is het half, en dus fout!
                V{}   -> True
                Mp1{} -> True

   data Declaration c = 
           Sgn { decnm   :: String  -- ^ the name of the declaration
               , desrc   :: c       -- ^ the source concept of the declaration
               , detrg   :: c       -- ^ the target concept of the declaration
                 --multiplicities returns decprps_calc so if you only need the user defined properties do not use multiplicities but decprps
               , decprps :: Props   -- ^ the user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx)
               , decprps_calc :: Props   -- ^ the calculated and user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx)
               , decprL  :: String  -- ^ three strings, which form the pragma. E.g. if pragma consists of the three strings: "Person ", " is married to person ", and " in Vegas."
               , decprM  :: String  -- ^    then a tuple ("Peter","Jane") in the list of links means that Person Peter is married to person Jane in Vegas.
               , decprR  :: String
               , decpopu :: Pairs   -- ^ the list of tuples, of which the relation consists.
               , decfpos :: FilePos -- ^ the position in the Ampersand source file where this declaration is declared.
               , decid   :: Int     -- ^ a unique number that can be used to identify the relation
               , deciss  :: Bool    -- ^ if true, this is a signal relation; otherwise it is an ordinary relation.
               , decusr  :: Bool    -- ^ if true, this relation is declared in the Ampersand script; otherwise it was generated by Ampersand.
               , decpat  :: String  -- ^ the pattern where this declaration has been declared.
               , decplug :: Bool    -- ^ if true, this relation may not be stored in or retrieved from the standard database (it should be gotten from a Plug of some sort instead)
               }
          | Isn 
               { degen   :: c       -- ^ The generic concept
               , despc   :: c       -- ^ The specific concept
               }
          | Iscompl 
               { degen   :: c
               , despc   :: c
               }
          | Vs 
               { desrc   :: c
               , detrg   :: c
               }

   mapDeclaration :: (a->b) -> Declaration a -> Declaration b
   mapDeclaration f d@Sgn{}
            = d{ desrc = f (desrc d)
               , detrg = f (detrg d)
               }
   mapDeclaration f d@Isn{}
            = d{ degen = f (degen d)
               , despc = f (despc d)
               }
   mapDeclaration f d@Iscompl{}
            = d{ degen = f (degen d)
               , despc = f (despc d)
               }
   mapDeclaration f d@Vs{}
            = d{ desrc = f (desrc d)
               , detrg = f (detrg d)
               }
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Declaration c               ***
-- \***********************************************************************
   instance Eq c => Eq (Declaration c) where
      d@Sgn{}     == d'@Sgn{}     = decnm d==decnm d' && desrc d==desrc d' && detrg d==detrg d'
      d@Isn{}     == d'@Isn{}     = despc d==despc d'
      d@Iscompl{} == d'@Iscompl{} = despc d==despc d'
      d@Vs{}      == d'@Vs{}      = desrc d==desrc d' && detrg d==detrg d'
      _ == _ = False
   instance (Identified c,Show c) => Show (Declaration c) where
    showsPrec _ d
     = showString (intercalate " " ([decnm d,"::",name (desrc d),"*",name (detrg d),show (decprps_calc d),"PRAGMA",show (decprL d),show (decprM d),show (decprR d)] {- obsolete: ++if null (decexpl d) then [] else ["EXPLANATION",show (decexpl d)] -} ))
   instance Identified (Declaration c) where
    name d@Sgn{}   = decnm d
    name Isn{}     = "I"
    name Iscompl{} = "-I"
    name Vs{}      = "V"

   instance Eq c => Association (Declaration c) c where
      source d = case d of
                   Sgn {}    -> desrc d
                   Isn {}    -> despc d
                   Iscompl{} -> despc d
                   Vs {}     -> desrc d
      target d = case d of
                   Sgn {}    -> detrg d
                   Isn {}    -> degen d
                   Iscompl{} -> degen d
                   Vs {}     -> detrg d
    --sign has already been taken care of in the class definition...

   instance Numbered (Declaration c) where
    pos d = case d of
              Sgn{} -> decfpos d
              _     -> Nowhere
    nr  d = case d of
              Sgn{} -> decid d
              _     -> 0

   instance Signaling (Declaration c) where
    isSignal d = case d of
           Sgn {}       -> deciss d
           _            -> False

   instance Conceptual c => Relational (Declaration c) c where
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
           Vs{}         -> (desrc d == detrg d) && isSingleton (desrc d)
    isNot d = case d of          -- > tells whether the argument is equivalent to I-
           Iscompl{}    -> True   
           _            -> False
    isTrue d = case d of 
           Vs{}         -> True
           _            -> False
    isFalse _ = False
    isIdent d = case d of
                 Isn{} -> True   -- > tells whether the argument is equivalent to I
                 _     -> False

   -- | Deze declaratie is de reden dat Declaration en Relation in precies een module moeten zitten.
--   makeRelation :: Declaration Concept -> Relation Concept
   makeRelation :: Eq concept => Declaration concept -> Relation concept
   makeRelation d
    = Rel { relnm  = name d
          , relpos = pos d
          , relats = []
          , relsrc = desrc d
          , reltrg = detrg d
          , relyin = True
          , reldcl = d
          }

--   instance SpecHierarchy (Relation Concept) -- SJ  2007/09/14: This is used solely for drawing conceptual graphs.

   isSgn :: Declaration c -> Bool
   isSgn Sgn{} = True
   isSgn  _    = False

   
   
   applyM :: Declaration c -> String -> String -> String    --TODO language afhankelijk maken. 
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

                        

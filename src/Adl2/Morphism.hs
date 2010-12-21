{-# OPTIONS_GHC -Wall #-}
module Adl2.Morphism where
  import Adl2.Relation
  import Adl.Declaration
  import Adl.FilePos
  
  data Morphism c          -- ^A relation for during parsing
     = MpRel { mphrel :: Relation c -- ^The actual relation
             , mphdcl :: Declaration -- ^Where the morphism was declared
             , mphpos :: FilePos -- ^File and position of this relation
             , mphats :: [c] -- ^The specified concepts
             }

  instance Eq (Morphism c) where
    m == m' = mphrel m == mphrel m'
--   m == m' = name m==name m' && source m==source m' && target m==target m' && yin==yin'
--   Mph nm _ _ (a,b) yin _ == Mph nm' _ _ (a',b') yin' _ = nm==nm' && yin==yin' && a==a' && b==b'
--   I _ g s yin            == I _ g' s' yin'             = if yin==yin' then g==g' && s==s' else g==s' && s==g'
--   V _ (a,b)              == V _ (a',b')                = a==a' && b==b'
--   Mp1 s _ c              == Mp1 s' _ c'                = s==s' && c==c'
--   _ == _ = False

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
             
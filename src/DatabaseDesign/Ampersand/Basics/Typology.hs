{-# OPTIONS_GHC -Wall #-}
-- | Taxonomy is the practice and science of classification. The word comes from the Greek taxis (meaning 'order', 'arrangement') and nomos ('law' or 'science'). Taxonomies, or taxonomic schemes, are composed of taxonomic units known as taxa (singular taxon), or kinds of things that are arranged frequently in a hierarchical structure. Typically they are related by subtype-supertype relationships, also called parent-child relationships. In such a subtype-supertype relationship the subtype kind of thing has by definition the same constraints as the supertype kind of thing plus one or more additional constraints. For example, car is a subtype of vehicle. So any car is also a vehicle, but not every vehicle is a car. Therefore, a thing needs to satisfy more constraints to be a car than to be a vehicle.
--   (Source: <en.wikipedia.org/wiki/Taxonomy> )
module DatabaseDesign.Ampersand.Basics.Typology
          ( Inheritance(Isa)
          , Typologic(typology)
          , genEq
          )
where
   import DatabaseDesign.Ampersand.Basics.Collection     ( Collection(..))

   data Inheritance a = Isa [(a,a)] [a] deriving (Eq,Show)

   type IsaPath a = [a]

   akin :: Eq a => IsaPath a -> IsaPath a -> Bool
   xs `akin` ys
    = head xs==head ys && last xs==last ys && (xs `sub` ys || ys `sub` xs)

   sub :: Eq a => IsaPath a -> IsaPath a -> Bool
   (x:xs) `sub` (y:ys) = if x==y then xs `sub` ys else xs `sub` (y:ys)
   _      `sub`    ys  = null ys

   data Typology a = Typ [IsaPath a] deriving Show

   genEq :: Eq a => Typology a -> a->a->Bool
   genEq (Typ world) left right = if left==right then True  else or[f left right ps|ps<-world]
     where
       f :: Eq a => a -> a -> [a] -> Bool
       f l r ps = not (null p2)
        where
         p1 = dropWhile (/=l) ps
         p2 = dropWhile (/=r) p1


   class Eq a => Typologic a where
    match    :: a -> a -> Bool
    match       = (==)
    mul      :: [a] -> [a] -> [a]
    xs `mul` ys = xs++tail ys
    typology :: Inheritance a -> Typology a
    typology (Isa rs cs)
     = Typ ([fst (head pth): map snd pth| pth<-clos fst snd rs, not (null pth)] ++ [[c]| c<-cs])
         where
            clos :: (Eq a, Eq b) => (b->a) -> (b->a) -> [b] -> [[b]] 
            clos left right tuples
              = [[e]| e<-tuples, right e==left e]++(unsublist.f 1) [[e]| e<-tuples, right e/=left e]
                where
                 m = length (rd [c|ts<-tuples, c<-[left ts,right ts]]) `min` length tuples  -- maximum path length possible
                 f n pths
                  = if n>length tuples then pths else
                    f (2*n) (long++pths)
                    where long = [xs++(ys>-xs)| xs<-pths, ys<-pths                         -- cartesian product
                                              , n-length xs < length ys                    -- so: n < length (xs++ys)
                                              , length ys <= (2*n `min` m)-length xs       -- so:     length (xs++ys) <=  (2*n `min` m)
                                              , right (last xs)==left (head ys)            -- join
                                              , not (or [t `isPrefix` xs| t<-tails ys])    -- no cycles
                                              ]
                 tails ts@(_:_) = ts: tails (tail ts)
                 tails [] = []
                 unsublist [] = []
                 unsublist (xs:xss) = xs: unsublist[ys| ys<-xss, not (ys `isSublist` xs)]

   isPrefix :: Eq a => [a] -> [a] -> Bool
   []     `isPrefix` _      = True
   (x:xs) `isPrefix` (y:ys) = x==y && xs `isPrefix` ys
   _      `isPrefix`  _     = False

   isSublist :: Eq a => [a] -> [a] -> Bool
   [] `isSublist` _  = True
   xs `isSublist` ys = xs `isPrefix` ys  ||  length xs<=length ys && xs `isSublist` tail ys

         

   compress :: Eq a => (IsaPath a->IsaPath a->Bool) -> [IsaPath a] -> [IsaPath a]
   compress f' paths
    = eqCl' paths
      where
       eqCl' [] = []
       eqCl' (x:xs)
        = l : eqCl' [e|e<-[x'|x'<-xs,x'/=l], not (f' l e)]
          where
           l = longest x xs
       longest l []     = l
       longest l (x:xs) | f' x l    = longest x xs
                        | otherwise = longest l xs


   instance Collection Inheritance where
    e `eleM` isa              = e `elem` (elems isa)
    empty                     = Isa [] []
    elems (Isa ts es)         = rd ([a|(a,_)<-ts]++[b|(_,b)<-ts]++es)
    Isa rx cx `uni` Isa ry cy = Isa (rd (rx++ry)) (rd (cx++cy))
    Isa rx cx `isc` Isa ry cy = Isa (rx `isc` ry) (cx `isc` cy)
    Isa rx cx >- Isa ry cy = Isa (rx>-ry) (cx>-cy)
    rd (Isa ts es)            = Isa (rd ts) (rd es)
    rd' _ (Isa _ _)         = error "!Fatal (module Typology 97): rd' not yet implemented for Isa"

   instance Collection Typology where
    eleM e (Typ pths) = or [e `elem` p| p<-pths]
    empty = Typ []
    elems (Typ paths) = rd (concat paths)
    Typ px `uni` Typ py = Typ (compress akin (px++py++[init r++r'| r<-px, r'<-py, last r==head r']))
    Typ _ `isc` Typ _ = error "!Fatal (module Typology 104): intersection not yet implemented"     --TODO
    Typ _ >-    Typ _ = error "!Fatal (module Typology 105): set difference not yet implemented"  --TODO
    rd (Typ pths) = Typ (rd pths)
    rd' _ (Typ _) = error "!Fatal (module Typology 107): rd' not yet implemented for Typologies"


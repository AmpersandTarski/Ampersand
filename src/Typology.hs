{-# OPTIONS_GHC -Wall #-}
-- | Taxonomy is the practice and science of classification. The word comes from the Greek taxis (meaning 'order', 'arrangement') and nomos ('law' or 'science'). Taxonomies, or taxonomic schemes, are composed of taxonomic units known as taxa (singular taxon), or kinds of things that are arranged frequently in a hierarchical structure. Typically they are related by subtype-supertype relationships, also called parent-child relationships. In such a subtype-supertype relationship the subtype kind of thing has by definition the same constraints as the supertype kind of thing plus one or more additional constraints. For example, car is a subtype of vehicle. So any car is also a vehicle, but not every vehicle is a car. Therefore, a thing needs to satisfy more constraints to be a car than to be a vehicle.
--   (Source: <en.wikipedia.org/wiki/Taxonomy> )
module Typology
          ( Inheritance(Isa)
          , Typologic(typology)
          , genEq
          )
where
   import Auxiliaries    ( clos )
   import Collection     ( Collection(..))

   data Inheritance a = Isa [(a,a)] [a] deriving (Eq,Show)

   type IsaPath a = [a]

   akin :: Eq a => IsaPath a -> IsaPath a -> Bool
   xs `akin` ys
    = head xs==head ys && last xs==last ys && (xs `sub` ys || ys `sub` xs)

--   kin :: Eq a => IsaPath a -> IsaPath a -> Bool
--   xs `kin` ys
--    = head xs==head ys && (xs `sub` ys || ys `sub` xs)

   sub :: Eq a => IsaPath a -> IsaPath a -> Bool
   (x:xs) `sub` (y:ys) = if x==y then xs `sub` ys else xs `sub` (y:ys)
   _      `sub`    ys  = null ys

   data Typology a = Typ [IsaPath a] deriving Show








   genEq :: Eq a => Typology a -> a->a->Bool
   genEq (Typ world) l r = if l==r then True  else or[f l r ps|ps<-world]
--   spcEq :: Eq a => Typology a -> a->a->Bool
--   spcEq (Typ world) l r = if l==r then True  else or[f r l ps|ps<-world]
--   gen   :: Eq a => Typology a -> a->a->Bool
--   gen   (Typ world) l r = if l==r then False else or[f l r ps|ps<-world]
--   spc   :: Eq a => Typology a -> a->a->Bool
--   spc   (Typ world) l r = if l==r then False else or[f r l ps|ps<-world]

   f :: Eq a => a -> a -> [a] -> Bool
   f l r ps = not (null p2)
    where
     p1 = dropWhile (/=l) ps
     p2 = dropWhile (/=r) p1

--   makeIsa :: Eq a => Typology a -> Inheritance a
--   makeIsa (Typ paths)
--     = Isa (rd [(a,b)|path<-paths, length path>1, (a,b)<-zip path (tail path)])
--           (rd [c| [c]<-paths])
--
--





   class Eq a => Typologic a where
    match    :: a -> a -> Bool
    match       = (==)
    mul      :: [a] -> [a] -> [a]
    xs `mul` ys = xs++tail ys
    typology :: Inheritance a -> Typology a
    typology (Isa rs cs)
     = Typ ([fst (head pth): map snd pth| pth<-clos fst snd rs, not (null pth)] ++ [[c]| c<-cs])

  {- was:
    typology :: Show a => Inheritance a -> Typology a
    typology (Isa rs cs)
     = Typ (compress akin [p|p<-f 1 [] (singles rs) []]
            ++ [[c]| c<-elems (Isa rs cs)])
       where
        singles rs         = [[a,b] | (a,b)<-rs, a/=b]
        f n paths [] []    = paths
        f n paths new []   = f (2*n) pn cands [p|p<-cands, head p `elem` tail p]
                             where pn = paths++new
                                   cands = [p|p<-pn `combine` pn,n<length p-1]
        f n paths new errs = error("cyclic specialization\n " ++ (chain "\n ".map show) [head e:takeWhile (/=head e) (tail e)| e<-errs])
        combine rs rs'     = [r `mul` r'| r<-rs, r'<-rs', last r `match` head r']
  -}



















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

   instance Collection Typology where
    eleM e (Typ pths) = or [e `elem` p| p<-pths]
    empty = Typ []
    elems (Typ paths) = rd (concat paths)
    Typ px `uni` Typ py = Typ (compress akin (px++py++[init r++r'| r<-px, r'<-py, last r==head r']))
    Typ _ `isc` Typ _ = error "intersection of Typology not yet implemented"     --TODO
    Typ _ >-    Typ _ = error "set difference of Typology not yet implemented"  --TODO
    rd (Typ pths) = Typ (rd pths)



--   makeTrees :: Eq a => Typology a -> [Classification a]
--   makeTrees (Typ []) = []
--   makeTrees (Typ paths)
--    = [ Cl (head (head pp)) (makeTrees (Typ [tail p| p<-pp, length p>1]))
--      | pp<-eqCl head (compress sub paths)]
--





--   check :: (Show i, Typologic i) => Inheritance i -> Bool
--   check (Isa zaken cs)
--      = not (basic zaken) ||
--         and [ cl==cl'| cl<-makeTrees (typology (Isa zaken []))
--                      , cl'<-makeClassifications zaken
--                      , root cl==root cl']
--   basic :: Eq a => [(a,a)] -> Bool
--   basic zaken = null [(a,b)|(a,b)<-zaken, not (null[c| (a',c)<-zaken, a==a', (c',b')<-zaken, b==b', c==c'])]






--   makeTypo :: Eq a => [Classification a] -> Typology a
--   makeTypo trees = Typ (paths trees)
--    where paths []    = [[]]
--          paths ts = [r: path| Cl r cls<-ts, path<-paths cls]



   instance Typologic Int

--   tt :: Typology Int
--   tt = (makeTypo . makeTrees . typology) (Isa [(1,178),(1,10),(1,12),(10,178)] [3])








--   t1 = typology (Isa [(1,2),(1,3),(1,4),(3,2)] []) :: Typology Int
--   t2 = typology (Isa [(5,6),(2,5),(5,7),(5,8),(9,5)] []) :: Typology Int
--   t3 = typology (Isa [(5,6),(3,5),(3,9),(2,6),(5,7),(5,8),(9,5)] []) :: Typology Int

--   trees :: (Eq a, Show a) => Typology a -> IO ()
--   trees = putStr . chain "\n" . map show . makeTrees


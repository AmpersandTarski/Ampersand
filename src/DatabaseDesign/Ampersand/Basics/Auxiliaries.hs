{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Basics.Auxiliaries
   ( eqCl 
   , eqClass
   , naming
   , sort
   , sort'
   , sord'
   , zip4
   , splits
   , combinations
   )
  where
   import DatabaseDesign.Ampersand.Basics.Version (fatalMsg)
 
   fatal :: Int -> String -> a
   fatal = fatalMsg "Basics.Auxiliaries"

{- naming - a naming function
  The objective is to name all items in a list uniquely
  
  The call below will label allItems as 1,2,3 etc, skipping 4:
  naming nameIt [(\x->show n) |n<-[(1::Integer)..]] ["4"] allItems
  
  Naming one item is done by: nameIt unnamedItem someName -> namedItem
  There should be a list of functions to name an item,
      the resulting names should form an infinite set.
-}
   naming :: Eq a => (b->a->c) -- function used to asign name a to element b
                  -> [b->a]    -- infinite list of functions to create a name for b
                  -> [a]       -- list of forbidden names (names already taken)
                  -> [b]       -- list of elements b that need a name
                  -> [c]       -- result: named alements (matches [b])
   naming _ _ _ [] = []
   naming _ [] _ _ = fatal 32 "no naming functions given"
   naming assignFunc as taken (l:ls)
                   = head [assignFunc l (a l):naming assignFunc as (a l:taken) ls
                          | a<-as, a l `notElem` taken]

   eqClass :: (a -> a -> Bool) -> [a] -> [[a]]
   eqClass _ [] = []
   eqClass f (x:xs) = (x:[e |e<-xs, f x e]) : eqClass f [e |e<-xs, not (f x e)]

   -- | eqCl is a very useful function for gathering things that are equal wrt some criterion f.
   --   For instance, if you want to have persons with the same name:
   --    'eqCl name persons' produces a list,in which each element is a list of persons with the same name.
   eqCl :: Eq b => (a -> b) -> [a] -> [[a]]
   eqCl _ [] = []
   eqCl f (x:xs) = (x:[e |e<-xs, f x==f e]) : eqCl f [e |e<-xs, f x/=f e]

   sort :: (Ord a) => [a] -> [a]
   sort [] = []
   sort (x:xs) = sort [e |e<-xs, e<x] ++ [x] ++ sort [e |e<-xs, e>=x]

   -- | If sorting is done wrt a specific criterion f, this function does the trick
   -- e.g. sorting people on the order of their date of birth, which might be:   sort' date_of_birth persons
   sort' :: (Ord b) => (a -> b) -> [a] -> [a]
   sort' _ [] = []
   sort' f (x:xs) = sort' f [e |e<-xs, f e<f x] ++ [x] ++ sort' f [e |e<-xs, f e>=f x]

   -- | Sorting, and at the same time removing double occurrences.
   sord' :: Ord b => (a -> b) -> [a] -> [a]
   sord' _ [] = []
   sord' f (x:xs) = sord' f [e |e<-xs, f e<f x] ++ [x] ++ sord' f [e |e<-xs, f e>f x]

   -- | 'zip4' takes four lists and returns a list of quadruples, analogous to  'zip3'.
   zip4 :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
   zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a,b,c,d) : zip4 as bs cs ds
   zip4 _      _      _      _      = []
   
   -- | 'splits' makes pairs of a list of things. Example : splits [1,2,3,4,5] = [([1],[2,3,4,5]),([1,2],[3,4,5]),([1,2,3],[4,5]),([1,2,3,4],[5])]
   splits :: [a] -> [([a], [a])]
   splits xs = [splitAt i xs | i<-[1..(length xs - 1)]]

   -- | example: combinations [[1,2,3],[10,20],[4]] = [[1,10,4],[1,20,4],[2,10,4],[2,20,4],[3,10,4],[3,20,4]]
   combinations :: [[a]] -> [[a]]
   combinations []       = [[]]
   combinations (es:ess) = [ x:xs | x<-es, xs<-combinations ess]


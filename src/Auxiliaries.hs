{-# OPTIONS_GHC -Wall #-}
module Auxiliaries
   ( sort
   , eqCl 
   , eqClass
   , naming
   , sort'
   , sord'
   )
  where

{- naming - a naming function
  The objective is to name all items in a list uniquely
  
  The call below will label allItems as 1,2,3 etc, skipping 4:
  naming nameIt [(\x->show n)|n<-[(1::Integer)..]] ["4"] allItems
  
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
   naming _ [] _ _ = error "!Fatal (module Auxiliaries 44): no naming functions given"
   naming assignFunc as taken (l:ls)
                   = head [assignFunc l (a l):naming assignFunc as (a l:taken) ls
                          | a<-as, a l `notElem` taken]
   

   eqClass :: (a -> a -> Bool) -> [a] -> [[a]]
   eqClass _ [] = []
   eqClass f (x:xs) = (x:[e|e<-xs, f x e]) : eqClass f [e|e<-xs, not (f x e)]

-- eqCl is a very useful function for gathering things that are equal wrt some criterion f.
-- For instance, if you want to have persons with the same name:
--    'eqCl name persons' produces a list,in which each element is a list of persons with the same name.
   eqCl :: Eq b => (a -> b) -> [a] -> [[a]]
   eqCl _ [] = []
   eqCl f (x:xs) = (x:[e|e<-xs, f x==f e]) : eqCl f [e|e<-xs, f x/=f e]

-- This sorting function is for general purpose use. The algorithm is quicksort.
   sort :: (Ord a) => [a] -> [a]
   sort [] = []
   sort (x:xs) = sort [e|e<-xs, e<x] ++ [x] ++ sort [e|e<-xs, e>=x]

-- If sorting is done wrt a specific criterion f, this function does the trick
-- e.g. sorting people on the order of their date of birth, which might be:   sort' date_of_birth persons
   sort' :: (Ord b) => (a -> b) -> [a] -> [a]
   sort' _ [] = []
   sort' f (x:xs) = sort' f [e|e<-xs, f e<f x] ++ [x] ++ sort' f [e|e<-xs, f e>=f x]

-- Sorting, and at the same time removing double occurrences.
   sord' :: Ord b => (a -> b) -> [a] -> [a]
   sord' _ [] = []
   sord' f (x:xs) = sord' f [e|e<-xs, f e<f x] ++ [x] ++ sord' f [e|e<-xs, f e>f x]



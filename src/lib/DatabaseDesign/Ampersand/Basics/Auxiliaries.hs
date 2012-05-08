{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Basics.Auxiliaries
   ( eqCl 
   , eqClass
   , sort
   , sort'
   , getCycles
   )
  where
   import Data.List
   import Data.Graph (stronglyConnComp, SCC(CyclicSCC))
   import Data.Maybe
   import GHC.Exts(sortWith)

   -- | The 'eqClass' function takes an equality test function and a list and returns a list of lists such
   -- that each sublist in the result contains only equal elements, and all equal elements are in 
   -- the same sublist.  For example,
   --
   -- > eqClass "Mississippi" = ["M","iiii","ssss","pp"]
   --
   eqClass :: (a -> a -> Bool) -> [a] -> [[a]]
   eqClass _ [] = []
   eqClass f (x:xs) = (x:[e |e<-xs, f x e]) : eqClass f [e |e<-xs, not (f x e)]

   -- | eqCl is a very useful function for gathering things that are equal wrt some criterion f.
   --   For instance, if you want to have persons with the same name:
   --    'eqCl name persons' produces a list,in which each element is a list of persons with the same name.
   eqCl :: Eq b => (a -> b) -> [a] -> [[a]]
   eqCl _ [] = []
   eqCl f (x:xs) = (x:[e |e<-xs, f x==f e]) : eqCl f [e |e<-xs, f x/=f e]

   --TODO Replace by Data.List.sort
--   sort :: (Ord a) => [a] -> [a]
--   sort [] = []
--   sort (x:xs) = sort [e |e<-xs, e<x] ++ [x] ++ sort [e |e<-xs, e>=x]

   -- | This function sorts a list of elements using the user supplied function to project something out of each element
   -- e.g. sorting people on the order of their date of birth, which might be:   sort' date_of_birth persons
   --TODO Replace by sortWith
   sort' :: (Ord b) => (a -> b) -> [a] -> [a]
   sort' = sortWith
--   sort' :: (Ord b) => (a -> b) -> [a] -> [a]
--   sort' _ [] = []
--   sort' f (x:xs) = sort' f [e |e<-xs, f e<f x] ++ [x] ++ sort' f [e |e<-xs, f e>=f x]

   -- | getCycles returns a list of cycles in the edges list (each edge is a pair of a from-vertex
   --   and a list of to-vertices)
   getCycles :: Eq a => [(a, [a])] -> [[a]]
   getCycles edges =
     let allVertices = nub . concat $ [ from : to | (from, to) <- edges ] 
         keyFor v = fromMaybe (error "FATAL") $ elemIndex v allVertices
         graphEdges = [ (v, keyFor v , map keyFor vs)  | (v, vs) <- edges ]
     in  [ vs | CyclicSCC vs <- stronglyConnComp graphEdges ]

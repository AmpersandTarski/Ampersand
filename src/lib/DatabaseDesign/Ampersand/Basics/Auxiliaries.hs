{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Basics.Auxiliaries
   ( eqCl 
   , eqClass
   , getCycles
   , combinations
   , commaEng
   , commaNL
   )
  where
   import Data.List (nub,elemIndex)
   import Data.Graph (stronglyConnComp, SCC(CyclicSCC))
   import Data.Maybe (fromMaybe)

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

   -- | getCycles returns a list of cycles in the edges list (each edge is a pair of a from-vertex
   --   and a list of to-vertices)
   getCycles :: Eq a => [(a, [a])] -> [[a]]
   getCycles edges =
     let allVertices = nub . concat $ [ from : to | (from, to) <- edges ] 
         keyFor v = fromMaybe (error "FATAL") $ elemIndex v allVertices
         graphEdges = [ (v, keyFor v , map keyFor vs)  | (v, vs) <- edges ]
     in  [ vs | CyclicSCC vs <- stronglyConnComp graphEdges ]

-- The following function can be used to determine how much of a set of alternative expression is already determined
   -- | The 'combinations' function returns all possible combinations of lists of list.
   -- For example,
   --
   -- > combinations [[1,2,3],[10,20],[4]] == [[1,10,4],[1,20,4],[2,10,4],[2,20,4],[3,10,4],[3,20,4]]
   combinations :: [[a]] -> [[a]]
   combinations []       = [[]]
   combinations (es:ess) = [ x:xs | x<-es, xs<-combinations ess]
                              
   commaEng :: String -> [String] -> String
   commaEng str [a,b,c] = a++", "++b++", "++str++" "++c
   commaEng str [a,b]   = a++" "++str++" "++b
   commaEng _   [a]     = a
   commaEng str (a:as)  = a++", "++commaEng str as
   commaEng _   []      = ""

   commaNL :: String -> [String] -> String
   commaNL str [a,b]  = a++" "++str++" "++b
   commaNL  _  [a]    = a
   commaNL str (a:as) = a++", "++commaNL str as
   commaNL  _  []     = ""

{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Basics.PartOrder(PartialOrder(..),makePartialOrder)
where
   import DatabaseDesign.Ampersand.Basics.Auxiliaries (sort', eqCl)
   import DatabaseDesign.Ampersand.Basics.Collection ((>-), isc)
   import DatabaseDesign.Ampersand.Basics.Version (fatalMsg)
   import Data.List

   fatal :: Int -> String -> a
   fatal = fatalMsg "Basics.PartOrder"

   data Eq a => PartialOrder a
    = PO { po_lE          ::  a  -> a -> Bool
         , po_comparable  ::  a  -> a -> Bool
         , po_comparables :: [a] -> Bool
         , po_lub         ::  a  -> a -> a
         , po_siblings    ::  a  -> [a]
         }

   makePartialOrder :: Eq a => [(a, a)] -> (PartialOrder a, [[(a, a)]], [(a, [a])])
   makePartialOrder rs = (PO lE comparable comparables lub siblings, sort' length paths, islands)
    where
       --  lE is the reflexive, transitive closure of rs
      a `lE` b = a==b || (a,b) `elem` [ (fst (head pth), snd (last pth)) | pth<-paths ]
      g `comparable` s = (not.null) [l | (l,cs)<-islands, null ([g,s]>-cs)]
      a `lub` b        = if null ls then fatal 20 "computing lub while none exists"
                         else head ls
                         where ls = [l | (l,cs)<-islands, null ([a,b]>-cs)]
      comparables xs   = (not.null) [l | (l,cs)<-islands, null (xs>-cs)]
      siblings x       = head ([cs | (_,cs)<-reverse islands, x `elem` cs]++[[x]])
    -- Auxiliaries: the paths and islands
      paths   = clos fst snd rs
      islands = sort' (length.snd)
                [ ( head (map (fst.head) cl)   -- each "island" contains a lub (which is the first concept on the left of any class)
                  , map (snd.last) cl)           -- and a set of concepts of which it is the lub.
                | cl<-eqCl (fst.head) paths
                ]
   
   clos :: (Eq b,Eq pair) => (pair->b) -> (pair->b) -> [pair] -> [[pair]]
   clos lft rht xs
    = foldl f [[x]| x<-xs] (nub (map lft xs) `isc` nub (map rht xs))
      where
       f q x = q ++ [ls ++ rs | ls <- q, x == rht (last ls), rs <- q,
                          x == lft (head rs), null (ls `isc` rs)]

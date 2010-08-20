{-# OPTIONS_GHC -Wall -XTypeSynonymInstances #-}
module Adl.Pair    (Paire,Pairs
                    , join
                    , srcPaire,trgPaire
                    , flipPair,mkPair
                    , closPair
                    , closGeneric
                   ) 
where
--   import Data.Tuple    -- TODO Is dit niet veel beter te gebruiken?  
   import Auxiliaries(sort',eqCl)
   import Collection (Collection(isc,uni,rd))
 
   type Pairs     = [Paire]
   srcPaire :: Paire -> String
   trgPaire :: Paire -> String
   mkPair   :: String -> String -> Paire
   type Paire     = (String,String)
   mkPair a b = (a,b)
   srcPaire = fst
   trgPaire = snd
--   type Paire     = [String]
--   srcPaire xs    = if null xs then error ("!Fatal (module Adl.Pair 25): src []") else head xs
--   trgPaire xs    = if null xs then error ("!Fatal (module Adl.Pair 26): trg []") else last xs
--   mkPair a b = a:[b]

   flipPair :: Paire -> Paire
   flipPair p = mkPair (trgPaire p) (srcPaire p)

   -- | Operations for representations that act as a Kleene algebra (RA without complement and with the closure operators)
   -- | A Kleene algebra has two binary operations 'union' and 'join', and one function 'closure' (usually written as +, á and * respectively)
   class KAComputable a where
     join :: a->a->a
     closPair :: a->a
     -- TODO: add the 'uni' operator
   
   instance (KAComputable a) => KAComputable (Maybe a) where
     join (Just a) (Just b) = Just (join a b)
     join _ _ = Nothing
     closPair (Just p) = Just (closPair p)
     closPair _ = Nothing
      
   instance KAComputable Pairs where
     join a b = merge ((sort' (trgPaire.head).eqCl trgPaire) a)
                    ((sort' (srcPaire.head).eqCl srcPaire) b)
                where merge (xs:xss) (ys:yss)
                       | trgPaire (head xs)<srcPaire (head ys) = merge xss (ys:yss)
                       | trgPaire (head xs)>srcPaire (head ys) = merge (xs:xss) yss
                       | otherwise = [mkPair (srcPaire x) (trgPaire y) |x<-xs,y<-ys]++ merge xss yss
                      merge _ _ = []
   --DESCR -> [b] is a list of two: [c1,c2] indicating a path from c1 to c2
   --TODO -> if [b] == [] then head results in Prelude.head: empty list error
   --        if not length b == 2 then that element will be ignored
     closPair ps = toPairs (clos1 (toList ps))
      where
       toPairs :: [[String]] -> Pairs
       toPairs [] = []
       toPairs (p:pps) = (mkPair (head p) (last p)) : toPairs pps
       toList :: Pairs -> [[String]]
       toList [] = []
       toList (p:pps) = [[srcPaire p, trgPaire p]] ++ (toList pps)
----------------------------------------------------
--  Warshall's transitive closure algorithm in Haskell:
----------------------------------------------------
       clos1 :: (Eq b) => [[b]] -> [[b]]     -- e.g. a list of pairs
       clos1 xs
         = f xs (rd (map head xs) `isc` rd (map last xs))
           where
            f q (x:xs') = f (q `uni` [[a,b']|[a,b]<-q,b==x,[a',b']<-q,a'==x]) xs'
            f q []      = q

   closGeneric :: (Eq a,Eq b) => (a->a->b) -> (b->a) -> (b->a) -> [b] -> [b]     -- e.g. a list of pairs
   closGeneric pair left right xs
     = f xs (rd (map left xs) `isc` rd (map right xs))
       where
        f pairs (x:xs') = f (pairs `uni` [left e `pair` right e'|e<-pairs,right e==x,e'<-pairs,left e'==x]) xs'
        f pairs []      = pairs

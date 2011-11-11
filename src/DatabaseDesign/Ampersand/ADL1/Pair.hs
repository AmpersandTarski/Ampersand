{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
module DatabaseDesign.Ampersand.ADL1.Pair
                    ( Paire,Pairs
                    , kleenejoin
                    , srcPaire,trgPaire
                    , flipPair,mkPair
                    , closPair
                    , clos1
                    ) 
where
--   import Data.Tuple    -- TODO Is dit niet veel beter te gebruiken?  
   import DatabaseDesign.Ampersand.Basics (Collection(isc,uni),sort',eqCl)
   import Data.List (nub)

   type Pairs = [Paire]
   srcPaire :: Paire -> String
   trgPaire :: Paire -> String
   mkPair   :: String -> String -> Paire
   type Paire = (String,String)
   mkPair a b = (a,b)
   srcPaire = fst
   trgPaire = snd

   flipPair :: Paire -> Paire
   flipPair p = mkPair (trgPaire p) (srcPaire p)

   -- | Operations for representations that act as a Kleene algebra (RA without complement and with the closure operators)
   -- | A Kleene algebra has two binary operations 'union' and 'kleenejoin', and one function 'closure' (usually written as +, � and * respectively)
   class KAComputable a where
     kleenejoin :: a->a->a
     closPair :: a->a
     -- TODO: add the 'uni' operator
   
   instance (KAComputable a) => KAComputable (Maybe a) where
     kleenejoin (Just a) (Just b) = Just (kleenejoin a b)
     kleenejoin _ _ = Nothing
     closPair (Just p) = Just (closPair p)
     closPair _ = Nothing
      
   instance KAComputable Pairs where
     kleenejoin a b = merge ((sort' (trgPaire.head).eqCl trgPaire) a)
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
       toPairs :: [(String,String)] -> Pairs
       toPairs pairs = [mkPair a b | (a,b)<-pairs]
       toList :: Pairs -> [(String,String)]
       toList pairs = [(srcPaire p, trgPaire p) | p<-pairs]
----------------------------------------------------
--  Warshall's transitive closure algorithm in Haskell:
----------------------------------------------------
   clos1 :: (Eq a) => [(a,a)] -> [(a,a)]     -- e.g. a list of pairs
   clos1 xs
     = foldl f xs (nub (map fst xs) `isc` nub (map snd xs))
       where
        f q x = q `uni` [(a, b') | (a, b) <- q, b == x, (a', b') <- q, a' == x]

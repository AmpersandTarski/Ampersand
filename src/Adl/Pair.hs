{-# OPTIONS_GHC -Wall #-}
module Adl.Pair    (Paire,Pairs
                    , join
                    , srcPaire,trgPaire
                    , flipPair,mkPaire
                    , closPair
                   ) 
where
--   import Data.Tuple    -- TODO Is dit niet veel beter te gebruiken?  
   import Auxiliaries(sort',eqCl)
   import Collection (Collection(isc,uni,rd))
 
   type Pairs     = [Paire]  -- WAAROM? Stef, zouden dit niet tweetallen moeten zijn? In dit geval mogen Paire ook uit meer dan twee bestaan...
                             --         Op die manier zouden we ook van standaard Tuples gebruik kunnen maken....
                             -- DAAROM! Han, je hebt gelijk. Het is er ooit ingeslopen op grond van een denkfout. Graag tuples van maken...
                             -- Opgelost! Paire zijn Tuples geworden
   srcPaire :: Paire -> String
   trgPaire :: Paire -> String
   mkPaire   :: String -> String -> Paire
   type Paire     = (String,String)
   mkPaire a b = (a,b)
   srcPaire = fst
   trgPaire = snd
--   type Paire     = [String]
--   srcPaire xs    = if null xs then error ("(module Adl.Pair) Fatal: src []") else head xs
--   trgPaire xs    = if null xs then error ("(module Adl.Pair) Fatal: trg []") else last xs
--   mkPaire a b = a:[b]

   join::Pairs->Pairs->Pairs
   join a b = merge ((sort' (trgPaire.head).eqCl trgPaire) a)
                    ((sort' (srcPaire.head).eqCl srcPaire) b)
              where merge (xs:xss) (ys:yss)
                     | trgPaire (head xs)<srcPaire (head ys) = merge xss (ys:yss)
                     | trgPaire (head xs)>srcPaire (head ys) = merge (xs:xss) yss
                     | otherwise = [mkPaire (srcPaire x) (trgPaire y) |x<-xs,y<-ys]++ merge xss yss
                    merge _ _ = []

   flipPair :: Paire -> Paire
   flipPair p = mkPaire (trgPaire p) (srcPaire p)

   --DESCR -> [b] is a list of two: [c1,c2] indicating a path from c1 to c2
   --TODO -> if [b] == [] then head results in Prelude.head: empty list error
   --        if not length b == 2 then that element will be ignored
   closPair :: Pairs -> Pairs
   closPair ps = toPairs (clos1 (toList ps))
     where
       toPairs :: [[String]] -> Pairs
       toPairs [] = []
       toPairs (p:pps) = (mkPaire (head p) (last p)) : (toPairs pps)
       toList :: Pairs -> [[String]]
       toList [] = []
       toList (p:pps) = [(srcPaire p) : [trgPaire p]] ++ (toList pps)

 --TODO :: Dit moet nog even een stuk vereenvoudigd worden! (En daardoor efficienter, ongetwijfeld!!)       

       clos1 :: (Eq b) => [[b]] -> [[b]] 
    --   closPair :: Pairs -> Pairs
       clos1 xs
         --DESCR -> rd - remove duplicates; isc - intersection
         --         the snd arg is a set of every c which is domain [[b]] /\ range [[b]] (b is a tuple)
         = f xs (rd (map head xs) `isc` rd (map last xs))
           where
            f q (x:xs') = f (q `uni` [[a,b']|[a,b]<-q,b==x,[a',b']<-q,a'==x]) xs'
            f q []      = q


                     
{-# OPTIONS_GHC -Wall #-}
module Adl.Pair    (Paire,Pairs
                    , join
                    , srcPair,trgPair
                    , flipPair,aPair
                   ) 
where
--   import Data.Tuple    -- TODO Is dit niet veel beter te gebruiken?  
   import Auxiliaries(sort',eqCl)
   
   type Pairs     = [Paire]  -- WAAROM? Stef, zouden dit niet tweetallen moeten zijn? In dit geval mogen Paire ook uit meer dan twee bestaan...
                             --         Op die manier zouden we ook van standaard Tuples gebruik kunnen maken....
                             -- DAAROM! Han, je hebt gelijk. Het is er ooit ingeslopen op grond van een denkfout. Graag tuples van maken...
   type Paire     = [String]

   srcPair :: Paire -> String
--  src = fst
   trgPair :: Paire -> String
--   trg = snd
   srcPair xs    = if null xs then error ("(module Adl.Pair) Fatal: src []") else head xs
   trgPair xs    = if null xs then error ("(module Adl.Pair) Fatal: trg []") else last xs

   join::Pairs->Pairs->Pairs
   join a b = merge ((sort' (trgPair.head).eqCl trgPair) a)
                    ((sort' (srcPair.head).eqCl srcPair) b)
              where merge (xs:xss) (ys:yss)
                     | trgPair (head xs)<srcPair (head ys) = merge xss (ys:yss)
                     | trgPair (head xs)>srcPair (head ys) = merge (xs:xss) yss
                     | otherwise = [aPair (srcPair x) (trgPair y) |x<-xs,y<-ys]++ merge xss yss
                    merge _ _ = []

   flipPair :: Paire -> Paire
   flipPair p = aPair (trgPair p) (srcPair p)

   aPair :: String -> String -> Paire
   aPair a b = [a,b]
   

                     
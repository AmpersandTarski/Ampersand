
module Adl.Pair where
   import Auxiliaries(sort',eqCl)

   type Pairs     = [Paire]  -- WAAROM? Zouden dit niet tweetallen moeten zijn? In dit geval mogen Paire ook uit meer dan twee bestaan...
   type Paire     = [String]

   src, trg      :: Paire -> String
   src xs         = if null xs then error ("(module ADLdef) Fatal: src []") else head xs
   trg xs         = if null xs then error ("(module ADLdef) Fatal: trg []") else last xs
   join::Pairs->Pairs->Pairs
   join a b = merge ((sort' (trg.head).eqCl trg) a)
                    ((sort' (src.head).eqCl src) b)
              where merge (xs:xss) (ys:yss)
                     | trg (head xs)<src (head ys) = merge xss (ys:yss)
                     | trg (head xs)>src (head ys) = merge (xs:xss) yss
                     | otherwise = [[x,y]|[x,i]<-xs,[j,y]<-ys]++ merge xss yss
                    merge _ _ = []

   class Populated a where
    contents  :: a -> Pairs

                    
{-# OPTIONS_GHC -Wall #-}
module Classification (
               Classification(Cl)
             , root
             , preCl
   ) 
where
   import ADL.MorphismAndDeclaration (Identified(..)) 
   import Collection    (Collection(..))
   import Data.List hiding (insert)

   data Classification a = Cl a [Classification a] | Bottom
   root :: Classification a -> a
   root (Cl c _) = c
   root Bottom = error ("!Fatal (module Classification 15): root Bottom is not defined.")
   subs :: Classification a -> [Classification a]
   subs (Cl _ cls) = cls
   subs Bottom = error ("!Fatal (module Classification 18): subs Bottom is not defined.")
   preCl :: Classification a -> [a]
   preCl Bottom         = []
   preCl (Cl c cls)     = [c] ++ concat (map preCl cls)

   instance Eq a => Eq (Classification a) where
    Bottom == Bottom = True
    Cl r cls == Cl r' cls'
        = r==r' && and[cl==cl'| cl<-cls, cl'<-cls', root cl==root cl']
    _ == _ = False

   instance Identified a => Identified (Classification a) where
    name (Cl r _) = name r
    name Bottom = "Bottom"


   instance Show a => Show (Classification a) where
    showsPrec _ cls
     = showString (shw "\n  " cls)
       where
        shw indent (Cl r' cls')
         = intercalate indent (show r':[shw (indent++"  ") (Cl r'' cls'')
                                      | Cl r'' cls''<-cls'])
        shw _ Bottom = ""



   tuples :: Classification t -> [(t, t)]
   tuples (Cl r cls) = [(r,root cl)| cl<-cls]++[t|cl<-cls, t<-tuples cl]
   tuples Bottom     = []

   instance Collection Classification where
    eleM e (Cl r cls) = e==r || or (map (eleM e) cls)
    eleM _ Bottom = False
    empty = Bottom
    elems Bottom = []
    elems (Cl r cls) = r: [c| cl<-cls, c<-elems cl]
    rd  _ = error ("!Fatal (module Classification 55): rd needs a fix....")
    rd' _ _ = error ("!Fatal (module Classification 56): rd' needs a fix....")
    lcl `isc` rcl | length trees==1 = head trees
                  | otherwise       = Bottom
     where trees = makeClassifications (tuples lcl `isc` tuples rcl)
    lcl >- rcl | length trees==1 = head trees
               | otherwise       = Bottom
     where trees = makeClassifications (tuples lcl>-tuples rcl)



    uni x Bottom = x
    uni Bottom y = y
    uni x y
     | root x==root y     = foldl insert x (subs y)
     | aINb && not bINa   = insert y x
     | not aINb && bINa   = insert x y
     | otherwise          = error "!Fatal (module Classification 72): uni of Collections"
     where
      aINb = root x `isDefinedIn` y && not (or [c `isDefinedIn` y|cl<-subs x,c<-elems cl])
      bINa = root y `isDefinedIn` x && not (or [c `isDefinedIn` x|cl<-subs y,c<-elems cl])



   isDefinedIn :: Eq a => a -> Classification a -> Bool
   _ `isDefinedIn` Bottom = False
   c `isDefinedIn` (Cl r cls)
    | r==c      = True
    | otherwise = or [c `isDefinedIn` cl| cl<-cls]





   insert:: Eq a => Classification a -> Classification a -> Classification a
   insert wls Bottom = wls
   insert Bottom cls = cls
   insert wls cls
    | (root cls) `isDefinedIn` wls = update up (root cls==) wls
    | or[c `isDefinedIn` wls|c<-rd[c|cl<-subs cls, c<-elems cl]]
        = error ("!Fatal (module Classification 95): insert error!")
    | otherwise                    = Cl (root wls) (subs wls++[cls])
    where up wls' = foldl insert wls' (subs cls)


   update :: Eq a => (Classification a -> Classification a)
              -> (a->Bool) -> Classification a  -> Classification a 
   update _ _ Bottom = Bottom
   update upd c (Cl r cls) 
     | c r       = upd(Cl r cls)
     | otherwise = Cl r [update upd c cl| cl<-cls]

{- makeClassifications creates trees in different ways. The easiest to understand is 
makeClassifications itself, because it works with tuples.
Visualize a cycle free graph, defined by these tuples.
makeClassifications will give you the trees you can build in that graph.
The precondition is that the graph cycle free.
-}
   makeClassifications :: Eq a => [(a,a)] -> [Classification a]
   makeClassifications tuples'
    = maketree (rd[a |(a,_)<-tuples', not (a `elem` rd [b |(_,b)<-tuples'])])
               (rd tuples')
      where
       maketree roots tuples'' = [ Cl root' (trees tuples'' root')| root'<-roots]
       trees tuples'' root' = maketree (rd [b |(a,b)<-tuples'', root'==a]) [(a,b) |(a,b)<-tuples'', root'/=a]




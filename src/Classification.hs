{-# OPTIONS_GHC -Wall #-}
module Classification (
               Classification(Cl)
             , root
             , makeClassifications
             , preCl
             , locatesF
             , makeClassificationsF
             , mapCl
   ) 
where
   import CommonClasses ( Identified(name,typ)
                        , Conceptual(conts)
                        ) 
   import Collection  (Collection(..))
   import Auxiliaries (eqCl) 
   import Strings     (chain)

   data Classification a = Cl a [Classification a] | Bottom
   root :: Classification a -> a
   root (Cl c _) = c
   root Bottom = undefined
   subs :: Classification a -> [Classification a]
   subs (Cl _ cls) = cls
   subs Bottom = undefined
--   isBot :: Classification a -> Bool
--   isBot Bottom =True
--   isBot _      = False
--   recur :: (t -> [t1] -> t1) -> t1 -> Classification t -> t1
--   recur _ e Bottom     = e
--   recur f e (Cl c cls) = f c [recur f e cl| cl<-cls]
   mapCl :: (t -> a) -> Classification t -> Classification a
   mapCl _ Bottom       = Bottom
   mapCl f (Cl c cls)   = Cl (f c) (map (mapCl f) cls)
   preCl :: Classification a -> [a]
   preCl Bottom         = []
   preCl (Cl c cls)     = [c] ++ concat (map preCl cls)
--   postCl :: Classification a -> [a]
--   postCl Bottom        = []
--   postCl (Cl c cls)    = concat (map postCl cls) ++ [c]

   instance Eq a => Eq (Classification a) where
    Bottom == Bottom = True
    Cl r cls == Cl r' cls'
        = r==r' && and[cl==cl'| cl<-cls, cl'<-cls', root cl==root cl']
    _ == _ = False

   instance Identified a => Identified (Classification a) where
    name (Cl r _) = name r
    name Bottom = "Bottom"
    typ (Cl r _) = "Classification_of_" ++ typ r
    typ Bottom = undefined


   instance Show a => Show (Classification a) where
    showsPrec _ cls
     = showString (shw "\n  " cls)
       where
        shw indent (Cl r' cls')
         = chain indent (show r':[shw (indent++"  ") (Cl r'' cls'')
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
    rd  _ = error ("!Fatal (module Classification 76): rd needs a fix....")
    rd' _ _ = error ("!Fatal (module Classification 77): rd' needs a fix....")
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
     | otherwise          = error "!Fatal (module Classification 93): uni of Collections"
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
        = error ("!Fatal (module Classification 116): insert error!")
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

{- the following is a variation on the same theme. The condition that the root of a tree equals the left
element of a tuple is generalized to a function, that provides the criterion to match the two. -}
   makeClassificationsF :: (Eq a,Eq b) => (a->b) -> [(a,a)] -> [Classification a]
   makeClassificationsF f tuples'
    = maketree (map head (eqCl f [a |(a,_)<-tuples', not (f a `elem` rd [f b |(_,b)<-tuples'])]))
               (rd tuples')
      where
       maketree roots tuples'' = [ Cl root' (trees tuples'' root')| root'<-roots]
       trees tuples'' root' = maketree (map head (eqCl f [b |(a,b)<-tuples'', f root'==f a])) [(a,b) |(a,b)<-tuples'', f root'/=f a]

{- In the following attempt, we generalize away from tuples.
Assume that the left element of a tuple is obtained by a function src, and the right element by the function trg.
This is done if there is more information in the graph than just the left and the right element.
For this reason, the links in the graph are taken into the result tree
-}
--                          src ::          trg::          roots
{-
   makeTrees :: (Eq node,Eq edge) => (edge->node) -> (edge->node) -> [edge] -> [Classification (edge,node)]
   makeTrees src trg edges
    = maketree (rd [(edge,src edge) |edge<-edges])  -- the nodes that are root
               (rd edges)                           -- the graph in which trees are built
      where
       maketree roots edges' = [ Cl (x,root') (trees edges' root')| (x,root')<-roots]
       trees edges' root'
        = maketree [(edge,trg edge) |edge<-edges', root'==src edge]  -- select the nodes to visit next
                   [edge |edge<-edges', root'/=src edge]             -- remove links and avoid cycles
-}
{- Opletten: in de initiele aanroep van maketree staat (rd [(edge,src edge) |edge<-edges]).
Dat is incorrect, omdat het eerste element van het tupel niet "edge" moet zijn maar een relatie die van buiten komt.
-}

   locatesF :: (a->Bool) -> Classification a -> [Classification a]
   locatesF _ Bottom = []
   locatesF f (Cl r cls) 
     | f r = [Cl r cls]
     | otherwise = concat (map (locatesF f) cls)



   instance Conceptual a => Conceptual (Classification a) where
    conts                                         = rd . concat . map conts . preCl



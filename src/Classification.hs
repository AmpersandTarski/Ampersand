{-# OPTIONS_GHC -Wall #-}
   module Classification
   (  Classification(Cl, Bottom)
    , root
    , subs
    , isBot
    , recur
 --   , restrict
    , sortCl
    , isDefinedIn
    , makeClassifications
    , makeClassificationsF
    , locates
    , locatesF
    , preCl
    , postCl
    , mapCl
  --  , index
   ) where
   import CommonClasses ( Identified(name,typ)
                        , Conceptual(conts)
                        ) 
   import Collection (Collection(eleM,elems,isc,(>-),uni,empty,rd))
   import Auxiliaries (chain, eqCl) 


   data Classification a = Cl a [Classification a] | Bottom
   root :: Classification a -> a
   root (Cl c _) = c
   root Bottom = undefined
   subs :: Classification a -> [Classification a]
   subs (Cl _ cls) = cls
   subs Bottom = undefined
   isBot :: Classification a -> Bool
   isBot Bottom =True
   isBot _      = False
   recur :: (t -> [t1] -> t1) -> t1 -> Classification t -> t1
   recur _ e Bottom     = e
   recur f e (Cl c cls) = f c [recur f e cl| cl<-cls]
   mapCl :: (t -> a) -> Classification t -> Classification a
   mapCl _ Bottom       = Bottom
   mapCl f (Cl c cls)   = Cl (f c) (map (mapCl f) cls)
   preCl,postCl :: Classification a -> [a]
   preCl Bottom         = []
   preCl (Cl c cls)     = [c] ++ concat (map preCl cls)
   postCl Bottom        = []
   postCl (Cl c cls)    = concat (map postCl cls) ++ [c]

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
    rd _ = error ("Module Classifications needs a fix....")

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
     | otherwise          = error "!Err: uni of Collections"
     where
      aINb = root x `isDefinedIn` y && not (or [c `isDefinedIn` y|cl<-subs x,c<-elems cl])
      bINa = root y `isDefinedIn` x && not (or [c `isDefinedIn` x|cl<-subs y,c<-elems cl])



   isDefinedIn :: Eq a => a -> Classification a -> Bool
   _ `isDefinedIn` Bottom = False
   c `isDefinedIn` (Cl r cls)
    | r==c      = True
    | otherwise = or [c `isDefinedIn` cl| cl<-cls]



   -- WAAROM?  Stef, ik zie zo gouw niet waarom deze functie goed zou zijn. Graag wat commentaar, 
   -- en even nakijken of deze functie écht wel doet wat-ie zou moeten doen...
   -- Ook blijkt hij verder niet gebruikt, dus hij mag wat mij betreft ook verdwijnen...
--   restrict :: Eq a => [a] -> [Classification a] -> [Classification a]
--   restrict stayers cls = rd(f cls)
--    where
--     f cls = [Cl r rest| Cl r subs<-cls, rest<-[f subs], r `elem` stayers || not (null rest)]









   insert:: Eq a => Classification a -> Classification a -> Classification a
   insert wls Bottom = wls
   insert Bottom cls = cls
   insert wls cls
    | (root cls) `isDefinedIn` wls = update up (root cls==) wls
    | or[c `isDefinedIn` wls|c<-rd[c|cl<-subs cls, c<-elems cl]]
        = error ("insert error!")
    | otherwise                    = Cl (root wls) (subs wls++[cls])
    where up wls' = foldl insert wls' (subs cls)
      --    rd [] = []
      --    rd (x:xs) = x:[e|e<-xs, e/=x]





   update :: Eq a => (Classification a -> Classification a)
              -> (a->Bool) -> Classification a  -> Classification a 
   update _ _ Bottom = Bottom
   update upd c (Cl r cls) 
     | c r       = upd(Cl r cls)
     | otherwise = Cl r [update upd c cl| cl<-cls]


   makeClassifications :: Eq a => [(a,a)] -> [Classification a]
   makeClassifications twos
    = maketree (rd[a |(a,_)<-twos, not (a `elem` rd [b |(_,b)<-twos])])
               (rd twos)
      where
       maketree roots twos' = [ Cl r [Cl b xs| Cl b xs<-trees twos' r]| r<-roots]
       trees twos' r = maketree (rd [b |(a,b)<-twos', r==a]) [(a,b) |(a,b)<-twos', r/=a]

   makeClassificationsF :: Eq b => (a->b) -> [(a,a)] -> [Classification a]
   makeClassificationsF f twos
    = maketree (map head (eqCl f [a |(a,_)<-twos, not (f a `elem` rd [f b |(_,b)<-twos])])) twos
      where
       maketree roots twos' = [ Cl r [Cl b xs| Cl b xs<-trees twos' r]| r<-roots]
       trees twos' r = maketree (map head (eqCl f [b |(a,b)<-twos', f r==f a])) [(a,b) |(a,b)<-twos', f r/=f a]



--   locate :: Eq a => a -> Classification a -> Classification a
--   locate c Bottom = Bottom
--   locate c (Cl r cls) 
--     | r==c      = (Cl r cls)
--     | otherwise = head([Cl r cls| Cl r cls<-[locate c cl| cl<-cls]]++[Bottom])
--
   locates :: Eq a => a -> Classification a -> [Classification a]
   locates _ Bottom = []
   locates c (Cl r cls) 
     | r==c      = [Cl r cls]
     | otherwise = concat (map (locates c) cls)

   locatesF :: (a->Bool) -> Classification a -> [Classification a]
   locatesF _ Bottom = []
   locatesF f (Cl r cls) 
     | f r = [Cl r cls]
     | otherwise = concat (map (locatesF f) cls)

   sortCl :: (a -> a -> Bool) -> Classification a -> Classification a
   sortCl geq classification
    = s classification
    where
     s (Cl r cls) = Cl r (srt (map s cls))
     s Bottom     = Bottom
     srt   []     = []
     srt (c:cls)  = srt [e|e<-cls, not (root e `geq` root c)] ++ [c] ++
                    srt [e|e<-cls,      root e `geq` root c]




   instance Conceptual a => Conceptual (Classification a) where
    conts                                         = rd . concat . map conts . preCl



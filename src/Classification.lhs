>  module Classification
>  (  Classification(Cl, Bottom)
>   , root
>   , subs
>   , isBot
>   , recur
>   , restrict
>   , sortCl
>   , isDefinedIn
>   , makeClassifications
>   , makeClassificationsF
>   , locates
>   , locatesF
>   , preCl
>   , postCl
>   , mapCl
>   , index
>  ) where
>  import CommonClasses ( Identified(name)
>                       , Conceptual(conts)
>                       ) 
>  import Collection (Collection(eleM,elems,isc,(>-),uni,empty,rd))
>  import Auxiliaries (chain, eqCl) 

Classifications
All elements of a classification tree can occur once only.
Every child in the tree has an ISA-relation to its parent.
This is interpreted as a subset relationship.
We use classification trees to classify both concepts and morphisms between concepts.



   rename newname (i:is) = (rename newname i:is)

>  data Classification a = Cl a [Classification a] | Bottom
>  root :: Classification a -> a
>  root (Cl c cls) = c
>  subs :: Classification a -> [Classification a]
>  subs (Cl c cls) = cls
>  isBot :: Classification a -> Bool
>  isBot Bottom =True
>  isBot _      = False
>  recur f e Bottom     = e
>  recur f e (Cl c cls) = f c [recur f e cl| cl<-cls]
>  mapCl f Bottom       = Bottom
>  mapCl f (Cl c cls)   = Cl (f c) (map (mapCl f) cls)
>  preCl,postCl :: Classification a -> [a]
>  preCl Bottom         = []
>  preCl (Cl c cls)     = [c] ++ concat (map preCl cls)
>  postCl Bottom        = []
>  postCl (Cl c cls)    = concat (map postCl cls) ++ [c]

>  instance Eq a => Eq (Classification a) where
>   Bottom == Bottom = True
>   Cl r cls == Cl r' cls'
>    = r==r' && and[cl==cl'| cl<-cls, cl'<-cls', root cl==root cl']
>   cl == cl' = False

>  instance Identified a => Identified (Classification a) where
>   name (Cl r cls) = name r
>   name Bottom = "Bottom"

   rename newname (Cl r cls) = (Cl (rename newname r) cls)
   rename newname Bottom = Bottom

>  instance Show a => Show (Classification a) where
>   showsPrec p cls
>    = showString (shw "\n  " cls)
>      where
>       shw indent (Cl r cls)
>        = chain indent (show r:[shw (indent++"  ") (Cl r cls')
>                               | Cl r cls'<-cls])
>       shw indent Bottom = ""

To flatten a classification using "elems" yields a list that contains all elements.

>  tuples (Cl r cls) = [(r,root cl)| cl<-cls]++[t|cl<-cls, t<-tuples cl]
>  tuples Bottom     = []

>  instance Collection Classification where
>   eleM e (Cl r cls) = e==r || or (map (eleM e) cls)
>   empty = Bottom
>   elems Bottom = []
>   elems (Cl r cls) = r: [c| cl<-cls, c<-elems cl]

isc is the intersection of two classifications, provided one of them contains the root of the other.

>   lcl `isc` rcl | length trees==1 = head trees
>                 | otherwise       = Bottom
>    where trees = makeClassifications (tuples lcl `isc` tuples rcl)
>   lcl >- rcl | length trees==1 = head trees
>              | otherwise       = Bottom
>    where trees = makeClassifications (tuples lcl>-tuples rcl)

uni merges two classifications, provided one of them contains the root of the other.

>   uni x Bottom = x
>   uni Bottom y = y
>   uni x y
>    | root x==root y     = foldl insert x (subs y)
>    | aINb && not bINa   = insert y x
>    | not aINb && bINa   = insert x y
>    | otherwise          = error "!Err: uni of Collections"
>    where
>     aINb = root x `isDefinedIn` y && not (or [c `isDefinedIn` y|cl<-subs x,c<-elems cl])
>     bINa = root y `isDefinedIn` x && not (or [c `isDefinedIn` x|cl<-subs y,c<-elems cl])

Note: Should also work with, 
    aINb = root x `isDefinedIn` y
    bINa = root y `isDefinedIn` x
However, I have not checked whether the appropriate preconditions are applied consequently.

Backup code for uni 
   uni x Bottom = x
   uni Bottom y = y
   uni x y
    | root x==root y
      = Cl (root x)
           ([ys|ys<-subs y, null (ys `common` x)]++[xs|xs<-subs x, null(xs `common` y)]++
            [update (uni ys) (root ys==) xs|ys<-subs y, xs<-subs x, root ys `isDefinedIn` xs]++
            [update (uni xs) (root xs==) ys|xs<-subs x, ys<-subs y, root xs/=root ys, root xs `isDefinedIn` ys])
    | otherwise          = error ("uni: "++show (x)++" /= "++show (y))
    where
     cla `common` clb = [a| a<-elems cla, a `elem` elems clb]

>  isDefinedIn :: Eq a => a -> Classification a -> Bool
>  c `isDefinedIn` Bottom = False
>  c `isDefinedIn` (Cl r cls)
>   | r==c      = True
>   | otherwise = or [c `isDefinedIn` cl| cl<-cls]

Restrict means to cut all items from classification trees that do not occur in the list of stayers.

>  restrict :: Eq a => [a] -> [Classification a] -> [Classification a]
>  restrict stayers cls = rd(f cls)
>   where
>    f cls = [Cl r rest| Cl r subs<-cls, rest<-[f subs], r `elem` stayers || not (null rest)]

The following body for "restrict" cuts redundant generalizations as well.
    f [] = []
    f cls = [Cl r (f subs)| Cl r subs<-cls, r `elem` stayers]++
            f ([cl| Cl r subs<-cls, not(r `elem` stayers), cl<-subs])

The function 'insert' adds a classification 'cls' to the classification 'wls'.
The precondition is that 'root cls' does and the elements of 'subs cls' do not occur in 'wls'.

>  insert:: Eq a => Classification a -> Classification a -> Classification a
>  insert wls Bottom = wls
>  insert Bottom cls = cls
>  insert wls cls
>   | (root cls) `isDefinedIn` wls = update up (root cls==) wls
>   | or[c `isDefinedIn` wls|c<-rd[c|cl<-subs cls, c<-elems cl]]
>       = error ("insert error!")
>   | otherwise                    = Cl (root wls) (subs wls++[cls])
>   where up wls' = foldl insert wls' (subs cls)
>         rd [] = []
>         rd (x:xs) = x:[e|e<-xs, e/=x]

The function 'ins' does exactly what 'insert' does, but it works with a list 'a:b:c:..[]',
with 'b isa a' and 'c isa b' etc.
Afterwards, 'wls' contains a path in which these relations are represented.
 
>  ins:: Eq a => Classification a -> [a] -> Classification a
>  ins wls [] = wls
>  ins Bottom (c:cs) = Cl c [ins Bottom cs]
>  ins wls (c:cs)
>   | c `isDefinedIn` wls = update up (c==) wls
>   | otherwise = Cl (root wls) (subs wls++[ins Bottom (c:cs)])
>   where up cl = ins cl cs

If we know a parent-child relationship, an entire child tree can be inserted directly:

>  insertParChd :: (Show a,Eq a) =>
>                  a -> Classification a -> Classification a -> Classification a
>  insertParChd par chd wls
>   = update up (par==) wls
>     where up wls' = wls' `uni` Cl par [chd]

Insert an element at the most specific location:

>  insertSpc :: (a -> a -> Bool)
>               -> a -> Classification a -> Classification a
>  insertSpc spc e Bottom = Cl e []
>  insertSpc spc e (Cl r cls)
>   | e `spc` r = Cl r [insertSpc spc e cl| cl<-cls]
>   | r `spc` e = Cl e [Cl r cls]
>   | otherwise = Cl r cls

This function lists all paths from root to c.
Each path contains all generalizations of c, down to the root.
If c is an element of classification cl, and cl contains only one c, then
a list with only one path is produced.

update substitutes all subtrees s in which c(root s), by upd s.

>  update :: Eq a => (Classification a -> Classification a)
>             -> (a->Bool) -> Classification a  -> Classification a 
>  update upd c Bottom = Bottom
>  update upd c (Cl r cls) 
>    | c r       = upd(Cl r cls)
>    | otherwise = Cl r [update upd c cl| cl<-cls]

Construction of classifications using tuples
(makeIsaTrees maakt gebruik van een exponentieel algoritme, en is snel voor kleinere bomen.)

>  makeClassifications :: Eq a => [(a,a)] -> [Classification a]
>  makeClassifications twos
>   = maketree (rd[a |(a,b)<-twos, not (a `elem` rd [b |(a,b)<-twos])]) (rd twos)
>     where
>      maketree roots twos = [ Cl r [Cl b xs| Cl b xs<-trees twos r]| r<-roots]
>      trees twos r = maketree (rd [b |(a,b)<-twos, r==a]) [(a,b) |(a,b)<-twos, r/=a]

>  makeClassificationsF :: Eq b => (a->b) -> [(a,a)] -> [Classification a]
>  makeClassificationsF f twos
>   = maketree (map head (eqCl f [a |(a,b)<-twos, not (f a `elem` rd [f b |(a,b)<-twos])])) twos
>     where
>      maketree roots twos = [ Cl r [Cl b xs| Cl b xs<-trees twos r]| r<-roots]
>      trees twos r = maketree (map head (eqCl f [b |(a,b)<-twos, f r==f a])) [(a,b) |(a,b)<-twos, f r/=f a]

Specialization with glue:

>  locate :: Eq a => a -> Classification a -> Classification a
>  locate c Bottom = Bottom
>  locate c (Cl r cls) 
>    | r==c      = (Cl r cls)
>    | otherwise = head([Cl r cls| Cl r cls<-[locate c cl| cl<-cls]]++[Bottom])

>  locates :: Eq a => a -> Classification a -> [Classification a]
>  locates c Bottom = []
>  locates c (Cl r cls) 
>    | r==c      = [Cl r cls]
>    | otherwise = concat (map (locates c) cls)

>  locatesF :: (a->Bool) -> Classification a -> [Classification a]
>  locatesF f Bottom = []
>  locatesF f (Cl r cls) 
>    | f r = [Cl r cls]
>    | otherwise = concat (map (locatesF f) cls)

>  sortCl :: (a -> a -> Bool) -> Classification a -> Classification a
>  sortCl geq classification
>   = s classification
>   where
>    s (Cl r cls) = Cl r (srt (map s cls))
>    s Bottom     = Bottom
>    srt   []     = []
>    srt (c:cls)  = srt [e|e<-cls, not (root e `geq` root c)] ++ [c] ++
>                   srt [e|e<-cls,      root e `geq` root c]

Fast Lookups using classification:

>  u = [("aapje",1),("aapenoot",2),("",3),("mies",4),("mies",5)]

>  data ClassificObj a b = Cly a [ClassificObj a b] | Oby b deriving Show

>  index :: [(String, a)] -> String -> [a]
>  index tuples = lookup (makeCly tuples)
>   where
>    lookup cls []  = [o| Oby o<-cls]
>    lookup cls key = [o| Cly r clss<-cls, r `prefix` key, o<-lookup clss (drop (length r) key)]
>    [] `prefix` xs = True
>    xs `prefix` [] = False
>    (x:xs) `prefix` (y:ys) = if x==y then xs `prefix` ys else False
>  makeCly set
>   = [Oby y| cl@((a,b):c)<-eqClasses, null a, (x,y)<-cl]++
>     [Cly (take p a) (makeCly [(drop p x,y)|(x,y)<-cl]) |cl@((a,b):c)<-eqClasses, not(null a),
>                                                                                 p<-[prefix cl] ] 
>     where
>      eqClasses = eqC set
>      prefix cl | length (eqC cl) == 1 = if null(fst(head cl)) then 0 else prefix [(tail x,y)|(x,y)<-cl]+1
>                | otherwise            = 0
>      eqC = eqCl (take 1.fst)

  makeCly [("aapje",1),("aapenoot",2),("",3),("mies",4)]
=
  [f cl |cl<-eqC [("aapje",1),("aapenoot",2),("",3),("mies",4)]] 
=
  [f [("aapje",1),("aapenoot",2)],f [("",3)],f [("mies",4)]]
=
  [Cly "aap" (makeCly [("je",1),("enoot",2)]),Cly "" [Oby 3],Cly "mies" [Oby 4]]
=
  [Cly "aap" [Cly "je" [Oby 1],Cly "enoot" [Oby 2]],Cly "" [Oby 3],Cly "mies" [Oby 4]]

>  invmakeCly = f ""
>   where f str (Oby y)     = [(str,y)]
>         f str (Cly r cls) = concat [f (str++r) cl| cl<-cls]

  invmakeCl (Cly "" [Cly "aap" [Cly "je" [],Cly "enoot" []],Cly "" [],Cly "mies" []])
=
  [""++str| str<-concat [invmakeCl cl| cl<-[Cly "aap" [Cly "je" [],Cly "enoot" []],Cly "" [],Cly "mies" []]]]
=
  [""++str| str<-concat [invmakeCl (Cly "aap" [Cly "je" [],Cly "enoot" []]),invmakeCl (Cly "" []),invmakeCl (Cly "mies" [])]]
=

De volgende expressie levert True op??
   allezaken == invmakeCly (Cly "" (makeCly allezaken))
   

>  instance Conceptual a => Conceptual (Classification a) where
>   conts                                         = rd . concat . map conts . preCl

   
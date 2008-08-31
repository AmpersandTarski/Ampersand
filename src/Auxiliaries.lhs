> module Auxiliaries
> (  adlVersion
>  , encode, decode
>  , unCap, upCap
>  , fst3, snd3, thd3
>  , chain
>  , showL
> -- Verplaatst naar CommonClasses , rd
>  , rEncode
>  , commaEng
>  , commaNL
>  , clos1
>  , clos
>  , diag
>  , sort
>  , sord
>  , eqCl 
>  , eqClass
>  , rd'
>  , enumerate
>  , sort'
>  , enc
>  , sord'
>  , elem'
>  , mumble
>  , fixSpaces
>  , transpose
>  , haskellIdentifier
> )
> where
>  import Char  (isAlpha,isAlphaNum,ord,isUpper,toLower,toUpper,digitToInt,intToDigit)
>  import Collection (Collection(isc,uni,(>-),rd))

>  adlVersion = "ADL vs. 0.8.10"
>  encode :: String -> Int
>  encode  = enc.reverse
>   where enc "" = 0
>         enc (c:cs) = digitToInt c + 10* enc cs
>  decode :: Int -> String
>  decode 0 = "0"
>  decode n = if n `div` 10 == 0 then [intToDigit (n `rem` 10)|n>0] else decode (n `div` 10)++[intToDigit (n `rem` 10)]

>  unCap [] = [] ; unCap (h:t) = toLower h:t
>  upCap [] = [] ; upCap (h:t) = toUpper h:t

>  fst3 (a,b,c) = a
>  snd3 (a,b,c) = b
>  thd3 (a,b,c) = c

>  rEncode :: String -> String
>  rEncode str = charEncode False str
>    where
>       charEncode :: Bool -> String -> String
>       charEncode casePrev str
>        = t casePrev (concat [if isAlphaNum c then [c] else "_"++three (ord c)| c<-str])
>          where three = reverse . take 3 . reverse . ("00"++) . show
>                t casePrev (c:str) | not (isAlpha c) || isUpper c == casePrev = c: t casePrev str
>                                   | otherwise                                = '_': c: t (not casePrev) str
>                t casePrev []      = []

>  mumble :: String -> String
>  mumble str  = concat [if c==' ' then "_" else [c]| c<-str]

----------------------------------------------------
Warshall's transitive closure algorithm in Haskell:
----------------------------------------------------
clos1 [[1,2],[2,3],[4,5]] = [[1,2],[2,3],[4,5],[1,3]]
clos1 [[1,2],[2,3],[4,5],[12,0],[0,5],[6,12],[12,24]] = [[1,2],[2,3],[4,5],[12,0],[0,5],[6,12],[12,24],[1,3],[12,5],[6,0],[6,24],[6,5]]

>  clos1 :: (Eq b) => [[b]] -> [[b]] 
>  clos1 xs
>    = f xs (rd (map head xs) `isc` rd (map last xs))
>      where
>       f q (x:xs) = f (q `uni` [[a,b']|[a,b]<-q,b==x,[a',b']<-q,a'==x]) xs
>       f q []     = q

----------------------------------------------------
clos is meant to calculate all paths, truncated at the first complete cycle.
----------------------------------------------------
clos [[2,2],[1,1],[2,3],[3,4],[0,1],[5,5]] =
  [ [[2,2]]
  , [[1,1]]
  , [[5,5]]
  , [[2,3],[3,4]]
  , [[0,1]]
  ]

clos [[1,2],[2,3],[4,5]] =
  [ [[1,2],[2,3]]
  , [[4,5]]
  ]

clos [[1,2],[2,1]] =
  [ [[1,2],[2,1]]
  , [[2,1],[1,2]]
  ]

clos [[1,2],[2,3],[4,5],[3,4]] =
  [ [[1,2],[2,3],[3,4],[4,5]]
  ]

clos [[1,2],[2,3],[4,5],[5,1]] =
  [ [[4,5],[5,1],[1,2],[2,3]]
  ]

clos [[1,2],[2,3],[4,5],[3,4],[5,1]] =
  [ [[1,2],[2,3],[3,4],[4,5],[5,1]]
  , [[2,3],[3,4],[4,5],[5,1],[1,2]]
  , [[4,5],[5,1],[1,2],[2,3],[3,4]]
  , [[3,4],[4,5],[5,1],[1,2],[2,3]]
  , [[5,1],[1,2],[2,3],[3,4],[4,5]]
  ]

clos [[1,2],[2,3],[4,5],[12,0],[0,5],[6,12],[12,24]] =
  [ [[6,12],[12,0],[0,5]]
  , [[1,2],[2,3]]
  , [[6,12],[12,24]]
  , [[4,5]]
  ]

clos [] =
  []

>  clos :: (Eq a, Eq b) => (b->a) -> (b->a) -> [b] -> [[b]] 
>  clos left right tuples
>    = [[e]| e<-tuples, right e==left e]++(unsublist.f 1) [[e]| e<-tuples, right e/=left e]
>      where
>       m = length (rd [c|ts<-tuples, c<-[left ts,right ts]]) `min` length tuples  -- maximum path length possible
>       f n pths
>        = if n>length tuples then pths else
>          f (2*n) (long++pths)
>          where long = [xs++(ys>-xs)| xs<-pths, ys<-pths                         -- cartesian product
>                                    , n-length xs < length ys                    -- so: n < length (xs++ys)
>                                    , length ys <= (2*n `min` m)-length xs       -- so:     length (xs++ys) <=  (2*n `min` m)
>                                    , right (last xs)==left (head ys)            -- join
>                                    , not (or [t `isPrefix` xs| t<-tails ys])    -- no cycles
>                                    ]
>       tails ts@(_:_) = ts: tails (tail ts)
>       tails [] = []
>       unsublist [] = []
>       unsublist (xs:xss) = xs: unsublist[ys| ys<-xss, not (ys `isSublist` xs)]

Test spul voor clos

>  tests = (putStr.chain "\n".map test)
>          [ [[2,2],[1,1],[2,3],[3,4],[0,1],[5,5]]
>          , [[1,2],[2,3],[4,5]]
>          , [[1,2],[2,3],[3,2]]
>          , [[1,1],[1,2],[2,3],[3,2]]
>          , [[1,2],[2,1]]
>          , [[1,2],[2,3],[4,5],[3,4]]
>          , [[1,2],[2,3],[4,5],[5,1]]
>          , [[1,2],[2,3],[4,5],[3,4],[5,1]]
>          , [[1,2],[2,3],[4,5],[12,0],[0,5],[6,12],[12,24]]
>          , []
>          ]
>   where 
>    test c = "clos "++show c++" = \n  "++(if null ps then "[]" else "[ "++chain "\n  , " (map show ps)++"  \n  ]")++"\n"
>             where ps = clos head last c


>  isPrefix :: Eq a => [a] -> [a] -> Bool
>  []     `isPrefix` _      = True
>  (x:xs) `isPrefix` (y:ys) = x==y && xs `isPrefix` ys
>  _      `isPrefix`  _     = False

>  isSublist :: Eq a => [a] -> [a] -> Bool
>  [] `isSublist` _  = True
>  xs `isSublist` ys = xs `isPrefix` ys  ||  length xs<=length ys && xs `isSublist` tail ys

clos [[1,2],[2,3],[4,5],[3,4]]
f 1 [[[1,2]],[[2,3]],[[4,5]],[[3,4]]]

Cartesian product by diagonalization of two (possibly infinite) lists

>  diag :: [a] -> [a] -> [a] -> [a] -> [[a]]
>  diag xt (x:xs) yt (y:ys)
>   = [x,y]: [[x,t]|t<-yt]++[[t,y]|t<-xt]++diag (x:xt) xs (y:yt) ys
>  diag xt [] yt ys = [[t,y]|y<-ys, t<-xt]
>  diag xt xs yt [] = [[x,t]|x<-xs, t<-yt]

>  chain :: String -> [String] -> String
>  chain str [] = []
>  chain str xs = foldl f (head xs) (tail xs) where f x y = x++str++y

>  showL   :: [String] -> String
>  showL xs = "["++chain "," xs++"]"

>  commaEng :: String -> [String] -> String
>  commaEng str [a,b,c]= a++", "++b++", "++str++" "++c
>  commaEng str [a,b]  = a++" "++str++" "++b
>  commaEng str [a]    = a
>  commaEng str (a:as) = a++", "++commaEng str as
>  commaEng str []     = ""

>  commaNL :: String -> [String] -> String
>  commaNL str [a,b,c]= a++", "++b++" "++str++" "++c
>  commaNL str [a,b]  = a++" "++str++" "++b
>  commaNL str [a]    = a
>  commaNL str (a:as) = a++", "++commaNL str as
>  commaNL str []     = ""

>  enumerate :: [String] -> String
>  enumerate [] = []
>  enumerate [x]= x
>  enumerate xs = chain ", " (init xs)++" and "++last xs

>  eqClass :: (a -> a -> Bool) -> [a] -> [[a]]
>  eqClass f [] = []
>  eqClass f (x:xs) = (x:[e|e<-xs, f x e]) : eqClass f [e|e<-xs, not (f x e)]

>  eqCl :: Eq b => (a -> b) -> [a] -> [[a]]
>  eqCl f [] = []
>  eqCl f (x:xs) = (x:[e|e<-xs, f x==f e]) : eqCl f [e|e<-xs, f x/=f e]

>  rd' ::  Eq e => ( a -> e ) -> [a] -> [a]
>  rd' f [] = []
>  rd' f (x:xs) = x: rd' f [e|e<-xs, f e /= f x]

>  sort :: (Ord a) => [a] -> [a]
>  sort [] = []
>  sort (x:xs) = sort [e|e<-xs, e<x] ++ [x] ++ sort [e|e<-xs, e>=x]

sort = sort' id

>  sort' :: (Ord b) => (a -> b) -> [a] -> [a]
>  sort' f [] = []
>  sort' f (x:xs) = sort' f [e|e<-xs, f e<f x] ++ [x] ++ sort' f [e|e<-xs, f e>=f x]

sord = sort.rd

>  sord :: (Ord a) => [a] -> [a]
>  sord [] = []
>  sord (x:xs) = sord [e|e<-xs, e<x] ++ [x] ++ sord [e|e<-xs, e>x]

Oppassen met sord', want er geldt niet  (voor alle f: sord' f = sort' f.rd)   !!!

>  sord' :: Ord b => (a -> b) -> [a] -> [a]
>  sord' f [] = []
>  sord' f (x:xs) = sord' f [e|e<-xs, f e<f x] ++ [x] ++ sord' f [e|e<-xs, f e>f x]

>  elem' :: (a -> a -> Bool) -> a -> [a] -> Bool
>  elem' eq e xs = not (null [x|x<-xs, eq e x])

>  enc :: Bool -> String -> String
>  enc upper (c:cs) | not (isAlphaNum c) = '_': htmlEnc c ++ enc upper cs
>                   | isUpper c==upper   = c: enc upper cs
>                   | otherwise          = '_': c: enc (not upper) cs
>    where 
>       htmlEnc = reverse . take 3 . (++"00") . reverse . show . ord
>  enc _ "" = ""

Spaties

>  fixSpaces :: Int -> String -> String
>  fixSpaces n a = [' '| i<-[1..n-length str]]++str
>   where str = show a

Haskell ondersteuning

maak een Haskell identifier van een willekeurige String
TODO 7 aug 2008: checken of dit klopt met de Haskell scanner!

>  haskellIdentifier "" = ""
>  haskellIdentifier (c:cs) | isAlphaNum c || c=='\''  = c: haskellIdentifier cs
>                           | otherwise                = haskellIdentifier (conceptForm cs)
>   where
>     conceptForm (c:cs) = toUpper c: map toLower cs
>     conceptForm "" = ""

> ---------Onderstaande code lijkt niet meer nodig....  ----------

  charDecode :: Bool -> String -> String
  -- ^ This function converts a string to a string. A boolean is needed that tells wether or not case-preservation is taken into account.
  charDecode casePrev ('_':c:str) | isAlpha c    = charDecode (not casePrev) (c:str)
                                  | otherwise    = chr (prsInt (c:take 2 str)): charDecode casePrev (drop 2 str)
                                                   where prsInt [a,b,c] = (ord a-ord '0')*100 + (ord b-ord '0')*10 + (ord c-ord '0')
  charDecode casePrev (c:str)     | isAlphaNum c = form c: charDecode casePrev str
                                  | otherwise    = error ("charDecode ("++show (c:str)++")")
                                                   where form = if casePrev then toUpper else toLower
  charDecode casePrev [] = []

  cEncode :: String -> String
  cEncode (c:str) = charEncode True [c]++charEncode False str
  cEncode "" = ""

  cDecode :: String -> String
  cDecode ('_':c:str) | isAlpha c = charDecode False [c]++ charDecode False str
                      | otherwise = error ("cDecode ("++show (c:str)++") requires a letter to start with.")
  cDecode (c:str)     | isAlpha c = charDecode True [c]++ charDecode False str
                      | otherwise = error ("cDecode ("++show (c:str)++") requires a letter to start with.")
  cDecode "" = ""

  rDecode :: String -> String
  rDecode str = charDecode False str


  dePoint :: String -> String
  dePoint ""  = ""
  dePoint str = if head str=='.' then tail str else
                if last str=='.' then init str else str
  strip :: String -> String
  strip = filter isAlphaNum

  conceptForm :: String -> String
  conceptForm (c:cs) = strip (toUpper c: map toLower cs)
  conceptForm "" = ""


  article :: String -> String 
  article "" = ""
  article (c:cs) = if toLower c `elem` "aeiou" then "an" else "a"

  redu1 :: (Eq eq) => [[eq]] -> [[eq]]
  redu1 xs
    = f xs (rd (map head xs) `isc` rd (map last xs))
      where
       f q (x:xs) = f (q >- [[a,b']|[a,b]<-q,b==x,[a',b']<-q,a'==x]) xs
       f q []     = q

  chainIfNull :: String -> String -> [String] -> String
  chainIfNull nlStr str xs = if null xs then nlStr else chain str xs

  commaAnd :: [String] -> String
  commaAnd [a,b,c]= a++", "++b++", and "++c
  commaAnd [a,b]  = a++" and "++b
  commaAnd [a]    = a
  commaAnd (a:as) = a++", "++commaAnd as
  commaAnd []     = ""


  commaEn :: [String] -> String
  commaEn [a,b,c]= a++", "++b++" en "++c
  commaEn [a,b]  = a++" en "++b
  commaEn [a]    = a
  commaEn (a:as) = a++", "++commaEn as
  commaEn []     = ""

  eqCls :: (Ord b) => (a -> b) -> [a] -> [[a]]
  eqCls f [] = []
  eqCls f (x:xs) = eqCls f [e|e<-xs, f x<f e] ++ [x:[e|e<-xs, f x==f e]] ++ eqCls f [e|e<-xs, f x>f e]

  fst3 :: ( a, b, c) -> a
  fst3 (a,b,c) = a
  snd3 :: ( a, b, c) -> b
  snd3 (a,b,c) = b
  thd3 :: ( a, b, c) -> c
  thd3 (a,b,c) = c

O(n+m)?
joinsort a b = sort (a++b) ; op voorwaarde dat sort a = a en sort b = b

  joinsort :: (Ord a) => [a] -> [a] -> [a]
  joinsort [] a = a
  joinsort a [] = a
  joinsort (a:as) (b:bs) | a<b = a:(joinsort (as) (b:bs))
  joinsort (a:as) (b:bs) | a>=b = b:(joinsort (a:as) (bs))

O(n+m)?
joinsord a b = (joinsort.rd) a b ; op voorwaarde dat sord a = a en sord b = b

  joinsord :: (Ord a) => [a] -> [a] -> [a]
  joinsord [] a = a
  joinsord a [] = a
  joinsord (a:as) (b:bs) | a<b = a:(joinsord (as) (b:bs))
  joinsord (a:as) (b:bs) | a>b = b:(joinsord (a:as) (bs))
  joinsord (a:as) (b:bs) | a==b = b:(joinsord (as) (bs))


Permutaties

  perms :: [a] -> [[a]]
  perms [] = [[]]
  perms (x:xs) = [ take i ps++[x]++drop i ps | i<-[0..length xs], ps<-perms xs ]

>  transpose :: [[e]] -> [[e]]
>  transpose (xs:yss) = [x:ys |x<-xs , ys<-transpose yss]
>  transpose []       = [[]|i<-[0..]]


Omzetting van case sensitive naar case insensitive

Example:
   htmlEnc '|' = "124"
   htmlDec "124" = '|'
   
  htmlEnc :: Char -> String
  htmlEnc = reverse . take 3 . (++"00") . reverse . show . ord

  htmlDec :: String -> Char
  htmlDec = chr . f . reverse
   where f "" = 0
         f (c:cs) = ord c - ord '0' + 10*f cs

  dec :: Bool -> String -> String
  dec upper ('_': c: cs) = if isAlpha c
                           then dec (not upper) (c:cs)
                           else htmlDec (c: take 2 cs) : dec upper (drop 2 cs)
      where
        htmlDec = chr . f . reverse
        f "" = 0
        f (c:cs) = ord c - ord '0' + 10*f cs      
  dec upper (c:cs)       = (if upper then toUpper else toLower) c: dec upper cs
  dec _ _ = ""
  
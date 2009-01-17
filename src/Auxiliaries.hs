  module Auxiliaries
  (fst3, snd3, thd3
   , adlVersion
 --  , unCap, upCap
   , showL
   , rEncode
   , commaEng
   , commaNL
   , clos1
   , clos
   , diag
   , sort
   , sord
   , eqCl 
   , eqClass
   , rd'
   , enumerate
   , sort'
   , enc
   , sord'
   , elem'
   , mumble
   , fixSpaces
   , haskellIdentifier

   , module Strings
  )
  where
   import Char  (isAlpha,isAlphaNum,ord,isUpper,toLower,toUpper,digitToInt,intToDigit)
   import Collection (Collection(isc,uni,(>-),rd))
   import Strings (chain, unCap, upCap,firstCaps)

   adlVersion = "ADL vs. 0.8.10-218" -- the number behind the last digit is the SVN revision number.

   fst3 (a,b,c) = a
   snd3 (a,b,c) = b
   thd3 (a,b,c) = c

   rEncode :: String -> String
   rEncode str = charEncode False str
     where
        charEncode :: Bool -> String -> String
        charEncode casePrev str
         = t casePrev (concat [if isAlphaNum c then [c] else "_"++three (ord c)| c<-str])
           where three = reverse . take 3 . reverse . ("00"++) . show
                 t casePrev (c:str) | not (isAlpha c) || isUpper c == casePrev = c: t casePrev str
                                    | otherwise                                = '_': c: t (not casePrev) str
                 t casePrev []      = []

   mumble :: String -> String
   mumble str  = concat [if c==' ' then "_" else [c]| c<-str]

   clos1 :: (Eq b) => [[b]] -> [[b]] 
   clos1 xs
     = f xs (rd (map head xs) `isc` rd (map last xs))
       where
        f q (x:xs) = f (q `uni` [[a,b']|[a,b]<-q,b==x,[a',b']<-q,a'==x]) xs
        f q []     = q



   clos :: (Eq a, Eq b) => (b->a) -> (b->a) -> [b] -> [[b]] 
   clos left right tuples
     = [[e]| e<-tuples, right e==left e]++(unsublist.f 1) [[e]| e<-tuples, right e/=left e]
       where
        m = length (rd [c|ts<-tuples, c<-[left ts,right ts]]) `min` length tuples  -- maximum path length possible
        f n pths
         = if n>length tuples then pths else
           f (2*n) (long++pths)
           where long = [xs++(ys>-xs)| xs<-pths, ys<-pths                         -- cartesian product
                                     , n-length xs < length ys                    -- so: n < length (xs++ys)
                                     , length ys <= (2*n `min` m)-length xs       -- so:     length (xs++ys) <=  (2*n `min` m)
                                     , right (last xs)==left (head ys)            -- join
                                     , not (or [t `isPrefix` xs| t<-tails ys])    -- no cycles
                                     ]
        tails ts@(_:_) = ts: tails (tail ts)
        tails [] = []
        unsublist [] = []
        unsublist (xs:xss) = xs: unsublist[ys| ys<-xss, not (ys `isSublist` xs)]



   tests = (putStr.chain "\n".map test)
           [ [[2,2],[1,1],[2,3],[3,4],[0,1],[5,5]]
           , [[1,2],[2,3],[4,5]]
           , [[1,2],[2,3],[3,2]]
           , [[1,1],[1,2],[2,3],[3,2]]
           , [[1,2],[2,1]]
           , [[1,2],[2,3],[4,5],[3,4]]
           , [[1,2],[2,3],[4,5],[5,1]]
           , [[1,2],[2,3],[4,5],[3,4],[5,1]]
           , [[1,2],[2,3],[4,5],[12,0],[0,5],[6,12],[12,24]]
           , []
           ]
    where 
     test c = "clos "++show c++" = \n  "++(if null ps then "[]" else "[ "++chain "\n  , " (map show ps)++"  \n  ]")++"\n"
              where ps = clos head last c


   isPrefix :: Eq a => [a] -> [a] -> Bool
   []     `isPrefix` _      = True
   (x:xs) `isPrefix` (y:ys) = x==y && xs `isPrefix` ys
   _      `isPrefix`  _     = False

   isSublist :: Eq a => [a] -> [a] -> Bool
   [] `isSublist` _  = True
   xs `isSublist` ys = xs `isPrefix` ys  ||  length xs<=length ys && xs `isSublist` tail ys






   diag :: [a] -> [a] -> [a] -> [a] -> [[a]]
   diag xt (x:xs) yt (y:ys)
    = [x,y]: [[x,t]|t<-yt]++[[t,y]|t<-xt]++diag (x:xt) xs (y:yt) ys
   diag xt [] yt ys = [[t,y]|y<-ys, t<-xt]
   diag xt xs yt [] = [[x,t]|x<-xs, t<-yt]

   showL   :: [String] -> String
   showL xs = "["++chain "," xs++"]"

   commaEng :: String -> [String] -> String
   commaEng str [a,b,c]= a++", "++b++", "++str++" "++c
   commaEng str [a,b]  = a++" "++str++" "++b
   commaEng str [a]    = a
   commaEng str (a:as) = a++", "++commaEng str as
   commaEng str []     = ""

   commaNL :: String -> [String] -> String
   commaNL str [a,b,c]= a++", "++b++" "++str++" "++c
   commaNL str [a,b]  = a++" "++str++" "++b
   commaNL str [a]    = a
   commaNL str (a:as) = a++", "++commaNL str as
   commaNL str []     = ""

   enumerate :: [String] -> String
   enumerate [] = []
   enumerate [x]= x
   enumerate xs = chain ", " (init xs)++" and "++last xs

   eqClass :: (a -> a -> Bool) -> [a] -> [[a]]
   eqClass f [] = []
   eqClass f (x:xs) = (x:[e|e<-xs, f x e]) : eqClass f [e|e<-xs, not (f x e)]

   eqCl :: Eq b => (a -> b) -> [a] -> [[a]]
   eqCl f [] = []
   eqCl f (x:xs) = (x:[e|e<-xs, f x==f e]) : eqCl f [e|e<-xs, f x/=f e]

   rd' ::  Eq e => ( a -> e ) -> [a] -> [a]
   rd' f [] = []
   rd' f (x:xs) = x: rd' f [e|e<-xs, f e /= f x]

   sort :: (Ord a) => [a] -> [a]
   sort [] = []
   sort (x:xs) = sort [e|e<-xs, e<x] ++ [x] ++ sort [e|e<-xs, e>=x]



   sort' :: (Ord b) => (a -> b) -> [a] -> [a]
   sort' f [] = []
   sort' f (x:xs) = sort' f [e|e<-xs, f e<f x] ++ [x] ++ sort' f [e|e<-xs, f e>=f x]



   sord :: (Ord a) => [a] -> [a]
   sord [] = []
   sord (x:xs) = sord [e|e<-xs, e<x] ++ [x] ++ sord [e|e<-xs, e>x]



   sord' :: Ord b => (a -> b) -> [a] -> [a]
   sord' f [] = []
   sord' f (x:xs) = sord' f [e|e<-xs, f e<f x] ++ [x] ++ sord' f [e|e<-xs, f e>f x]

   elem' :: (a -> a -> Bool) -> a -> [a] -> Bool
   elem' eq e xs = not (null [x|x<-xs, eq e x])

   enc :: Bool -> String -> String
   enc upper (c:cs) | not (isAlphaNum c) = '_': htmlEnc c ++ enc upper cs
                    | isUpper c==upper   = c: enc upper cs
                    | otherwise          = '_': c: enc (not upper) cs
     where 
        htmlEnc = reverse . take 3 . (++"00") . reverse . show . ord
   enc _ "" = ""



   fixSpaces :: Int -> String -> String
   fixSpaces n a = [' '| i<-[1..n-length str]]++str
    where str = show a



   haskellIdentifier :: String -> String
   haskellIdentifier "" = ""
   haskellIdentifier (c:cs) | isAlphaNum c || c=='\''  = c: haskellIdentifier cs
                            | otherwise                = haskellIdentifier (conceptForm cs)
    where
      conceptForm (c:cs) = toUpper c: map toLower cs
      conceptForm "" = ""


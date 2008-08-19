> module CommonClasses
> (  Identified(name)
>  , Collection (eleM,uni,isc,(>-),empty,elems)
>  , DataStruct(showStruct)
> )
> where
>  ----------------------------------------------
>  class Identified a where
>   name   :: a->String

>  instance Identified a => Identified [a] where
>   name [] = ""
>   name (i:is) = name i

>  ----------------------------------------------
>  ---- Collection of type a --------------------
>  ----------------------------------------------
>  infixl 5  >-

>  class Collection a where
>   eleM     :: Eq b => b -> a b -> Bool  
>   uni, isc :: Eq b => a b -> a b -> a b  
>   (>-)     :: Eq b => a b -> a b -> a b
>   empty    :: Eq b => a b
>   elems    :: Eq b => a b -> [b]

>  instance Collection [] where
>   eleM        = any . (==)
>   xs `uni` ys = xs++(ys>-xs)
>   xs `isc` ys = [y| y<-ys, y `elem` xs]
>   xs >- ys    = [x| x<-xs, not (x `elem` ys)]
>   empty       = []
>   elems       = id

>  class DataStruct a where
>   showStruct ::  a->String
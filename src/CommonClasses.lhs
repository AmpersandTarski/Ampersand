> module CommonClasses
> (  Identified(name)
>  , DataStruct(showStruct)
> )

> where
>  import  Strings

>  ----------------------------------------------
>  class Identified a where
>   name   :: a->String
>   -- idName :: a->String

>  instance Identified a => Identified [a] where
>   name [] = ""
>   name (i:is) = name i
>   -- idName c = idNam (name c)

>  class DataStruct a where
>   showStruct ::  a->String
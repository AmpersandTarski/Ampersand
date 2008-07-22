> module ObjBinGenObject where
>  import Char
>  import Auxiliaries
>  import Calc(informalRule, shrink, disjNF, computeOrder, ComputeRule, triggers)
>  import CC_aux
>  import CommonClasses
>  import ERmodel
>  import PredLogic -- (for error messages by dbCorrect)
>  import Hatml     -- (for converting error messages to HTML)
>  import Atlas     -- (for converting error messages to HTML)
>  import RelBinGenBasics
> 

>  objectServices context filename object
>   = (chain "\n  "
>     ([ "<?php // generated with "++adlVersion
>      , ""
>      , "/********* file "++filename++" on line "++(show (pos object))
>      ] ++ (map ((++) " * ") (
>         ("OBJECT "++(name object)++" "++(name (concept object)))
>       :[] ++ ["ENDOBJECT"] )) ++
>      [" *********/"
>      , ""
>      ]
>     )) ++ "\n?>"
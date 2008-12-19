> module Views --( viewEstimate )
> where

>  import Char
>  import CommonClasses ( Identified(name))
>  import Collection (Collection (isc,(>-),empty, rd))
>  import Auxiliaries
>         (  --adlVersion
>  --      , encode, decode
>            unCap, upCap
>          , fst3, snd3
>  --      , thd3
>          , chain
>          , showL
>  --      , rEncode
>          , commaEng
>          , commaNL
>  --      , clos1
>  --      , clos
>  --      , diag
>          , sort
>  --      , sord
>          , eqCl 
>          , eqClass
>          , rd'
>  --      , enumerate
>          , sort'
>  --      , enc
>          , sord'
>  --      , elem'
>  --      , mumble
>          , fixSpaces
>  --      , transpose
>          , haskellIdentifier
>         )
>  import Classification
>   (  Classification(Cl, Bottom)
>     , preCl
>    )
>  import Languages(Lang(Dutch,English),ShowLang(showLang),plural)
>  import Typology
>  import ADLdef
>  import ShowADL
>  import CC_aux( ShowHS(showHSname, showHS)
>                , isSgn
>                , applyM ,conts, explain, fEmpty
>                )
>  import Calc
>  import PredLogic
>  import HtmlFilenames
>  import ERmodel (erAnalysis)
>  import Fspec

>  viewEstimate :: Context -> String
>  viewEstimate context
>   = chain "\n\n" [showADL d| d<-datasets]
>     where
>      makeView (DS c pths)
>            = Obj (haskellIdentifier (name c))
>                  posNone
>                  (Tm (mIs c))
>                  [ Obj (name m) posNone (Tm m) [] | m<-pths ]
>      makeView (BR m) = Obj (name m) posNone (Tm m) []
>      (datasets, viewEsts, relations, ruls) = erAnalysis context


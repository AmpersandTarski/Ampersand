
> module FspecDef
  
    This module contains the structure definition of Fspec. It should
    not contain anything, but the structure of the Fspc together with 
    functionality to show it. 

> where


>  import CC_aux (ShowHS(showHS), showHSname
>            -- Onderstaande zijn taalconcepten uit ADL, en zijn één op een 
>            -- overgenomen in Fspec. Als de taal Fspec uitbreid, zou dit
>            -- waarschijnlijk onafhankelijker moeten gebeuren... 
>                , Concept
>                , Pattern
> 				 , Rule
>                , ObjectDef
>                , Morphism
>                , Morphic(source, target) 
>                , Morphical( concs, conceptDefs, mors, morlist, declarations, genE, closExprs )
>                , Expression)
>  import CommonClasses
>  import Auxiliaries

>  data Fspec = Fspc 
>               String        -- The name of the specification
>               [Ftheme]      -- One for every pattern
>               [Dataset]     -- One for every (group of) relations
>               [Fview]       -- One for every view 
>               [Frule]       -- One for every rule
>  instance DataStruct Fspec where
>   showStruct fspec =  "   -- context   (Fspc has this structure:  Fspc name themes datasets views vrules)"

>  instance Identified Fspec where
>   name    (Fspc fname fthemes fdatasets fviews frules) = fname

>  themes  (Fspc fname fthemes fdatasets fviews frules) = fthemes
>  datasets(Fspc fname fthemes fdatasets fviews frules) = fdatasets
>  views   (Fspc fname fthemes fdatasets fviews frules) = fviews
>  vrules  (Fspc fname fthemes fdatasets fviews frules) = frules

>  instance ShowHS Fspec where
>   showHSname fspec = "f_Ctx_"++haskellIdentifier (name fspec)
>   showHS indent fspec
>    = "Fspc "++showHSname fspec++showStruct fspec++
>      (if null (themes   fspec) then " []" else indent++"{- themes   -}  "++showL [showHSname t|t<-themes   fspec ])++
>      (if null (datasets fspec) then " []" else indent++"{- datasets -}  "++showL [showHSname d|d<-datasets fspec ])++
> --     (if null (views    fspec) then " []" else indent++"{- views    -}  "++showL [showHSname o|o<-views    fspec ])++
>      (if null (vrules   fspec) then " []" else indent++"{- rules    -}  "++showL [showHSname o|o<-vrules   fspec ])++
>      indent++"where"++
>      indent++" gE = genE "++showHSname fspec++
>  --  (if null (views     fspc ) then "" else concat [indent++" "++showHSname v++indent++"  = "++showHS (indent++"    ") v|v<-views   ]++"\n")++
>  --  (if null (vrules    fspc ) then "" else concat [indent++" "++showHSname r++indent++"  = "++showHS (indent++"    ") r|r<-vrules  ]++"\n")++
>  --  (if null (datasets  fspc ) then "" else concat [indent++" "++showHSname d++indent++"  = "++showHS (indent++"    ") d|d<-datasets]++"\n")++
>  --  (if null (themes    fspc ) then "" else concat [indent++" "++showHSname t++         " = "++showHS (indent++"    ") t|t<-themes  ]++"\n")
>     ""--  XXX     (if null (patterns context) then "" else concat ["\n\n>  "++showHSname pat++" gE"++"\n>   = "++showHS "\n>     " pat|pat<-patterns context]++"\n")
>



Every Ftheme is a specification that is split in units, which are textual entities.
Every unit specifies one dataset, and each dataset is discussed only once in the entire specification.

>  data Ftheme  = Tspc     -- The constructor
>                 String   -- The name of the theme (aka pattern)
>                 [Funit]  -- The units of the theme
>  instance DataStruct Ftheme where
>   showStruct theme =  "   -- context   (Fspc has this structure:  Fspc name themes datasets views vrules)"

>  instance ShowHS Ftheme where
>   showHSname (Tspc name _ )= name 
>   showHS indent ftheme
>    = indent ++ "NOG TE DOEN"

>  data Funit = Uspc String Pattern
>                    [ ( ObjectDef
>            --         ,FPA
>                       , [Morphism]
>                       ,[(Expression,Rule)])]
>                    [ServiceSpec] -- services

The following functional specification, funcSpec, computes which relations are may be affected by compute rules.
Assuming that they will be computed in all cases, all other relations are treated as input parameters.
This assumption, however, is not true.
TODO: determine which relations are affected but not computed, and report as an error.

>  data ServiceSpec = Sspc String       -- name of the service
>                          [Morphism]   -- the list of relations this service may see
>                          [Morphism]   -- the list of relations this service may change
>            -- Hoort hier niet meer thuis             FPA          -- function point analysis information
>                          [ParamSpec]  -- parameters
>                          [ParamSpec]  -- results
>                          [Rule]       -- Invariants
>                          [String]     -- Preconditions
>                          [String]     -- Postconditions
>  data ParamSpec   = Aspc String       -- name of the parameter
>                          String       -- type of the parameter
>                   | Pbool




Very often, concepts can be represented in rectangular relation (n-ary rather than binary).
For this purpose we introduce datasets.
Alternative DS represents a dataset as an identity (the concept) with an arbitrary number of attributes (morphisms, which are functions). 
Alternative BR represents a dataset as a binary relation. This contains morphisms m which are not a function in either direction.

>  data Dataset = DS Concept     -- the root of the dataset
>                    [Morphism]  -- the functions from the root
>               | BR Morphism    -- for every m that is not (isFunction m || isFunction (flp m))

>  instance Morphical Dataset where
>   concs        (DS c pths) = concs pths
>   concs        (BR m     ) = concs m
>   conceptDefs  (DS c pths) = []
>   conceptDefs  (BR m     ) = []
>   mors         (DS c pths) = pths
>   mors         (BR m     ) = [m]
>   morlist      (DS c pths) = pths
>   morlist      (BR m     ) = [m]
>   declarations (DS c pths) = declarations pths
>   declarations (BR m     ) = declarations m
>   genE         (DS c pths) = genE c
>   genE         (BR m     ) = genE m
>   closExprs    (DS c pths) = []
>   closExprs    (BR m     ) = []
>  -- Is dit een ommissie hier? objDefs, keyDefs zijn niet gedefinieerd voor DataSet...

>  instance ShowHS Dataset where
>   showHSname (DS c pths) = "f_DS_"++haskellIdentifier (name c)
>   showHSname (BR m)      = "f_BR_"++haskellIdentifier (name m++name (source m)++name(target m))
>   showHS indent (DS c  [] ) = "DS ("++showHS "" c++") []"
>   showHS indent (DS c pths) = "DS ("++showHS "" c++")"++indent++"   [ "++chain (indent++"   , ") [showHS (indent++"     ") pth| pth<-pths]++indent++"   ]"
>   showHS indent (BR m     ) = "BR ("++showHS "" m++")"

>  instance Eq Dataset where  -- opletten: een dataset moet één vast concept hebben waaraan het wordt herkend.
>   DS c _ == DS d _ = c==d
>   BR m   == BR m'  = m==m'
>   _      == _      = False

>  data Fview  = Fview Dataset ObjectDef [ServiceSpec] [Rule]

>  data Frule = Frul Rule
>  instance ShowHS Frule where
>   showHSname (Frul r)    = "f_rule_"++haskellIdentifier (name r)
>   showHS indent (Frul r) = "Frul ("++showHS "" r++")"



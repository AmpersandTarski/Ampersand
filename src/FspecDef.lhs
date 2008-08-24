
> module FspecDef
  
    This module contains the structure definition of Fspec. It should
    not contain anything, but the structure of the Fspc together with 
    functionality to show it. 

> where

>  import CC_aux (ShowHS(showHS), showHSname
>            -- Onderstaande zijn taalconcepten uit ADL, en zijn één op een 
>            -- overgenomen in Fspec. Als de taal Fspec uitgebreid wordt, zal
>            -- dit waarschijnlijk onafhankelijker moeten gebeuren... 
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

>  data Fspc = Fspc 
>               String        -- The name of the specification
>               [Ftheme]      -- One for every pattern
>               [Dataset]     -- One for every (group of) relations
>               [Fview]       -- One for every view 
>               [Frule]       -- One for every rule
>  instance DataStruct Fspc where
>   showStruct fspec =  "   -- context   (Fspc has this structure:  Fspc name themes datasets views vrules)"
>  instance Identified Fspc where
>   name    (Fspc nm _ _ _ _) = nm

>  themes  (Fspc _ fthemes _ _ _)   = fthemes
>  datasets(Fspc _ _ fdatasets _ _) = fdatasets
>  views   (Fspc _ _ _ fviews _)    = fviews
>  vrules  (Fspc _ _ _ _ frules)    = frules

>  instance ShowHS Fspc where
>   showHSname fspec = "f_Ctx_"++haskellIdentifier (name fspec)
>   showHS indent fspec
>    = "Fspc "++showHSname fspec++showStruct fspec++
>      (if null (themes   fspec) then " []" else indent++"{- themes:   -}  "++showL [showHSname t|t<-themes   fspec ])++
>      (if null (datasets fspec) then " []" else indent++"{- datasets: -}  "++showL [showHSname d|d<-datasets fspec ])++
>      (if null (views    fspec) then " []" else indent++"{- views:    -}  "++showL [showHSname o|o<-views    fspec ])++
>      (if null (vrules   fspec) then " []" else indent++"{- rules:    -}  "++showL [showHSname o|o<-vrules   fspec ])++
>      indent++"where"++
>      indent++" gE = genE "++showHSname fspec++
>      (if null (views    fspec ) then "" else concat [indent++" "++showHSname v++indent++"  = "++showHS (indent++"    ") v|v<-(views    fspec) ]++"\n")++
>      (if null (vrules   fspec ) then "" else concat [indent++" "++showHSname r++indent++"  = "++showHS (indent++"    ") r|r<-(vrules   fspec) ]++"\n")++
>      (if null (datasets fspec ) then "" else concat [indent++" "++showHSname d++indent++"  = "++showHS (indent++"    ") d|d<-(datasets fspec) ]++"\n")++
>      (if null (themes   fspec ) then "" else concat [indent++" "++showHSname t++         " = "++showHS (indent++"    ") t|t<-(themes   fspec) ]++"\n")
>  -- Deze regel met Stef bespreken. Patterns zitten nu in themes, maar daar zit méér in.
>  --    (if null (patterns context) then "" else concat ["\n\n>  "++showHSname pat++" gE"++"\n>   = "++showHS "\n>     " pat|pat<-patterns context]++"\n")
>



Every Ftheme is a specification that is split in units, which are textual entities.
Every unit specifies one dataset, and each dataset is discussed only once in the entire specification.


>  data Fview  = Fview Dataset ObjectDef [ServiceSpec] [Frule]
>  instance Identified Fview where
>   name fview = name (objectdef(fview))

>  dataset   (Fview dset _ _ _) = dset
>  objectdef (Fview _ objd _ _) = objd
>  services  (Fview _ _ svcs _) = svcs
>  frules    (Fview _ _ _ rs)   = rs
>  
>  instance ShowHS Fview where
>   showHSname fview = "f_View_"++haskellIdentifier (name fview)
>   showHS indent fview
>    = "Fobj "  --LATER NOG VERVANGEN DOOR Fview
>      --  ++ datasetSection ++ objdefSection ++ servicesSection ++ rulesSection
>      --  ++indent++" -- Einde Fobj "++showHSname fview
>       where
>         datasetSection  = showHSname (dataset(fview))
>         objdefSection   = indent++" -- objectdef NOG UIT TE WERKEN IN FspecDef.lhs " 
>                         ++indent++"     ("++showHS (indent++"      ") (objectdef fview)++")"
>         servicesSection = indent++" -- services NOG UIT TE WERKEN IN FspecDef.lhs "
>                         ++indent++"     [ "++chain (indent++"     , ") [showHS (indent++"       ") svc| svc<-services(fview)]++indent++"     ]"
>         rulesSection    = indent++" -- rules NOG UIT TE WERKEN IN FspecDef.lhs " 
>                         ++indent++"     ["++chain ", " ["xxx"++showHSname r| r<-frules(fview)]++"]"



>  data Ftheme  = Tspc     -- The constructor
>                 String   -- The name of the theme (aka pattern)
>                 [Funit]  -- The units of the theme
>  instance DataStruct Ftheme where
>   showStruct theme =  "   -- context   (Fspc has this structure:  Fspc name themes datasets views vrules)"
>  instance Identified Ftheme where
>   name (Tspc nm _) = nm 

>  units (Tspc _ funits) = funits
>  instance ShowHS Ftheme where
>   showHSname ftheme = "f_Thm_"++haskellIdentifier (name ftheme) 
>   showHS indent ftheme
>    = "Tspc ("++showHSname ftheme++" gE)"++indent++"     [ "++chain (indent++"     , ") [showHS (indent++"       ") u| u<-units(ftheme)]++indent++"     ]"


>  data Funit = Uspc String Pattern
>                    [ ( ObjectDef
>            --         ,FPA
>                       , [Morphism]
>                       ,[(Expression,Rule)])]
>                    [ServiceSpec] -- services
>  instance Identified Funit where
>   name (Uspc nm _ _ _) = nm 

>  instance ShowHS Funit where
>   showHSname funit = "f_Unit_"++haskellIdentifier (name funit)
>   showHS indent funit
>    = "Uspc "++show (name funit)++" ("++showHSname (pattern funit)++" gE)"
>      ++indent++"     [ "++chain (indent++"     , ") [showHS (indent++"       ") o| o<-objDefs(funit)]++indent++"     ]"
>      ++indent++"     [ "++chain (indent++"     , ") [showHS (indent++"       ") s| s<-servDefs(funit) ]++indent++"     ]"

>  pattern  (Uspc _ pat _ _) = pat
>  viewDefs (Uspc _ _ vwdefs _) = vwdefs
>  servDefs (Uspc _ _ _ svs) = svs
>  objDefs  funit = [o | (o,cs,rs)<-viewDefs(funit)]


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
>  instance ShowHS ServiceSpec where
>   showHSname (Sspc nm sees changes input output rs pre post) = "f_svc_"++haskellIdentifier nm
>   showHS indent (Sspc nm sees changes input output rs pre post)
>    = "Sspc "++nm
>      ++indent++"     [ " ++chain (indent++"     , ") (map (showHS (indent++"       ")) sees   )++indent++"     ] -- these are the visible morphisms: <sees> "
>      ++indent++"     [" ++(if null changes then "]   -- no relations will be changed"  else " "++chain (indent++"     , ") (map (showHS (indent++"       ")) changes)++indent++"     ] -- these are the morphisms that may be altered: <changes> ")
>      ++indent++"     [" ++(if null input   then "]   -- there are no input parameters" else " "++chain "," (map (showHS "") input )++"] -- these are the input parameters: <input>")
>      ++indent++"     [" ++(if null output  then "]   -- no output parameters"          else " "++chain "," (map (showHS "") output)++"] -- these are the output parameters: <output> ")
>      ++indent++"     [" ++(if null rs      then "]   -- there are no rules"            else " "++chain (indent++"     , ") (map (showHS (indent++"       ")) rs  )++indent++"     ]")
>      ++indent++"     [" ++(if null pre     then "]   -- there are no preconditions"    else " "++chain (indent++"     , ") (map  show                        pre )++indent++"     ] -- preconditions")
>      ++indent++"     [" ++(if null post    then "]   -- there are no postconditions"   else " "++chain (indent++"     , ") (map  show                        post)++indent++"     ] -- postconditions")

>  data ParamSpec   = Aspc String       -- name of the parameter
>                          String       -- type of the parameter
>                   | Pbool
>  instance ShowHS ParamSpec where
>   showHSname a@(Aspc nm typ) = error ("(module Fspec) should not showHSname the ParamSpec (Aspc): "++showHS "" a)
>   showHS indent (Aspc nm typ)
>    = "Aspc "++show nm++" "++show typ





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



>  data Frule = Frul Rule
>  instance Identified Frule where
>   name frul = name (rule frul)
>  rule (Frul r) = r

>  instance ShowHS Frule where
>   showHSname frul  = "f_rulexxx_"++haskellIdentifier (name frul)
>   showHS indent (Frul r) = "Frul ("++showHS "" r++")"


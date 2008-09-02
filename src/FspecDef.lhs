
> module FspecDef
  
    This module contains the structure definition of Fspec. It should
    not contain anything, but the structure of the Fspc together with 
    functionality to show it. 

> where

>  import ADLdef
>  import CC_aux ( ShowHS(showHS,showHSname)
>                , Morphical(conceptDefs, mors,morlist, declarations,genE,closExprs)
>                , concs)
>  import CommonClasses(DataStruct(..),Identified(name))
>  import Auxiliaries

>  data Fspc = Fspc -- Fctx 
>               FSid          -- The name of the specification
>               [Ftheme]      -- One for every pattern
>               [Dataset]     -- One for every (group of) relations
>               [Fview]       -- One for every view 
>               [Frule]       -- One for every rule
>  instance DataStruct Fspc where
>   showStruct fspec =  "   -- context   (Fspc has this structure:  Fspc name themes datasets views vrules)"

>  instance Identified Fspc where
>    name fspc = name (fsid fspc)
>      
>  instance Fidentified Fspc where
>   fsid    (Fspc fid _ _ _ _) = fid
>   typ     a = "f_Ctx"

>  themes   :: Fspc -> [Ftheme]
>  themes  (Fspc _ fthemes _ _ _)   = fthemes
>  datasets :: Fspc -> [Dataset]
>  datasets(Fspc _ _ fdatasets _ _) = fdatasets
>  views    :: Fspc -> [Fview]
>  views   (Fspc _ _ _ fviews _)    = fviews
>  vrules   :: Fspc -> [Frule]
>  vrules  (Fspc _ _ _ _ frules)    = frules

>  instance ShowHS Fspc where
>   showHSname fspec = typ fspec ++ "_" ++ showHSname (fsid fspec) --showHS "" (pfixFSid "f_Ctx_" (fsid fspec)) 
>   showHS indent fspec
>    = "Fspc "++showHS "" (fsid fspec)++showStruct fspec++
>      (if null (themes   fspec) then " []" else indent++"{- themes:   -}  "++showL [showHSname t|t<-themes   fspec ])++
>      (if null (datasets fspec) then " []" else indent++"{- datasets: -}  "++showL [showHSname d|d<-datasets fspec ])++
>      (if null (views    fspec) then " []" else indent++"{- views:    -}  "++showL [showHSname o|o<-views    fspec ])++
>      (if null (vrules   fspec) then " []" else indent++"{- rules:    -}  "++showL [showHSname o|o<-vrules   fspec ])++
>      indent++"where"++
>      indent++" gE = genE "++showHSname fspec++
>      (if null (views    fspec ) then "" else concat [indent++" "++showHSname v++indent++"  = "++showHS (indent++"    ") v|v<- views    fspec ]++"\n")++
>      (if null (vrules   fspec ) then "" else concat [indent++" "++showHSname r++indent++"  = "++showHS (indent++"    ") r|r<- vrules   fspec ]++"\n")++
>      (if null (datasets fspec ) then "" else concat [indent++" "++showHSname d++indent++"  = "++showHS (indent++"    ") d|d<- datasets fspec ]++"\n")++
>      (if null (themes   fspec ) then "" else concat [indent++" "++showHSname t++         " = "++showHS (indent++"    ") t|t<- themes   fspec ]++"\n")++
>  -- Deze regel met Stef bespreken. Patterns zitten nu in themes, maar daar zit méér in.
>  --    (if null (patterns context) then "" else concat ["\n\n>  "++showHSname pat++" gE"++"\n>   = "++showHS "\n>     " pat|pat<-patterns context]++"\n")
>      (if null (themes fspec) then "" else concat ["\n\n>  "++showHSname (theme)++" gE"++"\n>   = "++showHS "\n>     " theme|theme<-themes fspec]++"\n")



Every Ftheme is a specification that is split in units, which are textual entities.
Every unit specifies one dataset, and each dataset is discussed only once in the entire specification.


>  data Ftheme  = Tspc     -- The constructor
>                 FSid     -- The name of the theme (aka pattern)
>                 [Funit]  -- The units of the theme
>  instance DataStruct Ftheme where
>   showStruct theme =  "   -- context   (Ftheme has this structure:  Tspc name funits)"
>  instance Fidentified Ftheme where
>   fsid (Tspc fid _) = fid 
>   typ  f = "f_Thm"

>  units :: Ftheme -> [Funit]
>  units (Tspc _ funits) = funits
>  instance ShowHS Ftheme where
>   showHSname ftheme = typ ftheme ++ "_" ++ showHS "" (fsid ftheme) --showHS "" (pfixFSid "f_Theeeeeeeem_" (fsid ftheme))
>   showHS indent ftheme
>    = "Tspc ("++showHS "" (fsid ftheme)++" gE)"++indent++"     [ "++chain (indent++"     , ") [showHS (indent++"       ") u| u<-units(ftheme)]++indent++"     ]"


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

>  instance Fidentified Dataset where
>    fsid (DS c pths) = fsid c
>    fsid (BR m) = fsid m
>    typ  (DS c pths) = "f_DS"
>    typ  (BR m) = "f_DS"

>  instance Fidentified Morphism where
>    fsid m = FS_id (name m++name (source m)++name(target m))  --Hier moet nog goed naar worden gekeken....
>    typ m  = "f_morph"
>  instance Fidentified Concept where
>    fsid c = FS_id (name c)
>    typ m  = "f_cpt"
>  instance Fidentified ObjectDef where
>    fsid o = FS_id (name o)
>    typ m  = "f_objdef"

>  instance ShowHS Dataset where
>   showHSname dset = typ dset ++ "_" ++ showHS "" (fsid dset)
>  -- showHSname dset@(BR m)      = showHS "" (pfixFSid "f_BR_" (fsid dset))
>   showHS indent (DS c  [] ) = "DS ("++showHS "" c++") []"
>   showHS indent (DS c pths) = "DS ("++showHS "" c++")"++indent++"   [ "++chain (indent++"   , ") [showHS (indent++"     ") pth| pth<-pths]++indent++"   ]"
>   showHS indent (BR m     ) = "BR ("++showHS "" m++")"

>  instance Eq Dataset where  -- opletten: een dataset moet één vast concept hebben waaraan het wordt herkend.
>   DS c _ == DS d _ = c==d
>   BR m   == BR m'  = m==m'
>   _      == _      = False



>  data Fview  = Fview Dataset ObjectDef [ServiceSpec] [Frule]
>  instance Fidentified Fview where
>   fsid fview = fsid (objectdef(fview))
>   typ fview = "f_View"
>  dataset   (Fview dset _ _ _) = dset
>  objectdef (Fview _ objd _ _) = objd
>  services  (Fview _ _ svcs _) = svcs
>  frules    (Fview _ _ _ frls) = frls
>  
>  instance ShowHS Fview where
>   showHSname fview = typ fview ++ "_" ++ showHS "" (fsid fview) --showHS "" (pfixFSid "f_Obj_" (fsid fview))  -- XXX moet f_View worden
>   showHS indent fview
>    = "Fview "  --LATER NOG VERVANGEN DOOR Fview
>      ++ datasetSection
>      ++ objdefSection
>      ++ servicesSection
>      ++ rulesSection
>      ++indent++" -- Einde Fview "++showHSname (dataset fview)
>       where
>         datasetSection  = showHS "" (dataset fview)
>         objdefSection   = indent++"     ("++showHS (indent++"      ") (objectdef fview)++")"
>         servicesSection = indent++"     [ "++chain (indent++"     , ") [showHS (indent++"       ") svc| svc<-services(fview)]++indent++"     ]"
>         rulesSection    = indent++"     ["++chain ", " [showHSname fr| fr<-frules(fview)]++"]"




>  data Frule = Frul Rule
>  instance Fidentified Frule where
>   fsid frul = FS_id (name (rule frul))   
>   typ frul = "f_rule"
>  rule:: Frule -> Rule
>  rule (Frul r) = r

>  instance ShowHS Frule where
>   showHSname frul  = typ frul ++ "_" ++ showHS "" (fsid frul) -- showHSname (rule frul) -- XXX moet worden: "f_rule_"++haskellIdentifier (name frul)
>   showHS indent (Frul r) = "Frul ("++showHS "" r++")"



>  data Funit = Uspc FSid Pattern
>                    [ ( ObjectDef
>            --         ,FPA
>                       , [Morphism]
>                       ,[(Expression,Rule)])]
>                    [ServiceSpec] -- services
>  instance Fidentified Funit where
>   fsid (Uspc fid _ _ _) = fid 
>   typ funit = "f_Unit"
>  instance ShowHS Funit where
>   showHSname funit = typ funit ++ "_" ++ showHS "" (fsid funit) --"f_Unit_"++showHS "" (fsid funit)
>   showHS indent funit
>    = "Uspc "++showHS "" (fsid funit)++" ("++showHSname (pattern funit)++" gE)"
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

>  data ServiceSpec = Sspc FSid         -- name of the service
>                          [Morphism]   -- the list of relations this service may see
>                          [Morphism]   -- the list of relations this service may change
>            -- Hoort hier niet meer thuis             FPA          -- function point analysis information
>                          [ParamSpec]  -- parameters
>                          [ParamSpec]  -- results
>                          [Rule]       -- Invariants
>                          [String]     -- Preconditions
>                          [String]     -- Postconditions
>  instance Fidentified ServiceSpec where
>     fsid (Sspc fid _ _ _ _ _ _ _) = fid  
>     typ sspc = "f_svc"
>  sees    (Sspc _ x _ _ _ _ _ _) = x
>  changes (Sspc _ _ x _ _ _ _ _) = x
>  input   (Sspc _ _ _ x _ _ _ _) = x
>  output  (Sspc _ _ _ _ x _ _ _) = x
>  rs      (Sspc _ _ _ _ _ x _ _) = x
>  pre     (Sspc _ _ _ _ _ _ x _) = x
>  post    (Sspc _ _ _ _ _ _ _ x) = x

>  instance ShowHS ServiceSpec where
>   showHSname sspc  = typ sspc ++ "_" ++ showHS "" (fsid sspc) --"f_svc_"++showHS "" (fsid sspc)
>   showHS indent sspc
>     =            "Sspc " ++ showHS "" (fsid sspc)
>      ++indent++"     [ " ++chain (indent++"     , ") (map (showHS (indent++"       ")) (sees sspc)  )++indent++"     ] -- these are the visible morphisms: <sees> "
>      ++indent++"     [" ++(if null (changes sspc) then "]   -- no relations will be changed"  else " "++chain (indent++"     , ") (map (showHS (indent++"       ")) (changes sspc))++indent++"     ] -- these are the morphisms that may be altered: <changes> ")
>      ++indent++"     [" ++(if null (input   sspc) then "]   -- there are no input parameters" else " "++chain "," (map (showHS "") (input sspc) )++"] -- these are the input parameters: <input>")
>      ++indent++"     [" ++(if null (output  sspc) then "]   -- no output parameters"          else " "++chain "," (map (showHS "") (output sspc) )++"] -- these are the output parameters: <output> ")
>      ++indent++"     [" ++(if null (rs      sspc) then "]   -- there are no rules"            else " "++chain (indent++"     , ") (map (showHS (indent++"       ")) (rs sspc) )++indent++"     ]")
>      ++indent++"     [" ++(if null (pre     sspc) then "]   -- there are no preconditions"    else " "++chain (indent++"     , ") (map  show                        (pre sspc))++indent++"     ] -- preconditions")
>      ++indent++"     [" ++(if null (post    sspc) then "]   -- there are no postconditions"   else " "++chain (indent++"     , ") (map  show                        (post sspc))++indent++"     ] -- postconditions")


>  data ParamSpec   = Aspc FSid         -- name of the parameter
>                          String       -- type of the parameter
>                   | Pbool
>  instance ShowHS ParamSpec where
>   showHSname a@(Aspc fid typ) = error ("(module FspecDef) should not showHSname the ParamSpec (Aspc): "++showHS "" a)
>   showHS indent (Aspc fid typ)
>    = "Aspc "++showHS "" fid++" "++show typ

>  class Fidentified a where
>    fsid :: a -> FSid
>    typ  :: a -> String


>  data FSid = FS_id String     -- Identifiers in the Functional Specification Language contain strings that do not contain any spaces.
>            | NoName           -- some identified objects have no name...
>  instance Identified FSid where
>   name (FS_id nm) = nm
>  instance ShowHS FSid where
>   showHSname a@(FS_id nm ) = haskellIdentifier nm 
>   showHS indent (FS_id nm) 
>     = "(FS_id " ++ show nm ++ ")"
>   showHS indent NoName = "NoName"

>  --pfixFSid :: String -> FSid -> FSid
>  --pfixFSid pfix (FS_id nm)= FS_id (pfix ++ nm)
>  --pfixFSid pfix NoName = FS_id pfix      -- Het is de vraag of dit nuttig is...

>  mkdefname fid = typ fid ++ "_" ++ showHSname (fsid fid)
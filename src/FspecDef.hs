  module FspecDef
         ( module Data.Fspec )

  where

   import ADLdataDef
   import CC_aux ( ShowHS (showHS,showHSname)
                 )
   import CommonClasses(Identified(name))
   import Auxiliaries(showL,haskellIdentifier)
   import Strings(chain)

   import Data.Fspec


   instance Identified Fspc where
     name fspc = name (fsid fspc)
   
   fspc_patterns :: Fspc -> Patterns
   fspc_patterns spec = themesOfPatterns (themes spec)     
   instance Fidentified Fspc where
    fsid    spec = fsfsid spec
    typ     a = "f_Ctx"

   instance ShowHS Fspc where
    showHSname fspec = typ fspec ++ "_" ++ showHSname (fsid fspec) --showHS "" (pfixFSid "f_Ctx_" (fsid fspec)) 
    showHS indent fspec
     = "Fspc"++showHS " " (fsid fspec)++
       (if null (themes   fspec) then " []" else indent++"{- themes:    -}  "++showL [showHSname t|t<-themes   fspec ])++
       (if null (datasets fspec) then " []" else indent++"{- datasets:  -}  "++showL [showHSname d|d<-datasets fspec ])++
       (if null (views    fspec) then " []" else indent++"{- views:     -}  "++showL [showHSname v|v<-views    fspec ])++
       (if null (vrules   fspec) then " []" else indent++"{- rules:     -}  "++showL [showHSname r|r<-vrules   fspec ])++
       (if null (vrels    fspec) then " []" else indent++"{- relations: -}  "++showL [showHSname r|r<-vrels    fspec ])++
       indent++" isa "++
       indent++"where"++
       indent++" isa = "++ showHS (indent ++ "       ") (isa fspec)++
       indent++" gE = genEq (typology isa)"++
       "\n>-- ***VIEWS***: " ++
       (if null (views    fspec ) then "" else concat [indent++" "++showHSname v++indent++"  = "++showHS (indent++"    ") v|v<- views    fspec ]++"\n")++
        "\n>-- ***RULES***: "++
       (if null (vrules   fspec ) then "" else concat [indent++" "++showHSname r++indent++"  = "++showHS (indent++"    ") r|r<- vrules   fspec ]++"\n")++
        "\n>-- ***DATASETS***: "++
       (if null (datasets fspec ) then "" else concat [indent++" "++showHSname d++indent++"  = "++showHS (indent++"    ") d|d<- datasets fspec ]++"\n")++
        "\n>-- ***THEMES***: "++
       (if null (themes fspec)    then "" else concat [indent++" "++showHSname t++" = "++showHS (indent++"    ") t|t<- themes   fspec ]++"\n")++
        "\n>-- ***DECLARATIONS OF RELATIONS***: "++
       (if null (vrels fspec)     then "" else concat [indent++" "++showHSname d++indent++"  = "++showHS (indent++"    ") d|d<- vrels fspec]++"\n")++
        "\n>-- ***PATTERNS***: "++
       (if null (fspc_patterns fspec) then "" else concat ["\n\n>  "++showHSname pat++" gE"++"\n>   = "++showHS "\n>     " pat|pat<-fspc_patterns fspec]++"\n")




   themesOfPatterns :: [Ftheme] -> [Pattern]
   themesOfPatterns themes = [ftpat tm | tm <-themes]


   instance Fidentified Ftheme where
    fsid theme = ftsid theme
    typ  f = "f_Thm"
   
   instance ShowHS Ftheme where
    showHSname ftheme = typ ftheme ++ "_" ++ showHSname (fsid ftheme) --showHS "" (pfixFSid "f_Theeeeeeeem_" (fsid ftheme))
    showHS indent ftheme
     = "Tspc ("++showHS "" (fsid ftheme)++")"
              ++indent++"     [ "++chain (indent++"     , ") [showHS (indent++"       ") u| u<-units(ftheme)]++indent++"     ]"
              ++indent++"("++showHSname (ftpat ftheme)++" gE)"






   instance Fidentified Dataset where
     fsid (DS c pths) = fsid c
     fsid (BR m) = fsid m
     typ  (DS c pths) = "f_DS"
     typ  (BR m) = "f_DS"

   instance Fidentified Morphism where
     fsid m = FS_id (name m++name (source m)++name(target m))  --Hier moet nog goed naar worden gekeken....
         where 
           source (Mph nm pos atts (a,b) _ s) = a
           source m = error ("FspecDef.lhs : Cannot evaluate the source expression of the current morphism (yet)")
           target (Mph nm pos atts (a,b) _ s) = b    
           target m = error ("FspecDef.lhs : Cannot evaluate the target expression of the current morphism (yet)")
     typ m  = "f_morph"
   instance Fidentified Concept where
     fsid c = FS_id (name c)
     typ m  = "f_cpt"
   instance Fidentified ObjectDef where
     fsid o = FS_id (name o)
     typ m  = "f_objdef"

   instance ShowHS Dataset where
    showHSname dset = typ dset ++ "_" ++ showHSname (fsid dset)
   -- showHSname dset@(BR m)      = showHS "" (pfixFSid "f_BR_" (fsid dset))
    showHS indent (DS c  [] ) = "DS ("++showHS "" c++") []"
    showHS indent (DS c pths) = "DS ("++showHS "" c++")"++indent++"   [ "++chain (indent++"   , ") [showHS (indent++"     ") pth| pth<-pths]++indent++"   ]"
    showHS indent (BR m     ) = "BR ("++showHS "" m++")"

   instance Eq Dataset where  -- opletten: een dataset moet één vast concept hebben waaraan het wordt herkend.
    DS c _ == DS d _ = c==d
    BR m   == BR m'  = m==m'
    _      == _      = False

   instance Fidentified Frule where
    fsid frul = FS_id (name (rule frul))   
    typ frul = "f_rule"
   rule:: Frule -> Rule
   rule (Frul r) = r

   instance Fidentified Fview where
    fsid fview = fsid (objectdef(fview))
    typ fview = "f_View"
   
   instance ShowHS Fview where
    showHSname fview = typ fview ++ "_" ++ showHSname (fsid fview) --showHS "" (pfixFSid "f_Obj_" (fsid fview))
    showHS indent fview
     = "Fview "
       ++ datasetSection
       ++ objdefSection
       ++ servicesSection
       ++ rulesSection
       ++indent++" -- Einde Fview "++showHSname (dataset fview)
        where
          datasetSection  = "("++ showHS "" (dataset fview)++")"
          objdefSection   = indent++"     ("++showHS (indent++"      ") (objectdef fview)++")"
          servicesSection = indent++"     [ "++chain (indent++"     , ") [showHS (indent++"       ") svc| svc<-services(fview)]++indent++"     ]"
          rulesSection    = indent++"     ["++chain ", " [showHSname fr| fr<-frules(fview)]++"]"




   instance ShowHS Frule where
    showHSname frul  = typ frul ++ "_" ++ showHSname (fsid frul) -- showHSname (rule frul)
    showHS indent (Frul r) = "Frul ("++showHS "" r++")"


   instance Fidentified Funit where
    fsid funit = fusid funit 
    typ funit = "f_Unit"
   instance ShowHS Funit where
    showHSname funit = typ funit ++ "_" ++ showHSname (fsid funit) 
    showHS indent funit
     = "Uspc "++showHS "" (fsid funit)
        ++" ("++showHSname (pattern funit)++" gE)"
       ++indent++"     [ "++chain (indent++"     , ") [showHS (indent++"       ") v| v<-viewDefs(funit)]++indent++"     ]"
       ++indent++"     [ "++chain (indent++"     , ") [showHS (indent++"       ") s| s<-servDefs(funit) ]++indent++"     ]"

   -- objDefs  funit = [o | (o,cs,rs)<-viewDefs(funit)]
   instance ShowHS FViewDef where
    showHSname fvd = error ("(module FspecDef) should not showHSname the FViewDef (Vdef): "++showHS "" fvd)
    showHS indent fvd
      = "Vdef ("++ showHS indent (vdobjdef fvd)++")" 
          ++indent++"     [ "++chain (indent++"     ") [showHS (indent++"       ") m| m<-vdmorphs fvd]++indent++"     ]"
          ++indent++"     [ "++chain (indent++"     ") [showtuple (indent++"       ") tup| tup<-vdExprRules fvd]++indent++"     ]"
        where
          showtuple :: String -> (Expression,Rule) -> String
          showtuple indent (expr,rule) = "( "++ showHS (indent++"  ") expr
                               ++indent++", "++ showHS (indent++"  ") rule





   instance Fidentified ServiceSpec where
      fsid (Sspc fid _ _ _ _ _ _ _) = fid  
      typ sspc = "f_svc"

   instance ShowHS ServiceSpec where
    showHSname sspc  = typ sspc ++ "_" ++ showHSname (fsid sspc) --"f_svc_"++showHS "" (fsid sspc)
    showHS indent sspc
      =            "Sspc " ++ showHS "" (fsid sspc)
       ++indent++"     [ " ++chain (indent++"     , ") (map (showHS (indent++"       ")) (sees sspc)  )++indent++"     ] -- these are the visible morphisms: <sees> "
       ++indent++"     [" ++(if null (changes sspc) then "]   -- no relations will be changed"  else " "++chain (indent++"     , ") (map (showHS (indent++"       ")) (changes sspc))++indent++"     ] -- these are the morphisms that may be altered: <changes> ")
       ++indent++"     [" ++(if null (input   sspc) then "]   -- there are no input parameters" else " "++chain "," (map (showHS "") (input sspc) )++"] -- these are the input parameters: <input>")
       ++indent++"     [" ++(if null (output  sspc) then "]   -- no output parameters"          else " "++chain "," (map (showHS "") (output sspc) )++"] -- these are the output parameters: <output> ")
       ++indent++"     [" ++(if null (rs      sspc) then "]   -- there are no rules"            else " "++chain (indent++"     , ") (map (showHS (indent++"       ")) (rs sspc) )++indent++"     ]")
       ++indent++"     [" ++(if null (pre     sspc) then "]   -- there are no preconditions"    else " "++chain (indent++"     , ") (map  show                        (pre sspc))++indent++"     ] -- preconditions")
       ++indent++"     [" ++(if null (post    sspc) then "]   -- there are no postconditions"   else " "++chain (indent++"     , ") (map  show                        (post sspc))++indent++"     ] -- postconditions")

   instance ShowHS ParamSpec where
    showHSname a@(Aspc fid typ) = error ("(module FspecDef) should not showHSname the ParamSpec (Aspc): "++showHS "" a)
    showHS indent (Aspc fid typ)
     = "Aspc "++showHS "" fid++" "++show typ

   class Fidentified a where
     fsid :: a -> FSid
     typ  :: a -> String


   instance Identified FSid where
    name (FS_id nm) = nm
   instance ShowHS FSid where
    showHSname a@(FS_id nm ) = haskellIdentifier nm 
    showHS indent (FS_id nm) 
      = "(FS_id " ++ show nm ++ ")"
    showHS indent NoName = "NoName"

   --pfixFSid :: String -> FSid -> FSid
   --pfixFSid pfix (FS_id nm)= FS_id (pfix ++ nm)
   --pfixFSid pfix NoName = FS_id pfix      -- Het is de vraag of dit nuttig is...

   mkdefname fid = typ fid ++ "_" ++ showHSname (fsid fid)

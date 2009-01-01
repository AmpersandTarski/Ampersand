module ShowHS (showHS,ShowHS()) -- TODO: ShowHS() mag eigenlijk niet meer ontsnappen uit deze module, maar wordt nu nog ergens misbruikt...
where

   import Typology
   import FspecDef
   import ADLdataDef
   import ADLdef(isAnything,isNothing,singleton
                , populations
                , patterns
                , rules
                , declarations
                , conceptDefs
                , keyDefs
                , attributes
                , extends
                , declaredRules
                , morlist
                , isa
                )
   import Collection ( (>-) )
   import CommonClasses (explain )             
   import UU_Scanner (Pos(Pos))
   import ShowADL


   class ShowHS a where
    showHSname :: a -> String
    showHS     :: String -> a -> String

   instance ShowHS a => ShowHS [a] where
    showHSname _ = error ("(module ShowHS) lists are anonymous with respect to showHS.")
    showHS indent = chain "\n".map (showHS indent)

   instance ShowHS a => ShowHS (Inheritance a) where
    showHSname i = error ("(module CC_aux) every inheritance is anonymous with respect to showHS. Detected at: "++ showHS "" i)
    showHS indent (Isa ts cs) = "Isa "++showL ["("++showHS "" g++","++showHS "" s++")"|(g,s)<-ts] ++indent++"    "++ showL (map (showHS "") cs)


   -- | The following is used to showHS for signs: (Concept, Concept)
   instance (ShowHS a , ShowHS b) => ShowHS (a,b) where
    showHSname _ = error ("(module ShowHS) Tuples of Concepts are anonymous with respect to showHS.")
    showHS indent (a,b) = "("++showHS indent a++","++showHS indent b++")"
    
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fspc                          ***
-- \***********************************************************************
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
       indent++" isa = "++ showHS (indent ++ "       ") (FspecDef.isa fspec)++
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


-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Ftheme                        ***
-- \***********************************************************************

   instance ShowHS Ftheme where
    showHSname ftheme = typ ftheme ++ "_" ++ showHSname (fsid ftheme) --showHS "" (pfixFSid "f_Theeeeeeeem_" (fsid ftheme))
    showHS indent ftheme
     = "Tspc ("++showHS "" (fsid ftheme)++")"
              ++indent++"     [ "++chain (indent++"     , ") [showHS (indent++"       ") u| u<-units(ftheme)]++indent++"     ]"
              ++indent++"("++showHSname (ftpat ftheme)++" gE)"
  
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Funit                         ***
-- \***********************************************************************

   instance ShowHS Funit where
    showHSname funit = typ funit ++ "_" ++ showHSname (fsid funit) 
    showHS indent funit
     = "Uspc "++showHS "" (fsid funit)
        ++" ("++showHSname (pattern funit)++" gE)"
       ++indent++"     [ "++chain (indent++"     , ") [showHS (indent++"       ") v| v<-viewDefs(funit)]++indent++"     ]"
       ++indent++"     [ "++chain (indent++"     , ") [showHS (indent++"       ") s| s<-servDefs(funit) ]++indent++"     ]"

     
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fview                         ***
-- \***********************************************************************

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

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Frule                         ***
-- \***********************************************************************

   instance ShowHS Frule where
    showHSname frul  = typ frul ++ "_" ++ showHSname (fsid frul) -- showHSname (rule frul)
    showHS indent (Frul r) = "Frul ("++showHS "" r++")"

   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FViewDef                      ***
-- \***********************************************************************

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

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ServiceSpec                   ***
-- \***********************************************************************
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
       
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Dataset                       ***
-- \***********************************************************************

   instance ShowHS Dataset where
    showHSname dset = typ dset ++ "_" ++ showHSname (fsid dset)
   -- showHSname dset@(BR m)      = showHS "" (pfixFSid "f_BR_" (fsid dset))
    showHS indent (DS c  [] ) = "DS ("++showHS "" c++") []"
    showHS indent (DS c pths) = "DS ("++showHS "" c++")"++indent++"   [ "++chain (indent++"   , ") [showHS (indent++"     ") pth| pth<-pths]++indent++"   ]"
    showHS indent (BR m     ) = "BR ("++showHS "" m++")"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ParamSpec                     ***
-- \***********************************************************************

   instance ShowHS ParamSpec where
    showHSname a@(Aspc fid typ) = error ("(module FspecDef) should not showHSname the ParamSpec (Aspc): "++showHS "" a)
    showHS indent (Aspc fid typ)
     = "Aspc "++showHS "" fid++" "++show typ

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FSid                          ***
-- \***********************************************************************

   instance ShowHS FSid where
    showHSname a@(FS_id nm ) = haskellIdentifier nm 
    showHS indent (FS_id nm) 
      = "(FS_id " ++ show nm ++ ")"
    showHS indent NoName = "NoName"



-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Architecture                  ***
-- \***********************************************************************

   instance ShowHS Architecture where
    showHSname _ = error ("(module CC_aux) an architecture is anonymous with respect to showHS.")
    showHS indent arch = concat (map (showHS indent) (archContexts arch))

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Context                       ***
-- \***********************************************************************
   
   instance ShowHS Context where
 -- TODO: showHS should generate valid Haskell code for the entire pattern. Right now, it doesn't
    showHSname context = "ctx_"++haskellIdentifier (name context)
    showHS indent context
     = "Ctx "++show (name context)++"   -- (Ctx nm on isa world pats rs ds cs ks os pops)"++
       indent++"       "++(if null on   then "[]" else showL [show x|x<-on])++
       (if null on   then " " else indent++"       ")++"isa [ {- world is left empty -} ]"++
       (if null pats then " []" else indent++"       "++showL [showHSname p++" gE"| p<-pats])++
       (if null rs   then " []" else indent++"       "++showL [showHSname r       | r<-rs  ])++
       (if null ds   then " []" else indent++"       "++showL [showHSname d       | d<-ds  ])++
       (if null cs   then " []" else indent++"       "++showL [showHSname c       | c<-cs  ])++
       (if null ks   then " []" else indent++"       "++showL ["key_"++name k     | k<-ks  ])++
       (if null os   then " []" else indent++"       "++showL [showHSname o       | o<-os  ])++
       (if null pops then " []" else indent++"       "++showL [showHSname p       | p<-pops])++
       indent++"where"++
       indent++" isa = "++showHS (indent++"       ") (ADLdef.isa context)++
       indent++" gE  = genEq (typology isa)"++
       (if null on   then "" else indent++" on  = "++showL [show x|x<-on]++"\n")++
       (if null os   then "" else concat [indent++" "++showHSname o++" = "++showHS "" o| o<-os]++"\n")++
       (if null rs   then "" else concat [indent++" "++showHSname r++" = "++showHS "" r| r<-rs]++"\n")++
       (if null ds   then "" else concat [indent++" "++showHSname d++" = "++showHS "" d| d<-ds]++"\n")++
       (if null pops then "" else concat [indent++" "++showHSname p++indent++"  = "++showHS (indent++"    ") p  |p<-populations context]++"\n")++
       (if null cs   then "" else concat [indent++" "++showHSname c++" = "++showHS "" c| c<-cs]++"\n")++
       (if null ks   then "" else concat [indent++" "++showHSname k++" = "++showHS "" k| k<-ks]++"\n")
    -- patterns will be shown in  (showHS indent Fspec)
       where pats = patterns context
             rs   = rules context
             ds   = declarations context>-declarations (patterns context)
             cs   = conceptDefs context>-conceptDefs (patterns context)
             ks   = keyDefs context
             os   = attributes context
             pops = populations context
             on   = extends context

   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Pattern                       ***
-- \***********************************************************************
 
   instance ShowHS Pattern where
 -- TODO: showHS should generate valid Haskell code for the entire pattern. Right now, it doesn't
    showHSname pat = "pat_"++haskellIdentifier (name pat)
    showHS indent pat@(Pat nm rs gen pss cs ks)
     = "Pat "++show (name pat)++
       (if null (declaredRules pat) then " []" else indent++"    [" ++chain          "    , "  [showHSname r              | r<-declaredRules pat] ++            "]")++
       (if null gen                 then " []" else indent++"    [ "++chain (indent++"    , ") [showHS (indent++"     ") g| g<-gen              ] ++indent++"    ]")++
       (if null (declarations pat)  then " []" else indent++"    [" ++chain          "    , "  [showHSname d              | d<-declarations  pat] ++            "]")++
       (if null (conceptDefs pat)   then " []" else indent++"    [" ++chain          "    , "  [showHSname c              | c<-conceptDefs   pat] ++            "]")++
       (if null ks                  then " []" else indent++"    [ "++chain (indent++"    , ") [showHS (indent++"     ") k| k<-ks               ] ++indent++"    ]")++
       indent++"where"++
       (if null (declarations pat)  then "" else concat [indent++" "++showHSname d ++" = "++ showHS (indent++"   ") d |d <-declarations  pat] )++
       (if null (declaredRules pat) then "" else concat [indent++" "++showHSname r ++" = "++ showHS (indent++"   ") r |r <-declaredRules pat] )++
       (if null (conceptDefs pat)   then "" else concat [indent++" "++showHSname cd++" = "++ showHS (indent++"   ") cd|cd<-conceptDefs   pat] )++
       (if null ks                  then "" else concat [indent++" "++showHSname k ++" = "++ showHS (indent++"   ") k |k <-ks               ] )
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Rule                          ***
-- \***********************************************************************

   instance ShowHS Rule where
    showHSname r = "rule"++show (nr r)
    showHS indent r@(Ru 'A' _ pos cons cpu expla sgn nr pn)
     = chain " " ["Ru","'A'",undef,"("++showHS "" pos++")","("++showHS "" (consequent r)++")",showL (map (showHS "") cpu),show(explain r),(showHS "" sgn),show nr,show pn]
       where undef = "(let undef = undef in error \"Fatal: antecedent is not defined in an 'A' rule\")"
    showHS indent r@(Ru c antc pos cons cpu expla sgn nr pn)
     = chain " " ["Ru","'"++[c]++"'","("++showHS "" antc++")","("++showHS "" pos++")","("++showHS "" (consequent r)++")","["++chain "," (map (showHS "") cpu)++"]",show(explain r),(showHS "" sgn),show nr,show pn]
    showHS indent r@(Sg pos rule expla sgn nr pn signal)
     = chain " " ["Sg","("++showHS "" pos++")","("++showHS "" rule++")",show expla,(showHS "" sgn),show nr,show pn,show signal]
    showHS indent r@(Gc pos m expr cpu sgn nr pn)
     = chain " " ["Gc","("++showHS "" pos++")","("++showHS "" m++")","("++showHS "" (consequent r)++")","["++chain "," (map (showHS "") cpu)++"]",(showHS "" sgn),show nr,show pn]
    showHS indent r = ""
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: KeyDef                        ***
-- \***********************************************************************

   instance ShowHS KeyDef where
    showHSname kd = "kDef_"++haskellIdentifier (name kd)
    showHS indent kd@(Kd pos lbl ctx ats)
     = "Kd ("++showHS "" pos++") "++show lbl++" ("++showHS "" ctx++")"
       ++indent++"[ "++chain (indent++", ") [showHS (indent++"  ") a|a<-ats]++indent++"]"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Population                    ***
-- \***********************************************************************

   instance ShowHS Population where
    showHSname (Popu m ps) = "pop_"++haskellIdentifier (name m++name (source m)++name (target m))
    showHS indent p@(Popu m ps)
     = "Popu ("++showHS "" m++")"++indent++"     [ "++chain (indent++"     , ") (map show ps)++indent++"     ]"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ObjectDef                     ***
-- \***********************************************************************

   instance ShowHS ObjectDef where
    showHSname obj = "oDef_"++haskellIdentifier (name obj)
    showHS indent obj 
     = "Obj "++show (name obj)++" ("++showHS "" (objpos obj)++")"++ctxStr++
       (if null (objats obj)
        then " []"
        else indent++"    [ "++chain (indent++"    , ") (map (showHS (indent++"      ")) (objats obj))++indent++"    ]")
     where ctxStr | length (morlist (objctx obj)) >1 = indent++"    ("++showHS (indent++"     ") (objctx obj)++indent++"    )"
                  | otherwise               = indent++"    ("++showHS "" (objctx obj)++")"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Expression                    ***
-- \***********************************************************************

   instance ShowHS Expression where
    showHSname e = error ("(module CC_aux) an expression is anonymous with respect to showHS. Detected at: "++ showADL e)
    showHS indent (Tm m)   = "Tm ("++showHS "" m++") "
    showHS indent (Tc f)   = showHS indent f
    showHS indent (F [])   = "F [] <Id>"
    showHS indent (Fd [])  = "Fd [] <nId>"
    showHS indent (Fu [])  = "Fu [] <False>"
    showHS indent (Fi [])  = "Fi [] <True>"
    showHS indent (F [t])  = "F ["++showHS (indent++"   ") t++"]"
    showHS indent (F ts)   = "F [ "++chain (indent++"  , ") [showHS (indent++"    ") t| t<-ts]++indent++"  ]"
    showHS indent (Fd [t]) = "Fd ["++showHS (indent++"    ") t++"]"
    showHS indent (Fd ts)  = "Fd [ "++chain (indent++"   , ") [showHS (indent++"     ") t| t<-ts]++indent++"   ]"
    showHS indent (Fu [f]) = "Fu ["++showHS (indent++"    ") f++"]"
    showHS indent (Fu fs)  = "Fu [ "++chain (indent++"   , ") [showHS (indent++"     ") f| f<-fs]++indent++"   ]"
    showHS indent (Fi [f]) = "Fi ["++showHS (indent++"    ") f++"]"
    showHS indent (Fi fs)  = "Fi [ "++chain (indent++"   , ") [showHS (indent++"     ") f| f<-fs]++indent++"   ]"
    showHS indent (K0 e)   = "K0 ("++showHS (indent++"    ") e++") "
    showHS indent (K1 e)   = "K1 ("++showHS (indent++"    ") e++") "
    showHS indent (Cp e)   = "Cp ("++showHS (indent++"    ") e++") "
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Gen                           ***
-- \***********************************************************************

   instance ShowHS Gen where
    showHSname g = error ("(module CC_aux: showHS) Illegal call to showHSname ("++showADL g++"). A GEN statement gets no definition in Haskell code.")
    showHS indent (G pos g s)  = "G ("++showHS "" pos++") ("++showHS "" s++") ("++showHS "" g++")"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Morphism                      ***
-- \***********************************************************************

   instance ShowHS Morphism where
    showHSname m = error ("(module CC_aux: showHS) Illegal call to showHSname ("++showADL m++"). A morphism gets no definition in Haskell code.")
    showHS indent (Mph nm pos atts sgn@(a,b) yin d)
     = "Mph "++show nm++" ("++showHS "" pos++") "++showL(map (showHS "") atts)++" "++(showHS "" sgn)++" "++show yin++" "++showHSname d
    showHS indent (I atts g s yin)
     = "I"++" "++showL(map (showHS "") atts)++" ("++showHS "" g++") ("++showHS "" s++") "++show yin
    showHS indent (V atts sgn)
     = "V"++" "++showL(map (showHS "") atts)++" ("++(showHS "" sgn)++")"
    showHS indent (Mp1 str sgn)
     = "Mp1"++" "++show str++" ("++showHS "" sgn++")"
   
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Declaration                   ***
-- \***********************************************************************

   instance ShowHS Declaration where
    showHSname d = "rel_"++haskellIdentifier (name d++name (source d)++name (target d))
    showHS indent d@(Sgn nm a b props prL prM prR cs expla pos nr sig)
     = "Sgn "++show nm++
               " ("++showHS "" a++") ("++showHS "" b++") "
               ++showL(map (showHS "") props)++" "
               ++show prL++" "++show prM++" "++show prR++" "
               ++show cs -- (if null cs then "[]" else "[[\"Content not shown\",\"\"]]")
               ++" "++show expla
               ++" ("++showHS "" pos++")"
               ++" "++show nr
               ++" "++show sig
    showHS indent (Isn g s)
     = "Isn ("++showHS "" g++") ("++showHS "" s++")"
    showHS indent (Iscompl g s)
     = "Iscompl ("++showHS "" g++") ("++showHS "" s++")"
    showHS indent (Vs g s)
     = "Vs ("++showHS "" g++") ("++showHS "" s++")"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ConceptDef                    ***
-- \***********************************************************************

   instance ShowHS ConceptDef where
    showHSname cd = "cDef_"++haskellIdentifier (name cd)
    showHS indent cd
     = " Cd ("++showHS "" (cdpos cd)++") "++show (name cd)++" "++show (cddef cd)++(if null (cdref cd) then "" else " "++show (cdref cd))
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Concept                       ***
-- \***********************************************************************

   instance ShowHS Concept where
    showHSname c = error ("(module CC_aux: showHS) Illegal call to showHSname ("++name c++"). A concept gets no definition in Haskell code.")
    showHS indent c |  isAnything c = "Anything"
                    |  isNothing  c = "NOthing"
                    |  singleton  c = "S"
                    |  otherwise    = "C "++show (name c) ++ " gE []"    -- contents not shown. 
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: AutType                       ***
-- \***********************************************************************
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Prop                          ***
-- \***********************************************************************
   
   instance ShowHS Prop where
    showHSname p = error ("(module CC_aux) should not showHS the name of multiplicities (Prop): "++showHS "" p)
    showHS indent Uni = "Uni"
    showHS indent Inj = "Inj"
    showHS indent Sur = "Sur"
    showHS indent Tot = "Tot"
    showHS indent Sym = "Sym"
    showHS indent Asy = "Asy"
    showHS indent Trn = "Trn"
    showHS indent Rfx = "Rfx"
    showHS indent Aut = "AUT"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FilePos                       ***
-- \***********************************************************************

   instance ShowHS FilePos where
    showHSname p = error ("(module CC_aux: showHS) Illegal call to showHSname ("++showHS "" p++"). A position gets no definition in Haskell code.")
    showHS indent (FilePos (fn,Pos l c,sym))
      = "FilePos ("++show fn++",Pos "++show l++" "++show c++","++show sym++")"




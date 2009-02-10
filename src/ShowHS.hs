module ShowHS (showHS,ShowHS()) -- TODO: ShowHS() mag eigenlijk niet meer ontsnappen uit deze module, maar wordt nu nog ergens misbruikt...
where

   import Typology
   import FspecDef
   import Adl
   import Adl.Rule(ruleType, antecedent,consequent)
   import Collection ( (>-),uni )
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
       "##1## \n"++
   --    (if null (themes   fspec) then " []" else indent++"{- themes:    -}  \n"++showL [showHSname t ++"\n"|t<-themes   fspec ])++
       "##2## \n"++
       (if null (datasets fspec) then " []" else indent++"{- datasets:  -}  "++showL [showHSname d|d<-datasets fspec ])++
       "##3## \n"++
       (if null (serviceS fspec) then " []" else indent++"{- serviceS:  -}  "++showL [showHSname s|s<-serviceS fspec ])++
       "##4## \n"++
       (if null (serviceG fspec) then " []" else indent++"{- serviceG:  -}  "++showL [showHSname s|s<-serviceG fspec ])++
       "##5## \n"++
       (if null (vrules   fspec) then " []" else indent++"{- rules:     -}  "++showL [showHSname r|r<-vrules   fspec ])++
       "##6## \n"++
       (if null (vrels    fspec) then " []" else indent++"{- relations: -}  "++showL [showHSname r|r<-vrels    fspec ])++
       "##7## \n"++
       indent++" isa "++
       "##8## \n"++
       indent++"where"++
       "##9## \n"++
       indent++" isa = "++ showHS (indent ++ "       ") (FspecDef.isa fspec)++
       "##10## \n"++
       indent++" gE = genEq (typology isa)"++
        "\n>-- ***THEMES***: "++
   --    (if null (themes fspec)    then "" else concat [indent++" "++showHSname t++" = "++showHS (indent++"    ") t|t<- themes   fspec ]++"\n")++
        "\n>-- ***DATASETS***: "++
       (if null (datasets fspec ) then "" else concat [indent++" "++showHSname d++indent++"  = "++showHS (indent++"    ") d|d<- datasets fspec ]++"\n")++
        "\n>-- ***Service definitions (both serviceS and serviceG, but each one exactly once. ***: "++
       (if null 
            (uni (serviceS fspec)  (serviceG fspec)) then "" 
             else concat [indent++" "++showHSname s++indent++"  = "++showHS (indent++"    ") s|s<- (uni (serviceS fspec)  (serviceG fspec)) ]++"\n")++
 
        "\n>-- ***RULES***: "++
       (if null (vrules   fspec ) then "" else concat [indent++" "++showHSname r++indent++"  = "++showHS (indent++"    ") r|r<- vrules   fspec ]++"\n")++
 
        "\n>-- ***DECLARATIONS OF RELATIONS***: "++
       (if null (vrels fspec)     then "" else concat [indent++" "++showHSname d++indent++"  = "++showHS (indent++"    ") d|d<- vrels fspec]++"\n")++
        "\n>-- ***PATTERNS***: "++
--       (if null (fspc_patterns fspec) then "" else concat ["\n\n>  "++showHSname pat++" gE"++"\n>   = "++showHS "\n>     " pat|pat<-fspc_patterns fspec]++
        "\n"


-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Ftheme                        ***
-- \***********************************************************************

   instance ShowHS Ftheme where
    showHSname ftheme = "ZIT HIER DE FOUT?? \n" -- typ ftheme ++ "_" ++ showHSname (fsid ftheme) --showHS "" (pfixFSid "f_Theeeeeeeem_" (fsid ftheme))
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
-- \*** Eigenschappen met betrekking tot: Fservice                         ***
-- \***********************************************************************

   instance ShowHS Fservice where
    showHSname fservice = typ fservice ++ "_" ++ showHSname (fsid fservice) --showHS "" (pfixFSid "f_Obj_" (fsid fservice))
    showHS indent fservice
     = "Fservice "
       ++ datasetSection
       ++ objdefSection
       ++ servicesSection
       ++ rulesSection
       ++indent++" -- Einde Fservice "++showHSname (dataset fservice)
        where
          datasetSection  = "("++ showHS "" (dataset fservice)++")"
          objdefSection   = indent++"     ("++showHS (indent++"      ") (objectdef fservice)++")"
          servicesSection = indent++"     [ "++chain (indent++"     , ") [showHS (indent++"       ") svc| svc<-methods(fservice)]++indent++"     ]"
          rulesSection    = indent++"     ["++chain ", " [showHSname fr| fr<-frules(fservice)]++"]"

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
--    showHS indent NoName = "NoName"



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
       indent++" isa = "++showHS (indent++"       ") (Adl.isa context)++
       indent++" gE  = genEq (typology isa)"++
       (if null on   then "" else indent++" on  = "++showL [show x|x<-on]++"\n")++
       (if null on   then "" else indent++" on  = "++showL [show x|x<-on]++"")++
       (if null os   then "" else concat [indent++" "++showHSname o++" = "++showHS "" o| o<-os]++"\n")++
       (if null rs   then "" else concat [indent++" "++showHSname r++" = "++showHS "" r| r<-rs]++"\n")++
       (if null ds   then "" else concat [indent++" "++showHSname d++" = "++showHS "" d| d<-ds]++"\n")++
       (if null pops then "" else concat [indent++" "++showHSname p++indent++"  = "++showHS (indent++"    ") p  |p<-populations context]++"\n")++
       (if null cs   then "" else concat [indent++" "++showHSname c++" = "++showHS "" c| c<-cs]++"\n")++
       (if null ks   then "" else concat [indent++" "++showHSname k++" = "++showHS "" k| k<-ks]++"\n")
    -- patterns will be shown in  (showHS indent Fspec)
       where pats = ctxpats context     --  patterns declared in this context
             rs   = rules context       --  rules declared in this context, except the signals
             ds   = ctxds context       --  declaration declared in this context, outside patterns
             cs   = ctxcs context       --  A list of concept definitions defined in this context, outside the scope of patterns
             ks   = ctxks context       --  A list of key definitions defined in this context, outside the scope of patterns
             os   = attributes context  --  A list of attributes defined in this context, outside the scope of patterns
             pops = populations context --  A list of populations defined in this context
             on   = extends context

   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Pattern                       ***
-- \***********************************************************************
 
   instance ShowHS Pattern where
 -- TODO: showHS should generate valid Haskell code for the entire pattern. Right now, it doesn't
    showHSname pat = "pat_"++haskellIdentifier (name pat)
    showHS indent pat
     = "Pat "++show (name pat)++
       (if null (declaredRules pat) then " []" else indent++"    [" ++chain          "    , "  [showHSname r              | r<-declaredRules pat] ++            "]")++
       (if null (ptgns pat)         then " []" else indent++"    [ "++chain (indent++"    , ") [showHS (indent++"     ") g| g<-ptgns         pat] ++indent++"    ]")++
       (if null (ptdcs pat)         then " []" else indent++"    [" ++chain          "    , "  [showHSname d              | d<-ptdcs         pat] ++            "]")++
       (if null (ptcds pat)         then " []" else indent++"    [" ++chain          "    , "  [showHSname c              | c<-ptcds         pat] ++            "]")++
       (if null (ptkds pat)         then " []" else indent++"    [ "++chain (indent++"    , ") [showHS (indent++"     ") k| k<-ptkds         pat] ++indent++"    ]")++
       indent++"where"++
       (if null (ptdcs pat)         then "" else concat [indent++" "++showHSname d ++" = "++ showHS (indent++"   ") d |d <-ptdcs         pat] )++
       (if null (declaredRules pat) then "" else concat [indent++" "++showHSname r ++" = "++ showHS (indent++"   ") r |r <-declaredRules pat] )++
       (if null (ptcds pat)         then "" else concat [indent++" "++showHSname cd++" = "++ showHS (indent++"   ") cd|cd<-ptcds         pat] )++
       (if null (ptkds pat)         then "" else concat [indent++" "++showHSname k ++" = "++ showHS (indent++"   ") k |k <-ptkds         pat] )
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Rule                          ***
-- \***********************************************************************

   instance ShowHS Rule where
    showHSname r = "rule"++show (runum r)
    showHS indent r = "## " ++ -- " ##SHOW r## "
       (case r of
              Ru{} -> "Ru{}"  ++ aap r
              Sg{} -> "Sg{}"  -- ruleType (srsig r)
              Gc{} -> "Gc{}"  -- Generalization
              Fr{} -> "Fr{}"  -- Automatic
            )++ showHS "" (ruleType r) ++ " ##" 
     where 
      aap r = 
       case r of
        Ru{} -> 
           chain " " ["Ru"
                     ,showHS "" (rrsrt r)
                     ,"\n *1 "
                     ,"("++showHS "" (antecedent r)++")"
                     ,"\n *2 "
                     ,"("++showHS "" (rrfps r)++")"
                     ,"\n *3 "
                     ,"("++showHS "" (consequent r)++")"
                     ,"\n *4 "
                     ,"["++chain "," (map (showHS "") (r_cpu r))++"]"
                     ,"\n *5 "
                     ,show(rrxpl r)
                     ,"\n *6 "
                     ,showHS "" (rrtyp r)
                     ,"\n *7 "
                     ,show (runum r)
                     ,show (r_pat r)
                     ]
--    showHS indent r@(Ru _ _ _ _ _ _ _ _ _)
--     = if rrsrt r==Truth
--       then chain " " ["Ru",showHS "" Truth,undef,"("++showHS "" (rrfps r)++")","("++showHS "" (consequent r)++")",showL (map (showHS "") (r_cpu r)),show (rrxpl r),(showHS "" (rrtyp r)),show (runum r),show (r_pat r)]
--       else chain " " ["Ru",showHS "" (rrsrt r),"("++showHS "" (antecedent r)++")","("++showHS "" (rrfps r)++")","("++showHS "" (consequent r)++")","["++chain "," (map (showHS "") (r_cpu r))++"]",show(rrxpl r),(showHS "" (rrtyp r)),show (runum r),show (r_pat r)]
--       where undef = "(let undef = undef in error \"Fatal: antecedent is not defined in an Truth rule\")"
--    showHS indent r@(Sg _ _ _ _ _ _ _)
--     = chain " " ["Sg","("++showHS "" (srfps r)++")","("++showHS "" (srsig r)++")",show (srxpl r),(showHS "" (srtyp r)),show (runum r),show (r_pat r),show (srrel r)]
--    showHS indent r@(Gc _ _ _ _ _ _ _)
--     = chain " " ["Gc","("++showHS "" (grfps r)++")","("++showHS "" (grspe r)++")","("++showHS "" (consequent r)++")","["++chain "," (map (showHS "") (r_cpu r))++"]",(showHS "" (grtyp r)),show (runum r),show (r_pat r)]
--    showHS indent r = ""
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: RuleType                      ***
-- \***********************************************************************
   instance ShowHS RuleType where
     showHSname rt = error "showHSname undefined for Type 'RuleType'"
     showHS indent Truth     = "Truth"
     showHS indent Equivalence    = "Equivalence"
     showHS indent Implication    = "Implication"
     showHS indent Generalization = "Generalization"
     showHS indent Automatic      = "Automatic"
   
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
        else indent++"    [ "++chain (indent++"    , ") (map (showHS (indent++"      ")) (objats obj))++indent++"    ]")++
       (if null (objstrs obj)
        then " []"
        else indent++show (objstrs obj))
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
    showHS indent c = case c of
                       C{} -> "C "++show (name c) ++ " gE []"    -- contents not shown.
                       _   -> name c
--    showHS indent c |  isAnything c = "Anything"
--                    |  isNothing  c = "NOthing"
--                    |  singleton  c = "S"
--                    |  otherwise    = "C "++show (name c) ++ " gE []"    -- contents not shown. 
   
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




{-# OPTIONS_GHC -Wall #-}
module ShowHS (showHS)
where

   import Typology(Inheritance(..))
   import FspecDef
   import Adl
   import UU_Scanner (Pos(Pos))
   import ShowADL(showADL) -- wenselijk voor foutmeldingen.


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
         = (chain (indent ++"    ") 
            ["Fspc{ fsfsid = " ++ showHS " " (fsid fspec)
                  ,", themes   = " ++ "["++chain "," (map (showHS "") (themes fspec))++"]" 
                  ,", datasets = "++ "[ "++chain indentA (map showHSname (datasets fspec))++indent++"                 "++"]" 
                  ,", serviceS = serviceS'"
                  ,", serviceG = serviceG'"
                  ,", services = services'"
                  ,", vrules   = " ++ "[ "++chain indentA (map showHSname (vrules fspec))++indent++"                 "++"]"
                  ,", vrels    = " ++ "[ "++chain indentA (map showHSname (vrels  fspec))++indent++"                 "++"]"
                  ,", fsisa = isa'"
                  ,"}" 
                    ]) ++   

       indent++"where"++
       indent++" isa' = "++ showHS (indent ++ "        ") (fsisa fspec)++
       indent++" gE = genEq (typology isa')"++
        "\n -- ***THEMES***: "++
       (if null (themes fspec)    then "" else concat [indent++" "++showHSname t++indent++"  = "++showHS (indent++"    ") t|t<- themes   fspec ]++"\n")++
        "\n -- ***DATASETS***: "++
       (if null (datasets fspec ) then "" else concat [indent++" "++showHSname d++indent++"  = "++showHS (indent++"    ") d|d<- datasets fspec ]++"\n")++
        "\n -- ***Services S***: "++
       indent++" serviceS' = "++"["++chain (indentB++",") (map (showHS indentB) (serviceS fspec))++"]"++
        "\n -- ***Services G***: "++
       indent++" serviceG' = "++"["++chain (indentB++",") (map (showHS indentB) (serviceG fspec))++"]"++
        "\n -- ***Services***: "++
       indent++" services' = "++"["++chain (indentB++",") (map (showHS indentB) (services fspec))++"]"++

-- WAAROM?  Stef, je had hier ooit de intentie om de verschillende soorten servicedefinities apart op te sommen. Echter, dan moeten ze wel te onderscheiden zijn. de namen moeten
--          dan ook netjes uniek worden gemaakt. Dat is nu nog niet het geval. Is dat nodig/ wenselijk? Waarom wel, waarom niet?

--        "\n -- ***Service definitions (both serviceS and serviceG, but each one exactly once. ***: "++  
--       (if null 
--            (uni (serviceS fspec)  (serviceG fspec)) then "" 
--             else concat [indent++" "++showHSname s++indent++"  = "++showHS (indent++"    ") s|s<- (uni (serviceS fspec)  (serviceG fspec)) ]++"\n")++
-- 
        "\n -- ***Declarations of RULES***: "++
       (if null (vrules   fspec ) then "" else concat [indent++" "++showHSname r++indent++"  = "++showHS (indent++"    ") r|r<- vrules   fspec ]++"\n")++
        "\n -- ***Declarations OF RELATIONS***: "++
       (if null (vrels fspec)     then "" else concat [indent++" "++showHSname d++indent++"  = "++showHS (indent++"    ") d|d<- vrels fspec]++"\n")++
--        "\n -- ***PATTERNS***: "++
----       (if null (fspc_patterns fspec) then "" else concat ["\n\n   "++showHSname pat++" gE"++"\n>   = "++showHS "\n>     " pat|pat<-fspc_patterns fspec]++
        "\n"
           where indentA = indent ++"                 , "
                 indentB = indent ++"              "
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Ftheme                        ***
-- \***********************************************************************

   instance ShowHS Ftheme where
    showHSname ftheme = typ ftheme ++ "_" ++ showHSname (fsid ftheme)
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
       ++indent++"     [ "++chain (indent++"     , ") [showHS (indent++"       ") v'| v'<-viewDefs(funit)]++indent++"     ]"
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
          ++indent++"     [ "++chain (indent++"     ") [showHS (indent++"       ") m'| m'<-vdmorphs fvd]++indent++"     ]"
          ++indent++"     [ "++chain (indent++"     ") [showtuple (indent++"       ") tup| tup<-vdExprRules fvd]++indent++"     ]"
        where
          showtuple :: String -> (Expression,Rule) -> String
          showtuple indent' (expr,rule) = "( "++ showHS (indent'++"  ") expr
                               ++indent'++", "++ showHS (indent'++"  ") rule

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
    showHSname a = error ("(module FspecDef) should not showHSname the ParamSpec (Aspc): "++showHS "" a)
    showHS _ (Aspc fid typ')
     = "Aspc "++showHS "" fid++" "++show typ'

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FSid                          ***
-- \***********************************************************************

   instance ShowHS FSid where
    showHSname (FS_id nm ) = haskellIdentifier nm 
    showHS _ (FS_id nm) 
      = "(FS_id " ++ show nm ++ ")"
--    showHS indent NoName = "NoName"



-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Architecture                  ***
-- \***********************************************************************

--   instance ShowHS Architecture where
--    showHSname _ = error ("(module CC_aux) an architecture is anonymous with respect to showHS.")
--    showHS indent arch = concat (map (showHS indent) (archContexts arch))

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Context                       ***
-- \***********************************************************************
   
--   instance ShowHS Context where
-- -- TODO: showHS should generate valid Haskell code for the entire pattern. Right now, it doesn't
--    showHSname context = "ctx_"++haskellIdentifier (name context)
--    showHS indent context
--     = "Ctx "++show (name context)++"   -- (Ctx nm on isa world pats rs ds cs ks os pops)"++
--       indent++"       "++(if null on   then "[]" else showL [show x|x<-on])++
--       (if null on   then " " else indent++"       ")++"isa [ {- world is left empty -} ]"++
--       (if null pats then " []" else indent++"       "++showL [showHSname p++" gE"| p<-pats])++
--       (if null rs   then " []" else indent++"       "++showL [showHSname r       | r<-rs  ])++
--       (if null ds   then " []" else indent++"       "++showL [showHSname d       | d<-ds  ])++
--       (if null cs   then " []" else indent++"       "++showL [showHSname c       | c<-cs  ])++
--       (if null ks   then " []" else indent++"       "++showL ["key_"++name k     | k<-ks  ])++
--       (if null os   then " []" else indent++"       "++showL [showHSname o       | o<-os  ])++
--       (if null pops then " []" else indent++"       "++showL [showHSname p       | p<-pops])++
--       indent++"where"++
--       indent++" isa = "++showHS (indent++"       ") (Adl.isa context)++
--       indent++" gE  = genEq (typology isa)"++
--       (if null on   then "" else indent++" on  = "++showL [show x|x<-on]++"\n")++
--       (if null on   then "" else indent++" on  = "++showL [show x|x<-on]++"")++
--       (if null os   then "" else concat [indent++" "++showHSname o++" = "++showHS "" o| o<-os]++"\n")++
--       (if null rs   then "" else concat [indent++" "++showHSname r++" = "++showHS "" r| r<-rs]++"\n")++
--       (if null ds   then "" else concat [indent++" "++showHSname d++" = "++showHS "" d| d<-ds]++"\n")++
--       (if null pops then "" else concat [indent++" "++showHSname p++indent++"  = "++showHS (indent++"    ") p  |p<-populations context]++"\n")++
--       (if null cs   then "" else concat [indent++" "++showHSname c++" = "++showHS "" c| c<-cs]++"\n")++
--       (if null ks   then "" else concat [indent++" "++showHSname k++" = "++showHS "" k| k<-ks]++"\n")
--    -- patterns will be shown in  (showHS indent Fspec)
--       where pats = ctxpats context     --  patterns declared in this context
--             rs   = rules context       --  rules declared in this context, except the signals
--             ds   = ctxds context       --  declaration declared in this context, outside patterns
--             cs   = ctxcs context       --  A list of concept definitions defined in this context, outside the scope of patterns
--             ks   = ctxks context       --  A list of key definitions defined in this context, outside the scope of patterns
--             os   = attributes context  --  A list of attributes defined in this context, outside the scope of patterns
--             pops = populations context --  A list of populations defined in this context
--             on   = extends context
--
   
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
    showHS indent r   
      = case r of
           Ru{} -> (chain newIndent 
                    ["Ru{ rrsrt = " ++ showHS "" (rrsrt r)
                        ,", rrant = " ++ "("++showHS "" (rrant r)++")"
                        ,", rrfps = " ++ "("++showHS "" (rrfps r)++")"
                        ,", rrcon = " ++ "("++showHS "" (rrcon r)++")"
                        ,", r_cpu = " ++ "["++chain "," (map (showHS "") (r_cpu r))++"]"
                        ,", rrxpl = " ++ show(rrxpl r)
                        ,", rrtyp = " ++ showHS "" (rrtyp r)
                        ,", runum = " ++ show (runum r)
                        ,", r_pat = " ++ show (r_pat r)
                    ])++"}"
           Sg{} -> (chain newIndent
                    ["Sg{ srfps = " ++ "("++showHS "" (srfps r)++")"
                        ,", srsig = " ++ "("++showHS "" (srsig r)++")"
                        ,", srxpl = " ++ show (srxpl r)
                        ,", srtyp = " ++ "("++showHS "" (srtyp r)++")"
                        ,", runum = " ++ show (runum r)
                        ,", r_pat = " ++ show (r_pat r)
                        ,", srrel = " ++ show(srrel r)
                    ])++"}"
           Gc{} -> (chain newIndent
                    ["Gc{ grfps = " ++ "("++showHS "" (grfps r)++")"
                        ,", grspe = " ++ "("++showHS "" (grspe r)++")"
                        ,", grgen = " ++ "("++showHS "" (grgen r)++")"
                        ,", r_cpu = " ++ "["++chain "," (map (showHS "") (r_cpu r))++"]"
                        ,", grtyp = " ++ showHS "" (grtyp r)
                        ,", runum = " ++ show (runum r)
                        ,", r_pat = " ++ show (r_pat r)
                    ])++"}"
           Fr{} -> (chain newIndent
                    ["Fr{ fraut = " ++ show (fraut r)
                        ,", frdec = " ++ showHS "" (frdec r)
                        ,", frcmp = " ++ "("++showHS "" (frcmp r)++")"
                        ,", r_pat = " ++ show (r_pat r)
                    ])++"}"
         where newIndent = indent ++ "  " 
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: RuleType                      ***
-- \***********************************************************************
   instance ShowHS RuleType where
     showHSname _ = error "showHSname undefined for Type 'RuleType'"
     showHS _ Truth          = "Truth"
     showHS _ Equivalence    = "Equivalence"
     showHS _ Implication    = "Implication"
     showHS _ Generalization = "Generalization"
     showHS _ Automatic      = "Automatic"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: KeyDef                        ***
-- \***********************************************************************

   instance ShowHS KeyDef where
    showHSname kd = "kDef_"++haskellIdentifier (name kd)
    showHS indent kd
     = "Kd ("++showHS "" (kdpos kd)++") "++show (kdlbl kd)++" ("++showHS "" (kdctx kd)++")"
       ++indent++"[ "++chain (indent++", ") [showHS (indent++"  ") a|a<-(kdats kd)]++indent++"]"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Population                    ***
-- \***********************************************************************

   instance ShowHS Population where
    showHSname pop = "pop_"++haskellIdentifier (name mph++name (source mph)++name (target mph))
        where mph = popm pop
    showHS indent pop
     = "Popu ("++showHS "" (popm pop)++")"++indent++"     [ "++chain (indent++"     , ") (map show (popps pop))++indent++"     ]"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ObjectDef                     ***
-- \***********************************************************************

   instance ShowHS ObjectDef where
    showHSname obj = "oDef_"++haskellIdentifier (name obj)
    showHS indent r 
     = (chain (indent++"   ") 
           ["Obj{ objnm = " ++ show(objnm r)
                ,", objpos = " ++ "("++showHS "" (objpos r)++")"
                ,", objctx = " ++ "("++showHS "" (objctx r)++")"
                ,", objats = " ++ "["++chain (indent ++ "              ,")
                                             (map (showHS (indent ++"               "))
                                                  (objats r))
                                ++"]"
                ,", objstrs = " ++ show(objstrs r)
           ])++"}"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Expression                    ***
-- \***********************************************************************

   instance ShowHS Expression where
    showHSname expr = error ("(module ShowHS) an expression is anonymous with respect to showHS. Detected at: "++ showADL expr)
    showHS _ (Tm m')   = "Tm ("++showHS "" m'++") "
    showHS indent (Tc f)   = showHS indent f
    showHS _ (F [])   = "F [] <Id>"
    showHS _ (Fd [])  = "Fd [] <nId>"
    showHS _ (Fu [])  = "Fu [] <False>"
    showHS _ (Fi [])  = "Fi [] <True>"
    showHS indent (F [t])  = "F ["++showHS (indent++"   ") t++"]"
    showHS indent (F ts)   = "F [ "++chain (indent++"  , ") [showHS (indent++"    ") t| t<-ts]++indent++"  ]"
    showHS indent (Fd [t]) = "Fd ["++showHS (indent++"    ") t++"]"
    showHS indent (Fd ts)  = "Fd [ "++chain (indent++"   , ") [showHS (indent++"     ") t| t<-ts]++indent++"   ]"
    showHS indent (Fu [f]) = "Fu ["++showHS (indent++"    ") f++"]"
    showHS indent (Fu fs)  = "Fu [ "++chain (indent++"   , ") [showHS (indent++"     ") f| f<-fs]++indent++"   ]"
    showHS indent (Fi [f]) = "Fi ["++showHS (indent++"    ") f++"]"
    showHS indent (Fi fs)  = "Fi [ "++chain (indent++"   , ") [showHS (indent++"     ") f| f<-fs]++indent++"   ]"
    showHS indent (K0 e')   = "K0 ("++showHS (indent++"    ") e'++") "
    showHS indent (K1 e')   = "K1 ("++showHS (indent++"    ") e'++") "
    showHS indent (Cp e')   = "Cp ("++showHS (indent++"    ") e'++") "

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Gen                           ***
-- \***********************************************************************

   instance ShowHS Gen where
    showHSname g = error ("(module ShowHS) Illegal call to showHSname ("++showADL g++"). A GEN statement gets no definition in Haskell code.")
    showHS _ gen = "G ("++showHS "" (genfp gen)++") ("++showHS "" (gengen gen)++") ("++showHS "" (genspc gen)++")"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Morphism                      ***
-- \***********************************************************************

   instance ShowHS Morphism where
    showHSname mph = error ("(module CC_aux: showHS) Illegal call to showHSname ("++showADL mph++"). A morphism gets no definition in Haskell code.")
    showHS _ mph 
       = case mph of
            Mph{} -> "Mph "++show (mphnm mph)++" "++showPos++" "++showAtts
                         ++" "++showSgn++" "++show (mphyin mph)++" "++showHSname (mphdcl mph)
            I{}   -> "I "++showAtts++" "++showGen++" "++showSpc++" "++show (mphyin mph)
            V{}   -> "V "++showAtts++" "++showSgn
            Mp1{} -> "Mp1 "++show (mph1val mph)++" ("++showHS "" (mph1typ mph)++")"
           where showPos  = "("++showHS "" (mphpos mph)++")"
                 showAtts = showL(map (showHS "") (mphats mph))
                 showGen  = "("++showHS "" (mphgen mph)++")"
                 showSpc  = "("++showHS "" (mphspc mph)++")"
                 showSgn  = "("++showHS "" (mphtyp mph)++")"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Declaration                   ***
-- \***********************************************************************

   instance ShowHS Declaration where
    showHSname d = "rel_"++haskellIdentifier (name d++name (source d)++name (target d))
    showHS indent d 
       = case d of 
          Sgn{}     -> (chain newIndent
                        ["Sgn{ decnm   = " ++ show (decnm d)
                           ,", desrc   = " ++ showHS "" (desrc d)
                           ,", detgt   = " ++ showHS "" (detgt d)
                           ,", decprps = " ++ showL(map (showHS "") (decprps d))
                           ,", decprL  = " ++ show (decprL d)
                           ,", decprM  = " ++ show (decprM d)
                           ,", decprR  = " ++ show (decprR d)
                           ,", decpopu = " ++ show (decpopu d)
                           ,", decexpl = " ++ show (decexpl d)
                           ,", decfpos = " ++ showHS "" (decfpos d)
                           ,", decid   = " ++ show (decid d)
                           ,", deciss  = " ++ show (deciss d)
                        ])++"}"
          Isn{}     -> (chain newIndent
                        ["Isn{ degen   = " ++ showHS "" (degen d)
                           ,", despc   = " ++ showHS "" (despc d)
                        ])++"}"
          Iscompl{} -> (chain newIndent
                        ["Isn{ degen   = " ++ showHS "" (degen d)
                           ,", despc   = " ++ showHS "" (despc d)
                        ])++"}"
          Vs{}      -> (chain newIndent
                        ["Isn{ degen   = " ++ showHS "" (degen d)
                           ,", despc   = " ++ showHS "" (despc d)
                        ])++"}"
       where newIndent = indent ++ "   "
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ConceptDef                    ***
-- \***********************************************************************

   instance ShowHS ConceptDef where
    showHSname cd = "cDef_"++haskellIdentifier (name cd)
    showHS _ cd
     = " Cd ("++showHS "" (cdpos cd)++") "++show (name cd)++" "++show (cddef cd)++(if null (cdref cd) then "" else " "++show (cdref cd))
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Concept                       ***
-- \***********************************************************************

   instance ShowHS Concept where
    showHSname c = error ("(module CC_aux: showHS) Illegal call to showHSname ("++name c++"). A concept gets no definition in Haskell code.")
    showHS _ c = case c of
                       C{}      -> "C "++show (name c) ++ " gE []"    -- contents not shown.
                       S        -> "S "
                       Anything -> "Anything "
                       NOthing  -> "NOthing "
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: AutType                       ***
-- \***********************************************************************
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Prop                          ***
-- \***********************************************************************
   
   instance ShowHS Prop where
    showHSname p = error ("(module CC_aux) should not showHS the name of multiplicities (Prop): "++showHS "" p)
    showHS _ Uni = "Uni"
    showHS _ Inj = "Inj"
    showHS _ Sur = "Sur"
    showHS _ Tot = "Tot"
    showHS _ Sym = "Sym"
    showHS _ Asy = "Asy"
    showHS _ Trn = "Trn"
    showHS _ Rfx = "Rfx"
    showHS _ Aut = "AUT"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FilePos                       ***
-- \***********************************************************************

   instance ShowHS FilePos where
    showHSname p = error ("(module CC_aux: showHS) Illegal call to showHSname ("++showHS "" p++"). A position gets no definition in Haskell code.")
    showHS _ (FilePos (fn,Pos l c,sym))
      = "FilePos ("++show fn++",Pos "++show l++" "++show c++","++show sym++")"




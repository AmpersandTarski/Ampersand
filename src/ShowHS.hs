{-# OPTIONS_GHC -Wall #-}
module ShowHS (showHS,ShowHS(),fSpec2Haskell)
where

   import Typology              (Inheritance(..))
   import Data.Plug
   import FspecDef              (Fspc(..)
                                ,Fservice(..)
                                ,FSid(..)
                                ,Fidentified(..))
   import Strings               (chain)
   import Adl
   import UU_Scanner            (Pos(..))
   import ShowADL               (showADL) -- wenselijk voor foutmeldingen.
   import Auxiliaries           (haskellIdentifier,showL)
   import Options
   import Version               (versionbanner)
   
   fSpec2Haskell :: Fspc -> Options -> String
   fSpec2Haskell fSpec flags
           = "{-# OPTIONS_GHC -Wall #-}"
             ++"\n{-Generated code by "++versionbanner++" at "++show (genTime flags)++"-}"
             ++"\nmodule Main where"
             ++"\n  import UU_Scanner"
             ++"\n  --import Classification"
             ++"\n  import Typology"
             ++"\n  import Adl"
             ++"\n  import ShowHS (showHS)"
             ++"\n  import Data.Fspec"
             ++"\n  import Data.Plug"
             ++"\n"
             ++"\n  main :: IO ()"
             ++"\n  main = putStr (showHS \"\\n  \" fSpec_"++baseName flags++")"
             ++"\n"
             ++"\n  fSpec_"++baseName flags++" :: Fspc"
             ++"\n  fSpec_"++baseName flags++"\n   = "++showHS "\n     " fSpec


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
-- \*** Eigenschappen met betrekking tot: Plug                          ***
-- \***********************************************************************
   instance ShowHS Plug where
    showHSname plug = "plug_"++haskellIdentifier (name plug)
    showHS indent plug   
      = case plug of
           PlugSql{} -> (chain indent 
                          ["PlugSql{ fields = " ++ "[ "++
                                              chain (indent++"                  , ") (map (showHS (indent++"                    ")) (fields plug))++
                                              indent++"                  ]"
                          ,"       , plname = " ++ show (plname plug)
                          ,"       }"
                          ])
           PlugPhp{} -> (chain indent 
                          ["PlugPhp{ args = " ++ "[ "++
                                            chain (indent++"                , ") [ "("++show i++","++showHS "" a++")"|(i,a)<-args plug]++
                                            "                ]"
                          ,"       , returns  = " ++ showHS "" (returns plug)
                          ,"       , function = " ++ showHS "" (function plug)
                          ,"       , phpfile  = " ++ show (phpfile plug)
                          ,"       , plname   = " ++ show (plname  plug)
                          ,"       }"
                          ])

   instance ShowHS PhpValue where
    showHSname _ = error ("(module ShowHS) PhpValue is anonymous with respect to showHS.")
    showHS _ phpVal
      = case phpVal of
           PhpNull{}   -> "PhpNull"
           PhpObject{} -> "PhpObject{ objectdf = " ++ showHSname (objectdf phpVal) ++ ", phptype  = " ++ showHS "" (phptype phpVal) ++ "}"

   instance ShowHS PhpType where
    showHSname _ = error ("(module ShowHS) PhpType is anonymous with respect to showHS.")
    showHS indent PhpString = indent++"PhpString"
    showHS indent PhpInt    = indent++"PhpInt"
    showHS indent PhpFloat  = indent++"PhpFloat"
    showHS indent PhpArray  = indent++"PhpArray"

   instance ShowHS PhpReturn where
    showHSname _ = error ("(module ShowHS) PhpReturn is anonymous with respect to showHS.")
    showHS indent ret = indent++"PhpReturn {retval = "++showHS indent (retval ret)++"}"

   instance ShowHS PhpAction where
    showHSname _ = error ("(module ShowHS) PhpAction is anonymous with respect to showHS.")
    showHS indent act
      = (chain (indent ++"    ") 
          [ "PhpAction{ action = " ++ showHS "" (action act)
          , "         , on     = " ++ "["++chain ", " (map (showHS "") (on act))++"]"
          , "         }"
          ])

   instance ShowHS ActionType where
    showHSname _ = error ("(module ShowHS) ActionType is anonymous with respect to showHS.")
    showHS indent Create = indent++"Create"
    showHS indent Read   = indent++"Read"
    showHS indent Update = indent++"Update"
    showHS indent Delete = indent++"Delete"

   instance ShowHS SqlField where
    showHSname _ = error ("(module ShowHS) SqlField is anonymous with respect to showHS.")
    showHS indent sqFd
      = (chain indent
          [ "Fld { fldname = " ++ show (fldname sqFd)
          , "    , fldexpr = " ++ showHS "" (fldexpr sqFd)
          , "    , fldtype = " ++ showHS "" (fldtype sqFd)
          , "    , fldnull = " ++ show (fldnull sqFd) -- can there be empty field-values?
          , "    , flduniq = " ++ show (flduniq sqFd) -- are all field-values unique?
          , "    , fldauto = " ++ show (fldauto sqFd) -- is the field auto increment?
          , "    }"
          ])

   instance ShowHS SqlType where
    showHSname _ = error ("(module ShowHS) SqlType is anonymous with respect to showHS.")
    showHS indent (SQLChar i)    = indent++"SQLChar   "++show i
    showHS indent SQLBlob        = indent++"SQLBlob   "
    showHS indent SQLPass        = indent++"SQLPass   "
    showHS indent SQLSingle      = indent++"SQLSingle "
    showHS indent SQLDouble      = indent++"SQLDouble "
    showHS indent SQLText        = indent++"SQLText   "
    showHS indent (SQLuInt i)    = indent++"SQLuInt   "++show i
    showHS indent (SQLsInt i)    = indent++"SQLsInt   "++show i
    showHS indent SQLId          = indent++"SQLId     "
    showHS indent (SQLVarchar i) = indent++"SQLVarchar "++show i
    showHS indent (SQLEval strs) = indent++"SQLEval ["++chain ", " (map show strs)++"]"
    showHS indent SQLBool        = indent++"SQLBool   "

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fspc                          ***
-- \***********************************************************************
   instance ShowHS Fspc where
    showHSname fspec = typ fspec ++ "_" ++ showHSname (fsid fspec) --showHS "" (pfixFSid "f_Ctx_" (fsid fspec)) 
    showHS indent fspec
     = chain (indent ++"    ") 
            ["Fspc{ fsfsid = " ++ showHS " " (fsid fspec)
                  ,", themes   = " ++ "[]" -- SJ: tijdelijk om themes te omzeilen zolang ze nog niet werken.
                                           -- TODO: add an instance declaration for (ShowHS Data.Fspec.FTheme)
                                   -- "["++chain "," (map (showHS "") (themes fspec))++"]" 
                  ,", datasets = "++ "[ "++chain (indentA++", ") (map showHSname (datasets fspec))++indentA++"]" 
                  ,", vplugs   = "++ "[ "++chain (indentA++", ") (map showHSname (vplugs fspec))++indentA++"]"
                  ,", plugs    = "++ "[ "++chain (indentA++", ") (map showHSname (plugs fspec))++indentA++"]"
                  ,", serviceS = serviceS'"
                  ,", serviceG = serviceG'"
                  ,", services = services'"
                  ,", vrules   = " ++ "[ "++chain (indentA++", ") (map showHSname (vrules fspec))++indentA++"]"
                  ,", vrels    = " ++ "[ "++chain (indentA++", ") (map showHSname (vrels  fspec))++indentA++"]"
                  ,", fsisa = isa'"
                  ,"}" 
                  ] ++   
       indent++"where"++
       indent++" isa' = "++ showHS (indent ++ "        ") (fsisa fspec)++
       indent++" gE = genEq (typology isa')"++
   -- SJ: tijdelijk om themes te omzeilen zolang ze nog niet werken.
   --   "\n -- ***THEMES***: "++
   --  (if null (themes fspec)    then "" else concat [indent++" "++showHSname t++indent++"  = "++showHS (indent++"    ") t|t<- themes fspec ]++"\n")++
        
       (if null (datasets fspec ) then "" else "\n -- ***DATASETS***: "++concat [indent++" "++showHSname d++indent++"  = "++showHS (indent++"    ") d|d<- datasets fspec ]++"\n")++
        
       (if null (plugs fspec ) then "" else "\n -- ***PLUGS***: "++concat [indent++" "++showHSname p++indent++"  = "++showHS (indent++"    ") p|p<-plugs fspec ]++"\n")++
        
        "\n -- ***Services S***: "++
       indent++" serviceS' = "++"[ "++chain (indentB++", ") (map (showHS indentB) (serviceS fspec))++indentB++"]"++
        "\n -- ***Services G***: "++
       indent++" serviceG' = "++"[ "++chain (indentB++", ") (map (showHS indentB) (serviceG fspec))++indentB++"]"++
        "\n -- ***Services***: "++
       indent++" services' = "++"[ "++chain (indentB++", ") (map (showHS indentB) (FspecDef.services fspec))++indentB++"]"++

-- WAAROM?  Stef, je had hier ooit de intentie om de verschillende soorten servicedefinities apart op te sommen. Echter, dan moeten ze wel te onderscheiden zijn. de namen moeten
--          dan ook netjes uniek worden gemaakt. Dat is nu nog niet het geval. Is dat nodig/ wenselijk? Waarom wel, waarom niet?
-- DAAROM!  Een ADL-engineer besteedt veel tijd om vanuit een kennismodel (lees: een graaf met concepten en relaties)
--          alle services met de hand te verzinnen.
--          Je kunt natuurlijk ook een services-generator aan het werk zetten, die een aantal services klaarzet bij wijze
--          van steiger (scaffold). Dat bespaart een hoop werk. De functie serviceG is zo'n generator.
--          Door de gegenereerde services af te drukken, kun je dus heel snel ADL-sourcecode maken met correct-vertaalbare services.
--          Heb je eenmaal een goed werkend pakket services, dan wil je wellicht alleen de door jezelf gespecificeerde services
--          gebruiken. Dat gebeurt in serviceS.
--          De functie services is oude meuk; het definieert CRUD-services voor alle concepten, geloof ik.

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
           where indentA = indent ++"                 "
                 indentB = indent ++"              "
  
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fservice                         ***
-- \***********************************************************************

   instance ShowHS Fservice where
    showHSname fservice = typ fservice ++ "_" ++ showHSname (fsid fservice) --showHS "" (pfixFSid "f_Obj_" (fsid fservice))
    showHS indent fservice
     = "Fservice "
       ++ objdefSection
       ++indent++" -- Einde Fservice "++showHSname (objectdef fservice)
        where
          objdefSection   = indent++"     ("++showHS (indent++"      ") (objectdef fservice)++")"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FSid                          ***
-- \***********************************************************************

   instance ShowHS FSid where
    showHSname (FS_id nm ) = haskellIdentifier nm 
    showHS _ (FS_id nm) 
      = "(FS_id " ++ show nm ++ ")"

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
                    ["Fr{ frdec = " ++ showHS "" (frdec r)
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
    showHS _ Nowhere
      = "Nowhere"



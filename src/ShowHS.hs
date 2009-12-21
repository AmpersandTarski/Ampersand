{-# OPTIONS_GHC -Wall #-}
module ShowHS (showHS,ShowHS(),fSpec2Haskell)
where

   import Typology              (Inheritance(..))
   import Data.Plug
   import Data.Fspec
   import Strings               (chain)
   import Adl
   import UU_Scanner            (Pos(..))
   import ShowADL               (showADL) -- wenselijk voor foutmeldingen.
   import Auxiliaries           (haskellIdentifier,showL)
   import Options hiding (services)
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
             ++"\n  import ShowHS (showHS options)"
             ++"\n  import Data.Fspec"
             ++"\n  import Data.Plug"
             ++"\n"
             ++"\n  main :: IO ()"
             ++"\n  main = putStr (showHS options \"\\n  \" fSpec_"++baseName flags++")"
             ++"\n"
             ++"\n  fSpec_"++baseName flags++" :: Fspc"
             ++"\n  fSpec_"++baseName flags++"\n   = "++showHS flags "\n     " fSpec


   wrap :: String->String->(String->a->String)->[a]->String
   wrap initStr _      _ []  = initStr++"[]"
   wrap initStr indent f [x] = initStr++"[ "++f (indent++"  ") x++" ]"
   wrap initStr indent f xs  = initStr++"[ "++chain (indent++", ") [f (indent++"  ") x| x<-xs]++indent++"]"

   class ShowHS a where
    showHSname :: a -> String
    showHS     :: Options -> String -> a -> String

   instance ShowHS a => ShowHS [a] where
    showHSname xs = "["++chain "," (map showHSname xs)++"]"
    showHS options indent = chain "\n".map (showHS options indent)

   instance ShowHS a => ShowHS (Maybe a) where
    showHSname Nothing  = "Nothing"
    showHSname (Just x) = showHSname x
    showHS _ _ Nothing  = "Nothing"
    showHS options indent (Just x) = showHS options indent x

   instance ShowHS a => ShowHS (Inheritance a) where
    showHSname _ = error ("!Fatal (module ShowHS 60): every inheritance is anonymous with respect to showHS options.")
    showHS options indent (Isa ts cs) = "Isa "++showL ["("++showHS options "" g++","++showHS options "" s++")"|(g,s)<-ts] ++indent++"    "++ showL (map (showHS options "") cs)


   -- | The following is used to showHS options for signs: (Concept, Concept)
   instance (ShowHS a , ShowHS b) => ShowHS (a,b) where
    showHSname _ = error ("!Fatal (module ShowHS 66): Tuples of Concepts are anonymous with respect to showHS options.")
    showHS options indent (a,b) = "("++showHS options indent a++","++showHS options indent b++")"
    
   

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Plug                          ***
-- \***********************************************************************
   instance ShowHS Plug where
    showHSname plug = "plug_"++haskellIdentifier (name plug)
    showHS options indent plug   
      = case plug of
           PlugSql{} -> (chain indent 
                          ["PlugSql{ fields = " ++ "[ "++
                                              chain (indent++"                  , ") (map (showHS options (indent++"                    ")) (fields plug))++
                                              indent++"                  ]"
                          ,"       , plname = " ++ (show.haskellIdentifier.plname) plug
                          ,"       , plfpa  = " ++ showHS options "" (plfpa plug)
                          ,"       }"
                          ])
           PlugPhp{} -> (chain indent 
                          ["PlugPhp{ args = " ++ "[ "++
                                            chain (indent++"                , ") [ "("++show i++","++showHS options "" a++")"|(i,a)<-args plug]++
                                            "                ]"
                          ,"       , returns  = " ++ showHS options "" (returns plug)
                          ,"       , function = " ++ showHS options "" (function plug)
                          ,"       , phpfile  = " ++ show (phpfile plug)
                          ,"       , plname   = " ++ show (plname  plug)
                          ,"       , plfpa    = " ++ showHS options "" (plfpa plug)
                          ,"       }"
                          ])

   instance ShowHS PhpValue where
    showHSname _ = error ("!Fatal (module ShowHS): PhpValue is anonymous with respect to showHS options.")
    showHS options _ phpVal
      = case phpVal of
           PhpNull{}   -> "PhpNull"
           PhpObject{} -> "PhpObject{ objectdf = " ++ showHSname (objectdf phpVal) ++ ", phptype  = " ++ showHS options "" (phptype phpVal) ++ "}"

   instance ShowHS PhpType where
    showHSname _ = error ("!Fatal (module ShowHS): PhpType is anonymous with respect to showHS options.")
    showHS _ indent PhpString = indent++"PhpString"
    showHS _ indent PhpInt    = indent++"PhpInt"
    showHS _ indent PhpFloat  = indent++"PhpFloat"
    showHS _ indent PhpArray  = indent++"PhpArray"

   instance ShowHS PhpReturn where
    showHSname _ = error ("!Fatal (module ShowHS): PhpReturn is anonymous with respect to showHS options.")
    showHS options indent ret = indent++"PhpReturn {retval = "++showHS options indent (retval ret)++"}"

   instance ShowHS PhpAction where
    showHSname _ = error ("!Fatal (module ShowHS): PhpAction is anonymous with respect to showHS options.")
    showHS options indent act
      = (chain (indent ++"    ") 
          [ "PhpAction { action = " ++ showHS options "" (action act)
          , "          , on     = " ++ "["++chain ", " (map (showHS options "") (on act))++"]"
          , "          }"
          ])

   instance ShowHS ECArule where
    showHSname r = "ecaRule"++show (ecaNum r)
    showHS options indent r   
      = "ECA (" ++ showHS options "" (ecaTriggr r)++")" ++
        indent++"    (" ++ showHS options (indent++"     ")  (ecaDelta r)++")"++
        indent++"    (" ++ showHS options (indent++"     ")  (ecaAction r)++indent++"    )" ++
        indent++show (ecaNum r)

   instance ShowHS Event where
    showHSname _ = error ("!Fatal (module ShowHS): \"Event\" is anonymous with respect to showHS options.")
    showHS options indent e   
      = if take 1 indent == "\n"
        then "On (" ++ show (eSrt e)++")" ++ indent++"   (" ++ showHS options (indent++"    ") (eMhp e)++indent++"   )"
        else "On (" ++ show (eSrt e)++") (" ++ showHS options "" (eMhp e)++")"

   instance ShowHS PAclause where
    showHSname _ = error ("!Fatal (module ShowHS): \"PAclause\" is anonymous with respect to showHS options.")
    showHS options indent p   
      = case p of
           Chc{} -> wrap "Chc " (indent ++"    ") (showHS options) (paCls p)++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
           All{} -> "All [ "++chain (indent++"    , ") (map (showHS options (indent++"      ")) (paCls p))++indent++"    ]"++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
           Do{}  ->  "Do "++show (paSrt p)++ " ("++showHS options (indent++"        ") (paTo p)++indent++"       )"++
                            indent++"       ("++showHS options (indent++"        ") (paDelta p)++indent++"       )"++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
           New{} -> "New ("++showHS options "" (paCpt p)++")"++
                    indent++"    (\\x->"++showHS options (indent++"        ") (paCl p "x")++indent++"    )"++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
           Rmv{} -> "Rmv ("++showHS options "" (paCpt p)++")"++
                    indent++"    (\\x->"++showHS options (indent++"        ") (paCl p "x")++indent++"    )"++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
           Sel{} -> "Sel ("++showHS options "" (paCpt p)++")"++
                    indent++"    ( "++showHS options (indent++"      ") (paExp p)++indent++"    )"++
                    indent++"    (\\x->"++showHS options (indent++"        ") (paCl p "x")++indent++"    )"++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
           Nop{} -> "Nop "++wrap "" (indent ++"    ") showMotiv ms
           Blk{} -> "Blk "++wrap "" (indent ++"    ") showMotiv ms
           Dry{} -> "Dry "++wrap "" (indent ++"    ") showMotiv ms
        where ms = paMotiv p
              showMotiv ind (conj,rs) = "("++showHS options ind conj++", "++showHSname rs++")"

   instance ShowHS ActionType where
    showHSname _ = error ("!Fatal (module ShowHS 166): \"ActionType\" is anonymous with respect to showHS options.")
    showHS _ indent Create = indent++"Create"
    showHS _ indent Read   = indent++"Read"
    showHS _ indent Update = indent++"Update"
    showHS _ indent Delete = indent++"Delete"

   instance ShowHS SqlField where
    showHSname _ = error ("!Fatal (module ShowHS 173): \"SqlField\" is anonymous with respect to showHS options.")
    showHS options indent sqFd
      = (chain indent
          [ "Fld { fldname = " ++ show (fldname sqFd)
          , "    , fldexpr = " ++ showHS options "" (fldexpr sqFd)
          , "    , fldtype = " ++ showHS options "" (fldtype sqFd)
          , "    , fldnull = " ++ show (fldnull sqFd) -- can there be empty field-values?
          , "    , flduniq = " ++ show (flduniq sqFd) -- are all field-values unique?
          , "    , fldauto = " ++ show (fldauto sqFd) -- is the field auto increment?
          , "    }"
          ])

   instance ShowHS SqlType where
    showHSname _ = error ("!Fatal (module ShowHS 186): SqlType is anonymous with respect to showHS options.")
    showHS _ indent (SQLChar i)    = indent++"SQLChar   "++show i
    showHS _ indent SQLBlob        = indent++"SQLBlob   "
    showHS _ indent SQLPass        = indent++"SQLPass   "
    showHS _ indent SQLSingle      = indent++"SQLSingle "
    showHS _ indent SQLDouble      = indent++"SQLDouble "
    showHS _ indent SQLText        = indent++"SQLText   "
    showHS _ indent (SQLuInt i)    = indent++"SQLuInt   "++show i
    showHS _ indent (SQLsInt i)    = indent++"SQLsInt   "++show i
    showHS _ indent SQLId          = indent++"SQLId     "
    showHS _ indent (SQLVarchar i) = indent++"SQLVarchar "++show i
    showHS _ indent (SQLEval strs) = indent++"SQLEval ["++chain ", " (map show strs)++"]"
    showHS _ indent SQLBool        = indent++"SQLBool   "

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fspc                          ***
-- \***********************************************************************
   instance ShowHS Fspc where
    showHSname fspec = typ fspec ++ "_" ++ haskellIdentifier (name fspec)
    showHS options indent fspec
     = chain (indent ++"    ") 
            ["Fspc{ fsName = " ++ haskellIdentifier (name fspec)
                  ,wrap ", vplugs        = " indentA (\_->showHSname) (vplugs fspec)
                  ,wrap ", plugs         = " indentA (\_->showHSname) (plugs fspec)
                  ,", serviceS      = serviceS'"
                  ,", serviceG      = serviceG'"
                  ,wrap ", services      = " indentA (\_->showHSname) (services fspec)
                  ,wrap ", vrules        = " indentA (\_->showHSname) (vrules fspec)
                  ,wrap ", vrels         = " indentA (\_->showHSname) (vrels fspec)
                  ,", fsisa         = isa'"
                  ,wrap ", vpatterns     = " indentA (\_->showHSname) (patterns fspec)
                  ,", themes        = " ++ "[]" -- SJ: tijdelijk om themes te omzeilen zolang ze nog niet werken.
                                                -- TODO: add an instance declaration for (ShowHS Data.Fspec.FTheme)
                                        -- "["++chain "," (map (showHS options "") (themes fspec))++"]" 
                  ,", violations    = " ++ "[]" -- SJ: tijdelijk om te omzeilen zolang ze nog niet werken. [(Rule,Paire)]
                  ,"}" 
                  ] ++   
       indent++"where"++
       indent++" isa' = "++ showHS options (indent ++ "        ") (fsisa fspec)++
       indent++" gE = genEq (typology isa')"++
        
       (if null (plugs fspec ) then "" else "\n -- ***PLUGS***: "++concat [indent++" "++showHSname p++indent++"  = "++showHS options (indent++"    ") p|p<-plugs fspec ]++"\n")++
        
        "\n -- ***Services S***: "++
       indent++" serviceS' = "++"[ "++chain (indentB++", ") (map (showHS options indentB) (serviceS fspec))++indentB++"]"++
        "\n -- ***Services G***: "++
       indent++" serviceG' = "++"[ "++chain (indentB++", ") (map (showHS options indentB) (serviceG fspec))++indentB++"]"++
       (if null (plugs fspec ) then "" else "\n -- ***Patterns***: "++concat [indent++" "++showHSname p++indent++"  = "++showHS options (indent++"    ") p|p<-patterns fspec ]++"\n")++

-- WAAROM?  staan hier verschillende lijstjes met services?
-- DAAROM!  Een ADL-engineer besteedt veel tijd om vanuit een kennismodel (lees: een graaf met concepten en relaties)
--          alle services met de hand te verzinnen.
--          Je kunt natuurlijk ook een services-generator aan het werk zetten, die een aantal services klaarzet bij wijze
--          van steiger (scaffold). Dat bespaart een hoop werk. De functie serviceG is zo'n generator.
--          Door de gegenereerde services af te drukken, kun je dus heel snel ADL-sourcecode maken met correct-vertaalbare services.
--          Heb je eenmaal een goed werkend pakket services, dan wil je wellicht alleen de door jezelf gespecificeerde services
--          gebruiken. Dat gebeurt in serviceS.

--        "\n -- ***Service definitions (both serviceS and serviceG, but each one exactly once. ***: "++  
--       (if null 
--            (uni (serviceS fspec)  (serviceG fspec)) then "" 
--             else concat [indent++" "++showHSname s++indent++"  = "++showHS options (indent++"    ") s|s<- (uni (serviceS fspec)  (serviceG fspec)) ]++"\n")++
-- 
        
       (if null (services fspec ) then "" else
        "\n -- ***Declarations of Services ***: "++
        concat [indent++" "++showHSname s++indent++"  = "++showHS options (indent++"    ") s|s<-services fspec ]++"\n")++
       (if null (vrules   fspec ) then "" else
        "\n -- ***Declarations of RULES ***: "++
        concat [indent++" "++showHSname r++indent++"  = "++showHS options (indent++"    ") r|r<-vrules   fspec ]++"\n")++        
       (if null (vrels fspec)     then "" else
        "\n -- ***Declarations OF RELATIONS ***: "++
        concat [indent++" "++showHSname d++indent++"  = "++showHS options (indent++"    ") d|d<- vrels fspec]++"\n")++
--        "\n -- ***PATTERNS***: "++
----       (if null (fspc_patterns fspec) then "" else concat ["\n\n   "++showHSname pat++" gE"++"\n>   = "++showHS options "\n>     " pat|pat<-fspc_patterns fspec]++
        "\n"
           where indentA = indent ++"                      "
                 indentB = indent ++"              "
  
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fservice                         ***
-- \***********************************************************************

   instance ShowHS Fservice where
    showHSname fservice = typ fservice ++ "_" ++ haskellIdentifier (name fservice)
    showHS options indent fservice
     = "Fservice "
       ++ indent++"     ("++showHS options (indent++"      ") (fsv_objectdef fservice)++")"
       ++ indent++"     [ "++chain (indent++"     , ") (map (showHS options (indent++"       ")) (fsv_rels     fservice))++indent++"     ]"
       ++ indent++"     [ "++chain (indent++"     , ")
          [showHSname r ++ if null (explain options r) then "" else "    -- " ++ explain options r| r<-fsv_rules fservice]
          ++indent++"     ]"
       ++ indent++"     [ "++chain (indent++"     , ") (map showHSname (fsv_signals  fservice))++indent++"     ]"
       ++ indent++"     [ "++chain (indent++"     , ") (map (showHS options (indent++"       ")) (fsv_fields  fservice))++indent++"     ]"
       ++ indent++"     ("++showHS options (indent++"      ") (fsv_fpa fservice)++")"
       ++ indent++" -- Einde Fservice "++showHSname (fsv_objectdef fservice)

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Field                         ***
-- \***********************************************************************

   instance ShowHS Field where
    showHSname fld = "fld_" ++ (fld_name fld)
    showHS options indent fld
     = "Att "++       "{ fld_name     = "++                     show (fld_name     fld)
       ++ indent++"    , fld_expr     = "++showHS options (indent++"      ") (fld_expr     fld)
       ++ indent++"    , fld_mph      = "++
          ( if fld_editable fld
            then showHS options (indent++"      ") (fld_mph     fld)
            else "error(\"!Fatal (module ShowHS 249): reference to undefined editrelation in field "++fld_name fld++"\")" )
       ++ indent++"    , fld_editable = "++                     show (fld_editable fld)
       ++ indent++"    , fld_list     = "++                     show (fld_list     fld)
       ++ indent++"    , fld_must     = "++                     show (fld_must     fld)
       ++ indent++"    , fld_new      = "++                     show (fld_new      fld)
       ++ indent++"    , fld_sLevel   = "++                     show (fld_sLevel   fld)
       ++ indent++"    , fld_insAble  = "++                     show (fld_insAble  fld)
       ++ indent++"    , fld_onIns    = "++
          ( if fld_insAble fld
            then "(\\d->"++showHSname (fld_onIns fld arg)++" d)"
            else "error(\"!Fatal (module ShowHS 304): reference to undefined insert action in field "++fld_name fld++"\")" )
       ++ indent++"    , fld_delAble  = "++                     show (fld_delAble  fld)
       ++ indent++"    , fld_onDel    = "++
          ( if fld_delAble fld
            then "(\\d->"++showHSname (fld_onDel fld arg)++" d)"
            else "error(\"!Fatal (module ShowHS 309): reference to undefined delete action in field "++fld_name fld++"\")" )
       ++ indent++"    }"
       where arg = error ("!Fatal (module ShowHS 311): reference to undefined argument of ECA rule")

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FSid                          ***
-- \***********************************************************************

   instance ShowHS FSid where
    showHSname (FS_id nm ) = haskellIdentifier nm 
    showHS _ _ (FS_id nm) 
      = "(FS_id " ++ show nm ++ ")"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Architecture                  ***
-- \***********************************************************************

--   instance ShowHS Architecture where
--    showHSname _ = error ("!Fatal (module ShowHS 327): an architecture is anonymous with respect to showHS options.")
--    showHS options indent arch = concat (map (showHS options indent) (archContexts arch))

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Context                       ***
-- \***********************************************************************
   
--   instance ShowHS Context where
-- -- TODO: showHS options should generate valid Haskell code for the entire pattern. Right now, it doesn't
--    showHSname context = "ctx_"++haskellIdentifier (name context)
--    showHS options indent context
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
--       indent++" isa = "++showHS options (indent++"       ") (Adl.isa context)++
--       indent++" gE  = genEq (typology isa)"++
--       (if null on   then "" else indent++" on  = "++showL [show x|x<-on]++"\n")++
--       (if null on   then "" else indent++" on  = "++showL [show x|x<-on]++"")++
--       (if null os   then "" else concat [indent++" "++showHSname o++" = "++showHS options "" o| o<-os]++"\n")++
--       (if null rs   then "" else concat [indent++" "++showHSname r++" = "++showHS options "" r| r<-rs]++"\n")++
--       (if null ds   then "" else concat [indent++" "++showHSname d++" = "++showHS options "" d| d<-ds]++"\n")++
--       (if null pops then "" else concat [indent++" "++showHSname p++indent++"  = "++showHS options (indent++"    ") p  |p<-populations context]++"\n")++
--       (if null cs   then "" else concat [indent++" "++showHSname c++" = "++showHS options "" c| c<-cs]++"\n")++
--       (if null ks   then "" else concat [indent++" "++showHSname k++" = "++showHS options "" k| k<-ks]++"\n")
--    -- patterns will be shown in  (showHS options indent Fspec)
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
 -- TODO: showHS options should generate valid Haskell code for the entire pattern. Right now, it doesn't
    showHSname pat = "pat_"++haskellIdentifier (name pat)
    showHS options indent pat
     = "Pat "++show (name pat)++
       (if null (rules pat) then " []" else indent++"    [" ++chain          "    , "  [showHSname r              | r<-rules pat] ++            "]")++
       (if null (ptgns pat) then " []" else indent++"    [ "++chain (indent++"    , ") [showHS options (indent++"     ") g| g<-ptgns         pat] ++indent++"    ]")++
       (if null (ptdcs pat) then " []" else indent++"    [" ++chain          "    , "  [showHSname d              | d<-ptdcs         pat] ++            "]")++
       (if null (ptcds pat) then " []" else indent++"    [" ++chain          "    , "  [showHSname c              | c<-ptcds         pat] ++            "]")++
       (if null (ptkds pat) then " []" else indent++"    [ "++chain (indent++"    , ") [showHS options (indent++"     ") k| k<-ptkds         pat] ++indent++"    ]")++
       indent++"where"++
       (if null (ptdcs pat) then "" else concat [indent++" "++showHSname d ++" = "++ showHS options (indent++"   ") d |d <-ptdcs         pat] )++
       (if null (rules pat) then "" else concat [indent++" "++showHSname r ++" = "++ showHS options (indent++"   ") r |r <-rules pat] )++
       (if null (ptcds pat) then "" else concat [indent++" "++showHSname cd++" = "++ showHS options (indent++"   ") cd|cd<-ptcds         pat] )++
       (if null (ptkds pat) then "" else concat [indent++" "++showHSname k ++" = "++ showHS options (indent++"   ") k |k <-ptkds         pat] )
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Rule                          ***
-- \***********************************************************************

   instance ShowHS Rule where
    showHSname r = "rule"++show (runum r)
    showHS options indent r   
      = case r of
           Ru{} -> (chain newIndent 
                    ["Ru{ rrsrt = " ++ showHS options "" (rrsrt r)
                      ,", rrant = " ++ "("++showHS options "" (rrant r)++")"
                      ,", rrfps = " ++ "("++showHS options "" (rrfps r)++")"
                      ,", rrcon = " ++ "("++showHS options "" (rrcon r)++")"
                      ,", rrxpl = " ++ show(rrxpl r)
                      ,", rrtyp = " ++ showHS options "" (rrtyp r)
                      ,", rrdcl = " ++ case rrdcl r of
                                        Nothing   -> "Nothing"
                                        Just(p,d) -> "Just("++showHS options "" p++","++showHS options "" d++")"
                      ,", runum = " ++ show (runum r)
                      ,", r_pat = " ++ show (r_pat r)
                      ,", r_usr = " ++ show (r_usr r)
                    ])++"}"
           Sg{} -> (chain newIndent
                    ["Sg{ srfps = " ++ "("++showHS options "" (srfps r)++")"
                      ,", srsig = " ++ "("++showHS options "" (srsig r)++")"
                      ,", srxpl = " ++ show (srxpl r)
                      ,", srtyp = " ++ "("++showHS options "" (srtyp r)++")"
                      ,", runum = " ++ show (runum r)
                      ,", r_pat = " ++ show (r_pat r)
                      ,", srrel = " ++ show(srrel r)
                    ])++"}"
           Fr{} -> (chain newIndent
                    ["Fr{ frdec = " ++ showHS options "" (frdec r)
                        ,", frcmp = " ++ "("++showHS options "" (frcmp r)++")"
                        ,", r_pat = " ++ show (r_pat r)
                    ])++"}"
         where newIndent = indent ++ "  " 
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: RuleType                      ***
-- \***********************************************************************
   instance ShowHS RuleType where
     showHSname _ = error "!Fatal (module ShowHS 431): showHSname undefined for Type 'RuleType'"
     showHS _ _ Truth          = "Truth"
     showHS _ _ Equivalence    = "Equivalence"
     showHS _ _ Implication    = "Implication"
     showHS _ _ Generalization = "Generalization"
     showHS _ _ Automatic      = "Automatic"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: KeyDef                        ***
-- \***********************************************************************

   instance ShowHS KeyDef where
    showHSname kd = "kDef_"++haskellIdentifier (name kd)
    showHS options indent kd
     = "Kd ("++showHS options "" (kdpos kd)++") "++show (kdlbl kd)++" ("++showHS options "" (kdctx kd)++")"
       ++indent++"[ "++chain (indent++", ") [showHS options (indent++"  ") a|a<-(kdats kd)]++indent++"]"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Population                    ***
-- \***********************************************************************

   instance ShowHS Population where
    showHSname pop = "pop_"++haskellIdentifier (name mph++name (source mph)++name (target mph))
        where mph = popm pop
    showHS options indent pop
     = "Popu ("++showHS options "" (popm pop)++")"++indent++"     [ "++chain (indent++"     , ") (map show (popps pop))++indent++"     ]"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ObjectDef                     ***
-- \***********************************************************************

   instance ShowHS ObjectDef where
    showHSname obj = "oDef_"++haskellIdentifier (name obj)
    showHS options indent r 
     = (chain (indent++"   ") 
           ["Obj{ objnm = " ++ show(objnm r)
                ,", objpos = " ++ "("++showHS options "" (objpos r)++")"
                ,", objctx = " ++ "("++showHS options "" (objctx r)++")"
                ,", objats = " ++ "["++chain (indent ++ "              ,")
                                             (map (showHS options (indent ++"               "))
                                                  (objats r))
                                ++"]"
                ,", objstrs = " ++ show(objstrs r)
           ])++"}"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Expression                    ***
-- \***********************************************************************

   instance ShowHS Expression where
    showHSname expr = error ("!Fatal (module ShowHS 481): an expression is anonymous with respect to showHS options. Detected at: "++ showADL expr)
    showHS options _ (Tm m')   = "Tm ("++showHS options "" m'++") "
    showHS options indent (Tc f)   = showHS options indent f
    showHS _ _ (F [])   = "F [] <Id>"
    showHS _ _ (Fd [])  = "Fd [] <nId>"
    showHS _ _ (Fu [])  = "Fu [] {- False -}"
    showHS _ _ (Fi [])  = "Fi [] {- True -}"
    showHS options indent (F [t])  = "F ["++showHS options (indent++"   ") t++"]"
    showHS options indent (F ts)   = "F [ "++chain (indent++"  , ") [showHS options (indent++"    ") t| t<-ts]++indent++"  ]"
    showHS options indent (Fd [t]) = "Fd ["++showHS options (indent++"    ") t++"]"
    showHS options indent (Fd ts)  = "Fd [ "++chain (indent++"   , ") [showHS options (indent++"     ") t| t<-ts]++indent++"   ]"
    showHS options indent (Fu [f]) = "Fu ["++showHS options (indent++"    ") f++"]"
    showHS options indent (Fu fs)  = "Fu [ "++chain (indent++"   , ") [showHS options (indent++"     ") f| f<-fs]++indent++"   ]"
    showHS options indent (Fi [f]) = "Fi ["++showHS options (indent++"    ") f++"]"
    showHS options indent (Fi fs)  = "Fi [ "++chain (indent++"   , ") [showHS options (indent++"     ") f| f<-fs]++indent++"   ]"
    showHS options indent (K0 e')   = "K0 ("++showHS options (indent++"    ") e'++") "
    showHS options indent (K1 e')   = "K1 ("++showHS options (indent++"    ") e'++") "
    showHS options indent (Cp e')   = "Cp ("++showHS options (indent++"    ") e'++") "

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Gen                           ***
-- \***********************************************************************

   instance ShowHS Gen where
    showHSname g = error ("!Fatal (module ShowHS 505): Illegal call to showHSname ("++showADL g++"). A GEN statement gets no definition in Haskell code.")
    showHS options _ gen = "G ("++showHS options "" (genfp gen)++") ("++showHS options "" (gengen gen)++") ("++showHS options "" (genspc gen)++")"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Morphism                      ***
-- \***********************************************************************

   instance ShowHS Morphism where
    showHSname mph = error ("!Fatal (module ShowHS 513): Illegal call to showHSname ("++showADL mph++"). A morphism gets no definition in Haskell code.")
    showHS options _ mph 
       = case mph of
            Mph{} -> "Mph "++show (mphnm mph)++" "++showPos++" "++showAtts
                         ++" "++showSgn++" "++show (mphyin mph)++" "++showHSname (mphdcl mph)
            I{}   -> "I "++showAtts++" "++showGen++" "++showSpc++" "++show (mphyin mph)
            V{}   -> "V "++showAtts++" "++showSgn
            Mp1{} -> "Mp1 "++mph1val mph++" "++showAtts++" ("++showHS options "" (mph1typ mph)++")"  -- WAAROM wordt mph1val mph zonder quotes afgedrukt?
  -- DAAROM: mph1val mph wordt door een lambda gebonden in de omgeving van Mp1. Het is dus een haskell identifier en niet een haskell string.
           where showPos  = "("++showHS options "" (mphpos mph)++")"
                 showAtts = showL(map (showHS options "") (mphats mph))
                 showGen  = "("++showHS options "" (mphgen mph)++")"
                 showSpc  = "("++showHS options "" (mphspc mph)++")"
                 showSgn  = "("++showHS options "" (mphtyp mph)++")"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Declaration                   ***
-- \***********************************************************************

   instance ShowHS Declaration where
    showHSname d = "rel_"++haskellIdentifier (name d++name (source d)++name (target d))
    showHS options indent d 
       = case d of 
          Sgn{}     -> (chain newIndent
                        ["Sgn{ decnm   = " ++ show (decnm d)
                           ,", desrc   = " ++ showHS options "" (desrc d)
                           ,", detgt   = " ++ showHS options "" (detgt d)
                           ,", decprps = " ++ showL(map (showHS options "") (decprps d))
                           ,", decprL  = " ++ show (decprL d)
                           ,", decprM  = " ++ show (decprM d)
                           ,", decprR  = " ++ show (decprR d)
                           ,", decpopu = " ++ show (decpopu d)
                           ,", decexpl = " ++ show (decexpl d)
                           ,", decfpos = " ++ showHS options "" (decfpos d)
                           ,", decid   = " ++ show (decid d)
                           ,", deciss  = " ++ show (deciss d)
                        ])++"}"
          Isn{}     -> (chain newIndent
                        ["Isn{ degen   = " ++ showHS options "" (degen d)
                           ,", despc   = " ++ showHS options "" (despc d)
                        ])++"}"
          Iscompl{} -> (chain newIndent
                        ["Isn{ degen   = " ++ showHS options "" (degen d)
                           ,", despc   = " ++ showHS options "" (despc d)
                        ])++"}"
          Vs{}      -> (chain newIndent
                        ["Isn{ degen   = " ++ showHS options "" (degen d)
                           ,", despc   = " ++ showHS options "" (despc d)
                        ])++"}"
       where newIndent = indent ++ "   "
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ConceptDef                    ***
-- \***********************************************************************

   instance ShowHS ConceptDef where
    showHSname cd = "cDef_"++haskellIdentifier (name cd)
    showHS options _ cd
     = " Cd ("++showHS options "" (cdpos cd)++") "++show (name cd)++" "++show (cddef cd)++(if null (cdref cd) then "" else " "++show (cdref cd))
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Concept                       ***
-- \***********************************************************************

   instance ShowHS Concept where
    showHSname c = error ("!Fatal (module ShowHS 577): Illegal call to showHSname ("++name c++"). A concept gets no definition in Haskell code.")
    showHS _ _ c = case c of
                       C{}      -> "C "++show (name c) ++ " gE []"    -- contents not shown.
                       S        -> "S "
                       Anything -> "Anything "
                       NOthing  -> "NOthing "
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FPA                           ***
-- \***********************************************************************
   
   instance ShowHS FPcompl where
    showHSname c = error ("!Fatal (module ShowHS 586): Illegal call to showHSname ("++show c++"). A FPcompl gets no definition in Haskell code.")
    showHS _ _ c   = show c

   instance ShowHS FPA where
    showHSname c = error ("!Fatal (module ShowHS 595): Illegal call to showHSname ("++show c++"). A FPA gets no definition in Haskell code.")
    showHS _ _ (ILGV c) = "ILGV "++show c
    showHS _ _ (KGV  c) = "KGV "++show c
    showHS _ _ (IF   c) = "IF "++show c
    showHS _ _ (UF   c) = "UF "++show c
    showHS _ _ (OF   c) = "OF "++show c
    showHS _ _ NO       = "NO"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Prop                          ***
-- \***********************************************************************
   
   instance ShowHS Prop where
    showHSname p = error ("!Fatal (module ShowHS 605): should not showHS options the name of multiplicities (Prop): "++show p)
    showHS _ _ Uni = "Uni"
    showHS _ _ Inj = "Inj"
    showHS _ _ Sur = "Sur"
    showHS _ _ Tot = "Tot"
    showHS _ _ Sym = "Sym"
    showHS _ _ Asy = "Asy"
    showHS _ _ Trn = "Trn"
    showHS _ _ Rfx = "Rfx"
    showHS _ _ Aut = "AUT"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FilePos                       ***
-- \***********************************************************************

   instance ShowHS FilePos where
    showHSname p = error ("!Fatal (module ShowHS 621): Illegal call to showHSname ("++show p++"). A position is an anonymous entity in Haskell code.")
    showHS _ _ (FilePos (fn,Pos l c,sym))
      = "FilePos ("++show fn++",Pos "++show l++" "++show c++","++show sym++")"
    showHS _ _ Nowhere
      = "Nowhere"



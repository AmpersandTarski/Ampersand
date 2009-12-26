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
             ++"\n  import ShowHS (showHS flags)"
             ++"\n  import Data.Fspec"
             ++"\n  import Data.Plug"
             ++"\n"
             ++"\n  main :: IO ()"
             ++"\n  main = putStr (showHS flags \"\\n  \" fSpec_"++baseName flags++")"
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
    showHS flags indent = chain "\n".map (showHS flags indent)

   instance ShowHS a => ShowHS (Maybe a) where
    showHSname Nothing  = "Nothing"
    showHSname (Just x) = showHSname x
    showHS _ _ Nothing  = "Nothing"
    showHS flags indent (Just x) = showHS flags indent x

   instance ShowHS a => ShowHS (Inheritance a) where
    showHSname _ = error ("!Fatal (module ShowHS 60): every inheritance is anonymous with respect to showHS flags.")
    showHS flags indent (Isa ts cs) = "Isa "++showL ["("++showHS flags "" g++","++showHS flags "" s++")"|(g,s)<-ts] ++indent++"    "++ showL (map (showHS flags "") cs)


   -- | The following is used to showHS flags for signs: (Concept, Concept)
   instance (ShowHS a , ShowHS b) => ShowHS (a,b) where
    showHSname _ = error ("!Fatal (module ShowHS 66): Tuples of Concepts are anonymous with respect to showHS flags.")
    showHS flags indent (a,b) = "("++showHS flags indent a++","++showHS flags indent b++")"
    
   

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Plug                          ***
-- \***********************************************************************
   instance ShowHS Plug where
    showHSname plug = "plug_"++haskellIdentifier (name plug)
    showHS flags indent plug   
      = case plug of
           PlugSql{} -> (chain indent 
                          ["PlugSql{ fields = " ++ "[ "++
                                              chain (indent++"                  , ") (map (showHS flags (indent++"                    ")) (fields plug))++
                                              indent++"                  ]"
                          ,"       , plname = " ++ (show.haskellIdentifier.plname) plug
                          ,"       , plfpa  = " ++ showHS flags "" (plfpa plug)
                          ,"       }"
                          ])
           PlugPhp{} -> (chain indent 
                          ["PlugPhp{ args = " ++ "[ "++
                                            chain (indent++"                , ") [ "("++show i++","++showHS flags "" a++")"|(i,a)<-args plug]++
                                            "                ]"
                          ,"       , returns  = " ++ showHS flags "" (returns plug)
                          ,"       , function = " ++ showHS flags "" (function plug)
                          ,"       , phpfile  = " ++ show (phpfile plug)
                          ,"       , plname   = " ++ show (plname  plug)
                          ,"       , plfpa    = " ++ showHS flags "" (plfpa plug)
                          ,"       }"
                          ])

   instance ShowHS PhpValue where
    showHSname _ = error ("!Fatal (module ShowHS): PhpValue is anonymous with respect to showHS flags.")
    showHS flags _ phpVal
      = case phpVal of
           PhpNull{}   -> "PhpNull"
           PhpObject{} -> "PhpObject{ objectdf = " ++ showHSname (objectdf phpVal) ++ ", phptype  = " ++ showHS flags "" (phptype phpVal) ++ "}"

   instance ShowHS PhpType where
    showHSname _ = error ("!Fatal (module ShowHS): PhpType is anonymous with respect to showHS flags.")
    showHS _ indent PhpString = indent++"PhpString"
    showHS _ indent PhpInt    = indent++"PhpInt"
    showHS _ indent PhpFloat  = indent++"PhpFloat"
    showHS _ indent PhpArray  = indent++"PhpArray"

   instance ShowHS PhpReturn where
    showHSname _ = error ("!Fatal (module ShowHS): PhpReturn is anonymous with respect to showHS flags.")
    showHS flags indent ret = indent++"PhpReturn {retval = "++showHS flags indent (retval ret)++"}"

   instance ShowHS PhpAction where
    showHSname _ = error ("!Fatal (module ShowHS): PhpAction is anonymous with respect to showHS flags.")
    showHS flags indent act
      = (chain (indent ++"    ") 
          [ "PhpAction { action = " ++ showHS flags "" (action act)
          , "          , on     = " ++ "["++chain ", " (map (showHS flags "") (on act))++"]"
          , "          }"
          ])

   instance ShowHS ECArule where
    showHSname r = "ecaRule"++show (ecaNum r)
    showHS flags indent r   
      = "ECA (" ++ showHS flags "" (ecaTriggr r)++")" ++
        indent++"    (" ++ showHS flags (indent++"     ")  (ecaDelta r)++")"++
        indent++"    (" ++ showHS flags (indent++"     ")  (ecaAction r)++indent++"    )" ++
        indent++show (ecaNum r)

   instance ShowHS Event where
    showHSname _ = error ("!Fatal (module ShowHS): \"Event\" is anonymous with respect to showHS flags.")
    showHS flags indent e   
      = if take 1 indent == "\n"
        then "On (" ++ show (eSrt e)++")" ++ indent++"   (" ++ showHS flags (indent++"    ") (eMhp e)++indent++"   )"
        else "On (" ++ show (eSrt e)++") (" ++ showHS flags "" (eMhp e)++")"

   instance ShowHS PAclause where
    showHSname _ = error ("!Fatal (module ShowHS): \"PAclause\" is anonymous with respect to showHS flags.")
    showHS flags indent p   
      = case p of
           Chc{} -> wrap "Chc " (indent ++"    ") (showHS flags) (paCls p)++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
           All{} -> "All [ "++chain (indent++"    , ") (map (showHS flags (indent++"      ")) (paCls p))++indent++"    ]"++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
           Do{}  ->  "Do "++show (paSrt p)++ " ("++showHS flags (indent++"        ") (paTo p)++indent++"       )"++
                            indent++"       ("++showHS flags (indent++"        ") (paDelta p)++indent++"       )"++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
           New{} -> "New ("++showHS flags "" (paCpt p)++")"++
                    indent++"    (\\x->"++showHS flags (indent++"        ") (paCl p "x")++indent++"    )"++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
           Rmv{} -> "Rmv ("++showHS flags "" (paCpt p)++")"++
                    indent++"    (\\x->"++showHS flags (indent++"        ") (paCl p "x")++indent++"    )"++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
           Sel{} -> "Sel ("++showHS flags "" (paCpt p)++")"++
                    indent++"    ( "++showHS flags (indent++"      ") (paExp p)++indent++"    )"++
                    indent++"    (\\x->"++showHS flags (indent++"        ") (paCl p "x")++indent++"    )"++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
           Nop{} -> "Nop "++wrap "" (indent ++"    ") showMotiv ms
           Blk{} -> "Blk "++wrap "" (indent ++"    ") showMotiv ms
           Dry{} -> "Dry "++wrap "" (indent ++"    ") showMotiv ms
        where ms = paMotiv p
              showMotiv ind (conj,rs) = "("++showHS flags ind conj++", "++showHSname rs++")"

   instance ShowHS ActionType where
    showHSname _ = error ("!Fatal (module ShowHS 166): \"ActionType\" is anonymous with respect to showHS flags.")
    showHS _ indent Create = indent++"Create"
    showHS _ indent Read   = indent++"Read"
    showHS _ indent Update = indent++"Update"
    showHS _ indent Delete = indent++"Delete"

   instance ShowHS SqlField where
    showHSname _ = error ("!Fatal (module ShowHS 173): \"SqlField\" is anonymous with respect to showHS flags.")
    showHS flags indent sqFd
      = (chain indent
          [ "Fld { fldname = " ++ show (fldname sqFd)
          , "    , fldexpr = " ++ showHS flags "" (fldexpr sqFd)
          , "    , fldtype = " ++ showHS flags "" (fldtype sqFd)
          , "    , fldnull = " ++ show (fldnull sqFd) -- can there be empty field-values?
          , "    , flduniq = " ++ show (flduniq sqFd) -- are all field-values unique?
          , "    , fldauto = " ++ show (fldauto sqFd) -- is the field auto increment?
          , "    }"
          ])

   instance ShowHS SqlType where
    showHSname _ = error ("!Fatal (module ShowHS 186): SqlType is anonymous with respect to showHS flags.")
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
    showHS flags indent fspec
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
                                        -- "["++chain "," (map (showHS flags "") (themes fspec))++"]" 
                  ,", violations    = " ++ "[]" -- SJ: tijdelijk om te omzeilen zolang ze nog niet werken. [(Rule,Paire)]
                  ,"}" 
                  ] ++   
       indent++"where"++
       indent++" isa' = "++ showHS flags (indent ++ "        ") (fsisa fspec)++
       indent++" gE = genEq (typology isa')"++
        
       (if null (plugs fspec ) then "" else "\n -- ***PLUGS***: "++concat [indent++" "++showHSname p++indent++"  = "++showHS flags (indent++"    ") p|p<-plugs fspec ]++"\n")++
        
        "\n -- ***Services S***: "++
       indent++" serviceS' = "++"[ "++chain (indentB++", ") (map (showHS flags indentB) (serviceS fspec))++indentB++"]"++
        "\n -- ***Services G***: "++
       indent++" serviceG' = "++"[ "++chain (indentB++", ") (map (showHS flags indentB) (serviceG fspec))++indentB++"]"++
       (if null (plugs fspec ) then "" else "\n -- ***Patterns***: "++concat [indent++" "++showHSname p++indent++"  = "++showHS flags (indent++"    ") p|p<-patterns fspec ]++"\n")++

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
--             else concat [indent++" "++showHSname s++indent++"  = "++showHS flags (indent++"    ") s|s<- (uni (serviceS fspec)  (serviceG fspec)) ]++"\n")++
-- 
        
       (if null (services fspec ) then "" else
        "\n -- ***Declarations of Services ***: "++
        concat [indent++" "++showHSname s++indent++"  = "++showHS flags (indent++"    ") s|s<-services fspec ]++"\n")++
       (if null (vrules   fspec ) then "" else
        "\n -- ***Declarations of RULES ***: "++
        concat [indent++" "++showHSname r++indent++"  = "++showHS flags (indent++"    ") r|r<-vrules   fspec ]++"\n")++        
       (if null (vrels fspec)     then "" else
        "\n -- ***Declarations OF RELATIONS ***: "++
        concat [indent++" "++showHSname d++indent++"  = "++showHS flags (indent++"    ") d|d<- vrels fspec]++"\n")++
--        "\n -- ***PATTERNS***: "++
----       (if null (fspc_patterns fspec) then "" else concat ["\n\n   "++showHSname pat++" gE"++"\n>   = "++showHS flags "\n>     " pat|pat<-fspc_patterns fspec]++
        "\n"
           where indentA = indent ++"                      "
                 indentB = indent ++"              "
  
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fservice                         ***
-- \***********************************************************************

   instance ShowHS Fservice where
    showHSname fservice = typ fservice ++ "_" ++ haskellIdentifier (name fservice)
    showHS flags indent fservice
     = "Fservice "
       ++ indent++"     ("++showHS flags (indent++"      ") (fsv_objectdef fservice)++")"
       ++ indent++"     [ "++chain (indent++"     , ") (map (showHS flags (indent++"       ")) (fsv_rels     fservice))++indent++"     ]"
       ++ indent++"     [ "++chain (indent++"     , ")
          [showHSname r ++ if null (explain flags r) then "" else "    -- " ++ explain flags r| r<-fsv_rules fservice]
          ++indent++"     ]"
       ++ indent++"     [ "++chain (indent++"     , ") (map showHSname (fsv_signals  fservice))++indent++"     ]"
       ++ indent++"     [ "++chain (indent++"     , ") (map (showHS flags (indent++"       ")) (fsv_fields  fservice))++indent++"     ]"
       ++ indent++"     ("++showHS flags (indent++"      ") (fsv_fpa fservice)++")"
       ++ indent++" -- Einde Fservice "++showHSname (fsv_objectdef fservice)

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Field                         ***
-- \***********************************************************************

   instance ShowHS Field where
    showHSname fld = "fld_" ++ (fld_name fld)
    showHS flags indent fld
     = "Att "++       "{ fld_name     = "++                     show (fld_name     fld)
       ++ indent++"    , fld_sub      = "++showHS flags (indent++"      ") (fld_sub  fld)
       ++ indent++"    , fld_expr     = "++showHS flags (indent++"      ") (fld_expr fld)
       ++ indent++"    , fld_mph      = "++
          ( if fld_editable fld
            then showHS flags (indent++"      ") (fld_mph     fld)
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
--    showHSname _ = error ("!Fatal (module ShowHS 327): an architecture is anonymous with respect to showHS flags.")
--    showHS flags indent arch = concat (map (showHS flags indent) (archContexts arch))

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Context                       ***
-- \***********************************************************************
   
--   instance ShowHS Context where
-- -- TODO: showHS flags should generate valid Haskell code for the entire pattern. Right now, it doesn't
--    showHSname context = "ctx_"++haskellIdentifier (name context)
--    showHS flags indent context
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
--       indent++" isa = "++showHS flags (indent++"       ") (Adl.isa context)++
--       indent++" gE  = genEq (typology isa)"++
--       (if null on   then "" else indent++" on  = "++showL [show x|x<-on]++"\n")++
--       (if null on   then "" else indent++" on  = "++showL [show x|x<-on]++"")++
--       (if null os   then "" else concat [indent++" "++showHSname o++" = "++showHS flags "" o| o<-os]++"\n")++
--       (if null rs   then "" else concat [indent++" "++showHSname r++" = "++showHS flags "" r| r<-rs]++"\n")++
--       (if null ds   then "" else concat [indent++" "++showHSname d++" = "++showHS flags "" d| d<-ds]++"\n")++
--       (if null pops then "" else concat [indent++" "++showHSname p++indent++"  = "++showHS flags (indent++"    ") p  |p<-populations context]++"\n")++
--       (if null cs   then "" else concat [indent++" "++showHSname c++" = "++showHS flags "" c| c<-cs]++"\n")++
--       (if null ks   then "" else concat [indent++" "++showHSname k++" = "++showHS flags "" k| k<-ks]++"\n")
--    -- patterns will be shown in  (showHS flags indent Fspec)
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
 -- TODO: showHS flags should generate valid Haskell code for the entire pattern. Right now, it doesn't
    showHSname pat = "pat_"++haskellIdentifier (name pat)
    showHS flags indent pat
     = "Pat "++show (name pat)++
       (if null (rules pat) then " []" else indent++"    [" ++chain          "    , "  [showHSname r              | r<-rules pat] ++            "]")++
       (if null (ptgns pat) then " []" else indent++"    [ "++chain (indent++"    , ") [showHS flags (indent++"     ") g| g<-ptgns         pat] ++indent++"    ]")++
       (if null (ptdcs pat) then " []" else indent++"    [" ++chain          "    , "  [showHSname d              | d<-ptdcs         pat] ++            "]")++
       (if null (ptcds pat) then " []" else indent++"    [" ++chain          "    , "  [showHSname c              | c<-ptcds         pat] ++            "]")++
       (if null (ptkds pat) then " []" else indent++"    [ "++chain (indent++"    , ") [showHS flags (indent++"     ") k| k<-ptkds         pat] ++indent++"    ]")++
       indent++"where"++
       (if null (ptdcs pat) then "" else concat [indent++" "++showHSname d ++" = "++ showHS flags (indent++"   ") d |d <-ptdcs         pat] )++
       (if null (rules pat) then "" else concat [indent++" "++showHSname r ++" = "++ showHS flags (indent++"   ") r |r <-rules pat] )++
       (if null (ptcds pat) then "" else concat [indent++" "++showHSname cd++" = "++ showHS flags (indent++"   ") cd|cd<-ptcds         pat] )++
       (if null (ptkds pat) then "" else concat [indent++" "++showHSname k ++" = "++ showHS flags (indent++"   ") k |k <-ptkds         pat] )
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Rule                          ***
-- \***********************************************************************

   instance ShowHS Rule where
    showHSname r = "rule"++show (runum r)
    showHS flags indent r   
      = case r of
           Ru{} -> (chain newIndent 
                    ["Ru{ rrsrt = " ++ showHS flags "" (rrsrt r)
                      ,", rrant = " ++ "("++showHS flags "" (rrant r)++")"
                      ,", rrfps = " ++ "("++showHS flags "" (rrfps r)++")"
                      ,", rrcon = " ++ "("++showHS flags "" (rrcon r)++")"
                      ,", rrxpl = " ++ show(rrxpl r)
                      ,", rrtyp = " ++ showHS flags "" (rrtyp r)
                      ,", rrdcl = " ++ case rrdcl r of
                                        Nothing   -> "Nothing"
                                        Just(p,d) -> "Just("++showHS flags "" p++","++showHS flags "" d++")"
                      ,", runum = " ++ show (runum r)
                      ,", r_pat = " ++ show (r_pat r)
                      ,", r_usr = " ++ show (r_usr r)
                    ])++"}"
           Sg{} -> (chain newIndent
                    ["Sg{ srfps = " ++ "("++showHS flags "" (srfps r)++")"
                      ,", srsig = " ++ "("++showHS flags "" (srsig r)++")"
                      ,", srxpl = " ++ show (srxpl r)
                      ,", srtyp = " ++ "("++showHS flags "" (srtyp r)++")"
                      ,", runum = " ++ show (runum r)
                      ,", r_pat = " ++ show (r_pat r)
                      ,", srrel = " ++ show(srrel r)
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
    showHS flags indent kd
     = "Kd ("++showHS flags "" (kdpos kd)++") "++show (kdlbl kd)++" ("++showHS flags "" (kdctx kd)++")"
       ++indent++"[ "++chain (indent++", ") [showHS flags (indent++"  ") a|a<-(kdats kd)]++indent++"]"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Population                    ***
-- \***********************************************************************

   instance ShowHS Population where
    showHSname pop = "pop_"++haskellIdentifier (name mph++name (source mph)++name (target mph))
        where mph = popm pop
    showHS flags indent pop
     = "Popu ("++showHS flags "" (popm pop)++")"++indent++"     [ "++chain (indent++"     , ") (map show (popps pop))++indent++"     ]"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ObjectDef                     ***
-- \***********************************************************************

   instance ShowHS ObjectDef where
    showHSname obj = "oDef_"++haskellIdentifier (name obj)
    showHS flags indent r 
     = (chain (indent++"   ") 
           ["Obj{ objnm = " ++ show(objnm r)
                ,", objpos = " ++ "("++showHS flags "" (objpos r)++")"
                ,", objctx = " ++ "("++showHS flags "" (objctx r)++")"
                ,", objats = " ++ "["++chain (indent ++ "              ,")
                                             (map (showHS flags (indent ++"               "))
                                                  (objats r))
                                ++"]"
                ,", objstrs = " ++ show(objstrs r)
           ])++"}"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Expression                    ***
-- \***********************************************************************

   instance ShowHS Expression where
    showHSname expr = error ("!Fatal (module ShowHS 481): an expression is anonymous with respect to showHS flags. Detected at: "++ showADL expr)
    showHS flags _ (Tm m')   = "Tm ("++showHS flags "" m'++") "
    showHS flags indent (Tc f)   = showHS flags indent f
    showHS _ _ (F [])   = "F [] <Id>"
    showHS _ _ (Fd [])  = "Fd [] <nId>"
    showHS _ _ (Fu [])  = "Fu [] {- False -}"
    showHS _ _ (Fi [])  = "Fi [] {- True -}"
    showHS flags indent (F [t])  = "F ["++showHS flags (indent++"   ") t++"]"
    showHS flags indent (F ts)   = "F [ "++chain (indent++"  , ") [showHS flags (indent++"    ") t| t<-ts]++indent++"  ]"
    showHS flags indent (Fd [t]) = "Fd ["++showHS flags (indent++"    ") t++"]"
    showHS flags indent (Fd ts)  = "Fd [ "++chain (indent++"   , ") [showHS flags (indent++"     ") t| t<-ts]++indent++"   ]"
    showHS flags indent (Fu [f]) = "Fu ["++showHS flags (indent++"    ") f++"]"
    showHS flags indent (Fu fs)  = "Fu [ "++chain (indent++"   , ") [showHS flags (indent++"     ") f| f<-fs]++indent++"   ]"
    showHS flags indent (Fi [f]) = "Fi ["++showHS flags (indent++"    ") f++"]"
    showHS flags indent (Fi fs)  = "Fi [ "++chain (indent++"   , ") [showHS flags (indent++"     ") f| f<-fs]++indent++"   ]"
    showHS flags indent (K0 e')   = "K0 ("++showHS flags (indent++"    ") e'++") "
    showHS flags indent (K1 e')   = "K1 ("++showHS flags (indent++"    ") e'++") "
    showHS flags indent (Cp e')   = "Cp ("++showHS flags (indent++"    ") e'++") "

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Gen                           ***
-- \***********************************************************************

   instance ShowHS Gen where
    showHSname g = error ("!Fatal (module ShowHS 505): Illegal call to showHSname ("++showADL g++"). A GEN statement gets no definition in Haskell code.")
    showHS flags _ gen = "G ("++showHS flags "" (genfp gen)++") ("++showHS flags "" (gengen gen)++") ("++showHS flags "" (genspc gen)++")"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Morphism                      ***
-- \***********************************************************************

   instance ShowHS Morphism where
    showHSname mph = error ("!Fatal (module ShowHS 513): Illegal call to showHSname ("++showADL mph++"). A morphism gets no definition in Haskell code.")
    showHS flags _ mph 
       = case mph of
            Mph{} -> "Mph "++show (mphnm mph)++" "++showPos++" "++showAtts
                         ++" "++showSgn++" "++show (mphyin mph)++" "++showHSname (mphdcl mph)
            I{}   -> "I "++showAtts++" "++showGen++" "++showSpc++" "++show (mphyin mph)
            V{}   -> "V "++showAtts++" "++showSgn
            Mp1{} -> "Mp1 "++mph1val mph++" "++showAtts++" ("++showHS flags "" (mph1typ mph)++")"  -- WAAROM wordt mph1val mph zonder quotes afgedrukt?
  -- DAAROM: mph1val mph wordt door een lambda gebonden in de omgeving van Mp1. Het is dus een haskell identifier en niet een haskell string.
           where showPos  = "("++showHS flags "" (mphpos mph)++")"
                 showAtts = showL(map (showHS flags "") (mphats mph))
                 showGen  = "("++showHS flags "" (mphgen mph)++")"
                 showSpc  = "("++showHS flags "" (mphspc mph)++")"
                 showSgn  = "("++showHS flags "" (mphtyp mph)++")"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Declaration                   ***
-- \***********************************************************************

   instance ShowHS Declaration where
    showHSname d = "rel_"++haskellIdentifier (name d++name (source d)++name (target d))
    showHS flags indent d 
       = case d of 
          Sgn{}     -> (chain newIndent
                        ["Sgn{ decnm   = " ++ show (decnm d)
                           ,", desrc   = " ++ showHS flags "" (desrc d)
                           ,", detgt   = " ++ showHS flags "" (detgt d)
                           ,", decprps = " ++ showL(map (showHS flags "") (decprps d))
                           ,", decprL  = " ++ show (decprL d)
                           ,", decprM  = " ++ show (decprM d)
                           ,", decprR  = " ++ show (decprR d)
                           ,", decpopu = " ++ show (decpopu d)
                           ,", decexpl = " ++ show (decexpl d)
                           ,", decfpos = " ++ showHS flags "" (decfpos d)
                           ,", decid   = " ++ show (decid d)
                           ,", deciss  = " ++ show (deciss d)
                        ])++"}"
          Isn{}     -> (chain newIndent
                        ["Isn{ degen   = " ++ showHS flags "" (degen d)
                           ,", despc   = " ++ showHS flags "" (despc d)
                        ])++"}"
          Iscompl{} -> (chain newIndent
                        ["Isn{ degen   = " ++ showHS flags "" (degen d)
                           ,", despc   = " ++ showHS flags "" (despc d)
                        ])++"}"
          Vs{}      -> (chain newIndent
                        ["Isn{ degen   = " ++ showHS flags "" (degen d)
                           ,", despc   = " ++ showHS flags "" (despc d)
                        ])++"}"
       where newIndent = indent ++ "   "
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ConceptDef                    ***
-- \***********************************************************************

   instance ShowHS ConceptDef where
    showHSname cd = "cDef_"++haskellIdentifier (name cd)
    showHS flags _ cd
     = " Cd ("++showHS flags "" (cdpos cd)++") "++show (name cd)++" "++show (cddef cd)++(if null (cdref cd) then "" else " "++show (cdref cd))
   
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
    showHSname p = error ("!Fatal (module ShowHS 605): should not showHS flags the name of multiplicities (Prop): "++show p)
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



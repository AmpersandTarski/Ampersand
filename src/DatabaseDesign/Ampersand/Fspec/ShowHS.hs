{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec.ShowHS (ShowHS(..),fSpec2Haskell,haskellIdentifier)
where
   import DatabaseDesign.Ampersand.Core.ParseTree
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import Text.Pandoc
   import Data.Char                  (isAlphaNum)
   import DatabaseDesign.Ampersand.Basics
   import DatabaseDesign.Ampersand.Fspec.Plug
   import DatabaseDesign.Ampersand.Fspec.Fspec
   import DatabaseDesign.Ampersand.Fspec.ShowADL    (ShowADL(..))--,showADLcode) -- wenselijk voor foutmeldingen.
   import DatabaseDesign.Ampersand.Fspec.FPA   (fpa)
   import Data.List
   import DatabaseDesign.Ampersand.Classes
   import qualified DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner
   import DatabaseDesign.Ampersand.Misc
   --import DatabaseDesign.Ampersand.Fspec.FPA        (FPA(..),FPcompl)
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "Fspec.ShowHS"

   fSpec2Haskell :: Fspc -> Options -> String
   fSpec2Haskell fSpec flags
           = "{-# OPTIONS_GHC -Wall #-}"
             ++"\n{-Generated code by "++ampersandVersionStr++" at "++show (genTime flags)++"-}"
             ++"\nmodule Main where"
             ++"\n  import UU_Scanner"
             ++"\n  import ADL"
             ++"\n  import ShowHS (showHS)"
             ++"\n  import DatabaseDesign.Ampersand.Fspec.Fspec"
             ++"\n  import DatabaseDesign.Ampersand.Misc.Plug"
             ++"\n  import DatabaseDesign.Ampersand.Fspec.FPA"
             ++"\n  import DatabaseDesign.Ampersand.Misc (getOptions)"
             ++"\n  import DatabaseDesign.Ampersand.TypeInference.InfLibAGFuncs  hiding (fatal)"
             ++"\n  import DatabaseDesign.Ampersand.Basics"  
             ++"\n  import Prelude hiding (writeFile,readFile,getContents,putStr,putStrLn)"
             ++"\n"
             ++"\n  main :: IO ()"
             ++"\n  main = do flags <- getOptions"
             ++"\n            putStr (showHS flags \"\\n  \" fSpec_"++baseName flags++")"
     -- TODO: I temporarily disabled the following line, during work at ticket #85
     -- WHY was this text here in the first place?
     --        ++"\n"++"{- \n"++show [x |p<-vpatterns fSpec,x<-testexpr p]++show [x |p<-vpatterns fSpec,x<-inftestexpr p] ++ "\n-}\n"
             ++"\n  fSpec_"++baseName flags++" :: Fspc"
             ++"\n  fSpec_"++baseName flags++"\n   = "++showHS flags "\n     " fSpec


   wrap :: String->String->(String->a->String)->[a]->String
   wrap initStr indent f xs
    = initStr++
      case xs of
        []  -> "[]"
        [x] -> "[ "++f (indent++"  ") x++" ]"
        _   -> "[ "++intercalate (indent++", ") [f (indent++"  ") x | x<-xs]++indent++"]"

   class ShowHS a where
    showHSname :: a -> String
    showHS     :: Options -> String -> a -> String

   instance ShowHS a => ShowHS [a] where
    showHSname xs = "["++intercalate "," (map showHSname xs)++"]"
    showHS flags indent = wrap "" (indent++" ") (showHS flags)
    
   instance ShowHS a => ShowHS (Maybe a) where
    showHSname Nothing  = "Nothing"
    showHSname (Just x) = showHSname x
    showHS _ _ Nothing  = "Nothing"
    showHS flags indent (Just x) = showHS flags indent x


   -- | The following is used to showHS flags for signs: (Concept, Concept)
   instance (ShowHS a , ShowHS b) => ShowHS (a,b) where
    showHSname _ = fatal 77 "Tuples of Concepts are anonymous with respect to showHS flags."
    showHS flags indent (a,b) = "("++showHS flags (indent++" ") a++","++showHS flags (indent++" ") b++")"
    
   

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Plug                          ***
-- \***********************************************************************
   instance ShowHS PlugSQL where
    showHSname plug = haskellIdentifier ("plug_"++name plug)
    showHS flags indent plug
      = case plug of
          TblSQL{} -> intercalate indent 
                      ["let " ++ intercalate (indent++"    ")
                                             [showHSname f++indent++"     = "++showHS flags (indent++"       ") f | f<-fields plug] ++indent++"in"
                      ,"TblSQL { sqlname = " ++ (show.haskellIdentifier.name) plug
                      ,"       , fields  = ["++intercalate ", " (map showHSname (fields plug))++"]"
                      ,"       , cLkpTbl = [ "++intercalate (indent++"                   , ") ["("++showHS flags "" c++", "++showHSname cn++")" | (c,cn)<-cLkpTbl plug] ++ "]"
                      ,"       , mLkpTbl = [ "++intercalate (indent++"                   , ") ["("++showHS flags "" r++", "++showHSname ms++", "++showHSname mt++")" | (r,ms,mt)<-mLkpTbl plug] ++ "]"
                      ,"       , sqlfpa  = " ++ showHS flags "" (fpa plug)
                      ,"       }"
                      ]
          BinSQL{} -> intercalate indent 
                      ["let " ++ showHSname (fst (columns plug))++indent++"     = "++showHS flags (indent++"       ") (fst (columns plug))
                              ++ (indent++"    ") ++ showHSname (snd (columns plug))++indent++"     = "++showHS flags (indent++"       ") (snd (columns plug))
                              ++indent++"in"
                      ,"BinSQL { sqlname = " ++ (show.haskellIdentifier.name) plug
                      ,"       , columns = ("++showHSname (fst (columns plug))++ ", " ++showHSname (snd (columns plug))++")"
                      ,"       , cLkpTbl = [ "++intercalate (indent++"                   , ") ["("++showHS flags "" c++", "++showHSname cn++")" | (c,cn)<-cLkpTbl plug] ++ "]"
                      ,"       , mLkp = "++showHS flags "" (mLkp plug)
                      ,"       , sqlfpa  = " ++ showHS flags "" (fpa plug)
                      ,"       }"
                      ]
          ScalarSQL{} -> intercalate indent 
                      ["ScalarSQL { sqlname = " ++ (show.haskellIdentifier.name) plug
                      ,"          , column = "++showHS flags (indent++"                     ") (column plug)
                      ,"          , cLkpTbl = "++showHS flags "" (cLkp plug)
                      ,"          , sqlfpa  = " ++ showHS flags "" (fpa plug)
                      ,"          }"
                      ]

   instance ShowHS (ECArule) where
    showHSname r = "ecaRule"++show (ecaNum r)
    showHS flags indent r   
      = "ECA (" ++ showHS flags "" (ecaTriggr r)++")" ++
        indent++"    (" ++ showHS flags (indent++"     ")  (ecaDelta r)++")"++
        indent++"    (" ++ showHS flags (indent++"     ")  (ecaAction r)++indent++"    )" ++
        indent++show (ecaNum r)

   instance ShowHS Event where
    showHSname _ = fatal 129 "\"Event\" is anonymous with respect to showHS flags."
    showHS flags indent e   
      = if "\n" `isPrefixOf` indent
        then "On " ++ show (eSrt e)++indent++"   (" ++ showHS flags (indent++"    ") (eRel e)++indent++"   )"
        else "On " ++ show (eSrt e)++" (" ++ showHS flags "" (eRel e)++")"

   instance ShowHS PAclause where
    showHSname _ = fatal 136 "\"PAclause\" is anonymous with respect to showHS flags."
    showHS flags indent p   
      = case p of
           Chc{} -> wrap "Chc " (indent ++"    ") (showHS flags) (paCls p)++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
           All{} -> "All [ "++intercalate (indent++"    , ") (map (showHS flags (indent++"      ")) (paCls p))++indent++"    ]"++
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
        where ms = paMotiv p
              showMotiv ind (conj,rs) = "("++showHS flags (ind++" ") conj++", "++showHSname rs++")"

   instance ShowHS SqlField where
    showHSname sqFd = haskellIdentifier ("sqlFld_"++fldname sqFd)
    showHS flags indent sqFd
      = intercalate indent
          [ "Fld { fldname = " ++ show (fldname sqFd)
          , "    , fldexpr = " ++ showHS flags "" (fldexpr sqFd)
          , "    , fldtype = " ++ showHS flags "" (fldtype sqFd)
          , "    , fldnull = " ++ show (fldnull sqFd) -- can there be empty field-values?
          , "    , flduniq = " ++ show (flduniq sqFd) -- are all field-values unique?
          , "    , fldauto = " ++ show (fldauto sqFd) -- is the field auto increment?
          , "    }"
          ]

   instance ShowHS SqlType where
    showHSname _ = fatal 175 "SqlType is anonymous with respect to showHS flags."
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
    showHS _ indent SQLBool        = indent++"SQLBool   "

   instance ShowHS Quad where
   -- TODO: is showHSname injective? Make sure it is!
    showHSname q
      = haskellIdentifier ("quad_"++(name.qMorph) q++"_"++sgnt++"_"++(name.cl_rule.qClauses) q)
        where sgnt = if source r==target r then name (source r) else name (source r)++"_"++name (target r)
              r    = qMorph q
    showHS flags indent q 
      = intercalate indent
          [ "Quad{ qMorph   = " ++ showHS flags newindent (qMorph q)
          , "    , qClauses = " ++ showHS flags newindent (qClauses q)
          , "    }"
          ]
       where
         newindent = indent ++ "                 "
         
   instance ShowHS Fswitchboard where
    showHSname _ = fatal 204 "Fswitchboard is anonymous with respect to showHS flags."
    showHS flags indent fsb
      = intercalate indent
          [ "Fswtch { fsbEvIn  = " ++ showHS flags newindent (fsbEvIn  fsb)
          , "       , fsbEvOut = " ++ showHS flags newindent (fsbEvOut fsb)
          ,wrap
            "       , fsbConjs = " newindent' (\_->shConj) (fsbConjs  fsb)
          ,wrap
            "       , fsbECAs  = " newindent' (\_->showHSname) (fsbECAs  fsb)
          , "       }"
          ]
       where 
         newindent   = indent ++ "                   "
         newindent'  = newindent ++ " "
         newindent'' = newindent' ++ "    "
         shConj (r,conj) = "( "++showHSname r++newindent++"   , "++showHS flags newindent'' conj++newindent++"   )"

   instance ShowHS Clauses where
    showHSname _ = fatal 208 "Clauses is anonymous with respect to showHS flags."
    showHS flags indent c
      = intercalate indent
          [ "Clauses{ cl_conjNF = [ "++intercalate (ind++"  ") 
                                       [ "( "++showHS flags (ind++"    ") a++ind++"  , "++showHS flags (ind++"    ") b++ind++"  )"
                                       | (a,b)<-cl_conjNF c ]++ind++"]"
          , "       , cl_rule   = " ++ showHSname (cl_rule c)
          , "       }"
          ]
       where 
         ind = indent ++ "                     "

   instance ShowHS FTheme where
    showHSname _ = fatal 215 "FTheme is anonymous with respect to showHS flags."
    showHS flags indent tme 
     = intercalate newindent
            ["FTheme{ tconcept   = " ++ showHS flags newindent (tconcept tme)
            ,wrap  ", trules     = " indentA (\_->showHSname) (trules tme)
            ,      "}" 
            ]
            where newindent = indent ++"    "
                  indentA = newindent



-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fspc                          ***
-- \***********************************************************************
   instance ShowHS Fspc where
    showHSname fspec = haskellIdentifier ("fSpc_"++name fspec)
    showHS flags indent fspec
     = intercalate (indent ++"    ") 
           [ "Fspc{ fsName = " ++ show (name fspec)
           ,     ", fsLang        = " ++ show (fsLang fspec) ++ "  -- the default language for this specification"
           ,     ", themes        = " ++ show (themes fspec) ++ "  -- the names of themes to be printed in the documentation, meant for partial documentation.  Print all if empty..."
           ,wrap ", vprocesses    = " indentA (\_->showHSname) (vprocesses fspec)
           ,wrap ", vplugInfos    = " indentA (\_->showHS flags (indentA++"  ")) (vplugInfos fspec)
           ,wrap ", plugInfos     = " indentA (\_->showHS flags (indentA++"  ")) (plugInfos  fspec)
           ,     ", interfaceS    = interfaceS'"
           ,     ", interfaceG    = interfaceG'"
           ,     ", fSwitchboard  = "++showHS flags indentA (fSwitchboard fspec)
           ,     ", fActivities   = interfaceS'++interfaceG'"
           ,     ", fRoleRels     = " ++
                 case fRoleRels fspec of
                   []        -> "[]"
                   [(r,rel)] -> "[ ("++show r++", "++showHS flags "" rel++") ]"
                   _         -> "[ "++intercalate (indentA++", ") ["("++show r++","++showHS flags "" rel++")" | (r,rel)<-fRoleRels fspec]++indentA++"]"
           ,     ", fRoleRuls     = " ++
                 case fRoleRuls fspec of
                   []        -> "[]"
                   [(r,rul)] -> "[ ("++show r++", "++showHSname rul++") ]"
                   _         -> "[ "++intercalate (indentA++", ") ["("++show r++","++showHSname rul++")" | (r,rul)<-fRoleRuls fspec]++indentA++"]"
           ,wrap ", vrules        = " indentA (\_->showHSname) (vrules fspec)
           ,wrap ", grules        = " indentA (\_->showHSname) (grules fspec)
           ,wrap ", vkeys         = " indentA (\_->showHSname) (vkeys fspec)
           ,wrap ", vgens         = " indentA (showHS flags)   (vgens fspec)
           ,wrap ", vconjs        = " indentA (showHS flags)   (vconjs fspec)
           ,wrap ", vquads        = " indentA (\_->showHSname) (vquads fspec)
           ,wrap ", vEcas         = " indentA (\_->showHSname) (vEcas fspec)
           ,wrap ", vrels         = " indentA (\_->showHSname) (vrels fspec)
           ,     ", fsisa         = isa'"
           ,wrap ", vpatterns     = " indentA (\_->showHSname) (patterns fspec)
           ,     ", pictPatts     = []                                    -- Pictures are not in this generated file."
           ,wrap ", vConceptDefs  = " indentA (showHS flags)   (vConceptDefs fspec)
           ,wrap ", fSexpls       = " indentA (showHS flags)   (fSexpls fspec)
--           ,     ", fSexpls       = [ "++intercalate (indentA++", ") (map (showHS flags "") (fSexpls fspec))++"]" 
           ,     ", vctxenv       = vctxenv'"
           ,"}" 
           ] ++   
       indent++"where"++
       indent++" isa' = "++    showHS flags (indent ++ "        ") (fsisa fspec)++
       indent++" vctxenv'  = ("++showHS flags (indent ++ "         ") envExpr ++ ", bindings)"++
       indent++" bindings  = "++(if null bindings then "[]" else
                                 "[ "++intercalate (indentB++", ") (map showbinding bindings)++indentB++"]")++
       indent++" gE = genEq (typology isa')"++
        "\n -- ***Interfaces Specified in Ampersand script***: "++
       indent++" interfaceS' = "++(if null (interfaceS fspec) then "[]" else
                                 "[ "++intercalate (indentB++", ") (map showHSname (interfaceS fspec))++indentB++"]")++
        "\n -- ***Activities Generated by the Ampersand compiler ***: "++
       indent++" interfaceG' = "++(if null (interfaceG fspec) then "[]" else
                                 "[ "++intercalate (indentB++", ") (map showHSname (interfaceG fspec))++indentB++"]")++
--       (if null (plugs fspec ) then "" else "\n -- ***Patterns***: "++concat [indent++" "++showHSname p++indent++"  = "++showHS flags (indent++"    ") p |p<-patterns fspec ]++"\n")++

-- WHY?  staan hier verschillende lijstjes met interfaces?
-- BECAUSE! Een Ampersand engineer besteedt veel tijd om vanuit een kennismodel (lees: een graaf met concepten en relaties)
--          alle interfaces met de hand te verzinnen.
--          Je kunt natuurlijk ook een interfaces-generator aan het werk zetten, die een aantal interfaces klaarzet bij wijze
--          van steiger (scaffold). Dat bespaart een hoop werk. De functie interfaceG is zo'n generator.
--          Door de gegenereerde interfaces af te drukken, kun je dus heel snel Ampersand sourcecode maken met correct-vertaalbare interfaces.
--          Heb je eenmaal een goed werkend pakket interfaces, dan wil je wellicht alleen de door jezelf gespecificeerde interfaces
--          gebruiken. Dat gebeurt in interfaceS.

        "\n -- ***Interface definitions (both interfaceS and interfaceG, but each one exactly once. ***: "++  
       (if null 
            (uni (interfaceS fspec)  (interfaceG fspec)) then "" 
             else concat [indent++" "++showHSname s++indent++"  = "++showHS flags (indent++"    ") s |s<- (uni (interfaceS fspec)  (interfaceG fspec)) ]++"\n")++
 
        
       (if null (vrels fspec)     then "" else
        "\n -- *** Relations ***: "++
        concat [indent++" "++showHSname d++indent++"  = "++showHS flags (indent++"    ") d |d<- vrels fspec, decusr d]++"\n") ++
       (if null (vprocesses fspec ) then "" else
        "\n -- *** Processes ***: "++
        concat [indent++" "++showHSname p++indent++"  = "++showHS flags (indent++"    ") p |p<-vprocesses fspec ]++"\n")++
       (if null (vrules   fspec ) then "" else
        "\n -- *** User defined rules ***: "++
        concat [indent++" "++showHSname r++indent++"  = "++showHS flags (indent++"    ") r |r<-vrules     fspec ]++"\n")++        
       (if null (grules   fspec ) then "" else
        "\n -- *** Generated rules ***: "++
        concat [indent++" "++showHSname r++indent++"  = "++showHS flags (indent++"    ") r |r<-grules     fspec ]++"\n")++        
       (if null (interfaceG fspec ) then "" else
        "\n -- *** Generated interfaces ***: "++
        concat [indent++" "++showHSname ifc++indent++"  = "++showHS flags (indent++"    ") ifc |ifc<-interfaceG fspec ]++"\n")++        
       (if null (vquads fspec ) then "" else
        "\n -- *** Quads ***: "++
        concat [indent++" "++showHSname q++indent++"  = "++showHS flags (indent++"    ") q |q<-vquads     fspec ]++"\n")++
       (if null (vEcas fspec ) then "" else
        "\n -- *** ECA rules ***: "++
        concat [indent++" "++showHSname eca++indent++"  = "++showHS flags (indent++"    ") eca |eca<-vEcas fspec ]++"\n")++
       (if null (plugInfos fspec ) then "" else
        "\n -- *** PlugInfos ***: "++
        concat [indent++" "++showHSname p++indent++"  = "++showHS flags (indent++"    ") p |InternalPlug p<-plugInfos fspec ]++"\n")++
       (if null (vpatterns fspec) then "" else
        "\n -- *** Patterns ***: "++
        concat [indent++" "++showHSname pat++" gE"++indent++"  = "++showHS flags (indent++"    ") pat |pat<-vpatterns fspec]++"\n")
           where indentA = indent ++"                      "
                 indentB = indent ++"             "
                 (envExpr,bindings) = vctxenv fspec
                 showbinding :: (Declaration,String) -> String
                 showbinding (d,s)= "( "++showHS flags (indentB ++ "  ") d ++
                                    ", "++show s++") "
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: PlugInfo   ***
-- \***********************************************************************

   instance ShowHS PlugInfo where
    showHSname (InternalPlug p) = haskellIdentifier ("ipl_"++name p)
    showHSname (ExternalPlug _) = fatal 336 "a PlugInfo is anonymous with respect to showHS flags"
    showHS _ _ (InternalPlug p)
     = "InternalPlug "++showHSname p
    showHS flags ind (ExternalPlug o)
     = "ExternalPlug "++showHS flags (ind++"    ") o
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: RoleRelation   ***
-- \***********************************************************************

   instance ShowHS RoleRelation where
    showHSname _ = fatal 347 "a RoleRelation is anonymous with respect to showHS flags"
    showHS flags ind rr
     = "RR "++show (rrRoles rr)++" "++showHS flags (ind++"    ") (rrRels rr)++" "++showHS flags (ind++"    ") (rrPos rr)
   
   instance ShowHS RoleRule where
    showHSname _ = fatal 352 "a RoleRule is anonymous with respect to showHS flags"
    showHS flags ind rs
     = "Maintain "++show (mRoles rs)++" "++show (mRules rs)++" "++showHS flags (ind++"    ") (mPos rs)
   

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Field                         ***
-- \***********************************************************************

   instance ShowHS Field where
    showHSname fld = haskellIdentifier ("fld_"++fld_name fld)
    showHS flags indent fld
     = "Att "++       "{ fld_name     = "++                     show (fld_name     fld)
       ++ ( if null (fld_sub fld)
            then indent++"    , fld_sub      = []"
            else indent++"    , fld_sub      = [ "++
                 intercalate (indent++"                     , ")
                       [showHS flags (indent++"                       ") att | att<-fld_sub  fld]
                       ++indent++"                     ]" )
       ++ indent++"    , fld_expr     = "++showHS flags (indent++"                       ") (fld_expr fld)
       ++ indent++"    , fld_rel      = "++
          ( if fld_editable fld
            then showHS flags (indent++"      ") (fld_rel     fld)
            else "error(\"!Fatal: reference to undefined editrelation in field "++fld_name fld++"\")" )
       ++ indent++"    , fld_editable = "++show (fld_editable fld)
       ++ indent++"    , fld_list     = "++show (fld_list     fld)
       ++ indent++"    , fld_must     = "++show (fld_must     fld)
       ++ indent++"    , fld_new      = "++show (fld_new      fld)
       ++ indent++"    , fld_sLevel   = "++show (fld_sLevel   fld)
       ++ indent++"    , fld_insAble  = "++show (fld_insAble  fld)
       ++ indent++"    , fld_onIns    = "++
          ( if fld_insAble fld
            then showHSname (fld_onIns fld)
            else "error(\"!Fatal: reference to undefined insert action in field "++fld_name fld++"\")" )
       ++ indent++"    , fld_delAble  = "++                     show (fld_delAble  fld)
       ++ indent++"    , fld_onDel    = "++
          ( if fld_delAble fld
            then showHSname (fld_onDel fld)
            else "error(\"!Fatal: reference to undefined delete action in field "++fld_name fld++"\")" )
       ++ indent++"    }"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FSid                          ***
-- \***********************************************************************

   instance ShowHS FSid where
    showHSname (FS_id nm ) = haskellIdentifier nm 
    showHS _ _ (FS_id nm) 
      = "(FS_id " ++ show nm ++ ")"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Pattern                       ***
-- \***********************************************************************

   instance ShowHS Pattern where
    showHSname pat = haskellIdentifier ("pat_"++name pat)
    showHS flags indent pat
     = intercalate indentA
        [ "A_Pat { ptnm  = "++show (name pat)
        , ", ptpos = "++showHS flags "" (ptpos pat)
        , ", ptend = "++showHS flags "" (ptend pat)
        , if null (ptrls pat) then ", ptrls = [] -- no rules"
                              else ", ptrls = [" ++intercalate ", " [showHSname r | r<-ptrls pat] ++"]"
        , wrap ", ptgns = " indentB (showHS flags) (ptgns pat)
        , if null (ptdcs pat) then ", ptdcs = [] -- no declarations"
                              else ", ptdcs = [" ++intercalate          ", " [showHSname d | d<-ptdcs pat] ++"]"
        , wrap ", ptkds = " indentB (showHS flags) (ptkds pat)
        , wrap ", ptxps = " indentB (showHS flags) (ptxps pat)
        ] where indentA = indent ++"      "     -- adding the width of "A_Pat "
                indentB = indentA++"          " -- adding the width of ", ptrls = "

   instance ShowHS FProcess where
    showHSname prc = haskellIdentifier ("fprc_"++name (fpProc prc))
    showHS flags indent prc
     = intercalate indent
        [ "FProc { fpProc       = "++showHS flags (indent++"                     ") (fpProc prc)
        , "      , fpActivities = "++
           if null (fpActivities prc) 
           then "[] -- no activities"
           else " ["++intercalate (indent++"                      , ") [showHS flags (indent++"     ") a | a<-fpActivities prc] ++indent++"    ]"
        , "      }"
        ]
 
   instance ShowHS Process where
 -- TODO: showHS flags should generate valid Haskell code for the entire pattern. Right now, it doesn't
    showHSname prc = haskellIdentifier ("prc_"++name prc)
    showHS flags indent prc
     = intercalate indent
        [ "Proc { prcNm    = "++show (name prc)
--        , case prcGns prc of
--           []          -> "     , prcGns   = [] {- no generalizations -}"
--           [g]         -> "     , prcGns   = [ "++showHSname g++" ]"
--           gs          -> "     , prcGns   = [ "++intercalate (indent'++", ") [showHSname g | g<-gs]++indent'++"]"
--        , case prcDcs prc of
--           []          -> "     , prcDcs   = [] {- no declarations -}"
--           [d]         -> "     , prcDcs   = [ "++showHSname d++" ]"
--           ds          -> "     , prcDcs   = [ "++intercalate (indent'++", ") [showHSname d | d<-ds]++indent'++"]"
        , case prcRules prc of
           []          -> "     , prcRules = [] {- no rules -}"
           [r]         -> "     , prcRules = [ "++showHSname r++" ]"
           rs          -> "     , prcRules = [ "++intercalate (indent'++", ") [showHSname r | r<-rs]++indent'++"]"
        , case prcRRuls prc of
           []          -> "     , prcRRuls = [] {- no role-rule assignments -}"
           [(rol,rul)] -> "     , prcRRuls = [ ("++show rol++", "++showHSname rul++") ]"
           rs          -> "     , prcRRuls = [ "++intercalate (indent'++", ") ["("++show rol++", "++showHSname rul++")" | (rol,rul)<-rs] ++indent'++"]"
        , case prcRRels prc of
           []          -> "     , prcRRels = [] {- no role-relation assignments -}"
           [(rol,rel)] -> "     , prcRRels = [ ("++show rol++", "++showHS flags "" rel++") ]"
           rs          -> "     , prcRRels = [ "++intercalate (indent'++", ") ["("++show rol++", "++showHS flags "" rel++")" | (rol,rel)<-rs] ++indent'++"]"
        , case prcKds prc of
           []          -> "     , prcKds   = [] {- no key definitions -}"
           [k]         -> "     , prcKds   = [ "++showHS flags "" k++" ]"
           ks          -> "     , prcKds   = [ "++intercalate (indent'++", ") [showHS flags "" k | k<-ks] ++indent'++"]"
        , case prcXps prc of
           []          -> "     , prcXps   = [] {- no explanations -}"
           [e]         -> "     , prcXps   = [ "++showHS flags "" e++" ]"
           es          -> "     , prcXps   = [ "++intercalate (indent'++", ") [showHS flags "" e | e<-es] ++indent'++"]"
        , "     }"
        ]
       where indent'  = indent++"                  "

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Activity                         ***
-- \***********************************************************************

   instance ShowHS Activity where
    showHSname _ = fatal 469 "an Activity is anonymous with respect to showHS flags"
    showHS flags indent act = 
       intercalate indent
        [ "Act { actRule   = "++showHSname (actRule act)
        , case actTrig act of
           []  -> "      , actTrig   = [] {- no relations trigger this activity -}"
           [r] -> "      , actTrig   = [ "++showHS flags "" r++" ]"
           rs  -> "      , actTrig   = [ "++intercalate (indent'++", ") [showHS flags "" r | r<-rs] ++indent'++"]"
        , case actAffect act of
           []  -> "      , actAffect = [] {- no relations are affected by this activity -}"
           [r] -> "      , actAffect = [ "++showHS flags "" r++" ]"
           rs  -> "      , actAffect = [ "++intercalate (indent'++", ") [showHS flags "" r | r<-rs] ++indent'++"]"
        , case actQuads act of
           []  -> "      , actQuads  = [] {- no relations are affected by this activity -}"
           [q] -> "      , actQuads  = [ "++showHSname q++" ]"
           qs  -> "      , actQuads  = [ "++intercalate (indent'++", ") [showHSname q | q<-qs] ++indent'++"]"
        , case actEcas act of
           []  -> "      , actEcas   = [] {- no relations are affected by this activity -}"
           [e] -> "      , actEcas   = [ "++showHSname e++" ]"
           es  -> "      , actEcas   = [ "++intercalate (indent'++", ") [showHSname e | e<-es] ++indent'++"]"
        , "      , actFPA    = "++showHS flags "" (actFPA act)
        , case actPurp act of
           []  -> "      , actPurp   = [] {- no explanations in this activity -}"
           [e] -> "      , actPurp   = [ "++showHS flags "" e++" ]"
           es  -> "      , actPurp   = [ "++intercalate (indent'++", ") [showHS flags "" e | e<-es] ++indent'++"]"
        , "      }"
        ]
       where indent' = indent++"                    "

   instance ShowHS PPurpose where
    showHSname _ = fatal 499 "a PPurpose is anonymous with respect to showHS flags"
    showHS flags _ expla = 
       "PRef2 ("++showHS flags "" (pexPos     expla)++") "++
             "("++showHS flags "" (pexObj     expla)++") "++
             "("++showHS flags "" (pexMarkup  expla)++") "
                ++show (pexRefID expla)++" "
                
   instance ShowHS PRef2Obj where
    showHSname _ = fatal 508 "a PRef2Obj is anonymous with respect to showHS flags"
    showHS _ _ peObj
     = case peObj of 
            PRef2ConceptDef str     -> "PRef2ConceptDef " ++show str
            PRef2Declaration rel ps -> "PRef2Declaration "++show rel++if null (psign ps) then "" else show (psign ps)
            PRef2Rule str           -> "PRef2Rule "       ++show str
            PRef2KeyDef str         -> "PRef2KeyDef "     ++show str
            PRef2Pattern str        -> "PRef2Pattern "    ++show str
            PRef2Process str        -> "PRef2Process "    ++show str
            PRef2Interface str      -> "PRef2Interface "  ++show str
            PRef2Context str        -> "PRef2Context "    ++show str
            PRef2Fspc str           -> "PRef2Fspc "       ++show str
                           
   instance ShowHS Purpose where
    showHSname _ = fatal 522 "a Purpose is anonymous with respect to showHS flags"
    showHS flags _ expla = 
       "Expl "++showHS flags "" (explObj expla)++" "
              ++showHS flags "" (explMarkup  expla)++" "
              ++show (explRefId expla)++" "

   instance ShowHS ExplObj where
    showHSname _ = fatal 530 "a ExplObj is anonymous with respect to showHS flags"
    showHS _ {-flags-} _ {-i-} peObj = case peObj of                     -- SJ: names of variables commented out to prevent warnings.
             ExplConceptDef cd  -> "ExplConceptDef " ++showHSname cd
             ExplDeclaration d  -> "ExplDeclaration "++showHSname d
             ExplRule r         -> "ExplRule "       ++showHSname r
             ExplKeyDef kd      -> "ExplKeyDef "     ++showHSname kd
             ExplPattern str    -> "ExplPattern "    ++show str
             ExplProcess str    -> "ExplProcess "    ++show str
             ExplInterface str  -> "ExplInterface "  ++show str
             ExplContext str    -> "ExplContext "    ++show str
             ExplFspc str       -> "ExplFspc "       ++show str
           
   instance ShowHS P_Markup where
    showHSname _ = fatal 567 "showHSname undefined for Type 'P_Markup'"
    showHS _ indent m
      = intercalate indent 
        ["P_Markup{ mLang   = "++ show (mLang m)
        ,"        , mFormat = "++ show (mFormat m)
        ,"        , mString = "++ show (mString m)
        ,"        }"
        ]

   instance ShowHS A_Markup where
    showHSname _ = fatal 577 "showHSname undefined for Type 'A_Markup'"
    showHS _ indent m
      = intercalate indent 
        ["A_Markup{ amLang   = "++ show (amLang m)
        ,"        , amFormat = "++ show (amFormat m)
        ,"        , amPandoc = "++ show (amPandoc m)
        ,"        }"
        ]
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Rule                          ***
-- \***********************************************************************

   instance ShowHS Rule where
    showHSname r = haskellIdentifier ("rule_"++ rrnm r)
    showHS flags indent r   
      = intercalate indent 
        ["Ru{ rrnm  = " ++ show (rrnm r)
        ,"  , rrexp = ("++ showHS flags "" (rrexp r)++")"
        ,"  , rrfps = ("++ showHS flags "" (rrfps r)++")"
        ,"  , rrxpl = " ++ showHS flags "" (rrmean r)
        ,"  , rrtyp = " ++ showHS flags "" (rrtyp r)
        ,"  , rrtyp_proof = Nothing -- TBD generate a function for the proof."
        ,"  , rrdcl = " ++ case rrdcl r of
                            Nothing   -> "Nothing"
                            Just(p,d) -> "Just ("++showHS flags "" p++","++showHSname d++")"
        ,"  , r_env = " ++ show (r_env r)
        ,"  , r_usr = " ++ show (r_usr r)
        ,"  , r_sgl = " ++ show (r_sgl r)
        ,"  , srrel = " ++ showHSname (srrel r)
        ,"  }"
        ]

   instance ShowHS AMeaning where
     showHSname _ = fatal 610 "showHSname undefined for type 'AMeaning'"
     showHS flags indent (AMeaning x) = "AMeaning " ++ showHS flags indent x 

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: RuleType                      ***
-- \***********************************************************************
   instance ShowHS RuleType where
     showHSname _ = fatal 583 "showHSname undefined for Type 'RuleType'"
     showHS _ _ Truth          = "Truth"
     showHS _ _ Equivalence    = "Equivalence"
     showHS _ _ Implication    = "Implication"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: KeyDef                        ***
-- \***********************************************************************

   instance ShowHS KeyDef where
    showHSname kd = haskellIdentifier ("kDef_"++name kd)
    showHS flags indent kd
     = "Kd ("++showHS flags "" (kdpos kd)++") "++show (kdlbl kd)++" ("++showHS flags "" (kdcpt kd)++")"
       ++indent++"  [ "++intercalate (indent++"  , ") [showHS flags (indent++"    ") a |a<-kdats kd]++indent++"  ]"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: P_Population                    ***
-- \***********************************************************************

   instance  ShowHS Population where
    showHSname pop = haskellIdentifier ("pop_"++name rel++"_"++uniqueIDfromOrigin (relpos rel))
        where rel = popm pop
    showHS flags indent pop
     = "Popu ("++showHS flags "" (popm pop)++")"++indent++"     [ "++intercalate (indent++"     , ") (map show (popps pop))++indent++"     ]"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ObjectDef                     ***
-- \***********************************************************************

   instance ShowHS ObjectDef where
    showHSname obj = haskellIdentifier ("oDef_"++name obj)
    showHS flags indent r 
     = intercalate indent
           ["Obj{ objnm = " ++ show(objnm r)
       --  ,"   , objpos = " ++ showHS flags "" (objpos r)  -- This is not defined for every ObjectDef
           ,"   , objctx = " ++ showHS flags "" (objctx r)
           ,"   , objctx_proof = Nothing -- TBD: generation of proof in this haskell code."
           ,if null (objats r) then "   , objats = []" else
            "   , objats"++indent++"      = ["++intercalate (indent ++ "        ,")
                                                 (map (showHS flags (indent ++"         "))
                                                      (objats r))
                         ++indent++"        ]"
           ,"   , objstrs = " ++ show(objstrs r)
           ]++indent++"   }"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Interface                     ***
-- \***********************************************************************

   instance ShowHS Interface where
    showHSname obj = haskellIdentifier ("ifc_"++name obj)
    showHS flags indent ifc
     = intercalate indent 
           [ "Ifc { ifcName   = " ++ show(ifcName ifc)
           , "    , ifcParams = " ++ "["++intercalate ", " [showHS flags "" rel | rel<-ifcParams ifc] ++ "]"
           --, "    , ifcViols  = " ++ "["++intercalate ", " [showHSname rel | rel<-ifcViols ifc] ++ "]" -- TODO: uncomment when ifcViols is implemented
           , "    , ifcArgs   = " ++ show(ifcArgs ifc)
           , "    , ifcObj"++indent++"       = " ++ showHS flags (indent++"         ") (ifcObj ifc)
           , "    , ifcPos    = " ++ showHS flags "" (ifcPos ifc)
           , "    , ifcExpl   = " ++ show(ifcExpl ifc)
           ]++"    }"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Expression                    ***
-- \***********************************************************************

   instance ShowHS Expression where
    showHSname expr = fatal 651 $ "an expression is anonymous with respect to showHS flags. Detected at: "++ showADL expr
    showHS flags indent (EEqu (l,r))   = "EEqu ( "++showHS flags (indent++"       ") l++indent++"     , "++showHS flags (indent++"       ") r++indent++"     )"
    showHS flags indent (EImp (l,r))   = "EImp ( "++showHS flags (indent++"       ") l++indent++"     , "++showHS flags (indent++"       ") r++indent++"     )"
    showHS   _     _    (EIsc [])      = "EIsc [] {- True -}"
    showHS flags indent (EIsc [e])     = "EIsc ["++showHS flags (indent++"      ") e++"]"
    showHS flags indent (EIsc es)      = "EIsc [ "++intercalate (indent++"     , ") [showHS flags (indent++"       ") e | e<-es]++indent++"     ]"
    showHS   _     _    (EUni [])      = "EUni [] {- False -}"
    showHS flags indent (EUni [e])     = "EUni ["++showHS flags (indent++"      ") e++"]"
    showHS flags indent (EUni es)      = "EUni [ "++intercalate (indent++"     , ") [showHS flags (indent++"       ") e | e<-es]++indent++"     ]"
    showHS flags indent (EDif (l,r))   = "EDif ( "++showHS flags (indent++"       ") l++indent++"     , "++showHS flags (indent++"       ") r++indent++"     )"
    showHS flags indent (ELrs (l,r))   = "ELrs ( "++showHS flags (indent++"       ") l++indent++"     , "++showHS flags (indent++"       ") r++indent++"     )"
    showHS flags indent (ERrs (l,r))   = "ERrs ( "++showHS flags (indent++"       ") l++indent++"     , "++showHS flags (indent++"       ") r++indent++"     )"
    showHS   _     _    (ECps [])      = "ECps [] {- I -}"
    showHS flags indent (ECps [e])     = "ECps ["++showHS flags (indent++"     ") e++"]"
    showHS flags indent (ECps es)      = "ECps [ "++intercalate (indent++"     , ") [showHS flags (indent++"       ") e | e<-es]++indent++"     ]"
    showHS   _     _    (ERad [])      = "ERad [] {- -I -}"
    showHS flags indent (ERad [e])     = "ERad ["++showHS flags (indent++"     ") e++"]"
    showHS flags indent (ERad es)      = "ERad [ "++intercalate (indent++"     , ") [showHS flags (indent++"       ") e | e<-es]++indent++"     ]"
    showHS   _     _    (EPrd [])      = "EPrd [] {- ONE -}"
    showHS flags indent (EPrd [e])     = "EPrd ["++showHS flags (indent++"     ") e++"]"
    showHS flags indent (EPrd es)      = "EPrd [ "++intercalate (indent++"     , ") [showHS flags (indent++"       ") e | e<-es]++indent++"     ]"
    showHS flags indent (EKl0 e)       = "EKl0 ("++showHS flags (indent++"      ") e++")"
    showHS flags indent (EKl1 e)       = "EKl1 ("++showHS flags (indent++"      ") e++")"
    showHS flags indent (EFlp e)       = "EFlp ("++showHS flags (indent++"      ") e++")"
    showHS flags indent (ECpl e)       = "ECpl ("++showHS flags (indent++"      ") e++")"
    showHS flags indent (EBrk e)       = "EBrk ("++showHS flags (indent++"      ") e++")"
    showHS flags indent (ETyp e sgn)   = "ETyp ("++showHS flags (indent++"      ") e++") ("++showHS flags (indent++"    ") sgn++")"
    showHS flags   _    (ERel rel)     = "ERel ("++showHS flags "" rel++") "

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Sign                           ***
-- \***********************************************************************

   instance ShowHS Sign where
    showHSname sgn = fatal 702 $ "Illegal call to showHSname ("++show sgn++"). A signature gets no definition in Haskell code."
    showHS flags _ sgn = "Sign ("++showHS flags "" (source sgn)++") ("++showHS flags "" (target sgn)++")"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Gen                           ***
-- \***********************************************************************

   instance ShowHS A_Gen where
    showHSname g = fatal 677 $ "Illegal call to showHSname ("++showADL g++"). A GEN statement gets no definition in Haskell code."
    showHS flags _ gen = "Gen ("++showHS flags "" (genfp gen)++") ("++showHS flags "" (gengen gen)++") ("++showHS flags "" (genspc gen)++") "++show (genpat gen)
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Relation Concept            ***
-- \***********************************************************************

   instance  ShowHS Relation where
    showHSname rel = fatal 685 $ "Illegal call to showHSname ("++showADL rel++"). A Relation gets no definition in Haskell code."
    showHS flags _ rel 
       = case rel of
            Rel{} -> "Rel "++show (relnm rel)++" "++showPos
                         ++" "++showSign++" "++showHSname (reldcl rel)
            I{}   -> "I "++show1Typ
            V{}   -> "V "++showSgn
            Mp1{} -> "Mp1 "++relval rel++" "++show1Typ
  -- WHY wordt relval rel zonder quotes afgedrukt?
  -- BECAUSE: relval rel wordt door een lambda gebonden in de omgeving van Mp1. Het is dus een haskell identifier en niet een haskell string.
           where showPos  = "("++showHS flags "" (origin rel)++")"
                 showSign = "("++showHS flags "" (relsgn  rel)++")"
                 show1Typ = "("++showHS flags "" (rel1typ rel)++")"
                 showSgn  = "("++showHS flags "" (reltyp  rel)++")"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Declaration                   ***
-- \***********************************************************************

   instance ShowHS Declaration where
    showHSname d | decusr d  = haskellIdentifier ("rel_"++name d++name (source d)++name (target d)) -- user defined relations
                 | deciss d  = haskellIdentifier ("sgn_"++name d++name (source d)++name (target d)) -- relations generated for signalling
                 | otherwise = haskellIdentifier ("vio_"++name d++name (source d)++name (target d)) -- relations generated per rule
    showHS flags indent d 
       = case d of 
          Sgn{}     -> intercalate indent
                        ["Sgn{ decnm   = " ++ show (decnm d)
                        ,"   , decsgn  = " ++ showHS flags "" (sign d)
                        ,"   , decprps = " ++ showL(map (showHS flags "") (decprps d))
                        ,"   , decprps_calc = " ++ showL(map (showHS flags "") (decprps_calc d))
                        ,"   , decprL  = " ++ show (decprL d)
                        ,"   , decprM  = " ++ show (decprM d)
                        ,"   , decprR  = " ++ show (decprR d)
                        ,"   , decMean = " ++ show (decMean d)
                        ,"   , decpopu = " ++ show (decpopu d)
                        ,"   , decfpos = " ++ showHS flags "" (decfpos d)
                        ,"   , deciss  = " ++ show (deciss d)
                        ,"   , decusr  = " ++ show (decusr d)
                        ,"   , decpat  = " ++ show (decpat d)
                        ]++"}"
          Isn{}     -> "Isn{ detyp   = " ++ showHS flags "" (detyp d)++"}"
          Iscompl{} -> "Iscompl{ detyp   = " ++ showHS flags "" (detyp d)++"}"
          Vs{}      -> "Vs { decsgn  = " ++ showHS flags "" (sign d)++"}"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ConceptDef                    ***
-- \***********************************************************************

   instance ShowHS ConceptDef where
    showHSname cd = haskellIdentifier ("cDef_"++cdcpt cd)
    showHS flags _ cd
     = " Cd ("++showHS flags "" (cdpos cd)++") "++cdcpt cd++" "++show (cdplug cd)++" "++show (cddef cd)++" "++show (cdtyp cd)++" "++show (cdref cd)

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Concept                     ***
-- \***********************************************************************

   instance ShowHS A_Concept where
    showHSname c = fatal 757 $ "Illegal call to showHSname ("++name c++"). A concept gets no definition in Haskell code."
    showHS _ _ c = case c of
                       C{} -> "C "++show (name c) ++ " gE []"    -- contents not shown. Adapt this code if you must see the contents too.
                       ONE -> "ONE"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FPA                           ***
-- \***********************************************************************
   
   instance ShowHS FPcompl where
    showHSname c = fatal 768 $ "Illegal call to showHSname ("++show c++"). A FPcompl gets no definition in Haskell code."
    showHS _ _   = show

   instance ShowHS FPA where
    showHSname c = fatal 772 $ "Illegal call to showHSname ("++show c++"). A FPA gets no definition in Haskell code."
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
    showHSname p = fatal 785 $ "should not showHS flags the name of multiplicities (Prop): "++show p
    showHS _ _ Uni = "Uni"
    showHS _ _ Inj = "Inj"
    showHS _ _ Sur = "Sur"
    showHS _ _ Tot = "Tot"
    showHS _ _ Sym = "Sym"
    showHS _ _ Asy = "Asy"
    showHS _ _ Trn = "Trn"
    showHS _ _ Rfx = "Rfx"
    showHS _ _ Irf = "Irf"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FilePos                       ***
-- \***********************************************************************

   instance ShowHS FilePos where
    showHSname p = fatal 801 $ "Illegal call to showHSname ("++show p++"). A FilePos is an anonymous entity in Haskell code."
    showHS _ _ (FilePos (fn,DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner.Pos l c,sym))
      = "FilePos ("++show fn++",Pos "++show l++" "++show c++","++show sym++")"

   instance ShowHS Origin where
    showHSname p = fatal 806 $ "Illegal call to showHSname ("++show p++"). A Origin is an anonymous entity in Haskell code."
    showHS flags indent (FileLoc l) = "FileLoc (" ++ showHS flags indent l++")"
    showHS _ _ (DBLoc l) = "DBLoc " ++ l
    showHS _ _ (Origin s) = "Origin " ++ s
    showHS _ _ OriginUnknown
      = "OriginUnknown"

   uniqueIDfromOrigin :: Origin -> String
   uniqueIDfromOrigin (FileLoc (FilePos (fn,DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner.Pos l _,_)))
     = show fn++show l
   uniqueIDfromOrigin (Origin s) =  s
   uniqueIDfromOrigin _          = fatal 868 "Cannot make a unique id from argument"
   

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Block                         ***
-- \***********************************************************************

   instance ShowHS Block where
    showHSname b = fatal 817 $ "Illegal call to showHSname ("++show b++"). A Block is an anonymous entity in Haskell code."
    showHS _ _   = show

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Inline                        ***
-- \***********************************************************************

   instance ShowHS Inline where
    showHSname i = fatal 825 $ "Illegal call to showHSname ("++show i++"). An Inline is an anonymous entity in Haskell code."
    showHS _ _   = show

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: InfTree                       ***
-- \***********************************************************************
--   instance ShowHS InfTree where
--    showHSname itree = fatal 832 $ "Illegal call to showHSname ("++show itree++"). An inference tree is an anonymous entity in Haskell code."
--    showHS flags indent itree =
--        case itree of
--          InfExprs irt (ratype,raobj) itrees -> 
--              "InfExprs " ++ showHS flags indent irt ++ 
--              indent ++ "   (" ++ showRaType ratype ++ "," ++ "RelAlgObj{-"++show raobj++"-}" ++ ")" ++
--              indent ++ showHS flags (indent ++ "     ") itrees
--          InfRel drt ratype _ _ -> 
--              "InfRel " ++ showHS flags indent drt ++ " " ++ showRaType ratype
--      where 
--       showRaType rat = "RelAlgType{-"++show rat++"-}"
--
--   instance ShowHS RelDecl where
--    showHSname d = fatal 845 $ "Illegal call to showHSname ("++show d++"). A declared ruletype is an anonymous entity in Haskell code."
--    showHS _ indent d = case d of 
--                          RelDecl{}-> "RelDecl{ dname  = " ++ show (dname d) ++ indent
--                                   ++ "        ,dtype  = " ++ showRaType dtype ++ indent
--                                   ++ "        ,isendo = " ++ show (isendo d) 
--                          IDecl    -> "IDecl"
--                          VDecl    -> "VDecl"
--      where 
--       showRaType _ = "RelAlgType{- ++TODO++ -}"
--
--
--   instance ShowHS DeclRuleType where
--    showHSname drt = fatal 857 $ "Illegal call to showHSname ("++show drt++"). A declared ruletype is an anonymous entity in Haskell code."
--    showHS _ _ drt = case drt of
--                                         D_rel     -> "D_rel"
--                                         D_rel_h   -> "D_rel_h"
--                                         D_rel_c   -> "D_rel_c"
--                                         D_rel_c_h -> "D_rel_c_h"
--                                         D_id      -> "D_id"
--                                         D_v       -> "D_v"
--                                         D_id_c    -> "D_id_c"
--                                         D_v_c     -> "D_v_c"
--                        
--   instance ShowHS InfRuleType where
--    showHSname irt = fatal 869 $ "Illegal call to showHSname ("++show irt++"). A inference ruletype is an anonymous entity in Haskell code."
--    showHS _ _ irt = case irt of
--                                         ISect_cs  -> "ISect_cs"
--                                         ISect_ncs -> "ISect_ncs"
--                                         ISect_mix -> "ISect_mix"
--                                         Union_mix -> "Union_mix"
--                                         Comp_ncs  -> "Comp_ncs"
--                                         Comp_c1   -> "Comp_c1"
--                                         Comp_c2   -> "Comp_c2"
--                                         Comp_cs   -> "Comp_cs"
--                                         RAdd_ncs  -> "RAdd_ncs"
--                                         RAdd_c1   -> "RAdd_c1"
--                                         RAdd_c2   -> "RAdd_c2"
--                                         RAdd_cs   -> "RAdd_cs"
--                                         Conv_nc   -> "Conv_nc"
--                                         Conv_c    -> "Conv_c"
                                           
-- \***********************************************************************
-- \*** hulpfuncties                                                    ***
-- \***********************************************************************

   haskellIdentifier :: String -> String
   haskellIdentifier cs = unCap (hsId cs)
    where
      hsId ('_': cs')             = '_': hsId cs'
      hsId (c:cs') | isAlphaNum c = c: hsId cs'
                   | otherwise    = hsId cs'
      hsId ""                     = ""

   showL   :: [String] -> String
   showL xs = "["++intercalate "," xs++"]"


{- Obsolete?
   instance ShowHS P_Relation where
    showHSname rel = fatal 709 $ "Illegal call to showHSname ("++showADL rel++"). A P_Relation gets no definition in Haskell code."
    showHS flags _ rel 
       = case rel of
            P_Rel{} -> "P_Rel "++show (name rel)++" "++showPos
            P_I{}   -> "P_I"
            P_V{}   -> "P_V"
            P_Mp1{} -> "P_Mp1 "++rel_1val rel
  -- WHY is relval rel printed without quotes?
  -- BECAUSE: relval rel will be bound by a lambda in the environment of Mp1. It is a Haskell identifier and not a Haskell string.
  -- COMMENT: That cannot be correct, because the author of an Ampersand script may write an atom that is not a Haskell identifier. Then what?  TODO!
           where showPos  = "("++showHS flags "" (origin rel)++")"

   instance ShowHS P_Concept where
    showHSname c = fatal 793 $ "Illegal call to showHSname ("++name c++"). A concept gets no definition in Haskell code."
    showHS _ _ c = case c of
                       PCpt{}    -> "PCpt "++show (name c)
                       P_Singleton -> "P_Singleton "

-}


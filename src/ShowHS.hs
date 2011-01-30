{-# OPTIONS_GHC -Wall -XUndecidableInstances -XFlexibleContexts -XFlexibleInstances #-}
module ShowHS (ShowHS(..),fSpec2Haskell,haskellIdentifier)
where

   import Char                  (isAlphaNum)
   import DatabaseDesign.Ampersand.Core.Basics
   import Data.Plug
   import Data.Fspec
   import Data.List
   import DatabaseDesign.Ampersand.ADL1
   import qualified UU_Scanner
  --          (Pos(..) 
   import ShowADL               (ShowADL(..))--,showADLcode) -- wenselijk voor foutmeldingen.
   import Options hiding (services)
   import Version               (versionbanner)
   import FPA                   (FPA(..),FPcompl,fpa)
--   import TypeInference.InfLibAGFuncs
      
   fSpec2Haskell :: Fspc -> Options -> String
   fSpec2Haskell fSpec flags
           = "{-# OPTIONS_GHC -Wall #-}"
             ++"\n{-Generated code by "++versionbanner++" at "++show (genTime flags)++"-}"
             ++"\nmodule Main where"
             ++"\n  import UU_Scanner"
             ++"\n  import ADL"
             ++"\n  import ShowHS (showHS)"
             ++"\n  import Data.Fspec"
             ++"\n  import Data.Plug"
             ++"\n  import FPA"
             ++"\n  import Options (getOptions)"
             ++"\n  import TypeInference.InfLibAGFuncs"
             ++"\n  import DatabaseDesign.Ampersand.Core.Basics"  
             ++"\n  import Prelude hiding (writeFile,readFile,getContents,putStr,putStrLn)"
             ++"\n"
             ++"\n  main :: IO ()"
             ++"\n  main = do flags <- getOptions"
             ++"\n            putStr (showHS flags \"\\n  \" fSpec_"++baseName flags++")"
             ++"\n"++"{- \n"++show [x|p<-vpatterns fSpec,x<-testexpr p]++show [x|p<-vpatterns fSpec,x<-inftestexpr p] ++ "\n-}\n"
             ++"\n  fSpec_"++baseName flags++" :: Fspc"
             ++"\n  fSpec_"++baseName flags++"\n   = "++showHS flags "\n     " fSpec


   wrap :: String->String->(String->a->String)->[a]->String
   wrap initStr indent f xs
    = initStr++
      case xs of
        []  -> "[]"
        [x] -> "[ "++f (indent++"  ") x++" ]"
        _   -> "[ "++intercalate (indent++", ") [f (indent++"  ") x| x<-xs]++indent++"]"

   class ShowHS a where
    showHSname :: a -> String
    showHS     :: Options -> String -> a -> String

   instance ShowHS a => ShowHS [a] where
    showHSname xs = "["++intercalate "," (map showHSname xs)++"]"
    showHS flags indent xs = wrap "" (indent++" ") (showHS flags) xs
    
   instance ShowHS a => ShowHS (Maybe a) where
    showHSname Nothing  = "Nothing"
    showHSname (Just x) = showHSname x
    showHS _ _ Nothing  = "Nothing"
    showHS flags indent (Just x) = showHS flags indent x

   instance ShowHS a => ShowHS (Inheritance a) where
    showHSname _ = error ("!Fatal (module ShowHS 68): every inheritance is anonymous with respect to showHS flags.")
    showHS flags indent (Isa ts cs) = "Isa "++showL ["("++showHS flags "" g++","++showHS flags "" s++")"|(g,s)<-ts] ++indent++"    "++ showL (map (showHS flags "") cs)


   -- | The following is used to showHS flags for signs: (Concept, Concept)
   instance (ShowHS a , ShowHS b) => ShowHS (a,b) where
    showHSname _ = error ("!Fatal (module ShowHS 74): Tuples of Concepts are anonymous with respect to showHS flags.")
    showHS flags indent (a,b) = "("++showHS flags indent a++","++showHS flags indent b++")"
    
   

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Plug                          ***
-- \***********************************************************************
   instance ShowHS PlugSQL where
    showHSname plug = haskellIdentifier ("plug_"++name plug)
    showHS flags indent plug   
      = case plug of
          TblSQL{} -> (intercalate indent 
                      ["let " ++ intercalate (indent++"    ")
                                             [showHSname f++indent++"     = "++showHS flags (indent++"       ") f| f<-fields plug] ++indent++"in"
                      ,"TblSQL{ sqlname = " ++ (show.haskellIdentifier.name) plug
                      ,"      , fields  = ["++intercalate ", " (map showHSname (fields plug))++"]"
                      ,"      , cLkpTbl = [ "++intercalate (indent++"                   , ") ["("++showHS flags "" c++", "++showHSname cn++")"| (c,cn)<-cLkpTbl plug] ++ "]"
                      ,"      , mLkpTbl = [ "++intercalate (indent++"                   , ") ["("++showHS flags "" m++", "++showHSname ms++", "++showHSname mt++")"| (m,ms,mt)<-mLkpTbl plug] ++ "]"
                      ,"      , sqlfpa  = " ++ showHS flags "" (fpa plug)
                      ,"      }"
                      ])
          BinSQL{} -> (intercalate indent 
                      ["let " ++ showHSname (fst (columns plug))++indent++"     = "++showHS flags (indent++"       ") (fst (columns plug))
                              ++ (indent++"    ") ++ showHSname (snd (columns plug))++indent++"     = "++showHS flags (indent++"       ") (snd (columns plug))
                              ++indent++"in"
                      ,"BinSQL{ sqlname = " ++ (show.haskellIdentifier.name) plug
                      ,"      , columns = ("++(showHSname (fst (columns plug)))++ ", " ++ (showHSname (snd (columns plug)))++")"
                      ,"      , cLkpTbl = [ "++intercalate (indent++"                   , ") ["("++showHS flags "" c++", "++showHSname cn++")"| (c,cn)<-cLkpTbl plug] ++ "]"
                      ,"      , mLkp = "++showHS flags "" (mLkp plug)
                      ,"      , sqlfpa  = " ++ showHS flags "" (fpa plug)
                      ,"      }"
                      ])
          ScalarSQL{} -> (intercalate indent 
                      ["ScalarSQL{ sqlname = " ++ (show.haskellIdentifier.name) plug
                      ,"         , column = "++showHS flags "" (column plug)
                      ,"         , cLkpTbl = "++showHS flags "" (cLkp plug)
                      ,"         , sqlfpa  = " ++ showHS flags "" (fpa plug)
                      ,"         }"
                      ])
   


   instance (ShowHS (Relation c), ShowHS (Expression (Relation c))) => ShowHS (ECArule c) where
    showHSname r = "ecaRule"++show (ecaNum r)
    showHS flags indent r   
      = "ECA (" ++ showHS flags "" (ecaTriggr r)++")" ++
        indent++"    (" ++ showHS flags (indent++"     ")  (ecaDelta r)++")"++
        indent++"    (" ++ showHS flags (indent++"     ")  (ecaAction r)++indent++"    )" ++
        indent++show (ecaNum r)

   instance (ShowHS (Relation c)) => ShowHS (Event c) where
    showHSname _ = error ("!Fatal (module ShowHS 165): \"Event\" is anonymous with respect to showHS flags.")
    showHS flags indent e   
      = if take 1 indent == "\n"
        then "On " ++ show (eSrt e)++indent++"   (" ++ showHS flags (indent++"    ") (eMhp e)++indent++"   )"
        else "On " ++ show (eSrt e)++" (" ++ showHS flags "" (eMhp e)++")"

   instance (ShowHS r, ShowHS (Expression r)) => ShowHS (PAclause r) where
    showHSname _ = error ("!Fatal (module ShowHS 172): \"PAclause\" is anonymous with respect to showHS flags.")
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
              showMotiv ind (conj,rs) = "("++showHS flags ind conj++", "++showHSname rs++")"

   instance ShowHS SqlField where
    showHSname sqFd = haskellIdentifier ("sqlFld_"++fldname sqFd)
    showHS flags indent sqFd
      = (intercalate indent
          [ "Fld { fldname = " ++ show (fldname sqFd)
          , "    , fldexpr = " ++ showHS flags "" (fldexpr sqFd)
          , "    , fldtype = " ++ showHS flags "" (fldtype sqFd)
          , "    , fldnull = " ++ show (fldnull sqFd) -- can there be empty field-values?
          , "    , flduniq = " ++ show (flduniq sqFd) -- are all field-values unique?
          , "    , fldauto = " ++ show (fldauto sqFd) -- is the field auto increment?
          , "    }"
          ])

   instance ShowHS SqlType where
    showHSname _ = error ("!Fatal (module ShowHS 218): SqlType is anonymous with respect to showHS flags.")
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
    showHSname _ = error ("!Fatal (module ShowHS 232): Quad is anonymous with respect to showHS flags.")
    showHS flags indent q 
      = (intercalate indent
          [ "Quad{ qMorph   = " ++ showHS flags newindent (qMorph q)
          , "    , qClauses = " ++ showHS flags newindent (qClauses q)
          , "    }"
          ])
       where 
         newindent = indent ++ "                 "
         
   instance ShowHS Clauses where
    showHSname _ = error ("!Fatal (module ShowHS 243): Clauses is anonymous with respect to showHS flags.")
    showHS flags indent c
      = (intercalate indent
          [ "Clauses{ cl_conjNF = " ++ showHS flags newindent (cl_conjNF c)
          , "       , cl_rule   = " ++ showHS flags newindent (cl_rule c)
          , "       }"
          ])
       where 
         newindent = indent ++ "                     "
         
   instance ShowHS FTheme where
    showHSname _ = error ("!Fatal (module ShowHS 254): FTheme is anonymous with respect to showHS flags.")
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
            ["Fspc{ fsName = " ++ show (name fspec)
--                  ,wrap ", vplugs        = " indentA (\_->showHSname) (vplugs fspec)
--                  ,wrap ", plugs         = " indentA (\_->showHSname) (plugs fspec)
                  ,     ", serviceS      = serviceS'"
                  ,     ", serviceG      = serviceG'"
                  ,wrap ", services      = " indentA (\_->showHSname) (services fspec)
             --   ,wrap ", roleServices  = " indentA (showHS flags)   (roleServices fspec)
             --   ,wrap ", mayEdit       = " indentA (showHS flags)   (mayEdit fspec)
                  ,     ", roleServices  = " ++
                        case roleServices fspec of
                          []      -> "[]"
                          [(r,s)] -> "[ ("++show r++", "++show s++") ]"
                          _       -> "[ "++intercalate (indentA++", ") ["("++show r++","++show s++")"| (r,s)<-roleServices fspec]++indentA++"]"
                  ,     ", mayEdit       = " ++
                        case mayEdit fspec of
                          []      -> "[]"
                          [(r,m)] -> "[ ("++show r++", "++showHS flags (indentA++"  ") m++") ]"
                          _       -> "[ "++intercalate (indentA++", ") ["("++show r++","++showHS flags (indentA++"  ") m++")"| (r,m)<-mayEdit fspec]++indentA++"]"
                  ,wrap ", vrules        = " indentA (\_->showHSname) (vrules fspec)
                  ,wrap ", grules        = " indentA (\_->showHSname) (grules fspec)
                  ,wrap ", vkeys         = " indentA (\_->showHSname) (vkeys fspec)
                  ,wrap ", vgens         = " indentA (showHS flags)   (vgens fspec)
                  ,wrap ", vconjs        = " indentA (showHS flags)   (vconjs fspec)
                  ,wrap ", vquads        = " indentA (showHS flags)   (vquads fspec)
                  ,wrap ", vrels         = " indentA (\_->showHSname) (vrels fspec)
                  ,     ", fsisa         = isa'"
                  ,wrap ", vpatterns     = " indentA (\_->showHSname) (patterns fspec)
                  ,     ", pictPatts     = []                                    -- Pictures are not in this generated file."
                  ,wrap ", vConceptDefs  = " indentA (showHS flags)   (vConceptDefs fspec)
                  ,     ", fSexpls       = [ "++intercalate (indentA++", ") (map (showHS flags "") (fSexpls fspec))++"]" 
                  ,     ", vctxenv       = vctxenv'"
                  ,"}" 
                  ] ++   
       indent++"where"++
       indent++" isa' = "++    showHS flags (indent ++ "        ") (fsisa fspec)++
       indent++" vctxenv' = ("++showHS flags (indent ++ "        ") envExpr ++ ", bindings)"++
       indent++" bindings = "++(if null bindings then "[]" else
                                 "[ "++intercalate (indentB++", ") (map showbinding bindings)++indentB++"]")++
       
       
       indent++" gE = genEq (typology isa')"++
--       (if null (plugs fspec) then "" else "\n -- ***PLUGS***: "++concat [indent++" "++showHSname p++indent++"  = "++showHS flags (indent++"    ") p|p<-plugs fspec ]++"\n")++
        
        "\n -- ***Services Specified in Ampersand script***: "++
       indent++" serviceS' = "++(if null (serviceS fspec) then "[]" else
                                 "[ "++intercalate (indentB++", ") (map (showHS flags indentB) (serviceS fspec))++indentB++"]")++
        "\n -- ***Services Generated by the Ampersand compiler ***: "++
       indent++" serviceG' = "++(if null (serviceG fspec) then "[]" else
                                 "[ "++intercalate (indentB++", ") (map (showHS flags indentB) (serviceG fspec))++indentB++"]")++
--       (if null (plugs fspec ) then "" else "\n -- ***Patterns***: "++concat [indent++" "++showHSname p++indent++"  = "++showHS flags (indent++"    ") p|p<-patterns fspec ]++"\n")++

-- WHY?  staan hier verschillende lijstjes met services?
-- BECAUSE!  Een Ampersand engineer besteedt veel tijd om vanuit een kennismodel (lees: een graaf met concepten en relaties)
--          alle services met de hand te verzinnen.
--          Je kunt natuurlijk ook een services-generator aan het werk zetten, die een aantal services klaarzet bij wijze
--          van steiger (scaffold). Dat bespaart een hoop werk. De functie serviceG is zo'n generator.
--          Door de gegenereerde services af te drukken, kun je dus heel snel Ampersand sourcecode maken met correct-vertaalbare services.
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
        "\n -- ***User defined of RULES ***: "++
        concat [indent++" "++showHSname r++indent++"  = "++showHS flags (indent++"    ") r|r<-vrules   fspec ]++"\n")++        
       (if null (grules   fspec ) then "" else
        "\n -- ***generated RULES ***: "++
        concat [indent++" "++showHSname r++indent++"  = "++showHS flags (indent++"    ") r|r<-grules   fspec ]++"\n")++        
       (if null (vrels fspec)     then "" else
        "\n -- ***Declarations OF RELATIONS ***: "++
        concat [indent++" "++showHSname d++indent++"  = "++showHS flags (indent++"    ") d|d<- vrels fspec]++"\n")++
--        "\n -- ***PATTERNS***: "++
----       (if null (fspc_patterns fspec) then "" else concat ["\n\n   "++showHSname pat++" gE"++"\n>   = "++showHS flags "\n>     " pat|pat<-fspc_patterns fspec]++
        "\n"
           where indentA = indent ++"                      "
                 indentB = indent ++"              "
                 (envExpr,bindings) = vctxenv fspec
                 showbinding :: (Declaration Concept,String) -> String
                 showbinding (d,s)= "( "++showHS flags (indentB ++ "  ") d ++
                                    ", "++show s++") "

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: RoleService en RoleRelation   ***
-- \***********************************************************************

   instance ShowHS RoleService where
    showHSname _ = error ("!Fatal (module ShowHS 366): a RoleService is anonymous with respect to showHS flags")
    showHS flags ind rs
     = " RS "++show (rsRoles rs)++" "++show (rsServices rs)++" "++showHS flags (ind++"    ") (rsPos rs)
   
   instance ShowHS RoleRelation where
    showHSname _ = error ("!Fatal (module ShowHS 371): a RoleRelation is anonymous with respect to showHS flags")
    showHS flags ind rr
     = " RR "++show (rrRoles rr)++" "++showHS flags (ind++"    ") (rrRels rr)++" "++showHS flags (ind++"    ") (rrPos rr)
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fservice                         ***
-- \***********************************************************************

   instance ShowHS Fservice where
    showHSname fservice = haskellIdentifier ("svc_"++name fservice)
    showHS flags indent fservice
     = "Fservice{ -- The declaration of a service, which was either specified by the programmer or generated by the compiler:\n"
       ++ newindent++" fsv_svcdef = ("++showHS flags (newindent++"      ") (fsv_svcdef fservice)++")"
       ++ "\n-- The relations in which the user may insert elements:"
       ++ newindent++",fsv_insrels  = "
       ++ (if null (fsv_insrels fservice)
                   then "[] -- This service has no relations into which elements are inserted"
                   else "[ "++intercalate (newindent++"    , ") (map (showHS flags (newindent++"      ")) (fsv_insrels  fservice))
                        ++newindent++"    ]")
       ++ "\n-- The relations from which the user may remove elements:"
       ++ newindent++",fsv_delrels  = "
       ++ (if null (fsv_delrels fservice)
                   then "[] -- This service has no relations from which elements are removed."
                   else "[ "++intercalate (newindent++"    , ") (map (showHS flags (newindent++"      ")) (fsv_delrels  fservice))
                        ++newindent++"    ]")
       ++ "\n-- All rules that may be affected by this service:"
-- TODO: ShowHS Fservice weer op z'n voeten zetten.
--      ++ newindent++",fsv_rules    = "
--      ++ (if null (fsv_ecaRules fservice)
--                  then "[] -- This service uses no ECA-rules"
--                  else "[ "++intercalate (newindent++"    , ")
--         [showHSname r ++ if null (selfExplain flags r) then "" else "    -"++"- " ++ show (selfExplain flags r)| r<-fsv_rules fservice])
--      ++newindent++"    ]"
       ++ "\n-- The Quads that are used to make a switchboard. (generated by ADL2Fspec)"
       ++ newindent++",fsv_quads    = "
       ++ (if null (fsv_quads fservice)
                   then "[] -- This service has no quads"
                   else "[ "
                          ++intercalate (newindent++"    , ") (map (showHS flags (newindent++"       ")) (fsv_quads  fservice))
                          ++newindent++"    ]")
       ++ newindent++",fsv_ecaRules = "
       ++ (if null (fsv_ecaRules fservice)
                   then "[] -- This service uses no ECA-rules"
                   else "\n-- ECA-rules that may be used by this service to restore invariants. (generated by ADL2Fspec)"
                        ++ newindent++"    [ "
                        ++intercalate (newindent++"    , ") (map showHSname [r delt|r<-fsv_ecaRules fservice])
                        ++newindent++"    ]")
       ++ newindent++",fsv_signals  = "
       ++ (if null (fsv_signals fservice)
                   then "[] -- There are no signals visible in this service"
                   else "\n-- All signals that are visible in this service:"
                        ++ newindent++"     [ "++intercalate (newindent++"     , ") (map showHSname (fsv_signals fservice))++newindent++"     ]")
       ++ newindent++",fsv_fields   = "
       ++ (if null (fsv_fields fservice)
                   then "[] -- There are no fields/parameters in this service"
                   else   " -- All fields/parameters of this service:"
                          ++ newindent++"     [ "
                          ++intercalate (newindent++"     , ") (map (showHS flags (newindent++"       ")) (fsv_fields   fservice))
                          ++newindent++"     ]")
       ++ newindent++",fsv_creating = "
       ++ (if null (fsv_creating fservice)
                   then "[] -- There are no concepts of which this service can create new instances"
                   else   " -- All concepts of which this service can create new instances"
                          ++ newindent++"     [ "
                          ++intercalate (newindent++"     , ") (map (showHS flags (newindent++"       ")) (fsv_creating fservice))
                          ++newindent++"     ]")
       ++ newindent++",fsv_deleting = "
       ++ (if null (fsv_deleting fservice)
                   then "[] -- There are no concepts of which this service can delete instances"
                   else   " -- All concepts of which this service can delete instances"
                          ++ newindent++"     [ "
                          ++intercalate (newindent++"     , ") (map (showHS flags (newindent++"       ")) (fsv_deleting fservice))
                          ++newindent++"     ]")
       ++ "-- function point assessment of this service:"
       ++ newindent++",fsv_fpa      = ("++showHS flags (newindent++"      ") (fsv_fpa fservice)++")"
       ++ newindent++"}"
       ++ ( if null (fsv_ecaRules fservice)
            then ""
            else newindent++"where"                                                            ++
                 newindent++"--  vis        = rd (map makeInline rels++map (mIs.target) rels)"   ++
                 newindent++"--  visible m  = makeInline m `elem` vis"                           ++
                 newindent++"--  qs         = quads visible (rules fSpec)"                       ++
                 "\n -- *** ECA rules ***: "++concat [newindent++"  "++showHSname (r delt)++" _ {- TODO: Replace '_' with 'delta' and use it.-} "
                                                    ++newindent++"   = "++showHS flags (newindent++"     ") (r delt)
                                                     |r<-fsv_ecaRules fservice ]
          )
       ++ newindent++" -- Einde Fservice "++showHSname fservice
       where
        newindent = indent ++ "        "
        delt=error("!Fatal (module ShowHS 460): illegal reference to argument of ECA rule")
--        showQ (m, shs,conj,r)
--         = "" --TODO "\nQuad:\nmorphism: "++showHSname m++":\nshifts: "++concat ["\n"++showADLcode fSpec s|s<-shs]++"\nconjunct: "++showADLcode fSpec conj++"\nrule: "++showADLcode fSpec r++""


-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Field                         ***
-- \***********************************************************************

   instance ShowHS Field where
    showHSname fld = "fld_" ++ (fld_name fld)
    showHS flags indent fld
     = "Att "++       "{ fld_name     = "++                     show (fld_name     fld)
       ++ ( if null (fld_sub fld)
            then indent++"    , fld_sub      = []"
            else indent++"    , fld_sub      = [ "++
                 intercalate (indent++"                     , ")
                       [showHS flags (indent++"                       ") att| att<-fld_sub  fld]
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
            then "(\\d->"++showHSname (fld_onIns fld arg)++" d)"
            else "error(\"!Fatal: reference to undefined insert action in field "++fld_name fld++"\")" )
       ++ indent++"    , fld_delAble  = "++                     show (fld_delAble  fld)
       ++ indent++"    , fld_onDel    = "++
          ( if fld_delAble fld
            then "(\\d->"++showHSname (fld_onDel fld arg)++" d)"
            else "error(\"!Fatal: reference to undefined delete action in field "++fld_name fld++"\")" )
       ++ indent++"    }"
       where arg = error ("!Fatal (module ShowHS 500): reference to undefined argument of ECA rule")

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
 -- TODO: showHS flags should generate valid Haskell code for the entire pattern. Right now, it doesn't
    showHSname pat = haskellIdentifier ("pat_"++name pat)
    showHS flags indent pat
     = intercalate (indent++"   ")
       ([ "Pat "++show (name pat)
        , if null (rules pat) then " [ {- no rules -} ]"
                              else " [" ++intercalate          "    , "  [showHSname r                    | r<-rules pat] ++            "]"
        , if null (ptgns pat) then " [ {- no generalizations -} ]"
                              else " [ "++intercalate (indent++"    , ") [showHS flags (indent++"     ") g| g<-ptgns pat] ++indent++"    ]"
        , if null (ptdcs pat) then " [ {- no declarations -} ]"
                              else " [" ++intercalate          "    , "  [showHSname d                    | d<-ptdcs pat] ++            "]"
        , if null (ptcds pat) then " [ {- no concept definitions -} ]"
                              else " [" ++intercalate          "    , "  [showHSname c                    | c<-ptcds pat] ++            "]"
        , if null (ptkds pat) then " [ {- no key definitions -} ]"
                              else " [ "++intercalate (indent++"    , ") [showHS flags (indent++"     ") k| k<-ptkds pat] ++indent++"    ]"
        , if null (ptxps pat) then " [ {- no explanations -} ]"
                              else " [ "++intercalate (indent++"    , ") [showHS flags (indent++"     ") e| e<-ptxps pat] ++indent++"    ]"
        ])
--       indent++"where"++
--       (if null (ptdcs   pat) then "" else concat [indent++" "++showHSname d ++indent++"  = "++ showHS flags (indent++"    ") d |d <-ptdcs   pat] )++
--       (if null (signals pat) then "" else concat [indent++" "++showHSname s ++indent++"  = "++ showHS flags (indent++"    ") s |r <-signals pat, let s=srrel r] )++
--       (if null (rules   pat) then "" else concat [indent++" "++showHSname r ++indent++"  = "++ showHS flags (indent++"    ") r |r <-rules   pat] )++
--       (if null (ptcds   pat) then "" else concat [indent++" "++showHSname cd++indent++"  = "++ showHS flags (indent++"    ") cd|cd<-ptcds   pat] )++
--       (if null (ptkds   pat) then "" else concat [indent++" "++showHSname k ++indent++"  = "++ showHS flags (indent++"    ") k |k <-ptkds   pat] )

   instance ShowHS PExplanation where
    showHSname _ = error ("!Fatal (module ShowHS 542): a PExplanation is anonymous with respect to showHS flags")
    showHS flags _ expla = 
       "PExpl "++showHS flags "" (pexObj expla)++" "
               ++show (pexLang  expla)++" "
               ++show (pexRefID expla)++" "
               ++show (pexExpl  expla)

   instance ShowHS PExplObj where
    showHSname _ = error ("!Fatal (module ShowHS 550): a PExplObj is anonymous with respect to showHS flags")
    showHS flags _ peObj = case peObj of 
             PExplConceptDef str  -> "PExplConceptDef " ++show str
             PExplDeclaration rel -> "PExplDeclaration "++showHS flags "" rel
             PExplRule str        -> "PExplRule "       ++show str
             PExplKeyDef str      -> "PExplKeyDef "     ++show str
             PExplPattern str     -> "PExplPattern "    ++show str
             PExplService str     -> "PExplService "    ++show str
             PExplContext str     -> "PExplContext "    ++show str
            
               
   instance ShowHS Explanation where
    showHSname _ = error ("!Fatal (module ShowHS 562): a Explanation is anonymous with respect to showHS flags")
    showHS flags _ expla = 
       "Expl "++showHS flags "" (explObj expla)++" "
               ++show (explLang  expla)++" "
               ++show (explRefId expla)++" "
               ++show (explCont  expla)

   instance ShowHS ExplObj where
    showHSname _ = error ("!Fatal (module ShowHS 570): a ExplObj is anonymous with respect to showHS flags")
    showHS _ {-flags-} _ {-i-} peObj = case peObj of                     -- SJ: names of variables commented out to prevent warnings.
             ExplConceptDef cd  -> "ExplConceptDef " ++showHSname cd
             ExplDeclaration d  -> "ExplDeclaration "++showHSname d
             ExplRule r         -> "ExplRule "       ++showHSname r
             ExplKeyDef kd      -> "ExplKeyDef "     ++showHSname kd
             ExplPattern str    -> "ExplPattern "    ++show str
             ExplService str    -> "ExplService "    ++show str
             ExplContext str    -> "ExplContext "    ++show str
            

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Rule                          ***
-- \***********************************************************************

   instance (ShowHS (Expression r)) => ShowHS (Rule r) where
    showHSname r = "rule"++show (runum r)
    showHS flags indent r   
      = case r of
           Ru{} -> (intercalate newIndent 
                    ["Ru{ rrsrt = " ++ showHS flags "" (rrsrt r)
                      ,", rrant = " ++ "("++showHS flags "" (rrant r)++")"
                      ,", rrfps = " ++ "("++showHS flags "" (rrfps r)++")"
                      ,", rrcon = " ++ "("++showHS flags "" (rrcon r)++")"
                      ,", rrxpl = " ++ show(rrxpl r)
                      ,", rrtyp = " ++ showHS flags "" (rrtyp r)
                      ,", rrtyp_proof = Nothing -- TBD generate a function for the proof."
                      ,", rrdcl = " ++ case rrdcl r of
                                        Nothing   -> "Nothing"
                                        Just(p,d) -> "Just("++showHS flags "" p++","++showHSname d++")"
                      ,", runum = " ++ show (runum r)
                      ,", r_pat = " ++ show (r_pat r)
                      ,", r_usr = " ++ show (r_usr r)
                      ,", r_sgl = " ++ show (r_sgl r)
                      ,", srrel = " ++ "("++showHS flags "" (srrel r)++")"
                    ])++"}"
         where newIndent = indent ++ "  " 
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: RuleType                      ***
-- \***********************************************************************
   instance ShowHS RuleType where
     showHSname _ = error "!Fatal (module ShowHS 611): showHSname undefined for Type 'RuleType'"
     showHS _ _ Truth          = "Truth"
     showHS _ _ Equivalence    = "Equivalence"
     showHS _ _ Implication    = "Implication"
     showHS _ _ Generalization = "Generalization"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: KeyDef                        ***
-- \***********************************************************************

   instance ShowHS KeyDef where
    showHSname kd = haskellIdentifier ("kDef_"++name kd)
    showHS flags indent kd
     = "Kd ("++showHS flags "" (kdpos kd)++") "++show (kdlbl kd)++" ("++showHS flags "" (kdcpt kd)++")"
       ++indent++"  [ "++intercalate (indent++"  , ") [showHS flags (indent++"    ") a|a<-(kdats kd)]++indent++"  ]"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Population                    ***
-- \***********************************************************************

   instance (Eq r, Identified r, ShowHS (Relation r)) => ShowHS (Population r) where
    showHSname pop = haskellIdentifier ("pop_"++name rel++name (source rel)++name (target rel))
        where rel = popm pop
    showHS flags indent pop
     = "Popu ("++showHS flags "" (popm pop)++")"++indent++"     [ "++intercalate (indent++"     , ") (map show (popps pop))++indent++"     ]"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ObjectDef                     ***
-- \***********************************************************************

   instance ShowHS ObjectDef where
    showHSname obj = haskellIdentifier ("oDef_"++name obj)
    showHS flags indent r 
     = (intercalate (indent++"   ") 
           ["Obj{ objnm = " ++ show(objnm r)
                ,", objpos = " ++ showHS flags "" (objpos r)
                ,", objctx = " ++ showHS flags "" (objctx r)
                ,", objctx_proof = Nothing -- TBD: generation of proof in this haskell code."
                ,", objats = " ++ "["++intercalate (indent ++ "              ,")
                                                   (map (showHS flags (indent ++"               "))
                                                        (objats r))
                                ++"]"
                ,", objstrs = " ++ show(objstrs r)
           ])++"}"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ObjectDef                     ***
-- \***********************************************************************

   instance ShowHS Service where
    showHSname obj = haskellIdentifier ("srv_"++name obj)
    showHS flags indent svc
     = (intercalate (indent++"     ") 
           ["Serv { svName   = " ++ show(svName svc)
                ,", svParams = " ++ "["++intercalate ", " [showHS flags "" rel| rel<-svParams svc] ++ "]"
                ,", svViols  = " ++ "["++intercalate ", " [showHSname rel| rel<-svViols svc] ++ "]"
                ,", svArgs   = " ++ show(svArgs svc)
                ,", svObj    = " ++ showHS flags indent (svObj svc)
                ,", svPos    = " ++ showHS flags "" (svPos svc)
                ,", svExpl   = " ++ show(svExpl svc)
           ])++"}"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Expression                    ***
-- \***********************************************************************

   instance ShowHS (Expression (Relation Concept)) where
    showHSname expr = error ("!Fatal (module ShowHS 661): an expression is anonymous with respect to showHS flags. Detected at: "++ showADL expr)
    showHS flags _ (Tm rel i)   = "Tm ("++showHS flags "" rel++") "
         ++if i<0 then "(" ++ show i++")"
                  else        show i
    showHS flags indent (Tc f)   = showHS flags indent f
    showHS _ _ (F [])   = "F [] <Id>"
    showHS _ _ (Fdx [])  = "Fd [] <nId>"
    showHS _ _ (Fux [])  = "Fu [] {- False -}"
    showHS _ _ (Fix [])  = "Fi [] {- True -}"
    showHS flags indent (F [t])  = "F ["++showHS flags (indent++"   ") t++"]"
    showHS flags indent (F ts)   = "F [ "++intercalate (indent++"  , ") [showHS flags (indent++"    ") t| t<-ts]++indent++"  ]"
    showHS flags indent (Fdx [t]) = "Fd ["++showHS flags (indent++"    ") t++"]"
    showHS flags indent (Fdx ts)  = "Fd [ "++intercalate (indent++"   , ") [showHS flags (indent++"     ") t| t<-ts]++indent++"   ]"
    showHS flags indent (Fux [f]) = "Fu ["++showHS flags (indent++"    ") f++"]"
    showHS flags indent (Fux fs)  = "Fu [ "++intercalate (indent++"   , ") [showHS flags (indent++"     ") f| f<-fs]++indent++"   ]"
    showHS flags indent (Fix [f]) = "Fi ["++showHS flags (indent++"    ") f++"]"
    showHS flags indent (Fix fs)  = "Fi [ "++intercalate (indent++"   , ") [showHS flags (indent++"     ") f| f<-fs]++indent++"   ]"
    showHS flags indent (K0x e')  = "K0 ("++showHS flags (indent++"    ") e'++")"
    showHS flags indent (K1x e')  = "K1 ("++showHS flags (indent++"    ") e'++")"
    showHS flags indent (Cpx e')  = "Cp ("++showHS flags (indent++"    ") e'++")"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Gen                           ***
-- \***********************************************************************

   instance (Show c, ShowHS c, ShowADL c) => ShowHS (Gen c) where
    showHSname g = error ("!Fatal (module ShowHS 687): Illegal call to showHSname ("++showADL g++"). A GEN statement gets no definition in Haskell code.")
    showHS flags _ gen = "G ("++showHS flags "" (genfp gen)++") ("++showHS flags "" (gengen gen)++") ("++showHS flags "" (genspc gen)++") "++show (genpat gen)
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Relation Concept            ***
-- \***********************************************************************

   instance ShowHS (Relation Concept) where
    showHSname rel = error ("!Fatal (module ShowHS 695): Illegal call to showHSname ("++showADL rel++"). A morphism gets no definition in Haskell code.")
    showHS flags _ rel 
       = case rel of
            Rel{} -> "Rel "++show (relnm rel)++" "++showPos++" "++showAtts
                         ++" "++showSrc++" "++showTrg++" "++show (relyin rel)++" "++showHSname (reldcl rel)
            I{}   -> "I "++showAtts++" "++showGen++" "++showSpc++" "++show (relyin rel)
            V{}   -> "V "++showAtts++" "++showSgn
            Mp1{} -> "Mp1 "++rel1val rel++" "++showAtts++" ("++showHS flags "" (rel1typ rel)++")"
  -- WHY wordt rel1val rel zonder quotes afgedrukt?
  -- BECAUSE: rel1val rel wordt door een lambda gebonden in de omgeving van Mp1. Het is dus een haskell identifier en niet een haskell string.
           where showPos  = "("++showHS flags "" (pos rel)++")"
                 showAtts = showL(map (showHS flags "") (relats rel))
                 showGen  = "("++showHS flags "" (relgen rel)++")"
                 showSpc  = "("++showHS flags "" (relspc rel)++")"
                 showSrc  = "("++showHS flags "" (source rel)++")"
                 showTrg  = "("++showHS flags "" (target rel)++")"
                 showSgn  = "("++showHS flags "" (sign   rel)++")"
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Declaration                   ***
-- \***********************************************************************

   instance (Eq c, Identified c, ShowHS c) => ShowHS (Declaration c) where
    showHSname d | decusr d  = haskellIdentifier ("rel_"++name d++name (source d)++name (target d)) -- user defined relations
                 | deciss d  = haskellIdentifier ("sgn_"++name d++name (source d)++name (target d)) -- relations generated for signalling
                 | otherwise = haskellIdentifier ("vio_"++name d++name (source d)++name (target d)) -- relations generated per rule
    showHS flags indent d 
       = case d of 
          Sgn{}     -> (intercalate newIndent
                        ["Sgn{ decnm   = " ++ show (decnm d)
                           ,", desrc   = " ++ showHS flags "" (desrc d)
                           ,", detrg   = " ++ showHS flags "" (detrg d)
                           ,", decprps = " ++ showL(map (showHS flags "") (decprps d))
                           ,", decprps_calc = " ++ showL(map (showHS flags "") (decprps_calc d))
                           ,", decprL  = " ++ show (decprL d)
                           ,", decprM  = " ++ show (decprM d)
                           ,", decprR  = " ++ show (decprR d)
                           ,", decpopu = " ++ show (decpopu d)
 -- obsolete: 18 July 2010 ,", decexpl = " ++ show (decexpl d)
                           ,", decfpos = " ++ showHS flags "" (decfpos d)
                           ,", decid   = " ++ show (decid d)
                           ,", deciss  = " ++ show (deciss d)
                           ,", decusr  = " ++ show (decusr d)
                           ,", decpat  = " ++ show (decpat d)
                        ])++"}"
          Isn{}     -> (intercalate newIndent
                        ["Isn{ degen   = " ++ showHS flags "" (degen d)
                           ,", despc   = " ++ showHS flags "" (despc d)
                        ])++"}"
          Iscompl{} -> (intercalate newIndent
                        ["Isn{ degen   = " ++ showHS flags "" (degen d)
                           ,", despc   = " ++ showHS flags "" (despc d)
                        ])++"}"
          Vs{}      -> (intercalate newIndent
                        ["Isn{ desrc   = " ++ showHS flags "" (degen d)
                           ,", detrg   = " ++ showHS flags "" (despc d)
                        ])++"}"
       where newIndent = indent ++ "   "
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ConceptDef                    ***
-- \***********************************************************************

   instance ShowHS ConceptDef where
    showHSname cd = haskellIdentifier ("cDef_"++name cd)
    showHS flags _ cd
     = " Cd ("++showHS flags "" (cdpos cd)++") "++show (name cd)++" "++show (cddef cd)++(if null (cdref cd) then "" else " "++show (cdref cd))
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Concept                     ***
-- \***********************************************************************

   instance ShowHS Concept where
    showHSname c = error ("!Fatal (module ShowHS 765): Illegal call to showHSname ("++name c++"). A concept gets no definition in Haskell code.")
    showHS _ _ c = case c of
                       C{}      -> "C "++show (name c) ++ " gE [] "    -- contents not shown.
                       S        -> "S "
                       Anything -> "Anything "
                       NOthing  -> "NOthing "
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FPA                           ***
-- \***********************************************************************
   
   instance ShowHS FPcompl where
    showHSname c = error ("!Fatal (module ShowHS 776): Illegal call to showHSname ("++show c++"). A FPcompl gets no definition in Haskell code.")
    showHS _ _ c   = show c

   instance ShowHS FPA where
    showHSname c = error ("!Fatal (module ShowHS 780): Illegal call to showHSname ("++show c++"). A FPA gets no definition in Haskell code.")
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
    showHSname p = error ("!Fatal (module ShowHS 793): should not showHS flags the name of multiplicities (Prop): "++show p)
    showHS _ _ Uni = "Uni"
    showHS _ _ Inj = "Inj"
    showHS _ _ Sur = "Sur"
    showHS _ _ Tot = "Tot"
    showHS _ _ Sym = "Sym"
    showHS _ _ Asy = "Asy"
    showHS _ _ Trn = "Trn"
    showHS _ _ Rfx = "Rfx"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FilePos                       ***
-- \***********************************************************************

   instance ShowHS FilePos where
    showHSname p = error ("!Fatal (module ShowHS 808): Illegal call to showHSname ("++show p++"). A position is an anonymous entity in Haskell code.")
    showHS _ _ (FilePos (fn,UU_Scanner.Pos l c,sym))
      = "FilePos ("++show fn++",Pos "++show l++" "++show c++","++show sym++")"
    showHS _ _ Nowhere
      = "Nowhere"

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: InfTree                       ***
-- \***********************************************************************
--   instance ShowHS InfTree where
--    showHSname itree = error ("!Fatal (module ShowHS 818): Illegal call to showHSname ("++show itree++"). An inference tree is an anonymous entity in Haskell code.")
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
--    showHSname d = error ("!Fatal (module ShowHS 831): Illegal call to showHSname ("++show d++"). A declared ruletype is an anonymous entity in Haskell code.")
--    showHS _ indent d = case d of 
--                          RelDecl{}-> "RelDecl{ dname  = " ++ show (dname d) ++ indent
--                                   ++ "        ,dtype  = " ++ showRaType dtype ++ indent
--                                   ++ "        ,ishomo = " ++ show (ishomo d) 
--                          IDecl    -> "IDecl"
--                          VDecl    -> "VDecl"
--      where 
--       showRaType _ = "RelAlgType{- ++TODO++ -}"
--
--
--   instance ShowHS DeclRuleType where
--    showHSname drt = error ("!Fatal (module ShowHS 843): Illegal call to showHSname ("++show drt++"). A declared ruletype is an anonymous entity in Haskell code.")
--    showHS _ _ drt = case drt of
--					   D_rel     -> "D_rel"
--					   D_rel_h   -> "D_rel_h"
--					   D_rel_c   -> "D_rel_c"
--					   D_rel_c_h -> "D_rel_c_h"
--					   D_id      -> "D_id"
--					   D_v       -> "D_v"
--					   D_id_c    -> "D_id_c"
--					   D_v_c     -> "D_v_c"
--                        
--   instance ShowHS InfRuleType where
--    showHSname irt = error ("!Fatal (module ShowHS 855): Illegal call to showHSname ("++show irt++"). A inference ruletype is an anonymous entity in Haskell code.")
--    showHS _ _ irt = case irt of
--					   ISect_cs  -> "ISect_cs"
--					   ISect_ncs -> "ISect_ncs"
--					   ISect_mix -> "ISect_mix"
--					   Union_mix -> "Union_mix"
--					   Comp_ncs  -> "Comp_ncs"
--					   Comp_c1   -> "Comp_c1"
--					   Comp_c2   -> "Comp_c2"
--					   Comp_cs   -> "Comp_cs"
--					   RAdd_ncs  -> "RAdd_ncs"
--					   RAdd_c1   -> "RAdd_c1"
--					   RAdd_c2   -> "RAdd_c2"
--					   RAdd_cs   -> "RAdd_cs"
--					   Conv_nc   -> "Conv_nc"
--					   Conv_c    -> "Conv_c"
					   
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

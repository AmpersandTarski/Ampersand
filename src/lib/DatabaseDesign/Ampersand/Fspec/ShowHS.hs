{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
module DatabaseDesign.Ampersand.Fspec.ShowHS (ShowHS(..),ShowHSName(..),fSpec2Haskell,haskellIdentifier)
where
   import DatabaseDesign.Ampersand.Core.ParseTree
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import Text.Pandoc hiding (Meta)
   import Data.Char                  (isAlphaNum)
   import DatabaseDesign.Ampersand.Basics
   import DatabaseDesign.Ampersand.Fspec.Plug
   import DatabaseDesign.Ampersand.Fspec.Fspec
--   import DatabaseDesign.Ampersand.Fspec.ShowADL    (ShowADL(..))  -- for traceability, we generate comment in the Haskell code.
--   import DatabaseDesign.Ampersand.Fspec.FPA   (fpa)
   import Data.List
   import DatabaseDesign.Ampersand.Classes
   import qualified DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner
   import DatabaseDesign.Ampersand.Misc
   import Data.Hashable
   import Data.Ord
   import Data.Function
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "Fspec.ShowHS"

   fSpec2Haskell :: Fspc -> Options -> String
   fSpec2Haskell fSpec flags
           = "{-# OPTIONS_GHC -Wall #-}"
             ++"\n{-Generated code by "++ampersandVersionStr++" at "++show (genTime flags)++"-}"
             ++"\nmodule Main where"
             ++"\n  import DatabaseDesign.Ampersand"
             ++"\n  import Text.Pandoc hiding (Meta)"
             ++"\n  import Prelude hiding (writeFile,readFile,getContents,putStr,putStrLn)"
             ++"\n"
             ++"\n  main :: IO ()"
             ++"\n  main = do flags <- getOptions"
             ++"\n            putStr (showHS flags \"\\n  \" fSpec_"++baseName flags++")"
             ++"\n  fSpec_"++baseName flags++" :: Fspc"
             ++"\n  fSpec_"++baseName flags++"\n   = "++showHS flags "\n     " fSpec


   wrap :: String->String->(String->a->String)->[a]->String
   wrap initStr indent f xs
    = initStr++
      case xs of
        []  -> "[]"
        [x] -> "[ "++f (indent++"  ") x++" ]"
        _   -> "[ "++intercalate (indent++", ") [f (indent++"  ") x | x<-xs]++indent++"]"

   class ShowHSName a where
    showHSName :: a -> String
    
   class ShowHS a where
    showHS :: Options -> String -> a -> String

   instance ShowHSName a => ShowHSName [a] where
    showHSName xs = "["++intercalate "," (map showHSName xs)++"]"
   
   instance ShowHS a => ShowHS [a] where
    showHS flags indent = wrap "" (indent++" ") (showHS flags)
    
   instance ShowHSName a => ShowHSName (Maybe a) where
    showHSName Nothing  = "Nothing"
    showHSName (Just x) = showHSName x
    
   instance ShowHS a => ShowHS (Maybe a) where
    showHS _ _ Nothing  = "Nothing"
    showHS flags indent (Just x) = "Just (" ++ showHS flags indent x ++ ")"


   instance (ShowHSName a , ShowHSName b) => ShowHSName (a,b) where 
    showHSName (a,b) = "( "++showHSName a++" , "++showHSName b++" )"
   -- | The following is used to showHS flags for signs: (Concept, Concept)
   instance (ShowHS a , ShowHS b) => ShowHS (a,b) where
    showHS flags indent (a,b) = "("++showHS flags (indent++" ") a++","++showHS flags (indent++" ") b++")"
  
   

   instance ShowHSName PlugSQL where
    showHSName plug = haskellIdentifier ("plug_"++name plug)

   instance ShowHS PlugSQL where
    showHS flags indent plug
      = case plug of
          TblSQL{} -> intercalate indent 
                      ["let " ++ intercalate (indent++"    ")
                                             [showHSName f++indent++"     = "++showHS flags (indent++"       ") f | f<-fields plug] ++indent++"in"
                      ,"TblSQL { sqlname = " ++ (show.name) plug
                      ,"       , fields  = ["++intercalate ", " (map showHSName (fields plug))++"]"
                      ,"       , cLkpTbl = [ "++intercalate (indent++"                   , ") ["("++showHSName c++", "++showHSName cn++")" | (c,cn)<-cLkpTbl plug] ++ "]"
                      ,"       , mLkpTbl = [ "++intercalate (indent++"                   , ") ["("++showHS flags "" r++", "++showHSName ms++", "++showHSName mt++")" | (r,ms,mt)<-mLkpTbl plug] ++ "]"
                  --    ,"       , sqlfpa  = " ++ showHS flags "" (fpa plug)
                      ,"       }"
                      ]
          BinSQL{} -> intercalate indent 
                      ["let " ++ showHSName (fst (columns plug))++indent++"     = "++showHS flags (indent++"       ") (fst (columns plug))
                              ++ (indent++"    ") ++ showHSName (snd (columns plug))++indent++"     = "++showHS flags (indent++"       ") (snd (columns plug))
                              ++indent++"in"
                      ,"BinSQL { sqlname = " ++ (show.name) plug
                      ,"       , columns = ("++showHSName (fst (columns plug))++ ", " ++showHSName (snd (columns plug))++")"
                      ,"       , cLkpTbl = [ "++intercalate (indent++"                   , ") ["("++showHSName c++", "++showHSName cn++")" | (c,cn)<-cLkpTbl plug] ++ "]"
                      ,"       , mLkp = "++showHS flags "" (mLkp plug)
                  --    ,"       , sqlfpa  = " ++ showHS flags "" (fpa plug)
                      ,"       }"
                      ]
          ScalarSQL{} -> intercalate indent 
                      ["ScalarSQL { sqlname   = "++ (show.name) plug
                      ,"          , sqlColumn = "++ showHS flags (indent++"                     ") (sqlColumn plug)
                      ,"          , cLkp      = "++ showHSName (cLkp plug)
                  --    ,"          , sqlfpa    = "++ showHS flags "" (fpa plug)
                      ,"          }"
                      ]

   instance ShowHSName (ECArule) where
    showHSName r = "ecaRule"++show (ecaNum r)

   instance ShowHS (ECArule) where
    showHS flags indent r   
      =         "ECA { ecaTriggr = " ++ showHS flags "" (ecaTriggr r) ++
        indent++"    , ecaDelta  = " ++ showHS flags (indent++"                  ")  (ecaDelta r)++
        indent++"    , ecaAction = " ++ showHS flags (indent++"                  ")  (ecaAction r)++
        indent++"    , ecaNum    = " ++ show (ecaNum r)++
        indent++"    }"

   instance ShowHS Event where
    showHS _ indent e   
      = if "\n" `isPrefixOf` indent
        then "On " ++ show (eSrt e)++indent++"   " ++ showHSName (eDcl e)++indent++"   "
        else "On " ++ show (eSrt e)++          " " ++ showHSName (eDcl e)++           ""

   instance ShowHS PAclause where
    showHS flags indent p   
      = case p of
           CHC{} -> wrap "CHC " (indent ++"    ") (showHS flags) (paCls p)++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
           ALL{} -> wrap "ALL " (indent ++"    ") (showHS flags) (paCls p)++
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
           Let{} -> wrap "Let " (indent ++"    ") (showHS flags) (paCls p)++
                    "TODO: paBody of Let clause"++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms             
           Ref{} -> "Ref "++paVar p
        where ms = paMotiv p
              showMotiv ind (conj,rs) = "("++showHS flags (ind++" ") conj++", "++showHSName rs++")"

   instance ShowHSName SqlField where
    showHSName sqFd = haskellIdentifier ("sqlFld_"++fldname sqFd)

   instance ShowHS SqlField where
    showHS flags indent sqFd
      = intercalate indentA
          [  "Fld { fldname = " ++ show (fldname sqFd)
          ,      ", fldexpr = " ++ showHS flags indentB (fldexpr sqFd)
          ,      ", fldtype = " ++ showHS flags "" (fldtype sqFd)
          ,      ", flduse  = " ++ showHS flags "" (flduse sqFd)
          ,      ", fldnull = " ++ show (fldnull sqFd)
          ,      ", flduniq = " ++ show (flduniq sqFd)
          ,      "}"
          ] where indentA = indent ++"    "         -- adding the width of "Fld "
                  indentB = indentA++"            " -- adding the width of ", fldexpr = " 

   instance ShowHS SqlFieldUsage where
    showHS _ _ (PrimKey aCpt)    = "PrimKey "   ++showHSName aCpt
    showHS _ _ (ForeignKey aCpt) = "ForeignKey "++showHSName aCpt
    showHS _ _ PlainAttr         = "PlainAttr "
    showHS _ _ NonMainKey        = "NonMainKey "
    showHS _ _ UserDefinedUsage  = "UserDefinedUsage "
    showHS _ _ FillInLater       = "FillInLater "

   instance ShowHS SqlType where
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

   instance ShowHSName Quad where
    showHSName q
      = haskellIdentifier ("quad_"++(showHSName.qDcl) q++"_"++(name.cl_rule.qClauses) q)

   instance ShowHS Quad where
    showHS flags indent q 
      = intercalate indent
               [ "Quad{ qDcl     = " ++ showHSName (qDcl q)
               , "    , qClauses = " ++ showHS flags newindent (qClauses q)
               , "    }"
               ]
       where
         newindent = indent ++ "                 "
         
   instance ShowHS Fswitchboard where
    showHS flags indent fsb
      = intercalate indent
          [ "Fswtch { fsbEvIn  = " ++ showHS flags newindent (fsbEvIn  fsb)
          , "       , fsbEvOut = " ++ showHS flags newindent (fsbEvOut fsb)
          ,wrap
            "       , fsbConjs = " newindent' (\_->shConj) (fsbConjs  fsb)
          ,wrap
            "       , fsbECAs  = " newindent' (\_->showHSName) (fsbECAs  fsb)
          , "       }"
          ]
       where 
         newindent   = indent ++ "                   "
         newindent'  = newindent ++ " "
         newindent'' = newindent' ++ "    "
         shConj (r,conj) = "( "++showHSName r++newindent++"   , "++showHS flags newindent'' conj++newindent++"   )"

   instance ShowHS Clauses where
    showHS _ indent c
      = intercalate indent
          [ "Clauses{ cl_conjNF = " ++ showHSName (cl_conjNF c)
          , "       , cl_rule   = " ++ showHSName (cl_rule c)
          , "       }"
          ]

   instance ShowHS DnfClause where
    showHS flags indent (Dnf antcs conss)
      = intercalate indent
          [ wrap "Dnf " (indent++"   ") (\_->showHS flags (indent++"      ")) antcs
          , wrap "    " (indent++"   ") (\_->showHS flags (indent++"      ")) conss
          ]

   instance ShowHSName RuleClause where
    showHSName x = haskellIdentifier ("conj_"++rc_rulename x++"["++show (rc_int x)++"]")
    
   instance ShowHS RuleClause where
    showHS flags indent x
      = intercalate (indent ++"  ")
          [   "RC{ rc_int        = " ++ show (rc_int x)
          ,     ", rc_rulename   = " ++ show (rc_rulename x)
          ,     ", rc_conjunct   = " ++ showHS flags indentA (rc_conjunct x)
          ,wrap ", rc_dnfClauses = " indentA (\_->showHS flags (indentA++"  ")) (rc_dnfClauses x)
          ,     "}" 
          ]
        where indentA = indent ++"                    "

   instance ShowHSName Fspc where
    showHSName fspec = haskellIdentifier ("fSpc_"++name fspec)
   
   instance ShowHS Fspc where
    showHS flags indent fspec
     = intercalate (indent ++"    ") 
           [ "Fspc{ fsName        = " ++ show (name fspec)
           ,wrap ", fspos         = " indentA (showHS flags) (fspos fspec)
           ,     ", fsLang        = " ++ show (fsLang fspec) ++ "  -- the default language for this specification"
           ,     ", themes        = " ++ show (themes fspec) ++ "  -- the names of themes to be printed in the documentation, meant for partial documentation.  Print all if empty..."
           ,wrap ", vprocesses    = " indentA (\_->showHSName) (vprocesses fspec)
           ,wrap ", vplugInfos    = " indentA (\_->showHS flags (indentA++"  ")) (vplugInfos fspec)
           ,wrap ", plugInfos     = " indentA (\_->showHS flags (indentA++"  ")) (plugInfos  fspec)
           ,     ", interfaceS    = interfaceS'"
           ,     ", interfaceG    = interfaceG'"
           ,     ", fSwitchboard  = "++showHS flags indentA (fSwitchboard fspec)
           ,wrap ", fActivities   = " indentA (\_->showHS flags (indentA++"  ")) (fActivities fspec)
           ,     ", fRoleRels     = " ++
                 case fRoleRels fspec of
                   []        -> "[]"
                   [(r,rel)] -> "[ ("++show r++", "++showHS flags "" rel++") ]"
                   _         -> "[ "++intercalate (indentA++", ") ["("++show r++","++showHS flags "" rel++")" | (r,rel)<-fRoleRels fspec]++indentA++"]"
           ,     ", fRoleRuls     = " ++
                 case fRoleRuls fspec of
                   []        -> "[]"
                   [(r,rul)] -> "[ ("++show r++", "++showHSName rul++") ]"
                   _         -> "[ "++intercalate (indentA++", ") ["("++show r++","++showHSName rul++")" | (r,rul)<-fRoleRuls fspec]++indentA++"]"
           ,wrap ", fRoles        = " indentA (\_->id) (fRoles fspec)
           ,wrap ", vrules        = " indentA (\_->showHSName) (vrules fspec)
           ,wrap ", grules        = " indentA (\_->showHSName) (grules fspec)
           ,wrap ", invars        = " indentA (\_->showHSName) (invars fspec)
           ,wrap ", allRules      = " indentA (\_->showHSName) (allRules fspec)
           ,wrap ", allUsedDecls  = " indentA (\_->showHSName) (allUsedDecls fspec)
           ,wrap ", allDecls      = " indentA (\_->showHSName) (allDecls fspec)
           ,wrap ", allConcepts   = " indentA (\_->showHSName) (allConcepts fspec)
           ,wrap ", kernels       = " indentA (\_->showHSName) (kernels fspec)
           ,wrap ", vIndices      = " indentA (\_->showHSName) (vIndices fspec)
           ,wrap ", vviews        = " indentA (\_->showHSName) (vviews fspec)
           ,wrap ", vgens         = " indentA (showHS flags)   (vgens fspec)
           ,wrap ", vconjs        = " indentA (\_->showHSName) (vconjs fspec)
           ,wrap ", vquads        = " indentA (\_->showHSName) (vquads fspec)
           ,wrap ", vEcas         = " indentA (\_->showHSName) (vEcas fspec)
           ,wrap ", vrels         = " indentA (\_->showHSName) (vrels fspec)
           ,     ", fsisa         = isa'"
           ,wrap ", vpatterns     = " indentA (\_->showHSName) (patterns fspec)
           ,wrap ", conceptDefs   = " indentA (\_->showHSName) (conceptDefs fspec)
           ,wrap ", fSexpls       = " indentA (showHS flags)   (fSexpls fspec)
           ,     ", metas         = allMetas"
           ,wrap ", initialPops   = " indentA (showHS flags)   (initialPops fspec)
           ,wrap ", allViolations = " indentA showViolatedRule (allViolations fspec)
           ,"}" 
           ] ++   
       indent++"where"++
       indent++" isa' :: [(A_Concept, A_Concept)]"++
       indent++" isa'  = "++    showHSName (fsisa fspec)++
        "\n -- ***Interfaces Specified in Ampersand script***: "++
       indent++" interfaceS' = "++(if null (interfaceS fspec) then "[]" else
                                 "[ "++intercalate (indentB++"  , ") (map showHSName (interfaceS fspec))++indentB++"  ]")++
        "\n -- ***Activities Generated by the Ampersand compiler ***: " ++
       indent++" interfaceG' = "++(if null (interfaceG fspec) then "[]" else
                                 "[ "++intercalate (indentB++", ") (map showHSName (interfaceG fspec))++indentB++"]")++
       indent++" allMetas = "++(if null (metas fspec) then "[]" else
                                 "[ "++intercalate (indentB++", ") (map (showHS flags (indent ++ "         ")) (metas fspec))++indentB++"]") ++
       (if null (patterns fspec ) then "" else "\n -- ***Patterns***: "++concat [indent++" "++showHSName p++indent++"  = "++showHS flags (indent++"    ") p |p<-patterns fspec ]++"\n")++

-- WHY?  staan hier verschillende lijstjes met interfaces?
-- BECAUSE! Een Ampersand engineer besteedt veel tijd om vanuit een kennismodel (lees: een graaf met concepten en relaties)
--          alle interfaces met de hand te verzinnen.
--          Je kunt natuurlijk ook een interfaces-generator aan het werk zetten, die een aantal interfaces klaarzet bij wijze
--          van steiger (scaffold). Dat bespaart een hoop werk. De functie interfaceG is zo'n generator.
--          Door de gegenereerde interfaces af te drukken, kun je dus heel snel Ampersand sourcecode maken met correct-vertaalbare interfaces.
--          Heb je eenmaal een goed werkend pakket interfaces, dan wil je wellicht alleen de door jezelf gespecificeerde interfaces
--          gebruiken. Dat gebeurt in interfaceS.

       (if null  (interfaceS fspec) then ""  else
        "\n -- *** User defined interfaces (total: "++(show.length.interfaceS) fspec++" interfaces) ***: "++
        concat [indent++" "++showHSName s++indent++"  = "++showHS flags (indent++"    ") s | s<-interfaceS fspec]++"\n"
       )++
       (if null (interfaceG fspec ) then "" else
        "\n -- *** Generated interfaces (total: "++(show.length.interfaceG) fspec++" interfaces) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS flags (indent++"    ") x |x<-interfaceG fspec ]++"\n"
       )++        
       (let ds fs = allDecls fs `uni` allUsedDecls fs `uni` vrels fspec `uni` nub (map qDcl (vquads fs)) in
        if null (ds fspec)     then "" else
        "\n -- *** Declarations (total: "++(show.length.ds) fspec++" declarations) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS flags (indent++"    ") x |x<-ds fspec]++"\n"
       ) ++
       (if null (vIndices fspec)     then "" else
        "\n -- *** Indices (total: "++(show.length.vIndices) fspec++" indices) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS flags (indent++"    ") x |x<-vIndices fspec]++"\n"
       ) ++
       (if null (vviews fspec)     then "" else
        "\n -- *** Views (total: "++(show.length.vviews) fspec++" views) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS flags (indent++"    ") x |x<-vviews fspec]++"\n"
       ) ++
       (if null (vprocesses fspec ) then "" else
        "\n -- *** Processes (total: "++(show.length.vprocesses) fspec++" processes) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS flags (indent++"    ") x |x<-vprocesses fspec ]++"\n"
       )++
       (if null (vrules   fspec ) then "" else
        "\n -- *** User defined rules (total: "++(show.length.vrules) fspec++" rules) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS flags (indent++"    ") x |x<-vrules     fspec ]++"\n"++
        concat [indent++" "++showHSName x++indent++"  = "++showHS flags (indent++"    ") x |x<-map srrel (vrules fspec)]++"\n"
       )++
       (if null (grules   fspec ) then "" else
        "\n -- *** Generated rules (total: "++(show.length.grules) fspec++" rules) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS flags (indent++"    ") x |x<-grules     fspec ]++"\n"++
        concat [indent++" "++showHSName x++indent++"  = "++showHS flags (indent++"    ") x |x<-map srrel (grules fspec)]++"\n"
       )++
       (if null (vconjs fspec ) then "" else
        "\n -- *** Conjuncts (total: "++(show.length.vconjs) fspec++" conjuncts) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS flags (indent++"    ") x |x<-vconjs     fspec ]++"\n"
       )++
       (if null (vquads fspec ) then "" else
        "\n -- *** Quads (total: "++(show.length.vquads) fspec++" quads) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS flags (indent++"    ") x |x<-vquads     fspec ]++"\n"
       )++
       (if null (vEcas fspec ) then "" else
        "\n -- *** ECA rules (total: "++(show.length.vEcas) fspec++" ECA rules) ***: "++
        concat [indent++" "++showHSName eca++indent++"  = "++showHS flags (indent++"    ") eca |eca<-vEcas fspec ]++"\n"++
        concat [indent++" "++showHSName rel++indent++"  = "++showHS flags (indent++"    ") rel |rel<-nub(map ecaDelta (vEcas fspec)) ]++"\n"
       )++
       (if null (plugInfos fspec ) then "" else
        "\n -- *** PlugInfos (total: "++(show.length.plugInfos) fspec++" plugInfos) ***: "++
        concat [indent++" "++showHSName p++indent++"  = "++showHS flags (indent++"    ") p |InternalPlug p<-sortBy (compare `on` name) (plugInfos fspec) ]++"\n"
       )++
       (if null (vpatterns fspec) then "" else
        "\n -- *** Patterns (total: "++(show.length.vpatterns) fspec++" patterns) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS flags (indent++"    ") x |x<-vpatterns fspec]++"\n"
       )++
       (if null (conceptDefs fspec) then "" else
        "\n -- *** ConceptDefs (total: "++(show.length.conceptDefs) fspec++" conceptDefs) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS flags (indent++"    ") x | x<-sortBy (comparing showHSName) (conceptDefs fspec)]++"\n"
       )++
       (if null (allConcepts fspec) then "" else
        "\n -- *** Concepts (total: "++(show.length.allConcepts) fspec++" concepts) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS flags (indent++"    ") x
             ++ indent++"    "++showAtomsOfConcept x |x<-sortBy (comparing showHSName) (allConcepts fspec)]++"\n"
       )
           where indentA = indent ++"                      "
                 indentB = indent ++"             "
                 showAtomsOfConcept c =
                              "-- atoms: "++(show.sort) (atomsOf (gens fspec)(initialPops fspec) c)
                 showViolatedRule :: String -> (Rule,Pairs) -> String
                 showViolatedRule indent' (r,ps)
                    = intercalate indent'
                        [        " ( "++showHSName r++" -- This is "++(if r_sgl r then "a process rule." else "an invariant")++
                         indent'++" , "++ wrap "" (indent'++"   ") (let showPair _ p = show p --"( "++ (show.fst) p++", "++(show.snd) p++")"
                                                                      in showPair) ps++
                         indent'++" )"
                        ] 

   instance ShowHS Meta where
    showHS f i (Meta pos obj nm val) = "Meta ("++showHS f i pos ++ ") "++ show obj ++ " " ++ show nm ++ " " ++ show val 


   instance ShowHSName PlugInfo where
    showHSName (InternalPlug p) = haskellIdentifier ("ipl_"++name p)-- TODO
    showHSName (ExternalPlug _) = fatal 336 "a PlugInfo is anonymous with respect to showHS flags"

   instance ShowHS PlugInfo where
    showHS _ _ (InternalPlug p)
     = "InternalPlug "++showHSName p
    showHS flags ind (ExternalPlug o)
     = "ExternalPlug "++showHS flags (ind++"    ") o
   

   instance ShowHS RoleRelation where
    showHS flags ind rr
     = "RR "++show (rrRoles rr)++" "++showHS flags (ind++"    ") (rrRels rr)++" "++showHS flags (ind++"    ") (rrPos rr)
   
   instance ShowHS RoleRule where
    showHS flags ind rs
     = "Maintain "++show (mRoles rs)++" "++show (mRules rs)++" "++showHS flags (ind++"    ") (mPos rs)
   

   instance ShowHSName FSid where
    showHSName (FS_id nm ) = haskellIdentifier nm 

   instance ShowHS FSid where
    showHS _ _ (FS_id nm) 
      = "(FS_id " ++ show nm ++ ")"
     

   instance ShowHSName Pattern where
    showHSName pat = haskellIdentifier ("pat_"++name pat)
   
   instance ShowHS Pattern where
    showHS flags indent pat
     = intercalate indentA
        [ "A_Pat { ptnm  = "++show (name pat)
        , ", ptpos = "++showHS flags "" (ptpos pat)
        , ", ptend = "++showHS flags "" (ptend pat)
        , ", ptrls = [" ++intercalate ", " [showHSName r | r<-ptrls pat] ++ concat [" {- no rules -} "        | null (ptrls pat)] ++"]"
        , wrap ", ptgns = " indentB (showHS flags) (ptgns pat)
        , ", ptdcs = [" ++intercalate ", " [showHSName d | d<-ptdcs pat] ++ concat [" {- no declarations -} " | null (ptdcs pat)] ++"]"
        , wrap ", ptups = " indentB (showHS flags) (ptups pat) 
        , case ptrruls pat of
           []          -> ", ptrruls = [] {- no role-rule assignments -}"
           [(rol,rul)] -> ", ptrruls = [ ("++show rol++", "++showHSName rul++") ]"
           rs          -> ", ptrruls = [ "++intercalate (indentB++", ") ["("++show rol++", "++showHSName rul++")" | (rol,rul)<-rs] ++indentB++"]"
        , case ptrrels pat of
           []          -> ", ptrrels = [] {- no role-relation assignments -}"
           [(rol,rel)] -> ", ptrrels = [ ("++show rol++", "++showHS flags "" rel++") ]"
           rs          -> ", ptrrels = [ "++intercalate (indentB++", ") ["("++show rol++", "++showHS flags "" rel++")" | (rol,rel)<-rs] ++indentB++"]"
        , wrap ", ptids = " indentB (showHS flags) (ptids pat)
        , wrap ", ptxps = " indentB (showHS flags) (ptxps pat)
        , "}"
        ] where indentA = indent ++"      "     -- adding the width of "A_Pat "
                indentB = indentA++"          " -- adding the width of ", ptrls = "

   instance ShowHSName FProcess where
    showHSName prc = haskellIdentifier ("fprc_"++name (fpProc prc))
   
   instance ShowHS FProcess where
    showHS flags indent prc
     = intercalate indentA
        [ "FProc { fpProc       = "++showHS flags (indent++"                     ") (fpProc prc)
        , wrap  ", fpActivities = " indentB (showHS flags) (fpActivities prc)
        , "      }"
        ] where indentA = indent ++"      "     -- adding the width of "FProc "
                indentB = indentA++"                 " -- adding the width of ", fpActivities = "
 
   instance ShowHSName Process where
    showHSName prc = haskellIdentifier ("prc_"++name prc)

   instance ShowHS Process where
    showHS flags indent prc
     = intercalate indentA
        [ "Proc { prcNm = "++show (name prc)
        , ", prcPos = "++showHS flags "" (prcPos prc)
        , ", prcEnd = "++showHS flags "" (prcEnd prc)
        , ", prcRules = [" ++intercalate ", " [showHSName r | r<-prcRules prc] ++ concat [" {- no rules -} "                     | null (prcRules prc)] ++"]"
        , wrap ", prcGens = " indentB (showHS flags) (prcGens prc)
        , ", prcDcls = ["  ++intercalate ", " [showHSName d | d<-prcDcls  prc] ++ concat [" {- no declarations -} "              | null (prcDcls  prc)] ++"]"
        , wrap ", prcUps = " indentB (showHS flags) (prcUps prc) 
        , case prcRRuls prc of
           []          -> "     , prcRRuls = [] {- no role-rule assignments -}"
           [(rol,rul)] -> "     , prcRRuls = [ ("++show rol++", "++showHSName rul++") ]"
           rs          -> "     , prcRRuls = [ "++intercalate (indentB++", ") ["("++show rol++", "++showHSName rul++")" | (rol,rul)<-rs] ++indentB++"]"
        , case prcRRels prc of
           []          -> "     , prcRRels = [] {- no role-relation assignments -}"
           [(rol,rel)] -> "     , prcRRels = [ ("++show rol++", "++showHS flags "" rel++") ]"
           rs          -> "     , prcRRels = [ "++intercalate (indentB++", ") ["("++show rol++", "++showHS flags "" rel++")" | (rol,rel)<-rs] ++indentB++"]"
        , wrap ", prcIds = " indentB (showHS flags) (prcIds prc)
        , wrap ", prcVds = " indentB (showHS flags) (prcVds prc)
        , wrap ", prcXps = " indentB (showHS flags) (prcXps prc)
        , "}"
        ] where indentA = indent ++"      "     -- adding the width of "FProc "
                indentB = indentA++"             " -- adding the width of ", prcRules = "


   instance ShowHS Activity where
    showHS flags indent act = 
       intercalate indentA
        [ "Act { actRule   = "++showHSName (actRule act)
        , wrap ", actTrig   = " indentB (\_->showHSName) (actTrig   act)
        , wrap ", actAffect = " indentB (\_->showHSName) (actAffect act)
        , wrap ", actQuads  = " indentB (\_->showHSName) (actQuads  act)
        , wrap ", actEcas   = " indentB (\_->showHSName) (actEcas   act)
        , wrap ", actPurp   = " indentB (\_->(showHS flags indentB)) (actPurp act)
        , "      }"
        ]
       where indentA = indent ++replicate (length "Act "          ) ' ' 
             indentB = indentA++replicate (length ", actAffect = ") ' '

   instance ShowHS PPurpose where
    showHS flags _ expla = 
       "PRef2 ("++showHS flags "" (pexPos     expla)++") "++
             "("++showHS flags "" (pexObj     expla)++") "++
             "("++showHS flags "" (pexMarkup  expla)++") "
                ++show (pexRefID expla)++" "
                
   instance ShowHS PRef2Obj where
    showHS _ _ peObj
     = case peObj of 
            PRef2ConceptDef str               -> "PRef2ConceptDef " ++show str
            PRef2Declaration (PTrel _ nm sgn) -> "PRef2Declaration "++show nm++show sgn
            PRef2Declaration (Prel _ nm)      -> "PRef2Declaration "++show nm
            PRef2Declaration expr             -> fatal 583 ("Expression "++show expr++" should never occur in PRef2Declaration")
            PRef2Rule str                     -> "PRef2Rule "       ++show str
            PRef2IdentityDef str              -> "PRef2IdentityDef "++show str
            PRef2ViewDef str                  -> "PRef2ViewDef "    ++show str
            PRef2Pattern str                  -> "PRef2Pattern "    ++show str
            PRef2Process str                  -> "PRef2Process "    ++show str
            PRef2Interface str                -> "PRef2Interface "  ++show str
            PRef2Context str                  -> "PRef2Context "    ++show str
            PRef2Fspc str                     -> "PRef2Fspc "       ++show str
                           
   instance ShowHS Purpose where
    showHS flags _ expla = 
       "Expl "++"("++showHS flags "" (explPos expla)++") "
              ++"("++showHS flags "" (explObj expla)++") "
                   ++showHS flags "" (explMarkup  expla)++" "
                   ++show (explUserdefd expla)++" "
                   ++show (explRefId expla)++" "

   instance ShowHS ExplObj where
    showHS _ {-flags-} _ {-i-} peObj = case peObj of                     -- SJ: names of variables commented out to prevent warnings.
             ExplConceptDef cd  -> "ExplConceptDef " ++showHSName cd
             ExplDeclaration d  -> "ExplDeclaration "++showHSName d
             ExplRule str       -> "ExplRule "       ++show str
             ExplIdentityDef str-> "ExplIdentityDef "++show str
             ExplViewDef str    -> "ExplViewDef "    ++show str
             ExplPattern str    -> "ExplPattern "    ++show str
             ExplProcess str    -> "ExplProcess "    ++show str
             ExplInterface str  -> "ExplInterface "  ++show str
             ExplContext str    -> "ExplContext "    ++show str
           
   instance ShowHS P_Markup where
    showHS _ indent m
      = intercalate indent 
        ["P_Markup{ mLang   = "++ show (mLang m)
        ,"        , mFormat = "++ show (mFormat m)
        ,"        , mString = "++ show (mString m)
        ,"        }"
        ]

   instance ShowHS A_Markup where
    showHS _ indent m
      = intercalate indent 
        ["A_Markup{ amLang   = "++ show (amLang m)
        ,"        , amFormat = "++ show (amFormat m)
        ,"        , amPandoc = "++ show (amPandoc m)
        ,"        }"
        ]

   instance ShowHS (PairView Expression) where
     showHS flags indent (PairView pvs) = "PairView "++showHS flags indent pvs
     
   instance ShowHS (PairViewSegment Expression) where
     showHS _     _ (PairViewText txt) = "PairViewText "++show txt
     showHS flags _ (PairViewExp srcOrTgt e) = "PairViewExp "++show srcOrTgt++" ("++showHS flags "" e++")"


   instance ShowHSName Rule where
    showHSName r = haskellIdentifier ("rule_"++ rrnm r)

   instance ShowHS Rule where
    showHS flags indent r@(Ru _ _ _ _ _ _ _ _ _ _ _ _)  -- This pattern matching occurs so Haskell will detect any change in the definition of Ru.
      = intercalate indent 
        ["Ru{ rrnm   = " ++ show (rrnm   r)
        ,"  , rrexp  = " ++ showHS flags (indent++"             ") (rrexp  r)
        ,"  , rrfps  = " ++ showHS flags "" (rrfps  r)
        ,"  , rrmean = " ++ showHS flags (indent++"             ") (rrmean r)
        ,"  , rrmsg  = " ++ showHS flags "" (rrmsg  r)
        ,"  , rrviol = " ++ showHS flags "" (rrviol r)
        ,"  , rrtyp  = " ++ showHS flags "" (rrtyp  r)
        ,"  , rrdcl  = " ++ case rrdcl r of
                              Just (p,d) -> "Just ("++showHSName p++", "++showHSName d++" )"
                              Nothing    -> "Nothing"
        ,"  , r_env  = " ++ show (r_env  r)
        ,"  , r_usr  = " ++ show (r_usr  r)
        ,"  , r_sgl  = " ++ show (r_sgl  r)
        ,"  , srrel  = " ++ showHSName (srrel  r)
        ,"  }"
        ]

   instance ShowHS AMeaning where
     showHS flags indent (AMeaning x) = "AMeaning " ++ showHS flags (indent++"        ") x 

   instance ShowHS RuleType where
     showHS _ _ Truth          = "Truth"
     showHS _ _ Equivalence    = "Equivalence"
     showHS _ _ Implication    = "Implication"
   

   instance ShowHSName IdentityDef where
    showHSName identity = haskellIdentifier ("identity_"++name identity)
   
   instance ShowHS IdentityDef where
    showHS flags indent identity
     = "Id ("++showHS flags "" (idPos identity)++") "++show (idLbl identity)++" ("++showHSName (idCpt identity)++")"
       ++indent++"  [ "++intercalate (indent++"  , ") (map (showHS flags indent) $ identityAts identity)++indent++"  ]"
   

   
   instance ShowHS IdentitySegment where
    showHS flags indent (IdentityExp objDef) = "IdentityExp ("++ showHS flags indent objDef ++ ")"


   instance ShowHSName ViewDef where
    showHSName vd = haskellIdentifier ("vdef_"++name vd)
   
   instance ShowHS ViewDef where
    showHS flags indent vd
     = "Vd ("++showHS flags "" (vdpos vd)++") "++show (vdlbl vd)++" ("++showHSName (vdcpt vd)++")"
       ++indent++"  [ "++intercalate (indent++"  , ") (map (showHS flags indent) $ vdats vd)++indent++"  ]"
   

   --instance ShowHSName ViewSegment where
   -- showHSName vd = haskellIdentifier ("vdef_"++name vd)
   
   instance ShowHS ViewSegment where
    showHS _     _      (ViewText str) = "ViewText "++show str
    showHS _     _      (ViewHtml str) = "ViewHtml "++show str
    showHS flags indent (ViewExp objDef) = "ViewExp ("++ showHS flags indent objDef ++ ")"
   
   instance ShowHS Population where
    showHS _ indent pop
     = case pop of 
         PRelPopu{} -> "PRelPopu { popdcl = "++showHSName (popdcl pop)
             ++indent++"         , popps  = [ "++intercalate 
              (indent++"                    , ") (map show (popps pop))
             ++indent++"                    ]"
             ++indent++"         }"
         PCptPopu{} -> "PCptPopu { popcpt = "++showHSName (popcpt pop)
             ++indent++"         , popas  = [ "++intercalate
              (indent++"                    , ") (map show (popas pop))
             ++indent++"                    ]"
             ++indent++"         }"
   
   instance ShowHSName ObjectDef where
    showHSName obj = haskellIdentifier ("oDef_"++name obj)

   instance ShowHS ObjectDef where
    showHS flags indent r 
     = intercalate indent
           ["Obj{ objnm   = " ++ show(objnm r)
           ,"   , objpos  = " ++ showHS flags "" (objpos r)  
           ,"   , objctx  = " ++ showHS flags (indent++"               ") (objctx r)
           ,"   , objmsub = " ++ showHS flags (indent++"                    ") (objmsub r)
           ,"   , objstrs = " ++ show(objstrs r)
           ]++indent++"   }"

   instance ShowHSName Interface where
    showHSName obj = haskellIdentifier ("ifc_"++name obj)
   
   instance ShowHS Interface where
    showHS flags indent ifc
     = intercalate indent 
           [ wrap "Ifc { ifcParams = " (indent++"                  ") (showHS flags) (ifcParams ifc)
           , "    , ifcArgs   = " ++ show(ifcArgs ifc)
           , "    , ifcRoles  = " ++ show(ifcRoles ifc)
           , "    , ifcObj"++indent++"       = " ++ showHS flags (indent++"         ") (ifcObj ifc)
           , "    , ifcPos    = " ++ showHS flags "" (ifcPos ifc)
           , "    , ifcPrp    = " ++ show(ifcPrp ifc)
           ]++indent++"    }"

   instance ShowHS SubInterface where
    showHS _     _      (InterfaceRef n) = "InterfaceRef "++show n 
    showHS flags indent (Box x objs) = "Box ("++showHS flags indent x++")"++indent++"     ("++showHS flags (indent++"     ") objs++")" 

   instance ShowHS Expression where
    showHS flags indent (EEqu (l,r)) = "EEqu ("++showHS flags (indent++"      ") l++indent++"     ,"++showHS flags (indent++"      ") r++indent++"     )"
    showHS flags indent (EImp (l,r)) = "EImp ("++showHS flags (indent++"      ") l++indent++"     ,"++showHS flags (indent++"      ") r++indent++"     )"
    showHS flags indent (EIsc (l,r)) = "EIsc ("++showHS flags (indent++"      ") l++indent++"     ,"++showHS flags (indent++"      ") r++indent++"     )"
    showHS flags indent (EUni (l,r)) = "EUni ("++showHS flags (indent++"      ") l++indent++"     ,"++showHS flags (indent++"      ") r++indent++"     )"
    showHS flags indent (EDif (l,r)) = "EDif ("++showHS flags (indent++"      ") l++indent++"     ,"++showHS flags (indent++"      ") r++indent++"     )"
    showHS flags indent (ELrs (l,r)) = "ELrs ("++showHS flags (indent++"      ") l++indent++"     ,"++showHS flags (indent++"      ") r++indent++"     )"
    showHS flags indent (ERrs (l,r)) = "ERrs ("++showHS flags (indent++"      ") l++indent++"     ,"++showHS flags (indent++"      ") r++indent++"     )"
    showHS flags indent (ECps (l,r)) = "ECps ("++showHS flags (indent++"      ") l++indent++"     ,"++showHS flags (indent++"      ") r++indent++"     )"
    showHS flags indent (ERad (l,r)) = "ERad ("++showHS flags (indent++"      ") l++indent++"     ,"++showHS flags (indent++"      ") r++indent++"     )"
    showHS flags indent (EPrd (l,r)) = "EPrd ("++showHS flags (indent++"      ") l++indent++"     ,"++showHS flags (indent++"      ") r++indent++"     )"
    showHS flags indent (EKl0 e    ) = "EKl0 ("++showHS flags (indent++"      ") e++")"
    showHS flags indent (EKl1 e    ) = "EKl1 ("++showHS flags (indent++"      ") e++")"
    showHS flags indent (EFlp e    ) = "EFlp ("++showHS flags (indent++"      ") e++")"
    showHS flags indent (ECpl e    ) = "ECpl ("++showHS flags (indent++"      ") e++")"
    showHS flags indent (EBrk e    ) = "EBrk ("++showHS flags (indent++"      ") e++")"
    showHS _     _      (EDcD dcl  ) = "EDcD "++showHSName dcl
    showHS _     _      (EDcI c    ) = "EDcI "++showHSName c
    showHS flags _      (EEps i sgn) = "EEps ("++showHS flags "" i++") ("++showHS flags "" sgn++")"
    showHS flags _      (EDcV sgn  ) = "EDcV ("++showHS flags "" sgn++")"
    showHS _     _      (EMp1 a c  ) = "EMp1 ("++show a++") "++showHSName c

   instance ShowHS Sign where
    showHS _ _ sgn = "Sign "++showHSName (source sgn)++" "++showHSName (target sgn)
   
   instance ShowHS A_Gen where
    showHS flags _ gen =
      case gen of 
        Isa{} -> "Isa ("++showHS flags "" (genfp gen)++") ("++showHSName (genspc gen)++") ("++showHSName (gengen gen)++") "
        IsE{} -> "IsE ("++showHS flags "" (genfp gen)++") ("++showHSName (genspc gen)++") ["++intercalate ", " (map showHSName (genrhs gen))++"] "
   
   instance ShowHSName Declaration where
    showHSName d@Isn{}       = haskellIdentifier ("rel_"++name d++"_"++name (source d)) -- identity relation
    showHSName d@Vs{}        = haskellIdentifier ("rel_"++name d++"_"++name (source d)++name (target d)) -- full relation
    showHSName d | decusr d  = haskellIdentifier ("rel_"++name d++name (source d)++name (target d)) -- user defined relations
                 | deciss d  = haskellIdentifier ("sgn_"++name d++name (source d)++name (target d)) -- relations generated for signalling
                 | otherwise = haskellIdentifier ("vio_"++name d++name (source d)++name (target d)) -- relations generated per rule
   
   instance ShowHS Declaration where
    showHS flags indent d 
       = case d of 
          Sgn{}     -> intercalate indent
                        ["Sgn{ decnm   = " ++ show (decnm d)
                        ,"   , decsgn  = " ++ showHS flags "" (sign d)
                        ,"   , decprps = " ++ showL(map (showHS flags "") (decprps d))
                        ,"   , decprps_calc = " ++ case decprps_calc d of
                                                    Nothing -> "Nothing"
                                                    Just ps -> "Just "++showL(map (showHS flags "") ps)
                        ,"   , decprL  = " ++ show (decprL d)
                        ,"   , decprM  = " ++ show (decprM d)
                        ,"   , decprR  = " ++ show (decprR d)
                        ,"   , decMean = " ++ show (decMean d)
                        ,"   , decConceptDef = " ++ show (decConceptDef d)
                        ,"   , decfpos = " ++ showHS flags "" (decfpos d)
                        ,"   , deciss  = " ++ show (deciss d)
                        ,"   , decusr  = " ++ show (decusr d)
                        ,"   , decISA  = " ++ show (decISA d)
                        ,"   , decpat  = " ++ show (decpat d)
                        ,"   , decplug = " ++ show (decplug d)
                        ]++"}"
          Isn{}     -> "Isn{ detyp   = " ++ showHSName (detyp d)++"}"
          Vs{}      -> "Vs { decsgn  = " ++ showHS flags "" (sign d)++"}"


   instance ShowHSName ConceptDef where
    showHSName cd = haskellIdentifier ("cDef_"++cdcpt cd)

   instance ShowHS ConceptDef where
    showHS flags _ cd
     = " Cd ("++showHS flags "" (cdpos cd)++") "++show (cdcpt cd)++" "++show (cdplug cd)++" "++show (cddef cd)++" "++show (cdtyp cd)++" "++show (cdref cd)

   instance ShowHSName A_Concept where
    showHSName ONE = haskellIdentifier "cptOne"
    showHSName c = haskellIdentifier ("cpt_"++name c) 
   instance ShowHS A_Concept where
    showHS _ _ c = case c of
                       PlainConcept{} -> "PlainConcept "++show (name c) ++ " "++ show (cpttp c) ++ " ["++intercalate ", " (map showHSName (cptdf c))++"]"
                       ONE -> "ONE"

   instance ShowHS FPcompl where
    showHS _ _   = show

   instance ShowHS FPA where
    showHS _ _ (FPA t c) = "FPA "++show t++" "++show c
   
   instance ShowHSName Prop where
    showHSName Uni = "Uni"
    showHSName Inj = "Inj"
    showHSName Sur = "Sur"
    showHSName Tot = "Tot"
    showHSName Sym = "Sym"
    showHSName Asy = "Asy"
    showHSName Trn = "Trn"
    showHSName Rfx = "Rfx"
    showHSName Irf = "Irf"

   instance ShowHS Prop where
    showHS _ _ = showHSName
    
   instance ShowHS FilePos where
    showHS _ _ (FilePos (fn,DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner.Pos l c,sym))
      = "FilePos ("++show fn++",Pos "++show l++" "++show c++","++show sym++")"

   instance ShowHSName Origin where
    showHSName ori = "Orig"++show x++show (hash x)
      where x = case ori of
                 FileLoc l -> "FileLoc (" ++ show l++")"
                 DBLoc l   -> "DBLoc " ++ show l
                 Origin s  -> "Origin " ++ show s
                 OriginUnknown -> "OriginUnknown"
                 SomewhereNear s -> "SomewhereNear "++show s
   
   instance ShowHS Origin where
    showHS flags indent (FileLoc l) = "FileLoc (" ++ showHS flags indent l++")"
    showHS _ _ (DBLoc l) = "DBLoc " ++ show l
    showHS _ _ (Origin s) = "Origin " ++ show s
    showHS _ _ OriginUnknown
      = "OriginUnknown"
    showHS _ _ (SomewhereNear s) 
      = "SomewhereNear "++show s



   instance ShowHS Block where
    showHS _ _   = show


   instance ShowHS Inline where
    showHS _ _   = show

--   instance ShowHS InfTree where
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

   showL :: [String] -> String
   showL xs = "["++intercalate "," xs++"]"



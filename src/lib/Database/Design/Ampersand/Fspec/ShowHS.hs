{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
module Database.Design.Ampersand.Fspec.ShowHS (ShowHS(..),ShowHSName(..),fSpec2Haskell,haskellIdentifier)
where
   import Database.Design.Ampersand.Core.ParseTree
   import Database.Design.Ampersand.Core.AbstractSyntaxTree
   import Text.Pandoc hiding (Meta)
   import Data.Char                  (isAlphaNum)
   import Database.Design.Ampersand.Basics
   import Database.Design.Ampersand.Fspec.Plug
   import Database.Design.Ampersand.Fspec.Fspec
   import Database.Design.Ampersand.Fspec.ShowADL    (ShowADL(..))  -- for traceability, we generate comment in the Haskell code.
--   import Database.Design.Ampersand.Fspec.FPA   (fpa)
   import Data.List
   import Database.Design.Ampersand.Classes
   import qualified Database.Design.Ampersand.Input.ADL1.UU_Scanner
   import Database.Design.Ampersand.Misc
   import Data.Hashable
   import Data.Ord
   import Data.Function

   fatal :: Int -> String -> a
   fatal = fatalMsg "Fspec.ShowHS"

   fSpec2Haskell :: Fspc -> String
   fSpec2Haskell fSpec
           = "{-# OPTIONS_GHC -Wall #-}"
             ++"\n{-Generated code by "++ampersandVersionStr++" at "++show (genTime (flags fSpec))++"-}"
             ++"\nmodule Main where"
             ++"\n  import Database.Design.Ampersand"
             ++"\n  import Text.Pandoc hiding (Meta)"
             ++"\n  import Prelude hiding (writeFile,readFile,getContents,putStr,putStrLn)"
             ++"\n"
             ++"\n  main :: IO ()"
             ++"\n  main = do (flags fSpec) <- getOptions"
             ++"\n            putStr (showHS (flags fSpec) \"\\n  \" fSpec_"++baseName (flags fSpec)++")"
             ++"\n  fSpec_"++baseName (flags fSpec)++" :: Fspc"
             ++"\n  fSpec_"++baseName (flags fSpec)++"\n   = "++showHS (flags fSpec) "\n     " fSpec

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
    showHS opts indent = wrap "" (indent++" ") (showHS opts)

   instance ShowHSName a => ShowHSName (Maybe a) where
    showHSName Nothing  = "Nothing"
    showHSName (Just x) = showHSName x

   instance ShowHS a => ShowHS (Maybe a) where
    showHS _ _ Nothing  = "Nothing"
    showHS opts indent (Just x) = "Just (" ++ showHS opts indent x ++ ")"

   instance (ShowHSName a , ShowHSName b) => ShowHSName (a,b) where
    showHSName (a,b) = "( "++showHSName a++" , "++showHSName b++" )"
   -- | The following is used to showHS opts for signs: (Concept, Concept)
--   instance (ShowHS a , ShowHS b) => ShowHS (a,b) where
--    showHS opts indent (a,b) = "("++showHS opts (indent++" ") a++","++showHS opts (indent++" ") b++")"

   instance ShowHSName PlugSQL where
    showHSName plug = haskellIdentifier ("plug_"++name plug)

   instance ShowHS PlugSQL where
    showHS opts indent plug
      = case plug of
          TblSQL{} -> intercalate indent
                      ["let " ++ intercalate (indent++"    ")
                                             [showHSName f++indent++"     = "++showHS opts (indent++"       ") f | f<-fields plug] ++indent++"in"
                      ,"TblSQL { sqlname = " ++ (show.name) plug
                      ,"       , fields  = ["++intercalate ", " (map showHSName (fields plug))++"]"
                      ,"       , cLkpTbl = [ "++intercalate (indent++"                   , ") ["("++showHSName c++", "++showHSName cn++")" | (c,cn)<-cLkpTbl plug] ++ "]"
                      ,"       , mLkpTbl = [ "++intercalate (indent++"                   , ") ["("++showHS opts "" r++", "++showHSName ms++", "++showHSName mt++")" | (r,ms,mt)<-mLkpTbl plug] ++ "]"
                  --    ,"       , sqlfpa  = " ++ showHS opts "" (fpa plug)
                      ,"       }"
                      ]
          BinSQL{} -> intercalate indent
                      ["let " ++ showHSName (fst (columns plug))++indent++"     = "++showHS opts (indent++"       ") (fst (columns plug))
                              ++ (indent++"    ") ++ showHSName (snd (columns plug))++indent++"     = "++showHS opts (indent++"       ") (snd (columns plug))
                              ++indent++"in"
                      ,"BinSQL { sqlname = " ++ (show.name) plug
                      ,"       , columns = ("++showHSName (fst (columns plug))++ ", " ++showHSName (snd (columns plug))++")"
                      ,"       , cLkpTbl = [ "++intercalate (indent++"                   , ") ["("++showHSName c++", "++showHSName cn++")" | (c,cn)<-cLkpTbl plug] ++ "]"
                      ,"       , mLkp = "++showHS opts "" (mLkp plug)
                  --    ,"       , sqlfpa  = " ++ showHS opts "" (fpa plug)
                      ,"       }"
                      ]
          ScalarSQL{} -> intercalate indent
                      ["ScalarSQL { sqlname   = "++ (show.name) plug
                      ,"          , sqlColumn = "++ showHS opts (indent++"                     ") (sqlColumn plug)
                      ,"          , cLkp      = "++ showHSName (cLkp plug)
                  --    ,"          , sqlfpa    = "++ showHS opts "" (fpa plug)
                      ,"          }"
                      ]

   instance ShowHSName (ECArule) where
    showHSName r = "ecaRule"++show (ecaNum r)

   instance ShowHS (ECArule) where
    showHS opts indent r
      =         "ECA { ecaTriggr = " ++ showHS opts "" (ecaTriggr r) ++
        indent++"    , ecaDelta  = " ++ showHS opts (indent++"                  ")  (ecaDelta r)++
        indent++"    , ecaAction = " ++ showHS opts (indent++"                  ")  (ecaAction r)++
        indent++"    , ecaNum    = " ++ show (ecaNum r)++
        indent++"    }"

   instance ShowHS Event where
    showHS _ indent e
      = if "\n" `isPrefixOf` indent
        then "On " ++ show (eSrt e)++indent++"   " ++ showHSName (eDcl e)++indent++"   "
        else "On " ++ show (eSrt e)++          " " ++ showHSName (eDcl e)++           ""

   instance ShowHS (InsDel, Expression, PAclause) where
    showHS opts indent (tOp, links, p)
      = "( "++show tOp++indent++", "++showHS opts (indent++"  ") links++indent++", "++showHS opts (indent++"  ") p++indent++")"

   instance ShowHS PAclause where
    showHS opts indent p
      = case p of
           CHC{} -> wrap "CHC " (indent ++"    ") (showHS opts) (paCls p)++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
           GCH{} -> wrap "GCH " (indent ++"    ") (showHS opts) (paGCls p)++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
           ALL{} -> wrap "ALL " (indent ++"    ") (showHS opts) (paCls p)++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
           Do{}  ->  "Do "++show (paSrt p)++ " ("++showHS opts (indent++"        ") (paTo p)++indent++"       )"++
                            indent++"       ("++showHS opts (indent++"        ") (paDelta p)++indent++"       )"++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
           New{} -> "New ("++showHS opts "" (paCpt p)++")"++
                    indent++"    (\\x->"++showHS opts (indent++"        ") (paCl p "x")++indent++"    )"++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
           Rmv{} -> "Rmv ("++showHS opts "" (paCpt p)++")"++
                    indent++"    (\\x->"++showHS opts (indent++"        ") (paCl p "x")++indent++"    )"++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
           Nop{} -> "Nop "++wrap "" (indent ++"    ") showMotiv ms
           Blk{} -> "Blk "++wrap "" (indent ++"    ") showMotiv ms
           Let{} -> wrap "Let " (indent ++"    ") (showHS opts) (paCls p)++
                    "TODO: paBody of Let clause"++
                    wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
           Ref{} -> "Ref "++paVar p
        where ms = paMotiv p
              showMotiv ind (conj,rs) = "( "++showHS opts (ind++"  ") conj++" -- conjunct:  "++showADL conj++ind++", "++showHSName rs++ind++")"

   instance ShowHSName SqlField where
    showHSName sqFd = haskellIdentifier ("sqlFld_"++fldname sqFd)

   instance ShowHS SqlField where
    showHS opts indent sqFd
      = intercalate indentA
          [  "Fld { fldname = " ++ show (fldname sqFd)
          ,      ", fldexpr = " ++ showHS opts indentB (fldexpr sqFd)
          ,      ", fldtype = " ++ showHS opts "" (fldtype sqFd)
          ,      ", flduse  = " ++ showHS opts "" (flduse sqFd)
          ,      ", fldnull = " ++ show (fldnull sqFd)
          ,      ", flduniq = " ++ show (flduniq sqFd)
          ,      "}"
          ] where indentA = indent ++"    "         -- adding the width of "Fld "
                  indentB = indentA++"            " -- adding the width of ", fldexpr = "

   instance ShowHS SqlFieldUsage where
    showHS _ _ (TableKey isPrimary aCpt) = "TableKey "  ++show isPrimary++" "++showHSName aCpt
    showHS _ _ (ForeignKey aCpt)         = "ForeignKey "++showHSName aCpt
    showHS _ _ PlainAttr                 = "PlainAttr "

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
      = haskellIdentifier ("quad_"++(showHSName.qDcl) q++"_"++(name.qRule) q)

   instance ShowHS Quad where
    showHS opts indent q
      = intercalate indent
               [ "Quad{ qDcl     = " ++ showHSName (qDcl q)
               , "    , qRule    = " ++ showHSName (qRule q)
               , "    , qClauses = " ++ showHS opts newindent (qClauses q)
               , "    }"
               ]
       where
         newindent = indent ++ "                 "

   instance ShowHS Fswitchboard where
    showHS opts indent fsb
      = intercalate indent
          [ "Fswtch { fsbEvIn  = " ++ showHS opts newindent (fsbEvIn  fsb)
          , "       , fsbEvOut = " ++ showHS opts newindent (fsbEvOut fsb)
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
         shConj (r,conj) = "( "++showHSName r++newindent++"   , "++showHS opts newindent'' conj++newindent++"   )"

   instance ShowHS DnfClause where
    showHS opts indent (Dnf antcs conss)
      = intercalate indent
          [ wrap "Dnf " (indent++"    ") (\_->showHS opts (indent++"      ")) antcs
          , wrap "    " (indent++"    ") (\_->showHS opts (indent++"      ")) conss
          ]

   instance ShowHSName RuleClause where
    showHSName x = haskellIdentifier ("conj_"++rc_rulename x++"_"++show (rc_int x))

   instance ShowHS RuleClause where
    showHS opts indent x
      = intercalate (indent ++"  ")
          [   "RC{ rc_int        = " ++ show (rc_int x)
          ,     ", rc_rulename   = " ++ show (rc_rulename x)
          ,     ", rc_conjunct   = " ++ showHS opts indentA (rc_conjunct x)
          ,wrap ", rc_dnfClauses = " indentA (\_->showHS opts (indentA++"  ")) (rc_dnfClauses x)
          ,     "}"
          ]
        where indentA = indent ++"                    "

   instance ShowHSName Fspc where
    showHSName fSpec = haskellIdentifier ("fSpc_"++name fSpec)

   instance ShowHS Fspc where
    showHS opts indent fSpec
     = intercalate (indent ++"    ")
           [ "Fspc{ fsName        = " ++ show (name fSpec)
           ,wrap ", fspos         = " indentA (showHS opts) (fspos fSpec)
           ,     ", fsLang        = " ++ show (fsLang fSpec) ++ "  -- the default language for this specification"
           ,     ", themes        = " ++ show (themes fSpec) ++ "  -- the names of themes to be printed in the documentation, meant for partial documentation.  Print all if empty..."
           ,wrap ", pattsInScope  = " indentA (\_->showHSName) (pattsInScope fSpec)
           ,wrap ", procsInScope  = " indentA (\_->showHSName) (procsInScope fSpec)
           ,wrap ", rulesInScope  = " indentA (\_->showHSName) (rulesInScope fSpec)
           ,wrap ", declsInScope  = " indentA (\_->showHSName) (declsInScope fSpec)
           ,wrap ", cDefsInScope  = " indentA (\_->showHS opts (indentA++"  ")) (cDefsInScope fSpec)
           ,wrap ", gensInScope   = " indentA (showHS opts)   (gensInScope fSpec)
           ,wrap ", vprocesses    = " indentA (\_->showHSName) (vprocesses fSpec)
           ,wrap ", vplugInfos    = " indentA (\_->showHS opts (indentA++"  ")) (vplugInfos fSpec)
           ,wrap ", plugInfos     = " indentA (\_->showHS opts (indentA++"  ")) (plugInfos  fSpec)
           ,     ", interfaceS    = interfaceS'"
           ,     ", interfaceG    = interfaceG'"
           ,wrap ", fActivities   = " indentA (\_->showHS opts (indentA++"  ")) (fActivities fSpec)
           ,     ", fRoleRels     = " ++
                 case fRoleRels fSpec of
                   []        -> "[]"
                   [(r,rel)] -> "[ ("++show r++", "++showHS opts "" rel++") ]"
                   _         -> "[ "++intercalate (indentA++", ") ["("++show r++","++showHS opts "" rel++")" | (r,rel)<-fRoleRels fSpec]++indentA++"]"
           ,     ", fRoleRuls     = " ++
                 case fRoleRuls fSpec of
                   []        -> "[]"
                   [(r,rul)] -> "[ ("++show r++", "++showHSName rul++") ]"
                   _         -> "[ "++intercalate (indentA++", ") ["("++show r++","++showHSName rul++")" | (r,rul)<-fRoleRuls fSpec]++indentA++"]"
           ,wrap ", fRoles        = " indentA (\_->id) (fRoles fSpec)
           ,wrap ", vrules        = " indentA (\_->showHSName) (vrules fSpec)
           ,wrap ", grules        = " indentA (\_->showHSName) (grules fSpec)
           ,wrap ", invars        = " indentA (\_->showHSName) (invars fSpec)
           ,wrap ", allRules      = " indentA (\_->showHSName) (allRules fSpec)
           ,wrap ", allUsedDecls  = " indentA (\_->showHSName) (allUsedDecls fSpec)
           ,wrap ", allDecls      = " indentA (\_->showHSName) (allDecls fSpec)
           ,wrap ", vrels         = " indentA (\_->showHSName) (vrels fSpec)
           ,wrap ", allConcepts   = " indentA (\_->showHSName) (allConcepts fSpec)
           ,wrap ", kernels       = " indentA (\_->showHSName) (kernels fSpec)
           ,wrap ", vIndices      = " indentA (\_->showHSName) (vIndices fSpec)
           ,wrap ", vviews        = " indentA (\_->showHSName) (vviews fSpec)
           ,wrap ", vgens         = " indentA (showHS opts)   (vgens fSpec)
           ,wrap ", fsisa         = " indentA (\_->showHSName) (fsisa fSpec)
           ,wrap ", vconjs        = " indentA (\_->showHSName) (vconjs fSpec)
           ,wrap ", vquads        = " indentA (\_->showHSName) (vquads fSpec)
           ,wrap ", vEcas         = " indentA (\_->showHSName) (vEcas fSpec)
           ,     ", fSwitchboard  = "++showHS opts indentA (fSwitchboard fSpec)
           ,wrap ", vpatterns     = " indentA (\_->showHSName) (patterns fSpec)
           ,wrap ", conceptDefs   = " indentA (showHS opts) (conceptDefs fSpec)
           ,wrap ", fSexpls       = " indentA (showHS opts)   (fSexpls fSpec)
           ,     ", metas         = allMetas"
           ,wrap ", initialPops   = " indentA (showHS opts)   (initialPops fSpec)
           ,wrap ", allViolations = " indentA showViolatedRule (allViolations fSpec)
           ,"}"
           ] ++
       indent++"where"++
        "\n -- ***Interfaces Specified in Ampersand script***: "++
       indent++" interfaceS' = "++(if null (interfaceS fSpec) then "[]" else
                                 "[ "++intercalate (indentB++"  , ") (map showHSName (interfaceS fSpec))++indentB++"  ]")++
        "\n -- ***Activities Generated by the Ampersand compiler ***: " ++
       indent++" interfaceG' = "++(if null (interfaceG fSpec) then "[]" else
                                 "[ "++intercalate (indentB++", ") (map showHSName (interfaceG fSpec))++indentB++"]")++
       indent++" allMetas = "++(if null (metas fSpec) then "[]" else
                                 "[ "++intercalate (indentB++", ") (map (showHS opts (indent ++ "         ")) (metas fSpec))++indentB++"]") ++

-- WHY?  staan hier verschillende lijstjes met interfaces?
-- BECAUSE! Een Ampersand engineer besteedt veel tijd om vanuit een kennismodel (lees: een graaf met concepten en relaties)
--          alle interfaces met de hand te verzinnen.
--          Je kunt natuurlijk ook een interfaces-generator aan het werk zetten, die een aantal interfaces klaarzet bij wijze
--          van steiger (scaffold). Dat bespaart een hoop werk. De functie interfaceG is zo'n generator.
--          Door de gegenereerde interfaces af te drukken, kun je dus heel snel Ampersand sourcecode maken met correct-vertaalbare interfaces.
--          Heb je eenmaal een goed werkend pakket interfaces, dan wil je wellicht alleen de door jezelf gespecificeerde interfaces
--          gebruiken. Dat gebeurt in interfaceS.

       (if null  (interfaceS fSpec) then ""  else
        "\n -- *** User defined interfaces (total: "++(show.length.interfaceS) fSpec++" interfaces) ***: "++
        concat [indent++" "++showHSName s++indent++"  = "++showHS opts (indent++"    ") s | s<-interfaceS fSpec]++"\n"
       )++
       (if null (interfaceG fSpec ) then "" else
        "\n -- *** Generated interfaces (total: "++(show.length.interfaceG) fSpec++" interfaces) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS opts (indent++"    ") x |x<-interfaceG fSpec ]++"\n"
       )++
       (let ds fs = allDecls fs `uni` allUsedDecls fs `uni` vrels fSpec `uni` (nub . map qDcl . vquads) fs in
        if null (ds fSpec)     then "" else
        "\n -- *** Declared relations (in total: "++(show.length.ds) fSpec++" relations) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS opts (indent++"    ") x |x<-ds fSpec]++"\n"
       ) ++
       (if null (vIndices fSpec)     then "" else
        "\n -- *** Indices (total: "++(show.length.vIndices) fSpec++" indices) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS opts (indent++"    ") x |x<-vIndices fSpec]++"\n"
       ) ++
       (if null (vviews fSpec)     then "" else
        "\n -- *** Views (total: "++(show.length.vviews) fSpec++" views) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS opts (indent++"    ") x |x<-vviews fSpec]++"\n"
       ) ++
       (if null (vprocesses fSpec ) then "" else
        "\n -- *** Processes (total: "++(show.length.vprocesses) fSpec++" processes) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS opts (indent++"    ") x |x<-vprocesses fSpec ]++"\n"++
        concat [indent++" "++showHSName x++indent++"  = "++showHS opts (indent++"    ") x |x<-map fpProc (vprocesses fSpec) ]++"\n"
       ) ++
       (if null (vrules   fSpec ) then "" else
        "\n -- *** User defined rules (total: "++(show.length.vrules) fSpec++" rules) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS opts (indent++"    ") x |x<-vrules     fSpec ]++"\n"++
        concat [indent++" "++showHSName x++indent++"  = "++showHS opts (indent++"    ") x |x<-map srrel (vrules fSpec)]++"\n"
       )++
       (if null (grules   fSpec ) then "" else
        "\n -- *** Generated rules (total: "++(show.length.grules) fSpec++" rules) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS opts (indent++"    ") x |x<-grules     fSpec ]++"\n"++
        concat [indent++" "++showHSName x++indent++"  = "++showHS opts (indent++"    ") x |x<-map srrel (grules fSpec)]++"\n"
       )++
       (if null (vconjs fSpec ) then "" else
        "\n -- *** Conjuncts (total: "++(show.length.vconjs) fSpec++" conjuncts) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS opts (indent++"    ") x |x<-vconjs     fSpec ]++"\n"
       )++
       (if null (vquads fSpec ) then "" else
        "\n -- *** Quads (total: "++(show.length.vquads) fSpec++" quads) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS opts (indent++"    ") x |x<-vquads     fSpec ]++"\n"
       )++
       (if null (vEcas fSpec ) then "" else
        "\n -- *** ECA rules (total: "++(show.length.vEcas) fSpec++" ECA rules) ***: "++
        concat [indent++" "++showHSName eca++indent++"  = "++showHS opts (indent++"    ") eca |eca<-vEcas fSpec ]++"\n"++
        concat [indent++" "++showHSName rel++indent++"  = "++showHS opts (indent++"    ") rel |rel<-nub(map ecaDelta (vEcas fSpec)) ]++"\n"
       )++
       (if null (plugInfos fSpec ) then "" else
        "\n -- *** PlugInfos (total: "++(show.length.plugInfos) fSpec++" plugInfos) ***: "++
        concat [indent++" "++showHSName p++indent++"  = "++showHS opts (indent++"    ") p |InternalPlug p<-sortBy (compare `on` name) (plugInfos fSpec) ]++"\n"
       )++
       (if null (vpatterns fSpec) then "" else
        "\n -- *** Patterns (total: "++(show.length.vpatterns) fSpec++" patterns) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS opts (indent++"    ") x |x<-vpatterns fSpec]++"\n"
       )++
--       (if null (conceptDefs fSpec) then "" else
--        "\n -- *** ConceptDefs (total: "++(show.length.conceptDefs) fSpec++" conceptDefs) ***: "++
--        concat [indent++" "++showHSName cd++indent++"  = "++showHS opts (indent++"    ") cd | c<-concs fSpec, cd<-concDefs fSpec c]++"\n"
--       )++
       (if null (allConcepts fSpec) then "" else
        "\n -- *** Concepts (total: "++(show.length.allConcepts) fSpec++" concepts) ***: "++
        concat [indent++" "++showHSName x++indent++"  = "++showHS opts (indent++"    ") x
             ++ indent++"    "++showAtomsOfConcept x |x<-sortBy (comparing showHSName) (allConcepts fSpec)]++"\n"
       )
           where indentA = indent ++"                      "
                 indentB = indent ++"             "
                 showAtomsOfConcept c =
                              "-- atoms: [ "++ intercalate indentC strs++"]"
                     where
                       strs = map show (sort (atomsOf (gens fSpec)(initialPops fSpec) c))
                       indentC = if sum (map length strs) > 300
                                 then indent ++ "    --        , "
                                 else ", "
                 showViolatedRule :: String -> (Rule,Pairs) -> String
                 showViolatedRule indent' (r,ps)
                    = intercalate indent'
                        [        " ( "++showHSName r++" -- This is "++(if isSignal r then "a process rule." else "an invariant")++
                         indent'++" , "++ wrap "" (indent'++"   ") (let showPair _ p = show p --"( "++ (show.fst) p++", "++(show.snd) p++")"
                                                                      in showPair) ps++
                         indent'++" )"
                        ]

   instance ShowHS Meta where
    showHS f i (Meta pos obj nm val) = "Meta ("++showHS f i pos ++ ") "++ show obj ++ " " ++ show nm ++ " " ++ show val

   instance ShowHSName PlugInfo where
    showHSName (InternalPlug p) = haskellIdentifier ("ipl_"++name p)-- TODO
    showHSName (ExternalPlug _) = fatal 336 "a PlugInfo is anonymous with respect to showHS opts"

   instance ShowHS PlugInfo where
    showHS _ _ (InternalPlug p)
     = "InternalPlug "++showHSName p
    showHS opts ind (ExternalPlug o)
     = "ExternalPlug "++showHS opts (ind++"    ") o

   instance ShowHS RoleRelation where
    showHS opts ind rr
     = "RR "++show (rrRoles rr)++" "++showHS opts (ind++"    ") (rrRels rr)++" "++showHS opts (ind++"    ") (rrPos rr)

   instance ShowHS RoleRule where
    showHS opts ind rs
     = "Maintain "++show (mRoles rs)++" "++show (mRules rs)++" "++showHS opts (ind++"    ") (mPos rs)

   instance ShowHSName FSid where
    showHSName (FS_id nm ) = haskellIdentifier nm

   instance ShowHS FSid where
    showHS _ _ (FS_id nm)
      = "(FS_id " ++ show nm ++ ")"

   instance ShowHSName Pattern where
    showHSName pat = haskellIdentifier ("pat_"++name pat)

   instance ShowHS Pattern where
    showHS opts indent pat
     = intercalate indentA
        [ "A_Pat { ptnm  = "++show (name pat)
        , ", ptpos = "++showHS opts "" (ptpos pat)
        , ", ptend = "++showHS opts "" (ptend pat)
        , ", ptrls = [" ++intercalate ", " [showHSName r | r<-ptrls pat] ++ concat [" {- no rules -} "        | null (ptrls pat)] ++"]"
        , wrap ", ptgns = " indentB (showHS opts) (ptgns pat)
        , ", ptdcs = [ " ++intercalate (indentB++", ") [showHSName d | d<-ptdcs pat] ++ concat [" {- no relations -} " | null (ptdcs pat)] ++indentB++"]"
        , wrap ", ptups = " indentB (showHS opts) (ptups pat)
        , case ptrruls pat of
           []          -> ", ptrruls = [] {- no role-rule assignments -}"
           [(rol,rul)] -> ", ptrruls = [ ("++show rol++", "++showHSName rul++") ]"
           rs          -> ", ptrruls = [ "++intercalate (indentB++", ") ["("++show rol++", "++showHSName rul++")" | (rol,rul)<-rs] ++indentB++"]"
        , case ptrrels pat of
           []          -> ", ptrrels = [] {- no role-relation assignments -}"
           [(rol,rel)] -> ", ptrrels = [ ("++show rol++", "++showHS opts "" rel++") ]"
           rs          -> ", ptrrels = [ "++intercalate (indentB++", ") ["("++show rol++", "++showHS opts "" rel++")" | (rol,rel)<-rs] ++indentB++"]"
        , wrap ", ptids = " indentB (showHS opts) (ptids pat)
        , wrap ", ptxps = " indentB (showHS opts) (ptxps pat)
        , "}"
        ] where indentA = indent ++"      "     -- adding the width of "A_Pat "
                indentB = indentA++"          " -- adding the width of ", ptrls = "

   instance ShowHSName FProcess where
    showHSName prc = haskellIdentifier ("fprc_"++name (fpProc prc))

   instance ShowHS FProcess where
    showHS opts indent prc
     = intercalate indentA
        [ "FProc { fpProc       = "++showHSName (fpProc prc)
        , wrap  ", fpActivities = " indentB (showHS opts) (fpActivities prc)
        , "      }"
        ] where indentA = indent ++"      "     -- adding the width of "FProc "
                indentB = indentA++"                 " -- adding the width of ", fpActivities = "

   instance ShowHSName Process where
    showHSName prc = haskellIdentifier ("prc_"++name prc)

   instance ShowHS Process where
    showHS opts indent prc
     = intercalate indentA
        [ "Proc { prcNm = "++show (name prc)
        , ", prcPos = "++showHS opts "" (prcPos prc)
        , ", prcEnd = "++showHS opts "" (prcEnd prc)
        , ", prcRules = [" ++intercalate ", " [showHSName r | r<-prcRules prc] ++ concat [" {- no rules -} "                     | null (prcRules prc)] ++"]"
        , wrap ", prcGens = " indentB (showHS opts) (prcGens prc)
        , ", prcDcls = ["  ++intercalate ", " [showHSName d | d<-prcDcls  prc] ++ concat [" {- no relations -} "              | null (prcDcls  prc)] ++"]"
        , wrap ", prcUps = " indentB (showHS opts) (prcUps prc)
        , case prcRRuls prc of
           []          -> ", prcRRuls = [] {- no role-rule assignments -}"
           [(rol,rul)] -> ", prcRRuls = [ ("++show rol++", "++showHSName rul++") ]"
           rs          -> ", prcRRuls = [ "++intercalate (indentB++", ") ["("++show rol++", "++showHSName rul++")" | (rol,rul)<-rs] ++indentB++"]"
        , case prcRRels prc of
           []          -> ", prcRRels = [] {- no role-relation assignments -}"
           [(rol,rel)] -> ", prcRRels = [ ("++show rol++", "++showHS opts "" rel++") ]"
           rs          -> ", prcRRels = [ "++intercalate (indentB++", ") ["("++show rol++", "++showHS opts "" rel++")" | (rol,rel)<-rs] ++indentB++"]"
        , wrap ", prcIds = " indentB (showHS opts) (prcIds prc)
        , wrap ", prcVds = " indentB (showHS opts) (prcVds prc)
        , wrap ", prcXps = " indentB (showHS opts) (prcXps prc)
        , "}"
        ] where indentA = indent ++"      "     -- adding the width of "FProc "
                indentB = indentA++"             " -- adding the width of ", prcRules = "

   instance ShowHS Activity where
    showHS opts indent act =
       intercalate indentA
        [ "Act { actRule   = "++showHSName (actRule act)
        , wrap ", actTrig   = " indentB (\_->showHSName) (actTrig   act)
        , wrap ", actAffect = " indentB (\_->showHSName) (actAffect act)
        , wrap ", actQuads  = " indentB (\_->showHSName) (actQuads  act)
        , wrap ", actEcas   = " indentB (\_->showHSName) (actEcas   act)
        , wrap ", actPurp   = " indentB (\_->(showHS opts indentB)) (actPurp act)
        , "      }"
        ]
       where indentA = indent ++replicate (length "Act "          ) ' '
             indentB = indentA++replicate (length ", actAffect = ") ' '

   instance ShowHS PPurpose where
    showHS opts _ expl =
       "PRef2 ("++showHS opts "" (pexPos     expl)++") "++
             "("++showHS opts "" (pexObj     expl)++") "++
             "("++showHS opts "" (pexMarkup  expl)++") "
                ++show (intercalate ";" (pexRefIDs expl))++" "

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
    showHS opts _ expla =
       "Expl "++"("++showHS opts "" (explPos expla)++") "
              ++"("++showHS opts "" (explObj expla)++") "
                   ++showHS opts "" (explMarkup  expla)++" "
                   ++show (explUserdefd expla)++" "
                   ++show (explRefIds expla)++" "

   instance ShowHS ExplObj where
    showHS opts i peObj = case peObj of
             ExplConceptDef cd  -> "ExplConceptDef " ++showHS opts i cd
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
     showHS opts indent (PairView pvs) = "PairView "++showHS opts indent pvs

   instance ShowHS (PairViewSegment Expression) where
     showHS _     _ (PairViewText txt) = "PairViewText "++show txt
     showHS opts _ (PairViewExp srcOrTgt e) = "PairViewExp "++show srcOrTgt++" ("++showHS opts "" e++")"

   instance ShowHSName Rule where
    showHSName r = haskellIdentifier ("rule_"++ rrnm r)

   instance ShowHS Rule where
    showHS opts indent r@(Ru _ _ _ _ _ _ _ _ _ _ _ _)  -- This pattern matching occurs so Haskell will detect any change in the definition of Ru.
      = intercalate indent
        ["Ru{ rrnm   = " ++ show (rrnm   r)
        ,"  , rrexp  = -- " ++ showADL (rrexp  r) ++ indent++"             " ++ showHS opts (indent++"             ") (rrexp  r)
        ,"  , rrfps  = " ++ showHS opts "" (rrfps  r)
        ,"  , rrmean = " ++ showHS opts (indent++"             ") (rrmean r)
        ,"  , rrmsg  = " ++ showHS opts "" (rrmsg  r)
        ,"  , rrviol = " ++ showHS opts "" (rrviol r)
        ,"  , rrtyp  = " ++ showHS opts "" (rrtyp  r)
        ,"  , rrdcl  = " ++ case rrdcl r of
                              Just (p,d) -> "Just ("++showHSName p++", "++showHSName d++" )"
                              Nothing    -> "Nothing"
        ,"  , r_env  = " ++ show (r_env  r)
        ,"  , r_usr  = " ++ show (r_usr  r)
        ,"  , isSignal = " ++ show (isSignal  r)
        ,"  , srrel  = " ++ showHSName (srrel  r)
        ,"  }"
        ]

   instance ShowHS AMeaning where
     showHS opts indent (AMeaning x) = "AMeaning " ++ showHS opts (indent++"        ") x

   instance ShowHS RuleType where
     showHS _ _ Truth          = "Truth"
     showHS _ _ Equivalence    = "Equivalence"
     showHS _ _ Implication    = "Implication"

   instance ShowHSName IdentityDef where
    showHSName identity = haskellIdentifier ("identity_"++name identity)

   instance ShowHS IdentityDef where
    showHS opts indent identity
     = "Id ("++showHS opts "" (idPos identity)++") "++show (idLbl identity)++" ("++showHSName (idCpt identity)++")"
       ++indent++"  [ "++intercalate (indent++"  , ") (map (showHS opts indent) $ identityAts identity)++indent++"  ]"

   instance ShowHS IdentitySegment where
    showHS opts indent (IdentityExp objDef) = "IdentityExp ("++ showHS opts indent objDef ++ ")"

   instance ShowHSName ViewDef where
    showHSName vd = haskellIdentifier ("vdef_"++name vd)

   instance ShowHS ViewDef where
    showHS opts indent vd
     = "Vd ("++showHS opts "" (vdpos vd)++") "++show (vdlbl vd)++" "++showHSName (vdcpt vd)
       ++indent++"  [ "++intercalate (indent++"  , ") (map (showHS opts indent) $ vdats vd)++indent++"  ]"

   --instance ShowHSName ViewSegment where
   -- showHSName vd = haskellIdentifier ("vdef_"++name vd)

   instance ShowHS ViewSegment where
    showHS _     _      (ViewText str)   = "ViewText "++show str
    showHS _     _      (ViewHtml str)   = "ViewHtml "++show str
    showHS opts indent (ViewExp objDef) = "ViewExp "++ showHS opts (indent++"            ") objDef

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
    showHS opts indent r
     = intercalate indent
           ["Obj{ objnm   = " ++ show(objnm r)
           ,"   , objpos  = " ++ showHS opts "" (objpos r)
           ,"   , objctx  = " ++ showHS opts (indent++"               ") (objctx r)
           ,"   , objmsub = " ++ showHS opts (indent++"                    ") (objmsub r)
           ,"   , objstrs = " ++ show(objstrs r)
           ]++indent++"   }"

   instance ShowHSName Interface where
    showHSName obj = haskellIdentifier ("ifc_"++name obj)

   instance ShowHS Interface where
    showHS opts indent ifc
     = intercalate indent
           [ wrap "Ifc { ifcParams = " (indent++"                  ") (showHS opts) (ifcParams ifc)
           , "    , ifcArgs   = " ++ show(ifcArgs ifc)
           , "    , ifcRoles  = " ++ show(ifcRoles ifc)
           , "    , ifcObj"++indent++"       = " ++ showHS opts (indent++"         ") (ifcObj ifc)
           , "    , ifcPos    = " ++ showHS opts "" (ifcPos ifc)
           , "    , ifcPrp    = " ++ show(ifcPrp ifc)
           ]++indent++"    }"

   instance ShowHS SubInterface where
    showHS _     _      (InterfaceRef n) = "InterfaceRef "++show n
    showHS opts indent (Box x objs) = "Box ("++showHS opts indent x++")"++indent++"     ("++showHS opts (indent++"     ") objs++")"

   instance ShowHS Expression where
    showHS opts indent (EEqu (l,r)) = "EEqu ("++showHS opts (indent++"      ") l++indent++"     ,"++showHS opts (indent++"      ") r++indent++"     )"
    showHS opts indent (EImp (l,r)) = "EImp ("++showHS opts (indent++"      ") l++indent++"     ,"++showHS opts (indent++"      ") r++indent++"     )"
    showHS opts indent (EIsc (l,r)) = "EIsc ("++showHS opts (indent++"      ") l++indent++"     ,"++showHS opts (indent++"      ") r++indent++"     )"
    showHS opts indent (EUni (l,r)) = "EUni ("++showHS opts (indent++"      ") l++indent++"     ,"++showHS opts (indent++"      ") r++indent++"     )"
    showHS opts indent (EDif (l,r)) = "EDif ("++showHS opts (indent++"      ") l++indent++"     ,"++showHS opts (indent++"      ") r++indent++"     )"
    showHS opts indent (ELrs (l,r)) = "ELrs ("++showHS opts (indent++"      ") l++indent++"     ,"++showHS opts (indent++"      ") r++indent++"     )"
    showHS opts indent (ERrs (l,r)) = "ERrs ("++showHS opts (indent++"      ") l++indent++"     ,"++showHS opts (indent++"      ") r++indent++"     )"
    showHS opts indent (EDia (l,r)) = "EDia ("++showHS opts (indent++"      ") l++indent++"     ,"++showHS opts (indent++"      ") r++indent++"     )"
    showHS opts indent (ECps (l,r)) = "ECps ("++showHS opts (indent++"      ") l++indent++"     ,"++showHS opts (indent++"      ") r++indent++"     )"
    showHS opts indent (ERad (l,r)) = "ERad ("++showHS opts (indent++"      ") l++indent++"     ,"++showHS opts (indent++"      ") r++indent++"     )"
    showHS opts indent (EPrd (l,r)) = "EPrd ("++showHS opts (indent++"      ") l++indent++"     ,"++showHS opts (indent++"      ") r++indent++"     )"
    showHS opts indent (EKl0 e    ) = "EKl0 ("++showHS opts (indent++"      ") e++")"
    showHS opts indent (EKl1 e    ) = "EKl1 ("++showHS opts (indent++"      ") e++")"
    showHS opts indent (EFlp e    ) = "EFlp ("++showHS opts (indent++"      ") e++")"
    showHS opts indent (ECpl e    ) = "ECpl ("++showHS opts (indent++"      ") e++")"
    showHS opts indent (EBrk e    ) = "EBrk ("++showHS opts (indent++"      ") e++")"
    showHS _     _      (EDcD dcl  ) = "EDcD "++showHSName dcl
    showHS _     _      (EDcI c    ) = "EDcI "++showHSName c
    showHS opts _      (EEps i sgn) = "EEps ("++showHS opts "" i++") ("++showHS opts "" sgn++")"
    showHS opts _      (EDcV sgn  ) = "EDcV ("++showHS opts "" sgn++")"
    showHS _     _      (EMp1 a c  ) = "EMp1 " ++show a++" "++showHSName c

   instance ShowHS Sign where
    showHS _ _ sgn = "Sign "++showHSName (source sgn)++" "++showHSName (target sgn)

   instance ShowHS A_Gen where
    showHS _ _ gen =
      case gen of
        Isa{} -> "Isa "++showHSName (genspc gen)++" "++showHSName (gengen gen)++" "
        IsE{} -> "IsE "++showHSName (genspc gen)++" ["++intercalate ", " (map showHSName (genrhs gen))++"] "

   instance ShowHSName Declaration where
    showHSName d@Isn{}       = haskellIdentifier ("rel_"++name d++"_"++name (source d)) -- identity relation
    showHSName d@Vs{}        = haskellIdentifier ("rel_"++name d++"_"++name (source d)++name (target d)) -- full relation
    showHSName d | decusr d  = haskellIdentifier ("rel_"++name d++name (source d)++name (target d)) -- user defined relations
                 | deciss d  = haskellIdentifier ("sgn_"++name d++name (source d)++name (target d)) -- relations generated for signalling
                 | otherwise = haskellIdentifier ("vio_"++name d++name (source d)++name (target d)) -- relations generated per rule

   instance ShowHS Declaration where
    showHS opts indent d
       = case d of
          Sgn{}     -> intercalate indent
                        ["Sgn{ decnm   = " ++ show (decnm d)
                        ,"   , decsgn  = " ++ showHS opts "" (sign d)
                        ,"   , decprps = " ++ showL(map (showHS opts "") (decprps d))
                        ,"   , decprps_calc = " ++ case decprps_calc d of
                                                    Nothing -> "Nothing"
                                                    Just ps -> "Just "++showL(map (showHS opts "") ps)
                        ,"   , decprL  = " ++ show (decprL d)
                        ,"   , decprM  = " ++ show (decprM d)
                        ,"   , decprR  = " ++ show (decprR d)
                        ,"   , decMean = " ++ show (decMean d)
                        ,"   , decfpos = " ++ showHS opts "" (decfpos d)
                        ,"   , deciss  = " ++ show (deciss d)
                        ,"   , decusr  = " ++ show (decusr d)
                        ,"   , decpat  = " ++ show (decpat d)
                        ,"   , decplug = " ++ show (decplug d)
                        ]++"}"
          Isn{}     -> "Isn{ detyp   = " ++ showHSName (detyp d)++"}"
          Vs{}      -> "Vs { decsgn  = " ++ showHS opts "" (sign d)++"}"

--   instance ShowHSName ConceptDef where
--    showHSName cd = haskellIdentifier ("cDef_"++cdcpt cd)

   instance ShowHS ConceptDef where
    showHS opts _ cd
     = " Cd ("++showHS opts "" (cdpos cd)++") "++show (cdcpt cd)++" "++show (cdplug cd)++" "++show (cddef cd)++" "++show (cdtyp cd)++" "++show (cdref cd)++" "++show (cdfrom cd)
   instance ShowHSName Char where
    showHSName c = show c
   instance ShowHS Char where
    showHS _ _ c = show c
   instance ShowHSName A_Concept where
    showHSName ONE = haskellIdentifier "cptOne"
    showHSName c = haskellIdentifier ("cpt_"++name c)
   instance ShowHS A_Concept where
    showHS _ _ c = case c of
                       PlainConcept{} -> "PlainConcept "++show (name c)
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
    showHS _ _ (FilePos (fn,Database.Design.Ampersand.Input.ADL1.UU_Scanner.Pos l c,sym))
      = "FilePos ("++show fn++",Pos "++show l++" "++show c++","++show sym++")"

   instance ShowHSName Origin where
    showHSName ori = "Orig"++show x++show (hash x)
      where x = case ori of
                 FileLoc l -> "FileLoc (" ++ show l++")"
                 DBLoc l   -> "DBLoc " ++ show l
                 Origin s  -> "Origin " ++ show s
                 OriginUnknown -> "OriginUnknown"

   instance ShowHS Origin where
    showHS opts indent (FileLoc l) = "FileLoc (" ++ showHS opts indent l++")"
    showHS _     _      (DBLoc l)   = "DBLoc "  ++ show l
    showHS _     _      (Origin s)  = "Origin " ++ show s
    showHS _     _    OriginUnknown = "OriginUnknown"

   instance ShowHS Block where
    showHS _ _   = show

   instance ShowHS Inline where
    showHS _ _   = show

--   instance ShowHS InfTree where
--    showHS opts indent itree =
--        case itree of
--          InfExprs irt (ratype,raobj) itrees ->
--              "InfExprs " ++ showHS opts indent irt ++
--              indent ++ "   (" ++ showRaType ratype ++ "," ++ "RelAlgObj{-"++show raobj++"-}" ++ ")" ++
--              indent ++ showHS opts (indent ++ "     ") itrees
--          InfRel drt ratype _ _ ->
--              "InfRel " ++ showHS opts indent drt ++ " " ++ showRaType ratype
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

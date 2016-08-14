{-# LANGUAGE FlexibleInstances #-}
module Ampersand.FSpec.ShowHS (ShowHS(..),ShowHSName(..),fSpec2Haskell,haskellIdentifier) where
import Ampersand.Core.ParseTree
import Ampersand.Core.AbstractSyntaxTree
import Text.Pandoc hiding (Meta)
import Data.Char                  (isAlphaNum)
import Ampersand.Basics hiding (indent)
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.ShowADL    (ShowADL(..))  -- for traceability, we generate comments in the Haskell code.
--import Ampersand.FSpec.FPA   (fpa)
import Data.List
import Ampersand.Misc
import Data.Hashable
import Data.Ord
import Data.Function

fSpec2Haskell :: FSpec -> String
fSpec2Haskell fSpec
        = "{-# OPTIONS_GHC -Wall #-}"
          ++"\n{-Generated code by "++ampersandVersionStr++" at "++show (genTime (getOpts fSpec))++"-}"
          ++"\nmodule Main where\n"
          ++"\nimport Ampersand"
          ++"\nimport Text.Pandoc hiding (Meta)"
          ++"\nimport Prelude hiding (writeFile,readFile,getContents,putStr,putStrLn)"
          ++"\n"
          ++"\nmain :: IO ()"
          ++"\nmain = do (getOpts fSpec) <- getOptions"
          ++"\n          putStr (showHS (getOpts fSpec) \"\\n  \" fSpec_"++baseName (getOpts fSpec)++")\n"
          ++"\nfSpec_"++baseName (getOpts fSpec)++" :: FSpec"
          ++"\nfSpec_"++baseName (getOpts fSpec)++" =\n  "++showHS (getOpts fSpec) "\n  " fSpec

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
                                          [showHSName f++indent++"     = "++showHS opts (indent++"       ") f | f<-plugAttributes plug] ++indent++"in"
                   ,"TblSQL { sqlname    = " ++ (show.name) plug
                   ,"       , attributes = ["++intercalate ", " (map showHSName (attributes plug))++"]"
                   ,"       , cLkpTbl    = [ "++intercalate (indent++"                      , ") ["("++showHSName c++", "++showHSName cn++")" | (c,cn)<-cLkpTbl plug] ++ "]"
                   ,"       , dLkpTbl    = [ "++intercalate (indent++"                      , ") 
                                                      [ "RelStore "++showHSName (rsDcl store)++" "
                                                                   ++showHSName (rsSrcAtt store)++" "
                                                                   ++showHSName (rsTrgAtt store)++" "
                                                      | store<-dLkpTbl plug] ++ "]"
                   ,"       }"
                   ]
       BinSQL{} -> intercalate indent
                   ["let " ++ intercalate (indent++"    ")
                                          [showHSName f++indent++"     = "++showHS opts (indent++"       ") f | f<-plugAttributes plug] ++indent++"in"
                   ,"BinSQL { sqlname = " ++ (show.name) plug
                   ,"       , cLkpTbl = [ "++intercalate (indent++"                   , ") ["("++showHSName c++", "++showHSName cn++")" | (c,cn)<-cLkpTbl plug] ++ "]"
                   ,"       , dLkpTbl    = [ "++intercalate (indent++"                      , ") 
                                                      [ "RelStore "++showHSName (rsDcl store)++" "
                                                                   ++showHSName (rsSrcAtt store)++" "
                                                                   ++showHSName (rsTrgAtt store)++" "
                                                      | store<-dLkpTbl plug] ++ "]"
               --    ,"       , sqlfpa  = " ++ showHS opts "" (fpa plug)
                   ,"       }"
                   ]

instance ShowHSName ECArule where
 showHSName r = "ecaRule"++show (ecaNum r)

instance ShowHS ECArule where
 showHS opts indent r
   =         "ECA { ecaTriggr = " ++ showHS opts "" (ecaTriggr r) ++
     indent++"    , ecaDelta  = " ++ showHSName (ecaDelta r)++
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
        Do{}  ->  "Do "++show (paSrt p)++" "++showHSName (paTo p)++
                         indent++"       ("++showHS opts (indent++"        ") (paDelta p)++indent++"       )"++
                 wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
        New{} -> "New ("++showHS opts "" (paCpt p)++")"++
                 indent++"    (\\x->"++showHS opts (indent++"        ") (paCl p (makePSingleton "x"))++indent++"    )"++
                 wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
        Rmv{} -> "Rmv ("++showHS opts "" (paCpt p)++")"++
                 indent++"    (\\x->"++showHS opts (indent++"        ") (paCl p (makePSingleton "x"))++indent++"    )"++
                 wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
        Nop{} -> "Nop "++wrap "" (indent ++"    ") showMotiv ms
        Blk{} -> "Blk "++wrap "" (indent ++"    ") showMotiv ms
        Let{} -> wrap "Let " (indent ++"    ") (showHS opts) (paCls p)++
                 "TODO: paBody of Let clause"++
                 wrap (if null ms then "" else indent ++"    ") (indent ++"    ") showMotiv ms
        Ref{} -> "Ref "++paVar p
     where ms = paMotiv p
           showMotiv ind (conj,rs) = "( "++showHS opts (ind++"  ") conj++" -- conjunct:  "++showADL conj++ind++", "++showHSName rs++ind++")"

instance ShowHSName SqlAttribute where
 showHSName sqAtt = haskellIdentifier ("sqlAtt_"++attName sqAtt)

instance ShowHS SqlAttribute where
 showHS opts indent sqAtt
   = intercalate indentA
       [  "Att { attName    = " ++ show (attName sqAtt)
       ,      ", attExpr    = " ++ showHS opts indentB (attExpr sqAtt)
       ,      ", attType    = " ++ showHS opts "" (attType sqAtt)
       ,      ", attUse     = " ++ showHS opts "" (attUse sqAtt)
       ,      ", attNull    = " ++ show (attNull sqAtt)
       ,      ", attDBNull    = " ++ show (attDBNull sqAtt)
       ,      ", attUniq    = " ++ show (attUniq sqAtt)
       ,      ", attFlipped = " ++ show (attFlipped sqAtt)
       ,      "}"
       ] where indentA = indent ++"    "         -- adding the width of "Att "
               indentB = indentA++"            " -- adding the width of ", attExpr = "

instance ShowHS SqlAttributeUsage where
 showHS _ _ (PrimaryKey aCpt) = "PrimaryKey "++showHSName aCpt
 showHS _ _ (ForeignKey aCpt) = "ForeignKey "++showHSName aCpt
 showHS _ _ PlainAttr         = "PlainAttr "

instance ShowHS TType where
 showHS _ indent tt = indent ++ show tt

instance ShowHSName Quad where
 showHSName q
   = haskellIdentifier ("quad_"++(showHSName.qDcl) q++"_"++(name.qRule) q)

instance ShowHS Quad where
 showHS _ indent q
   = intercalate indent
            [ "Quad{ qDcl     = " ++ showHSName (qDcl q)
            , "    , qRule    = " ++ showHSName (qRule q)
            , wrap "    , qConjuncts = " newindent (const showHSName) (qConjuncts q)
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
         "       , fsbConjs = " newindent' (const shConj) (fsbConjs  fsb)
       ,wrap
         "       , fsbECAs  = " newindent' (const showHSName) (fsbECAs  fsb)
       , "       }"
       ]
    where
      newindent   = indent ++ "                   "
      newindent'  = newindent ++ " "
      newindent'' = newindent' ++ "    "
      shConj (r,conj) = "( "++showHSName r++newindent++"   , "++showHS opts newindent'' conj++newindent++"   )"

instance ShowHS DnfClause where
 showHS opts indent dnf
   = intercalate indent
       [ wrap "Dnf " (indent++"    ") (\_->showHS opts (indent++"      ")) (antcs dnf)
       , wrap "    " (indent++"    ") (\_->showHS opts (indent++"      ")) (conss dnf)
       ]

instance ShowHSName Conjunct where
 showHSName x = haskellIdentifier (rc_id x)

instance ShowHS Conjunct where
 showHS opts indent x
   = intercalate (indent ++"    ")
       [   "Cjct{ rc_id         = " ++ show (rc_id x)
       ,       ", rc_orgRules   = " ++ "[ "++intercalate ", " (map showHSName (rc_orgRules x))++"]"
       ,       ", rc_conjunct   = " ++ showHS opts indentA (rc_conjunct x)
       , wrap  ", rc_dnfClauses = " indentA (\_->showHS opts (indentA++"  ")) (rc_dnfClauses x)
       ,       "}"
       ]
     where indentA = indent ++"                    "

instance ShowHSName FSpec where
 showHSName fSpec = haskellIdentifier ("fSpc_"++name fSpec)

instance ShowHS FSpec where
 showHS opts indent fSpec
  = intercalate (indent ++"     ")
        [ "FSpec{ fsName        = " ++ show (name fSpec)
        , wrap ", fspos         = " indentA (showHS opts) (fspos fSpec)
        ,      ", fsLang        = " ++ show (fsLang fSpec) ++ "  -- the default language for this specification"
        ,      ", themes        = " ++ show (themes fSpec) ++ "  -- the names of themes to be printed in the documentation, meant for partial documentation.  Print all if empty..."
        , wrap ", pattsInScope  = " indentA (const showHSName) (pattsInScope fSpec)
        , wrap ", rulesInScope  = " indentA (const showHSName) (rulesInScope fSpec)
        , wrap ", declsInScope  = " indentA (const showHSName) (declsInScope fSpec)
        , wrap ", cDefsInScope  = " indentA (\_->showHS opts (indentA++"  ")) (cDefsInScope fSpec)
        , wrap ", gensInScope   = " indentA (showHS opts)   (gensInScope fSpec)
        , wrap ", vplugInfos    = " indentA (\_->showHS opts (indentA++"  ")) (vplugInfos fSpec)
        , wrap ", plugInfos     = " indentA (\_->showHS opts (indentA++"  ")) (plugInfos  fSpec)
        ,      ", interfaceS    = interfaceS'"
        ,      ", interfaceG    = interfaceG'"
        , wrap ", fActivities   = " indentA (\_->showHS opts (indentA++"  ")) (fActivities fSpec)
        ,      ", fRoleRels     = " ++
               case fRoleRels fSpec of
                 []        -> "[]"
                 [(r,rel)] -> "[ ("++show r++", "++showHS opts "" rel++") ]"
                 _         -> "[ "++intercalate (indentA++", ") ["("++show r++","++showHS opts "" rel++")" | (r,rel)<-fRoleRels fSpec]++indentA++"]"
        ,      ", fRoleRuls     = " ++showHS opts indentA (fRoleRuls fSpec)
        , wrap ", fRoles        = " indentA (showHS opts)    [rol | (rol,_) <- fRoles fSpec]
        , wrap ", vrules        = " indentA (const showHSName) (vrules fSpec)
        , wrap ", grules        = " indentA (const showHSName) (grules fSpec)
        , wrap ", invariants    = " indentA (const showHSName) (invariants fSpec)
        , wrap ", fallRules     = " indentA (const showHSName) (fallRules fSpec)
        , wrap ", allUsedDecls  = " indentA (const showHSName) (allUsedDecls fSpec)
        , wrap ", vrels         = " indentA (const showHSName) (vrels fSpec)
        , wrap ", allConcepts   = " indentA (const showHSName) (allConcepts fSpec)
        , wrap ", vIndices      = " indentA (const showHSName) (vIndices fSpec)
        , wrap ", vviews        = " indentA (const showHSName) (vviews fSpec)
        , wrap ", vgens         = " indentA (showHS opts)    (vgens fSpec)
        , wrap ", fsisa         = " indentA (const showHSName) (fsisa fSpec)
        , wrap ", allConjuncts        = " indentA (const showHSName) (allConjuncts fSpec)
        , wrap ", vquads        = " indentA (const showHSName) (vquads fSpec)
        , wrap ", vEcas         = " indentA (const showHSName) (vEcas fSpec)
        ,      ", fSwitchboard  = "++showHS opts indentA (fSwitchboard fSpec)
        , wrap ", vpatterns     = " indentA (const showHSName) (vpatterns fSpec)
        , wrap ", conceptDefs   = " indentA (showHS opts)    (conceptDefs fSpec)
        , wrap ", fSexpls       = " indentA (showHS opts)    (fSexpls fSpec)
        ,      ", metas         = allMetas"
        , wrap ", allViolations = " indentA showViolatedRule (allViolations fSpec)
        , wrap ", allExprs      = " indentA (showHS opts)    (allExprs fSpec)
        , "}"
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
    (let ds = vrels fSpec `uni` allUsedDecls fSpec `uni` (nub . map qDcl . vquads) fSpec in
     if null ds then "" else
     "\n -- *** Declared relations (in total: "++(show.length) ds++" relations) ***: "++
     concat [indent++" "++showHSName x++indent++"  = "++showHS opts (indent++"    ") x |x<-ds]++"\n"
    ) ++
    (if null (vIndices fSpec)     then "" else
     "\n -- *** Indices (total: "++(show.length.vIndices) fSpec++" indices) ***: "++
     concat [indent++" "++showHSName x++indent++"  = "++showHS opts (indent++"    ") x |x<-vIndices fSpec]++"\n"
    ) ++
    (if null (vviews fSpec)     then "" else
     "\n -- *** Views (total: "++(show.length.vviews) fSpec++" views) ***: "++
     concat [indent++" "++showHSName x++indent++"  = "++showHS opts (indent++"    ") x |x<-vviews fSpec]++"\n"
    ) ++
    (if null (vrules   fSpec ) then "" else
     "\n -- *** User defined rules (total: "++(show.length.vrules) fSpec++" rules) ***: "++
     concat [indent++" "++showHSName x++indent++"  = "++showHS opts (indent++"    ") x |x<-vrules     fSpec ]++"\n"
    )++
    (if null (grules   fSpec ) then "" else
     "\n -- *** Generated rules (total: "++(show.length.grules) fSpec++" rules) ***: "++
     concat [indent++" "++showHSName x++indent++"  = "++showHS opts (indent++"    ") x |x<-grules     fSpec ]++"\n"
    )++
    (if null (allConjuncts fSpec ) then "" else
     "\n -- *** Conjuncts (total: "++(show.length.allConjuncts) fSpec++" conjuncts) ***: "++
     concat [indent++" "++showHSName x++indent++"  = "++showHS opts (indent++"    ") x |x<-allConjuncts     fSpec ]++"\n"
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
        where indentA = indent ++"                       "
              indentB = indent ++"             "
              showAtomsOfConcept c =
                           "-- atoms: [ "++ intercalate indentC strs++"]"
                  where
                    strs = map showVal (sort (atomsInCptIncludingSmaller fSpec c))
                      where showVal val= "`"++showValADL val++"`" 
                    indentC = if sum (map length strs) > 300
                              then indent ++ "    --        , "
                              else ", "
              showViolatedRule :: String -> (Rule,[AAtomPair]) -> String
              showViolatedRule indent' (r,ps)
                 = intercalate indent'
                     [        " ( "++showHSName r++" -- This is "++(if isSignal r then "a process rule." else "an invariant")++
                      indent'++" , "++ wrap "" (indent'++"   ") (let showPair _ p = "( "++ (show.showValADL.apLeft) p++", "++(show.showValADL.apRight) p++")"
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

instance ShowHS A_RoleRelation where
 showHS opts ind rr
  = "RR "++show (rrRoles rr)++" "++showHS opts (ind++"    ") (rrRels rr)++" "++showHS opts (ind++"    ") (rrPos rr)
instance ShowHS Role where
 showHS _ ind r = ind++
                  (case r of
                     Role str -> "Role "++show str
                     Service str -> "Service "++show str
                  ) 
 
instance ShowHS P_RoleRule where
 showHS opts ind rs
  = "Maintain "++show (mRoles rs)++" "++show (mRules rs)++" "++showHS opts (ind++"    ") (mPos rs)
instance ShowHS (Role,Rule) where
 showHS _ _ (rol,rul)
  = "("++show rol++", "++showHSName rul++")"
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
     , wrap ", ptids = " indentB (showHS opts) (ptids pat)
     , wrap ", ptvds = " indentB (showHS opts) (ptvds pat)
     , wrap ", ptxps = " indentB (showHS opts) (ptxps pat)
     , "}"
     ] where indentA = indent ++"      "     -- adding the width of "A_Pat "
             indentB = indentA++"          " -- adding the width of ", ptrls = "

instance ShowHS Activity where
 showHS opts indent act =
    intercalate indentA
     [ "Act { actRule   = "++showHSName (actRule act)
     , wrap ", actTrig   = " indentB (const showHSName) (actTrig   act)
     , wrap ", actAffect = " indentB (const showHSName) (actAffect act)
     , wrap ", actQuads  = " indentB (const showHSName) (actQuads  act)
     , wrap ", actEcas   = " indentB (const showHSName) (actEcas   act)
     , wrap ", actPurp   = " indentB (const $ showHS opts indentB) (actPurp act)
     , "      }"
     ]
    where indentA = indent ++replicate (length ("Act "::String)) ' '
          indentB = indentA++replicate (length (", actAffect = " :: String)) ' '

instance ShowHS PPurpose where
 showHS opts _ expl =
    "PRef2 ("++showHS opts "" (pexPos     expl)++") "++
          "("++showHS opts "" (pexObj     expl)++") "++
          "("++showHS opts "" (pexMarkup  expl)++") "
             ++show (intercalate ";" (pexRefIDs expl))++" "

instance ShowHS PRef2Obj where
 showHS _ _ peObj
  = case peObj of
         PRef2ConceptDef str                    -> "PRef2ConceptDef " ++show str
         PRef2Declaration (PNamedRel _ nm mSgn) -> "PRef2Declaration "++show nm++maybe "" show mSgn
         PRef2Rule str                          -> "PRef2Rule "       ++show str
         PRef2IdentityDef str                   -> "PRef2IdentityDef "++show str
         PRef2ViewDef str                       -> "PRef2ViewDef "    ++show str
         PRef2Pattern str                       -> "PRef2Pattern "    ++show str
         PRef2Interface str                     -> "PRef2Interface "  ++show str
         PRef2Context str                       -> "PRef2Context "    ++show str

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
     ,"        , amPandoc = "++ show (amPandoc m)
     ,"        }"
     ]

instance ShowHS (PairView Expression) where
  showHS opts indent (PairView pvs) = "PairView "++showHS opts indent pvs

instance ShowHS (PairViewSegment Expression) where
  showHS _     _ (PairViewText _ txt) = "PairViewText "++show txt
  showHS opts _ (PairViewExp _ srcOrTgt e) = "PairViewExp "++show srcOrTgt++" ("++showHS opts "" e++")"

instance ShowHSName Rule where
 showHSName r = haskellIdentifier ("rule_"++ rrnm r)

instance ShowHS Rule where
 showHS opts indent r@(Ru _ _ _ _ _ _ _ _ _ _ _)  -- This pattern matching occurs so Haskell will detect any change in the definition of Ru.
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
     ,"  }"
     ]

instance ShowHS AMeaning where
  showHS opts indent (AMeaning x) = "AMeaning " ++ showHS opts (indent++"        ") x

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
  = "Vd ("++showHS opts "" (vdpos vd)++") "++show (name vd)++" "++showHSName (vdcpt vd)
    ++indent++"  [ "++intercalate (indent++"  , ") (map (showHS opts indent) $ vdats vd)++indent++"  ]"

instance ShowHS ViewSegment where
  showHS opts indent vs =
    "ViewSegment "++showHS opts indent (origin vs) ++ " "
                  ++showHS opts indent (vsmlabel vs)
                  ++" "++show (vsmSeqNr vs)++" "
                  ++showHS opts indent (vsmLoad vs)
-- showHSName vd = haskellIdentifier ("vdef_"++name vd)

instance ShowHS ViewSegmentPayLoad where
 showHS _     _     (ViewText str)  = "ViewText "++show str
 showHS opts indent (ViewExp  expr) = showHS opts (indent++"            ") expr

instance ShowHS Population where
 showHS _ indent pop
  = case pop of
      ARelPopu{} -> "ARelPopu { popdcl = "++showHSName (popdcl pop)
--TODOFIX
--          ++indent++"         , popps  = [ "++intercalate
--           (indent++"                    , ") (map show (popps pop))
          ++indent++"                    ]"
          ++indent++"         }"
      ACptPopu{} -> "ACptPopu { popcpt = "++showHSName (popcpt pop)
--TODOFIX
--          ++indent++"         , popas  = [ "++intercalate
--           (indent++"                    , ") (map show (popas pop))
          ++indent++"                    ]"
          ++indent++"         }"

instance ShowHSName ObjectDef where
 showHSName obj = haskellIdentifier ("oDef_"++name obj)

instance ShowHS ObjectDef where
 showHS opts indent r
  = intercalate indent
        ["Obj{ objnm    = " ++ show(objnm r)
        ,"   , objpos   = " ++ showHS opts "" (objpos r)
        ,"   , objctx   = " ++ showHS opts (indent++"                ") (objctx r)
        ,"   , objcrud  = " ++ showHS opts (indent++"                ") (objcrud r)
        ,"   , objmView = " ++ show(objmView r)
        ,"   , objmsub  = " ++ showHS opts (indent++"                ") (objmsub r)
        ]++indent++"   }"

instance ShowHS Cruds where
 showHS opts indent x 
  = intercalate indent
      ["Cruds { crudOrig = "++ showHS opts "" (crudOrig x)
      ,"      , crudC    = "++ show (crudC x)
      ,"      , crudR    = "++ show (crudR x)
      ,"      , crudU    = "++ show (crudU x)
      ,"      , crudD    = "++ show (crudD x)
      ,"      }"
      ]  
instance ShowHSName Interface where
 showHSName obj = haskellIdentifier ("ifc_"++name obj)

instance ShowHS Interface where
 showHS opts indent ifc
  = intercalate indent
        [ "Ifc { ifcRoles  = " ++ show(ifcRoles ifc)
        , "    , ifcObj"++indent++"       = " ++ showHS opts (indent++"         ") (ifcObj ifc)
        , "    , ifcEcas   = " ++ showHS opts (indent++"                 ") (ifcEcas ifc)
        , wrap "    , ifcControls = " (indent++"                  ") (const showHSName) (ifcControls ifc)
        , "    , ifcPos    = " ++ showHS opts "" (ifcPos ifc)
        , "    , ifcPrp    = " ++ show(ifcPrp ifc)
        ]++indent++"    }"

instance ShowHS SubInterface where
 showHS _     _     (InterfaceRef isLink n cs) = "InterfaceRef "++show isLink ++" "++show n++" "++show cs
 showHS opts indent (Box x cl objs) = "Box ("++showHS opts indent x++") ("++showHS opts indent cl++")"++indent++"     ("++showHS opts (indent++"     ") objs++")"

instance ShowHS Expression where
 showHS opts indent (EEqu (l,r)) = "EEqu ("++showHS opts (indent++"      ") l++indent++"     ,"++showHS opts (indent++"      ") r++indent++"     )"
 showHS opts indent (EInc (l,r)) = "EInc ("++showHS opts (indent++"      ") l++indent++"     ,"++showHS opts (indent++"      ") r++indent++"     )"
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
 showHS _    _      (EDcD dcl  ) = "EDcD "++showHSName dcl
 showHS _    _      (EDcI c    ) = "EDcI "++showHSName c
 showHS opts _      (EEps i sgn) = "EEps ("++showHS opts "" i++") ("++showHS opts "" sgn++")"
 showHS opts _      (EDcV sgn  ) = "EDcV ("++showHS opts "" sgn++")"
 showHS _    _      (EMp1 a c  ) = "EMp1 " ++show a++" "++showHSName c

instance ShowHS Signature where
 showHS _ _ sgn = "Sign "++showHSName (source sgn)++" "++showHSName (target sgn)

instance ShowHS A_Gen where
 showHS _ _ gen =
   case gen of
     Isa{} -> "Isa "++showHSName (genspc gen)++" "++showHSName (gengen gen)++" "
     IsE{} -> "IsE "++showHSName (genspc gen)++" ["++intercalate ", " (map showHSName (genrhs gen))++"] "

instance ShowHSName Declaration where
 showHSName d@Isn{}       = haskellIdentifier ("rel_"++name d++"_"++name (source d)) -- identity relation
 showHSName d@Vs{}        = haskellIdentifier ("rel_"++name d++"_"++name (source d)++"_"++name (target d)) -- full relation
 showHSName d | decusr d  = haskellIdentifier ("rel_"++name d++"_"++name (source d)++"_"++name (target d)) -- user defined relations
              | otherwise = haskellIdentifier ("vio_"++name d++"_"++name (source d)++"_"++name (target d)) -- relations generated per rule

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
  = " Cd ("++showHS opts "" (cdpos cd)++") "++show (cdcpt cd)++" "++show (cdplug cd)++" "++show (cddef cd)++" "++show (cdref cd)++" "++show (cdfrom cd)
instance ShowHSName Char where
 showHSName = show
instance ShowHS Char where
 showHS _ _ = show
instance ShowHSName A_Concept where
 showHSName ONE = haskellIdentifier "cptOne"
 showHSName c = haskellIdentifier ("cpt_"++name c)
instance ShowHS A_Concept where
 showHS _ _ c = case c of
                    PlainConcept{} -> "PlainConcept "++show (name c)
                    ONE -> "ONE"

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
 showHSName Prop = "Prop"

instance ShowHS Prop where
 showHS _ _ = showHSName

instance ShowHS FilePos where
 showHS _ _ = show

instance ShowHSName Origin where
 showHSName ori = "Orig"++show x++show (hash x)
   where x :: String
         x = case ori of
              FileLoc l sym -> "FileLoc (" ++ show l ++ " " ++ sym ++ ")"
              DBLoc l       -> "DBLoc " ++ show l
              Origin s      -> "Origin " ++ show s
              OriginUnknown -> "OriginUnknown"
              XLSXLoc fPath sheet (a,b) -> "XLSXLoc "++fPath++" "++sheet++" "++show(a,b)
instance ShowHS Origin where
 showHS opts indent (FileLoc l s) = "FileLoc (" ++ showHS opts indent l ++ " " ++ s ++ ")"
 showHS _     _      (DBLoc l)    = "DBLoc "  ++ show l
 showHS _     _      (Origin s)   = "Origin " ++ show s
 showHS _     _    OriginUnknown  = "OriginUnknown"
 showHS _     _    (XLSXLoc fPath sheet (a,b)) = "XLSXLoc "++fPath++" "++sheet++" "++show(a,b)  

instance ShowHS Block where
 showHS _ _   = show

instance ShowHS Inline where
 showHS _ _   = show
                  
{-
instance ShowHS InfTree where
 showHS opts indent itree =
     case itree of
       InfExprs irt (ratype,raobj) itrees ->
           "InfExprs " ++ showHS opts indent irt ++
           indent ++ "   (" ++ showRaType ratype ++ "," ++ "RelAlgObj{-"++show raobj++"-}" ++ ")" ++
           indent ++ showHS opts (indent ++ "     ") itrees
       InfRel drt ratype _ _ ->
           "InfRel " ++ showHS opts indent drt ++ " " ++ showRaType ratype
   where
    showRaType rat = "RelAlgType{-"++show rat++"-}"

instance ShowHS RelDecl where
 showHS _ indent d = case d of
                       RelDecl{}-> "RelDecl{ dname  = " ++ show (dname d) ++ indent
                                ++ "        ,dtype  = " ++ showRaType dtype ++ indent
                                ++ "        ,isendo = " ++ show (isendo d)
                       IDecl    -> "IDecl"
                       VDecl    -> "VDecl"
   where
    showRaType _ = "RelAlgType{- ++TODO++ -}"


instance ShowHS DeclRuleType where
 showHS _ _ drt = case drt of
                                      D_rel     -> "D_rel"
                                      D_rel_h   -> "D_rel_h"
                                      D_rel_c   -> "D_rel_c"
                                      D_rel_c_h -> "D_rel_c_h"
                                      D_id      -> "D_id"
                                      D_v       -> "D_v"
                                      D_id_c    -> "D_id_c"
                                      D_v_c     -> "D_v_c"

instance ShowHS InfRuleType where
 showHS _ _ irt = case irt of
                                      ISect_cs  -> "ISect_cs"
                                      ISect_ncs -> "ISect_ncs"
                                      ISect_mix -> "ISect_mix"
                                      Union_mix -> "Union_mix"
                                      Comp_ncs  -> "Comp_ncs"
                                      Comp_c1   -> "Comp_c1"
                                      Comp_c2   -> "Comp_c2"
                                      Comp_cs   -> "Comp_cs"
                                      RAdd_ncs  -> "RAdd_ncs"
                                      RAdd_c1   -> "RAdd_c1"
                                      RAdd_c2   -> "RAdd_c2"
                                      RAdd_cs   -> "RAdd_cs"
                                      Conv_nc   -> "Conv_nc"
                                      Conv_c    -> "Conv_c"
-}

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

{-# LANGUAGE FlexibleInstances,DuplicateRecordFields,OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-} 
module           Ampersand.FSpec.ShowHS
    (ShowHS(..),ShowHSName(..),fSpec2Haskell,haskellIdentifier)
where
import           Ampersand.Basics
import           Ampersand.ADL1
import           Ampersand.Core.ShowAStruct  (AStruct(..))  -- for traceability, we generate comments in the Haskell code.
import           Ampersand.FSpec.FSpec
import           Ampersand.Misc.HasClasses
import           RIO.Char                  (isAlphaNum)
import           Data.Hashable
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import           RIO.Time
import           Text.Pandoc hiding (Meta)

fSpec2Haskell :: (HasRootFile env) =>
       env -> UTCTime -> FSpec -> String
fSpec2Haskell env now fSpec
        = "{-# OPTIONS_GHC -Wall #-}"
          ++"\n{-Generated code by "++ampersandVersionStr++" at "++show now++"-}"
          ++"\nmodule Main where\n"
          ++"\nimport Ampersand"
          ++"\nimport Text.Pandoc hiding (Meta)"
          ++"\n"
          ++"\nmain :: IO ()"
          ++"\nmain = do env <- getOptions"
          ++"\n          say (showHS env \"\\n  \" fSpec_"++baseName env++")\n"
          ++"\nfSpec_"++baseName env++" :: FSpec"
          ++"\nfSpec_"++baseName env++" =\n  "++showHS env "\n  " fSpec
wrap :: String->String->(String->a->String)->[a]->String
wrap initStr indent f xs
 = initStr++
   case xs of
     []  -> "[]"
     [x] -> "[ "++f (indent++"  ") x++" ]"
     _   -> "[ "++L.intercalate (indent++", ") [f (indent++"  ") x | x<-xs]++indent++"]"

class ShowHSName a where
 showHSName :: a -> String

class ShowHS a where
 showHS :: (HasRootFile env) => env -> String -> a -> String

instance ShowHSName a => ShowHSName [a] where
 showHSName xs = "["++L.intercalate "," (map showHSName xs)++"]"

instance ShowHS a => ShowHS [a] where
 showHS env indent = wrap "" (indent++" ") (showHS env)

instance ShowHS a => ShowHS (NE.NonEmpty a) where
 showHS env indent = wrap "" (indent++" ") (showHS env) . NE.toList

instance ShowHSName a => ShowHSName (Maybe a) where
 showHSName Nothing  = "Nothing"
 showHSName (Just x) = showHSName x

instance ShowHS a => ShowHS (Maybe a) where
 showHS _ _ Nothing  = "Nothing"
 showHS env indent (Just x) = "Just (" ++ showHS env indent x ++ ")"

instance (ShowHSName a , ShowHSName b) => ShowHSName (a,b) where
 showHSName (a,b) = "( "++showHSName a++" , "++showHSName b++" )"
-- | The following is used to showHS env for signs: (Concept, Concept)
--   instance (ShowHS a , ShowHS b) => ShowHS (a,b) where
--    showHS env indent (a,b) = "("++showHS env (indent++" ") a++","++showHS env (indent++" ") b++")"

instance ShowHSName PlugSQL where
 showHSName plug = haskellIdentifier ("plug_"++name plug)

instance ShowHS PlugSQL where
 showHS env indent plug
   = case plug of
       TblSQL{} -> L.intercalate indent
                   ["let " ++ L.intercalate (indent++"    ")
                                          [showHSName f++indent++"     = "++showHS env (indent++"       ") f 
                                          | f<-NE.toList $ plugAttributes plug] ++indent++"in"
                   ,"TblSQL { sqlname    = " ++ (show.name) plug
                   ,"       , attributes = ["++L.intercalate ", " (map showHSName (attributes plug))++"]"
                   ,"       , cLkpTbl    = [ "++L.intercalate (indent++"                      , ") ["("++showHSName c++", "++showHSName cn++")" | (c,cn)<-cLkpTbl plug] ++ "]"
                   ,"       , dLkpTbl    = [ "++L.intercalate (indent++"                      , ") 
                                                ( map (showHS env (indent++"                        ")) . dLkpTbl $ plug) 
                                              ++ "]"
                   ,"       }"
                   ]
       BinSQL{} -> L.intercalate indent
                   ["let " ++ L.intercalate (indent++"    ")
                                          [showHSName f++indent++"     = "++showHS env (indent++"       ") f 
                                          | f<-NE.toList $ plugAttributes plug] ++indent++"in"
                   ,"BinSQL { sqlname = " ++ (show.name) plug
                   ,"       , cLkpTbl = [ "++L.intercalate (indent++"                   , ") ["("++showHSName c++", "++showHSName cn++")" | (c,cn)<-cLkpTbl plug] ++ "]"
                   ,"       , dLkpTbl    = [ "++L.intercalate (indent++"                      , ") 
                                                ( map (showHS env (indent++"                        ")) . dLkpTbl $ plug) 
                                              ++ "]"
                   ,"       }"
                   ]

instance ShowHS RelStore where
 showHS _ indent store
   = L.intercalate indent
       [  "Relstore { rsDcl           = " ++ showHSName (rsDcl store)
       ,  "         , rsStoredFlipped = " ++ show (rsStoredFlipped store)
       ,  "         , rsSrcAtt        = " ++ showHSName (rsSrcAtt store)
       ,  "         , rsTrgAtt        = " ++ showHSName (rsTrgAtt store)
       ,  "         }"
       ]

instance ShowHSName SqlAttribute where
 showHSName sqAtt = haskellIdentifier ("sqlAtt_"++attName sqAtt)

instance ShowHS SqlAttribute where
 showHS env indent sqAtt
   = L.intercalate indentA
       [  "Att { attName    = " ++ show (attName sqAtt)
       ,      ", attExpr    = " ++ showHS env indentB (attExpr sqAtt)
       ,      ", attType    = " ++ showHS env "" (attType sqAtt)
       ,      ", attUse     = " ++ showHS env "" (attUse sqAtt)
       ,      ", attNull    = " ++ show (attNull sqAtt)
       ,      ", attDBNull  = " ++ show (attDBNull sqAtt)
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
   = L.intercalate indent
            [ "Quad{ qDcl     = " ++ showHSName (qDcl q)
            , "    , qRule    = " ++ showHSName (qRule q)
            , wrap "    , qConjuncts = " newindent (const showHSName) (NE.toList $ qConjuncts q)
            , "    }"
            ]
    where
      newindent = indent ++ "                 "

instance ShowHS DnfClause where
 showHS env indent dnf
   = L.intercalate indent
       [ wrap "Dnf " (indent++"    ") (\_->showHS env (indent++"      ")) (antcs dnf)
       , wrap "    " (indent++"    ") (\_->showHS env (indent++"      ")) (conss dnf)
       ]

instance ShowHSName Conjunct where
 showHSName x = haskellIdentifier (rc_id x)

instance ShowHS Conjunct where
 showHS env indent x
   = L.intercalate (indent ++"    ")
       [   "Cjct{ rc_id         = " ++ show (rc_id x)
       ,       ", rc_orgRules   = " ++ "[ "++L.intercalate ", " (NE.toList . fmap showHSName $ rc_orgRules x)++"]"
       ,       ", rc_conjunct   = " ++ showHS env indentA (rc_conjunct x)
       , wrap  ", rc_dnfClauses = " indentA (\_->showHS env (indentA++"  ")) (rc_dnfClauses x)
       ,       "}"
       ]
     where indentA = indent ++"                    "

instance ShowHSName FSpec where
 showHSName fSpec = haskellIdentifier ("fSpc_"++name fSpec)

instance ShowHS FSpec where
 showHS env indent fSpec
  = L.intercalate (indent ++"     ")
        [ "FSpec{ fsName        = " ++ show (name fSpec)
        , wrap ", fspos         = " indentA (showHS env) (fspos fSpec)
        , wrap ", plugInfos     = " indentA (\_->showHS env (indentA++"  ")) (plugInfos  fSpec)
        ,      ", interfaceS    = interfaceS'"
        ,      ", interfaceG    = interfaceG'"
        ,      ", fRoleRuls     = " ++showHS env indentA (fRoleRuls fSpec)
        , wrap ", fRoles        = " indentA (showHS env)    [rol | (rol,_) <- fRoles fSpec]
        , wrap ", vrules        = " indentA (const showHSName) (Set.elems $ vrules fSpec)
        , wrap ", grules        = " indentA (const showHSName) (Set.elems $ grules fSpec)
        , wrap ", invariants    = " indentA (const showHSName) (Set.elems $ invariants fSpec)
        , wrap ", fallRules     = " indentA (const showHSName) (Set.elems $ fallRules fSpec)
        , wrap ", allUsedDecls  = " indentA (const showHSName) (Set.elems $ allUsedDecls fSpec)
        , wrap ", vrels         = " indentA (const showHSName) (Set.elems $ vrels fSpec)
        , wrap ", allConcepts   = " indentA (const showHSName) (Set.elems $ allConcepts fSpec)
        , wrap ", vIndices      = " indentA (const showHSName) (vIndices fSpec)
        , wrap ", vviews        = " indentA (const showHSName) (vviews fSpec)
        , wrap ", vgens         = " indentA (showHS env)    (vgens fSpec)
        , wrap ", fsisa         = " indentA (const showHSName) (fsisa fSpec)
        , wrap ", allConjuncts  = " indentA (const showHSName) (allConjuncts fSpec)
        , wrap ", vquads        = " indentA (const showHSName) (vquads fSpec)
        , wrap ", vpatterns     = " indentA (const showHSName) (vpatterns fSpec)
        , wrap ", conceptDefs   = " indentA (showHS env)    (conceptDefs fSpec)
        , wrap ", fSexpls       = " indentA (showHS env)    (fSexpls fSpec)
        ,      ", metas         = allMetas"
        , wrap ", allViolations = " indentA showViolatedRule (allViolations fSpec)
        , wrap ", allExprs      = " indentA (showHS env)    (Set.toList $ allExprs fSpec)
        , "}"
        ] ++
    indent++"where"++
     "\n -- ***Interfaces Specified in Ampersand script***: "++
    indent++" interfaceS' = "++(if null (interfaceS fSpec) then "[]" else
                              "[ "++L.intercalate (indentB++"  , ") (map showHSName (interfaceS fSpec))++indentB++"  ]")++
     "\n -- ***Activities Generated by the Ampersand compiler ***: " ++
    indent++" interfaceG' = "++(if null (interfaceG fSpec) then "[]" else
                              "[ "++L.intercalate (indentB++", ") (map showHSName (interfaceG fSpec))++indentB++"]")++
    indent++" allMetas = "++(if null (metas fSpec) then "[]" else
                              "[ "++L.intercalate (indentB++", ") (map (showHS env (indent ++ "         ")) (metas fSpec))++indentB++"]") ++

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
     concat [indent++" "++showHSName s++indent++"  = "++showHS env (indent++"    ") s | s<-interfaceS fSpec]++"\n"
    )++
    (if null (interfaceG fSpec ) then "" else
     "\n -- *** Generated interfaces (total: "++(show.length.interfaceG) fSpec++" interfaces) ***: "++
     concat [indent++" "++showHSName x++indent++"  = "++showHS env (indent++"    ") x |x<-interfaceG fSpec ]++"\n"
    )++
    (let ds = vrels fSpec `Set.union` allUsedDecls fSpec `Set.union` (Set.fromList . map qDcl . vquads) fSpec in
     if null ds then "" else
     "\n -- *** Declared relations (in total: "++(show.length) ds++" relations) ***: "++
     concat [indent++" "++showHSName x++indent++"  = "++showHS env (indent++"    ") x |x<-Set.elems ds]++"\n"
    ) ++
    (if null (vIndices fSpec)     then "" else
     "\n -- *** Indices (total: "++(show.length.vIndices) fSpec++" indices) ***: "++
     concat [indent++" "++showHSName x++indent++"  = "++showHS env (indent++"    ") x |x<-vIndices fSpec]++"\n"
    ) ++
    (if null (vviews fSpec)     then "" else
     "\n -- *** Views (total: "++(show.length.vviews) fSpec++" views) ***: "++
     concat [indent++" "++showHSName x++indent++"  = "++showHS env (indent++"    ") x |x<-vviews fSpec]++"\n"
    ) ++
    (if null (vrules   fSpec ) then "" else
     "\n -- *** User defined rules (total: "++(show.length.vrules) fSpec++" rules) ***: "++
     concat [indent++" "++showHSName x++indent++"  = "++showHS env (indent++"    ") x |x<-Set.elems $ vrules     fSpec ]++"\n"
    )++
    (if null (grules   fSpec ) then "" else
     "\n -- *** Generated rules (total: "++(show.length.grules) fSpec++" rules) ***: "++
     concat [indent++" "++showHSName x++indent++"  = "++showHS env (indent++"    ") x |x<-Set.elems $ grules     fSpec ]++"\n"
    )++
    (if null (allConjuncts fSpec ) then "" else
     "\n -- *** Conjuncts (total: "++(show.length.allConjuncts) fSpec++" conjuncts) ***: "++
     concat [indent++" "++showHSName x++indent++"  = "++showHS env (indent++"    ") x |x<-allConjuncts     fSpec ]++"\n"
    )++
    (if null (vquads fSpec ) then "" else
     "\n -- *** Quads (total: "++(show.length.vquads) fSpec++" quads) ***: "++
     concat [indent++" "++showHSName x++indent++"  = "++showHS env (indent++"    ") x |x<-vquads     fSpec ]++"\n"
    )++
    (if null (plugInfos fSpec ) then "" else
     "\n -- *** PlugInfos (total: "++(show.length.plugInfos) fSpec++" plugInfos) ***: "++
     concat [indent++" "++showHSName p++indent++"  = "++showHS env (indent++"    ") p |InternalPlug p<-L.sortBy (compare `on` name) (plugInfos fSpec) ]++"\n"
    )++
    (if null (vpatterns fSpec) then "" else
     "\n -- *** Patterns (total: "++(show.length.vpatterns) fSpec++" patterns) ***: "++
     concat [indent++" "++showHSName x++indent++"  = "++showHS env (indent++"    ") x |x<-vpatterns fSpec]++"\n"
    )++
--       (if null (conceptDefs fSpec) then "" else
--        "\n -- *** ConceptDefs (total: "++(show.length.conceptDefs) fSpec++" conceptDefs) ***: "++
--        concat [indent++" "++showHSName cd++indent++"  = "++showHS env (indent++"    ") cd | c<-concs fSpec, cd<-concDefs fSpec c]++"\n"
--       )++
    (if null (allConcepts fSpec) then "" else
     "\n -- *** Concepts (total: "++(show.length.allConcepts) fSpec++" concepts) ***: "++
     concat [indent++" "++showHSName x++indent++"  = "++showHS env (indent++"    ") x
          ++ indent++"    "++showAtomsOfConcept x |x<-L.sortBy (comparing showHSName) (Set.toList $ allConcepts fSpec)]++"\n"
    )
        where indentA = indent ++"                       "
              indentB = indent ++"             "
              showAtomsOfConcept c =
                           "-- atoms: [ "++ L.intercalate indentC strs++"]"
                  where
                    strs = map showVal . L.sort . Set.elems . atomsInCptIncludingSmaller fSpec $ c
                      where showVal val= "`"++showValADL val++"`" 
                    indentC = if sum (map length strs) > 300
                              then indent ++ "    --        , "
                              else ", "
              showViolatedRule :: String -> (Rule,AAtomPairs) -> String
              showViolatedRule indent' (r,ps)
                 = L.intercalate indent'
                     [        " ( "++showHSName r++" -- This is "++(if isSignal r then "a process rule." else "an invariant")++
                      indent'++" , "++ wrap "" (indent'++"   ") 
                                               (let showPair _ p = "( "++ (show.showValADL.apLeft) p++", "++(show.showValADL.apRight) p++")"
                                                in showPair
                                               ) (Set.elems ps)++
                      indent'++" )"
                     ]

instance ShowHS Meta where
 showHS f i (Meta pos' obj nm val) = "Meta ("++showHS f i pos' ++ ") "++ show obj ++ " " ++ show nm ++ " " ++ show val

instance ShowHSName PlugInfo where
 showHSName (InternalPlug p) = haskellIdentifier ("ipl_"++name p)-- TODO
 
instance ShowHS PlugInfo where
 showHS _ _ (InternalPlug p)
  = "InternalPlug "++showHSName p

instance ShowHS Role where
 showHS _ ind r = ind++
                  (case r of
                     Role str -> "Role "++show str
                     Service str -> "Service "++show str
                  ) 
 
instance ShowHS P_RoleRule where
 showHS env ind rs
  = "Maintain "++show (mRoles rs)++" "++show (mRules rs)++" "++showHS env (ind++"    ") (origin rs)
instance ShowHS (Role,Rule) where
 showHS _ _ (rol,rul)
  = "("++show rol++", "++showHSName rul++")"

instance ShowHSName Pattern where
 showHSName pat = haskellIdentifier ("pat_"++name pat)

instance ShowHS Pattern where
 showHS env indent pat
  = L.intercalate indentA
     [ "A_Pat { ptnm  = "++show (name pat)
     , ", ptpos = "++showHS env "" (ptpos pat)
     , ", ptend = "++showHS env "" (ptend pat)
     , ", ptrls = [" ++L.intercalate ", " [showHSName r | r<-Set.elems $ ptrls pat] ++ concat [" {- no rules -} "        | Set.null (ptrls pat)] ++"]"
     , wrap ", ptgns = " indentB (showHS env) (ptgns pat)
     , ", ptdcs = [ " ++L.intercalate (indentB++", ") [showHSName d | d<-Set.elems $ ptdcs pat] ++ concat [" {- no relations -} " | null (ptdcs pat)] ++indentB++"]"
     , wrap ", ptups = " indentB (showHS env) (ptups pat)
     , wrap ", ptids = " indentB (showHS env) (ptids pat)
     , wrap ", ptvds = " indentB (showHS env) (ptvds pat)
     , wrap ", ptxps = " indentB (showHS env) (ptxps pat)
     , "}"
     ] where indentA = indent ++"      "     -- adding the width of "A_Pat "
             indentB = indentA++"          " -- adding the width of ", ptrls = "

instance ShowHS PPurpose where
 showHS env _ expl =
    "PRef2 ("++showHS env "" (origin     expl)++") "++
          "("++showHS env "" (pexObj     expl)++") "++
          "("++showHS env "" (pexMarkup  expl)++") "
             ++show (L.intercalate ";" (pexRefIDs expl))++" "

instance ShowHS PRef2Obj where
 showHS _ _ peObj
  = case peObj of
         PRef2ConceptDef str                    -> "PRef2ConceptDef " ++show str
         PRef2Relation (PNamedRel _ nm mSgn) -> "PRef2Relation "++show nm++maybe "" show mSgn
         PRef2Rule str                          -> "PRef2Rule "       ++show str
         PRef2IdentityDef str                   -> "PRef2IdentityDef "++show str
         PRef2ViewDef str                       -> "PRef2ViewDef "    ++show str
         PRef2Pattern str                       -> "PRef2Pattern "    ++show str
         PRef2Interface str                     -> "PRef2Interface "  ++show str
         PRef2Context str                       -> "PRef2Context "    ++show str

instance ShowHS Purpose where
 showHS env _ expla =
    "Expl "++"("++showHS env "" (explPos expla)++") "
           ++"("++showHS env "" (explObj expla)++") "
                ++showHS env "" (explMarkup  expla)++" "
                ++show (explUserdefd expla)++" "
                ++show (explRefIds expla)++" "

instance ShowHS ExplObj where
 showHS env i peObj = case peObj of
          ExplConcept cpt    -> "ExplConcept "    ++showHS env i cpt
          ExplRelation rel   -> "ExplRelation "   ++showHSName rel
          ExplRule str       -> "ExplRule "       ++show str
          ExplIdentityDef str-> "ExplIdentityDef "++show str
          ExplViewDef str    -> "ExplViewDef "    ++show str
          ExplPattern str    -> "ExplPattern "    ++show str
          ExplInterface str  -> "ExplInterface "  ++show str
          ExplContext str    -> "ExplContext "    ++show str

instance ShowHS P_Markup where
 showHS _ indent m
   = L.intercalate indent
     ["P_Markup{ mLang   = "++ show (mLang m)
     ,"        , mFormat = "++ show (mFormat m)
     ,"        , mString = "++ show (mString m)
     ,"        }"
     ]

instance ShowHS Markup where
 showHS _ indent m
   = L.intercalate indent
     ["Markup{ amLang   = "++ show (amLang m)
     ,"        , amPandoc = "++ show (amPandoc m)
     ,"        }"
     ]

instance ShowHS (PairView Expression) where
  showHS env indent (PairView pvs) = "PairView "++showHS env indent (NE.toList pvs)

instance ShowHS (PairViewSegment Expression) where
  showHS _     _ (PairViewText _ txt) = "PairViewText "++show txt
  showHS env _ (PairViewExp _ srcOrTgt e) = "PairViewExp "++show srcOrTgt++" ("++showHS env "" e++")"

instance ShowHSName Rule where
 showHSName r = haskellIdentifier ("rule_"++ rrnm r)

instance ShowHS Rule where
 showHS env indent r@(Ru _ _ _ _ _ _ _ _ _ _)  -- This pattern matching occurs so Haskell will detect any change in the definition of Ru.
   = L.intercalate indent
     ["Ru{ rrnm   = " ++ show (rrnm   r)
     ,"  , formalExpression  = -- " ++ showA (formalExpression  r) ++ indent++"             " ++ showHS env (indent++"             ") (formalExpression  r)
     ,"  , rrfps  = " ++ showHS env "" (rrfps  r)
     ,"  , rrmean = " ++ showHS env (indent++"             ") (rrmean r)
     ,"  , rrmsg  = " ++ showHS env "" (rrmsg  r)
     ,"  , rrviol = " ++ showHS env "" (rrviol r)
     ,"  , rrdcl  = " ++ case rrdcl r of
                           Just (p,d) -> "Just ("++showHSName p++", "++showHSName d++" )"
                           Nothing    -> "Nothing"
     ,"  , rrpat  = " ++ show (rrpat  r)
     ,"  , r_usr  = " ++ show (r_usr  r)
     ,"  , isSignal = " ++ show (isSignal  r)
     ,"  }"
     ]

instance ShowHS Meaning where
  showHS env indent (Meaning x) = "Meaning " ++ showHS env (indent++"        ") x

instance ShowHSName IdentityDef where
 showHSName identity = haskellIdentifier ("identity_"++name identity)

instance ShowHS IdentityDef where
 showHS env indent identity
  = "Id ("++showHS env "" (idPos identity)++") "++show (idLbl identity)++" ("++showHSName (idCpt identity)++")"
    ++indent++"  [ "++L.intercalate (indent++"  , ") (NE.toList . fmap (showHS env indent) $ identityAts identity)++indent++"  ]"

instance ShowHS IdentitySegment where
 showHS env indent (IdentityExp objDef) = "IdentityExp ("++ showHS env indent objDef ++ ")"

instance ShowHSName ViewDef where
 showHSName vd = haskellIdentifier ("vdef_"++name vd)

instance ShowHS ViewDef where
 showHS env indent vd
  = "Vd ("++showHS env "" (vdpos vd)++") "++show (name vd)++" "++showHSName (vdcpt vd)
    ++indent++"  [ "++L.intercalate (indent++"  , ") (fmap (showHS env indent) $ vdats vd)++indent++"  ]"

instance ShowHS ViewSegment where
  showHS env indent vs =
    "ViewSegment "++showHS env indent (origin vs) ++ " "
                  ++showHS env indent (vsmlabel vs)
                  ++" "++show (vsmSeqNr vs)++" "
                  ++showHS env indent (vsmLoad vs)
-- showHSName vd = haskellIdentifier ("vdef_"++name vd)

instance ShowHS ViewSegmentPayLoad where
 showHS _     _     (ViewText str)  = "ViewText "++show str
 showHS env indent (ViewExp  expr) = showHS env (indent++"            ") expr

instance ShowHS Population where
 showHS _ indent pop
  = case pop of
      ARelPopu{} -> "ARelPopu { popdcl = "++showHSName (popdcl pop)
--TODOFIX
--          ++indent++"         , popps  = [ "++L.intercalate
--           (indent++"                    , ") (map show (popps pop))
          ++indent++"                    ]"
          ++indent++"         }"
      ACptPopu{} -> "ACptPopu { popcpt = "++showHSName (popcpt pop)
--TODOFIX
--          ++indent++"         , popas  = [ "++L.intercalate
--           (indent++"                    , ") (map show (popas pop))
          ++indent++"                    ]"
          ++indent++"         }"

instance ShowHSName ObjectDef where
 showHSName obj = haskellIdentifier ("oDef_"++name obj)

instance ShowHS ObjectDef where
 showHS env indent x
  = L.intercalate indent
        ["ObjectDef { objnm    = " ++ show(name x)
        ,"       , objpos   = " ++ showHS env "" (origin x)
        ,"       , objExpression   = " ++ showHS env (indent++"                ") (objExpression x)
        ,"       , objcrud  = " ++ showHS env (indent++"                ") (objcrud x)
        ,"       , objmView = " ++ show(objmView x)
        ,"       , objmsub  = " ++ showHS env (indent++"                ") (objmsub x)
        ,"       }"
        ]
instance ShowHS BoxTxt where
 showHS env indent x
  = L.intercalate indent
        ["BoxTxt { objnm    = " ++ show(name x)
        ,"       , objpos   = " ++ showHS env "" (origin x)
        ,"       , objtxt   = " ++ show(objtxt x)
        ,"       }"
        ]
instance ShowHS Cruds where
 showHS env indent x 
  = L.intercalate indent
      ["Cruds { crudOrig = "++ showHS env "" (crudOrig x)
      ,"      , crudC    = "++ show (crudC x)
      ,"      , crudR    = "++ show (crudR x)
      ,"      , crudU    = "++ show (crudU x)
      ,"      , crudD    = "++ show (crudD x)
      ,"      }"
      ]  
instance ShowHSName Interface where
 showHSName obj = haskellIdentifier ("ifc_"++name obj)

instance ShowHS Interface where
 showHS env indent ifc
  = L.intercalate indent
        [ "Ifc { ifcname   = " ++ show(ifcname ifc)
        , "    , ifcRoles  = " ++ show(ifcRoles ifc)
        , "    , ifcObj"++indent++"       = " ++ showHS env (indent++"         ") (ifcObj ifc)
        , wrap "    , ifcControls = " (indent++"                  ") (const showHSName) (ifcControls ifc)
        , "    , ifcPos    = " ++ showHS env "" (ifcPos ifc)
        , "    , ifcPrp    = " ++ show(ifcPrp ifc)
        ]++indent++"    }"
instance ShowHS BoxItem where
 showHS env indent obj =
   case obj of
     (BxExpr e) -> "BxExpr ("++showHS env indent e++")"
     (BxTxt t) -> "BxTxt ("++showHS env indent t++")"
 
instance ShowHS SubInterface where
 showHS _     _     (InterfaceRef isLink n) = "InterfaceRef "++show isLink ++" "++show n
 showHS env indent (Box x cl objs) = "Box ("++showHS env indent x++") ("++showHS env indent cl++")"++indent++"     ("++showHS env (indent++"     ") objs++")"

instance ShowHS Expression where
 showHS env indent (EEqu (l,r)) = "EEqu ("++showHS env (indent++"      ") l++indent++"     ,"++showHS env (indent++"      ") r++indent++"     )"
 showHS env indent (EInc (l,r)) = "EInc ("++showHS env (indent++"      ") l++indent++"     ,"++showHS env (indent++"      ") r++indent++"     )"
 showHS env indent (EIsc (l,r)) = "EIsc ("++showHS env (indent++"      ") l++indent++"     ,"++showHS env (indent++"      ") r++indent++"     )"
 showHS env indent (EUni (l,r)) = "EUni ("++showHS env (indent++"      ") l++indent++"     ,"++showHS env (indent++"      ") r++indent++"     )"
 showHS env indent (EDif (l,r)) = "EDif ("++showHS env (indent++"      ") l++indent++"     ,"++showHS env (indent++"      ") r++indent++"     )"
 showHS env indent (ELrs (l,r)) = "ELrs ("++showHS env (indent++"      ") l++indent++"     ,"++showHS env (indent++"      ") r++indent++"     )"
 showHS env indent (ERrs (l,r)) = "ERrs ("++showHS env (indent++"      ") l++indent++"     ,"++showHS env (indent++"      ") r++indent++"     )"
 showHS env indent (EDia (l,r)) = "EDia ("++showHS env (indent++"      ") l++indent++"     ,"++showHS env (indent++"      ") r++indent++"     )"
 showHS env indent (ECps (l,r)) = "ECps ("++showHS env (indent++"      ") l++indent++"     ,"++showHS env (indent++"      ") r++indent++"     )"
 showHS env indent (ERad (l,r)) = "ERad ("++showHS env (indent++"      ") l++indent++"     ,"++showHS env (indent++"      ") r++indent++"     )"
 showHS env indent (EPrd (l,r)) = "EPrd ("++showHS env (indent++"      ") l++indent++"     ,"++showHS env (indent++"      ") r++indent++"     )"
 showHS env indent (EKl0 e    ) = "EKl0 ("++showHS env (indent++"      ") e++")"
 showHS env indent (EKl1 e    ) = "EKl1 ("++showHS env (indent++"      ") e++")"
 showHS env indent (EFlp e    ) = "EFlp ("++showHS env (indent++"      ") e++")"
 showHS env indent (ECpl e    ) = "ECpl ("++showHS env (indent++"      ") e++")"
 showHS env indent (EBrk e    ) = "EBrk ("++showHS env (indent++"      ") e++")"
 showHS _    _      (EDcD dcl  ) = "EDcD "++showHSName dcl
 showHS _    _      (EDcI c    ) = "EDcI "++showHSName c
 showHS env _      (EEps i sgn) = "EEps ("++showHS env "" i++") ("++showHS env "" sgn++")"
 showHS env _      (EDcV sgn  ) = "EDcV ("++showHS env "" sgn++")"
 showHS _    _      (EMp1 a c  ) = "EMp1 " ++show a++" "++showHSName c

instance ShowHS Signature where
 showHS _ _ sgn = "Sign "++showHSName (source sgn)++" "++showHSName (target sgn)

instance ShowHS AClassify where
 showHS _ _ gen =
   case gen of
     Isa{} -> "Isa "++showHSName (genspc gen)++" "++showHSName (gengen gen)++" "
     IsE{} -> "IsE "++showHSName (genspc gen)++" ["++L.intercalate ", " (map showHSName (genrhs gen))++"] "

instance ShowHSName Relation where
 showHSName d | decusr d  = haskellIdentifier ("rel_"++name d++"_"++name (source d)++"_"++name (target d)) -- user defined relations
              | otherwise = haskellIdentifier ("vio_"++name d++"_"++name (source d)++"_"++name (target d)) -- relations generated per rule

instance ShowHS Relation where
 showHS env indent d
    = L.intercalate indent
                     ["Relation { decnm   = " ++ show (decnm d)
                     ,"         , decsgn  = " ++ showHS env "" (sign d)
                     ,"         , decprps = " ++ showL(map (showHS env "") (Set.elems $ decprps d))
                     ,"         , decprps_calc = " ++ case decprps_calc d of
                                                 Nothing -> "Nothing"
                                                 Just ps -> "Just "++showL(map (showHS env "") (Set.elems ps))
                     ,"         , decprL  = " ++ show (decprL d)
                     ,"         , decprM  = " ++ show (decprM d)
                     ,"         , decprR  = " ++ show (decprR d)
                     ,"         , decMean = " ++ show (decMean d)
                     ,"         , decfpos = " ++ showHS env "" (decfpos d)
                     ,"         , decusr  = " ++ show (decusr d)
                     ,"         , decpat  = " ++ show (decpat d)
                     ]++"}"

--   instance ShowHSName ConceptDef where
--    showHSName cd = haskellIdentifier ("cDef_"++cdcpt cd)

instance ShowHS ConceptDef where
 showHS env _ cd
  = " Cd ("++showHS env "" (origin cd)++") "++show (cdcpt cd)++" "++show (cddef cd)++" "++show (cdref cd)++" "++show (cdfrom cd)
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
              Origin s      -> "Origin " ++ show s
              PropertyRule str declOrig 
                            -> "PropertyRule of "++str++" "++
                                  case declOrig of 
                                    FileLoc l sym -> "declared at FileLoc (" ++ show l ++ " " ++ sym ++ ")"
                                    _             -> fatal $ "This should be the origin of a Relation, but it doesn't seem like it is.\n"
                                                               ++show declOrig 
              OriginUnknown -> "OriginUnknown"
              XLSXLoc fPath sheet (a,b) -> "XLSXLoc "++fPath++" "++sheet++" "++show(a,b)
              MeatGrinder   -> "MeatGrinder"
instance ShowHS Origin where
 showHS env indent (FileLoc l s)               = "FileLoc (" ++ showHS env indent l ++ " " ++ s ++ ")"
 showHS env indent (PropertyRule str declOrig) = "PropertyRule " ++ show str ++ " ("++showHS env indent declOrig++")"
 showHS _     _     (Origin s)                  = "Origin " ++ show s
 showHS _     _     OriginUnknown               = "OriginUnknown"
 showHS _     _     (XLSXLoc fPath sheet (a,b)) = "XLSXLoc "++fPath++" "++sheet++" "++show(a,b)  
 showHS _     _     MeatGrinder                 = "MeatGrinder"

instance ShowHS Block where
 showHS _ _   = show

instance ShowHS Inline where
 showHS _ _   = show
                  
{-
instance ShowHS InfTree where
 showHS env indent itree =
     case itree of
       InfExprs irt (ratype,raobj) itrees ->
           "InfExprs " ++ showHS env indent irt ++
           indent ++ "   (" ++ showRaType ratype ++ "," ++ "RelAlgObj{-"++show raobj++"-}" ++ ")" ++
           indent ++ showHS env (indent ++ "     ") itrees
       InfRel drt ratype _ _ ->
           "InfRel " ++ showHS env indent drt ++ " " ++ showRaType ratype
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
showL xs = "["++L.intercalate "," xs++"]"

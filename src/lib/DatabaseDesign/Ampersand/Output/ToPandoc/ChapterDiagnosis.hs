{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.ChapterDiagnosis 
where
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Classes
import Data.List
import DatabaseDesign.Ampersand.Fspec.Fspec
import DatabaseDesign.Ampersand.Output.PandocAux

fatal :: Int -> String -> a
fatal = fatalMsg "Output.ToPandoc.ChapterDiagnosis.hs"

chpDiagnosis :: Fspc -> Options -> (Blocks,[Picture])
chpDiagnosis fSpec flags
 = ( fromList $
     header ++                -- the chapter header
     diagIntro ++             -- an introductory text
     roleomissions ++         -- says which role-rule, role-interface, and role-relation assignments are missing
     roleRuleTable ++         -- gives an overview of rule-rule assignments
     missingConceptDefs ++    -- says which concept definitions are missing
     missingRels ++           -- says which relation declarations are missing
     relsNotUsed ++           -- says which relations are not used in any rule
     missingRules ++          -- says which rule definitions are missing
     ruleRelationRefTable ++  -- table that shows percentages of relations and rules that have references
     invariantsInProcesses ++ -- 
     processrulesInPatterns++ -- 
-- TODO: Needs rework.     populationReport++       -- says which relations are populated.
     wipReport++              -- sums up the work items (i.e. the violations of process rules)
     toList violationReport          -- sums up the violations caused by the population of this script.
   , pics )
  where
  header :: [Block]
  header = toList (chptHeader flags Diagnosis)
  diagIntro :: [Block]
  diagIntro = 
    case language flags of
      Dutch   -> [Para
                  [ Str "Dit hoofdstuk geeft een analyse van het Ampersand-script van ", Quoted  SingleQuote [Str (name fSpec)], Str ". "
                  , Str "Deze analyse is bedoeld voor de auteurs van dit script. "
                  , Str "Op basis hiervan kunnen zij het script completeren en mogelijke tekortkomingen verbeteren. "
                  ]]
      English -> [Para
                  [ Str "This chapter provides an analysis of the Ampersand script of ", Quoted  SingleQuote [Str (name fSpec)], Str ". "
                  , Str "This analysis is intended for the authors of this script. "
                  , Str "It can be used to complete the script or to improve possible flaws. "
                  ]]
   

  roleRuleTable :: [Block]
  roleRuleTable
    | null ruls = []
    | null (fRoleRuls fSpec) && null(fRoleRels fSpec) = 
        case language flags of
          Dutch    -> [Para [ Str $ upCap (name fSpec)++" specificeert geen rollen. " ]]
          English  -> [Para [ Str $ upCap (name fSpec)++" does not define any roles. " ]]
    | null [r | r<-vrules fSpec, isSignal r ] =
        case language flags of
          Dutch    -> [Para [ Str $ upCap (name fSpec)++" kent geen procesregels. " ]]
          English  -> [Para [ Str $ upCap (name fSpec)++" does not define any process rules. " ]]
    | otherwise =
        (case language flags of
          Dutch    -> Para [ Str $ upCap (name fSpec)++" kent regels aan rollen toe. "
                            , Str "De volgende tabel toont welke regels door een bepaalde rol kunnen worden gehandhaafd."]
          English  -> Para [ Str $ upCap (name fSpec)++" assigns rules to roles. "
                            , Str "The following table shows the rules that are being maintained by a given role."]
        ) :
        [Table []  -- the table containing the role-rule assignments
        (AlignLeft:[AlignCenter |_<-rs])
        (0.0:[0.0 |_<-rs])
        (( case language flags of
          Dutch   -> [Plain [Str "regel"]] 
          English -> [Plain [Str "rule" ]] 
        ) :    [ [Plain [Str r]] | r <- rs ]
        )
        [ [Plain [Str (name rul)]]:[f r rul | r<-rs] | rul<-ruls ] 
        ]
     where
      rs = nub ( [r | (r,_) <- fRoleRuls fSpec]++
                 [r | (r,_) <- fRoleRels fSpec] )
      ruls = if null (themes fSpec)
             then [r | r<-vrules fSpec, isSignal r ]
             else [r | pat<-patterns   fSpec, name pat `elem` themes fSpec, r<-udefrules pat,         isSignal r ] ++
                  [r | prc<-vprocesses fSpec, name prc `elem` themes fSpec, r<-udefrules (fpProc prc) , isSignal r ]
      f r rul | (r,rul) `elem` maintained      = [Plain [Math InlineMath "\\surd"]]
              | (r,rul) `elem` dead            = [Plain [Math InlineMath "\\times"]]
              | (r,rul) `elem` fRoleRuls fSpec = [Plain [Math InlineMath "\\odot"]]
              | otherwise                      = []
      maintained  -- (r,rul) `elem` maintained means that r can maintain rul without restrictions.
        = [ (role,rul)
          | (role,rul)<-fRoleRuls fSpec
          , and (map (mayEdit role) (declsUsedIn rul))
          ]
      mayEdit :: String -> Declaration -> Bool
      mayEdit role decl = decl `elem` ((snd.unzip) (filter (\x -> role == fst x) (fRoleRels fSpec))) 
      dead -- (r,rul) `elem` dead means that r cannot maintain rul without restrictions.
       = [ (role,rul)
         | (role,rul)<-fRoleRuls fSpec
         , (not.or) (map (mayEdit role) (declsUsedIn rul))
         ]

  roleomissions :: [Block]
  roleomissions
   = if      null  (themes fSpec) && (not.null) (vprocesses fSpec) ||
        (not.null) (themes fSpec) && (not.null) (themes fSpec `isc` [name prc | prc<-vprocesses fSpec])
     then [ case language flags of
              Dutch   ->
                Plain [ Str $ upCap (name fSpec)++" kent geen regels aan rollen toe. "
                       , Str "Een generieke rol, User, zal worden gedefinieerd om al het werk te doen wat in het bedrijfsproces moet worden uitgevoerd."
                       ]
              English ->
                Plain [ Str $ upCap (name fSpec)++" does not assign rules to roles. "
                       , Str "A generic role, User, will be defined to do all the work that is necessary in the business process."
                       ]
          | (null.fRoleRuls) fSpec && (not.null.udefrules) fSpec] ++
          [ case language flags of
              Dutch   ->
                Plain [ Str $ upCap (name fSpec)++" specificeert niet welke rollen de inhoud van welke relaties mogen wijzigen. "
                       , Str ""
                       ]
              English ->
                Plain [ Str $ upCap (name fSpec)++" does not specify which roles may change the contents of which relations. "
                       , Str ""
                       ]
          | null (fRoleRels fSpec), (not.null.fRoleRuls) fSpec ||(not.null.fRoleRels) fSpec]
     else []
  missingConceptDefs :: [Block]
  missingConceptDefs
   = case (language flags, missing) of
      (Dutch,[])  -> [Para
                       [Str "Alle concepten in dit document zijn voorzien van een bestaansreden."]
                     | (not.null.concs) fSpec]
      (Dutch,[c]) -> [Para
                       [Str "De bestaansreden van concept ", Quoted SingleQuote [Str (name c)], Str " is niet gedocumenteerd."]
                     ]
      (Dutch,xs)  -> [Para $
                       [Str "De bestaansreden van de concepten: "]++commaNLPandoc (Str "en") (map (Str . name) xs)++[Str " is niet gedocumenteerd."]
                     ]
      (English,[])  -> [Para 
                        [Str "All concepts in this document have been provided with a purpose."]
                     | (not.null.concs) fSpec]
      (English,[c]) -> [Para 
                         [Str "The concept ", Quoted SingleQuote [Str (name c)], Str " remains without a purpose."]
                     ]
      (English,xs)  -> [Para $
                       [Str "Concepts "]++commaEngPandoc (Str "and") (map (Str . name) xs)++[Str " remain without a purpose."]
                     ]
   where missing = [c | c <-ccs
                      , cd <- cptdf c
                      , null (purposesDefinedIn fSpec (language flags) cd)
                   ]++
                   [c | c <-ccs
                      , null (cptdf c)
                   ]
         ccs = concs [ d | d<-declarations fSpec, null (themes fSpec)||decpat d `elem` themes fSpec]  -- restrict if the documentation is partial.
  missingRels :: [Block]
  missingRels
   = case (language flags, missing) of
      (Dutch,[])  -> [Para 
                       [Str "Alle relaties in dit document zijn voorzien van een reden van bestaan (purpose)."]
                     | (not.null.declsUsedIn.udefrules) fSpec]
      (Dutch,[r]) -> [Para 
                       [ Str "De reden waarom relatie ", r
                       , Str " bestaat wordt niet uitgelegd."
                     ] ]
      (Dutch,rs)  -> [Para $
                       [ Str "Relaties "]++commaNLPandoc (Str "en") rs++
                       [ Str " zijn niet voorzien van een reden van bestaan (purpose)."
                     ] ]
      (English,[])  -> [Para 
                         [Str "All relations in this document have been provided with a purpose."]
                       | (not.null.declsUsedIn.udefrules) fSpec]
      (English,[r]) -> [Para 
                         [ Str "The purpose of relation ", r
                         , Str " remains unexplained."
                       ] ]
      (English,rs)  -> [Para $
                         [ Str "The purpose of relations "]++commaEngPandoc (Str "and") rs++
                         [ Str " is not documented."
                       ] ]
     where missing = [(Math InlineMath . showMath) (EDcD d (sign d)) 
                     | d@Sgn{} <-if null (themes fSpec)
                                 then declsUsedIn fSpec
                                 else declsUsedIn [pat | pat<-patterns fSpec, name pat `elem` themes fSpec]++
                                      declsUsedIn [fpProc prc | prc<-vprocesses fSpec, name prc `elem` themes fSpec]
                     , not (isIdent d)
                     , null (purposesDefinedIn fSpec (language flags) d)
                     ]

  relsNotUsed :: [Block]
  pics :: [Picture]
  (relsNotUsed,pics)
   = ( ( case (language flags, notUsed) of
          (Dutch,[])  -> [Para 
                           [Str "Alle relaties in dit document worden in één of meer regels gebruikt."]
                         | (not.null.declsUsedIn.udefrules) fSpec]
          (Dutch,[r]) -> [Para 
                           [ Str "De relatie ", r
                           , Str " wordt in geen enkele regel gebruikt. "
                         ] ]
          (Dutch,rs)  -> [Para $
                           [ Str "Relaties "]++commaNLPandoc (Str "en") rs++
                           [ Str " worden niet gebruikt in regels. "
                         ] ]
          (English,[])  -> [Para 
                             [Str "All relations in this document are being used in one or more rules."]
                           | (not.null.declsUsedIn.udefrules) fSpec]
          (English,[r]) -> [Para 
                             [ Str "Relation ", r
                             , Str " is not being used in any rule. "
                           ] ]
          (English,rs)  -> [Para $
                             [ Str "Relations "]++commaEngPandoc (Str "and") rs++
                             [ Str " are not used in any rule. "
                           ] ] ) ++
       ( case (language flags, pictsWithUnusedRels) of
          (Dutch,[pict])     -> [ Para [ Str "Figuur "
                                       , xrefReference pict
                                       , Str " geeft een conceptueel diagram met alle relaties."
                                       ] 
                                , Plain (xrefFigure1 pict)
                                ]
          (English,[pict])   -> [ Para [ Str "Figure "
                                       , xrefReference pict
                                       , Str " shows a conceptual diagram with all relations."
                                       ]
                                , Plain (xrefFigure1 pict)
                                ]
          (Dutch,picts)   -> concat
                                  [ Para [ Str "Figuur "
                                         , xrefReference pict
                                         , Str " geeft een conceptueel diagram met alle relaties die gedeclareerd zijn in "
                                         , Quoted SingleQuote [Str (name pat)]
                                         , Str "."
                                         ] 
                                    : [Plain (xrefFigure1 pict)]
                                  | (pict,pat)<-zip picts pats ]
          (English,picts) -> concat
                                  [ Para [ Str "Figure "
                                         , xrefReference pict
                                         , Str " shows a conceptual diagram with all relations declared in "
                                         , Quoted SingleQuote [Str (name pat)]
                                         , Str "."
                                         ]
                                    : [Plain (xrefFigure1 pict)]
                                  | (pict,pat)<-zip picts pats ] )
       , pictsWithUnusedRels           -- draw the conceptual diagram
     )
     where notUsed = nub [(Math InlineMath . showMath) (EDcD d (sign d))
                         | d@Sgn{} <- declarations fSpec
                         , null (themes fSpec) || decpat d `elem` themes fSpec  -- restrict if the documentation is partial.
                         , decusr d
                         , d `notElem` (declsUsedIn . udefrules) fSpec
                         ]
           pats  = [ pat | pat<-patterns fSpec
                         , null (themes fSpec) || name pat `elem` themes fSpec  -- restrict if the documentation is partial.
                         , (not.null) (declarations pat>-declsUsedIn pat) ]
           pictsWithUnusedRels = [makePicture flags fSpec Rel_CG pat | pat<-pats ]

  missingRules :: [Block]
  missingRules
   = case (language flags, missingPurp, missingMeaning) of
      (Dutch,[],[])    -> [ Para [Str "Alle regels in dit document zijn voorzien van een uitleg."]
                          | (length.udefrules) fSpec>1]
      (Dutch,rs,rs')   -> [Para 
                           (case rs>-rs' of
                              []  -> []
                              [r] -> [ Str "De bestaansreden van regel ", Emph [Str (name r)]
                                     , Str (" op regelnummer "++ln r++" van bestand "++fn r)
                                     , Str " wordt niet uitgelegd. "
                                     ]
                              rls -> (upC . commaNLPandoc (Str "en")  )
                                        [let nrs = [(Str . show . linenr) l | l<-cl] in
                                         strconcat ([Str ("op regelnummer"++(if length nrs>1 then "s" else "")++" ")]++
                                                    commaNLPandoc (Str "en") nrs++
                                                    [Str " van bestand "]++[(Str . locnm . head) cl])
                                        | cl<-eqCl locnm (map origin rls)] ++
                                       [ Str " worden regels gedefinieerd, waarvan de bestaansreden niet wordt uitgelegd. " ]
                            ++
                            case rs'>-rs of
                              []  -> []
                              [r] -> [ Str "De betekenis van regel ", Emph [Str (name r)]
                                     , Str (" op regelnummer "++ln r++" van bestand "++fn r)
                                     , Str " wordt uitgelegd in taal die door de computer is gegenereerd. "
                                     ]
                              rls -> (upC . commaNLPandoc (Str "en")  )
                                        [let nrs = [(Str . show . linenr) l | l<-cl] in
                                         strconcat ([Str ("op regelnummer"++(if length nrs>1 then "s" else "")++" ")]++
                                                    commaNLPandoc (Str "en") nrs++
                                                    [Str " van bestand "]++[(Str . locnm . head) cl])
                                        | cl<-eqCl locnm (map origin rls)] ++
                                       [ Str " staan regels, waarvan de betekenis wordt uitgelegd in taal die door de computer is gegenereerd. " ]
                            ++
                            case rs `isc` rs' of
                              []  -> []
                              [r] -> [ Str "Regel ", Emph [Str (name r)]
                                     , Str (" op regelnummer "++ln r++" van bestand "++fn r++" wordt niet uitgelegd. ")
                                     ]
                              rls -> (upC . commaNLPandoc (Str "en")  )
                                        [let nrs = [(Str . show . linenr) l | l<-cl] in
                                         strconcat ([Str ("op regelnummer"++(if length nrs>1 then "s" else "")++" ")]++
                                                    commaNLPandoc (Str "en") nrs++
                                                    [Str " van bestand "]++[(Str . locnm . head) cl])
                                        | cl<-eqCl locnm (map origin rls)] ++
                                       [ Str " worden regels gedefinieerd, zonder verdere uitleg. " ]
                           )
                          ]
      (English,[],[])  -> [ Para [Str "All rules in this document have been provided with a meaning and a purpose."]
                          | (length.udefrules) fSpec>1]
      (English,rs,rs') -> [Para $
                           ( case rs>-rs' of
                              []  -> []
                              [r] -> [ Str "The purpose of rule ", Emph [Str (name r)]
                                     , Str (" on line "++ln r++" of file "++fn r)
                                     , Str " is not documented. "
                                     ]
                              rls -> (upC . commaEngPandoc (Str "and") )
                                        [let nrs = [(Str . show . linenr) l | l<-cl] in
                                         strconcat ([ Str ("on line number"++(if length nrs>1 then "s" else "")++" ")]++
                                                    commaEngPandoc (Str "and") nrs ++
                                                    [Str " of file "]++[(Str . locnm . head) cl])
                                        | cl<-eqCl locnm (map origin rls)] ++
                                       [ Str " rules are defined without documenting their purpose. " ]
                           ) ++
                           ( case rs'>-rs of
                              []  -> []
                              [r] -> [ Str "The meaning of rule ", Emph [Str (name r)]
                                     , Str (" on line "++ln r++" of file "++fn r)
                                     , Str " is documented by means of computer generated language. "
                                     ]
                              rls -> (upC . commaEngPandoc (Str "and") )
                                        [let nrs = [(Str . show . linenr) l | l<-cl] in
                                         strconcat ([ Str ("on line number"++(if length nrs>1 then "s" else "")++" ")]++
                                                    commaEngPandoc (Str "and") nrs ++
                                                    [Str " of file "]++[(Str . locnm . head) cl])
                                        | cl<-eqCl locnm (map origin rls)] ++
                                       [ Str " rules are defined, the meaning of which is documented by means of computer generated language. " ]
                           ) ++
                           ( case rs `isc` rs' of
                              []  -> []
                              [r] -> [ Str "Rule ", Emph [Str (name r)]
                                     , Str (" on line "++ln r++" of file "++fn r++" is not documented. ")
                                     ]
                              rls -> (upC . commaEngPandoc (Str "and") )
                                        [let nrs = [(Str . show . linenr) l | l<-cl] in
                                         strconcat ([ Str ("on line number"++(if length nrs>1 then "s" else "")++" ")]++
                                                    commaEngPandoc (Str "and") nrs ++
                                                    [Str " of file "]++[(Str . locnm . head) cl])
                                        | cl<-eqCl locnm (map origin rls)] ++
                                       [ Str " rules are defined without any explanation. " ]
                           )
                          ]
     where missingPurp
            = nub [ r
                  | r<-ruls
                  , null (purposesDefinedIn fSpec (language flags) r)
                  ]
           missingMeaning
            = nub [ r
                  | r<-ruls
                  , null [m | m <- ameaMrk (rrmean r), amLang m == language flags]
                  ]
           ruls = if null (themes fSpec)
                  then udefrules fSpec
                  else concat [udefrules pat | pat<-patterns fSpec, name pat `elem` themes fSpec]++
                       concat [udefrules (fpProc prc) | prc<-vprocesses fSpec, name prc `elem` themes fSpec]
           upC (Str str:strs) = Str (upCap str):strs
           upC str = str
           fn r = locnm (origin r)
           ln r = locln (origin r)
           strconcat :: [Inline] -> Inline
           strconcat strs = (Str . concat) [ str | Str str<-strs]

  ruleRelationRefTable =
    [ Para [ Str descriptionStr ]
    , Table [] (AlignLeft : replicate 6 AlignCenter) [0.0,0.0,0.0,0.0,0.0,0.0,0.0] 
            (map strCell [ themeStr, relationsStr, withRefStr, "%", rulesStr, withRefStr, "%"])
            (map mkTableRowPat (vpatterns fSpec) ++ map mkTableRowProc (vprocesses fSpec) ++ 
            [[]] ++ -- empty row
            [mkTableRow contextStr (filter decusr $ vrels fSpec) (vrules fSpec)])
    ]
    where mkTableRowPat p = mkTableRow (name p) (ptdcs p) (ptrls p)
          mkTableRowProc (FProc p _) = mkTableRow (name p) (prcDcls p) (prcRules p) 
          mkTableRow nm decls ruls = 
            let nrOfRels = length decls
                nrOfRefRels = length $ filter hasRef decls
                nrOfRules = length ruls
                nrOfRefRules = length $ filter hasRef ruls
            in  map strCell [ nm
                            , show nrOfRels, show nrOfRefRels, showPercentage nrOfRels nrOfRefRels
                            , show nrOfRules, show nrOfRefRules, showPercentage nrOfRules nrOfRefRules 
                            ]
        
          hasRef x = maybe False (any  ((/="").explRefId)) (purposeOf fSpec (language flags) x)
          
          showPercentage x y = if x == 0 then "-" else show (y*100 `div` x)++"%" 
          
          strCell str = [Plain [Str str]]
          
          (descriptionStr, themeStr, relationsStr, withRefStr, rulesStr, contextStr) = 
            case (language flags) of Dutch -> ( "Onderstaande tabel bevat per thema (dwz. proces of patroon) tellingen van het aantal relaties en regels, " ++
                                                "gevolgd door het aantal en het percentage daarvan dat een referentie bevat. Relaties die in meerdere thema's " ++
                                                "gedeclareerd worden, worden ook meerdere keren geteld."
                                              , "Thema", "Relaties",  "Met referentie", "Regels", "Gehele context")
                                     _     -> ( "The table below shows for each theme (i.e. process or pattern) the number of relations and rules, followed " ++
                                                " by the number and percentage that have a reference. Relations declared in multiple themes are counted multiple " ++
                                                " times."
                                              , "Theme", "Relations", "With reference", "Rules", "Entire context")

  locnm (FileLoc(FilePos(filename,_,_))) = filename
  locnm (DBLoc str) = str
  locnm _ = "NO FILENAME"
  locln (FileLoc(FilePos(_,Pos l _,_))) = show l
  locln (DBLoc str) = str
  locln p = fatal 875 ("funny position "++show p++" in function 'locln'")

-- TODO: give richer feedback...
  invariantsInProcesses :: [Block]
  invariantsInProcesses
   = (case (language flags, prs, procs) of
      (_,      [],[] )  -> []
      (Dutch,  [],[p])  -> [ Para [ Str $ "Alle regels in proces "++name p++" zijn gekoppeld aan rollen." ]]
      (English,[],[p])  -> [ Para [ Str $ "All rules in process "++name p++" are linked to roles." ]]
      (Dutch,  [], _ )  -> [ Para [ Str "Alle regels in alle processen zijn gekoppeld aan rollen." ]]
      (English,[], _ )  -> [ Para [ Str "All rules in all processes are linked to roles." ]]
      (Dutch,  _ , _ )  -> [ Para [ Str "De volgende tabel toont welke regels in welke processen niet aan een rol gekoppeld zijn. "
                                  , Str "Dit heeft als consequentie dat de computer de betreffende regel(s) zal handhaven."
                                  ]]
      (English,_ , _ )  -> [ Para [ Str "The following table shows which rules are not linked to a role within a particular process. "
                                  , Str "This has as consequence that these rule(s) will be maintained by the computer."
                                  ]]
     )++
-- the table containing the role-rule assignments
     [ Table [] [AlignLeft,AlignLeft] [0.0,0.0]
       ( case language flags of
          Dutch   -> [ [Plain [Str "proces" ]] , [Plain [Str "regel"]] ]
          English -> [ [Plain [Str "process"]] , [Plain [Str "rule" ]] ]
       )
       [ [[Plain [Str (name p)]], [Plain (intercalate [Str ", "] [[Str (name r)] | r<-rs])]]
       | (p,rs)<-prs
       ]
     | not (null prs)]
     where prs = [(fp,rs) | fp<-procs
                          , let rs=invariants (fpProc fp), not (null rs) ]
           procs = if null (themes fSpec) then vprocesses fSpec else [prc | prc<-vprocesses fSpec, name prc `elem` themes fSpec ]

  processrulesInPatterns :: [Block]
  processrulesInPatterns
   = [ case (language flags, procs,prs) of
        (Dutch,  [p],[])  -> Para [ Str "Alle rol-regel-koppelingen gaan over regels die binnen proces ", Quoted SingleQuote [Str (name p)], Str " gedefinieerd zijn. " ]
        (English,[p],[])  -> Para [ Str "All role-rule assigments involve rules that are defined in process ", Quoted SingleQuote [Str (name p)], Str ". " ]
        (Dutch,  _,[])    -> Para [ Str "Voor elk proces geldt dat alle rol-regel-koppelingen gaan over regels die binnen dat proces zijn gedefinieerd." ]
        (English,_,[])    -> Para [ Str "The role-rule assignments in any of the described processes have been assigned to rules within that same process." ]
        (Dutch,  _,[(p,rol,rul)])
                          -> Para [ Str "Er is één koppeling tussen een rol en een regel van buiten het proces: "
                                  , Str "Rol ", Quoted SingleQuote [Str rol], Str " uit proces ", Quoted SingleQuote [Str (name p)], Str " is gebonden aan regel ", Quoted SingleQuote [Str (name rul)], Str " uit ", Quoted SingleQuote [Str (r_env rul)], Str "."
                                  ]
        (English,_,[(p,rol,rul)])
                          -> Para [ Str "There is one role that is assigned to a rule outside the process: "
                                  , Str "Role ", Quoted SingleQuote [Str rol], Str ", defined in process ", Quoted SingleQuote [Str (name p)], Str ", is assigned to rule ", Quoted SingleQuote [Str (name rul)], Str " from ", Quoted SingleQuote [Str (r_env rul)], Str "."
                                  ]
        (Dutch,  [p],_)   -> Para [ Str "De volgende tabel toont welke regels in welke patterns aan een rol gekoppeld zijn. "
                                  , Str "Dit heeft als consequentie dat de computer de betreffende regel(s) in proces ", Quoted SingleQuote [Str (name p)], Str " zal handhaven. "
                                  ]
        (English,[p],_)   -> Para [ Str "The following table shows which rules from outside the process are linked to a role in the process. "
                                  , Str "This has as consequence that these rule(s) will be maintained in the corresponding process ", Quoted SingleQuote [Str (name p)], Str ". "
                                  ]
        (Dutch,  _,_)     -> Para [ Str "Er zijn koppelingen tussen rollen en regels, die buiten de grenzen van het proces reiken. "
                                  , Str "De volgende tabel toont welke regels in welke patterns aan een rol gekoppeld zijn. "
                                  , Str "Dit heeft als consequentie dat de computer de betreffende regel(s) in de bijbehorende processen zal handhaven."
                                  ]
        (English,_,_)     -> Para [ Str "There are roles assigned to rules outside the bounds of the process. "
                                  , Str "The following table shows which rules that are defined in a pattern are linked to a role within a process."
                                  , Str "This has as consequence that these rule(s) will be maintained in the corresponding process(es)."
                                  ]
     | (not.null.vprocesses) fSpec && (not.null) [rra | prc<-procs, rra<-maintains prc]
     ]        ++          
-- the table containing the role-rule assignments
     [ Table []
       ([AlignLeft]++[AlignLeft | multProcs]++[AlignLeft,AlignLeft])
       ([0.0]++[0.0 | multProcs]++[0.0,0.0])
       ( case language flags of
          Dutch   ->
              [[Plain [Str "rol"]] ]++[[Plain [Str "in proces" ]] | multProcs]++[[Plain [Str "regel"]], [Plain [Str "uit"  ]] ]
          English ->
              [[Plain [Str "role"]]]++[[Plain [Str "in process"]] | multProcs]++[[Plain [Str "rule" ]], [Plain [Str "from" ]] ]
       )
       [ [[Plain [Str rol]]]++[[Plain [Str (name p)]] | multProcs]++[[Plain [Str (name rul)]], [Plain [Str (r_env rul)]]]
       | (p,rol,rul)<-prs
       ] 
     | length prs>1]
     where prs = [(p,rol,rul) | p<-procs, (rol,rul)<-maintains p, name rul `notElem` map name (udefrules p) ]
           multProcs = length procs>1
           procs = [fpProc fp | fp<-vprocesses fSpec
                            , null (themes fSpec) || name fp `elem` themes fSpec]  -- restrict if this is partial documentation.

--  populationReport :: [Block]
--  populationReport
--   = [ Para (case (language flags, ps, declarations fSpec) of
--        (Dutch,  [], [] ) -> [ Str "Dit script is leeg. " ]
--        (English,[], [] ) -> [ Str "This script is empty. " ]
--        (Dutch,  [],  _ ) -> [ Str "Geen relatie bevat enige populatie. " ]
--        (English,[],  _ ) -> [ Str "No relation contains any population. " ]
--        (Dutch,  [p],[_]) -> [ Str "Relatie ", Math InlineMath ((showMath.popdcl) p), Str " heeft een populatie van ", Str (count flags (length (popps p)) "paar"), Str ". " ]  -- Every d is typeable, so showMathDamb may be used.
--        (English,[p],[_]) -> [ Str "Relation ", Math InlineMath ((showMath.popdcl) p), Str " has ", Str (count flags (length (popps p)) "pair"), Str " in its population. " ]
--        (Dutch,  [p], _ ) -> [ Str "Alleen relatie ", Math InlineMath ((showMath.popdcl) p), Str " heeft een populatie. Deze bevat ", Str (count flags (length (popps p)) "paar"), Str ". " ]
--        (English,[p], _ ) -> [ Str "Only relation ", Math InlineMath ((showMath.popdcl) p), Str " is populated. It contains ", Str (count flags (length (popps p)) "pair"), Str ". " ]
--        (Dutch,   _ , _ ) -> [ Str "De onderstaande tabel geeft de populatie van de verschillende relaties weer. " ]
--        (English, _ , _ ) -> [ Str "The following table represents the population of various relations. " ])
--     ] ++
--     [ Table []
--        [AlignLeft,AlignRight]
--        [0.0,0.0]
--        (case language flags of
--          Dutch   -> [[Plain [Str "Concept"]], [Plain [Str "Populatie"]  ]]
--          English -> [[Plain [Str "Concept"]], [Plain [Str "Population"] ]]
--        )
--        [ [[Plain [Str (name c)]], [Plain [(Str . show . length . atomsOf) c]]]
--        | c<-cs
--        ]
--     | length cs>=1 ] ++
--     [ Table []
--        [AlignLeft,AlignRight]
--        [0.0,0.0]
--        (case language flags of
--          Dutch   -> [[Plain [Str "Relatie"]],  [Plain [Str "Populatie"]  ]]
--          English -> [[Plain [Str "Relation"]], [Plain [Str "Population"] ]]
--        )
--        [ [[Plain [Math InlineMath ((showMath .popdcl) p)]], [Plain [(Str . show . length . popps) p]]]  -- Every d is typeable, so showMathDamb may be used.
--        | p<-ps
--        ]
--     | length ps>1 ]
--     where
--      ps  = [p | p<-userDefPops fSpec
--               , null (themes fSpec) || (decpat.popdcl) p `elem` themes fSpec  -- restrict if the documentation is partial.
--               , (not.null.popps) p]
--      cs  = [c | c@C{}<-ccs, (not.null.atomsOf) c]
--      ccs = concs [ d | d<-declarations fSpec, null (themes fSpec)||decpat d `elem` themes fSpec]  -- restrict if the documentation is partial.

  wipReport :: [Block]
  wipReport
   = [ Para (case (language flags, concat popwork,popwork) of
              (Dutch,  [],_)       -> [ Str "De populatie in dit script beschrijft geen onderhanden werk. "
                                      | (not.null.userDefPops) fSpec ]
              (English,[],_)       -> [ Str "The population in this script does not specify any work in progress. "
                                      | (not.null.userDefPops) fSpec ]
              (Dutch,  [(r,ps)],_) -> [ Str "Regel ", quoterule r, Str (" laat "++count flags (length ps) "taak"++" zien.") ]
              (English,[(r,ps)],_) -> [ Str "Rule ", quoterule r, Str (" shows "++count flags (length ps) "task"++".") ]
              (Dutch,  _,[_])      -> [ Str "Dit script bevat onderhanden werk. De volgende tabel bevat details met regelnummers in het oorspronkelijk script-bestand." ]
              (English,_,[_])      -> [ Str "This script contains work in progress. The following table provides details with line numbers from the original script file." ]
              (Dutch,  _,_)        -> [ Str "Dit script bevat onderhanden werk. De volgende tabellen geven details met regelnummers in de oorspronkelijk script-bestanden." ]
              (English,_,_)        -> [ Str "This script contains work in progress. The following tables provide details with line numbers from the original script files." ]
            )
     ]        ++          
-- the following table actually belongs to the intro
     [ Table []
       [AlignLeft,AlignRight,AlignRight]
       [0.0,0.0,0.0]
       ( case language flags of
          Dutch   ->
              [[Plain [Str "regel"]], [Plain $[Str ((locnm . origin . fst . head) cl++" ") |length popwork>1]++[Str "script",LineBreak,Str "regel#"]], [Plain [Str "#signalen"] ]]
          English ->
              [[Plain [Str "rule" ]], [Plain $[Str ((locnm . origin . fst . head) cl++" ") |length popwork>1]++[Str "line#"]], [Plain [Str "#signals"] ]]
       )
       [ [[Plain [Str (name r)]], [Plain [(Str . locln . origin) r]], [Plain [(Str . show . length) ps]]]
       | (r,ps)<-cl, length ps>0
       ]
     | (length.concat) popwork>1, cl<-popwork ]        ++          
-- the tables containing the actual work in progress population
     concat
     [ [ Para ( (case language flags of
                  Dutch   -> Str "Regel"
                  English -> Str "Rule"):
                [Space,quoterule r,Space]++
                if xrefSupported flags then [ Str "(", RawInline "latex" $ symReqRef r, Str ") "] else []++
                (case language flags of
                  Dutch   -> [ Str "luidt: " ]
                  English -> [ Str "says: "  ]
                )  
              )]  ++meaning2Blocks (language flags) r++
       [Plain ( case language flags of
                  Dutch  ->
                     [ Str "Deze regel bevat nog werk (voor "]++
                     commaNLPandoc (Str "of") (nub [Str rol | (rol, rul)<-fRoleRuls fSpec, r==rul])++[Str ")"]++
                     (if length ps == 1 then [Str ", te weten "]++oneviol r ps++[Str ". "] else
                      [ Str (". De volgende tabel laat de "++(if length ps>10 then "eerste tien " else "")++"items zien die aandacht vragen.")]
                     )
                  English ->
                     [ Str "This rule contains work"]++
                     commaEngPandoc (Str "or") (nub [Str rol | (rol, rul)<-fRoleRuls fSpec, r==rul])++[Str ")"]++
                     if length ps == 1 then [Str " by "]++oneviol r ps++[Str ". "] else
                      [ Str ("The following table shows the "++(if length ps>10 then "first ten " else "")++"items that require attention.")]
                     
              ) ]++
       [ violtable r ps | length ps>1]
     | (r,ps)<-concat popwork ]
     where
--      text r
--       = if null expls
--         then explains2Blocks (autoMeaning (language flags) r) 
--         else expls 
--         where expls = [Plain (block++[Space]) | Means l econt<-rrxpl r, l==Just (language flags) || l==Nothing, Para block<-econt]
      quoterule r
       = if name r==""
         then case language flags of
               English -> Str ("on "++show (origin r))
               Dutch   -> Str ("op "++show (origin r))
         else Quoted SingleQuote [Str (name r)]
      oneviol r [(a,b)]
       = if source r==target r && a==b
         then [Quoted  SingleQuote [Str (name (source r)),Space,Str a]]
         else [Str "(",Str (name (source r)),Space,Str a,Str ", ",Str (name (target r)),Space,Str b,Str ")"]
      oneviol _ _ = fatal 810 "oneviol must have a singleton list as argument."
      popwork :: [[(Rule,[(String, String)])]];
      popwork = eqCl (locnm.origin.fst) [(r,ps) | (r,ps) <- allViolations fSpec, isSignal r, partofThemes r]
  partofThemes r = 
        or [ null (themes fSpec) 
           , r `elem` concat [udefrules pat | pat<-patterns fSpec, name pat `elem` themes fSpec]
           , r `elem` concat [udefrules (fpProc prc) | prc<-vprocesses fSpec, name prc `elem` themes fSpec]
           ]

  violationReport :: Blocks    
  violationReport
   = let (processViolations,invariantViolations) = partition (isSignal.fst) (allViolations fSpec)
         showViolatedRule :: (Rule,Pairs) -> Blocks
         showViolatedRule (r,ps) 
             = let capt = case language flags of
                               Dutch   -> text "Overtredingen van regel "
                               English -> text "Violations of rule "
                         <>  text (name r)
                   showRow :: Paire -> [Blocks]
                   showRow p = [(para.text.fst) p,(para.text.snd) p]
               in para ( case language flags of
                            Dutch   -> text "Regel "
                            English -> text "Rule "
                         <>  text (name r)
                       )
               <> para (text ("Totaal aantal overtredingen: "++show (length ps))
                       )
               <> table capt 
                   [(AlignLeft,0)                          ,(AlignLeft,0)          ]
                   [(para.strong.text.name.source.rrexp) r,(para.strong.text.name.target.rrexp) r]
                   (map showRow ps)
            
     in (para (case (language flags, invariantViolations, processViolations) of
                (Dutch  ,[] , [] ) -> text "De populatie in dit script overtreedt geen regels. "
                (English,[] , [] ) -> text "The population in this script violates no rule. "
                (Dutch  ,iVs, pVs) 
                   -> text ("De populatie in dit script overtreedt "
                             ++show(length iVs)++" invariant"++(if length iVs == 1 then "" else "en")++" en "
                             ++show(length pVs)++" procesregel"++if length pVs == 1 then "" else "s"++"."
                           )
                (English,iVs, pVs) 
                   -> text ("The population in this script violates "
                             ++show(length iVs)++" invariant"++(if length iVs == 1 then "" else "s")++" and "
                             ++show(length pVs)++" process rule"++if length pVs == 1 then "" else "s"++"."
                           )
              )
        )
     <> bulletList  [showViolatedRule vs | vs<- invariantViolations]
     <> bulletList  [showViolatedRule vs | vs<- processViolations]
        
             
---- the table containing the rule violation counts
--     [ Table []
--       [AlignLeft,AlignRight,AlignRight]
--       [0.0,0.0,0.0]
--       ( case language flags of
--          Dutch   ->
--             [[Plain [Str "regel"]], [Plain $[Str ((locnm . origin . fst . head) cl++" ") |length popviol>1]++[Str "regel#"]], [Plain [Str "#overtredingen"] ]]
--          English ->
--             [[Plain [Str "rule" ]], [Plain $[Str ((locnm . origin . fst . head) cl++" ") |length popviol>1]++[Str "line#"]], [Plain [Str "#violations"] ]]
--       )
--       [ [[Plain [Str (name r)]], [Plain [(Str . locln . origin) r]], [Plain [(Str . show . length) ps]]]
--       | (r,ps)<-cl, length ps>0
--       ]
--     | (length.concat) popviol>1, cl<-popviol, not (null cl) ]        ++          
---- the table containing the multiplicity counts
--     [ Table []
--       [AlignLeft,AlignRight,AlignRight]
--       [0.0,0.0,0.0]
--       ( case language flags of
--           Dutch   ->
--              [[Plain [Str "regel"]], [Plain $[Str ((locnm . origin . fst . head) cl++" ") |length multviol>1]++[Str "regel#"]], [Plain [Str "#overtredingen"] ]]
--           English ->
--              [[Plain [Str "rule" ]], [Plain $[Str ((locnm . origin . fst . head) cl++" ") |length multviol>1]++[Str "line#"]], [Plain [Str "#violations"] ]]
--       )
--       [ [[Plain [Str (name r)]], [Plain [(Str . locln . origin) r]], [Plain [(Str . show . length) ps]]]
--       | (r,ps)<-cl, length ps>0
--       ]
--     | (length.concat) multviol>1, cl<-multviol, not (null cl) ]        ++          
-- the tables containing the actual violations of user defined rules
--     concat
--     [ [ Para ( (case language flags of
--                   Dutch   -> Str "Regel"
--                   English -> Str "Rule"):
--                [Space,quoterule r,Space]++
--                if fspecFormat flags==FLatex then [ Str "(", RawInline "latex" $ symReqRef r, Str ") "] else []++
--                (case language flags of
--                    Dutch   -> [ Str "luidt: " ]
--                    English -> [ Str "says: "])
--              )]  ++meaning2Blocks (language flags) r++
--       [Plain ( case language flags of
--                  Dutch   ->
--                     Str "Deze regel wordt overtreden":
--                     (if length ps == 1 then [Str " door "]++oneviol r ps++[Str ". "] else
--                      [ Str (". De volgende tabel laat de "++if length ps>10 then "eerste tien " else ""++"overtredingen zien.")]
--                     )
--                  English ->
--                     Str "This rule is violated":
--                     (if length ps == 1 then [Str " by "]++oneviol r ps++[Str ". "] else
--                      [ Str ("The following table shows the "++if length ps>10 then "first ten " else ""++"violations.")]
--                     )
--              )]++
--       [ violtable r ps | length ps>1]
--     | (r,ps)<-popviols, length popviols>1 ]++
---- the tables containing the actual violations of multiplicity rules
--     [ BulletList
--       [ textMult r++
--         [Plain ( case language flags of
--                   Dutch   ->
--                     if length ps == 1 then [Str "Deze regel wordt overtreden door "]++oneviol r ps++[Str ". "] else
--                      [ Str ("De volgende tabel laat de "++(if length ps>10 then "eerste tien overtredingen zien." else count flags (length ps) ((unCap.name.source)r)++" zien die deze regel overtreden."))]
--                     
--                   English ->
--                     if length ps == 1 then [Str "This rule is violated by "]++oneviol r ps++[Str ". "] else
--                      [ Str ("The following table shows the "++(if length ps>10 then "first ten violations." else count flags (length ps) ((unCap.name.source)r)++" that violate this rule."))]
--                     
--                )]++
--         [ violtable r ps | length ps>1]
--       | (r,ps)<-multviols, length multviols>1 ]
--     | not (null multviols) ]
--     where
--     textMult r
--       = concat [    [Plain [Str "De relatie ",Space]]
--                  ++ amPandoc mrkup
--                  ++ [Plain [Str ".",Space]]
--                  
--                 | mrkup <- (ameaMrk . rrmean) r, amLang mrkup==language flags]
--     quoterule r = if name r==""
--                   then Str ("on "++show (origin r))
--                   else Quoted SingleQuote [Str (name r)]
--     oneviol r [(a,b)]
--      = if source r==target r && a==b
--        then [Quoted  SingleQuote [Str (name (source r)),Space,Str a]]
--        else [Str "(",Str (name (source r)),Space,Str a,Str ", ",Str (name (target r)),Space,Str b,Str ")"]
--     oneviol _ _ = fatal 810 "oneviol must have a singleton list as argument."
--     popviols  = [(r,ps) | (r,ps) <- allViolations fSpec, partofThemes r,      r_usr r == UserDefined ]
--     multviols = [(r,ps) | (r,ps) <- allViolations fSpec, partofThemes r, not (r_usr r == UserDefined)]
     

--     popviols = [(r,ps) | r<-invs++identityRs
--                        , let ps=ruleviolations r, not (null ps)]
--     multviols = [(r,ps) | r<-mults
--                         , let ps=ruleviolations r, not (null ps)]
--     popviol :: [[(Rule,[(String, String)])]]
--     popviol  = eqCl (locnm.origin.fst) [(r,ps) | r<-invs, let ps=ruleviolations r, not (null ps)]
--     multviol :: [[(Rule,[(String, String)])]]
--     multviol  = eqCl (locnm.origin.fst) [(r,ps) | r<-mults, let ps=ruleviolations r, not (null ps)]
--     invs  = if null (themes fSpec)
--             then invariants fSpec
--             else concat [invariants pat | pat<-patterns fSpec, name pat `elem` themes fSpec]++
--                  concat [invariants (fpProc prc) | prc<-vprocesses fSpec, name prc `elem` themes fSpec]
--     mults = if null (themes fSpec)
--             then multrules fSpec
--             else concat [multrules pat | pat<-patterns fSpec, name pat `elem` themes fSpec]++
--                  concat [multrules (fpProc prc) | prc<-vprocesses fSpec, name prc `elem` themes fSpec]
--     identityRs = if null (themes fSpec)
--                  then identityRules fSpec
--                  else concat [identityRules pat | pat<-patterns fSpec, name pat `elem` themes fSpec]++
--                       concat [identityRules (fpProc prc) | prc<-vprocesses fSpec, name prc `elem` themes fSpec]

  violtable r ps
      = if hasantecedent r && isIdent (antecedent r)  -- note: treat 'isIdent (consequent r) as binary table.
        then Table []
             [AlignLeft]
             [0.0]
             [[Plain [(Str . name . source) r]]]
             [ [[Plain [Str a]]]
             | (a,_)<-take 10 ps
             ]
        else Table []
             [AlignLeft,AlignLeft]
             [0.0,0.0]
             [[Plain [(Str . name . source) r]], [Plain [(Str . name . target) r] ]]
             [ [[Plain [Str a]], [Plain [Str b]]]
             | (a,b)<-take 10 ps
             ]
        


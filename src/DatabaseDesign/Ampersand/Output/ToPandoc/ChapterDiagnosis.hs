{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.ChapterDiagnosis 
where
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
import DatabaseDesign.Ampersand.Basics  
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Classes
import Data.List
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Fspec.Fspec
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Output.AdlExplanation (purpose,Explainable(..))
import DatabaseDesign.Ampersand.Output.PandocAux

fatal :: Int -> String -> a
fatal = fatalMsg "ChapterDiagnosis.hs"

chpDiagnosis :: Int -> Fspc -> Options -> ([Block],[Picture])
chpDiagnosis lev fSpec flags
 = ( header ++                -- the chapter header
     diagIntro ++             -- an introductory text
     roleomissions ++         -- says which role-rule, role-interface, and role-relation assignments are missing
     roleRuleTable ++         -- gives an overview of rule-rule assignments
     missingConceptDefs ++    -- says which concept definitions are missing
     missingRels ++           -- says which relation declarations are missing
     relsNotUsed ++           -- says which relations are not used in any rule
     missingRules ++          -- says which rule definitions are missing
     invariantsInProcesses ++ -- 
     processrulesInPatterns++ -- 
     populationReport++       -- says which relations are populated.
     wipReport++              -- sums up the work items (i.e. the violations of process rules)
     violationReport          -- sums up the violations caused by the population of this script.
   , pics )
  where
  header :: [Block]
  header = labeledHeader lev (xLabel Diagnosis)
                                         (case language flags of
                                             Dutch   ->  "Diagnose"   
                                             English ->  "Diagnosis"
                                         )
  diagIntro :: [Block]
  diagIntro = 
    case language flags of
      Dutch   -> [Para
                  [ Str "Dit hoofdstuk geeft een analysis van het Ampersand-script van ", Quoted  SingleQuote [Str (name fSpec)], Str ". "
                  , Str "Deze analysis is bedoeld voor de auteurs van dit script. "
                  , Str "Op basis hiervan kunnen zij het script completeren en mogelijke tekortkomingen verbeteren. "
                  ]]
      English -> [Para
                  [ Str "This chapter provides an analysis of the Ampersand script of ", Quoted  SingleQuote [Str (name fSpec)], Str ". "
                  , Str "This analysis is intended for the authors of this script. "
                  , Str "It can be used to complete the script or to improve possible flaws. "
                  ]]
   

  roleRuleTable :: [Block]
  roleRuleTable 
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
      ruls = [r | r<-vrules fSpec, isSignal r ]
      f r rul | (r,rul) `elem` maintained      = [Plain [Math InlineMath "\\surd"]]
              | (r,rul) `elem` dead            = [Plain [Math InlineMath "\\times"]]
              | (r,rul) `elem` fRoleRuls fSpec = [Plain [Math InlineMath "\\odot"]]
              | otherwise                      = []
      maintained  -- (r,rul) `elem` maintained means that r can maintain rul without restrictions.
       = [ (r,rul)
         | (r,rul)<-fRoleRuls fSpec
         , and [(r,rel) `elem` fRoleRels fSpec | rel<-mors rul]
         ]
      dead -- (r,rul) `elem` maintained means that r cannot maintain rul without restrictions.
       = [ (r,rul)
         | (r,rul)<-fRoleRuls fSpec
         , (not.or) [(r,rel) `elem` fRoleRels fSpec | rel<-mors rul]
         ]

  roleomissions :: [Block]
  roleomissions
   = [ case language flags of
         Dutch   ->
           Plain [ Str $ upCap (name fSpec)++" kent geen regels aan rollen toe. "
                  , Str "Een generieke rol, User, zal worden gedefinieerd om al het werk te doen wat in het bedrijfsproces moet worden uitgevoerd."
                  ]
         English ->
           Plain [ Str $ upCap (name fSpec)++" does not assign rules to roles. "
                  , Str "A generic role, User, will be defined to do all the work that is necessary in the business process."
                  ]
     | (null.fRoleRuls) fSpec && (not.null.rules) fSpec] ++
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
   where missing = [c | c <-concs (declarations fSpec)
                     , cd <- vConceptDefs fSpec
                     , name c == name cd
                     , null (purpose fSpec (language flags) cd)
                   ]++
                   [c | c <-concs fSpec
                     , null [cd | cd <- vConceptDefs fSpec, name c == name cd]
                   ]

  missingRels :: [Block]
  missingRels
   = case (language flags, missing) of
      (Dutch,[])  -> [Para 
                       [Str "Alle relaties in dit document zijn voorzien van een reden van bestaan (purpose)."]
                     | (not.null.mors.rules) fSpec]
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
                       | (not.null.mors.rules) fSpec]
      (English,[r]) -> [Para 
                         [ Str "The purpose of relation ", r
                         , Str " remains unexplained."
                       ] ]
      (English,rs)  -> [Para $
                         [ Str "The purpose of relations "]++commaEngPandoc (Str "and") rs++
                         [ Str " is not documented."
                       ] ]
     where missing = [(Math InlineMath . showMath . ERel) r  -- ERel is always typeable, so showMathDamb may be used.
                     | r@(Rel{}) <-mors fSpec, not (isIdent r)
                     , null (purpose fSpec (language flags) r)
                     ]

  relsNotUsed :: [Block]
  pics        :: [Picture]
  (relsNotUsed,pics)
   = ( ( case (language flags, notUsed) of
          (Dutch,[])  -> [Para 
                           [Str "Alle relaties in dit document worden in één of meer regels gebruikt."]
                         | (not.null.mors.rules) fSpec]
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
                           | (not.null.mors.rules) fSpec]
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
     where notUsed = nub [(Math InlineMath . showMath . ERel . makeRelation) d  -- makeRelation d is always typeable, so showMathDamb may be used.
                         | d@(Sgn{}) <- declarations fSpec, decusr d
                         , d `notElem` (nub . map makeDeclaration . mors . rules) fSpec
                         ]
           pats = [ pat | pat<-patterns fSpec, (not.null) (declarations pat>-map makeDeclaration (mors pat)) ]
           pictsWithUnusedRels = [makePicture flags fSpec Rel_CG pat | pat<-pats ]

  missingRules :: [Block]
  missingRules
   = case (language flags, missingPurp, missingExpl) of
      (Dutch,[],[])    -> [ Para [Str "Alle regels in dit document zijn voorzien van een uitleg."]
                          | (length.rules) fSpec>1]
      (Dutch,rs,rs')   -> [Para 
                           (case rs>-rs' of
                              []  -> []
                              [r] -> [ Str "De bestaansreden van regel ", Emph [Str (name r)]
                                     , Str (" op regelnummer "++ln r++" van bestand "++fn r)
                                     , Str " wordt niet uitgelegd. "
                                     ]
                              rls -> (upC . commaNLPandoc (Str "en")  )
                                        [let nrs = [(Str . show . lineNumber) l | l<-cl] in
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
                                        [let nrs = [(Str . show . lineNumber) l | l<-cl] in
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
                                        [let nrs = [(Str . show . lineNumber) l | l<-cl] in
                                         strconcat ([Str ("op regelnummer"++(if length nrs>1 then "s" else "")++" ")]++
                                                    commaNLPandoc (Str "en") nrs++
                                                    [Str " van bestand "]++[(Str . locnm . head) cl])
                                        | cl<-eqCl locnm (map origin rls)] ++
                                       [ Str " worden regels gedefinieerd, zonder verdere uitleg. " ]
                           )
                          ]
      (English,[],[])  -> [ Para [Str "All rules in this document have been provided with a meaning and a purpose."]
                          | (length.rules) fSpec>1]
      (English,rs,rs') -> [Para $
                           ( case rs>-rs' of
                              []  -> []
                              [r] -> [ Str "The purpose of rule ", Emph [Str (name r)]
                                     , Str (" on line "++ln r++" of file "++fn r)
                                     , Str " is not documented. "
                                     ]
                              rls -> (upC . commaEngPandoc (Str "and") )
                                        [let nrs = [(Str . show . lineNumber) l | l<-cl] in
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
                                        [let nrs = [(Str . show . lineNumber) l | l<-cl] in
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
                                        [let nrs = [(Str . show . lineNumber) l | l<-cl] in
                                         strconcat ([ Str ("on line number"++(if length nrs>1 then "s" else "")++" ")]++
                                                    commaEngPandoc (Str "and") nrs ++
                                                    [Str " of file "]++[(Str . locnm . head) cl])
                                        | cl<-eqCl locnm (map origin rls)] ++
                                       [ Str " rules are defined without any explanation. " ]
                           )
                          ]
     where missingPurp
            = nub [ r
                  | r<-rules fSpec
                  , null (purpose fSpec (language flags) r)
                  ]
           missingExpl
            = nub [ r
                  | r<-rules fSpec
                  , null [block | Means l econt<-rrxpl r, l==language flags, block<-econt]
                  ]
           upC (Str str:strs) = Str (upCap str):strs
           upC str = str
           fn r = locnm (origin r)
           ln r = locln (origin r)
           strconcat :: [Inline] -> Inline
           strconcat strs = (Str . concat) [ str | Str str<-strs]

  locnm (FileLoc(FilePos(filename,_,_))) = filename
  locnm (DBLoc str) = str
  locnm _ = "NO FILENAME"
  locln (FileLoc(FilePos(_,Pos l _,_))) = show l
  locln (DBLoc str) = str
  locln p = fatal 875 ("funny position "++show p++" in function 'locln'")

-- TODO: give richer feedback...
  invariantsInProcesses :: [Block]
  invariantsInProcesses
   = (case (language flags, prs, vprocesses fSpec) of
      (Dutch,  [],[] )  -> []
      (English,[],[] )  -> []
      (Dutch,  [],[p])  -> [ Para [ Str $ "Alle regels in proces "++name p++" zijn gekoppeld aan rollen." ]]
      (English,[],[p])  -> [ Para [ Str $ "All rules in process "++name p++" are linked to roles." ]]
      (Dutch,  [], _ )  -> [ Para [ Str "Alle regels in alle processen zijn gekoppeld aan rollen." ]]
      (English,[], _ )  -> [ Para [ Str "All rules in all processes are linked to roles." ]]
      (Dutch,  _ , _ )  -> [ Para [ Str "De volgende tabel toont welke regels in welke processen niet aan een rol gekoppeld zijn."
                                  , Str "Dit heeft als consequentie dat de computer de betreffende regel(s) zal handhaven."
                                  ]]
      (English,_ , _ )  -> [ Para [ Str "The following table shows which rules are not linked to a role within a particular process."
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
     where prs = [(fp,rs) | fp<-vprocesses fSpec, let rs=invariants (proc fp), not (null rs) ]

  processrulesInPatterns :: [Block]
  processrulesInPatterns
   = [ case (language flags, vprocesses fSpec,prs) of
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
        (English,_,_)     -> Para [ Str "There are roles assigne to rules outside the bounds of the process. "
                                  , Str "The following table shows which rules that are defined in a pattern are linked to a role within a process."
                                  , Str "This has as consequence that these rule(s) will be maintained in the corresponding process(es)."
                                  ]
     | (not.null.vprocesses) fSpec && (not.null) [rra | fp<-vprocesses fSpec, rra<-maintains (proc fp)]
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
     where prs = [(p,rol,rul) | p<-map proc (vprocesses fSpec), (rol,rul)<-maintains p, name rul `notElem` map name (rules p) ]
           multProcs = length (vprocesses fSpec)>1

  populationReport :: [Block]
  populationReport
   = [ Para (case (language flags, ds, declarations fSpec) of
        (Dutch,  [], [] ) -> [ Str "Dit script is leeg. " ]
        (English,[], [] ) -> [ Str "This script is empty. " ]
        (Dutch,  [],  _ ) -> [ Str "Dit script bevat geen populatie. " ]
        (English,[],  _ ) -> [ Str "This script contains no population. " ]
        (Dutch,  [d],[_]) -> [ Str "Relatie ", Math InlineMath (showMath d), Str " heeft een populatie van ", Str (count flags (length (decpopu d)) "paar"), Str ". " ]  -- Every d is typeable, so showMathDamb may be used.
        (English,[d],[_]) -> [ Str "Relation ", Math InlineMath (showMath d), Str " has ", Str (count flags (length (decpopu d)) "pair"), Str " in its population. " ]
        (Dutch,  [d], _ ) -> [ Str "Alleen relatie ", Math InlineMath (showMath d), Str " heeft een populatie. Deze bevat ", Str (count flags (length (decpopu d)) "paar"), Str ". " ]
        (English,[d], _ ) -> [ Str "Only relation ", Math InlineMath (showMath d), Str " is populated. It contains ", Str (count flags (length (decpopu d)) "pair"), Str ". " ]
        (Dutch,   _ , _ ) -> [ Str "De onderstaande tabel geeft de populatie van de verschillende relaties weer. " ]
        (English, _ , _ ) -> [ Str "The following table represents the population of various relations. " ])
     ] ++
     [ Table []
        [AlignLeft,AlignRight]
        [0.0,0.0]
        (case language flags of
          Dutch   -> [[Plain [Str "Concept"]], [Plain [Str "Populatie"]  ]]
          English -> [[Plain [Str "Concept"]], [Plain [Str "Population"] ]]
        )
        [ [[Plain [Str (name c)]], [Plain [(Str . show . length . cptos) c]]]
        | c<-cs
        ]
     | length cs>=1 ] ++
     [ Table []
        [AlignLeft,AlignRight]
        [0.0,0.0]
        (case language flags of
          Dutch   -> [[Plain [Str "Relatie"]],  [Plain [Str "Populatie"]  ]]
          English -> [[Plain [Str "Relation"]], [Plain [Str "Population"] ]]
        )
        [ [[Plain [Math InlineMath (showMath d)]], [Plain [(Str . show . length . decpopu) d]]]  -- Every d is typeable, so showMathDamb may be used.
        | d<-ds
        ]
     | length ds>1 ]
     where
      ds = [d | d<-declarations fSpec, (not.null.decpopu) d]
      cs = [c | c@C{}<-concs fSpec, (not.null.cptos) c]

  wipReport :: [Block]
  wipReport
   = [ Para (case (language flags, concat popwork,popwork) of
              (Dutch,  [],_)       -> [ Str "De populatie in dit script beschrijft geen onderhanden werk. "
                                      | (not.null) [d | d<-declarations fSpec, not (null (decpopu d))] ]
              (English,[],_)       -> [ Str "The population in this script does not specify any work in progress. "
                                      | (not.null) [d | d<-declarations fSpec, not (null (decpopu d))] ]
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
                if fspecFormat flags==FLatex then [ Str "(", RawInline "latex" $ symReqRef r, Str ") "] else []++
                (case language flags of
                  Dutch   -> [ Str "luidt: " ]
                  English -> [ Str "says: "  ]
                )  
              )]  ++text r++
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
      text r
       = if null expls
         then explains2Blocks (autoMeaning (language flags) r) 
         else expls 
         where expls = [Plain (block++[Space]) | Means l econt<-rrxpl r, l==language flags, Para block<-econt]
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
      popwork :: [[(Rule,[(String, String)])]]
      popwork  = eqCl (locnm.origin.fst) [(r,ps) | r<-[r | r<-vrules fSpec, isSignal r ], let ps=ruleviolations r, not (null ps)]

  violationReport :: [Block]
  violationReport
   = [ Para (case (language flags, popviols, multviols) of
        (Dutch,  [],[])      -> [ Str "De populatie in dit script overtreedt geen regels. "
                                | (not.null) [d | d<-declarations fSpec, not (null (decpopu d))] ]
        (English,[],[])      -> [ Str "The population in this script violates no rule. "
                                | (not.null) [d | d<-declarations fSpec, not (null (decpopu d))] ]
        (Dutch,  [], _:_:_ )  -> [ Str "De populatie in dit script overtreedt alleen multipliciteitsregels. "
                                | (not.null) [d | d<-declarations fSpec, not (null (decpopu d))] ]
        (English,[], _:_:_ ) -> [ Str "The population in this script violates multiplicity rules only. "
                                | (not.null) [d | d<-declarations fSpec, not (null (decpopu d))] ]
        (Dutch,  [(r,ps)],_) -> [ Str "Regel ", quoterule r, Str (" veroorzaakt "++count flags (length ps) "overtreding"++". ") ]
        (English,[(r,ps)],_) -> [ Str "Rule ", quoterule r, Str (" causes "++count flags (length ps) "violation"++". ") ]
        (Dutch,  _,_)        -> [ Str "De onderstaande tabellen geven overtredingen van regels weer. " ]
        (English,_,_)        -> [ Str "The following tables represent rule violations. " ])
     ]        ++          
-- the table containing the rule violation counts
     [ Table []
       [AlignLeft,AlignRight,AlignRight]
       [0.0,0.0,0.0]
       ( case language flags of
          Dutch   ->
             [[Plain [Str "regel"]], [Plain $[Str ((locnm . origin . fst . head) cl++" ") |length popviol>1]++[Str "regel#"]], [Plain [Str "#overtredingen"] ]]
          English ->
             [[Plain [Str "rule" ]], [Plain $[Str ((locnm . origin . fst . head) cl++" ") |length popviol>1]++[Str "line#"]], [Plain [Str "#violations"] ]]
       )
       [ [[Plain [Str (name r)]], [Plain [(Str . locln . origin) r]], [Plain [(Str . show . length) ps]]]
       | (r,ps)<-cl, length ps>0
       ]
     | (length.concat) popviol>1, cl<-popviol ]        ++          
-- the table containing the multiplicity counts
     [ Table []
       [AlignLeft,AlignRight,AlignRight]
       [0.0,0.0,0.0]
       ( case language flags of
           Dutch   ->
              [[Plain [Str "regel"]], [Plain $[Str ((locnm . origin . fst . head) cl++" ") |length multviol>1]++[Str "regel#"]], [Plain [Str "#overtredingen"] ]]
           English ->
              [[Plain [Str "rule" ]], [Plain $[Str ((locnm . origin . fst . head) cl++" ") |length multviol>1]++[Str "line#"]], [Plain [Str "#violations"] ]]
       )
       [ [[Plain [Str (name r)]], [Plain [(Str . locln . origin) r]], [Plain [(Str . show . length) ps]]]
       | (r,ps)<-cl, length ps>0
       ]
     | (length.concat) multviol>1, cl<-multviol ]        ++          
-- the tables containing the actual violations of user defined rules
     concat
     [ [ Para ( (case language flags of
                   Dutch   -> Str "Regel"
                   English -> Str "Rule"):
                [Space,quoterule r,Space]++
                if fspecFormat flags==FLatex then [ Str "(", RawInline "latex" $ symReqRef r, Str ") "] else []++
                (case language flags of
                    Dutch   -> [ Str "luidt: " ]
                    English -> [ Str "says: "])
              )]  ++text r++
       [Plain ( case language flags of
                  Dutch   ->
                     Str "Deze regel wordt overtreden":
                     (if length ps == 1 then [Str " door "]++oneviol r ps++[Str ". "] else
                      [ Str (". De volgende tabel laat de "++if length ps>10 then "eerste tien " else ""++"overtredingen zien.")]
                     )
                  English ->
                     Str "This rule is violated":
                     (if length ps == 1 then [Str " by "]++oneviol r ps++[Str ". "] else
                      [ Str ("The following table shows the "++if length ps>10 then "first ten " else ""++"violations.")]
                     )
              )]++
       [ violtable r ps | length ps>1]
     | (r,ps)<-popviols ]++
-- the tables containing the actual violations of multiplicity rules
     concat
     [ textMult r++
       [Plain ( case language flags of
                 Dutch   ->
                   if length ps == 1 then [Str "Deze regel wordt overtreden door "]++oneviol r ps++[Str ". "] else
                    [ Str ("De volgende tabel laat de "++(if length ps>10 then "eerste tien overtredingen zien." else count flags (length ps) ((unCap.name.source)r)++" zien die deze regel overtreden."))]
                   
                 English ->
                   if length ps == 1 then [Str "This rule is violated by "]++oneviol r ps++[Str ". "] else
                    [ Str ("The following table shows the "++(if length ps>10 then "first ten violations." else count flags (length ps) ((unCap.name.source)r)++" that violate this rule."))]
                   
              )]++
       [ violtable r ps | length ps>1]
     | (r,ps)<-multviols ]
     where
     text r = if null expls
              then explains2Blocks (autoMeaning (language flags) r) 
              else expls 
              where expls = [Plain (block++[Space]) | Means l econt<-rrxpl r, l==language flags, Para block<-econt]
     textMult r
            = if null expls
              then explains2Blocks (autoMeaning (language flags) r) 
              else expls 
              where expls = [Plain ([Str "De relatie ",Space]++block++[Str ".",Space]) | Means l econt<-rrxpl r, l==language flags, Para block<-econt]
     quoterule r = if name r==""
                   then Str ("on "++show (origin r))
                   else Quoted SingleQuote [Str (name r)]
     oneviol r [(a,b)]
      = if source r==target r && a==b
        then [Quoted  SingleQuote [Str (name (source r)),Space,Str a]]
        else [Str "(",Str (name (source r)),Space,Str a,Str ", ",Str (name (target r)),Space,Str b,Str ")"]
     oneviol _ _ = fatal 810 "oneviol must have a singleton list as argument."
     popviols = [(r,ps) | r<-invariants fSpec++keyrules fSpec
                        , let ps=ruleviolations r, not (null ps)]
     multviols = [(r,ps) | r<-multrules fSpec
                         , let ps=ruleviolations r, not (null ps)]
     popviol :: [[(Rule,[(String, String)])]]
     popviol  = eqCl (locnm.origin.fst) [(r,ps) | r<-invariants fSpec, let ps=ruleviolations r, not (null ps)]
     multviol :: [[(Rule,[(String, String)])]]
     multviol  = eqCl (locnm.origin.fst) [(r,ps) | r<-multrules fSpec, let ps=ruleviolations r, not (null ps)]

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
        


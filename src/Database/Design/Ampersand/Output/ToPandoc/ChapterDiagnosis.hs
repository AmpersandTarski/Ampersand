{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Design.Ampersand.Output.ToPandoc.ChapterDiagnosis where

import Database.Design.Ampersand.Output.ToPandoc.SharedAmongChapters
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.FSpec.Graphic.Graphics (makePicture, Picture)
import Data.List
import System.FilePath
import Data.Maybe


fatal :: Int -> String -> a
fatal = fatalMsg "Output.ToPandoc.ChapterDiagnosis"

chpDiagnosis :: FSpec -> (Blocks,[Picture])
chpDiagnosis fSpec
 = (  chptHeader (fsLang fSpec) Diagnosis
   <> diagIntro                       -- an introductory text
   <> fromList roleomissions          -- tells which role-rule, role-interface, and role-relation assignments are missing
   <> fromList roleRuleTable          -- gives an overview of rule-rule assignments
   <> fromList missingConceptDefs     -- tells which concept definitions have been declared without a purpose
   <>  missingRels                      -- tells which relations have been declared without a purpose and/or without a meaning
   <> fromList unusedConceptDefs      -- tells which concept definitions are not used in any relation
   <> fromList relsNotUsed            -- tells which relations are not used in any rule
   <> fromList missingRules           -- tells which rule definitions are missing
   <> fromList ruleRelationRefTable   -- table that shows percentages of relations and rules that have references
   <> fromList invariantsInProcesses  --
   <> fromList processrulesInPatterns --
-- TODO: Needs rework.     populationReport++       -- says which relations are populated.
   <> fromList wipReport              -- sums up the work items (i.e. the violations of process rules)
   <> violationReport          -- sums up the violations caused by the population of this script.
     
   , pics )
  where
  -- shorthand for easy localizing    
  l :: LocalizedStr -> String
  l lstr = localize (fsLang fSpec) lstr
  diagIntro :: Blocks
  diagIntro =
    case fsLang fSpec of
      Dutch   -> para (
                   str "Dit hoofdstuk geeft een analyse van het Ampersand-script van " <> (singleQuoted.str.name) fSpec <> ". "<>
                   str "Deze analyse is bedoeld voor de auteur(s) van dit script. " <>
                   str "Op basis hiervan kunnen zij het script completeren en mogelijke tekortkomingen verbeteren."
                  )
      English -> para (
                   str "This chapter provides an analysis of the Ampersand script of " <> (singleQuoted.str.name) fSpec <> ". "<>
                   str "This analysis is intended for the author(s) of this script. " <>
                   str "It can be used to complete the script or to improve possible flaws."
                  )

  roleRuleTable :: [Block]
  roleRuleTable
    | null ruls = []
    | null (fRoles fSpec) =
        case fsLang fSpec of
          Dutch    -> [Para [ Str $ upCap (name fSpec)++" specificeert geen rollen. " ]]
          English  -> [Para [ Str $ upCap (name fSpec)++" does not define any roles. " ]]
    | null [r | r<-vrules fSpec, isSignal r ] =
        case fsLang fSpec of
          Dutch    -> [Para [ Str $ upCap (name fSpec)++" kent geen procesregels. " ]]
          English  -> [Para [ Str $ upCap (name fSpec)++" does not define any process rules. " ]]
    | otherwise =
        (case fsLang fSpec of
          Dutch    -> Para [ Str $ upCap (name fSpec)++" kent regels aan rollen toe. "
                            , Str "De volgende tabel toont welke regels door een bepaalde rol kunnen worden gehandhaafd."]
          English  -> Para [ Str $ upCap (name fSpec)++" assigns rules to roles. "
                            , Str "The following table shows the rules that are being maintained by a given role."]
        ) :
        [Table []  -- the table containing the role-rule assignments
        (AlignLeft:[AlignCenter |_<-fRoles fSpec])
        (0.0:[0.0 |_<-fRoles fSpec])
        (( case fsLang fSpec of
          Dutch   -> [Plain [Str "regel"]]
          English -> [Plain [Str "rule" ]]
        ) :    [ [Plain [Str (name r)]] | r <- fRoles fSpec ]
        )
        [ [Plain [Str (name rul)]]:[f r rul | r<-fRoles fSpec] | rul<-ruls ]
        ]
     where
      ruls = if null (themes fSpec)
             then [r | r<-vrules fSpec, isSignal r ]
             else [r | pat<-vpatterns   fSpec, name pat `elem` themes fSpec, r<-udefrules pat,         isSignal r ]                  
      f r rul | (r,rul) `elem` maintained      = [Plain [Math InlineMath "\\surd"]]
              | (r,rul) `elem` dead            = [Plain [Math InlineMath "\\times"]]
              | (r,rul) `elem` fRoleRuls fSpec = [Plain [Math InlineMath "\\odot"]]
              | otherwise                      = []
      maintained  -- (r,rul) `elem` maintained means that r can maintain rul without restrictions.
        = [ (role,rul)
          | (role,rul)<-fRoleRuls fSpec
          , and (map (mayedit role) (relsUsedIn rul))
          ]
      mayedit :: Role -> Declaration -> Bool
      mayedit role decl = decl `elem` ((snd.unzip) (filter (\x -> role == fst x) (fRoleRels fSpec)))
      dead -- (r,rul) `elem` dead means that r cannot maintain rul without restrictions.
       = [ (role,rul)
         | (role,rul)<-fRoleRuls fSpec
         , (not.or) (map (mayedit role) (relsUsedIn rul))
         ]

  roleomissions :: [Block]
  roleomissions
   = if      null  (themes fSpec) && (not.null) (vpatterns fSpec) ||
        (not.null) (themes fSpec) && (not.null) (themes fSpec `isc` map name (vpatterns fSpec))
     then [ case fsLang fSpec of
              Dutch   ->
                Plain [ Str $ upCap (name fSpec)++" kent geen regels aan rollen toe. "
                       , Str "Een generieke rol, User, zal worden gedefinieerd om al het werk te doen wat in het bedrijfsproces moet worden uitgevoerd."
                       ]
              English ->
                Plain [ Str $ upCap (name fSpec)++" does not assign rules to roles. "
                       , Str "A generic role, User, will be defined to do all the work that is necessary in the business process."
                       ]
          | (null.fRoleRuls) fSpec && (not.null.vrules) fSpec] ++
          [ case fsLang fSpec of
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
   = case (fsLang fSpec, missing) of
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
                      , cd <- concDefs fSpec c
                      , null (purposesDefinedIn fSpec (fsLang fSpec) cd)
                   ]++
                   [c | c <-ccs, null (concDefs fSpec c)]
         ccs = concs [ d | d<-vrels fSpec, null (themes fSpec)||decpat d `elem` themes fSpec]  -- restrict if the documentation is partial.
  unusedConceptDefs :: [Block]
  unusedConceptDefs
   = case (fsLang fSpec, unused) of
      (Dutch,[])  -> [Para
                       [Str "Alle concepten, die in dit document zijn voorzien van een definitie, worden gebruikt in relaties."]
                     | (not.null.cDefsInScope) fSpec]
      (Dutch,[c]) -> [Para
                       [Str "Het concept ", Quoted SingleQuote [Str (name c)], Str " is gedefinieerd, maar wordt niet gebruikt."]
                     ]
      (Dutch,xs)  -> [Para $
                       [Str "De concepten: "]++commaNLPandoc (Str "en") (map (Str . name) xs)++[Str " zijn gedefinieerd, maar worden niet gebruikt."]
                     ]
      (English,[])  -> [Para
                        [Str "All concepts defined in this document are used in relations."]
                     | (not.null.cDefsInScope) fSpec]
      (English,[c]) -> [Para
                         [Str "The concept ", Quoted SingleQuote [Str (name c)], Str " is defined, but isn't used."]
                     ]
      (English,xs)  -> [Para $
                       [Str "Concepts "]++commaEngPandoc (Str "and") (map (Str . name) xs)++[Str " are defined, but not used."]
                     ]
   where unused = [cd | cd <-cDefsInScope fSpec, name cd `notElem` map name (allConcepts fSpec)]

  missingRels :: Blocks
  missingRels
   = case bothMissing ++ purposeOnlyMissing ++ meaningOnlyMissing of
      [] -> (para.str.l) (NL "Alle relaties in dit document zijn voorzien van zowel een reden van bestaan (purpose) als een betekenis (meaning)."
                         ,EN "All relations in this document have been provided with a purpose as well as a meaning.")
      _ ->(case bothMissing of
            []  -> mempty
            [d] -> para (   (str.l) (NL "Van de relatie ",EN "The relation ")
                         <> showDcl d
                         <> (str.l) (NL " ontbreekt zowel de betekenis (meaning) als de reden van bestaan (purpose)."
                                    ,EN " lacks both a purpose as well as a meaning.")
                        )
            ds  -> para (   (str.l) (NL "Van de relaties ",EN "The relations ")
                          <> commaPandocAnd (fsLang fSpec) (map showDcl ds) 
                          <>(str.l) (NL " ontbreken zowel de betekenis (meaning) als de reden van bestaan (purpose)."
                                    ,EN " all lack both a purpose and a meaning.")
                        )
          )<>
          (case purposeOnlyMissing of
            []  -> mempty
            [d] -> para (   (str.l) (NL "De reden waarom relatie ",EN "The purpose of relation ")
                         <> showDcl d
                         <> (str.l) (NL " bestaat wordt niet uitgelegd."
                                    ,EN " remains unexplained.")
                        )
            ds  -> para (   (str.l) (NL "Relaties ",EN "The purpose of relations ")
                          <> commaPandocAnd (fsLang fSpec) (map showDcl ds) 
                          <>(str.l) (NL " zijn niet voorzien van een reden van bestaan (purpose)."
                                    ,EN " is not documented.")
                        )
          )<>
          (case meaningOnlyMissing of
            []  -> mempty
            [d] -> para (   (str.l) (NL "De betekenis van relatie ",EN "The meaning of relation ")
                         <> showDcl d
                         <> (str.l) (NL " is niet gedocumenteerd."
                                    ,EN " is not documented.")
                        )
            ds  -> para (   (str.l) (NL "De betekenis van relaties ",EN "The meaning of relations ")
                          <> commaPandocAnd (fsLang fSpec) (map showDcl ds) 
                          <>(str.l) (NL " zijn niet gedocumenteerd."
                                    ,EN " is not documented.")
                        )
          )
     where bothMissing, purposeOnlyMissing, meaningOnlyMissing :: [Declaration]
           bothMissing        = filter (not . hasPurpose) . filter (not . hasMeaning) $ decls
           purposeOnlyMissing = filter (not . hasPurpose) . filter (      hasMeaning) $ decls
           meaningOnlyMissing = filter (      hasPurpose) . filter (not . hasMeaning) $ decls
           decls = allDecls fSpec  -- A restriction on only themes that the user wants the document for is not supported, 
                                   -- because it is possible that declarations from other themes are required in the
                                   -- generated document. 
           showDcl = math . showMath . EDcD
  hasPurpose :: Motivated a => a -> Bool
  hasPurpose = not . null . purposesDefinedIn fSpec (fsLang fSpec)
  hasMeaning = isJust . meaning (fsLang fSpec)

  relsNotUsed :: [Block]
  pics :: [Picture]
  (relsNotUsed,pics)
   = ( ( case (fsLang fSpec, notUsed) of
          (Dutch,[])  -> [Para
                           [Str "Alle relaties in dit document worden in één of meer regels gebruikt."]
                         | (not.null.relsMentionedIn.vrules) fSpec]
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
                           | (not.null.relsMentionedIn.vrules) fSpec]
          (English,[r]) -> [Para
                             [ Str "Relation ", r
                             , Str " is not being used in any rule. "
                           ] ]
          (English,rs)  -> [Para $
                             [ Str "Relations "]++commaEngPandoc (Str "and") rs++
                             [ Str " are not used in any rule. "
                           ] ] ) ++
       toList
       ( case (fsLang fSpec, pictsWithUnusedRels) of
          (Dutch,[pict])   -> para ("Figuur " <> xRefReference (getOpts fSpec) pict <> " geeft een conceptueel diagram met alle relaties.") <>
                              plain((showImage (getOpts fSpec)) pict)
          (English,[pict]) -> para ("Figure " <> xRefReference (getOpts fSpec) pict <> " shows a conceptual diagram with all relations.") <>
                              plain((showImage (getOpts fSpec)) pict)
          (Dutch,picts)    -> mconcat
                                  [ para (  "Figuur " <> xRefReference (getOpts fSpec) pict
                                         <> " geeft een conceptueel diagram met alle relaties die gedeclareerd zijn in "
                                         <> (singleQuoted.str.name) pat <> "."
                                         ) <>
                                    (plain . showImage (getOpts fSpec)) pict
                                  | (pict,pat)<-zip picts pats ]
          (English,picts) -> mconcat
                                  [ para (  "Figure " <> xRefReference (getOpts fSpec) pict
                                         <> " shows a conceptual diagram with all relations declared in "
                                         <> (singleQuoted.str.name) pat <> "."
                                         )<>
                                    (plain . showImage (getOpts fSpec)) pict
                                  | (pict,pat)<-zip picts pats ]
       )
       , pictsWithUnusedRels           -- draw the conceptual diagram
     )
     where notUsed = nub [(Math InlineMath . showMath) (EDcD d)
                         | d@Sgn{} <- relsInThemes fSpec -- only signal relations that are used or defined in the selected themes
                         , decusr d
                         , d `notElem` (relsMentionedIn . vrules) fSpec
                         ]
           pats  = [ pat | pat<-vpatterns fSpec
                         , null (themes fSpec) || name pat `elem` themes fSpec  -- restrict if the documentation is partial.
                         , (not.null) (relsDefdIn pat>-relsUsedIn pat) ]
           pictsWithUnusedRels = [makePicture fSpec (PTDeclaredInPat pat) | pat<-pats ]

  missingRules :: [Block]
  missingRules
   = case (fsLang fSpec, missingPurp, missingMeaning) of
      (Dutch,[],[])    -> [ Para [Str "Alle regels in dit document zijn voorzien van een uitleg."]
                          | (length.vrules) fSpec>1]
      (Dutch,rs,rs')   -> [Para
                           (case rs>-rs' of
                              []  -> []
                              [r] -> [ Str "De bestaansreden van regel ", Emph [Str (name r)]
                                     , Str (" op regelnummer "++getLineNr r++" van bestand "++getFileName r)
                                     , Str " wordt niet uitgelegd. "
                                     ]
                              rls -> (upC . commaNLPandoc (Str "en")  )
                                        [let nrs = [(Str . show . linenr) l | l<-cl] in
                                         strconcat ([Str ("op regelnummer"++(if length nrs>1 then "s" else "")++" ")]++
                                                    commaNLPandoc (Str "en") nrs++
                                                    [Str " van bestand "]++[(Str . takeFileName . locnm . head) cl])
                                        | cl<-eqCl locnm (map origin rls)] ++
                                       [ Str " worden regels gedefinieerd, waarvan de bestaansreden niet wordt uitgelegd. " ]
                            ++
                            case rs'>-rs of
                              []  -> []
                              [r] -> [ Str "De betekenis van regel ", Emph [Str (name r)]
                                     , Str (" op regelnummer "++getLineNr r++" van bestand "++getFileName r)
                                     , Str " wordt uitgelegd in taal die door de computer is gegenereerd. "
                                     ]
                              rls -> (upC . commaNLPandoc (Str "en")  )
                                        [let nrs = [(Str . show . linenr) l | l<-cl] in
                                         strconcat ([Str ("op regelnummer"++(if length nrs>1 then "s" else "")++" ")]++
                                                    commaNLPandoc (Str "en") nrs++
                                                    [Str " van bestand "]++[(Str . takeFileName . locnm . head) cl])
                                        | cl<-eqCl locnm (map origin rls)] ++
                                       [ Str " staan regels, waarvan de betekenis wordt uitgelegd in taal die door de computer is gegenereerd. " ]
                            ++
                            case rs `isc` rs' of
                              []  -> []
                              [r] -> [ Str "Regel ", Emph [Str (name r)]
                                     , Str (" op regelnummer "++getLineNr r++" van bestand "++getFileName r++" wordt niet uitgelegd. ")
                                     ]
                              rls -> (upC . commaNLPandoc (Str "en")  )
                                        [let nrs = [(Str . show . linenr) l | l<-cl] in
                                         strconcat ([Str ("op regelnummer"++(if length nrs>1 then "s" else "")++" ")]++
                                                    commaNLPandoc (Str "en") nrs++
                                                    [Str " van bestand "]++[(Str . takeFileName . locnm . head) cl])
                                        | cl<-eqCl locnm (map origin rls)] ++
                                       [ Str " worden regels gedefinieerd, zonder verdere uitleg. " ]
                           )
                          ]
      (English,[],[])  -> [ Para [Str "All rules in this document have been provided with a meaning and a purpose."]
                          | (length.vrules) fSpec>1]
      (English,rs,rs') -> [Para $
                           ( case rs>-rs' of
                              []  -> []
                              [r] -> [ Str "The purpose of rule ", Emph [Str (name r)]
                                     , Str (" on line "++getLineNr r++" of file "++getFileName r)
                                     , Str " is not documented. "
                                     ]
                              rls -> (upC . commaEngPandoc (Str "and") )
                                        [let nrs = [(Str . show . linenr) l | l<-cl] in
                                         strconcat ([ Str ("on line number"++(if length nrs>1 then "s" else "")++" ")]++
                                                    commaEngPandoc (Str "and") nrs ++
                                                    [Str " of file "]++[(Str . takeFileName . locnm . head) cl])
                                        | cl<-eqCl locnm (map origin rls)] ++
                                       [ Str " rules are defined without documenting their purpose. " ]
                           ) ++
                           ( case rs'>-rs of
                              []  -> []
                              [r] -> [ Str "The meaning of rule ", Emph [Str (name r)]
                                     , Str (" on line "++getLineNr r++" of file "++getFileName r)
                                     , Str " is documented by means of computer generated language. "
                                     ]
                              rls -> (upC . commaEngPandoc (Str "and") )
                                        [let nrs = [(Str . show . linenr) l | l<-cl] in
                                         strconcat ([ Str ("on line number"++(if length nrs>1 then "s" else "")++" ")]++
                                                    commaEngPandoc (Str "and") nrs ++
                                                    [Str " of file "]++[(Str . takeFileName . locnm . head) cl])
                                        | cl<-eqCl locnm (map origin rls)] ++
                                       [ Str " rules are defined, the meaning of which is documented by means of computer generated language. " ]
                           ) ++
                           ( case rs `isc` rs' of
                              []  -> []
                              [r] -> [ Str "Rule ", Emph [Str (name r)]
                                     , Str (" on line "++getLineNr r++" of file "++getFileName r++" is not documented. ")
                                     ]
                              rls -> (upC . commaEngPandoc (Str "and") )
                                        [let nrs = [(Str . show . linenr) l | l<-cl] in
                                         strconcat ([ Str ("on line number"++(if length nrs>1 then "s" else "")++" ")]++
                                                    commaEngPandoc (Str "and") nrs ++
                                                    [Str " of file "]++[(Str . takeFileName . locnm . head) cl])
                                        | cl<-eqCl locnm (map origin rls)] ++
                                       [ Str " rules are defined without any explanation. " ]
                           )
                          ]
     where missingPurp
            = nub [ r
                  | r<-ruls
                  , null (purposesDefinedIn fSpec (fsLang fSpec) r)
                  ]
           missingMeaning
            = nub [ r
                  | r<-ruls
                  , null [m | m <- ameaMrk (rrmean r), amLang m == fsLang fSpec]
                  ]
           ruls = if null (themes fSpec)
                  then vrules fSpec
                  else concat [udefrules pat | pat<-vpatterns fSpec, name pat `elem` themes fSpec]
           upC (Str str':strs) = Str (upCap str'):strs
           upC str' = str'

           getFileName :: Traced a => a -> String
           getFileName x = takeFileName . locnm . origin $ x 
           
           getLineNr :: Traced a => a -> String
           getLineNr x = locln . origin $ x

           strconcat :: [Inline] -> Inline
           strconcat strs = (Str . concat) [ str' | Str str'<-strs]

  ruleRelationRefTable =
    [ Para [ Str descriptionStr ]
    , Table [] (AlignLeft : replicate 6 AlignCenter) [0.0,0.0,0.0,0.0,0.0,0.0,0.0]
            (map strCell [ themeStr, relationsStr, withRefStr, "%", rulesStr, withRefStr, "%"])
            (map mkTableRowPat (vpatterns fSpec) ++
            [[]] ++ -- empty row
            [mkTableRow contextStr (filter decusr $ vrels fSpec) (vrules fSpec)])
    ]
    where mkTableRowPat p            = mkTableRow (name p) (ptdcs p) (ptrls p)
          mkTableRow nm decls ruls =
            let nrOfRels = length decls
                nrOfRefRels = length $ filter hasRef decls
                nrOfRules = length ruls
                nrOfRefRules = length $ filter hasRef ruls
            in  map strCell [ nm
                            , show nrOfRels, show nrOfRefRels, showPercentage nrOfRels nrOfRefRels
                            , show nrOfRules, show nrOfRefRules, showPercentage nrOfRules nrOfRefRules
                            ]

          hasRef x = maybe False (any  ((/=[]).explRefIds)) (purposeOf fSpec (fsLang fSpec) x)

          showPercentage x y = if x == 0 then "-" else show (y*100 `div` x)++"%"

          strCell strng = [Plain [Str strng]]

          (descriptionStr, themeStr, relationsStr, withRefStr, rulesStr, contextStr) =
            case fsLang fSpec of Dutch -> ( "Onderstaande tabel bevat per thema (dwz. proces of patroon) tellingen van het aantal relaties en regels, " ++
                                            "gevolgd door het aantal en het percentage daarvan dat een referentie bevat. Relaties die in meerdere thema's " ++
                                            "gedeclareerd worden, worden ook meerdere keren geteld."
                                          , "Thema", "Relaties",  "Met referentie", "Regels", "Gehele context")
                                 _     -> ( "The table below shows for each theme (i.e. process or pattern) the number of relations and rules, followed " ++
                                            " by the number and percentage that have a reference. Relations declared in multiple themes are counted multiple " ++
                                            " times."
                                          , "Theme", "Relations", "With reference", "Rules", "Entire context")

  locnm (FileLoc(FilePos filename _ _) _) = filename
  locnm (DBLoc str') = str'
  locnm _ = "NO FILENAME"
  locln (FileLoc(FilePos _ line _) _) = show line
  locln (DBLoc str') = str'
  locln p = fatal 875 ("funny position "++show p++" in function 'locln'")

-- TODO: give richer feedback...
  invariantsInProcesses :: [Block]
  invariantsInProcesses
   = (case (fsLang fSpec, prs, procs) of
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
       ( case fsLang fSpec of
          Dutch   -> [ [Plain [Str "proces" ]] , [Plain [Str "regel"]] ]
          English -> [ [Plain [Str "process"]] , [Plain [Str "rule" ]] ]
       )
       [ [[Plain [Str (name p)]], [Plain (intercalate [Str ", "] [[Str (name r)] | r<-rs])]]
       | (p,rs)<-prs
       ]
     | not (null prs)]
     where prs = [(p,rs) | p<-procs
                          , let rs=[r | r<-invariants fSpec, name p == r_env r], not (null rs) ]
           procs = if null (themes fSpec) then vpatterns fSpec else [prc | prc<-vpatterns fSpec, name prc `elem` themes fSpec ]

  processrulesInPatterns :: [Block]
  processrulesInPatterns = (toList $ para ("TODO: Inleiding bij de rol-regel tabel"))++
     [ Table []
       ([AlignLeft]++[AlignLeft | multProcs]++[AlignLeft,AlignLeft])
       ([0.0]++[0.0 | multProcs]++[0.0,0.0])
       ( case fsLang fSpec of
          Dutch   ->
              [[Plain [Str "rol"]] ]++[[Plain [Str "in proces" ]] | multProcs]++[[Plain [Str "regel"]], [Plain [Str "uit"  ]] ]
          English ->
              [[Plain [Str "role"]]]++[[Plain [Str "in process"]] | multProcs]++[[Plain [Str "rule" ]], [Plain [Str "from" ]] ]
       )
       [ [[Plain [Str (name rol)]]]++[[Plain [Str (r_env rul)]] | multProcs]++[[Plain [Str (name rul)]], [Plain [Str (r_env rul)]]]
       | (rol,rul)<-prs
       ]
     | length prs>1]
     where prs :: [( Role, Rule )]
           prs = fRoleRuls fSpec
           multProcs = length procs>1
           procs = [ p | p<-vpatterns fSpec
                   , null (themes fSpec) || name p `elem` themes fSpec]  -- restrict if this is partial documentation.


  wipReport :: [Block]
  wipReport
   = [ Para (case (fsLang fSpec, concat popwork,popwork) of
              (Dutch,  [],_)       -> [ Str "De populatie in dit script beschrijft geen onderhanden werk. "
                                      | (not.null.initialPops) fSpec ]  -- SJ 20131212 Is dit correct? Waarom?
              (English,[],_)       -> [ Str "The population in this script does not specify any work in progress. "
                                      | (not.null.initialPops) fSpec ]  -- SJ 20131212 Is this correct? Why
              (Dutch,  [(r,ps)],_) -> [ Str "Regel ", quoterule r, Str (" laat "++count Dutch   (length ps) "taak"++" zien.") ]
              (English,[(r,ps)],_) -> [ Str "Rule ", quoterule r, Str (" shows "++count English (length ps) "task"++".") ]
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
       ( case fsLang fSpec of
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
     [ [ Para ( (case fsLang fSpec of
                  Dutch   -> Str "Regel"
                  English -> Str "Rule"):
                [Space,quoterule r,Space]++
                toList(xRefTo (XRefNaturalLanguageRule r) )++
                (case fsLang fSpec of
                  Dutch   -> [ Str " luidt: " ]
                  English -> [ Str " says: "  ]
                )
              )]  ++meaning2Blocks (fsLang fSpec) r++
       [Plain ( case fsLang fSpec of
                  Dutch  ->
                     [ Str "Deze regel bevat nog werk (voor "]++
                     commaNLPandoc (Str "of") (nub [Str (name rol) | (rol, rul)<-fRoleRuls fSpec, r==rul])++[Str ")"]++
                     (if length ps == 1 then [Str ", te weten "]++oneviol r ps++[Str ". "] else
                      [ Str (". De volgende tabel laat de "++(if length ps>10 then "eerste tien " else "")++"items zien die aandacht vragen.")]
                     )
                  English ->
                     [ Str "This rule contains work"]++
                     commaEngPandoc (Str "or") (nub [Str (name rol) | (rol, rul)<-fRoleRuls fSpec, r==rul])++[Str ")"]++
                     if length ps == 1 then [Str " by "]++oneviol r ps++[Str ". "] else
                      [ Str ("The following table shows the "++(if length ps>10 then "first ten " else "")++"items that require attention.")]

              ) ]++
       [ violtable r ps | length ps>1]
     | (r,ps)<-concat popwork ]
     where
--      text r
--       = if null expls
--         then explains2Blocks (autoMeaning (fsLang fSpec) r)
--         else expls
--         where expls = [Plain (block++[Space]) | Means l econt<-rrxpl r, l==Just (fsLang fSpec) || l==Nothing, Para block<-econt]
      quoterule r
       = if name r==""
         then case fsLang fSpec of
               English -> Str ("on "++show (origin r))
               Dutch   -> Str ("op "++show (origin r))
         else Quoted SingleQuote [Str (name r)]
      oneviol :: Rule -> [AAtomPair] -> [Inline]
      oneviol r [p]
       = if source r==target r && apLeft p==apRight p
         then [Quoted  SingleQuote [Str (name (source r)),Space,Str ((showVal.apLeft) p)]]
         else [Str "(",Str (name (source r)),Space,Str ((showVal.apLeft) p),Str ", ",Str (name (target r)),Space,Str ((showVal.apRight) p),Str ")"]
      oneviol _ _ = fatal 810 "oneviol must have a singleton list as argument."
      popwork :: [[(Rule,[AAtomPair])]];
      popwork = eqCl (locnm.origin.fst) [(r,ps) | (r,ps) <- allViolations fSpec, isSignal r, partofThemes r]
  partofThemes r =
        or [ null (themes fSpec)
           , r `elem` concat [udefrules pat | pat<-vpatterns fSpec, name pat `elem` themes fSpec]
           ]

  violationReport :: Blocks
  violationReport
   = let (processViolations,invariantViolations) = partition (isSignal.fst) (allViolations fSpec)
         showViolatedRule :: (Rule,[AAtomPair]) -> Blocks
         showViolatedRule (r,ps)
             = let capt = case (fsLang fSpec,isSignal r) of
                               (Dutch  , False) -> text "Overtredingen van regel "<>  text (name r)
                               (English, False) -> text "Violations of rule "<>  text (name r)
                               (Dutch  , True ) -> text "Openstaande taken voor "        <> text (commaNL  "of" (map name (nub [rol | (rol, rul)<-fRoleRuls fSpec, r==rul])))
                               (English, True ) -> text "Tasks yet to be performed by "  <> text (commaEng "or" (map name (nub [rol | (rol, rul)<-fRoleRuls fSpec, r==rul])))

                   showRow :: AAtomPair -> [Blocks]
                   showRow p = [(para.text.showVal.apLeft) p,(para.text.showVal.apRight) p]
               in para ( case fsLang fSpec of
                            Dutch   -> text "Regel "
                            English -> text "Rule "
                         <>  text (name r)
                       )
               <> para (text (case (fsLang fSpec,isSignal r) of
                               (Dutch  , False) -> "Totaal aantal overtredingen: "++show (length ps)
                               (English, False) -> "Total number of violations: " ++show (length ps)
                               (Dutch  , True ) -> "Totaal aantal taken: "        ++show (length ps)
                               (English, True ) -> "Total number of work items: " ++show (length ps)
                             )
                       )
               <> table capt
                   [(AlignLeft,0)                          ,(AlignLeft,0)          ]
                   [(para.strong.text.name.source.rrexp) r,(para.strong.text.name.target.rrexp) r]
                   (map showRow ps)

     in (para (case (fsLang fSpec, invariantViolations, processViolations) of
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


  violtable :: Rule -> [AAtomPair] -> Block
  violtable r ps
      = if hasantecedent r && isIdent (antecedent r)  -- note: treat 'isIdent (consequent r) as binary table.
        then Table []
             [AlignLeft]
             [0.0]
             [[Plain [(Str . name . source) r]]]
             [ [[Plain [Str (showVal(apLeft p))]]]
             | p <-take 10 ps
             ]
        else Table []
             [AlignLeft,AlignLeft]
             [0.0,0.0]
             [[Plain [(Str . name . source) r]], [Plain [(Str . name . target) r] ]]
             [ [[Plain [Str (showVal (apLeft p))]], [Plain [Str (showVal(apRight p))]]]
             | p <-take 10 ps
             ]


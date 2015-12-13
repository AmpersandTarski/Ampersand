{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Design.Ampersand.Output.ToPandoc.ChapterDiagnosis where

import Database.Design.Ampersand.Output.ToPandoc.SharedAmongChapters
import Data.List(nub,partition)
import Data.Maybe(isJust)

chpDiagnosis :: FSpec -> (Blocks,[Picture])
chpDiagnosis fSpec
 = (  chptHeader (fsLang fSpec) Diagnosis
   <> para (   (str.l) (NL "Dit hoofdstuk geeft een analyse van het Ampersand-script van "
                       ,EN "This chapter provides an analysis of the Ampersand script of ")
            <> (emph.singleQuoted.str.name) fSpec 
            <>  str ". "
            <> (str.l) (NL $ "Deze analyse is bedoeld voor de auteur(s) van dit script. "
                          ++ "Op basis hiervan kunnen zij het script completeren en mogelijke tekortkomingen verbeteren."
                       ,EN $ "This analysis is intended for the author(s) of this script. "
                          ++ "It can be used to complete the script or to improve possible flaws.")
           )   
   <> roleomissions          -- tells which role-rule, role-interface, and role-relation assignments are missing
   <> roleRuleTable          -- gives an overview of rule-rule assignments
   <> missingConceptDefs     -- tells which concept definitions have been declared without a purpose
   <> missingRels            -- tells which relations have been declared without a purpose and/or without a meaning
   <> unusedConceptDefs      -- tells which concept definitions are not used in any relation
   <> relsNotUsed            -- tells which relations are not used in any rule
   <> missingRules           -- tells which rule definitions are missing
   <> ruleRelationRefTable   -- table that shows percentages of relations and rules that have references
   <> processrulesInPatterns --
   <> wipReport              -- sums up the work items (i.e. the violations of process rules)
   <> violationReport          -- sums up the violations caused by the population of this script.
     
   , pics )
  where
  -- shorthand for easy localizing    
  l :: LocalizedStr -> String
  l lstr = localize (fsLang fSpec) lstr

  roleRuleTable :: Blocks
  roleRuleTable
    | null ruls = mempty
    | null (fRoles fSpec) =
         para (   (emph.str.upCap.name) fSpec
               <> (str.l) (NL " specificeert geen rollen. "
                          ,EN " does not define any roles. ")
              )
    | otherwise =
        case (filter isSignal) ruls of
          []   -> para (   (emph.str.upCap.name) fSpec
                        <> (str.l) (NL " kent geen procesregels. "
                                   ,EN " does not define any process rules. ")
                       )
          sigs -> (para (   (emph.str.upCap.name) fSpec
                         <> (str.l) (NL " kent regels aan rollen toe. "
                                    ,EN " assigns rules to roles. ")
                         <> (str.l) (NL "De volgende tabel toont welke regels door een bepaalde rol kunnen worden gehandhaafd."
                                    ,EN "The following table shows the rules that are being maintained by a given role.")
                        )
                  ) <> 
                  ( table -- No caption:
                          mempty
                          -- Alignment:
                          (  (AlignLeft,0.4) 
                           : replicate (length.fRoles $ fSpec) (AlignLeft, 0.6/(fromIntegral.length.fRoles $ fSpec))
                          )  
                          -- Header row:
                          (  (plain.str.l) (NL "Regel", EN "Rule")
                           : map (plain.str.name) (fRoles fSpec)
                          )
                          -- Content rows:
                          [  (plain.str.name) rul
                            :[f rol rul | rol<-fRoles fSpec] 
                          | rul<-sigs 
                          ]
                  )
     where
                  
      ruls = filter inScopeRule . filter isSignal . vrules $ fSpec                  
      f :: Role -> Rule -> Blocks
      f rol rul | (rol,rul) `elem` maintained      = (plain.emph.str.l) (NL "ja",EN "yes")
                | (rol,rul) `elem` dead            = (plain.emph.str.l) (NL "nee",EN "no")
                | (rol,rul) `elem` fRoleRuls fSpec = (plain.emph.str.l) (NL "part",EN "part")
                | otherwise                      = mempty
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

  roleomissions :: Blocks
  roleomissions
   = if (null . filter inScopePat . vpatterns) fSpec
     then mempty
     else (if (null.fRoleRuls) fSpec && (not.null.vrules) fSpec
           then plain (   (emph.str.upCap.name) fSpec
                       <> (str.l) (NL " kent geen regels aan rollen toe. "
                                  ,EN " does not assign rules to roles. ")
                       <> (str.l) (NL "Een generieke rol, User, zal worden gedefinieerd om al het werk te doen wat in het bedrijfsproces moet worden uitgevoerd."
                                  ,EN "A generic role, User, will be defined to do all the work that is necessary in the business process.")
                      )
           else mempty
          )<>
          (if null (fRoleRels fSpec) && (not.null.fRoleRuls) fSpec ||(not.null.fRoleRels) fSpec
           then plain (   (emph.str.upCap.name) fSpec
                       <> (str.l) (NL " specificeert niet welke rollen de inhoud van welke relaties mogen wijzigen. "
                                  ,EN " does not specify which roles may change the contents of which relations. ")
                      )
           else mempty
          )

  missingConceptDefs :: Blocks
  missingConceptDefs =
   case missing of
      []  -> if (null.concs) fSpec
             then mempty
             else (para.str.l) (NL "Alle concepten in dit document zijn voorzien van een bestaansreden."
                               ,EN "All concepts in this document have been provided with a purpose.")
      [c] -> para (   (str.l) (NL "De bestaansreden van concept "
                              ,EN "The concept ")
                   <> (singleQuoted.str.name) c
                   <> (str.l) (NL " is niet gedocumenteerd."
                              ,EN " remains without a purpose.")
                  )
      xs  -> para (   (str.l) (NL "De bestaansreden van de concepten: "
                              ,EN "Concepts ")
                   <> commaPandocAnd (fsLang fSpec) (map (str.name) xs)
                   <> (str.l) (NL " is niet gedocumenteerd."
                              ,EN " remain without a purpose.")
                  )
   where missing = [c | c <-ccs
                      , cd <- concDefs fSpec c
                      , null (purposesDefinedIn fSpec (fsLang fSpec) cd)
                   ]++
                   [c | c <-ccs, null (concDefs fSpec c)]
         ccs = concs [ d | d<-vrels fSpec, null (themes fSpec)||decpat d `elem` themes fSpec]  -- restrict if the documentation is partial.
  unusedConceptDefs :: Blocks
  unusedConceptDefs
   = case [cd | cd <-cDefsInScope fSpec, name cd `notElem` map name (concs fSpec)] of
      []  -> if (null.cDefsInScope) fSpec
             then mempty
             else para.str.l $
                     (NL "Alle concepten, die in dit document zijn voorzien van een definitie, worden gebruikt in relaties."
                     ,EN "All concepts defined in this document are used in relations.")
      [c] -> para (   (str.l) (NL "Het concept ",EN "The concept ") 
                   <> singleQuoted (str (name c))
                   <> (str.l) (NL " is gedefinieerd, maar wordt niet gebruikt."
                              ,EN " is defined, but isn't used.")
                  )
      xs  -> para (   (str.l) (NL "De concepten: ", EN "Concepts ")
                   <> commaPandocAnd (fsLang fSpec) (map (str . name) xs)
                   <> (str.l) (NL " zijn gedefinieerd, maar worden niet gebruikt."
                              ,EN " are defined, but not used.")
                  )

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
  hasMeaning :: Meaning a => a -> Bool
  hasMeaning = isJust . meaning (fsLang fSpec)

  relsNotUsed :: Blocks
  pics :: [Picture]
  (relsNotUsed,pics)
   = (  
       (case notUsed of
          []  -> ( if (null.relsMentionedIn.vrules) fSpec
                   then mempty
                   else (para .str.l)
                            (NL "Alle relaties in dit document worden in één of meer regels gebruikt."
                            ,EN "All relations in this document are being used in one or more rules.")
                 )
          [r] -> para (   (str.l) (NL "De relatie ",EN  "Relation ")
                       <> r 
                       <> (str.l) (NL " wordt in geen enkele regel gebruikt. "
                                  ,EN " is not being used in any rule. ")
                      )
          rs  -> para (   (str.l) (NL "Relaties ", EN "Relations ")
                       <> commaPandocAnd (fsLang fSpec) rs
                       <> (str.l) (NL " worden niet gebruikt in regels. "
                                  ,EN " are not used in any rule. ")
                      ) 
       ) <>
       ( case pictsWithUnusedRels of
          [pict] -> para (    (str.l) (NL "Figuur ", EN "Figure ")
                           <> xRefReference (getOpts fSpec) pict 
                           <> (str.l) (NL " geeft een conceptueel diagram met alle relaties."
                                      ,EN " shows a conceptual diagram with all relations.")
                         ) <>
                    plain((showImage (getOpts fSpec)) pict)
          picts  -> mconcat
                       [ para (   (str.l) (NL "Figuur ", EN "Figure ")
                               <> xRefReference (getOpts fSpec) pict
                               <> (str.l) (NL " geeft een conceptueel diagram met alle relaties die gedeclareerd zijn in "
                                          ,EN " shows a conceptual diagram with all relations declared in ")
                               <> (singleQuoted.str.name) pat <> "."
                              )
                       <>(plain . showImage (getOpts fSpec)) pict
                       | (pict,pat)<-zip picts pats
                       ]
       )
       , pictsWithUnusedRels           -- draw the conceptual diagram
     )
     where notUsed :: [Inlines]
           notUsed = [(math . showMath) (EDcD d)
                     | d@Sgn{} <- nub (relsInThemes fSpec) -- only signal relations that are used or defined in the selected themes
                     , decusr d
                     , d `notElem` (relsMentionedIn . vrules) fSpec
                     ]
           pats  = [ pat | pat<-vpatterns fSpec
                         , null (themes fSpec) || name pat `elem` themes fSpec  -- restrict if the documentation is partial.
                         , (not.null) (relsDefdIn pat>-relsUsedIn pat) ]
           pictsWithUnusedRels = [makePicture fSpec (PTDeclaredInPat pat) | pat<-pats ]

  missingRules :: Blocks
  missingRules
   = case if null (themes fSpec)
          then vrules fSpec
          else concat [udefrules pat | pat<-vpatterns fSpec, name pat `elem` themes fSpec] of
      []   -> mempty
      ruls ->
       ( if all hasMeaning ruls && all hasPurpose ruls
         then (para.str.l) (NL "Alle regels in dit document zijn voorzien van een uitleg."
                           ,EN "All rules in this document have been provided with a meaning and a purpose.")
         else ( case filter (not.hasPurpose) ruls of
                  []  -> mempty
                  rls -> (para.str.l) (NL "Van de volgende regels is de bestaansreden niet uitgelegd:"
                                      ,EN "Rules are defined without documenting their purpose:")
                       <> bulletList [    (para.emph.str.name) r 
                                       <> (plain.str.show.origin) r 
                                     | r <- rls]
              ) <>
              ( case filter (not.hasMeaning) ruls of
                  []  -> mempty
                  rls -> (para.str.l) (NL "Van de volgende regels is de betekenis uitgelegd in taal die door de computer is gegenereerd:"
                                      ,EN "Rules are defined, the meaning of which is documented by means of computer generated language:")
                       <> bulletList [    (para.emph.str.name) r 
                                       <> (plain.str.show.origin) r 
                                     | r <- rls]
              )
       ) 

  ruleRelationRefTable :: Blocks
  ruleRelationRefTable =
      (para.str.l) (NL $ "Onderstaande tabel bevat per thema (dwz. proces of patroon) tellingen van het aantal relaties en regels, " ++
                         "gevolgd door het aantal en het percentage daarvan dat een referentie bevat. Relaties die in meerdere thema's " ++
                         "gedeclareerd worden, worden ook meerdere keren geteld."
                   ,EN $ "The table below shows for each theme (i.e. process or pattern) the number of relations and rules, followed " ++
                         " by the number and percentage that have a reference. Relations declared in multiple themes are counted multiple " ++
                         " times."
                   )
    <>(table -- No caption:
             mempty
             -- Alignment:
             ((AlignLeft,0.4) : replicate 6 (AlignCenter,0.1))
             -- Headers
             (map (plain.str.l) [ (NL "Thema"         , EN "Theme")
                                , (NL "Relaties"      , EN "Relations")
                                , (NL "Met referentie", EN "With reference")
                                , (NL "%"             , EN "%")
                                , (NL "Regels"        , EN "Rules")
                                , (NL "Gehele context", EN "Entire context")
                                , (NL "%"             , EN "%")
                                ]
             )
             -- Content rows
             (   map mkTableRowPat (vpatterns fSpec)
              ++ [mempty] -- empty row
              ++ [mkTableRow (l (NL "Gehele context", EN "Entire context")) (filter decusr $ vrels fSpec) (vrules fSpec)]
             )
      )
    where mkTableRow :: String  -- The name of the pattern / fSpec 
                     -> [Declaration] --The user-defined relations of the pattern / fSpec
                     -> [Rule]  -- The user-defined rules of the pattern / fSpec
                     -> [Blocks]
          mkTableRowPat p = mkTableRow (name p) (relsDefdIn p) (udefrules p)
          mkTableRow nm rels ruls =
            map (plain.str) [ nm
                            , (show.length) rels 
                            , (show.length) (filter hasRef rels)
                            , showPercentage (length rels) (length.filter hasRef $ rels)
                            , (show.length) ruls
                            , (show.length) (filter hasRef ruls)
                            , showPercentage (length ruls) (length.filter hasRef $ ruls)
                            ]

          hasRef x = maybe False (any  ((/=[]).explRefIds)) (purposeOf fSpec (fsLang fSpec) x)

          showPercentage x y = if x == 0 then "-" else show (y*100 `div` x)++"%"

  processrulesInPatterns :: Blocks
  processrulesInPatterns = 
       para ("TODO: Inleiding bij de rol-regel tabel")
    <> if null (fRoleRuls fSpec)
       then mempty
       else table -- No caption:
                  mempty
                  -- Alignment:
                  ( if multProcs
                    then replicate 4 (AlignLeft,1/4)
                    else replicate 3 (AlignLeft,1/3)
                  )
                  -- Headers:
                  (  [ (plain.str.l) (NL "rol"      , EN "role")]
                   ++[ (plain.str.l) (NL "in proces", EN "in process") | multProcs]
                   ++[ (plain.str.l) (NL "regel"    , EN "rule")
                     , (plain.str.l) (NL "uit"      , EN "from")
                     ]
                  )
                  -- Rows:
                  [  [ (plain.str.name) rol]
                   ++[ (plain.str.r_env) rul | multProcs]
                   ++[ (plain.str.name) rul
                     , (plain.str.r_env) rul
                     ]
                  | (rol,rul)<-fRoleRuls fSpec]
     where multProcs = length procsInScope>1
           procsInScope = filter inScopePat (vpatterns fSpec)

  wipReport :: Blocks
  wipReport
   =   case popwork of
         []       -> (para.str.l) (NL "De populatie in dit script beschrijft geen onderhanden werk. "
                                  ,EN "The population in this script does not specify any work in progress. ")
         [(r,ps)] -> para (   (str.l) (NL"Regel ",EN "Rule")
                            <> quoterule r
                            <>(str.l) (NL $ " laat " ++count Dutch   (length ps) "taak"++" zien."
                                      ,EN $ " shows "++count English (length ps) "task"++".")
                          )
         _        -> (para.str.l) (NL "Dit script bevat onderhanden werk. De volgende tabellen geven details met regelnummers in de oorspronkelijk script-bestanden."
                                  ,EN "This script contains work in progress. The following tables provide details with line numbers from the original script files.")
            
     <> if null popwork
        then mempty
        else table -- No caption:
                    mempty
                    -- Alignment:
                    ((AlignLeft,1/3): replicate 2 (AlignRight,1/3))
                    --Header:
                    (map (plain.str.l) 
                           [ (NL "regel"  ,EN "rule" )
                           , (NL "locatie",EN "location" )
                           , (NL "#taken" ,EN "#tasks" )
                           ])
                    -- Rows:
                    [ map (plain.str)
                           [ name r
                           , (show.origin) r
                           , (show.length) ps
                           ]
                    | (r,ps)<-popwork
                    ]
     <>
-- the tables containing the actual work in progress population
     mconcat
     [    para (  (str.l) (NL "Regel", EN "Rule")
               <> quoterule r
               <> xRefTo (XRefNaturalLanguageRule r)
               <> (str.l) (NL " luidt: ", EN "says: ")
               )
       <> fromList (meaning2Blocks (fsLang fSpec) r)
       <> para (  (str.l) (NL "Deze regel bevat nog werk (voor "
                          ,EN "This rule contains work (for ")
                <>commaPandocOr (fsLang fSpec) (map (str.name) (nub [rol | (rol, rul)<-fRoleRuls fSpec, r==rul]))
                <>")"
                <> if length ps == 1
                   then   (str.l) (NL ", te weten ", EN " by ")
                       <> oneviol r (head ps)
                       <> "."
                   else   (str.l) (NL $ ". De volgende tabel laat de "++(if length ps>10 then "eerste tien " else "")++"items zien die aandacht vragen."
                                  ,EN $ "The following table shows the "++(if length ps>10 then "first ten " else "")++"items that require attention.")
               )
       <> if length ps <= 1
          then mempty -- iff there is a single violation, it is already shown in the previous paragraph
          else violtable r ps 
     | (r,ps)<- popwork ]
     where
--      text r
--       = if null expls
--         then explains2Blocks (autoMeaning (fsLang fSpec) r)
--         else expls
--         where expls = [Plain (block++[Space]) | Means l econt<-rrxpl r, l==Just (fsLang fSpec) || l==Nothing, Para block<-econt]
      quoterule r
       = if null (name r)
         then (str.l) (NL $ "op "++show (origin r)
                      ,EN $ "at "++show (origin r))
         else (singleQuoted.str.name) r
      oneviol :: Rule -> AAtomPair -> Inlines
      oneviol r p
       = if source r==target r && apLeft p==apRight p
         then singleQuoted (  (str.name.source) r 
                            <>(str.showValADL.apLeft) p
                           )
         else    "("  <> (str.name.source) r <> (str.showValADL.apLeft) p 
              <> ", " <> (str.name.target) r <> (str.showValADL.apRight) p
              <> ")"
      popwork :: [(Rule,[AAtomPair])]
      popwork = [(r,ps) | (r,ps) <- allViolations fSpec, isSignal r, inScopeRule r]

  violationReport :: Blocks
  violationReport =
        (para (case (invariantViolations, processViolations) of
                ([] , [] ) ->  (str.l) (NL "De populatie in dit script overtreedt geen regels. "
                                       ,EN "The population in this script violates no rule. ")
                (iVs, pVs) ->  (str.l) (NL "De populatie in dit script overtreedt "
                                       ,EN "The population in this script violates ")
                             <>(str.show.length) iVs
                             <>(str.l) (NL $ " invariant"++(if length iVs == 1 then "" else "en")++" en "
                                       ,EN $ " invariant"++(if length iVs == 1 then "" else "s")++" and ")
                             <>(str.show.length) pVs
                             <>(str.l) (NL $ " procesregel" ++if length pVs == 1 then "" else "s"++"."
                                       ,EN $ " process rule"++if length pVs == 1 then "" else "s"++"."
                           )
              )
        )
     <> bulletList (map showViolatedRule invariantViolations)
     <> bulletList (map showViolatedRule processViolations)
    where
         (processViolations,invariantViolations) = partition (isSignal.fst) (allViolations fSpec)
         showViolatedRule :: (Rule,[AAtomPair]) -> Blocks
         showViolatedRule (r,ps)
             =    (para.emph)
                      (  (str.l) (NL "Regel ", EN "Rule ")
                       <>(str.name) r
                      )
               <> para(  (if isSignal r 
                          then (str.l) (NL "Totaal aantal taken: "        ,EN "Total number of work items: ")
                          else (str.l) (NL "Totaal aantal overtredingen: ",EN "Total number of violations: ")
                         )
                       <>(str.show.length) ps
                      )
               <> table -- Caption
                        (if isSignal r
                         then ( (str.l) (NL "Openstaande taken voor "     ,EN "Tasks yet to be performed by ")
                              <>(commaPandocOr (fsLang fSpec) (map (str.name) (nub [rol | (rol, rul)<-fRoleRuls fSpec, r==rul])))
                              )
                         else ( (str.l) (NL "Overtredingen van invariant ",EN "Violations of invariant ")
                              <>(str.name) r
                              )
                        )  
                        -- Alignment:
                        (replicate 2 (AlignLeft,1/2))
                        -- Headers:
                        [(para.strong.text.name.source.rrexp) r
                        ,(para.strong.text.name.target.rrexp) r
                        ]
                        -- Rows:
                        [ [(para.text.showValADL.apLeft) p
                          ,(para.text.showValADL.apRight) p
                          ]
                        | p<- ps]



  violtable :: Rule -> [AAtomPair] -> Blocks
  violtable r ps
      = if hasantecedent r && isIdent (antecedent r)  -- note: treat 'isIdent (consequent r) as binary table.
        then table -- No caption:
                   mempty
                   -- Alignment:
                   [(AlignLeft,1.0)]
                   -- Header:
                   [(plain.str.name.source) r]
                   -- Data rows:
                   [ [(plain.str.showValADL.apLeft) p]
                   | p <-take 10 ps --max 10 rows
                   ]
        else table -- No caption:
                   mempty
                   -- Alignment:
                   (replicate 2 (AlignLeft,1/2))
                   -- Header:
                   [(plain.str.name.source) r , (plain.str.name.target) r ]
                   -- Data rows:
                   [ [(plain.str.showValADL.apLeft) p,(plain.str.showValADL.apRight) p]
                   | p <-take 10 ps --max 10 rows
                   ]

  inScopePat :: Pattern -> Bool
  inScopePat x = null (themes fSpec) || name x `elem` themes fSpec  -- restrict if this is partial documentation.

  inScopeRule :: Rule -> Bool
  inScopeRule r =
        or [ null (themes fSpec)
           , r `elem` concat [udefrules pat | pat<-vpatterns fSpec, name pat `elem` themes fSpec]
           ]

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Ampersand.Output.ToPandoc.ChapterDiagnosis where

import           Ampersand.Output.ToPandoc.SharedAmongChapters
import qualified RIO.List as L
import           Data.Maybe(isJust,fromMaybe)
import qualified RIO.Set as Set
import qualified RIO.Text as T

chpDiagnosis :: (HasDirOutput env, HasDocumentOpts env) 
   => env -> FSpec -> (Blocks,[Picture])
chpDiagnosis env fSpec
 | Diagnosis `notElem` view chaptersL env = mempty
 | otherwise
 = (  xDefBlck env fSpec Diagnosis
   <> para (   (str.l) (NL "Dit hoofdstuk geeft een analyse van het Ampersand-script van "
                       ,EN "This chapter provides an analysis of the Ampersand script of ")
            <> (emph.singleQuoted.str.name) fSpec 
            <>  str ". "
            <> (str.l) (NL $ "Deze analyse is bedoeld voor de auteur(s) van dit script. "
                          <> "Op basis hiervan kunnen zij het script completeren en mogelijke tekortkomingen verbeteren."
                       ,EN $ "This analysis is intended for the author(s) of this script. "
                          <> "It can be used to complete the script or to improve possible flaws.")
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
  l :: LocalizedStr -> Text
  l = localize outputLang'
  outputLang' = outputLang env fSpec
  roleRuleTable :: Blocks
  roleRuleTable
    | null ruls = mempty
    | null (fRoles fSpec) =
         para (   (emph.str.upCap.name) fSpec
               <> (str.l) (NL " specificeert geen rollen. "
                          ,EN " does not define any roles. ")
              )
    | otherwise =
        case filter isSignal . Set.elems $ ruls of
          []   -> para (   (emph.str.upCap.name) fSpec
                        <> (str.l) (NL " kent geen procesregels. "
                                   ,EN " does not define any process rules. ")
                       )
          sigs ->  para (   (emph.str.upCap.name) fSpec
                         <> (str.l) (NL " kent regels aan rollen toe. "
                                    ,EN " assigns rules to roles. ")
                         <> (str.l) (NL "De volgende tabel toont welke regels door een bepaalde rol worden bewaakt."
                                    ,EN "The following table shows the rules that are being maintained by a given role.")
                        )
                <> table -- No caption:
                          mempty
                          -- Alignment:
                          (  (AlignLeft,0.4) 
                           : replicate (length.fRoles $ fSpec) (AlignLeft, 0.6/(fromIntegral.length.fRoles $ fSpec))
                          )  
                          -- Header row:
                          (  (plain.str.l) (NL "Regel", EN "Rule")
                           : map (plain.str.name.fst) (fRoles fSpec)
                          )
                          -- Content rows:
                          [  (plain.str.name) rul
                            :[f rol rul | (rol,_)<-fRoles fSpec] 
                          | rul<-sigs 
                          ]
                  
     where
                  
      ruls = Set.filter isSignal . vrules $ fSpec                  
      f :: Role -> Rule -> Blocks
      f _ _ = mempty

  roleomissions :: Blocks
  roleomissions
   = if null (vpatterns fSpec)
     then mempty
     else (if (null.fRoleRuls) fSpec && (not.null.vrules) fSpec
           then plain (   (emph.str.upCap.name) fSpec
                       <> (str.l) (NL " kent geen regels aan rollen toe. "
                                  ,EN " does not assign rules to roles. ")
                       <> (str.l) (NL "Een generieke rol, User, zal worden gedefinieerd om al het werk te doen wat in het bedrijfsproces moet worden uitgevoerd."
                                  ,EN "A generic role, User, will be defined to do all the work that is necessary in the business process.")
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
                   <> commaPandocAnd outputLang' (map (str.name) xs)
                   <> (str.l) (NL " is niet gedocumenteerd."
                              ,EN " remain without a purpose.")
                  )
   where missing = [c | c <-ccs
                      , null (purposesDefinedIn fSpec outputLang' c)
                   ]<>
                   [c | c <-ccs, null (concDefs fSpec c)]
         ccs = Set.elems . concs . vrels $ fSpec
  unusedConceptDefs :: Blocks
  unusedConceptDefs
   = case [cd | cd <-conceptDefs fSpec, name cd `notElem` map name (Set.elems $ concs fSpec)] of
      []  -> if (null.conceptDefs) fSpec
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
                   <> commaPandocAnd outputLang' (map (str . name) xs)
                   <> (str.l) (NL " zijn gedefinieerd, maar worden niet gebruikt."
                              ,EN " are defined, but not used.")
                  )

  missingRels :: Blocks
  missingRels
   = case bothMissing <> purposeOnlyMissing <> meaningOnlyMissing of
      [] -> (para.str.l) (NL "Alle relaties in dit document zijn voorzien van zowel een reden van bestaan (purpose) als een betekenis (meaning)."
                         ,EN "All relations in this document have been provided with a purpose as well as a meaning.")
      _ ->(case bothMissing of
            []  -> mempty
            [d] -> para (   (str.l) (NL "Van de relatie ",EN "The relation ")
                         <> showDclMath d
                         <> (str.l) (NL " ontbreekt zowel de betekenis (meaning) als de reden van bestaan (purpose)."
                                    ,EN " lacks both a purpose as well as a meaning.")
                        )
            ds  -> para (   (str.l) (NL "Van de relaties ",EN "The relations ")
                          <> commaPandocAnd outputLang' (map showDclMath ds) 
                          <>(str.l) (NL " ontbreken zowel de betekenis (meaning) als de reden van bestaan (purpose)."
                                    ,EN " all lack both a purpose and a meaning.")
                        )
          )<>
          (case purposeOnlyMissing of
            []  -> mempty
            [d] -> para (   (str.l) (NL "De reden waarom relatie ",EN "The purpose of relation ")
                         <> showDclMath d
                         <> (str.l) (NL " bestaat wordt niet uitgelegd."
                                    ,EN " remains unexplained.")
                        )
            ds  -> para (   (str.l) (NL "Relaties ",EN "The purpose of relations ")
                          <> commaPandocAnd outputLang' (map showDclMath ds) 
                          <>(str.l) (NL " zijn niet voorzien van een reden van bestaan (purpose)."
                                    ,EN " is not documented.")
                        )
          )<>
          (case meaningOnlyMissing of
            []  -> mempty
            [d] -> para (   (str.l) (NL "De betekenis van relatie ",EN "The meaning of relation ")
                         <> showDclMath d
                         <> (str.l) (NL " is niet gedocumenteerd."
                                    ,EN " is not documented.")
                        )
            ds  -> para (   (str.l) (NL "De betekenis van relaties ",EN "The meaning of relations ")
                          <> commaPandocAnd outputLang' (map showDclMath ds) 
                          <>(str.l) (NL " zijn niet gedocumenteerd."
                                    ,EN " is not documented.")
                        )
          )
     where bothMissing, purposeOnlyMissing, meaningOnlyMissing :: [Relation]
           bothMissing        = filter (not . hasPurpose) . filter (not . hasMeaning) . Set.elems $ decls
           purposeOnlyMissing = filter (not . hasPurpose) . filter        hasMeaning  . Set.elems $ decls
           meaningOnlyMissing = filter        hasPurpose  . filter (not . hasMeaning) . Set.elems $ decls
           decls = vrels fSpec
           showDclMath = math . showRel
  hasPurpose :: Motivated a => a -> Bool
  hasPurpose = not . null . purposesDefinedIn fSpec outputLang'
  hasMeaning :: HasMeaning a => a -> Bool
  hasMeaning = isJust . meaning outputLang'

  relsNotUsed :: Blocks
  pics :: [Picture]
  (relsNotUsed,pics)
   = (  
        case notUsed of
          []  -> if (null.bindedRelationsIn.vrules) fSpec
                   then mempty
                   else (para .str.l)
                            (NL "Alle relaties in dit document worden in één of meer regels gebruikt."
                            ,EN "All relations in this document are being used in one or more rules.")
          [r] -> para (   (str.l) (NL "De relatie ",EN  "Relation ")
                       <> r 
                       <> (str.l) (NL " wordt in geen enkele regel gebruikt. "
                                  ,EN " is not being used in any rule. ")
                      )
          rs  -> para (   (str.l) (NL "Relaties ", EN "Relations ")
                       <> commaPandocAnd outputLang' rs
                       <> (str.l) (NL " worden niet gebruikt in regels. "
                                  ,EN " are not used in any rule. ")
                      ) 
     <>( case pictsWithUnusedRels of
          [pict] -> para (    hyperLinkTo pict 
                           <> (str.l) (NL " geeft een conceptueel diagram met alle relaties."
                                      ,EN " shows a conceptual diagram with all relations.")
                         )
                 <> xDefBlck env fSpec pict
          picts  -> mconcat
                       [ para (   hyperLinkTo pict
                               <> (str.l) (NL " geeft een conceptueel diagram met alle relaties die gedeclareerd zijn in "
                                          ,EN " shows a conceptual diagram with all relations declared in ")
                               <> (singleQuoted.str.name) pat <> "."
                              )
                       <> xDefBlck env fSpec pict
                       | (pict,pat)<-zip picts pats
                       ]
       )
       , pictsWithUnusedRels           -- draw the conceptual diagram
     )
     where notUsed :: [Inlines]
           notUsed = [ showMath (EDcD d)
                     | d <- Set.elems (vrels fSpec) -- only relations that are used or defined in the selected themes
                     , decusr d
                     , d `notElem` (bindedRelationsIn . vrules) fSpec
                     ]
           pats  = [ pat | pat<-vpatterns fSpec
                         , (not.null) (relsDefdIn pat Set.\\ bindedRelationsIn pat) ]
           pictsWithUnusedRels = [makePicture env fSpec (PTDeclaredInPat pat) | pat<-pats ]

  missingRules :: Blocks
  missingRules
   = case Set.elems $ vrules fSpec of
      []   -> mempty
      ruls ->
         if all hasMeaning ruls && all hasPurpose ruls
         then (para.str.l) (NL "Alle regels in dit document zijn voorzien van een uitleg."
                           ,EN "All rules in this document have been provided with a meaning and a purpose.")
         else ( case filter (not.hasPurpose) ruls of
                  []  -> mempty
                  rls -> (para.str.l) (NL "Van de volgende regels is de bestaansreden niet uitgelegd:"
                                      ,EN "Rules are defined without documenting their purpose:")
                       <> bulletList [    (para.emph.str.name) r 
                                       <> (plain.str.tshow.origin) r 
                                     | r <- rls]
              ) <>
              ( case filter (not.hasMeaning) ruls of
                  []  -> mempty
                  rls -> (para.str.l) (NL "Van de volgende regels is de betekenis uitgelegd in taal die door de computer is gegenereerd:"
                                      ,EN "Rules are defined, the meaning of which is documented by means of computer generated language:")
                       <> bulletList [    (para.emph.str.name) r 
                                       <> (plain.str.tshow.origin) r 
                                     | r <- rls]
              )
        

  ruleRelationRefTable :: Blocks
  ruleRelationRefTable =
       (para.str.l) (NL $ "Onderstaande tabel bevat per thema (dwz. patroon) tellingen van het aantal relaties en regels, " <>
                          "gevolgd door het aantal en het percentage daarvan dat een referentie bevat. Relaties die in meerdere thema's " <>
                          "gedeclareerd worden, worden ook meerdere keren geteld."
                    ,EN $ "The table below shows for each theme (i.e. pattern) the number of relations and rules, followed " <>
                          "by the number and percentage that have a reference. Relations declared in multiple themes are counted multiple " <>
                          "times."
                    )
    <> table -- No caption:
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
              <> [mkTableRow (l (NL "Gehele context", EN "Entire context")) (Set.filter decusr $ vrels fSpec) (vrules fSpec)]
             )
      
    where mkTableRow :: Text  -- The name of the pattern / fSpec 
                     -> Relations --The user-defined relations of the pattern / fSpec
                     -> Rules  -- The user-defined rules of the pattern / fSpec
                     -> [Blocks]
          mkTableRowPat p = mkTableRow (name p) (relsDefdIn p) (udefrules p)
          mkTableRow nm rels ruls =
            map (plain.str) [ nm
                            , (tshow . Set.size) rels 
                            , (tshow . Set.size) (Set.filter hasRef rels)
                            , showPercentage (Set.size rels) (Set.size . Set.filter hasRef $ rels)
                            , (tshow . Set.size) ruls
                            , (tshow . Set.size) (Set.filter hasRef ruls)
                            , showPercentage (Set.size ruls) (Set.size . Set.filter hasRef $ ruls)
                            ]

          hasRef x = (any  ((/=[]).explRefIds)) (purposesDefinedIn fSpec outputLang' x)

          showPercentage x y = if x == 0 then "-" else tshow (y*100 `div` x)<>"%"

  processrulesInPatterns :: Blocks
  processrulesInPatterns = 
       if null (fRoleRuls fSpec)
       then mempty
       else (para.str.l) (NL "Onderstaande tabel bevat een overzicht van de signaalregels per rol."
                         ,EN "The table below shows the signal rules per role."
                         ) 
         <> table -- No caption:
                  mempty
                  -- Alignment:
                  ( if multProcs
                    then replicate 3 (AlignLeft,1/3)
                    else replicate 2 (AlignLeft,1/2)
                  )
                  -- Headers:
                  (  [ (plain.str.l) (NL "rol"      , EN "role")]
                   <>[ (plain.str.l) (NL "thema", EN "in pattern") | multProcs]
                   <>[ (plain.str.l) (NL "regel"    , EN "rule")
                     ]
                  )
                  -- Rows:
                  [  [ (plain.str.name) rol]
                   <>[ (plain.str.fromMaybe "--".rrpat) rul | multProcs]
                   <>[ (plain.str.name) rul
                     , (plain.str.fromMaybe "--".rrpat) rul
                     ]
                  | (rol,rul)<-fRoleRuls fSpec]
          
     where multProcs = length (vpatterns fSpec)>1
           
  wipReport :: Blocks
  wipReport
   =   case popwork of
         []       -> (para.str.l) (NL "De populatie in dit script beschrijft geen onderhanden werk. "
                                  ,EN "The population in this script does not specify any work in progress. ")
         [(r,ps)] -> para (   (str.l) (NL"Regel ",EN "Rule")
                            <> quoterule r
                            <>(str.l) (NL $ " laat " <>count Dutch   (length ps) "taak"<>" zien."
                                      ,EN $ " shows "<>count English (length ps) "task"<>".")
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
                           , (tshow.origin) r
                           , (tshow.length) ps
                           ]
                    | (r,ps)<-popwork
                    ]
     <>
-- the tables containing the actual work in progress population
     mconcat
     [    para (  str (l (NL "Afspraak ", EN "Agreement "))
               <> hyperLinkTo (XRefSharedLangRule r)
               <> " ( " <> quoterule r <> " )"
               <> (str.l) (NL " luidt: ", EN " says: ")
               )
       <> printMeaning outputLang' r
       <> para (  (str.l) (NL "Deze regel bevat nog werk (voor "
                          ,EN "This rule contains work (for ")
                <>commaPandocOr outputLang' (map (str.name) (L.nub [rol | (rol, rul)<-fRoleRuls fSpec, r==rul]))
                <>")"
                <> case Set.toList ps of
                     [v] ->   (str.l) (NL ", te weten ", EN " by ")
                           <> oneviol r v
                           <> "."
                     _ -> (str.l) (NL $ ". De volgende tabel laat de "<>(if Set.size ps>10 then "eerste tien " else "")<>"items zien die aandacht vragen."
                                  ,EN $ "The following table shows the "<>(if Set.size ps>10 then "first ten " else "")<>"items that require attention.")
               )
       <> if Set.size ps <= 1
          then mempty -- iff there is a single violation, it is already shown in the previous paragraph
          else violtable r ps 
     | (r,ps)<- popwork ]
     where
--      text r
--       = if null expls
--         then explains2Blocks (autoMeaning outputLang' r)
--         else expls
--         where expls = [Plain (block<>[Space]) | Means l econt<-rrxpl r, l==Just outputLang' || l==Nothing, Para block<-econt]
      quoterule r
       = if T.null (name r)
         then (str.l) (NL $ "op "<>tshow (origin r)
                      ,EN $ "at "<>tshow (origin r))
         else (singleQuoted.str.name) r
      oneviol :: Rule -> AAtomPair -> Inlines
      oneviol r p
       = if isEndo (formalExpression r) && apLeft p==apRight p
         then singleQuoted (  (str.name.source.formalExpression) r 
                            <>(str.showValADL.apLeft) p
                           )
         else    "("  <> (str.name.source.formalExpression) r <> (str.showValADL.apLeft) p 
              <> ", " <> (str.name.target.formalExpression) r <> (str.showValADL.apRight) p
              <> ")"
      popwork :: [(Rule,AAtomPairs)]
      popwork = [(r,ps) | (r,ps) <- allViolations fSpec, isSignal r]

  violationReport :: Blocks
  violationReport =
        para (case (invariantViolations, processViolations) of
                ([] , [] ) ->  (str.l) (NL "De populatie in dit script overtreedt geen regels. "
                                       ,EN "The population in this script violates no rule. ")
                (iVs, pVs) ->  (str.l) (NL "De populatie in dit script overtreedt "
                                       ,EN "The population in this script violates ")
                             <>(str.tshow.length) iVs
                             <>(str.l) (NL $ " invariant"<>(if length iVs == 1 then "" else "en")<>" en "
                                       ,EN $ " invariant"<>(if length iVs == 1 then "" else "s")<>" and ")
                             <>(str.tshow.length) pVs
                             <>(str.l) (NL $ " procesregel" <>if length pVs == 1 then "" else "s"<>"."
                                       ,EN $ " process rule"<>if length pVs == 1 then "" else "s"<>"."
                           )
              )
     <> bulletList (map showViolatedRule invariantViolations)
     <> bulletList (map showViolatedRule processViolations)
    where
         (processViolations,invariantViolations) = L.partition (isSignal.fst) (allViolations fSpec)
         showViolatedRule :: (Rule,AAtomPairs) -> Blocks
         showViolatedRule (r,ps)
             =    (para.emph)
                      (  (str.l) (NL "Regel ", EN "Rule ")
                       <>(str.name) r
                      )
               <> para(  (if isSignal r 
                          then (str.l) (NL "Totaal aantal taken: "        ,EN "Total number of work items: ")
                          else (str.l) (NL "Totaal aantal overtredingen: ",EN "Total number of violations: ")
                         )
                       <>(str.tshow.length) ps
                      )
               <> table -- Caption
                        (if isSignal r
                         then   (str.l) (NL "Openstaande taken voor "     ,EN "Tasks yet to be performed by ")
                             <> commaPandocOr outputLang' (map (str.name) (L.nub [rol | (rol, rul)<-fRoleRuls fSpec, r==rul]))
                         else   (str.l) (NL "Overtredingen van invariant ",EN "Violations of invariant ")
                              <>(str.name) r
                        )  
                        -- Alignment:
                        (replicate 2 (AlignLeft,1/2))
                        -- Headers:
                        [(para.strong.text.name.source.formalExpression) r
                        ,(para.strong.text.name.target.formalExpression) r
                        ]
                        -- Rows:
                        [ [(para.text.showValADL.apLeft) p
                          ,(para.text.showValADL.apRight) p
                          ]
                        | p<- Set.elems ps]



  violtable :: Rule -> AAtomPairs -> Blocks
  violtable r ps
      = if hasantecedent r && isIdent (antecedent r)  -- note: treat 'isIdent (consequent r) as binary table.
        then table -- No caption:
                   mempty
                   -- Alignment:
                   [(AlignLeft,1.0)]
                   -- Header:
                   [(plain.str.name.source.formalExpression) r]
                   -- Data rows:
                   [ [(plain.str.showValADL.apLeft) p]
                   | p <-take 10 . Set.elems $ ps --max 10 rows
                   ]
        else table -- No caption:
                   mempty
                   -- Alignment:
                   (replicate 2 (AlignLeft,1/2))
                   -- Header:
                   [(plain.str.name.source.formalExpression) r , (plain.str.name.target.formalExpression) r ]
                   -- Data rows:
                   [ [(plain.str.showValADL.apLeft) p,(plain.str.showValADL.apRight) p]
                   | p <-take 10 . Set.elems $ ps --max 10 rows
                   ]

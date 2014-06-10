{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.ChapterProcessAnalysis
where
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
import DatabaseDesign.Ampersand.Classes
import Data.List
import DatabaseDesign.Ampersand.Output.PandocAux

--DESCR -> the process analysis contains a section for each process in the fSpec
-- If an Ampersand script contains no reference to any role whatsoever, a process analysis is meaningless.
-- In that case it will not be printed. To detect whether this is the case, we can look whether the
-- mayEdit attributes remain empty.
noProcesses :: Fspc -> Bool
noProcesses fSpec = null (fRoles fSpec)

chpProcessAnalysis :: Int -> Fspc -> Options -> (Blocks,[Picture])
chpProcessAnalysis lev fSpec flags
 = if null procs
   then (mempty,[])
   else (headerBlocks <> fromList (roleRuleBlocks ++ roleRelationBlocks ++ processSections) , pictures)
 where
  pictures = [pict | (_,picts)<-procSections procs,pict<-picts]
  procs = if null (themes fSpec)
          then vprocesses fSpec
          else [ prc | prc<-vprocesses fSpec, name prc `elem` themes fSpec ]
  processSections :: [Block]
  processSections
   = [block | (bs,_)<-procSections procs, block<-bs]

  headerBlocks :: Blocks
  headerBlocks
   = (chptHeader (fsLang fSpec) ProcessAnalysis) <>
     purposes2Blocks flags purps <> -- This explains the purpose of this context.
     fromList( 
     [ case fsLang fSpec of
         Dutch   ->
            Plain [ Str $ upCap (name fSpec)++" benoemt geen enkele rol. "
                  , Str "Een generieke rol, User, zal worden gedefinieerd om al het werk te doen wat in het bedrijfsproces moet worden uitgevoerd."
                  ]
         English ->
            Plain [ Str $ upCap (name fSpec)++" does not mention any role. "
                  , Str "A generic role, User, will be defined to do all the work that is necessary in the business process."
                  ]
     | null (fRoles fSpec)] ++
     [ case fsLang fSpec of
         Dutch   ->
            Plain [ Str $ upCap (name fSpec)++" specificeert niet welke rollen de inhoud van welke relaties mogen wijzigen. "
                  , Str ""
                  ]
         English ->
            Plain [ Str $ upCap (name fSpec)++" does not specify which roles may change the contents of which relations. "
                  , Str ""
                  ]
     | null (fRoleRels fSpec)])
     where purps = purposesDefinedIn fSpec (fsLang fSpec) fSpec

  roleRuleBlocks :: [Block]
  roleRuleBlocks
   = if null (fRoleRuls fSpec) && (not.null.udefrules) fSpec then [] else
     [ case fsLang fSpec of
          Dutch   ->
            Para [ Str $ upCap (name fSpec)++" kent regels aan rollen toe. "
                 , Str "De volgende tabel toont de regels die door een bepaalde rol worden gehandhaafd."
                 ]
          English ->
            Para [ Str $ upCap (name fSpec)++" assigns rules to roles. "
                 , Str "The following table shows the rules that are being maintained by a given role."
                 ]
-- the table containing the role-rule assignments
     , Para  $ [ RawInline (Format "latex") "\\begin{tabular}{|l|l|}\\hline\n"
               , case fsLang fSpec of
                  Dutch   -> RawInline (Format "latex") "Rol&Regel\\\\ \\hline\n"
                  English -> RawInline (Format "latex") "Role&Rule\\\\ \\hline\n"
               ]++
               [ RawInline (Format "latex") $ intercalate "\\\\ \\hline\n   " 
                       [ role++" & "++name r++
                         concat[ "\\\\\n   &"++name rul | rul<-map snd (tail rrClass)]
                       | rrClass<-eqCl fst (fRoleRuls fSpec)
                       , let role=fst (head rrClass), let r=snd (head rrClass)
                       ]
               ]++
               [ RawInline (Format "latex") "\\\\ \\hline\n\\end{tabular}"
               ]
     ]

-- the table containing the role-relation assignments
  roleRelationBlocks :: [Block]
  roleRelationBlocks
   = if null (fRoleRels fSpec) then [] else
     [ case fsLang fSpec of
          Dutch   ->
            Para [ Str $ upCap (name fSpec)++" kent rollen aan relaties toe. "
                 , Str "De volgende tabel toont de relaties waarvan de inhoud gewijzigd kan worden door iemand die een bepaalde rol vervult."
                 ]
          English ->
            Para [ Str $ upCap (name fSpec)++" assigns roles to relations. "
                 , Str "The following table shows the relations, the content of which can be altered by anyone who fulfills a given role."
                 ]
     , Para  $ [ RawInline (Format "latex") "\\begin{tabular}{|l|l|}\\hline\n"
               , RawInline (Format "latex")
                    (case  fsLang fSpec of
                       Dutch   -> "Rol&Relatie\\\\ \\hline\n"
                       English -> "Role&Relation\\\\ \\hline\n")
               ]++
               [ RawInline (Format "latex") $ intercalate "\\\\ \\hline\n   " 
                       [ role++" & $"++showMath r++"$"++
                         concat[ "\\\\\n   &$"++showMath (snd rs)++"$" | rs<-tail rrClass]
                       | rrClass<-eqCl fst (fRoleRels fSpec)
                       , let role=fst (head rrClass), let r=snd (head rrClass)
                       ]
               ]++
               [ RawInline (Format "latex") "\\\\ \\hline\n" | not (null rolelessRels)]++
               [ RawInline (Format "latex") $ intercalate "\\\\\n   " [ "&$"++showMath d++"$" | d<-rolelessRels] | not (null rolelessRels)]++
               [ RawInline (Format "latex") "\\\\ \\hline\n\\end{tabular}"
               ]
     ]
     where
      rolelessRels = [ d | d<-relsDefdIn fSpec, d `notElem` (nub.map snd) (fRoleRels fSpec) ]

  emptyProcess :: Process -> Bool
  emptyProcess p = null (udefrules p)
  
-- the sections in which processes are analyzed
  procSections :: [FProcess] -> [([Block],[Picture])]
  procSections fprocs = iterat [fp |fp<-fprocs, (not.emptyProcess.fpProc) fp] 1 declaredConcepts  declaredRelations
   where
    declaredRelations = (concatMap relsDefdIn.map fpProc.vprocesses) fSpec
    declaredConcepts  = (concs.map fpProc.vprocesses) fSpec
    iterat :: [FProcess] -> Int -> [A_Concept] -> [Declaration] -> [([Block],[Picture])]
    iterat [] _ _ _ = []
    iterat (fproc:fps) i seenConcepts seenDeclarations
     = (  toList (labeledThing flags (lev+1) (xLabel ProcessAnalysis++"_"++name fproc) (name fproc))    -- new section to explain this theme
       ++ toList (purposes2Blocks flags (purposesDefinedIn fSpec (fsLang fSpec) fproc))  
       ++ txtProcessModel fproc
   --    ++ txtLangModel fproc
       ++ (if null sctRules then [] else [DefinitionList sctRules])
       , [picProcessModel fproc]):  iterat fps i' seenCrs seenDrs
       where
         sctRules :: [([Inline], [[Block]])]
         (sctRules,i',seenCrs,seenDrs) = dpRule fSpec flags (udefrules (fpProc fproc)) i seenConcepts seenDeclarations

--  txtLangModel :: FProcess->[Block]
--  txtLangModel fp
--   = if not (genGraphics flags) then [] else
--     -- (if name ifc==name (head (fActivities fSpec)) then processModelIntro else [])++
--      [Para (case fsLang fSpec of                                     -- announce the conceptual diagram
--             Dutch   -> [ Str "Het conceptueel diagram in figuur ", xrefReference pict
--                        , Str " geeft een overzicht van de taal waarin dit proces wordt uitgedrukt."]
--             English -> [ Str "The conceptual diagram of figure ", xrefReference pict
--                        , Str " provides an overview of the language in which this process is expressed."])
--      ,Plain ((toList . showImage flags) pict)]                     -- draw the diagram
--     where pict = picLangModel fp

  txtProcessModel :: FProcess->[Block]
  txtProcessModel p
   = if not (genGraphics flags) then [] else
     -- (if name ifc==name (head (fActivities fSpec)) then processModelIntro else [])++
     [Para (case fsLang fSpec of                                     -- announce the processModel diagram
             Dutch   -> [ Str "Figuur ", xrefReference pict
                        , Str " geeft het procesmodel weer."]
             English -> [ Str "Figure ", xrefReference pict
                        , Str " shows the process model."])
     ,Plain ((toList . showImage flags) pict)]                     -- draw the diagram
     where pict = picProcessModel p



  -- | the Picture that represents this interface's knowledge graph with only those relations that are used in rules (controlled by Plain_CG).
  picProcessModel :: FProcess->Picture
  picProcessModel fproc = makePicture flags fSpec (PTProcess fproc) 



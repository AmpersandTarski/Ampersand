{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Design.Ampersand.Output.ToPandoc.ChapterProcessAnalysis
where
import Database.Design.Ampersand.Output.ToPandoc.SharedAmongChapters
import Database.Design.Ampersand.Classes
import Data.List
import Database.Design.Ampersand.Output.PandocAux

--DESCR -> the process analysis contains a section for each process in the fSpec
-- If an Ampersand script contains no reference to any role whatsoever, a process analysis is meaningless.
-- In that case it will not be printed. To detect whether this is the case, we can look whether the
-- mayEdit attributes remain empty.
noProcesses :: FSpec -> Bool
noProcesses fSpec = null (fRoles fSpec)

chpProcessAnalysis :: Int -> FSpec -> (Blocks,[Picture])
chpProcessAnalysis lev fSpec
 = ( if null procs
     then mempty
     else headerBlocks <> roleRuleBlocks <> fromList roleRelationBlocks <> processSections
   , pictures)
 where
  pictures = [] -- Disabled the pictures, because they are not helpful. (They look like a big ball of Mud)  (map picProcessModel procs)
  procs = if null (themes fSpec)
          then vprocesses fSpec
          else [ prc | prc<-vprocesses fSpec, name prc `elem` themes fSpec ]
  processSections :: Blocks
  processSections = mconcat (procSections procs)

  headerBlocks :: Blocks
  headerBlocks
   = (chptHeader (fsLang fSpec) ProcessAnalysis) <>
     purposes2Blocks (getOpts fSpec) purps <> -- This explains the purpose of this context.
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

  roleRuleBlocks :: Blocks
  roleRuleBlocks
   = if null (fRoleRuls fSpec) && (not.null.udefrules) fSpec then mempty else
      (case fsLang fSpec of
          Dutch   ->
            para ( (str.upCap.name) fSpec <> " kent regels aan rollen toe. "
                 <> "De volgende tabel toont de regels die door een bepaalde rol worden gehandhaafd."
                 )
          English ->
            para ( (str.upCap.name) fSpec <> " assigns rules to roles. "
                 <> "The following table shows the rules that are being maintained by a given role."
                 )
-- the table containing the role-rule assignments
     )<>
     fromList
     [ Para  $ [ RawInline (Format "latex") "\\begin{tabular}{|l|l|}\\hline\n"
               , case fsLang fSpec of
                  Dutch   -> RawInline (Format "latex") "Rol&Regel\\\\ \\hline\n"
                  English -> RawInline (Format "latex") "Role&Rule\\\\ \\hline\n"
               ]++
               [ RawInline (Format "latex") $ intercalate "\\\\ \\hline\n   "
                       [ latexEscShw role++" & "++latexEscShw (name r)++
                         concat[ "\\\\\n   &"++latexEscShw  (name rul) | rul<-map snd (tail rrClass)]
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

-- the sections in which processes are analyzed
  procSections :: [FProcess] -> [Blocks]
  procSections fprocs = iterat [fp |fp<-fprocs, (not.null.udefrules.fpProc) fp] 1 declaredConcepts  declaredRelations
   where
    declaredRelations = (concatMap relsDefdIn.map fpProc.vprocesses) fSpec
    declaredConcepts  = (concs.map fpProc.vprocesses) fSpec
    iterat :: [FProcess] -> Int -> [A_Concept] -> [Declaration] -> [Blocks]
    iterat [] _ _ _ = mempty
    iterat (fproc:fps) i seenConcepts seenDeclarations
     = (  (labeledThing (getOpts fSpec) (lev+1) (xLabel ProcessAnalysis++"_"++name fproc) (name fproc))    -- new section to explain this theme
       <> (purposes2Blocks (getOpts fSpec) (purposesDefinedIn fSpec (fsLang fSpec) fproc))
   --    <> (txtProcessModel fproc)
       <> (if null sctRules then mempty else definitionList sctRules)
       ):  iterat fps i' seenCrs seenDrs
       where
         sctRules :: [(Inlines, [Blocks])]
         (sctRules,i',seenCrs,seenDrs) = dpRule fSpec(udefrules (fpProc fproc)) i seenConcepts seenDeclarations

  txtProcessModel :: FProcess->Blocks
  txtProcessModel p
   = if not (genGraphics (getOpts fSpec)) || True -- temporarily disabled picture, because it currently is a big ball of mud, which takes too long to generate
     then mempty
     else
        (case fsLang fSpec of                                     -- announce the processModel diagram
             Dutch   -> para ( "Figuur " <> xRefReference (getOpts fSpec) pict <> " geeft het procesmodel weer.")
             English -> para ( "Figure " <> xRefReference (getOpts fSpec) pict <> " shows the process model.")
        ) <>
        plain (showImage (getOpts fSpec) pict)
     where pict = picProcessModel p

  -- | the Picture that represents this interface's knowledge graph with only those relations that are used in rules (controlled by Plain_CG).
  picProcessModel :: FProcess->Picture
  picProcessModel fproc = makePicture fSpec (PTProcess fproc)


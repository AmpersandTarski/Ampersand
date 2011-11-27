{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.ChapterProcessAnalysis
where
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
import DatabaseDesign.Ampersand.Basics  
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.Classes
import Data.List
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Fspec.Fspec
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Output.PandocAux

--DESCR -> the process analysis contains a section for each process in the fspec
-- If an Ampersand script contains no reference to any role whatsoever, a process analysis is meaningless.
-- In that case it will not be printed. To detect whether this is the case, we can look whether the
-- mayEdit attributes remain empty.
noProcesses :: Fspc -> Bool
noProcesses fSpec = null (fRoleRels fSpec) && null (fRoleRuls fSpec)

chpProcessAnalysis :: Int -> Fspc -> Options -> ([Block],[Picture])
chpProcessAnalysis lev fSpec flags
 = if null procs
   then ([],[])
   else (header ++ roleRuleBlocks ++ roleRelationBlocks ++ processSections , pictures)
 where
  pictures = [pict | (_,picts)<-procSections procs,pict<-picts]
  procs = if null (themes fSpec)
          then vprocesses fSpec
          else [ prc | prc<-vprocesses fSpec, name prc `elem` themes fSpec ]
  processSections :: [Block]
  processSections
   = [block | (bs,_)<-procSections procs, block<-bs]

  header :: [Block]
  header
   = labeledHeader lev (xLabel ProcessAnalisys)
                                       (case language flags of
                                              Dutch   ->  "Procesanalyse"   
                                              English ->  "Process Analysis"
                                        ) ++
     purposes2Blocks purps ++ -- This explains the purpose of this context.
     [ case language flags of
         Dutch   ->
            Plain [ Str $ upCap (name fSpec)++" kent geen regels aan rollen toe. "
                  , Str "Een generieke rol, User, zal worden gedefinieerd om al het werk te doen wat in het bedrijfsproces moet worden uitgevoerd."
                  ]
         English ->
            Plain [ Str $ upCap (name fSpec)++" does not assign rules to roles. "
                  , Str "A generic role, User, will be defined to do all the work that is necessary in the business process."
                  ]
     | null (fRoleRuls fSpec)] ++
     [ case language flags of
         Dutch   ->
            Plain [ Str $ upCap (name fSpec)++" specificeert niet welke rollen de inhoud van welke relaties mogen wijzigen. "
                  , Str ""
                  ]
         English ->
            Plain [ Str $ upCap (name fSpec)++" does not specify which roles may change the contents of which relations. "
                  , Str ""
                  ]
     | null (fRoleRels fSpec)]
     where purps = purposes fSpec (language flags) fSpec

  roleRuleBlocks :: [Block]
  roleRuleBlocks
   = if null (fRoleRuls fSpec) && (not.null.rules) fSpec then [] else
     [ case language flags of
          Dutch   ->
            Para [ Str $ upCap (name fSpec)++" kent regels aan rollen toe. "
                 , Str "De volgende tabel toont de regels die door een bepaalde rol worden gehandhaafd."
                 ]
          English ->
            Para [ Str $ upCap (name fSpec)++" assigns rules to roles. "
                 , Str "The following table shows the rules that are being maintained by a given role."
                 ]
-- the table containing the role-rule assignments
     , Para  $ [ RawInline "latex" "\\begin{tabular}{|l|l|}\\hline\n"
               , case language flags of
                  Dutch   -> RawInline "latex" "Rol&Regel\\\\ \\hline\n"
                  English -> RawInline "latex" "Role&Rule\\\\ \\hline\n"
               ]++
               [ RawInline "latex" $ intercalate "\\\\ \\hline\n   " 
                       [ role++" & "++name r++
                         concat[ "\\\\\n   &"++name rul | rul<-map snd (tail rrClass)]
                       | rrClass<-eqCl fst (fRoleRuls fSpec)
                       , let role=fst (head rrClass), let r=snd (head rrClass)
                       ]
               ]++
               [ RawInline "latex" "\\\\ \\hline\n\\end{tabular}"
               ]
     ]

-- the table containing the role-relation assignments
  roleRelationBlocks :: [Block]
  roleRelationBlocks
   = if null (fRoleRels fSpec) then [] else
     [ case language flags of
          Dutch   ->
            Para [ Str $ upCap (name fSpec)++" kent rollen aan relaties toe. "
                 , Str "De volgende tabel toont de relaties waarvan de inhoud gewijzigd kan worden door iemand die een bepaalde rol vervult."
                 ]
          English ->
            Para [ Str $ upCap (name fSpec)++" assigns roles to relations. "
                 , Str "The following table shows the relations, the content of which can be altered by anyone who fulfills a given role."
                 ]
     , Para  $ [ RawInline "latex" "\\begin{tabular}{|l|l|}\\hline\n"
               , RawInline "latex"
                    (case  language flags of
                       Dutch   -> "Rol&Relatie\\\\ \\hline\n"
                       English -> "Role&Relation\\\\ \\hline\n")
               ]++
               [ RawInline "latex" $ intercalate "\\\\ \\hline\n   " 
                       [ role++" & $"++showMath r++"$"++
                         concat[ "\\\\\n   &$"++showMath (snd rs)++"$" | rs<-tail rrClass]
                       | rrClass<-eqCl fst (fRoleRels fSpec)
                       , let role=fst (head rrClass), let r=snd (head rrClass)
                       ]
               ]++
               [ RawInline "latex" "\\\\ \\hline\n" | not (null rolelessRels)]++
               [ RawInline "latex" $ intercalate "\\\\\n   " [ "&$"++showMath d++"$" | d<-rolelessRels] | not (null rolelessRels)]++
               [ RawInline "latex" "\\\\ \\hline\n\\end{tabular}"
               ]
     ]
     where
      rolelessRels = [ d | d<-declarations fSpec, d `notElem` (nub.map (makeDeclaration.snd)) (fRoleRels fSpec) ]

  emptyProcess :: Process -> Bool
  emptyProcess p = null (rules p)
  
-- the sections in which processes are analyzed
  procSections :: [FProcess] -> [([Block],[Picture])]
  procSections fprocs = iterat [fp |fp<-fprocs, not (emptyProcess (proc fp))] 1 (concs (patterns fSpec)) ((concatMap declarations.patterns) fSpec)
   where
    iterat :: [FProcess] -> Int -> [A_Concept] -> [Declaration] -> [([Block],[Picture])]
    iterat [] _ _ _ = []
    iterat (fproc:fps) i seenConcepts seenDeclarations
     = ( [Header (lev+1) [Str (name fproc)]]    -- new section to explain this theme
       ++ sctMotivation                       -- The section startss with the reason why this process exists,
       ++ txtProcessModel fproc
       ++ txtLangModel fproc
       ++ (if null sctRules then [] else [DefinitionList sctRules])
       , [picProcessModel fproc, picLangModel fproc]):  iterat fps i' seenCrs seenDrs
       where
         sctMotivation
          = purposes2Blocks purps
         purps = purposes fSpec (language flags) fproc
         sctRules  :: [([Inline], [[Block]])]
         (sctRules,i',seenCrs,seenDrs) = dpRule fSpec flags (rules (proc fproc)) i seenConcepts seenDeclarations

  txtLangModel :: FProcess->[Block]
  txtLangModel fp
   = if not (genGraphics flags) then [] else
     -- (if name ifc==name (head (fActivities fSpec)) then processModelIntro else [])++
      [Para (case language flags of                                     -- announce the conceptual diagram
             Dutch   -> [ Str "Het conceptueel diagram in figuur ", xrefReference pict
                        , Str " geeft een overzicht van de taal waarin dit proces wordt uitgedrukt."]
             English -> [ Str "The conceptual diagram of figure ", xrefReference pict
                        , Str " provides an overview of the language in which this process is expressed."])
      ,Plain (xrefFigure1 pict)]                     -- draw the diagram
     where pict = picLangModel fp

  txtProcessModel :: FProcess->[Block]
  txtProcessModel p
   = if not (genGraphics flags) then [] else
     -- (if name ifc==name (head (fActivities fSpec)) then processModelIntro else [])++
     [Para (case language flags of                                     -- announce the processModel diagram
             Dutch   -> [ Str "Figuur ", xrefReference pict
                        , Str " geeft het procesmodel weer."]
             English -> [ Str "Figure ", xrefReference pict
                        , Str " shows the process model."])
     ,Plain (xrefFigure1 pict)]                     -- draw the diagram
     where pict = picProcessModel p

  picLangModel :: FProcess->Picture
  picLangModel fproc
   = ((makePictureObj flags (name fproc) PTProcLang . printDotGraph . conceptualGraph fSpec flags Rel_CG) fproc)   -- the Picture that represents this process's knowledge graph with all user defined relations (controlled by Rel_CG)
                {caption = case language flags of
                            Dutch   ->"Basiszinnen van "++name fproc
                            English ->"Basic sentences of "++name fproc}

  picProcessModel :: FProcess->Picture
  picProcessModel fproc
   = (makePicture flags fSpec Plain_CG fproc) -- the Picture that represents this interface's knowledge graph with only those relations that are used in rules (controlled by Plain_CG).
        {caption = case language flags of
                    Dutch   ->"Procesmodel van "++name fproc
                    English ->"Process model of "++name fproc}



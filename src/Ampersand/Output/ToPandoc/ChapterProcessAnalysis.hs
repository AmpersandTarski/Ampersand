{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.ToPandoc.ChapterProcessAnalysis
where
import Ampersand.Output.ToPandoc.SharedAmongChapters
import Data.List

--DESCR -> the process analysis contains a section for each process in the fSpec
-- If an Ampersand script contains no reference to any role whatsoever, a process analysis is meaningless.
-- In that case it will not be printed. To detect whether this is the case, we can look whether the
-- mayEdit attributes remain empty.
noProcesses :: FSpec -> Bool
noProcesses fSpec = null (fRoles fSpec)

chpProcessAnalysis :: FSpec -> Blocks
chpProcessAnalysis fSpec
 = case vpatterns fSpec of
     [] -> mempty
     ps ->    headerBlocks 
           <> roleRuleBlocks
           <> fromList roleRelationBlocks 
           <> mconcat (procSections ps)
   
 where
  -- shorthand for easy localizing    
  l :: LocalizedStr -> String
  l = localize (fsLang fSpec)

  headerBlocks :: Blocks
  headerBlocks
   = xDefBlck fSpec ProcessAnalysis <>
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
   = if null (fRoleRuls fSpec) && (not.null.vrules) fSpec then mempty else
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
      table -- Caption: 
            ((str.l) (NL "regeltoewijzingen", EN "role-rule assignments"))
            -- Alignment:
            (replicate 2 (AlignLeft, 1/2))
            -- Header:
            (map (plain.str.l) 
               [ (NL "Rol"  , EN "Role")
               , (NL "Regel", EN "Rule")
               ])
            -- Data rows:
            ( [map (plain.str) 
                 [ name role'
                 , name rul
                 ]
              | (role',rul)<-sort $ fRoleRuls fSpec
              ]
            )

-- the table containing the role-relation assignments
  roleRelationBlocks :: [Block]
  roleRelationBlocks = [] --BITROTTED (because to much specific LaTeX stuff, in a chapter that isn't fit for use at the moment.)

-- the sections in which processes are analyzed
  procSections :: [Pattern] -> [Blocks]
  procSections fprocs = iterat [fp |fp<-fprocs, (not.null.udefrules) fp] 1 declaredConcepts  declaredRelations
   where
    declaredRelations = (concatMap relsDefdIn.vpatterns) fSpec
    declaredConcepts  = (concs.vpatterns) fSpec
    iterat :: [Pattern] -> Int -> [A_Concept] -> [Relation] -> [Blocks]
    iterat [] _ _ _ = mempty
    iterat (fproc:fps) i seenConcepts seenRelations
     = (
           xDefBlck fSpec (XRefProcessAnalysis fproc) 
        <> purposes2Blocks (getOpts fSpec) (purposesDefinedIn fSpec (fsLang fSpec) fproc)
        <> (if null sctRules then mempty else definitionList sctRules)
       ):  iterat fps i' seenCrs seenDrs
       where
         sctRules :: [(Inlines, [Blocks])]
         (sctRules,i',seenCrs,seenDrs) = dpRule' fSpec (udefrules fproc) i seenConcepts seenRelations


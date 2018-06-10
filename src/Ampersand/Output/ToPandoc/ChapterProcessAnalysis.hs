{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.ToPandoc.ChapterProcessAnalysis(chpProcessAnalysis)
where
import           Ampersand.Output.ToPandoc.SharedAmongChapters
import           Data.List
import qualified Data.Set as Set

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
     if null (fRoles fSpec)
     then
      plain (  fnm
            <> (str.l) (NL " benoemt geen enkele rol. "
                       ,EN " does not mention any role. ")
            <> (str.l) (NL "Een generieke rol, User, zal worden gedefinieerd om al het werk te doen wat in het bedrijfsproces moet worden uitgevoerd."
                       ,EN "A generic role, User, will be defined to do all the work that is necessary in the business process.")
            ) 
     else
      plain (  fnm
            <> (str.l) (NL " specificeert niet welke rollen de inhoud van welke relaties mogen wijzigen. "
                       ,EN " does not specify which roles may change the contents of which relations. ")
            )
     where purps = purposesDefinedIn fSpec (fsLang fSpec) fSpec
           
  fnm :: Inlines
  fnm   = str . upCap . name $ fSpec
  roleRuleBlocks :: Blocks
  roleRuleBlocks
   = if null (fRoleRuls fSpec) && (not.null.vrules) fSpec then mempty else
      (case fsLang fSpec of
          Dutch   ->
            para ( fnm <> " kent regels aan rollen toe. "
                 <> "De volgende tabel toont de regels die door een bepaalde rol worden gehandhaafd."
                 )
          English ->
            para ( fnm <> " assigns rules to roles. "
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
    declaredRelations = relsDefdIn . vpatterns $ fSpec
    declaredConcepts  = concs . vpatterns $ fSpec
    iterat :: [Pattern] -> Int -> A_Concepts -> Relations -> [Blocks]
    iterat [] _ _ _ = mempty
    iterat (fproc:fps) i seenConcepts seenRelations
     = (
           xDefBlck fSpec (XRefProcessAnalysis fproc) 
        <> purposes2Blocks (getOpts fSpec) (purposesDefinedIn fSpec (fsLang fSpec) fproc)
        <> (if null sctRules then mempty else definitionList sctRules)
       ):  iterat fps i' seenCrs seenDrs
       where
         sctRules :: [(Inlines, [Blocks])]
         (sctRules,i',seenCrs,seenDrs) = dpRule' fSpec (Set.elems $ udefrules fproc) i seenConcepts seenRelations


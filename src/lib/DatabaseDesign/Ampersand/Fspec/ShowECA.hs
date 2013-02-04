{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec.ShowECA (showECA) where
   import DatabaseDesign.Ampersand.Fspec.Fspec
   import DatabaseDesign.Ampersand.Basics                       (fatalMsg,Identified(..))
   import DatabaseDesign.Ampersand.Fspec.ShowADL            (ShowADL(..))
   import Data.List (intercalate)

   fatal :: Int -> String -> a
   fatal = fatalMsg "Fspec.ShowECA"


   class ECA a where 
    showECA :: Fspc -> String -> a -> String

   instance ECA ECArule where
    showECA fSpec indent er = showECA fSpec indent (ecaTriggr er)++ " EXECUTE    -- (ECA rule "
                                                                 ++ show (ecaNum er)
                                                                 ++ ")"
                                                                 ++
                              indent++showECA fSpec indent (ecaAction er)
   instance ECA Event where
    showECA _ _ (On Ins rel) = "ON INSERT Delta IN " ++ showADL rel
    showECA _ _ (On Del rel) = "ON DELETE Delta FROM " ++ showADL rel

   instance ECA PAclause where
    showECA _ = showPAclause
     where
      showPAclause :: String -> PAclause -> String
      showPAclause indent pa@Do{}
       = ( case paSrt pa of
            Ins -> "INSERT INTO "
            Del -> "DELETE FROM ")++
         showADL (paTo pa)++
         " SELECTFROM"++indent++"  "++
         showADL (paDelta pa)++
         motivate indent "TO MAINTAIN" (paMotiv pa)
      showPAclause indent (New c clause cj_ruls)
       = "CREATE x:"++show c++";"++indent'++showPAclause indent' (clause "x")++motivate indent "MAINTAINING" cj_ruls
         where indent'  = indent++"  "
      showPAclause indent (Rmv c clause cj_ruls)
       = "REMOVE x:"++show c++";"++indent'++showPAclause indent' (clause "x")++motivate indent "MAINTAINING" cj_ruls
         where indent'  = indent++"  "
      showPAclause indent (Sel c e r cj_ruls)
       = "SELECT x:"++show c++" FROM codomain("++ showADL e ++");"
                 ++indent'++showPAclause indent' (r "x")++motivate indent "MAINTAINING" cj_ruls
         where indent'  = indent++"  "
      showPAclause indent (CHC ds cj_ruls)
       = "ONE of "++intercalate indent' [showPAclause indent' d | d<-ds]++motivate indent "MAINTAINING" cj_ruls
         where indent'  = indent++"       "
      showPAclause indent (ALL ds cj_ruls)
       = "ALL of "++intercalate indent' [showPAclause indent' d | d<-ds]++motivate indent "MAINTAINING" cj_ruls
         where indent'  = indent++"       "
      showPAclause indent (Nop cj_ruls)
       = "DO NOTHING"++motivate indent "TO MAINTAIN" cj_ruls
      showPAclause indent (Blk cj_ruls)
       = "BLOCK"++motivate indent "CANNOT CHANGE" cj_ruls
      showPAclause  _ (Let _ _ _)  = fatal 55 "showPAclause is missing for `Let`. Contact your dealer!"
      showPAclause  _ (Ref _)      = fatal 56 "showPAclause is missing for `Ref`. Contact your dealer!"
                     
      motivate indent motive motives = concat [ indent++showConj cj_rul | cj_rul<-motives ]
         where showConj (conj,rs) = "("++motive++" "++showADL conj++" FROM "++intercalate ", " (map name rs) ++")"

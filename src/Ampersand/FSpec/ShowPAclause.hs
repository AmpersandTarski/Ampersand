{-# LANGUAGE FlexibleInstances #-}
  -- The purpose of ShowPAclause is to print things in Ampersand source format.
  -- Rule: The semantics of each fSpec produced by the compiler is identical to the semantics  of (parse (showPAclause fSpec)).
  -- Rule: The standard show is used only for simple error messages during testing.
  -- Question (SJC): If STRING is the code produced by showPAclause, would STRING == showPAclause (parse STRING) (context (parse STRING)) be true?
  -- Answer (SJ):   No, not for every STRING. Yet, for every fSpec we want  semantics fSpec == semantics (parse (showPAclause fSpec)).
  --                Note that 'parse' and 'semantics' do not exist in this shape, so the actual expression is slightly more complicated.
  --
  -- Every Expression should be disambiguated before printing to ensure unambiguity.
module Ampersand.FSpec.ShowPAclause
    ( showPAclause)
where
import Ampersand.Core.ParseTree
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Core.ShowAStruct
import Ampersand.Basics      (fatal,Named(..))
import Data.List
import Prelude

showPAclause :: String -> PAclause -> String
showPAclause indent pa@Do{}
       = ( case paSrt pa of
            Ins -> "INSERT INTO "
            Del -> "DELETE FROM ")++
         showA (paTo pa) ++
         indent++" SELECTFROM "++
         showA (paDelta pa)++
         indent++motivate indent "TO MAINTAIN " (paMotiv pa)
showPAclause indent (New c clause cj_ruls)
       = "NEW x:"++show c++";"++indent'++showPAclause indent' (clause (makePSingleton "x"))++motivate indent "MAINTAINING" cj_ruls
         where indent'  = indent++"  "
showPAclause indent (Rmv c clause cj_ruls)
       = "REMOVE x:"++show c++";"++indent'++showPAclause indent' (clause (makePSingleton "x"))++motivate indent "MAINTAINING" cj_ruls
         where indent'  = indent++"  "
showPAclause indent (CHC ds cj_ruls)
       = "ONE OF "++intercalate indent' [showPAclause indent' d | d<-ds]++motivate indent "MAINTAINING" cj_ruls
         where indent'  = indent++"       "
showPAclause indent (GCH ds cj_ruls)
       = "ONE NONEMPTY ALTERNATIVE OF "++intercalate indent'
         ["PICK a,b FROM "++showA links++indent'++"THEN "++showPAclause (indent'++"     ") p| (_,links,p)<-ds]++
         motivate indent "MAINTAINING" cj_ruls
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

motivate :: [Char] -> [Char] -> [(Expression, [Rule])] -> [Char]
motivate indent motive motives = concat [ indent++showConj cj_rul | cj_rul<-motives ]
   where showConj (conj,rs) = "("++motive++" "++showA conj++" FROM "++intercalate ", " (map name rs) ++")"
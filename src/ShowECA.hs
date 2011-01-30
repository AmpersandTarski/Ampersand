{-# OPTIONS_GHC -Wall #-}
module ShowECA (showECA) where
   import Data.Fspec
   import Ampersand
   import ShowADL            (ShowADL(..))
--   import DatabaseDesign.Ampersand.Core.Basics         (Collection (rd))
   
   class ECA a where 
    showECA :: Fspc -> String -> a -> String

   instance (Identified c, Eq c, Show c, ShowADL c) => ECA (ECArule c) where
    showECA fSpec indent er = showECA fSpec indent (ecaTriggr er)++" EXECUTE    -- (ECA rule "++show (ecaNum er)++")"++
                              indent++showECA fSpec indent (ecaAction er)

   instance (Identified c, Eq c, ShowADL c) => ECA (Event c) where
    showECA fSpec _ (On Ins m') = "ON INSERT Delta IN "++showADLcode fSpec m'
    showECA fSpec _ (On Del m') = "ON DELETE Delta FROM "++showADLcode fSpec m'

   instance Show r => ECA (PAclause r) where
    showECA _ _ p = show p 
--     where
--      showFragm indent pa@Do{}
--       = ( case paSrt pa of
--            Ins -> "INSERT INTO "
--            Del -> "DELETE FROM ")++
--         showADLcode fSpec (paTo pa)++
--         " SELECTFROM "++
--         showADLcode fSpec (paDelta pa)
--         ++motivate indent "TO MAINTAIN" (paMotiv pa)
--      showFragm indent (New c clause m) = "CREATE x:"++show c++";"++indent++"    "++showECA fSpec (indent++"    ") (clause "x")++motivate indent "MAINTAINING" m
--      showFragm indent (Rmv c clause m) = "REMOVE x:"++show c++";"++indent++"    "++showECA fSpec (indent++"    ") (clause "x")++motivate indent "MAINTAINING" m
--      showFragm indent (Sel c e r m)    = "SELECT x:"++show c++" FROM codomain("++showADLcode fSpec e++");"
--                                          ++indent++"    "++showECA fSpec (indent++"    ") (r "x")++motivate indent "MAINTAINING" m
--      showFragm indent (Chc ds m)       = "ONE of "++concat [indent++"       "++showFragm (indent++"       ") d| d<-ds]++motivate indent "MAINTAINING" m
--      showFragm indent (All ds m)       = "ALL of "++concat [indent++"       "++showFragm (indent++"       ") d| d<-ds]++motivate indent "MAINTAINING" m
--      showFragm indent (Nop m)          = "DO NOTHING"++motivate indent "TO MAINTAIN" m
--      showFragm indent (Blk m)          = "BLOCK"++motivate indent "CANNOT CHANGE" m
--
--      motivate indent motive motives = concat [ indent++showConj m | m<-motives ]
--       where showConj (conj,rs) = "("++motive++" "++showADLcode fSpec conj++" FROM "++commaEng "" ["R"++show (nr r)| r<-rs]++")"

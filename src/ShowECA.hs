{-# OPTIONS_GHC -Wall #-}
module ShowECA (showECA) where
   import Strings (commaEng)
   import Data.Fspec
   import Adl
   import ShowADL            (showADLcode)
--   import Collection         (Collection (rd))
   
   class ECA a where 
    showECA :: Fspc -> String -> a -> String

   instance ECA ECArule where
    showECA fSpec indent er = showECA fSpec indent (ecaTriggr er)++" EXECUTE    -- (ECA rule "++show (ecaNum er)++")"++
                              indent++showECA fSpec indent (ecaAction er)

   instance ECA Event where
    showECA fSpec _ (On Ins m') = "ON INSERT Delta IN   "++showADLcode fSpec m'
    showECA fSpec _ (On Del m') = "ON DELETE Delta FROM "++showADLcode fSpec m'

   instance ECA PAclause where
    showECA fSpec ind p = showFragm ind p 
     where
      showFragm indent pa@Do{}
       = ( case paSrt pa of
            Ins -> "INSERT INTO "
            Del -> "DELETE FROM ")++
         showADLcode fSpec (paTo pa)++
         " SELECTFROM "++
         showADLcode fSpec (paDelta pa)
         ++motivate indent "TO MAINTAIN" (paMotiv pa)
      showFragm indent (New c clause m) = "CREATE x:"++show c++";"++indent++"    "++showECA fSpec (indent++"    ") (clause "x")++motivate indent "MAINTAINING" m
      showFragm indent (Rmv c clause m) = "REMOVE x:"++show c++";"++indent++"    "++showECA fSpec (indent++"    ") (clause "x")++motivate indent "MAINTAINING" m
      showFragm indent (Sel c e r m)    = "SELECT x:"++show c++" FROM codomain("++showADLcode fSpec e++");"
                                          ++indent++"    "++showECA fSpec (indent++"    ") (r "x")++motivate indent "MAINTAINING" m
      showFragm indent (Chc ds m)       = "ONE of "++concat [indent++"       "++showFragm (indent++"       ") d| d<-ds]++motivate indent "MAINTAINING" m
      showFragm indent (All ds m)       = "ALL of "++concat [indent++"       "++showFragm (indent++"       ") d| d<-ds]++motivate indent "MAINTAINING" m
      showFragm indent (Nop m)          = "DO NOTHING"++motivate indent "TO MAINTAIN" m
      showFragm indent (Blk m)          = "BLOCK"++motivate indent "CANNOT CHANGE" m
      showFragm indent (Dry m)          = "BLOCK"++motivate indent "NO RULES TO HANDLE" m

      motivate indent motive motives = concat [ indent++showConj m | m<-motives ]
       where showConj (conj,rs) = "("++motive++" "++showADLcode fSpec conj++" FROM "++commaEng "" ["R"++show (nr r)| r<-rs]++")"
      

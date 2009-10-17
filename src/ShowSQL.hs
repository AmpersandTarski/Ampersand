{-# OPTIONS_GHC -Wall #-}
module ShowSQL (showSQL) where
   import Data.Fspec
   import Strings (chain)
   import Adl
   import ShowADL            (showADL)
   import Collection         (Collection (rd))
   
   class SQL a where 
    showSQL :: a -> String

   instance SQL ECArule where
    showSQL (ECA event pa n) = (showSQL event++"\nEXECUTE "++showSQL pa)
   instance SQL Event where
    showSQL (On Ins m') = "ON INSERT Delta IN   "++show m'
    showSQL (On Del m') = "ON DELETE Delta FROM "++show m'

   instance SQL PAclause where
    showSQL fragm = showFragm "\n        " fragm
     where
      showFragm _ (Do Ins tExpr delt) = "INSERT INTO "++sh tExpr++" SELECTFROM "++sh delt
      showFragm _ (Do Del tExpr delt) = "DELETE FROM "++sh tExpr++" SELECTFROM "++sh delt
      showFragm _ (New _) = error "In the definition of `showFragm': Patterns not matched: _ (New _)" -- WAAROM? Stef, was deze vergeten?
      showFragm indent (Choice ds)
        = "ONE of "++chain (indent++"       ") [showFragm (indent++"       ") d | d<-ds]
      showFragm indent (All ds)
        = "ALL of "++chain (indent++"       ") [showFragm (indent++"       ") d | d<-ds]
      sh x = if isTrue x then "V["++(chain ",".rd.map name) [source x,target x]++"]" else showADL x


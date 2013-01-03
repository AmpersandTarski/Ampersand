{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec.FPA 

where
   
import DatabaseDesign.Ampersand.Misc (Lang(..))
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.Fspec.Fspec
import DatabaseDesign.Ampersand.Classes

-------------- Function Points ------------------

class FPAble a where
  fpa :: a->FPA
  fPoints :: a ->Int
  fPoints x = fPoints'(fpa x)
  
instance FPAble PlugSQL where
  fpa x = FPA ILGV $
    case x of
      TblSQL{}    | (length.fields) x < 2 -> Eenvoudig
                  | (length.fields) x < 6 -> Gemiddeld
                  | otherwise             -> Moeilijk 
      BinSQL{}    -> Gemiddeld
      ScalarSQL{} -> Eenvoudig  

instance FPAble Interface where
  fpa x 
     | (not.null.ifcParams) x  = FPA IF complxy  -- In case there are editeble relations, this must be an import function. 
     | (isUni.objctx.ifcObj) x = FPA OF complxy  -- If there is max one atom, this is a simple function. 
     | otherwise               = FPA UF complxy  -- Otherwise, it is a UF
   
    where 
      complxy = case depth (ifcObj x) of
                   0 -> Eenvoudig
                   1 -> Eenvoudig
                   2 -> Gemiddeld
                   _ -> Moeilijk 
      depth :: ObjectDef -> Int
      depth Obj{objmsub=Nothing} = 0
      depth Obj{objmsub=Just si}
         = case si of
             Box os          -> 1 + maximum (map depth os) 
             InterfaceRef{} -> 1
                  

class ShowLang a where
  showLang :: Lang -> a -> String

instance ShowLang FPcompl where
 showLang Dutch Eenvoudig   = "Eenvoudig"
 showLang Dutch Gemiddeld   = "Gemiddeld"
 showLang Dutch Moeilijk    = "Moeilijk"
 showLang English Eenvoudig = "Simple"
 showLang English Gemiddeld = "Average"
 showLang English Moeilijk  = "Difficult"

instance ShowLang FPA where
 showLang lang x = show (fpType x)++" "++showLang lang (complexity x)

fPoints' :: FPA -> Int
fPoints' (FPA ILGV Eenvoudig) = 7
fPoints' (FPA ILGV Gemiddeld) = 10
fPoints' (FPA ILGV Moeilijk ) = 15
fPoints' (FPA KGV  Eenvoudig) = 5
fPoints' (FPA KGV  Gemiddeld) = 7
fPoints' (FPA KGV  Moeilijk ) = 10
fPoints' (FPA IF   Eenvoudig) = 3
fPoints' (FPA IF   Gemiddeld) = 4
fPoints' (FPA IF   Moeilijk ) = 6
fPoints' (FPA UF   Eenvoudig) = 4
fPoints' (FPA UF   Gemiddeld) = 5
fPoints' (FPA UF   Moeilijk ) = 7
fPoints' (FPA OF   Eenvoudig) = 3
fPoints' (FPA OF   Gemiddeld) = 4
fPoints' (FPA OF   Moeilijk ) = 6
   



module FPA 

where
   import Languages (Lang(..),ShowLang(..))
   import Data.Fspec(FPA(..),FPcompl(..))

   instance ShowLang FPcompl where
    showLang Dutch Eenvoudig   = "Eenvoudig"
    showLang Dutch Gemiddeld   = "Gemiddeld"
    showLang Dutch Moeilijk    = "Moeilijk"
    showLang English Eenvoudig = "Simple"
    showLang English Gemiddeld = "Average"
    showLang English Moeilijk  = "Difficult"

   instance ShowLang FPA where
    showLang lang (ILGV c) = "ILGV "++showLang lang c
    showLang lang (KGV  c) = "KGV "++showLang lang c
    showLang lang (IF   c) = "IF "++showLang lang c
    showLang lang (UF   c) = "UF "++showLang lang c
    showLang lang (OF   c) = "OF "++showLang lang c
    showLang lang NO       = ""

   fPoints :: FPA -> Int
   fPoints (ILGV Eenvoudig) = 7
   fPoints (ILGV Gemiddeld) = 10
   fPoints (ILGV Moeilijk ) = 15
   fPoints (KGV  Eenvoudig) = 5
   fPoints (KGV  Gemiddeld) = 7
   fPoints (KGV  Moeilijk ) = 10
   fPoints (IF   Eenvoudig) = 3
   fPoints (IF   Gemiddeld) = 4
   fPoints (IF   Moeilijk ) = 6
   fPoints (UF   Eenvoudig) = 4
   fPoints (UF   Gemiddeld) = 5
   fPoints (UF   Moeilijk ) = 7
   fPoints (OF   Eenvoudig) = 3
   fPoints (OF   Gemiddeld) = 4
   fPoints (OF   Moeilijk ) = 6
   fPoints NO               = 0
   
   complexity :: FPA -> FPcompl
   complexity (ILGV c) = c
   complexity (KGV  c) = c
   complexity (IF   c) = c
   complexity (UF   c) = c
   complexity (OF   c) = c
   complexity NO       = Eenvoudig



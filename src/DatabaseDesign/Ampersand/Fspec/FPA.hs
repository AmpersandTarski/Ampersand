{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec.FPA 

where
   import DatabaseDesign.Ampersand.Misc (Lang(..))

   class FPAble a where
    fpa :: a->FPA

-------------- Function Points ------------------
   data FPA = ILGV FPcompl -- bevat permanente, voor de gebruiker relevante gegevens. De gegevens worden door het systeem gebruikt en onderhouden. Onder "onderhouden" verstaat FPA het toevoegen, wijzigen of verwijderen van gegevens.
            | KGV  FPcompl -- bevat permanente, voor de gebruiker relevante gegevens. Deze gegevens worden door het systeem gebruikt, maar worden door een ander systeem onderhouden (voor dat andere systeem is het dus een ILGV).
            | IF   FPcompl -- verwerkt gegevens in een ILGV van het systeem. (dus create, update en delete functies)
            | UF   FPcompl -- presenteert gegevens uit het systeem. Voorbeelden: het afdrukken van alle debiteuren; het aanmaken van facturen; het aanmaken van een diskette met betalingsopdrachten; het medium is hierbij niet van belang: papier, scherm, magneetband, datacom, enzovoorts.
            | OF   FPcompl -- is een speciaal (eenvoudig) soort uitvoerfunctie. Een opvraagfunctie presenteert gegevens uit het systeem op basis van een uniek identificerend zoekgegeven, waarbij geen aanvullende bewerkingen (zoals berekeningen of het bijwerken van een gegevensverzameling) plaats hebben. Voorbeeld: Het tonen van de gegevens van de klant met klantnummer 123456789.
            | NO           -- een onderdeel waaraan geen functiepunten worden toegekend.
              deriving (Eq, Show)

   data FPcompl = Eenvoudig | Gemiddeld | Moeilijk deriving (Eq, Show)

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
    showLang lang (ILGV c) = "ILGV "++showLang lang c
    showLang lang (KGV  c) = "KGV "++showLang lang c
    showLang lang (IF   c) = "IF "++showLang lang c
    showLang lang (UF   c) = "UF "++showLang lang c
    showLang lang (OF   c) = "OF "++showLang lang c
    showLang _    NO       = ""

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



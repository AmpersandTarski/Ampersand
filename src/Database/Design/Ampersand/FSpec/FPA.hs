module Database.Design.Ampersand.FSpec.FPA (FPA(..), FP(..), FPType(..), ShowLang(..), fpAnalyze, fpVal, fpaPlugInfo, fpaInterface) where 
                                           -- fpaPlugInfo and fpaInterface are exported for legacy modules Statistics and FSpec2Excel

import Database.Design.Ampersand.Misc (Lang(..))
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.Basics
import Data.Maybe

data FPA = FPA { dataModelFPA :: ([FP], Int), userTransactionFPA :: ([FP],Int) } deriving Show

-- NOTE: FPA constructors are in Dutch (so 'output function' is not OF, but UF)   

data Complexity = Eenvoudig | Gemiddeld | Moeilijk deriving (Show, Eq, Ord)

data FPType
 = ILGV -- ^ bevat permanente, voor de gebruiker relevante gegevens. De gegevens worden door het systeem gebruikt en onderhouden. Onder "onderhouden" verstaat FPA het toevoegen, wijzigen of verwijderen van gegevens.
 | KGV  -- ^ bevat permanente, voor de gebruiker relevante gegevens. Deze gegevens worden door het systeem gebruikt, maar worden door een ander systeem onderhouden (voor dat andere systeem is het dus een ILGV).
 | IF   -- ^ verwerkt gegevens in een ILGV van het systeem. (dus create, update en delete functies)
 | UF   -- ^ presenteert gegevens uit het systeem. Voorbeelden: het afdrukken van alle debiteuren; het aanmaken van facturen; het aanmaken van een diskette met betalingsopdrachten; het medium is hierbij niet van belang: papier, scherm, magneetband, datacom, enzovoorts.
 | OF   -- ^ is een speciaal (eenvoudig) soort uitvoerfunctie. Een opvraagfunctie presenteert gegevens uit het systeem op basis van een uniek identificerend zoekgegeven, waarbij geen aanvullende bewerkingen (zoals berekeningen of het bijwerken van een gegevensverzameling) plaats hebben. Voorbeeld: Het tonen van de gegevens van de klant met klantnummer 123456789.
  deriving Show

data FP = FP { fpType :: FPType, fpName :: String, fpComplexity :: Complexity } deriving Show


-- | Valuing of function points according to par. 3.9 (UK) or par. 2.9 (NL), see http://www.nesma.nl/sectie/fpa/hoefpa.asp
fpVal :: FP -> Int
fpVal FP{fpType=ILGV, fpComplexity=Eenvoudig} = 7
fpVal FP{fpType=ILGV, fpComplexity=Gemiddeld} = 10
fpVal FP{fpType=ILGV, fpComplexity=Moeilijk}  = 15
fpVal FP{fpType=KGV,  fpComplexity=Eenvoudig} = 5
fpVal FP{fpType=KGV,  fpComplexity=Gemiddeld} = 7
fpVal FP{fpType=KGV,  fpComplexity=Moeilijk}  = 10
fpVal FP{fpType=IF,   fpComplexity=Eenvoudig} = 3
fpVal FP{fpType=IF,   fpComplexity=Gemiddeld} = 4
fpVal FP{fpType=IF,   fpComplexity=Moeilijk}  = 6
fpVal FP{fpType=UF,   fpComplexity=Eenvoudig} = 4
fpVal FP{fpType=UF,   fpComplexity=Gemiddeld} = 5
fpVal FP{fpType=UF,   fpComplexity=Moeilijk}  = 7
fpVal FP{fpType=OF,   fpComplexity=Eenvoudig} = 3
fpVal FP{fpType=OF,   fpComplexity=Gemiddeld} = 4
fpVal FP{fpType=OF,   fpComplexity=Moeilijk}  = 6

fpAnalyze :: FSpec -> FPA
fpAnalyze fSpec = FPA (countFPs $ fpaDataModel fSpec) (countFPs $ fpaUserTransactions fSpec)
  where countFPs :: [FP] -> ([FP], Int)
        countFPs fps = (fps, sum $ map fpVal fps)

-- Na overleg met Frank Vogelenzang zijn de volgende criteria voor functiepuntentelling toegepast
fpaDataModel :: FSpec -> [FP]
fpaDataModel fSpec = mapMaybe fpaPlugInfo $ plugInfos fSpec

fpaPlugInfo :: PlugInfo -> Maybe FP
fpaPlugInfo p@(InternalPlug (TblSQL{attributes=atts})) | Just cmplxty <- ilgvComplexity $ length atts =
  Just $ FP ILGV (name p) cmplxty
  where ilgvComplexity :: Int -> Maybe Complexity
        ilgvComplexity n | n <= 2    = Nothing 
                         | n <= 15   = Just Eenvoudig
                         | n <= 40   = Just Gemiddeld
                         | otherwise = Just Moeilijk
fpaPlugInfo _ = Nothing

fpaUserTransactions :: FSpec -> [FP]
fpaUserTransactions fSpec = map fpaInterface $ interfaceS fSpec

fpaInterface :: Interface -> FP
fpaInterface ifc = 
   let nm = name ifc
       cmplxty = depth2Cmplxty $ getDepth $ ifcObj ifc
       tp = case ifc of
              Ifc{ifcClass=Just "IF"} -> IF
              Ifc{ifcClass=Just "UF"} -> UF
              Ifc{ifcClass=Just "OF"} -> OF
              
              -- code for interfaces without CLASS comes from old FPA.hs
              _ | (not.null.ifcParams)  ifc -> IF  -- In case there are editable relations, this must be an import function.
                | (isUni.objctx.ifcObj) ifc -> OF  -- If there is max one atom, this is a simple function.
                | otherwise                 -> UF  -- Otherwise, it is a UF
    in FP tp nm cmplxty
  where depth2Cmplxty :: Int -> Complexity
        depth2Cmplxty d | d <= 1    = Eenvoudig
                        | d == 2    = Gemiddeld
                        | otherwise = Moeilijk 

        getDepth Obj{objmsub=Nothing}             = 0
        getDepth Obj{objmsub=Just InterfaceRef{}} = 1 -- TODO: shouldn't we follow the ref? (this def. is from old FPA.hs)
        getDepth Obj{objmsub=Just (Box _ _ objs)}   = 1 + maximum (map getDepth objs)

class ShowLang a where
  showLang :: Lang -> a -> String

instance ShowLang FP where
  showLang lang fp = showLang lang (fpType fp) ++ " " ++ showLang lang (fpComplexity fp)


instance ShowLang FPType where
  showLang Dutch   ILGV = "ILGV" 
  showLang Dutch   KGV  = "KGV" 
  showLang Dutch   IF   = "IF"
  showLang Dutch   UF   = "UF"
  showLang Dutch   OF   = "OF"
  showLang English ILGV = "ILF" -- Internal Logical File 
  showLang English KGV  = "ELF" -- External Logical File 
  showLang English IF   = "EI"  -- External Input
  showLang English UF   = "EO"  -- External Output
  showLang English OF   = "EQ"  -- External inQuiry

instance ShowLang Complexity where
  showLang Dutch   Eenvoudig = "Eenvoudig" 
  showLang Dutch   Gemiddeld = "Gemiddeld"
  showLang Dutch   Moeilijk  = "Moeilijk"
  showLang English Eenvoudig = "Low" 
  showLang English Gemiddeld = "Average"
  showLang English Moeilijk  = "High"

{-# OPTIONS_GHC -Wall #-}
--DESCR -> functions translating adl to natural language.
--TODO -> Maybe this module is useful at more places than just func spec rendering. In that case it's not a Rendering module and it needs to be replaced
module Rendering.AdlExplanation(explainArt,explainDecl,explainMult,explainRule) where
import Adl
import Data.Fspec
import Collection (Collection ((>-)))
import Char (toLower)
import Strings (unCap, upCap, firstCaps)
import Languages(Lang(Dutch,English),plural)
import PredLogic (showPredLogic, applyM)
import Options

--instance Explained Expression where
--    explain flags e = showPredLogic flags e

explainDecl :: Options -> Declaration -> String
explainDecl options d
  | explain options d=="NONE" = ""
--  | null (explain options d)  = 
  | otherwise                 = explain options d++"\n"++explainMult options d

explainMult :: Options -> Declaration -> String
explainMult options d
 = upCap (explMult (language options))  -- WAAROM (SJ) is dit geen case statement? DAAROM (SJ) omdat de "|" notatie zo uitvoerig wordt gebruikt.
      where
       explMult Dutch
         | null ([Sym,Asy]         >- multiplicities d) = name d++" is een eigenschap van "++(unCap.plural Dutch .name.source) d++"."
         | null ([Sym,Rfx,Trn]     >- multiplicities d) = name d++" is een equivalentierelatie tussen "++(unCap.plural Dutch .name.source) d++"."
         | null ([Asy,Trn]         >- multiplicities d) = name d++" is een ordeningsrelatie tussen "++(unCap.plural Dutch .name.source) d++"."
         | null ([Uni,Tot,Inj,Sur] >- multiplicities d) = applyM d ("elke "++(unCap.name.source) d) ("precies "++preciesEen++" "++(unCap.name.target) d) ++" en vice versa."
         | null ([Uni,Tot,Inj]     >- multiplicities d) = applyM d ("elke "++(unCap.name.source) d) ("precies "++preciesEen++" "++(unCap.name.target) d) ++", maar niet voor elke "++(unCap.name.target) d++" hoeft er een "++(unCap.name.source) d++" te zijn."
         | null ([Uni,Tot,    Sur] >- multiplicities d) = applyM d ("elke "++(unCap.name.source) d) ("precies "++preciesEen++" "++(unCap.name.target) d) ++", maar elke "++(unCap.name.target) d++" is gerelateerd aan "++preciesEen++" of meer "++(unCap.plural Dutch .name.source) d++"."
         | null ([Uni,Inj,Sur]     >- multiplicities d) = "Er is precies "++preciesEen++" "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: " ++applyM d "b" "a" ++", maar niet voor elke "++(unCap.name.source) d++" hoeft er een "++(unCap.name.target) d++" te zijn."
         | null ([Tot,Inj,Sur]     >- multiplicities d) = "Er is precies "++preciesEen++" "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: "++applyM d "b" "a"++", maar elke "++(unCap.name.source) d++" mag gerelateerd zijn aan meerdere "++(unCap.plural Dutch .name.target) d++"."
         | null ([Uni,Tot        ] >- multiplicities d) = applyM d ("elke "++(unCap.name.source) d) ("precies "++preciesEen++" "++(unCap.name.target) d)++"."
         | null ([Uni,    Inj    ] >- multiplicities d) = applyM d ("elke "++(unCap.name.source) d) ("ten hoogste "++preciesEen++" "++(unCap.name.target) d)
                                                          ++" en elke "++(unCap.name.target) d++" is gerelateerd aan ten hoogste "++preciesEen++" "++(unCap.name.source) d++"."
         | null ([Uni,        Sur] >- multiplicities d) = applyM d ("elke "++(unCap.name.source) d) ("ten hoogste "++preciesEen++" "++(unCap.name.target) d)
                                                          ++", terwijl elke "++(unCap.name.target) d++" is gerelateerd aan tenminste "++preciesEen++" "++(unCap.name.source) d++"."
         | null ([    Tot,Inj    ] >- multiplicities d) = applyM d ("elke "++(unCap.name.source) d) ("tenminste "++preciesEen++" "++(unCap.name.target) d)
                                                          ++", terwijl elke "++(unCap.name.target) d++" is gerelateerd aan ten hoogste "++preciesEen++" "++(unCap.name.source) d++"."
         | null ([    Tot,    Sur] >- multiplicities d) = applyM d ("elke "++(unCap.name.source) d) ("tenminste "++preciesEen++" "++(unCap.name.target) d)
                                                          ++" en elke "++(unCap.name.target) d++" is gerelateerd aan tenminste "++preciesEen++" "++(unCap.name.source) d++"."
         | null ([        Inj,Sur] >- multiplicities d) = "Er is precies "++preciesEen++" "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: "
                                                          ++applyM d "b" "a"++"."
         | null ([            Sur] >- multiplicities d) = "Er is tenminste "++preciesEen++" "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: "
                                                          ++applyM d "b" "a"++"."
         | null ([        Inj    ] >- multiplicities d) = "Er is hooguit "++preciesEen++" "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: "
                                                          ++applyM d "b" "a"++"."
         | null ([    Tot        ] >- multiplicities d) = applyM d ("elke "++(unCap.name.source) d) ("tenminste "++preciesEen++" "++(unCap.name.target) d)++"."
         | null ([Uni            ] >- multiplicities d) = applyM d ("elke "++(unCap.name.source) d) ("nul of "++preciesEen++" "++(unCap.name.target) d)++"."
         | otherwise                                    = if null (explain options d)
                                                          then "De zin: ``"++applyM d ((var [].source) d) ((var [source d].target) d) ++"'' heeft betekenis (dus: is waar of niet waar) voor een "++(unCap.name.source) d++" "++(var [].source) d++" en een "++(unCap.name.target) d++" "++(var [source d].target) d++"."
                                                          else "Dus heeft de zin: ``"++applyM d ((var [].source) d) ((var [source d].target) d) ++"'' betekenis (dus: is waar of niet waar) voor een "++(unCap.name.source) d++" "++(var [].source) d++" en een "++(unCap.name.target) d++" "++(var [source d].target) d++"."
       explMult English
         | null ([Sym,Asy]         >- multiplicities d) = name d++" is a property of "++(unCap.plural English .name.source) d++"."
         | null ([Sym,Rfx,Trn]     >- multiplicities d) = name d++" is an equivalence relation on "++(unCap.plural English .name.source) d++"."
         | null ([Asy,Trn]         >- multiplicities d) = name d++" is an ordering relation on "++(unCap.plural English .name.source) d++"."
         | null ([Uni,Tot,Inj,Sur] >- multiplicities d) = applyM d ("every "++(unCap.name.source) d) ("exactly one "++(unCap.name.target) d)
                                                          ++" and vice versa."
         | null ([Uni,Tot,Inj    ] >- multiplicities d) = applyM d ("every "++(unCap.name.source) d) ("exactly one "++(unCap.name.target) d)
                                                          ++", but not for every "++(unCap.name.target) d++" there must be a "++(unCap.name.source) d++"."
         | null ([Uni,Tot,    Sur] >- multiplicities d) = applyM d ("every "++(unCap.name.source) d) ("exactly one "++(unCap.name.target) d)
                                                          ++", but every "++(unCap.name.target) d++" is related to one or more "++(unCap.plural English .name.source) d++"."
         | null ([Uni,    Inj,Sur] >- multiplicities d) = "There is exactly one "++(unCap.name.source) d++" (a) for every "++(unCap.name.target) d++" (b), for which: "
                                                          ++applyM d "b" "a"
                                                          ++", but not for every "++(unCap.name.source) d++" there must be a "++(unCap.name.target) d++"."
         | null ([    Tot,Inj,Sur] >- multiplicities d) = "There is exactly one "++(unCap.name.source) d++" (a) for every "++(unCap.name.target) d++" (b), for which: "
                                                          ++applyM d "b" "a"
                                                          ++", but every "++(unCap.name.source) d++" is related to one or more "++(unCap.plural English .name.target) d++"."
         | null ([Uni,Tot        ] >- multiplicities d) = applyM d ("every "++(unCap.name.source) d) ("exactly one "++(unCap.name.target) d)++"."
         | null ([Uni,    Inj    ] >- multiplicities d) = applyM d ("every "++(unCap.name.source) d) ("at most one "++(unCap.name.target) d)
                                                          ++" and every "++(unCap.name.target) d++" is related to at most one "++(unCap.name.source) d++"."
         | null ([Uni,        Sur] >- multiplicities d) = applyM d ("every "++(unCap.name.source) d) ("at most one "++(unCap.name.target) d)
                                                          ++", whereas every "++(unCap.name.target) d++" is related to at least one "++(unCap.name.source) d++"."
         | null ([    Tot,Inj    ] >- multiplicities d) = applyM d ("every "++(unCap.name.source) d) ("at least one "++(unCap.name.target) d)
                                                          ++", whereas every "++(unCap.name.target) d++" is related to at most one "++(unCap.name.source) d++"."
         | null ([    Tot,    Sur] >- multiplicities d) = applyM d ("every "++(unCap.name.source) d) ("at least one "++(unCap.name.target) d)
                                                          ++" and every "++(unCap.name.target) d++" is related to at least one "++(unCap.name.source) d++"."
         | null ([        Inj,Sur] >- multiplicities d) = "There is exactly one "++(unCap.name.source) d++" (a) for every "++(unCap.name.target) d++" (b), for which: "
                                                          ++applyM d "b" "a"++"."
         | null ([            Sur] >- multiplicities d) = "There is at least one "++(unCap.name.source) d++" (a) for every "++(unCap.name.target) d++" (b), for which: "
                                                          ++applyM d "b" "a"++"."
         | null ([        Inj    ] >- multiplicities d) = "There is at most one "++(unCap.name.source) d++" (a) for every "++(unCap.name.target) d++" (b), for which: "
                                                          ++applyM d "b" "a"++"."
         | null ([    Tot        ] >- multiplicities d) = applyM d ("every "++(unCap.name.source) d) ("at least one "++(unCap.name.target) d)++"."
         | null ([Uni            ] >- multiplicities d) = applyM d ("every "++(unCap.name.source) d) ("zero or one "++(unCap.name.target) d)++"."
         | otherwise                                    = (if null (explain options d)
                                                           then "The sentence: "
                                                           else "So, the sentence: ")++
                                                          doublequote (upCap (applyM d ((unCap.firstCaps.name.source) d++" "++(var [].source) d) ((unCap.firstCaps.name.target) d++" "++(var [source d].target) d)))++
                                                          " is meaningful (i.e. it is either true or false) "++
                                                          "for any "++(unCap.name.source) d++" "++(var [].source) d++" and "++
                                                          (unCap.name.target) d++" "++(var [source d].target) d++"."
       preciesEen = "een(1)" --"één"  TODO moet utf8 resistent worden gemaakt.

doublequote :: String -> String
doublequote str = "``"++str++[' '|last str=='\'']++"''"

var :: Identified a => [a] -> a -> String     -- TODO Vervangen door mkvar, uit predLogic.hs
var seen c = low c ++ ['\''| c'<-seen, low c == low c']
             where low idt= if null (name idt) then "x" else [(toLower.head.name) idt]

explainArt :: Options -> Fspc -> Rule ->  String
explainArt flags _ rul  -- TODO Geef een mooie uitleg van deze regel. 
    = if null (explain flags rul)
      then case language flags of
              English   -> "Artificial explanation: "
              Dutch     -> "Kunstmatige uitleg: " 
           ++ showPredLogic flags rul
      else explain flags rul

explainRule :: Options -> Rule -> String
explainRule flags r
  = if null (explain flags r)
    then case language flags of
            English -> "Artificial explanation: "
            Dutch   -> "Kunstmatige uitleg: "
         ++ showPredLogic flags r
    else (if explain flags r=="NONE" then "" else explain flags r)



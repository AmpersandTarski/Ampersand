--DESCR -> functions translating adl to natural language.
--TODO -> Maybe this module is useful at more places than just func spec rendering. In that case it's not a Rendering module and it needs to be replaced
module Rendering.AdlExplanation where
import Adl
import Collection (Collection (isc,(>-),empty, rd))
import Strings (unCap)
import Languages(Lang(Dutch,English),ShowLang(showLang),plural)
import PredLogic (lang, assemble, normRule,applyM)
import Options
   
explainRule :: Options -> Rule -> String
explainRule flags r
  = if null (explain r)
    then case language flags of
            English -> "Artificial explanation: "
            Dutch   -> "Kunstmatige uitleg: "
         ++(lang flags (language flags) .assemble.normRule) r
    else (if explain r=="NONE" then "" else explain r)

explainDecl :: Lang -> Declaration -> String
explainDecl language d
  | explain d=="NONE" = ""
  | null (explain d)  = explainMult language d
  | otherwise         = explain d

explainMult :: Lang -> Declaration -> String
explainMult Dutch d
  | null ([Sym,Asy] >- multiplicities d) = name d++" is een eigenschap van "++(unCap.plural Dutch .name.source) d++"."
  | null ([Sym,Rfx,Trn] >- multiplicities d) = name d++" is een equivalentierelatie tussen "++(unCap.plural Dutch .name.source) d++"."
  | null ([Asy,Trn] >- multiplicities d) = name d++" is een ordeningsrelatie tussen "++(unCap.plural Dutch .name.source) d++"."
  | null ([Sym,Asy] >- multiplicities d) = name d++" is een eigenschap van "++(unCap.plural Dutch .name.source) d++"."
  | null ([Uni,Tot,Inj,Sur] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("precies één "++(unCap.name.target) d) ++" en vice versa."
  | null ([Uni,Tot,Inj] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("precies één "++(unCap.name.target) d) ++", maar niet voor elke "++(unCap.name.target) d++" hoeft er een "++(unCap.name.source) d++" te zijn."
  | null ([Uni,Tot,    Sur] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("precies één "++(unCap.name.target) d) ++", maar elke "++(unCap.name.target) d++" is gerelateerd aan één of meer "++(unCap.plural Dutch .name.source) d++"."
  | null ([Uni,Inj,Sur] >- multiplicities d) = "Er is precies één "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: " ++applyM d "b" "a" ++", maar niet voor elke "++(unCap.name.source) d++" hoeft er een "++(unCap.name.target) d++" te zijn."
  | null ([Tot,Inj,Sur] >- multiplicities d) = "Er is precies één "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: "++applyM d "b" "a"++", maar elke "++(unCap.name.source) d++" mag gerelateerd zijn aan meerdere "++(unCap.plural Dutch .name.target) d++"."
  | null ([Uni,Tot        ] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("precies één "++(unCap.name.target) d)++"."
  | null ([Uni,    Inj    ] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("ten hoogste één "++(unCap.name.target) d)
                                                     ++" en elke "++(unCap.name.target) d++" is gerelateerd aan ten hoogste één "++(unCap.name.source) d++"."
    | null ([Uni,        Sur] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("ten hoogste één "++(unCap.name.target) d)
                                                     ++", terwijl elke "++(unCap.name.target) d++" is gerelateerd aan tenminste één "++(unCap.name.source) d++"."
    | null ([    Tot,Inj    ] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("tenminste één "++(unCap.name.target) d)
                                                     ++", terwijl elke "++(unCap.name.target) d++" is gerelateerd aan ten hoogste één "++(unCap.name.source) d++"."
    | null ([    Tot,    Sur] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("tenminste één "++(unCap.name.target) d)
                                                     ++" en elke "++(unCap.name.target) d++" is gerelateerd aan tenminste één "++(unCap.name.source) d++"."
    | null ([        Inj,Sur] >- multiplicities d) = "Er is precies één "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: "
                                                     ++applyM d "b" "a"++"."
    | null ([            Sur] >- multiplicities d) = "Er is tenminste één "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: "
                                                     ++applyM d "b" "a"++"."
    | null ([        Inj    ] >- multiplicities d) = "Er is hooguit één "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: "
                                                     ++applyM d "b" "a"++"."
    | null ([    Tot        ] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("tenminste één "++(unCap.name.target) d)++"."
    | null ([Uni            ] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("nul of één "++(unCap.name.target) d)++"."
    | otherwise                                    = applyM d ("een "++(unCap.name.source) d) ("een "++(unCap.name.target) d) ++"."

explainMult _ d -- default English
    | null ([Sym,Asy]         >- multiplicities d) = name d++" is a property of "++(unCap.plural English .name.source) d++"."
    | null ([Sym,Rfx,Trn]     >- multiplicities d) = name d++" is an equivalence relation on "++(unCap.plural English .name.source) d++"."
    | null ([Asy,Trn]         >- multiplicities d) = name d++" is an ordering relation on "++(unCap.plural English .name.source) d++"."
    | null ([Uni,Tot,Inj,Sur] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("exactly one "++(unCap.name.target) d)
                                                     ++" and vice versa."
    | null ([Uni,Tot,Inj    ] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("exactly one "++(unCap.name.target) d)
                                                     ++", but not for every "++(unCap.name.target) d++" there must be a "++(unCap.name.source) d++"."
    | null ([Uni,Tot,    Sur] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("exactly one "++(unCap.name.target) d)
                                                     ++", but every "++(unCap.name.target) d++" is related to one or more "++(unCap.plural English .name.source) d++"."
    | null ([Uni,    Inj,Sur] >- multiplicities d) = "There is exactly one "++(unCap.name.source) d++" (a) for every "++(unCap.name.target) d++" (b), for which: "
                                                     ++applyM d "b" "a"
                                                     ++", but not for every "++(unCap.name.source) d++" there must be a "++(unCap.name.target) d++"."
    | null ([    Tot,Inj,Sur] >- multiplicities d) = "There is exactly one "++(unCap.name.source) d++" (a) for every "++(unCap.name.target) d++" (b), for which: "
                                                     ++applyM d "b" "a"
                                                     ++", but every "++(unCap.name.source) d++" is related to one or more "++(unCap.plural English .name.target) d++"."
    | null ([Uni,Tot        ] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("exactly one "++(unCap.name.target) d)++"."
    | null ([Uni,    Inj    ] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("at most one "++(unCap.name.target) d)
                                                     ++" and every "++(unCap.name.target) d++" is related to at most one "++(unCap.name.source) d++"."
    | null ([Uni,        Sur] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("at most one "++(unCap.name.target) d)
                                                     ++", whereas every "++(unCap.name.target) d++" is related to at least one "++(unCap.name.source) d++"."
    | null ([    Tot,Inj    ] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("at least one "++(unCap.name.target) d)
                                                     ++", whereas every "++(unCap.name.target) d++" is related to at most one "++(unCap.name.source) d++"."
    | null ([    Tot,    Sur] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("at least one "++(unCap.name.target) d)
                                                     ++" and every "++(unCap.name.target) d++" is related to at least one "++(unCap.name.source) d++"."
    | null ([        Inj,Sur] >- multiplicities d) = "There is exactly one "++(unCap.name.source) d++" (a) for every "++(unCap.name.target) d++" (b), for which: "
                                                     ++applyM d "b" "a"++"."
    | null ([            Sur] >- multiplicities d) = "There is at least one "++(unCap.name.source) d++" (a) for every "++(unCap.name.target) d++" (b), for which: "
                                                     ++applyM d "b" "a"++"."
    | null ([        Inj    ] >- multiplicities d) = "There is at most one "++(unCap.name.source) d++" (a) for every "++(unCap.name.target) d++" (b), for which: "
                                                     ++applyM d "b" "a"++"."
    | null ([    Tot        ] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("at least one "++(unCap.name.target) d)++"."
    | null ([Uni            ] >- multiplicities d) = applyM d ("Every "++(unCap.name.source) d) ("zero or one "++(unCap.name.target) d)++"."
    | otherwise                                    = applyM d ("a "++(unCap.name.source) d) ("a "++(unCap.name.target) d) ++"."

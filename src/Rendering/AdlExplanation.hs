--DESCR -> functions translating adl to natural language.
--TODO -> Maybe this module is useful at more places than just func spec rendering. In that case it's not a Rendering module and it needs to be replaced
module Rendering.AdlExplanation(lang,explainArt,explainDecl,explainMult,explainRule) where
import Auxiliaries (eqCl)
import Adl
import Data.Fspec
import Collection (Collection (isc,(>-),empty, rd))
import Char (toLower)
import Strings (unCap,chain)
import Languages(Lang(Dutch,English),ShowLang(showLang),plural)
import PredLogic (PredLogic(..),ruleToPL,applyM,expr2predLogic)
import Options

instance Explained Expression where
    explain options e = lang options (expr2predLogic e)

explainDecl :: Options -> Declaration -> String
explainDecl options d
  | explain options d=="NONE" = ""
  | null (explain options d)  = explainMult options d
  | otherwise                 = explain options d

explainMult :: Options -> Declaration -> String
explainMult options d
 = explMult (language options)  -- WAAROM (SJ) is dit geen case statement? DAAROM (SJ) omdat de "|" notatie zo uitvoerig wordt gebruikt.
      where
       explMult Dutch
         | null ([Sym,Asy]         >- multiplicities d) = name d++" is een eigenschap van "++(unCap.plural Dutch .name.source) d++"."
         | null ([Sym,Rfx,Trn]     >- multiplicities d) = name d++" is een equivalentierelatie tussen "++(unCap.plural Dutch .name.source) d++"."
         | null ([Asy,Trn]         >- multiplicities d) = name d++" is een ordeningsrelatie tussen "++(unCap.plural Dutch .name.source) d++"."
         | null ([Uni,Tot,Inj,Sur] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("precies één "++(unCap.name.target) d) ++" en vice versa."
         | null ([Uni,Tot,Inj]     >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("precies één "++(unCap.name.target) d) ++", maar niet voor elke "++(unCap.name.target) d++" hoeft er een "++(unCap.name.source) d++" te zijn."
         | null ([Uni,Tot,    Sur] >- multiplicities d) = applyM d ("Elke "++(unCap.name.source) d) ("precies één "++(unCap.name.target) d) ++", maar elke "++(unCap.name.target) d++" is gerelateerd aan één of meer "++(unCap.plural Dutch .name.source) d++"."
         | null ([Uni,Inj,Sur]     >- multiplicities d) = "Er is precies één "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: " ++applyM d "b" "a" ++", maar niet voor elke "++(unCap.name.source) d++" hoeft er een "++(unCap.name.target) d++" te zijn."
         | null ([Tot,Inj,Sur]     >- multiplicities d) = "Er is precies één "++(unCap.name.source) d++" (a) voor elke "++(unCap.name.target) d++" (b), waarvoor geldt: "++applyM d "b" "a"++", maar elke "++(unCap.name.source) d++" mag gerelateerd zijn aan meerdere "++(unCap.plural Dutch .name.target) d++"."
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
         | otherwise                                    = "De zin: ``"++applyM d ((var [].source) d) ((var [source d].target) d) ++"'' heeft betekenis (dus: is waar of niet waar) voor een "++(unCap.name.source) d++" "++(var [].source) d++" en een "++(unCap.name.target) d++" "++(var [source d].target) d++"."
       explMult English
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
         | otherwise                                    = "The sentence: ``"++applyM d ((var [].source) d) ((var [source d].target) d) ++"'' is meaningful (i.e. it is either true or false) for any "++(unCap.name.source) d++" "++(var [].source) d++" and "++(unCap.name.target) d++" "++(var [source d].target) d++"."

var :: Identified a => [a] -> a -> String
var seen c = low c ++ ['\''| c'<-seen, low c == low c']
             where low c = if null (name c) then "x" else [(toLower.head.name) c]

explainArt :: Options -> Fspc -> Rule ->  String
explainArt flags fspc rul  -- TODO Geef een mooie uitleg van deze regel. 
    = if null (explain flags rul)
      then case language flags of
              English   -> "Artificial explanation: "
              Dutch     -> "Kunstmatige uitleg: " 
           ++(lang flags.ruleToPL) rul
      else explain flags rul

explainRule :: Options -> Rule -> String
explainRule options r
  = if null (explain options r)
    then case language options of
            English -> "Artificial explanation: "
            Dutch   -> "Kunstmatige uitleg: "
         ++(lang options.ruleToPL) r
    else (if explain options r=="NONE" then "" else explain options r)

lang :: Options -> PredLogic -> String
lang flags x =
     case language flags of
       English -> predLshow flags ("For each", "There exists", implies, "is equivalent to", "equals", "is unequal to", "or", "and", "not", rel, fun, langVars flags, "\n  ", " ") x
       Dutch   -> predLshow flags ("Voor elke", "Er is een", implies, "is equivalent met", "gelijk aan", "is ongelijk aan", "of", "en", "niet", rel, fun, langVars flags, "\n  ", " ") x
     where rel m lhs rhs = applyM (makeDeclaration m) lhs rhs
           fun m x = name m++"("++x++")"
           implies antc cons = case language flags of 
                                   English  -> "If "++antc++", then "++cons
                                   Dutch    -> "Als "++antc++", dan "++cons

predLshow flags (forall, exists, implies, equiv, equal, nequal, or, and, not, rel, fun, showVars, break, space) e
 = charshow 0 e
     where
      wrap i j str = if i<=j then str else "("++str++")"
      charshow i (Forall vars restr)
       = wrap i 1 (showVars forall vars ++ charshow 1 restr)
      charshow i (Exists vars restr)
       = wrap i 1 (showVars exists vars  ++ charshow 1 restr)
      charshow i (Implies antc conseq)
       = wrap i 2 (break++implies (charshow 2 antc) (charshow 2 conseq))
      charshow i (Equiv lhs rhs)
       = wrap i 2 (break++charshow 2 lhs++space++equiv++space++ charshow 2 rhs)
      charshow i (Disj rs)
       = if null rs then "" else
         wrap i 3 (chain (space++or++space) (map (charshow 3) rs))
      charshow i (Conj rs)
       = if null rs then "" else
         wrap i 4 (chain (space++and++space) (map (charshow 4) rs))
      charshow i (Rel (Funs l []) m (Funs r []))
       = wrap i 5 (applyM (makeDeclaration m) l r)
      charshow i (Rel (Funs x [l]) m (Funs r []))
       = wrap i 5 (if isIdent m
                   then applyM (makeDeclaration l) x r
                   else applyM (makeDeclaration m) x r)
      charshow i (Rel (Funs l []) m (Funs y [r]))
       = wrap i 5 (if isIdent m
                   then applyM (makeDeclaration r) l y
                   else applyM (makeDeclaration m) l y)
      charshow i (Rel lhs m rhs)
       = wrap i 5 (if inline m
                   then rel m (charshow 5 lhs) (charshow 5 rhs)
                   else rel m (charshow 5 rhs) (charshow 5 lhs))
      charshow i (Funs x [])     = x
      charshow i (Funs x (m:ms)) = if isIdent m then charshow i (Funs x ms) else charshow i (Funs (fun m x) ms)
      charshow i (Not rs)        = wrap i 6 (space++not++charshow 6 rs)
      charshow i (Pred nm v)     = nm++"{"++v++"}"
      ishow e | idsOnly e = equal
              | isNot e   = nequal
              | otherwise = show e

langVars flags q vs
    = case language flags of
       English -> if null vs then "" else
                  if q=="Exists"
                  then chain " and " ["there exist"++(if length vs==1 then "s a "++dType else " "++plural English dType)++" called "++chain ", " vs | (vs,dType)<-vss]
                  else "If "++langVars flags "Exists" vs++", "
                  where
                   vss = [(map fst vs,show(snd (head vs))) |vs<-eqCl snd vs]
       Dutch   -> if null vs then "" else
                  if q=="Er is"
                  then chain " en " ["er "++(if length vs==1 then "is een "++dType else "zijn "++plural Dutch dType)++" genaamd "++chain ", " vs | (vs,dType)<-vss]
                  else "Als "++langVars flags "Er is" vs++", "
                  where
                   vss = [(map fst vs,show(snd (head vs))) |vs<-eqCl snd vs]



objOrShow :: Options -> PredLogic -> String
objOrShow flags = predLshow flags ("For all", "Exists", implies, " = ", " = ", "<>", "OR", "AND", "NOT", rel, fun, langVars flags, "\n", " ")
               where rel m lhs rhs = applyM (makeDeclaration m) lhs rhs
                     fun m x = x++"."++name m
                     implies antc cons = "IF "++antc++" THEN "++cons


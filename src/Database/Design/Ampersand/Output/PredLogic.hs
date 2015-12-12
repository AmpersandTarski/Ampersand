module Database.Design.Ampersand.Output.PredLogic
         ( PredLogicShow(..), showLatex, showRtf, mkVar
         ) where

import Data.List
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.FSpec.ShowADL
import Data.Char
import Database.Design.Ampersand.Output.PandocAux (latexEscShw,texOnly_Id)

--  data PredVar = PV String     -- TODO Bedoeld om predicaten inzichtelijk te maken. Er bestaan namelijk nu verschillende manieren om hier mee om te gaan (zie ook Motivations. HJO.
data PredLogic
 = Forall [Var] PredLogic            |
   Exists [Var] PredLogic            |
   Implies PredLogic PredLogic       |
   Equiv PredLogic PredLogic         |
   Conj [PredLogic]                  |
   Disj [PredLogic]                  |
   Not PredLogic                     |
   Pred String String                |  -- Pred nm v, with v::type   is equiv. to Rel nm Nowhere [] (type,type) True (Sgn (showADL e) type type [] "" "" "" [Asy,Sym] Nowhere 0 False)
   PlK0 PredLogic                    |
   PlK1 PredLogic                    |
   R PredLogic Declaration PredLogic |
   Atom String                       |
   Funs String [Declaration]         |
   Dom Expression Var                |
   Cod Expression Var                deriving Eq

data Notation = Flr | Frl | Rn | Wrap deriving Eq   -- yields notations y=r(x)  |  x=r(y)  |  x r y  | exists ... respectively.

--   predKeyWords l =
--     case l of
--        English  ->

class PredLogicShow a where
  showPredLogic :: Lang -> a -> String
  showPredLogic l r =
    predLshow (natLangOps l) (toPredLogic r) -- predLshow produces raw LaTeX
  toPredLogic :: a -> PredLogic

instance PredLogicShow Rule where
  toPredLogic ru = assemble (rrexp ru)

instance PredLogicShow Expression where
  toPredLogic = assemble

-- showLatex ought to produce PandDoc mathematics instead of LaTeX source code.
-- PanDoc, however, does not support mathematics sufficiently, as to date. For this reason we have showLatex.
-- It circumvents the PanDoc structure and goes straight to LaTeX source code.
-- TODO when PanDoc is up to the job.
showLatex :: PredLogic -> [[String]]
showLatex x 
 = chop (predLshow ("\\forall", "\\exists", implies, "\\Leftrightarrow", "\\vee", "\\ \\wedge\t", "^{\\asterisk}", "^{+}", "\\neg", rel, fun, mathVars, "", " ", apply, "\\in") x)
   where rel r lhs rhs  -- TODO: the stuff below is very sloppy. This ought to be derived from the stucture, instead of by this naming convention.
           = if isIdent r then lhs++"\\ =\\ "++rhs else
             case name r of
              "lt"     -> lhs++"\\ <\\ "++rhs
              "gt"     -> lhs++"\\ >\\ "++rhs
              "le"     -> lhs++"\\ \\leq\\ "++rhs
              "leq"    -> lhs++"\\ \\leq\\ "++rhs
              "ge"     -> lhs++"\\ \\geq\\ "++rhs
              "geq"    -> lhs++"\\ \\geq\\ "++rhs
              _        -> lhs++"\\ \\id{"++latexEscShw (name r)++"}\\ "++rhs
         fun r e = "\\id{"++latexEscShw (name r)++"}("++e++")"
         implies antc cons = antc++" \\Rightarrow "++cons
         apply :: Declaration -> String -> String -> String    --TODO language afhankelijk maken.
         apply decl d c =
            case decl of
              Sgn{}     -> d++"\\ \\id{"++latexEscShw (name decl)++"}\\ "++c
              Isn{}     -> d++"\\ =\\ "++c
              Vs{}      -> "V"
         mathVars :: String -> [Var] -> String
         mathVars q vs
          = if null vs then "" else
            q++" "++intercalate "; " [intercalate ", " var++"\\coloncolon\\id{"++latexEscShw dType++"}" | (var,dType)<-vss]++":\n"
            where
             vss = [(map fst varCl,show(snd (head varCl))) |varCl<-eqCl snd vs]
         chop :: String -> [[String]]
         chop str = (map chops.lins) str
          where
            lins ""        = []
            lins ('\n':cs) = "": lins cs
            lins (c:cs)    = (c:r):rs where r:rs = case lins cs of  [] -> [""] ; e -> e
            chops cs = let [a,b,c] = take 3 (tabs cs) in [a,b,c]
            tabs "" = ["","","",""]
            tabs ('\t':cs) = "": tabs cs
            tabs (c:cs) = (c:r):rs where r:rs = tabs cs

showRtf :: PredLogic -> String
showRtf  p = predLshow (forallP, existsP, impliesP, equivP, orP, andP, k0P, k1P, notP, relP, funP, showVarsP, breakP, spaceP, apply, el)
               p
  where unicodeSym :: Int -> Char -> Char -> String
        unicodeSym fs sym altChar = "{\\fs"++show fs++" \\u"++show (ord sym)++[altChar]++"}"
        forallP = unicodeSym 32 '∀' 'A' --"{\\fs36 \\u8704A}"
        existsP = unicodeSym 32 '∃' 'E'
        impliesP antc cons = antc++" "++unicodeSym 26 '⇒' '?'++" "++cons
        equivP = unicodeSym 26 '⇔' '='
        orP = unicodeSym 30 '∨' 'v'
        andP = unicodeSym 30 '∧' '^'
        k0P = "{\\super "++unicodeSym 30 '∗' '*'++"}"
        k1P = "{\\super +}"
        notP = unicodeSym 26 '¬' '!'
        el = unicodeSym 30 '∈' '?' 
        relP r lhs rhs  -- TODO: sloppy code, copied from showLatex
         = if isIdent r then lhs++"\\ =\\ "++rhs else
           case name r of
            "lt"     -> lhs++" < "++rhs
            "gt"     -> lhs++" > "++rhs
            "le"     -> lhs++" "++unicodeSym 28 '≤' '?'++" "++rhs
            "leq"    -> lhs++" "++unicodeSym 28 '≤' '?'++" "++rhs
            "ge"     -> lhs++" "++unicodeSym 28 '≥' '?'++" "++rhs
            "geq"    -> lhs++" "++unicodeSym 28 '≥' '?'++" "++rhs
            _        -> lhs++" "++name r++" "++rhs
        funP r e = name r++"("++e++")"
        
        apply :: Declaration -> String -> String -> String
        apply decl d c =
           case decl of
             Sgn{}     -> d++" "++name decl++" "++c
             Isn{}     -> d++" = "++c
             Vs{}      -> "V"
        showVarsP :: String -> [Var] -> String
        showVarsP q vs
         = if null vs then "" else
           q++intercalate "; " [intercalate ", " var++" "++unicodeSym 28 '∷' '?'++" "++dType | (var,dType)<-vss]++":\\par\n"
           where
            vss = [(map fst varCl,show(snd (head varCl))) |varCl<-eqCl snd vs]
        breakP = ""
        spaceP = " "

-- natLangOps exists for the purpose of translating a predicate logic expression to natural language.
-- It yields a vector of mostly strings, which are used to assemble a natural language text in one of the natural languages supported by Ampersand.
natLangOps :: Named a => Lang -> (String,
                                       String,
                                       String -> String -> String,
                                       String,
                                       String,
                                       String,
                                       String,
                                       String,
                                       String,
                                       Declaration -> String -> String -> String,
                                       a -> String -> String,
                                       String -> [(String, A_Concept)] -> String,
                                       String,
                                       String,
                                       Declaration -> String -> String -> String,
                                       String)
natLangOps l
         = case l of
-- parameternamen:         (forallP,     existsP,        impliesP, equivP,             orP,  andP,  k0P, k1P, notP,  relP, funP, showVarsP, breakP, spaceP)
             English -> ("For each",  "There exists", implies, "is equivalent to",  "or", "and", "*", "+", "not",  rel, fun,  langVars , "\n  ", " ", apply, "is element of")
             Dutch   -> ("Voor elke", "Er is een",    implies, "is equivalent met", "of", "en",  "*", "+", "niet", rel, fun,  langVars , "\n  ", " ", apply, "is element van")
            where
               rel r = apply r
               fun r x' = texOnly_Id(name r)++"("++x'++")"
               implies antc cons = case l of
                                     English  -> "If "++antc++", then "++cons
                                     Dutch    -> "Als "++antc++", dan "++cons
               apply decl d c =
                  case decl of
                    Sgn{}     -> if null (prL++prM++prR)
                                   then "$"++d++"$ "++name decl++" $"++c++"$"
                                   else prL++" $"++d++"$ "++prM++" $"++c++"$ "++prR
                       where prL = decprL decl
                             prM = decprM decl
                             prR = decprR decl
                    Isn{}     -> case l of
                                     English  -> "$"++d++"$ equals $"++c++"$"
                                     Dutch    -> "$"++d++"$ is gelijk aan $"++c++"$"
                    Vs{}      -> case l of
                                     English  -> show True
                                     Dutch    -> "Waar"
               langVars :: String -> [(String, A_Concept)] -> String
               langVars q vs
                   = case l of
                      English | null vs     -> ""
                              | q=="Exists" ->
                                  intercalate " and "
                                  ["there exist"
                                   ++(if length vs'==1 then "s a "++dType else ' ':plural English dType)
                                   ++" called "
                                   ++intercalate ", " ['$':v'++"$" | v'<-vs'] | (vs',dType)<-vss]
                              | otherwise   -> "If "++langVars "Exists" vs++", "
                      Dutch   | null vs     -> ""
                              | q=="Er is"  ->
                                  intercalate " en "
                                  ["er "
                                    ++(if length vs'==1 then "is een "++dType else "zijn "++plural Dutch dType)
                                    ++" genaamd "
                                    ++intercalate ", " ['$':v'++"$" | v'<-vs'] | (vs',dType)<-vss]
                              | otherwise   -> "Als "++langVars "Er is" vs++", "
                    where
                     vss = [(map fst vs',show(snd (head vs'))) |vs'<-eqCl snd vs]

-- predLshow exists for the purpose of translating a predicate logic expression to natural language.
-- It uses a vector of operators (mostly strings) in order to produce text. This vector can be produced by, for example, natLangOps.
-- example:  'predLshow (natLangOps l) e' translates expression 'e'
-- into a string that contains a natural language representation of 'e'.
predLshow :: ( String                                    -- forallP
             , String                                    -- existsP
             , String -> String -> String                -- impliesP
             , String                                    -- equivP
             , String                                    -- orP
             , String                                    -- andP
             , String                                    -- kleene *
             , String                                    -- kleene +
             , String                                    -- notP
             , Declaration -> String -> String -> String    -- relP
             , Declaration -> String -> String              -- funP
             , String -> [(String, A_Concept)] -> String -- showVarsP
             , String                                    -- breakP
             , String                                    -- spaceP
             , Declaration -> String -> String -> String -- apply
             , String                                    -- set element
             ) -> PredLogic -> String
predLshow (forallP, existsP, impliesP, equivP, orP, andP, k0P, k1P, notP, relP, funP, showVarsP, breakP, spaceP, apply, el)
 = charshow 0
     where
      wrap i j str = if i<=j then str else "("++str++")"
      charshow :: Integer -> PredLogic -> String
      charshow i predexpr
       = case predexpr of
               Forall vars restr   -> wrap i 1 (showVarsP forallP vars ++ charshow 1 restr)
               Exists vars restr   -> wrap i 1 (showVarsP existsP vars  ++ charshow 1 restr)
               Implies antc conseq -> wrap i 2 (breakP++impliesP (charshow 2 antc) (charshow 2 conseq))
               Equiv lhs rhs       -> wrap i 2 (breakP++charshow 2 lhs++spaceP++equivP++spaceP++ charshow 2 rhs)
               Disj rs             -> if null rs
                                      then ""
                                      else wrap i 3 (intercalate (spaceP++orP ++spaceP) (map (charshow 3) rs))
               Conj rs             -> if null rs
                                      then ""
                                      else wrap i 4 (intercalate (spaceP++andP++spaceP) (map (charshow 4) rs))
               Funs x ls           -> case ls of
                                         []    -> x
                                         r:ms  -> if isIdent r then charshow i (Funs x ms) else charshow i (Funs (funP r x) ms)
               Dom expr (x,_)      -> x++el++funP (makeRel "dom") (showADL expr)
               Cod expr (x,_)      -> x++el++funP (makeRel "cod") (showADL expr)
               R pexpr dec pexpr'  -> case (pexpr,pexpr') of
                                         (Funs l [] , Funs r [])  -> wrap i 5 (apply dec l r)
{-
                                            (Funs l [f], Funs r [])  -> wrap i 5 (if isIdent rel
                                                                                  then apply (makeDeclaration f) l r
                                                                                  else apply (makeDeclaration rel) (funP f l) r)
                                            (Funs l [] , Funs r [f]) -> wrap i 5 (if isIdent rel
                                                                                  then apply (makeDeclaration f) l r
                                                                                  else apply (makeDeclaration rel) l (funP f r))
-}
                                         (lhs,rhs)                -> wrap i 5 (relP dec (charshow 5 lhs) (charshow 5 rhs))
               Atom atom           -> "'"++atom++"'"
               PlK0 rs             -> wrap i 6 (charshow 6 rs++k0P)
               PlK1 rs             -> wrap i 7 (charshow 7 rs++k1P)
               Not rs              -> wrap i 8 (spaceP++notP++charshow 8 rs)
               Pred nm v'          -> nm++"{"++v'++"}"
      makeRel :: String -> Declaration -- This function exists solely for the purpose of dom and cod
      makeRel str
          =    Sgn { decnm   = str
                   , decsgn  = fatal 217 "Do not refer to decsgn of this dummy relation"
                   , decprps = [Uni,Tot]
                   , decprps_calc = Nothing
                   , decprL  = ""
                   , decprM  = ""
                   , decprR  = ""
                   , decMean = fatal 223 "Do not refer to decMean of this dummy relation"
                   , decfpos = OriginUnknown
                   , decusr  = False
                   , decpat  = fatal 228 "Do not refer to decpat of this dummy relation"
                   , decplug = fatal 229 "Do not refer to decplug of this dummy relation"
                   }

--objOrShow :: Lang -> PredLogic -> String
--objOrShow l = predLshow ("For all", "Exists", implies, " = ", " = ", "<>", "OR", "AND", "*", "+", "NOT", rel, fun, langVars l, "\n", " ")
--               where rel r lhs rhs = applyM (makeDeclaration r) lhs rhs
--                     fun r x = x++"."++name r
--                     implies antc cons = "IF "++antc++" THEN "++cons

-- The function 'assemble' translates a rule to predicate logic.
-- In order to remain independent of any representation, it transforms the Haskell data structure Rule
-- into the data structure PredLogic, rather than manipulate with texts.
type Var = (String,A_Concept)
assemble :: Expression -> PredLogic
assemble expr
 = case (source expr, target expr) of
        (ONE, ONE) -> rc
        (_  , ONE) -> Forall [s] rc
        (ONE, _)   -> Forall [t] rc
        (_  , _)   -> Forall [s,t] rc
  where
   [s,t] = mkVar [] [source expr, target expr]
   rc = f [s,t] expr (s,t)
   f :: [Var] -> Expression -> (Var,Var) -> PredLogic
   f exclVars (EEqu (l,r)) (a,b)  = Equiv (f exclVars l (a,b)) (f exclVars r (a,b))
   f exclVars (EInc (l,r)) (a,b)  = Implies (f exclVars l (a,b)) (f exclVars r (a,b))
   f exclVars e@EIsc{}     (a,b)  = Conj [f exclVars e' (a,b) | e'<-exprIsc2list e]
   f exclVars e@EUni{}     (a,b)  = Disj [f exclVars e' (a,b) | e'<-exprUni2list e]
   f exclVars (EDif (l,r)) (a,b)  = Conj [f exclVars l (a,b), Not (f exclVars r (a,b))]
   f exclVars (ELrs (l,r)) (a,b)  = Forall [c] (Implies (f eVars r (b,c)) (f eVars l (a,c)))
                                    where [c]   = mkVar exclVars [target l]
                                          eVars = exclVars++[c]
   f exclVars (ERrs (l,r)) (a,b)  = Forall [c] (Implies (f eVars l (c,a)) (f eVars r (c,b)))
                                    where [c]   = mkVar exclVars [source l]
                                          eVars = exclVars++[c]
   f exclVars (EDia (l,r)) (a,b)  = Forall [c] (Equiv (f eVars r (b,c)) (f eVars l (a,c)))
                                    where [c]   = mkVar exclVars [target l]
                                          eVars = exclVars++[c]
   f exclVars e@ECps{}     (a,b)  = fECps exclVars e (a,b)  -- special treatment, see below
   f exclVars e@ERad{}     (a,b)  = fERad exclVars e (a,b)  -- special treatment, see below
   f _        (EPrd (l,r)) (a,b)  = Conj [Dom l a, Cod r b]
   f exclVars (EKl0 e)     (a,b)  = PlK0 (f exclVars e (a,b))
   f exclVars (EKl1 e)     (a,b)  = PlK1 (f exclVars e (a,b))
   f exclVars (ECpl e)     (a,b)  = Not (f exclVars e (a,b))
   f exclVars (EBrk e)     (a,b)  = f exclVars e (a,b)
   f _ e@(EDcD dcl) ((a,sv),(b,tv)) = res
    where
     res = case denote e of
            Flr  -> R (Funs a [dcl]) (Isn tv) (Funs b [])
            Frl  -> R (Funs a []) (Isn sv) (Funs b [dcl])
            Rn   -> R (Funs a []) (dcl) (Funs b [])
            Wrap -> fatal 246 "function res not defined when denote e == Wrap. "
   f _ e@(EFlp (EDcD dcl)) ((a,sv),(b,tv)) = res
    where
     res = case denote e of
            Flr  -> R (Funs a [dcl]) (Isn tv) (Funs b [])
            Frl  -> R (Funs a []) (Isn sv) (Funs b [dcl])
            Rn   -> R (Funs b []) (dcl) (Funs a [])
            Wrap -> fatal 253 "function res not defined when denote e == Wrap. "
   f exclVars (EFlp e)       (a,b) = f exclVars e (b,a)
   f _ (EMp1 val _) _             = Atom . showADL $ val
   f _ (EDcI _) ((a,_),(b,tv))     = R (Funs a []) (Isn tv) (Funs b [])
   f _ (EDcV _) _                  = Atom "True"
   f _ e _ = fatal 298 ("Non-exhaustive pattern in subexpression "++showADL e++" of assemble (<"++showADL expr++">)")

-- fECps treats the case of a composition.  It works as follows:
--       An expression, e.g. r;s;t , is translated to Exists (zip ivs ics) (Conj (frels s t)),
--       in which ivs is a list of variables that are used inside the resulting expression,
--       ics contains their types, and frels s t the subexpressions that
--       are used in the resulting conjuct (at the right of the quantifier).
   fECps :: [Var] -> Expression -> (Var,Var) -> PredLogic
   fECps exclVars    e             (a,b)
                            --   f :: [Var] -> Expression -> (Var,Var) -> PredLogic
     | and [isCpl e' | e'<-es] = f exclVars (deMorganECps e) (a,b)
     | otherwise               = Exists ivs (Conj (frels a b))
     where
      es :: [Expression]
      es   = [ x | x<-exprCps2list e, not (isEpsilon x) ]
     -- Step 1: split in fragments at those points where an exists-quantifier is needed.
     --         Each fragment represents a subexpression with variables
     --         at the outside only. Fragments will be reconstructed in a conjunct.
      res :: [(Var -> Var -> PredLogic, A_Concept, A_Concept)]
      res = pars3 (exclVars++ivs) (split es)  -- yields triples (r,s,t): the fragment, its source and target.
     -- Step 2: assemble the intermediate variables from at the right spot in each fragment.
      frels :: Var -> Var -> [PredLogic]
      frels src trg = [r v w | ((r,_,_),v,w)<-zip3 res' (src: ivs) (ivs++[trg]) ]
     -- Step 3: compute the intermediate variables and their types
      res' :: [(Var -> Var -> PredLogic, A_Concept, A_Concept)]
      res' = [triple | triple<-res, not (atomic triple)]
      ivs ::  [Var]
      ivs  = mkvar exclVars ics
      ics ::  [ Either PredLogic A_Concept ] -- each element is either an atom or a concept
      ics  = concat
             [ case (v',w) of
                 (Left _,    Left _   ) -> []
                 (Left atom, Right  _ ) -> [ Left atom ]
                 (Right  _ , Left atom) -> [ Left atom ]
                 (Right trg, Right  _ ) -> [ Right trg ] -- SJ 20131117, was: (if trg==src then [ Right trg ] else [ Right (trg `meet` src) ])
                                                         -- This code assumes no ISA's in the A-structure. This works due to the introduction of EEps expressions.
             | (v',w)<-zip [ case l ("",src) ("",trg) of
                              atom@Atom{} -> Left atom
                              _           -> Right trg
                           | (l,src,trg)<-init res]
                           [ case r ("",src) ("",trg) of
                              atom@Atom{} -> Left atom
                              _           -> Right src
                           | (r,src,trg)<-tail res]
             ]
   atomic :: (Var -> Var -> PredLogic, A_Concept, A_Concept) -> Bool
   atomic (r,a,b) = case r ("",a) ("",b) of
                     Atom{} -> True
                     _      -> False
   mkvar :: [Var] -> [ Either PredLogic A_Concept ] -> [Var]
   mkvar exclVars (Right z: ics) = let vz = head (mkVar exclVars [z]) in vz: mkvar (exclVars++[vz]) ics
   mkvar exclVars (Left  _: ics) = mkvar exclVars ics
   mkvar _ [] = []

   fERad :: [Var] -> Expression -> (Var,Var) -> PredLogic
   fERad exclVars e (a,b)
     | and[isCpl e' |e'<-es] = f exclVars (deMorganERad e) (a,b)                      -- e.g.  -r!-s!-t
     | isCpl (head es)       = f exclVars (foldr1 (.:.) antr .\. foldr1 (.!.) conr) (a,b)  -- e.g.  -r!-s! t  antr cannot be empty, because isCpl (head es) is True; conr cannot be empty, because es has an element that is not isCpl.
     | isCpl (last es)       = f exclVars (foldr1 (.!.) conl ./. foldr1 (.:.) antl) (a,b)  -- e.g.   r!-s!-t  antl cannot be empty, because isCpl (head es) is True; conl cannot be empty, because es has an element that is not isCpl.
     | otherwise             = Forall ivs (Disj (frels a b))                               -- e.g.   r!-s! t  the condition or [isCpl e' |e'<-es] is true.
{- was:
        | otherwise             = Forall ivs (Disj alls)
                                  where alls = [f (exclVars++ivs) e' (sv,tv) | (e',(sv,tv))<-zip es (zip (a:ivs) (ivs++[b]))]
-}
     where
      es   = [ x | x<-exprRad2list e, not (isEpsilon x) ] -- The definition of exprRad2list guarantees that length es>=2
      res  = pars3 (exclVars++ivs) (split es)  -- yields triples (r,s,t): the fragment, its source and target.
      conr = dropWhile isCpl es -- There is at least one positive term, because conr is used in the second alternative (and the first alternative deals with absence of positive terms).
                                -- So conr is not empty.
      antr = let x = (map notCpl.map flp.reverse.takeWhile isCpl) es in
             if null x then fatal 367 ("Entering in an empty foldr1") else x
      conl = let x = (reverse.dropWhile isCpl.reverse) es in
             if null x then fatal 369 ("Entering in an empty foldr1") else x
      antl = let x = (map notCpl.map flp.takeWhile isCpl.reverse) es in
             if null x then fatal 371 ("Entering in an empty foldr1") else x
     -- Step 2: assemble the intermediate variables from at the right spot in each fragment.
      frels :: Var -> Var -> [PredLogic]
      frels src trg = [r v w | ((r,_,_),v,w)<-zip3 res' (src: ivs) (ivs++[trg]) ]
     -- Step 3: compute the intermediate variables and their types
      res' :: [(Var -> Var -> PredLogic, A_Concept, A_Concept)]
      res' = [triple | triple<-res, not (atomic triple)]
      ivs ::  [Var]
      ivs  = mkvar exclVars ics
      ics ::  [ Either PredLogic A_Concept ] -- each element is either an atom or a concept
      ics  = concat
             [ case (v',w) of
                 (Left _,    Left _   ) -> []
                 (Left atom, Right  _ ) -> [ Left atom ]
                 (Right  _ , Left atom) -> [ Left atom ]
                 (Right trg, Right  _ ) -> [ Right trg ] -- SJ 20131117, was: (if trg==src then [ Right trg ] else [ Right (trg `meet` src) ])
                                                         -- This code assumes no ISA's in the A-structure. This works due to the introduction of EEps expressions.
             | (v',w)<-zip [ case l ("",src) ("",trg) of
                              atom@Atom{} -> Left atom
                              _           -> Right trg
                           | (l,src,trg)<-init res]
                           [ case r ("",src) ("",trg) of
                              atom@Atom{} -> Left atom
                              _           -> Right src
                           | (r,src,trg)<-tail res]
             ]

   relFun :: [Var] -> [Expression] -> Expression -> [Expression] -> Var->Var->PredLogic
   relFun exclVars lhs e rhs
     = case e of
         EDcD dcl        -> \sv tv->R (Funs (fst sv) [r | t'<-        lhs, r<-relsMentionedIn t']) dcl (Funs (fst tv) [r | t'<-reverse rhs, r<-relsMentionedIn t'])
         EFlp (EDcD dcl) -> \sv tv->R (Funs (fst tv) [r | t'<-reverse rhs, r<-relsMentionedIn t']) dcl (Funs (fst sv) [r | t'<-        lhs, r<-relsMentionedIn t'])
         EMp1 val _      -> \_ _-> Atom . showADL $ val
         EFlp EMp1{}     -> relFun exclVars lhs e rhs
         _               -> \sv tv->f (exclVars++[sv,tv]) e (sv,tv)

   pars3 :: [Var] -> [[Expression]] -> [(Var -> Var -> PredLogic, A_Concept, A_Concept)]
   pars3 exclVars (lhs: [e]: rhs: ts)
    | denotes lhs==Flr && denote e==Rn && denotes rhs==Frl
       = ( relFun exclVars lhs e rhs, source (head lhs), target (last rhs)): pars3 exclVars ts
    | otherwise = pars2 exclVars (lhs:[e]:rhs:ts)
   pars3 exclVars ts = pars2 exclVars ts -- for lists shorter than 3

   pars2 :: [Var] -> [[Expression]]-> [(Var -> Var -> PredLogic, A_Concept, A_Concept)]
   pars2 exclVars (lhs: [e]: ts)
    | denotes lhs==Flr && denote e==Rn
                = (relFun exclVars lhs e [], source (head lhs), target e): pars3 exclVars ts
    | denotes lhs==Flr && denote e==Frl
                = (relFun exclVars lhs (EDcI (source e)) [e], source (head lhs), target e): pars3 exclVars ts
    | otherwise = pars1 exclVars (lhs:[e]:ts)
   pars2 exclVars ([e]: rhs: ts)
    | denotes rhs==Frl && denote e==Rn
                = (relFun exclVars [] e rhs, source e, target (last rhs)): pars3 exclVars ts
    | denote e==Flr && denotes rhs==Frl
                = (relFun exclVars [e] (EDcI (source e)) rhs, source e, target (last rhs)): pars3 exclVars ts
    | otherwise = pars1 exclVars ([e]:rhs:ts)
   pars2 exclVars (lhs: rhs: ts)
    | denotes lhs==Flr && denotes rhs==Frl
                = (relFun exclVars lhs (EDcI (source (head rhs))) rhs, source (head lhs), target (last rhs)): pars3 exclVars ts
    | otherwise = pars1 exclVars (lhs:rhs:ts)
   pars2 exclVars ts = pars1 exclVars ts -- for lists shorter than 2

   pars1 :: [Var] -> [[Expression]] -> [(Var -> Var -> PredLogic, A_Concept, A_Concept)]
   pars1 exclVars expressions
     = case expressions of
         []        -> []
         (lhs: ts) -> (pars0 exclVars lhs, source (head lhs), target (last lhs)): pars3 exclVars ts

   pars0 :: [Var] -> [Expression] -> Var -> Var -> PredLogic
   pars0 exclVars lhs
    | denotes lhs==Flr = relFun exclVars lhs (EDcI (source (last lhs))) []
    | denotes lhs==Frl = relFun exclVars []  (EDcI (target (last lhs))) lhs
    | otherwise        = relFun exclVars [] (let [r]=lhs in r) []

   denote :: Expression -> Notation
   denote e = case e of
      (EDcD d)
        | null([Uni,Inj,Tot,Sur] >- properties d)  -> Rn
        | isUni d && isTot d                           -> Flr
        | isInj d && isSur d                           -> Frl
        | otherwise                                    -> Rn
      _                                                -> Rn
   denotes :: [Expression] -> Notation
   denotes = denote . head

   split :: [Expression] -> [[Expression]]
   split []  = []
   split [e] = [[e]]
   split (e:e':es)
    = --if denote e `eq` Wrap      then (e:spl):spls else
      if denote e `eq` denote e' then (e:spl):spls else
                                      [e]:spl:spls
      where
        spl:spls = split (e':es)
        Flr `eq` Flr = True
        Frl `eq` Frl = True
        _ `eq` _     = False

-- mkVar is bedoeld om nieuwe variabelen te genereren, gegeven een set (ex) van reeds vergeven variabelen.
-- mkVar garandeert dat het resultaat niet in ex voorkomt, dus postconditie:   not (mkVar ex cs `elem` ex)
-- Dat gebeurt door het toevoegen van apostrofes.
mkVar :: [Var] -> [A_Concept] -> [Var]
mkVar ex cs = mknew (map fst ex) [([(toLower.head.(++"x").name) c],c) |c<-cs]
 where
  mknew _ [] = []
  mknew ex' ((x,c):xs) = if x `elem` ex'
                         then mknew ex' ((x++"'",c):xs)
                         else (x,c): mknew (ex'++[x]) xs

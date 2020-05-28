{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.PredLogic
         ( -- PredLogicShow(..), mkVar
         ) where
{- TODO: Dit commentaarblok ont-commentaren en deze module netjes herschrijven!

import           Ampersand.ADL1
import           Ampersand.Basics
import           Ampersand.Classes
import           Ampersand.Core.ShowAStruct
import           Ampersand.Core.ShowPStruct
import           RIO.Char
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import qualified RIO.List as L
import           Text.Pandoc.Builder

--  data PredVar = PV Text     -- TODO Bedoeld om predicaten inzichtelijk te maken. Er bestaan namelijk nu verschillende manieren om hier mee om te gaan (zie ook Motivations. HJO.
data PredLogic
 = Forall [Var] PredLogic            |
   Exists [Var] PredLogic            |
   Implies PredLogic PredLogic       |
   Equiv PredLogic PredLogic         |
   Conj [PredLogic]                  |
   Disj [PredLogic]                  |
   Not PredLogic                     |
   Pred Text Text                |  -- Pred nm v, with v::type   is equiv. to Rel nm Nowhere [] (type,type) True (Relation (showA e) type type [] "" "" "" [Asy,Sym] Nowhere 0 False)
   PlK0 PredLogic                    |
   PlK1 PredLogic                    |
   R PredLogic Relation PredLogic |
   Atom Text                       |
   Funs Text [Relation]         |
   Dom Expression Var                |
   Cod Expression Var                deriving Eq

data Notation = Flr | Frl | Rn | Wrap deriving Eq   -- yields notations y=r(x)  |  x=r(y)  |  x r y  | exists ... respectively.

--   predKeyWords l =
--     case l of
--        English  ->

class PredLogicShow a where
  showPredLogic :: Lang -> a -> Text
  showPredLogic l r =
    predLshow (natLangOps l) (toPredLogic r) -- predLshow produces raw LaTeX
  toPredLogic :: a -> PredLogic

instance PredLogicShow Rule where
  toPredLogic ru = assemble (formalExpression ru)

instance PredLogicShow Expression where
  toPredLogic = assemble

data NatLangOpts a = NatLangOpts
   { forallP   :: Inline
   , existsP   :: Inline
   , impliesP  :: Inline -> Inline -> Inline
   , equivP    :: Inline
   , orP       :: Inline
   , andP      :: Inline
   , k0P       :: Inline
   , k1P       :: Inline
   , notP      :: Inline
   , relP      :: Relation -> Inline -> Inline -> Inline
   , funP      :: a -> Inline -> Inline
   , showVarsP :: [(Inline, A_Concept)] -> Block
   , breakP    :: Inline
   , spaceP    :: Inline
   , applyP    :: Relation -> Inline -> Inline -> Inline
   , elemP     :: Inline
   }
 
-- natLangOps exists for the purpose of translating a predicate logic expression to natural language.
-- It yields a vector of mostly strings, which are used to assemble a natural language text in one of the natural languages supported by Ampersand.
natLangOps :: Named a => Lang -> NatLangOpts a
natLangOps l = case l of
    English -> NatLangOpts
      { forallP   = "For each"
      , existsP   = "There exists"
      , impliesP  = implies
      , equivP    = "is equivalent to"
      , orP       = "or"
      , andP      = "and"
      , k0P       = "*"
      , k1P       = "+"
      , notP      = "not"
      , relP      = rel
      , funP      = fun
      , showVarsP = langVars
      , breakP    = "\n  "
      , spaceP    = " "
      , applyP    = apply
      , elemP     = "is element of"
      }    
    Dutch -> NatLangOpts
      { forallP   = "Voor elke"
      , existsP   = "Er is een"
      , impliesP  = implies
      , equivP    = "is equivalent met"
      , orP       = "of"
      , andP      = "en"
      , k0P       = "*"
      , k1P       = "+"
      , notP      = "niet"
      , relP      = rel
      , funP      = fun
      , showVarsP = langVars
      , breakP    = "\n  "
      , spaceP    = " "
      , applyP    = apply
      , elemP     = "is element van"
      }    
  where
               rel = apply
               fun r x' = {- Todo: when using pandoc stuff: showMath -} (name r)<>"("<>x'<>")"
               implies antc cons = case l of
                                     English  -> "If "<>antc<>", then "<>cons
                                     Dutch    -> "Als "<>antc<>", dan "<>cons
               apply decl d c =
                  if T.null (prL<>prM<>prR)
                  then "$"<>d<>"$ "<>name decl<>" $"<>c<>"$"
                  else prL<>" $"<>d<>"$ "<>prM<>" $"<>c<>"$ "<>prR
                 where prL = decprL decl
                       prM = decprM decl
                       prR = decprR decl
               langVars :: Text -> [(Text, A_Concept)] -> Text
               langVars q vs
                   = case l of
                      English | null vs     -> ""
                              | q=="Exists" ->
                                  T.intercalate " and "
                                  ["there exist"
                                   <>(if length vs'==1 then "s a "<>dType else " "<>plural English dType)
                                   <>" called "
                                   <>T.intercalate ", " (map math vs') | (vs',dType)<-vss]
                              | otherwise   -> "If "<>langVars "Exists" vs<>", "
                      Dutch   | null vs     -> ""
                              | q=="Er is"  ->
                                  T.intercalate " en "
                                  ["er "
                                    <>(if length vs'==1 then "is een "<>dType else "zijn "<>plural Dutch dType)
                                    <>" genaamd "
                                    <>T.intercalate ", " (map math vs') | (vs',dType)<-vss]
                              | otherwise   -> "Als "<>langVars "Er is" vs<>", "
                    where
                     vss = [(NE.toList $ fmap fst vs',tshow(snd (NE.head vs'))) |vs'<-eqCl snd vs]

-- predLshow exists for the purpose of translating a predicate logic expression to natural language.
-- It uses a vector of operators (mostly strings) in order to produce text. This vector can be produced by, for example, natLangOps.
-- example:  'predLshow (natLangOps l) e' translates expression 'e'
-- into a string that contains a natural language representation of 'e'.
predLshow :: ( Text                                    -- forallP
             , Text                                    -- existsP
             , Text -> Text -> Text                -- impliesP
             , Text                                    -- equivP
             , Text                                    -- orP
             , Text                                    -- andP
             , Text                                    -- kleene *
             , Text                                    -- kleene +
             , Text                                    -- notP
             , Relation -> Text -> Text -> Text    -- relP
             , Relation -> Text -> Text              -- funP
             , Text -> [(Text, A_Concept)] -> Text -- showVarsP
             , Text                                    -- breakP
             , Text                                    -- spaceP
             , Relation -> Text -> Text -> Text -- apply
             , Text                                    -- set element
             ) -> PredLogic -> Text
predLshow (forallP, existsP, impliesP, equivP, orP, andP, k0P, k1P, notP, relP, funP, showVarsP, breakP, spaceP, apply, el)
 = charshow 0
     where
      wrap i j str = if i<=j then str else "("<>str<>")"
      charshow :: Integer -> PredLogic -> Text
      charshow i predexpr
       = case predexpr of
               Forall vars restr   -> wrap i 1 (showVarsP forallP vars <> charshow 1 restr)
               Exists vars restr   -> wrap i 1 (showVarsP existsP vars  <> charshow 1 restr)
               Implies antc conseq -> wrap i 2 (breakP<>impliesP (charshow 2 antc) (charshow 2 conseq))
               Equiv lhs rhs       -> wrap i 2 (breakP<>charshow 2 lhs<>spaceP<>equivP<>spaceP<> charshow 2 rhs)
               Disj rs             -> if null rs
                                      then ""
                                      else wrap i 3 (T.intercalate (spaceP<>orP <>spaceP) (map (charshow 3) rs))
               Conj rs             -> if null rs
                                      then ""
                                      else wrap i 4 (T.intercalate (spaceP<>andP<>spaceP) (map (charshow 4) rs))
               Funs x ls           -> case ls of
                                         []    -> x
                                         r:ms  -> if isIdent (EDcD r) then charshow i (Funs x ms) else charshow i (Funs (funP r x) ms)
               Dom expr (x,_)      -> x<>el<>funP (makeRel "dom") (showA expr)
               Cod expr (x,_)      -> x<>el<>funP (makeRel "cod") (showA expr)
               R pexpr dec pexpr'  -> case (pexpr,pexpr') of
                                         (Funs l [] , Funs r [])  -> wrap i 5 (apply dec l r)
{-
                                            (Funs l [f], Funs r [])  -> wrap i 5 (if isIdent rel
                                                                                  then apply (makeRelation f) l r
                                                                                  else apply (makeRelation rel) (funP f l) r)
                                            (Funs l [] , Funs r [f]) -> wrap i 5 (if isIdent rel
                                                                                  then apply (makeRelation f) l r
                                                                                  else apply (makeRelation rel) l (funP f r))
-}
                                         (lhs,rhs)                -> wrap i 5 (relP dec (charshow 5 lhs) (charshow 5 rhs))
               Atom atom           -> "'"<>atom<>"'"
               PlK0 rs             -> wrap i 6 (charshow 6 rs<>k0P)
               PlK1 rs             -> wrap i 7 (charshow 7 rs<>k1P)
               Not rs              -> wrap i 8 (spaceP<>notP<>charshow 8 rs)
               Pred nm v'          -> nm<>"{"<>v'<>"}"
      makeRel :: Text -> Relation -- This function exists solely for the purpose of dom and cod
      makeRel str
          = Relation { decnm   = T.pack str
                   , decsgn  = fatal "Do not refer to decsgn of this dummy relation"
                   , decprps = Set.fromList [Uni,Tot]
                   , decprps_calc = Nothing
                   , decprL  = ""
                   , decprM  = ""
                   , decprR  = ""
                   , decMean = fatal "Do not refer to decMean of this dummy relation"
                   , decfpos = OriginUnknown
                   , decusr  = False
                   , decpat  = fatal "Do not refer to decpat of this dummy relation"
                   , dechash = fatal "Do not use EQ on this dummy relation"
                   }

--objOrShow :: Lang -> PredLogic -> Text
--objOrShow l = predLshow ("For all", "Exists", implies, " = ", " = ", "<>", "OR", "AND", "*", "+", "NOT", rel, fun, langVars l, "\n", " ")
--               where rel r lhs rhs = applyM (makeRelation r) lhs rhs
--                     fun r x = x<>"."<>name r
--                     implies antc cons = "IF "<>antc<>" THEN "<>cons

-- The function 'assemble' translates a rule to predicate logic.
-- In order to remain independent of any representation, it transforms the Haskell data structure Rule
-- into the data structure PredLogic, rather than manipulate with texts.
type Var = (Text,A_Concept)
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
   f exclVars e@EIsc{}     (a,b)  = Conj [f exclVars e' (a,b) | e'<-NE.toList $ exprIsc2list e]
   f exclVars e@EUni{}     (a,b)  = Disj [f exclVars e' (a,b) | e'<-NE.toList $ exprUni2list e]
   f exclVars (EDif (l,r)) (a,b)  = Conj [f exclVars l (a,b), Not (f exclVars r (a,b))]
   f exclVars (ELrs (l,r)) (a,b)  = Forall [c] (Implies (f eVars r (b,c)) (f eVars l (a,c)))
                                    where [c]   = mkVar exclVars [target l]
                                          eVars = exclVars<>[c]
   f exclVars (ERrs (l,r)) (a,b)  = Forall [c] (Implies (f eVars l (c,a)) (f eVars r (c,b)))
                                    where [c]   = mkVar exclVars [source l]
                                          eVars = exclVars<>[c]
   f exclVars (EDia (l,r)) (a,b)  = Forall [c] (Equiv (f eVars r (b,c)) (f eVars l (a,c)))
                                    where [c]   = mkVar exclVars [target l]
                                          eVars = exclVars<>[c]
   f exclVars e@ECps{}     (a,b)  = fECps exclVars e (a,b)  -- special treatment, see below
   f exclVars e@ERad{}     (a,b)  = fERad exclVars e (a,b)  -- special treatment, see below
   f _        (EPrd (l,r)) (a,b)  = Conj [Dom l a, Cod r b]
   f exclVars (EKl0 e)     (a,b)  = PlK0 (f exclVars e (a,b))
   f exclVars (EKl1 e)     (a,b)  = PlK1 (f exclVars e (a,b))
   f exclVars (ECpl e)     (a,b)  = Not (f exclVars e (a,b))
   f exclVars (EBrk e)     (a,b)  = f exclVars e (a,b)
   f _ e@(EDcD dcl) ((a,_{-sv-}),(b,_{-tv-})) = res
    where
     res = case denote e of
            Flr  -> fatal "Bitrot! R (Funs a [dcl]) (Isn tv) (Funs b [])"
            Frl  -> fatal "Bitrot! R (Funs a []) (Isn sv) (Funs b [dcl])"
            Rn   -> R (Funs a []) dcl (Funs b [])
            Wrap -> fatal "function res not defined when denote e == Wrap. "
   f _ e@(EFlp (EDcD dcl)) ((a,_{-sv-}),(b,_{-tv-})) = res
    where
     res = case denote e of
            Flr  -> fatal "Bitrot! R (Funs a [dcl]) (Isn tv) (Funs b [])"
            Frl  -> fatal "Bitrot! R (Funs a []) (Isn sv) (Funs b [dcl])"
            Rn   -> R (Funs b []) dcl (Funs a [])
            Wrap -> fatal "function res not defined when denote e == Wrap. "
   f exclVars (EFlp e)       (a,b) = f exclVars e (b,a)
   f _ (EMp1 val _) _             = Atom . showP $ val
   f _ (EDcI _) ((_{-a-},_),(_{-b-},_{-tv-}))     = fatal "Bitrot! R (Funs a []) (Isn tv) (Funs b [])"
   f _ (EDcV _) _                  = Atom "True"
   f _ e _ = fatal ("Non-exhaustive pattern in subexpression "<>showA e<>" of assemble (<"<>showA expr<>">)")

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
      es   = NE.filter (not . isEpsilon) $ exprCps2list e
     -- Step 1: split in fragments at those points where an exists-quantifier is needed.
     --         Each fragment represents a subexpression with variables
     --         at the outside only. Fragments will be reconstructed in a conjunct.
      res :: [(Var -> Var -> PredLogic, A_Concept, A_Concept)]
      res = pars3 (exclVars<>ivs) (split es)  -- yields triples (r,s,t): the fragment, its source and target.
     -- Step 2: assemble the intermediate variables from at the right spot in each fragment.
      frels :: Var -> Var -> [PredLogic]
      frels src trg = [r v w | ((r,_,_),v,w)<-L.zip3 res' (src: ivs) (ivs<>[trg]) ]
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
   mkvar exclVars (Right z: ics) = let vz = head (mkVar exclVars [z]) in vz: mkvar (exclVars<>[vz]) ics
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
                                  where alls = [f (exclVars<>ivs) e' (sv,tv) | (e',(sv,tv))<-zip es (zip (a:ivs) (ivs<>[b]))]
-}
     where
      es   = NE.filter (not . isEpsilon) $ exprRad2list e -- The definition of exprRad2list guarantees that length es>=2
      res  = pars3 (exclVars<>ivs) (split es)  -- yields triples (r,s,t): the fragment, its source and target.
      conr = dropWhile isCpl es -- There is at least one positive term, because conr is used in the second alternative (and the first alternative deals with absence of positive terms).
                                -- So conr is not empty.
      antr = let x = (map (notCpl . flp) . reverse . takeWhile isCpl) es in
             if null x then fatal "Entering in an empty foldr1" else x
      conl = let x = (reverse . dropWhile isCpl . reverse) es in
             if null x then fatal "Entering in an empty foldr1" else x
      antl = let x = (map (notCpl . flp) . takeWhile isCpl . reverse) es in
             if null x then fatal "Entering in an empty foldr1" else x
     -- Step 2: assemble the intermediate variables from at the right spot in each fragment.
      frels :: Var -> Var -> [PredLogic]
      frels src trg = [r v w | ((r,_,_),v,w)<-L.zip3 res' (src: ivs) (ivs<>[trg]) ]
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
         EDcD dcl        -> \sv tv->R (Funs (fst sv) [r | t'<-        lhs, r<-Set.elems $ bindedRelationsIn t']) dcl (Funs (fst tv) [r | t'<-reverse rhs, r<-Set.elems $ bindedRelationsIn t'])
         EFlp (EDcD dcl) -> \sv tv->R (Funs (fst tv) [r | t'<-reverse rhs, r<-Set.elems $ bindedRelationsIn t']) dcl (Funs (fst sv) [r | t'<-        lhs, r<-Set.elems $ bindedRelationsIn t'])
         EMp1 val _      -> \_ _-> Atom . showP $ val
         EFlp EMp1{}     -> relFun exclVars lhs e rhs
         _               -> \sv tv->f (exclVars<>[sv,tv]) e (sv,tv)

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
        | null(Set.elems (Set.fromList [Uni,Inj,Tot,Sur] Set.\\ properties d))  -> Rn
        | isUni e && isTot e                           -> Flr
        | isInj e && isSur e                           -> Frl
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
mkVar ex cs = mknew (map fst ex) [([(toLower.head.(<>"x").name) c],c) |c<-cs]
 where
  mknew _ [] = []
  mknew ex' ((x,c):xs) = if x `elem` ex'
                         then mknew ex' ((x<>"'",c):xs)
                         else (x,c): mknew (ex'<>[x]) xs


-- TODO: Rewrite this module. Also get rid of:
head,last :: [a] -> a
head [] = fatal $ "head is used on an empty list."
head (h:_) = h
last [] = fatal $ "last is used on an empty list."
last x = head . reverse $ x
tail, init :: [a] -> [a]
tail [] = fatal $ "tail is used on an empty list."
tail (_:tl) = tl
init [] = fatal $ "init is used on an empty list."
init x = tail . reverse $ x
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 fun nonEmptyList = foldr fun (head nonEmptyList) (tail nonEmptyList)

-}
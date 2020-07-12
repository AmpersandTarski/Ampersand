{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.PredLogic
         ( showPredLogic
         ) where

import           Ampersand.ADL1
import           Ampersand.Basics hiding (toList)
import           Ampersand.Classes
import           Ampersand.Core.ShowAStruct
import           Ampersand.Core.ShowPStruct
import           RIO.Char
import qualified RIO.NonEmpty as NE
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.Set as Set
import qualified RIO.Text as T
import           Text.Pandoc.Builder
import           Ampersand.Output.PandocAux(showMath)
type VarMap = M.Map Var Text

data PredLogic = 
    Forall VarMap (NE.NonEmpty Var) PredLogic
  | Exists VarMap (NE.NonEmpty Var) PredLogic
  | Implies VarMap PredLogic PredLogic
  | Equiv VarMap PredLogic PredLogic
  | Conj VarMap [PredLogic]
  | Disj VarMap [PredLogic]
  | Not VarMap PredLogic
  | Kleene0 VarMap PredLogic
  | Kleene1 VarMap PredLogic
  | R VarMap PredLogic Relation PredLogic
  -- ^ R _ a r b is represented as a r b 
  --  but if isIdent r then it is represented as a = b
  | Constant PAtomValue
  -- ^ A constant. e.g.: "Churchill", 1
  | Function VarMap PredLogic Relation
  -- ^ Function a f is represented in text as f(a)
  | Dom VarMap Expression Var
  -- ^ Dom expr (a,_) is represented as a ∈ dom(expr) 
  | Cod VarMap Expression Var
    deriving Eq

data Var = Var Integer A_Concept
   deriving (Eq,Ord,Show)

showPredLogic :: Lang -> Expression -> Inlines
showPredLogic lang = predLshow lang . predNormalize . toPredLogic

-- should be exported by Pandoc?
intercalate :: Inlines -> Many Inlines -> Inlines
intercalate x = fromList . L.intercalate (toList x) . map toList . toList 

predLshow :: Lang -> PredLogic -> Inlines
predLshow lang predlogic = charshow 0 predlogic
     where
        -- shorthand for easy localizing    
      l :: LocalizedStr -> Text
      l = localize lang
      listVars :: Inlines -> VarMap -> NE.NonEmpty Var -> Inlines
      listVars sep vMap vars = intercalate  sep . fromList . NE.toList . fmap (showVar vMap) $ vars
      showVar :: VarMap -> Var -> Inlines
      showVar vMap var = case M.lookup var vMap of
                                Nothing -> fatal $ "Variable not found:" <> tshow var
                                Just t  -> text t

      wrap :: Integer -> Integer -> Inlines -> Inlines
      wrap i j txt = if i<=j then txt else "("<>txt<>")"
      charshow :: Integer -> PredLogic -> Inlines
      charshow i predexpr
       = case predexpr of
               Forall vMap vars restr   -> wrap i 1 (               (text . l)(NL "Voor alle", EN "For all") 
                                                       <> listVars ((text . l)(NL "en voor alle", EN "and for all")
                                                    ) vMap vars <> charshow 1 restr)
               Exists vMap vars restr   -> wrap i 1 (               (text . l)(NL "Er is een", EN "There exists")
                                                       <> listVars ((text . l)(NL "en er is een", EN "and there exists")
                                                    ) vMap vars <> charshow 1 restr)
               Implies _ ante cons -> wrap i 2 (linebreak<>implies (charshow 2 ante) (charshow 2 cons))
                                             where implies :: Inlines -> Inlines -> Inlines
                                                   implies a b =
                                                      (text . l)(NL "Als ",EN "If" )<>a<>(text.l)(NL" dan ",EN " then ")<>b
               Equiv _ lhs rhs       -> wrap i 2 (linebreak<>charshow 2 lhs<>space<>((text . l)(NL "is equivalent met",EN "is equivalent to"))<>space<> charshow 2 rhs)
               Disj _ rs             -> if null rs
                                           then mempty
                                           else wrap i 3 (chain ((text.l)(NL " of ",EN" or ")) (map (charshow 3) rs))
               Conj _ rs             -> if null rs
                                           then mempty
                                           else wrap i 4 (chain ((text.l)(NL " en ",EN" and ")) (map (charshow 4) rs))
--               Funs x ls           -> case ls of
--                                         []    -> x
--                                         r:ms  -> if isIdent (EDcD r) then charshow i (Funs x ms) else charshow i (Funs (fun r x) ms)
               Dom vMap expr var      -> showVar vMap var<>" ∈ dom(" <>showMath expr<>")"
               Cod vMap expr var      -> showVar vMap var<>" ∈ cod(" <>showMath expr<>")"
               R _ pexpr rel pexpr'  
                   | isIdent (EDcD rel) -> wrap i 5 (charshow 5 pexpr) <> " = " <> wrap i 5 (charshow 5 pexpr')
                   | otherwise          -> if T.null (prL<>prM<>prR)
                                              then d<>text (name rel)<>c
                                              else text prL<>d<>text prM<>c<>text prR
                                        where d = wrap i 5 (charshow 5 pexpr)
                                              c = wrap i 5 (charshow 5 pexpr')
                                              prL = decprL rel
                                              prM = decprM rel
                                              prR = decprR rel
               Constant atom           -> showMath atom
               Kleene0 _ rs             -> wrap i 6 (charshow 6 rs<>"*")
               Kleene1 _ rs             -> wrap i 7 (charshow 7 rs<>"+")
               Not _ rs              -> wrap i 8 ((text . l)(NL " niet",EN " not")<>charshow 8 rs)


predNormalize :: PredLogic -> PredLogic
predNormalize predlogic = predlogic  --TODO: Fix normalization of PredLogic
toPredLogic :: Expression -> PredLogic
toPredLogic expr = undefined  
{-
data Notation = Flr | Frl | Rn | Wrap deriving Eq   -- yields notations y=r(x)  |  x=r(y)  |  x r y  | exists ... respectively.
data NatLangOpts = NatLangOpts
   { forallP   :: Inlines
   , existsP   :: Inlines
   , impliesP  :: Inlines -> Inlines -> Inlines
   , equivP    :: Inlines
   , orP       :: Inlines
   , andP      :: Inlines
   , k0P       :: Inlines
   , k1P       :: Inlines
   , notP      :: Inlines
   , relP      :: Relation -> Inlines -> Inlines -> Inlines
   , funP      :: Relation -> Inlines -> Inlines
   , showVarsP :: Inlines -> [(Inline,A_Concept)] -> Inlines
   , applyP    :: Relation -> Inlines -> Inlines -> Inlines
   , elemP     :: Inlines
   }
 
-- natLangOps exists for the purpose of translating a predicate logic expression to natural language.
-- It yields a vector of mostly strings, which are used to assemble a natural language text in one of the natural languages supported by Ampersand.
natLangOps :: Lang -> NatLangOpts
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
      , relP      = apply
      , funP      = fun
      , showVarsP = langVars
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
      , relP      = apply
      , funP      = fun
      , showVarsP = langVars
      , applyP    = apply
      , elemP     = "is element van"
      }    
  where
               fun :: Relation -> Inlines -> Inlines
               fun r x' = {- Todo: when using pandoc stuff: showMath (@Han: why?)-}
                 text (name r)<>"("<>x'<>")"
               implies :: Inlines -> Inlines -> Inlines
               implies antc cons = case l of
                                     English  -> "If "<>antc<>", then "<>cons
                                     Dutch    -> "Als "<>antc<>", dan "<>cons
               apply :: Relation -> Inlines -> Inlines -> Inlines
               apply decl d c =
                  if T.null (prL<>prM<>prR)
                  then "$"<>d<>"$ "<>text (name decl)<>" $"<>c<>"$"
                  else text prL<>" $"<>d<>"$ "<>text prM<>" $"<>c<>"$ "<>text prR
                 where prL = decprL decl
                       prM = decprM decl
                       prR = decprR decl
               langVars :: Inlines -> [(Inline,A_Concept)] -> Inlines
               langVars q vs
                   = case l of
                      English | null vs     -> ""
                              | q=="There exists" ->
                                  chain " and "
                                  [ "there exist"
                                    <>(if length vs'==1
                                       then "s a "<>dType
                                       else " "<>(text . plural English . tshow) dType
                                      )
                                    <>" called "
                                    <>chain ", " (fmap singleton vs')
                                  | (vs',dType)<-vss]
                              | otherwise   -> "If "<>langVars "There exists" vs<>", "
                      Dutch   | null vs     -> ""
                              | q=="Er is een"  ->
                                  chain " en "
                                  ["er "
                                    <>(if length vs'==1
                                       then "is een "<>dType
                                       else "zijn "<>(text . plural Dutch . tshow) dType
                                      )
                                    <>" genaamd "
                                    <>chain ", " (fmap singleton vs')
                                  | (vs',dType)<-vss]
                              | otherwise   -> "Als "<>langVars "Er is een" vs<>", "
                    where
                      vss :: [([Inline], Inlines)]
                      vss = [ ( (NE.toList . fmap fst) vs'
                              , (text . tshow . snd  . NE.head) vs'
                              )
                            | vs'<-eqCl snd vs]

-- predLshow exists for the purpose of translating a predicate logic expression to natural language.
-- It uses a vector of operators (mostly strings) in order to produce text. This vector can be produced by, for example, natLangOps.
-- example:  'predLshow (natLangOps l) e' translates expression 'e'
-- into a string that contains a natural language representation of 'e'.
predLshow :: NatLangOpts -> PredLogic -> Inlines
predLshow NatLangOpts
   { forallP   = forall   -- :: Inlines
   , existsP   = exists   -- :: Inlines
   , impliesP  = implies  -- :: Inlines -> Inlines -> Inlines
   , equivP    = equiv    -- :: Inlines
   , orP       = or'      -- :: Inlines
   , andP      = and'     -- :: Inlines
   , k0P       = k0       -- :: Inlines
   , k1P       = k1       -- :: Inlines
   , notP      = not'     -- :: Inlines
   , relP      = rel      -- :: Relation -> Inlines -> Inlines -> Inlines
   , funP      = fun      -- :: Relation -> Inlines -> Inlines
   , showVarsP = showVars -- :: Inlines -> [(Inline,A_Concept)] -> Inlines
   , applyP    = apply    -- :: Relation -> Inlines -> Inlines -> Inlines
   , elemP     = elem'    -- :: Inlines
   }
 
 = charshow 0
     where
      wrap :: Integer -> Integer -> Inlines -> Inlines
      wrap i j txt = if i<=j then txt else "("<>txt<>")"
      charshow :: Integer -> PredLogic -> Inlines
      charshow i predexpr
       = case predexpr of
               Forall vars restr   -> wrap i 1 (showVars forall vars <> charshow 1 restr)
               Exists vars restr   -> wrap i 1 (showVars exists vars  <> charshow 1 restr)
               Implies antc conseq -> wrap i 2 (linebreak<>implies (charshow 2 antc) (charshow 2 conseq))
               Equiv lhs rhs       -> wrap i 2 (linebreak<>charshow 2 lhs<>space<>equiv<>space<> charshow 2 rhs)
               Disj rs             -> if null rs
                                      then ""
                                      else wrap i 3 (chain (space<>or' <>space) (map (charshow 3) rs))
               Conj rs             -> if null rs
                                      then ""
                                      else wrap i 4 (chain (space<>and'<>space) (map (charshow 4) rs))
               Funs x ls           -> case ls of
                                         []    -> x
                                         r:ms  -> if isIdent (EDcD r) then charshow i (Funs x ms) else charshow i (Funs (fun r x) ms)
               Dom expr (x,_)      -> singleton x<>elem'<>fun (makeRel "dom") (text (showA expr))
               Cod expr (x,_)      -> singleton x<>elem'<>fun (makeRel "cod") (text (showA expr))
               R pexpr dec pexpr'  -> case (pexpr,pexpr') of
                                         (Funs l [] , Funs r [])  -> wrap i 5 (apply dec l r)
{-
                                            (Funs l [f], Funs r [])  -> wrap i 5 (if isIdent rel
                                                                                  then apply (makeRelation f) l r
                                                                                  else apply (makeRelation rel) (fun f l) r)
                                            (Funs l [] , Funs r [f]) -> wrap i 5 (if isIdent rel
                                                                                  then apply (makeRelation f) l r
                                                                                  else apply (makeRelation rel) l (fun f r))
-}
                                         (lhs,rhs)                -> wrap i 5 (rel dec (charshow 5 lhs) (charshow 5 rhs))
               Constant atom           -> "'"<>atom<>"'"
               Kleene0 rs             -> wrap i 6 (charshow 6 rs<>k0)
               Kleene1 rs             -> wrap i 7 (charshow 7 rs<>k1)
               Not rs              -> wrap i 8 (space<>not'<>charshow 8 rs)
               Pred nm v'          -> nm<>"{"<>v'<>"}"
      makeRel :: Text -> Relation -- This function exists solely for the purpose of dom and cod
      makeRel txt
        = Relation { decnm   = tshow txt
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
   f varMap (EEqu (l,r)) (a,b)  = Equiv (f varMap l (a,b)) (f varMap r (a,b))
   f varMap (EInc (l,r)) (a,b)  = Implies (f varMap l (a,b)) (f varMap r (a,b))
   f varMap e@EIsc{}     (a,b)  = Conj [f varMap e' (a,b) | e'<-NE.toList $ exprIsc2list e]
   f varMap e@EUni{}     (a,b)  = Disj [f varMap e' (a,b) | e'<-NE.toList $ exprUni2list e]
   f varMap (EDif (l,r)) (a,b)  = Conj [f varMap l (a,b), Not (f varMap r (a,b))]
   f varMap (ELrs (l,r)) (a,b)  = Forall [c] (Implies (f eVars r (b,c)) (f eVars l (a,c)))
                                    where [c]   = mkVar varMap [target l]
                                          eVars = varMap<>[c]
   f varMap (ERrs (l,r)) (a,b)  = Forall [c] (Implies (f eVars l (c,a)) (f eVars r (c,b)))
                                    where [c]   = mkVar varMap [source l]
                                          eVars = varMap<>[c]
   f varMap (EDia (l,r)) (a,b)  = Forall [c] (Equiv (f eVars r (b,c)) (f eVars l (a,c)))
                                    where [c]   = mkVar varMap [target l]
                                          eVars = varMap<>[c]
   f varMap e@ECps{}     (a,b)  = fECps varMap e (a,b)  -- special treatment, see below
   f varMap e@ERad{}     (a,b)  = fERad varMap e (a,b)  -- special treatment, see below
   f _        (EPrd (l,r)) (a,b)  = Conj [Dom l a, Cod r b]
   f varMap (EKl0 e)     (a,b)  = Kleene0 (f varMap e (a,b))
   f varMap (EKl1 e)     (a,b)  = Kleene1 (f varMap e (a,b))
   f varMap (ECpl e)     (a,b)  = Not (f varMap e (a,b))
   f varMap (EBrk e)     (a,b)  = f varMap e (a,b)
   f _ e@(EDcD dcl) ((a,_{-sv-}),(b,_{-tv-})) = res
    where
     res = case denote e of
            Flr  -> fatal "Bitrot! R (Funs a [dcl]) (Isn tv) (Funs b [])"
            Frl  -> fatal "Bitrot! R (Funs a []) (Isn sv) (Funs b [dcl])"
            Rn   -> R (Funs (singleton a) []) dcl (Funs (singleton b) [])
            Wrap -> fatal "function res not defined when denote e == Wrap. "
   f _ e@(EFlp (EDcD dcl)) ((a,_{-sv-}),(b,_{-tv-})) = res
    where
     res = case denote e of
            Flr  -> fatal "Bitrot! R (Funs a [dcl]) (Isn tv) (Funs b [])"
            Frl  -> fatal "Bitrot! R (Funs a []) (Isn sv) (Funs b [dcl])"
            Rn   -> R (Funs (singleton b) []) dcl (Funs (singleton a) [])
            Wrap -> fatal "function res not defined when denote e == Wrap. "
   f varMap (EFlp e)       (a,b) = f varMap e (b,a)
   f _ (EMp1 val _) _             = (Constant . text . showP) val
   f _ (EDcI _) ((_{-a-},_),(_{-b-},_{-tv-}))     = fatal "Bitrot! R (Funs a []) (Isn tv) (Funs b [])"
   f _ (EDcV _) _                  = Constant "True"
   f _ e _ = fatal ("Non-exhaustive pattern in subexpression "<>showA e<>" of assemble (<"<>showA expr<>">)")

-- fECps treats the case of a composition.  It works as follows:
--       An expression, e.g. r;s;t , is translated to Exists (zip ivs ics) (Conj (frels s t)),
--       in which ivs is a list of variables that are used inside the resulting expression,
--       ics contains their types, and frels s t the subexpressions that
--       are used in the resulting conjuct (at the right of the quantifier).
   fECps :: [Var] -> Expression -> (Var,Var) -> PredLogic
   fECps varMap    e             (a,b)
                            --   f :: [Var] -> Expression -> (Var,Var) -> PredLogic
     | and [isCpl e' | e'<-es] = f varMap (deMorganECps e) (a,b)
     | otherwise               = Exists ivs (Conj (frels a b))
     where
      es :: [Expression]
      es   = NE.filter (not . isEpsilon) $ exprCps2list e
     -- Step 1: split in fragments at those points where an exists-quantifier is needed.
     --         Each fragment represents a subexpression with variables
     --         at the outside only. Fragments will be reconstructed in a conjunct.
      res :: [(Var -> Var -> PredLogic, A_Concept, A_Concept)]
      res = pars3 (varMap<>ivs) (split es)  -- yields triples (r,s,t): the fragment, its source and target.
     -- Step 2: assemble the intermediate variables from at the right spot in each fragment.
      frels :: Var -> Var -> [PredLogic]
      frels src trg = [r v w | ((r,_,_),v,w)<-L.zip3 res' (src: ivs) (ivs<>[trg]) ]
     -- Step 3: compute the intermediate variables and their types
      res' :: [(Var -> Var -> PredLogic, A_Concept, A_Concept)]
      res' = [triple | triple<-res, not (atomic triple)]
      ivs ::  [Var]
      ivs  = mkvar varMap ics
      ics ::  [ Either PredLogic A_Concept ] -- each element is either an atom or a concept
      ics  = concat
             [ case (v',w) of
                 (Left _,    Left _   ) -> []
                 (Left atom, Right  _ ) -> [ Left atom ]
                 (Right  _ , Left atom) -> [ Left atom ]
                 (Right trg, Right  _ ) -> [ Right trg ] -- SJ 20131117, was: (if trg==src then [ Right trg ] else [ Right (trg `meet` src) ])
                                                         -- This code assumes no ISA's in the A-structure. This works due to the introduction of EEps expressions.
             | (v',w)<-zip [ case l (Str "",src) (Str "",trg) of
                              atom@Constant{} -> Left atom
                              _           -> Right trg
                           | (l,src,trg)<-init res]
                           [ case r (Str "",src) (Str "",trg) of
                              atom@Constant{} -> Left atom
                              _           -> Right src
                           | (r,src,trg)<-tail res]
             ]
   atomic :: (Var -> Var -> PredLogic, A_Concept, A_Concept) -> Bool
   atomic (r,a,b) = case r (Str "",a) (Str "",b) of
                     Constant{} -> True
                     _      -> False
   mkvar :: [Var] -> [ Either PredLogic A_Concept ] -> [Var]
   mkvar varMap (Right z: ics) = let vz = head (mkVar varMap [z]) in vz: mkvar (varMap<>[vz]) ics
   mkvar varMap (Left  _: ics) = mkvar varMap ics
   mkvar _ [] = []

   fERad :: [Var] -> Expression -> (Var,Var) -> PredLogic
   fERad varMap e (a,b)
     | and[isCpl e' |e'<-es] = f varMap (deMorganERad e) (a,b)                      -- e.g.  -r!-s!-t
     | isCpl (head es)       = f varMap (foldr1 (.:.) antr .\. foldr1 (.!.) conr) (a,b)  -- e.g.  -r!-s! t  antr cannot be empty, because isCpl (head es) is True; conr cannot be empty, because es has an element that is not isCpl.
     | isCpl (last es)       = f varMap (foldr1 (.!.) conl ./. foldr1 (.:.) antl) (a,b)  -- e.g.   r!-s!-t  antl cannot be empty, because isCpl (head es) is True; conl cannot be empty, because es has an element that is not isCpl.
     | otherwise             = Forall ivs (Disj (frels a b))                               -- e.g.   r!-s! t  the condition or [isCpl e' |e'<-es] is true.
{- was:
        | otherwise             = Forall ivs (Disj alls)
                                  where alls = [f (varMap<>ivs) e' (sv,tv) | (e',(sv,tv))<-zip es (zip (a:ivs) (ivs<>[b]))]
-}
     where
      es   = NE.filter (not . isEpsilon) $ exprRad2list e -- The definition of exprRad2list guarantees that length es>=2
      res  = pars3 (varMap<>ivs) (split es)  -- yields triples (r,s,t): the fragment, its source and target.
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
      ivs  = mkvar varMap ics
      ics ::  [ Either PredLogic A_Concept ] -- each element is either an atom or a concept
      ics  = concat
             [ case (v',w) of
                 (Left _,    Left _   ) -> []
                 (Left atom, Right  _ ) -> [ Left atom ]
                 (Right  _ , Left atom) -> [ Left atom ]
                 (Right trg, Right  _ ) -> [ Right trg ] -- SJ 20131117, was: (if trg==src then [ Right trg ] else [ Right (trg `meet` src) ])
                                                         -- This code assumes no ISA's in the A-structure. This works due to the introduction of EEps expressions.
             | (v',w)<-zip [ case l (Str "",src) (Str "",trg) of
                              atom@Constant{} -> Left atom
                              _           -> Right trg
                           | (l,src,trg)<-init res]
                           [ case r (Str "",src) (Str "",trg) of
                              atom@Constant{} -> Left atom
                              _           -> Right src
                           | (r,src,trg)<-tail res]
             ]

   relFun :: [Var] -> [Expression] -> Expression -> [Expression] -> Var->Var->PredLogic
   relFun varMap lhs e rhs
     = case e of
         EDcD dcl        -> \sv _->R (Funs (singleton (fst sv)) [r | t'<-        lhs, r<-Set.elems $ bindedRelationsIn t']) dcl (Funs (singleton (fst sv)) [r | t'<-reverse rhs, r<-Set.elems $ bindedRelationsIn t'])
         EFlp (EDcD dcl) -> \_ tv->R (Funs (singleton (fst tv)) [r | t'<-reverse rhs, r<-Set.elems $ bindedRelationsIn t']) dcl (Funs (singleton (fst tv)) [r | t'<-        lhs, r<-Set.elems $ bindedRelationsIn t'])
         EMp1 val _      -> \_ _-> Constant . text . showP $ val
         EFlp EMp1{}     -> relFun varMap lhs e rhs
         _               -> \sv tv->f (varMap<>[sv,tv]) e (sv,tv)

   pars3 :: [Var] -> [[Expression]] -> [(Var -> Var -> PredLogic, A_Concept, A_Concept)]
   pars3 varMap (lhs: [e]: rhs: ts)
    | denotes lhs==Flr && denote e==Rn && denotes rhs==Frl
       = ( relFun varMap lhs e rhs, source (head lhs), target (last rhs)): pars3 varMap ts
    | otherwise = pars2 varMap (lhs:[e]:rhs:ts)
   pars3 varMap ts = pars2 varMap ts -- for lists shorter than 3

   pars2 :: [Var] -> [[Expression]]-> [(Var -> Var -> PredLogic, A_Concept, A_Concept)]
   pars2 varMap (lhs: [e]: ts)
    | denotes lhs==Flr && denote e==Rn
                = (relFun varMap lhs e [], source (head lhs), target e): pars3 varMap ts
    | denotes lhs==Flr && denote e==Frl
                = (relFun varMap lhs (EDcI (source e)) [e], source (head lhs), target e): pars3 varMap ts
    | otherwise = pars1 varMap (lhs:[e]:ts)
   pars2 varMap ([e]: rhs: ts)
    | denotes rhs==Frl && denote e==Rn
                = (relFun varMap [] e rhs, source e, target (last rhs)): pars3 varMap ts
    | denote e==Flr && denotes rhs==Frl
                = (relFun varMap [e] (EDcI (source e)) rhs, source e, target (last rhs)): pars3 varMap ts
    | otherwise = pars1 varMap ([e]:rhs:ts)
   pars2 varMap (lhs: rhs: ts)
    | denotes lhs==Flr && denotes rhs==Frl
                = (relFun varMap lhs (EDcI (source (head rhs))) rhs, source (head lhs), target (last rhs)): pars3 varMap ts
    | otherwise = pars1 varMap (lhs:rhs:ts)
   pars2 varMap ts = pars1 varMap ts -- for lists shorter than 2

   pars1 :: [Var] -> [[Expression]] -> [(Var -> Var -> PredLogic, A_Concept, A_Concept)]
   pars1 varMap expressions
     = case expressions of
         []        -> []
         (lhs: ts) -> (pars0 varMap lhs, source (head lhs), target (last lhs)): pars3 varMap ts

   pars0 :: [Var] -> [Expression] -> Var -> Var -> PredLogic
   pars0 varMap lhs
    | denotes lhs==Flr = relFun varMap lhs (EDcI (source (last lhs))) []
    | denotes lhs==Frl = relFun varMap []  (EDcI (target (last lhs))) lhs
    | otherwise        = relFun varMap [] (let [r]=lhs in r) []

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
mkVar ex cs = mknew (fmap fst ex) [((Str . T.pack) [(toLower . D.head . (<>"x") . name) c],c) |c<-cs]
 where
   mknew :: [Inline] -> [(Inline, A_Concept)] -> [(Inline, A_Concept)]
   mknew _ [] = []
   mknew ex' ((x,c):xs) = if x `elem` ex'
                          then mknew ex' ((Str (T.pack (show x++"'")),c):xs)
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
chain :: Monoid a => a -> [a] -> a
chain  _  []    = mempty
chain  _  [x]   = x
chain txt (h:t) = h<>txt<>chain txt t


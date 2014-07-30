{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
module Database.Design.Ampersand.Fspec.ToFspec.NormalForms
  (delta,conjNF,disjNF,normPA,cfProof,dfProof,proofPA,simplify)
where
   import Database.Design.Ampersand.Basics
   import Database.Design.Ampersand.ADL1.ECArule
   import Database.Design.Ampersand.ADL1.P2A_Converters (pCpt2aCpt)
   import Database.Design.Ampersand.ADL1.Expression 
   import Database.Design.Ampersand.Classes.Relational 
   import Database.Design.Ampersand.Core.AbstractSyntaxTree
   import Database.Design.Ampersand.Core.ParseTree
   import Database.Design.Ampersand.Misc.Options (ParserVersion(..))
   import Database.Design.Ampersand.Input.ADL1.Parser (pRule)
   import Database.Design.Ampersand.Input.Parsing
   import Database.Design.Ampersand.Fspec.Fspec
   import Database.Design.Ampersand.Fspec.ShowADL  -- for debug purposes only
   import Data.List (nub, intercalate)
--   import Debug.Trace
   import Prelude hiding (head)
   import Data.Set (Set)
   import qualified Data.Set as Set
   fatal :: Int -> String -> a
   fatal = fatalMsg "Fspec.ToFspec.NormalForms"

-- Deriving normal forms and representing the neccessary derivation rules are defined by means of RTerms.
-- The data structure RTerm is a representation of relation algebra expressions,
-- which is not redundant with respect to associativity and commutativity.
-- The reason for this is that we use term rewriting for normalization.
-- This algorithm performs poorly with commutative rules, because it may explode combinatorially.
   data RTerm = RIsc (Set RTerm)  -- intersection is associative and commutative
              | RUni (Set RTerm)  -- union is associative and commutative
              | RDif RTerm RTerm
              | RCpl RTerm
              | RDia RTerm RTerm
              | RLrs RTerm RTerm
              | RRrs RTerm RTerm
              | RRad [RTerm]      -- ! is associative
              | RCps [RTerm]      -- ; is associative
              | RPrd [RTerm]      -- # is associative
              | RKl0 RTerm
              | RKl1 RTerm
              | RFlp RTerm
              | RId  A_Concept
              | RVee A_Concept A_Concept
              | RAtm String A_Concept
              | RVar String String String  -- relation name, source name, target name.
              | RConst Expression
              deriving (Eq,Ord)

   instance ShowADL RTerm where
    showADL term = showADL (rTerm2expr term)

-- In order to write deriviation rules in the Ampersand syntax, RTerms are obtained by means of the (already available) Ampersand parser.
-- For that reason, we need a function term2rTerm to translate a term obtained by parsing (type: Term TermPrim) to a RTerm.
   term2rTerm :: Term TermPrim -> RTerm
   term2rTerm term
    = case term of
        PEqu _ l r               -> RIsc (Set.fromList [RUni (Set.singleton (RCpl lt) `Set.union` rTrm), RUni (Set.singleton (RCpl rt) `Set.union` lTrm)])
                                    where lt=term2rTerm l; rt=term2rTerm r
                                          lTrm = case lt of
                                                   RUni terms -> terms
                                                   trm        -> Set.singleton trm
                                          rTrm = case rt of
                                                   RUni terms -> terms
                                                   trm        -> Set.singleton trm
        PImp _ l r               -> RUni (Set.singleton (RCpl (term2rTerm l)) `Set.union` rTrm)
                                    where rTrm = case term2rTerm r of
                                                   RUni terms -> terms
                                                   trm        -> Set.singleton trm
        PIsc _ l r               -> RIsc (lSet `Set.union` rSet)
                                    where lSet = case term2rTerm l of
                                                   RIsc terms -> terms
                                                   trm        -> Set.singleton trm
                                          rSet = case term2rTerm r of
                                                   RIsc terms -> terms
                                                   trm        -> Set.singleton trm
        PUni _ l r               -> RUni (lSet `Set.union` rSet)
                                    where lSet = case term2rTerm l of
                                                   RUni terms -> terms
                                                   trm        -> Set.singleton trm
                                          rSet = case term2rTerm r of
                                                   RUni terms -> terms
                                                   trm        -> Set.singleton trm
        PDif _ l r               -> RDif (term2rTerm l) (term2rTerm r)
        PCpl _ e                 -> RCpl (term2rTerm e)
        PDia _ l r               -> RDia (term2rTerm l) (term2rTerm r)
        PLrs _ l r               -> RLrs (term2rTerm l) (term2rTerm r)
        PRrs _ l r               -> RRrs (term2rTerm l) (term2rTerm r)
        PRad o (PRad o' l l') r  -> term2rTerm (PRad o l (PRad o' l' r))
        PRad _ l (r@PRad{})      -> RRad (term2rTerm l: rs) where RRad rs=term2rTerm r
        PRad _ l r               -> RRad [term2rTerm l, term2rTerm r]
        PCps o (PCps o' l l') r  -> term2rTerm (PCps o l (PCps o' l' r))
        PCps _ l (r@PCps{})      -> RCps (term2rTerm l: rs) where RCps rs=term2rTerm r
        PCps _ l r               -> RCps [term2rTerm l, term2rTerm r]
        PPrd o (PPrd o' l l') r  -> term2rTerm (PPrd o l (PPrd o' l' r))
        PPrd _ l (r@PPrd{})      -> RPrd (term2rTerm l: rs) where RPrd rs=term2rTerm r
        PPrd _ l r               -> RPrd [term2rTerm l, term2rTerm r]
        PKl0 _ e                 -> RKl0 (term2rTerm e)
        PKl1 _ e                 -> RKl1 (term2rTerm e)
        PFlp _ e                 -> RFlp (term2rTerm e)
        PBrk _ e                 -> term2rTerm e 
        Prim (Prel _ _)          -> fatal 103 ("Cannot cope with untyped "++showADL term++" in a dRule inside the normalizer.")
        Prim (PTrel _ str sgn)   -> RVar str (name (pSrc sgn)) (name (pTgt sgn))
        Prim (Pid _ c)           -> RId  (pCpt2aCpt c)
        Prim (Pfull _ s t)       -> RVee (pCpt2aCpt s) (pCpt2aCpt t)
        Prim (Patm _ a (Just c)) -> RAtm a (pCpt2aCpt c)
        _                        -> fatal 108 ("Cannot cope with untyped "++showADL term++" in a dRule inside the normalizer.")

   expr2RTerm :: Expression -> RTerm
   expr2RTerm expr
      = case expr of
          EEqu (l,r)           -> RIsc (Set.fromList [RUni (Set.singleton (RCpl lt) `Set.union` rTrm), RUni (Set.singleton (RCpl rt) `Set.union` lTrm)])
                                  where lt=expr2RTerm l; rt=expr2RTerm r
                                        lTrm = case lt of
                                                 RUni terms -> terms
                                                 trm        -> Set.singleton trm
                                        rTrm = case rt of
                                                 RUni terms -> terms
                                                 trm        -> Set.singleton trm
          EImp (l,r)           -> RUni (Set.singleton (RCpl (expr2RTerm l)) `Set.union` rTrm)
                                  where rTrm = case expr2RTerm r of
                                                 RUni terms -> terms
                                                 trm        -> Set.singleton trm
          EIsc (l,r)           -> RIsc (lSet `Set.union` rSet)
                                  where lSet = case expr2RTerm l of
                                                 RIsc terms -> terms
                                                 trm        -> Set.singleton trm
                                        rSet = case expr2RTerm r of
                                                 RIsc terms -> terms
                                                 trm        -> Set.singleton trm
          EUni (l,r)           -> RUni (lSet `Set.union` rSet)
                                  where lSet = case expr2RTerm l of
                                                 RUni terms -> terms
                                                 trm        -> Set.singleton trm
                                        rSet = case expr2RTerm r of
                                                 RUni terms -> terms
                                                 trm        -> Set.singleton trm
          EDif (l,r)           -> RDif (expr2RTerm l) (expr2RTerm r)
          ECpl e               -> RCpl (expr2RTerm e)
          EDia (l,r)           -> RDia (expr2RTerm l) (expr2RTerm r)
          ELrs (l,r)           -> RLrs (expr2RTerm l) (expr2RTerm r)
          ERrs (l,r)           -> RRrs (expr2RTerm l) (expr2RTerm r)
          ERad (ERad (l,l'),r) -> expr2RTerm (ERad (l, ERad (l',r)))
          ERad (l,r@ERad{})    -> RRad (expr2RTerm l: rs) where RRad rs=expr2RTerm r
          ERad (l,r)           -> RRad [expr2RTerm l, expr2RTerm r]
          ECps (ECps (l,l'),r) -> expr2RTerm (ECps (l, ECps (l',r)))
          ECps (l,r@ECps{})    -> RCps (expr2RTerm l: rs) where RCps rs=expr2RTerm r
          ECps (l,r)           -> RCps [expr2RTerm l, expr2RTerm r]
          EPrd (EPrd (l,l'),r) -> expr2RTerm (EPrd (l, EPrd (l',r)))
          EPrd (l,r@EPrd{})    -> RPrd (expr2RTerm l: rs) where RPrd rs=expr2RTerm r
          EPrd (l,r)           -> RPrd [expr2RTerm l, expr2RTerm r]
          EKl0 e               -> RKl0 (expr2RTerm e)
          EKl1 e               -> RKl1 (expr2RTerm e)
          EFlp e               -> RFlp (expr2RTerm e)
          EBrk e               -> expr2RTerm e 
          _                    -> RConst expr

   rTerm2expr :: RTerm -> Expression
   -- implementation note: because RTerms contain variables, it is cumbersome to reconstruct the type. So we don't.
   -- Once the variables have been replaced (by means of substitutions) by real expressions, we get a type correct expression again.
   -- As a consequence, we cannot use ./\., .\/., etc. in this code.
   rTerm2expr term
    = case term of
        RIsc rs    -> case Set.toList (Set.map rTerm2expr rs) of
                       [e] -> e
                       []  -> fatal 164 "empty set in RIsc is illegal."
                       es  -> let oper l r = EIsc (l,r) in foldr1 oper es
        RUni rs    -> case Set.toList (Set.map rTerm2expr rs) of
                       [e] -> e
                       []  -> fatal 168 "empty set in RUni is illegal."
                       es  -> let oper l r = EUni (l,r) in foldr1 oper es
        RDif l r   -> EDif (rTerm2expr l, rTerm2expr r)
        RCpl e     -> ECpl (rTerm2expr e)
        RDia l r   -> EDia (rTerm2expr l, rTerm2expr r)
        RLrs l r   -> ELrs (rTerm2expr l, rTerm2expr r)
        RRrs l r   -> ERrs (rTerm2expr l, rTerm2expr r)
        RRad rs    -> case map rTerm2expr rs of
                       [e] -> e
                       []  -> fatal 177 "empty set in RRad is illegal."
                       es  -> let oper l r = ERad (l,r) in foldr1 oper es
        RCps rs    -> case map rTerm2expr rs of
                       [e] -> e
                       []  -> fatal 181 "empty set in RCps is illegal."
                       es  -> let oper l r = ECps (l,r) in foldr1 oper es
        RPrd rs    -> case map rTerm2expr rs of
                       [e] -> e
                       []  -> fatal 185 "empty set in RPrd is illegal."
                       es  -> let oper l r = EPrd (l,r) in foldr1 oper es
        RKl0 e     -> rTerm2expr e
        RKl1 e     -> rTerm2expr e
        RFlp e     -> rTerm2expr e
        RVar r s t -> EDcD (makeDecl r (Sign (makeConcept s) (makeConcept t)))
        RId  c     -> EDcI c
        RVee s t   -> EDcV (Sign s t)
        RAtm a c   -> EMp1 a c
        RConst e   -> e
      where
        makeDecl nm sgn
         = Sgn { decnm   = nm
               , decsgn  = sgn
               , decprps = fatal 200 "Illegal RTerm in rTerm2expr"
               , decprps_calc = Nothing
               , decprL  = fatal 202 "Illegal RTerm in rTerm2expr"
               , decprM  = fatal 203 "Illegal RTerm in rTerm2expr"
               , decprR  = fatal 204 "Illegal RTerm in rTerm2expr"
               , decMean = fatal 205 "Illegal RTerm in rTerm2expr"
               , decfpos = fatal 206 "Illegal RTerm in rTerm2expr"
               , deciss  = fatal 207 "Illegal RTerm in rTerm2expr"
               , decusr  = fatal 208 "Illegal RTerm in rTerm2expr"
               , decpat  = fatal 209 "Illegal RTerm in rTerm2expr"
               , decplug = fatal 210 "Illegal RTerm in rTerm2expr"
               }
        makeConcept "ONE" = ONE
        makeConcept  str  = PlainConcept str

   isVar :: RTerm -> Bool
   isVar (RVar{}) = True
   isVar _ = False

   unVar :: RTerm -> String
   unVar (RVar r _ _) = r
   unVar _ = fatal 221 "Illegal call on unVar"

{- momentarily redundant
   vars :: RTerm -> Set String
   vars (RIsc rs)     = (Set.unions . map vars . Set.toList) rs
   vars (RUni rs)     = (Set.unions . map vars . Set.toList) rs
   vars (RDif l r)    = vars l `Set.union` vars r
   vars (RCpl e)      = vars e
   vars (RDia l r)    = vars l `Set.union` vars r
   vars (RLrs l r)    = vars l `Set.union` vars r
   vars (RRrs l r)    = vars l `Set.union` vars r
   vars (RRad rs)     = foldr Set.union Set.empty (map vars rs)
   vars (RCps rs)     = foldr Set.union Set.empty (map vars rs)
   vars (RPrd rs)     = foldr Set.union Set.empty (map vars rs)
   vars (RKl0 e)      = vars e
   vars (RKl1 e)      = vars e
   vars (RFlp e)      = vars e
   vars (RId  c)      = Set.fromList [name c]
   vars (RVee s t)    = Set.fromList [name s, name t]
   vars (RVar r s t)  = Set.fromList [r, s, t]
   vars (RConst{})    = Set.empty
   vars  RAtm{}       = Set.empty
-}

   data DerivRule = DEquR { lTerm :: RTerm
                          , rTerm :: RTerm
                          }
                  | DImpR { lTerm :: RTerm
                          , rTerm :: RTerm
                          }

   instance Show DerivRule where
    showsPrec _ r@DEquR{}  = showString (showADL (lTerm r)++" = " ++showADL (rTerm r))
    showsPrec _ r@DImpR{}  = showString (showADL (lTerm r)++" |- "++showADL (rTerm r))
    
-- For documentation purposes, the derivation rule which proves the step is included.

   data DerivStep = DStep { lhs :: RTerm
                          , rul :: DerivRule
                          , rhs :: RTerm
                          }

   instance Show DerivStep where
    showsPrec _ r@DStep{}  = showString ("    "++showADL (lhs r)++"\n =  {" ++show (rul r)++"}\n    " ++showADL (rhs r))

-- In order to read derivation rules, we use the Ampersand parser.
-- Since it is applied on static code only, error messagea may be produced as fatals.

   parseRule :: String -> Term TermPrim
   parseRule str
      = case  runParser Current pRule "inside Haskell code" str of
          Right result -> result
          Left  msg    -> fatal 274 ("Parse errors in "++str++":\n   "++show msg)

   dRule :: Term TermPrim -> DerivRule
   dRule (PEqu _ l r) = DEquR { lTerm=term2rTerm l, rTerm=term2rTerm r }
   dRule (PImp _ l r) = DImpR { lTerm=term2rTerm l, rTerm=term2rTerm r }
   dRule term         = fatal 279 ("Illegal use of dRule with term "++show term)

   dSteps :: RTerm -> [DerivStep]
   dSteps term = [ DStep { lhs = term, rul = drul, rhs = substitute (show drul) unif (rTerm drul) }
                 | drul<-tceDerivRules, Just ss<-[matches (lTerm drul, term)]   -- drul   matches  DEquR{}
                 , unif <- Set.toList ss
                 ] ++
                 [ DStep { rhs = term, rul = drul, lhs = substitute (show drul) unif (lTerm drul) }
                 | drul<-tceDerivRules, Just ss<-[matches (rTerm drul, term)]   -- drul   matches  DEquR{}
                 , unif <- Set.toList ss
                 ]

   slideDown :: (RTerm -> Integer) -> RTerm -> [(Integer,DerivStep)]
   slideDown weight term
    = let w = weight term in
      case [e | e<-dSteps term, weight (rhs e)<w] of
        step: _ -> (w,step): (slideDown weight) (rhs step)
        _       -> []

   conjNF, disjNF :: Expression -> Expression
   (conjNF, disjNF) = (pr False, pr True)
    where pr dnf expr = case (reverse.slideDown (weightNF dnf).expr2RTerm) expr of
                         (_,d):_ -> rTerm2expr (rhs d)
                         _       -> expr

   cfProof, dfProof :: Expression -> Proof Expression
   (cfProof,dfProof) = (proof False, proof True)
    where 
      proof :: Bool -> Expression -> Proof Expression
      proof dnf expr = [ (rTerm2expr term, explStr, logicSym) | (term, explStr, logicSym)<-pr (expr2RTerm expr) ]
       where
        pr :: RTerm -> [(RTerm, [String], String)]
        pr term
           = case slideDown (weightNF dnf) term of
              [] -> [ (term, ["weight: "++show (weightNF dnf term)], "<=>") ]
              ds -> [ (lhs d, [showADL (rTerm2expr left)++" = "++showADL (rTerm2expr right)++"  (weight: "++show w++")"], "<=>")
                    | (w,d)<-ds, let r=rul d, let left=lTerm r, let right=rTerm r ] ++
                    [ (rhs d, ["weight: "++show w], "<=>")
                    | let (w,d) = last ds ]

   weightNF :: Bool -> RTerm -> Integer
   weightNF dnf term = w term
    where
     hash nm
      = case [i | (i,str)<-zip [100..] (names term), nm==str] of
                [i] -> i
                _   -> fatal 325 ("Something wrong with hashing...")
     two :: Integer
     two = 2
     w :: RTerm -> Integer
     w trm
      = case trm of
          RIsc ls  -> (sum (map w (Set.toList ls))+1) ^ if dnf then two else 1
          RUni ls  -> (sum (map w (Set.toList ls))+1) ^ if dnf then 1 else two
          RDif l r -> (w l+w r+1) ^ two
          RCpl e   -> (w e + 1)   ^ two
          RDia l r -> (w l+w r+1) ^ two
          RLrs l r -> (w l+w r+1) ^ two
          RRrs l r -> (w l+w r+1) ^ two
          RRad ls  -> (sum (map w ls)+1) ^ two
          RCps ls  -> (sum (map w ls)+1) ^ two
          RPrd ls  -> (sum (map w ls)+1) ^ two
          RKl0 e   -> (w e)     + 1
          RKl1 e   -> (w e)     + 1
          RFlp e   -> (w e)     + 1
          _        -> case [hash nm | nm<-names trm] of
                       [h] -> h
                       _   -> fatal 346 ("Something wrong with hashing...")

-- If  'matches (d, expr')  yields  'Just ss', then  'substitute ss (lTerm d) == expr'

   type Unifier = Set (String, RTerm)

   substitute :: String         -- A string to document fatals
              -> Unifier   -- the substitution, which in reality is a list of substitutions.
              -> RTerm          -- The term to be transformed to an expression, with all variables replaced by subexpressions
              -> RTerm
   substitute ruleDoc dePairs term = subs term
    where
       subs :: RTerm -> RTerm
       subs (RIsc ls)    = RIsc (Set.map subs ls)
       subs (RUni ls)    = RUni (Set.map subs ls)
       subs (RDif l r)   = RDif (subs l) (subs r)
       subs (RLrs l r)   = RLrs (subs l) (subs r)
       subs (RRrs l r)   = RRrs (subs l) (subs r)
       subs (RDia l r)   = RDia (subs l) (subs r)
       subs (RCps ls)    = RCps (map subs ls)
       subs (RRad ls)    = RRad (map subs ls)
       subs (RPrd ls)    = RPrd (map subs ls)
       subs (RKl0 e  )   = RKl0 (subs e)
       subs (RKl1 e  )   = RKl1 (subs e)
       subs (RFlp e  )   = RFlp (subs e)
       subs (RCpl e  )   = RCpl (subs e)
       subs (RVar r _ _) = case [ e | (v,e)<-Set.toList dePairs, v==r] of
                              [e] -> e
                              [] ->  fatal 378 ("Rule:  "++ruleDoc++"\nVariable "++r++" is not in term "++showADL term)
                              es ->  fatal 379 ("Rule:  "++ruleDoc++"\nVariable "++r++" in term "++showADL term++" has been bound to multiple expressions:\n   "++intercalate "\n   " [showADL e | e<-es])
       subs (RId c)      = case [ e | (v,e)<-Set.toList dePairs, v==name c] of
                              [e] -> e  -- This is e@(RId c')
                              []  -> fatal 382 ("Rule:  "++ruleDoc++"\nVariable "++name c++" is not in term "++showADL term)
                              es  -> fatal 383 ("Rule:  "++ruleDoc++"\nVariable "++name c++" in term "++showADL term++" has been bound to multiple expressions:\n   "++intercalate "\n   " [showADL e | e<-es])
       subs (RVee s t)   = case ([ e | (v,e)<-Set.toList dePairs, v==name s], [ e | (v,e)<-Set.toList dePairs, v==name t]) of
                              ([RId s'], [RId t']) -> RVee s' t'
                              _  -> fatal 386 ("Rule:  "++ruleDoc++"\nSomething wrong with RVee "++" in term "++showADL term)
       subs (RAtm a c)   = case [ e | (v,e)<-Set.toList dePairs, v==name c] of
                              [RId c'] -> RAtm a c'
                              []  -> fatal 389 ("Rule:  "++ruleDoc++"\nVariable "++name c++" is not in term "++showADL term)
                              es  -> fatal 390 ("Rule:  "++ruleDoc++"\nVariable "++name c++" in term "++showADL term++" has been bound to multiple expressions:\n   "++intercalate "\n   " [showADL e | e<-es])
       subs e@RConst{}   = e
--     subs t            = fatal 392 ("Rule:  "++ruleDoc++"\nError: "++showADL t++"is not a variable.")  -- commented out, because it causes Haskell to emit an overlapping pattern warning.
   matches :: (RTerm, RTerm) -> Maybe (Set Unifier)
   matches (term,expr) 
     = rd $
       case (term,expr) of
        (RIsc es,        RIsc es')   -> matchSets es es'
        (RUni es,        RUni es')   -> matchSets es es'
        (RDif l r,       RDif l' r') -> case (matches(l,l'), matches(r,r')) of
                                         (Just subs, Just subs') -> Just (subs `Set.union` subs')
                                         _ -> Nothing
        (RLrs l r,       RLrs l' r') -> case (matches(l,l'), matches(r,r')) of
                                         (Just subs, Just subs') -> Just (subs `Set.union` subs')
                                         _ -> Nothing
        (RRrs l r,       RRrs l' r') -> case (matches(l,l'), matches(r,r')) of
                                         (Just subs, Just subs') -> Just (subs `Set.union` subs')
                                         _ -> Nothing
        (RDia l r,       RDia l' r') -> case (matches(l,l'), matches(r,r')) of
                                         (Just subs, Just subs') -> Just (subs `Set.union` subs')
                                         _ -> Nothing
        (RCps _, RCps (RCps r0:rs')) -> matches (term, RCps (r0++rs'))
        (RCps (l:ls), RCps (l':ls')) -> case (matches(l,l'), matches(RCps ls,RCps ls')) of
                                         (Just subs, Just subs') -> Just (subs `Set.union` subs')
                                         _ -> Nothing
        (RRad _, RRad (RRad r0:rs')) -> matches (term, RRad (r0++rs'))
        (RRad (l:ls), RRad (l':ls')) -> case (matches(l,l'), matches(RRad ls,RRad ls')) of
                                         (Just subs, Just subs') -> Just (subs `Set.union` subs')
                                         _ -> Nothing
        (RPrd _, RPrd (RPrd r0:rs')) -> matches (term, RPrd (r0++rs'))
        (RPrd (l:ls), RPrd (l':ls')) -> case (matches(l,l'), matches(RPrd ls,RPrd ls')) of
                                          (Just subs, Just subs') -> Just (subs `Set.union` subs')
                                          _ -> Nothing
        (RKl0 e,         RKl0 e')    -> matches(e,e')
        (RKl1 e,         RKl1 e')    -> matches(e,e')
        (RFlp e,         RFlp e')    -> matches(e,e')
        (RCpl e,         RCpl e')    -> matches(e,e')
        (RId  c,         RId _     ) -> Just (Set.singleton (Set.fromList [(name c,expr)]))
        (RVee s t,       RVee s' t') -> Just (Set.singleton (Set.fromList [(name s,RId s'), (name t,RId t')]))
        (RVar v _ _,     r         ) -> Just (Set.singleton (Set.fromList [(v,r)]))
        (RAtm a c,       RAtm a' c') -> if a==a' then Just (Set.singleton (Set.fromList [(name c,RId c')])) else Nothing
        (RConst e,       RConst e' ) -> if e==e' then Just Set.empty else Nothing
        (_, _)                       -> Nothing
      where testUnique :: Unifier -> Bool
            testUnique ves = case Set.toList ves of
                              [] -> True
                              us -> null [ () | cl<-eqCl fst us, length cl>1 ] -- This means: every variable is matched to one expression only.
            rd :: Maybe (Set Unifier) -> Maybe (Set Unifier)
            rd (Just unifiers) = let us=Set.filter testUnique unifiers in
                                 if us==Set.empty then Nothing else Just us
            rd Nothing = Nothing

   matchSets :: Set RTerm -> Set RTerm -> Maybe (Set Unifier)
   matchSets es es'
    = if Set.size rest > Set.size rels   -- All subexpressions in expr can be matched to a variable.
      then (Just . Set.fromList)
           [ subs0 `Set.union` subs1
           | subs0<-ss                                               -- bindings that originate from the top level
           , let candidates=Set.difference rest (Set.map snd subs0)  -- candidates for recursively matching subexpressions:  {x, y}
           , assignment<-assignments xprs candidates                 -- mix ...        [ {(r;s , x) , ('Piet', y)} , {(r;s , y) , ('Piet', x)} ]
           , Just sbs1<-[allMatch [ matches (x,c) | (x,c)<-Set.toList assignment]]     -- ... and match.
           , subs1<-Set.toList sbs1
           ]
      else Nothing
      where                                -- Example: es = {p , r;s , q , 'Piet'}  and es' = { a\b , a;b;c , d, 'Piet', e }
        rels :: Set String
        xprs, isct, rest :: Set RTerm
        rels = Set.map unVar (Set.filter isVar es) -- the variables in this level of the template:  {p , q}
        xprs = Set.filter (not.isVar) es           -- the other subexpressions:  {r;s , 'Piet'}
        isct = Set.intersection es' xprs           -- {'Piet'}
        rest = Set.difference es' isct             -- the candidates in es' for binding to a variable: { a\b , a;b;c , d , e }
        ss   = assignments rels rest               -- assignments [p, q] [a\b , a;b;c , d, e]   -- all possible bindings
                                                   --   =  [ {(p, a\b),   (q, a;b;c)}
                                                   --      , {(p, a\b),   (q, d)}
                                                   --      , {(p, a\b),   (q, e)}
                                                   --      , {(p, a;b;c), (q, a\b)}
                                                   --      , {(p, a;b;c), (q, d)}
                                                   --      , {(p, a;b;c), (q, e)}
                                                   --      , {(p, x),     (q, a\b)}
                                                   --      , {(p, x),     (q, a;b;c)}
                                                   --      , {(p, x),     (q, e)}
                                                   --      , {(p, y),     (q, a\b)}
                                                   --      , {(p, y),     (q, a;b;c)}
                                                   --      , {(p, y),     (q, d)}
                                                   --      ]

   allMatch :: [Maybe (Set Unifier)] -> Maybe (Set Unifier)
   allMatch (Nothing:_)  = Nothing
   allMatch (Just ss:ms) = case allMatch ms of
                            Nothing -> Nothing
                            Just rs -> Just (ss `Set.union` rs)
   allMatch []           = Just Set.empty
{-
   assignments {a,p} {2,3,4}
=
   { {(a,2), (p,3)}, {(a,2), (p,4)}, {(a,3), (p,4)}, {(a,3), (p,4)}, {(a,4), (p,4)}, {(a,4), (p,3)} }
-}
   assignments :: (Ord a, Ord b) => Set a -> Set b -> [Set (a,b)]
   assignments xs ys = map Set.fromList (recur (Set.toList xs) (Set.toList ys))
    where
      recur [] _ = [[]]
      recur (v:vs) es = [ (v,e):pairs | e<-es, pairs<-recur vs [e' | e'<-es, e'/=e ] ]

-- The function 'names' exists for the purpose of hashing.
   names :: RTerm -> [String]
   names term = nub (nms term)
    where nms trm = case trm of
                        RIsc ls    -> (nub . concatMap nms . Set.toList) ls
                        RUni ls    -> (nub . concatMap nms . Set.toList) ls
                        RDif l r   -> nms l++nms r
                        RLrs l r   -> nms l++nms r
                        RRrs l r   -> nms l++nms r
                        RDia l r   -> nms l++nms r
                        RCps ls    -> (nub . concatMap nms) ls
                        RRad ls    -> (nub . concatMap nms) ls
                        RPrd ls    -> (nub . concatMap nms) ls
                        RKl0 e     -> nms e
                        RKl1 e     -> nms e
                        RFlp e     -> nms e
                        RCpl e     -> nms e
                        RVar r s t -> [r++":"++s++"*"++t]
                        RId c      -> ["I["++name c++"]"]
                        RVee s t   -> ["V["++name s++"*"++name t++"]"]
                        RAtm a c   -> ["'"++a++"'["++name c++"]"]
                        RConst e   -> [showADL e]

-- In order to write rules for the normalizer in a legible manner, I am using the Ampersand parser.
-- The terminal symbols, except I and V, are interpreted as variables in these rules.
-- As these rules may be used in two directions, all concept variables that are used on one side must be used on the other side as well.
-- relation names r, s, q are used as relation variables and A, B, C are used as concept variables.
-- If rules are ill formed, this will result in fatal errors.

-- Type conserving equivalences: The following equivalences have an identical signature on either side.
   tceDerivRules :: [DerivRule]
   tceDerivRules = map (dRule.parseRule)
    [ "r[A*B]\\/s[A*B] = s[A*B]\\/r[A*B]"                         --  Commutativity of \/
    , "r[A*B]/\\s[A*B] = s[A*B]/\\r[A*B]"                         --  Commutativity of /\
    , "(r[A*B]\\/s[A*B])\\/q[A*B] = r[A*B]\\/(s[A*B]\\/q[A*B])"   --  Associativity of \/
    , "(r[A*B]/\\s[A*B])/\\q[A*B] = r[A*B]/\\(s[A*B]/\\q[A*B])"   --  Associativity of /\
    , "(r[A*B];s[B*C]);q[C*D] = r[A*B];(s[B*C];q[C*D])"           --  Associativity of ;
    , "(r[A*B]#s[B*C])#q[C*D] = r[A*B]#(s[B*C]#q[C*D])"           --  Associativity of #
    , "(r[A*B]!s[B*C])!q[C*D] = r[A*B]!(s[B*C]!q[C*D])"           --  Associativity of !
    , "-(-r[A*A]) = r[A*A]"                                       --  Double negation
    , "(r[A*A]~)~ = r[A*A]"                                       --  Double flip
    , "-(r[A*A]~) = (-r[A*A])~"                                   --  Peirce's[A*A] trick, which allows us to write -r[A*A]~
    , "-r[A*A]/\\-s[A*A] = -(r[A*A]\\/s[A*A])"                    --  De Morgan
    , "-r[A*A]\\/-s[A*A] = -(r[A*A]/\\s[A*A])"                    --  De Morgan
    , "-r[A*A];-s[A*C] = -(r[A*A]!s[A*C])"                        --  De Morgan
    , "-r[A*A]!-s[A*C] = -(r[A*A];s[A*C])"                        --  De Morgan
    , "(r[A*A]\\r[A*A]);(r[A*A]\\r[A*A]) = r[A*A]\\r[A*A]"        --  Jipsen&Tsinakis      
    , "(r[A*A]/r[A*A]);(r[A*A]/r[A*A]) = r[A*A]/r[A*A]"           --  Jipsen&Tsinakis   
    , "r[A*A];(r[A*A]\\r[A*A]) = r[A*A]"                          --  Jipsen&Tsinakis  
    , "(r[A*A]/r[A*A]);r[A*A] = r[A*A]"                           --  Jipsen&Tsinakis  
    , "I[A];r[A*B] = r[A*B]"                
    , "r[A*B];I[B] = r[A*B]"                
    , "(r[A*B]\\/s[A*B]);q[B*C] = r[A*B];q[B*C]\\/s[A*B];q[B*C]"  --  Distribution
    , "r[A*B];(s[B*C]\\/q[B*C]) = r[A*B];s[B*C]\\/r[A*B];q[B*C]"  --  Distribution
    , "-r[A*B]~!s[A*C] = r[A*B]\\s[A*C]"                          --  eliminate dagger
    , "-r[A*B]!s[B*C] = r[A*B]~\\s[B*C]"                          --  eliminate dagger
    , "r[A*C]!-s[B*C]~ = r[A*C]/s[B*C]"                           --  eliminate dagger
    , "r[A*C]!-s[C*B] = r[A*C]/s[C*B]~"                           --  eliminate dagger
--  , "r[A*B]#s[B*C]#q[C*D] = r[A*B]#q[C*D]"                      --  eliminate middle in cartesian product -- conditie toevoegen: s[A*B] /= -V
    , "r[A*B]/\\r[A*B] = r[A*B]"                                  --  Absorb equals
    , "r[A*B]\\/r[A*B] = r[A*B]"                                  --  Absorb equals
    , "r[A*B]/\\V[A*B] = r[A*B]"                                  --  Absorb V
    , "V[A*B]/\\r[A*B] = r[A*B]"                                  --  Absorb V
    , "r[A*B]/\\-V[A*B] = -V[A*B]"                                --  Contradiction
    , "-V[A*B]/\\r[A*B] = -V[A*B]"                                --  Contradiction
    , "r[A*B]\\/V[A*B] = V[A*B]"                                  --  Tautology
    , "r[A*B]\\/-V[A*B] = r[A*B]"                                 --  Absorb -V
    , "r[A*B]/\\-r[A*B] = -V[A*B]"                                --  Contradiction
    , "r[A*B]\\/-r[A*B] =  V[A*B]"                                --  Tautology
    , "-r[A*B]\\/r[A*B] = V[A*B]"                                 --  Tautology
    , "(r[A*B]\\/s[A*B])/\\s[A*B] = s[A*B]"                       --  Absorption
    , "(s[A*B]\\/r[A*B])/\\s[A*B] = s[A*B]"                       --  Absorption
    , "(r[A*B]\\/-s[A*B])/\\s[A*B] = r[A*B]/\\s[A*B]"             --  Absorption
    , "(-s[A*B]\\/r[A*B])/\\s[A*B] = r[A*B]/\\s[A*B]"             --  Absorption
    , "r[A*B]/\\(s[A*B]\\/-r[A*B]) = r[A*B]/\\s[A*B]"             --  Absorption
    , "r[A*B]/\\(s[A*B]\\/-r[A*B]) = r[A*B]/\\s[A*B]"             --  Absorption
    ]

-- Type conserving implications: The following implications have an identical signature on either side.
   tciDerivRules :: [DerivRule]
   tciDerivRules = map (dRule.parseRule)
    [ "(r[A*B]\\I[A]);s[A*C] |- r[A*B]\\s[A*C]"                   --  T{r\\I[A]}=[B*A] ; T{(r\\I[A]);s}=[B*C] ; T{r\\s}=[B*C] ; Jipsen&Tsinakis  
    , "r[A*C];(I[C]/s[B*C]) |- r[A*C]/s[B*C]"                     --  Jipsen&Tsinakis  
    , "(r[A*B]\\s[A*C]);q[C*D] |- r[A*B]\\(s[A*C];q[C*D])"        --  Jipsen&Tsinakis      
    , "r[A*B];(s[B*C]/q[D*C]) |- (r[A*B];s[B*C])/q[D*C]"          --  Jipsen&Tsinakis    
    , "(r[A*B]\\s[A*C]);(s[A*C]\\q[A*D]) |- r[A*B]\\q[A*D]"       --  Jipsen&Tsinakis       
    , "(r[A*B]/s[A*C]);(s[A*C]/q[D*B]) |- r[A*B]/q[D*B]"          --  Jipsen&Tsinakis    
    , "r[A*B];(s[B*C]!q[C*D]) |- (r[A*B];s[B*C])!q[C*D]"          --  Peirce
    , "(r[A*B]!s[B*C]);q[C*D] |- r[A*B]!(s[B*C];q[C*D])"          --  Peirce
    , "(r[A*B]/\\s[A*B]);q[B*C] |- r[A*B];q[B*C]/\\s[A*B];q[B*C]" --  Distribution
    , "r[A*B];(s[B*C]/\\q[B*C]) |- r[A*B];s[B*C]/\\r[A*B];q[B*C]" --  Distribution
    , "(r[A*B];s[B*C])/s[B*C] |- r[A*B]"                          --  Absorption
    , "r[A*B]\\(r[A*B];s[B*C]) |- s[B*C]"                         --  Absorption
    ]

-- Type altering equivalences: The following equivalences have an different signature on either side.
   taeDerivRules :: [DerivRule]
   taeDerivRules = map (dRule.parseRule)
    [ "-r[A*B]\\/(q[A*C]/s[B*C]) = -(r[A*B];s[B*C])\\/q[A*C]"     -- T{-r\\/(q/s)} = [A*B] ;   T{-(r;s)\\/q} = [A*C] ; remove left residual (/)
    , "(r[A*B]\\q[A*C])\\/-s[B*C] = -(r[A*B];s[B*C])\\/q[A*C]"    -- T{(r\\q)\\/-s)} = [B*C] ; T{-(r;s)\\/q} = [A*C] ; remove right residual (\\)
    ]

   head :: [a] -> a
   head [] = fatal 30 "head must not be used on an empty list!"
   head (a:_) = a

   -- | This delta is meant to be used as a placeholder for inserting or removing links from expressions.
   delta :: Sign -> Expression
   delta sgn
    = EDcD   Sgn { decnm   = "Delta"
                 , decsgn  = sgn
                 , decprps = []
                 , decprps_calc = Nothing
                 , decprL  = ""
                 , decprM  = ""
                 , decprR  = ""
                 , decMean = AMeaning [ --   A_Markup Dutch   ReST (string2Blocks ReST "Delta is bedoeld als variabele, die de plaats in een expressie vasthoudt waar paren worden ingevoegd of verwijderd.")
                                        -- , A_Markup English ReST (string2Blocks ReST "Delta is meant as a variable, to be used as a placeholder for inserting or removing links from expressions.")
                                      ]
                 , decfpos = Origin ("generated relation (Delta "++show sgn++")")
                 , deciss  = True
                 , decusr  = False
                 , decpat  = ""
                 , decplug = True
                 } 

{- Normalization of process algebra clauses -}

   normPA :: PAclause -> PAclause
   normPA pac = pac' 
       where (pac',_,_) = if null (proofPA pac) then fatal 21 "last: empty list" else last (proofPA pac)

   type Proof a = [(a, [String], String)]

   proofPA :: PAclause -> Proof PAclause
   proofPA = {-reverse.take 3.reverse.-}pPA
    where pPA pac' = case normstepPA pac' of
                       ( _ , []  ,equ) -> [(pac',[]   ,equ)]    -- is dus (pac,[],"<=>")
                       (res,steps,equ) -> (pac',steps,equ):pPA res

{- The following rewriter is used to simplify the actions inside eca rules.
-- WHY? Stef, kan je uitleggen wat hier gebeurt? Enig commentaar is hier wel op zijn plaats.
-- Ook zou het helpen om bij de verschillende constructoren van PAclause een beschrijving te geven van het idee er achter. 
-- BECAUSE! Kan ik wel uitleggen, maar het is een heel verhaal. Dat moet tzt in een wetenschappelijk artikel gebeuren, zodat het er goed staat.
-- Het idee is dat een procesalgebra is weergegeven in Haskell combinatoren (gedefinieerd als PAclause(..), zie ADL.ECArule).
-- Die kun je vervolgens normaliseren met herschrijfregels op basis van gelijkheden die gelden in de bewuste procesalgebra.
-- Helaas zijn de herschrijfregels nu nog hard gecodeerd, zodat ik voor PAclause een afzonderlijke Haskell functie moet schrijven.
-- Hierna volgt de normalisator voor relatiealgebra-expressies, genaamd normStep. Die heeft dezelfde structuur,
-- maar gebruikt herschrijfregels vanuit gelijkheden die gelden in relatiealgebra.
-}
   normstepPA :: PAclause -> (PAclause,[String],String)
   normstepPA pac = (res,ss,"<=>")
    where
     (res,ss) = norm pac
     norm :: PAclause -> (PAclause,[String])
     norm (CHC [] ms)  = (Blk ms, ["Run out of options"])
     norm (CHC [r] ms) = (r', ["Flatten ONE"])
                       where r' = case r of
                                    Blk{} -> r
                                    _     -> r{paMotiv = ms} 
     norm (CHC ds ms)  | (not.null) msgs = (CHC ops ms, msgs)
                       | (not.null) [d | d<-ds, isCHC d] = (CHC (nub [ d' | d<-ds, d'<-if isCHC d then let CHC ops' _ = d in ops' else [d] ]) ms, ["flatten CHC"])  -- flatten
                       | (not.null) [Nop | Nop{}<-ops] = (Nop{paMotiv=ms}, ["Choose to do nothing"])
                       | (not.null) [Blk | Blk{}<-ops] = (CHC [op | op<-ops, not (isBlk op)] ms, ["Choose anything but block"])
                       | (not.null) doubles = (CHC [ head cl | cl<-eqClass (==) ds ] ms, ["remove double occurrences"])
                       | otherwise = (CHC ds ms, [])
                       where nds  = map norm ds
                             msgs = concatMap snd nds
                             ops  = map fst nds
                             doubles = [ d | cl<-eqClass (==) ds, length cl>1, d<-cl ]
     norm (GCH [] ms)  = (Blk ms, ["Run out of options"])
     norm (GCH ds ms)  | (not.null) [() | (_,links,_)<-normds, isFalse links] = (GCH [(tOp,links,p) | (tOp,links,p)<-normds, not (isFalse links)] ms, ["Remove provably empty guard(s)."])
                       | (not.null) [()          | (_,  _    ,p)<-normds, isNop p]
                           = (GCH [(tOp,links,p) | (tOp,links,p)<-normds, not (isNop p)] ms, ["Remove unneccessary SELECT."])
                       | (not.null) doubles = (GCH [ (fst3 (head cl), foldr1 (.\/.) (map snd3 cl), thd3 (head cl)) | cl<-eqCl (\(tOp,_,p)->(tOp,p)) ds ] ms, ["remove double occurrences"])
                       | otherwise = (GCH ds ms, [])
                       where normds = [ (tOp, conjNF links, let (p',_)=norm p in p') | (tOp,links,p)<-ds]
                             doubles = [ d | cl<-eqCl (\(tOp,_,p)->(tOp,p)) ds, length cl>1, d<-cl ]
     norm (ALL [] ms)  = (Nop ms, ["ALL [] = No Operation"])
     norm (ALL [d] ms) = (d', ["Flatten ONE"])
                       where d' = case d of
                                    Blk{} -> d
                                    _     -> d{paMotiv = ms} 
     norm (ALL ds ms)  | (not.null) msgs = (ALL ops ms, msgs)
                       | (not.null) [d | d<-ds, isAll d] = (ALL (nub [ d' | d<-ds, d'<-if isAll d then let ALL ops' _ = d in ops' else [d] ]) ms, ["flatten ALL"])  -- flatten
                       | (not.null) [Blk | Blk{}<-ops] = (Blk{paMotiv = [m | op@Blk{}<-ops,m<-paMotiv op]}, ["Block all"])
                       | (not.null) [Nop | Nop{}<-ops] = (ALL [op | op<-ops, not (isNop op)] ms, ["Ignore Nop"])
                       | (not.null) doubles = (CHC [ head cl | cl<-eqClass (==) ds ] ms, ["remove double occurrences"])
                       | (not.null) long    = (ALL ds' ms, ["Take the expressions for "++commaEng "and" [name (paTo (head cl)) |cl<-long]++"together"])
                       | otherwise = (ALL ds ms, [])
                       where ds'     = [ let p=head cl in
                                           if length cl==1 then p else p{paDelta=disjNF (foldr1 (.\/.) [paDelta c | c<-cl]), paMotiv=concatMap paMotiv cl}
                                       | cl<-dCls {- not (null cl) is guaranteed by eqCl -} ]
                                       ++[d | d<-ds, not (isDo d)]
                             nds     = map norm ds
                             msgs    = concatMap snd nds
                             ops     = map fst nds
                             doubles = [ d | cl<-eqClass (==) ds, length cl>1, d<-cl ]
                             dCls :: [[PAclause]]
                             dCls = eqCl to [d | d<-ds, isDo d]
                             long :: [[PAclause]]
                             long = [cl | cl<-dCls, length cl>1]
                             to d = case d of 
                                      Do{} -> (paSrt d, paTo d)
                                      _    -> fatal 74 "illegal call of to(d)"
     norm (New c p ms)        = ( case p' of
                                   Blk{} -> p'{paMotiv = ms}
                                   _     -> New c (\x->let (p'', _) = norm (p x) in p'') ms
                                , msgs)
                                where (p', msgs) = norm (p "x")
     norm (Rmv c p ms)        = ( case p' of
                                   Blk{} -> p'{paMotiv = ms}
                                   _     -> Rmv c (\x->let (p'', _) = norm (p x) in p'') ms
                                , msgs)
                                where (p', msgs) = norm (p "x")
     norm p                   = (p, [])

{- Normalization of expressions -}

   simplify :: Expression -> Expression
   simplify expr = expr' 
       where (expr',_,_) = if null (simpProof shw expr) then fatal 101 "last: empty list" else last (simpProof shw expr)
             shw _ = ""

   simpProof :: (Expression -> String) -> Expression -> Proof Expression
   simpProof shw expr    
    = if expr==res
      then [(expr,[],"<=>")]
      else (expr,steps,equ):simpProof shw res
    where (res,steps,equ) = normStep shw True True True expr


   -- | The purpose of "normStep" is to elaborate a single step in a rewrite process,
   -- in which the expression is normalized by means of rewrite rules.
   -- This function can be used for simplification, which means that an Expression is standardized
   -- using associativity and other 'trivial' rules only.
   -- These 'trivial' rules do not produce a step in the proof.
   -- Use normstep shw eq True expr to do simplification only.
   -- Use normstep shw eq False expr to obtain a single proof step or none when no rule is applicable.
   -- This function returns a resulting expression that is closer to a normal form.
   -- The normal form is not unique. This function simply uses the first rewrite rule it encounters.
   normStep :: (Expression -> String) -> Bool -> Bool -> Bool ->
               Expression -> (Expression,[String],String) -- This might be generalized to "Expression" if it weren't for the fact that flip is embedded in the Relation type.
   normStep shw   -- a function to print an expression. Might be "showADL"
            eq    -- If eq==True, only equivalences are used. Otherwise, implications are used as well.
            dnf   -- If dnf==True, the result is in disjunctive normal form, otherwise in conjunctive normal form
            simpl -- If True, only simplification rules are used, which is a subset of all rules. Consequently, simplification is implied by normalization.
            expr = if sign expr==sign res then (res,ss,equ) else
                   fatal 166 ("Violation of sign expr==sign res in the normalizer\n  expr: sign( "++showADL expr++" ) == "++showSign res++"\n  res:  sign( "++showADL res++" ) == "++showSign res)
{-SJ 20140720: You might wonder why we test sign expr==sign res, which was introduced as a result of ticket #409 (the residu bug)
It turns out that many rewrite rules in the normalizer change the type of an expression; an aspect I have been overlooking all the time.
Until the new normalizer works, we will have to work with this one. So I have inserted this test to ensure that the type remains constant during normalization.
-}
    where
     (res,ss,equ) = nM True expr []
     nM :: Bool -> Expression -> [Expression] -> (Expression,[String],String)
-- posCpl indicates whether the expression is positive under a complement. It is False when expr is inside a complemented expression.
     nM posCpl (EEqu (l,r)) _     | simpl = (t .==. f, steps++steps', fEqu [equ',equ''])
                                            where (t,steps, equ')  = nM posCpl l []  -- TODO: the use of posCpl is erroneous
                                                  (f,steps',equ'') = nM posCpl r []  -- TODO: the use of posCpl is erroneous
     nM posCpl (EImp (l,r)) _     | simpl = (t .|-. f, steps++steps', fEqu [equ',equ''])
                                            where (t,steps, equ')  = nM (not posCpl) l []
                                                  (f,steps',equ'') = nM posCpl r []
     nM posCpl (EUni (EUni (l,k),r)) rs   = nM posCpl (l .\/. (k .\/. r)) rs  -- standardize, using associativity of .\/.
     nM posCpl (EUni (l,r)) rs    | simpl = (t .\/. f, steps++steps', fEqu [equ',equ''])
                                            where (t,steps, equ')  = nM posCpl l []
                                                  (f,steps',equ'') = nM posCpl r (l:rs)
     nM posCpl (EIsc (EIsc (l,k),r)) rs   = nM posCpl (l ./\. (k ./\. r)) rs  -- standardize, using associativity of ./\.
     nM posCpl (EIsc (l,r)) rs    | simpl = (t ./\. f, steps++steps', fEqu [equ',equ''])
                                            where (t,steps, equ')  = nM posCpl l []
                                                  (f,steps',equ'') = nM posCpl r (l:rs)
     nM posCpl (ECps (ECps (l,k),r)) rs   = nM posCpl (l .:. (k .:. r)) rs  -- standardize, using associativity of .:. 
                                                -- Note: function shiftL and shiftR make use of the fact that this normalizes to (l .:. (k .:. r))
     nM posCpl (ECps (l,r)) rs    | simpl = (t .:. f, steps++steps', fEqu [equ',equ''])
                                             where (t,steps, equ')  = nM posCpl l []
                                                   (f,steps',equ'') = nM posCpl r (l:rs)
     nM posCpl (ELrs (l,r)) _     | simpl = (t ./. f, steps++steps', fEqu [equ',equ''])
                                            where (t,steps, equ')  = nM posCpl l []
                                                  (f,steps',equ'') = nM (not posCpl) r []
     nM posCpl (ERrs (l,r)) _     | simpl = (t .\. f, steps++steps', fEqu [equ',equ''])
                                            where (t,steps, equ')  = nM (not posCpl) l []
                                                  (f,steps',equ'') = nM posCpl r []
     nM posCpl (EDia (l,r)) _     | simpl = (t .<>. f, steps++steps', fEqu [equ',equ''])
                                            where (t,steps, equ')  = nM posCpl l []  -- TODO: the use of posCpl is erroneous
                                                  (f,steps',equ'') = nM posCpl r []  -- TODO: the use of posCpl is erroneous
     nM posCpl (ERad (ERad (l,k),r)) rs   = nM posCpl (l .!. (k .!. r)) rs  -- standardize, using associativity of .!.
     nM posCpl (ERad (l,r)) rs    | simpl = (t .!. f, steps++steps', fEqu [equ',equ''])
                                            where (t,steps, equ')    = nM posCpl l []
                                                  (f,steps',equ'')   = nM posCpl r (l:rs)
     nM posCpl (EPrd (EPrd (l,k),r)) rs   = nM posCpl (l .*. (k .*. r)) rs  -- standardize, using associativity of .*.
     nM posCpl (EPrd (l,r)) _     | simpl = (t .*. f, steps++steps', fEqu [equ',equ''])
                                            where (t,steps, equ')  = nM posCpl l []
                                                  (f,steps',equ'') = nM posCpl r []
     nM posCpl (EKl0 e)              _    = (EKl0 res', steps, equ')
                                            where (res',steps,equ') = nM posCpl e []
     nM posCpl (EKl1 e)              _    = (EKl1 res', steps, equ')
                                            where (res',steps,equ') = nM posCpl e []
     nM posCpl (ECpl (ECpl e))         rs = nM posCpl e rs
     nM posCpl (ECpl e) _         | simpl = (notCpl res',steps,equ')
                                            where (res',steps,equ') = nM (not posCpl) e []
     nM posCpl (EBrk e)                _  = nM posCpl e []
     nM posCpl (EFlp (ECpl e))         rs = nM posCpl (notCpl (flp e)) rs
     nM _      x _                | simpl = (x,[],"<=>")
-- up to here, simplification has been treated. The remaining rules can safely assume  simpl==False
     nM _      (EEqu (l,r)) _                            = ((l .|-. r) ./\. (r .|-. l), ["remove ="],"<=>")
     nM _      (EImp (x,r@(ELrs (z,y)))) _               = if sign x==sign z -- necessary to guarantee that sign expr is equal to sign of the result
                                                           then (x .:. y .|-. z, ["remove left residual (/)"],"<=>")
                                                           else (notCpl x .\/. r, ["remove |-"],"<=>")
     nM _      (EImp (y,r@(ERrs (x,z)))) _               = if sign y==sign z -- necessary to guarantee that sign expr is equal to sign of the result
                                                           then (x .:. y .|-. z, ["remove right residual (\\)"],"<=>")
                                                           else (notCpl y .\/. r, ["remove |-"],"<=>")
     nM _      (EImp (l,r)) _                            = (notCpl l .\/. r, ["remove |-"],"<=>")
--   nM posCpl e@(ECpl EIsc{}) _           | posCpl==dnf = (deMorganEIsc e, ["De Morgan"], "<=>")
--   nM posCpl e@(ECpl EUni{}) _           | posCpl/=dnf = (deMorganEUni e, ["De Morgan"], "<=>")
     nM _      e@(ECpl EIsc{}) _                         = (deMorganEIsc e, ["De Morgan"], "<=>")
     nM _      e@(ECpl EUni{}) _                         = (deMorganEUni e, ["De Morgan"], "<=>")
     nM _      e@(ECpl (ERad (_,ECpl{}))) _              = (deMorganERad e, ["De Morgan"], "<=>")
     nM _      e@(ECpl (ERad (ECpl{},_))) _              = (deMorganERad e, ["De Morgan"], "<=>")
     nM _      e@(ECpl (ECps (ECpl{},ECpl{}))) _         = (deMorganECps e, ["De Morgan"], "<=>")
     nM posCpl (ECpl e) _                                = (notCpl res',steps,equ')
                                                           where (res',steps,equ') = nM (not posCpl) e []
     nM _      (ECps (EEps c (Sign s _),EEps c' (Sign _  t'))) _ | c ==c' = (EEps c  (Sign s t'), [], "<=>")
     nM _      (ECps (EEps c (Sign s t),EEps c' (Sign _  t'))) _ | c ==t  = (EEps c' (Sign s t'), [], "<=>")
     nM _      (ECps (EEps c (Sign s _),EEps c' (Sign s' t'))) _ | s'==c' = (EEps c  (Sign s t'), [], "<=>")
     nM _      (ECps (EEps c (Sign s _),ECps(EEps c' (Sign _  t'),r))) _ | c ==c' = (ECps (EEps c  (Sign s t'),r), [], "<=>")
     nM _      (ECps (EEps c (Sign s t),ECps(EEps c' (Sign _  t'),r))) _ | c ==t  = (ECps (EEps c' (Sign s t'),r), [], "<=>")
     nM _      (ECps (EEps c (Sign s _),ECps(EEps c' (Sign s' t'),r))) _ | s'==c' = (ECps (EEps c  (Sign s t'),r), [], "<=>")
     nM _      (ECps (ERrs (x,e),y)) _ | not eq && isIdent e = (ERrs (x,y), ["Jipsen&Tsinakis: (x\\I);y |- x\\y"], "==>")
     nM _      (ECps (x,ELrs (e,y))) _ | not eq && isIdent e = (ELrs (x,y), ["Jipsen&Tsinakis: x;(I/y) |- x/y"], "==>")
     nM _      (ECps (ERrs (x,y),z)) _          | not eq = (ERrs (x,ECps (y,z)), ["Jipsen&Tsinakis: (x\\y);z |- x\\(y;z)"], "==>")
     nM _      (ECps (x,ELrs (y,z))) _          | not eq = (ERrs (x,ECps (y,z)), ["Jipsen&Tsinakis: x;(y/z) |- (x;y)/z"], "==>")
     nM _      (ECps (ERrs (x,y),ERrs (y',z))) _ | not eq && y==y' = (ERrs (x,z), ["Jipsen&Tsinakis: (x\\y);(y\\z) |- x\\z"], "==>")
     nM _      (ECps (ELrs (x,y),ELrs (y',z))) _ | not eq && y==y' = (ERrs (x,z), ["Jipsen&Tsinakis: (x/y);(y/z) |- x/z"], "==>")
     nM _      (ECps (ERrs (x,y),ERrs (y',z))) _ | y==y' && x==y && x==z = (ERrs (x,z), ["Jipsen&Tsinakis: (x\\x);(x\\x) = x\\x"], "<=>")
     nM _      (ECps (ELrs (x,y),ELrs (y',z))) _ | y==y' && x==y && x==z = (ERrs (x,z), ["Jipsen&Tsinakis: (x/x);(x/x) = x/x"], "<=>")
     nM _      (ECps (x,ERrs (y,z))) _    | x==y && x==z = (x, ["Jipsen&Tsinakis: x;(x\\x) = x"], "<=>")
     nM _      (ECps (ELrs (x,y),z)) _    | x==z && y==z = (x, ["Jipsen&Tsinakis: (x/x);x = x"], "<=>")
     nM _      (ECps (l,r)) _                | isIdent l = (r, ["I;x = x"], "<=>")
     nM _      (ECps (l,r)) _                | isIdent r = (l, ["x;I = x"], "<=>")
     nM True   (ECps (r,ERad (s,q))) _          | not eq = ((r.:.s).!.q, ["Peirce: r;(s!q) |- (r;s)!q"],"==>")
     nM True   (ECps (ERad (r,s),q)) _          | not eq = (r.!.(s.:.q), ["Peirce: (r!s);q |- r!(s;q)"],"==>")
     nM True   (ECps (EIsc (r,s),q)) _          | not eq = ((r.:.q)./\.(s.:.q), ["distribute ; over /\\"],"==>")
     nM True   (ECps (r,EIsc (s,q))) _          | not eq = ((r.:.s)./\.(r.:.q), ["distribute ; over /\\"],"==>")
     nM _      (ECps (EUni (q,s),r)) _                   = ((q.:.r).\/.(s.:.r), ["distribute ; over \\/"],"<=>")
     nM _      (ECps (l,EUni (q,s))) _                   = ((l.:.q).\/.(l.:.s), ["distribute ; over \\/"],"<=>")
     nM _      x@(ECps (l@EFlp{},r)) _ | not eq && flp l==r && isInj l   = (EDcI (source x), ["r~;r |- I (r is univalent)"], "==>")
     nM _      x@(ECps (l,       r)) _ | not eq && l==flp r && isInj l   = (EDcI (source x), ["r;r~ |- I (r is injective)"], "==>")
     nM _      x@(ECps (l@EFlp{},r)) _ | flp l==r && isInj l && isTot l  = (EDcI (source x), ["r~;r=I because r is univalent and surjective"], "<=>")
     nM _      x@(ECps (l,       r)) _ | l==flp r && isInj l && isTot l  = (EDcI (source x), ["r;r~=I because r is injective and total"], "<=>")
     nM posCpl (ECps (l,r))           rs                 = (t .:. f, steps++steps', fEqu [equ',equ''])
                                                             where (t,steps, equ')  = nM posCpl l []
                                                                   (f,steps',equ'') = nM posCpl r (l:rs)
     nM _      x@(EEps i sgn) _ | source sgn==i && i==target sgn = (EDcI i, ["source and target are equal to "++name i++", so "++showADL x++"="++showADL (EDcI i)], "<=>")
     nM _      (ELrs (ECps (x,y),z)) _ | not eq && y==z  = (x,     ["(x;y)/y |- x"], "==>")
     nM _      (ELrs (ECps (x,y),z)) _ | not eq && flp x==z= (flp y, [case (x, y) of
                                                                           (EFlp _, EFlp _) -> "(SJ) (x~;y~)/x |- y"
                                                                           (     _, EFlp _) -> "(SJ) (x;y~)/x~ |- y"
                                                                           (EFlp _,      _) -> "(SJ) (x~;y)/x |- y~"
                                                                           (     _,      _) -> "(SJ) (x;y)/x~ |- y~"], "==>")
     nM _      (ELrs (ELrs (x,z),y)) _                     = (ELrs (x,ECps (y,z)), ["Jipsen&Tsinakis: x/yz = (x/z)/y"], "<=>") -- note: sign (x/yz) == sign ((x/z)/y)
     nM posCpl (ELrs (l,r)) _                              = (t ./. f, steps++steps', fEqu [equ',equ''])
                                                             where (t,steps, equ')  = nM posCpl l []
                                                                   (f,steps',equ'') = nM (not posCpl) r []
     nM _      (ERrs (y,ERrs (x,z))) _                     = (ERrs (ECps (x,y),z), ["Jipsen&Tsinakis: xy\\z = y\\(x\\z)"], "<=>")
     nM _      (ERrs (x,ECps (y,z))) _ | not eq && x==y    = (z,     ["x\\(x;y) |- y"], "==>")
     nM posCpl (ERrs (l,r)) _                              = (t .\. f, steps++steps', fEqu [equ',equ''])
                                                             where (t,steps, equ')  = nM (not posCpl) l []
                                                                   (f,steps',equ'') = nM posCpl r []
     nM posCpl (EDia (l,r)) _                              = (t .<>. f, steps++steps', fEqu [equ',equ''])
                                                             where (t,steps, equ')  = nM posCpl l []
                                                                   (f,steps',equ'') = nM posCpl r []
     nM _      (ERad (l,r)) _                   | isImin l = (r, ["-I!x = x"], "<=>")
     nM _      (ERad (l,r)) _                   | isImin r = (l, ["x!-I = x"], "<=>")
--     nM False  (ERad (ECps (r,s),q)) _            | not eq = (r.:.(s.!.q), ["Peirce: (r;s)!q |- r;(s!q)"],"==>")  -- SJ 20131124 TODO: check this rule. It is wrong!
--     nM False  (ERad (r,ECps (s,q))) _            | not eq = ((r.!.s).:.q, ["Peirce: (r!s);q |- r!(s;q)"],"==>")  -- SJ 20131124 TODO: check this rule. It is wrong!
     nM False  (ERad (EUni (r,s),q)) _            | not eq = ((r.!.q).\/.(s.!.q), ["distribute ! over \\/"],"==>")
     nM False  (ERad (r,EUni (s,q))) _            | not eq = ((r.!.s).\/.(r.!.q), ["distribute ! over \\/"],"==>")
     nM _      (ERad (EIsc (q,s),r)) _                     = ((q.!.r)./\.(s.!.r), ["distribute ! over /\\"],"<=>")
     nM _      (ERad (l,EIsc (q,s))) _                     = ((l.!.q)./\.(l.!.s), ["distribute ! over /\\"],"<=>")
     nM _      (ERad(ECpl l,r))      _                     = (flp l .\. r, [case l of EFlp{} -> "-l~!r = l\\r"; _ -> "-l!r = l~\\r"], "<=>")
     nM _      (ERad(l,ECpl r))      _                     = (l ./. flp r, [case r of EFlp{} -> "l!-r~ = l/r"; _ -> "l!-r = l/r~"], "<=>")
     nM posCpl (ERad (l,r))         rs                     = (t .!. f, steps++steps', fEqu [equ',equ''])
                                                                 where (t,steps, equ')  = nM posCpl l []
                                                                       (f,steps',equ'') = nM posCpl r (l:rs)
     nM _      (EPrd (l,EPrd (_,r))) _                     = (l .*. r, ["eliminate middle in cartesian product"], "<=>")
     nM posCpl (EPrd (l,r)) _                              = (t .*. f, steps++steps', fEqu [equ',equ''])
                                                                 where (t,steps, equ')  = nM posCpl l []
                                                                       (f,steps',equ'') = nM posCpl r []
     nM posCpl (EIsc (EUni (l,k),r)) _       | posCpl/=dnf = ((l./\.r) .\/. (k./\.r), ["distribute /\\ over \\/"],"<=>")
     nM posCpl (EIsc (l,EUni (k,r))) _       | posCpl/=dnf = ((l./\.k) .\/. (l./\.r), ["distribute /\\ over \\/"],"<=>")
     nM posCpl x@(EIsc (l,r)) rs
-- Absorb equals:    r/\r  -->  r
         | or [length cl>1 |cl<-absorbClasses]
              = ( case absorbClasses of [] -> fatal 243 "Going into foldr1 with empty absorbClasses"; _ -> foldr1 (./\.) [head cl | cl<-absorbClasses]
                , [shw e++" /\\ "++shw e++" = "++shw e | cl<-absorbClasses, length cl>1, let e=head cl]
                , "<=>"
                )
-- Absorb True:    r/\V  --> r
         | isTrue l                      = (r, ["V/\\x = x"], "<=>")
         | isTrue r                      = (l, ["x/\\V = x"], "<=>")
-- Inconsistency:    r/\-r   -->  False
         | not (null incons)
              = let i = head incons in (notCpl (EDcV (sign i)), [shw (notCpl i)++" /\\ "++shw i++" = V-"], "<=>")
-- Inconsistency:    x/\\V-  -->  False
         | isFalse l                     = (notCpl (EDcV (sign x)), ["-V/\\x = -V"], "<=>")
         | isFalse r                     = (notCpl (EDcV (sign x)), ["x/\\-V = -V"], "<=>")
-- Absorb if r is antisymmetric:    r/\r~ --> I
         | t/=l || f/=r
              = (t ./\. f, steps++steps', fEqu [equ',equ''])
         | not eq && or [length cl>1 |cl<-absorbAsy]
              = ( foldr1 (./\.) [if length cl>1 then EDcI (source e) else e | cl<-absorbAsy, let e=head cl] 
                , [shw e++" /\\ "++shw (flp e)++" |- I, because"++shw e++" is antisymmetric" | cl<-absorbAsy, let e=head cl]
                , "==>"
                )
-- Absorb if r is antisymmetric and reflexive:    r/\r~ = I
         | or [length cl>1 |cl<-absorbAsyRfx]
              = ( foldr1 (./\.) [if length cl>1 then EDcI (source e) else e | cl<-absorbAsyRfx, let e=head cl] 
                , [shw e++" /\\ "++shw (flp e)++" = I, because"++shw e++" is antisymmetric and reflexive" | cl<-absorbAsyRfx, let e=head cl]
                , "<=>"
                )
-- Absorb:    (x\\/y)/\\y  =  y
         | isEUni l && not (null absor0)
              = let t'=head absor0  in (r, ["absorb "++shw l++" because of "++shw t'++", using law  (x\\/y)/\\y = y"], "<=>")
         | isEUni r && not (null absor0')
              = let t'=head absor0' in (r, ["absorb "++shw r++" because of "++shw t'++", using law  (x\\/y)/\\x = x"], "<=>")
-- Absorb:    (x\\/-y)/\\y  =  x/\\y
         | isEUni l && not (null absor1)
              = ( case head absor1 of
                    (_,[]) -> r
                    (_,ts) -> foldr1 (.\/.) ts ./\. r
                , ["absorb "++shw t'++", using law (x\\/-y)/\\y  =  x/\\y" | (t',_)<-absor1] -- this take 1 is necessary. See Ticket #398
                , "<=>"
                )
         | isEUni r && not (null absor1')
              = ( case head absor1' of
                    (_,[]) -> l
                    (_,ts) -> l ./\. foldr1 (.\/.) ts
                , ["absorb "++shw t'++", using law x/\\(y\\/-x)  =  x/\\y" | (t',_)<-absor1'] -- this take 1 is necessary. See Ticket #398
                , "<=>"
                )
         | otherwise = (t ./\. f, steps++steps', fEqu [equ',equ''])
         where (t,steps, equ')  = nM posCpl l []
               (f,steps',equ'') = nM posCpl r (l:rs)
               absorbClasses = eqClass (==) (exprIsc2list l++exprIsc2list r)
               incons = [conjunct |conjunct<-exprIsc2list r,conjunct==notCpl l]
               absor0  = [disjunct | disjunct<-exprUni2list l, f'<-rs++exprIsc2list r, disjunct==f']
               absor0' = [disjunct | disjunct<-exprUni2list r, f'<-rs++exprIsc2list l, disjunct==f']
               absor1  = [(disjunct, exprUni2list l>-[disjunct]) | disjunct<-exprUni2list l, ECpl f'<-rs++exprIsc2list r, disjunct==f']++
                         [(disjunct, exprUni2list l>-[disjunct]) | disjunct@(ECpl t')<-exprUni2list l, f'<-rs++exprIsc2list r, t'==f']
               absor1' = [(disjunct, exprUni2list r>-[disjunct]) | disjunct<-exprUni2list r, ECpl f'<-rs++exprIsc2list l, disjunct==f']++
                         [(disjunct, exprUni2list r>-[disjunct]) | disjunct@(ECpl t')<-exprUni2list r, f'<-rs++exprIsc2list l, t'==f']
               absorbAsy = eqClass same eList where e `same` e' = isAsy e && isAsy e' && e == flp e'
               absorbAsyRfx = eqClass same eList where e `same` e' = isRfx e && isAsy e && isRfx e' && isAsy e' && e == flp e'
               eList  = rs++exprIsc2list l++exprIsc2list r
     nM posCpl (EUni (EIsc (l,k),r)) _  | posCpl==dnf    = ((l.\/.r) ./\. (k.\/.r), ["distribute \\/ over /\\"],"<=>")
     nM posCpl (EUni (l,EIsc (k,r))) _  | posCpl==dnf    = ((l.\/.k) ./\. (l.\/.r), ["distribute \\/ over /\\"],"<=>")
     nM posCpl (EUni (ECpl x,r@(ELrs (z,y)))) _          = if sign x==sign z -- necessary to guarantee that sign expr is equal to sign of the result
                                                           then (notCpl (x .:. y) .\/. z, ["remove left residual (/)"],"<=>")
                                                           else (notCpl t .\/. f, steps++steps', fEqu [equ',equ''])
                                                              where (t,steps, equ')  = nM (not posCpl) x []
                                                                    (f,steps',equ'') = nM posCpl r []
     nM posCpl (EUni (l@(ELrs (z,y)),ECpl x)) _          = if sign x==sign z -- necessary to guarantee that sign expr is equal to sign of the result
                                                           then (notCpl (x .:. y) .\/. z, ["remove left residual (/)"],"<=>")
                                                           else (notCpl t .\/. f, steps++steps', fEqu [equ',equ''])
                                                              where (t,steps, equ')  = nM (not posCpl) x []
                                                                    (f,steps',equ'') = nM posCpl l []
     nM posCpl (EUni (l@(ERrs (x,z)),ECpl y)) _          = if sign y==sign z -- necessary to guarantee that sign expr is equal to sign of the result
                                                           then (notCpl (x .:. y) .\/. z, ["remove right residual (\\)"],"<=>")
                                                           else (notCpl t .\/. f, steps++steps', fEqu [equ',equ''])
                                                              where (t,steps, equ')  = nM (not posCpl) y []
                                                                    (f,steps',equ'') = nM posCpl l []
     nM posCpl (EUni (ECpl y,r@(ERrs (x,z)))) _          = if sign y==sign z -- necessary to guarantee that sign expr is equal to sign of the result
                                                           then (notCpl (x .:. y) .\/. z, ["remove right residual (\\)"],"<=>")
                                                           else (notCpl t .\/. f, steps++steps', fEqu [equ',equ''])
                                                              where (t,steps, equ')  = nM (not posCpl) y []
                                                                    (f,steps',equ'') = nM posCpl r []
     nM posCpl x@(EUni (l,r)) rs
-- Absorb equals:    r\/r  -->  r
         | t/=l || f/=r
              = (t .\/. f, steps++steps', fEqu [equ',equ''])
         | or [length cl>1 |cl<-absorbClasses]   -- yields False if absorbClasses is empty
              = ( foldr1 (.\/.) [head cl | cl<-absorbClasses]  -- cl cannot be empty, because it is made by eqClass
                , [shw e++" \\/ "++shw e++" = "++shw e | cl<-absorbClasses, length cl>1, let e=head cl]
                , "<=>"
                )
-- Tautologies:
         | (not.null) tauts               = (EDcV (sign x), ["let e = "++ shw (head tauts)++". Since -e\\/e = V we get"], "<=>")   -- r\/-r  -->  V
         | isTrue l                       = (EDcV (sign x), ["V\\/x = V"], "<=>")                                                  -- r\/V   -->  V
         | isTrue r                       = (EDcV (sign x), ["x\\/V = V"], "<=>")
-- Absorb -V:    r\/-V  --> r
         | isFalse l                      = (r, ["-V\\/x = x"], "<=>")
         | isFalse r                      = (l, ["x\\/-V = x"], "<=>")
-- Absorb:    (x/\\y)\\/y  =  y
         | isEIsc l && not (null absor0)  = let t'=head absor0  in (r, ["absorb "++shw l++" because of "++shw t'++", using law  (x/\\y)\\/y = y"], "<=>")
         | isEIsc r && not (null absor0') = let t'=head absor0' in (r, ["absorb "++shw r++" because of "++shw t'++", using law  (x/\\y)\\/x = x"], "<=>")
-- Absorb:    (x/\\-y)\\/y  =  x\\/y
         | isEIsc l && not (null absor1)
              = ( case head absor1 of
                    (_,[]) -> r
                    (_,ts) -> foldr1 (./\.) ts .\/. r
                , ["absorb "++shw t'++", using law (x/\\-y)\\/y  =  x\\/y" | (t',_)<-absor1]
                , "<=>"
                )
         | isEIsc r && not (null absor1')
              = ( case head absor1' of
                    (_,[]) -> l
                    (_,ts) -> l .\/. foldr1 (./\.) ts
                , ["absorb "++shw t'++", using law x\\/(y/\\-x)  =  x\\/y" | (t',_)<-absor1' ]
                , "<=>"
                )
         | otherwise = (t .\/. f, steps++steps', fEqu [equ',equ''])
         where (t,steps, equ')  = nM posCpl l []
               (f,steps',equ'') = nM posCpl r (l:rs)
            -- absorption can take place if two terms are equal. So let us make a list of equal terms: absorbClasses (for substituting r\/r by r)
               absorbClasses = eqClass (==) (exprUni2list l++exprUni2list r)
            -- tautologies occur if -r\/r, so we are looking for pairs, (x,l) such that x== -l
               tauts = [t' |disjunct<-exprUni2list r,disjunct==notCpl l, ECpl t'<-[disjunct,l]]
               absor0  = [t' | t'<-exprIsc2list l, f'<-rs++exprUni2list r, t'==f']
               absor0' = [t' | t'<-exprIsc2list r, f'<-rs++exprUni2list l, t'==f']
               absor1  = [(t', exprIsc2list l>-[t']) | t'<-exprIsc2list l, ECpl f'<-rs++exprUni2list r, t'==f']++[(e, exprIsc2list l>-[e]) | e@(ECpl t')<-exprIsc2list l, f'<-rs++exprUni2list r, t'==f']
               absor1' = [(t', exprIsc2list r>-[t']) | t'<-exprIsc2list r, ECpl f'<-rs++exprUni2list l, t'==f']++[(e, exprIsc2list r>-[e]) | e@(ECpl t')<-exprIsc2list r, f'<-rs++exprUni2list l, t'==f']
     nM _ (EFlp e) _ | isSym e =  (e,[shw e++" is symmetric"],"<=>")
     nM _ x _               = (x,[],"<=>")

   fEqu :: [String] -> String
   fEqu ss = if and [s=="<=>" | s<-ss] then "<=>" else "==>"
{-
   nfProof :: (Expression -> String) -> Expression -> Proof Expression
   nfProof shw = nfPr shw True True -- The first boolean True means that clauses are derived using <=> derivations. The second True means that a disjunctive normal form is produced.
   nfPr :: (Expression -> String) -> Bool -> Bool -> Expression -> [(Expression, [String], String)]
   nfPr shw eq dnf expr
    = {-if showADL expr=="r \\/ s"
      then fatal 360 ("Diagnose expr: "++showADL expr++"\n"++
                      "eq:            "++show eq++"\n"++
                      "dnf:           "++show eq++"\n"++
                      "res:           "++showADL res++"\n"++
                      "expr==res:     "++show (expr==res)
                     ) else-}
      if expr==res
      then [(expr,[],"<=>")]
      else (expr,steps,equ):nfPr shw eq dnf (simplify res)
    where (res,steps,equ) = normStep shw eq dnf False expr

   cfProof :: (Expression -> String) -> Expression -> Proof Expression
   cfProof shw expr
    = [line | step, line<-init pr]++
      [line | step', line<-init pr']++
      [last ([(expr,[],"<=>")]++
             [line | step, line<-pr]++
             [line | step', line<-pr']
            )]
      where pr           = nfPr shw True False (simplify expr)
            (expr',_,_)  = if null pr then fatal 328 "last: empty list" else last pr
            step         = simplify expr/=expr' -- obsolete?    || and [null s | (_,ss,_)<-pr, s<-ss]
            pr'          = nfPr shw True False (simplify expr')
            step'        = simplify expr'/=simplify expr'' -- obsolete?    || and [null s | (_,ss,_)<-pr', s<-ss]
            (expr'',_,_) = if null pr' then fatal 337 "last: empty list" else last pr'

   conjNF :: Expression -> Expression
   conjNF expr = e where (e,_,_) = if null (cfProof (\_->"") expr) then fatal 340 "last: empty list" else last (cfProof (\_->"") expr)

   disjNF :: Expression -> Expression
   disjNF expr = e where (e,_,_) = if null (dfProof (\_->"") expr) then fatal 343 "last: empty list" else last (dfProof (\_->"") expr)

   dfProof :: (Expression -> String) -> Expression -> Proof Expression
   dfProof shw expr
    = [line | step, line<-init pr]++
      [line | step', line<-init pr']++
      [last ([(expr,[],"<=>")]++
             [line | step, line<-pr]++
             [line | step', line<-pr']
            )]
      where pr           = nfPr shw True True expr
            (expr',_,_)  = if null pr then fatal 356 "last: empty list" else last pr
            step         = simplify expr/=simplify expr'
            pr'          = nfPr shw True True expr'
            step'        = simplify expr'/=simplify expr''
            (expr'',_,_) = if null pr' then fatal 365 "last: empty list" else last pr'
-}
   isEUni :: Expression -> Bool
   isEUni EUni{}  = True
   isEUni _       = False

   isEIsc :: Expression -> Bool
   isEIsc EIsc{}  = True
   isEIsc _       = False
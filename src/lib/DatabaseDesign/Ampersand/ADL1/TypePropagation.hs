{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
{-# LANGUAGE RelaxedPolyRec #-} -- RelaxedPolyRec required for OpenSuse, for as long as we@OpenUniversityNL use an older GHC
module DatabaseDesign.Ampersand.ADL1.TypePropagation (
 -- * Exported functions
 typing, TypeTerm(..), dual, typGlb, typLub, symClosure, Typemap, typeToMap, (.+.), (.<.), (.=.), parallelList, findIn, showType, Guarded(..), p_flp
 , mrgUnion
 )
where
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree hiding (sortWith, maxima, greatest)
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Basics (name, isc, fatalMsg)
import DatabaseDesign.Ampersand.Fspec.ShowADL
import Prelude hiding (head)
import DatabaseDesign.Ampersand.Input.ADL1.CtxError
import Data.List hiding (head)
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map hiding (Map)

head :: [a] -> a
head [] = fatal 30 "head must not be used on an empty list!"
head (a:_) = a

fatal :: Int -> String -> a
fatal = fatalMsg "ADL1.TypePropagation"


{- Type terms are introduced to represent a set of atoms.
The atoms are not actually computed, but sets of atoms are represented for the purpose of constructing a graph of sets.
Example: TypExpr t Src False means (the set of atoms that is) the domain of term t.
-}

data TypeTerm
   = TypExpr      { ttTerm    :: Term        -- The term of which the type is analyzed
                  , ttSorT    :: SrcOrTgt    -- Src if this term represents the domain, Trg if it represents the codomain
                  , ttCplt    :: Bool        -- True if this term represents the complement, False if it doesn't represent the complement
                  }
   | TypEmpty     { ttTerm    :: Term        -- TypEmpty t s represents the empty set. Term t and String s are there to distinguish different empty sets.
                  , ttStr     :: String      -- a string to distinguish two TypEmpty's with the same term (used in P_Declaration, for instance)
                  }
   | TypFull      { ttConcept :: P_Concept   -- TypFull t s represents a concept. Term t and String s are there to distinguish different empty sets.
                  }
   | TypLub       { ttA       :: TypeTerm
                  , ttB       :: TypeTerm
                  }
   | TypGlb       { ttA       :: TypeTerm
                  , ttB       :: TypeTerm
                  }
   | TypInter     { ttTerm    :: Term
                  , ttCplt    :: Bool        -- True if this term represents the complement, False if it doesn't represent the complement
                  }

typGlb, typLub :: TypeTerm -> TypeTerm -> TypeTerm
(typGlb, typLub) = (f TypGlb,  f TypLub)
 where
   f combinator a b = combinator a' b' where [a',b'] = sort [tidy a, tidy b]
   tidy :: TypeTerm -> TypeTerm
   tidy (TypExpr (Pid _ c) _ False) = TypFull c
   tidy (TypExpr t@Pid{}   _ True ) = TypEmpty t ""
   tidy (TypExpr (Pfull _ s _) Src False) = TypFull s
   tidy (TypExpr (Pfull _ _ t) Tgt False) = TypFull t
   tidy (TypExpr (Pfull o s _) Src True ) = TypEmpty (Pid o s) ""
   tidy (TypExpr (Pfull o _ t) Tgt True ) = TypEmpty (Pid o t) ""
   tidy tt = tt

type Typemap = Map TypeTerm [TypeTerm]

infixl 2 .+.   -- concatenate two lists of types
infixl 3 .<.   -- makes a list of one tuple (t,t'), meaning that t is a subset of t'
infixl 3 .=.   -- makes a list of two tuples (t,t') and (t',t), meaning that t is equal to t'
--infixl 4 .&.   -- 
typeToMap :: TypeTerm -> Typemap
typeToMap x = Map.fromList [(x,[])]
(.<.) :: TypeTerm -> TypeTerm -> Typemap
a .<. b = case (a,b) of
           (TypExpr (Pid _ c) _ False, _) -> TypFull c .<. b
           (_, TypExpr (Pid _ c) _ False) -> a .<. TypFull c
           (TypExpr t@Pid{}   _ True , _) -> TypEmpty t "" .<. b
           (_, TypExpr t@Pid{}   _ True ) -> a .<. TypEmpty t ""
           (TypEmpty _ _, _) -> a `leq` b 
           (_, TypEmpty _ _) -> b `leq` a 
           (TypFull  _  , _) -> b `leq` a 
           (_, TypFull  _  ) -> a `leq` b 
           _                 -> a `leq` b .+. dual b `leq` dual a
           where leq x y = Map.fromList [(x, [y]),(y, [])] -- x tuple meaning that x is x subset of y, and introducing y as x key.
(.=.) :: TypeTerm -> TypeTerm -> Typemap
a .=. b = case (a,b) of
           (TypExpr (Pid _ c) _ False, _) -> TypFull c .=. b
           (_, TypExpr (Pid _ c) _ False) -> a .=. TypFull c
           (TypExpr t@Pid{}   _ True , _) -> TypEmpty t "" .=. b
           (_, TypExpr t@Pid{}   _ True ) -> a .=. TypEmpty t ""
           (TypEmpty _ _, _) -> a `eq` b 
           (_, TypEmpty _ _) -> a `eq` b 
           (TypFull  _  , _) -> a `eq` b 
           (_, TypFull  _  ) -> a `eq` b 
           _                 -> a `eq` b .+. dual b `eq` dual a
           where eq x y = Map.fromList [(x, [y]),(y, [x])]  -- for some reason,  eq x y = x.<.y .+. y.<.x   does not work... Why?  No idea!
(.+.) :: (Ord a, Show a) => Map a [a] -> Map a [a] -> Map a [a]
m1 .+. m2 = Map.unionWith mrgUnion m1 m2

data Between = Between BetweenError -- Error in case this between turns out to be untypable. WARNING: equality-check ignores this!
                    TypeTerm -- lhs type, e.g. cod(a)
                    TypeTerm -- rhs type, e.g. dom(b)
                    BetweenType -- whether for this term, the intersection or the union should be a valid type, or both
type BetweenError = ([P_Concept] -> [P_Concept] -> CtxError)
data BetweenType = BetweenType BTUOrI TypeTerm -- this must be a union/intersect type: foloowing the st-graph, a type must be encountered in the union/intersection (this must be a non-empty union/intersection and it must get a name)
                 | BTEqual     -- both sides must have the same type. Note that this is different from adding .=.
                               -- BTEqual requires both sides to be named and equal; this will be tested
                               -- while adding .=. makes both sides equal
                   -- deriving (Ord,Eq,Show) -- Show is for debug purposes only)
data BTUOrI = BTUnion | BTIntersection deriving (Ord,Eq,Show) -- Show is for debug purposes only.

instance Show Between where
    showsPrec _ (Between _ lhs rhs _) = showString (show lhs++"\n"++show rhs)

instance Prelude.Ord BetweenType where
  compare (BetweenType a _) (BetweenType b _) = compare a b
  compare BTEqual BTEqual = EQ
  compare _ BTEqual = LT
  compare BTEqual _ = GT

instance Eq BetweenType where
  (==) a b = (compare a b) == EQ
  
instance Prelude.Ord Between where
  compare (Between _ a b c) (Between _ d e f) = compare (c,a,b) (f,d,e)

instance Eq Between where
  (==) a b = (compare a b) == EQ

instance Show TypeTerm where
    showsPrec _ typTerm = showString (showType typTerm)

showType :: TypeTerm -> String
showType t
 = case t of
     TypExpr (Pid _ c) _ False                    -> fatal 84 ("pop ("++name c++")")
     TypExpr (Pid _ c) _ True                     -> fatal 85 ("empty ("++name c++")")
     TypExpr term@(PVee _)      sORt complemented -> codOrDom sORt complemented++" ("++showADL term++") "  --   ++"("++ show o++")"
     TypExpr term@(Pfull _ _ _) sORt complemented -> codOrDom sORt complemented++" ("++showADL term++")"   --   
     TypExpr term               sORt complemented -> codOrDom sORt complemented++" ("++showADL term++") "  --   ++ show (origin term)
     TypEmpty (Pid _ c) _                         -> "empty ("++name c++")"
     TypEmpty term str                            -> "empty "++"("++showADL term++") "++str++" side"
     TypFull c                                    -> "pop ("++name c++")"
     TypLub a b                                   -> showType a++" \\/ "++showType b
     TypGlb a b                                   -> showType a++" /\\ "++showType b
     TypInter term complemented                   -> "between"++(if complemented then "c " else " ")++showADL term
   where codOrDom Src True  = "domc"
         codOrDom Src False = "dom"
         codOrDom Tgt True  = "codc"
         codOrDom Tgt False = "cod"

dual :: TypeTerm -> TypeTerm
dual tt
 = case tt of
     TypExpr (Pid _ c) _ False      -> TypEmpty (Pid (origin c) c) "" -- fatal 102 "Can TypExpr (Pid _ c) _ False occur?" -- 
     TypExpr (Pid _ c) _ True       -> TypFull c                      -- fatal 103 "Can TypExpr (Pid _ c) _ True  occur?" -- 
     TypExpr (PVee _)      _ _      -> fatal 104 ("TypExpr (PVee o) _ True  should never occur.")
     TypExpr (Pfull _ _ _) _ _      -> fatal 105 ("TypExpr (Pfull _ _ _) _ True  should never occur.")  
     TypExpr term sORt complemented -> TypExpr term sORt (not complemented)
     TypEmpty (Pid _ c) _           -> TypFull c
     TypEmpty term _                -> fatal 108 ("TypEmpty ("++showADL term++")  has no dual.")
     TypFull c                      -> TypEmpty (Pid (origin c) c) ""
     TypLub a b                     -> typGlb (dual a) (dual b)
     TypGlb a b                     -> typLub (dual a) (dual b)
     TypInter t complemented        -> TypInter t (not complemented)

-- | Equality of TypeTerm is needed for the purpose of making graphs of types.
--   These are used in the type checking process.
--   The main idea is that the equality distinguishes between occurrences.
--   So term 'r' on line 14:3 differs from  the term 'r' on line 87:19.
--   However, different occurrences of specific terms that are fully typed (e.g. I[Person] or parent[Person*Person]), need not be distinguised.
instance Prelude.Ord TypeTerm where -- first we fix all forms of I's, which satisfy r = r~.
  compare (TypExpr (Pid _ c)      _ b) (TypExpr (Pid _ c')       _ b') = Prelude.compare (c,b) (c',b')
--  compare (TypFull c                 ) (TypExpr (Pid _ c')    _ False) = Prelude.compare c c'
--  compare (TypExpr (Pid _ c)  _ False) (TypFull c'                   ) = Prelude.compare c c'
--  compare (TypEmpty (Pid _ c) s      ) (TypExpr (Pid _ c')    _ True ) = Prelude.compare c c'
--  compare (TypExpr (Pid _ c)  _  True) (TypEmpty (Pid _ c') s'       ) = Prelude.compare c c'
  compare (TypExpr (Pid _ _)      _ _) (TypExpr _                _ _ ) = Prelude.LT
  compare (TypExpr _              _ _) (TypExpr (Pid _ _ )       _ _ ) = Prelude.GT
  compare (TypExpr (Patm _ x [c]) _ b) (TypExpr (Patm _ x' [c']) _ b') = Prelude.compare (x,c,b) (x',c',b')
  compare (TypExpr (Patm _ _ [_]) _ _) (TypExpr (Patm _ _   _  ) _ _ ) = Prelude.LT
  compare (TypExpr (Patm _ _  _ ) _ _) (TypExpr (Patm _ _  [_ ]) _ _ ) = Prelude.GT
  compare (TypExpr (Patm o _ [] ) _ b) (TypExpr (Patm o' _  [] ) _ b') = Prelude.compare (o,b) (o',b')  -- the atom name is determined by o, so only compare with o.
  compare (TypExpr (Patm _ _ _  ) _ _) (TypExpr (Patm _  _  _  ) _ _ ) = fatal 76 "Patm should not have two types"
  compare (TypExpr (Patm _ _ _  ) _ _) (TypExpr _                _ _ ) = Prelude.LT
  compare (TypExpr _              _ _) (TypExpr (Patm _ _ _)     _ _ ) = Prelude.GT
  -- note that V = V~ is only true under restrictions
  compare (TypExpr (PVee o)       x b) (TypExpr (PVee o')      x' b' ) = Prelude.compare (o,x,b) (o',x',b') -- This is a V of which the type must be determined (by the environment).
  compare (TypExpr (PVee _)       _ _) (TypExpr _                _ _ ) = Prelude.LT
  compare (TypExpr _              _ _) (TypExpr (PVee _)         _ _ ) = Prelude.GT
  -- here we implement V[A*B] = V[B*A]~ directly in the TypExpr
  compare (TypExpr (Pfull _ s t)  x b) (TypExpr (Pfull _ s' t') x' b') = Prelude.compare (if x==Src then (s,t,b) else (t,s,b)) (if x'==Src then (s',t',b') else (t',s',b')) -- This is a V of which the type is determined by the user
  compare (TypExpr (Pfull _ _ _)  _ _) (TypExpr _                _ _ ) = Prelude.LT
  compare (TypExpr _              _ _) (TypExpr (Pfull _ _ _)    _ _ ) = Prelude.GT
  -- as r = r~ does not hold in general, we need to compare x'==y'
  compare (TypExpr x             x' b) (TypExpr y               y' b') = Prelude.compare (x,x',b) (y,y',b')
  compare (TypExpr _              _ _) _                               = Prelude.LT
  compare _                            (TypExpr _                _ _ ) = Prelude.GT
  compare (TypEmpty (Pid _ c) s      ) (TypEmpty (Pid _ c') s'       ) = Prelude.compare (c,s) (c',s')
  compare (TypEmpty  t        s      ) (TypEmpty  t'        s'       ) = Prelude.compare (t,s) (t',s')
  compare (TypEmpty _ _              ) _                               = Prelude.LT
  compare _                            (TypEmpty _ _                 ) = Prelude.GT
  compare (TypFull c                 ) (TypFull c'                   ) = Prelude.compare c c'
  compare (TypFull  _                ) _                               = Prelude.LT
  compare _                            (TypFull  _                   ) = Prelude.GT
  compare (TypInter t b              ) (TypInter t' b'               ) = compare (t,b) (t',b')
  compare (TypInter _ _              ) _                               = Prelude.LT
  compare _                            (TypInter _ _                 ) = Prelude.GT
  compare (TypLub a b                ) (TypLub a' b'                 ) = compare (a,b) (a',b')
  compare (TypLub _ _                ) _                               = Prelude.LT
  compare _                            (TypLub _ _                   ) = Prelude.GT
  compare (TypGlb a b                ) (TypGlb a' b'                 ) = compare (a,b) (a',b')
--  compare (TypGlb _ _                ) _                               = Prelude.LT -- these two lines are superfluous if all combinators are covered.
--  compare _                            (TypGlb _ _                   ) = Prelude.GT

  -- since the first argument of Between is a function, we cannot compare it.
  -- Besides, if there are two identical type inferences with different error messages, we should just pick one.
  -- compare (Between _ a b t) (Between _ a' b' t')                    = compare (t,a,b) (t',a',b')

instance Eq TypeTerm where
  t == t' = compare t t' == EQ

-- On Guarded: it is intended to return something, as long as there were no errors creating it.
-- For instance, (Guarded P_Context) would return the P_Context, unless there are errors.

data Guarded a = Errors [CtxError] | Checked a deriving Show


parallelList :: [Guarded a] -> Guarded [a] -- get all values or collect all error messages
parallelList = foldr (parallel (:)) (Checked [])
  where parallel :: (a->b->c) -> Guarded a -> Guarded b -> Guarded c -- get both values or collect all error messages
        parallel f ga = (<*>) (fmap f ga)
        {- the following definition is equivalent:
        parallel f (Checked a) (Checked b) = Checked (f a b)
        parallel f (Errors  a) (Checked b) = Errors a
        parallel f (Checked a) (Errors  b) = Errors b
        parallel f (Errors  a) (Errors  b) = Errors (a ++ b)
        -- this function is used as a convenience to define parallelList
        -}

instance Functor Guarded where
 fmap _ (Errors a) = (Errors a)
 fmap f (Checked a) = Checked (f a)

instance Applicative Guarded where
 pure = Checked
 (<*>) (Checked f) (Checked a) = Checked (f a)
 (<*>) (Errors  a) (Checked _) = Errors a 
 (<*>) (Checked _) (Errors  b) = Errors b
 (<*>) (Errors  a) (Errors  b) = Errors (a ++ b)

instance Monad Guarded where
 (>>=) (Errors  a) _ = (Errors a)
 (>>=) (Checked a) f = f a
 return = Checked
 fail s = fatal 926 ("Error generated by fail of a Guarded something (probably a pattern-match failure in a `do'), message: \n "++s)

-- | p_flp computes the inverse of a Term.
p_flp :: Term -> Term
p_flp a@PI{}        = a
p_flp a@Pid{}       = a
p_flp a@Patm{}      = a
p_flp (Pfull o s t) = Pfull o t s
p_flp (PFlp _ a)    = a
p_flp a             = PFlp (origin a) a

decToTyp :: SrcOrTgt -> Bool -> P_Declaration -> TypeTerm
decToTyp sORt complemented d = TypExpr (PTrel (origin d) (dec_nm d) (dec_sign d)) sORt complemented

improveBindings :: (Ord a,Show a,Ord b,Show b)
                => (Map a [b] -> Map TypeTerm [(a,b,TypeTerm)])
                -> [[TypeTerm]] -- equality classes for Between-like bindings yield improved propagation on (I /\ I /\ I);I-like terms.
                -> (Map a [b], Map TypeTerm [TypeTerm])
                -> (Map a [b], Map TypeTerm [TypeTerm])
improveBindings typByTyp eqtyps (oldMap,st')
 = (bindings', stPlus)
  where bindings' = Map.union decisions oldMap
        bindings = Map.mapMaybe checkOne bindings' -- these are final: add them to st'
        expanded = Map.fromListWith mrgUnion [(t1,[t2 | (_,_,t2)<-r]) | (t1,r)<-Map.toList (typByTyp bindings)]
        decisions = Map.filter (not.null) $ makeDecisions typByTyp' st' eqtyps
        stPlus = setClosure (st' .+. symClosure expanded) "(st' \\/ bindings')*"
        typByTyp' = typByTyp oldMap
        checkOne [c] = Just [c]
        checkOne _ = Nothing

-- find out which bindings can be determined.
-- candidate bindings are given in the first argument (inp), where the triple (from,to,TypeTerm) is used to bind "from" to "to"
-- (this results in a map of possible bindings)
-- Note that it is possible that an element of type "from" is not in the final map (hence totality of the resulting map is not guaranteed)
makeDecisions :: (Ord from,Ord to,Show to,Show from) =>
                    Map TypeTerm [(from,to,TypeTerm)] -- when binding "from" to "to", one knows that the first type equals the second
                 -> Map TypeTerm [TypeTerm] -- reflexive transitive graph with inferred types
                 -> [[TypeTerm]] -- classes of types that - according to Between-like bindings - should be of equal types
                 -> Map from [to] -- resulting bindings
makeDecisions inp    -- 
              trnRel
              eqtyps
 = foldl (Map.unionWith mrgIntersect) Map.empty [ Map.filter (not . null) d
                                                | eqs <- eqtyps
                                                , let d = (getDecision eqs)
                                                ]
 where inpTrnRel = composeMaps trnRel inp                      -- Relational: inpTrnRel = trnRel;inp
       typsFull = trnRel .+. Map.map (getConcsFromTriples) inpTrnRel
       getConcsFromTriples [] = []
       getConcsFromTriples ((_,_,x):xs) = mrgUnion (Map.findWithDefault [] x trnRel') (getConcsFromTriples xs)
       trnRel' = Map.map (filter isConc) trnRel
        where
--         isConc (TypExpr (Pid _ _) _ False) = True
         isConc (TypExpr (Pid _ _) _ _) = fatal 239 "can this occur?"
         isConc (TypFull _) = True
         isConc _ = False
       f :: TypeTerm -> [TypeTerm]
       f x = Map.findWithDefault [] x trnRel'
       f' x = Map.findWithDefault [] x typsFull
       getDecision equals
        = foldl (Map.unionWith mrgIntersect) Map.empty allDecisions
          where iscTyps = isctAll (map f equals) `orWhenEmpty` isctAll (map f' equals)
                isctAll :: (Ord a,Show a) => [[a]] -> [a]
                -- isctAll [] = []
                isctAll x = [e | (e,v)<-Map.toList tmap, v>=top]
                  where tmap = Map.fromListWith (+) [(x'',1) | x'<-x,x''<-x']
                        top = (foldr max (0::Int) (Map.elems tmap))
                allDecisions
                 = [ Map.fromListWith mrgUnion [ (t,[d]) | (t,d,tp) <- Map.findWithDefault [] src inpTrnRel
                                                         , t' <- Map.findWithDefault [] tp trnRel'
                                                         , t' `elem` iscTyps
                                                          ]
                   | src <- equals]

orWhenEmpty :: [a] -> [a] -> [a]
orWhenEmpty [] n = n
orWhenEmpty n  _ = n

-- | The purpose of 'typing' is to analyse the domains and codomains of a term in a context.
--   It expects a Typemap, st, which is based on list of tuples [(TypeTerm,TypeTerm)] made by uType.
--   This Typemap represents an order on TypeTerms, which can be interpreted as subsets.
--   The function typing also expects a list of "between terms",
--   which represent types that were created due to the use of specific operators, such as compose, union and intersect.
--   Finally, it expects the declaration table.

typing :: Typemap -> Map String [P_Declaration]
          -> ( Typemap                    -- st               -- the st relation: 'a st b' means that  'dom a' is a subset of 'dom b'
             , Typemap                    -- stClos           -- (st\/stAdded)*\/I  (reflexive and transitive)
             , Typemap                    -- eqType           -- (st*/\st*~)\/I  (reflexive, symmetric and transitive)
             , Typemap                    -- stClos           -- additional links added to stClos
             , Typemap                    -- stClos0          -- st* plus bindings for terms (transitive)
             , Guarded ( Map Term P_Declaration -- bindings   -- declarations that may be bound to relations
                       , TypeTerm -> P_Concept) -- srcTypes   -- types of terms and betweens
             , Map P_Concept [P_Concept]  -- isaClos          -- concept lattice
             , Map P_Concept [P_Concept]  -- isaClosReversed  -- same, but reversed
             )                                   
typing st declsByName
  = ( st
    , stClos
    , eqType
    , stClos
    , stClos0
    , do _ <- checkUndefined  -- relation for which there is no declaration
         _ <- checkBindings   -- unresolved bindings for relations
         _ <- checkIVBindings -- unresolved bindings for I and V
--         _ <- checkBetweens   -- errors in matching operations such as ;
         return ( bindings, srcTypes )
  -- isas is produced for the sake of making a partial order of concepts in the A-structure.
    , isaClos
    , isaClosReversed   -- a list containing all tuples of concepts that are in the subset relation with each other.
             -- it is used for the purpose of making a poset of concepts in the A-structure.
    ) 
 where
   -- The story: two Typemaps are made by uType, each of which contains tuples of the relation st.
   --            These are converted into two maps (each of type Typemap) for efficiency reasons.
   
    allIVs     = nub' (sort [e | (TypExpr e _ _)<-typeTerms, isIV e])
    isIV   (PI _)       = True
    isIV   (PVee _)     = True
    isIV   (Patm _ _ _) = True
    isIV   _            = False
    allTerms    = [e | TypExpr e _ _ <- typeTerms]
    allConcs    = [c | (Pid _ c) <- allTerms]
    
    checkUndefined = parallelList (map checkNonempty (Map.toList declByTerm))  -- declByTerm maps Prel and PTrel terms to declarations, merely by their name or name+signature.
    checkNonempty (t,[]) = Errors [CxeRelUndefined { cxeExpr = t}]
    checkNonempty _ = return ()
    checkBindings = parallelList (map checkUnique (Map.toList newBindings)) -- newBindings maps Prel and PTrel terms to declarations after the typing algorithm.
    checkUnique (_,[_]) = return ()
    checkUnique (t,xs) = Errors [CxeRel { cxeExpr=t
                                        , cxeDecs=xs
                                        , cxeSNDs=Map.findWithDefault [] t declByTerm
                                        }]
                                        
    -- check that all I's and V's have types. If not, throw an error where V's are replaced for Cpl if they occur in it
    checkIVBindings = parallelList (map checkUnique2 allIVs)
    checkUnique2 iv = case ( Map.findWithDefault [] (TypExpr iv Src False) ivBoundConcepts
                           , Map.findWithDefault [] (TypExpr iv Tgt False) ivBoundConcepts) of
                        ([_],[_]) -> return ()
                        (xs,ys) -> Errors [CxeSign {cxeExpr=fromVtoCpl iv, cxeSrcs=xs, cxeTrgs=ys}]
    
    fromVtoCpl v@(PVee o) = head ([t | t@(PCpl o' _) <- allTerms, o==o'] ++ [v])
    fromVtoCpl x = x

{- checkBetweens produces error messages for the "between terms", 
    checkBetweens = parallelList (map checkBetween betweenTerms)
    checkBetween (Between e src trg BTEqual) -- since the BTEqual does not participate in stClosAdd, it will be isolated here
     = case (srcTypes' src,srcTypes' trg) of
              ([a],[b]) -> if a==b then    -- applicable to equality (i.e. rules with = )
                             return ()
                           else Errors [e [a] [b]]
              (a,b) -> Errors [e a b]
    checkBetween (Between e src trg (BetweenType _ t))
     = case srcTypes' t of
        [_] -> return () -- if the between-term is unique, everything is fine...
        _ -> Errors [e (srcTypes' src) (srcTypes' trg)]
-}

    stClosAdded :: Typemap         -- stClosAdded should be transitive and reflexive
    stClosAdded = addIdentity (setClosure (fixPoint stAddEdges (addIdentity stClos1)) "stClosAdded")

-- The purpose of stAddEdges is to enhance a type map with the edges from Between nodes.
--  If a type-term tt is the lub of a set of type-terms tts, all other upper bounds of tt are smaller.
--  So, we must find those upper bounds and add an edge to encode that information in the stGraph.
    stAddEdges :: Typemap -> Typemap
    stAddEdges st
     = foldr (.+.) st
          ( [ a.<.tt .+. b.<.tt | tt@(TypLub a b) <- typeTerms ] ++
            [ tt .<. upper
            | tt@(TypLub a b) <- typeTerms
            , upper <- nub [ t | t<-stClos Map.! a ]
                       `isc`
                       nub [ t | t<-stClos Map.! b ]
            ] ++
            [ tt.<.a .+. tt.<.b | tt@(TypGlb a b) <- typeTerms ] ++
            [ lower .<. tt
            | tt@(TypGlb a b) <- typeTerms
            , lower <- nub [ t | t<-stClosRev Map.! a ]
                       `isc`
                       nub [ t | t<-stClosRev Map.! b ]
            ] ++
            [ lub.=.a | lub@(TypLub a _)<-typeTerms, e@(TypFull  _  )<-eq Map.! a] ++ [ lub.=.b | lub@(TypLub _ b)<-typeTerms, e@(TypFull  _  )<-eq Map.! b] ++
            [ lub.=.b | lub@(TypLub a b)<-typeTerms, e@(TypEmpty _ _)<-eq Map.! a] ++ [ lub.=.a | lub@(TypLub a b)<-typeTerms, e@(TypEmpty _ _)<-eq Map.! b] ++
            [ glb.=.b | glb@(TypGlb a b)<-typeTerms, e@(TypFull  _  )<-eq Map.! a] ++ [ glb.=.a | glb@(TypGlb a b)<-typeTerms, e@(TypFull  _  )<-eq Map.! b] ++
            [ glb.=.a | glb@(TypGlb a _)<-typeTerms, e@(TypEmpty _ _)<-eq Map.! a] ++ [ glb.=.b | glb@(TypGlb _ b)<-typeTerms, e@(TypEmpty _ _)<-eq Map.! b]
          )
          where typeTerms = Map.keys st
                stClosClosRev = composeMaps stClos stClosRev
                stClosRevClos = composeMaps stClosRev stClos
                eq = Map.intersectionWith mrgIntersect st (reverseMap st)   -- eq = (st/\st~)*\/I, so eq is symmetric, transitive (due to setClosure) and reflexive.
                stClos = addIdentity (setClosure st "st")   -- stClos = stClosAdded*\/I, so stClos is transitive (due to setClosure) and reflexive.
                stClosRev = reverseMap stClos  -- stClosReversed is transitive too and like stClos, I is a subset of stClosReversed.

    declByTerm :: Map Term [P_Declaration]
    declByTerm
      = Map.fromList ( [ (o, dbn) 
                       | o@(Prel _ s) <- allTerms
                       , let dbn = Map.findWithDefault [] s declsByName
                       ] ++
                       [ (o,filter ((==) sgn . dec_sign) $ dbn)
                       | o@(PTrel _ s sgn) <- allTerms
                       , let dbn = Map.findWithDefault [] s declsByName
                       ]
                     )
    
    ivTypByTyp :: Map TypeTerm [P_Concept] -> Map TypeTerm [(TypeTerm,P_Concept,TypeTerm)]
    ivTypByTyp ivMap = Map.fromListWith mrgUnion [ (tp,map (\c -> (tp,c,TypFull c)) concs)
                                                 | (tp,concs) <- Map.toList ivMap ]
    
    typByTyp :: Map Term [P_Declaration] -> Map TypeTerm [(Term,P_Declaration,TypeTerm)]
    typByTyp oldMap = Map.fromList [ (trm, quadruples sORt complemented t) | trm@(TypExpr t sORt complemented) <- typeTerms ]
         where quadruples sORt complemented t = sort [(t,d,decToTyp sORt complemented d ) | d <- Map.findWithDefault [] t oldMap]

    firstClos = setClosure (addIdentity st) "(st \\/ I)*"
    firstClosSym = Map.intersectionWith mrgIntersect firstClos (reverseMap firstClos)
    eqtyps = Map.elems (Map.filterWithKey (\x y -> x == head y) firstClosSym)

    (newBindings,stClos0) = fixPoint (improveBindings typByTyp eqtyps)
                                     (declByTerm,firstClos)
    bindings :: Map Term P_Declaration
    bindings = Map.mapMaybe exactlyOne newBindings
    ivBoundConcepts :: Map TypeTerm [P_Concept]
    (ivBoundConcepts, stClos1)
      = fixPoint (improveBindings ivTypByTyp eqtyps) ( Map.fromList [(iv,allConcs) | iv' <- allIVs, iv <- ivToTyps iv']
                                                     , fixPoint stAddEdges stClos0)
    ivToTyps o = nub' [TypExpr o Src False, TypExpr o Tgt False]
    

    exactlyOne [x] = Just x
    exactlyOne _ = Nothing
    
    -- together, the firstSetOfEdges and secondSetOfEdges form the relation st
    typeTerms :: [TypeTerm]          -- The list of all type terms in st.
    typeTerms = Map.keys st -- Because a Typemap is total, it is sufficient to compute  Map.keys st
    
    fixPoint :: Eq a => (a->a) -> a -> a
    fixPoint f a = if a==b then a else fixPoint f b
      where b = f a
    
    -- stClos :: Typemap -- ^ represents the transitive closure of stClosAdded.
    -- Check whether stClosAdded is transitive and reflexive...
    stClos = if stClosAdded == addIdentity (setClosure stClosAdded "stClosAdded") then stClosAdded else
                fatal 358 "stClosAdded should be transitive and reflexive"  -- stClos = stClosAdded*\/I, so stClos is transitive (due to setClosure) and reflexive.
    stClosReversed = reverseMap stClos  -- stClosReversed is transitive too and like stClos, I is a subset of stClosReversed.
    eqType = addIdentity (Map.intersectionWith mrgIntersect stClos stClosReversed)  -- eqType = (st* /\ st*~)\/I   (so: eqType is reflexive, symmetric and transitive)
--    eqType = addIdentity (setClosure (Map.intersectionWith mrgIntersect stClos stClosReversed) "symClos")  -- eqType = (stClos/\stClos~)*\/I   (so: eqType is reflexive, symmetric and transitive)
    isaClos :: Map P_Concept [P_Concept]
    isaClos' = Map.fromDistinctAscList [(c,[c' | TypFull c'<-ts, c'/=c]) | (TypFull c, ts)<-Map.toAscList stClos]
    isaClos  = addIdentity isaClos' 
    isaClosReversed :: Map P_Concept [P_Concept]
    isaClosReversed = reverseMap isaClos
    stConcepts :: Map TypeTerm [P_Concept]
    stConcepts =  Map.map f stClos
                  where f :: [TypeTerm] -> [P_Concept]
                        f ts = [t | t <- ownTypes ts, not (t `elem` derived ts)]
                        ownTypes ts = [c | TypFull c<-ts]
                        derived ts = foldl mrgUnion [] [Map.findWithDefault [] t isaClos' | t<-ownTypes ts]
    srcTypes' :: TypeTerm -> [P_Concept]
    srcTypes' typ = case Map.lookup typ stConcepts of
                      Just x -> x
                      _ -> fatal 447 ("TypeTerm "++show typ++" was not found in stConcepts.")
    srcTypes :: TypeTerm -> P_Concept
    srcTypes typ = case srcTypes' typ of
                   -- A type may have an empty codomain in stConcepts, because that means it is type incorrect.
                    [cs] -> cs
                    [] -> fatal 389 ("TypeTerm "++show typ++" was not found in stConcepts.")
                    cs -> fatal 390 ("TypeTerm "++show typ++" was found in stConcepts more than once: "++intercalate ", " (map name cs))
     
-- | if lst represents a binary relation, then reverseMap lst represents its inverse (viz. flip, wok)
-- | note that the domain must be preserved!
-- | reverseMap r = r~
reverseMap :: (Prelude.Ord a, Show a) => Map a [a] -> Map a [a]
reverseMap lst = (Map.fromListWith mrgUnion (concat [(a,[]):[(b,[a])|b<-bs] | (a,bs)<-Map.toAscList lst]))
-- note: reverseMap is relatively slow, but only needs to be calculated once

-- | addIdentity r = r\/I
addIdentity :: (Prelude.Ord a, Show a) => Map a [a] -> Map a [a]
addIdentity mp = Map.mapWithKey (\k a->mrgUnion a [k]) mp

mapIsOk :: (Show a,Ord a) => Map k [a] -> Bool
mapIsOk m = Map.fold (&&) True (Map.map (isSortedAndDistinct . checkRfx) m)
isSortedAndDistinct :: Ord a => [a] -> Bool
isSortedAndDistinct (x:y:rest) = if (x<y) then isSortedAndDistinct (y:rest) else False
isSortedAndDistinct _ = True
checkRfx :: (Eq a, Show a) => [a] -> [a]
checkRfx (a:as) = if not (a==a) then fatal 192 ("Eq is not reflexive on "++show a) else a:checkRfx as
checkRfx [] = []  

{-
Since vertices of type v may be expensive to compare, we first create an isomorphic key map that
has integer keys as its vertices. For this graph we compute the transitive closure, which we map
back onto a graph with the original vertex type v.
-}
setClosure :: (Show v,Ord v) => Map v [v] -> String -> Map v [v]
setClosure graph s =
  let vertexKeyMap = makeVertexKeyMap $ Map.keys graph
      keyGraph = vertexToKeyGraph vertexKeyMap graph
      keyResult = setClosureSlow keyGraph s
      vertexResult = keyGraphToVertexGraph vertexKeyMap keyResult
  in  vertexResult
  
  -- a map function for graphs represented as (Map vertex [vertex])
mapGraph :: (Ord a, Ord b) => (a->b) -> Map a [a] -> Map b [b]
mapGraph f graph = Map.fromList [ (f v, map f vs) | (v,vs) <- Map.toList graph]

makeVertexKeyMap :: (Show v,Ord v) => [v] -> Map v Int
makeVertexKeyMap allVertices = Map.fromList $ zip allVertices [0..]

vertexToKeyGraph :: (Show v,Ord v) => Map v Int -> Map v [v] -> Map Int [Int]
vertexToKeyGraph vertexKeyMap vertexGraph = mapGraph vertexToKey vertexGraph
 where vertexToKey v = case Map.lookup v vertexKeyMap of
                         Nothing -> fatal 210 $ "vertexToKeyGraph: vertex "++show v++" not in vertexKeyMap"
                         Just i  -> i
  
keyGraphToVertexGraph :: (Show a,Ord a) => Map a Int -> Map Int [Int] -> Map a [a]
keyGraphToVertexGraph vertexKeyMap keyGraph = mapGraph keyToVertex keyGraph
 where keyToVertex i = if i < length allVertices 
                       then allVertices !! i 
                       else fatal 217 $ "keyToVertexGraph: key "++show i++" too large (number of vertices is " ++show (length allVertices)++")"
       allVertices = Map.keys vertexKeyMap

-- | The purpose of 'setClosureSlow' is to compute the transitive closure of relations that are represented as a Map (Map a [a]).
--   For that purpose we use a Warshall algorithm.
setClosureSlow :: (Show a,Ord a) => Map a [a] -> String -> Map a [a]
setClosureSlow xs s | not (mapIsOk xs) = fatal 144 ("setClosure on the non-ok set "++s)
setClosureSlow xs _ = if (mapIsOk res) then res else fatal 145 ("setClosure contains errors!")
  where
--   f q x = Map.map (\bs->foldl mrgUnion bs [b' | b<-bs, b == x, (a', b') <- Map.toList q, a' == x]) q
   f q x = Map.map (\bs->foldl mrgUnion bs [b' | x `elem` bs, Just b' <- [Map.lookup x q]]) q
   res   = foldl f xs (Map.keys xs `isc` nub (concat (Map.elems xs)))


composeMaps :: (Ord k1, Ord a, Show a) => Map k [k1] -> Map k1 [a] -> Map k [a]
composeMaps m1 m2 = Map.map (\x -> foldr mrgUnion [] [Map.findWithDefault [] s m2 | s <- x]) m1
symClosure :: (Ord a, Show a) => Map a [a] -> Map a [a]
symClosure m = m .+. reverseMap m

-- The following mrgUnion and mrgIntersect are more efficient, but lack checking...
mrgUnion :: (Show a,Ord a) => [a] -> [a] -> [a]
mrgUnion (a:as) (b:bs) | a<b       = a:mrgUnion as (b:bs)
                       | a==b      = b: mrgUnion as bs
                       | otherwise = b:mrgUnion (a:as) bs
mrgUnion a b = a ++ b -- since either a or b is the empty list

mrgIntersect :: (Show a,Ord a) => [a] -> [a] -> [a]
mrgIntersect (a:as) (b:bs) | a<b       = mrgIntersect as (b:bs)
                           | a==b      = b: mrgIntersect as bs
                           | otherwise = mrgIntersect (a:as) bs
mrgIntersect _ _ = [] -- since either a or b is the empty list

nub' :: (Show a,Ord a,Eq a) => [a] -> [a]
nub' (x:y:xs) | x==y      = nub' (y:xs)
              | otherwise = x:(nub' (y:xs))
nub' [x] = [x]
nub' [] = []
-- -}

{- The following mrgUnion and mrgIntersect are for debug purposes. DO NOT DELETE!!!
mrgUnion :: (Show a,Ord a) => [a] -> [a] -> [a]
mrgUnion l r = if isSortedAndDistinct res then res else fatal 172 ("merge contains an error")
  where res = if isSortedAndDistinct l then
                (if isSortedAndDistinct r then merge l r
                 else fatal 175 ("mrgUnion should be called on sorted distinct lists, but its second argument is:\n "++show r)
                 )
              else fatal 177 ("mrgUnion should be called on sorted distinct lists, but its first argument is:\n "++show l)
        merge :: (Show a,Ord a) => [a] -> [a] -> [a]
        merge (a:as) (b:bs) | a<b  = if not (b<a) && not (a==b) then a:merge as (b:bs) else fatal 179 ("Compare is not antisymmetric for: "++show a++" and "++show b)
                            | a==b = if (b==a) then distinctCons a b (merge as bs) else fatal 180 ("Eq is not symmetric for: "++show a++" and "++show b)
                            | b<a  = if not (a<b) && not (b==a) then b:merge (a:as) bs else fatal 181 ("Compare is not antisymmetric for: "++show a++" and "++show b)
        merge a b = a ++ b -- since either a or b is the empty list

mrgIntersect :: (Show a,Ord a) => [a] -> [a] -> [a]
mrgIntersect l r = if isSortedAndDistinct res then res else fatal 185 ("merge contains an error")
  where res = if isSortedAndDistinct l then
                (if isSortedAndDistinct r then merge l r
                 else fatal 188 ("mrgIntersect should be called on sorted distinct lists, but its second argument is:\n "++show r)
                 )
              else fatal 190 ("mrgIntersect should be called on sorted distinct lists, but its first argument is:\n "++show l)
        merge :: (Show a,Ord a) => [a] -> [a] -> [a]
        merge (a:as) (b:bs) | a<b  = if not (b<a) && not (a==b) then merge as (b:bs) else fatal 192 ("Compare is not antisymmetric for: "++show a++" and "++show b)
                            | a==b = if b==a then distinctCons a b (merge as bs) else fatal 193 ("Eq is not symmetric for: "++show a++" and "++show b)
                            | b<a  = if not (a<b) && not (b==a) then merge (a:as) bs else fatal 194 ("Compare is not antisymmetric for: "++show a++" and "++show b)
        merge _ _ = [] -- since either a or b is the empty list

nub' :: (Show a,Ord a,Eq a) => [a] -> [a]
nub' xs' = if isSortedAndDistinct res then res else fatal 428 ("nub' expects a sorted list, but the following is not:\n"++show xs')
          where
              res = nub'' xs'
              nub'' (x:y:xs) | x==y      = nub' (y:xs)
                            | otherwise = x:(nub' (y:xs))
              nub'' [x] = [x]
              nub'' [] = []

distinctCons :: (Ord a, Eq a, Show a) => a -> a -> [a] -> [a]
distinctCons a b' (b:bs) = if a<b then b':(b:bs)
                           else if a==b then fatal 164 ("Eq is not transitive:\n "++show a++"=="++show b++"\n but `==` ("++show b'++") ("++show b++") is "++show (b' == b))
                           else fatal 167 (concat (["Ord is not transitive:\n "
                                                   ,"compare ("++show a++") ("++show b'++") == "++show (compare a b')++"\n"
                                                   ,"compare ("++show b'++") ("++show b++") == "++show (compare b' b)++"\n"
                                                   ,"compare ("++show a++") ("++show b++") == "++show (compare a b)++"\n"]))
distinctCons a _ bs = a:bs    
-- -}


-- | lookups is the reflexive closure of findIn. lookups a R = findIn(a,R\/I) where a is an element and R is a relation.
lookups :: (Show a,Ord a) => a -> Map a [a] -> [a]
lookups o q = head ([mrgUnion [o] e | Just e<-[Map.lookup o q]]++[[o]])  

-- | findIn(k,R) yields all l such that: k R l.
findIn :: Ord k => k -> Map k [a] -> [a]
findIn = Map.findWithDefault []  
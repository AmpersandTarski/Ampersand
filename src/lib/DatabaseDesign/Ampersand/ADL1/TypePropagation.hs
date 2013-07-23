{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
{-# LANGUAGE RelaxedPolyRec #-} -- RelaxedPolyRec required for OpenSuse, for as long as we@OpenUniversityNL use an older GHC
module DatabaseDesign.Ampersand.ADL1.TypePropagation (
 -- * Exported functions
 typing, Type(..), Typemap, parallelList, findIn, showType, Guarded(..), p_flp
 , mrgUnion, BetweenType(..), Between(..), BTUOrI(..)
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


type Typemap = Map Type [Type]

data Type = TypExpr Term SrcOrTgt | TypInCps Term | TypInObjDef P_ObjectDef -- term is deriving Ord
data Between = Between BetweenError -- Error in case this between turns out to be untypable. WARNING: equality-check ignores this!
                    Type -- lhs type, e.g. cod(a)
                    Type -- rhs type, e.g. dom(b)
                    BetweenType -- whether for this term, the intersection or the union should be a valid type, or both
type BetweenError = ([P_Concept] -> [P_Concept] -> CtxError)
data BetweenType = BetweenType BTUOrI Type -- this must be a union/intersect type: folowing the st-graph, a type must be encountered in the union/intersection (this must be a non-empty union/intersection and it must get a name)
                 | BTEqual     -- both sides must have the same type. Note that this is different from adding .=.
                               -- BTEqual requires both sides to be named and equal; this will be tested
                               -- while adding .=. makes both sides equal
                   -- deriving (Ord,Eq)
data BTUOrI = BTUnion | BTIntersection deriving (Ord,Eq)

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

instance Show Type where
    showsPrec _ typTerm = showString (showType typTerm)

showType :: Type -> String
showType t
 = case t of
     TypExpr (Pid _ c) _             -> "pop ("++name c++") "
     TypExpr term@(PVee o) sORt      -> codOrDom sORt++" ("++showADL term++") "++"("++ show o++")"
     TypExpr term@(Pfull _ _ _) sORt -> codOrDom sORt++" ("++showADL term++")"
     TypExpr term sORt               -> codOrDom sORt++" ("++showADL term++") "++ show (origin term)
     TypInCps term                   -> "Arising inside ("++showADL term++") "++show (origin term)
     TypInObjDef obj                 -> "Arising inside object ("++name obj++") " ++ show (origin obj)
   where codOrDom Src = "dom"
         codOrDom Tgt = "cod"

-- | Equality of Type is needed for the purpose of making graphs of types.
--   These are used in the type checking process.
--   The main idea is that the equality distinguishes between occurrences.
--   So term 'r' on line 14:3 differs from  the term 'r' on line 87:19.
--   However, different occurrences of specific terms that are fully typed (e.g. I[Person] or parent[Person*Person]), need not be distinguised.
instance Prelude.Ord Type where -- first we fix all forms of I's, which satisfy r = r~.
  compare (TypExpr (Pid _ c)      _) (TypExpr (Pid _ c')       _) = Prelude.compare c c'
  compare (TypExpr (Pid _ _)      _) (TypExpr _                _) = Prelude.LT
  compare (TypExpr _              _) (TypExpr (Pid _ _ )       _) = Prelude.GT
  compare (TypExpr (Patm _ x [c]) _) (TypExpr (Patm _ x' [c']) _) = Prelude.compare (x,c) (x',c')
  compare (TypExpr (Patm _ _ [_]) _) (TypExpr (Patm _ _   _  ) _) = Prelude.LT
  compare (TypExpr (Patm _ _  _ ) _) (TypExpr (Patm _ _  [_ ]) _) = Prelude.GT
  compare (TypExpr (Patm o x [] ) _) (TypExpr (Patm o' x' [] ) _) = Prelude.compare (o,x) (o',x')
  compare (TypExpr (Patm _ _ _  ) _) (TypExpr (Patm _  _  _  ) _) = fatal 76 "Patm should not have two types"
  compare (TypExpr (Patm _ _ _  ) _) (TypExpr _                _) = Prelude.LT
  compare (TypExpr _              _) (TypExpr (Patm _ _ _)     _) = Prelude.GT
  -- note that V = V~ does not hold in general
  compare (TypExpr (PVee o)       x) (TypExpr (PVee o')       x') = Prelude.compare (o,x) (o',x') -- This is a V of which the type must be determined (by the environment).
  compare (TypExpr (PVee _)       _) (TypExpr _                _) = Prelude.LT
  compare (TypExpr _              _) (TypExpr (PVee _)         _) = Prelude.GT
  -- here we implement V[A*B] = V[B*A]~ directly in the TypExpr
  compare (TypExpr (Pfull _ s t)  x) (TypExpr (Pfull _ s' t') x') = Prelude.compare (if x==Src then (s,t) else (t,s)) (if x'==Src then (s',t') else (t',s')) -- This is a V of which the type is determined by the user
  compare (TypExpr (Pfull _ _ _)  _) (TypExpr _                _) = Prelude.LT
  compare (TypExpr _              _) (TypExpr (Pfull _ _ _)    _) = Prelude.GT
  -- as r = r~ does not hold in general, we need to compare x'==y'
  compare (TypExpr x             x') (TypExpr y               y') = Prelude.compare (x,x') (y,y')
  compare (TypInObjDef _)            (TypInCps _)                 = Prelude.LT
  compare (TypInCps _)               (TypInObjDef _)              = Prelude.GT
  compare (TypInObjDef o)            (TypInObjDef o')             = Prelude.compare (origin o) (origin o')
  compare (TypExpr _              _) _                            = Prelude.LT
  compare _                          (TypExpr _                _) = Prelude.GT
  compare (TypInCps t)               (TypInCps t')                = compare (origin t) (origin t')
  -- since the first argument of Between is a function, we cannot compare it.
  -- Besides, if there are two identical type inferences with different error messages, we should just pick one.
  -- compare (Between _ a b t) (Between _ a' b' t')                  = compare (t,a,b) (t',a',b')

instance Eq Type where
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
p_flp a@PI{}       = a
p_flp a@Pid{}      = a
p_flp a@Patm{}     = a
-- p_flp a@(PVee _)   = PFlp a -- This was earlier: a, which is a mistake. (V[A*B])~ = V[B*A])
p_flp (Pfull o s t)  = Pfull o t s
p_flp (PFlp _ a)   = a
p_flp a            = PFlp (origin a) a

decToTyp :: SrcOrTgt -> P_Declaration -> Type
decToTyp b d = TypExpr (PTrel (origin d) (dec_nm d) (dec_sign d)) b

improveBindings :: (Ord a,Show a,Ord b,Show b)
                => (Map a [b] -> Map Type [(a,b,Type)])
                -> [[Type]] -- equality classes for Between-like bindings yield improved propagation on (I /\ I /\ I);I-like terms.
                -> (Map a [b], Map Type [Type])
                -> (Map a [b], Map Type [Type])
improveBindings typByTyp eqtyps (oldMap,st')
 = (bindings', stPlus)
  where bindings' = Map.union decisions oldMap
        bindings = Map.mapMaybe checkOne bindings' -- these are final: add them to st'
        expanded = Map.fromListWith mrgUnion [(t1,[t2 | (_,_,t2)<-r]) | (t1,r)<-Map.toList (typByTyp bindings)]
        decisions = Map.filter (not.null) $ makeDecisions typByTyp' st' eqtyps
        stPlus = setClosure (Map.unionWith mrgUnion st' (symClosure (expanded))) "(st' \\/ bindings')*"
        typByTyp' = typByTyp oldMap
        checkOne [c] = Just [c]
        checkOne _ = Nothing

-- find out which bindings can be determined.
-- candidate bindings are given in the first argument (inp), where the triple (from,to,Type) is used to bind "from" to "to"
-- (this results in a map of possible bindings)
-- Note that it is possible that an element of type "from" is not in the final map (hence totality of the resulting map is not guaranteed)
makeDecisions :: (Ord from,Ord to,Show to,Show from) =>
                    Map Type [(from,to,Type)] -- when binding "from" to "to", one knows that the first type equals the second
                 -> Map Type [Type] -- reflexive transitive graph with inferred types
                 -> [[Type]] -- classes of types that - according to Between-like bindings - should be of equal types
                 -> Map from [to] -- resulting bindings
makeDecisions inp trnRel eqtyps
 = foldl (Map.unionWith mrgIntersect) Map.empty [ Map.filter (not . null) d
                                                | eqs <- eqtyps
                                                , let d = (getDecision eqs)
                                                ]
 where inpTrnRel = composeMaps trnRel inp
       typsFull = Map.unionWith mrgUnion trnRel
                    (Map.map (getConcsFromTriples) inpTrnRel)
       getConcsFromTriples [] = []
       getConcsFromTriples ((_,_,x):xs) = mrgUnion (Map.findWithDefault [] x trnRel') (getConcsFromTriples xs)
       trnRel' = Map.map (filter isConc) trnRel
       isConc (TypExpr (Pid _ _) _) = True
       isConc _ = False
       f :: Type -> [Type]
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
--   As a result, it builds a list of tuples st::[(Type,Type)], which represents a relation, st,  over Type*Type.
--   For any two Terms a and b,  if 'typing' can tell that dom(a) is a subset of dom(b),
--   this is represented by a tuple (TypExpr a _,TypExpr b _) in st.
--   In the code below, this shows up as  dom a.<.dom b
--   The function typing does a recursive scan through all subterms, collecting all tuples on its way.
--   Besides term term, this function requires a universe in which to operate.
--   If the source and target of term is restricted to concepts c and d, specify (thing c) (thing d).

typing :: Typemap -> [Between] -> Map String [P_Declaration]
          -> ( Typemap                    -- st               -- the st relation: 'a st b' means that  'dom a' is a subset of 'dom b'
             , Typemap                    -- stClos           -- (st\/stAdded)*\/I  (reflexive and transitive)
             , Typemap                    -- eqType           -- (st*/\st*~)\/I  (reflexive, symmetric and transitive)
             , Typemap                    -- stClos           -- additional links added to stClos
             , Typemap                    -- stClos0          -- st* plus bindings for terms (transitive)
             , Guarded ( Map Term P_Declaration -- bindings   -- declarations that may be bound to relations
                       , Type -> P_Concept)     -- srcTypes   -- types of terms and betweens
             , Map P_Concept [P_Concept]  -- isaClos          -- concept lattice
             , Map P_Concept [P_Concept]  -- isaClosReversed  -- same, but reversed
             )                                   
typing st betweenTerms declsByName
  = ( st
    , stClos
    , eqType
    , stClos
    , stClos0
    , do _ <- checkUndefined  -- relation for which there is no declaration
         _ <- checkBindings   -- unresolved bindings for relations
         _ <- checkIVBindings -- unresolved bindings for I and V
         _ <- checkBetweens   -- errors in matching operations such as ;
         return ( bindings, srcTypes )
  -- isas is produced for the sake of making a partial order of concepts in the A-structure.
    , isaClos
    , isaClosReversed   -- a list containing all tuples of concepts that are in the subset relation with each other.
             -- it is used for the purpose of making a poset of concepts in the A-structure.
    ) 
 where
   -- The story: two Typemaps are made by uType, each of which contains tuples of the relation st.
   --            These are converted into two maps (each of type Typemap) for efficiency reasons.
   
    allIVs     = nub' (sort [e | (TypExpr e _)<-typeTerms, isIV e])
    isIV   (PI _)       = True
    isIV   (PVee _)     = True
    isIV   (Patm _ _ _) = True
    isIV   _            = False
    allTerms    = [e | TypExpr e _ <- typeTerms]
    allConcs    = [c | (Pid _ c) <- allTerms]
    
    checkUndefined = parallelList (map checkNonempty (Map.toList declByTerm))
    checkNonempty (t,[]) = Errors [CxeRelUndefined { cxeExpr = t}]
    checkNonempty _ = return ()
    checkBindings = parallelList (map checkUnique (Map.toList newBindings))
    checkUnique (_,[_]) = return ()
    checkUnique (t,xs) = Errors [CxeRel { cxeExpr=t
                                        , cxeDecs=xs
                                        , cxeSNDs=Map.findWithDefault [] t declByTerm
                                        }]
                                        
    -- check that all I's and V's have types. If not, throw an error where V's are replaced for Cpl if they occur in it
    checkIVBindings = parallelList (map checkUnique2 allIVs)
    checkUnique2 iv = case ( Map.findWithDefault [] (TypExpr iv Src) ivBoundConcepts
                           , Map.findWithDefault [] (TypExpr iv Tgt) ivBoundConcepts) of
                        ([_],[_]) -> return ()
                        (xs,ys) -> Errors [CxeSign {cxeExpr=fromVtoCpl iv, cxeSrcs=xs, cxeTrgs=ys}]
    
    fromVtoCpl v@(PVee o) = head ([t | t@(PCpl o' _) <- allTerms, o==o'] ++ [v])
    fromVtoCpl x = x
    
    checkBetweens = parallelList (map checkBetween betweenTerms)
    checkBetween (Between e src trg BTEqual) -- since the BTEqual does not participate in stClosAdd, it will be isolated here
     = case (srcTypes' src,srcTypes' trg) of
              ([a],[b]) -> if a==b then
                             return ()
                           else Errors [e [a] [b]]
              (a,b) -> Errors [e a b]
    checkBetween (Between e src trg (BetweenType _ t))
     = case srcTypes' t of
        [_] -> return ()
        _ -> Errors [e (srcTypes' src) (srcTypes' trg)]
    
    stClosAdded :: Typemap
    stClosAdded = fixPoint stClosAdd (addIdentity stClos1)

 -- The purpose of stClosAdd is to enhance a type map with the edges from Between nodes.
    stClosAdd :: Typemap -> Typemap
    stClosAdd tm = reverseMap (foldl f (reverseMap (foldl f tm glbs)) lubs)
      where
       f :: Typemap -> (Type,Type,Type) -> Typemap
       f dataMap (a, b, t) = Map.map (\cs -> mrgUnion cs [e | a `elem` cs, b `elem` cs, e<-lookups t dataMap]) dataMap
       -- We add arcs for the TypLubs, which means: if x .<. a  and x .<. b, then x .<. (a ./\. b), so we add this arc
       -- (These arcs show up as dotted lines in the type graphs)
       lubs = [(a,b,t) | (Between _ a b (BetweenType BTUnion t)) <- betweenTerms]
       glbs = [(a,b,t) | (Between _ a b (BetweenType BTIntersection t)) <- betweenTerms]
    
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
    
    ivTypByTyp :: Map Type [P_Concept] -> Map Type [(Type,P_Concept,Type)]
    ivTypByTyp ivMap = Map.fromListWith mrgUnion [ (tp,map (\c -> (tp,c,TypExpr (Pid (SomewhereNear (fatal 313 "Hopefully this isn't inspected")) c) Src)) concs)
                                                 | (tp,concs) <- Map.toList ivMap ]
    
    typByTyp :: Map Term [P_Declaration] -> Map Type [(Term,P_Declaration,Type)]
    typByTyp oldMap = Map.fromList [ (trm, triples b t) | trm@(TypExpr t b) <- typeTerms ]
         where triples b t = sort [(t,d,decToTyp b d ) | d <- Map.findWithDefault [] t oldMap]

    firstClos = setClosure (addIdentity st) "(st \\/ I)*"
    firstClosSym = Map.intersectionWith mrgIntersect firstClos (reverseMap firstClos)

    (newBindings,stClos0) = fixPoint (improveBindings typByTyp eqtyps)
                                     (declByTerm,firstClos)
    bindings :: Map Term P_Declaration
    bindings = Map.mapMaybe exactlyOne newBindings
    ivBoundConcepts :: Map Type [P_Concept]
    (ivBoundConcepts, stClos1)
      = fixPoint (improveBindings ivTypByTyp eqtyps) ( Map.fromList [(iv,allConcs) | iv' <- allIVs, iv <- ivToTyps iv']
                                                     , fixPoint stClosAdd stClos0)
    ivToTyps o = nub' [TypExpr o Src, TypExpr o Tgt]
    betweensAsMap = (Map.fromListWith mrgUnion [ (case thrd of {(BetweenType _ t) -> t;_ -> rhs}
                                                 , nub'$ sort [rhs,lhs]) | (Between _ lhs rhs thrd) <- betweenTerms ])
    
    eqtyps' = setClosure (Map.unionWith mrgUnion firstClosSym (symClosure betweensAsMap))
                         "between types"
    eqtyps = Map.elems (Map.filterWithKey (\x y -> x == head y) eqtyps')
    

    exactlyOne [x] = Just x
    exactlyOne _ = Nothing
    
    -- together, the firstSetOfEdges and secondSetOfEdges form the relation st
    typeTerms :: [Type]          -- The list of all type terms in st.
    typeTerms = Map.keys st -- Because a Typemap is total, it is sufficient to compute  Map.keys st
    
    fixPoint :: Eq a => (a->a) -> a -> a
    fixPoint f a = if a==b then a else fixPoint f b
      where b = f a
    
    -- stClos :: Typemap -- ^ represents the transitive closure of stClosAdded.
    -- Check whether stClosAdded is transitive and reflexive...
    stClos = if stClosAdded == addIdentity (setClosure stClosAdded "stClosAdded") then stClosAdded else
                fatal 358 "stClosAdded should be transitive and reflexive"  -- stClos = stClosAdded*\/I, so stClos is transitive (due to setClosure) and reflexive.
    stClosReversed = reverseMap stClos  -- stClosReversed is transitive too and like stClos, I is a subset of stClosReversed.
    eqType = Map.intersectionWith mrgIntersect stClos stClosReversed  -- eqType = stAdded* /\ stAdded*~ i.e there exists a path from a to be and a path from b.
    isaClos :: Map P_Concept [P_Concept]
    isaClos' = Map.fromDistinctAscList [(c,[c' | TypExpr (Pid _ c') _<-ts,c'/=c]) | (TypExpr (Pid _ c) _, ts)<-Map.toAscList stClos]
    isaClosReversed :: Map P_Concept [P_Concept]
    isaClosReversed = reverseMap isaClos
    isaClos = addIdentity isaClos' 
    stConcepts :: Map Type [P_Concept]
    stConcepts =  Map.map f stClos
                  where f :: [Type] -> [P_Concept]
                        f ts = [t | t <- ownTypes ts, not (t `elem` derived ts)]
                        ownTypes ts = [c | TypExpr (Pid _ c) _<-ts]
                        derived ts = foldl mrgUnion [] [Map.findWithDefault [] t isaClos' | t<-ownTypes ts]
    srcTypes' :: Type -> [P_Concept]
    srcTypes' typ = case Map.lookup typ stConcepts of
                      Just x -> x
                      _ -> fatal 447 ("Type "++show typ++" was not found in stConcepts.")
    srcTypes :: Type -> P_Concept
    srcTypes typ = case srcTypes' typ of
                   -- A type may have an empty codomain in stConcepts, because that means it is type incorrect.
                    [cs] -> cs
                    _ -> fatal 446 ("Type "++show typ++" was found in stConcepts, but not a singleton.")
     
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
symClosure m = Map.unionWith mrgUnion m (reverseMap m)

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


-- | lookups is the reflexive closure of findIn. lookups(a,R) = findIn(a,R\/I) where a is an element and R is a relation.
lookups :: (Show a,Ord a) => a -> Map a [a] -> [a]
lookups o q = head ([mrgUnion [o] e | Just e<-[Map.lookup o q]]++[[o]])  

-- | findIn(k,R) yields all l such that: k R l.
findIn :: Ord k => k -> Map k [a] -> [a]
findIn = Map.findWithDefault []  
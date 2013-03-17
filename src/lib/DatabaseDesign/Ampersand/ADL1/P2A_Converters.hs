﻿{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
{-# LANGUAGE RelaxedPolyRec #-} -- RelaxedPolyRec required for OpenSuse, for as long as we@OpenUniversityNL use an older GHC
module DatabaseDesign.Ampersand.ADL1.P2A_Converters (
     -- * Exported functions
     pCtx2aCtx,
     Guarded(..)
     )
where
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree hiding (sortWith, maxima, greatest)
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Basics (name, isc, uni, eqCl, eqClass, getCycles, (>-), fatalMsg, flp)
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Misc
-- import DatabaseDesign.Ampersand.Fspec.Fspec
import DatabaseDesign.Ampersand.Fspec.ShowADL
import qualified DatabaseDesign.Ampersand.Core.Poset as Poset hiding (sortWith)
import GHC.Exts (sortWith)
import Prelude -- hiding (Ord(..))
import DatabaseDesign.Ampersand.Input.ADL1.CtxError
import Data.GraphViz hiding (addExtension, C)
import Data.GraphViz.Attributes.Complete hiding (Box, Pos)
import Data.Maybe
import Data.List
import Data.Char
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map hiding (Map)

-- TODO: this module should import Database.Ampersand.Core.ParseTree directly, and it should be one 
--       of the very few modules that imports it. (might require some refactoring due to shared stuff)

fatal :: Int -> String -> a
fatal = fatalMsg "ADL1.P2A_Converters"

{-The data structure Type is used to represent a term inside the type checker.
TypExpr e flipped is read as:
  "the source type of e, with e equal to (if flipped then PFlp origExpr else origExpr), and origExpr occurs in the P_Context p_context."
origExpr (in conjunction with Origin o) is kept for the purpose of generating messages in terms of the original term written by the user.
TypGlb a b e is read as:
  "the least upper bound of types a and b, in the context of term e.
-}
data Type =  TypExpr Term Bool       -- term is deriving Ord
           | TypLub  Type Type Term  -- a term is smaller if it represents a smaller set
           | TypGlb  Type Type Term  -- a term is larger if it represents a larger set
           | Anything
           | Nothng
            -- note: do not put "deriving Ord", because Eq is specified (and not derived)

-- | We create a normal form for lubs and glbs, in order to get comparable type expressions.
normalType :: Type -> Type
normalType (TypGlb Anything v _) = normalType v
normalType (TypGlb v Anything _) = normalType v
normalType (TypGlb Nothng _ _)   = Nothng
normalType (TypGlb _ Nothng _)   = Nothng
normalType t@(TypGlb u v e) = if u<=v then t else TypGlb v u e
normalType (TypLub Anything _ _) = Anything
normalType (TypLub _ Anything _) = Anything
normalType (TypLub Nothng v _)   = normalType v
normalType (TypLub v Nothng _)   = normalType v
normalType t@(TypLub u v e) = if u<=v then t else TypLub v u e
normalType t = t

instance Show Type where
    showsPrec _ typTerm = showString (showType typTerm)

showType :: Type -> String
showType (TypExpr term@(Pid _) _)     = showADL term
showType (TypExpr term@(PVee o) _)    = showADL term     ++"("++ shOrig o++")"
showType (TypExpr term@(Pfull _ _) _) = showADL term
showType (TypExpr term _)             = showADL term     ++"("++ shOrig (origin term)++")"
showType (TypLub a b _)               = showType a++" .\\/. "++showType b  -- The Lub is the smallest set in which both a and b are contained.
showType (TypGlb a b _)               = showType a++" ./\\. "++showType b  -- The Glb is the largest set that a and b have in common
showType Anything                     = "Anything"
showType Nothng                       = "Nothing"


-- | Equality of Type is needed for the purpose of making graphs of types.
--   These are used in the type checking process.
--   The main idea is that the equality distinguishes between occurrences.
--   So term 'r' on line 14:3 differs from  the term 'r' on line 87:19.
--   However, different occurrences of specific terms that are fully typed (e.g. I[Person] or parent[Person*Person]), need not be distinguised.
instance Prelude.Ord Type where
  compare Anything                   Anything                     = Prelude.EQ
  compare Anything                   _                            = Prelude.LT  -- Anything  < everything
  compare _                          Anything                     = Prelude.GT  -- everyting > Anything
  compare Nothng                     Nothng                       = Prelude.EQ
  compare Nothng                     _                            = Prelude.LT
  compare _                          Nothng                       = Prelude.GT
  compare (TypExpr (Pid c)        _) (TypExpr (Pid c')         _) = Prelude.compare c c'
  compare (TypExpr (Pid _)        _) (TypExpr _                _) = Prelude.LT
  compare (TypExpr _              _) (TypExpr (Pid _)          _) = Prelude.GT
  compare (TypExpr (Pnid c)       _) (TypExpr (Pnid c')        _) = Prelude.compare c c'
  compare (TypExpr (Pnid _)       _) (TypExpr _                _) = Prelude.LT
  compare (TypExpr _              _) (TypExpr (Pnid _)         _) = Prelude.GT
  compare (TypExpr (Patm _ x [c]) _) (TypExpr (Patm _ x' [c']) _) = Prelude.compare (x,c) (x',c')
  compare (TypExpr (Patm _ _ [_]) _) (TypExpr (Patm _ _   _  ) _) = Prelude.LT
  compare (TypExpr (Patm _ _  _ ) _) (TypExpr (Patm _ _  [_ ]) _) = Prelude.GT
  compare (TypExpr (Patm o x cs)  _) (TypExpr (Patm o' x' cs') _) = Prelude.compare (o,x,cs) (o',x',cs')
  compare (TypExpr (Patm _ _ _)   _) (TypExpr _                _) = Prelude.LT
  compare (TypExpr _              _) (TypExpr (Patm _ _ _)     _) = Prelude.GT
  compare (TypExpr (PVee o)       _) (TypExpr (PVee o')        _) = Prelude.compare o o' -- This is a V of which the type must be determined (by the environment).
  compare (TypExpr (PVee _)       _) (TypExpr _                _) = Prelude.LT
  compare (TypExpr _              _) (TypExpr (PVee _)         _) = Prelude.GT
  compare (TypExpr (Pfull s t)    _) (TypExpr (Pfull s' t')    _) = Prelude.compare (s,t) (s',t') -- This is a V of which the type is determined by the user
  compare (TypExpr (Pfull _ _)    _) (TypExpr _                _) = Prelude.LT
  compare (TypExpr _              _) (TypExpr (Pfull _ _)      _) = Prelude.GT
  compare (TypExpr x              _) (TypExpr y                _) = Prelude.compare x y
  compare (TypExpr _              _) _                            = Prelude.LT
  compare _                          (TypExpr _                _) = Prelude.GT
  compare (TypLub l r             _) (TypLub l' r'             _) = compare (l,r) (l',r')
  compare (TypLub _ _             _) _                            = Prelude.LT
  compare _                          (TypLub _ _               _) = Prelude.GT
  compare (TypGlb l r             _) (TypGlb l' r'             _) = compare (l,r) (l',r')
  -- in order to prevent a warning for overlapping patters, the following are disabled
  -- compare (TypGlb _ _ _) _                = Prelude.LT
  -- compare _              (TypGlb _ _ _)   = Prelude.GT

instance Eq Type where
  t == t' = compare t t' == EQ

-- | p_flp computes the inverse of a Term.
p_flp :: Term -> Term
p_flp a@PI{}       = a
p_flp a@Pid{}      = a
p_flp a@Patm{}     = a
p_flp Pnull        = Pnull
-- p_flp a@(PVee _)   = PFlp a -- This was earlier: a, which is a mistake. (V[A*B])~ = V[B*A])
p_flp (Pfull s t)  = Pfull t s
p_flp (Prel o a)   = Pflp o a
p_flp (Pflp o a)   = Prel o a
p_flp (PFlp _ a)   = a
p_flp a            = PFlp (origin a) a

complement :: Term -> Term
complement (PCpl _ a) = a
complement (Pnid c)   = Pid c
complement a          = PCpl (origin a) a

--cmpl :: Type -> Type
--cmpl (TypExpr e b)  = TypExpr (complement e) b
--cmpl (TypLub a b e) = TypGlb (cmpl a) (cmpl b) (complement e)
--cmpl (TypGlb a b e) = TypLub (cmpl a) (cmpl b) (complement e)
--cmpl Anything       = Nothng
--cmpl Nothng         = Anything

type Typemap = Map Type [Type]

{- The type  Typemap  is used to represent the population of relations r[Type*Type] (in Ampersand's metamodel)
For the following, let m be a Typemap that represents relation r[Type*Type]
Invariants are:
1. m contains all elements of the source of r
         keys m     equals the population of  I [source r], which are all Type object drawn from the script
1a.      keys m     represents    dom r
1b.      m is total (dom m=I[source r])
   By the way, keys m produces the elements in ascending order without duplicates.
2. All elements of the codomain of r are obtained by 'elems'
         concat (Map.elems m)     represents    cod r
3. The map contains sorted lists without duplicates:
         let isSortedAndDistinct (x:y:rest) = if (x<y) then isSortedAndDistinct (y:rest) else False
             isSortedAndDistinct _ = True
         in Map.fold (&&) True (Map isSortedAndDistinct m)
-}

-- | if lst represents a binary relation, then reverseMap lst represents its inverse (viz. flip, wok)
-- | note that the domain must be preserved!
-- | reverseMap r = r~
reverseMap :: (Prelude.Ord a, Show a) => Map a [a] -> Map a [a]
reverseMap lst = (Map.fromListWith mrgUnion (concat [(a,[]):[(b,[a])|b<-bs] | (a,bs)<-Map.toAscList lst]))
-- note: reverseMap is relatively slow, but only needs to be calculated once

-- | addIdentity r = r\/I
addIdentity :: (Prelude.Ord a, Show a) => Map a [a] -> Map a [a]
addIdentity mp = Map.mapWithKey (\k a->mrgUnion a [k]) mp

-- | if lst represents a binary relation r, then reflexiveMap lst represents r/\r~
{-
reflexiveMap :: (Prelude.Ord a, Show a) => Map a [a] -> Map a [a]
reflexiveMap lst = Map.map sort (Map.intersectionWith isc lst (reverseMap lst))
-}

mapIsOk :: (Show a,Ord a) => Map k [a] -> Bool
mapIsOk m = Map.fold (&&) True (Map.map (isSortedAndDistinct . checkRfx) m)
isSortedAndDistinct :: Ord a => [a] -> Bool
isSortedAndDistinct (x:y:rest) = if (x<y) then isSortedAndDistinct (y:rest) else False
isSortedAndDistinct _ = True


{-
Since vertices of type v may be expensive to compare, we first create an isomorphic index map that
has integer indices as its vertices. For this graph we compute the transitive closure, which we map
back onto a graph with the original vertex type v.
-}
setClosure :: (Show v,Ord v) => Map v [v] -> String -> Map v [v]
setClosure graph s =
  let vertexIndexMap = makeVertexIndexMap $ Map.keys graph
      indexGraph = vertexToIndexGraph vertexIndexMap graph
      indexResult = setClosureSlow indexGraph s
      vertexResult = indexGraphToVertexGraph vertexIndexMap indexResult
  in  vertexResult

-- a map function for graphs represented as (Map vertex [vertex])
mapGraph :: (Ord a, Ord b) => (a->b) -> Map a [a] -> Map b [b]
mapGraph f graph = Map.fromList [ (f v, map f vs) | (v,vs) <- Map.toList graph]

makeVertexIndexMap :: (Show v,Ord v) => [v] -> Map v Int
makeVertexIndexMap allVertices = Map.fromList $ zip allVertices [0..]

vertexToIndexGraph :: (Show v,Ord v) => Map v Int -> Map v [v] -> Map Int [Int]
vertexToIndexGraph vertexIndexMap vertexGraph = mapGraph vertexToIndex vertexGraph
 where vertexToIndex v = case Map.lookup v vertexIndexMap of
                         Nothing -> fatal 210 $ "vertexToIndexGraph: vertex "++show v++" not in vertexIndexMap"
                         Just i  -> i
  
indexGraphToVertexGraph :: (Show a,Ord a) => Map a Int -> Map Int [Int] -> Map a [a]
indexGraphToVertexGraph vertexIndexMap indexGraph = mapGraph indexToVertex indexGraph
 where indexToVertex i = if i < length allVertices 
                         then allVertices !! i 
                         else fatal 217 $ "indexToVertexGraph: index "++show i++" too large (number of vertices is " ++show (length allVertices)++")"
       allVertices = Map.keys vertexIndexMap

-- | The purpose of 'setClosureSlow' is to compute the transitive closure of relations that are represented as a Map (Map a [a]).
--   For that purpose we use a Warshall algorithm.
setClosureSlow :: (Show a,Ord a) => Map a [a] -> String -> Map a [a]
setClosureSlow xs s | not (mapIsOk xs) = fatal 144 ("setClosure on the non-ok set "++s)
setClosureSlow xs _ = if (mapIsOk res) then res else fatal 145 ("setClosure contains errors!")
  where
--   f q x = Map.map (\bs->foldl mrgUnion bs [b' | b<-bs, b == x, (a', b') <- Map.toList q, a' == x]) q
   f q x = Map.map (\bs->foldl mrgUnion bs [b' | x `elem` bs, Just b' <- [Map.lookup x q]]) q
   res   = foldl f xs (Map.keys xs `isc` nub (concat (Map.elems xs)))

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

{- The following mrgUnion and mrgIntersect are for debug purposes
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

distinctCons :: (Ord a, Eq a, Show a) => a -> a -> [a] -> [a]
distinctCons a b' (b:bs) = if a<b then b':(b:bs)
                           else if a==b then fatal 164 ("Eq is not transitive:\n "++show a++"=="++show b++"\n but `==` ("++show b'++") ("++show b++") is "++show (b' == b))
                           else fatal 167 (concat (["Ord is not transitive:\n "
                                                   ,"compare ("++show a++") ("++show b'++") == "++show (compare a b')++"\n"
                                                   ,"compare ("++show b'++") ("++show b++") == "++show (compare b' b)++"\n"
                                                   ,"compare ("++show a++") ("++show b++") == "++show (compare a b)++"\n"]))
distinctCons a _ bs = a:bs
-}

checkRfx :: (Eq a, Show a) => [a] -> [a]
checkRfx (a:as) = if not (a==a) then fatal 192 ("Eq is not reflexive on "++show a) else a:checkRfx as
checkRfx [] = []        

-- | lookups is the reflexive closure of findIn. lookups(a,R) = findIn(a,R\/I) where a is an element and R is a relation.
lookups :: (Show a,Ord a) => a -> Map a [a] -> [a]
lookups o q = head ([mrgUnion [o] e | Just e<-[Map.lookup o q]]++[[o]])
-- To Stef: lookups was not designed in this way: once upon a time it returned findIn unless the element did not exist.
-- This is a fundamental difference and wherever lookups was used, there might be bugs now.
{- Trying to understand lookups:
lookups "2" [("1",["aap","noot","mies"]), ("2",["vuur","mus"])]
= 
head ([mrgUnion [o] e | (Just e)<-[Map.lookup "2" [("1",["aap","noot","mies"]), ("2",["vuur","mus"])]]]++[["2"]])
= 
head ([mrgUnion ["2"] e | (Just e)<-[Just ["vuur","mus"]]]++[["2"]])
= 
head ([mrgUnion ["2"] ["vuur","mus"] ]++[["2"]])
= 
head (["2", "vuur","mus"]++[["2"]])
= 
["2", "vuur","mus"]
-}
-- | findIn(k,R) yields all l such that: k R l.
findIn :: Ord k => k -> Map k [a] -> [a]
findIn t cl = getList (Map.lookup t cl)
                 where getList Nothing = []
                       getList (Just a) = a


nothing :: (Typemap,Typemap)
nothing = (Map.empty,Map.empty)
{-
isFull (TypExpr (Pfull _ _) _) = True
isFull (TypLub a b _) = isFull a || isFull b
isFull (TypGlb a b _) = isFull a && isFull b
isFull _ = False -}
isNull :: Type -> Bool
isNull (TypExpr Pnull _) = True
isNull (TypLub a b _) = isNull a && isNull b
isNull (TypGlb a b _) = isNull a || isNull b
isNull _ = False
infixl 2 .+.   -- concatenate two lists of types
infixl 3 .<.   -- makes a list of one tuple (t,t'), meaning that t is a subset of t'
infixl 3 .=.   -- makes a list of two tuples (t,t') and (t',t), meaning that t is equal to t'
(.<.) :: Type -> Type -> (Typemap , Typemap)
_ .<. Anything = nothing
Nothng .<. _   = nothing
a .<. _ | isNull a = nothing
a .<. b  = (Map.fromList [(a, [b]),(b, [])],snd nothing) -- a tuple meaning that a is a subset of b, and introducing b as a key.
(.=.) :: Type -> Type -> (Typemap, Typemap)
a .=. b  = (Map.fromList [(a, [b]),(b, [a])],snd nothing)
(.++.) :: Typemap -> Typemap -> Typemap
m1 .++. m2  = Map.unionWith mrgUnion m1 m2
(.+.) :: (Typemap , Typemap) -> (Typemap , Typemap) -> (Typemap, Typemap)
(a,b) .+. (c,d) = (c.++.a,d.++.b)
thing :: P_Concept -> Type
thing c  = TypExpr (Pid c) False
dom, cod :: Term -> Type
dom x    = TypExpr x         False -- the domain of x, and make sure to check subterms of x as well
cod x    = TypExpr (p_flp x) True 
mSpecific, mGeneric :: Type -> Type -> Term -> ( (Typemap , Typemap) ,Type)
mGeneric  a b e = (a .<. r .+. b .<. r , r) where r = normalType (TypLub a b e)
mSpecific a b e = (r .<. a .+. r .<. b , r) where r = normalType (TypGlb a b e)

flattenMap :: Map t [t1] -> [(t, t1)]
flattenMap = Map.foldWithKey (\s ts o -> o ++ [(s,t)|t<-ts]) []
-- alternatively: flattenMap mp = [ (a,b) | (a,bs)<-Map.toList mp , b<-bs])

-- | The purpose of 'typing' is to analyse the domains and codomains of a term in a context.
--   As a result, it builds a list of tuples st::[(Type,Type)], which represents a relation, st,  over Type*Type.
--   For any two Terms a and b,  if 'typing' can tell that dom(a) is a subset of dom(b),
--   this is represented by a tuple (TypExpr a _,TypExpr b _) in st.
--   In the code below, this shows up as  dom a.<.dom b
--   The function typing does a recursive scan through all subterms, collecting all tuples on its way.
--   Besides term term, this function requires a universe in which to operate.
--   Specify 'Anything Anything' if there are no restrictions.
--   If the source and target of term is restricted to concepts c and d, specify (thing c) (thing d).

typing :: P_Context -> ( Typemap                    -- st          -- the st relation: 'a st b' means that  'dom a' is a subset of 'dom b'
                       , Typemap                    -- stClos      -- (st\/stAdded)*\/I  (reflexive and transitive)
                       , Typemap                    -- eqType      -- (st*/\st*~)\/I  (reflexive, symmetric and transitive)
                       , Typemap                    -- stClosAdded -- additional links added to stClos
                       , Typemap                    -- stClos1     -- st*  (transitive)
                       , Map Type [P_Declaration]   -- bindings    -- declarations that may be bound to relations
                       , Type -> [P_Concept]        -- srcTypes -- 
                       , Map P_Concept [P_Concept]  -- isaClos          -- 
                       , Map P_Concept [P_Concept]  -- isaClosReversed  -- 
                       )                                   
typing p_context
  = ( st
    , stClos
    , eqType
    , stClosAdded
    , stClos1
    , bindings -- for debugging use: (error.concat) ["\n  "++show b | b<-Map.toAscList bindings] -- 
    , srcTypes
  -- isas is produced for the sake of making a partial order of concepts in the A-structure.
    , isaClos, isaClosReversed   -- a list containing all tuples of concepts that are in the subset relation with each other.
             -- it is used for the purpose of making a poset of concepts in the A-structure.
    ) 
 where
   -- The story: two Typemaps are made by uType, each of which contains tuples of the relation st.
   --            These are converted into two maps (each of type Typemap) for efficiency reasons.
     (firstSetOfEdges,secondSetOfEdges)
      = uType (p_context, compat) p_context Anything Anything p_context
     compat :: Type -> Type -> Bool
     compat a b = null set1 -- approve compatibility for isolated types, such as "Nothng", "Anything" and singleton elements
                      || null set2
                      || (not.null) (set1 `isc` set2)
      where -- TODO: we can calculate scc with a Dijkstra algorithm, which is much faster than the current closure used:
       included = addIdentity (setClosure (reverseMap firstSetOfEdges) "firstSetOfEdges~")
       set1 = findIn a included
       set2 = findIn b included
   -- together, the firstSetOfEdges and secondSetOfEdges form the relation st
     st = Map.unionWith mrgUnion firstSetOfEdges secondSetOfEdges
     typeTerms :: [Type]          -- The list of all type terms in st.
     typeTerms = Map.keys st -- Because a Typemap is total, it is sufficient to compute  Map.keys st
-- In order to compute the condensed graph, we need the transitive closure of st:
     stClos1 :: Typemap
     stClos1 = setClosure st "st"       -- represents (st)* .   stClos1 is transitive
     stClosAdded :: Typemap
     stClosAdded = fixPoint stClosAdd (addIdentity stClos1)
     fixPoint :: Eq a => (a->a) -> a -> a
     fixPoint f a = if a==b then a else fixPoint f b
       where b = f a
     stClosAdd :: Typemap -> Typemap
     stClosAdd tm = reverseMap (foldl f' (reverseMap (foldl f tm someWhatSortedGlbs)) (reverse someWhatSortedLubs))
       where
        f :: Typemap -> Type -> Typemap
        f dataMap o@(TypGlb a b _) = Map.map (\cs -> mrgUnion cs [e | a `elem` cs, b `elem` cs, e<-lookups o dataMap]) dataMap
        --        f dataMap o@(TypGlb a b _) = Map.map (\cs -> mrgUnion cs [o| a `elem` cs, b `elem` cs]) dataMap
        f  _ o = fatal 406 ("Inexhaustive pattern in f in stClosAdded in tableOfTypes: " ++show o)
        f' dataMap o@(TypLub a b _) = Map.map (\cs -> mrgUnion cs [e | a `elem` cs, b `elem` cs, e<-lookups o dataMap]) dataMap
        f' _ o = fatal 407 ("Inexhaustive pattern in f' in stClosAdded in tableOfTypes: "++show o)
        -- We add arcs for the TypLubs, which means: if x .<. a  and x .<. b, then x .<. (a ./\. b), so we add this arc
        -- This explains why we did the sorting:
        --    after deducing x .<. (a ./\. b), we might deduce x .<. c, and then x .<. ((a ./\. b) ./\. c)
        --    should we do the deduction about ((a ./\. b) ./\. c) before that of (a ./\. b), we would miss this
        -- We filter for TypLubs, since we do not wish to create a new TypExpr for (a ./\. b) if it was not already in the st-graph
        someWhatSortedLubs = sortBy compr [l | l@(TypLub _ _ _) <- typeTerms]
        someWhatSortedGlbs = sortBy compr [l | l@(TypGlb _ _ _) <- typeTerms]
        -- Compr uses the stClos1 to decide how (a ./\. b) and ((a ./\. b) ./\. c) should be sorted
        compr a b  = if b `elem` (lookups a stClos1) then LT else -- note: by LT we mean: less than or equal
                     if a `elem` (lookups b stClos1) then GT -- likewise, GT means: greater or equal
                      else EQ -- and EQ means: don't care about the order (sortBy will preserve the original order as much as possible)

     -- stClos :: Typemap -- ^ represents the transitive closure of stClosAdded.
     stClos = if (stClosAdded == (addIdentity $ setClosure stClosAdded "stClosAdded")) then
                 stClosAdded else fatal 358 "stClosAdded should be transitive and reflexive"  -- stClos = stClosAdded*\/I, so stClos is transitive (due to setClosure) and reflexive.
     stClosReversed = reverseMap stClos  -- stClosReversed is transitive too and like stClos, I is a subset of stClosReversed.
     eqType = Map.intersectionWith mrgIntersect stClos stClosReversed  -- eqType = stAdded* /\ stAdded*~ i.e there exists a path from a to be and a path from b.
     isaClos = Map.fromDistinctAscList [(c,[c' | TypExpr (Pid c') _<-ts]) | (TypExpr (Pid c) _, ts)<-Map.toAscList stClos]
     isaClosReversed = reverseMap isaClos
     stConcepts :: Map Type [P_Concept]
     stConcepts = Map.map f stClos
                  where f ts = [ (fst.head.sortWith (length.snd)) [(c,lkp c) | c<-cl]
                               | cl<-eqClass compatible cs]
                               where cs = [c | TypExpr (Pid c) _<-ts] -- all concepts reachable from one type term
                                     lkp c = case Map.lookup c isaClosReversed of
                                              Just cs' -> cs'
                                              _ -> fatal 387 ("P_Concept "++show c++" was not found in isaClosReversed")
     srcTypes :: Type -> [P_Concept]
     srcTypes typ = case Map.lookup typ stConcepts of
                    -- A type may have an empty codomain in stConcepts, because that means it is type incorrect.
                     Just cs -> cs
                     _ -> fatal 447 ("Type "++show typ++" was not found in stConcepts."++concat ["\n  "++show b | b<-Map.toAscList stConcepts, take 7 (show b)==take 7 (show typ) ])
     compatible a b = (not.null) (lkp a `isc` lkp b)
      where lkp c = case Map.lookup c isaClosReversed of
                     Just cs -> cs
                     _ -> fatal 395 ("P_Concept "++show c++" was not found in isaClosReversed")
{- Bindings:
Relations that are used in a term must be bound to declarations. When they have a type annotation, as in r[A*B], there is no  problem.
When it is just 'r', and there are multiple declarations to which it can be bound, the type checker must choose between candidates.
Sometimes, there is only one candidate, even though the type checker cannot prove it correct (i.e. the domain of the term is a subset of the candidate type).
In such cases, we want to give that candidate to the user by way of suggestion to resolve the type conflict.
-}
     bindings :: Map Type [P_Declaration]
     bindings
      = Map.fromListWith union
        ([ tuple
         | nm<-Map.keys declsByName `isc` Map.keys domsByName `isc` Map.keys codsByName
         , Just domTypes<-[Map.lookup nm domsByName], dType<-domTypes
         , Just codTypes<-[Map.lookup nm codsByName], cType<-codTypes
         , Just dcls<-[Map.lookup nm declsByName], decl<-dcls
         , let P_Sign sgn = dec_sign decl
         , tDecl<-terms decl
         , match dType (dom tDecl) (head sgn), match cType (cod tDecl) (last sgn)
         , tuple<-[(dType,[decl]), (cType,[decl])]
         ] ++
         [ (t,[]) | t@(TypExpr (Prel _ _) _)<-typeTerms] ++
         [ (t,[]) | t@(TypExpr (Pflp _ _) _)<-typeTerms])
       where
        declsByName :: Map String [P_Declaration]
        declsByName = Map.fromList [ (name (head cl), [ decl | decl<-cl]) | cl<-eqCl name (p_declarations p_context) ]
        domsByName :: Map String [Type]
        domsByName = Map.fromList [ (fst (head cl), map snd cl) | cl<-eqCl fst [ (nm,t) | t@(TypExpr (Prel _ nm) _)<-typeTerms] ]
        codsByName :: Map String [Type]
        codsByName = Map.fromList [ (fst (head cl), map snd cl) | cl<-eqCl fst [ (nm,t) | t@(TypExpr (Pflp _ nm) _)<-typeTerms] ]
        {- Each relation must be bound to declarations. 
        The predicate match checks whether a relation x (Prel or Pflp) can be bound to a declaration.
        match x decl
           = x (st* ; I[P_Concept] ; st*~ ; I[P_Concept] ; st* ; I[P_Concept] ; st*~ ) decl
           = x (lookup ; isaClosReversed ; isaClos ; lookup~) decl
        -}
        match :: Type -> Type -> P_Concept -> Bool
        match x decl dConc
           = (not.null) 
             [ () | intersectConcept<-nub [c | Just cs<-[Map.lookup dConc isaClosReversed]
                                             , c<-cs]
                  , Just termTypes<-[Map.lookup intersectConcept isaClos]
                  , not (null (termTypes `isc` srcTypes decl))
                  , not (null (termTypes `isc` srcTypes x))
                  ]
{- legacy code (most likely correct but this code is inefficient)
     bindings :: Map Type [(P_Declaration,[P_Concept],[P_Concept])]
     bindings = (Map.map unDouble . Map.fromListWith union)
                ( [ tuple
                  | decl<-p_declarations p_context
                  , let P_Sign sgn = dec_sign decl; srce=head sgn; targ=last sgn
                  , dTerm@(TypExpr (Prel _ dName) _) <- typeTerms, dName==dec_nm decl
                  , cTerm@(TypExpr (Pflp _ cName) _) <- typeTerms, cName==dec_nm decl
                  , Just dConcepts<-[Map.lookup dTerm stClos], dc<-dConcepts
                  , Just cConcepts<-[Map.lookup cTerm stClos], cc<-cConcepts
                  , let srcConcepts = thing srce `mayShareAtoms` dc, not (null srcConcepts)
                  , let trgConcepts = thing targ `mayShareAtoms` cc, not (null trgConcepts)
                  , let declSrcTrgs = (decl, srcConcepts, trgConcepts)
                  , tuple<-[(dTerm,[declSrcTrgs]),(cTerm,[declSrcTrgs])]
                  ] ++
                  [ (t,[]) | t@(TypExpr (Prel _ _) _)<-typeTerms] ++
                  [ (t,[]) | t@(TypExpr (Pflp _ _) _)<-typeTerms]
                )
       where
      -- Two concepts are compatible if they can share atoms. If there exists a concept that is greater than both, this is obvious.
      -- However, also if there exists a concept that is smaller than both, they can share atoms. So either a glb or a lub is sufficient.
        mayShareAtoms :: Type -> Type -> [P_Concept]
        mayShareAtoms a b = nub [c | t<-greatest (lkp a `isc` lkp b), c<-srcTypes t]
         where lkp c = case Map.lookup c stClosReversed of
                        Just cs -> cs
                        _ -> fatal 400 ("P_Concept "++show c++" was not found in stClosReversed")
        unDouble (tr@(d,_,_):triples) = tr : unDouble [tr' | tr'@(d',_,_)<-triples, d `neq` d']
        unDouble [] = []
        d `neq` d'
         = let P_Sign sgn  = dec_sign d
               P_Sign sgn' = dec_sign d'
           in dec_nm d/=dec_nm d || head sgn/=head sgn' || last sgn/=last sgn'
-}

{- The following function draws two graphs for educational or debugging purposes. If you want to see them, run Ampersand --typing.
-}
typeAnimate :: Typemap -> Typemap -> Typemap -> Typemap -> Typemap -> (DotGraph String,DotGraph String)
typeAnimate st stClos eqType stClosAdded stClos1 = (stTypeGraph, eqTypeGraph)
   where
     -- testTable = concat [ "\n  "++show (stNr t, eqNr t, t, map stNr eqs, map eqNr eqs)| (t,eqs)<-Map.toAscList eqType]
{- The set st contains the essence of the type analysis. It contains tuples (t,t'),
   each of which means that the set of atoms contained by t is a subset of the set of atoms contained by t'. -}
     typeTerms = Map.keys stClos -- stClos contains more than st, because some terms were added through stClosAdded.
     stNr :: Type -> Int
     stNr typ = case Map.lookup typ stTable of
                 Just x -> x
                 _ -> fatal 529 ("Element "++show typ++" not found in stNr")
      where
       stTable = Map.fromAscList [(t,i) | (i,t)<-zip [0..] typeTerms ]
     stTypeGraph :: DotGraph String
     stTypeGraph = toDotGraph showStVertex show [0..length typeTerms-1] [] stEdges []
     stEdges :: [(Int,Int)]
     stEdges = [(i,j) | (s,t) <- flattenMap st, let (i,j)=(stNr s,stNr t), i/=j]
     showStVertex :: Int -> String
     showStVertex i
      = head ([ showType t | (i',t)<-zip [0..] typeTerms, i==i' ]
              ++fatal 506 ("No term numbered "++show i++" found by showStVertex\n numbered typeTerms:\n  "++(intercalate "\n  ".map show. zip [0::Int ..]) typeTerms)
             )
     eqNr :: Type -> Int
     eqNr typ = case Map.lookup typ (Map.fromList [(t,i) | (i,cl)<-zip [0..] eqClasses, t<-cl ]) of
                 Just x -> x
                 _ -> fatal 544 ("Element "++show typ++" not found in eqNr")
     eqClasses :: [[Type]]             -- The strongly connected components of stGraph
     eqClasses = nub (Map.elems eqType)

     eqTypeGraph :: DotGraph String
     eqTypeGraph = toDotGraph showVtx show [0..length eqClasses-1] [] condensedEdges condensedEdges2
      where showVtx n = (intercalate "\n".map showType.nub) [  typTerm| typTerm<-typeTerms, n==eqNr typTerm]
{- This function was used to find the original expression from the user script....
     original :: Type -> Type
     original t@(TypExpr (Pid _)     _) = t
     original t@(TypExpr (Pfull _ _) _) = t
     original (TypExpr t b)
      = case Map.lookup (origin t) origMap of
                   Just term -> TypExpr (if b then p_flp term else term) b
                   Nothing   -> fatal 586 ("wrong argument for original ("++show t++")")
        where origMap = Map.fromList [ (origin subTerm,subTerm) | subTerm<-subterms p_context ]
     original (TypLub a b e) = TypLub (original a) (original b) e
     original (TypGlb a b e) = TypGlb (original a) (original b) e
     original t = t
-}

{- condensedGraph is the condensed graph. Elements that are equal are brought together in the same equivalence class.
   The graph in which equivalence classes are vertices is called the condensed graph.
   These equivalence classes are the strongly connected components of the original graph
-}
     condensedEdges :: [(Int,Int)]
     condensedEdges = nub [ (nr t, nr t') | (t,t')<-flattenMap st, nr t /= nr t' ]
     nr t = case Map.lookup t eqType of
             Just xs -> eqNr (head xs)
             _ -> fatal 571 ("Element "++show t++" not found in nr")
     condensedEdges2 = nub [(nr t,nr t') | (t,t')<-flattenMap stClosAdded, nr t /= nr t', t' `notElem` findIn t stClos1]>-condensedEdges

class Expr a where
  p_gens :: a -> [P_Gen]
  p_gens _ = []
--  p_concs :: a -> [P_Concept]
  p_declarations :: a -> [P_Declaration]
  p_declarations _ = []
  p_rules :: a -> [P_Rule]
  p_rules _ = []
  p_keys :: a -> [P_KeyDef]
  p_keys _ = []
  terms :: a -> [Term]
  subterms :: a -> [Term]
  subterms x = subterms (terms x)
  -- | uType provides the basis for a domain analysis. It traverses an Ampersand script recursively, harvesting on its way
  --   the tuples of a relation st :: Type * Type. Each tuple (TypExpr t, TypExpr t') means that the domain of t is a subset of the domain of t'.
  --   These tuples are produced in two Typemaps. The second Typemap is kept separate, because it depends on the existence of the first Typemap.
  --   The first element of the first argument is a P_Context that represents the parse tree of one context.
  --   This is provided to obtain a declaration table and a list of interfaces from the script.
  --   The second element of the first argument is a compatibility function, that determines whether two types are compatible.
  uType :: (P_Context, Type -> Type -> Bool)
        -> a               -- x:    the original term from the script, meant for representation in the graph.
        -> Type            -- uLft: the type of the universe for the domain of x 
        -> Type            -- uRt:  the type of the universe for the codomain of x
        -> a               -- z:    the term to be analyzed, which must be logically equivalent to x
        -> ( Typemap   -- for each type, a list of types that are subsets of it, which is the result of analysing term x.
           , Typemap ) -- for some edges, we need to know the rest of the graph. These can be created in this second part.

instance Expr P_Context where
 p_gens pContext
  = concat [ p_gens pat | pat<-ctx_pats  pContext] ++
    concat [ p_gens prc | prc<-ctx_PPrcs pContext] ++
    ctx_gs pContext
 p_declarations pContext
  = concat [ p_declarations pat | pat<-ctx_pats  pContext] ++
    concat [ p_declarations prc | prc<-ctx_PPrcs pContext] ++
    ctx_ds pContext
 p_rules pContext
  = concat [ p_rules pat | pat<-ctx_pats  pContext] ++
    concat [ p_rules prc | prc<-ctx_PPrcs pContext] ++
    ctx_rs pContext
 p_keys pContext
  = concat [ p_keys pat | pat<-ctx_pats  pContext] ++
    concat [ p_keys prc | prc<-ctx_PPrcs pContext] ++
    ctx_ks pContext
 terms pContext
  = nub (terms (ctx_pats  pContext) ++
         terms (ctx_PPrcs pContext) ++
         terms (ctx_rs    pContext) ++
         terms (ctx_ds    pContext) ++
         terms (ctx_ks    pContext) ++
         terms (ctx_gs    pContext) ++
         terms (ctx_ifcs  pContext) ++
         terms (ctx_sql   pContext) ++
         terms (ctx_php   pContext) ++
         terms (ctx_pops  pContext)
        )
 uType ctxt _ uLft uRt pContext
  = (let x=ctx_pats  pContext in uType ctxt x uLft uRt x) .+.
    (let x=ctx_PPrcs pContext in uType ctxt x uLft uRt x) .+.
    (let x=ctx_rs    pContext in uType ctxt x uLft uRt x) .+.
    (let x=ctx_ds    pContext in uType ctxt x uLft uRt x) .+.
    (let x=ctx_ks    pContext in uType ctxt x uLft uRt x) .+.
    (let x=ctx_gs    pContext in uType ctxt x uLft uRt x) .+.
    (let x=ctx_ifcs  pContext in uType ctxt x uLft uRt x) .+.
    (let x=ctx_ps    pContext in uType ctxt x uLft uRt x) .+.
    (let x=ctx_sql   pContext in uType ctxt x uLft uRt x) .+.
    (let x=ctx_php   pContext in uType ctxt x uLft uRt x) .+.
    (let x=ctx_pops  pContext in uType ctxt x uLft uRt x)
         
instance Expr P_Pattern where
 p_gens pPattern
  = pt_gns pPattern
 p_declarations pPattern
  = pt_dcs pPattern
 p_rules pPattern
  = pt_rls pPattern
 p_keys pPattern
  = pt_kds pPattern
 terms pPattern
  = nub (terms (pt_rls pPattern) ++
         terms (pt_gns pPattern) ++
         terms (pt_dcs pPattern) ++
         terms (pt_kds pPattern) ++
         terms (pt_xps pPattern) ++
         terms (pt_pop pPattern)
        )
 uType ctxt _ uLft uRt pPattern
  = (let x=pt_rls pPattern in uType ctxt x uLft uRt x) .+.
    (let x=pt_gns pPattern in uType ctxt x uLft uRt x) .+.
    (let x=pt_dcs pPattern in uType ctxt x uLft uRt x) .+.
    (let x=pt_kds pPattern in uType ctxt x uLft uRt x) .+.
    (let x=pt_xps pPattern in uType ctxt x uLft uRt x) .+.
    (let x=pt_pop pPattern in uType ctxt x uLft uRt x)

instance Expr P_Process where
 p_gens pProcess
  = procGens pProcess
 p_declarations pProcess
  = procDcls pProcess
 p_rules pProcess
  = procRules pProcess
 p_keys pProcess
  = procKds pProcess
 terms pProcess
  = nub (terms (procRules pProcess) ++
         terms (procGens  pProcess) ++
         terms (procDcls  pProcess) ++
         terms (procKds   pProcess) ++
         terms (procXps   pProcess) ++
         terms (procPop   pProcess)
        )
 uType ctxt _ uLft uRt pProcess
  = (let x=procRules pProcess in uType ctxt x uLft uRt x) .+.
    (let x=procGens  pProcess in uType ctxt x uLft uRt x) .+.
    (let x=procDcls  pProcess in uType ctxt x uLft uRt x) .+.
    (let x=procKds   pProcess in uType ctxt x uLft uRt x) .+.
    (let x=procXps   pProcess in uType ctxt x uLft uRt x) .+.
    (let x=procPop   pProcess in uType ctxt x uLft uRt x)

instance Expr P_Rule where
 terms r = terms (rr_exp r)++terms (rr_viol r)
 p_rules r = [r]
 uType ctxt _ uLft uRt r
  = let x=rr_exp r; v=rr_viol r in
    uType ctxt x uLft uRt x .+. uType ctxt v (dom x) (cod x) v

instance Expr P_PairView where
 terms (P_PairView segments) = terms segments
 uType ctxt _ uLft uRt (P_PairView segments) = uType ctxt segments uLft uRt segments

instance Expr P_PairViewSegment where
 terms (P_PairViewExp _ term) = [term]
 terms _                      = []
 uType ctxt _ uLft _   (P_PairViewExp Src term) = uType ctxt term t Anything term .+. dm
  where (dm,t) = mSpecific (dom term) uLft term
 uType ctxt _ _    uRt (P_PairViewExp Tgt term) = uType ctxt term t Anything term .+. dm
  where (dm,t) = mSpecific (dom term) uRt term
 uType _ _ _ _ _ = nothing
  
instance Expr P_KeyDef where
 terms k = terms [ks_obj keyExpr | keyExpr@P_KeyExp{} <- kd_ats k]
 p_keys k = [k]
 uType ctxt _ uLft uRt k
  = let x=Pid (kd_cpt k) in
    uType ctxt x uLft uRt x .+.
    foldr (.+.) nothing [ uType ctxt obj t Anything obj .+. dm
                        | P_KeyExp obj <- kd_ats k
                        , let (dm,t) = mSpecific (dom x) (dom (obj_ctx obj)) (obj_ctx obj)
                        ]
    where 

instance Expr P_Interface where
 terms ifc = terms (ifc_Obj ifc)
 uType ctxt _ uLft uRt ifc
  = let x=ifc_Obj ifc in
    uType ctxt x uLft uRt x .+.
    foldr (.+.) nothing [ uType ctxt param uLft uRt param | param<-ifc_Params ifc ]

instance Expr P_ObjectDef where
 terms o = [obj_ctx o | null (terms (obj_msub o))]++terms [PCps (origin e) (obj_ctx o) e | e<-terms (obj_msub o)]
 uType ctxt _ uLft uRt o
  = let x=obj_ctx o in
    uType ctxt x uLft uRt x .+.
    foldr (.+.) nothing [ uType ctxt obj t Anything obj .+. dm
                        | Just subIfc <- [obj_msub o]
                        , obj <- case subIfc of
                                   P_Box{}          -> si_box subIfc
                                   P_InterfaceRef{} -> [ifc_Obj ifc | ifc<-ctx_ifcs (fst ctxt), name ifc==si_str subIfc]
                        , let (dm,t) = mSpecific (cod x) (dom (obj_ctx obj)) (obj_ctx obj)
                        ]
 
instance Expr P_SubInterface where
 terms x@P_Box{} = terms (si_box x)
 terms _           = []
 uType ctxt _ uLft uRt mIfc@P_Box{} = let x=si_box mIfc in uType ctxt x uLft uRt x
 uType _    _ _    _   _              = nothing

instance Expr PPurpose where
 terms _ = []
 uType ctxt _ uLft uRt purp = let x=pexObj purp in uType ctxt x uLft uRt x

instance Expr PRef2Obj where
 terms _ = []
 uType ctxt _ uLft uRt (PRef2ConceptDef str) = let x=Pid (PCpt str) in uType ctxt x uLft uRt x
 uType ctxt _ uLft uRt (PRef2Declaration t)  = uType ctxt t uLft uRt t
 uType _    _ _    _   _                     = nothing

instance Expr P_Sign where
 terms _ = []
 uType _ _ _ _ _ = nothing

instance Expr P_Gen where
 terms g = [Pimp (origin g) (Pid (gen_spc g)) (Pid (gen_gen g))]
 uType ctxt _ uLft uRt g
  = let x=Pimp (origin g) (Pid (gen_spc g)) (Pid (gen_gen g)) in uType ctxt x uLft uRt x

instance Expr P_Declaration where
 terms d = [PTrel (origin d) (dec_nm d) (dec_sign d)]
 uType _ _ _ _ d
  = dom decl.<.thing src .+. cod decl.<.thing trg
    where [decl] = terms d
          P_Sign sgn = dec_sign d
          src = head sgn; trg = last sgn
   -- let x=PTrel (origin d) (dec_nm d) (dec_sign d) in uType ctxt x uLft uRt x

instance Expr P_Population where
 terms pop@(P_RelPopu{p_type=P_Sign []}) = [Prel (p_orig pop) (name pop)]
 terms pop@P_RelPopu{}                   = [PTrel (p_orig pop) (name pop) (p_type pop)]
 terms pop@P_CptPopu{}                   = [Pid (PCpt (name pop))]
 uType ctxt _ uLft uRt pop
  = foldr (.+.) nothing [ uType ctxt x uLft uRt x .+. dom x.=.dom x .+. cod x.=.cod x | x<-terms pop ]
    -- the reason for inserting dom x.=.dom x .+. cod x.=.cod x on this location,
    -- is possibility that a population without type signature might be overlooked by the type checker,
    -- resulting in a fatal 1601, i.e. a term that is not in the TypeMap bindings.

instance Expr a => Expr (Maybe a) where
 terms Nothing  = []
 terms (Just x) = terms x
 uType _    _ _    _   Nothing  = nothing
 uType ctxt _ uLft uRt (Just x) = uType ctxt x uLft uRt x

instance Expr a => Expr [a] where
 terms = concat.map terms
 uType ctxt _ uLft uRt xs = foldr (.+.) nothing [ uType ctxt x uLft uRt x | x <- xs]

instance Expr Term where
 terms e = [e]
 subterms e@(Pequ _ a b) = [e]++subterms a++subterms b
 subterms e@(Pimp _ a b) = [e]++subterms a++subterms b
 subterms e@(PIsc _ a b) = [e]++subterms a++subterms b
 subterms e@(PUni _ a b) = [e]++subterms a++subterms b
 subterms e@(PDif _ a b) = [e]++subterms a++subterms b
 subterms e@(PLrs _ a b) = [e]++subterms a++subterms b
 subterms e@(PRrs _ a b) = [e]++subterms a++subterms b
 subterms e@(PCps _ a b) = [e]++subterms a++subterms b
 subterms e@(PRad _ a b) = [e]++subterms a++subterms b
 subterms e@(PPrd _ a b) = [e]++subterms a++subterms b
 subterms e@(PKl0 _ a)   = [e]++subterms a
 subterms e@(PKl1 _ a)   = [e]++subterms a
 subterms e@(PFlp _ a)   = [e]++subterms a
 subterms e@(PCpl _ a)   = [e]++subterms a
 subterms e@(PBrk _ a)   = [e]++subterms a
 subterms e              = [e]
 
 uType ctxt x uLft uRt expr 
  = case expr of
     Pnid{}       -> fatal 136 "Pnid has no representation"
     PI{}         -> dom x.=.cod x .+. dom x.<.uLft .+. cod x.<.uRt             -- I
     Pid{}        -> dom x.=.cod x                                              -- I[C]
     (Patm _ _ []) -> dom x.=.cod x .+. dom x.<.uLft .+. cod x.<.uRt      -- 'Piet'   (an untyped singleton)
     (Patm _ _ cs) -> dom x.<.thing (head cs) .+. cod x.<.thing (last cs) -- 'Piet'[Persoon]  (a typed singleton)
                       .+. dom x.=.cod x
     Pnull        -> nothing                                                  -- -V     (the empty set)
     PVee{}       -> dom x.<.uLft .+. cod x.<.uRt
     (Pfull s t)  -> dom x.=.dom (Pid s) .+. cod x.=.cod (Pid t)                          --  V[A*B] (the typed full set)
     (Pequ _ a b) -> dom a.=.dom x .+. cod a.=.cod x .+. dom b.=.dom x .+. cod b.=.cod x    --  a=b    equality
                     .+. uType ctxt a (dom x) (cod x) a .+. uType ctxt b (dom x) (cod x) b 
     (PIsc _ a b) -> let (dm,interDom)  = mSpecific (dom a) (dom b)  x
                         (cm,interCod)  = mSpecific (cod a) (cod b)  x
                         (d2,interDom2) = mSpecific interDom uLft  x
                         (c2,interCod2) = mSpecific interCod uRt   x
                     in dom x.=.interDom .+. cod x.=.interCod    --  intersect ( /\ )
                        .+. dom x.<.dom a .+. dom x.<.dom b .+. cod x.<.cod a .+. cod x.<.cod b
                        .+. dm .+. cm .+. d2 .+. c2 -- d2 and c2 are needed for try15
                        .+. uType ctxt a interDom2 interCod2 a .+. uType ctxt b interDom2 interCod2 b
     (PUni _ a b) -> let (dm,interDom) = mGeneric (dom a) (dom b)  x
                         (cm,interCod) = mGeneric (cod a) (cod b)  x
                         (d2,interDom2) = mSpecific interDom uLft  x
                         (c2,interCod2) = mSpecific interCod uRt   x
                     in dom x.=.interDom .+. cod x.=.interCod    --  union     ( \/ )
                        .+. dom a.<.dom x .+. dom b.<.dom x .+. cod a.<.cod x .+. cod b.<.cod x
                        .+. dm .+. cm .+. d2 .+. c2
                        .+. uType ctxt a interDom2 interCod2 a .+. uType ctxt b interDom2 interCod2 b
     (PDif _ a b) -> let (dm,interDom) = mSpecific uLft (dom a) x
                         (cm,interCod) = mSpecific uRt  (cod a) x
                     in dom x.<.dom a .+. cod x.<.cod a                                        --  a-b    (difference)
                        .+. dm .+. cm
                        .+. uType ctxt a uLft uRt a
                        .+. uType ctxt b interDom interCod b
     (PCps _ a b) -> let (bm,between) = mSpecific (cod a) (dom b) x
                         pidTest (PI{}) r = r
                         pidTest (Pid{}) r = r
                         pidTest _ _ = nothing
                     in dom x.<.dom a .+. cod x.<.cod b .+.                                    -- a;b      composition
                        bm .+. uType ctxt a uLft between a .+. uType ctxt b between uRt b
                        .+. pidTest a (dom x.<.dom b) .+. pidTest b (cod x.<.cod a)
-- PRad is the De Morgan dual of PCps. However, since PUni and UIsc, mGeneric and mSpecific are not derived, neither can PRad
     (PRad _ a b) -> let (bm,between) = mGeneric (cod a) (dom b) x
                         pnidTest (PCpl _ (PI{})) r = r
                         pnidTest (PCpl _ (Pid{})) r = r
                         pnidTest (Pnid{}) r = r
                         pnidTest _ _ = nothing
                     in dom a.<.dom x .+. cod b.<.cod x .+.                                    -- a!b      relative addition, dagger
                        bm .+. uType ctxt a uLft between a .+. uType ctxt b between uRt b
                        .+. pnidTest a (dom b.<.dom x) .+. pnidTest b (cod a.<.cod x)
     (PPrd _ a b) -> dom x.=.dom a .+. cod x.=.cod b                                        -- a*b cartesian product
                     .+. uType ctxt a uLft Anything a .+. uType ctxt b Anything uRt b
     (PCpl o a)   -> let c = normalType (TypLub (dom a) (dom t) x)
                         c'= normalType (TypLub (cod a) (cod t) x)
                         t = complement a
                     in dom x.<.c .+. cod x.<.c' .+.
                        dom a.<.c .+. cod a.<.c' .+.
                        uType ctxt x uLft uRt (PDif o (PVee o) a)
     (PKl0 _ e)   -> dom e.<.dom x .+. cod e.<.cod x .+. uType ctxt e uLft uRt e
     (PKl1 _ e)   -> dom e.<.dom x .+. cod e.<.cod x .+. uType ctxt e uLft uRt e
     (PFlp _ e)   -> cod e.=.dom x .+. dom e.=.cod x .+. uType ctxt e uRt uLft e
     (PBrk _ e)   -> dom x.=.dom e .+. cod x.=.cod e .+.
                    uType ctxt x uLft uRt e                                                     -- (e) brackets
     (PTrel _ _ sgn) ->
       case sgn of
         (P_Sign [])        -> fatal 196 "P_Sign is empty"
         (P_Sign (_:_:_:_)) -> fatal 197 "P_Sign too large"  
         (P_Sign cs)        -> let iSrc = thing (head cs)
                                   iTrg = thing (last cs)
                                   x'=complement x
                               in dom x.<.iSrc .+. cod x.<.iTrg .+. dom x'.<.iSrc .+. cod x'.<.iTrg
     (Prel _ nm) -> case decls of
                          [d] -> dom x.=.dom d .+. cod x.=.cod d .+. dom x'.<.dom d .+. cod x'.<.cod d
                          _   -> dom x.<.uLft .+. cod x.<.uRt .+. dom x'.<.uLft .+. cod x'.<.uRt
                    where
                        (p_context, _) = ctxt
                        x'=complement x
                        decls' = [term | decl<-p_declarations p_context, name decl==nm, term<-terms decl ]
                        -- decls looks whether a declaration gives an exact match. We cannot use `compatible`, because that has not been computed at this point.
                        decls  = if length decls' == 1 then decls' else
                                 [decl | decl@(PTrel _ _ (P_Sign cs@(_:_)))<-decls'
                                       , uLft == thing (head cs)
                                       , uRt  == thing (last cs)
                                       ]
                    {-let (p_context, compatible) = ctxt
                        y=complement x
                        
                        spcls  = case (dSrcs, dTrgs) of
                                      ( []  ,  []  ) -> []
                                      ( [d] ,  []  ) -> [d]
                                      ( []  ,  [d] ) -> [d]
                                      ( [d] ,  _   ) -> [d]
                                      ( _   ,  [d] ) -> [d]
                                      ( _   ,  _   ) -> dSrcs `isc` dTrgs

                        dSrcs, dTrgs ::  [Term]
                        dSrcs  = [decl | decl@(PTrel _ _ (P_Sign (_:_)))<-decls'
                                       , compatible uLft (dom decl)   -- this is compatibility wrt firstSetOfEdges, thus avoiding a computational loop
                                       ]
                        dTrgs  = [decl | decl@(PTrel _ _ (P_Sign (_:_)))<-decls'
                                       , compatible uRt  (cod decl)
                                       ]
                     in -- WHY is:
                        -- dom x.<.uLft .+. cod x.<.uRt 
                        -- correct for PVee but not for Prel?
                        -- Explanation:
                        -- In the case of PVee, we decide to change the occurrence of PVee for a different one, and choose to pick the smallest such that the end-result will not change
                        -- In the case of Prel, we cannot decide to change the occurrence, since sharing occurs. More specifically, the statement is simply not true.
                          _   -> ( Map.empty -- produce first element outside of case to prevent loop
                                 , case spcls of
                                     [c] -> let (t1,t2) = dom x.=.dom c .+. cod x.=.cod c .+. dom y.=.dom c .+. cod y.=.cod c 
                                            in  t1 .++. t2
                                     _   -> Map.empty
                                 ) -}
     (Pflp o nm) -> let e = Prel o nm
                    in dom x.=.cod e .+. cod x.=.dom e .+. uType ctxt e uRt uLft e
     (PTflp o nm (P_Sign cs)) -> let e = PTrel o nm (P_Sign (reverse cs))
                    in dom x.=.cod e .+. cod x.=.dom e .+. uType ctxt e uRt uLft e
 -- derived uTypes: the following do no calculations themselves, but merely rewrite terms to the ones we covered
{-     (PRad o a b) -> let e = complement (PCps o (complement a) (complement b))
                     in dom x.=.dom e .+. cod x.=.cod e .+.
                        uType ctxt x uLft uRt e                 --  a!b     relative addition, dagger
-}
     (Pimp o a b) -> let e = Pequ o a (PIsc o a b)
                     in dom x.=.dom e .+. cod x.=.cod e .+.
                        uType ctxt x uLft uRt e                 --  a|-b    implication (aka: subset)
     (PLrs o a b) -> let e = complement (PCps o (complement a) (p_flp b))
                     in dom x.=.dom e .+. cod x.=.cod e .+.
                        uType ctxt x uLft uRt e                 --  a/b = a!-b~ = -(-a;b~)
     (PRrs o a b) -> let e = complement (PCps o (p_flp a) (complement b))
                     in dom x.=.dom e .+. cod x.=.cod e .+.
                        uType ctxt x uLft uRt e                 --  a\b = -a~!b = -(a~;-b)


--  The following is for drawing graphs.

toDotGraph ::   (a->String) -- ^ a show-function for displaying vertices.
             -> (a->String) -- ^ a show-function for getting different dot-identifiers for vertices.
             -> [a] -- ^ main vertices
             -> [a] -- ^ secondary vertices (if any)
             -> [(a,a)] -- ^ main edges
             -> [(a,a)] -- ^ secondary edges (if any)
             -> DotGraph String        -- ^ The resulting DotGraph
toDotGraph showVtx idVtx vtx1 vtx2 edg1 edg2
       = DotGraph { strictGraph = False
                  , directedGraph = True
                  , graphID = Nothing
                  , graphStatements 
                        = DotStmts { attrStmts = [GraphAttrs [Splines SplineEdges, RankDir FromLeft]]
                                   , subGraphs = []
                                   , nodeStmts = map (constrNode []) vtx1 ++ map (constrNode [Style [SItem Dashed []]]) vtx2
                                   , edgeStmts = map (constrEdge []) edg1 ++ map (constrEdge [Style [SItem Dashed []]]) edg2
                                   }
                  }
   where
    -- constrNode :: Graph.Vertex -> DotNode String
    constrNode attr v
      = DotNode { nodeID = idVtx v
                , nodeAttributes = [ toLabel (showVtx v)]++attr
                }
    -- constrEdge :: Graph.Edge -> DotEdge String
    constrEdge attr (v, v')
      = DotEdge { fromNode = idVtx v
                , toNode   = idVtx v'
                , edgeAttributes = attr
                }

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



{-
 -- move an error into a new error, for giving a per-pattern/procedure/interface grouping of errors
reLocate :: String -> String -> Origin -> Guarded a -> Guarded a
reLocate tp nm or (Errors  a) = Errors [CxeOrig a tp nm or]
reLocate _  _  _  (Checked b) = Checked b
-}

-- note the difference between Monad and Applicative:
-- if we want to get no errors from val2 if there are still errors in val1, use:
-- do { a <- val1; b <- val2; return (f a b) }
-- if we want to get both errors, use:
-- f <$> a <*> b

-- | Transform a context as produced by the parser into a type checked heterogeneous algebra.
--   Produce type error messages if necessary and produce proof graphs to document the type checking process.
pCtx2aCtx :: P_Context -> (Guarded A_Context,DotGraph String,DotGraph String)
pCtx2aCtx p_context
 = ( if null typeErrors then Checked contxt else Errors typeErrors
   , stTypeGraph, eqTypeGraph)
   where
    contxt = 
         ACtx{ ctxnm     = name p_context
             , ctxpos    = ctx_pos p_context
             , ctxlang   = fromMaybe Dutch (ctx_lang p_context)
             , ctxmarkup = fromMaybe ReST  (ctx_markup p_context)
             , ctxpo     = gEandClasses
             , ctxthms   = ctx_thms p_context
             , ctxpats   = pats
             , ctxprocs  = procs
             , ctxrs     = ctxrules
             , ctxds     = map fst adecsNPops
             , ctxpopus  = populationTable
             , ctxcds    = acds
             , ctxks     = keys
             , ctxgs     = agens
             , ctxifcs   = ifcs
             , ctxps     = apurp
             , ctxsql    = sqlPlugs
             , ctxphp    = phpPlugs
             , ctxenv    = let sgn = Sign ONE ONE in (vExpr sgn,[])
             , ctxmetas  = [ Meta pos metaObj nm val | P_Meta pos metaObj nm val <- ctx_metas p_context ]
             }
    populationTable = map (foldl1 addPops) (eqClass sameClass allpops)
      where 
        sameClass a b = 
           case (a , b) of 
             (PRelPopu{},PRelPopu{}) -> popdcl a == popdcl b
             (PCptPopu{},PCptPopu{}) -> popcpt a == popcpt b
             _                       -> False
        addPops :: UserDefPop -> UserDefPop -> UserDefPop
        addPops (PRelPopu d ps1) (PRelPopu _ ps2) = PRelPopu d (ps1 `union` ps2)
        addPops (PCptPopu c as1) (PCptPopu _ as2) = PCptPopu c (as1 `union` as2)
        addPops _                 _    = fatal 1009 "Two different population types must not be added!"
        allpops = popsfrompops    -- Populations declared as separate population statement.
               ++ popsfromdecls   -- Populations declared inside declaration statements
               ++ popsFromMp1Rels -- Populations from singletons. (defined all over the place)
        popsfromdecls = concatMap ptups pats    -- Populations from declarations inside all patterns
                     ++ concatMap prcUps procs  -- Populations from declarations inside all processes
                     ++ mapMaybe snd adecsNPops      -- Populations from declarations directly in side the context
                      
    st, eqType :: Typemap                  -- eqType = (st*/\st*~)\/I  (total, reflexive, symmetric and transitive)
    bindings ::   Map Type [P_Declaration]         -- declarations that may be bound to relations, intended as a suggestion to the programmer
    isaClos, isaClosReversed :: Map P_Concept [P_Concept]                   -- 
    (st, stClos, eqType, stClosAdded, stClos1 , bindings, srcTypes, isaClos, isaClosReversed) = typing p_context
    gEandClasses :: GenR
    gEandClasses
{- The following may be useful for debugging:
     = (error.concat)
       (["isaClos:"]++
        ["\n  "++show b | b<-Map.toAscList isaClos ]++
        ["\nisaClosReversed:"]++
        ["\n  "++show b | b<-Map.toAscList isaClosReversed ]++
        ["\nsorted: "++show sorted]++
        ["\nclasses: "++show classes]++
        ["\nisas: "++show isas]
       )
-}
     = (gE, classes, isas, meets, joins)   -- The base hierarchy for the partial order of concepts (see makePartialOrder)
       where 
          gE a b = pgE (aCpt2pCpt a) (aCpt2pCpt b)
          pgE a b | a==b                              = Poset.EQ
                  | b `elem` isaClos Map.! a          = Poset.LT
                  | b `elem` isaClosReversed Map.! a  = Poset.GT
                  | null (((isaClosReversed Map.! a) `isc` (isaClosReversed Map.! b)) `uni`
                          ((isaClos         Map.! a) `isc` (isaClos         Map.! b)))
                                                      = Poset.NC
                  | otherwise                         = Poset.CP
          meets a b = map pCpt2aCpt ((isaClosReversed Map.! aCpt2pCpt a) `isc` (isaClosReversed Map.! aCpt2pCpt b))
          joins a b = map pCpt2aCpt ((isaClos         Map.! aCpt2pCpt a) `isc` (isaClos         Map.! aCpt2pCpt b))
          sorted = (GHC.Exts.sortWith ((0-).length.snd) (Map.toList isaClosReversed))
          classes = [map pCpt2aCpt (x:filter (/=x) xs) | (x,xs)<-recur sorted ]
           where
           -- Example: if  sorted == [(E,[A,B,D,E]),(A,[A,D]),(B,[B,D]),(D,[D]),(X,[X]),(Y,[Y])]
           --          then recur sorted = [(E,[A,B,D,E]),(X,[X]),(Y,[Y])]
              recur :: [(P_Concept,[P_Concept])] -> [(P_Concept,[P_Concept])]
              recur ((c,smallerCs):cs)
               = (c,smallerCs) : recur [ c' | c'<-cs, fst c' `notElem` smallerCs ]
              recur [] = []
          isas = [(pCpt2aCpt s, pCpt2aCpt g) | (s,g)<-recur sorted, s/=g ]
           where
              recur :: [(P_Concept,[P_Concept])] -> [(P_Concept,P_Concept)]
              recur ((generic,smallerCs):cs)
               = tuples++ recur cs
                 where
                  tuples = [(specific,generic)
                           | specific <- smallerCs>-(generic:[ x | (c',cs')<-cs, c' `elem` smallerCs, x<-cs', x/=c' ])
                           ]
              recur [] = []
    typeErrors :: [CtxError]
    typeErrors
     | (not.null) derivedEquals = derivedEquals
     | (not.null) cxerrs        = cxerrs
     | otherwise                = postchks
    derivedEquals :: [CtxError]
    derivedEquals   -- These concepts can be proven to be equal, based on st (= typing sentences, i.e. the terms derived from the script).
     = [ CxeEqConcepts eqs
       | (TypExpr (Pid{}) _, equals)<-Map.toAscList eqType
       , let eqs=[c | TypExpr (Pid c) _<-equals ]
       , length eqs>1]
    (stTypeGraph,eqTypeGraph) = typeAnimate st stClos eqType stClosAdded stClos1
    cxerrs = rulecxes++keycxes++interfacecxes++patcxes++proccxes++sPlugcxes++pPlugcxes++popcxes++deccxes++xplcxes++themeschk
    --postchcks are those checks that require null cxerrs 
    postchks = rulenmchk ++ ifcnmchk ++ patnmchk ++ cyclicInterfaces
    acds = ctx_cs p_context++concatMap pt_cds (ctx_pats p_context)++concatMap procCds (ctx_PPrcs p_context)
    agens = map (pGen2aGen "NoPattern") (ctx_gs p_context)
    (adecsNPops,deccxes)   = case (parallelList . map pDecl2aDecl               . ctx_ds   ) p_context of
                            Checked decs -> ([(d{decpat="NoPattern"},mp) | (d,mp)<-decs], [])
                            Errors  errs -> (fatal 1030 ("Do not refer to undefined declarations\n"++show errs), errs)
    (apurp,   xplcxes)   = case (parallelList . map  pPurp2aPurp              . ctx_ps   ) p_context of
                            Checked purps -> (purps, [])
                            Errors  errs  -> (fatal 1033 ("Do not refer to undefined purposes\n"++show errs), errs)
    (pats,    patcxes)   = case (parallelList . map pPat2aPat                 . ctx_pats ) p_context of
                            Checked pats' -> (pats', [])
                            Errors  errs  -> (fatal 1036 ("Do not refer to undefined patterns\n"++show errs), errs)
    (procs,   proccxes)  = case (parallelList . map pProc2aProc               . ctx_PPrcs) p_context of
                            Checked prcs -> (prcs, [])
                            Errors errs  -> (fatal 1039 ("Do not refer to undefined processes\n" ++ show errs), errs)
    (ctxrules,rulecxes)  = case (parallelList . map (pRul2aRul "NoPattern")   . ctx_rs   ) p_context of
                            Checked ruls -> (ruls, [])
                            Errors errs  -> (fatal 1042 ("Do not refer to undefined rules\n"++show errs), errs)
    (keys,    keycxes)   = case (parallelList . map pKDef2aKDef               . ctx_ks   ) p_context of
                            Checked ks   -> (ks, [])
                            Errors errs  -> (fatal 1045 ("Do not refer to undefined keys\n"++show errs), errs)
    (ifcs,interfacecxes) = case (parallelList . map  pIFC2aIFC                . ctx_ifcs ) p_context of
                            Checked is -> (is, [])
                            Errors errs  -> (fatal 1048 ("Do not refer to undefined interfaces\n"++show errs), errs)
    (sqlPlugs,sPlugcxes) = case (parallelList . map (pODef2aODef [] Anything) . ctx_sql  ) p_context of
                            Checked plugs -> (plugs, [])
                            Errors errs   -> (fatal 1051 ("Do not refer to undefined sqlPlugs\n"++show errs), errs)
    (phpPlugs,pPlugcxes) = case (parallelList . map (pODef2aODef [] Anything) . ctx_php  ) p_context of
                            Checked plugs -> (plugs, [])
                            Errors errs   -> (fatal 1054 ("Do not refer to undefined phpPlugs\n"++show errs), errs)
    (popsfrompops, popcxes)   = case (parallelList . map  pPop2aPop                . pops     ) p_context of
                            Checked ps -> (ps, [])
                            Errors errs  -> (fatal 1057 ("Do not refer to undefined populations\n"++show errs), errs)
    pops pc
     = ctx_pops pc ++
       [ pop | pat<-ctx_pats pc,  pop<-pt_pop pat] ++
       [ pop | prc<-ctx_PPrcs pc, pop<-procPop prc]
    popsFromMp1Rels = [PCptPopu { popcpt = source e
                                , popas  = [str]
                                }
                      | e@(EMp1 str _)<-allMp1Rels]
      where 
        allMp1Rels =    mp1Exprs pats
                  `uni` mp1Exprs procs
                  `uni` mp1Exprs ctxrules
                  `uni` mp1Exprs keys
                  `uni` mp1Exprs ifcs 
             
    themeschk = case orphans of
                 []   -> []
                 [nm] -> [newcxe ("Theme '"++nm++"' is selected for output, but is not defined.")]
                 _    -> [newcxe ("The following themes are selected for output, but are not defined:\n   "++intercalate ", " orphans)]
                where orphans = ctxthms contxt>-themenames
                      themenames=[name p |p<-pats]++[name p |p<-procs]
    rulenmchk = [ newcxe ("Rules with identical names at positions "++show(map origin cl))
                | cl<-eqCl name (udefrules contxt),length cl>1]
    ifcnmchk  = [newcxe ("Interfaces with identical names at positions "++show(map origin cl))
                | cl<-eqCl name ifcs,length cl>1]
    patnmchk  = [newcxe ("Patterns or processes with identical names at positions "++show(map fst cl))
                | cl<-eqCl snd (zip (map origin pats++map origin procs)
                                    (map name   pats++map name   procs)),length cl>1]
    cyclicInterfaces = [ newcxe $ "These interfaces form a reference cycle:\n" ++
                                  unlines [ "- " ++ show ifcNm ++ " at " ++ show (origin $ lookupInterface ifcNm)
                                          | ifcNm <- iCycle ]
                       | iCycle <- getCycles refsPerInterface ]
      where refsPerInterface = [(name ifc, getDeepIfcRefs $ ifcObj ifc) | ifc <- ifcs ]
            getDeepIfcRefs obj = case objmsub obj of
                                   Nothing                -> []
                                   Just (InterfaceRef nm) -> [nm]
                                   Just (Box objs)        -> concatMap getDeepIfcRefs objs
            lookupInterface nm = case [ ifc | ifc <- ifcs, name ifc == nm ] of
                                   [ifc] -> ifc
                                   _     -> fatal 124 "Interface lookup returned zero or more than one result"

    pPat2aPat :: P_Pattern -> Guarded Pattern
    pPat2aPat ppat
     = f <$> parRuls ppat <*> parKeys ppat <*> parDcls ppat <*> parPrps ppat
       where
        f prules keys' decsNpops xpls
         = A_Pat { ptnm  = name ppat
                 , ptpos = pt_pos ppat
                 , ptend = pt_end ppat
                 , ptrls = prules
                 , ptgns = agens'
                 , ptdcs = [d{decpat=name ppat} | (d,_)<-decsNpops]
                 , ptups = catMaybes (map snd decsNpops)
                 , ptkds = keys'
                 , ptxps = xpls
                 }
        agens'  = map (pGen2aGen (name ppat)) (pt_gns ppat)
        parRuls = parallelList . map (pRul2aRul (name ppat)) . pt_rls
        parKeys = parallelList . map pKDef2aKDef . pt_kds
        parDcls = parallelList . map pDecl2aDecl . pt_dcs
        parPrps = parallelList . map pPurp2aPurp . pt_xps

    pProc2aProc :: P_Process -> Guarded Process
    pProc2aProc pproc
     = f <$> parRuls pproc <*> parKeys pproc <*> parDcls pproc <*> parRRels pproc <*> parRRuls pproc <*> parPrps pproc
       where
        f prules keys' decsNpops rrels rruls expls
         = Proc { prcNm    = procNm pproc
                , prcPos   = procPos pproc
                , prcEnd   = procEnd pproc
                , prcRules = prules
                , prcGens  = map (pGen2aGen (name pproc)) (procGens pproc)            -- The generalizations defined in this pattern
                , prcDcls  = [d{decpat=name pproc} | (d,_)<-decsNpops]                         -- The declarations declared in this pattern
                , prcUps   = catMaybes (map snd decsNpops)
                , prcRRuls = [(rol,rul) |rul<-udefrules contxt, rr<-rruls, name rul `elem` mRules rr, rol<-mRoles rr] -- The assignment of roles to rules.
                , prcRRels = [(rol,rel) |rr<-rrels, rol<-rrRoles rr, rel<-rrRels rr]  -- The assignment of roles to Relations.
                , prcKds   = keys'                                                    -- The key definitions defined in this process
                , prcXps   = expls                                                    -- The purposes of elements defined in this process
                } 
        parRuls  = parallelList . map (pRul2aRul (name pproc)) . procRules
        parDcls  = parallelList . map pDecl2aDecl . procDcls
        parRRels = parallelList . map pRRel2aRRel . procRRels
        parRRuls = parallelList . map pRRul2aRRul . procRRuls
        parKeys  = parallelList . map pKDef2aKDef . procKds
        parPrps  = parallelList . map pPurp2aPurp . procXps
 
    pRRul2aRRul :: RoleRule -> Guarded RoleRule
    pRRul2aRRul prrul
     = do { let pRuleNames = [name rul | rul<-ctx_rs p_context++(concat.map pt_rls.ctx_pats) p_context++(concat.map procRules.ctx_PPrcs) p_context]
          ; case mRules prrul>-pRuleNames of  -- If there are rule names without rules attached to them, we have an error....
             []  -> return prrul
             rs  -> Errors [ CxeNoRules { cxePos = origin prrul , cxeRules = rs } ]
          }
           
    pRRel2aRRel :: P_RoleRelation -> Guarded RoleRelation
    pRRel2aRRel prrel
     = f <$> parRels prrel
       where
        f erels = RR { rrRoles = rr_Roles prrel
                     , rrRels  = [ case erel of
                                    ERel rel _ -> rel
                                    _   -> fatal 1149 ("Erroneous expression "++showADL erel++" in pRRel2aRRel.")
                                 | (erel,_,_)<-erels ]
                     , rrPos   = rr_Pos prrel
                     }
        parRels = parallelList . map pExpr2aExpr . rr_Rels
    
{- PairViews are made for the purpose of assembling nice messages for violations. E.g. VIOLATION lateEntry : (afmaken)
   data P_PairView = P_PairView [P_PairViewSegment] deriving Show
   data P_PairViewSegment = P_PairViewText String
                          | P_PairViewExp SrcOrTgt Term

-}
    p2aPairView :: P_Concept -> P_Concept -> P_PairView -> Guarded PairView
    p2aPairView srcCpt trgCpt (P_PairView ppvs) = do { guardedPpvs <- (parallelList . map (p2aPairViewSegment srcCpt trgCpt)) ppvs ; return (PairView guardedPpvs) }

    p2aPairViewSegment :: P_Concept -> P_Concept -> P_PairViewSegment -> Guarded PairViewSegment
    p2aPairViewSegment _      _        (P_PairViewText str)          = Checked (PairViewText str)
    p2aPairViewSegment srcCpt trgCpt v@(P_PairViewExp srcOrTgt pexp) = do { (aexp,s,_) <- pExpr2aExpr pexp
                                                                          ; case srcOrTgt of
                                                                             Src -> if s==srcCpt
                                                                                    then Checked (PairViewExp srcOrTgt aexp)
                                                                                    else Errors [CxeViol v s srcCpt]
                                                                             Tgt -> if s==trgCpt
                                                                                    then Checked (PairViewExp srcOrTgt aexp)
                                                                                    else Errors [CxeViol v s trgCpt]
                                                                          }
    pRul2aRul :: String -> P_Rule -> Guarded Rule
    pRul2aRul patname prul        -- for debugging the parser, this is a good place to put     error (show (rr_exp prul))
     = do { (aexpr,srcCpt,trgCpt) <- pExpr2aExpr (rr_exp prul)
          ; mviol <- case rr_viol prul of
                      Nothing        -> Checked Nothing
                      Just pViolSegs -> case p2aPairView srcCpt trgCpt pViolSegs of
                                          Errors errs -> Errors errs
                                          Checked violSegs -> Checked (Just violSegs)
          ; let meanings = pMeanings2aMeaning (ctxlang contxt) (ctxmarkup contxt) (rr_mean prul)
          ; return (Ru { rrnm   = rr_nm prul                 -- Name of this rule
                       , rrexp  = aexpr                      -- The rule term
                       , rrfps  = rr_fps prul                -- Position in the Ampersand file
                       , rrmean = meanings                   -- Ampersand generated meaning (for all known languages)
                       , rrmsg  = map (pMarkup2aMarkup (ctxlang contxt) (ctxmarkup contxt)) $ rr_msg prul
                       , rrviol = mviol                      -- contains instructions for making violation messages.
                       , rrtyp  = sign aexpr                 -- Allocated type
                       , rrdcl  = Nothing                    -- The property, if this rule originates from a property on a Declaration
                       , r_env  = patname                    -- Name of pattern in which it was defined.
                       , r_usr  = UserDefined 
                       , r_sgl  = or [rr_nm prul `elem` map (name.snd) (prcRRuls p) | p<-ctxprocs contxt]
                       , srrel  = Sgn { decnm = rr_nm prul        -- the signal relation
                                      , decsgn = sign aexpr
                                      , decprps = []
                                      , decprps_calc = Nothing --[]
                                      , decprL = ""
                                      , decprM = ""
                                      , decprR = ""
                                      , decMean = meanings
                                      , decConceptDef = Nothing
                                      , decfpos = rr_fps prul
                                      , deciss = True
                                      , decusr = False
                                      , decpat = ""
                                      , decplug = True
                                      }
                       } )
          }

    pMeanings2aMeaning :: Lang          -- The default language
                      -> PandocFormat  -- The default format
                      -> [PMeaning]
                      -> AMeaning
    pMeanings2aMeaning ldefLang defFormat pms
       = AMeaning (map (pMeaning2Amarkup ldefLang defFormat) pms)
         where  pMeaning2Amarkup l f (PMeaning pm)
                 = pMarkup2aMarkup l f pm
                   
    pMarkup2aMarkup :: Lang          -- The default language
                    -> PandocFormat  -- The default format
                    -> P_Markup
                    -> A_Markup
    pMarkup2aMarkup defLang defFormat pm
       = A_Markup { amLang   = fromMaybe defLang (mLang pm)
                  , amFormat = fmt
                  , amPandoc = string2Blocks fmt (mString pm)
                  }
               where fmt = fromMaybe defFormat (mFormat pm)

    -- | pKDef2aKDef checks compatibility of composition with key concept on equality
    pKDef2aKDef :: P_KeyDef -> Guarded KeyDef
    pKDef2aKDef pkdef
     = case typeErrors' of
        [] -> Checked (Kd { kdpos = kd_pos pkdef
                          , kdlbl = kd_lbl pkdef
                          , kdcpt = c
                          , kdats = segs
                          })
        _  -> Errors [CxeOrig typeErrors' "key definition" "" (origin pkdef) | (not.null) typeErrors']
       where
        typeErrors' = kdcxe++segscxes
        (segs, segscxes) = case (parallelList . map (pKeySeg2aKeySeg (kd_cpt pkdef)) . kd_ats) pkdef of
                             Checked segments -> (segments, [])
                             Errors  errs     -> (fatal 1166 ("Do not refer to undefined segments\n"++show errs), errs)
        c  = pCpt2aCpt (kd_cpt pkdef)
        -- check equality
        ats = [ expr | KeyExp expr <- segs ]
        kdcxe = newcxeif (null segscxes && length (nub (c:map (source.objctx) ats))/=1)
                         (intercalate "\n" ["The source of term " ++ showADL (objctx x) 
                                            ++" ("++showADL (source (objctx x))++") is compatible, but not equal to the key concept ("++ showADL c ++ ")."
                                           |x<-ats,source (objctx x)/=c])
    
    pKeySeg2aKeySeg :: P_Concept -> P_KeySegment -> Guarded KeySegment
    pKeySeg2aKeySeg _      (P_KeyText str)   = return (KeyText str)
    pKeySeg2aKeySeg _      (P_KeyHtml str)   = return (KeyHtml str)
    pKeySeg2aKeySeg concpt (P_KeyExp keyExp) = do { objDef <- pODef2aODef [] (thing concpt) keyExp
                                                  ; return (KeyExp objDef)
                                                  }
    
    -- TODO -> What is the intention of ifcViols?
    -- TODO -> What is the intention of ifcArgs?
    pIFC2aIFC :: P_Interface -> Guarded Interface
    pIFC2aIFC pifc 
     = f <$> parParams pifc <*> (pODef2aODef parentIfcRoles Anything . ifc_Obj) pifc
       where
        f prms obj
         = Ifc { ifcParams = [ case erel of
                                ERel rel _ -> rel
                                _ -> fatal 1273 ("Erroneous expression "++showADL erel++" in pIFC2aIFC.")
                             | (erel,_,_)<-prms ]
               , ifcViols  = fatal 206 "not implemented ifcViols"
               , ifcArgs   = ifc_Args pifc
               , ifcRoles  = parentIfcRoles
               , ifcObj    = obj
               , ifcPos    = ifc_Pos pifc
               , ifcPrp    = ifc_Prp pifc
               }
        parParams = parallelList . map pExpr2aExpr . ifc_Params
        parentIfcRoles = if null $ ifc_Roles pifc then roles contxt else nub (ifc_Roles pifc) -- if no roles are specified, the interface supports all roles
        
    -- | pODef2aODef checks compatibility of composition of terms on equality
    pODef2aODef :: [String]              -- a list of roles that may use this object
                -> Type                  -- the universe for type checking this object. anything if the type checker decides freely, thing c if it must be of type c.
                -> P_ObjectDef           -- the object definition as specified in the parse tree
                -> Guarded ObjectDef     -- result: the type checked object definition (only defined if there are no type errors) and a list of type errors
    pODef2aODef parentIfcRoles universe podef 
     = do { let oTerm = obj_ctx podef
          ; _ <- case (srcTypes.normalType) (TypGlb universe (dom oTerm) oTerm) of
                    [s] -> return s
                    _   -> Errors [CxeObjMismatch oTerm (srcTypes universe) (srcTypes (dom oTerm))]
          ; msub <- p2a_MaybeSubInterface parentIfcRoles oTerm (obj_msub podef)
          ; (expr, _, _) <- pExpr2aExpr oTerm
                -- A name check ensures that all attributes have unique names
          ; _ <- case [ cl | Just (P_Box objs)<-[obj_msub podef], cl<-eqCl name objs, length cl>1] of
                   []  -> return []
                   cls -> Errors [CxeEqAttribs (origin podef) (name (head cl)) (map obj_ctx cl) | cl<-cls ]
          ; return ( Obj { objnm   = obj_nm podef   
                         , objpos  = obj_pos podef  
                         , objctx  = expr           
                         , objmsub = msub           
                         , objstrs = obj_strs podef 
                         } )                        
          }
    p2a_MaybeSubInterface :: [String] -> Term -> Maybe P_SubInterface -> Guarded (Maybe SubInterface)
    p2a_MaybeSubInterface _              _    Nothing               = return Nothing
    p2a_MaybeSubInterface parentIfcRoles env (Just (P_Box p_objs))
     = do { objects <- parallelList [pODef2aODef parentIfcRoles (cod env) p_obj | p_obj<-p_objs]
          ; return (Just (Box objects))
          }
    p2a_MaybeSubInterface parentIfcRoles env (Just (P_InterfaceRef pos nm))
     = do { p_ifc <- case [p_ifc | p_ifc <- ctx_ifcs p_context, name p_ifc == nm ] of
                       [p_ifc] -> return p_ifc
                       ifs     -> Errors [CxeNoIfcs nm pos ifs]
          ; let oTerm  = obj_ctx (ifc_Obj p_ifc)
          ; _ <- case (srcTypes.normalType) (TypGlb (cod env) (dom oTerm) oTerm) of
                    [s] -> return s
                    _   -> Errors [CxeObjMismatch oTerm (srcTypes (cod env)) (srcTypes (dom oTerm))]
          ; thisIfcRoles <- case ifc_Roles p_ifc of
                             [] -> Errors [CxeNoRoles p_ifc]
                             rs -> return rs
          ; case parentIfcRoles \\ thisIfcRoles of
              [] -> return (Just (InterfaceRef nm))
              rs -> Errors [CxeUnsupRoles p_ifc rs]
          }
      
    pPurp2aPurp :: PPurpose -> Guarded Purpose
    pPurp2aPurp pexpl
     = do { explobs <- pExOb2aExOb (pexObj pexpl)
          ; return ( Expl { explPos      = pexPos   pexpl
                          , explObj      = explobs
                          , explMarkup   = pMarkup2aMarkup (ctxlang contxt) (ctxmarkup contxt) (pexMarkup pexpl)
                          , explRefId    = pexRefID pexpl
                          , explUserdefd = True
                         -- , explCont  = string2Blocks (ctxmarkup contxt) (pexExpl  pexpl)
                          })
          }
    
    pExOb2aExOb :: PRef2Obj -> Guarded ExplObj
    pExOb2aExOb (PRef2ConceptDef str  ) = case [cd | cd<-acds, cdcpt cd==str ] of
                                           []   ->  Errors [newcxe (" No concept definition for '"++str++"'")]
                                           cd:_ ->  Checked (ExplConceptDef cd)
    pExOb2aExOb (PRef2Declaration t@(PTrel o nm sgn))
                                        = case [pDecl2aDecl d | d<-p_declarations p_context, name d==nm, dec_sign d== sgn ] of
                                            Checked (decl,_):_ -> Checked (ExplDeclaration decl)
                                            Errors ers:_   -> Errors ers
                                            []             -> Errors [CxeOrig [newcxe ("No declaration for '"++showADL t++"'")] "relation" nm o ]
    pExOb2aExOb (PRef2Declaration t@Prel{})
                                        = do { decl <- getDeclaration t
                                             ; return (ExplDeclaration decl)
                                             }
    pExOb2aExOb (PRef2Declaration term) = fatal 1270 $ "Nothing defined for "++show term
    pExOb2aExOb (PRef2Rule str        ) = case [rul | rul<-p_rules p_context, name rul==str ] of
                                           [] -> Errors [newcxe (" No rule named '"++str++"'")]
                                           _  -> Checked (ExplRule str)
    pExOb2aExOb (PRef2KeyDef str      ) = case [kd | kd<-p_keys p_context, name kd==str] of
                                           [] -> Errors [newcxe (" No key definition named '"++str++"'")]
                                           _  -> Checked (ExplKeyDef str)
    pExOb2aExOb (PRef2Pattern str     ) = case [pat |pat<-ctx_pats  p_context, name pat==str] of
                                           [] -> Errors [newcxe (" No pattern named '"++str++"'")]
                                           _  -> Checked (ExplPattern str)
    pExOb2aExOb (PRef2Process str     ) = case [prc |prc<-ctx_PPrcs p_context, name prc==str] of
                                           [] -> Errors [newcxe (" No process named '"++str++"'")]
                                           _  -> Checked (ExplProcess str)
    pExOb2aExOb (PRef2Interface str   ) = case [ifc |ifc<-ctx_ifcs  p_context, name ifc==str] of
                                           [] -> Errors [newcxe (" No interface named '"++str++"'")]
                                           _  -> Checked (ExplInterface str)
    pExOb2aExOb (PRef2Context str     ) = if name p_context/=str
                                          then Errors [newcxe (" No context named '"++str++"'")]
                                          else Checked (ExplContext str)
    pExOb2aExOb (PRef2Fspc str        ) = if name p_context/=str
                                          then Errors [newcxe (" No cospecificationntext named '"++str++"'")]
                                          else Checked (ExplFspc str)

    pPop2aPop :: P_Population -> Guarded UserDefPop
    pPop2aPop pop
     = case pExpr2aExpr expr of
        Checked (  ERel aRel _ ,_,_)       -> Checked (thePop aRel)
        Checked (ETyp (ERel aRel _) _,_,_) -> Checked (thePop aRel)
        Checked _                          -> fatal 1223 "illegal call of pPop2aPop"
        Errors errs                        -> Errors errs
       where expr = case pop of
                      P_CptPopu{}                 -> Pid (PCpt (name pop))
                      P_RelPopu{p_type=P_Sign []} -> Prel (origin pop) (name pop)
                      P_RelPopu{}                 -> PTrel (origin pop) (name pop) (p_type pop)
             thePop rel = case rel of
                            Rel{} -> PRelPopu { popdcl = reldcl rel
                                              , popps  = case pop of
                                                           P_RelPopu{} -> p_popps pop
                                                           P_CptPopu{} -> fatal 1438 ("Unexpected issue with population of "++name pop) 
                                              }
                            I{}   -> PCptPopu { popcpt = rel1typ rel
                                              , popas  = case pop of
                                                           P_RelPopu{} -> fatal 1441 ("Unexpected issue with population of "++name pop)
                                                           P_CptPopu{} -> p_popas pop
                                              }
                            V{}   -> fatal 1444 "V has no population of it's own"
    pGen2aGen :: String -> P_Gen -> A_Gen
    pGen2aGen patNm pg
       = Gen{genfp  = gen_fp  pg
            ,gengen = pCpt2aCpt (gen_gen pg)
            ,genspc = pCpt2aCpt (gen_spc pg)
            ,genpat = patNm
            }
              
    pSign2aSign :: P_Sign -> Sign
    pSign2aSign (P_Sign cs) = Sign (head ts) (last ts)
      where ts = map pCpt2aCpt cs
            
    pCpt2aCpt :: P_Concept -> A_Concept
    pCpt2aCpt pc
        = case pc of
            PCpt{} -> c 
            P_Singleton -> ONE
          where 
          c = C {cptnm = p_cptnm pc
                ,cptgE = genE contxt
                ,cpttp = head ([cdtyp cd | cd<-conceptDefs contxt,cdcpt cd==p_cptnm pc]++[""])
                ,cptdf = [cd | cd<-conceptDefs contxt,cdcpt cd==p_cptnm pc]
                }
    
    aCpt2pCpt :: A_Concept -> P_Concept
    aCpt2pCpt c
        = case c of
            C{} -> PCpt { p_cptnm = cptnm c }
            ONE -> P_Singleton
    
    pDecl2aDecl :: P_Declaration -> Guarded (Declaration , Maybe UserDefPop)
    pDecl2aDecl pd =
     case dec_conceptDef pd of 
          Just (RelConceptDef srcOrTgt _) | relConceptName (dec_nm pd) `elem` map name (concs contxt)
            -> Errors [CxeOrig [newcxe ("Illegal DEFINE "++showSRCorTGT++" for relation "++show (dec_nm pd)++". Concept "++
                                        relConceptName (dec_nm pd)++" already exists.")]
                               "declaration" "" (origin pd)]
               where showSRCorTGT = if srcOrTgt == Src then "SRC" else "TGT"
                     relConceptName ""     = fatal 472 "empty concept"
                     relConceptName (c:cs) = toUpper c : cs
          _ -> Checked ( d,mp )
               where d = Sgn { decnm   = dec_nm pd
                             , decsgn  = pSign2aSign (dec_sign pd)
                             , decprps = dec_prps pd
                             , decprps_calc = Nothing  --decprps_calc in an A_Context are still the user-defined only. prps are calculated in adl2fspec.
                             , decprL  = dec_prL pd
                             , decprM  = dec_prM pd
                             , decprR  = dec_prR pd
                             , decMean = pMeanings2aMeaning (ctxlang contxt) (ctxmarkup contxt) (dec_Mean pd)
                             , decConceptDef = dec_conceptDef pd
                             , decfpos = dec_fpos pd 
                             , deciss  = True
                             , decusr  = True
                             , decpat  = ""
                             , decplug = dec_plug pd
                             } 
                     mp = case dec_popu pd of
                            [] -> Nothing
                            ps -> Just (PRelPopu {popdcl = d, popps  = ps}) 
                       

    pExpr2aExpr :: Term                               -- The term to be typed
                -> Guarded ( Expression               -- the resulting expression.
                           , P_Concept, P_Concept     -- the source and target types of the resulting expression.
                           )                          -- The result might be incorrect, so it is guarded with type error messages.
    pExpr2aExpr pTerm
     = do { r <- f pTerm
          ; return (r, lookupType pTerm, lookupType (p_flp pTerm))
          }
       where
         f :: Term -> Guarded Expression
         f x = case x of
           PI _            -> do { c<-returnIConcepts x
                                 ; sgn<-getSign x
                                 ; return (ERel (I (pCpt2aCpt c)) sgn)
                                 }
           Pid c           -> return (iExpr (pCpt2aCpt c))
           Pnid c          -> do { sgn<-getSign x
                                 ; return (ECpl (ERel (I (pCpt2aCpt c)) sgn) sgn)
                                 }
           Patm _ atom _   -> do { sgn<-getSign x
                                 ; return (EMp1 atom sgn)
                                 }
           Pnull           -> fatal 1546 ("unsigned Pnull.")
           PVee _          -> do { sgn<-getSign x
                                    ; return (vExpr sgn)
                                 }
           Pfull s t       -> return (vExpr (Sign (pCpt2aCpt s) (pCpt2aCpt t)))
           Prel o a        -> do { decl <- getDeclaration x
                                 ; let [s] = srcTypes (dom x)
                                       [t] = srcTypes (cod x)
                                       sgn = Sign (pCpt2aCpt s) (pCpt2aCpt t)
                                 ; return (ERel (Rel{ relnm=a
                                                    , relpos=o
                                                    , reldcl=decl
                                                    }) sgn)
                                 }
           Pflp o a        -> do { decl <- getDeclaration x
                                 ; let [s] = srcTypes (dom x)
                                       [t] = srcTypes (cod x)
                                       sgn = Sign (pCpt2aCpt s) (pCpt2aCpt t)
                                 ; return (EFlp (ERel (Rel{ relnm=a
                                                          , relpos=o
                                                          , reldcl=decl
                                                          }) sgn) (flp sgn))
                                 }
           Pequ _ a b      -> do { let srcAs = srcTypes (dom a)
                                       trgAs = srcTypes (cod a)
                                       srcBs = srcTypes (dom b)
                                       trgBs = srcTypes (cod b)
                                       srcTyps  = srcAs `uni` srcBs
                                       trgTyps  = trgAs `uni` trgBs
                                 ; case (srcTyps, trgTyps) of
                                        ([_], [_]) -> do { (a',b') <- (,) <$> f a <*> f b
                                                         ; return (a' .==. b')
                                                         }
                                        _          -> Errors [CxeEquLike {cxeExpr    = x
                                                                         ,cxeLhs     = a
                                                                         ,cxeRhs     = b
                                                                         ,cxeSrcCpts = (srcAs `uni` srcBs) >- (srcAs `isc` srcBs)
                                                                         ,cxeTrgCpts = (trgAs `uni` trgBs) >- (trgAs `isc` trgBs)
                                                                         }]
                                 }
           Pimp _ a b      -> do { let srcAs = (srcTypes.normalType) (TypGlb (dom a) (dom b) x)
                                       trgAs = (srcTypes.normalType) (TypGlb (cod a) (cod b) x)
                                       srcBs = srcTypes (dom b)
                                       trgBs = srcTypes (cod b)
                                 ; case (srcAs, trgBs) of
                                        ([_], [_]) -> do { (a',b') <- (,) <$> f a <*> f b
                                                         ; return (a' .|-. b')
                                                         }
                                        _          -> Errors [CxeEquLike {cxeExpr    = x
                                                                         ,cxeLhs     = a
                                                                         ,cxeRhs     = b
                                                                         ,cxeSrcCpts = (srcAs `uni` srcBs) >- (srcAs `isc` srcBs)
                                                                         ,cxeTrgCpts = (trgAs `uni` trgBs) >- (trgAs `isc` trgBs)
                                                                         }]
                                 }
           PIsc _ a b      -> do { let srcs = (srcTypes.normalType) (TypGlb (dom a) (dom b) x)
                                       trgs = (srcTypes.normalType) (TypGlb (cod a) (cod b) x)
                         --      ; _ <- fatal 1638 ("Diagnostic of "++show x++" on "++show (origin x)++"\n  srcs = "++show srcs++"\n  trgs = "++show trgs)
                                 ; case (srcs, trgs) of
                                        ([_], [_]) -> do { (a',b') <- (,) <$> f a <*> f b
                                                         ; return (a' ./\. b')
                                                         }
                                        _          -> Errors [CxeEquLike {cxeExpr    = x
                                                                         ,cxeLhs     = a
                                                                         ,cxeRhs     = b
                                                                         ,cxeSrcCpts = srcs
                                                                         ,cxeTrgCpts = trgs
                                                                         }]
                                 }
           PUni _ a b      -> do { let srcs = (srcTypes.normalType) (TypLub (dom a) (dom b) x)
                                       trgs = (srcTypes.normalType) (TypLub (cod a) (cod b) x)
                                 ; case (srcs, trgs) of
                                        ([_], [_]) -> do { (a',b') <- (,) <$> f a <*> f b
                                                         ; return (a' .\/. b')
                                                         }
                                        _          -> Errors [CxeEquLike {cxeExpr    = x
                                                                         ,cxeLhs     = a
                                                                         ,cxeRhs     = b
                                                                         ,cxeSrcCpts = srcs
                                                                         ,cxeTrgCpts = trgs
                                                                         }]
                                 }
           PDif _ a b      -> do { (a',b') <- (,) <$> f a <*> f b
                                 ; return (a' .-. b')
                                 }
           PLrs _ a b      -> do { case (srcTypes.normalType) (TypGlb (cod (complement a)) (dom (p_flp b)) x) of
--         PLrs _ a b      -> do { case (srcTypes.normalType) (TypGlb (TypExpr (p_flp (complement a)) True) (TypExpr (p_flp b) True) x) of
                                        [_] -> do { (a',b') <- (,) <$> f a <*> f b
                                                  ; return (a' ./. b')
                                                  }
                                        cs  -> Errors [ CxeCpsLike {cxeExpr   = x
                                                                   ,cxeCpts   = cs
                                                                   }
                                                      ]
                                 }
           PRrs _ a b      -> do { case (srcTypes.normalType) (TypGlb (dom a) (dom (complement b)) x) of
--         PRrs _ a b      -> do { case (srcTypes.normalType) (TypGlb (TypExpr a False) (TypExpr (complement b) False) x) of
                                        [_] -> do { (a',b') <- (,) <$> f a <*> f b
                                                  ; return (a' .\. b')
                                                  }
                                        cs  -> Errors [ CxeCpsLike {cxeExpr   = x
                                                                   ,cxeCpts   = cs
                                                                   }
                                                      ]
                                 }
           PCps _ a b      -> do { case (srcTypes.normalType) (TypGlb (cod a) (dom b) x) of
                                      [_] -> do { (a',b') <- (,) <$> f a <*> f b
                                                ; return (a' .:. b')
                                                }
                                      cs  -> Errors [ CxeCpsLike {cxeExpr   = x
                                                                 ,cxeCpts   = cs
                                                                }
                                                    ]
                                 }
           PRad _ a b      -> do { case (srcTypes.normalType) (TypLub (cod a) (dom b) x) of
                                    [_] -> do { (a',b') <- (,) <$> f a <*> f b
                                              ; return (a' .!. b')
                                              }
                                    cs  -> Errors [ CxeCpsLike {cxeExpr   = x
                                                               ,cxeCpts   = cs
                                                               }
                                                  ]
                                 }
           PPrd _ a b      -> do { (a',b') <- (,) <$> f a <*> f b
                                 ; return (a' .*. b')
                                 }
           PKl0 _ a        -> do { a' <- f a
                                 ; return (EKl0 a' (sign a'))
                                 }
           PKl1 _ a        -> do { a' <- f a
                                 ; return (EKl1 a' (sign a'))
                                 }
           PFlp _ a        -> do { a' <- f a
                                 ; return (flp a')
                                 }
           PCpl _ a        -> do { a' <- f a
                                 ; return (notCpl (sign a') a')
                                 }
           PBrk _ a        -> do { a' <- f a
                                 ; return (EBrk a')
                                 }
           PTrel o relNm (P_Sign cs)
            -> case ( delimit (head cs) (srcTypes (dom x))
                    , delimit (last cs) (srcTypes (cod x))
                    ) of
                 ([src],[trg]) -> return (pRel2aRel relNm src trg)
                 (srcs , trgs) -> Errors [CxeCast {cxeExpr    = x
                                                  ,cxeDomCast = srcs
                                                  ,cxeCodCast = trgs
                                                  }]
               where pRel2aRel relName src trg
                      = ETyp aRel sgn
                        where aRel  = case decls of
                                       [decl] -> ERel (Rel relName o decl) sgn
                                       _ -> fatal 1739 ("decls should contain one element only!\n   x = "++show x++"\n   decls = "++show decls++"\n   decs = [ "++intercalate "\n          , " [show d | d<-decs]++"\n          ]")
                              sgn   = Sign aSrc aTrg
                              aSrc  = pCpt2aCpt src
                              aTrg  = pCpt2aCpt trg
                              decls = [ decl | decl<-declarations contxt
                                             , name decl==relName, aSrc Poset.<= source decl, aTrg Poset.<= target decl ]
                              decs  = [ decl | decl<-declarations contxt
                                             , name decl==relName]
           PTflp o relNm (P_Sign cs) -> f (PTrel o relNm (P_Sign (reverse cs)))
           
         delimit :: P_Concept -> [P_Concept] -> [P_Concept]
         delimit cpt concepts = concepts `isc` [c | Just cs <- [Map.lookup cpt isaClos], c<-cs]
         returnIConcepts term
          = case getConceptsFromTerm term of
             [c] -> return c
             cs  -> Errors[CxeILike { cxeExpr = term
                                    , cxeCpts = cs
                                    }]
         getConceptsFromTerm :: Term -> [P_Concept]
         getConceptsFromTerm x
          = srcTypes (dom x)
         getSign :: Term -> Guarded Sign
         getSign term
          = case (srcs,trgs) of
             ([s],[t]) -> Checked (Sign (pCpt2aCpt s) (pCpt2aCpt t))
             (_  ,_  ) -> Errors [CxeSign { cxeExpr = term
                                          , cxeSrcs = srcs
                                          , cxeTrgs = trgs}]
             where srcs = getConceptsFromTerm term
                   trgs = getConceptsFromTerm (p_flp term)
                   
         lookupType :: Term -> P_Concept
         lookupType t = case getConceptsFromTerm t of
                         [c] -> c
                         []  -> fatal 1586 ("No type found for term "++showADL t++" on "++show (origin t)++", even though that term is known in stConcepts.")
                         cs  -> fatal 1586 ("Multiple types found for term "++showADL t++": "++show cs++" on "++show (origin t))

    getDeclaration :: Term -> Guarded Declaration
    getDeclaration term@(Prel _ a)
     = case Map.lookup (dom term) bindings of
        Just [d] -> do { (decl,_) <- pDecl2aDecl d ; return decl }
        Just ds  -> Errors [CxeRel {cxeExpr = term        -- the erroneous term
                                   ,cxeDecs = ds          -- the declarations to which this term has been matched
                                   ,cxeSNDs = [ decl | decl<-p_declarations p_context, name decl==a ]
                                   }]
                                 
        Nothing -> fatal 1601 ("Term "++showADL term++" ("++show(origin term)++") was not found in "++show (length (Map.toAscList bindings))++" bindings."++concat ["\n  "++show b | b<-Map.toAscList bindings, take 7 ( tail (show b))==take 7 (show term) ])
    getDeclaration term = fatal 1607 ("Illegal call to getDeclaration ("++show term++")")



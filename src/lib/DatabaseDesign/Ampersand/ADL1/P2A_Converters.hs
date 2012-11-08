﻿{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
{-# LANGUAGE RelaxedPolyRec #-} -- -RelaxedPolyRec required for OpenSuse, for as long as we@OpenUniversityNL use an older GHC
module DatabaseDesign.Ampersand.ADL1.P2A_Converters (
     -- * Exported functions
     pCtx2aCtx, disambiguate,
     Guarded(..)
     )
where
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree hiding (sortWith, maxima, greatest)
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Basics (name, isc, uni, eqCl, eqClass, getCycles, (>-), fatalMsg)
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec.Fspec
import DatabaseDesign.Ampersand.Fspec.ShowADL
import qualified DatabaseDesign.Ampersand.Core.Poset hiding (sortWith)
import GHC.Exts (sortWith)
import Prelude -- hiding (Ord(..))
import DatabaseDesign.Ampersand.Input.ADL1.CtxError
import Data.GraphViz hiding (addExtension, C)
import Data.GraphViz.Attributes.Complete hiding (Box, Pos)
import Data.Maybe
import Data.List
import Data.Char
import Control.Applicative
import qualified Data.Map

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
p_flp a@(PI{})     = a
p_flp a@(Pid{})    = a
p_flp a@(Patm{})   = a
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

cmpl :: Type -> Type
cmpl (TypExpr e b)  = TypExpr (complement e) b
cmpl (TypLub a b e) = TypGlb (cmpl a) (cmpl b) (complement e)
cmpl (TypGlb a b e) = TypLub (cmpl a) (cmpl b) (complement e)
cmpl Anything       = Nothng
cmpl Nothng         = Anything

thing :: P_Concept -> Type
thing c  = TypExpr (Pid c) False

type Typemap = Data.Map.Map Type [Type]

{- The type  Typemap  is used to represent the population of relations r[Type*Type] (in Ampersand's metamodel)
For the following, let m be a Typemap that represents relation r[Type*Type]
Invariants are:
1. m contains all elements of the source of r
         keys m     equals the population of  I [source r], which are all Type object drawn from the script
1a.      keys m     represents    dom r
1b.      m is total (dom m=I[source r])
   By the way, keys m produces the elements in ascending order without duplicates.
2. All elements of the codomain of r are obtained by 'elems'
         concat (Data.Map.elems m)     represents    cod r
3. The map contains sorted lists without duplicates:
         let isSortedAndDistinct (x:y:rest) = if (x<y) then isSortedAndDistinct (y:rest) else False
             isSortedAndDistinct _ = True
         in Data.Map.fold (&&) True (Data.Map.map isSortedAndDistinct m)
-}

-- | if lst represents a binary relation, then reverseMap lst represents its inverse (viz. flip, wok)
-- | note that the domain must be preserved!
-- | reverseMap r = r~
reverseMap :: (Prelude.Ord a, Show a) => Data.Map.Map a [a] -> Data.Map.Map a [a]
reverseMap lst = (Data.Map.fromListWith mrgUnion (concat [(a,[]):[(b,[a])|b<-bs] | (a,bs)<-Data.Map.toAscList lst]))
-- note: reverseMap is relatively slow, but only needs to be calculated once

-- | addIdentity r = r\/I
addIdentity :: (Prelude.Ord a, Show a) => Data.Map.Map a [a] -> Data.Map.Map a [a]
addIdentity mp = Data.Map.mapWithKey (\k a->mrgUnion a [k]) mp

-- | if lst represents a binary relation r, then reflexiveMap lst represents r/\r~
{-
reflexiveMap :: (Prelude.Ord a, Show a) => Data.Map.Map a [a] -> Data.Map.Map a [a]
reflexiveMap lst = Data.Map.map sort (Data.Map.intersectionWith isc lst (reverseMap lst))
-}

mapIsOk :: (Show a,Ord a) => Data.Map.Map k [a] -> Bool
mapIsOk m = Data.Map.fold (&&) True (Data.Map.map (isSortedAndDistinct . checkRfx) m)
isSortedAndDistinct :: Ord a => [a] -> Bool
isSortedAndDistinct (x:y:rest) = if (x<y) then isSortedAndDistinct (y:rest) else False
isSortedAndDistinct _ = True
-- | The purpose of 'setClosure' is to compute the transitive closure of relations that are represented as a Map (Data.Map.Map a [a]).
--   For that purpose we use a Warshall algorithm.
setClosure :: (Show a,Ord a) => Data.Map.Map a [a] -> String -> Data.Map.Map a [a]
setClosure xs s | not (mapIsOk xs) = fatal 144 ("setClosure on the non-ok set "++s)
setClosure xs _ = if (mapIsOk res) then res else fatal 145 ("setClosure contains errors!")
  where
--   f q x = Data.Map.map (\bs->foldl mrgUnion bs [b' | b<-bs, b == x, (a', b') <- Data.Map.toList q, a' == x]) q
   f q x = Data.Map.map (\bs->foldl mrgUnion bs [b' | x `elem` bs, Just b' <- [Data.Map.lookup x q]]) q
   res   = foldl f xs (Data.Map.keys xs `isc` nub (concat (Data.Map.elems xs)))

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

checkRfx :: (Eq a, Show a) => [a] -> [a]
checkRfx (a:as) = if not (a==a) then fatal 192 ("Eq is not reflexive on "++show a) else a:checkRfx as
checkRfx [] = []        

-- | lookups is the reflexive closure of findIn. lookups(a,R) = findIn(a,R\/I) where a is an element and R is a relation.
lookups :: (Show a,Ord a) => a -> Data.Map.Map a [a] -> [a]
lookups o q = head ([mrgUnion [o] e | Just e<-[Data.Map.lookup o q]]++[[o]])
-- To Stef: lookups was not designed in this way: once upon a time it returned findIn unless the element did not exist.
-- This is a fundamental difference and wherever lookups was used, there might be bugs now.
{- Trying to understand lookups:
lookups "2" [("1",["aap","noot","mies"]), ("2",["vuur","mus"])]
= 
head ([mrgUnion [o] e | (Just e)<-[Data.Map.lookup "2" [("1",["aap","noot","mies"]), ("2",["vuur","mus"])]]]++[["2"]])
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
findIn :: Ord k => k -> Data.Map.Map k [a] -> [a]
findIn t cl = getList (Data.Map.lookup t cl)
                 where getList Nothing = []
                       getList (Just a) = a


nothing :: (Typemap,Typemap)
nothing = (Data.Map.empty,Data.Map.empty)
{-
isFull (TypExpr (Pfull _ _) _) = True
isFull (TypLub a b _) = isFull a && isFull b
isFull (TypGlb a b _) = isFull a && isFull b
isFull _ = False -}
isNull :: Type -> Bool
isNull (TypExpr Pnull _) = True
isNull (TypLub a b _) = isNull a && isNull b
isNull (TypGlb a b _) = isNull a && isNull b
isNull _ = False
infixl 2 .+.   -- concatenate two lists of types
infixl 3 .<.   -- makes a list of one tuple (t,t'), meaning that t is a subset of t'
infixl 3 .=.   -- makes a list of two tuples (t,t') and (t',t), meaning that t is equal to t'
(.<.) :: Type -> Type -> (Typemap , Typemap)
_ .<. Anything = nothing
Nothng .<. _   = nothing
a .<. _ | isNull a = nothing
a .<. b  = (Data.Map.fromList [(a, [b]),(b, [])],snd nothing) -- a tuple meaning that a is a subset of b, and introducing b as a key.
(.=.) :: Type -> Type -> (Typemap, Typemap)
a .=. b  = (Data.Map.fromList [(a, [b]),(b, [a])],snd nothing)
(.++.) :: Typemap -> Typemap -> Typemap
m1 .++. m2  = Data.Map.unionWith mrgUnion m1 m2
(.+.) :: (Typemap , Typemap) -> (Typemap , Typemap) -> (Typemap, Typemap)
(a,b) .+. (c,d) = (c.++.a,d.++.b)
dom, cod :: Term -> Type
dom x    = TypExpr x         False -- the domain of x, and make sure to check subterms of x as well
cod x    = TypExpr (p_flp x) True 
mSpecific, mGeneric :: Type -> Type -> Term -> ( (Typemap , Typemap) ,Type)
mGeneric  a b e = (a .<. r .+. b .<. r , r) where r = normalType (TypLub a b e)
mSpecific a b e = (r .<. a .+. r .<. b , r) where r = normalType (TypGlb a b e)

flattenMap :: Data.Map.Map t [t1] -> [(t, t1)]
flattenMap = Data.Map.foldWithKey (\s ts o -> o ++ [(s,t)|t<-ts]) []
-- alternatively: flattenMap mp = [ (a,b) | (a,bs)<-Data.Map.toList mp , b<-bs])

-- | The purpose of 'typing' is to analyse the domains and codomains of a term in a context.
--   As a result, it builds a list of tuples st::[(Type,Type)], which represents a relation, st,  over Type*Type.
--   For any two Terms a and b,  if 'typing' can tell that dom(a) is a subset of dom(b),
--   this is represented by a tuple (TypExpr a _,TypExpr b _) in st.
--   In the code below, this shows up as  dom a.<.dom b
--   The function typing does a recursive scan through all subterms, collecting all tuples on its way.
--   Besides term term, this function requires a universe in which to operate.
--   Specify 'Anything Anything' if there are no restrictions.
--   If the source and target of term is restricted to concepts c and d, specify (thing c) (thing d).

typing :: P_Context -> ( Typemap                                   -- st          -- the st relation: 'a st b' means that  'dom a' is a subset of 'dom b'
                       , Typemap                                   -- stClos      -- (st\/stAdded)*\/I  (reflexive and transitive)
                       , Typemap                                   -- eqType      -- (st*/\st*~)\/I  (reflexive, symmetric and transitive)
                       , Typemap                                   -- stClosAdded -- additional links added to stClos
                       , Typemap                                   -- stClos1     -- st*  (transitive)
                       , Data.Map.Map Type [(P_Declaration,[P_Concept],[P_Concept])] -- bindings    -- declarations that may be bound to relations
                       , Type -> [P_Concept]                       --  srcTypes -- 
                       , [(P_Concept,P_Concept)]                   -- isas        -- 
                       )                                   
typing p_context
  = ( st
    , stClos
    , eqType
    , stClosAdded
    , stClos1
    , bindings -- for debugging: (error.concat) ["\n  "++show b | b<-Data.Map.toAscList bindings] -- 
    , srcTypes
  -- isas is produced for the sake of making a partial order of concepts in the A-structure.
    , isas   -- a list containing all tuples of concepts that are in the subset relation with each other.
             -- it is used for the purpose of making a poset of concepts in the A-structure.
    ) 
 where
   -- The story: two Typemaps are made by uType, each of which contains tuples of the relation st.
   --            These are converted into two maps (each of type Typemap) for efficiency reasons.
     (firstSetOfEdges,secondSetOfEdges)
      = uType (p_declarations p_context, compat) p_context Anything Anything p_context
     compat :: Type -> Type -> Bool
     compat a b = null set1 -- approve compatibility for isolated types, such as "Nothng", "Anything" and singleton elements
                      || null set2
                      || (not.null) (set1 `isc` set2)
      where -- TODO: we can calculate scc with a Dijkstra algorithm, which is much faster than the current closure used:
       included = addIdentity (setClosure (reverseMap firstSetOfEdges) "firstSetOfEdges~")
       set1 = findIn a included
       set2 = findIn b included
   -- together, the firstSetOfEdges and secondSetOfEdges form the relation st
     st = Data.Map.unionWith mrgUnion firstSetOfEdges secondSetOfEdges
     typeTerms :: [Type]          -- The list of all type terms in st.
     typeTerms = Data.Map.keys st -- Because a Typemap is total, it is sufficient to compute  Data.Map.keys st
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
        f dataMap o@(TypGlb a b _) = Data.Map.map (\cs -> mrgUnion cs [e | a `elem` cs, b `elem` cs, e<-lookups o dataMap]) dataMap
        --        f dataMap o@(TypGlb a b _) = Data.Map.map (\cs -> mrgUnion cs [o| a `elem` cs, b `elem` cs]) dataMap
        f  _ o = fatal 406 ("Inexhaustive pattern in f in stClosAdded in tableOfTypes: " ++show o)
        f' dataMap o@(TypLub a b _) = Data.Map.map (\cs -> mrgUnion cs [e | a `elem` cs, b `elem` cs, e<-lookups o dataMap]) dataMap
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
                 stClosAdded else fatal 358 "stClosAdded should be transitive and reflexive"  -- stClos = stClosAdded*\/I, so stClos is transitive (due to setClosure).
     stClosReversed = reverseMap stClos  -- stClosReversed is transitive too and like stClos, I is a subset of stClosReversed.
     eqType = Data.Map.intersectionWith mrgIntersect stClos stClosReversed  -- eqType = stAdded* /\ stAdded*~ i.e there exists a path from a to be and a path from b.
     isaClos = Data.Map.fromDistinctAscList [(c,[c' | TypExpr (Pid c') _<-ts]) | (TypExpr (Pid c) _, ts)<-Data.Map.toAscList stClos]
     isaClosReversed = reverseMap isaClos
     isas = [ (s,g) | s<-Data.Map.keys isaClos, g<-lkp s ]  -- there are too many tuples in isas. Because isas=isas*, it may contain tuples (s,g) for which there exists q and (s,q) and (q,g) are in isas.
            where lkp c = case Data.Map.lookup c isaClos of
                           Just cs -> cs
                           _ -> fatal 379 ("P_Concept "++show c++" was not found in isaClos")
     stConcepts :: Data.Map.Map Type [P_Concept]
     stConcepts = Data.Map.map f stClos
                  where f ts = [ (fst.head.sortWith (length.snd)) [(c,lkp c) | c<-cl]
                               | cl<-eqClass compatible cs]
                               where cs = [c | TypExpr (Pid c) _<-ts] -- all concepts reachable from one type term
                                     lkp c = case Data.Map.lookup c isaClosReversed of
                                              Just cs' -> cs'
                                              _ -> fatal 387 ("P_Concept "++show c++" was not found in isaClosReversed")
     srcTypes :: Type -> [P_Concept]
     srcTypes typ = case Data.Map.lookup typ stConcepts of
                     Just cs -> cs
                     _ -> fatal 391 ("Type "++show typ++" was not found in stConcepts."++concat ["\n  "++show b | b<-Data.Map.toAscList stConcepts ])
     compatible a b = (not.null) (lkp a `isc` lkp b)
      where lkp c = case Data.Map.lookup c isaClosReversed of
                     Just cs -> cs
                     _ -> fatal 395 ("P_Concept "++show c++" was not found in isaClosReversed")
     tGlb :: Type -> Type -> [P_Concept]
     tGlb a b = nub [c | t<-greatest (lkp a `isc` lkp b), c<-srcTypes t]
      where lkp c = case Data.Map.lookup c stClosReversed of
                     Just cs -> cs
                     _ -> fatal 400 ("P_Concept "++show c++" was not found in stClosReversed")
     greatest :: [Type] -> [Type]
     greatest ts = foldr isc ts [ lkp t | t<-ts]
      where lkp t = case Data.Map.lookup t stClos of
                     Just cs -> cs
                     _ -> fatal 405 ("P_Concept "++show t++" was not found in isaClosReversed")
{- Bindings:
Relations that are used in a term must be bound to declarations. When they have a type annotation, as in r[A*B], there is no  problem.
When it is just 'r', and there are multiple declarations to which it can be bound, the type checker must choose between candidates.
Sometimes, there is only one candidate, even though the type checker cannot prove it correct (i.e. the domain of the term is a subset of the candidate type).
In such cases, we want to give that candidate to the user by way of suggestion to resolve the type conflict.
-}
     bindings :: Data.Map.Map Type [(P_Declaration,[P_Concept],[P_Concept])]
     bindings    = Data.Map.fromListWith union
                   ( [ tuple
                     | decl<-p_declarations p_context
                     , let P_Sign sgn = dec_sign decl; srce=head sgn; targ=last sgn
                     , dTerm@(TypExpr (Prel _ dnm) _) <- typeTerms, dnm==dec_nm decl
                     , cTerm@(TypExpr (Pflp _ cnm) _) <- typeTerms, cnm==dec_nm decl
                     , let srcConcepts = thing srce `tGlb` dTerm, not (null srcConcepts)
                     , let trgConcepts = thing targ `tGlb` cTerm, not (null trgConcepts)
                     , let declSrcTrgs = (decl, srcConcepts, trgConcepts)
                     , tuple<-[(dTerm,[declSrcTrgs]),(cTerm,[declSrcTrgs])]
                     ] ++
                     [ (t,[]) | t<-typeTerms]
                   )

-- The TypGlb types are sorted to ensure that terms like ((a ./\. b) ./\. c) are handled from the inside out. In our example (a ./\. b) comes first.

{- The following function draws two graphs for educational or debugging purposes. If you want to see them, run Ampersand --typing.
-}
typeAnimate :: Typemap -> Typemap -> Typemap -> Typemap -> Typemap -> (DotGraph String,DotGraph String)
typeAnimate st stClos eqType stClosAdded stClos1 = (stTypeGraph, eqTypeGraph)
   where
     -- testTable = concat [ "\n  "++show (stNr t, eqNr t, t, map stNr eqs, map eqNr eqs)| (t,eqs)<-Data.Map.toAscList eqType]
{- The set st contains the essence of the type analysis. It contains tuples (t,t'),
   each of which means that the set of atoms contained by t is a subset of the set of atoms contained by t'. -}
     typeTerms = Data.Map.keys stClos -- stClos contains more than st, because some terms were added through stClosAdded.
     stNr :: Type -> Int
     stNr typ = case Data.Map.lookup typ stTable of
                 Just x -> x
                 _ -> fatal 529 ("Element "++show typ++" not found in stNr")
      where
       stTable = Data.Map.fromAscList [(t,i) | (i,t)<-zip [0..] typeTerms ]
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
     eqNr typ = case Data.Map.lookup typ (Data.Map.fromList [(t,i) | (i,cl)<-zip [0..] eqClasses, t<-cl ]) of
                 Just x -> x
                 _ -> fatal 544 ("Element "++show typ++" not found in eqNr")
     eqClasses :: [[Type]]             -- The strongly connected components of stGraph
     eqClasses = nub (Data.Map.elems eqType)

     eqTypeGraph :: DotGraph String
     eqTypeGraph = toDotGraph showVtx show [0..length eqClasses-1] [] condensedEdges condensedEdges2
      where showVtx n = (intercalate "\n".map showType.nub) [  typTerm| typTerm<-typeTerms, n==eqNr typTerm]
{- This function was used to find the original expression from the user script....
     original :: Type -> Type
     original t@(TypExpr (Pid _)     _) = t
     original t@(TypExpr (Pfull _ _) _) = t
     original (TypExpr t b)
      = case Data.Map.lookup (origin t) origMap of
                   Just term -> TypExpr (if b then p_flp term else term) b
                   Nothing   -> fatal 586 ("wrong argument for original ("++show t++")")
        where origMap = Data.Map.fromList [ (origin subTerm,subTerm) | subTerm<-subterms p_context ]
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
     nr t = case Data.Map.lookup t eqType of
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
  uType :: ([P_Declaration], Type -> Type -> Bool)  -- The declaration table from the script, a compatibility test.
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
 uType dcls _ uLft uRt pContext
  = (let x=ctx_pats  pContext in uType dcls x uLft uRt x) .+.
    (let x=ctx_PPrcs pContext in uType dcls x uLft uRt x) .+.
    (let x=ctx_rs    pContext in uType dcls x uLft uRt x) .+.
    (let x=ctx_ds    pContext in uType dcls x uLft uRt x) .+.
    (let x=ctx_ks    pContext in uType dcls x uLft uRt x) .+.
    (let x=ctx_gs    pContext in uType dcls x uLft uRt x) .+.
    (let x=ctx_ifcs  pContext in uType dcls x uLft uRt x) .+.
    (let x=ctx_sql   pContext in uType dcls x uLft uRt x) .+.
    (let x=ctx_php   pContext in uType dcls x uLft uRt x) .+.
    (let x=ctx_pops  pContext in uType dcls x uLft uRt x)
         
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
         terms (pt_pop pPattern)
        )
 uType dcls _ uLft uRt pPattern
  = (let x=pt_rls pPattern in uType dcls x uLft uRt x) .+.
    (let x=pt_gns pPattern in uType dcls x uLft uRt x) .+.
    (let x=pt_dcs pPattern in uType dcls x uLft uRt x) .+.
    (let x=pt_kds pPattern in uType dcls x uLft uRt x) .+.
    (let x=pt_pop pPattern in uType dcls x uLft uRt x)

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
         terms (procPop   pProcess)
        )
 uType dcls _ uLft uRt pProcess
  = (let x=procRules pProcess in uType dcls x uLft uRt x) .+.
    (let x=procGens  pProcess in uType dcls x uLft uRt x) .+.
    (let x=procDcls  pProcess in uType dcls x uLft uRt x) .+.
    (let x=procKds   pProcess in uType dcls x uLft uRt x) .+.
    (let x=procPop   pProcess in uType dcls x uLft uRt x)

instance Expr P_Rule where
 terms r = terms (rr_exp r)++terms (rr_viol r)
 p_rules r = [r]
 uType dcls _ uLft uRt r
  = let x=rr_exp r; v=rr_viol r in
    uType dcls x uLft uRt x .+. uType dcls v (dom x) (cod x) v

instance Expr P_PairView where
 terms (P_PairView segments) = terms segments
 uType dcls _ uLft uRt (P_PairView segments) = uType dcls segments uLft uRt segments

instance Expr P_PairViewSegment where
 terms (P_PairViewExp _ term) = [term]
 terms _                      = []
 uType dcls _ uLft _   (P_PairViewExp Src term) = uType dcls term t Anything term .+. dm
  where (dm,t) = mSpecific (dom term) uLft term
 uType dcls _ _    uRt (P_PairViewExp Tgt term) = uType dcls term t Anything term .+. dm
  where (dm,t) = mSpecific (dom term) uRt term
 uType _ _ _ _ _ = nothing
  
instance Expr P_KeyDef where
 terms k = terms [ks_obj keyExpr | keyExpr@P_KeyExp{} <- kd_ats k]
 p_keys k = [k]
 uType dcls _ uLft uRt k
  = let x=Pid (kd_cpt k) in
    uType dcls x uLft uRt x .+.
    foldr (.+.) nothing [ uType dcls obj t Anything obj .+. dm
                        | P_KeyExp obj <- kd_ats k
                        , let (dm,t) = mSpecific (dom x) (dom (obj_ctx obj)) (obj_ctx obj)
                        ]
    where 

instance Expr P_Interface where
 terms ifc = terms (ifc_Obj ifc)
 uType dcls _ uLft uRt ifc = let x=ifc_Obj ifc in uType dcls x uLft uRt x

instance Expr P_ObjectDef where
 terms o = [obj_ctx o | null (terms (obj_msub o))]++terms [PCps (origin e) (obj_ctx o) e | e<-terms (obj_msub o)]
 uType dcls _ uLft uRt o
  = let x=obj_ctx o in
    uType dcls x uLft uRt x .+.
    foldr (.+.) nothing [ uType dcls obj t Anything obj .+. dm
                        | Just subIfc <- [obj_msub o], obj <- si_box subIfc
                        , let (dm,t) = mSpecific (cod x) (dom (obj_ctx obj)) (obj_ctx obj)
                        ]
 
instance Expr P_SubInterface where
 terms x@(P_Box{}) = terms (si_box x)
 terms _           = []
 uType dcls _ uLft uRt mIfc@(P_Box{}) = let x=si_box mIfc in uType dcls x uLft uRt x
 uType _    _ _    _   _              = nothing

instance Expr P_Sign where
 terms _ = []
 uType _ _ _ _ _ = nothing

instance Expr P_Gen where
 terms g = [Pimp (origin g) (Pid (gen_spc g)) (Pid (gen_gen g))]
 uType dcls _ uLft uRt g
  = let x=Pimp (origin g) (Pid (gen_spc g)) (Pid (gen_gen g)) in uType dcls x uLft uRt x

instance Expr P_Declaration where
 terms d = [PTyp (origin d) (Prel (origin d) (dec_nm d)) (dec_sign d)]
 uType _ _ _ _ d
  = dom decl.<.thing src .+. cod decl.<.thing trg
    where [decl] = terms d
          P_Sign sgn = dec_sign d
          src = head sgn; trg = last sgn
   -- let x=PTyp (origin d) (Prel (origin d) (dec_nm d)) (dec_sign d) in uType dcls x uLft uRt x

instance Expr P_Population where
 terms pop@(P_Popu{p_type=P_Sign []}) = [Prel (p_orig pop) (p_popm pop)]
 terms pop@(P_Popu{})                 = [PTyp (p_orig pop) (Prel (p_orig pop) (p_popm pop)) (p_type pop)]
 terms pop@(P_CptPopu{})              = [Pid (PCpt (p_popm pop))]
 uType dcls _ uLft uRt pop = let x=terms pop in  uType dcls x uLft uRt x

instance Expr a => Expr (Maybe a) where
 terms Nothing  = []
 terms (Just x) = terms x
 uType _    _ _    _   Nothing  = nothing
 uType dcls _ uLft uRt (Just x) = uType dcls x uLft uRt x

instance Expr a => Expr [a] where
 terms = concat.map terms
 uType dcls _ uLft uRt xs = foldr (.+.) nothing [ uType dcls x uLft uRt x | x <- xs]

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
 subterms e@(PTyp _ a _) = [e]++subterms a
 subterms e              = [e]
 
 uType dcls x uLft uRt expr 
  = case expr of
     Pnid{}       -> fatal 136 "Pnid has no representation"
     PI{}         -> dom x.=.cod x .+. dom x.<.uLft .+. cod x.<.uRt             -- I
     Pid{}        -> dom x.=.cod x                                              -- I[C]
     (Patm _ _ []) -> dom x.=.cod x .+. dom x.<.uLft .+. cod x.<.uRt      -- 'Piet'   (an untyped singleton)
     (Patm _ _ cs) -> dom x.<.thing (head cs) .+. cod x.<.thing (last cs) -- 'Piet'[Persoon]  (a typed singleton)
                       .+. dom x.=.cod x
     Pnull        ->nothing                                                  -- -V     (the empty set)
     PVee{}       -> dom x.<.uLft .+. cod x.<.uRt
     (Pfull s t)  -> dom x.=.dom (Pid s) .+. cod x.=.cod (Pid t)                          --  V[A*B] (the typed full set)
     (Pequ _ a b) -> dom a.=.dom x .+. cod a.=.cod x .+. dom b.=.dom x .+. cod b.=.cod x    --  a=b    equality
                     .+. uType dcls a (dom x) (cod x) a .+. uType dcls b (dom x) (cod x) b 
     (PIsc _ a b) -> let (dm,interDom)  = mSpecific (dom a) (dom b)  x
                         (cm,interCod)  = mSpecific (cod a) (cod b)  x
                         (d2,interDom2) = mSpecific interDom uLft  x
                         (c2,interCod2) = mSpecific interCod uRt   x
                     in dom x.=.interDom .+. cod x.=.interCod    --  intersect ( /\ )
                        .+. dm .+. cm .+. d2 .+. c2 -- d2 and c2 are needed for try15
                        .+. uType dcls a interDom2 interCod2 a .+. uType dcls b interDom2 interCod2 b
     (PUni _ a b) -> let (dm,interDom) = mGeneric (dom a) (dom b)  x
                         (cm,interCod) = mGeneric (cod a) (cod b)  x
                         (d2,interDom2) = mSpecific interDom uLft  x
                         (c2,interCod2) = mSpecific interCod uRt   x
                     in dom x.=.interDom .+. cod x.=.interCod    --  union     ( \/ )
                        .+. dm .+. cm .+. d2 .+. c2
                        .+. uType dcls a interDom2 interCod2 a .+. uType dcls b interDom2 interCod2 b
     (PDif _ a b) -> let (dm,interDom) = mSpecific uLft (dom a) x
                         (cm,interCod) = mSpecific uRt  (cod a) x
                     in dom x.<.dom a .+. cod x.<.cod a                                        --  a-b    (difference)
                        .+. dm .+. cm
                        .+. uType dcls a uLft uRt a
                        .+. uType dcls b interDom interCod b
     (PCps _ a b) -> let (bm,between) = mSpecific (cod a) (dom b) x
                         pidTest (PI{}) r = r
                         pidTest (Pid{}) r = r
                         pidTest _ _ = nothing
                     in dom x.<.dom a .+. cod x.<.cod b .+.                                    -- a;b      composition
                        bm .+. uType dcls a uLft between a .+. uType dcls b between uRt b
                        .+. pidTest a (dom x.<.dom b) .+. pidTest b (cod x.<.cod a)
-- PRad is the De Morgan dual of PCps. However, since PUni and UIsc, mGeneric and mSpecific are not derived, neither can PRad
     (PRad _ a b) -> let (bm,between) = mGeneric (cod a) (dom b) x
                         pnidTest (PCpl _ (PI{})) r = r
                         pnidTest (PCpl _ (Pid{})) r = r
                         pnidTest (Pnid{}) r = r
                         pnidTest _ _ = nothing
                     in dom a.<.dom x .+. cod b.<.cod x .+.                                    -- a!b      relative addition, dagger
                        bm .+. uType dcls a uLft between a .+. uType dcls b between uRt b
                        .+. pnidTest a (dom b.<.dom x) .+. pnidTest b (cod a.<.cod x)
     (PPrd _ a b) -> dom x.=.dom a .+. cod x.=.cod b                                        -- a*b cartesian product
                     .+. uType dcls a uLft Anything a .+. uType dcls b Anything uRt b
     (PCpl o a)   -> let c = normalType (TypLub (dom a) (dom t) x)
                         c'= normalType (TypLub (cod a) (cod t) x)
                         t = complement a
                     in dom x.<.c .+. cod x.<.c' .+.
                        dom a.<.c .+. cod a.<.c' .+.
                        uType dcls x uLft uRt (PDif o (PVee o) a)
     (PKl0 _ e)   -> dom e.<.dom x .+. cod e.<.cod x .+. uType dcls e uLft uRt e
     (PKl1 _ e)   -> dom e.<.dom x .+. cod e.<.cod x .+. uType dcls e uLft uRt e
     (PFlp _ e)   -> cod e.=.dom x .+. dom e.=.cod x .+. uType dcls e uRt uLft e
     (PBrk _ e)   -> dom x.=.dom e .+. cod x.=.cod e .+.
                    uType dcls x uLft uRt e                                                     -- (e) brackets
     (PTyp _ e sgn) ->
       case sgn of
         (P_Sign [])        -> fatal 196 "P_Sign is empty"
         (P_Sign (_:_:_:_)) -> fatal 197 "P_Sign too large"  
         (P_Sign cs)        -> let declarationTable = fst dcls
                                   iSrc = thing (head cs)
                                   iTrg = thing (last cs)
                                   y=complement x
                                   decls nm = [term | decl<-declarationTable, name decl==nm, dec_sign decl == sgn, term<-terms decl ] 
                               in case e of
                                 (Prel _ nm) -> dom x.<.iSrc  .+. cod x.<.iTrg  .+.
                                                dom x.<.dom e .+. cod x.<.cod e .+.
                                                case decls nm of
                                                  [d] -> dom x.=.dom d .+. cod x.=.cod d .+. dom y.=.dom d .+. cod y.=.cod d
                                                  _   -> nothing
                                 _           -> dom x.<.iSrc  .+. cod x.<.iTrg  .+.                                    -- e[A*B]  type-annotation
                                                dom x.<.dom e .+. cod x.<.cod e .+.
                                                uType dcls e iSrc iTrg e

     (Prel _ nm) -> let declarationTable = fst dcls -- keep this lazy
                        compatible = snd dcls       -- keep this lazy. Do not turn into a pattern, because that would cause a loop. Patterns are strict...
                        y=complement x
                        decls' = [term | decl<-declarationTable, name decl==nm, term<-terms decl ]
                        decls  = if length decls' == 1 then decls' else
                                 [decl | decl@(PTyp _ (Prel _ _) (P_Sign cs@(_:_)))<-decls'
                                       , uLft == thing (head cs)
                                       , uRt  == thing (last cs) ]
                        {- to exclude the possibility that the following pattern matching causes a loop, it is rewritten fully lazily.
                        spcls  = case (dSrcs, dTrgs) of
                                      ( []  ,  []  ) -> []
                                      ( [d] ,  []  ) -> [d]
                                      ( []  ,  [d] ) -> [d]
                                      ( [d] ,  _   ) -> [d]
                                      ( _   ,  [d] ) -> [d]
                                      ( _   ,  _   ) -> dSrcs `isc` dTrgs
                        -}
                        spcls = if null dSrcs && null dTrgs then [] else
                                if null dTrgs  then [head dSrcs ] else
                                if null dSrcs  then [head dTrgs ] else
                                if null dSrcs' then [head dSrcs'] else
                                if null dTrgs' then [head dTrgs'] else
                                dSrcs `isc` dTrgs
                                where dSrcs' = tail dSrcs
                                      dTrgs' = tail dTrgs
                        dSrcs, dTrgs ::  [Term]
                        dSrcs  = [decl | decl@(PTyp _ (Prel _ _) (P_Sign (_:_)))<-decls'
                                       , compatible uLft (dom decl)   -- this is compatibility wrt firstSetOfEdges, thus avoiding a computational loop
                                       ]
                        dTrgs  = [decl | decl@(PTyp _ (Prel _ _) (P_Sign (_:_)))<-decls'
                                       , compatible uRt  (cod decl)
                                       ]
                        carefully :: (Typemap , Typemap ) -> (Typemap, Typemap)
                        carefully tt = (fst nothing,fst tt.++.snd tt)
                     in -- WHY is:
                        -- dom x.<.uLft .+. cod x.<.uRt 
                        -- correct for PVee but not for Prel?
                        -- Explanation:
                        -- In the case of PVee, we decide to change the occurrence of PVee for a different one, and choose to pick the smallest such that the end-result will not change
                        -- In the case of Prel, we cannot decide to change the occurrence, since sharing occurs. More specifically, the statement is simply not true.
                        if length decls==1
                        then let d=head decls in dom x.=.dom d .+. cod x.=.cod d .+. dom y.=.dom d .+. cod y.=.cod d
                        else if False -- length spcls==1 -- if you replace this condition with 'False', the loop will disappear
                             then let c=head spcls in
                                  carefully ( -- what is to come will use the first iteration of edges, so to avoid loops, we carefully only create second edges instead
                                             dom x.=.dom c .+. cod x.=.cod c .+. dom y.=.dom c .+. cod y.=.cod c
                                            )
                             else nothing
     (Pflp o nm) -> let e = Prel o nm
                    in dom x.=.cod e .+. cod x.=.dom e .+. uType dcls e uRt uLft e
 -- derived uTypes: the following do no calculations themselves, but merely rewrite terms to the ones we covered
{-     (PRad o a b) -> let e = complement (PCps o (complement a) (complement b))
                     in dom x.=.dom e .+. cod x.=.cod e .+.
                        uType dcls x uLft uRt e                 --  a!b     relative addition, dagger
-}
     (Pimp o a b) -> let e = Pequ o a (PIsc o a b)
                     in dom x.=.dom e .+. cod x.=.cod e .+.
                        uType dcls x uLft uRt e                 --  a|-b    implication (aka: subset)
     (PLrs o a b) -> let e = complement (PCps o (complement a) (p_flp b))
                     in dom x.=.dom e .+. cod x.=.cod e .+.
                        uType dcls x uLft uRt e                 --  a/b = a!-b~ = -(-a;b~)
     (PRrs o a b) -> let e = complement (PCps o (p_flp a) (complement b))
                     in dom x.=.dom e .+. cod x.=.cod e .+.
                        uType dcls x uLft uRt e                 --  a\b = -a~!b = -(a~;-b)

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x
snd3 :: (a,b,c) -> b
snd3 (_,y,_) = y
thd3 :: (a,b,c) -> c
thd3 (_,_,z) = z

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

data Guarded a = Errors [CtxError] | Checked a


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


-- careful: only use when no references to a are made while computing b.
-- even then, it is better to avoid (a, [CtxError]) all together in favor of using Guarded directly.
reZip :: (a, [CtxError]) -> Guarded a
reZip (a,b) = if null b then Checked a else Errors b

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
 = (reZip(contxt,typeErrors)
   ,stTypeGraph,eqTypeGraph)
   where
    contxt = 
         ACtx{ ctxnm     = name p_context     -- The name of this context
             , ctxpos    = ctx_pos p_context
             , ctxlang   = fromMaybe Dutch (ctx_lang p_context)
             , ctxmarkup = fromMaybe ReST  (ctx_markup p_context) -- The default markup format for free text in this context
             , ctxpo     = gEandClasses
             , ctxthms   = ctx_thms p_context -- The patterns/processes to be printed in the functional specification. (for making partial documentation)
             , ctxpats   = pats          -- The patterns defined in this context
                                         -- Each pattern contains all user defined rules inside its scope
             , ctxprocs  = procs         -- The processes defined in this context
             , ctxrs     = ctxrules
             , ctxds     = adecs         -- The declarations defined in this context, outside the scope of patterns
             , ctxcds    = acds          -- All concept definitions
             , ctxks     = keys          -- The key definitions defined in this context, outside the scope of patterns
             , ctxgs     = agens         -- The gen definitions defined in this context, outside the scope of patterns
             , ctxifcs   = ifcs          -- The interfaces defined in this context, outside the scope of patterns
             , ctxps     = apurp         -- The purposes defined in this context, outside the scope of patterns
             , ctxsql    = sqlPlugs      -- user defined sqlplugs, taken from the Ampersand script
             , ctxphp    = phpPlugs      -- user defined phpplugs, taken from the Ampersand script
             , ctxenv    = (ERel(V (Sign ONE ONE)) ,[])
             , ctxmetas  = [ Meta pos metaObj nm val | P_Meta pos metaObj nm val <- ctx_metas p_context ]
             , ctxatoms  = allExplicitAtoms
             }
    st, eqType  :: Typemap                  -- eqType = (st*/\st*~)\/I  (total, reflexive, symmetric and transitive)
    bindings    :: Data.Map.Map Type [(P_Declaration,[P_Concept],[P_Concept])]         -- declarations that may be bound to relations, intended as a suggestion to the programmer
    isas        :: [(P_Concept,P_Concept)]                   -- 
    (st, stClos, eqType, stClosAdded, stClos1 , bindings, srcTypes, isas) = typing p_context
    specializationTuples :: [(A_Concept,A_Concept)]
    specializationTuples = [(pCpt2aCpt specCpt,pCpt2aCpt genCpt) | (specCpt, genCpt)<-isas]
    gEandClasses :: (A_Concept->A_Concept->DatabaseDesign.Ampersand.Core.Poset.Ordering, [[A_Concept]])
    gEandClasses = DatabaseDesign.Ampersand.Core.Poset.makePartialOrder specializationTuples   -- The base hierarchy for the partial order of concepts (see makePartialOrder)
    typeErrors :: [CtxError]
    typeErrors
     | (not.null) derivedEquals = derivedEquals
     | (not.null) cxerrs        = cxerrs
     | otherwise                = postchks
    derivedEquals :: [CtxError]
    derivedEquals   -- These concepts can be proven to be equal, based on st (= typing sentences, i.e. the terms derived from the script).
     = [ CxeEqConcepts eqs
       | (TypExpr (Pid{}) _, equals)<-Data.Map.toAscList eqType
       , let eqs=[c | TypExpr (Pid c) _<-equals ]
       , length eqs>1]
    (stTypeGraph,eqTypeGraph) = typeAnimate st stClos eqType stClosAdded stClos1
    cxerrs = patcxes++rulecxes++keycxes++interfacecxes++proccxes++sPlugcxes++pPlugcxes++popcxes++deccxes++xplcxes++themeschk
    --postchcks are those checks that require null cxerrs 
    postchks = rulenmchk ++ ifcnmchk ++ patnmchk ++ cyclicInterfaces
    acds = ctx_cs p_context++concatMap pt_cds (ctx_pats p_context)++concatMap procCds (ctx_PPrcs p_context)
    agens = map (pGen2aGen "NoPattern") (ctx_gs p_context)
    (adecs,   deccxes)   = case (parallelList . map (pDecl2aDecl allpops)     . ctx_ds   ) p_context of
                            Checked decs -> ([d{decpat="NoPattern"} | d<-decs], [])
                            Errors  errs -> (fatal 930 "Do not refer to undefined declarations", errs)
    (apurp,   xplcxes)   = case (parallelList . map  pPurp2aPurp              . ctx_ps   ) p_context of
                            Checked purps -> (purps, [])
                            Errors  errs  -> (fatal 938 "Do not refer to undefined purposes", errs)
    (pats,    patcxes)   = case (parallelList . map (pPat2aPat   allpops)     . ctx_pats ) p_context of
                            Checked pats' -> (pats', [])
                            Errors  errs  -> (fatal 938 "Do not refer to undefined patterns", errs)
    (procs,   proccxes)  = case (parallelList . map (pProc2aProc allpops)     . ctx_PPrcs) p_context of
                            Checked prcs -> (prcs, [])
                            Errors errs  -> (fatal 938 ("Do not refer to undefined processes" ++ show errs), errs)
    (ctxrules,rulecxes)  = case (parallelList . map (pRul2aRul "NoPattern")   . ctx_rs   ) p_context of
                            Checked ruls -> (ruls, [])
                            Errors errs  -> (fatal 938 "Do not refer to undefined rules", errs)
    (keys,    keycxes)   = case (parallelList . map pKDef2aKDef               . ctx_ks   ) p_context of
                            Checked keys -> (keys, [])
                            Errors errs  -> (fatal 938 "Do not refer to undefined keys", errs)
    (ifcs,interfacecxes) = case (parallelList . map  pIFC2aIFC                . ctx_ifcs ) p_context of
                            Checked ifcs -> (ifcs, [])
                            Errors errs  -> (fatal 938 "Do not refer to undefined interfaces", errs)
    (sqlPlugs,sPlugcxes) = case (parallelList . map (pODef2aODef [] Anything) . ctx_sql  ) p_context of
                            Checked plugs -> (plugs, [])
                            Errors errs   -> (fatal 951 "Do not refer to undefined sqlPlugs", errs)
    (phpPlugs,pPlugcxes) = case (parallelList . map (pODef2aODef [] Anything) . ctx_php  ) p_context of
                            Checked plugs -> (plugs, [])
                            Errors errs   -> (fatal 954 "Do not refer to undefined phpPlugs", errs)
    (allpops, popcxes)   = case (parallelList . map  pPop2aPop                . pops     ) p_context of
                            Checked pops -> (pops, [])
                            Errors errs  -> (fatal 957 "Do not refer to undefined populations", errs)
    allExplicitAtoms :: [(String,[String])]
    allExplicitAtoms = [(p_popm cptos',[a | (a,_)<-p_popps cptos']) | cptos'@P_CptPopu{}<-pops p_context]
    pops pc
     = ctx_pops pc ++
       [ pop | pat<-ctx_pats pc,  pop<-pt_pop pat] ++
       [ pop | prc<-ctx_PPrcs pc, pop<-procPop prc]
    themeschk = case orphans of
                 []   -> []
                 [nm] -> [newcxe ("Theme '"++nm++"' is selected for output, but is not defined.")]
                 _    -> [newcxe ("The following themes are selected for output, but are not defined:\n   "++intercalate ", " orphans)]
                where orphans = ctxthms contxt>-themenames
                      themenames=[name p |p<-pats]++[name p |p<-procs]
    rulenmchk = [ newcxe ("Rules with identical names at positions "++show(map origin cl))
                | cl<-eqCl name (rules contxt),length cl>1]
    ifcnmchk  = [newcxe ("Interfaces with identical names at positions "++show(map origin cl))
                | cl<-eqCl name ifcs,length cl>1]
    patnmchk  = [newcxe ("Patterns or processes with identical names at positions "++show(map fst cl))
                | cl<-eqCl snd (zip (map origin pats++map origin procs)
                                    (map name   pats++map name   procs)),length cl>1]
    cyclicInterfaces = [ newcxe $ "These interfaces form a reference cycle:\n" ++
                                  unlines [ "- " ++ show ifcNm ++ " at " ++ show (origin $ lookupInterface ifcNm)
                                          | ifcNm <- iCycle ]
                       | iCycle <- getCycles refsPerInterface ]
      where refsPerInterface = [(ifcName ifc, getDeepIfcRefs $ ifcObj ifc) | ifc <- ifcs ]
            getDeepIfcRefs obj = case objmsub obj of
                                   Nothing                -> []
                                   Just (InterfaceRef nm) -> [nm]
                                   Just (Box objs)        -> concatMap getDeepIfcRefs objs
            lookupInterface nm = case [ ifc | ifc <- ifcs, ifcName ifc == nm ] of
                                   [ifc] -> ifc
                                   _     -> fatal 124 "Interface lookup returned zero or more than one result"

    pPat2aPat :: [Population] -> P_Pattern -> Guarded Pattern
    pPat2aPat pops' ppat 
     = case typeErrs of
        [] -> Checked (A_Pat { ptnm  = name ppat    -- Name of this pattern
                             , ptpos = pt_pos ppat  -- the position in the file in which this pattern was declared.
                             , ptend = pt_end ppat  -- the position in the file in which this pattern was declared.
                             , ptrls = prules       -- The user defined rules in this pattern
                             , ptgns = agens'       -- The generalizations defined in this pattern
                             , ptdcs = adecs'       -- The declarations declared in this pattern
                             , ptkds = keys'        -- The key definitions defined in this pattern
                             , ptxps = xpls         -- The purposes of elements defined in this pattern
                             } )
        _  -> Errors [CxeOrig typeErrs "pattern" (name ppat) (origin ppat) | (not.null) typeErrs]
       where
        typeErrs = rulecxes'++keycxes'++deccxes'++xplcxes'
        (prules,rulecxes') = case (parallelList . map (pRul2aRul (name ppat)) .pt_rls) ppat of
                              Checked ruls -> (ruls, [])
                              Errors errs  -> (fatal 995 "Do not refer to undefined rules", errs)
        (keys',keycxes')   = case (parallelList . map pKDef2aKDef .pt_kds) ppat of
                              Checked keys -> (keys, [])
                              Errors errs  -> (fatal 998 "Do not refer to undefined keys", errs)
        (adecs',deccxes')  = case (parallelList . map (pDecl2aDecl pops') . pt_dcs) ppat of
                              Checked decs -> ([d{decpat=name ppat} | d<-decs], [])
                              Errors  errs -> (fatal 1001 "Do not refer to undefined declarations", errs)
        (xpls,xplcxes')    = case (parallelList . map pPurp2aPurp . pt_xps) ppat of
                              Checked purps -> (purps, [])
                              Errors  errs  -> (fatal 1005 "Do not refer to undefined purposes", errs)
        agens'  = map (pGen2aGen (name ppat)) (pt_gns ppat)

    pProc2aProc :: [Population] -> P_Process -> Guarded Process
    pProc2aProc pops' pproc
     = case typeErrs of
        [] -> Checked(Proc { prcNm    = procNm pproc
                           , prcPos   = procPos pproc
                           , prcEnd   = procEnd pproc
                           , prcRules = prules
                           , prcGens  = agens'          -- The generalizations defined in this pattern
                           , prcDcls  = adecs'          -- The declarations declared in this pattern
                           , prcRRuls = arruls          -- The assignment of roles to rules.
                           , prcRRels = arrels          -- The assignment of roles to Relations.
                           , prcKds   = keys'           -- The key definitions defined in this process
                           , prcXps   = expls          -- The purposes of elements defined in this process
                           } )
        _  -> Errors [CxeOrig typeErrs "process" (name pproc) (origin pproc)]
       where
        typeErrs = concat ([rulecxes']++[keycxes']++[deccxes']++[rrcxes]++editcxes++[explcxes])
        (prules,rulecxes') = case (parallelList . map (pRul2aRul (name pproc)) . procRules) pproc of
                              Checked ruls -> (ruls, [])
                              Errors errs  -> (fatal 1025 "Do not refer to undefined rules", errs)
        arrels = [(rol,rel) |rr<-rrels, rol<-rrRoles rr, rel<-rrRels rr]
        (rrels,editcxes)  = (unzip . map pRRel2aRRel            . procRRels) pproc
        agens'  = map (pGen2aGen (name pproc)) (procGens pproc)
        arruls = [(rol,rul) |rul<-rules contxt, rr<-rruls, name rul `elem` mRules rr, rol<-mRoles rr]
        (adecs',deccxes') = case (parallelList . map (pDecl2aDecl pops') . procDcls) pproc of
                             Checked decs -> ([d{decpat=name pproc} | d<-decs], [])
                             Errors  errs -> (fatal 1030 "Do not refer to undefined declarations", errs)
        (rruls,rrcxes)    = case (parallelList . map pRRul2aRRul . procRRuls) pproc of
                             Checked rrls -> (rrls, [])
                             Errors errs  -> (fatal 1029 "Do not refer to undefined roleRules", errs)
    --  (keys',keycxes')    = (unzip . map  pKDef2aKDef                    . procKds) pproc
        (keys',keycxes')  = case (parallelList . map pKDef2aKDef . procKds) pproc of
                             Checked keys -> (keys, [])
                             Errors errs  -> (fatal 1029 "Do not refer to undefined keys", errs)
        (expls,explcxes)  = case (parallelList . map pPurp2aPurp . procXps) pproc of
                             Checked purps -> (purps, [])
                             Errors  errs  -> (fatal 1032 "Do not refer to undefined purposes", errs)
 
    pRRul2aRRul :: RoleRule -> Guarded RoleRule
    pRRul2aRRul prrul
     = case rrcxes of
        [] -> Checked prrul
        _  -> Errors [CxeOrig rrcxes "role rule" "" (origin prrul) | (not.null) rrcxes]
       where
         rrcxes = [ newcxe ("Rule '"++r++" does not exist.")
                  | r<-mRules prrul, null [rul | rul<-ctx_rs p_context++(concat.map pt_rls.ctx_pats) p_context++(concat.map procRules.ctx_PPrcs) p_context, name rul==r]]
         
    pRRel2aRRel :: P_RoleRelation -> (RoleRelation,[CtxError])
    pRRel2aRRel prrel
     = ( RR { rrRoles = rr_Roles prrel
            , rrRels  = rels
            , rrPos   = rr_Pos prrel
            }
       , [CxeOrig typeErrs "role relation" "" (origin prrel) | (not.null) typeErrs]
       )
       where
         typeErrs = concat editcxes
         (rels,editcxes) = unzip [ pRel2aRel (psign sgn) r
                                 | PTyp _ r@(Prel{}) sgn<-rr_Rels prrel
                                                          ++fatal 547 ("Untyped relation(s) "++ intercalate ", " [nm | Prel _ nm<-rr_Rels prrel])
                                 ]
    
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
                       , rrviol = mviol
                       , rrtyp  = sign aexpr                 -- Allocated type
                       , rrdcl  = Nothing                    -- The property, if this rule originates from a property on a Declaration
                       , r_env  = patname                    -- Name of pattern in which it was defined.
                       , r_usr  = True                       -- True if this rule was specified explicitly as a rule in the Ampersand script;
                                                             -- False if it follows implicitly from the Ampersand script and generated by a computer
                       , r_sgl  = or [rr_nm prul `elem` map (name.snd) (prcRRuls p) | p<-ctxprocs contxt]  -- True if this is a signal; False if it is an ALWAYS rule
                       , srrel  = Sgn { decnm = rr_nm prul        -- the signal relation
                                      , decsgn = sign aexpr
                                      , decprps = []
                                      , decprps_calc = []
                                      , decprL = ""
                                      , decprM = ""
                                      , decprR = ""
                                      , decMean = meanings
                                      , decConceptDef = Nothing
                                      , decpopu = []
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
  --pKDef2aKDef :: P_KeyDef -> (KeyDef, [CtxError])
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
                             Errors  errs     -> (fatal 1166 "Do not refer to undefined segments", errs)
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
    
    -- TODO -> Does pIFC2aIFC require more checks? What is the intention of params, viols, args i.e. the interface data type?
    pIFC2aIFC :: P_Interface -> Guarded Interface
    pIFC2aIFC pifc 
     = case typeErrors' of
        [] -> Checked (Ifc { ifcName   = ifc_Name pifc
                           , ifcParams = prms
                           , ifcViols  = fatal 206 "not implemented ifcViols"
                           , ifcArgs   = ifc_Args pifc
                           , ifcRoles  = ifc_Roles pifc
                           , ifcObj    = obj
                           , ifcPos    = ifc_Pos pifc
                           , ifcExpl   = ifc_Expl pifc
                           } )
        _  -> Errors [CxeOrig typeErrors' "interface" (name pifc) (origin pifc)]
       where
        typeErrors' = objcxe++concat prmcxes++duplicateRoleErrs++undeclaredRoleErrs
        parentIfcRoles = if null $ ifc_Roles pifc then roles contxt else ifc_Roles pifc -- if no roles are specified, the interface supports all roles
        (obj,objcxe) = case pODef2aODef parentIfcRoles Anything (ifc_Obj pifc) of
                        Checked object -> (object, [])
                        Errors errs    -> (fatal 1201 "Do not refer to undefined objects", errs)
        (prms,prmcxes) = unzip [ pRel2aRel (psign sgn) r
                               | param<-ifc_Params pifc, let (r,sgns)=getSgn param, sgn<-sgns
                               ]
                         where
                            getSgn :: Term -> (Term,[P_Sign])
                            getSgn (PTyp _ r@(Prel{}) (P_Sign []))  = (r,[])
                            getSgn (PTyp _ r@(Prel{}) sgn) = (r,[sgn])
                            getSgn r@(Prel _ rel)          = (r,[dec_sign d | d<-p_declarations p_context, name d==rel ])
                            getSgn r                       = fatal 1070 ("Illegal call of getSgn ("++show r++").")
        duplicateRoleErrs = [newcxe $ "Duplicate interface role \""++role++"\" at "++show (origin pifc) | role <- nub $ ifc_Roles pifc, length (filter (==role) $ ifc_Roles pifc) > 1 ]
        undeclaredRoleErrs = [newcxe $ "Undeclared interface role \""++role++"\" at "++show (origin pifc) | null duplicateRoleErrs, role <- nub $ ifc_Roles pifc, role `notElem` roles contxt ]
        -- we show the line nr for the interface, which may be slightly inaccurate, but roles have no position 
        -- and the implementation of error messages makes it difficult to give a nice one here
        
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
    pExOb2aExOb (PRef2Declaration t@(PTyp o (Prel _ nm) sgn))
                                        = case [pDecl2aDecl [] d | d<-p_declarations p_context, name d==nm, dec_sign d== sgn ] of
                                            Checked decl:_ -> Checked (ExplDeclaration decl)
                                            Errors ers:_   -> Errors ers
                                            []             -> Errors [CxeOrig [newcxe ("No declaration for '"++showADL t++"'")] "relation" nm o ]
    pExOb2aExOb (PRef2Declaration t@(Prel o _ ))
                                        = do { (decl,_) <- getDeclarationAndSign (p_declarations p_context) t
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

    pPop2aPop :: P_Population -> Guarded Population
    pPop2aPop pop
     = case pExpr2aExpr expr of
        Checked (ERel aRel,_,_)          -> Checked ( Popu { popm  = aRel
                                                           , popps = p_popps pop
                                                           } )
        Checked (ETyp (ERel aRel) _,_,_) -> Checked ( Popu { popm  = aRel
                                                           , popps = p_popps pop
                                                           } )
        Checked _                        -> fatal 1223 "illegal call of pPop2aPop"
        Errors errs                      -> Errors errs
       where expr = case pop of
                      P_CptPopu{}              -> Pid (PCpt (name pop))
                      P_Popu{p_type=P_Sign []} -> Prel (origin pop) (name pop)
                      P_Popu{}                 -> PTyp (origin pop) (Prel (origin pop) (name pop)) (p_type pop)
                    
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
                ,cptos = nub$[srcPaire p | d<-declarations contxt,decusr d,p<-contents d, source d DatabaseDesign.Ampersand.Core.Poset.<= c]
                           ++[trgPaire p | d<-declarations contxt,decusr d,p<-contents d, target d DatabaseDesign.Ampersand.Core.Poset.<= c]
                           ++[v | r<-rules contxt,Mp1 v c'<-mors r,c' DatabaseDesign.Ampersand.Core.Poset.<=c]
                           ++[x | (cnm,xs)<-initialatoms contxt, cnm==p_cptnm pc, x<-xs]
                ,cpttp = head ([cdtyp cd | cd<-conceptDefs contxt,cdcpt cd==p_cptnm pc]++[""])
                ,cptdf = [cd | cd<-conceptDefs contxt,cdcpt cd==p_cptnm pc]
                }
    
    pDecl2aDecl :: [Population] -> P_Declaration -> Guarded Declaration
    pDecl2aDecl pops' pd =
     case dec_conceptDef pd of 
          Just (RelConceptDef srcOrTgt _) | relConceptName (dec_nm pd) `elem` map name (concs contxt)
            -> Errors [CxeOrig [newcxe ("Illegal DEFINE "++showSRCorTGT++" for relation "++show (dec_nm pd)++". Concept "++
                                        relConceptName (dec_nm pd)++" already exists.")]
                               "declaration" "" (origin pd)]
               where showSRCorTGT = if srcOrTgt == Src then "SRC" else "TGT"
                     relConceptName ""     = fatal 472 "empty concept"
                     relConceptName (c:cs) = toUpper c : cs
          _ -> Checked ( Sgn { decnm   = dec_nm pd
                             , decsgn  = pSign2aSign (dec_sign pd)
                             , decprps = dec_prps pd
                             , decprps_calc = dec_prps pd --decprps_calc in an A_Context are still the user-defined only. prps are calculated in adl2fspec.
                             , decprL  = dec_prL pd
                             , decprM  = dec_prM pd
                             , decprR  = dec_prR pd
                             , decMean = pMeanings2aMeaning (ctxlang contxt) (ctxmarkup contxt) (dec_Mean pd)
                             , decConceptDef = dec_conceptDef pd
                             , decpopu = nub$    -- All populations from the P_structure will be assembled in the decpopu field of the corresponding declaratio
                                         dec_popu pd ++ 
                                         concat [popps pop | pop<-pops', let ad=popm pop
                                                           , name ad==name pd
                                                           , relsgn ad==pSign2aSign (dec_sign pd)
                                                           ]
                             , decfpos = dec_fpos pd 
                             , deciss  = True
                             , decusr  = True
                             , decpat  = ""
                             , decplug = dec_plug pd
                             } )
      
    -- | p2a for isolated references to relations. Use pExpr2aExpr instead if relation is used in a term.
    pRel2aRel :: [P_Concept] -> Term -> (Relation,[CtxError])
    pRel2aRel _ (PVee orig)
     =( fatal 326 "Ambiguous universal relation."
      , [CxeOrig [newcxe "Ambiguous universal relation."] "relation" "" orig]
      )
    pRel2aRel _ (Pfull s t)
     = (V (Sign (pCpt2aCpt s) (pCpt2aCpt t)), [])
    pRel2aRel _ (PI orig)
     = ( fatal 331 ("Ambiguous identity relation I in "++show orig)
       , [CxeOrig [newcxe "Ambiguous identity relation."]  "relation" "" orig]
       )
    pRel2aRel _ (Pid pConc)
     = (I (pCpt2aCpt pConc), [])
    pRel2aRel _ (Patm orig atom pConcepts) 
     = case pConcepts of
        [] -> ( fatal 343 "Ambiguous value."
              , [CxeOrig [newcxe "Ambiguous value."] "relation" "" orig]
              )
        [c] -> (Mp1 atom (pCpt2aCpt c), [])
        _   -> fatal 354 "Encountered a Sign with more than one element. This should be impossible."
    pRel2aRel sgn (Prel _ nm)
     = case (ds,dts,sgn,unknowncpts) of
        ( _ , _ , _ ,c:cs) -> ( fatal 324 ("Unknown concept in a relation named '"++nm++".")
                              , newcxeif (null cs)      ("Unknown concept: '"++name c++"'.")++
                                newcxeif (not(null cs)) ("Unknown concepts: '"++name c++"' and '"++name (head cs)++"'." )
                              )
                              --        "relation" "" (origin prel) )
        ([] , _ , _ , _  ) -> ( fatal 329 ("Relation undeclared: '"++nm++".")
                              , [newcxe ("Relation undeclared: '"++nm++"'.")]
                              )
                              --        "relation" "" (origin prel) )
        ([d],[] ,[] , _  ) -> (makeRelation d, [])
        ([d],[] , _ , _  ) -> ( fatal 334 ("Relation undeclared: '"++nm++".")
                              , [newcxe ("Relation undeclared: '"++nm++show sgn++"'."
                                        ++".\nDo you intend the one with type "++(show.sign) d++"?")]
                              )
                              --        "relation" "" (origin prel) )
        ( _ ,[d], _ , _  ) -> (makeRelation d, [])
        ( _ ,[] ,[] , _  ) -> ( fatal 340 ("Ambiguous reference to a relation named: '"++nm++".")
                              , [newcxe ("Ambiguous relation: '"++nm++"'.\nUse the full relation signature."
                                        ++"\nPossible types are "++concatMap (show.sign) ds++".")]
                              )
                              --        "relation" "" (origin prel) )
        ( _ ,[] , _ , _  ) -> ( fatal 345 ("Illegal reference to a relation named '"++nm++".")
                              , [newcxe ("Relation undeclared: '"++nm++show sgn++"'."
                                        ++"\nPossible types are "++concatMap (show.sign) ds++".")]
                              )
                              --        "relation" "" (origin prel) )
        (_ : (_ : _), _ : (_ : _), [], []) -> fatal 350 "dts should be empty because dts=[..|.., not(null sgn), ..]"
        (_ : (_ : _), _ : (_ : _), _ : _, []) -> fatal 351 ("length dts should be at most 1 when not(null sgn)\n"++show dts)
        ([_], _ : (_ : _), _, []) -> fatal 352 "More ds than dts should be impossible due to implementation of dts i.e. dts=[d |d<-ds,condition]"
       where
        unknowncpts = nub[c |c<-sgn, pCpt2aCpt c `notElem` concs contxt]
        ds  = [d | d<-declarations contxt, name d==nm]
        dts = [d | d<-ds, not(null sgn)
                        , name (head sgn)==name (source d) &&
                          name (last sgn)==name (target d)   ]
    pRel2aRel _ r = fatal 1314 ("pRel2aRel could not patternmatch on "++show r)
    
    pExpr2aExpr :: Term                               -- The term to be typed
                -> Guarded ( Expression               -- the resulting expression.
                           , P_Concept, P_Concept     -- the source and target types of the resulting expression.
                           )                          -- The result might be incorrect, so it is guarded with type error messages.
    pExpr2aExpr pTerm
     = (\r -> (r, lookupType pTerm, lookupType (p_flp pTerm))) <$> f pTerm
       -- do { r <- f pTerm ; return (r, lookupType pTerm, lookupType (p_flp pTerm))}
       -- or equivalently:
       -- case f pTerm of
       --  Checked r   -> Checked (r, lookupType pTerm, lookupType (p_flp pTerm))
       --  Errors errs -> Errors errs
       where
         f :: Term -> Guarded Expression
         f t@(PI _)            = do { c<-returnIConcepts t
                                    ; return (ERel (I (pCpt2aCpt c)))
                                    }
         f   (Pid c)           = return (ERel (I (pCpt2aCpt c)))
         f   (Pnid c)          = return (ECpl (ERel (I (pCpt2aCpt c))))
         f t@(Patm _ atom [])  = do { c<-returnIConcepts t
                                    ; return (ERel (Mp1 atom (pCpt2aCpt c)))
                                    }
         f   (Patm _ atom [c]) = return (ERel (Mp1 atom (pCpt2aCpt c)))
         f t@(Patm _ _     _ ) = fatal 1459 ("multiple concepts in "++show t++".")
         f t@Pnull             = fatal 988 ("pExpr2aExpr cannot transform "++show t++" to a term.")
         f t@(PVee _)          = ERel <$> (V <$> getSignFromTerm t)
         f (Pfull s t)         = return (ERel (V (Sign (pCpt2aCpt s) (pCpt2aCpt t))))
         f t@(Prel o a)        = do { (decl,sgn) <- getDeclarationAndSign (p_declarations p_context) t
                                    ; return (ERel (Rel{relnm=a, relpos=o, relsgn=sgn, reldcl=decl}))
                                    }
         f (Pflp o a)          = do { (decl,sgn) <- getDeclarationAndSign (p_declarations p_context) (Prel o a)
                                    ; return (EFlp (ERel (Rel{relnm=a, relpos=o, relsgn=sgn, reldcl=decl})))
                                    }
         f t@(Pequ _ a b)      = do { (a',b') <- (,) <$> f a <*> f b
                                    ; let srcAs = srcTypes (TypExpr        a  False)
                                          trgAs = srcTypes (TypExpr (p_flp a) True )
                                          srcBs = srcTypes (TypExpr        b  False)
                                          trgBs = srcTypes (TypExpr (p_flp b) True )
                                          srcTyps = srcAs `uni` srcBs
                                          trgTyps = trgAs `uni` trgBs
                                    ; case (srcTyps, trgTyps) of
                                           ([_], [_]) -> return (EEqu (a', b'))
                                           _          -> Errors [CxeEquLike {cxeExpr    = t
                                                                            ,cxeLhs     = a
                                                                            ,cxeRhs     = b
                                                                            ,cxeSrcCpts = (srcAs `uni` srcBs) >- (srcAs `isc` srcBs)
                                                                            ,cxeTrgCpts = (trgAs `uni` trgBs) >- (trgAs `isc` trgBs)
                                                                            }]
                                    }
         f t@(Pimp _ a b)      = do { (a',b') <- (,) <$> f a <*> f b
                                    ; let srcAs = (srcTypes.normalType) (TypGlb (TypExpr        a  False) (TypExpr        b  False) t)
                                          trgAs = (srcTypes.normalType) (TypGlb (TypExpr (p_flp a) True ) (TypExpr (p_flp b) True ) t)
                                          srcBs = srcTypes (TypExpr        b  False)
                                          trgBs = srcTypes (TypExpr (p_flp b) True )
                                    ; case (srcAs, trgBs) of
                                           ([_], [_]) -> return (EImp (a', b'))
                                           _          -> Errors [CxeEquLike {cxeExpr    = t
                                                                            ,cxeLhs     = a
                                                                            ,cxeRhs     = b
                                                                            ,cxeSrcCpts = (srcAs `uni` srcBs) >- (srcAs `isc` srcBs)
                                                                            ,cxeTrgCpts = (trgAs `uni` trgBs) >- (trgAs `isc` trgBs)
                                                                            }]
                                    }
         f t@(PIsc _ a b)      = do { (a',b') <- (,) <$> f a <*> f b
                                    ; let srcs = (srcTypes.normalType) (TypGlb (TypExpr        a  False) (TypExpr        b  False) t)
                                          trgs = (srcTypes.normalType) (TypGlb (TypExpr (p_flp a) True ) (TypExpr (p_flp b) True ) t)
                                    ; case (srcs, trgs) of
                                           ([_], [_]) -> return (EIsc (case a' of {EIsc ts -> ts; t'-> [t']} ++ case b' of {EIsc ts -> ts; t'-> [t']}))
                                           _          -> Errors [CxeEquLike {cxeExpr    = t
                                                                            ,cxeLhs     = a
                                                                            ,cxeRhs     = b
                                                                            ,cxeSrcCpts = srcs
                                                                            ,cxeTrgCpts = trgs
                                                                            }]
                                    }
         f t@(PUni _ a b)      = do { (a',b') <- (,) <$> f a <*> f b
                                    ; let srcs = (srcTypes.normalType) (TypLub (TypExpr        a  False) (TypExpr        b  False) t)
                                          trgs = (srcTypes.normalType) (TypLub (TypExpr (p_flp a) True ) (TypExpr (p_flp b) True ) t)
                                    ; case (srcs, trgs) of
                                           ([_], [_]) -> return (EUni (case a' of {EUni ts -> ts; t'-> [t']} ++ case b' of {EUni ts -> ts; t'-> [t']}))
                                           _          -> Errors [CxeEquLike {cxeExpr    = t
                                                                            ,cxeLhs     = a
                                                                            ,cxeRhs     = b
                                                                            ,cxeSrcCpts = srcs
                                                                            ,cxeTrgCpts = trgs
                                                                            }]
                                    }
         f   (PDif _ a b)      = do { (a',b') <- (,) <$> f a <*> f b
                                    ; return (EDif (a', b'))
                                    }
         f t@(PLrs _ a b)      = do { (a',b') <- (,) <$> f a <*> f b    -- a/b = a!-b~ = -(-a;b~)
                                    ; case (srcTypes.normalType) (TypGlb (TypExpr (p_flp (complement a)) True) (TypExpr (p_flp b) True) t) of
                                       [_] -> return (ELrs (a', b'))
                                       cs  -> Errors [ CxeCpsLike {cxeExpr   = t
                                                                  ,cxeCpts   = cs
                                                                  }
                                                     ]
                                    }
         f t@(PRrs _ a b)      = do { (a',b') <- (,) <$> f a <*> f b    -- a\b = -a~!b = -(a~;-b)
                                    ; case (srcTypes.normalType) (TypGlb (TypExpr a False) (TypExpr (complement b) False) t) of
                                       [_] -> return (ERrs (a', b'))
                                       cs  -> Errors [ CxeCpsLike {cxeExpr   = t
                                                                  ,cxeCpts   = cs
                                                                  }
                                                     ]
                                    }
         f t@(PCps _ a b)      = do { (a',b') <- (,) <$> f a <*> f b
                                    ; case (srcTypes.normalType) (TypGlb (TypExpr (p_flp a) True) (TypExpr b False) t) of
                                       [_] -> return (ECps (case a' of {ECps ts -> ts; t'-> [t']} ++ case b' of {ECps ts -> ts; t'-> [t']}))
                                       cs  -> Errors [ CxeCpsLike {cxeExpr   = t
                                                                  ,cxeCpts   = cs
                                                                  }
                                                     ]
                                    }
         f t@(PRad _ a b)      = do { (a',b') <- (,) <$> f a <*> f b
                                    ; case (srcTypes.normalType) (TypLub (TypExpr (p_flp a) True) (TypExpr b False) t) of
                                       [_] -> return (ERad (case a' of {ERad ts -> ts; t'-> [t']} ++ case b' of {ERad ts -> ts; t'-> [t']}))
                                       cs  -> Errors [ CxeCpsLike {cxeExpr   = t
                                                                  ,cxeCpts   = cs
                                                                  }
                                                     ]
                                    }
         f (PPrd _ a b)        = do { (fa, fb) <- (,) <$> f a <*> f b
                                    ; return (EPrd [fa,fb]) }
         f (PKl0 _ a)          = do { a' <- f a
                                    ; return (EKl0 a')
                                    }
         f (PKl1 _ a)          = EKl1 <$> f a
         f (PFlp _ a)          = EFlp <$> f a
         f (PCpl _ a)          = ECpl <$> f a
         f (PBrk _ a)          = EBrk <$> f a
         f t@(PTyp o _        (P_Sign [])) = fatal 991 ("pExpr2aExpr cannot transform "++show t++" ("++show o++") to a term.")
         f   (PTyp _ e@(Prel o a) sgnCast) = do { (decl,_) <- getDeclarationAndSign (p_declarations p_context) e
                                                ; return (ERel (Rel{relnm=a, relpos=o, relsgn=pSign2aSign sgnCast, reldcl=decl}))
                                                }
         f t@(PTyp _ a (P_Sign _))
                               = do { a' <- f a
                                    ; case (srcTypes (TypExpr t False), srcTypes (TypExpr (p_flp t) True )) of
                                        ([src],[trg]) -> return (ETyp a' (Sign (pCpt2aCpt src) (pCpt2aCpt trg)))
                                        (srcs , trgs) -> Errors [CxeCast {cxeExpr    = t
                                                                         ,cxeDomCast = srcs
                                                                         ,cxeCodCast = trgs
                                                                         }]
                                    }
         returnIConcepts term
          = case getConceptsFromTerm term of
             [c] -> return c
             cs  -> Errors[CxeILike { cxeExpr = term
                                    , cxeCpts = cs
                                    }]
         getConceptsFromTerm :: Term -> [P_Concept]
         getConceptsFromTerm x
          = srcTypes (TypExpr x False)
         getSignFromTerm :: Term -> Guarded Sign
         getSignFromTerm term
          = getSign (\s t -> [ CxeV { cxeExpr = term
                                    , cxeSrcs = s
                                    , cxeTrgs = t}]) term
         getSign :: ([P_Concept]->[P_Concept]->[CtxError]) -> Term -> Guarded Sign
         getSign e term
          = case (src,trg) of
             ([s],[t]) -> Checked (Sign (pCpt2aCpt s) (pCpt2aCpt t))
             (_  ,_  ) -> Errors (e src trg)
             where src = getConceptsFromTerm term
                   trg = getConceptsFromTerm (p_flp term)
         lookupType :: Term -> P_Concept
         lookupType t = case getConceptsFromTerm t of
                         [c] -> c
                         []  -> fatal 1586 ("No type found for term "++show t)
                         cs  -> fatal 1586 ("Multiple types found for term "++show t++": "++show cs)

    getDeclarationAndSign :: [P_Declaration] -> Term -> Guarded (Declaration, Sign)
    getDeclarationAndSign decls term@(Prel _ relname)
     = case Data.Map.lookup (TypExpr term False) bindings of
        Just [(d, [s], [t])] -> do { decl <- pDecl2aDecl [] d ; return (decl, Sign (pCpt2aCpt s) (pCpt2aCpt t)) }
        Just ds              -> case [(decl,[],[]) | decl<-decls, name decl==relname] of
                                 []  -> Errors [CxeRel {cxeExpr   = term        -- the erroneous term
                                                       ,cxeDecs   = ds          -- the declarations to which this term has been matched
                                                       }]
                                 ds' -> Errors [CxeRel {cxeExpr   = term        -- the erroneous term
                                                       ,cxeDecs   = ds'         -- the declarations to which this term has been matched
                                                       }]
                                 
        Nothing -> fatal 1601 ("Term "++showADL term++" ("++show(origin term)++") was not found in bindings."++concat ["\n  "++show b | b<-Data.Map.toAscList bindings ])
    getDeclarationAndSign _ term = fatal 1607 ("Illegal call to getDeclarationAndSign ("++show term++")")

--the type checker always returns a term with sufficient type casts, it should remove redundant ones.
--applying the type checker on an complete, explicitly typed term is equivalent to disambiguating the term

--   Disambiguation is needed for the purpose of printing a term.    parse (disambiguate expr) = expr
--   Produce type error messages if necessary and produce proof graphs to document the type checking process.
disambiguate :: Fspc -> Expression -> Expression
disambiguate _ x = x
{- -- temporarily disabled (19 july 2012), in order to get the type checker correct first...
disambiguate :: Fspc -> Expression -> Expression
disambiguate fspec x = 
 | null errs = expr 
 | otherwise  = fatal 428 ("a term must be type correct, but this one is not:\n" ++ show errs)
 where
   (expr,errs) = pExpr2aExpr fSpec{vrels=vrels fSpec++deltas} NoCast (f x)
   -- f transforms x to a Term using full relation signatures
   f (EEqu (l,r)) = Pequ OriginUnknown (f l) (f r)
   f (EImp (l,r)) = Pimp OriginUnknown (f l) (f r)
   f (EIsc [l])   = f l
   f (EIsc [l,r]) = PIsc OriginUnknown (f l) (f r)
   f (EIsc (l:rs))= PIsc OriginUnknown (f l) (f (EIsc rs))
   f (EIsc _)     = PVee OriginUnknown
   f (EUni [l])   = f l
   f (EUni [l,r]) = PUni OriginUnknown (f l) (f r)
   f (EUni (l:rs))= PUni OriginUnknown (f l) (f (EUni rs))
   f (EUni _)     = Pnull
   f (EDif (l,r)) = PDif OriginUnknown (f l) (f r)
   f (ELrs (l,r)) = PLrs OriginUnknown (f l) (f r)
   f (ERrs (l,r)) = PRrs OriginUnknown (f l) (f r)
   f (ECps es)    = foldr1 (PCps OriginUnknown) (map f es)
   f (ERad es)    = foldr1 (PRad OriginUnknown) (map f es)
   f (EPrd es)    = foldr1 (PPrd OriginUnknown) (map f es)
   f (EKl0 e)     = PKl0 OriginUnknown (f e)
   f (EKl1 e)     = PKl1 OriginUnknown (f e)
   f (EFlp e)     = PFlp OriginUnknown (f e)
   f (ECpl e)     = PCpl OriginUnknown (f e)
   f (EBrk e)     = PBrk OriginUnknown (f e)
   f (ETyp e _)   = f e
   f (ERel rel@(Rel{})) = PTyp OriginUnknown (Prel OriginUnknown (name rel))
                               (P_Sign [g (source rel),g (target rel)])
   f (ERel rel@(I{}))   = Pid (g (source rel))
   f (ERel rel@(V{}))   = Pfull (g (source rel)) (g (target rel))
   f (ERel rel@(Mp1{})) = Patm OriginUnknown (relval rel) [g (source rel)]
   g c@(C{}) = PCpt (name c) 
   g ONE     = P_Singleton
   deltas    = [ makeDeclaration r | r<-mors x, name r=="Delta" ]
-}

-- | An InfExpression yields a list of alternatives that are type correct (type: [Expression]) and a list of error messages (type: [String]).
--type InfExpression  = AutoCast -> ([Expression],[String])
-- | internal type to push down the type as far as known on the ERel, thus possibly with wild cards on source or target
data AutoCast = NoCast | SourceCast A_Concept | TargetCast A_Concept | Cast A_Concept A_Concept deriving (Show,Eq)
-- | AutoCast is not of class Association, but it should be flippable

{-
flpcast :: AutoCast -> AutoCast
flpcast NoCast = NoCast
flpcast (SourceCast x) = TargetCast x
flpcast (TargetCast x) = SourceCast x
flpcast (Cast x y) = Cast y x
-}


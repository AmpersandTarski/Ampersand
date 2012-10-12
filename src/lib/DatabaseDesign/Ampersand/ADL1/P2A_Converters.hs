{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
{-# LANGUAGE RelaxedPolyRec #-} -- -RelaxedPolyRec required for OpenSuse, for as long as we@OpenUniversityNL use an older GHC
module DatabaseDesign.Ampersand.ADL1.P2A_Converters (
     -- * Exported functions
     pCtx2aCtx, disambiguate,
     Guarded(..)
     )
where
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree hiding (sortWith, maxima)
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
            -- note: do not put "deriving Ord", because Eq is specified (and not derived)

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


-- | Equality of Type is needed for the purpose of making graphs of types.
--   These are used in the type checking process.
--   The main idea is that the equality distinguishes between occurrences.
--   So term 'r' on line 14:3 differs from  the term 'r' on line 87:19.
--   However, different occurrences of specific terms that are fully typed (e.g. I[Person] or parent[Person*Person]), need not be distinguised.
instance Prelude.Ord Type where
  compare Anything                   Anything                     = Prelude.EQ
  compare Anything                   _                            = Prelude.LT  -- Anything < everything
  compare _                          Anything                     = Prelude.GT  -- everyting > Anything
  compare (TypExpr (Pid c)        _) (TypExpr (Pid c')         _) = Prelude.compare c c'
  compare (TypExpr (Pid _)        _) (TypExpr _                _) = Prelude.LT
  compare (TypExpr _              _) (TypExpr (Pid _)          _) = Prelude.GT
  compare (TypExpr (Pnid c)       _) (TypExpr (Pnid c')        _) = Prelude.compare c c'
  compare (TypExpr (Pnid _)       _) (TypExpr _                _) = Prelude.LT
  compare (TypExpr _              _) (TypExpr (Pnid _)         _) = Prelude.GT
  compare (TypExpr (Patm _ x [c]) _) (TypExpr (Patm _ x' [c']) _) = Prelude.compare (x,c) (x',c')
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
p_flp a@(Pnid{})   = a
p_flp a@(Patm{})   = a
p_flp Pnull        = Pnull
p_flp a@(PVee _)   = a
p_flp (Pfull s t)  = Pfull t s
p_flp (Prel o a)   = Pflp o a
p_flp (Pflp o a)   = Prel o a
p_flp (PFlp _ a)   = a
p_flp a            = PFlp (origin a) a

complement :: Term -> Term
complement (PCpl _ a)     = a
complement (Pnid c)       = Pid c
complement a              = PCpl (origin a) a

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
reverseMap lst = (Data.Map.fromListWith mrgUnion (concat [(a,[]):[(b,[a])|b<-bs] | (a,bs)<-Data.Map.toList lst]))
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
                            | a==b = if (b==a) then distinctCons a b (merge as bs) else fatal 193 ("Eq is not symmetric for: "++show a++" and "++show b)
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

-- | The purpose of 'typing' is to analyse the domains and codomains of a term in a context.
--   As a result, it builds a list of tuples st::[(Type,Type)], which represents a relation, st,  over Type*Type.
--   For any two Terms a and b,  if 'typing' can tell that dom(a) is a subset of dom(b),
--   this is represented by a tuple (TypExpr a _,TypExpr b _) in st.
--   In the code below, this shows up as  dom a.<.dom b
--   The function typing does a recursive scan through all subterms, collecting all tuples on its way.
--   Besides term term, this function requires a universe in which to operate.
--   Specify 'Anything Anything' if there are no restrictions.
--   If the source and target of term is restricted to concepts c and d, specify (thing c) (thing d).
typing :: P_Context -> Typemap -- subtypes (.. is subset of ..)
typing p_context
 = Data.Map.unionWith mrgUnion firstSetOfEdges secondSetOfEdges
   where
   -- The story: two Typemaps are made by uType, each of which contains tuples of the relation st.
   --            These are converted into two maps (each of type Typemap) for efficiency reasons.
     compatible :: Type -> Type -> Bool                        -- comparable = isa*~;isa*, i.e. a lower bound exists
     compatible a b = (not.null) ((revEdgesClos Data.Map.! a) `isc` (revEdgesClos Data.Map.! b))
      where
       revEdgesClos = setClosure (reverseMap firstSetOfEdges) "revEdgesClos"
     (firstSetOfEdges,secondSetOfEdges)
      = (foldr (.+.) nothing [uType term Anything Anything term | term <- terms p_context])
     pDecls = concat (map terms (p_declarations p_context))   --  this amounts to [PTyp (origin d) (Prel (origin d) (dec_nm d)) (dec_sign d) | d<-p_declarations p_context]
     uType :: Term    -- x:    the original term from the script, meant for representation in the graph.
           -> Type            -- uLft: the type of the universe for the domain of x 
           -> Type            -- uRt:  the type of the universe for the codomain of x
           -> Term    -- z:    the term to be analyzed, which must be logically equivalent to x
           -> ( Typemap  -- for each type, a list of types that are subsets of it, which is the result of analysing term x.
              , Typemap ) -- for some edges, we need to know the rest of the graph. These can be created in this second part.
     uType _ _    _     (Pnid _)              = fatal 136 "Pnid has no representation"
     uType x uLft uRt   (PI{})                = dom x.=.cod x .+. dom x.<.uLft .+. cod x.<.uRt    -- I
     uType x _    _     (Pid{})               = dom x.=.cod x                                     -- I[C]
     uType x uLft uRt   (Patm _ _ [])         = dom x.=.cod x .+. dom x.<.uLft .+. cod x.<.uRt    -- 'Piet'   (an untyped singleton)
     uType x _    _     (Patm _ _ cs)         = dom x.<.thing (head cs) .+. cod x.<.thing (last cs)      -- 'Piet'[Persoon]  (a typed singleton)
                                                .+. dom x.=.cod x
     uType _ _    _      Pnull                = nothing                                                              -- -V     (the empty set)
     uType x uLft uRt   (PVee _)              = dom x.<.uLft .+. cod x.<.uRt
     uType x _    _     (Pfull s t)           = dom x.=.dom (Pid s) .+. cod x.=.cod (Pid t)                          --  V[A*B] (the typed full set)
     uType x uLft uRt   (Prel _ nm)           = dom x.<.uLft .+. cod x.<.uRt .+.
                                                if length decls' == 1 then dom x.=.dom (head decls') .+. cod x.=.cod (head decls') else
                                                carefully ( -- what is to come will use the first iteration of edges, so to avoid loops, we carefully only create second edges instead
                                                            if length spcls == 1
                                                            then dom x.=.dom (head spcls) .+. cod x.=.cod (head spcls)
                                                            else nothing
                                                          )
                                                where decls' = [decl | decl@(PTyp _ (Prel _ dnm) _)<-pDecls, dnm==nm ]
                                                      spcls  = [decl | decl@(PTyp _ (Prel _ _) (P_Sign cs@(_:_)))<-decls'
                                                                     , compatible (dom x) (thing (head cs))  -- this is compatibility wrt firstSetOfEdges, thus avoiding a computational loop
                                                                     , compatible (cod x) (thing (last cs))
                                                                     ]
                                                      carefully :: (Typemap , Typemap ) -> (Typemap, Typemap)
                                                      carefully tt = (fst nothing,fst tt.++.snd tt)
     uType x uLft uRt   (Pflp o nm)           = dom x.=.cod e .+. cod x.=.dom e .+. uType e uRt uLft e
                                                where e = Prel o nm
     uType x uLft uRt   (Pequ _ a b)          = dom a.=.dom x .+. cod a.=.cod x .+. dom b.=.dom x .+. cod b.=.cod x  --  a=b    equality
                                                 .+. uType a uLft uRt a .+. uType b uLft uRt b 
     uType x uLft uRt   (PIsc _ a b)          = dom x.=.interDom .+. cod x.=.interCod    --  intersect ( /\ )
                                                .+. dm .+. cm .+. d2 .+. c2 -- d2 and c2 are needed for try15
                                                .+. uType a interDom2 interCod2 a .+. uType b interDom2 interCod2 b
                                                where (dm,interDom) = mSpecific (dom a) (dom b)  x
                                                      (cm,interCod) = mSpecific (cod a) (cod b)  x
                                                      (d2,interDom2) = mSpecific interDom uLft  x
                                                      (c2,interCod2) = mSpecific interCod uRt   x
     uType x uLft uRt   (PUni _ a b)          = dom x.=.interDom .+. cod x.=.interCod    --  union     ( \/ )
                                                .+. dm .+. cm .+. d2 .+. c2
                                                .+. uType a interDom2 interCod2 a .+. uType b interDom2 interCod2 b
                                                where (dm,interDom) = mGeneric (dom a) (dom b)  x
                                                      (cm,interCod) = mGeneric (cod a) (cod b)  x
                                                      (d2,interDom2) = mSpecific interDom uLft  x
                                                      (c2,interCod2) = mSpecific interCod uRt   x
     uType x uLft uRt   (PCps _ a b)          = dom x.<.dom a .+. cod x.<.cod b .+.                                  -- a;b      composition
                                                bm .+. uType a uLft between a .+. uType b between uRt b
                                                .+. pidTest a (dom x.<.dom b) .+. pidTest b (cod x.<.cod a)
                                                where (bm,between) = mSpecific (cod a) (dom b) x
                                                      pidTest (PI{}) r = r
                                                      pidTest (Pid{}) r = r
                                                      pidTest _ _ = nothing
     uType x uLft uRt   (PDif _ a b)          = dom x.<.dom a .+. cod x.<.cod a                                        --  a-b    (difference)
                                                 .+. dm .+. cm
                                                 .+. uType a uLft uRt a
                                                 .+. uType b interDom interCod b
                                                where (dm,interDom) = (mSpecific uLft (dom a) x)
                                                      (cm,interCod) = (mSpecific uRt  (cod a) x)
     uType x uLft uRt   (PKl0 _ e)            = dom e.<.dom x .+. cod e.<.cod x .+. uType e uLft uRt e
     uType x uLft uRt   (PKl1 _ e)            = dom e.<.dom x .+. cod e.<.cod x .+. uType e uLft uRt e
     uType x uLft uRt   (PFlp _ e)            = cod e.=.dom x .+. dom e.=.cod x .+. uType e uRt uLft e
     uType x uLft uRt   (PBrk _ e)            = dom x.=.dom e .+. cod x.=.cod e .+.  
                                                uType x uLft uRt e                                                     -- (e) brackets
     uType _  _    _    (PTyp _ _ (P_Sign []))= fatal 196 "P_Sign is empty"
     uType _  _    _    (PTyp _ _ (P_Sign (_:_:_:_))) = fatal 197 "P_Sign too large"
     uType x uLft uRt   (PTyp o e (P_Sign cs))= dom x.<.iSrc  .+. cod x.<.iTrg  .+.                                    -- e[A*B]  type-annotation
                                                if o `elem` [origin d| d<-pDecls]     -- if this is a declaration
                                                then nothing                          -- then nothing
                                                else dom x.<.dom e .+. cod x.<.cod e  -- else treat as type restriction
                                                     .+. dom x.<.iSrc .+. cod x.<.iTrg
                                                     .+. iSrc.<.uLft .+. iTrg.<.uRt
                                                     .+. uType e iSrc iTrg e
                                                --   .+. error ("Diagnostic: uType ("++showADL e++") ("++showType iSrc++") ("++showType iTrg++")\n x="++show x)
                                                where iSrc = thing (head cs)
                                                      iTrg = thing (last cs)
     uType x uLft uRt   (PPrd _ a b)          = dom x.<.dom a .+. cod x.<.cod b                                        -- a*b cartesian product
                                                .+. uType a uLft Anything a .+. uType b Anything uRt b
     -- derived uTypes: the following do no calculations themselves, but merely rewrite terms to the ones we covered
     uType x uLft uRt   (Pimp o a b)          = dom x.=.dom e .+. cod x.=.cod e .+. 
                                                uType x uLft uRt e                 --  a|-b   implication (aka: subset)
                                                where e = Pequ o a (PIsc o a b)
     uType x uLft uRt   (PLrs o a b)          = dom x.=.dom e .+. cod x.=.cod e .+.  
                                                uType x uLft uRt e                 -- a/b = a!-b~ = -(-a;b~)
                                                where e = PCpl o (PCps o (complement a) (p_flp b))
     uType x uLft uRt   (PRrs o a b)          = dom x.=.dom e .+. cod x.=.cod e .+.  
                                                uType x uLft uRt e                 -- a\b = -a~!b = -(a~;-b)
                                                where e = PCpl o (PCps o (p_flp a) (complement b))
     uType x uLft uRt   (PRad o a b)          = dom x.=.dom e .+. cod x.=.cod e .+.  
                                                uType x uLft uRt e                 -- a!b = -(-a;-b) relative addition
                                                where e = PCps o (complement a) (complement b)
     uType x uLft uRt   (PCpl o a)            = dom x.=.dom e .+. cod x.=.cod e .+.  
                                                uType x uLft uRt e                 -- -a = V - a
                                                where e = PDif o (PVee (origin x)) a
     nothing :: (Typemap,Typemap)
     nothing = (Data.Map.empty,Data.Map.empty)
     {-
     isFull (TypExpr (Pfull _ _) _) = True
     isFull (TypLub a b _) = isFull a && isFull b
     isFull (TypGlb a b _) = isFull a && isFull b
     isFull _ = False -}
     isNull (TypExpr Pnull _) = True
     isNull (TypLub a b _) = isNull a && isNull b
     isNull (TypGlb a b _) = isNull a && isNull b
     isNull _ = False
     infixl 2 .+.   -- concatenate two lists of types
     infixl 3 .<.   -- makes a list of one tuple (t,t'), meaning that t is a subset of t'
     infixl 3 .=.   -- makes a list of two tuples (t,t') and (t',t), meaning that t is equal to t'
     (.<.) :: Type -> Type -> (Typemap , Typemap)
     _ .<. Anything = nothing
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
     mGeneric  a b e = (a .<. r .+. b .<. r , r) where r = TypLub a b e
     mSpecific a b e = (r .<. a .+. r .<. b , r) where r = TypGlb a b e

flattenMap :: Data.Map.Map t [t1] -> [(t, t1)]
flattenMap = Data.Map.foldWithKey (\s ts o -> o ++ [(s,t)|t<-ts]) []
-- alternatively: flattenMap mp = [ (a,b) | (a,bs)<-Data.Map.toList mp , b<-bs])

{- The following table is a data structure that is meant to facilitate drawing type graphs and creating the correct messages for users.
This table is organized as follows:
Int          : a vertex (number) in the stGraph, which contains the raw tuples from function 'typing'
Int          : a vertex (number) in the condensedGraph, which is a condensed form of the stGraph, leaving the semantics identical
Type         : a type term, containing a Term, which is represented by a number in the type graphs. Different terms may carry the same number in the condensedGraph.
[P_Concept]  : a list of concepts. If (_,_,term,cs) is an element of this table, then for every c in cs there is a proof that dom term is a subset of I[c].
               For a type correct term, list cs contains precisely one element.
-}
tableOfTypes :: P_Context -> Typemap ->
                ( Typemap          -- stClos      -- (st\/stAdded)*\/I  (reflexive and transitive)
                , Typemap          -- eqType      -- (st*/\st*~)\/I  (reflexive, symmetric and transitive)
                , Typemap          -- stClosAdded -- additional links added to stClos
                , Typemap          -- stClos1     -- st*  (transitive)
                , P_Concept -> P_Concept -> Maybe P_Concept -- cGlb-- a function to compute a greates lower bound, if it exists.
                , Data.Map.Map Type [(P_Declaration,P_Sign)] -- bindings    -- declarations that may be bound to relations
                , [(P_Concept,P_Concept)]           -- isas        -- 
                )                                   
tableOfTypes p_context st
  = ( stClos
    , eqType
    , stClosAdded
    , stClos1
    , cGlb
    , bindings
  -- isas is produced for the sake of making a partial order of concepts in the A-structure.
    , isas   -- a list containing all tuples of concepts that are in the subset relation with each other.
             -- it is used for the purpose of making a poset of concepts in the A-structure.
    ) 
 where
{-  stGraph is a graph whose edges are precisely st, but each element in
    st is replaced by a pair of integers. The reason is that datatype
    Graph expects integers. The list st contains the essence of the
    type analysis. It contains tuples (t,t'), each of which means
    that the set of atoms contained by dom t is a subset of the set
    of atoms contained by dom t'.
-}
     typeTerms :: [Type]          -- The list of all type terms in st.
     typeTerms = Data.Map.keys st -- Because a Typemap is total, it is sufficient to compute  Data.Map.keys st
-- In order to compute the condensed graph, we need the transitive closure of st:
     stClos1 :: Typemap
     stClos1 = setClosure st "st"       -- represents (st)* .   stClos1 is transitive
     someWhatSortedGlbs = sortBy compr [l | l@(TypGlb _ _ _) <- typeTerms]
      where
      -- Compr uses the stClos1 to decide how (a ./\. b) and ((a ./\. b) ./\. c) should be sorted
       compr a b  = if b `elem` (lookups a stClos1) then LT else -- note: by LT we mean: less than or equal
                    if a `elem` (lookups b stClos1) then GT -- likewise, GT means: greater or equal
                     else EQ -- and EQ means: don't care about the order (sortBy will preserve the original order as much as possible)
      -- We add arcs for the TypLubs, which means: if x .<. a  and x .<. b, then x .<. (a ./\. b), so we add this arc
      -- This explains why we did the sorting:
      --    after deducing x .<. (a ./\. b), we might deduce x .<. c, and then x .<. ((a ./\. b) ./\. c)
      --    should we do the deduction about ((a ./\. b) ./\. c) before that of (a ./\. b), we would miss this
      -- We filter for TypLubs, since we do not wish to create a new TypExpr for (a ./\. b) if it was not already in the st-graph
     stClosAdded :: Typemap
     stClosAdded = foldl f stClos1 someWhatSortedGlbs
       where
        f :: Typemap -> Type -> Typemap 
        f dataMap o@(TypGlb a b _) = Data.Map.map (\cs -> mrgUnion cs [e | a `elem` cs, b `elem` cs, e<-lookups o dataMap]) dataMap
        f _ o = fatal 406 ("Inexhaustive pattern in f in stClosAdded in tableOfTypes: "++show o)
     stClos :: Typemap -- ^ represents the transitive closure of stClosAdded.
     stClos = addIdentity (setClosure stClosAdded "stClosAdded")  -- stClos = stClosAdded*\/I, so stClos is transitive (due to setClosure).
     stClosReversed = reverseMap stClos  -- stClosReversed is transitive too and like stClos, I is a subset of stClosReversed.
     eqType = Data.Map.intersectionWith mrgIntersect stClos stClosReversed  -- eqType = stAdded* /\ stAdded*~ i.e there exists a path from a to be and a path from b.
     isaClos = addIdentity (Data.Map.fromDistinctAscList [(c,[c' | TypExpr (Pid c') _<-ts]) | (TypExpr (Pid c) _, ts)<-Data.Map.toAscList stClos])
     isaClosReversed = reverseMap isaClos
     cGlb :: P_Concept -> P_Concept -> Maybe P_Concept
     cGlb a b = case (isaClosReversed Data.Map.! a) `isc` (isaClosReversed Data.Map.! b) of
                 [c] -> Just c
                 []  -> Nothing
                 cs  -> (Just . fst.head.sortWith ((0-).length.snd)) [ (c, isaClosReversed Data.Map.! c) | c<-cs]
     isas = [ (s,g) | s<-Data.Map.keys isaClos, g<-isaClos Data.Map.! s ]  -- this is redundant, because isas=isas*. Hence, isas may contain tuples (s,g) for which there exists q and (s,q) and (q,g) are in isas.
     stConcepts :: Data.Map.Map Type [P_Concept]
     stConcepts = Data.Map.map f stClos
                  where f ts = [ (fst.head.sortWith (length.snd)) [(c,isaClosReversed Data.Map.! c) | c<-cl]
                               | cl<-eqClass compatible cs]
                               where cs = [c | TypExpr (Pid c) _<-ts] -- all concepts reachable from one type term
                        compatible a b = (not.null) ((isaClosReversed Data.Map.! a) `isc` (isaClosReversed Data.Map.! b))

{- Bindings:
Relations that are used in a term must be bound to declarations. When they have a type annotation, as in r[A*B], there is no  problem.
When it is just 'r', and there are multiple declarations to which it can be bound, the type checker must choose between candidates.
Sometimes, there is only one candidate, even though the type checker cannot prove it correct (i.e. the domain of the term is a subset of the candidate type).
In such cases, we want to give that candidate to the user by way of suggestion to resolve the type conflict.
-}
     bindings :: Data.Map.Map Type [(P_Declaration,P_Sign)]
     bindings    = {- for debugging and illustration purposes: 
                   error ("\nbindings = \n  "++(intercalate "\n  ".map show)
                                                [ tuple      -- the bindings of domain and codomain terms should lead to the identical declaration. Both bindings will be added to the graph.
                                                | decl<-p_declarations p_context
                                                , dTerm@(TypExpr (Prel _ dnm) _) <- typeTerms, dnm==dec_nm decl, length (stConcepts Data.Map.! dTerm)<=1
                                                , cTerm@(TypExpr (Pflp _ cnm) _) <- typeTerms, cnm==dec_nm decl, length (stConcepts Data.Map.! dTerm)<=1
                                                , let P_Sign sgn = dec_sign decl; srce=head sgn; targ=last sgn
                                                , Just s<-case stConcepts Data.Map.! dTerm of
                                                           [c] -> [c `cGlb` srce]
                                                           _   -> [Just srce]
                                                , Just t<-case stConcepts Data.Map.! cTerm of
                                                           [c] -> [c `cGlb` targ]
                                                           _   -> [Just targ]
                                                , tuple<-[(dTerm,(decl,P_Sign [s,t])),(cTerm,(decl,P_Sign [s,t]))]
                                                ]
                         ) -}
                   Data.Map.fromListWith union
                   ( [ tuple      -- the bindings of domain and codomain terms should lead to the identical declaration. Both bindings will be added to the grap
                     | decl<-p_declarations p_context
                     , dTerm@(TypExpr (Prel _ dnm) _) <- typeTerms, dnm==dec_nm decl, length (stConcepts Data.Map.! dTerm)<=1
                     , cTerm@(TypExpr (Pflp _ cnm) _) <- typeTerms, cnm==dec_nm decl, length (stConcepts Data.Map.! dTerm)<=1
                     , let P_Sign sgn = dec_sign decl; srce=head sgn; targ=last sgn
                     , Just s<-case stConcepts Data.Map.! dTerm of
                                [c] -> [c `cGlb` srce]
                                _   -> [Just srce]
                     , Just t<-case stConcepts Data.Map.! cTerm of
                                [c] -> [c `cGlb` targ]
                                _   -> [Just targ]
                     , tuple<-[(dTerm,[(decl,P_Sign [s,t])]),(cTerm,[(decl,P_Sign [s,t])])]
                     ] ++
                     [ (t,[]) | t<-typeTerms]
                   )

-- The TypGlb types are sorted to ensure that terms like ((a ./\. b) ./\. c) are handled from the inside out. In our example (a ./\. b) comes first.

{- The following function draws two graphs for educational or debugging purposes. If you want to see them, run Ampersand --typing.
-}
typeAnimate :: P_Context -> Typemap -> (DotGraph String,DotGraph String)
typeAnimate p_context st = (stTypeGraph, eqTypeGraph)
   where
     -- testTable = concat [ "\n  "++show (stNr t, eqNr t, t, map stNr eqs, map eqNr eqs)| (t,eqs)<-Data.Map.toAscList eqType]
{- The set st contains the essence of the type analysis. It contains tuples (t,t'),
   each of which means that the set of atoms contained by t is a subset of the set of atoms contained by t'. -}
     (stClos, eqType, stClosAdded, stClos1, _ , _ , _) = tableOfTypes p_context st
     typeTerms = Data.Map.keys stClos
     stNr :: Type -> Int
     stNr typ = stTable Data.Map.! typ
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
     eqNr typ = Data.Map.fromList [(t,i) | (i,cl)<-zip [0..] eqClasses, t<-cl ] Data.Map.! typ
     eqClasses :: [[Type]]             -- The strongly connected components of stGraph
     eqClasses = nub (Data.Map.elems eqType)

     eqTypeGraph :: DotGraph String
     eqTypeGraph = toDotGraph showVtx show [0..length eqClasses-1] [] condensedEdges condensedEdges2
      where showVtx n = (intercalate "\n".map showType.nub) [ original typTerm| typTerm<-typeTerms, n==eqNr typTerm]
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

{- condensedGraph is the condensed graph. Elements that are equal are brought together in the same equivalence class.
   The graph in which equivalence classes are vertices is called the condensed graph.
   These equivalence classes are the strongly connected components of the original graph
-}
     condensedEdges :: [(Int,Int)]
     condensedEdges = nub [ (nr t, nr t') | (t,t')<-flattenMap st, nr t /= nr t' ]
     nr t = eqNr (head (eqType Data.Map.! t))

     condensedEdges2 = nub [(nr t,nr t') | (t,t')<-flattenMap stClosAdded, t' `notElem` findIn t stClos1, nr t /= nr t']

class Expr a where
  p_gens :: a -> [P_Gen]
  p_gens _ = []
--  p_concs :: a -> [P_Concept]
  p_declarations :: a -> [P_Declaration]
  p_declarations _ = []
  terms :: a -> [Term]
  subterms :: a -> [Term]

instance Expr P_Context where
 p_gens pContext
  = concat [ p_gens pat | pat<-ctx_pats  pContext] ++
    concat [ p_gens prc | prc<-ctx_PPrcs pContext] ++
    ctx_gs pContext
{-
 p_concs pContext
  = nub (p_concs (ctx_pats  pContext) ++
         p_concs (ctx_PPrcs pContext) ++
         p_concs (ctx_rs    pContext) ++
         p_concs (ctx_ds    pContext) ++
         p_concs (ctx_ks    pContext) ++
         p_concs (ctx_gs    pContext) ++
         p_concs (ctx_ifcs  pContext) ++
         p_concs (ctx_pops  pContext) ++
         p_concs (ctx_sql   pContext) ++
         p_concs (ctx_php   pContext)
        )
-}
 p_declarations pContext
  = concat [ p_declarations pat | pat<-ctx_pats  pContext] ++
    concat [ p_declarations prc | prc<-ctx_PPrcs pContext] ++
    ctx_ds pContext
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
 subterms pContext
  = subterms (ctx_pats  pContext) ++
    subterms (ctx_PPrcs pContext) ++
    subterms (ctx_rs    pContext) ++
    subterms (ctx_ds    pContext) ++
    subterms (ctx_ks    pContext) ++
    subterms (ctx_ifcs  pContext) ++
    subterms (ctx_sql   pContext) ++
    subterms (ctx_php   pContext) ++
    subterms (ctx_pops  pContext)

instance Expr P_Pattern where
 p_gens pPattern
  = pt_gns pPattern
{-
 p_concs pPattern
  = nub (p_concs (pt_rls pPattern) ++
         p_concs (pt_gns pPattern) ++
         p_concs (pt_dcs pPattern) ++
         p_concs (pt_kds pPattern)
        )
-}
 p_declarations pPattern
  = pt_dcs pPattern
 terms pPattern
  = nub (terms (pt_rls pPattern) ++
         terms (pt_gns pPattern) ++
         terms (pt_dcs pPattern) ++
         terms (pt_kds pPattern) ++
         terms (pt_pop pPattern)
        )
 subterms pPattern
  = subterms (pt_rls pPattern) ++
    subterms (pt_gns pPattern) ++
    subterms (pt_dcs pPattern) ++
    subterms (pt_kds pPattern) ++
    subterms (pt_pop pPattern)

instance Expr P_Process where
 p_gens pProcess
  = procGens pProcess
{-
 p_concs pProcess
  = nub (p_concs (procRules pProcess) ++
         p_concs (procGens  pProcess) ++
         p_concs (procDcls  pProcess) ++
         p_concs (procKds   pProcess)
        )
-}
 p_declarations pProcess
  = procDcls pProcess
 terms pProcess
  = nub (terms (procRules pProcess) ++
         terms (procGens  pProcess) ++
         terms (procDcls  pProcess) ++
         terms (procKds   pProcess) ++
         terms (procPop   pProcess)
        )
 subterms pProcess
  = subterms (procRules pProcess) ++
    subterms (procGens  pProcess) ++
    subterms (procDcls  pProcess) ++
    subterms (procKds   pProcess) ++
    subterms (procPop   pProcess)

instance Expr P_Rule where
-- p_concs r = p_concs (rr_exp r)
 terms r = terms (rr_exp r)
 subterms r = subterms (rr_exp r)
instance Expr P_Sign where
-- p_concs x = nub (psign x)
 terms _ = []
 subterms _ = []
instance Expr P_Gen where
-- p_concs g        = p_concs        (Pimp (origin g) (Pid (gen_spc g)) (Pid (gen_gen g)))
 terms g          =                [Pimp (origin g) (Pid (gen_spc g)) (Pid (gen_gen g))]
 subterms g = subterms (Pimp (origin g) (Pid (gen_spc g)) (Pid (gen_gen g)))
instance Expr P_Declaration where
-- p_concs d = p_concs (dec_sign d)
 terms d = [PTyp (origin d) (Prel (origin d) (dec_nm d)) (dec_sign d)]
-- terms d = [PCps orig (Pid (head sgn)) (PCps orig (Prel orig (dec_nm d)) (Pid (last sgn)))] where P_Sign sgn = dec_sign d; orig = origin d
 subterms d = [PTyp (origin d) (Prel (origin d) (dec_nm d)) (dec_sign d)]
instance Expr P_KeyDef where
-- p_concs k = p_concs [ks_obj keyExpr | keyExpr@P_KeyExp{} <- kd_ats k]
 terms k = terms [ks_obj keyExpr | keyExpr@P_KeyExp{} <- kd_ats k]
 subterms k = subterms [ks_obj keyExpr | keyExpr@P_KeyExp{} <- kd_ats k]
instance Expr P_Interface where
-- p_concs k = p_concs (ifc_Obj k)
 terms k = terms (ifc_Obj k)
 subterms k = subterms (ifc_Obj k)
instance Expr P_ObjectDef where
-- p_concs o = p_concs (obj_ctx o) `uni` p_concs (terms (obj_msub o))
 terms o = [obj_ctx o | null (terms (obj_msub o))]++terms [PCps (origin e) (obj_ctx o) e | e<-terms (obj_msub o)]
 subterms o = subterms (obj_ctx o)++subterms (obj_msub o)
instance Expr P_SubInterface where
-- p_concs x@(P_Box{}) = p_concs (si_box x)
-- p_concs _ = []
 terms x@(P_Box{}) = terms (si_box x)
 terms _ = []
 subterms x@(P_Box{}) = subterms (si_box x)
 subterms _ = []
instance Expr P_Population where
-- p_concs x = p_concs (p_type x)
 terms pop@(P_Popu{p_type=P_Sign []}) = [Prel (p_orig pop) (p_popm pop)]
 terms pop@(P_Popu{})                 = [PTyp (p_orig pop) (Prel (p_orig pop) (p_popm pop)) (p_type pop)]
 terms pop@(P_CptPopu{})              = [Pid (PCpt (p_popm pop))]
 subterms pop = terms pop

instance Expr a => Expr (Maybe a) where
-- p_concs Nothing = []
-- p_concs (Just x) = p_concs x
 terms Nothing = []
 terms (Just x) = terms x
 subterms Nothing = []
 subterms (Just x) = subterms x
instance Expr a => Expr [a] where
-- p_concs = concat.map p_concs
 terms = concat.map terms
 subterms = concat.map subterms
instance Expr Term where
{-
 p_concs   (PI _)         = []
 p_concs   (Pid c)        = [c]
 p_concs   (Pnid c)       = [c]
 p_concs   (Patm _ _ cs)  = nub cs
 p_concs   Pnull          = []
 p_concs   (PVee _)       = []
 p_concs   (Pfull s t)    = nub [s,t]
 p_concs   (Prel{})       = []
 p_concs   (Pflp{})       = []
 p_concs (Pequ _ a b)   = p_concs a `uni` p_concs b
 p_concs (Pimp _ a b)   = p_concs a `uni` p_concs b
 p_concs (PIsc _ a b)   = p_concs a `uni` p_concs b
 p_concs (PUni _ a b)   = p_concs a `uni` p_concs b
 p_concs (PDif _ a b)   = p_concs a `uni` p_concs b
 p_concs (PLrs _ a b)   = p_concs a `uni` p_concs b
 p_concs (PRrs _ a b)   = p_concs a `uni` p_concs b
 p_concs (PCps _ a b)   = p_concs a `uni` p_concs b
 p_concs (PRad _ a b)   = p_concs a `uni` p_concs b
 p_concs (PPrd _ a b)   = p_concs a `uni` p_concs b
 p_concs (PKl0 _ a)     = p_concs a
 p_concs (PKl1 _ a)     = p_concs a
 p_concs (PFlp _ a)     = p_concs a
 p_concs (PCpl _ a)     = p_concs a
 p_concs (PBrk _ a)     = p_concs a
 p_concs (PTyp _ a sgn) = p_concs a `uni` p_concs sgn
-}
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

createVoid :: [CtxError] -> Guarded ()
createVoid x = if null x then return () else Errors x

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
    st = typing p_context
    stClos      :: Typemap                  -- (st\/stAdded)*\/I  (total and transitive)
    eqType      :: Typemap                  -- (st*/\st*~)\/I  (total, reflexive, symmetric and transitive)
    cGlb        :: P_Concept -> P_Concept -> Maybe P_Concept -- a function to compute a greates lower bound, if it exists. 
    bindings    :: Data.Map.Map Type [(P_Declaration,P_Sign)]         -- declarations that may be bound to relations, intended as a suggestion to the programmer
    isas        :: [(P_Concept,P_Concept)]                   -- 
    (stClos, eqType, _ , _ , cGlb, bindings, isas) = tableOfTypes p_context st
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
    (stTypeGraph,eqTypeGraph) = typeAnimate p_context st
    cxerrs = patcxes++rulecxes++keycxes++interfacecxes++proccxes++sPlugcxes++pPlugcxes++popcxes++deccxes++xplcxes++themeschk
    --postchcks are those checks that require null cxerrs 
    postchks = rulenmchk ++ ifcnmchk ++ patnmchk ++ cyclicInterfaces
    acds = ctx_cs p_context++concatMap pt_cds (ctx_pats p_context)++concatMap procCds (ctx_PPrcs p_context)
    agens = map (pGen2aGen "NoPattern") (ctx_gs p_context)
    (adecs,   deccxes)   = case (parallelList . map (pDecl2aDecl allpops "NoPattern")  . ctx_ds   ) p_context of
                            Checked decs -> (decs, [])
                            Errors  errs -> (fatal 930 "Do not refer to undefined declarations", errs)
    (apurp,   xplcxes)   = case (parallelList . map  pPurp2aPurp                       . ctx_ps   ) p_context of
                            Checked purps -> (purps, [])
                            Errors  errs  -> (fatal 938 "Do not refer to undefined purposes", errs)
    (pats,    patcxes)   = case (parallelList . map (pPat2aPat   allpops)              . ctx_pats ) p_context of
                            Checked pats' -> (pats', [])
                            Errors  errs  -> (fatal 938 "Do not refer to undefined patterns", errs)
    (procs,   proccxes)  = case (parallelList . map (pProc2aProc allpops)              . ctx_PPrcs) p_context of
                            Checked prcs -> (prcs, [])
                            Errors errs  -> (fatal 938 "Do not refer to undefined processes", errs)
    (ctxrules,rulecxes)  = case (parallelList . map (pRul2aRul "NoPattern")            . ctx_rs   ) p_context of
                            Checked ruls -> (ruls, [])
                            Errors errs  -> (fatal 938 "Do not refer to undefined rules", errs)
    (keys,    keycxes)   = case (parallelList . map pKDef2aKDef                        . ctx_ks   ) p_context of
                            Checked keys -> (keys, [])
                            Errors errs  -> (fatal 938 "Do not refer to undefined keys", errs)
    (ifcs,interfacecxes) = case (parallelList . map  pIFC2aIFC                         . ctx_ifcs ) p_context of
                            Checked ifcs -> (ifcs, [])
                            Errors errs  -> (fatal 938 "Do not refer to undefined interfaces", errs)
    (sqlPlugs,sPlugcxes) = case (parallelList . map (pODef2aODef [] Anything)          . ctx_sql  ) p_context of
                            Checked plugs -> (plugs, [])
                            Errors errs   -> (fatal 951 "Do not refer to undefined sqlPlugs", errs)
    (phpPlugs,pPlugcxes) = case (parallelList . map (pODef2aODef [] Anything)          . ctx_php  ) p_context of
                            Checked plugs -> (plugs, [])
                            Errors errs   -> (fatal 954 "Do not refer to undefined phpPlugs", errs)
    (allpops, popcxes)   = case (parallelList . map  pPop2aPop                         . pops     ) p_context of
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
        (adecs',deccxes')  = case (parallelList . map (pDecl2aDecl pops' (name ppat)) . pt_dcs) ppat of
                              Checked decs -> (decs, [])
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
                           , prcRRuls = arruls         -- The assignment of roles to rules.
                           , prcRRels = arrels         -- The assignment of roles to Relations.
                           , prcKds   = keys'           -- The key definitions defined in this process
                           , prcXps   = expls          -- The purposes of elements defined in this process
                           } )
        _  -> Errors [CxeOrig typeErrs "process" (name pproc) (origin pproc) | (not.null) typeErrs]
       where
        typeErrs = concat ([rulecxes']++[keycxes']++[deccxes']++[rrcxes]++editcxes++[explcxes])
        (prules,rulecxes') = case (parallelList . map (pRul2aRul (name pproc)) . procRules) pproc of
                              Checked ruls -> (ruls, [])
                              Errors errs  -> (fatal 1025 "Do not refer to undefined rules", errs)
        arrels = [(rol,rel) |rr<-rrels, rol<-rrRoles rr, rel<-rrRels rr]
        (rrels,editcxes)  = (unzip . map pRRel2aRRel            . procRRels) pproc
        agens'  = map (pGen2aGen (name pproc)) (procGens pproc)
        arruls = [(rol,rul) |rul<-rules contxt, rr<-rruls, name rul `elem` mRules rr, rol<-mRoles rr]
        (adecs',deccxes') = case (parallelList . map (pDecl2aDecl pops' (name pproc)) . procDcls) pproc of
                             Checked decs -> (decs, [])
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
                  | r<-mRules prrul, null [rul | rul<-rules contxt, name rul==r]]
         
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
    p2aPairViewSegment srcCpt trgCpt (P_PairViewText str)          = Checked (PairViewText str)
    p2aPairViewSegment srcCpt trgCpt (P_PairViewExp srcOrTgt pexp) = do { (aexpr,s,t) <- pExpr2aExpr pexp
                                                                        ; return (PairViewExp srcOrTgt aexpr)
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
                -> Guarded ObjectDef -- result: the type checked object definition (only defined if there are no type errors) and a list of type errors
    pODef2aODef parentIfcRoles universe podef 
     = do { (expr, _, tTrg) <- pExpr2aExpr (obj_ctx podef)
          ; let (msub,msubcxes) = p2a_MaybeSubInterface parentIfcRoles tTrg $ obj_msub podef
                -- Step 1: A name check ensures that all attributes have unique names
          ; let nmchk = [CxeEqAttribs (origin podef) (name (head cl)) (map obj_ctx cl)
                        |cl<-eqCl name (getSubPObjs podef),length cl>1]
                 where getSubPObjs P_Obj { obj_msub = Just (P_Box objs) } = objs
                       getSubPObjs _                                      = []
          ; case nmchk++msubcxes of
             [] -> return ( Obj { objnm   = obj_nm podef   
                                , objpos  = obj_pos podef  
                                , objctx  = expr           
                                , objmsub = msub           
                                , objstrs = obj_strs podef 
                                } )                        
             typeErrs -> Errors typeErrs
          }
    p2a_MaybeSubInterface :: [String] -> P_Concept -> Maybe P_SubInterface -> (Maybe SubInterface, [CtxError])
    p2a_MaybeSubInterface _              _    Nothing               = (Nothing, [])
    p2a_MaybeSubInterface parentIfcRoles conc (Just (P_Box p_objs)) =
      case (parallelList . map (pODef2aODef parentIfcRoles (thing conc))) p_objs of
        Checked objects -> (Just (Box objects), [])
        Errors  errs    -> (fatal 1254 "Do not refer to undefined objects", errs)
    p2a_MaybeSubInterface parentIfcRoles conc (Just (P_InterfaceRef pos nm)) =
      (Just (InterfaceRef nm), errs)
     where errs = case [ifc | ifc <- interfaces contxt, name ifc == nm ] of
                   []                                     -> [newcxe $ "Undeclared interface \""++nm++"\" at " ++show pos ++ "."]
                   (_:_:_)                                -> fatal 350 $ "Multiple interfaces for ref "++nm
                   [Ifc { ifcObj = Obj {objctx= ifcExp}, ifcRoles = thisIfcRoles }] ->
                     if source ifcExp DatabaseDesign.Ampersand.Core.Poset.< pCpt2aCpt conc
                     then [newcxe $ "Incompatible interface "++show nm++" at "++show pos++":"++
                                    "\nInterface source concept "++name (source ifcExp)++" is not equal to or a supertype of "++name conc]
                     else let unsupportedRoles = if null thisIfcRoles
                                                 then [] -- no roles specified means all roles are supported
                                                 else parentIfcRoles \\ thisIfcRoles
                          in  newcxeif (not $ null unsupportedRoles) $
                             "Interface "++show nm++", referenced at "++show pos++", does not support all roles of the containing interface. "++
                             "Unsupported roles: "++ intercalate ", " unsupportedRoles ++"."
      
    pPurp2aPurp :: PPurpose -> Guarded Purpose
    pPurp2aPurp pexpl
     = case xplcxe of
        [] -> Checked ( Expl { explPos      = pexPos   pexpl
                             , explObj      = explobs
                             , explMarkup   = pMarkup2aMarkup (ctxlang contxt) (ctxmarkup contxt) (pexMarkup pexpl)
                             , explRefId    = pexRefID pexpl
                             , explUserdefd = True
                            -- , explCont  = string2Blocks (ctxmarkup contxt) (pexExpl  pexpl)
                             })
        _  -> Errors [CxeOrig xplcxe "explanation" "" (origin pexpl) | (not.null) xplcxe]
       where (explobs,xplcxe) = pExOb2aExOb (pexObj   pexpl)
    
    pExOb2aExOb :: PRef2Obj -> (ExplObj, [CtxError])
    pExOb2aExOb (PRef2ConceptDef str  ) = (ExplConceptDef (head cds), newcxeif(null cds)("No concept definition for '"++str++"'"))
                                          where cds = [cd | cd<-conceptDefs contxt, cdcpt cd==str ]
    pExOb2aExOb (PRef2Declaration x@(PTyp o (Prel _ nm) sgn))
                                        = ( ExplDeclaration (head decls)
                                          , [CxeOrig [newcxe ("No declaration for '"++showADL x++"'")] "relation" nm o | null decls]
                                          )
                                          where decls = [d | d<-declarations contxt, name d==nm, sign d==pSign2aSign sgn ]
    pExOb2aExOb (PRef2Rule str        ) = (ExplRule (head ruls), newcxeif(null ruls)("No rule named '"++str++"'") )
                                          where ruls = [rul | rul<-rules contxt, name rul==str ]
    pExOb2aExOb (PRef2KeyDef str      ) = (ExplKeyDef (head kds), newcxeif(null kds)("No key definition named '"++str++"'") )
                                          where kds = [kd | kd<-keyDefs contxt, name kd==str]
    pExOb2aExOb (PRef2Pattern str     ) = (ExplPattern str,   newcxeif(null[pat |pat<-patterns   contxt,   name pat==str])("No pattern named '"++str++"'") )
    pExOb2aExOb (PRef2Process str     ) = (ExplProcess str,   newcxeif(null[prc |prc<-processes  contxt,  name prc==str]) ("No process named '"++str++"'") )
    pExOb2aExOb (PRef2Interface str   ) = (ExplInterface str, newcxeif(null[ifc |ifc<-interfaces contxt, name ifc==str])  ("No interface named '"++str++"'") )
    pExOb2aExOb (PRef2Context str     ) = (ExplContext str,   newcxeif(name contxt/=str) ("No context named '"++str++"'") )  
    pExOb2aExOb (PRef2Fspc str        ) = (ExplFspc str,      newcxeif(name contxt/=str) ("No specification named '"++str++"'") )
    pExOb2aExOb po = fatal 1150 ("pExOb2aExOb is non-exhaustive, unexpected PO: "++show po)
    
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
    
    pDecl2aDecl :: [Population] -> String -> P_Declaration -> Guarded Declaration
    pDecl2aDecl pops' patname pd =
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
                             , decpat  = patname
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
    
    pExpr2aExpr :: Term                              -- The term to be typed
                -> Guarded ( Expression              -- the resulting expression.
                           , P_Concept, P_Concept    -- the source and target types of the resulting expression.
                           )                         -- The result might be incorrect, so it is guarded with type error messages.
    pExpr2aExpr pTerm
     = (\r -> (r, lookupType pTerm, lookupType (p_flp pTerm))) <$> f pTerm
       -- do { r <- f pTerm ; return (r, lookupType pTerm, lookupType (p_flp pTerm))}
       -- or equivalently:
       -- case f pTerm of
       --  Checked r   -> Checked (r, lookupType pTerm, lookupType (p_flp pTerm))
       --  Errors errs -> Errors errs
       where
         f :: Term -> Guarded Expression
         f t@(PI _)                     = do { c<-returnIConcepts t
                                             ; return (ERel (I (pCpt2aCpt c)))
                                             }
         f   (Pid c)                    = return (ERel (I (pCpt2aCpt c)))
         f   (Pnid c)                   = return (ECpl (ERel (I (pCpt2aCpt c))))
         f t@(Patm _ atom [])           = do { c<-returnIConcepts t
                                             ; return (ERel (Mp1 atom (pCpt2aCpt c)))}
         f   (Patm _ atom [c])          = return (ERel (Mp1 atom (pCpt2aCpt c)))
         f t@Pnull                      = fatal 988 ("pExpr2aExpr cannot transform "++show t++" to a term.")
         f t@(PVee _)                   = ERel <$> (V <$> getSignFromTerm t)
         f (Pfull s t)                  = return (ERel (V (Sign (pCpt2aCpt s) (pCpt2aCpt t))))
         f t@(Prel o a)                 = do { (decl,sgn) <- getDeclarationAndSign t
                                             ; return (ERel (Rel{relnm=a, relpos=o, relsgn=sgn, reldcl=decl}))
                                             }
         f t@(Pflp o a)                 = do { (decl,sgn) <- getDeclarationAndSign t
                                             ; return (EFlp (ERel (Rel{relnm=a, relpos=o, relsgn=sgn, reldcl=decl})))
                                             }
         f t@(Pequ _ a b)               = do { (a',b') <- (,) <$> f a <*> f b
                                             ; _{-sign-} <- errEquLike t a b
                                             ; return (EEqu (a', b'))
                                             }
         f t@(Pimp _ a b)               = do { (a',b') <- (,) <$> f a <*> f b
                                             ; _{-sign-} <- errEquLike t a b
                                             ; return (EImp (a', b'))
                                             }
         f t@(PIsc _ a b)               = do { (a',b') <- (,) <$> f a <*> f b
                                             ; _{-sign-} <- errEquLike t a b
                                             ; return (EIsc (case a' of {EIsc ts -> ts; t'-> [t']} ++ case b' of {EIsc ts -> ts; t'-> [t']}))
                                             }
         f t@(PUni _ a b)               = do { (a',b') <- (,) <$> f a <*> f b
                                             ; _{-sign-} <- errEquLike t a b
                                             ; return (EUni (case a' of {EUni ts -> ts; t'-> [t']} ++ case b' of {EUni ts -> ts; t'-> [t']}))
                                             }
         f t@(PDif _ a b)               = do { (a',b') <- (,) <$> f a <*> f b
                                             ; _{-sign-} <- errEquLike t a b
                                             ; return (EDif (a', b'))
                                             }
         f t@(PLrs _ a b)               = do { (_{-concept-}, a', b') <- errCpsLike t a b
                                             ; return (ELrs (a', b'))
                                             }
         f t@(PRrs _ a b)               = do { (_{-concept-}, a', b') <- errCpsLike t a b
                                             ; return (ERrs (a', b'))
                                             }
         f t@(PCps _ a b)               = do { (_{-concept-}, a', b') <- errCpsLike t a b
                                             ; return (ECps (case a' of {ECps ts -> ts; t'-> [t']} ++ case b' of {ECps ts -> ts; t'-> [t']}))
                                             }
         f t@(PRad _ a b)               = do { (_{-concept-}, a', b') <- errCpsLike t a b
                                             ; return (ERad (case a' of {ERad ts -> ts; t'-> [t']} ++ case b' of {ERad ts -> ts; t'-> [t']}))
                                             }
         f (PPrd o (PPrd _ a b) c)      = do { (EPrd a_b_s, fc) <- (,) <$> f (PPrd o a b) <*> f c
                                             ; return (EPrd (a_b_s++[fc])) }
         f (PPrd o a (PPrd _ b c))      = do { (fa, EPrd b_c_s) <- (,) <$> f a <*> f (PPrd o b c)
                                             ; return (EPrd ([fa]++b_c_s)) }
         f (PPrd _ a b)                 = do { (fa, fb) <- (,) <$> f a <*> f b
                                             ; return (EPrd [fa,fb]) }
         f (PKl0 _ a)                   = do { a' <- f a
                                             ; return (EKl0 a') }
         f (PKl1 _ a)      = EKl1 <$> f a
         f (PFlp _ a)      = EFlp <$> f a
         f t@(PCpl _ a)    = do { fa <- f a; return (ECpl fa) <* errCpl t a }
         f (PBrk _ a)      = EBrk <$> f a
         -- alternatively:  WHY (SJ): Bas, waarom staat dit commentaar hier? kunnen we hier nog iets nuttigs mee? 
         -- f t@(PTyp _ (Pid _) (P_Sign _))   = do{ c<-returnIConcepts t; return (ERel (I (pCpt2aCpt c)))}
         f t@(PTyp o _ (P_Sign [])) = fatal 991 ("pExpr2aExpr cannot transform "++show t++" ("++show o++") to a term.")
         f t@(PTyp _ e@(Prel o a) sgnCast) = do { _ <- errCastType t
                                                ; (decl,_) <- getDeclarationAndSign e
                                                ; return (ERel (Rel{relnm=a, relpos=o, relsgn=pSign2aSign sgnCast, reldcl=decl}))
                                                }
         f (PTyp _ a sgn)  = ETyp <$> (f a) <*> return (Sign (pCpt2aCpt s) (pCpt2aCpt t))
                             where P_Sign cs = sgn; s=head cs; t=last cs
         f t = fatal 1542 ("Pattern match on f in pExpr2aExpr failed for "++show t)
         returnIConcepts term
          = if (length conflictingConcepts /= 1) then Errors 
            [CxeILike {cxeExpr   = term
                      ,cxeCpts   = conflictingConcepts
                      }] else return (head conflictingConcepts)
          where conflictingConcepts = getConceptsFromTerm term
         errCastType :: Term -> Guarded ()
         errCastType term@(PTyp _ e@(Prel _ _) _)
          = case (csDomCast,csCodCast,csDomTerm,csCodTerm) of
                 (  [_]    ,  [_]    ,  [_]    ,  [_]    ) -> Checked ()
                 _                                         -> Errors [CxeCast{ cxeExpr    = term
                                                                             , cxeDomCast = csDomCast
                                                                             , cxeCodCast = csCodCast
                                                                             , cxeDomTerm = csDomTerm
                                                                             , cxeCodTerm = csCodTerm
                                                                             }       
                                                                     ]
            where csDomCast = getConceptsFromType (TypExpr        term  False)
                  csCodCast = getConceptsFromType (TypExpr (p_flp term) True )
                  csDomTerm = getConceptsFromType (TypExpr        e  False)
                  csCodTerm = getConceptsFromType (TypExpr (p_flp e) True )
         errCastType term = fatal 1508 ("illegal call to errCastType ("++show term++")")
         errCpsLike :: Term -> Term -> Term -> Guarded (A_Concept, Expression, Expression)
         errCpsLike term a b
          = case conflictingConcepts of
{-           []  -> error ("\nterm:      "++ show term++
                           "\na:         "++ show a++
                           "\nb:         "++ show b++
                           show err)  -}
             [c] -> case (pExpr2aExpr a, pExpr2aExpr b) of
                      (Checked (a', _,_), Checked (b', _,_)) -> Checked (pCpt2aCpt c, a', b')
                      (Errors errsA     , Checked ( _, _,_)) -> Errors errsA
                      (Checked ( _, _,_), Errors errsB     ) -> Errors errsB
                      (Errors errsA     , Errors errsB     ) -> Errors (errsA++errsB)
             _   -> Errors [ CxeCpsLike {cxeExpr   = term
                                        ,cxeCpts   = conflictingConcepts
                                        }
                           ]
            where conflictingConcepts = getConceptsFromType (TypExpr term False)
         getConceptsFromTerm :: Term -> [P_Concept]
         getConceptsFromTerm x
          = getConceptsFromType (TypExpr x False) `uni` getConceptsFromType (TypExpr x True)
         getConceptsFromType :: Type -> [P_Concept]
         getConceptsFromType typ
          = glbs [c | TypExpr (Pid c) _ <- stClos Data.Map.! typ ]
            where
              -- the function glbs computes the greatest lower bounds of all concepts in cs.
              glbs cs = [ foldcGlb cl | cl<-eqClass eqCpt cs]
              a `eqCpt` b = case a `cGlb` b of
                              Nothing -> False
                              Just _  -> True
              foldcGlb [c]    = c
              foldcGlb (c:cs) = case c `cGlb` foldcGlb cs of
                                 Just x -> x
                                 _      -> fatal 1524 "error in glbs"
              foldcGlb _      = fatal 1525 "error in glbs"
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
         errEquLike :: Term -> Term -> Term -> Guarded Sign
         errEquLike term a b
          = getSign (\s t -> [ CxeEquLike {cxeExpr    = term
                                          ,cxeLhs     = a
                                          ,cxeRhs     = b
                                          ,cxeSrcCpts = s
                                          ,cxeTrgCpts = t}]) term
         getDeclarationAndSign :: Term -> Guarded (Declaration, Sign)
         getDeclarationAndSign term
          = case bindings Data.Map.! TypExpr term False  of
             [(d,P_Sign [s,t])] -> do { decl <- pDecl2aDecl [] "" d ; return (decl, Sign (pCpt2aCpt s) (pCpt2aCpt t)) }
             ds                 -> Errors [CxeRel {cxeExpr   = term        -- the erroneous term
                                                  ,cxeDecs   = ds          -- the declarations to which this term has been matched
                                                  }]
         errCpl :: Term -> Term -> Guarded ()
         errCpl term a
          = createVoid $  [ CxeCpl {cxeExpr   = a
                                   ,cxeCpts   = conflictingConcepts
                                   }
                          | length conflictingConcepts/=1
                          ]
            where conflictingConcepts = getConceptsFromTerm term
         lookupType :: Term -> P_Concept
         lookupType t = case getConceptsFromTerm t of
                         [c] -> c
                         []  -> fatal 1586 ("No type found for term "++show t)
                         cs  -> fatal 1586 ("Multiple types found for term "++show t++": "++show cs)

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


{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
{-# LANGUAGE RelaxedPolyRec #-} -- -RelaxedPolyRec required for OpenSuse, for as long as we@OpenUniversityNL use an older GHC
module DatabaseDesign.Ampersand.ADL1.P2A_Converters (
     -- * Exported functions
     pCtx2aCtx, disambiguate,
     Guarded(..)
     )
where
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree hiding (sortWith)
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Basics (name, uni, isc, eqCl, getCycles, (>-), eqClass, fatalMsg)
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec.Fspec
import DatabaseDesign.Ampersand.Fspec.ShowADL
import qualified DatabaseDesign.Ampersand.Core.Poset -- hiding (sortWith)
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
TypExpr e flipped origExpr is read as:
  "the source type of e, with e equal to (if flipped then PFlp origExpr else origExpr), and origExpr occurs in the P_Context p_context."
origExpr (in conjunction with Origin o) is kept for the purpose of generating messages in terms of the original term written by the user.
TypLub a b e is read as:
  "the least upper bound of types a and b, in the context of term e.
-}
data Type =  TypExpr Term Bool Term
           | TypLub Type Type Term
           | TypGlb Type Type Term
           | Anything
            -- note: do not put "deriving Ord", because Eq is specified (and not derived)

instance Show Type where
    showsPrec _ typExpr = showString (showType typExpr)

showType :: Type -> String
showType (TypExpr term@(Pid _) _ _)     = showADL term
showType (TypExpr term@(PVee o) _ _)    = showADL term     ++"("++ shOrig o++")"
showType (TypExpr term@(Pfull _ _) _ _) = showADL term
showType (TypExpr term _ _)             = showADL term     ++"("++ shOrig (origin term)++")"
showType (TypLub a b _)                 = showType a++" ./\\. "++showType b
showType (TypGlb a b _)                 = showType a++" .\\/. "++showType b
showType Anything                       = "Anything"

instance Traced Type where
  origin (TypExpr _ _ x) = origin x
  origin (TypLub _ _ x) = origin x
  origin (TypGlb _ _ x) = origin x

-- | Equality of Type is needed for the purpose of making graphs of types.
--   These are used in the type checking process.
--   The main idea is that the equality distinguishes between occurrences.
--   So term 'r' on line 14:3 differs from  the term 'r' on line 87:19.
--   However, different occurrences of specific terms that are fully typed (e.g. I[Person] or parent[Person*Person]), need not be distinguised.
instance Prelude.Ord Type where
  compare p q = comp p q
   where
    comp Anything _                                                  = Prelude.LT  -- Anything < everything
    comp _ Anything                                                  = Prelude.GT  -- everyting > Anything
    comp (TypExpr (Pid c)        _ _) (TypExpr (Pid c')         _ _) = Prelude.compare c c'
    comp (TypExpr (Pnid c)       _ _) (TypExpr (Pnid c')        _ _) = Prelude.compare c c'
    comp (TypExpr (Patm _ x [c]) _ _) (TypExpr (Patm _ x' [c']) _ _) = Prelude.compare (x,c) (x',c')
    comp (TypExpr (PVee o)       _ _) (TypExpr (PVee o')        _ _) = Prelude.compare o o' -- This is a V of which the type must be determined (by the environment).
    comp (TypExpr (Pfull s t)    _ _) (TypExpr (Pfull s' t')    _ _) = Prelude.compare (s,t) (s',t') -- This is a V of which the type is determined by the user
    comp (TypExpr e@(PTyp _ _ (P_Sign [])) _ _) (TypExpr e'@(PTyp _ _ (P_Sign [])) _ _) = Prelude.compare e e'
    comp (TypExpr (PTyp _ (Prel _ a) sgn) _ _) (TypExpr (PTyp _ (Prel _ a') sgn') _ _) = Prelude.compare (sgn,a) (sgn',a')
    comp (TypExpr (PTyp _ (Pflp _ a) sgn) _ _) (TypExpr (PTyp _ (Pflp _ a') sgn') _ _) = Prelude.compare (sgn,a) (sgn',a')
    comp (TypExpr x _ _) (TypExpr y _ _) = Prelude.compare x y
    comp (TypLub l r _) (TypLub l' r' _) = compare (l,r) (l',r')
    comp (TypGlb l r _) (TypGlb l' r' _) = compare (l,r) (l',r')
    comp (TypExpr _ _ _) _               = Prelude.LT
    comp (TypLub _ _ _)  (TypExpr _ _ _) = Prelude.GT
    comp (TypLub _ _ _) _ = Prelude.LT
    comp (TypGlb _ _ _) _ = Prelude.GT
    
normalize   (TypLub Anything right _)                                       = normalize right
normalize   (TypLub left Anything _)                                        = normalize left
normalize t@(TypLub left right x)                                           = norm t left right x
normalize   (TypGlb Anything _ _)                                           = Anything
normalize   (TypGlb _ Anything _)                                           = Anything
normalize t@(TypGlb left right x)                                           = norm t left right x
normalize   (TypExpr (PTyp _ x' (P_Sign [])) flipped x)                     = TypExpr x' flipped x
normalize   (TypExpr (PTyp _ x'@(PTyp _ _ sgn') sgn) flipped x) | sgn==sgn' = TypExpr x' flipped x
normalize t                                                                 = t
norm :: Type -> Type -> Type -> Term -> Type
norm t left right x
    = case (normalize left, normalize right) of
           (TypExpr (PI _)          _ _ , t'@(TypExpr _           _ _)) -> t'
           (TypExpr (Pid c)         _ _ , TypExpr r'              _ _ ) -> TypExpr (PTyp (origin r') r' (P_Sign [c])) False x
           (t'@(TypExpr _           _ _), TypExpr (PI _)          _ _ ) -> t'
           (TypExpr l'              _ _ , TypExpr (Pid c)         _ _ ) -> TypExpr (PTyp (origin l') l' (P_Sign [c])) False x
           (TypExpr (PVee _)        _ _ , t'                          ) -> t'
           (TypExpr (Pfull src trg) _ _ , TypExpr r'              _ _ ) -> TypExpr (PTyp (origin r') r' (P_Sign [src,trg])) False x
           (t'                          , TypExpr (PVee _)        _ _ ) -> t'
           (TypExpr l'              _ _ , TypExpr (Pfull src trg) _ _ ) -> TypExpr (PTyp (origin l') l' (P_Sign [src,trg])) False x
           (_                           , _                           ) -> t


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
p_flp a            = PFlp OriginUnknown a

complement :: Term -> Term
complement (PCpl _ a)     = a
complement (Pnid c)       = Pid c
complement a              = PCpl (origin a) a

thing :: P_Concept -> Type
thing c  = TypExpr ic False ic where ic=Pid c

type Typemap = [(Type,Type)] --Data.Map.Map Type [Type]

mapIsOk :: Ord a => Data.Map.Map k [a] -> Bool
mapIsOk m = Data.Map.fold (&&) True (Data.Map.map isSortedAndDistinct m)
 where isSortedAndDistinct (x:y:rest) = if (x<y) then isSortedAndDistinct (y:rest) else False
       isSortedAndDistinct _ = True
-- | The purpose of 'setClosure' is to compute the transitive closure of relations that are represented as a Map (Data.Map.Map a [a]).
--   For that purpose we use a Warshall algorithm.
setClosure :: Ord a => Data.Map.Map a [a] -> String -> Data.Map.Map a [a]
setClosure xs s | not (mapIsOk xs) = fatal 144 ("setClosure on the non-ok set "++s)
setClosure xs _ = if (mapIsOk res) then res else fatal 145 ("setClosure contains errors!")
  where
--   f q x = Data.Map.map (\bs->foldl merge bs [b' | b<-bs, b == x, (a', b') <- Data.Map.toList q, a' == x]) q
   f q x = Data.Map.map (\bs->foldl merge bs [b' | b<-bs, b == x, (Just b') <- [Data.Map.lookup x q]]) q
   res   = foldl f xs (Data.Map.keys xs `isc` nub (concat (Data.Map.elems xs)))

merge :: Ord a => [a] -> [a] -> [a]
merge (a:as) (b:bs) | a<b  = a:merge as (b:bs)
                    | a==b = a:merge as bs
                    | a>b  = b:merge (a:as) bs
merge a b = a ++ b -- since either a or b is the empty list

-- | lookups is the reflexive closure of findIn. lookups(a,R) = findIn(a,R\/I) where a is an element and R is a relation.
lookups :: Ord a => a -> Data.Map.Map a [a] -> [a]
lookups o q = head ([merge [o] e | (Just e)<-[Data.Map.lookup o q]]++[[o]])
{- Trying to understand lookups:
lookups "2" [("1",["aap","noot","mies"]), ("2",["vuur","mus"])]
= 
head ([merge [o] e | (Just e)<-[Data.Map.lookup "2" [("1",["aap","noot","mies"]), ("2",["vuur","mus"])]]]++[["2"]])
= 
head ([merge ["2"] e | (Just e)<-[Just ["vuur","mus"]]]++[["2"]])
= 
head ([merge ["2"] ["vuur","mus"] ]++[["2"]])
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
--   For any two Terms a and b,  if dom(a) is a subset of dom(b), this is represented as a tuple (TypExpr a _ _,TypExpr b _ _) in st.
--   In the code below, this shows up as  dom a.<.dom b
--   The function typing does a recursive scan through all subexpressions, collecting all tuples on its way.
--   Besides term term, this function requires a universe in which to operate.
--   Specify 'Anything Anything' if there are no restrictions.
--   If the source and target of term is restricted to concepts c and d, specify (thing c) (thing d).
typing :: P_Context -> Data.Map.Map Type [Type] -- subtypes (.. is subset of ..)
typing p_context
 = Data.Map.unionWith merge secondSetOfEdges firstSetOfEdges
   where
     (firstSetOfEdges,secondSetOfEdges)
      = makeDataMaps
        (foldr (.+.) nothing ([uType term Anything Anything term | term <- terms p_context] ++
                              [dom spc.<.dom gen | g<-p_gens p_context
                                                 , let spc=Pid (gen_spc g)
                                                 , let gen=Pid (gen_gen g)
                                                 -- , let x=Pimp (origin g) spc gen
                                                 ]))
     makeDataMaps :: (Typemap,Typemap) -> (Data.Map.Map Type [Type],Data.Map.Map Type [Type])
     makeDataMaps (a,b) = (makeDataMap a, makeDataMap b)
     makeDataMap lst = Data.Map.fromDistinctAscList (foldr compress [] (sort lst))
       where
         compress (a,b) o@(r:rs) = if a==(fst r) then (a,addTo b (snd r)):rs else (a,[b]):o
         compress (a,b) [] = [(a,[b])]
         addTo b o@(c:_) | c==b = o
         addTo b o = b:o
     stClos   = setClosure firstSetOfEdges "firstSetOfEdges" -- the transitive closure of 'firstSetOfEdges'
     decls    = p_declarations p_context
     pDecls   = [PTyp (origin d) (Prel (origin d) (dec_nm d)) (dec_sign d) | d<-decls]
     isTypeSubset t c = c `elem` findIn t stClos
     uType :: Term    -- x:    the original term from the script, meant for representation in the graph.
           -> Type            -- uLft: the type of the universe for the domain of x 
           -> Type            -- uRt:  the type of the universe for the codomain of x
           -> Term    -- z:    the term to be analyzed, which must be logically equivalent to x
           -> ( Typemap  -- for each type, a list of types that are subsets of it, which is the result of analysing term x.
              , Typemap ) -- for some edges, we need to know the rest of the graph. These can be created in this second part.
     uType _ _    _     (Pnid _)              = fatal 136 "Pnid has no representation"
     uType x _    _     (PI{})                = dom x.=.cod x                                                        -- I
     uType x _    _     (Pid{})               = dom x.=.cod x                                                        -- I[C]
     uType x _    _     (Patm _ _ [])         = dom x.=.cod x                                                        -- 'Piet'   (an untyped singleton)
     uType x _    _     (Patm _ _ cs)         = dom x.<.dom (Pid (head cs)) .+. cod x.<.cod (Pid (last cs))          -- 'Piet'[Persoon]  (a typed singleton)
     uType _ _    _      Pnull                = nothing                                                              -- -V     (the empty set)
     uType x uLft uRt   (PVee _)              = dom x.<.uLft .+. cod x.<.uRt
     uType x _    _     (Pfull s t)           = dom x.=.dom (Pid s) .+. cod x.=.cod (Pid t)                          --  V[A*B] (the typed full set)
     uType x uLft uRt   (Prel _ nm)           = carefully ( -- what is to come will use the first iteration of edges, so to avoid loops, we carefully only create second edges instead
                                                                  if length spcls == 1 then dom x.=.dom (head spcls) .+. cod x.=.cod (head spcls)
                                                                  else nothing
                                                          )
                                                where decls' = [decl | decl@(PTyp _ (Prel _ dnm) _)<-pDecls, dnm==nm ]
                                                      spcls = if length decls'==1 then decls' else
                                                              [d    | d@(PTyp _ (Prel _ _) (P_Sign cs@(_:_)))<-decls', compatible (head cs) (last cs)]
                                                      compatible l r =    isTypeSubset uLft (thing l)
                                                                       && isTypeSubset uRt  (thing r)
     uType x uLft uRt   (Pequ _ a b)          = dom a.=.dom b .+. cod a.=.cod b .+. dom b.=.dom x .+. cod b.=.cod x  --  a=b    equality
                                                 .+. uType a uLft uRt a .+. uType b uLft uRt b 
     uType x uLft uRt   (PIsc _ a b)          = dom x.=.interDom .+. cod x.=.interCod    --  intersect ( /\ )
                                                .+. dm .+. cm
                                                .+. uType a interDom2 interCod2 a .+. uType b interDom2 interCod2 b
                                                where (dm,interDom) = mSpecific (dom a) (dom b)  x
                                                      (cm,interCod) = mSpecific (cod a) (cod b)  x
                                                      (_,interDom2) = mSpecific interDom uLft  x -- probably the first bits are needed, but if there is no try*.adl that reports a bug caused by this commenting, please keep it disabled
                                                      (_,interCod2) = mSpecific interCod uRt   x
     uType x uLft uRt   (PUni _ a b)          = dom x.=.interDom .+. cod x.=.interCod    --  union     ( \/ )
                                                .+. dm .+. cm
                                                .+. uType a interDom2 interCod2 a .+. uType b interDom2 interCod2 b
                                                where (dm,interDom) = mGeneric (dom a) (dom b)  x
                                                      (cm,interCod) = mGeneric (cod a) (cod b)  x
                                                      (_,interDom2) = mSpecific interDom uLft  x -- probably the first bits are needed, but if there is no try*.adl that reports a bug caused by this commenting, please keep it disabled
                                                      (_,interCod2) = mSpecific interCod uRt   x
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
     uType x uLft uRt   (PBrk _ e)            = uType x uLft uRt e                                                     -- (e) brackets
     uType _  _    _    (PTyp _ _ (P_Sign []))= fatal 196 "P_Sign is empty"
     uType _  _    _    (PTyp _ _ (P_Sign (_:_:_:_))) = fatal 197 "P_Sign too large"
     uType x  _    _    (PTyp o e (P_Sign cs))= dom x.<.iSrc  .+. cod x.<.iTrg  .+.                                  -- e[A*B]  type-annotation
                                                if o `elem` [origin d| d<-decls]
                                                then nothing
                                                else dom x.<.dom e .+. cod x.<.cod e
                                                     .+. uType e iSrc iTrg e
                                                where iSrc = thing (head cs)
                                                      iTrg = thing (last cs)
     uType x uLft uRt   (PPrd _ a b)          = dom x.<.dom a .+. cod x.<.cod b                                        -- a*b cartesian product
                                                .+. uType a uLft Anything a .+. uType b Anything uRt b
     -- derived uTypes: the following do no calculations themselves, but merely rewrite terms to the ones we covered
     uType x uLft uRt   (Pflp o nm)           = dom x.=.cod e .+. cod x.=.dom e .+. uType e uRt uLft e
                                                where e = Prel o nm
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
     nothing = ([],[])
     {-
     isFull (TypExpr (Pfull _ _) _ _) = True
     isFull (TypLub a b _) = isFull a && isFull b
     isFull (TypGlb a b _) = isFull a && isFull b
     isFull _ = False -}
     isNull (TypExpr Pnull _ _) = True
     isNull (TypLub a b _) = isNull a && isNull b
     isNull (TypGlb a b _) = isNull a && isNull b
     isNull _ = False
     infixl 2 .+.   -- concatenate two lists of types
     infixl 3 .<.   -- makes a list of one tuple (t,t'), meaning that t is a subset of t'
     infixl 3 .=.   -- makes a list of two tuples (t,t') and (t',t), meaning that t is equal to t'
     (.<.) :: Type -> Type -> (Typemap , Typemap)
     _ .<. Anything = nothing
     a .<. _ | isNull a = nothing
     a .<. b  = ([(a,b),(b,b)],snd nothing) -- (Data.Map.fromList [(a, [b]),(b, [])],nothing) -- a tuple meaning that a is a subset of b, and introducing b as a key.
     (.=.) :: Type -> Type -> (Typemap, Typemap)
     a .=. b  = ([(a,b),(b,a)],snd nothing) -- (Data.Map.fromList [(a, [b]),(b, [a])],nothing)
     (.++.) :: Typemap -> Typemap -> Typemap
     m1 .++. m2  = m1 ++ m2 -- Data.Map.unionWith merge m1 m2
     (.+.) :: (Typemap , Typemap) -> (Typemap , Typemap) -> (Typemap, Typemap)
     (a,b) .+. (c,d) = (c.++.a,d.++.b)
     carefully :: (Typemap , Typemap ) -> (Typemap, Typemap)
     carefully x = (fst nothing,fst x.++.snd x)
     dom, cod :: Term -> Type
     dom x    = TypExpr x         False x -- the domain of x, and make sure to check subexpressions of x as well
     cod x    = TypExpr (p_flp x) True  x 
     mSpecific, mGeneric :: Type -> Type -> Term -> ( (Typemap , Typemap) ,Type)
     mSpecific a b e = (r .<. a .+. r .<. b , r) where r = TypLub a b e
     mGeneric  a b e = (a .<. r .+. b .<. r , r) where r = TypGlb a b e

flattenMap :: Data.Map.Map t [t1] -> [(t, t1)]
flattenMap = Data.Map.foldWithKey (\s ts o -> o ++ [(s,t)|t<-ts]) []

{- The following table is a data structure that is meant to facilitate drawing type graphs and creating the correct messages for users.
This table is organized as follows:
Int          : a vertex (number) in the stGraph, which contains the raw tuples from function 'typing'
Int          : a vertex (number) in the condensedGraph, which is a condensed form of the stGraph, leaving the semantics identical
Type         : a type term, containing a Term, which is represented by a number in the type graphs. Different terms may carry the same number in the condensedGraph.
[P_Concept]  : a list of concepts. If (_,_,term,cs) is an element of this table, then for every c in cs there is a proof that dom term is a subset of I[c].
               For a type correct term, list cs contains precisely one element.
-}
tableOfTypes :: Data.Map.Map Type [Type] -> ([(Int,Int,Type,[P_Concept])], [(P_Concept,P_Concept)], [Int],[(Int,Int)],[Int],[(Int,Int)],[(Int,Int)])
tableOfTypes st
  = ( table
  -- isas is produced for the sake of making a partial order of concepts in the A-structure.
    , isas   -- a list containing all tuples of concepts that are in the subset relation with each other.
             -- it is used for the purpose of making a poset of concepts in the A-structure.
  -- the following are merely for drawing graphs.
    , [0..length typeExpressions-1]
    , stEdges
    , map fst classTable
    , (map (\(x,y)->(classNr x,classNr y)) condensedEdges)
    , (map (\(x,y)->(classNr x,classNr y)) condensedEdges2)
    ) 
 where
{-  stGraph is a graph whose edges are precisely st, but each element in
    st is replaced by a pair of integers. The reason is that datatype
    Graph expects integers. The list st contains the essence of the
    type analysis. It contains tuples (t,t'), each of which means
    that the set of atoms contained by dom t is a subset of the set
    of atoms contained by dom t'.
-}
     typeExpressions :: [Type]     -- a list of all type terms in st.
     typeExpressions = Data.Map.keys st
     expressionTable :: [(Int, Type)]
     expressionTable = [(i,typeExpr) | (i,typeExpr)<-zip [0..] typeExpressions ]
     expressionNr :: Type -> Int
     expressionNr t  = head ([i | (i,v)<-expressionTable, t == v]++[fatal 178 ("Type Term "++show t++" not found by expressionNr")])
-- In order to compute the condensed graph, we need the transitive closure of st:
     stClos1 = setClosure st "st"  -- the transitive closure of 'st'
-- The TypLub types are sorted to ensure that terms like ((a ./\. b) ./\. c) are handled from the inside out. In our example (a ./\. b) comes first.
     someWhatSortedLubs = sortBy compr [l | l@(TypLub _ _ _) <- Data.Map.keys st]
      where
      -- Compr uses the stClos1 to decide how (a ./\. b) and ((a ./\. b) ./\. c) should be sorted
       compr a b  = if b `elem` (lookups a stClos1) then LT else
                    if a `elem` (lookups b stClos1) then GT else EQ
      {- this definition of compr should be equivalent to:
       compr a b  = if b==a  ||  b `elem` findIn a stClos1 then LT else
                    if a==b  ||  a `elem` findIn b stClos1 then GT else EQ
      -}
      -- Why do we need to add the TypLubs? Aren't they already in there?
     stClosAdded = foldl f stClos1 someWhatSortedLubs
       where
        f :: Data.Map.Map Type [Type] -> Type -> Data.Map.Map Type [Type] 
        f q o@(TypLub a b _) = Data.Map.map (\cs -> foldr merge cs [lookups o q | a `elem` cs, b `elem` cs]) q
        f _ o = fatal 406 ("Inexhaustive pattern in f in stClosAdded in tableOfTypes: "++show o)
{- snap ik niet.... Kijk eens naar de lijst [lookups o q | a `elem` cs, b `elem` cs].
   Die heeft een lengte van 0 of 1. De foldr is dus overbodig. 
   De code had ook mogen luiden:
        f q o@(TypLub a b _) = Data.Map.map (\cs -> merge cs (head ([lookups o q | a `elem` cs, b `elem` cs] ++ [[]]))) q
        f _ o = fatal 406 ("Inexhaustive pattern in f in stClosAdded in tableOfTypes: "++show o)
   Hoe zit dat?
   Antwoord: waarom laten we de foldr niet staan? Die is korter.
-}

     stClos :: Data.Map.Map Type [Type] -- ^ represents the transitive clusure of st.
     stClos = setClosure stClosAdded "stClosAdded"
     -- stEdges is only defined for the sake of drawing pictures
     stEdges :: [(Int,Int)]
     stEdges = [(i,j) | (s,t) <- flattenMap st, let (i,j)=(expressionNr s,expressionNr t), i/=j]
{- condensedGraph is the condensed graph. Elements that are equal are brought together in the same equivalence class.
   The graph in which equivalence classes are vertices is called the condensed graph.
   These equivalence classes are the strongly connected components of the original graph
-}
-- | a vertex in the condensed graph contains the TypExprs that are in a cycle of the stGraph.
     eqClasses :: [[Type]]             -- The strongly connected components of stGraph
     eqClasses = efficientNub (sort (Data.Map.elems (addIdentity (reflexiveMap stClos))))
      where addIdentity = Data.Map.mapWithKey (\k a->if null a then [k] else (if k `elem` a then a else fatal 404 ("reflexiveMap must yield something reflexive! But "++show a++" does not contain "++show k))) -- Should reflexiveMap contain an element with an empty list, we don't want the element to get lost.
--note:  To compute equivalence classes of relation st, we need st*/\st*~
     efficientNub :: [[Type]] -> [[Type]] -- should be sorted!
     efficientNub ((a:_):bs@(b:_):rs) | a==b = efficientNub (bs:rs) -- efficient nub for sorted classes: only compares the first element
     efficientNub (a:b:_) | a>b = fatal 408 "efficientNub is called on a non-sorted list"
     efficientNub (a:rs) = a:efficientNub rs
     efficientNub [] = []
     exprClass :: Type -> [Type]
     exprClass typ = case classes of
                      []   -> fatal 191 ("Type Term "++show typ++" not found by exprClass"++    -- SJ: 13 sep 2012 This error has occured in Ticket #344
                                         if null ([i | (i,v)<-expressionTable, typ == v]) then "" else "\n  expressionNr = "++show (expressionNr typ)
                                        )
                      [cl] -> cl
                      _    -> fatal 372 ("Type Term "++show typ++" is found in multiple classes:\n  Class: "++
                                         intercalate "\n  Class: " [ intercalate "\n         " (map show cl) | cl<-classes ]++
                                         if null ([i | (i,v)<-expressionTable, typ == v]) then "" else "\n  expressionNr = "++show (expressionNr typ)
                                        )
      where classes = [cl | cl<-eqClasses, typ `elem` cl]
     condensedEdges :: [([Type],[Type])]
     condensedEdges = nub $ sort [(c,c') | (i,i')<-flattenMap st, let c=exprClass i, let c'=exprClass i', head c/=head c']
     condensedEdges2 = nub $ sort [(c,c') | (i,i')<-flattenMap stClosAdded, i' `notElem` findIn i stClos1, let c=exprClass i, let c'=exprClass i', head c/=head c']
     condensedClos  = nub $ sort [(c,c') | (i,i')<-flattenMap stClos, let c=exprClass i, let c'=exprClass i', head c/=head c']
     -- condensedVerts is eqClasses
     -- condensedVerts = nub [c | (i,i')<-condensedEdges, c<-[i,i']]
     classTable :: [(Int,[Type])]
     classTable = zip [0..] eqClasses
     classNr :: [Type] -> Int
     classNr (t:_)  = head ([i | (i,(v:_))<-classTable, t == v]++[fatal 178 ("Type Class "++show t++" not found by classNr")])
     classNr [] = fatal 385 "no ClassNr for an empty class. An empty class was generated somewhere, which is very wrong!"
     -- | if lst represents a binary relation, then reverseMap lst represents its inverse (viz. flip, wok)
     -- | note that the domain must be preserved!
     reverseMap :: (Prelude.Ord a) => Data.Map.Map a [a] -> Data.Map.Map a [a]
     reverseMap lst = (Data.Map.fromListWith merge (concat [(a,[]):[(b,[a])|b<-bs] | (a,bs)<-Data.Map.toList lst]))
     -- note: reverseMap is relatively slow, but only needs to be calculated once
     -- | if lst represents a binary relation r, then reflexiveMap lst represents r/\r~
     reflexiveMap :: (Prelude.Ord a) => Data.Map.Map a [a] -> Data.Map.Map a [a]
     reflexiveMap lst = Data.Map.map sort (Data.Map.intersectionWith isc lst (reverseMap lst))
     -- if not all of reflexiveMap will be used, a different implementation might be more useful
     -- classNumbers is the relation from stGraph to condensedGraph, relating the different numberings
     -- classNumbers = sort [ (exprNr,classNr) | (classNr,eClass)<-zip [0..] eqClasses, exprNr<-eClass]
{-  The following table is made by merging expressionTable and classNumbers into one list.
    This has already caught mistakes in the past, so it is advisable to leave the checks in the code for debugging reasons.
-}
     table
      = [ (expressionNr s,classNr c,s, typeConcepts c)
             | c<-eqClasses,s<-c]
-- function typeConcepts computes the type 
     typeConcepts :: [Type] -> [P_Concept]
     typeConcepts cls = if null isAType then [ c | (i,_,c)<-reducedTypes, i==cls] else isAType
       where isAType = nub [c | TypExpr (Pid c) _ _<-cls]
     -- The possible types are all concepts of which term i is a subset.
     possibleTypes :: [([Type],[Type],P_Concept)]
     possibleTypes = [ (i,j,c) | (i,j) <- condensedClos, TypExpr (Pid c) _ _<- j, i/=j ]
     typeSubsets   = [ (i,j) | (i,j,_) <- possibleTypes, TypExpr (Pid _) _ _<- i ]
     secondaryTypes= [ (i,j') | (i,j,_) <- possibleTypes, (i',j')<-typeSubsets, head i'==head j]
     -- reducedtypes contains all types for which there is not a more specific type
     reducedTypes  = [ (i,j,c) | (i,j,c) <- possibleTypes, null [j | (i',j')<-secondaryTypes,head i==head i',head j==head j']]
     isas = [ (x,g) | (i,j)<-typeSubsets, TypExpr (Pid x) _ _<- i, TypExpr (Pid g) _ _<- j ]

 -- for debug
showTypeTable :: [(Int,Int,Type,[P_Concept])] -> String
showTypeTable typeTable
 = "Type table has "++show (length typeTable)++" rows.\n  " ++ intercalate "\n  " (map showLine typeTable)
   where  -- hier volgt een (wellicht wat onhandige, maar goed...) manier om de type table leesbaar neer te zetten.
    nMax = maximum [i | (stIndex,cIndex,_,_)<-typeTable, i<-[stIndex, cIndex]]
    sh i = [ ' ' | _<-[length (show i)..length (show nMax)] ]++show i
    shPos t = str++[ ' ' | _<-[length str..maxPos] ]
     where str = showPos t
           maxPos = maximum [length (showPos (origin typExpr)) | (_,_,typExpr,_)<-typeTable]
    shType t = str++[ ' ' | _<-[length str..maxType] ]
     where str = show t
           maxType = maximum [length (show typExpr) | (_,_,typExpr,_)<-typeTable]
    shExp t = str++[ ' ' | _<-[length str..maxExpr] ]
     where str = showADL (showTypeExpr t)
           maxExpr = maximum [length (showADL (showTypeExpr typExpr)) | (_,_,typExpr,_)<-typeTable]
    showLine (stIndex,cIndex,t,concepts) = sh stIndex++","++sh cIndex++", "++shPos (origin t)++"  "++shExp t++"  "++shType t++"  "++show concepts
    showTypeExpr (TypLub _ _ term)  = term
    showTypeExpr (TypGlb _ _ term)  = term
    showTypeExpr (TypExpr term _ _) = term
    showTypeExpr Anything = fatal 427 "cannot showTypeExpr Anything"
    showPos OriginUnknown = "Unknown"
    showPos (FileLoc (FilePos (_,Pos l c,_)))
       = "("++show l++":"++show c++")"
    showPos _ = fatal 517 "Unexpected pattern in showPos"

original :: Type -> Term
original (TypExpr _ _ e) = e
original (TypLub  _ _ e) = e
original (TypGlb  _ _ e) = e
original Anything = fatal 437 "Anything cannot be expressed as a term."

{- The following function draws two graphs for educational or debugging purposes. If you want to see them, run Ampersand --typing.
-}
typeAnimate :: Data.Map.Map Type [Type] -> (DotGraph String,DotGraph String)
typeAnimate st = (stTypeGraph, condTypeGraph)
   where
{- The set st contains the essence of the type analysis. It contains tuples (t,t'),
   each of which means that the set of atoms contained by t is a subset of the set of atoms contained by t'. -}
    (typeTable,_,stVtx,stEdg,cdVtx,cdEdg,cdEdg2) = tableOfTypes st
    stTypeGraph :: DotGraph String
    stTypeGraph = toDotGraph showStVertex show stVtx [] stEdg []
    condTypeGraph :: DotGraph String
    condTypeGraph = toDotGraph showVtx show cdVtx [] cdEdg cdEdg2
     where showVtx n = (intercalate "\n".nub)
                       [ showType (head cl)
                       | cl<-eqCl original [ typExpr| (_, classNr, typExpr,_)<-typeTable, n==classNr ]
                       ]
    showStVertex :: Int -> String
    showStVertex i
     = head ([ showType e | (exprNr, _, e,_)<-typeTable, i==exprNr ]++fatal 506 ("No term numbered "++show i++" found by showStVertex"))

class Expr a where
  p_gens :: a -> [P_Gen]
  p_gens _ = []
  p_concs :: a -> [P_Concept]
  p_declarations :: a -> [P_Declaration]
  p_declarations _ = []
  terms :: a -> [Term]
  subexpressions :: a -> [Term]

instance Expr P_Context where
 p_gens pContext
  = concat [ p_gens pat | pat<-ctx_pats  pContext] ++
    concat [ p_gens prc | prc<-ctx_PPrcs pContext] ++
    ctx_gs pContext
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
         terms (ctx_ifcs  pContext) ++
         terms (ctx_sql   pContext) ++
         terms (ctx_php   pContext) ++
         terms (ctx_pops  pContext)
        )
 subexpressions pContext
  = subexpressions (ctx_pats  pContext) ++
    subexpressions (ctx_PPrcs pContext) ++
    subexpressions (ctx_rs    pContext) ++
    subexpressions (ctx_ds    pContext) ++
    subexpressions (ctx_ks    pContext) ++
    subexpressions (ctx_ifcs  pContext) ++
    subexpressions (ctx_sql   pContext) ++
    subexpressions (ctx_php   pContext) ++
    subexpressions (ctx_pops  pContext)

instance Expr P_Pattern where
 p_gens pPattern
  = pt_gns pPattern
 p_concs pPattern
  = nub (p_concs (pt_rls pPattern) ++
         p_concs (pt_dcs pPattern) ++
         p_concs (pt_kds pPattern)
        )
 p_declarations pPattern
  = pt_dcs pPattern
 terms pPattern
  = nub (terms (pt_rls pPattern) ++
         terms (pt_dcs pPattern) ++
         terms (pt_kds pPattern) ++
         terms (pt_pop pPattern)
        )
 subexpressions pPattern
  = subexpressions (pt_rls pPattern) ++
    subexpressions (pt_dcs pPattern) ++
    subexpressions (pt_kds pPattern) ++
    subexpressions (pt_pop pPattern)

instance Expr P_Process where
 p_gens pProcess
  = procGens pProcess
 p_concs pProcess
  = nub (p_concs (procRules pProcess) ++
         p_concs (procDcls  pProcess) ++
         p_concs (procKds   pProcess)
        )
 p_declarations pProcess
  = procDcls pProcess
 terms pProcess
  = nub (terms (procRules pProcess) ++
         terms (procDcls  pProcess) ++
         terms (procKds   pProcess) ++
         terms (procPop   pProcess)
        )
 subexpressions pProcess
  = subexpressions (procRules pProcess) ++
    subexpressions (procDcls  pProcess) ++
    subexpressions (procKds   pProcess) ++
    subexpressions (procPop   pProcess)

instance Expr P_Rule where
 p_concs r = p_concs (rr_exp r)
 terms r = terms (rr_exp r)
 subexpressions r = subexpressions (rr_exp r)
instance Expr P_Sign where
 p_concs x = nub (psign x)
 terms _ = []
 subexpressions _ = []
instance Expr P_Gen where
 p_concs x = nub [gen_gen x, gen_spc x]
 terms _ = []
 subexpressions _ = []
instance Expr P_Declaration where
 p_concs d = p_concs (dec_sign d)
 terms d = [PTyp (origin d) (Prel (origin d) (dec_nm d)) (dec_sign d)]
-- terms d = [PCps orig (Pid (head sgn)) (PCps orig (Prel orig (dec_nm d)) (Pid (last sgn)))] where P_Sign sgn = dec_sign d; orig = origin d
 subexpressions d = [PTyp (origin d) (Prel (origin d) (dec_nm d)) (dec_sign d)]
instance Expr P_KeyDef where
 p_concs k = p_concs [ks_obj keyExpr | keyExpr@P_KeyExp{} <- kd_ats k]
 terms k = terms [ks_obj keyExpr | keyExpr@P_KeyExp{} <- kd_ats k]
 subexpressions k = subexpressions [ks_obj keyExpr | keyExpr@P_KeyExp{} <- kd_ats k]
instance Expr P_Interface where
 p_concs k = p_concs (ifc_Obj k)
 terms k = terms (ifc_Obj k)
 subexpressions k = subexpressions (ifc_Obj k)
instance Expr P_ObjectDef where
 p_concs o = p_concs (obj_ctx o) `uni` p_concs (terms (obj_msub o))
 terms o = [obj_ctx o | null (terms (obj_msub o))]++terms [PCps (origin o) (obj_ctx o) e | e<-terms (obj_msub o)]
 subexpressions o = subexpressions (obj_ctx o)++subexpressions (obj_msub o)
instance Expr P_SubInterface where
 p_concs x@(P_Box{}) = p_concs (si_box x)
 p_concs _ = []
 terms x@(P_Box{}) = terms (si_box x)
 terms _ = []
 subexpressions x@(P_Box{}) = subexpressions (si_box x)
 subexpressions _ = []
instance Expr P_Population where
 p_concs x = p_concs (p_type x)
 terms pop@(P_Popu{p_type=P_Sign []}) = [Prel (p_orig pop) (p_popm pop)]
 terms pop@(P_Popu{})                 = [PTyp (p_orig pop) (Prel (p_orig pop) (p_popm pop)) (p_type pop)]
 terms pop@(P_CptPopu{})              = [Pid (PCpt (p_popm pop))]
 subexpressions pop = terms pop

instance Expr a => Expr (Maybe a) where
 p_concs Nothing = []
 p_concs (Just x) = p_concs x
 terms Nothing = []
 terms (Just x) = terms x
 subexpressions Nothing = []
 subexpressions (Just x) = subexpressions x
instance Expr a => Expr [a] where
 p_concs = concat.map p_concs
 terms = concat.map terms
 subexpressions = concat.map subexpressions
instance Expr Term where
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
 terms e = [e]
 subexpressions e@(Pequ _ a b) = [e]++subexpressions a++subexpressions b
 subexpressions e@(Pimp _ a b) = [e]++subexpressions a++subexpressions b
 subexpressions e@(PIsc _ a b) = [e]++subexpressions a++subexpressions b
 subexpressions e@(PUni _ a b) = [e]++subexpressions a++subexpressions b
 subexpressions e@(PDif _ a b) = [e]++subexpressions a++subexpressions b
 subexpressions e@(PLrs _ a b) = [e]++subexpressions a++subexpressions b
 subexpressions e@(PRrs _ a b) = [e]++subexpressions a++subexpressions b
 subexpressions e@(PCps _ a b) = [e]++subexpressions a++subexpressions b
 subexpressions e@(PRad _ a b) = [e]++subexpressions a++subexpressions b
 subexpressions e@(PPrd _ a b) = [e]++subexpressions a++subexpressions b
 subexpressions e@(PKl0 _ a)   = [e]++subexpressions a
 subexpressions e@(PKl1 _ a)   = [e]++subexpressions a
 subexpressions e@(PFlp _ a)   = [e]++subexpressions a
 subexpressions e@(PCpl _ a)   = [e]++subexpressions a
 subexpressions e@(PBrk _ a)   = [e]++subexpressions a
 subexpressions e@(PTyp _ a _) = [e]++subexpressions a
 subexpressions e              = [e]


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

data Guarded a = Errors [CtxError] | Checked a deriving (Eq)

{-
parallelList :: [Guarded a] -> Guarded [a] -- get all values or collect all error messages
parallelList = foldr (parallel (:)) (Checked [])
  where parallel :: (a->b->c) -> Guarded a -> Guarded b -> Guarded c -- get both values or collect all error messages
        parallel f ga = (<*>) (fmap f ga)
 -- the following definition is equivalent:
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
 pure a = Checked a
 (<*>) (Checked f) (Checked a) = Checked (f a)
 (<*>) (Errors  a) (Checked _) = Errors a 
 (<*>) (Checked _) (Errors  b) = Errors b
 (<*>) (Errors  a) (Errors  b) = Errors (a ++ b)
 
instance Monad Guarded where
 (>>=) (Errors  a) _ = (Errors a)
 (>>=) (Checked a) f = f a
 return = Checked
 fail s = fatal 926 ("Error generated by fail of a Guarded something (probably a pattern-match failure in a `do'), message: \n "++s)

fatalGet :: Int -> String -> Guarded a -> a
fatalGet nr msg (Errors _) = fatal nr msg
fatalGet _ _ (Checked a)   = a

getErrors :: Guarded t -> [CtxError]
getErrors (Errors a) = a
getErrors _ = []

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
   ,stTypeGraph,condTypeGraph)
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
    (typeTable,isas,_,_,_,_,_) = tableOfTypes st
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
    derivedEquals   -- These concepts can be proven to be equal, based on st (= typing sentences, i.e. the expressions derived from the script).
     = [ CxeEqConcepts [ c| (_,_,TypExpr (Pid c) _ _)<-diffs]
       | diffs<-eqCl (\(_,classNr,_) -> classNr) (map head (eqClass tripleEq conceptTypes))
       , length diffs>1]
       where (_,_,t) `tripleEq` (_,_,t') = t == t'
    conceptTypes :: [(Int,Int,Type)]
    conceptTypes = [ (exprNr, classNr, e) | (exprNr, classNr, e@(TypExpr (Pid{}) _ _), _)<-typeTable ] -- error (showTypeTable typeTable) -- this is a good place to show the typeTable for debugging purposes.
    (stTypeGraph,condTypeGraph) = typeAnimate st
    cxerrs = concat (patcxes++rulecxes++keycxes++interfacecxes++proccxes++sPlugcxes++pPlugcxes++popcxes++deccxes++xplcxes)++themeschk
    --postchcks are those checks that require null cxerrs 
    postchks = rulenmchk ++ ifcnmchk ++ patnmchk ++ cyclicInterfaces
    acds = ctx_cs p_context++concatMap pt_cds (ctx_pats p_context)++concatMap procCds (ctx_PPrcs p_context)
    agens = map (pGen2aGen "NoPattern") (ctx_gs p_context)
    (adecs,   deccxes)   = (unzip . map (pDecl2aDecl allpops "NoPattern")  . ctx_ds) p_context
    (apurp,   xplcxes)   = (unzip . map  pPurp2aPurp                       . ctx_ps   ) p_context
    (pats,    patcxes)   = (unzip . map (pPat2aPat   allpops)              . ctx_pats ) p_context
    (procs,   proccxes)  = (unzip . map (pProc2aProc allpops)              . ctx_PPrcs) p_context
    (ctxrules,rulecxes)  = (unzip . map (pRul2aRul   "NoPattern")          . ctx_rs   ) p_context
    (keys,    keycxes)   = (unzip . map  pKDef2aKDef                       . ctx_ks   ) p_context
    (ifcs,interfacecxes) = (unzip . map  pIFC2aIFC                         . ctx_ifcs ) p_context
    (sqlPlugs,sPlugcxes) = (unzip . map (pODef2aODef [] Anything)          . ctx_sql  ) p_context
    (phpPlugs,pPlugcxes) = (unzip . map (pODef2aODef [] Anything)          . ctx_php  ) p_context
    (allpops, popcxes)   = (unzip . map  pPop2aPop                         . pops     ) p_context
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

    pPat2aPat :: [Population] -> P_Pattern -> (Pattern, [CtxError])
    pPat2aPat pops' ppat 
     = (A_Pat { ptnm  = name ppat    -- Name of this pattern
              , ptpos = pt_pos ppat  -- the position in the file in which this pattern was declared.
              , ptend = pt_end ppat  -- the position in the file in which this pattern was declared.
              , ptrls = prules       -- The user defined rules in this pattern
              , ptgns = agens'        -- The generalizations defined in this pattern
              , ptdcs = adecs'        -- The declarations declared in this pattern
              , ptkds = keys'         -- The key definitions defined in this pattern
              , ptxps = xpls         -- The purposes of elements defined in this pattern
              }
       , [CxeOrig typeErrs "pattern" (name ppat) (origin ppat) | (not.null) typeErrs]
       )
       where
        typeErrs = concat (rulecxes'++keycxes'++deccxes'++xplcxes')
        (prules,rulecxes') = unzip arls
        arls  = map (pRul2aRul (name ppat)) (pt_rls ppat)
        agens' = map (pGen2aGen (name ppat)) (pt_gns ppat)
        (keys',keycxes') = unzip akds
        akds  = map pKDef2aKDef (pt_kds ppat)
        (adecs',deccxes') = (unzip . map (pDecl2aDecl pops' (name ppat)) . pt_dcs) ppat
        (xpls,xplcxes') = (unzip . map pPurp2aPurp . pt_xps) ppat
    
    pProc2aProc :: [Population] -> P_Process -> (Process,[CtxError])
    pProc2aProc pops' pproc
     = (Proc { prcNm    = procNm pproc
             , prcPos   = procPos pproc
             , prcEnd   = procEnd pproc
             , prcRules = prules
             , prcGens  = agens'          -- The generalizations defined in this pattern
             , prcDcls  = adecs'          -- The declarations declared in this pattern
             , prcRRuls = arruls         -- The assignment of roles to rules.
             , prcRRels = arrels         -- The assignment of roles to Relations.
             , prcKds   = keys'           -- The key definitions defined in this process
             , prcXps   = expls          -- The purposes of elements defined in this process
             }
       , [CxeOrig typeErrs "process" (name pproc) (origin pproc) | (not.null) typeErrs]
       )
       where
        typeErrs = concat (rulecxes'++keycxes'++deccxes'++rrcxes++editcxes++explcxes)
        (prules,rulecxes') = (unzip . map (pRul2aRul (name pproc)) . procRules) pproc
        arrels = [(rol,rel) |rr<-rrels, rol<-rrRoles rr, rel<-rrRels rr]
        (rrels,editcxes)  = (unzip . map pRRel2aRRel            . procRRels) pproc
        agens'  = map (pGen2aGen (name pproc)) (procGens pproc)
        arruls = [(rol,rul) |rul<-rules contxt, rr<-rruls, name rul `elem` mRules rr, rol<-mRoles rr]
        (adecs',deccxes')   = (unzip . map (pDecl2aDecl pops' (name pproc)) . procDcls) pproc
        (rruls,rrcxes)    = (unzip . map  pRRul2aRRul                    . procRRuls) pproc
        (keys',keycxes')    = (unzip . map  pKDef2aKDef                    . procKds) pproc
        (expls,explcxes)  = (unzip . map  pPurp2aPurp                    . procXps) pproc
 
    pRRul2aRRul :: RoleRule -> (RoleRule,[CtxError])
    pRRul2aRRul prrul
     = ( prrul, [CxeOrig rrcxes "role rule" "" (origin prrul) | (not.null) rrcxes] )
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
    
    p2aPairView :: Sign -> P_PairView -> (PairView,[CtxError])
    p2aPairView _ (P_PairView ppvs) = (PairView pvs, concat errs) 
     where (pvs, errs) = unzip $ map p2aPairViewSegment ppvs
    
    p2aPairViewSegment :: P_PairViewSegment -> (PairViewSegment,[CtxError])
    p2aPairViewSegment  (P_PairViewText str)          = (PairViewText str, [])
    p2aPairViewSegment  (P_PairViewExp srcOrTgt pexp) = (PairViewExp srcOrTgt aexpr, exprcxe)
        where (aexpr,_,exprcxe) = pExpr2aExpr pexp
               
    pRul2aRul :: String -> P_Rule -> (Rule,[CtxError])
    pRul2aRul patname prul        -- for debugging the parser, this is a good place to put     error (show (rr_exp prul))
     = (Ru { rrnm  = rr_nm prul                 -- Name of this rule
           , rrexp = aexpr                      -- The rule expression
           , rrfps = rr_fps prul                -- Position in the Ampersand file
           , rrmean = meanings (rr_mean prul)   -- Ampersand generated meaning (for all known languages)
           , rrmsg = map (pMarkup2aMarkup (ctxlang contxt) (ctxmarkup contxt)) $ rr_msg prul
           , rrviol = mviol
           , rrtyp = sign aexpr                 -- Allocated type
           , rrdcl = Nothing                    -- The property, if this rule originates from a property on a Declaration
           , r_env = patname                    -- Name of pattern in which it was defined.
           , r_usr = True                       -- True if this rule was specified explicitly as a rule in the Ampersand script;
                                                -- False if it follows implicitly from the Ampersand script and generated by a computer
           , r_sgl = or [rr_nm prul `elem` map (name.snd) (prcRRuls p) | p<-ctxprocs contxt]  -- True if this is a signal; False if it is an ALWAYS rule
           , srrel = -- the signal relation
                     Sgn { decnm = rr_nm prul
                         , decsgn = sign aexpr
                         , decprps = []
                         , decprps_calc = []
                         , decprL = ""
                         , decprM = ""
                         , decprR = ""
                         , decMean = meanings (rr_mean prul)
                         , decConceptDef = Nothing
                         , decpopu = []
                         , decfpos = rr_fps prul
                         , deciss = True
                         , decusr = False
                         , decpat = ""
                         , decplug = True
                         }
           }
       , [CxeOrig typeErrors' "rule" "" (origin prul) | (not.null) typeErrors']
       )
       where typeErrors'        = exprcxe++mviolcxe
             (aexpr,_,exprcxe) = pExpr2aExpr (rr_exp prul)
             (mviol, mviolcxe) = case fmap (p2aPairView $ sign aexpr) $ rr_viol prul of
                                   Nothing              -> (Nothing, [])
                                   Just (viol, violcxe) -> (Just viol, violcxe)
             meanings = pMeanings2aMeaning (ctxlang contxt) (ctxmarkup contxt) 
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
    pKDef2aKDef :: P_KeyDef -> (KeyDef, [CtxError])
    pKDef2aKDef pkdef
     = (Kd { kdpos = kd_pos pkdef
           , kdlbl = kd_lbl pkdef
           , kdcpt = c
           , kdats = segs
                        }
       , [CxeOrig typeErrors' "key definition" "" (origin pkdef) | (not.null) typeErrors']
       )
       where
        typeErrors' = kdcxe++concat segscxes
        (segs, segscxes) = unzip . map (pKeySeg2aKeySeg (kd_cpt pkdef)) $ kd_ats pkdef
        c  = pCpt2aCpt (kd_cpt pkdef)
        -- check equality
        ats = [ expr | KeyExp expr <- segs ]
        kdcxe = newcxeif (null segscxes && length (nub (c:map (source.objctx) ats))/=1)
                         (intercalate "\n" ["The source of expression " ++ showADL (objctx x) 
                                            ++" ("++showADL (source (objctx x))++") is compatible, but not equal to the key concept ("++ showADL c ++ ")."
                                           |x<-ats,source (objctx x)/=c])
    
    -- (ats,atscxes)  = (unzip . map (pODef2aODef [] (thing c)) . kd_ats) pkdef
    pKeySeg2aKeySeg :: P_Concept -> P_KeySegment -> (KeySegment, [CtxError])
    pKeySeg2aKeySeg _      (P_KeyText str)   = (KeyText str, [])
    pKeySeg2aKeySeg _      (P_KeyHtml str)   = (KeyHtml str, [])
    pKeySeg2aKeySeg concpt (P_KeyExp keyExp) = let (objDef, cxe) = pODef2aODef [] (thing concpt) keyExp
                                               in ( KeyExp objDef, cxe)
    
    -- TODO -> Does pIFC2aIFC require more checks? What is the intention of params, viols, args i.e. the interface data type?
    pIFC2aIFC :: P_Interface -> (Interface,[CtxError])
    pIFC2aIFC pifc 
     = (Ifc { ifcName   = ifc_Name pifc
            , ifcParams = prms
            , ifcViols  = fatal 206 "not implemented ifcViols"
            , ifcArgs   = ifc_Args pifc
            , ifcRoles  = ifc_Roles pifc
            , ifcObj    = obj
            , ifcPos    = ifc_Pos pifc
            , ifcExpl   = ifc_Expl pifc
            }
       , [CxeOrig typeErrors' "interface" (name pifc) (origin pifc) | (not.null) typeErrors']
       )
       where
        typeErrors' = objcxe++concat prmcxes++duplicateRoleErrs++undeclaredRoleErrs
        parentIfcRoles = if null $ ifc_Roles pifc then roles contxt else ifc_Roles pifc -- if no roles are specified, the interface supports all roles
        (obj,objcxe) = pODef2aODef parentIfcRoles Anything (ifc_Obj pifc)
        (prms,prmcxes) = unzip [ pRel2aRel (psign sgn) r
                               | param<-ifc_Params pifc, let (r,sgns)=getSign param, sgn<-sgns
                               ]
                         where
                            getSign :: Term -> (Term,[P_Sign])
                            getSign (PTyp _ r@(Prel{}) (P_Sign []))  = (r,[])
                            getSign (PTyp _ r@(Prel{}) sgn) = (r,[sgn])
                            getSign r@(Prel _ rel)          = (r,[dec_sign d | d<-p_declarations p_context, name d==rel ])
                            getSign r                       = fatal 1070 ("Illegal call of getSign ("++show r++").")
        duplicateRoleErrs = [newcxe $ "Duplicate interface role \""++role++"\" at "++show (origin pifc) | role <- nub $ ifc_Roles pifc, length (filter (==role) $ ifc_Roles pifc) > 1 ]
        undeclaredRoleErrs = [newcxe $ "Undeclared interface role \""++role++"\" at "++show (origin pifc) | null duplicateRoleErrs, role <- nub $ ifc_Roles pifc, role `notElem` roles contxt ]
        -- we show the line nr for the interface, which may be slightly inaccurate, but roles have no position 
        -- and the implementation of error messages makes it difficult to give a nice one here
        
    -- | pODef2aODef checks compatibility of composition of expressions on equality
    pODef2aODef :: [String]              -- a list of roles that may use this object
                -> Type                  -- the universe for type checking this object. anything if the type checker decides freely, thing c if it must be of type c.
                -> P_ObjectDef           -- the object definition as specified in the parse tree
                -> (ObjectDef,[CtxError]) -- result: the type checked object definition (only defined if there are no type errors) and a list of type errors
    pODef2aODef parentIfcRoles _ podef 
     = (Obj { objnm   = obj_nm podef
            , objpos  = obj_pos podef
            , objctx  = expr
            , objmsub = msub
            , objstrs = obj_strs podef
            }
       , [CxeOrig typeErrors' "object definition" "" (origin podef) | (not.null) typeErrors']
       )
       where
        typeErrors' = nmchk++exprcxe++msubcxes
        -- A name check ensures that all attributes have unique names
        nmchk = [CxeEqAttribs (origin podef) (name (head cl)) (map obj_ctx cl)
                |cl<-eqCl name (getSubPObjs podef),length cl>1]
        getSubPObjs P_Obj { obj_msub = Just (P_Box objs) } = objs
        getSubPObjs _                                      = []
        -- Step1: check obj_ctx
        (expr,(_,tTrg),exprcxe)  = pExpr2aExpr (obj_ctx podef)
        -- Step2: check obj_ats in the context of expr
        (msub,msubcxes) = p2a_MaybeSubInterface parentIfcRoles conc $ obj_msub podef
         where
           conc = case tTrg of
                   TypExpr (Pid c) _ _ -> c
                   _                   -> fatal 1235 ("erroneous type found by pODef2aODef.")
        
    p2a_MaybeSubInterface :: [String] -> P_Concept -> Maybe P_SubInterface -> (Maybe SubInterface, [CtxError])
    p2a_MaybeSubInterface _              _    Nothing               = (Nothing, [])
    p2a_MaybeSubInterface parentIfcRoles conc (Just (P_Box p_objs)) =
      let (objs, errss) = unzip [pODef2aODef parentIfcRoles (thing conc) p_obj | p_obj<-p_objs] 
      in  (Just $ Box objs, concat errss)
    p2a_MaybeSubInterface parentIfcRoles conc (Just (P_InterfaceRef pos nm)) =
      (Just $ InterfaceRef nm, errs)
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
      
    pPurp2aPurp :: PPurpose -> (Purpose, [CtxError])
    pPurp2aPurp pexpl
     = ( Expl { explPos      = pexPos   pexpl
              , explObj      = explobs
              , explMarkup   = pMarkup2aMarkup (ctxlang contxt) (ctxmarkup contxt) (pexMarkup pexpl)
              , explRefId    = pexRefID pexpl
              , explUserdefd = True
             -- , explCont  = string2Blocks (ctxmarkup contxt) (pexExpl  pexpl)
              }
       , [CxeOrig xplcxe "explanation" "" (origin pexpl) | (not.null) xplcxe]
       )
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
    
    pPop2aPop :: P_Population -> (Population,[CtxError])
    pPop2aPop pop
     = ( Popu { popm  = aRel
              , popps = p_popps pop
              }
       , relcxe
       )
       where (ERel aRel, _, relcxe) = pExpr2aExpr expr
             expr = case pop of
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
    
    pDecl2aDecl :: [Population] -> String -> P_Declaration -> (Declaration, [CtxError])
    pDecl2aDecl pops' patname pd =
     ( Sgn { decnm   = dec_nm pd
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
           }
      , case dec_conceptDef pd of 
          Just (RelConceptDef srcOrTgt _) | relConceptName (dec_nm pd) `elem` map name (concs contxt) -> 
            [CxeOrig [newcxe ("Illegal DEFINE "++showSRCorTGT++" for relation "++show (dec_nm pd)++". Concept "++
                             relConceptName (dec_nm pd)++" already exists.")]
                    "declaration" "" (origin pd)]
             where showSRCorTGT = if srcOrTgt == Src then "SRC" else "TGT"
                   relConceptName ""     = fatal 472 "empty concept"
                   relConceptName (c:cs) = toUpper c : cs
          _ -> []
      )
      
    -- | p2a for isolated references to relations. Use pExpr2aExpr instead if relation is used in an expression.
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
    
    pExpr2aExpr :: Term    -- The term to be typed
                -> ( Expression    -- the resulting expression. It is defined only when the list of errors is empty.
                   , (Type,Type)   -- the type of the resulting expression. It is defined only when the list of errors is empty.
                   , [CtxError]    -- the list of type errors
                   )
    pExpr2aExpr pTerm                        -- The expression to be typed
     = ( fatalGet 1441 "call to fatalGet in pExpr2aExpr" $ f pTerm -- the resulting expression. It is defined only when the list of errors is empty.
       , (lookupType pTerm,lookupType (p_flp pTerm)) -- the type of the resulting expression. It is defined only when the list of errors is empty.
       , getErrors (f pTerm)                    -- the list of type errors
       )
       where
         f :: Term -> Guarded Expression
         -- alternatively:
         -- f x@(PTyp _ (Pid _) (P_Sign _))   = do{ c<-returnIConcepts x; return (ERel (I (pCpt2aCpt c)))}
         f x@(PI _)                     = do { c<-returnIConcepts x
                                             ; return (ERel (I (pCpt2aCpt c)))}
         f   (Pid c)                    = return (ERel (I (pCpt2aCpt c)))
         f   (Pnid c)                   = return (ECpl (ERel (I (pCpt2aCpt c))))
         f x@(Patm _ atom [])           = do { c<-returnIConcepts x
                                             ; return (ERel (Mp1 atom (pCpt2aCpt c)))}
         f   (Patm _ atom [c])          = return (ERel (Mp1 atom (pCpt2aCpt c)))
         f x@Pnull                      = fatal 988 ("pExpr2aExpr cannot transform "++show x++" to a term.")
         f x@(PVee o)                   = fatal 991 ("pExpr2aExpr cannot transform "++show x++" ("++show o++") to a term.")
         f (Pfull s t)                  = return (ERel (V (Sign (pCpt2aCpt s) (pCpt2aCpt t))))
         f x@(Prel o a)                 = do { (decl,sgn) <- errRelLike x
                                             ; return (ERel (Rel{relnm=a, relpos=o, relsgn=sgn, reldcl=decl}))
                                             }
         f x@(Pflp o a)                 = do { (decl,sgn) <- errRelLike x
                                             ; return (EFlp (ERel (Rel{relnm=a, relpos=o, relsgn=sgn, reldcl=decl})))
                                             }
         f x@(Pequ _ a b)               = do { (a',b') <- (,) <$> f a <*> f b
                                             ; _{-sign-} <- errEquLike x a b
                                             ; return (EEqu (a', b'))
                                             }
         f x@(Pimp _ a b)               = do { (a',b') <- (,) <$> f a <*> f b
                                             ; _{-sign-} <- errEquLike x a b
                                             ; return (EImp (a', b'))
                                             }
         f x@(PIsc _ a b)               = do { (a',b') <- (,) <$> f a <*> f b
                                             ; _{-sign-} <- errEquLike x a b
                                             ; return (EIsc (case a' of {EIsc xs -> xs; x'-> [x']} ++ case b' of {EIsc xs -> xs; x'-> [x']}))
                                             }
         f x@(PUni _ a b)               = do { (a',b') <- (,) <$> f a <*> f b
                                             ; _{-sign-} <- errEquLike x a b
                                             ; return (EUni (case a' of {EUni xs -> xs; x'-> [x']} ++ case b' of {EUni xs -> xs; x'-> [x']}))
                                             }
         f x@(PDif _ a b)               = do { (a',b') <- (,) <$> f a <*> f b
                                             ; _{-sign-} <- errEquLike x a b
                                             ; return (EDif (a', b'))
                                             }
         f x@(PLrs _ a b)               = do { (_{-concept-}, a', b') <- errCpsLike x a b
                                             ; return (ELrs (a', b'))
                                             }
         f x@(PRrs _ a b)               = do { (_{-concept-}, a', b') <- errCpsLike x a b
                                             ; return (ERrs (a', b'))
                                             }
         f x@(PCps _ a b)               = do { (_{-concept-}, a', b') <- errCpsLike x a b
                                             ; return (ECps (case a' of {ECps xs -> xs; x'-> [x']} ++ case b' of {ECps xs -> xs; x'-> [x']}))
                                             }
         f x@(PRad _ a b)               = do { (_{-concept-}, a', b') <- errCpsLike x a b
                                             ; return (ERad (case a' of {ERad xs -> xs; x'-> [x']} ++ case b' of {ERad xs -> xs; x'-> [x']}))
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
         f x@(PCpl _ a)      = do { fa <- f a; return (ECpl fa) <* errCpl x a }
         f (PBrk _ a)      = EBrk <$> f a
         f x@(PTyp o _ (P_Sign [])) = fatal 991 ("pExpr2aExpr cannot transform "++show x++" ("++show o++") to a term.")
         f (PTyp _ a sgn)  = ETyp <$> (f a) <*> return (Sign (pCpt2aCpt s) (pCpt2aCpt t))
                             where P_Sign cs = sgn; s=head cs; t=last cs
         f x = fatal 1542 ("Pattern match on f in pExpr2aExpr failed for "++show x)
         returnIConcepts x
          = if (length conflictingConcepts /= 1) then Errors 
            [CxeILike {cxeExpr   = x
                      ,cxeCpts   = conflictingConcepts
                      }] else return (head conflictingConcepts)
          where conflictingConcepts = obtainConceptsFromTerm x
         errCpsLike :: Term -> Term -> Term -> Guarded (A_Concept, Expression, Expression)
         errCpsLike x a b
          = case conflictingConcepts of
{-           []  -> error ("\ntypeTable: "++ showTypeTable typeTable++
                           "\ntable:    "++ intercalate "\n          " [ showType typ | (_,_,typ@(TypLub (TypExpr s _ _) (TypExpr t _ _) _),_)<-typeTable, p_flp s==a, t==b]++
                           "\nx:        "++ show x++
                           "\na:        "++ show a++
                           "\nb:        "++ show b++
                           show err)  -}
             [c] -> Checked (pCpt2aCpt c, a', b')
                     where (a',_,_) = pExpr2aExpr a;   (b',_,_) = pExpr2aExpr b
             _   -> Errors err
            where
              err = [ CxeCpsLike {cxeExpr   = x
                                 ,cxeCpts   = conflictingConcepts
                                 }
                    ]
              conflictingConcepts = [ c | (_,_,TypLub (TypExpr s _ _) (TypExpr t _ _) _,cs)<-typeTable
                                        , p_flp s==a, t==b, c<-cs]
         getConcepts x
          = [ c | (_,_,TypExpr x' _ _, cs)<-typeTable, x' == x, c<-cs]
         getSign :: ([P_Concept]->[P_Concept]->[CtxError]) -> Term -> Guarded Sign
         getSign e x
          = case (src,trg) of
             ([s],[t]) -> Checked (Sign (pCpt2aCpt s) (pCpt2aCpt t))
             (_  ,_  ) -> Errors (e src trg)
             where src = getConcepts x
                   trg = getConcepts (p_flp x)
         errEquLike :: Term -> Term -> Term -> Guarded Sign
         errEquLike x a b
          = getSign (\s t -> [ CxeEquLike {cxeExpr    = x
                                          ,cxeLhs     = a
                                          ,cxeRhs     = b
                                          ,cxeSrcCpts = s
                                          ,cxeTrgCpts = t}]) x
         errRelLike :: Term -> Guarded (Declaration, Sign)
         errRelLike term
          = case [ decl | decl<-declarations contxt, nm==name decl
                        , source decl `elem` srcTypes, target decl `elem` trgTypes]
            of [d] -> Checked (d, Sign (head srcTypes) (head trgTypes))
               _   -> let err = [ CxeRel {cxeExpr   = term'
                                         ,cxeCpts   = conflictingConcepts
                                         }
                                | (_,_,TypExpr term' _ _,conflictingConcepts)<-typeTable, term'==term || term'==p_flp term
                                , length conflictingConcepts/=1
                                ]
                      in if null err then fatal 1585 ("Impossible situation: non-unique binding, but still correctly typed.\nTerm: "++show term )-- ++"\n"++showTypeTable typeTable)
                         else Errors err
            where srcTypes = [ pCpt2aCpt c | (_,_,TypExpr term' _ _,conflictingConcepts)<-typeTable, term'==      term, c<-conflictingConcepts]
                  trgTypes = [ pCpt2aCpt c | (_,_,TypExpr term' _ _,conflictingConcepts)<-typeTable, term'==p_flp term, c<-conflictingConcepts]
                  nm = head ([ rel | Prel _ rel<-[term] ]++[ rel | Pflp _ rel<-[term] ]++
                             fatal 1590 "no name for nm in a call of errRelLike")
         errCpl x a
          = createVoid $  [ CxeCpl {cxeExpr   = a
                                   ,cxeCpts   = conflictingConcepts
                                   }
                          | (_,_,TypExpr term _ _,conflictingConcepts)<-typeTable
                          , length conflictingConcepts/=1
                          , origin x==origin term, term==x
                          ]
         obtainConceptsFromTerm x
          = if (length cs==0) then fatal 1514 ("Term "++show x++" does not occur in typeTable")
            else if not (foldr (\y x' -> x' && (head cs == y)) True cs) then fatal 1515 ("Term "++show x++" has multiple ambiguous occurences in typeTable")
            else head cs
          where
            cs = [ concepts | (_,_,TypExpr term _ _,concepts)<-typeTable
                            , origin x==origin term, term==x]
         lookupType pTerm' = head ([ thing c| (_,_,TypLub _ _ origExpr,[c])<-typeTable, pTerm'==origExpr ]++fatal 1535 ("cannot find "++showADL pTerm'++" in the lookup table"))

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


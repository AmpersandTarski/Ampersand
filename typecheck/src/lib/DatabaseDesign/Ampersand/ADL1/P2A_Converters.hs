{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
{-# LANGUAGE RelaxedPolyRec #-} -- -RelaxedPolyRec required for OpenSuse, for as long as we@OpenUniversityNL use an older GHC
module DatabaseDesign.Ampersand.ADL1.P2A_Converters (
     -- * Exported functions
     pCtx2aCtx,
     disambiguate
     )
where
import qualified Data.Graph as Graph
import qualified Data.Tree as Tree
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree hiding (sortWith)
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.ADL1.Pair
import DatabaseDesign.Ampersand.Basics
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
import Data.Array
import Control.Applicative
import qualified Data.Map

-- TODO: this module should import Database.Ampersand.Core.ParseTree directly, and it should be one 
--       of the very few modules that imports it. (might require some refactoring due to shared stuff)

fatal :: Int -> String -> a
fatal = fatalMsg "P2A_Converters"

{-The data structure Type is used to represent an expression inside the type checker.
TypExpr e flipped o origExpr is read as:
  "the source type of e, with e equal to (if flipped then PFlp origExpr else origExpr), and origExpr occurs in the P_Context p_context on location o."
origExpr (in conjunction with Origin o) is kept for the purpose of generating messages in terms of the original expression written by the user.
TypLub a b o e is read as:
  "the least upper bound of types a and b, in the context of expression e, which is located at o.
-}
data Type =  TypExpr Term Bool Origin Term
           | TypLub Type Type Origin Term
           | TypGlb Type Type Origin Term
            -- note: do not put "deriving Ord", because Eq is specified (and not derived)

instance Show Type where
    showsPrec _ typExpr = showString (showType typExpr)

-- Equality of type expressions is based on occurrence. Only I[c] and V[a,b] have equality irrespective of their occurrence.
{-
instance Eq Type where
 TypExpr (PI _)       _ o _ == TypExpr (PI _)        _ o' _  =  o==o'
 TypExpr (Pid     c ) _ o _ == TypExpr (Pid     c' ) _ o' _  =  c==c'
 TypExpr (Pfull _ []) _ o _ == TypExpr (Pfull _ [] ) _ o' _  =  o==o'
 TypExpr (Pfull _ cs) _ o _ == TypExpr (Pfull _ cs') _ o' _  =  cs==cs' && o==o'
 TypExpr p _ o _ == TypExpr p' _  o' _  =  p `p_eq` p'    && o==o'
 TypLub  a b o _ == TypLub  a' b' o' _  =  a==a' && b==b' && o==o'
 TypGlb  a b o _ == TypGlb  a' b' o' _  =  a==a' && b==b' && o==o'
 _ == _ = False
-}

showType :: Type -> String
showType (TypExpr expr@(Pid _) _ OriginUnknown _)       = showADL expr
showType (TypExpr expr@(Pfull OriginUnknown []) _ _ _)  = showADL expr
showType (TypExpr expr _ orig _)                        = showADL expr     ++"("++ shOrig orig++")"
showType (TypLub a b _ _)                               = showType a++" ./\\. "++showType b
showType (TypGlb a b _ _)                               = showType a++" .\\/. "++showType b

showOrig :: Type -> String
showOrig (TypExpr _ _       orig origExpr@(Pid _))      = showADL origExpr ++"("++ shOrig orig++")"
showOrig (TypExpr expr@(Pid _) _ orig _)                = showADL expr     ++"("++ shOrig orig++")"
showOrig (TypExpr expr@(Pid _) _ _ _)                   = showADL expr
showOrig (TypExpr _ _       orig origExpr@(Pid _))      = showADL origExpr
showOrig (TypExpr _ _       orig origExpr@(Pfull _ [])) = showADL origExpr ++"("++ shOrig orig++")"
showOrig (TypExpr expr@(Pfull _ []) _ orig _)           = showADL expr     ++"("++ shOrig orig++")"
showOrig (TypExpr expr@(Pfull _ cs) _ _ _)              = showADL expr
showOrig (TypExpr _ _       orig origExpr@(Pfull _ _))  = showADL origExpr
showOrig (TypExpr _ flipped orig origExpr)              = showADL (if flipped then p_flp origExpr else origExpr) ++"("++ shOrig orig++")"
showOrig (TypLub _ _ orig origExpr)                     = showADL origExpr ++"("++ shOrig orig++")"
showOrig (TypGlb _ _ orig origExpr)                     = showADL origExpr ++"("++ shOrig orig++")"

shOrig :: Origin -> String
shOrig (FileLoc (FilePos (fn,DatabaseDesign.Ampersand.ADL1.Pos l c,_))) = "line " ++ show l++":"++show c
shOrig (DBLoc str)   = "Database location: "++str
shOrig (Origin str)  = str
shOrig OriginUnknown = "Unknown origin"

instance Traced Type where
  origin (TypExpr _ _ o _) = o
  origin (TypLub _ _ o _) = o
  origin (TypGlb _ _ o _) = o

-- | Equality of Type is needed for the purpose of making graphs of types.
--   These are used in the type checking process.
--   The main idea is that the equality distinguishes between occurrences. So the expression 'r' on line 14:3 differs from 
--   the expression 'r' on line 87:19.
--   However, different occurrences of specific expressions that are fully typed (e.g. I[Person] or parent[Person*Person]), need not be distinguised.
instance Prelude.Ord Type where
  compare (TypExpr (Pid c)        _ _ _) (TypExpr (Pid c')         _ _ _) = Prelude.compare c c'
  compare (TypExpr (Pnid c)       _ _ _) (TypExpr (Pnid c')        _ _ _) = Prelude.compare c c'
  compare (TypExpr (Patm _ x [c]) _ _ _) (TypExpr (Patm _ x' [c']) _ _ _) = Prelude.compare (x,c) (x',c')
  compare (TypExpr (Pfull o [])   _ _ _) (TypExpr (Pfull o' [])    _ _ _) = Prelude.compare o o'
  compare (TypExpr (Pfull _ cs)   _ _ _) (TypExpr (Pfull _ cs')    _ _ _) = Prelude.compare cs cs'
  compare (TypExpr e@(PTyp _ _ (P_Sign [])) _ _ _) (TypExpr e'@(PTyp _ _ (P_Sign [])) _ _ _) = Prelude.compare e e'
  compare (TypExpr (PTyp _ (Prel _ a) sgn) _ _ _) (TypExpr (PTyp _ (Prel _ a') sgn') _ _ _) = Prelude.compare (sgn,a) (sgn',a')
  compare (TypExpr (PTyp _ (Pflp _ a) sgn) _ _ _) (TypExpr (PTyp _ (Pflp _ a') sgn') _ _ _) = Prelude.compare (sgn,a) (sgn',a')
  compare (TypExpr x _ _ _) (TypExpr y _ _ _) = Prelude.compare x y
  compare (TypLub l r _ _) (TypLub l' r' _ _) = compare (l,r) (l',r')
  compare (TypGlb l r _ _) (TypGlb l' r' _ _) = compare (l,r) (l',r')
  compare (TypExpr _ _ _ _) _  = Prelude.LT
  compare (TypLub _ _ _ _)  (TypExpr _ _ _ _) = Prelude.GT
  compare (TypLub _ _ _ _) _ = Prelude.LT
  compare (TypGlb _ _ _ _) _ = Prelude.GT

instance Eq Type where
  t == t' = compare t t' == EQ

-- | `p_eq` is an equivalence relation, which does not distinguish between occurrences.
--   It is intended as the mathematical equivalence of P_Expressions.
--   It treats two occurrences of the same expression as the same.
p_eq :: Term -> Term -> Bool
p_eq (PI _)         (PI _)           = True
p_eq (Pid c)        (Pid c')         = c==c'
p_eq (Pnid c)       (Pnid c')        = c==c'
p_eq (Patm _ x cs)  (Patm _ x' cs')  = x==x' && cs==cs'
p_eq Pnull          Pnull            = True
p_eq (Pfull _ cs)   (Pfull _ cs')    = cs==cs'
p_eq (Prel _ a)     (Prel _ a')      = a==a'
p_eq (Pflp _ a)     (Pflp _ a')      = a==a'
p_eq (Pequ _ a b)   (Pequ _ a' b')   = p_eq a a' && p_eq b b'
p_eq (Pimp _ a b)   (Pimp _ a' b')   = p_eq a a' && p_eq b b'
p_eq (PIsc _ a b)   (PIsc _ a' b')   = p_eq a a' && p_eq b b'
p_eq (PUni _ a b)   (PUni _ a' b')   = p_eq a a' && p_eq b b'
p_eq (PDif _ a b)   (PDif _ a' b')   = p_eq a a' && p_eq b b'
p_eq (PLrs _ a b)   (PLrs _ a' b')   = p_eq a a' && p_eq b b'
p_eq (PRrs _ a b)   (PRrs _ a' b')   = p_eq a a' && p_eq b b'
p_eq (PCps _ a b)   (PCps _ a' b')   = p_eq a a' && p_eq b b'
p_eq (PRad _ a b)   (PRad _ a' b')   = p_eq a a' && p_eq b b'
p_eq (PPrd _ a b)   (PPrd _ a' b')   = p_eq a a' && p_eq b b'
p_eq (PKl0 _ a)     (PKl0 _ a')      = p_eq a a'
p_eq (PKl1 _ a)     (PKl1 _ a')      = p_eq a a'
p_eq (PFlp _ a)     (PFlp _ a')      = p_eq a a'
p_eq (PCpl _ a)     (PCpl _ a')      = p_eq a a'
p_eq (PBrk _ a)     (PBrk _ a')      = p_eq a a'
p_eq (PTyp _ a sgn) (PTyp _ a' sgn') = p_eq a a' && sgn==sgn'
p_eq _ _ = False

-- | p_flp computes the inverse of a Term.
p_flp :: Term -> Term
p_flp (PFlp _ a) = a
p_flp a          = PFlp OriginUnknown a

{-
p_flp :: Term -> Term
p_flp a@(PI{})     = a
p_flp a@(Pid{})    = a
p_flp a@(Pnid{})   = a
p_flp a@(Patm{})   = a
p_flp Pnull        = Pnull
p_flp (Pfull o cs)   = Pfull o (reverse cs)
p_flp (Prel o a)     = Pflp o a
p_flp (Pflp o a)     = Prel o a
p_flp (Pequ o a b)   = Pequ o (p_flp a) (p_flp b)
p_flp (Pimp o a b)   = Pimp o (p_flp a) (p_flp b)
p_flp (PIsc o a b)   = PIsc o (p_flp a) (p_flp b)
p_flp (PUni o a b)   = PUni o (p_flp a) (p_flp b)
p_flp (PDif o a b)   = PDif o (p_flp a) (p_flp b)
p_flp (PLrs o a b)   = PRrs o (p_flp b) (p_flp a)
p_flp (PRrs o a b)   = PLrs o (p_flp b) (p_flp a)
p_flp (PCps o a b)   = PCps o (p_flp b) (p_flp a)
p_flp (PRad o a b)   = PRad o (p_flp b) (p_flp a)
p_flp (PPrd o a b)   = PPrd o (p_flp b) (p_flp a)
p_flp (PKl0 o a)     = PKl0 o (p_flp a)
p_flp (PKl1 o a)     = PKl1 o (p_flp a)
p_flp (PFlp _ a)     = p_flp (p_flp a) -- ensures that inner PFlp is removed too
p_flp (PCpl o a)     = PCpl o (p_flp a)
p_flp (PBrk o a)     = PBrk o (p_flp a)
p_flp (PTyp o a sgn) = PTyp o (p_flp a) (let P_Sign cs=sgn in P_Sign (reverse cs))
-}

t_complement :: Type -> Type
t_complement (TypExpr e flipped orig origExpr) = TypExpr (complement e) flipped orig (PCpl (origin origExpr) origExpr)
t_complement (TypLub a b orig origExpr)        = TypGlb (t_complement a) (t_complement b) orig (PCpl (origin origExpr) origExpr)
t_complement (TypGlb a b orig origExpr)        = TypLub (t_complement a) (t_complement b) orig (PCpl (origin origExpr) origExpr)

complement :: Term -> Term
complement (PCpl _ a)     = a
complement Pnull          = Pfull OriginUnknown []
complement (Pnid c)       = Pid c
complement a              = PCpl (origin a) a

anything :: Type
anything = TypExpr (Pfull OriginUnknown []) False OriginUnknown (Pfull OriginUnknown [])
thing :: P_Concept -> Type
thing c  = TypExpr (Pid c) False OriginUnknown (Pid c)

type Typemap = [(Type,Type)] --Data.Map.Map Type [Type]

setClosure xs = foldl f xs (Data.Map.keys xs `isc` nub (concat$Data.Map.elems xs))
  where
--   f q x = Data.Map.map (\bs->foldl merge bs [b' | b<-bs, b == x, (a', b') <- Data.Map.toList q, a' == x]) q
   f q x = Data.Map.map (\bs->foldl merge bs [b' | b<-bs, b == x, (Just b') <- [Data.Map.lookup x q]]) q
merge (a:as) (b:bs) | a<b  = a:merge as (b:bs)
                    | a==b = a:merge as bs
                    | a>b  = b:merge (a:as) bs
merge a b = a ++ b -- since either a or b is the empty list

findIn t cl = getList (Data.Map.lookup t cl)
                 where getList Nothing = []
                       getList (Just a) = a
-- | The purpose of 'typing' is to analyse the domains and codomains of an expression in a context.
--   As a result, it builds a list of tuples st::[(Type,Type)], which represents a relation, st,  over Type*Type.
--   For any two P_Expressions a and b,  if dom(a) is a subset of dom(b), this is represented as a tuple (TypExpr a _ _ _,TypExpr b _ _ _) in st.
--   In the code below, this shows up as  dom a.<.dom b
--   The function typing does a recursive scan through all subexpressions, collecting all tuples on its way.
--   Besides expression expr, this function requires a universe in which to operate.
--   Specify 'anything anything' if there are no restrictions.
--   If the source and target of expr is restricted to concepts c and d, specify (thing c) (thing d).
typing :: P_Context -> Data.Map.Map Type [Type] -- subtypes (.. is subset of ..)
typing p_context
 = Data.Map.unionWith merge secondSetOfEdges firstSetOfEdges
   where
     (firstSetOfEdges,secondSetOfEdges)
      = makeDataMaps
        (foldr (.+.) nothing ([uType expr anything anything expr | expr <- expressions p_context] ++
                              [dom spc.<.dom gen | g<-p_gens p_context
                                                 , let spc=Pid (gen_spc g)
                                                 , let gen=Pid (gen_gen g)
                                                 , let x=Pimp (origin g) spc gen ]))
     makeDataMaps :: (Typemap,Typemap) -> (Data.Map.Map Type [Type],Data.Map.Map Type [Type])
     makeDataMaps (a,b) = (makeDataMap a, makeDataMap b)
     makeDataMap lst = Data.Map.fromDistinctAscList (foldr compress [] (sort lst))
       where
         compress (a,b) o@(r:rs) = if a==(fst r) then (a,addTo b (snd r)):rs else (a,[b]):o
         compress (a,b) [] = [(a,[b])]
         addTo b o@(c:_) | c==b = o
         addTo b o = b:o
     stClos   = setClosure firstSetOfEdges
     decls    = p_declarations p_context
     pDecls   = [PTyp (origin d) (Prel (origin d) (dec_nm d)) (dec_sign d) | d<-decls]
     isTypeSubset t c = c `elem` findIn t stClos
     u = OriginUnknown
     uType :: Term    -- x:    the original expression from the script, meant for representation in the graph.
           -> Type            -- uLft: the type of the universe for the domain of x 
           -> Type            -- uRt:  the type of the universe for the codomain of x
           -> Term    -- z:    the expression to be analyzed, which must be logically equivalent to x
           -> ( Typemap  -- for each type, a list of types that are subsets of it, which is the result of analysing expression x.
              , Typemap ) -- for some edges, we need to know the rest of the graph. These can be created in this second part.
     uType _ _    _     (Pnid _)              = fatal 136 "Pnid has no representation"
     uType x _    _     (PI{})                = dom x.=.cod x                                                        -- I
     uType x _    _     (Pid{})               = dom x.=.cod x                                                        -- I[C]
     uType x _    _     (Patm _ _ [])         = dom x.=.cod x                                                        -- 'Piet'   (an untyped singleton)
     uType x _    _     (Patm _ _ cs)         = dom x.<.dom (Pid (head cs)) .+. cod x.<.cod (Pid (last cs))          -- 'Piet'[Persoon]  (a typed singleton)
     uType _ _    _      Pnull                = nothing                                                              -- -V     (the empty set)
     uType x uLft uRt   (Pfull _ [])          = dom x.<.uLft .+. cod x.<.uRt
     uType x _    _     (Pfull _ cs)          = dom x.<.dom (Pid (head cs)) .+. cod x.<.cod (Pid (last cs))          --  V[A*B] (the typed full set)
     uType x uLft uRt   (Prel _ nm)           = -- disambiguate nm
                                                carefully ( -- what is to come will use the first iteration of edges, so to avoid loops, we carefully only create second edges instead
                                                                  if length spcls == 1 then dom x.=.dom (head spcls) .+. cod x.=.cod (head spcls)
                                                                  else nothing
                                                          )
                                                where decls = [decl | decl@(PTyp _ (Prel _ dnm) _)<-pDecls, dnm==nm ]
                                                      spcls = if length decls==1 then decls else
                                                              [d    | d@(PTyp _ (Prel _ dnm) (P_Sign cs@(_:_)))<-decls, compatible (head cs) (last cs)]
                                                      compatible l r =    isTypeSubset uLft (thing l)
                                                                       && isTypeSubset uRt  (thing r)
     uType x uLft uRt   (Pequ _ a b)          = dom a.=.dom b .+. cod a.=.cod b .+. dom b.=.dom x .+. cod b.=.cod x  --  a=b    equality
                                                 .+. uType a uLft uRt a .+. uType b uLft uRt b 
     uType x uLft uRt   (PIsc _ a b)          = dom x.=.interDom .+. cod x.=.interCod    --  intersect ( /\ )
                                                .+. dm .+. cm -- .+. dm2 .+. cm2 -- probably needed, but if there is no try*.adl that reports a bug caused by this commenting, please keep it commented
                                                .+. uType a interDom2 interCod2 a .+. uType b interDom2 interCod2 b
                                                where (dm,interDom) = mSpecific (dom a) (dom b)  x
                                                      (cm,interCod) = mSpecific (cod a) (cod b)  x
                                                      (dm2,interDom2) = mSpecific interDom uLft  x
                                                      (cm2,interCod2) = mSpecific interCod uRt   x
     uType x uLft uRt   (PUni _ a b)          = dom x.=.interDom .+. cod x.=.interCod    --  union     ( \/ )
                                                .+. dm .+. cm -- .+. dm2 .+. cm2 -- probably needed, but if there is no try*.adl that reports a bug caused by this commenting, please keep it commented
                                                .+. uType a interDom2 interCod2 a .+. uType b interDom2 interCod2 b
                                                where (dm,interDom) = mGeneric (dom a) (dom b)  x
                                                      (cm,interCod) = mGeneric (cod a) (cod b)  x
                                                      (dm2,interDom2) = mSpecific interDom uLft  x
                                                      (cm2,interCod2) = mSpecific interCod uRt   x
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
     uType _  _    _    (PTyp _ _ (P_Sign (a:b:c:_))) = fatal 197 "P_Sign too large"
     uType x uLft uRt   (PTyp o e (P_Sign cs))= dom x.<.iSrc  .+. cod x.<.iTrg  .+.                                  -- e[A*B]  type-annotation
                                                if o `elem` [origin d| d<-decls]
                                                then nothing
                                                else dom x.<.dom e .+. cod x.<.cod e
                                                     .+. uType e iSrc iTrg e
                                                where iSrc = thing (head cs)
                                                      iTrg = thing (last cs)
     uType x uLft uRt   (PPrd _ a b)          = dom x.<.dom a .+. cod x.<.cod b                                        -- a*b cartesian product
                                                .+. uType a uLft anything a .+. uType b anything uRt b
     -- derived uTypes: the following do no calculations themselves, but merely rewrite expressions to the ones we covered
     uType x uLft uRt   (Pflp o nm)           = dom x.=.cod e .+. cod x.=.dom e .+. uType e uRt uLft e
                                                where e = Prel o nm
     uType x uLft uRt   (Pimp _ a b)          = dom x.=.dom e .+. cod x.=.cod e .+. 
                                                uType x uLft uRt e                 --  a|-b   implication (aka: subset)
                                                where e = Pequ OriginUnknown a (PIsc OriginUnknown a b)
     uType x uLft uRt   (PLrs _ a b)          = dom x.=.dom e .+. cod x.=.cod e .+.  
                                                uType x uLft uRt e                 -- a/b = a!-b~ = -(-a;b~)
                                                where e = PCpl OriginUnknown (PCps OriginUnknown (complement a) (p_flp b))
     uType x uLft uRt   (PRrs _ a b)          = dom x.=.dom e .+. cod x.=.cod e .+.  
                                                uType x uLft uRt e                 -- a\b = -a~!b = -(a~;-b)
                                                where e = PCpl OriginUnknown (PCps OriginUnknown (p_flp a) (complement b))
     uType x uLft uRt   (PRad _ a b)          = dom x.=.dom e .+. cod x.=.cod e .+.  
                                                uType x uLft uRt e                 -- a!b = -(-a;-b) relative addition
                                                where e = PCps OriginUnknown (complement a) (complement b)
     uType x uLft uRt   (PCpl _ a)            = dom x.=.dom e .+. cod x.=.cod e .+.  
                                                uType x uLft uRt e                 -- -a = V - a
                                                where e = PDif OriginUnknown (Pfull (origin x) []) a
     nothing :: (Typemap,Typemap)
     nothing = ([],[])
     isFull (TypExpr (Pfull _ []) _ _ _) = True
     isFull (TypLub a b _ _) = isFull a && isFull b
     isFull (TypGlb a b _ _) = isFull a && isFull b
     isFull _ = False
     isNull (TypExpr Pnull _ _ _) = True
     isNull (TypLub a b _ _) = isNull a && isNull b
     isNull (TypGlb a b _ _) = isNull a && isNull b
     isNull _ = False
     infixl 2 .+.   -- concatenate two lists of types
     infixl 3 .<.   -- makes a list of one tuple (t,t'), meaning that t is a subset of t'
     infixl 3 .=.   -- makes a list of two tuples (t,t') and (t',t), meaning that t is equal to t'
     (.<.) :: Type -> Type -> (Typemap , Typemap)
     _ .<. b | isFull b = nothing
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
     dom x    = TypExpr x         False (origin x) x -- the domain of x, and make sure to check subexpressions of x as well
     cod o@(PI{})  = dom o
     cod o@(Pid{}) = dom o
     cod o@(Patm{}) = dom o
     cod o@(Pnull{}) = dom o
     cod (Pfull o cs) = dom (Pfull o (reverse cs))
     cod x    = TypExpr (p_flp x) True  (origin x) x 
     mSpecific, mGeneric :: Type -> Type -> Term -> ( (Typemap , Typemap) ,Type)
     mSpecific a b e = (r .<. a .+. r .<. b , r) where r = TypLub a b OriginUnknown e
     mGeneric  a b e = (a .<. r .+. b .<. r , r) where r = TypGlb a b OriginUnknown e


flattenMap = Data.Map.foldWithKey (\s ts o -> o ++ [(s,t)|t<-ts]) []

{- The following table is a data structure that is meant to facilitate drawing type graphs and creating the correct messages for users.
This table is organized as follows:
Int          : a vertex (number) in the stGraph, which contains the raw tuples from function 'typing'
Int          : a vertex (number) in the condensedGraph, which is a condensed form of the stGraph, leaving the semantics identical
Type         : a type expression, containing a Term, which is represented by a number in the type graphs. Different expressions may carry the same number in the condensedGraph.
[P_Concept]  : a list of concepts. If (_,_,expr,cs) is an element of this table, then for every c in cs there is a proof that dom expr is a subset of I[c].
               For a type correct expr, list cs contains precisely one element.
-}
tableOfTypes :: Data.Map.Map Type [Type] -> ([(Int,Int,Type,[P_Concept])], [Int],[(Int,Int)],[Int],[(Int,Int)],[(Int,Int)])
tableOfTypes st = (table, [0..length typeExpressions-1],stEdges,map fst classTable,(map (\(x,y)->(classNr x,classNr y)) condensedEdges),(map (\(x,y)->(classNr x,classNr y)) condensedEdges2)) 
 where
{-  stGraph is a graph whose edges are precisely st, but each element in
	st is replaced by a pair of integers. The reason is that datatype
	Graph expects integers. The list st contains the essence of the
	type analysis. It contains tuples (t,t'), each of which means
	that the set of atoms contained by dom t is a subset of the set
	of atoms contained by dom t'.
-}
     typeExpressions :: [Type]     -- a list of all type expressions in st.
     typeExpressions = Data.Map.keys st
     expressionTable :: [(Int, Type)]
     expressionTable = [(i,typeExpr) | (i,typeExpr)<-zip [0..] typeExpressions ]
     expressionNr :: Type -> Int
     expressionNr t  = head ([i | (i,v)<-expressionTable, t == v]++[fatal 178 ("Type Expression "++show t++" not found by expressionNr")])
     stClos1 = setClosure st
     someWhatSortedLubs = decompose (Data.Map.keys st)
      where
       decompose (o@(TypLub a b _ _):rs) = decompose (lookups a st) ++ decompose (lookups b st) ++ o:[r|r<-decompose rs, r `notElem` lookups a st, r `notElem` lookups b st]
       decompose (o:rs) = decompose rs
       decompose [] = []
     lookups o q = head ([merge [o] e | (Just e)<-[Data.Map.lookup o q]]++[[o]])
     stClosAdded = foldl f stClos1 someWhatSortedLubs
       where
        f :: Data.Map.Map Type [Type] -> Type -> Data.Map.Map Type [Type] 
        f q o@(TypLub a b _ _) = Data.Map.map (\cs -> foldr merge cs [lookups o q | a `elem` cs, b `elem` cs]) q
        sortisc :: (Ord a, Eq a) => [a]->[a]->[a]
        sortisc (a:as) (b:bs) | a == b = a:sortisc as bs
                              | a < b = sortisc as (b:bs)
                              | a > b = sortisc (a:as) bs
        sortisc _ _ = []
     stClos = setClosure stClosAdded
     stEdges :: [(Int,Int)]
     stEdges = [(expressionNr s,expressionNr t) | (s,t) <- flattenMap st]
{- condensedGraph is the condensed graph. Elements that are equal are brought together in the same equivalence class.
   The graph in which equivalence classes are vertices is called the condensed graph.
   These equivalence classes are the strongly connected components of the original graph
-}
     eqClasses :: [[Type]]             -- The strongly connected components of stGraph
     eqClasses = (efficientNub $ sort (Data.Map.elems (addIdentity (reflexiveMap stClos))))
      where addIdentity = Data.Map.mapWithKey (\k a->if null a then [k] else a)
     efficientNub :: [[Type]] -> [[Type]] -- should be sorted!
     efficientNub ((a:_):bs@(b:_):rs) | a==b = efficientNub (bs:rs) -- efficient nub for sorted classes: only compares the first element
     efficientNub (a:rs) = a:efficientNub rs
     efficientNub [] = []
     exprClass :: Type -> [Type]
     exprClass i = head ([c | c<-eqClasses, i `elem` c]++[fatal 191 ("Type Expression "++show i++" not found by exprClass")])
     condensedEdges :: [([Type],[Type])]
     condensedEdges = nub $ sort [(c,c') | (i,i')<-flattenMap st, let c=exprClass i, let c'=exprClass i', head c/=head c']
     condensedEdges2 = nub $ sort [(c,c') | (i,i')<-flattenMap stClosAdded, i' `notElem` findIn i stClos1, let c=exprClass i, let c'=exprClass i', head c/=head c']
     condensedClos  = nub $ sort [(c,c') | (i,i')<-flattenMap stClos, let c=exprClass i, let c'=exprClass i', head c/=head c']
     -- condensedVerts is eqClasses
     -- condensedVerts = nub [c | (i,i')<-condensedEdges, c<-[i,i']]
     classTable :: [(Int,[Type])]
     classTable = zip [0..] eqClasses
     classNr :: [Type] -> Int
     classNr (t:ts)  = head ([i | (i,(v:vs))<-classTable, t == v]++[fatal 178 ("Type Class "++show t++" not found by classNr")])
     reverseMap :: (Prelude.Ord a) => Data.Map.Map a [a] -> Data.Map.Map a [a]
     reverseMap lst = (Data.Map.fromListWith merge (concat [(a,[]):[(b,[a])|b<-bs] | (a,bs)<-Data.Map.toList lst]))
     reflexiveMap :: (Prelude.Ord a) => Data.Map.Map a [a] -> Data.Map.Map a [a]
     reflexiveMap lst = Data.Map.map sort (Data.Map.intersectionWith isc lst (reverseMap lst))
     -- note: reverseMap is relatively slow, but only needs to be calculated once
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
       where isAType = nub [c | TypExpr (Pid c) _ _ _<-cls]
     -- The possible types are all concepts of which term i is a subset.
     possibleTypes :: [([Type],[Type],P_Concept)]
     possibleTypes = [ (i,j,c) | (i,j) <- condensedClos, TypExpr (Pid c) _ _ _<- j, i/=j ]
     typeSubsets   = [ (i,j) | (i,j,_) <- possibleTypes, TypExpr (Pid c) _ _ _<- i ]
     secondaryTypes= [ (i,j') | (i,j,_) <- possibleTypes, (i',j')<-typeSubsets, head i'==head j]
     -- reducedtypes contains all types for which there is not a more specific type
     reducedTypes  = [ (i,j,c) | (i,j,c) <- possibleTypes, null [j | (i',j')<-secondaryTypes,head i==head i',head j==head j']]
instance Show CtxError where
    showsPrec _ err = showString (showErr err)

showErr :: CtxError -> String
-- * CxeEqConcepts tells us that there are concepts with different names, which can be proven equal by the type checker.
showErr err@(CxeEqConcepts{})
 = concat
     ["The following concepts are equal:\n"++commaEng "and" (map show (cxeConcepts err))++"." | not (null (cxeConcepts err)) ]
-- * CxeEqAttribs tells us that there are concepts with different names, which can be proven equal by the type checker.
showErr err@(CxeEqAttribs{})
 = show (cxeOrig err)++": Different attributes share the same name \""++cxeName err++"\":\n   "++
   intercalate "\n   " [ show (origin expr)++" : "++showADL expr | expr<-cxeAtts err ]
showErr err@(CxeAmbExpr{})
 = concat
     ( [show (origin (cxeExpr err))++": Ambiguous expression\n   "++showADL (cxeExpr err)++"\n   "]++
       ["can be typed by either "++commaEng "or" (map show (cxeSign err))++"." | not (null (cxeSign err)) ]++
       ["has either "++commaEng "or" (map showADL (cxeSrcT err))++" as its source concept." | not (null (cxeSrcT err)) ]++
       ["has either "++commaEng "or" (map showADL (cxeTrgT err))++" as its target concept." | not (null (cxeTrgT err)) ]
     )
showErr err@(CxeILike { cxeCpts=[]})
 = concat
     ( [show (origin (cxeExpr err))++"\n"]++
       ["    Expression  "++showADL (cxeExpr err)++"  can be of any type."]
     )
showErr err@(CxeILike {})
 = concat
     ( [show (origin (cxeExpr err))++"\n"]++
       ["    Expression  "++showADL (cxeExpr err)++",\n"]++
       ["    cannot be "++commaEng "and" (map showADL (cxeCpts err)) | (not.null) (cxeCpts err)]++[" at the same time."]
     )
showErr err@(CxeCpl{})
 = concat
     ( [show (origin (cxeExpr err))++"\n"]++
       ["    There is no universe from which to compute a complement in expression  "++showADL (cxeExpr err)++"." | null (cxeCpts err)]++
       ["    There are multiple universes from which to compute a complement in expression  "++showADL (cxeExpr err)++":\n    "++commaEng "and" (map showADL (cxeCpts err)) | (not.null) (cxeCpts err)]
     )
showErr err@(CxeEquLike { cxeExpr=Pequ _ _ _ }) = showErrEquation err
showErr err@(CxeEquLike { cxeExpr=Pimp _ _ _ }) = showErrEquation err
showErr err@(CxeEquLike { cxeExpr=PIsc _ _ _ }) = showErrBoolTerm err
showErr err@(CxeEquLike { cxeExpr=PUni _ _ _ }) = showErrBoolTerm err
showErr err@(CxeEquLike { cxeExpr=PDif _ _ _ }) = showErrBoolTerm err
showErr err@(CxeCpsLike { cxeExpr=PLrs _ a b})
 = concat
     ( [show (origin (cxeExpr err))++"\n"]++
       ["    Inside expression  "++showADL (cxeExpr err)++",\n"]++
       ["    between the target of  "++showADL a++"  and the target of  "++showADL b++",\n"]++
       ["    concepts "++commaEng "and" (map showADL (cxeCpts err)) | (not.null) (cxeCpts err)]++[" are in conflict."]
     )
showErr err@(CxeCpsLike { cxeExpr=PRrs _ a b})
 = concat
     ( [show (origin (cxeExpr err))++"\n"]++
       ["    Inside expression   "++showADL (cxeExpr err)++",\n"]++
       ["    between the source of  "++showADL a++"  and the source of  "++showADL b++",\n"]++
       ["    concepts "++commaEng "and" (map showADL (cxeCpts err)) | (not.null) (cxeCpts err)]++[" are in conflict."]
     )
showErr err@(CxeCpsLike { cxeExpr=PCps _ a b})
 = concat
     ( [show (origin (cxeExpr err))++"\n"]++
       ["    Inside expression   "++showADL (cxeExpr err)++",\n"]++
       ["    concepts "++commaEng "and" (map showADL (cxeCpts err))++
        "\n    between the target of  "++showADL a++"  and the source of  "++showADL b++" are in conflict." | (not.null) (cxeCpts err)]++
       ["    the type between the target of  "++showADL a++"  and the source of  "++showADL b++" is undefined." | null (cxeCpts err)]
     )
showErr err@(CxeCpsLike { cxeExpr=PRad _ a b})
 = concat
     ( [show (origin (cxeExpr err))++"\n"]++
       ["    Inside expression   "++showADL (cxeExpr err)++",\n"]++
       ["    concepts "++commaEng "and" (map showADL (cxeCpts err))++
        "\n    between the target of  "++showADL a++"  and the source of  "++showADL b++" are in conflict." | (not.null) (cxeCpts err)]++
       ["    the type between the target of  "++showADL a++"  and the source of  "++showADL b++" is undefined." | null (cxeCpts err)]
     )
showErr (CxeOrig typeErrors t nm o)
 | null typeErrors                              = ""
 | t `elem` ["pattern", "process", "interface"] = "The " ++ t ++ " named \""++ nm ++ "\" contains errors " ++ intercalate "\n" (map showErr typeErrors)
 | otherwise                                    = "in the " ++ t ++ " at "++ shOrig o ++ ":\n" ++ intercalate "\n" (map showErr typeErrors)
showErr (Cxe typeErrors x)                      = x ++ "\n" ++ intercalate "\n" (map showErr typeErrors)
showErr (PE msg)                                = "Parse error:\n"++ show (case msg of 
                                                                             []  -> fatal 35 "No messages??? The impossible happened!" 
                                                                             x:_ -> x)
showErr _ = fatal 580 "missing pattern in type error."
showErrEquation err@(CxeEquLike { cxeExpr=x, cxeLhs=a, cxeRhs=b})
 = concat
     ( [show (origin x)++"\n"]++
       case (cxeSrcCpts err, cxeTrgCpts err) of
            ([], [])  -> ["    Entirely ambiguous equation  "++showADL (cxeExpr err)]
            (cs, [])  -> ["    The source of  "++showADL a++"  and the source of  "++showADL b++"\n"]++
                         ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs)++"."]
            ([], cs') -> ["    The target of  "++showADL a++"  and the target of  "++showADL b++"\n"]++
                         ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs')++"."]
            (cs, cs') -> ["    The source of  "++showADL a++"  and the source of  "++showADL b++"\n"]++
                         ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs)++"\n"]++
                         ["    and the target of  "++showADL a++"  and the target of  "++showADL b++"\n"]++
                         ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs')++"."]
     )
showErrBoolTerm err@(CxeEquLike { cxeExpr=x, cxeLhs=a, cxeRhs=b})
 = concat
     ( [show (origin x)++"\n"]++
       case (cxeSrcCpts err, cxeTrgCpts err) of
            ([], [])  -> ["    Entirely ambiguous term  "++showADL (cxeExpr err)]
            (cs, [])  -> ["    Inside term   "++showADL x++",\n"]++
                         ["    the source of  "++showADL a++"  and the source of  "++showADL b++"\n"]++
                         ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs)++"."]
            ([], cs') -> ["    Inside term   "++showADL x++",\n"]++
                         ["    the target of  "++showADL a++"  and the target of  "++showADL b++"\n"]++
                         ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs')++"."]
            (cs, cs') -> ["    Inside term   "++showADL x++",\n"]++
                         if sort cs==sort cs'
                         then ["    the sources and targets of  "++showADL a++"  and  "++showADL b++"\n"]++
                              ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs)++"."]
                         else ["    the source of  "++showADL a++"  and the source of  "++showADL b++"\n"]++
                              ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs)++"\n"]++
                              ["    and the target of  "++showADL a++"  and the target of  "++showADL b++"\n"]++
                              ["    are in conflict with respect to concepts "++commaEng "and" (map showADL cs')++"."]
     )

showTypeTable :: [(Int,Int,Type,[P_Concept])] -> String
showTypeTable typeTable
 = "Type table has "++show (length typeTable)++" rows.\n  " ++ intercalate "\n  " (map showLine typeTable)
   where  -- hier volgt een (wellicht wat onhandige, maar goed...) manier om de type table leesbaar neer te zetten.
    nMax = maximum [i | (stIndex,cIndex,_,_)<-typeTable, i<-[stIndex, cIndex]]
    sh i = [ ' ' | j<-[length (show i)..length (show nMax)] ]++show i
    shPos t = str++[ ' ' | j<-[length str..maxPos] ]
     where str = showPos t
           maxPos = maximum [length (showPos (origin typExpr)) | (_,_,typExpr,_)<-typeTable]
    shType t = str++[ ' ' | j<-[length str..maxType] ]
     where str = show t
           maxType = maximum [length (show typExpr) | (_,_,typExpr,_)<-typeTable]
    shExp t = str++[ ' ' | j<-[length str..maxExpr] ]
     where str = showADL (showTypeExpr t)
           maxExpr = maximum [length (showADL (showTypeExpr typExpr)) | (_,_,typExpr,_)<-typeTable]
    showLine (stIndex,cIndex,t,concepts) = sh stIndex++","++sh cIndex++", "++shPos (origin t)++"  "++shExp t++"  "++shType t++"  "++show concepts
    showTypeExpr (TypLub _ _ _ expr)  = expr
    showTypeExpr (TypGlb _ _ _ expr)  = expr
    showTypeExpr (TypExpr expr _ _ _) = expr
    showPos OriginUnknown = "Unknown"
    showPos (FileLoc (FilePos (_,Pos l c,_)))
       = "("++show l++":"++show c++")"
    showPos _ = fatal 517 "Unexpected pattern in showPos"

showStVertex :: [(Int,Int,Type,[P_Concept])] -> Int -> String
showStVertex typeTable i
 = head ([ showType e | (exprNr, _, e,_)<-typeTable, i==exprNr ]++fatal 506 ("No expression numbered "++show i++" found by showStVertex"))
showVertex :: [(Int,Int,Type,[P_Concept])] -> Int -> String
showVertex typeTable i
 = (intercalate "\n".nub) [ showType (head cl)
                          | cl<-eqCl original [ typExpr | (_, classNr, typExpr,_)<-typeTable, i==classNr ]
                          ]
vertex2Concept :: [(Int,Int,Type,[P_Concept])] -> Int -> P_Concept
vertex2Concept typeTable i
 = head ([ c | (_, classNr, TypExpr (Pid c) _ _ _, _)<-typeTable , i==classNr]
         ++fatal 603 ("No concept numbered "++show i++" found by vertex2Concept typeTable.\n  "++intercalate "\n  " [ show tti | tti@(_, classNr, _, _)<-typeTable , i==classNr])
        )
unFlip :: Type -> Term
unFlip (TypExpr _ flipped _ e) = if flipped then p_flp e else e
unFlip x = fatal 607 ("May not call 'original' with "++showType x)
original :: Type -> Term
original (TypExpr _ _ _ e) = e
original (TypLub  _ _ _ e) = e
original (TypGlb  _ _ _ e) = e
lookupType :: [(Int,Int,Type,[P_Concept])] -> Int -> Type
lookupType typeTable i
 = head ([ typExpr | (_, classNr, typExpr, _)<-typeTable, i==classNr ]++fatal 562 ("No expression numbered "++show i++" found by tExpr"))

{- The following function draws two graphs for educational or debugging purposes. If you want to see them, run Ampersand --typing.
-}
typeAnimate :: Data.Map.Map Type [Type] -> (DotGraph String,DotGraph String)
typeAnimate st = (stTypeGraph, condTypeGraph)
   where
{- The set st contains the essence of the type analysis. It contains tuples (t,t'),
   each of which means that the set of atoms contained by t is a subset of the set of atoms contained by t'. -}
    (typeTable,stVtx,stEdg,cdVtx,cdEdg,cdEdg2) = tableOfTypes st
    stTypeGraph :: DotGraph String
    stTypeGraph = toDotGraph (showStVertex typeTable) show stVtx [] stEdg []
    condTypeGraph :: DotGraph String
    condTypeGraph = toDotGraph showVtx show cdVtx [] cdEdg cdEdg2
     where showVtx n = (intercalate "\n".nub)
                       [ showType (head cl)
                       | cl<-eqCl original [ typExpr| (_, classNr, typExpr,_)<-typeTable, n==classNr ]
                       ]
class Expr a where
  p_gens :: a -> [P_Gen]
  p_gens _ = []
  p_concs :: a -> [P_Concept]
  p_declarations :: a -> [P_Declaration]
  p_declarations _ = []
  expressions :: a -> [Term]
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
 expressions pContext
  = nub (expressions (ctx_pats  pContext) ++
         expressions (ctx_PPrcs pContext) ++
         expressions (ctx_rs    pContext) ++
         expressions (ctx_ds    pContext) ++
         expressions (ctx_ks    pContext) ++
         expressions (ctx_ifcs  pContext) ++
         expressions (ctx_sql   pContext) ++
         expressions (ctx_php   pContext)
        )
 subexpressions pContext
  = subexpressions (ctx_pats  pContext) ++
    subexpressions (ctx_PPrcs pContext) ++
    subexpressions (ctx_rs    pContext) ++
    subexpressions (ctx_ds    pContext) ++
    subexpressions (ctx_ks    pContext) ++
    subexpressions (ctx_ifcs  pContext) ++
    subexpressions (ctx_sql   pContext) ++
    subexpressions (ctx_php   pContext)

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
 expressions pPattern
  = nub (expressions (pt_rls pPattern) ++
         expressions (pt_dcs pPattern) ++
         expressions (pt_kds pPattern)
        )
 subexpressions pPattern
  = subexpressions (pt_rls pPattern) ++
    subexpressions (pt_dcs pPattern) ++
    subexpressions (pt_kds pPattern)

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
 expressions pProcess
  = nub (expressions (procRules pProcess) ++
         expressions (procDcls  pProcess) ++
         expressions (procKds   pProcess)
        )
 subexpressions pProcess
  = subexpressions (procRules pProcess) ++
    subexpressions (procDcls  pProcess) ++
    subexpressions (procKds   pProcess)

instance Expr P_Rule where
 p_concs r = p_concs (rr_exp r)
 expressions r = expressions (rr_exp r)
 subexpressions r = subexpressions (rr_exp r)
instance Expr P_Sign where
 p_concs x = nub (psign x)
 expressions r = []
 subexpressions r = []
instance Expr P_Gen where
 p_concs x = nub [gen_gen x, gen_spc x]
 expressions r = []
 subexpressions r = []
instance Expr P_Declaration where
 p_concs d = p_concs (dec_sign d)
 expressions d = [PTyp (origin d) (Prel (origin d) (dec_nm d)) (dec_sign d)]
-- expressions d = [PCps orig (Pid (head sgn)) (PCps orig (Prel orig (dec_nm d)) (Pid (last sgn)))] where P_Sign sgn = dec_sign d; orig = origin d
 subexpressions d = [PTyp (origin d) (Prel (origin d) (dec_nm d)) (dec_sign d)]
instance Expr P_KeyDef where
 p_concs k = p_concs [ks_obj keyExpr | keyExpr@P_KeyExp{} <- kd_ats k]
 expressions k = expressions [ks_obj keyExpr | keyExpr@P_KeyExp{} <- kd_ats k]
 subexpressions k = subexpressions [ks_obj keyExpr | keyExpr@P_KeyExp{} <- kd_ats k]
instance Expr P_Interface where
 p_concs k = p_concs (ifc_Obj k)
 expressions k = expressions (ifc_Obj k)
 subexpressions k = subexpressions (ifc_Obj k)
instance Expr P_ObjectDef where
 p_concs o = p_concs (obj_ctx o) `uni` p_concs (expressions (obj_msub o))
 expressions o = [obj_ctx o | null (expressions (obj_msub o))]++expressions [PCps (origin o) (obj_ctx o) e | e<-expressions (obj_msub o)]
 subexpressions o = subexpressions (obj_ctx o)++subexpressions (obj_msub o)
instance Expr P_SubInterface where
 p_concs x@(P_Box{}) = p_concs (si_box x)
 p_concs _ = []
 expressions x@(P_Box{}) = expressions (si_box x)
 expressions _ = []
 subexpressions x@(P_Box{}) = subexpressions (si_box x)
 subexpressions _ = []
instance Expr P_Population where
 p_concs x = p_concs (p_type x)
 expressions x = []
 subexpressions x = []
instance Expr a => Expr (Maybe a) where
 p_concs Nothing = []
 p_concs (Just x) = p_concs x
 expressions Nothing = []
 expressions (Just x) = expressions x
 subexpressions Nothing = []
 subexpressions (Just x) = subexpressions x
instance Expr a => Expr [a] where
 p_concs = concat.map p_concs
 expressions = concat.map expressions
 subexpressions = concat.map subexpressions
instance Expr Term where
 p_concs   (PI _)         = []
 p_concs   (Pid c)        = [c]
 p_concs   (Pnid c)       = [c]
 p_concs   (Patm _ _ cs)  = nub cs
 p_concs   Pnull          = []
 p_concs   (Pfull _ cs)   = nub cs
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
 expressions e = [e]
 subexpressions e@(PI{})       = [e]
 subexpressions e@(Pid{})      = [e]
 subexpressions e@(Pnid{})     = [e]
 subexpressions e@(Patm{})     = [e]
 subexpressions e@Pnull        = [e]
 subexpressions e@(Pfull{})    = [e]
 subexpressions e@(Prel{})     = [e]
 subexpressions e@(Pflp{})     = [e]
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

data Guarded a = Errors [CtxError] | Checked a deriving (Show, Eq)

getErrors :: (Guarded a) -> [CtxError]
getErrors (Errors l) = l
getErrors _ = []

parallel :: (a->b->c) -> Guarded a -> Guarded b -> Guarded c -- get both values or collect all error messages
parallel f (Checked a) = (<*>) $ Checked (f a) 
parallel f (Errors  a) = (<*>) $ Errors a

parallelList :: [Guarded a] -> Guarded [a] -- get both values or collect all error messages
parallelList = foldr (parallel (:)) (Checked [])

instance Functor Guarded where
 fmap _ (Errors a) = (Errors a)
 fmap f (Checked a) = Checked (f a)
 
instance Applicative Guarded where
 pure a = Checked a
 (<*>) (Checked f) (Checked a) = Checked (f a)
 (<*>) (Errors  a) (Checked b) = Errors a 
 (<*>) (Checked a) (Errors  b) = Errors b
 (<*>) (Errors  a) (Errors  b) = Errors (a ++ b)
 
instance Monad Guarded where
 (>>=) (Errors  a) _ = (Errors a)
 (>>=) (Checked a) f = f a
 return = Checked

-- note the difference: if we want to get no errors from val2 if there are still errors in val1, use:
-- do { a <- val1; b <- val2; return (f a b) }
-- if we want to get both errors, use:
-- do { (a,b) <- getPair val1 val2; return (f a b) }

-- | Transform a context as produced by the parser into a type checked heterogeneous algebra.
--   Produce type error messages if necessary and produce proof graphs to document the type checking process.
pCtx2aCtx :: P_Context -> (A_Context,[CtxError],DotGraph String,DotGraph String)
pCtx2aCtx p_context
 = (contxt,typeErrors
   ,stTypeGraph,condTypeGraph)
   where
    contxt = ACtx{ ctxnm     = name p_context     -- The name of this context
             , ctxpos    = ctx_pos p_context
             , ctxlang   = fromMaybe Dutch (ctx_lang p_context)
             , ctxmarkup = fromMaybe ReST  (ctx_markup p_context) -- The default markup format for free text in this context
             , ctxpo     = gE -- 1
             , ctxthms   = ctx_thms p_context -- The patterns/processes to be printed in the functional specification. (for making partial documentation)
             , ctxrs     = ctxrules
             , ctxks     = keys          -- The key definitions defined in this context, outside the scope of patterns
             , ctxpats   = pats          -- The patterns defined in this context
                                         -- Each pattern contains all user defined rules inside its scope
             , ctxprocs  = procs         -- The processes defined in this context
             , ctxifcs   = ifcs          -- The interfaces defined in this context, outside the scope of patterns
             , ctxps     = apurp         -- The purposes defined in this context, outside the scope of patterns
             , ctxsql    = sqlPlugs      -- user defined sqlplugs, taken from the Ampersand script
             , ctxphp    = phpPlugs      -- user defined phpplugs, taken from the Ampersand script

             , ctxds     = adecs         -- The declarations defined in this context, outside the scope of patterns
             , ctxcds    = acds          -- All concept definitions
             , ctxgs     = agens         -- The gen definitions defined in this context, outside the scope of patterns
             , ctxenv    = (ERel(V (Sign ONE ONE)) ,[])
             , ctxmetas  = [ Meta pos metaObj nm val | P_Meta pos metaObj nm val <- ctx_metas p_context ]
             , ctxatoms  = allexplicitatoms
             }
    st = typing p_context
    (typeTable,_,_,_,_,_) = tableOfTypes st
    specializationTuples :: [(P_Concept,P_Concept)]
    specializationTuples = [(specCpt,genCpt) | (_, _, (TypExpr (Pid specCpt) _ _ _), genCpts)<-typeTable, genCpt<-genCpts]
    gE :: (A_Concept->A_Concept->DatabaseDesign.Ampersand.Core.Poset.Ordering, [[A_Concept]])
    gE = DatabaseDesign.Ampersand.Core.Poset.makePartialOrder [(pC2aC s, pC2aC g) | (s,g)<-specializationTuples]   -- The base hierarchy for the partial order of concepts (see makePartialOrder)
             where
                pC2aC pc = C {cptnm = p_cptnm pc
                             ,cptgE = fatal 63 "do not refer to this concept"
                             ,cptos = fatal 64 "do not refer to this concept"
                             ,cpttp = fatal 65 "do not refer to this concept"
                             ,cptdf = fatal 66 "do not refer to this concept"
                             }
    typeErrors :: [CtxError]
    typeErrors
     | (not.null) derivedEquals = derivedEquals
     | (not.null) cxerrs        = cxerrs
     | otherwise                = postchks
    derivedEquals :: [CtxError]
    derivedEquals   -- These concepts can be proven to be equal, based on st (= typing sentences, i.e. the expressions derived from the script).
     = [ CxeEqConcepts [ c| (_,_,TypExpr (Pid c) _ _ _)<-diffs]
       | diffs<-eqCl (\(_,classNr,_) -> classNr) (map head (eqClass tripleEq conceptTypes))
       , length diffs>1]
       where (_,_,t) `tripleEq` (_,_,t') = t == t'
    conceptTypes :: [(Int,Int,Type)]
    conceptTypes = [ (exprNr, classNr, e) | (exprNr, classNr, e@(TypExpr (Pid{}) _ _ _), _)<-typeTable ] --
                   -- error (showTypeTable typeTable) -- this is a good place to show the typeTable for debugging purposes.
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
    (sqlPlugs,sPlugcxes) = (unzip . map (pODef2aODef [] anything)          . ctx_sql  ) p_context
    (phpPlugs,pPlugcxes) = (unzip . map (pODef2aODef [] anything)          . ctx_php  ) p_context
    (allmbpops, popcxes) = (unzip . map  pPop2aPop                         . pops     ) p_context
    allpops    = [pop | Just pop<-allmbpops]
    allexplicitatoms = [cptos' | P_CptPopu cptos'<-pops p_context]
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
    pPat2aPat pops ppat 
     = (A_Pat { ptnm  = name ppat    -- Name of this pattern
              , ptpos = pt_pos ppat  -- the position in the file in which this pattern was declared.
              , ptend = pt_end ppat  -- the position in the file in which this pattern was declared.
              , ptrls = prules       -- The user defined rules in this pattern
              , ptgns = agens        -- The generalizations defined in this pattern
              , ptdcs = adecs        -- The declarations declared in this pattern
              , ptkds = keys         -- The key definitions defined in this pattern
              , ptxps = xpls         -- The purposes of elements defined in this pattern
              }
       , [CxeOrig typeErrs "pattern" (name ppat) (origin ppat) | (not.null) typeErrs]
       )
       where
        typeErrs = concat (rulecxes++keycxes++deccxes++xplcxes)
        (prules,rulecxes) = unzip arls
        arls  = map (pRul2aRul (name ppat)) (pt_rls ppat)
        agens = map (pGen2aGen (name ppat)) (pt_gns ppat)
        (keys,keycxes) = unzip akds
        akds  = map pKDef2aKDef (pt_kds ppat)
        (adecs,deccxes) = (unzip . map (pDecl2aDecl pops (name ppat)) . pt_dcs) ppat
        (xpls,xplcxes) = (unzip . map pPurp2aPurp . pt_xps) ppat
    
    pProc2aProc :: [Population] -> P_Process -> (Process,[CtxError])
    pProc2aProc pops pproc
     = (Proc { prcNm    = procNm pproc
             , prcPos   = procPos pproc
             , prcEnd   = procEnd pproc
             , prcRules = prules
             , prcGens  = agens          -- The generalizations defined in this pattern
             , prcDcls  = adecs          -- The declarations declared in this pattern
             , prcRRuls = arruls         -- The assignment of roles to rules.
             , prcRRels = arrels         -- The assignment of roles to Relations.
             , prcKds   = keys           -- The key definitions defined in this process
             , prcXps   = expls          -- The purposes of elements defined in this process
             }
       , [CxeOrig typeErrs "process" (name pproc) (origin pproc) | (not.null) typeErrs]
       )
       where
        typeErrs = concat (rulecxes++keycxes++deccxes++rrcxes++editcxes++explcxes)
        (prules,rulecxes) = (unzip . map (pRul2aRul (name pproc)) . procRules) pproc
        arrels = [(rol,rel) |rr<-rrels, rol<-rrRoles rr, rel<-rrRels rr]
        (rrels,editcxes)  = (unzip . map pRRel2aRRel            . procRRels) pproc
        agens  = map (pGen2aGen (name pproc)) (procGens pproc)
        arruls = [(rol,rul) |rul<-rules contxt, rr<-rruls, name rul `elem` mRules rr, rol<-mRoles rr]
        (adecs,deccxes)   = (unzip . map (pDecl2aDecl pops (name pproc)) . procDcls) pproc
        (rruls,rrcxes)    = (unzip . map  pRRul2aRRul                    . procRRuls) pproc
        (keys,keycxes)    = (unzip . map  pKDef2aKDef                    . procKds) pproc
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
    p2aPairView sgn (P_PairView ppvs) = (PairView pvs, concat errs) 
     where (pvs, errs) = unzip $ map (p2aPairViewSegment sgn) ppvs
    
    p2aPairViewSegment :: Sign -> P_PairViewSegment -> (PairViewSegment,[CtxError])
    p2aPairViewSegment  _  (P_PairViewText str)          = (PairViewText str, [])
    p2aPairViewSegment sgn (P_PairViewExp srcOrTgt pexp) = (PairViewExp srcOrTgt aexpr, exprcxe)
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
       , [CxeOrig typeErrors "rule" "" (origin prul) | (not.null) typeErrors]
       )
       where typeErrors        = exprcxe++mviolcxe
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
       , [CxeOrig typeErrors "key definition" "" (origin pkdef) | (not.null) typeErrors]
       )
       where
        typeErrors = kdcxe++concat segscxes
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
       , [CxeOrig typeErrors "interface" (name pifc) (origin pifc) | (not.null) typeErrors]
       )
       where
        typeErrors = objcxe++concat prmcxes++duplicateRoleErrs++undeclaredRoleErrs
        parentIfcRoles = if null $ ifc_Roles pifc then roles contxt else ifc_Roles pifc -- if no roles are specified, the interface supports all roles
        (obj,objcxe) = pODef2aODef parentIfcRoles anything (ifc_Obj pifc)
        (prms,prmcxes) = unzip [pRel2aRel (psign sgn) r
                               | PTyp _ r@(Prel{}) sgn<-ifc_Params pifc -- Todo: link untyped relations to their type!
                                                        ++fatal 669 ("Untyped relation(s) "++ intercalate ", " [nm | (Prel _ nm)<-ifc_Params pifc])
                               ]
        duplicateRoleErrs = [newcxe $ "Duplicate interface role \""++role++"\" at "++show (origin pifc) | role <- nub $ ifc_Roles pifc, length (filter (==role) $ ifc_Roles pifc) > 1 ]
        undeclaredRoleErrs = [newcxe $ "Undeclared interface role \""++role++"\" at "++show (origin pifc) | null duplicateRoleErrs, role <- nub $ ifc_Roles pifc, role `notElem` roles contxt ]
        -- we show the line nr for the interface, which may be slightly inaccurate, but roles have no position 
        -- and the implementation of error messages makes it difficult to give a nice one here
        
    -- | pODef2aODef checks compatibility of composition of expressions on equality
    pODef2aODef :: [String]              -- a list of roles that may use this object
                -> Type                  -- the universe for type checking this object. anything if the type checker decides freely, thing c if it must be of type c.
                -> P_ObjectDef           -- the object definition as specified in the parse tree
                -> (ObjectDef,[CtxError]) -- result: the type checked object definition (only defined if there are no type errors) and a list of type errors
    pODef2aODef parentIfcRoles universe podef 
     = (Obj { objnm   = obj_nm podef
            , objpos  = obj_pos podef
            , objctx  = expr
            , objmsub = msub
            , objstrs = obj_strs podef
            }
       , [CxeOrig typeErrors "object definition" "" (origin podef) | (not.null) typeErrors]
       )
       where
        typeErrors = nmchk++exprcxe++msubcxes
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
                   TypExpr (Pid c) _ _ _ -> c
                   _                     -> fatal 1235 ("erroneous type found by pODef2aODef.")
        
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
    
    pPop2aPop :: P_Population -> (Maybe Population,[CtxError])
    pPop2aPop (P_CptPopu{}) = (Nothing,[])
    pPop2aPop p@(P_Popu{})
     = ( Just (Popu { popm  = aRel
                    , popps = p_popps p
                    })
       , relcxe
       )
       where (ERel aRel, _, relcxe) = pExpr2aExpr (PTyp (origin p) (Prel (origin p) (name p)) (p_type p))
    
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
    pDecl2aDecl pops patname pd =
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
                       concat [popps pop | pop<-pops, let ad=popm pop
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
    pRel2aRel _ (Pfull orig pConcepts)
     = case pConcepts of
        [] -> ( fatal 326 "Ambiguous universal relation."
              , [CxeOrig [newcxe "Ambiguous universal relation."] "relation" "" orig]
              )
        [c] -> (V (Sign (pCpt2aCpt c) (pCpt2aCpt c)), [])
        [s,t] -> (V (Sign (pCpt2aCpt s) (pCpt2aCpt t)), [])
        _   -> fatal 328 "Encountered a Sign with more than two elements."
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
    pRel2aRel sgn (Prel orig nm)
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

    pExpr2aExpr :: Term    -- The expression to be typed
                -> ( Expression    -- the resulting expression. It is defined only when the list of errors is empty.
                   , (Type,Type)   -- the type of the resulting expression. It is defined only when the list of errors is empty.
                   , [CtxError]    -- the list of type errors
                   )
    pExpr2aExpr pExpr                        -- The expression to be typed
     = ( f pExpr                             -- the resulting expression. It is defined only when the list of errors is empty.
       , (lookup pExpr,lookup (p_flp pExpr)) -- the type of the resulting expression. It is defined only when the list of errors is empty.
       , g pExpr                             -- the list of type errors
       )
       where
         f :: Term -> Expression
         f (PTyp _ (PI _)  (P_Sign (c:_))) = ERel (I (pCpt2aCpt c))
         f (PTyp _ (Pid _) (P_Sign (c:_))) = ERel (I (pCpt2aCpt c))
         f x@(PTyp o (Pid _) (P_Sign _))   = fatal 983 ("pExpr2aExpr cannot transform "++show x++" ("++show o++") to an expression.")
         f (Pid c)         = ERel (I (pCpt2aCpt c))
         f (Pnid c)        = ECpl (ERel (I (pCpt2aCpt c)))
         f (Patm _ atom [c]) = ERel (Mp1 atom (pCpt2aCpt c))
         f x@Pnull         = fatal 988 ("pExpr2aExpr cannot transform "++show x++" to an expression.")
         f (Pfull _ [s,t]) = ERel (V (Sign (pCpt2aCpt s) (pCpt2aCpt t)))
         f (Pfull _ [s])   = ERel (V (Sign (pCpt2aCpt s) (pCpt2aCpt s)))
         f x@(Pfull o [])  = fatal 991 ("pExpr2aExpr cannot transform "++show x++" ("++show o++") to an expression.")
         f (Prel o a)      = ERel (Rel{relnm=a, relpos=o})
         f (Pflp o a)      = EFlp (f (Prel o a))
         f (Pequ _ a b)    = EEqu (f a, f b)
         f (Pimp _ a b)    = EImp (f a, f b)
         f (PIsc o (PIsc _ a b) c) = EIsc (a_b_s++[f c]) where EIsc a_b_s = f (PIsc o a b)
         f (PIsc o a (PIsc _ b c)) = EIsc ([f a]++b_c_s) where EIsc b_c_s = f (PIsc o b c)
         f (PIsc _ a b)            = EIsc [f a, f b]
         f (PUni o (PUni _ a b) c) = EUni (a_b_s++[f c]) where EUni a_b_s = f (PUni o a b)
         f (PUni o a (PUni _ b c)) = EUni ([f a]++b_c_s) where EUni b_c_s = f (PUni o b c)
         f (PUni _ a b)            = EUni [f a, f b]
         f (PDif _ a b)    = EDif (f a, f b)
         f (PLrs _ a b)    = ELrs (f a, f b)
         f (PRrs _ a b)    = ERrs (f a, f b)
         f (PCps o (PCps _ a b) c) = ECps (a_b_s++[f c]) where ECps a_b_s = f (PCps o a b)
         f (PCps o a (PCps _ b c)) = ECps ([f a]++b_c_s) where ECps b_c_s = f (PCps o b c)
         f (PCps _ a b)            = ECps [f a, f b]
         f (PRad o (PRad _ a b) c) = ERad (a_b_s++[f c]) where ERad a_b_s = f (PRad o a b)
         f (PRad o a (PRad _ b c)) = ERad ([f a]++b_c_s) where ERad b_c_s = f (PRad o b c)
         f (PRad _ a b)            = ERad [f a, f b]
         f (PPrd o (PPrd _ a b) c) = EPrd (a_b_s++[f c]) where EPrd a_b_s = f (PPrd o a b)
         f (PPrd o a (PPrd _ b c)) = EPrd ([f a]++b_c_s) where EPrd b_c_s = f (PPrd o b c)
         f (PPrd _ a b)            = EPrd [f a, f b]
         f (PKl0 _ a)      = EKl0 (f a)
         f (PKl1 _ a)      = EKl1 (f a)
         f (PFlp _ a)      = EFlp (f a)
         f (PCpl _ a)      = ECpl (f a)
         f (PBrk _ a)      = EBrk (f a)
         f x@(PTyp o _ (P_Sign [])) = fatal 991 ("pExpr2aExpr cannot transform "++show x++" ("++show o++") to an expression.")
         f (PTyp _ a sgn)  = ETyp (f a) (Sign (pCpt2aCpt s) (pCpt2aCpt t))
                             where P_Sign cs = sgn; s=head cs; t=last cs
         
         g :: Term -> [CtxError]
         g x@(PI _)            = errILike x
         g x@(Pid _)           = errILike x
         g x@(Pnid _)          = errILike x
         g x@(Patm _ _ _)      = errILike x
         g x@Pnull             = errILike x
         g x@(Pfull _ [s,t])   = []
         g x@(Pfull _ [s])     = []
         g x@(Pfull o [])      = []
         g x@(Prel o a)        = []
         g x@(Pflp o a)        = []
         g x@(Pequ _ a b)      = errEquLike x a b
         g x@(Pimp _ a b)      = errEquLike x a b
         g x@(PIsc _ a b)      = errEquLike x a b
         g x@(PUni _ a b)      = errEquLike x a b
         g x@(PDif _ a b)      = errEquLike x a b
         g x@(PLrs _ a b)      = errCpsLike x a b
         g x@(PRrs _ a b)      = errCpsLike x a b
         g x@(PCps _ a b)      = errCpsLike x a b
         g x@(PRad _ a b)      = errCpsLike x a b
         g x@(PPrd _ a b)      = g a++g b
         g x@(PKl0 _ a)        = g a
         g x@(PKl1 _ a)        = g a
         g x@(PFlp _ a)        = g a
         g x@(PCpl _ a)        = errCpl x a
         g x@(PBrk _ a)        = g a
         g x@(PTyp _ a (P_Sign [])) = g a
         g x@(PTyp _ a sgn)    = g a
         errEquLike x a b
          = if null deepErrors then nodeError else deepErrors -- for debugging, in front of this if statement is a good place to add  error (showTypeTable typeTable) ++ 
            where
             nodeError = [ CxeEquLike {cxeExpr    = x
                                      ,cxeLhs     = a
                                      ,cxeRhs     = b
                                      ,cxeSrcCpts = srcConflictingConcepts
                                      ,cxeTrgCpts = trgConflictingConcepts
                                      }
                         | (_,_,TypExpr t _ _ _,srcConflictingConcepts)<-typeTable, t==x
                         , (_,_,TypExpr t _ _ _,trgConflictingConcepts)<-typeTable, t==p_flp x
                         , length srcConflictingConcepts/=1, length trgConflictingConcepts/=1
                         ]
             deepErrors = g a++g b
         errCpl x a
          = if null deepErrors then nodeError else deepErrors -- for debugging, in front of this if statement is a good place to add  error (showTypeTable typeTable) ++ 
            where
             nodeError =  [ CxeCpl {cxeExpr   = a
                                   ,cxeCpts   = conflictingConcepts
                                   }
                          | (_,_,TypExpr expr _ _ _,conflictingConcepts)<-typeTable
                          , length conflictingConcepts/=1
                          , origin x==origin expr, expr==x
                          ]
             deepErrors = g a
         errILike x
          = nub [ CxeILike {cxeExpr   = expr
                           ,cxeCpts   = conflictingConcepts
                           }
                | (_,_,TypExpr expr _ _ _,conflictingConcepts)<-typeTable
                , length conflictingConcepts/=1
                , origin x==origin expr, expr==x
                ]
         errCpsLike x a b
          = if null deepErrors then nodeError else deepErrors -- for debugging, in front of this if statement is a good place to add  error (showTypeTable typeTable) ++ 
            where
             nodeError = [ CxeCpsLike {cxeExpr   = x
                                      ,cxeCpts   = conflictingConcepts
                                      }
                         | (_,_,TypLub _ _ _ origExpr,conflictingConcepts)<-typeTable
                         , length conflictingConcepts/=1
                         , origin x==origin origExpr
                         ]
             deepErrors = g a++g b
         lookup pExpr = head ([ thing c| (_,_,TypLub _ _ _ origExpr,[c])<-typeTable, pExpr==origExpr ]++fatal 1535 ("cannot find "++showADL pExpr++" in the lookup table"))

isConceptTerm :: Term -> Bool
isConceptTerm (Pid{}) = True
isConceptTerm _ = False

--the type checker always returns an expression with sufficient type casts, it should remove redundant ones.
--applying the type checker on an complete, explicitly typed expression is equivalent to disambiguating the expression

-- | Disambiguation is needed for the purpose of printing an expression.    parse (disambiguate expr) = expr
--   Produce type error messages if necessary and produce proof graphs to document the type checking process.
disambiguate :: Fspc -> Expression -> Expression
disambiguate fSpec x = x -- temporarily disabled (19 july 2012), in order to get the type checker correct first...
{-
 | null errs = expr 
 | otherwise  = fatal 428 ("an expression must be type correct, but this one is not:\n" ++ show errs)
 where
   (expr,errs) = pExpr2aExpr fSpec{vrels=vrels fSpec++deltas} NoCast (f x)
   -- f transforms x to a Term using full relation signatures
   f (EEqu (l,r)) = Pequ OriginUnknown (f l) (f r)
   f (EImp (l,r)) = Pimp OriginUnknown (f l) (f r)
   f (EIsc [l])   = f l
   f (EIsc [l,r]) = PIsc OriginUnknown (f l) (f r)
   f (EIsc (l:rs))= PIsc OriginUnknown (f l) (f (EIsc rs))
   f (EIsc _)     = Pfull OriginUnknown []
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
   f (ERel rel@(V{}))   = Pfull OriginUnknown [g (source rel),g (target rel)]
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


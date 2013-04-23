{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
{-# LANGUAGE RelaxedPolyRec #-} -- RelaxedPolyRec required for OpenSuse, for as long as we@OpenUniversityNL use an older GHC
module DatabaseDesign.Ampersand.ADL1.P2A_Converters (
     -- * Exported functions
     pCtx2aCtx,
     Guarded(..),parallelList
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
import Prelude hiding (head)
import DatabaseDesign.Ampersand.Input.ADL1.CtxError
import Data.GraphViz hiding (addExtension, C)
import Data.GraphViz.Attributes.Complete hiding (Box, Pos)
import Data.Maybe
import Data.List hiding (head)
import DatabaseDesign.Ampersand.ADL1.TypePropagation

import Data.Char
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map hiding (Map)

head :: [a] -> a
head [] = fatal 30 "head must not be used on an empty list!"
head (a:_) = a
-- TODO: this module should import Database.Ampersand.Core.ParseTree directly, and it should be one 
--       of the very few modules that imports it. (might require some refactoring due to shared stuff)

fatal :: Int -> String -> a
fatal = fatalMsg "ADL1.P2A_Converters"

{-The data structure Type is used to represent a term inside the type checker.
TypExpr e flipped is read as:
  "the source type of e, with e equal to (if flipped then PFlp e else e)."
Between err tl tr btp
  "the btp (upperbound/lowerbound/equality) of types a and b, which must satisfy the property of btp, or else err is thrown."
-}




complement :: Term -> Term
complement (PCpl _ a) = a
complement a          = PCpl (origin a) a

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





nothing :: Typemap
nothing = Map.empty

infixl 2 .+.   -- concatenate two lists of types
infixl 3 .<.   -- makes a list of one tuple (t,t'), meaning that t is a subset of t'
infixl 3 .=.   -- makes a list of two tuples (t,t') and (t',t), meaning that t is equal to t'
typeToMap :: Type -> Typemap
typeToMap x = Map.fromList [(x,[])]
(.<.) :: Type -> Type -> Typemap
a .<. b  = (Map.fromList [(a, [b]),(b, [])]) -- a tuple meaning that a is a subset of b, and introducing b as a key.
(.=.) :: Type -> Type -> Typemap
a .=. b  = (Map.fromList [(a, [b]),(b, [a])])
(.+.) :: Typemap -> Typemap -> Typemap
m1 .+. m2 = Map.unionWith mrgUnion m1 m2
thing :: P_Concept -> Type
thing c  = TypExpr (Pid (SomewhereNear (fatal 90 "clueless about where this is found. Sorry" )) c) Src
dom, cod :: Term -> Type
dom x    = TypExpr x Src -- the domain of x
cod x    = TypExpr x Tgt 
domOrCod :: SrcOrTgt -> Term -> Type
domOrCod Src = dom
domOrCod Tgt = cod
mSpecific', mGeneric', mEqual' :: Type -> SrcOrTgt -> Term -> SrcOrTgt -> Term -> Term -> Typemap
mSpecific' s ta a tb b e = s .=. s' .+. dm  where (dm,s')=mSpecific'' ta a tb b e
mGeneric'  s ta a tb b e = s .=. s' .+. dm  where (dm,s')=mGeneric''  ta a tb b e
mEqual'    s ta a tb b e = s .=. s' .+. dm  where (dm,s')=mEqual''    ta a tb b e
mSpecific'', mGeneric'', mEqual'' :: SrcOrTgt -> Term -> SrcOrTgt -> Term -> Term -> (Typemap,Type)
mGeneric''  ta a tb b e = ((domOrCod ta a) .<. r .+. (domOrCod tb b) .<. r, r ) where r = Between (tCxe ta a tb b TETUnion e) (domOrCod ta a) (domOrCod tb b) BTUnion
mSpecific'' ta a tb b e = (r .<. (domOrCod ta a) .+. r .<. (domOrCod tb b), r ) where r = Between (tCxe ta a tb b TETIsc   e) (domOrCod ta a) (domOrCod tb b) BTIntersect
mEqual''    ta a tb b e = (r .=. (domOrCod ta a) .+. r .=. (domOrCod tb b), r ) where r = Between (tCxe ta a tb b TETEq    e) (domOrCod ta a) (domOrCod tb b) BTEqual
mSpecific, mGeneric, mEqual :: SrcOrTgt -> Term -> SrcOrTgt -> Term -> Term -> Typemap
mSpecific ta a tb b e = fst (mSpecific'' ta a tb b e)
mGeneric  ta a tb b e = fst (mGeneric''  ta a tb b e)
mEqual    ta a tb b e = typeToMap$ Between (tCxe ta a tb b TETEq e) (domOrCod ta a) (domOrCod tb b) BTEqual
tCxe :: SrcOrTgt -> Term -> SrcOrTgt -> Term -> (t -> TypErrTyp) -> t -> [P_Concept] -> [P_Concept] -> CtxError
tCxe ta a tb b msg e src trg = CxeTyping{cxeLhs=(a,ta,src),cxeRhs=(b,tb,trg),cxeTyp=msg e}

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

class Expr a where
  p_declarations :: a -> [P_Declaration]
  p_declarations _ = []
  p_rules :: a -> [P_Rule]
  p_rules _ = []
  p_keys :: a -> [P_IdentDef]
  p_keys _ = []
  p_views :: a -> [P_ViewDef]
  p_views _ = []
  -- | uType provides the basis for a domain analysis. It traverses an Ampersand script recursively, harvesting on its way
  --   the tuples of a relation st :: Type * Type. Each tuple (TypExpr t, TypExpr t') means that the domain of t is a subset of the domain of t'.
  --   These tuples are produced in two Typemaps. The second Typemap is kept separate, because it depends on the existence of the first Typemap.
  --   The first element of the first argument is a P_Context that represents the parse tree of one context.
  --   This is provided to obtain a declaration table and a list of interfaces from the script.
  --   The second element of the first argument is a compatibility function, that determines whether two types are compatible.
  uType :: a           -- x:    the original term from the script, meant for representation in the graph.
        -> a           -- z:    the term to be analyzed, which must be logically equivalent to x
        -> Typemap   -- for each type, a list of types that are subsets of it, which is the result of analysing term x.
  uType' :: a -> Typemap
  uType' x = uType x x

instance Expr P_Context where
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
 p_views pContext
  = concat [ p_views pat | pat<-ctx_pats  pContext] ++
    concat [ p_views prc | prc<-ctx_PPrcs pContext] ++
    ctx_vs pContext
 uType _ pContext
  = uType' (ctx_pats  pContext) .+.
    uType' (ctx_PPrcs pContext) .+.
    uType' (ctx_rs    pContext) .+.
    uType' (ctx_ds    pContext) .+.
    uType' (ctx_ks    pContext) .+.
    uType' (ctx_vs    pContext) .+.
    uType' (ctx_gs    pContext) .+.
    uType' (ctx_ifcs  pContext) .+.
    uType' (ctx_ps    pContext) .+.
    uType' (ctx_sql   pContext) .+.
    uType' (ctx_php   pContext) .+.
    uType' (ctx_pops  pContext)

instance Expr P_Pattern where
 p_declarations pPattern
  = pt_dcs pPattern
 p_rules pPattern
  = pt_rls pPattern
 p_keys pPattern
  = pt_ids pPattern
 p_views pPattern
  = pt_vds pPattern
 uType _ pPattern
  = uType' (pt_rls pPattern) .+.
    uType' (pt_gns pPattern) .+.
    uType' (pt_dcs pPattern) .+.
    uType' (pt_ids pPattern) .+.
    uType' (pt_vds pPattern) .+.
    uType' (pt_xps pPattern) .+.
    uType' (pt_pop pPattern)

instance Expr P_Process where
 p_declarations pProcess
  = procDcls pProcess
 p_rules pProcess
  = procRules pProcess
 p_keys pProcess
  = procIds pProcess
 p_views pProcess
  = procVds pProcess
 uType _ pProcess
  = uType' (procRules pProcess) .+.
    uType' (procGens  pProcess) .+.
    uType' (procDcls  pProcess) .+.
    uType' (procIds   pProcess) .+.
    uType' (procVds   pProcess) .+.
    uType' (procXps   pProcess) .+.
    uType' (procPop   pProcess)

instance Expr P_Rule where
 p_rules r = [r]
 uType _ r
  = uType' (rr_exp r) .+. 
    uType' (rr_viol r) .+.
    foldr (.+.) nothing ( [ typeToMap$
                            Between (\s t->CxeObjMismatch{cxeExpr=trm,cxeEnv=s,cxeSrcs=t})
                                    ((\x -> TypExpr x sOrT) (rr_exp r))
                                    (TypExpr trm Src) BTEqual
                          | Just pv <- [rr_viol r]
                          , (P_PairViewExp sOrT trm) <- ppv_segs pv ]
                        )

instance Expr P_PairView where
 uType _ (P_PairView segments) = uType segments segments

instance Expr P_PairViewSegment where
 uType _ (P_PairViewExp Src term) = uType term term
 uType _ (P_PairViewExp Tgt term) = uType term term
 uType _ P_PairViewText{} = nothing
  
instance Expr P_IdentDef where
 p_keys k = [k]
 uType _ k
  = let x=Pid (SomewhereNear (fatal 233 "clueless about where this is found. Sorry" )) (ix_cpt k) in
    uType x x .+.
    foldr (.+.) nothing [ uType obj obj .+.
                          mSpecific Src x Src (obj_ctx obj) (obj_ctx obj)
                           -- TODO: improve mSpecific's error message here
                        | P_IdentExp obj <- ix_ats k
                        ]

instance Expr P_ViewDef where
 p_views v = [v]
 uType _ v
  = let x=Pid (SomewhereNear (fatal 244 "clueless about where this is found. Sorry" )) (vd_cpt v) in
    uType x x .+.
    foldr (.+.) nothing [ uType obj obj .+.
                          mSpecific Src x Src (obj_ctx obj) (obj_ctx obj)
                           -- TODO: improve mSpecific's error message here
                        | P_ViewExp obj <- vd_ats v
                        ]

-- TODO: continue adding errors until you reach instance Expr Term
instance Expr P_Interface where
 uType _ ifc
  = let x=ifc_Obj ifc in
    foldr (.+.) nothing (map uType' (ifc_Params ifc)) .+.
    uType x x

instance Expr P_ObjectDef where
 uType _ o
  = let x=obj_ctx o in
    uType x x .+. 
    foldr (.+.) nothing [ uType obj obj .+. dm
                        | Just subIfc <- [obj_msub o]
                        , obj <- case subIfc of
                                   P_Box{}          -> si_box subIfc
                                   P_InterfaceRef{} -> []
                        , let dm = mSpecific Tgt x Src (obj_ctx obj) (obj_ctx obj)
                        ]
 
instance Expr P_SubInterface where
 uType _  mIfc = 
   case mIfc of
     P_Box{} -> uType' (si_box mIfc)
     P_InterfaceRef {} -> nothing

instance Expr PPurpose where
 uType _ purp = uType' (pexObj purp)

instance Expr PRef2Obj where
 uType _ pRef =
   case pRef of 
     PRef2ConceptDef str -> uType' (Pid (SomewhereNear (fatal 279 "clueless about where this is found. Sorry" ))(PCpt str))
     PRef2Declaration t  -> uType' t
     _                   -> nothing

instance Expr P_Sign where
 uType _ _ = nothing

instance Expr P_Gen where
 uType _ g
  = uType' (Pimp (origin g) (Pid (origin g) (gen_spc g)) (Pid (origin g) (gen_gen g)))

instance Expr P_Declaration where
 uType _ d
  = dom decl.<.thing src .+. cod decl.<.thing trg
    where decl = PTrel (origin d) (dec_nm d) (dec_sign d)
          P_Sign src trg = dec_sign d

instance Expr P_Population where
 uType _ pop
  = uType x x .+. dom x.=.dom x .+. cod x.=.cod x
    where x = case pop of
                   P_RelPopu{} -> Prel  (p_orig pop) (name pop)
                   P_TRelPop{} -> PTrel (p_orig pop) (name pop) (p_type pop)
                   P_CptPopu{} -> Pid   (p_orig pop) (PCpt (name pop))

instance Expr a => Expr (Maybe a) where
 uType _ Nothing  = nothing
 uType _ (Just x) = uType' x

instance Expr a => Expr [a] where
 uType _ xs = foldr (.+.) nothing (map uType' xs)

instance Expr Term where 
 uType x term 
  = case term of
     PI{}          -> dom x.=.cod x              -- I
     Pid{}         -> dom x.=.cod x              -- I[C]
     (Patm _ _ []) -> dom x.=.cod x             -- 'Piet'   (an untyped singleton)
     (Patm _ _ cs) -> dom x.<.thing (head cs) .+. cod x.<.thing (last cs) -- 'Piet'[Persoon]  (a typed singleton)
                       .+. dom x.=.cod x
     PVee{}        -> typeToMap (dom x) .+. typeToMap (cod x) 
     (Pfull o s t) -> dom x.<.dom (Pid o s) .+. cod x.<.cod (Pid o t)              --  V[A*B] (the typed full set)
     (Pequ _ a b)  -> dom a.=.dom b .+. cod a.=.cod b .+. dom b.=.dom x .+. cod b.=.cod x    --  a=b    equality
                      .+. mEqual' (dom x) Src a Src b x .+. mEqual' (cod x) Tgt a Tgt b x
                      .+. uType a a .+. uType b b
     (PIsc _ a b)  -> dom x.<.dom a .+. dom x.<.dom b .+. cod x.<.cod a .+. cod x.<.cod b
                      .+. mSpecific' (dom x) Src a Src b x .+. mSpecific' (cod x) Tgt a Tgt b x
                      .+. uType a a .+. uType b b
     (PUni _ a b)  -> dom a.<.dom x .+. dom b.<.dom x .+. cod a.<.cod x .+. cod b.<.cod x
                      .+. mGeneric' (dom x) Src a Src b x .+. mGeneric' (cod x) Tgt a Tgt b x
                      .+. uType a a .+. uType b b
     (PDif _ a b)  -> dom x.<.dom a .+. cod x.<.cod a                                        --  a-b    (difference)
                      .+. uType' a .+. uType' b
                      .+. mSpecific Src a Src b x .+. mSpecific Tgt a Tgt b x
     (PCps _ a b)  -> let (bm,s) = mSpecific'' Tgt a Src b x
                          pidTest (PI{}) r = r
                          pidTest (Pid{}) r = r
                          pidTest (Patm{}) r = r
                          pidTest _ _ = nothing
                      in dom x.<.dom a .+. cod x.<.cod b .+.                                    -- a;b      composition
                         bm .+. uType a a .+. uType b b
                         .+. pidTest a (dom x.=.s) .+. pidTest b (cod x.=.s)
-- PRad is the De Morgan dual of PCps. However, since PUni and UIsc are treated separately, mGeneric and mSpecific are not derived, hence PRad cannot be derived either
     (PRad _ a b) -> let pnidTest (PCpl _ (PI{})) r = r
                         pnidTest (PCpl _ (Pid{})) r = r
                         pnidTest (PCpl _ (Patm{})) r = r
                         pnidTest _ _ = nothing
                     in dom x .<. dom a .+. cod x .<. cod b
                        .+. mGeneric Tgt a Src b x .+. uType a a .+. uType b b
                        .+. pnidTest a (dom b.<.dom x) .+. pnidTest b (cod a.<.cod x)
     (PPrd _ a b) -> dom x.=.dom a .+. cod x.=.cod b                                        -- a*b cartesian product
                     .+. uType a a .+. uType b b
     (PKl0 _ e)   -> dom e.<.dom x .+. cod e.<.cod x .+. uType e e
     (PKl1 _ e)   -> dom e.<.dom x .+. cod e.<.cod x .+. uType e e
     (PFlp _ e)   -> cod e.=.dom x .+. dom e.=.cod x .+. uType e e
     (PBrk _ e)   -> dom x.=.dom e .+. cod x.=.cod e .+. uType x e  -- (e) brackets
     (PTrel _ _ (P_Sign src trg)) -> dom x.<.thing src .+. cod x.<.thing trg
     (Prel _ _)   -> typeToMap (dom x) .+. typeToMap (cod x)
 -- derived uTypes: the following do no calculations themselves, but merely rewrite terms to the ones we covered
     (PCpl o a)   -> let e = PDif o (PVee o) a
                     in dom x.=.dom e .+. cod x.=.cod e .+.
                        uType x e                  --  -a    unary complement
     (Pimp o a b) -> let e = Pequ o a (PIsc o a b)
                     in dom x.=.dom e .+. cod x.=.cod e .+.
                        uType x e                 --  a|-b    implication (aka: subset)
     (PLrs o a b) -> let e = complement (PCps o (complement a) (p_flp b))
                     in dom x.=.dom e .+. cod x.=.cod e .+.
                        uType x e                 --  a/b = a!-b~ = -(-a;b~)
     (PRrs o a b) -> let e = complement (PCps o (p_flp a) (complement b))
                     in dom x.=.dom e .+. cod x.=.cod e .+.
                        uType x e                 --  a\b = -a~!b = -(a~;-b)


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
             , ctxvs     = views
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
    bindings :: Map Term P_Declaration         -- yields declarations that may be bound to relations, intended as a suggestion to the programmer
    isaClos, isaClosReversed :: Map P_Concept [P_Concept]                   -- 
    (st, stClos, eqType, stClosAdded, stClos1 , bindingsandsrcTypes, isaClos, isaClosReversed)
     = typing (uType p_context p_context)
              (Map.fromListWith mrgUnion [ (name (head cl)
                                           , uniqueCl cl)
                                         | cl<-eqCl name (p_declarations p_context) ])
    uniqueCl :: [P_Declaration] -> [P_Declaration]
    uniqueCl cl = sort (removeDoubleDeclarations (sortBy (\x y -> compare (dec_sign x) (dec_sign y)) cl))
    removeDoubleDeclarations [] = []
    removeDoubleDeclarations [x] = [x]
    removeDoubleDeclarations (x:y:ys) | dec_sign x == dec_sign y = removeDoubleDeclarations (y:ys)
                                      | otherwise = x:removeDoubleDeclarations (y:ys)
    
    
    (bindings,srcTypes,srcTypErrs)
     = case bindingsandsrcTypes of
          Checked (a,b) -> (a,b,[])
          Errors t -> (fatal 930 "bindings undefined",fatal 931 "srcTypes undefined",t)
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
     | (not.null) srcTypErrs    = srcTypErrs
     | (not.null) cxerrs        = cxerrs
     | otherwise                = postchks
    derivedEquals :: [CtxError]
    derivedEquals   -- These concepts can be proven to be equal, based on st (= typing sentences, i.e. the terms derived from the script).
     = [ CxeEqConcepts eqs
       | (TypExpr (Pid{}) _, equals)<-Map.toAscList eqType
       , let eqs=[c | TypExpr (Pid _ c) _<-equals ]
       , length eqs>1]
    (stTypeGraph,eqTypeGraph) = typeAnimate st stClos eqType stClosAdded stClos1
    cxerrs = rulecxes++keycxes++viewcxes++interfacecxes++patcxes++proccxes++sPlugcxes++pPlugcxes++popcxes++deccxes++xplcxes++themeschk
    --postchcks are those checks that require null cxerrs 
    postchks = rulenmchk ++ ifcnmchk ++ patnmchk ++ cyclicInterfaces
    acds = ctx_cs p_context++concatMap pt_cds (ctx_pats p_context)++concatMap procCds (ctx_PPrcs p_context)
    agens = map (pGen2aGen "NoPattern") (ctx_gs p_context)
    (adecsNPops,deccxes)   = case (parallelList . map pDecl2aDecl           . ctx_ds   ) p_context of
                              Checked decs -> ([(d{decpat="NoPattern"},mp) | (d,mp)<-decs], [])
                              Errors  errs -> (fatal 1030 ("Do not refer to undefined declarations\n"++show errs), errs)
    (apurp,   xplcxes)   = case (parallelList . map  pPurp2aPurp            . ctx_ps   ) p_context of
                            Checked purps -> (purps, [])
                            Errors  errs  -> (fatal 1033 ("Do not refer to undefined purposes\n"++show errs), errs)
    (pats,    patcxes)   = case (parallelList . map pPat2aPat               . ctx_pats ) p_context of
                            Checked pats' -> (pats', [])
                            Errors  errs  -> (fatal 1036 ("Do not refer to undefined patterns\n"++show errs), errs)
    (procs,   proccxes)  = case (parallelList . map pProc2aProc             . ctx_PPrcs) p_context of
                            Checked prcs -> (prcs, [])
                            Errors errs  -> (fatal 1039 ("Do not refer to undefined processes\n" ++ show errs), errs)
    (ctxrules,rulecxes)  = case (parallelList . map (pRul2aRul "NoPattern") . ctx_rs   ) p_context of
                            Checked ruls -> (ruls, [])
                            Errors errs  -> (fatal 1042 ("Do not refer to undefined rules\n"++show errs), errs)
    (keys,    keycxes)   = case (parallelList . map pIdentity2aIdentity     . ctx_ks   ) p_context of
                            Checked ks   -> (ks, [])
                            Errors errs  -> (fatal 1045 ("Do not refer to undefined keys\n"++show errs), errs)
    (views,   viewcxes)  = case (parallelList . map pViewDef2aViewDef       . ctx_vs   ) p_context of
                            Checked vs   -> (vs, [])
                            Errors errs  -> (fatal 566 ("Do not refer to undefined views\n"++show errs), errs)
    (ifcs,interfacecxes) = case (parallelList . map  pIFC2aIFC              . ctx_ifcs ) p_context of
                            Checked is -> (is, []) -- (fatal 543 ("Diagnostic: "++concat ["\n\n   "++show ifc | ifc<-is])::[Interface], [])
                            Errors errs  -> (fatal 1048 ("Do not refer to undefined interfaces\n"++show errs), errs)
    (sqlPlugs,sPlugcxes) = case (parallelList . map (pODef2aODef [])        . ctx_sql  ) p_context of
                            Checked plugs -> (plugs, [])
                            Errors errs   -> (fatal 1051 ("Do not refer to undefined sqlPlugs\n"++show errs), errs)
    (phpPlugs,pPlugcxes) = case (parallelList . map (pODef2aODef [])        . ctx_php  ) p_context of
                            Checked plugs -> (plugs, [])
                            Errors errs   -> (fatal 1054 ("Do not refer to undefined phpPlugs\n"++show errs), errs)
    (popsfrompops, popcxes) = case (parallelList . map  pPop2aPop . pops) p_context of
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
                  `uni` mp1Exprs views
                  `uni` mp1Exprs ifcs 
             
    themeschk = case orphans of
                 []   -> []
                 [nm] -> [newcxe ("Theme '"++nm++"' is selected for output, but is not defined.")]
                 _    -> [newcxe ("The following themes are selected for output, but are not defined:\n   "++intercalate ", " orphans)]
                where orphans = ctxthms contxt>-themenames
                      themenames=[name p |p<-pats]++[name p |p<-procs]
    rulenmchk = [ newcxe ("Rules with identical names at positions "++show(map origin cl)++"\nrules:"++concat ["\n\n   "++show rul | rul<-cl])
                | cl<-eqCl name (udefrules contxt),length cl>1]
    ifcnmchk  = [newcxe ("Interfaces with identical names at positions "++show(map origin cl)) -- ++"\ncl:"++concat ["\n\n   "++show ifc | ifc<-cl])
                | cl<-eqCl name ifcs,length cl>1]
    patnmchk  = [newcxe ("Patterns or processes with identical names at positions "++show(map fst cl)) -- ++"\ncl:"++concat ["\n\n   "++show ifc | ifc<-cl])
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
     = f <$> parRuls ppat <*> parKeys ppat <*> parViews ppat <*> parDcls ppat <*> parPrps ppat
       where
        f prules keys' views' decsNpops xpls
         = A_Pat { ptnm  = name ppat
                 , ptpos = pt_pos ppat
                 , ptend = pt_end ppat
                 , ptrls = prules
                 , ptgns = agens'
                 , ptdcs = [d{decpat=name ppat} | (d,_)<-decsNpops]
                 , ptups = catMaybes (map snd decsNpops)
                 , ptids = keys'
                 , ptvds = views'
                 , ptxps = xpls
                 }
        agens'   = map (pGen2aGen (name ppat)) (pt_gns ppat)
        parRuls  = parallelList . map (pRul2aRul (name ppat)) . pt_rls
        parKeys  = parallelList . map pIdentity2aIdentity . pt_ids
        parViews = parallelList . map pViewDef2aViewDef . pt_vds
        parDcls  = parallelList . map pDecl2aDecl . pt_dcs
        parPrps  = parallelList . map pPurp2aPurp . pt_xps

    pProc2aProc :: P_Process -> Guarded Process
    pProc2aProc pproc
     = f <$> parRuls pproc <*> parKeys pproc <*> parViews pproc <*> parDcls pproc <*> parRRels pproc <*> parRRuls pproc <*> parPrps pproc
       where
        f prules keys' views' decsNpops rrels rruls expls
         = Proc { prcNm    = procNm pproc
                , prcPos   = procPos pproc
                , prcEnd   = procEnd pproc
                , prcRules = prules
                , prcGens  = map (pGen2aGen (name pproc)) (procGens pproc)            -- The generalizations defined in this pattern
                , prcDcls  = [d{decpat=name pproc} | (d,_)<-decsNpops]                         -- The declarations declared in this pattern
                , prcUps   = catMaybes (map snd decsNpops)
                , prcRRuls = [(rol,rul) |rul<-udefrules contxt, rr<-rruls, name rul `elem` mRules rr, rol<-mRoles rr] -- The assignment of roles to rules.
                , prcRRels = [(rol,dcl) |rr<-rrels, rol<-rrRoles rr, dcl<-rrRels rr]  -- The assignment of roles to Relations.
                , prcIds   = keys'                                                    -- The identity definitions defined in this process
                , prcVds   = views'                                                   -- The view definitions defined in this process
                , prcXps   = expls                                                    -- The purposes of elements defined in this process
                } 
        parRuls  = parallelList . map (pRul2aRul (name pproc)) . procRules
        parDcls  = parallelList . map pDecl2aDecl . procDcls
        parRRels = parallelList . map pRRel2aRRel . procRRels
        parRRuls = parallelList . map pRRul2aRRul . procRRuls
        parKeys  = parallelList . map pIdentity2aIdentity . procIds
        parViews = parallelList . map pViewDef2aViewDef . procVds
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
                                    EDcD dcl _ -> dcl
                                    _   -> fatal 1149 ("Erroneous expression "++showADL erel++" in pRRel2aRRel.")
                                 | (erel,_,_)<-erels ]
                     , rrPos   = rr_Pos prrel
                     }
        parRels = parallelList . map pExpr2aExpr . rr_Rels
    
    p2aPairView :: P_Concept -> P_Concept -> P_PairView -> Guarded PairView
    p2aPairView srcCpt trgCpt ppv = do { guardedPpvs <- (parallelList . map (p2aPairViewSegment srcCpt trgCpt) . ppv_segs) ppv ; return (PairView guardedPpvs) }

    p2aPairViewSegment :: P_Concept -> P_Concept -> P_PairViewSegment -> Guarded PairViewSegment
    p2aPairViewSegment _      _        (P_PairViewText str)          = Checked (PairViewText str)
    p2aPairViewSegment _      _        (P_PairViewExp srcOrTgt pexp) = do { (aexp,_,_) <- pExpr2aExpr pexp
                                                                          ; case srcOrTgt of
                                                                             Src -> return (PairViewExp srcOrTgt aexp)
                                                                             Tgt -> return (PairViewExp srcOrTgt aexp)
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
                                      , decissX = True
                                      , decusrX = False
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

    -- | pIdentity2aIdentity checks compatibility of composition with identity concept on equality
    pIdentity2aIdentity :: P_IdentDef -> Guarded IdentityDef
    pIdentity2aIdentity identity
     = case typeErrors' of
        [] -> Checked (Id { idPos = ix_pos identity
                          , idLbl = ix_lbl identity
                          , idCpt = c
                          , identityAts = segs
                          })
        _  -> Errors [CxeOrig typeErrors' "identity definition" "" (origin identity) | (not.null) typeErrors']
       where
        typeErrors' = identityCxe++segscxes
        (segs, segscxes) = case (parallelList . map (pKeySeg2aKeySeg (ix_cpt identity)) . ix_ats) identity of
                             Checked segments -> (segments, [])
                             Errors  errs     -> (fatal 1166 ("Do not refer to undefined segments\n"++show errs), errs)
        c  = pCpt2aCpt (ix_cpt identity)
        -- check equality
        ats = [ expr | IdentityExp expr <- segs ]
        identityCxe = newcxeif (null segscxes && length (nub (c:map (source.objctx) ats))/=1)
                         (intercalate "\n" ["The source of term " ++ showADL (objctx x) 
                                            ++" ("++showADL (source (objctx x))++") is compatible, but not equal to the identity concept ("++ showADL c ++ ")."
                                           |x<-ats,source (objctx x)/=c])

    pKeySeg2aKeySeg :: P_Concept -> P_IdentSegment -> Guarded IdentitySegment
    pKeySeg2aKeySeg _ (P_IdentExp pObj) = do { objDef <- pODef2aODef [] pObj
                                           ; return (IdentityExp objDef)
                                           }

    -- | pViewDef2aViewDef checks compatibility of composition with view concept on equality
    pViewDef2aViewDef :: P_ViewDef -> Guarded ViewDef
    pViewDef2aViewDef pvdef
     = case typeErrors' of
        [] -> Checked (Vd { vdpos = vd_pos pvdef
                          , vdlbl = vd_lbl pvdef
                          , vdcpt = c
                          , vdats = segs
                          })
        _  -> Errors [CxeOrig typeErrors' "view definition" "" (origin pvdef) | (not.null) typeErrors']
       where
        typeErrors' = vdcxe++segscxes
        (segs, segscxes) = case (parallelList . map (pViewSeg2aViewSeg (vd_cpt pvdef)) . vd_ats) pvdef of
                             Checked segments -> (segments, [])
                             Errors  errs     -> (fatal 1166 ("Do not refer to undefined segments\n"++show errs), errs)
        c  = pCpt2aCpt (vd_cpt pvdef)
        -- check equality
        ats = [ expr | ViewExp expr <- segs ]
        vdcxe = newcxeif (null segscxes && length (nub (c:map (source.objctx) ats))/=1)
                         (intercalate "\n" ["The source of term " ++ showADL (objctx x) 
                                            ++" ("++showADL (source (objctx x))++") is compatible, but not equal to the view concept ("++ showADL c ++ ")."
                                           |x<-ats,source (objctx x)/=c])

    pViewSeg2aViewSeg :: P_Concept -> P_ViewSegment -> Guarded ViewSegment
    pViewSeg2aViewSeg _ (P_ViewText str) = return (ViewText str)
    pViewSeg2aViewSeg _ (P_ViewHtml str) = return (ViewHtml str)
    pViewSeg2aViewSeg _ (P_ViewExp pObj) = do { objDef <- pODef2aODef [] pObj
                                              ; return (ViewExp objDef)
                                              }

    -- QUESTION -> What is the intention of ifcArgs? ANSWER: see the declaration of P_Interface
    pIFC2aIFC :: P_Interface -> Guarded Interface
    pIFC2aIFC pifc 
     = f <$> parParams pifc <*> (pODef2aODef parentIfcRoles . ifc_Obj) pifc
       where
        f prms obj
         = Ifc { ifcParams = [ case erel of
                                EDcD{} -> erel
                                _ -> fatal 1273 ("Erroneous expression "++showADL erel++" in pIFC2aIFC.")
                             | (erel,_,_)<-prms ]
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
                -> P_ObjectDef           -- the object definition as specified in the parse tree
                -> Guarded ObjectDef     -- result: the type checked object definition (only defined if there are no type errors) and a list of type errors
    pODef2aODef parentIfcRoles podef 
     = do { let oTerm = obj_ctx podef
          ; msub <- p2a_MaybeSubInterface parentIfcRoles (obj_msub podef)
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
    p2a_MaybeSubInterface :: [String] -> Maybe P_SubInterface -> Guarded (Maybe SubInterface)
    p2a_MaybeSubInterface _              Nothing               = return Nothing
    p2a_MaybeSubInterface parentIfcRoles (Just (P_Box p_objs))
     = do { objects <- parallelList [pODef2aODef parentIfcRoles p_obj | p_obj<-p_objs]
          ; return (Just (Box objects))
          }
    p2a_MaybeSubInterface parentIfcRoles (Just (P_InterfaceRef pos nm))
     = do { p_ifc <- case [p_ifc | p_ifc <- ctx_ifcs p_context, name p_ifc == nm ] of
                       [p_ifc] -> return p_ifc
                       ifs     -> Errors [CxeNoIfcs nm pos ifs]
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
    pExOb2aExOb (PRef2IdentityDef str ) = case [identity | identity<-p_keys p_context, name identity==str] of
                                           [] -> Errors [newcxe (" No identity definition named '"++str++"'")]
                                           _  -> Checked (ExplIdentityDef str)
    pExOb2aExOb (PRef2ViewDef str     ) = case [vd | vd<-p_views p_context, name vd==str] of
                                           [] -> Errors [newcxe (" No view definition named '"++str++"'")]
                                           _  -> Checked (ExplViewDef str)
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
        Checked (e,_,_) -> popsOf e
        Errors errs     -> Errors errs
       where
        expr = case pop of
                    P_CptPopu{} -> Pid   (origin pop) (PCpt (name pop))
                    P_RelPopu{} -> Prel  (origin pop) (name pop)
                    P_TRelPop{} -> PTrel (origin pop) (name pop) (p_type pop)
        popsOf e =
          case e of
             EDcD d _   -> Checked (PRelPopu { popdcl = d
                                   , popps  = case pop of
                                                P_RelPopu{} -> p_popps pop
                                                P_TRelPop{} -> p_popps pop
                                                P_CptPopu{} -> fatal 1470 ("Unexpected issue with population of "++name pop) 
                                             })
             EDcI   sgn -> Checked (PCptPopu { popcpt = source sgn
                                             , popas  = case pop of
                                                P_CptPopu{} -> p_popas pop
                                                _ -> fatal 1474 ("Unexpected issue with population of "++name pop)
                                             })
             EDcV   _   -> fatal 1477 "V has no population of it's own"
             ETyp e' _  -> popsOf e'
             _          -> fatal 1223 "illegal call of pPop2aPop"

    pGen2aGen :: String -> P_Gen -> A_Gen
    pGen2aGen patNm pg
       = Gen{genfp  = gen_fp  pg
            ,gengen = pCpt2aCpt (gen_gen pg)
            ,genspc = pCpt2aCpt (gen_spc pg)
            ,genpat = patNm
            }
              
    pSign2aSign :: P_Sign -> Sign
    pSign2aSign (P_Sign src trg) = Sign (pCpt2aCpt src) (pCpt2aCpt trg)
            
    pCpt2aCpt :: P_Concept -> A_Concept
    pCpt2aCpt pc
        = case pc of
            PCpt{} -> 
              PlainConcept 
                {cptnm = p_cptnm pc
                ,cptgE = genE contxt
                ,cpttp = head ([cdtyp cd | cd<-conceptDefs contxt,cdcpt cd==p_cptnm pc]++[""])
                ,cptdf = [cd | cd<-conceptDefs contxt,cdcpt cd==p_cptnm pc]
                } 
            P_Singleton -> ONE
    
    aCpt2pCpt :: A_Concept -> P_Concept
    aCpt2pCpt c
        = case c of
            PlainConcept{} -> PCpt { p_cptnm = cptnm c }
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
                             , decissX  = True
                             , decusrX  = True
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
           PI _            -> return (EDcI $ getSign x)
           Pid _ c         -> return (iExpr (pCpt2aCpt c))
           Patm _ atom _   -> return (EMp1 atom $ getSign x)
           PVee _          -> return (vExpr $ getSign x)
           Pfull _ s t     -> return (vExpr (Sign (pCpt2aCpt s) (pCpt2aCpt t)))
           Prel _ _        -> do { decl <- getDeclaration x
                                 ; return$ EDcD decl (getSign x) }
           Pequ _ a b      -> (.==.) <$> f a <*> f b
           Pimp _ a b      -> (.|-.) <$> f a <*> f b
           PIsc _ a b      -> (./\.) <$> f a <*> f b
           PUni _ a b      -> (.\/.) <$> f a <*> f b
           PDif _ a b      -> (.-.) <$> f a <*> f b
           PLrs _ a b      -> (./.) <$> f a <*> f b
           PRrs _ a b      -> (.\.) <$> f a <*> f b
           PCps _ a b      -> (.:.) <$> f a <*> f b
           PRad _ a b      -> (.!.) <$> f a <*> f b
           PPrd _ a b      -> (.*.) <$> f a <*> f b
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
           PTrel _ _ _
            -> pRel2aRel (srcTypes (dom x)) (srcTypes (cod x))
               where pRel2aRel src trg
                      = do {ar<-aRel;return$ ETyp ar sgn}
                        where aRel  = do {(d,_)<-(pDecl2aDecl decl); return$ EDcD d sgn}
                              sgn   = Sign aSrc aTrg
                              aSrc  = pCpt2aCpt src
                              aTrg  = pCpt2aCpt trg
                              decl  = case Map.lookup x bindings of
                                        Just x' -> x'
                                        Nothing -> fatal 1480 "Map.lookup failed to find binding"
         getSign :: Term -> Sign
         getSign term
          = Sign (pCpt2aCpt (srcTypes (dom term))) (pCpt2aCpt (srcTypes (cod term)))
         lookupType :: Term -> P_Concept
         lookupType t = srcTypes (dom t)
    getDeclaration :: Term -> Guarded Declaration
    getDeclaration term@(Prel _ _)
     = case Map.lookup term bindings of
        Just d  -> do { (decl,_) <- pDecl2aDecl d ; return decl }
        Nothing -> fatal 1601 ("Term "++showADL term++" ("++show(origin term)++") was not found in "++show (length (Map.toAscList bindings))++" bindings.\n"
                              ++ "  A possible cause could be that it isn't harvested. (see function uType)\n"
                              ++  intercalate "\n  " [showADL x++"("++show(origin x)++")" | x<-Map.keys bindings]
                              )
                   --concat ["\n  "++show b | b<-Map.toAscList bindings, take 7 ( tail (show b))==take 7 (show term) ])
    getDeclaration term = fatal 1607 ("Illegal call to getDeclaration ("++show term++")")


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
{- condensedGraph is the condensed graph. Elements that are equal are brought together in the same equivalence class.
   The graph in which equivalence classes are vertices is called the condensed graph.
   These equivalence classes are the strongly connected components of the original graph
-}
     condensedEdges :: [(Int,Int)]
     condensedEdges = nub [ (nr t, nr t') | (t,t')<-flattenMap st, nr t /= nr t' ]
     nr t = case Map.lookup t eqType of
             Just (x:_) -> eqNr x
             _ -> fatal 571 ("Element "++show t++" not found in nr")
     condensedEdges2 = nub [(nr t,nr t') | (t,t')<-flattenMap stClosAdded, nr t /= nr t', t' `notElem` findIn t stClos1]>-condensedEdges

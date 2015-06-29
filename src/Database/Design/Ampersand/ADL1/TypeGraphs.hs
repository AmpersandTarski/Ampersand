{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
{-# LANGUAGE RelaxedPolyRec #-} -- RelaxedPolyRec required for OpenSuse, for as long as we@OpenUniversityNL use an older GHC
module Database.Design.Ampersand.ADL1.TypeGraphs (
     -- * Exported functions
     computeTypeGraphs
     )
where

import Prelude hiding (head)
import GHC.Exts (sortWith)
import Data.Char
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map hiding (Map)
import Data.Maybe
import Data.List hiding (head)

import Data.GraphViz hiding (addExtension, C)
import Data.GraphViz.Attributes.Complete hiding (Box, Pos)

import Database.Design.Ampersand.Core.AbstractSyntaxTree hiding (sortWith, maxima, greatest)
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.Basics (name, isc, uni, eqCl, eqClass, getCycles, (>-), fatalMsg, flp)
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.Misc
-- import DatabaseDesign.Ampersand.Fspec.Fspec
import Database.Design.Ampersand.FSpec.ShowADL
import qualified Database.Design.Ampersand.Core.Poset as Poset hiding (sortWith)
import Database.Design.Ampersand.Input.ADL1.CtxError
import Database.Design.Ampersand.ADL1.TypePropagation

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

-- SignOrd is used for finding declarations efficiently. From SJ to @SJC: I would expect compare (name a,name c) (name b,name d). WHY is compare being used as it is?
newtype SignOrd = SignOrd Sign
instance Ord SignOrd where
  compare (SignOrd (Sign a b)) (SignOrd (Sign c d)) = compare (name a,name b) (name c,name d)
instance Eq SignOrd where
  (==) (SignOrd (Sign a b)) (SignOrd (Sign c d)) = (name a,name b) == (name c,name d)

complement :: Term TermPrim -> Term TermPrim
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



type TypeInfo = (Typemap,[Between])

nothing :: TypeInfo
nothing = (Map.empty,[])
between :: Type -> Between -> TypeInfo
between t a = (Map.fromList [(t,[])],[a])

infixl 2 .+.   -- concatenate two lists of types
infixl 3 .<.   -- makes a list of one tuple (t,t'), meaning that t is a subset of t'
infixl 3 .=.   -- makes a list of two tuples (t,t') and (t',t), meaning that t is equal to t'
typeToMap :: Type -> TypeInfo
typeToMap x = (Map.fromList [(x,[])],[])
(.<.) :: Type -> Type -> TypeInfo
a .<. b  = (Map.fromList [(a, [b]),(b, [])],[]) -- a tuple meaning that a is a subset of b, and introducing b as a key.
(.=.) :: Type -> Type -> TypeInfo
a .=. b  = (Map.fromList [(a, [b]),(b, [a])],[])
(.+.) :: TypeInfo -> TypeInfo -> TypeInfo
(m1,l1) .+. (m2,l2) = (Map.unionWith mrgUnion m1 m2, l1++l2)
thing :: P_Concept -> Type
thing c  = TypExpr (Pid (fatal 90 "clueless about where this is found. Sorry" ) c) Src
dom, cod :: Term TermPrim -> Type
dom x    = TypExpr x Src -- the domain of x
cod x    = TypExpr x Tgt 
domOrCod :: SrcOrTgt -> Term TermPrim -> Type
domOrCod Src = dom
domOrCod Tgt = cod
-- | mSpecific, mGeneric are shorthands for creating links in the type graph. mSpecific is used in unions, whereas mGeneric is used in intersections.
{-
mSpecific doet twee dingen:
1) probeert een Type te geven aan het laatste argument, e.
2) genereert een foutmelding als 1) mislukt.
   De markering BTUnion (dan wel BTIntersect) betekent dat er getest moet worden (in TypePropagation.adl) of het UNION-type bestaat.
   De feitelijke test wordt uitgevoerd in checkBetweens (in TypePropagation.adl)
mSpecific' gebruik je wanneer je in de aanroep niet weet of je de source of target bedoelt, bijv. "intersection arising inside r;s" 
-}
mSpecific, mGeneric :: SrcOrTgt -> Term TermPrim -> SrcOrTgt -> Term TermPrim -> SrcOrTgt -> Term -> TypeInfo
mGeneric   ta a tb b te e = (domOrCod ta a) .<. (domOrCod te e) .+. (domOrCod tb b) .<. (domOrCod te e) .+. between (domOrCod te e) (Between (tCxe ta a tb b TETUnion e) (domOrCod ta a) (domOrCod tb b) (BetweenType BTUnion (domOrCod te e)))
mSpecific  ta a tb b te e = (domOrCod te e) .<. (domOrCod ta a) .+. (domOrCod te e) .<. (domOrCod tb b) .+. between (domOrCod te e) (Between (tCxe ta a tb b TETIsc   e) (domOrCod ta a) (domOrCod tb b) (BetweenType BTIntersection (domOrCod te e)))
mSpecific', mGeneric' :: SrcOrTgt -> Term TermPrim -> SrcOrTgt -> Term TermPrim -> Term TermPrim -> TypeInfo
mGeneric'   ta a tb b e = (domOrCod ta a) .<. (TypInCps  e) .+. (domOrCod tb b) .<. (TypInCps  e) .+. between (TypInCps e) (Between (tCxe ta a tb b TETUnion e) (domOrCod ta a) (domOrCod tb b) (BetweenType BTUnion (TypInCps e)))
mSpecific'  ta a tb b e = (TypInCps  e) .<. (domOrCod ta a) .+. (TypInCps  e) .<. (domOrCod tb b) .+. between (TypInCps e) (Between (tCxe ta a tb b TETIsc   e) (domOrCod ta a) (domOrCod tb b) (BetweenType BTIntersection (TypInCps e)))
mEqual' :: SrcOrTgt -> Term TermPrim -> Term TermPrim -> Term TermPrim -> TypeInfo
mEqual'    sORt a b e = (Map.empty, [Between (tCxe sORt a sORt b TETEq e) (domOrCod sORt a) (domOrCod sORt b) BTEqual])
existsSpecific :: Type -> Type -> ([P_Concept] -> [P_Concept] -> CtxError) -> Type -> TypeInfo
existsSpecific = existsGS BTIntersection
existsGS :: BTUOrI -> Type -> Type -> ([P_Concept] -> [P_Concept] -> CtxError) -> Type -> TypeInfo
existsGS BTIntersection a b err at = at .<. a .+. at .<. b .+. between at (Between err a b (BetweenType BTIntersection at))
existsGS BTUnion        a b err at = a .<. at .+. b .<. at .+. between at (Between err a b (BetweenType BTUnion        at))
tCxe :: SrcOrTgt -> Term TermPrim -> SrcOrTgt -> Term TermPrim -> (t -> TypErrTyp) -> t -> [P_Concept] -> [P_Concept] -> CtxError
tCxe ta a tb b msg e src trg = CxeBetween{cxeLhs=(a,ta,src),cxeRhs=(b,tb,trg),cxeTyp=msg e}

flattenMap :: Map t [t1] -> [(t, t1)]
flattenMap = Map.foldWithKey (\s ts o -> o ++ [(s,t)|t<-ts]) []
-- alternatively: flattenMap mp = [ (a,b) | (a,bs)<-Map.toList mp , b<-bs])

--Auxiliaries iExpr and vExpr (speaking for themselves)
iExpr :: A_Concept -> Expression
iExpr  c = EDcI (Sign c c)
vExpr :: Sign -> Expression
vExpr sgn = EDcV sgn

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
  --   the tuples of a relation st :: Type * Type. Each Type represents a set of atoms, even though the type checker will only use the fact that a type represents a set.
  --   Let t, t' be types, then    (t, t') `elem` st    means that the set that t represents is a subset of the set that t' represents.
  --   These tuples are produced in two TypeInfos. The second TypeInfo is kept separate, because it depends on the existence of the first TypeInfo.
  --   The first element of the first argument is a P_Context that represents the parse tree of one context.
  --   This is provided to obtain a declaration table and a list of interfaces from the script.
  --   The second element of the first argument is a compatibility function, that determines whether two types are compatible.
  uType :: a           -- x:    the original term from the script, meant for representation in the graph.
        -> a           -- z:    the term to be analyzed, which must be logically equivalent to x
        -> TypeInfo   -- for each type, a list of types that are subsets of it, which is the result of analysing term x.
  uType' :: a -> TypeInfo
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
    (Map.empty, [ Between (\s t -> CxeObjMismatch{cxeExpr=trm,cxeEnv=s,cxeSrcs=t})
                          (TypExpr (rr_exp r) sOrT) (TypExpr trm Src)
                          BTEqual
                | Just pv <- [rr_viol r]
                , P_PairViewExp sOrT trm <- ppv_segs pv
                ])

instance Expr P_PairView where
 uType _ (P_PairView segments) = uType segments segments

instance Expr P_PairViewSegment where
 uType _ (P_PairViewExp Src term) = uType term term
 uType _ (P_PairViewExp Tgt term) = uType term term
 uType _ P_PairViewText{} = nothing
  
instance Expr P_IdentDef where
 p_keys k = [k]
 uType _ k
  = let x=Pid (ix_pos k) (ix_cpt k) in
    uType x x .+. 
    foldr (.+.) nothing [ dom (obj_ctx obj) .<. dom x .+. uType obj obj
                        | P_IdentExp obj <- ix_ats k
                        ]

instance Expr P_ViewDef where
 p_views v = [v]
 uType _ v
  = let x=Pid (vd_pos v) (vd_cpt v) in
    uType x x .+. 
    foldr (.+.) nothing [ dom (obj_ctx obj) .<. dom x .+. uType obj obj
                        | P_ViewExp obj <- vd_ats v
                        ]
 
-- TODO: continue adding errors until you reach instance Expr (Term TermPrim)
instance Expr P_Interface where
 uType _ ifc
  = let x=ifc_Obj ifc in
    foldr (.+.) nothing (map uType' (ifc_Params ifc)) .+.
    uType x x

instance Expr P_ObjectDef where
 uType _ o
  = let x=obj_ctx o in
    uType x x .+. 
    foldr (.+.) nothing [ uType obj obj .+.
                          existsSpecific (cod x) (dom (obj_ctx obj)) (tCxe Tgt x Src (obj_ctx obj) TETBox (obj_ctx obj)) (TypInObjDef obj)
                        | Just subIfc <- [obj_msub o]
                        , obj <- case subIfc of
                                   P_Box{}          -> si_box subIfc
                                   P_InterfaceRef{} -> []
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
     PRef2ConceptDef str -> uType' (Pid (fatal 279 "clueless about where this is found. Sorry") (PCpt str))
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
    where decl = Prim (PNamedRel (origin d) (dec_nm d) (Just (dec_sign d)))
          P_Sign src trg = dec_sign d

instance Expr P_Population where
 uType _ pop
  = uType x x .+. dom x.=.dom x .+. cod x.=.cod x
    where x = Prim $
              case pop of
                   P_RelPopu{} -> PNamedRel (p_orig pop) (name pop) Nothing
                   P_TRelPop{} -> PNamedRel (p_orig pop) (name pop) (Just (p_type pop))
                   P_CptPopu{} -> Pid       (p_orig pop) (PCpt (name pop))

instance Expr a => Expr (Maybe a) where
 uType _ Nothing  = nothing
 uType _ (Just x) = uType' x

instance Expr a => Expr [a] where
 uType _ xs = foldr (.+.) nothing (map uType' xs)

instance TermPrim where 
 uType x term 
  = case term of
           PI _                   -> dom x.=.cod x                                    -- I
           Pid _ _                -> dom x.=.cod x                                    -- I[C]
           Patm _ s Nothing       -> dom x.=.cod x                                    -- 'Piet'   (an untyped singleton)
           Patm _ s (Just conspt) -> dom x.<.thing conspt .+. cod x.<.thing conspt    -- 'Piet'[Persoon]  (a typed singleton)
           PVee _                 -> typeToMap (dom x) .+. typeToMap (cod x)          -- V
           Pfull o s t            -> dom x.<.dom (Pid o s) .+. cod x.<.cod (Pid o t)  -- V[A*B]
           PNamedR nr             -> uType x nr

instance P_NamedRel where 
 uType x rel 
  = case rel of
      PNamedRel _ _ (Just s) -> dom x.<.thing src .+. cod x.<.thing trg where Sign src trg = pSign2aSign s
      PNamedRel _ _ Nothing  -> typeToMap (dom x) .+. typeToMap (cod x)

instance Expr (Term TermPrim) where 
 uType x term 
  = case term of
     (Prim prTerm) -> uType x prTerm
     (Pfull o s t) -> dom x.<.dom (Pid o s) .+. cod x.<.cod (Pid o t)              --  V[A*B] (the typed full set)
     (Pequ _ a b)  -> dom a.=.dom b .+. cod a.=.cod b .+. dom b.=.dom x .+. cod b.=.cod x    --  a=b    equality
                      .+. mEqual' Src a b x .+. mEqual' Tgt a b x
                      .+. uType a a .+. uType b b
     (PIsc _ a b)  -> dom x.<.dom a .+. dom x.<.dom b .+. cod x.<.cod a .+. cod x.<.cod b
                      .+. mSpecific Src a Src b Src x .+. mSpecific Tgt a Tgt b Tgt x
                      .+. uType a a .+. uType b b
     (PUni _ a b)  -> dom a.<.dom x .+. dom b.<.dom x .+. cod a.<.cod x .+. cod b.<.cod x
                      .+. mGeneric Src a Src b Src x .+. mGeneric Tgt a Tgt b Tgt x
                      .+. uType a a .+. uType b b
     (PDif o a b)  -> dom x.<.dom a .+. cod x.<.cod a  --  a-b    (difference)
                      .+. mGeneric Src x Src b Src (PUni o a b) .+. mGeneric Tgt x Tgt b Tgt (PUni o a b)
                      .+. uType a a .+. uType b b
     (PCps _ a b)  -> let s = TypInCps x
                          pidTest (PI{}) r = r
                          pidTest (Pid{}) r = r
                          pidTest (Patm{}) r = r
                          pidTest _ _ = nothing
                      in dom x.<.dom a .+. cod x.<.cod b .+.                                    -- a;b      composition
                         mSpecific' Tgt a Src b x .+. uType a a .+. uType b b
                         .+. pidTest a (dom x.=.s) .+. pidTest b (cod x.=.s)
-- PRad is the De Morgan dual of PCps. However, since PUni and UIsc are treated separately, mGeneric and mSpecific are not derived, hence PRad cannot be derived either
     (PRad _ a b) -> let pnidTest (PCpl _ (PI{})) r = r
                         pnidTest (PCpl _ (Pid{})) r = r
                         pnidTest (PCpl _ (Patm{})) r = r
                         pnidTest _ _ = nothing
                         s = TypInCps x
                     in dom x .<. dom a .+. cod x .<. cod b
                        .+. mGeneric' Tgt a Src b x .+. uType a a .+. uType b b
                        .+. pnidTest a (dom b.<. s) .+. pnidTest b (cod a.<. s)
     (PPrd _ a b) -> dom x.=.dom a .+. cod x.=.cod b                                        -- a*b cartesian product
                     .+. uType a a .+. uType b b
     (PKl0 _ e)   -> dom e.<.dom x .+. cod e.<.cod x .+. uType e e
     (PKl1 _ e)   -> dom e.<.dom x .+. cod e.<.cod x .+. uType e e
     (PFlp _ e)   -> cod e.=.dom x .+. dom e.=.cod x .+. uType e e
     (PBrk _ e)   -> dom x.=.dom e .+. cod x.=.cod e .+. uType x e  -- (e) brackets
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
computeTypeGraphs :: P_Context -> (DotGraph String, DotGraph String)
computeTypeGraphs p_context
 = (stTypeGraph, eqTypeGraph)
   where
    contxt = 
         ACtx{ ctxnm     = name p_context
             , ctxpos    = ctx_pos p_context
             , ctxlang   = fromMaybe Dutch (ctx_lang p_context)
             , ctxmarkup = fromMaybe ReST  (ctx_markup p_context)
             , ctxpo     = gEandClasses
             , ctxthms   = ctx_thms p_context
             , ctxpats   = p_patterns
             , ctxprocs  = procs
             , ctxrs     = ctxrules
             , ctxds     = decls
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
        popsfromdecls = concatMap ptups p_patterns    -- Populations from declarations inside all patterns
                     ++ concatMap prcUps procs  -- Populations from declarations inside all processes
                      
    st, eqType :: Typemap                  -- eqType = (st*/\st*~)\/I  (total, reflexive, symmetric and transitive)
    bindings :: Map (Term TermPrim) P_Declaration         -- yields declarations that may be bound to relations, intended as a suggestion to the programmer
    isaClos, isaClosReversed :: Map P_Concept [P_Concept]                   -- 
    (st, stClos, eqType, stClosAdded, stClos1 , bindingsandsrcTypes, isaClos, isaClosReversed)
     = typing utypeST betweens
              (Map.fromListWith mrgUnion [ (name (head cl)
                                           , uniqueCl cl)
                                         | cl<-eqCl name (p_declarations p_context) ])
    (utypeST,betweens) = uType p_context p_context
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
    (decls,   deccxes)   = case (parallelList . map pDecl2aDecl           . ctx_ds   ) p_context of
                              Checked decs -> ([d{decpat="NoPattern"} | d<-decs], [])
                              Errors  errs -> (fatal 1030 ("Do not refer to undefined declarations\n"++show errs), errs)
    (apurp,   xplcxes)   = case (parallelList . map  pPurp2aPurp            . ctx_ps   ) p_context of
                            Checked purps -> (purps, [])
                            Errors  errs  -> (fatal 1033 ("Do not refer to undefined purposes\n"++show errs), errs)
    (p_patterns,patcxes) = case (parallelList . map pPat2aPat               . ctx_pats ) p_context of
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
        allMp1Rels =    mp1Exprs p_patterns
                  `uni` mp1Exprs procs
                  `uni` mp1Exprs ctxrules
                  `uni` mp1Exprs keys
                  `uni` mp1Exprs views
                  `uni` mp1Exprs ifcs 
             
    themeschk = case orphans of
                 []   -> []
                 [nm] -> [newcxe ("Theme '"++nm++"' is selected for output, but is not defined.")]
                 _    -> [newcxe ("The following themes are selected for output, but are not defined:\n   "++intercalate ", " orphans)]
                where orphans = ctx_thms p_context >- themenames
                      themenames=[name p |p<-p_patterns]++[name p |p<-procs]
    rulenmchk = [ newcxe ("Rules with identical names at positions "++show(map origin cl)++"\nrules:"++concat ["\n\n   "++show rul | rul<-cl])
                | cl<-eqCl name (udefrules contxt),length cl>1]
    ifcnmchk  = [newcxe ("Interfaces with identical names at positions "++show(map origin cl)) -- ++"\ncl:"++concat ["\n\n   "++show ifc | ifc<-cl])
                | cl<-eqCl name ifcs,length cl>1]
    patnmchk  = [newcxe ("Patterns or processes with identical names at positions "++show(map fst cl)) -- ++"\ncl:"++concat ["\n\n   "++show ifc | ifc<-cl])
                | cl<-eqCl snd (zip (map origin p_patterns++map origin procs)
                                    (map name   p_patterns++map name   procs)),length cl>1]
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
     = f <$> parRuls ppat <*> parKeys ppat <*> parViews ppat <*> parDcls ppat <*> parRRels ppat <*> parRRuls ppat <*> parPrps ppat
       where
        f prules keys' views' decsNpops rrels rruls xpls
         = A_Pat { ptnm  = name ppat
                 , ptpos = pt_pos ppat
                 , ptend = pt_end ppat
                 , ptrls = prules
                 , ptgns = agens'
                 , ptdcs = [d{decpat=name ppat} | (d,_)<-decsNpops]
                 , ptups = catMaybes (map snd decsNpops)
                 , ptrruls = [(rol,rul) |rul<-udefrules contxt, rr<-rruls, name rul `elem` mRules rr, rol<-mRoles rr] -- The assignment of roles to rules.
                 , ptrrels = [(rol,dcl) |rr<-rrels, rol<-rrRoles rr, dcl<-rrRels rr]  -- The assignment of roles to Relations.
                 , ptids = keys'
                 , ptvds = views'
                 , ptxps = xpls
                 }
        agens'   = map (pGen2aGen (name ppat)) (pt_gns ppat)
        parRuls  = parallelList . map (pRul2aRul (name ppat)) . pt_rls
        parRRels = parallelList . map pRRel2aRRel . pt_res
        parRRuls = parallelList . map pRRul2aRRul . pt_rus
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
                                      , decISA = False
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
                         (intercalate "\n" [message att |att<-ats, (source (objctx att) `Poset.compare` c) `elem` [Poset.LT, Poset.CP, Poset.NC]])
        message x
         = case source (objctx x) `Poset.compare` c of
            Poset.LT -> "   The source of " ++ showADL (objctx x)++" is "++showADL (source (objctx x))++"."
                        ++"\n   Each instance of "++name (source (objctx x))++" is a "++ name c ++ "."
                        ++"\n   Your VIEW definition does not specify how to view instances of "++ name c ++ " that are not "++name (source (objctx x))++"."
                        ++"\n   In order to view every "++ name c ++ ", you must replace " ++ showADL (objctx x)++" by an expression whose source concept is equal to or more generic than "++ name c ++ "."
            Poset.CP -> "   The source of " ++ showADL (objctx x)++" is "++name (source (objctx x))++"."
                        ++"\n   Your VIEW definition does not specify how to view instances of "++ name c ++ " that are not "++showADL (source (objctx x))++"."
                        ++"\n   In order to view every "++ name c ++ ", you must replace " ++ showADL (objctx x)++" by an expression whose source concept is equal to or more generic than "++ name c ++ "."
            Poset.NC -> "   The source of " ++ showADL (objctx x)++" is "++name (source (objctx x))++","
                        ++"   which is incompatible with "++ name c++"."
                        ++"\nYou must replace " ++ showADL (objctx x)++" by an expression whose source concept is equal to "++name c++" or more generic."
            _        -> fatal 850 ("This code should be unreachable, because the source of " ++ showADL (objctx x)++", which is "++name (source (objctx x))++", is equal to or more generic than "++name c++".")

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
                                ETyp EDcD{} _ -> erel
                                _ -> fatal 1273 ("Erroneous expression "++show erel++" in pIFC2aIFC.")
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
          ; (expr, _, t) <- pExpr2aExpr oTerm
                -- A name check ensures that all attributes have unique names
          ; _ <- case [ cl | Just (P_Box objs)<-[obj_msub podef], cl<-eqCl name objs, length cl>1] of
                   []  -> return []
                   cls -> Errors [CxeEqAttribs (origin podef) (name (head cl)) (map obj_ctx cl) | cl<-cls ]
          ; msub <- p2a_MaybeSubInterface parentIfcRoles (obj_msub podef)
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
                       ifs     -> Errors []
          ; thisIfcRoles <- case ifc_Roles p_ifc of
                             [] -> Errors []
                             rs -> return rs
          ; case parentIfcRoles \\ thisIfcRoles of
              [] -> return (Just (InterfaceRef nm))
              rs -> Errors []
          }
      
    pPurp2aPurp :: PPurpose -> Guarded Purpose
    pPurp2aPurp PRef2 { pexPos    = orig     -- :: Origin
                      , pexObj    = objref   -- :: PRefObj
                      , pexMarkup = pmarkup  -- :: P_Markup
                      , pexRefIDs  = refIds  -- :: [String]
                      }
     = (\ obj -> Expl { explPos      = orig
                      , explObj      = obj
                      , explMarkup   = pMarkup2aMarkup
                                         (fromMaybe Dutch (ctx_lang p_context))
                                         (fromMaybe ReST  (ctx_markup p_context))
                                         pmarkup
                      , explUserdefd = True
                      , explRefIds   = refIds
                      })
       <$> pRefObj2aRefObj objref

    pRefObj2aRefObj :: PRef2Obj -> Guarded ExplObj
    pRefObj2aRefObj (PRef2Declaration tm) = ExplDeclaration <$> (namedRel2Decl tm)
    pRefObj2aRefObj (PRef2Rule        s ) = pure$ ExplRule s
    pRefObj2aRefObj (PRef2IdentityDef s ) = pure$ ExplIdentityDef s
    pRefObj2aRefObj (PRef2ViewDef     s ) = pure$ ExplViewDef s
    pRefObj2aRefObj (PRef2Pattern     s ) = pure$ ExplPattern s
    pRefObj2aRefObj (PRef2Interface   s ) = pure$ ExplInterface s
    pRefObj2aRefObj (PRef2Context     s ) = pure$ ExplContext s
    pRefObj2aRefObj (PRef2Fspc        s ) = pure$ ExplContext s
    pRefObj2aRefObj (PRef2ConceptDef  s ) = pure$ ExplConceptDef (lookupConceptDef s)
     where
      lookupConceptDef :: String -> ConceptDef
      lookupConceptDef s
       = case filter (\cd -> name cd == s) allConceptDefs of
           []    -> Cd{cdpos=OriginUnknown, cdcpt=s, cdplug=True, cddef="", cdtyp="", cdref="", cdfrom=""} 
           (x:_) -> x
      allConceptDefs :: [ConceptDef]
      allConceptDefs = {-p_conceptdefs++ -} concatMap pt_cds p_patterns

    namedRel2Decl :: P_NamedRel -> Guarded Declaration
    namedRel2Decl o@(PNamedRel _ r Nothing)  = getOneExactly o [ dc | dc <- (Map.elems $ findDecls r)]
    namedRel2Decl o@(PNamedRel _ r (Just s)) = getOneExactly o [ dc | dc <- (findDeclsTyped r (pSign2aSign s))]

{-
    allRoleRules :: [A_RoleRule]
    allRoleRules = map pRoleRule2aRoleRule
                      (p_roleRules ++ concatMap pt_RRuls p_patterns)
-}
    castConcept :: String -> A_Concept
    castConcept "ONE" = ONE
    castConcept x     = PlainConcept { cptnm = x }

    pPop2aPop :: P_Population -> Guarded Population
    pPop2aPop P_CptPopu { p_cnme = cnm, p_popas = ps }
     = pure PCptPopu{ popcpt = castConcept cnm, popas = ps }
    pPop2aPop orig@(P_RelPopu { p_rnme = rnm, p_popps = ps })
     = fmap (\dcl -> PRelPopu { popdcl = dcl, popps = ps})
            (findDecl orig rnm)
    pPop2aPop orig@(P_TRelPop { p_rnme = rnm, p_type = tp, p_popps = ps })
     = fmap (\dcl -> PRelPopu { popdcl = dcl, popps = ps})
            (findDeclTyped orig rnm (pSign2aSign tp))

    pGen2aGen :: P_Gen -> A_Gen
    pGen2aGen pg@PGen{}
       = Isa{gengen = pCpt2aCpt (gen_gen pg)
            ,genspc = pCpt2aCpt (gen_spc pg)
            }
    pGen2aGen pg@P_Cy{}
       = IsE { genrhs = map pCpt2aCpt (gen_rhs pg)
             , genspc = pCpt2aCpt (gen_spc pg)
             }
              
    pSign2aSign :: P_Sign -> Sign
    pSign2aSign (P_Sign src trg) = Sign (pCpt2aCpt src) (pCpt2aCpt trg)
            
    findConcept :: String -> A_Concept
    -- SJC: ONE should be tokenized, so it cannot occur as a string
    -- especially because we require that concepts are identifiable by their name
    -- hence if this line would change the semantics, we have either
    -- (1) made a programming error in the call of findConcept (in which case you should call findConceptOrONE instead)
    -- (2) made an error in the tokenizer/parser
    findConcept "ONE" = fatal 200 "ONE is not a valid name for a concept"
    findConcept x = PlainConcept { cptnm = x }

    pCpt2aCpt :: P_Concept -> A_Concept
    pCpt2aCpt pc
       = case pc of
           PCpt{} -> findConcept (p_cptnm pc)
           P_Singleton -> ONE

    aCpt2pCpt :: A_Concept -> P_Concept
    aCpt2pCpt c
        = case c of
            PlainConcept{} -> PCpt { p_cptnm = cptnm c }
            ONE -> P_Singleton
    
-- In order to find declarations efficiently, a Map is constructed to search declarations by name.
    declMap = Map.map groupOnTp (Map.fromListWith (++) [(name d,[d]) | d <- decls])
      where groupOnTp lst = Map.fromListWith accumDecl [(SignOrd$ sign d,d) | d <- lst]
    findDecls x = Map.findWithDefault Map.empty x declMap  -- get all declarations with the same name as x
    findDecl o x = (getOneExactly o . Map.elems . findDecls) x
    findDeclsTyped x tp = Map.findWithDefault [] (SignOrd tp) (Map.map (:[]) (findDecls x))
    findDeclTyped o x tp = getOneExactly o (findDeclsTyped x tp)
    -- accumDecl is the function that combines two relations into one
    -- meanings, for instance, two should get combined into a list of meanings, et cetera
    -- positions are combined
    -- TODO
    accumDecl :: Declaration -> Declaration -> Declaration
    accumDecl a _ = a

    pDecl2aDecl :: P_Declaration -> Guarded (Declaration {- , Maybe UserDefPop-})
    pDecl2aDecl pd =
     {- SJ: Is the following obsolete? I think it is, but I'm not sure... (commented on June 28th, 2015 when rebuilding type graphs)
     case dec_conceptDef pd of
          Just (RelConceptDef srcOrTgt _) | relConceptName (dec_nm pd) `elem` map name (concs contxt)
            -> Errors [CxeOrig [newcxe ("Illegal DEFINE "++showSRCorTGT++" for relation "++show (dec_nm pd)++". Concept "++
                                        relConceptName (dec_nm pd)++" already exists.")]
                               "declaration" "" (origin pd)]
               where showSRCorTGT = if srcOrTgt == Src then "SRC" else "TGT"
                     relConceptName ""     = fatal 472 "empty concept"
                     relConceptName (c:cs) = toUpper c : cs
          _ -> -}
               Checked ( d,mp )
               where d = Sgn { decnm   = dec_nm pd
                             , decsgn  = pSign2aSign (dec_sign pd)
                             , decprps = dec_prps pd
                             , decprps_calc = Nothing  --decprps_calc in an A_Context are still the user-defined only. prps are calculated in adl2fspec.
                             , decprL  = decprL pd
                             , decprM  = decprM pd
                             , decprR  = decprR pd
                             , decMean = pMeanings2aMeaning (ctxlang contxt) (ctxmarkup contxt) (dec_Mean pd)
                             , decfpos = dec_fpos pd 
                             , deciss  = True
                             , decusr  = True
                             , decpat  = ""
                             , decplug = dec_plug pd
                             } 
                     mp = case dec_popu pd of
                            [] -> Nothing
                            ps -> Just (PRelPopu {popdcl = d, popps  = ps}) 
                       

    pExpr2aExpr :: Term TermPrim                               -- The term to be typed
                -> Guarded ( Expression               -- the resulting expression.
                           , P_Concept, P_Concept     -- the source and target types of the resulting expression.
                           )                          -- The result might be incorrect, so it is guarded with type error messages.
    pExpr2aExpr pTerm
     = do { r <- f pTerm
          ; return (r, lookupType pTerm, lookupType (p_flp pTerm))
          }
       where
         fTermPrim :: TermPrim -> Guarded Expression
         fTermPrim x = case x of
           PI _             -> return (EDcI $ getSign x)
           Pid _ c          -> return (iExpr (pCpt2aCpt c))
           Patm _ atom _    -> return (EMp1 atom $ getSign x)
           PVee _           -> return (vExpr $ getSign x)
           Pfull _ s t      -> return (vExpr (Sign (pCpt2aCpt s) (pCpt2aCpt t)))
           PNamedR namedrel -> do { decl <- getDeclaration x
                                  ; return$ EDcD decl (getSign x)
                                  }

         f :: Term TermPrim -> Guarded Expression
         f x = case x of
           Prim _ a        -> fTermPrim a
           PEqu _ a b      -> (.==.) <$> f a <*> f b
           PImp _ a b      -> (.|-.) <$> f a <*> f b
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

    getSign :: Term TermPrim -> Sign
    getSign term
     = Sign (pCpt2aCpt (srcTypes (dom term))) (pCpt2aCpt (srcTypes (cod term)))
    lookupType :: Term TermPrim -> P_Concept
    lookupType t = srcTypes (dom t)
    getDeclaration :: Term TermPrim -> Guarded Declaration
    getDeclaration term@(Prim (PNamedRel _ _ _))
     = case Map.lookup term bindings of
        Just d  -> do { decl <- pDecl2aDecl d ; return decl }
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

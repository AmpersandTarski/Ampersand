{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TupleSections #-}

module Ampersand.ADL1.DisambNew
where


import Ampersand.Basics
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Core.ParseTree
import Ampersand.Input.ADL1.CtxError
import Algebra.Graph.AdjacencyMap as Graph
-- import Algebra.Graph.AdjacencyMap.Algorithm
-- import Control.Arrow
import qualified RIO.NonEmpty as NE
-- import qualified RIO.NonEmpty.Partial as PARTIAL
import qualified RIO.List as L
import qualified RIO.Set as Set
-- import Text.PrettyPrint.Leijen (Pretty (..), text)


-- data DisambPrim
--   = Rel [Dxpression] -- It is an expression, we don't know which, but it's going to be one of these (usually this is a list of relations)
--   | Ident -- identity, and we know nothing about its type
--   | Vee -- vee, type unknown
--   | Mp1 PAtomValue -- a singleton atomvalue, type unknown
--   | BinOper PBinOp -- a binary operator, type unknown
--   | Known Dxpression -- It is an expression, and we know exactly which. That is: disambiguation was succesful here
--   deriving (Show) -- Here, deriving Show serves debugging purposes only.

makeGraph :: [PClassify] -> AdjacencyMap P_Concept
makeGraph conceptPairs = Graph.overlays [vertex (specific gn) `Graph.connect` vertex c | gn <- conceptPairs, c <- NE.toList (generics gn)]
synonym :: Ord a => AdjacencyMap a -> a -> a -> Bool
synonym g a b = a==b || (hasEdge a b g && hasEdge b a g)


-- Compute the least upper bound (lub) of a list of pairs
lub :: (Ord a, Show a) =>AdjacencyMap a -> a -> a -> Maybe a
lub conceptGraph a b
    | hasEdge a b rel = Just b
    | hasEdge b a rel = Just a
    | otherwise =
       case Set.toList (postSet a rel `Set.intersection` postSet b rel) L.\\ [a,b] of
        [] -> Nothing
        x:xs -> Just (L.foldr minimum x xs) -- find the minimum of the upper bounds
    where
      minimum a b = if hasEdge a b rel then a else b
      rel = reflexiveClosure (transitiveClosure conceptGraph)

-- Compute the greatest lower bound (glb) of a list of pairs
glb :: (Ord a, Show a) =>AdjacencyMap a -> a -> a -> Maybe a
glb conceptGraph a b
    | hasEdge a b rel = Just a
    | hasEdge b a rel = Just b
    | otherwise =
       case Set.toList (preSet a rel `Set.intersection` preSet b rel) L.\\ [a,b] of
        [] -> Nothing
        x:xs -> Just (L.foldr maximum x xs) -- find the minimum of the upper bounds
    where
      maximum a b = if hasEdge a b rel then b else a
      rel = reflexiveClosure (transitiveClosure conceptGraph)

{- Here is some test output for the lub and glb functions, applied on the following graph:
edges [("even","int"),("float","num"),("int","num"),("integer","even"),("integer","oneven"),("num","gegeven"),("oneven","int")]

"even" `lub` "even" = Just "even"   and   "even" `glb` "even" = Just "even"
The lub intersection set for "even" and "float" is: ["gegeven","num"]
"even" `lub` "float" = Just "num"   and   "even" `glb` "float" = Nothing
"even" `lub` "gegeven" = Just "gegeven"   and   "even" `glb` "gegeven" = Just "even"
"even" `lub` "int" = Just "int"   and   "even" `glb` "int" = Just "even"
"even" `lub` "integer" = Just "even"   and   "even" `glb` "integer" = Just "integer"
"even" `lub` "num" = Just "num"   and   "even" `glb` "num" = Just "even"
The lub intersection set for "even" and "oneven" is: ["gegeven","int","num"]
"even" `lub` "oneven" = Just "int"   and   "even" `glb` "oneven" = Just "integer"
The lub intersection set for "float" and "even" is: ["gegeven","num"]
"float" `lub` "even" = Just "num"   and   "float" `glb` "even" = Nothing
The lub intersection set for "float" and "int" is: ["gegeven","num"]
"float" `lub` "int" = Just "num"   and   "float" `glb` "int" = Nothing
The lub intersection set for "float" and "integer" is: ["gegeven","num"]
"float" `lub` "integer" = Just "num"   and   "float" `glb` "integer" = Nothing
"float" `lub` "num" = Just "num"   and   "float" `glb` "num" = Just "float"
The lub intersection set for "float" and "oneven" is: ["gegeven","num"]
"float" `lub` "oneven" = Just "num"   and   "float" `glb` "oneven" = Nothing
"gegeven" `lub` "even" = Just "gegeven"   and   "gegeven" `glb` "even" = Just "even"
"int" `lub` "even" = Just "int"   and   "int" `glb` "even" = Just "even"
The lub intersection set for "int" and "float" is: ["gegeven","num"]
"int" `lub` "float" = Just "num"   and   "int" `glb` "float" = Nothing
"integer" `lub` "even" = Just "even"   and   "integer" `glb` "even" = Just "integer"
The lub intersection set for "integer" and "float" is: ["gegeven","num"]
"integer" `lub` "float" = Just "num"   and   "integer" `glb` "float" = Nothing
"num" `lub` "oneven" = Just "num"   and   "num" `glb` "oneven" = Just "oneven"
The lub intersection set for "oneven" and "even" is: ["gegeven","int","num"]
"oneven" `lub` "even" = Just "int"   and   "oneven" `glb` "even" = Just "integer"
The lub intersection set for "oneven" and "float" is: ["gegeven","num"]
"oneven" `lub` "float" = Just "num"   and   "oneven" `glb` "float" = Nothing
"oneven" `lub` "gegeven" = Just "gegeven"   and   "oneven" `glb` "gegeven" = Just "oneven"
"oneven" `lub` "oneven" = Just "oneven"   and   "oneven" `glb` "oneven" = Just "oneven"
-}

data Dxpression
  = -- | equivalence             =
    DEqu !(Dxpression, Dxpression)
  | -- | inclusion               |-
    DInc !(Dxpression, Dxpression)
  | -- | intersection            /\
    DIsc !(Dxpression, Dxpression)
  | -- | union                   \/
    DUni !(Dxpression, Dxpression)
  | -- | difference              -
    DDif !(Dxpression, Dxpression)
  | -- | left residual           /
    DLrs !(Dxpression, Dxpression)
  | -- | right residual          \
    DRrs !(Dxpression, Dxpression)
  | -- | diamond                 <>
    DDia !(Dxpression, Dxpression)
  | -- | composition             ;
    DCps !(Dxpression, Dxpression)
  | -- | relative addition       !
    DRad !(Dxpression, Dxpression)
  | -- | cartesian product       *
    DPrd !(Dxpression, Dxpression)
  | -- | Rfx.Trn closure         *  (Kleene star)
    DKl0 !Dxpression
  | -- | Transitive closure      +  (Kleene plus)
    DKl1 !Dxpression
  | -- | conversion (flip, wok)  ~
    DFlp !Dxpression
  | -- | Complement
    DCpl !Dxpression
  | -- | simple relation
    DDcD !Relation
  | -- | Identity relation
    DDcI !A_Concept
  | -- | Epsilon relation (introduced by the system to ensure we compare concepts by equality only.
    DBin !PBinOp !A_Concept
  | -- | Signed expression, to disambiguate
    DSgn !Dxpression !Signature
  | -- | Signed expression, to disambiguate
    DSet !(Set.Set Dxpression)
  | -- | Full relation
    DDcV !Signature
  | -- | constant PAtomValue, because when building the Dxpression, the TType of the concept isn't known yet.
    DMp1 PAtomValue !A_Concept
  deriving (Eq, Ord, Show, Typeable) -- , Generic, Data

term2Dxpr :: ContextInfo -> Term TermPrim -> Dxpression
term2Dxpr contextInfo trm = case trm of
    Prim (PI _) -> DSet (Set.fromList [ DDcI c | c<-conceptlist ])
    Prim (Pid _ c) -> DDcI c
    Prim (Patm _ av (Just c)) -> DSet (Set.fromList [ DMp1 av c | c<-conceptlist ])
    Prim (Patm _ av  Nothing) -> DMp1 av c
    Prim (PVee _) -> DSet (Set.fromList [ DDcV (Sign src tgt) | src<-conceptlist, tgt<-conceptlist ])
    Prim (Pfull _ src tgt) -> DDcV (Sign src tgt)
    Prim (PBin _ oper) -> DSet (Set.fromList [ DBin oper c | c<-conceptlist ])
    Prim (PBind _ oper c) -> DBin oper c
    Prim (PNamedR relname) -> DSet (Set.fromList [ DDcD (Sign src tgt) | Just sgn<-lookup relname declMap ])
    PEqu _ a b -> let sa = s a; sb = s b in (DSet . Set.fromList) [ DSgn (DEqu (sa, sb)) (Sign src tgt) | sgn_a<-signatures sa, sgn_b<-signatures sb, Just src<-[source sgn_a `lub` source sgn_b], Just tgt<-[target sgn_a `lub` target sgn_b] ]
    PInc _ a b -> let sa = s a; sb = s b in (DSet . Set.fromList) [ DSgn (DInc (sa, sb)) (Sign src tgt) | sgn_a<-signatures sa, sgn_b<-signatures sb, Just src<-[source sgn_a `lub` source sgn_b], Just tgt<-[target sgn_a `lub` target sgn_b] ]
    PIsc _ a b -> let sa = s a; sb = s b in (DSet . Set.fromList) [ DSgn (DIsc (sa, sb)) (Sign src tgt) | sgn_a<-signatures sa, sgn_b<-signatures sb, Just src<-[source sgn_a `lub` source sgn_b], Just tgt<-[target sgn_a `lub` target sgn_b] ]
    PUni _ a b -> let sa = s a; sb = s b in (DSet . Set.fromList) [ DSgn (DUni (sa, sb)) (Sign src tgt) | sgn_a<-signatures sa, sgn_b<-signatures sb, Just src<-[source sgn_a `lub` source sgn_b], Just tgt<-[target sgn_a `lub` target sgn_b] ]
    PDif _ a b -> let sa = s a; sb = s b in (DSet . Set.fromList) [ DSgn (DDif (sa, sb)) (Sign src tgt) | sgn_a<-signatures sa, sgn_b<-signatures sb, Just src<-[source sgn_a `lub` source sgn_b], Just tgt<-[target sgn_a `lub` target sgn_b] ]
    PLrs _ a b -> let sa = s a; sb = s b in (DSet . Set.fromList) [ DSgn (DLrs (sa, sb)) (Sign (source sgn_a) (source sgn_b)) | sgn_a<-signatures sa, sgn_b<-signatures sb ]
    PRrs _ a b -> let sa = s a; sb = s b in (DSet . Set.fromList) [ DSgn (DRrs (sa, sb)) (Sign (target sgn_a) (target sgn_b)) | sgn_a<-signatures sa, sgn_b<-signatures sb ]
    PDia _ a b -> let sa = s a; sb = s b in DDia (sa, sb)
    PCps _ a b -> let sa = s a; sb = s b in (DSet . Set.fromList) [ DSgn (DCps (sa, sb)) (Sign (source sgn_a) (target sgn_b)) | sgn_a<-signatures sa, sgn_b<-signatures sb, Just _between<-[target sgn_a `lub` source sgn_b] ]
    PRad _ a b -> let sa = s a; sb = s b in (DSet . Set.fromList) [ DSgn (DRad (sa, sb)) (Sign (source sgn_a) (target sgn_b)) | sgn_a<-signatures sa, sgn_b<-signatures sb, Just _between<-[target sgn_a `lub` source sgn_b] ]
    PPrd _ a b -> let sa = s a; sb = s b in DPrd (sa, sb)
    PKl0 _ e   -> DKl0 (s e)
    PKl1 _ e   -> DKl1 (s e)
    PFlp _ e   -> DFlp (s e)
    PCpl _ e   -> DCpl (s e)
    PBrk _ e   -> s e
 where
    s :: Term TermPrim -> Dxpression
    s = term2Dxpr contextInfo
    conceptGraph = gens_graph contextInfo
    conceptlist :: [P_Concept]
    conceptlist = vertexList conceptGraph

signatures :: Dxpression -> [Signature]
signatures (DSet s)      = concatMap signatures (Set.toList s)
signatures (DSgn _ sign) = [sign]
signatures (DDcV sign)   = [sign]
signatures (DMp1 _ c)    = [Sign c c]
signatures (DBin _ c)    = [Sign c c]
signatures (DDcI c)      = [Sign c c]
signatures _             = []

typecheck :: Term TermPrim -> Guarded Expression
typecheck trm
 = do
    case term2Dxpr trm of
      DEqu (sa, sb) -> do
        a <- typecheck sa
        b <- typecheck sb
        if signature a == signature b
          then pure (EEqu a b)
          else Error
      DInc (sa, sb) -> do
        a <- typecheck sa
        b <- typecheck sb
        if signature a <= signature b
          then pure (EInc a b)
          else Error
      DIsc (sa, sb) -> do
        a <- typecheck sa
        b <- typecheck sb
        case signature a `glb` signature b of
          Just _sgn -> pure (EIsc a b)
          Nothing   -> Error
      DUni (sa, sb) -> do
        a <- typecheck sa
        b <- typecheck sb
        case signature a `lub` signature b of
          Just _sgn -> pure (EUni a b)
          Nothing   -> Error
      DDif (sa, sb) -> do
        a <- typecheck sa
        b <- typecheck sb
        case signature a `lub` signature b of
          Just _sgn -> pure (EDif a b)
          Nothing   -> Error
      DLrs (sa, sb) -> do  -- Sign (source sgn_a) (source sgn_b), join $ binary' (./.) (MBE (Tgt, snd) (Tgt, fst)) ((Src, fst), (Src, snd))
        a <- typecheck sa
        b <- typecheck sb
        if target a <= target b
          then pure (ELrs a b)
          else Error
      DRrs (sa, sb) -> do  -- Sign (target sgn_a) (target sgn_b), join $ binary' (.\.) (MBE (Src, fst) (Src, snd)) ((Tgt, fst), (Tgt, snd))
        a <- typecheck sa
        b <- typecheck sb
        if source a <= source b
          then pure (ERrs a b)
          else Error
      DDia (sa, sb) -> do
        a <- typecheck sa
        b <- typecheck sb
        case signature a `lub` signature b of
          Just _sgn -> pure (EDia a b)
          Nothing   -> Error
      DCps (sa, sb) -> do
        a <- typecheck sa
        b <- typecheck sb
        case signature a `glb` signature b of
          Just _sgn -> pure (ECps a b)
          Nothing   -> Error
      DRad (sa, sb) -> do
        a <- typecheck sa
        b <- typecheck sb
        pure (ERad a b)
      DDcD rel -> pure (EDcD rel)
      x@(DSet dExprs) -> case Set.toList dExprs of
        [] -> return (Error "Empty set")
        [x] -> return x
        _ -> Error (concatMap signatures dExprs)
      DDcI c -> pure (EDcI c)
      DMp1 av c -> pure (EMp1 av c)
      DDcV (Sign src tgt) -> pure (EDcV src tgt)
      DBin oper c -> pure (EBin oper c)
      DDia (sa, sb) -> do
        a <- typecheck sa
        b <- typecheck sb
        pure (EDia a b)
      DPrd (sa, sb) -> do
        a <- typecheck sa
        b <- typecheck sb
        pure (EPrd a b)
      DKl0 e -> do
        a <- typecheck e
        pure (EKl0 a)
      DKl1 e -> do
        a <- typecheck e
        pure (EKl1 a)
      DFlp e -> do
        a <- typecheck e
        pure (EFlp a)
      DCpl e -> do
        a <- typecheck e
        pure (ECpl a)
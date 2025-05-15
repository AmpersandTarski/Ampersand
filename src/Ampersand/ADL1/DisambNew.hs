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
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Ampersand.ADL1.P2A_Converters (pCpt2aCpt, findRels)
-- import Text.PrettyPrint.Leijen (Pretty (..), text)


-- data DisambPrim
--   = Rel [Dxpression] -- It is an expression, we don't know which, but it's going to be one of these (usually this is a list of relations)
--   | Ident -- identity, and we know nothing about its type
--   | Vee -- vee, type unknown
--   | Mp1 PAtomValue -- a singleton atomvalue, type unknown
--   | BinOper PBinOp -- a binary operator, type unknown
--   | Known Dxpression -- It is an expression, and we know exactly which. That is: disambiguation was succesful here
--   deriving (Show) -- Here, deriving Show serves debugging purposes only.

-- data AClassify
--   = Isa
--       { genpos :: !Origin,
--         -- | specific concept
--         genspc :: !A_Concept,
--         -- | generic concept
--         gengen :: !A_Concept
--       }
--   | IsE
--       { genpos :: !Origin,
--         -- | specific concept
--         genspc :: !A_Concept,
--         -- | concepts of which the conjunction is equivalent to the specific concept
--         genrhs :: !(NE.NonEmpty A_Concept)
--       }

makeGraph :: [AClassify] -> AdjacencyMap A_Concept
makeGraph conceptPairs = Graph.overlays [vertex spc `Graph.connect` vertex gen | (spc,gen) <- pairs]
  where
    pairs = [ (genspc isa, gengen isa) | isa@(Isa{})<-conceptPairs]<>[ (genspc ise, c) | ise@(IsE{})<-conceptPairs, c<-toList (genrhs ise)]

synonym :: Ord a => AdjacencyMap a -> a -> a -> Bool
synonym g a b = a==b || (hasEdge a b g && hasEdge b a g)


-- Compute the least upper bound (lub) of a list of pairs
lub :: (Ord a) =>AdjacencyMap a -> a -> a -> Maybe a
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
glb :: (Ord a) =>AdjacencyMap a -> a -> a -> Maybe a
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

leq :: (Ord a) =>AdjacencyMap a -> a -> a -> Maybe Bool
leq conceptGraph a b
    | hasEdge a b rel = Just True
    | hasEdge b a rel = Just False
    | otherwise       = Nothing
    where
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
    DDcD Name
  | -- | Identity relation
    DDcI
  | -- | Epsilon relation (introduced by the system to ensure we compare concepts by equality only.
    DBin !PBinOp
  | -- | Signed expression, to disambiguate
    DSgn (Term TermPrim) !(Set.Set Signature)
  | -- | Full relation
    DDcV
  | -- | constant PAtomValue, because when building the Dxpression, the TType of the concept isn't known yet.
    DMp1 PAtomValue
  deriving (Eq, Ord, Show, Typeable, Traversable) -- , Generic, Data

term2Dxpr :: ContextInfo -> Term TermPrim -> Dxpression
term2Dxpr contextInfo trm = case trm of
    Prim (PI _)               ->                         (DSgn DDcI . Set.fromList) [Sign c c| c<-conceptList]
    Prim (Pid _ c)            -> let c'=toA_Concept c in (DSgn DDcI . Set.fromList) [Sign c' c']
    Prim (Patm _ av (Just c)) -> let c'=toA_Concept c in (DSgn (DMp1 av) . Set.fromList) [Sign c' c']
    Prim (Patm _ av  Nothing) ->                         (DSgn (DMp1 av) . Set.fromList) [Sign c c| c<-conceptList]
    Prim (PVee _)             ->                         (DSgn DDcV . Set.fromList) [Sign src tgt | src<-conceptList, tgt<-conceptList ]
    Prim (Pfull _ src tgt)    ->                         (DSgn DDcV . Set.fromList) [Sign (toA_Concept src) (toA_Concept tgt)]
    Prim (PBin _ oper)        ->                         (DSgn (DBin oper) . Set.fromList) [Sign c c| c<-conceptList]
    Prim (PBind _ oper c)     -> let c'=toA_Concept c in (DSgn (DBin oper) . Set.fromList) [Sign c' c'| c<-conceptList]
    Prim (PNamedR rel)        ->                         (DSgn (DDcD (name rel)) . DSet . Set.fromList . map (sign.elems) . findRels (declDisambMap contextInfo) . name) rel
    PEqu _ a b -> let DSgn sa sgna = s a; DSgn sb sgnb = s b in (DSgn (DEqu (sa, sb)) . Set.fromList) [ Sign src tgt | sgn_a<-Set.toList sgna, sgn_b<-Set.toList sgnb, Just src<-[lub conceptGraph (source sgn_a) (source sgn_b)], Just tgt<-[lub conceptGraph (target sgn_a) (target sgn_b)] ]
    PInc _ a b -> let DSgn sa sgna = s a; DSgn sb sgnb = s b in (DSgn (DInc (sa, sb)) . Set.fromList) [ Sign src tgt | sgn_a<-Set.toList sgna, sgn_b<-Set.toList sgnb, Just src<-[lub conceptGraph (source sgn_a) (source sgn_b)], Just tgt<-[lub conceptGraph (target sgn_a) (target sgn_b)] ]
    PIsc _ a b -> let DSgn sa sgna = s a; DSgn sb sgnb = s b in (DSgn (DIsc (sa, sb)) . Set.fromList) [ Sign src tgt | sgn_a<-Set.toList sgna, sgn_b<-Set.toList sgnb, Just src<-[lub conceptGraph (source sgn_a) (source sgn_b)], Just tgt<-[lub conceptGraph (target sgn_a) (target sgn_b)] ]
    PUni _ a b -> let DSgn sa sgna = s a; DSgn sb sgnb = s b in (DSgn (DUni (sa, sb)) . Set.fromList) [ Sign src tgt | sgn_a<-Set.toList sgna, sgn_b<-Set.toList sgnb, Just src<-[glb conceptGraph (source sgn_a) (source sgn_b)], Just tgt<-[glb conceptGraph (target sgn_a) (target sgn_b)] ]
    PDif _ a b -> let DSgn sa sgna = s a; DSgn sb sgnb = s b in (DSgn (DDif (sa, sb)) . Set.fromList) [ Sign (source sgn_a) (target sgn_a) | sgn_a<-Set.toList sgna, sgn_b<-Set.toList sgnb, Just src<-[glb conceptGraph (source sgn_a) (source sgn_b)], Just tgt<-[glb conceptGraph (target sgn_a) (target sgn_b)] ]
    PLrs _ a b -> let DSgn sa sgna = s a; DSgn sb sgnb = s b in (DSgn (DLrs (sa, sb)) . Set.fromList) [ Sign (source sgn_a) (source sgn_b) | sgn_a<-Set.toList sgna, sgn_b<-Set.toList sgnb, leq conceptGraph (target sgn_b) (target sgn_a) ]
    PRrs _ a b -> let DSgn sa sgna = s a; DSgn sb sgnb = s b in (DSgn (DRrs (sa, sb)) . Set.fromList) [ Sign (target sgn_a) (target sgn_b) | sgn_a<-Set.toList sgna, sgn_b<-Set.toList sgnb, leq conceptGraph (source sgn_a) (source sgn_b) ]
    PDia _ a b -> let DSgn sa sgna = s a; DSgn sb sgnb = s b in (DSgn (DDia (sa, sb)) . Set.fromList) [ Sign (source sgn_a) (target sgn_b) | sgn_a<-Set.toList sgna, sgn_b<-Set.toList sgnb, source sgn_a == source sgn_b ]
    PCps _ a b -> let DSgn sa sgna = s a; DSgn sb sgnb = s b in (DSgn (DCps (sa, sb)) . Set.fromList) [ Sign src tgt | sgn_a<-Set.toList sgna, sgn_b<-Set.toList sgnb, Just _between<-[glb conceptGraph (target sgn_a) (source sgn_b)] ]
    PRad _ a b -> let DSgn sa sgna = s a; DSgn sb sgnb = s b in (DSgn (DRad (sa, sb)) . Set.fromList) [ Sign src tgt | sgn_a<-Set.toList sgna, sgn_b<-Set.toList sgnb, Just _between<-[lub conceptGraph (target sgn_a) (source sgn_b)] ]
    PPrd _ a b -> let DSgn sa sgna = s a; DSgn sb sgnb = s b in (DSgn (DPrd (sa, sb)) . Set.fromList) [ Sign src tgt | sgn_a<-Set.toList sgna, sgn_b<-Set.toList sgnb ]
    PKl0 _ e   -> let DSgn se sgne = s e in DSgn (DKl0 se) sgne
    PKl1 _ e   -> let DSgn se sgne = s e in DSgn (DKl1 se) sgne
    PFlp _ e   -> let DSgn se sgne = s e in DFlp (DKl1 se) sgne
    PCpl _ e   -> let DSgn se sgne = s e in DCpl (DKl1 se) sgne

    PFlp _ e   -> DFlp (s e)
    PCpl _ e   -> DCpl (s e)
    PBrk _ e   -> s e
 where
    s :: Term TermPrim -> Dxpression
    s = term2Dxpr contextInfo
    conceptGraph = makeGraph (allGens contextInfo)
    toA_Concept :: P_Concept -> A_Concept
    toA_Concept = pCpt2aCpt (conceptMap contextInfo)
    conceptList :: [A_Concept]
    conceptList = [toA_Concept c | c<-vertexList conceptGraph ]

signatures :: Dxpression -> Set.Set Signature
signatures (DSgn _ sgns) = sgns
signatures _             = Set.empty

typecheck ::ContextInfo -> Dxpression -> Guarded Expression
typecheck ci dExpr
 =  case dExpr of
      DEqu (sa, sb) -> do
        a <- tCheck sa
        b <- tCheck sb
        if null (signatures sa `Set.intersection` signatures sb0)
          then pure (EEqu a b)
          else Error
      DInc (sa, sb) -> do
        a <- tCheck sa
        b <- tCheck sb
        if signature a <= signature b
          then pure (EInc a b)
          else Error
      DIsc (sa, sb) -> do
        a <- tCheck sa
        b <- tCheck sb
        case signature a `glb` signature b of
          Just _sgn -> pure (EIsc a b)
          Nothing   -> Error
      DUni (sa, sb) -> do
        a <- tCheck sa
        b <- tCheck sb
        case signature a `lub` signature b of
          Just _sgn -> pure (EUni a b)
          Nothing   -> Error
      DDif (sa, sb) -> do
        a <- tCheck sa
        b <- tCheck sb
        case signature a `lub` signature b of
          Just _sgn -> pure (EDif a b)
          Nothing   -> Error
      DLrs (sa, sb) -> do  -- Sign (source sgn_a) (source sgn_b), join $ binary' (./.) (MBE (Tgt, snd) (Tgt, fst)) ((Src, fst), (Src, snd))
        a <- tCheck sa
        b <- tCheck sb
        if target a <= target b
          then pure (ELrs a b)
          else Error
      DRrs (sa, sb) -> do  -- Sign (target sgn_a) (target sgn_b), join $ binary' (.\.) (MBE (Src, fst) (Src, snd)) ((Tgt, fst), (Tgt, snd))
        a <- tCheck sa
        b <- tCheck sb
        if source a <= source b
          then pure (ERrs a b)
          else Error
      DDia (sa, sb) -> do
        a <- tCheck sa
        b <- tCheck sb
        case signature a `lub` signature b of
          Just _sgn -> pure (EDia a b)
          Nothing   -> Error
      DCps (sa, sb) -> do
        a <- tCheck sa
        b <- tCheck sb
        case signature a `glb` signature b of
          Just _sgn -> pure (ECps a b)
          Nothing   -> Error
      DRad (sa, sb) -> do
        a <- tCheck sa
        b <- tCheck sb
        pure (ERad a b)
      DPrd (sa, sb) -> do
        a <- tCheck sa
        b <- tCheck sb
        pure (EPrd a b)
      DKl0 e -> do
        a <- tCheck e
        pure (EKl0 a)
      DKl1 e -> do
        a <- tCheck e
        pure (EKl1 a)
      DFlp e -> do
        a <- tCheck e
        pure (EFlp a)
      DCpl e -> do
        a <- tCheck e
        pure (ECpl a)
      DDcD rel -> pure (EDcD rel)
      DDcI c -> pure (EDcI c)
      DBin oper c -> pure (EBin oper c)
      DSgn dExpr sgn -> do
        a <- tCheck sa
        b <- tCheck sb
        pure (ESgn a b)
      x@(DSet dExprs) -> case Set.toList dExprs of
        [] -> return (Error "Empty set")
        [x] -> return x
        _ -> Error (concatMap signatures dExprs)
      DDcV (Sign src tgt) -> pure (EDcV src tgt)
      DMp1 av -> Error ("cannot tCheck constant " ++ show av)
    where
      tCheck :: Dxpression -> Guarded Expression
      tCheck = typecheck ci

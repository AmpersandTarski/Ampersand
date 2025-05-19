{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
-- import qualified RIO.NonEmpty.Partial as PARTIAL
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Ampersand.ADL1.P2A_Converters (findRels, findRelsTyped, pSign2aSign)
import qualified Data.List.NonEmpty as NE
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
    | hasEdge a b rtc = Just b
    | hasEdge b a rtc = Just a
    | otherwise =
       case Set.toList (postSet a rtc `Set.intersection` postSet b rtc) L.\\ [a,b] of
        [] -> Nothing
        x:xs -> Just (L.foldr minimum x xs) -- find the minimum of the upper bounds
    where
      minimum a' b' = if hasEdge a' b' rtc then a' else b'
      rtc = reflexiveClosure (transitiveClosure conceptGraph)

-- Compute the greatest lower bound (glb) of a list of pairs
glb :: (Ord a) =>AdjacencyMap a -> a -> a -> Maybe a
glb conceptGraph a b
    | hasEdge a b rtc = Just a
    | hasEdge b a rtc = Just b
    | otherwise =
       case Set.toList (preSet a rtc `Set.intersection` preSet b rtc) L.\\ [a,b] of
        [] -> Nothing
        x:xs -> Just (L.foldr maximum x xs) -- find the minimum of the upper bounds
    where
      maximum a' b' = if hasEdge a' b' rtc then b' else a'
      rtc = reflexiveClosure (transitiveClosure conceptGraph)

leq :: (Ord a) =>AdjacencyMap a -> a -> a -> Maybe Bool
leq conceptGraph a b
    | hasEdge a b rtc = Just True
    | hasEdge b a rtc = Just False
    | otherwise       = Nothing
    where
      rtc = reflexiveClosure (transitiveClosure conceptGraph)

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
  = DSgn (Term TermPrim) ![Signature]
  deriving (Show, Typeable) -- , Generic, Data

signatures :: ContextInfo -> Term TermPrim -> Guarded [Signature]
signatures contextInfo trm = case trm of
  Prim (PI _)              ->                       pure [Sign c c| c<-conceptList]
  Prim (Pid _ c)           -> let c'=pCpt2aCpt c in pure [Sign c' c']
  Prim (Patm _ _ (Just c)) -> let c'=pCpt2aCpt c in pure [Sign c' c']
  Prim (Patm _ _  Nothing) ->                       pure [Sign c c| c<-conceptList]
  Prim (PVee _)            ->                       pure [Sign src tgt | src<-conceptList, tgt<-conceptList ]
  Prim (Pfull _ src tgt)   ->                       pure [Sign (pCpt2aCpt src) (pCpt2aCpt tgt)]
  Prim (PBin _ _)          ->                       pure [Sign c c| c<-conceptList]
  Prim (PBind _ _ c)       -> let c'=pCpt2aCpt c in pure [Sign c' c']
  Prim (PNamedR rel)       -> let sgns :: Maybe P_Sign -> [Signature]
                                  sgns (Just sgn) = (map sign . findRelsTyped (declDisambMap contextInfo) (name rel) . pSign2aSign pCpt2aCpt) sgn
                                  sgns Nothing    = (Map.elems . fmap sign . findRels (declDisambMap contextInfo) . name) rel
                              in  case sgns (p_mbSign rel) of
                                               [] -> (Errors . return . CTXE (origin trm)) ("No signature found for relation "<> tshow rel)
                                               ss -> pure ss
  PEqu o a b -> do sgna <- signats a; sgnb <- signats b
                   case [ Sign src tgt | sgn_a<-sgna, sgn_b<-sgnb, Just src<-[lub conceptGraph (source sgn_a) (source sgn_b)], Just tgt<-[lub conceptGraph (target sgn_a) (target sgn_b)] ] of
                    []  -> (Errors . return . CTXE o) ("Cannot match the signatures of the two sides of the equation.\nlhs: "<>tshow a<>" :: "<>tshow sgna<>"\nrhs: "<>tshow b<>" :: "<>tshow sgnb)
                    ss -> return ss
  PInc o a b -> do sgna <- signats a; sgnb <- signats b
                   case [ Sign src tgt | sgn_a<-sgna, sgn_b<-sgnb, Just src<-[lub conceptGraph (source sgn_a) (source sgn_b)], Just tgt<-[lub conceptGraph (target sgn_a) (target sgn_b)] ] of
                    []  -> (Errors . return . CTXE o) ("Cannot match the signatures of the two sides of the inclusion.\nlhs: "<>tshow a<>" :: "<>tshow sgna<>"\nrhs: "<>tshow b<>" :: "<>tshow sgnb)
                    ss -> return ss
  PIsc o a b -> do sgna <- signats a; sgnb <- signats b
                   case [ Sign src tgt | sgn_a<-sgna, sgn_b<-sgnb, Just src<-[lub conceptGraph (source sgn_a) (source sgn_b)], Just tgt<-[lub conceptGraph (target sgn_a) (target sgn_b)] ] of
                    []  -> (Errors . return . CTXE o) ("Cannot match the signatures of the two sides of the intersection.\nlhs: "<>tshow a<>" :: "<>tshow sgna<>"\nrhs: "<>tshow b<>" :: "<>tshow sgnb)
                    ss -> return ss
  PUni o a b -> do sgna <- signats a; sgnb <- signats b
                   case [ Sign src tgt | sgn_a<-sgna, sgn_b<-sgnb, Just src<-[glb conceptGraph (source sgn_a) (source sgn_b)], Just tgt<-[glb conceptGraph (target sgn_a) (target sgn_b)] ] of
                    []  -> (Errors . return . CTXE o) ("Cannot match the signatures of the two sides of the union.\nlhs: "<>tshow a<>" :: "<>tshow sgna<>"\nrhs: "<>tshow b<>" :: "<>tshow sgnb)
                    ss -> return ss
  PDif o a b -> do sgna <- signats a; sgnb <- signats b
                   case [ Sign src tgt | sgn_a<-sgna, sgn_b<-sgnb, Just src<-[glb conceptGraph (source sgn_a) (source sgn_b)], Just tgt<-[glb conceptGraph (target sgn_a) (target sgn_b)] ] of
                    []  -> (Errors . return . CTXE o) ("Cannot match the signatures of the two sides of the difference.\nlhs: "<>tshow a<>" :: "<>tshow sgna<>"\nrhs: "<>tshow b<>" :: "<>tshow sgnb)
                    ss -> return ss
  PLrs o a b -> do sgna <- signats a; sgnb <- signats b
                   case [ Sign (source sgn_a) (source sgn_b) | sgn_a<-sgna, sgn_b<-sgnb, Just True<-[leq conceptGraph (target sgn_b) (target sgn_a)] ] of
                    []  -> (Errors . return . CTXE o) ("Cannot match the target concepts of the two sides of the left residual.\n  The target of: "<>tshow a<>", which is "<>tshow (target sgna)<>", should be equal (or more generic) than\n  the target of: "<>tshow b<>", which is "<>tshow (target sgnb)<>".")
                    ss -> return ss
  PRrs o a b -> do sgna <- signats a; sgnb <- signats b
                   case [ Sign (target sgn_a) (target sgn_b) | sgn_a<-sgna, sgn_b<-sgnb, Just True<-[leq conceptGraph (source sgn_a) (source sgn_b)] ] of
                    []  -> (Errors . return . CTXE o) ("Cannot match the source concepts of the two sides of the right residual.\n  The source of: "<>tshow a<>", which is "<>tshow (source sgna)<>", should be equal (or more specific) than\n  the source of: "<>tshow b<>", which is "<>tshow (source sgnb)<>".")
                    ss -> return ss
  PDia o a b -> do sgna <- signats a; sgnb <- signats b
                   case [ Sign (source sgn_a) (target sgn_b) | sgn_a<-sgna, sgn_b<-sgnb, target sgn_a == source sgn_b ] of
                    []  -> (Errors . return . CTXE o) ("Cannot match the signatures of the two sides of the diamond.\n  The target of: "<>tshow a<>", which is "<>tshow (target sgna)<>"\n, should be equal to\n  the source of: "<>tshow b<>", which is "<>tshow (source sgnb)<>".")
                    ss -> return ss
  PCps o a b -> do sgna <- signats a; sgnb <- signats b
                   case [ Sign (source sgn_a) (target sgn_b) | sgn_a<-sgna, sgn_b<-sgnb, Just _between<-[glb conceptGraph (target sgn_a) (source sgn_b)] ] of
                    []  -> (Errors . return . CTXE o) ("Cannot match the signatures of the two sides of the composition.\n  The target of: "<>tshow a<>", which is "<>tshow (target sgna)<>"\n, should be equal to (or share a concept) \n  the source of: "<>tshow b<>", which is "<>tshow (source sgnb)<>".")
                    ss -> return ss
  PRad o a b -> do sgna <- signats a; sgnb <- signats b
                   case [ Sign (source sgn_a) (target sgn_b) | sgn_a<-sgna, sgn_b<-sgnb, Just _between<-[lub conceptGraph (target sgn_a) (source sgn_b)] ] of
                    []  -> (Errors . return . CTXE o) ("Cannot match the signatures of the two sides of the relative addition.\n  The target of: "<>tshow a<>", which is "<>tshow (target sgna)<>"\n, should be equal to (or share a concept) \n  the source of: "<>tshow b<>", which is "<>tshow (source sgnb)<>".")
                    ss -> return ss
  PPrd o a b -> do sgna <- signats a; sgnb <- signats b
                   return [ Sign (source sgn_a) (target sgn_b) | sgn_a<-sgna, sgn_b<-sgnb ]
  PKl0 _ e   -> signats e
  PKl1 _ e   -> signats e
  PFlp _ e   -> fmap flp (signats e)
  PCpl _ e   -> signats e
  PBrk _ e   -> signats e
  where
    conceptList :: [A_Concept]
    conceptList = (vertexList . makeGraph . allGens) contextInfo
    pCpt2aCpt = conceptMap contextInfo
    conceptGraph :: AdjacencyMap A_Concept
    conceptGraph = makeGraph (allGens contextInfo)
    signats = signatures contextInfo

-- constrain :: Maybe A_Concept -> Maybe A_Concept -> [Signature] -> Guarded Signature
-- constrain mSrc mTgt sgns
--  = do let sgns' = case (mSrc, mTgt) of
--                    (Just src, Just tgt) -> [Sgn src' tgt' | Sign s t<-sgns, Just src'<-[s `grLwB`  src], Just tgt'<-[t `grLwB`  tgt]]
--                    (Just src, Nothing ) -> [Sgn src' tgt' | Sign s t<-sgns, Just src'<-[s `grLwB`  src]]
--                    (Nothing,  Just tgt) -> [Sgn src' tgt' | Sign s t<-sgns, Just tgt'<-[t `grLwB`  tgt]]
--                    _ -> sgns
--       case sgns' of
--         [sgn] -> cast trm sgn
--         [] -> (Errors . return . CTXE (origin trm)) ("Cannot find a signature for "<>tshow trm<>".")
--         sgs -> (Errors . return . CTXE (origin trm)) ("Ambiguous signatures for "<>tshow trm<>": "<>tshow sgs)
   

term2Expr :: ContextInfo -> Term TermPrim -> Guarded Expression
term2Expr contextInfo trm 
 = do sgns<-signatures contextInfo trm
      sgn<-case sgns of
             [] -> (Errors . return . CTXE (origin trm)) ("Cannot find a signature for "<>tshow trm<>".")
             [sgn] -> pure sgn
             sgs -> (Errors . return . CTXE (origin trm)) ("Ambiguous signatures for "<>tshow trm<>": "<>tshow sgs)
      case trm of
        Prim (PI _) -> pure (EDcI (source sgn))
        Prim (Pid _ _) -> pure (EDcI (source sgn))
        Prim (Patm _ av _) -> pure (EMp1 av (source sgn))
        Prim (PVee _) -> pure (EDcV sgn)
        Prim (Pfull{}) -> pure (EDcV sgn)
        Prim (PBin _ oper) -> pure (EBin oper (source sgn))
        Prim (PBind _ oper _) -> pure (EBin oper (source sgn))
        Prim (PNamedR rel) -> let rels :: Maybe P_Sign -> [Relation]
                                  rels (Just sgn) = (findRelsTyped (declDisambMap contextInfo) (name rel) . pSign2aSign pCpt2aCpt) sgn
                                  rels Nothing    = (Map.elems . findRels (declDisambMap contextInfo) . name) rel
                              in  case [Sign src tgt| Sign s t<-sgns (p_mbSign rel), Just src<-[s `grLwB` source sgn], Just tgt<-[t `grLwB` target sgn]] of
                                               [] -> (Errors . return . CTXE (origin trm)) ("No signature found for relation "<> tshow rel)
                                               ss -> pure ss

  where
    t2e :: Maybe A_Concept -> Maybe A_Concept -> Term TermPrim -> Guarded Expression
    t2e (Just src) (Just tgt) trm@(Prim (PI o))
     = case src `grLwB` tgt of
         Just c             -> pure (EDcI c)
         Nothing            -> (Errors . return . CTXE o) ("Cannot find a signature for "<>tshow trm<>".")
    t2e (Just src) Nothing _ = pure (EDcI src)
    t2e Nothing (Just tgt) _ = pure (EDcI tgt)
    t2e Nothing Nothing trm  = (Errors . return . CTXE (origin trm)) ("Cannot find a signature for "<>tshow trm<>".")

    t2e (Just src) (Just tgt) trm@(Prim (Pid o c))
     = case (src `grLwB` pCpt2aCpt c, tgt `grLwB` pCpt2aCpt c) of
         (Just sc, Just tc) -> case sc `grLwB` tc of
                                 Just stc -> pure (EDcI stc)
                                 Nothing  -> (Errors . return . CTXE o) ("Cannot find a signature for "<>tshow trm<>".")
         (Just _ , Nothing) -> (Errors . return . CTXE o) ("Cannot find a signature for "<>tshow trm<>".")
         (Nothing, Just _ ) -> (Errors . return . CTXE o) ("Cannot find a signature for "<>tshow trm<>".")
         (Nothing, Nothing) -> (Errors . return . CTXE o) ("Cannot find a signature for "<>tshow trm<>".")
    
    t2e (Just src) (Just tgt) (Prim (Patm o av (Just c)))
     = case grLwB src tgt of
         Just c             -> pure (EMp1 av c)
         Nothing            -> (Errors . return . CTXE o) ("Cannot find a signature for "<>tshow trm<>".")
    t2e (Just src) Nothing trm = EDcI src
    t2e Nothing (Just tgt) trm = EDcI tgt
    t2e Nothing Nothing trm = (Errors . return . CTXE (origin trm)) ("Cannot find a signature for "<>tshow trm<>".")

    conceptGraph :: AdjacencyMap A_Concept
    conceptGraph = makeGraph (allGens contextInfo)
    grLwB = glb conceptGraph
    lsUpB = glb conceptGraph
    pCpt2aCpt = conceptMap contextInfo

--  = do sgns <- signatures contextInfo trm
      
--       case trm of
--         Prim prim ->
--           do 
--              case sgns of
--                [sgn] -> return (cast prim sgn)
--                _ -> (Errors . NE.fromList) [CTXE (origin prim) ("Multiple signatures fit "<>tshow prim<>": "<>tshow sgns<>".")]
--           where
--              cast :: TermPrim -> Signature -> Expression
--              cast (PI{}) sgn               = EDcI (source sgn)
--              cast (Pid _ c) sgn            = EDcI (source sgn)
--              cast (Patm _ av (Just c)) sgn = EMp1 av (source sgn)
--              cast (Patm _ av  Nothing) sgn = EMp1 av (source sgn)
--              cast (PVee{}) sgn             = EDcV sgn
--              cast (Pfull{}) sgn            = EDcV sgn
--              cast (PBin _ oper) sgn        = EBin oper (source sgn)
--              cast (PBind _ oper c) sgn     = EBin oper (source sgn)
--              cast (PNamedR rel) sgn        = case findRelsTyped (declDisambMap contextInfo) (name rel) sgn of
--                                               [dclExpr@EDcD{}]-> dclExpr
--                                               [x] -> fatal ("I expected a declaration of relation "<>tshow rel<>", but got "<>tshow x<>" instead.")
--                                               xs  -> fatal ("I expected only one declaration of relation "<>tshow rel<>", but got "<>tshow xs<>" instead.")
--         PEqu _ a b -> do sgna <- signatures contextInfo a; sgnb <- signatures contextInfo b
--                          case [ Sign src tgt | sgn_a<-sgna, sgn_b<-sgnb, Just src<-[lub conceptGraph (source sgn_a) (source sgn_b)], Just tgt<-[lub conceptGraph (target sgn_a) (target sgn_b)] ] of
--                           [sgn] -> return (EEqu)
--         PInc _ a b -> do sgna <- s a; sgnb <- s b
--                          guarded trm [ Sign src tgt | sgn_a<-sgna, sgn_b<-sgnb, Just src<-[lub conceptGraph (source sgn_a) (source sgn_b)], Just tgt<-[lub conceptGraph (target sgn_a) (target sgn_b)] ]
--         PIsc _ a b -> do sgna <- s a; sgnb <- s b
--                          guarded trm [ Sign src tgt | sgn_a<-sgna, sgn_b<-sgnb, Just src<-[lub conceptGraph (source sgn_a) (source sgn_b)], Just tgt<-[lub conceptGraph (target sgn_a) (target sgn_b)] ]
--         PUni _ a b -> do sgna <- s a; sgnb <- s b
--                          guarded trm [ Sign src tgt | sgn_a<-sgna, sgn_b<-sgnb, Just src<-[glb conceptGraph (source sgn_a) (source sgn_b)], Just tgt<-[glb conceptGraph (target sgn_a) (target sgn_b)] ]
--         PDif _ a b -> do sgna <- s a; sgnb <- s b
--                          guarded trm [ Sign (source sgn_a) (target sgn_a) | sgn_a<-sgna, sgn_b<-sgnb, Just _<-[glb conceptGraph (source sgn_a) (source sgn_b)], Just _<-[glb conceptGraph (target sgn_a) (target sgn_b)] ]
--         PLrs _ a b -> do sgna <- s a; sgnb <- s b
--                          guarded trm [ Sign (source sgn_a) (source sgn_b) | sgn_a<-sgna, sgn_b<-sgnb, Just True<-[leq conceptGraph (target sgn_b) (target sgn_a)] ]
--         PRrs _ a b -> do sgna <- s a; sgnb <- s b
--                          guarded trm [ Sign (target sgn_a) (target sgn_b) | sgn_a<-sgna, sgn_b<-sgnb, Just True<-[leq conceptGraph (source sgn_a) (source sgn_b)] ]
--         PDia _ a b -> do sgna <- s a; sgnb <- s b
--                          guarded trm [ Sign (source sgn_a) (target sgn_b) | sgn_a<-sgna, sgn_b<-sgnb, source sgn_a == source sgn_b ]
--         PCps _ a b -> do sgna <- s a; sgnb <- s b
--                          guarded trm [ Sign (source sgn_a) (target sgn_b) | sgn_a<-sgna, sgn_b<-sgnb, Just _between<-[glb conceptGraph (target sgn_a) (source sgn_b)] ]
--         PRad _ a b -> do sgna <- s a; sgnb <- s b
--                          guarded trm [ Sign (source sgn_a) (target sgn_b) | sgn_a<-sgna, sgn_b<-sgnb, Just _between<-[lub conceptGraph (target sgn_a) (source sgn_b)] ]
--         PPrd _ a b -> do sgna <- s a; sgnb <- s b
--                      guarded trm [ Sign (source sgn_a) (target sgn_b) | sgn_a<-sgna, sgn_b<-sgnb ]
--         PKl0 _ e   -> do sgne <- s e; guarded trm sgne
--         PKl1 _ e   -> do sgne <- s e; guarded trm sgne
--         PFlp _ e   -> do sgne <- s e; guarded trm sgne
--         PCpl _ e   -> do sgne <- s e; guarded trm sgne
--         PBrk _ e   -> s e
--      where
--         s :: Term TermPrim -> Guarded [Expression]
--         s = term2Expr contextInfo
--         conceptGraph = makeGraph (allGens contextInfo)

guarded :: Term TermPrim -> [Signature] -> Guarded Expression
guarded (PEqu _ a b) [sgn] = pure (EEqu (aExpr, bExpr))
guarded (PEqu _ a b) [] = Errors

{-
data Expression
  = -- | equivalence             =
    EEqu !(Expression, Expression)
  | -- | inclusion               |-
    EInc !(Expression, Expression)
  | -- | intersection            /\
    EIsc !(Expression, Expression)
  | -- | union                   \/
    EUni !(Expression, Expression)
  | -- | difference              -
    EDif !(Expression, Expression)
  | -- | left residual           /
    ELrs !(Expression, Expression)
  | -- | right residual          \
    ERrs !(Expression, Expression)
  | -- | diamond                 <>
    EDia !(Expression, Expression)
  | -- | composition             ;
    ECps !(Expression, Expression)
  | -- | relative addition       !
    ERad !(Expression, Expression)
  | -- | cartesian product       *
    EPrd !(Expression, Expression)
  | -- | Rfx.Trn closure         *  (Kleene star)
    EKl0 !Expression
  | -- | Transitive closure      +  (Kleene plus)
    EKl1 !Expression
  | -- | conversion (flip, wok)  ~
    EFlp !Expression
  | -- | Complement
    ECpl !Expression
  | -- | bracketed expression ( ... )
    EBrk !Expression
  | -- | simple relation
    EDcD !Relation
  | -- | Identity relation
    EDcI !A_Concept
  | -- | Epsilon relation (introduced by the system to ensure we compare concepts by equality only.
    EEps !A_Concept !Signature
  | -- | relation based on a simple binary operator  (e.g. x > y)
    EBin !PBinOp !A_Concept
  | -- | Cartesian product relation
    EDcV !Signature
  | -- | constant PAtomValue, because when building the Expression, the TType of the concept isn't known yet.
    EMp1 !PAtomValue !A_Concept
  deriving (Eq, Ord, Show, Typeable, Generic, Data)
-}

typecheck ::ContextInfo -> Term TermPrim -> Guarded Dxpression
typecheck ci pTerm
 =  case pTerm of
      PEqu _ pa pb -> do
        let DSgn sa sgna = check pa; DSgn sb sgnb = check pb
        Just src<-[lub conceptGraph (source sgn_a) (source sgn_b)]
        Just tgt<-[lub conceptGraph (target sgn_a) (target sgn_b)]
        if null (signatures sa `Set.intersection` signatures sb0)
          then pure (EEqu a b)
          else Error
    where
      check :: Term TermPrim -> Guarded (Expression, [Signature])
      check = typecheck ci

{- }
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
-}
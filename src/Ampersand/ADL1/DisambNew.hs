{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Ampersand.ADL1.Disambiguate
  ( disambiguate,
    orWhenEmpty,
    DisambPrim (..),
    pCpt2aCpt,
  )
where

import Ampersand.Basics
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Core.ParseTree
import Control.Arrow
import qualified RIO.NonEmpty as NE
import qualified RIO.NonEmpty.Partial as PARTIAL
import qualified RIO.Set as Set
import Text.PrettyPrint.Leijen (Pretty (..), text)


data DisambPrim
  = Rel [Dxpression] -- It is an expression, we don't know which, but it's going to be one of these (usually this is a list of relations)
  | Ident -- identity, and we know nothing about its type
  | Vee -- vee, type unknown
  | Mp1 PAtomValue -- a singleton atomvalue, type unknown
  | BinOper PBinOp -- a binary operator, type unknown
  | Known Dxpression -- It is an expression, and we know exactly which. That is: disambiguation was succesful here
  deriving (Show) -- Here, deriving Show serves debugging purposes only.

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
    DEps !Signature
  | -- | relation based on a simple binary operator  (e.g. x > y)
    DBin !PBinOp !A_Concept
  | -- | Signed expression, to disambiguate
    DSgn !Dxpression !Signature
  | -- | Signed expression, to disambiguate
    DSet !(Set.Set Dxpression)
  | -- | Full relation
    DDcV !Signature
  | -- | constant PAtomValue, because when building the Dxpression, the TType of the concept isn't known yet.
    DMp1 Value !A_Concept
  deriving (Eq, Ord, Show, Typeable, Generic, Data)

s :: Term TermPrim -> Dxpression
s trm = case trm of
    Prim (PI _) -> DSet (Set.fromList [ DDcI c | c<-conceptlist ])
    Prim (Pid _ c) -> DDcI c
    Prim (Patm _ av) -> DSet (Set.fromList [ DMp1 av c | c<-conceptlist ])
    Prim (PVee _) -> DSet (Set.fromList [ DDcV (Sign src tgt) | src<-conceptlist, tgt<-conceptlist ])
    Prim (Pfull _ src tgt) -> DDcV (Sign src tgt)
    Prim (PBin _ oper) -> DSet (Set.fromList [ DBin oper c | c<-conceptlist ])
    Prim (PBind _ oper c) -> DBin oper c
    Prim (PNamedR relname) -> DSet (Set.fromList [ DDcD (Sign src tgt) | Just sgn<-lookup relname declMap ])
    PEqu _ a b -> DSet (Set.fromList [ DSgn (DEqu (s a, s b)) sgn_a | sgn_a<-signatures a, sgn_b<-signatures b, sgn_a==sgn_b ])
    PInc _ a b -> DSet (Set.fromList [ DSgn (DInc (s a, s b)) (Sign src tgt) | sgn_a<-signatures a, sgn_b<-signatures b, Just src<-[source sgn_a `lub` source sgn_b], Just tgt<-[target sgn_a `lub` target sgn_b] ])
    PIsc _ a b -> DSet (Set.fromList [ DSgn (DIsc (s a, s b)) (Sign src tgt) | sgn_a<-signatures a, sgn_b<-signatures b, Just src<-[source sgn_a `lub` source sgn_b], Just tgt<-[target sgn_a `lub` target sgn_b] ])
    PUni _ a b -> DSet (Set.fromList [ DSgn (DUni (s a, s b)) (Sign src tgt) | sgn_a<-signatures a, sgn_b<-signatures b, Just src<-[source sgn_a `lub` source sgn_b], Just tgt<-[target sgn_a `lub` target sgn_b] ])
    PDif _ a b -> DSet (Set.fromList [ DSgn (DDif (s a, s b)) (Sign src tgt) | sgn_a<-signatures a, sgn_b<-signatures b, Just src<-[source sgn_a `lub` source sgn_b], Just tgt<-[target sgn_a `lub` target sgn_b] ])
    PLrs _ a b -> DLrs (s a, s b)
    PRrs _ a b -> DPrs (s a, s b)
    PDia _ a b -> DDia (s a, s b)
    PCps _ a b -> DSet (Set.fromList [ DSgn (DCps (s a, s b)) (Sign (source sgn_a) (target sgn_b)) | sgn_a<-signatures a, sgn_b<-signatures b, Just between<-[target sgn_a `lub` source sgn_b] ])
    PRad _ a b -> DSet (Set.fromList [ DSgn (DRad (s a, s b)) (Sign (source sgn_a) (target sgn_b)) | sgn_a<-signatures a, sgn_b<-signatures b, Just between<-[target sgn_a `lub` source sgn_b] ])
    PPrd _ a b -> DPrd (s a, s b)
    PKl0 _ e   -> DKl0 (s e)
    PKl1 _ e   -> DKl1 (s e)
    PFlp _ e   -> DFlp (s e)
    PCpl _ e   -> DCpl (s e)
    PBrk _ e   -> s e

{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.ADL1 (module X) 
where
import DatabaseDesign.Ampersand.Core.ParseTree as X (
           PPurpose(..), PRef2Obj(..)
         , Paire, Pairs, mkPair , srcPaire, trgPaire
         , Label(..)
         , FilePos(..), Origin(..), Pos(..), Traced(..)
         , Prop(..)
         , P_Concept(..)
         , P_Sign(..)
         , P_Context(..)
         , Meta(..)
         , MetaObj(..)
         , P_Process(..), P_RoleRelation(..), RoleRule(..)
         , P_Pattern(..)
         , PairView(..), PairViewSegment(..)
         , SrcOrTgt(..)
         , P_Rule(..)
         , P_IdentDef(..), P_IdentSegment(..)
         , P_ViewDef, P_ViewSegment
         , P_Population(..)
         , P_ObjectDef
         , P_Interface(..)
         , P_SubInterface
         , Term(..)
         , TermPrim(..)
         , P_Gen(..)
         , P_Declaration(..)
         , ConceptDef(..)
         , gen_concs
         )
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree as X  (
          A_Concept(..)
         ,Sign(..),showSign,GenR()
         , (<==>),meet,join
         ,Signaling(..)
         ,A_Context(..),Process(..)
         ,Association(..)
         ,Expression(..)
         ,A_Gen(..)
         ,IdentityDef(..)
         ,IdentitySegment(..)
         ,ViewDef(..)
         ,ViewSegment(..)
         ,ObjectDef(..)
         ,objAts
         ,objatsLegacy
         ,SubInterface(..)
         ,Declaration(..),decusr
         ,Interface(..)
         ,Pattern(..)
         ,Rule(..)
         ,RuleType(..)
         ,RoleRelation(..)
         ,Population(..)
         ,Purpose(..), ExplObj(..)
         , (.==.), (.|-.), (./\.), (.\/.), (.-.), (./.), (.\.), (.:.), (.!.), (.*.)
         )
import DatabaseDesign.Ampersand.ADL1.Expression as X (
         notCpl, isCpl, deMorganERad, deMorganECps, deMorganEUni, deMorganEIsc)
import DatabaseDesign.Ampersand.ADL1.ECArule as X (
         isAll, isCHC, isBlk, isNop, isDo, dos)
import DatabaseDesign.Ampersand.ADL1.Rule as X (
          rulefromProp, isaRule, ruleviolations, violationsexpr
         ,consequent,antecedent,hasantecedent)

{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.ADL1
           ( P_Context(..), A_Context(..)
           , P_Meta(..), Meta(..)
           , MetaObj(..)
           , Process(..), P_Process(..), P_RoleRelation(..),RoleRelation(..), RoleRule(..)
           , P_Pattern(..), Pattern(..)
           , P_PairView(..), P_PairViewSegment(..), PairView(..), PairViewSegment(..) 
           , SrcOrTgt(..)
           , P_Rule(..), Rule(..), consequent, rulefromProp, ruleviolations, violationsexpr
           , P_IndDef(..), P_IndSegment(..), IdentityDef(..), IdentitySegment(..)
           , P_ViewDef(..), P_ViewSegment(..), ViewDef(..), ViewSegment(..)
           , P_Population(..), UserDefPop(..)
           , P_ObjectDef(..)
           , ObjectDef(..), P_Interface(..)
           , objAts, objatsLegacy
           , P_SubInterface(..), SubInterface(..)
           , Interface(..)
           , Term(..),Expression(..) 
           , A_Gen(..),P_Gen(..)
           , Declaration(..),decusr
           , P_Declaration(..)
           , ConceptDef(..)
           , P_Concept(..), A_Concept(..), (<==>),meet,join, P_Sign(..), Sign(..), showSign, GenR
           , RuleType(..)
           , Prop(..)
           , isaRule
           , FilePos(..), Origin(..), Pos(..), Traced(..)
           , iExpr, vExpr
           , antecedent,hasantecedent,notCpl,isCpl,deMorgan
           , Signaling(..)
           , Association(..)
           , Label(..)
           , Paire, Pairs, mkPair , srcPaire, trgPaire
           , isAll, isCHC, isBlk, isNop, isDo, dos
           , PPurpose(..), PRef2Obj(..), ExplObj(..)
           , Purpose(..)
           , (.==.), (.|-.), (./\.), (.\/.), (.-.), (./.), (.\.), (.:.), (.!.), (.*.)
           )
where
   import DatabaseDesign.Ampersand.Core.ParseTree
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree         (
                                          A_Concept(..)
                                         ,Meta(..)
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
                                         ,PairView(..)
                                         ,PairViewSegment(..) 
                                         ,Rule(..)
                                         ,RuleType(..)
                                         ,RoleRelation(..)
                                         ,UserDefPop(..)
                                         ,Purpose(..), ExplObj(..)
                                         , iExpr, vExpr
                                         , (.==.), (.|-.), (./\.), (.\/.), (.-.), (./.), (.\.), (.:.), (.!.), (.*.)
                                         )
   import DatabaseDesign.Ampersand.ADL1.Expression                 (
                                          notCpl, isCpl, deMorgan)
   import DatabaseDesign.Ampersand.ADL1.ECArule                    (isAll, isCHC, isBlk, isNop, isDo, dos)
   import DatabaseDesign.Ampersand.ADL1.Rule                       (
                                          rulefromProp, isaRule, ruleviolations, violationsexpr
                                         ,consequent,antecedent,hasantecedent)

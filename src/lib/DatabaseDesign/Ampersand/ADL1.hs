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
           , P_KeyDef(..), P_KeySegment(..), KeyDef(..), KeySegment(..)
           , P_Population(..), UserDefPop(..)
           , P_ObjectDef(..)
           , ObjectDef(..), P_Interface(..)
           , objAts, objatsLegacy
           , P_SubInterface(..), SubInterface(..)
           , Interface(..)
           , Term(..),Expression(..) 
           , A_Gen(..),P_Gen(..)
           , Relation(..), makeRelation
           , Declaration(..)
           , P_Relation(..),P_Declaration(..)
           , ConceptDef(..)
           , P_Concept(..), A_Concept(..), (<==>),meet,order,join, P_Sign(..), Sign(..), GenR
           , RuleType(..)
           , Prop(..),allprops,endoprops
           , isaRule
           , FilePos(..), Origin(..), Pos(..), Traced(..)
           , makeDeclaration
           , antecedent,hasantecedent,notCpl,isCpl
           , Signaling(..)
           , Association(..), Relational(..)
           , Label(..)
           , Paire, Pairs, mkPair , srcPaire, trgPaire
           , isAll, isChc, isBlk, isNop, isDo, dos
           , PPurpose(..), PRef2Obj(..), ExplObj(..)
           , Purpose(..)
           
           )
where
   import DatabaseDesign.Ampersand.Core.ParseTree
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree         (
                                          A_Concept(..)
                                         ,Meta(..)
                                         ,Sign(..),GenR()
                                         , (<==>),meet,order,join
                                         ,Signaling(..)
                                         ,A_Context(..),Process(..)
                                         ,Association(..)
                                         ,Expression(..)
                                         ,A_Gen(..)
                                         ,KeyDef(..)
                                         ,KeySegment(..)
                                         ,ObjectDef(..)
                                         ,objAts
                                         ,objatsLegacy
                                         ,SubInterface(..)
                                         ,Declaration(..)
                                         ,Interface(..)
                                         ,Pattern(..)
                                         ,PairView(..)
                                         ,PairViewSegment(..) 
                                         ,Relation(..)
                                         ,Rule(..)
                                         ,RuleType(..)
                                         ,RoleRelation(..)
                                         ,UserDefPop(..)
                                         ,Purpose(..), ExplObj(..)
                                         ,makeDeclaration
                                         )
   import DatabaseDesign.Ampersand.ADL1.Expression                 (
                                          notCpl, isCpl)
   import DatabaseDesign.Ampersand.ADL1.MorphismAndDeclaration     (
                                          Relational(..)
                                         ,makeRelation
                                         )
   import DatabaseDesign.Ampersand.ADL1.ECArule                    (isAll, isChc, isBlk, isNop, isDo, dos)
   import DatabaseDesign.Ampersand.ADL1.Prop                       (allprops,endoprops)
   import DatabaseDesign.Ampersand.ADL1.Rule                       (
                                          rulefromProp, isaRule, ruleviolations, violationsexpr
                                         ,consequent,antecedent,hasantecedent)

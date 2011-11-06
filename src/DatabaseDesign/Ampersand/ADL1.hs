{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.ADL1
           ( P_Architecture(..),Architecture(..)
           , P_Context(..), A_Context(..)
           , Process(..), P_Process(..), P_RoleRelation(..),RoleRelation(..), RoleRule(..)
           , P_Pattern(..), Pattern(..)
           , P_Rule(..), Rule(..), consequent, rulefromProp, ruleviolations
           , P_KeyDef(..), P_KeyDefs, KeyDef(..)
           , P_Population(..), Population(..)
           , P_ObjectDef(..), P_ObjectDefs
           , ObjectDef(..), P_Interface(..), Interface(..)
           , P_Expression(..),Expression(..), subst, subsi, foldlMapExpression, foldrMapExpression
           , showExpr, isPos, isNeg, insParentheses, isFc, isFd, isEIsc, isEUni
           , A_Gen(..),P_Gen(..), P_Gens
           , Relation(..), flp, isTypeable, makeRelation
           , Declaration(..), flpDecl, isSgn
           , P_Relation(..),P_Declaration(..)
           , ConceptDef(..), ConceptDefs
           , P_Concept(..), A_Concept(..), Conceptual(..), comparable,lub,order,glb, P_Sign(..), Sign(..), GenR, newAcpt, cptos'
           , RuleType(..)
           , Prop(..),allprops,endoprops,flipProps
           , isaRule
           , FilePos(..), Origin(..), Pos(..), Traced(..)
           , makeDeclaration
           , antecedent, notCpl, isCpl
           , Signaling(..)
           , Association(..), Relational(..)
           , Label(..)
           , Paire, Pairs, srcPaire, trgPaire, mkPair, clos1
           , isAll, isChc, isBlk, isNop, isDo, dos
           , PExplanation(..), PExplObj(..), ExplObj(..)
           , Explanation(..)
           
           )
where
   import DatabaseDesign.Ampersand.Core.ParseTree
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree         (
                                          A_Concept(..)
                                         ,Sign(..),GenR()
                                         , comparable,lub,order,glb
                                         ,Signaling(..)
                                         ,A_Context(..),Process(..)
                                         ,Architecture(..)
                                         ,Expression(..)
                                         ,A_Gen(..)
                                         ,KeyDef(..)
                                         ,ObjectDef(..)
                                         ,Interface(..)
                                         ,Pattern(..)
                                         ,Rule(..)
                                         ,RuleType(..)
                                         ,RoleRelation(..)
                                         ,Population(..)
                                         ,Explanation(..), ExplObj(..)
                                         ,showExpr,insParentheses
                                         ,makeDeclaration
                                         )
   import DatabaseDesign.Ampersand.ADL1.Concept                    (Conceptual(..), newAcpt,cptos' )
   import DatabaseDesign.Ampersand.ADL1.Expression                 (flp,isTypeable,subst,subsi,foldlMapExpression,foldrMapExpression
                                         ,isPos,isNeg,isFc,isFd,isEIsc,isEUni,notCpl, isCpl)
   import DatabaseDesign.Ampersand.ADL1.MorphismAndDeclaration     (Relation(..)
                                         ,Association(..),Relational(..)
                                         ,Declaration(..),flpDecl
                                         ,isSgn
                                         ,makeRelation
                                         )
   import DatabaseDesign.Ampersand.ADL1.ECArule                    (isAll, isChc, isBlk, isNop, isDo, dos)
   import DatabaseDesign.Ampersand.ADL1.Prop                       (allprops,endoprops,flipProps)
   import DatabaseDesign.Ampersand.ADL1.Rule                       (
                                          rulefromProp, isaRule, ruleviolations
                                         ,consequent,antecedent)

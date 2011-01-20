{-# OPTIONS_GHC -Wall #-}
module Ampersand
           ( Architecture(..)
           , Context(..),Contexts,RoleService(..),RoleRelation(..)
           , Pattern(..),Patterns
           , Rule(..),mapRule,Rules,consequent, rulefromProp, ruleviolations
           , KeyDef(..),KeyDefs
           , Population(..),Populations
           , ObjectDef(..),ObjectDefs,Service(..),actions
           , Expression(..),mapExpression,foldlExpression,foldrExpression,PExpressions,PExpression(..)
           , Expressions,isPos,isNeg,idsOnly,insParentheses,UnOp(..),MulOp(..),isF,isFd,isFi,isFu,isI
           , Gen(..),Gens
           , Relation(..),mapMorphism,inline,makeMph
           , Declaration(..),isSgn
           , ConceptDef(..),ConceptDefs
           , Concept(..), Conceptual(..), SpecHierarchy(..), Sign, GenR, v,cptnew,cptS,cptos'
           , RuleType(..)
           , mIs
           , Prop(..)
           , isaRule
           , FilePos(..), Numbered(..)
           , makeDeclaration,ruleType,showSign,applyM
           , antecedent,notCp,cptAnything
           , Object(..)
           , ViewPoint(..)
           , explanationDeclarations
           , ConceptStructure(..),Signaling(..)
           , Association(..), Relational(..)
           , normExpr
           , Populated(..)
           , Substitutive(..)
           , Identified(..), uniqueNames
           , Label(..)
           , Paire,Pairs,srcPaire,trgPaire,mkPair
           , isAll, isChc, isBlk, isNop, isDo, InsDel(..), ECArule(..), Event(..), PAclause(..)
           , PExplanation(..),PExplObj(..), Explanations,ExplObj(..)
           , Explanation(..)
           )
where

   import DatabaseDesign.Ampersand.ADL.Concept                    (Concept(..),Conceptual(..),cptnew,cptS,cptAnything,cptos' 
                                         ,Sign,GenR()
                                         ,SpecHierarchy(..)
                                         ,Signaling(..))
   import DatabaseDesign.Ampersand.ADL.ConceptDef                 (ConceptDef(..),ConceptDefs)
   import DatabaseDesign.Ampersand.ADL.Context                    (Context(..),Contexts
                                         ,RoleService(..),RoleRelation(..)
                                         ,Architecture(..))
   import DatabaseDesign.Ampersand.ADL.Expression                 (Expression(..),mapExpression,foldlExpression,foldrExpression,PExpressions,PExpression(..),Expressions,UnOp(..),MulOp(..)
                                         ,isPos,isNeg,idsOnly,isF,isFd,isFi,isFu,isI,v,notCp,insParentheses)
   import DatabaseDesign.Ampersand.ADL.FilePos                    (FilePos(..)
                                         ,Numbered(..))
   import DatabaseDesign.Ampersand.ADL.Gen                        (Gen(..),Gens)
   import DatabaseDesign.Ampersand.ADL.KeyDef                     (KeyDef(..),KeyDefs)
   import DatabaseDesign.Ampersand.ADL.Label                      (Label(..))
   import DatabaseDesign.Ampersand.ADL.MorphismAndDeclaration     (Relation(..),mapMorphism
                                         ,Association(..),Relational(..)
                                         ,Declaration(..)
                                         ,Identified(..),uniqueNames
                                         ,isSgn
                                         ,makeMph,makeDeclaration
                                         ,inline
                                         ,mIs,showSign,applyM)
   import DatabaseDesign.Ampersand.ADL.ObjectDef                  (ObjectDef(..),ObjectDefs,Service(..),actions)
   import DatabaseDesign.Ampersand.ADL.ECArule                    (isAll, isChc, isBlk, isNop, isDo, InsDel(..),ECArule(..),Event(..),PAclause(..))
   import DatabaseDesign.Ampersand.ADL.Pair                       (Paire,Pairs,srcPaire,trgPaire,mkPair)
                                         
   import Classes.Populated              (Populated(..))
   import DatabaseDesign.Ampersand.ADL.Pattern                    (Pattern(..),Patterns)
   import DatabaseDesign.Ampersand.ADL.Explanation                (Explanation(..), PExplanation(..),PExplObj(..),Explanations,ExplObj(..))
   import DatabaseDesign.Ampersand.ADL.Population                 (Population(..),Populations)
   import DatabaseDesign.Ampersand.ADL.Prop                       (Prop(..))
   import DatabaseDesign.Ampersand.ADL.Rule                       (Rule(..),mapRule,Rules
                                         ,RuleType(..),rulefromProp, isaRule, ruleviolations
                                         ,consequent,antecedent,ruleType
                                         ,normExpr)
   import Classes.ConceptStructure              (ConceptStructure(..))
   import Classes.Substitutive           (Substitutive(..))
   import Classes.Object                 (Object(..))
   import Classes.ViewPoint              (ViewPoint(..))
   import Classes.Explainable            (explanationDeclarations)


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

   import ADL.Concept                    (Concept(..),Conceptual(..),cptnew,cptS,cptAnything,cptos' 
                                         ,Sign,GenR()
                                         ,SpecHierarchy(..)
                                         ,Signaling(..))
   import ADL.ConceptDef                 (ConceptDef(..),ConceptDefs)
   import ADL.Context                    (Context(..),Contexts
                                         ,RoleService(..),RoleRelation(..)
                                         ,Architecture(..))
   import ADL.Expression                 (Expression(..),mapExpression,foldlExpression,foldrExpression,PExpressions,PExpression(..),Expressions,UnOp(..),MulOp(..)
                                         ,isPos,isNeg,idsOnly,isF,isFd,isFi,isFu,isI,v,notCp,insParentheses)
   import ADL.FilePos                    (FilePos(..)
                                         ,Numbered(..))
   import ADL.Gen                        (Gen(..),Gens)
   import ADL.KeyDef                     (KeyDef(..),KeyDefs)
   import ADL.Label                      (Label(..))
   import ADL.MorphismAndDeclaration     (Relation(..),mapMorphism
                                         ,Association(..),Relational(..)
                                         ,Declaration(..)
                                         ,Identified(..),uniqueNames
                                         ,isSgn
                                         ,makeMph,makeDeclaration
                                         ,inline
                                         ,mIs,showSign,applyM)
   import ADL.ObjectDef                  (ObjectDef(..),ObjectDefs,Service(..),actions)
   import ADL.ECArule                    (isAll, isChc, isBlk, isNop, isDo, InsDel(..),ECArule(..),Event(..),PAclause(..))
   import ADL.Pair                       (Paire,Pairs,srcPaire,trgPaire,mkPair)
                                         
   import Classes.Populated              (Populated(..))
   import ADL.Pattern                    (Pattern(..),Patterns)
   import ADL.Explanation                (Explanation(..), PExplanation(..),PExplObj(..),Explanations,ExplObj(..))
   import ADL.Population                 (Population(..),Populations)
   import ADL.Prop                       (Prop(..))
   import ADL.Rule                       (Rule(..),mapRule,Rules
                                         ,RuleType(..),rulefromProp, isaRule, ruleviolations
                                         ,consequent,antecedent,ruleType
                                         ,normExpr)
   import Classes.ConceptStructure              (ConceptStructure(..))
   import Classes.Substitutive           (Substitutive(..))
   import Classes.Object                 (Object(..))
   import Classes.ViewPoint              (ViewPoint(..))
   import Classes.Explainable            (explanationDeclarations)


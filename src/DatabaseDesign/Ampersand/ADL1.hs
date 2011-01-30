{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.ADL1
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
           , Relation(..),mapMorphism,inline,makeRelation
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

   import DatabaseDesign.Ampersand.ADL1.Concept                    (Concept(..),Conceptual(..),cptnew,cptS,cptAnything,cptos' 
                                         ,Sign,GenR()
                                         ,SpecHierarchy(..)
                                         ,Signaling(..))
   import DatabaseDesign.Ampersand.ADL1.ConceptDef                 (ConceptDef(..),ConceptDefs)
   import DatabaseDesign.Ampersand.ADL1.Context                    (Context(..),Contexts
                                         ,RoleService(..),RoleRelation(..)
                                         ,Architecture(..))
   import DatabaseDesign.Ampersand.ADL1.Expression                 (Expression(..),mapExpression,foldlExpression,foldrExpression,PExpressions,PExpression(..),Expressions,UnOp(..),MulOp(..)
                                         ,isPos,isNeg,idsOnly,isF,isFd,isFi,isFu,isI,v,notCp,insParentheses)
   import DatabaseDesign.Ampersand.ADL1.FilePos                    (FilePos(..)
                                         ,Numbered(..))
   import DatabaseDesign.Ampersand.ADL1.Gen                        (Gen(..),Gens)
   import DatabaseDesign.Ampersand.ADL1.KeyDef                     (KeyDef(..),KeyDefs)
   import DatabaseDesign.Ampersand.ADL1.Label                      (Label(..))
   import DatabaseDesign.Ampersand.ADL1.MorphismAndDeclaration     (Relation(..),mapMorphism
                                         ,Association(..),Relational(..)
                                         ,Declaration(..)
                                         ,Identified(..),uniqueNames
                                         ,isSgn
                                         ,makeRelation,makeDeclaration
                                         ,inline
                                         ,mIs,showSign,applyM)
   import DatabaseDesign.Ampersand.ADL1.ObjectDef                  (ObjectDef(..),ObjectDefs,Service(..),actions)
   import DatabaseDesign.Ampersand.ADL1.ECArule                    (isAll, isChc, isBlk, isNop, isDo, InsDel(..),ECArule(..),Event(..),PAclause(..))
   import DatabaseDesign.Ampersand.ADL1.Pair                       (Paire,Pairs,srcPaire,trgPaire,mkPair)
                                         
   import Classes.Populated              (Populated(..))
   import DatabaseDesign.Ampersand.ADL1.Pattern                    (Pattern(..),Patterns)
   import DatabaseDesign.Ampersand.ADL1.Explanation                (Explanation(..), PExplanation(..),PExplObj(..),Explanations,ExplObj(..))
   import DatabaseDesign.Ampersand.ADL1.Population                 (Population(..),Populations)
   import DatabaseDesign.Ampersand.ADL1.Prop                       (Prop(..))
   import DatabaseDesign.Ampersand.ADL1.Rule                       (Rule(..),mapRule,Rules
                                         ,RuleType(..),rulefromProp, isaRule, ruleviolations
                                         ,consequent,antecedent,ruleType
                                         ,normExpr)
   import Classes.ConceptStructure              (ConceptStructure(..))
   import Classes.Substitutive           (Substitutive(..))
   import Classes.Object                 (Object(..))
   import Classes.ViewPoint              (ViewPoint(..))
   import Classes.Explainable            (explanationDeclarations)


{-# OPTIONS_GHC -Wall #-}
module Adl ( Architecture(..)
           , Context(..),Contexts
           , Pattern(..),Patterns,union
           , Rule(..),Rules,consequent, rulefromProp, ruleviolations
           , KeyDef(..),KeyDefs,Key(..)
           , Population(..),Populations
           , ObjectDef(..),ObjectDefs,actions
           , Expression(..),PExpressions,Expressionx(..),PExpression(..), Expressions,isPos,isNeg,insParentheses, uniquemphs,UnOp(..),MulOp(..)
           , Gen(..),Gens
           , Morphism(..),Morphisms,inline,makeMph,makeInline
           , Declaration(..),Declarations,isSgn
           , ConceptDef(..),ConceptDefs
           , Concept(..), Sign, GenR, Concepts, v,cptnew,cptS
           , RuleType(..)
           , Prop(..)
           , isaRule
           , FilePos(..), Numbered(..)
           , makeDeclaration,mIs,ruleType,isProperty,applyM
           , antecedent,notCp,cptAnything
           , Object(..)
           , ViewPoint(..)
           , UserExplainable(..)
           , Morphical(..)
           , Association(..)
           , Morphic(..),normExpr
           , MorphicId(..)
           , Populated(..)
           , Substitutive(..)
           , Identified(..)
           , SelfExplained(..)
           , Label(..)
           , Paire,Pairs,srcPaire,trgPaire,mkPair
           , InsDel(..)
           , ECArule(..)
           , Event(..)
           , PAclause(..)
           , PExplanation(..)
           , Explanation(..)
           )
where

   import Adl.Concept                    (Concept(..),Concepts,cptnew,cptS,cptAnything
                                         ,Sign,GenR()
                                         ,Association(..)
                                         ,MorphicId(..),Morphic(..))
   import Adl.ConceptDef                 (ConceptDef(..),ConceptDefs)
   import Adl.Context                    (Context(..),Contexts
                                         ,Architecture(..))
   import Adl.Expression                 (Expression(..),PExpressions,Expressionx(..),PExpression(..),Expressions,UnOp(..),MulOp(..)
                                         ,isPos,isNeg,v,notCp,insParentheses, uniquemphs)
   import Adl.FilePos                    (FilePos(..)
                                         ,Numbered(..))
   import Adl.Gen                        (Gen(..),Gens)
   import Adl.KeyDef                     (KeyDef(..),KeyDefs)
   import Adl.Label                      (Label(..))
   import Adl.MorphismAndDeclaration     (Morphism(..),Morphisms
                                         ,Declaration(..),Declarations
                                         ,isSgn
                                         ,makeMph,makeDeclaration
                                         ,inline,makeInline
                                         ,mIs,isProperty,applyM)
   import Adl.ObjectDef                  (ObjectDef(..),ObjectDefs,actions)
   import Adl.ECArule                    (InsDel(..),ECArule(..),Event(..),PAclause(..))
   import Adl.Pair                       (Paire,Pairs,srcPaire,trgPaire,mkPair)
                                         
   import Classes.Populated              (Populated(..))
   import Adl.Pattern                    (Pattern(..),Patterns,union)
   import Adl.Explanation                (Explanation(..), PExplanation(..))
   import Adl.Population                 (Population(..),Populations)
   import Adl.Prop                       (Prop(..))
   import Adl.Rule                       (Rule(..),Rules
                                         ,RuleType(..),rulefromProp, isaRule, ruleviolations
                                         ,consequent,antecedent,ruleType
                                         ,normExpr)
   import Classes.Morphical              (Morphical(..))
   import Classes.Substitutive           (Substitutive(..))
   import Classes.Key                    (Key(..))
   import Classes.Object                 (Object(..))
   import Classes.ViewPoint              (ViewPoint(..))
   import Classes.Explainable            (UserExplainable(..))
   import CommonClasses                  (Identified(..)
                                         ,SelfExplained(..))


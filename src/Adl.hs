{-# OPTIONS_GHC -Wall #-}
module Adl ( Architecture(..)
           , Context(..),Contexts
           , Pattern(..),Patterns,union
           , Rule(..),Rules,consequent
           , KeyDef(..),KeyDefs,Key(..)
           , Population(..),Populations
           , ObjectDef(..),ObjectDefs
           , Expression(..), Expressions,isPos,isNeg,insParentheses
           , Gen(..),Gens
           , Morphism(..),Morphisms,inline,makeMph,makeInline
           , Declaration(..),Declarations,isSgn
           , ConceptDef(..),ConceptDefs
           , Concept(..), GenR, Concepts, v,cptnew,cptS
           , RuleType(..)
           , Prop(..)
           , multRules
           , FilePos(..), Numbered(..)
           , makeDeclaration,mIs,ruleType,isProperty
           , antecedent,notCp,cptAnything
           , Object(..)
           , ViewPoint(..)
           , Morphical(..)
           , Association(..)
           , Morphic(..),normExpr
           , MorphicId(..)
           , Populated(..)
           , Substitutive(..)
           , Identified(..)
           , Explained(..)
           , Label(..)
           , Paire,Pairs,srcPaire,trgPaire,mkPaire
           , InsDel(..)
           , ECArule(..)
           , Event(..)
           , PAclause(..)
           )
where

   import Adl.Concept                    (Concept(..),Concepts,cptnew,cptS,cptAnything
                                         ,GenR()
                                         ,Association(..)
                                         ,MorphicId(..),Morphic(..))
   import Adl.ConceptDef                 (ConceptDef(..),ConceptDefs)
   import Adl.Context                    (Context(..),Contexts
                                         ,Architecture(..))
   import Adl.Expression                 (Expression(..),Expressions
                                         ,isPos,isNeg,v,notCp,insParentheses)
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
                                         ,mIs,isProperty)
   import Adl.ObjectDef                  (ObjectDef(..),ObjectDefs)
   import Adl.ECArule                    (InsDel(..),ECArule(..),Event(..),PAclause(..))
   import Adl.Pair                       (Paire,Pairs,srcPaire,trgPaire,mkPaire)
                                         
   import Classes.Populated              (Populated(..))
   import Adl.Pattern                    (Pattern(..),Patterns,union)
   import Adl.Population                 (Population(..),Populations)
   import Adl.Prop                       (Prop(..))
   import Adl.Rule                       (Rule(..),Rules
                                         ,RuleType(..),multRules
                                         ,consequent,antecedent,ruleType
                                         ,normExpr)
   import Classes.Morphical              (Morphical(..))
   import Classes.Substitutive           (Substitutive(..))
   import Classes.Key                    (Key(..))
   import Classes.Object                 (Object(..))
   import Classes.ViewPoint              (ViewPoint(..))
   import CommonClasses                  (Identified(..)
                                         ,Explained(..))




  

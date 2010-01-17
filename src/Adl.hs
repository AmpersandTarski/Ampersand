{-# OPTIONS_GHC -Wall #-}
module Adl ( Architecture(..)
           , Context(..),Contexts
           , Pattern(..),Patterns,union
           , Rule(..),Rules,consequent, rulefromProp
           , KeyDef(..),KeyDefs,Key(..)
           , Population(..),Populations
           , ObjectDef(..),ObjectDefs
           , Expression(..), Expressions,isPos,isNeg,insParentheses
           , Gen(..),Gens
           , Morphism(..),Morphisms,inline,makeMph,makeInline
           , Declaration(..),Declarations,isSgn
           , ConceptDef(..),ConceptDefs
           , Concept(..), Sign, GenR, Concepts, v,cptnew,cptS
           , RuleType(..)
           , Prop(..)
           , multRules, isaRule
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
           , Paire,Pairs,srcPaire,trgPaire,mkPair
           , InsDel(..)
           , ECArule(..)
           , Event(..)
           , PAclause(..)
           , FPA(..), FPcompl(..)
           )
where

   import Adl.Concept                    (Concept(..),Concepts,cptnew,cptS,cptAnything
                                         ,Sign,GenR()
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
   import Adl.Pair                       (Paire,Pairs,srcPaire,trgPaire,mkPair)
                                         
   import Classes.Populated              (Populated(..))
   import Adl.Pattern                    (Pattern(..),Patterns,union)
   import Adl.Population                 (Population(..),Populations)
   import Adl.Prop                       (Prop(..))
   import Adl.Rule                       (Rule(..),Rules
                                         ,RuleType(..),multRules,rulefromProp, isaRule
                                         ,consequent,antecedent,ruleType
                                         ,normExpr)
   import Classes.Morphical              (Morphical(..))
   import Classes.Substitutive           (Substitutive(..))
   import Classes.Key                    (Key(..))
   import Classes.Object                 (Object(..))
   import Classes.ViewPoint              (ViewPoint(..))
   import CommonClasses                  (Identified(..)
                                         ,Explained(..))

-------------- Function Points ------------------
   data FPA = ILGV FPcompl -- bevat permanente, voor de gebruiker relevante gegevens. De gegevens worden door het systeem gebruikt en onderhouden. Onder "onderhouden" verstaat FPA het toevoegen, wijzigen of verwijderen van gegevens.
            | KGV  FPcompl -- bevat permanente, voor de gebruiker relevante gegevens. Deze gegevens worden door het systeem gebruikt, maar worden door een ander systeem onderhouden (voor dat andere systeem is het dus een ILGV).
            | IF   FPcompl -- verwerkt gegevens in een ILGV van het systeem. (dus create, update en delete functies)
            | UF   FPcompl -- presenteert gegevens uit het systeem. Voorbeelden: het afdrukken van alle debiteuren; het aanmaken van facturen; het aanmaken van een diskette met betalingsopdrachten; het medium is hierbij niet van belang: papier, scherm, magneetband, datacom, enzovoorts.
            | OF   FPcompl -- is een speciaal (eenvoudig) soort uitvoerfunctie. Een opvraagfunctie presenteert gegevens uit het systeem op basis van een uniek identificerend zoekgegeven, waarbij geen aanvullende bewerkingen (zoals berekeningen of het bijwerken van een gegevensverzameling) plaats hebben. Voorbeeld: Het tonen van de gegevens van de klant met klantnummer 123456789.
            | NO           -- een onderdeel waaraan geen functiepunten worden toegekend.
              deriving (Eq, Show)

   data FPcompl = Eenvoudig | Gemiddeld | Moeilijk deriving (Eq, Show)

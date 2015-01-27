{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Database.Design.Ampersand.Test.Parser.ArbitraryTree () where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers
import Data.Char
import Control.Applicative

import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.Core.ParseTree

instance Arbitrary PandocFormat where
    arbitrary = elements [HTML,ReST,LaTeX,Markdown]

instance Arbitrary A_Context where
    arbitrary = ACtx <$> identifier              -- ^ The name of this context
         <*> elements [[]] -- [Origin]           -- ^ The origin of the context. A context can be a merge of a file including other files c.q. a list of Origin.
         <*> elements [English,Dutch]            -- ^ The default language used in this context.
         <*> arbitrary -- ^ The default markup format for free text in this context (currently: LaTeX, ...)
         <*> arbitrary                           -- ^ Names of patterns/processes to be printed in the functional specification. (For partial documents.)
         <*> elements [[]] -- [Pattern]        -- ^ The patterns defined in this context
         <*> elements [[]] -- [Process]        -- ^ The processes defined in this context
         <*> elements [[]] -- [Rule]           -- ^ All user defined rules in this context, but outside patterns and outside processes
         <*> elements [[]] -- [Declaration]    -- ^ The relations that are declared in this context, outside the scope of patterns
         <*> elements [[]] -- [Population]     -- ^ The user defined populations of relations defined in this context, including those from patterns and processes
         <*> elements [[]] -- [ConceptDef]     -- ^ The concept definitions defined in this context, including those from patterns and processes
         <*> elements [[]] -- [IdentityDef]    -- ^ The identity definitions defined in this context, outside the scope of patterns
         <*> elements [[]] -- [ViewDef]        -- ^ The view definitions defined in this context, outside the scope of patterns
         <*> elements [[]] -- [A_Gen]          -- ^ The specialization statements defined in this context, outside the scope of patterns
         <*> elements [[]] -- [[A_Concept]]    -- ^ A partitioning of all concepts: the union of all these concepts contains all atoms, and the concept-lists are mutually distinct in terms of atoms in one of the mentioned concepts
         <*> elements [[]] -- [Interface]      -- ^ The interfaces defined in this context
         <*> elements [[]] -- [Purpose]        -- ^ The purposes of objects defined in this context, outside the scope of patterns and processes
         <*> elements [[]] -- [ObjectDef]      -- ^ user defined sqlplugs, taken from the Ampersand script
         <*> elements [[]] -- [ObjectDef]      -- ^ user defined phpplugs, taken from the Ampersand script
         <*> elements [[]] -- [Meta]           -- ^ used for Pandoc authors (and possibly other things)
       where identifier = suchThat (listOf1 (elements (['a'..'z']++['A'..'Z']++['0'..'9']++"_"))) (\a -> isUpper (head a) && length a > 1)

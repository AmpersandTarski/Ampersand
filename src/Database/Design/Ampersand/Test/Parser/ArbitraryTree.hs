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
import Database.Design.Ampersand.Test.Parser.ArbitraryPandoc

fixed :: a -> Gen a
fixed x = elements [x]

none :: Gen [a]
none = elements [[]]

ascii :: Gen Char
ascii = elements (['a'..'z']++['A'..'Z']++['0'..'9']++"_")

str :: Gen String
str = listOf ascii

str1 :: Gen String
str1 = listOf1 ascii

str2 :: Gen String
str2 = suchThat str1 (\s -> length s > 1)

startUpper :: String -> Bool
startUpper = isUpper . head

instance Arbitrary PandocFormat where
    arbitrary = elements [HTML,ReST,LaTeX,Markdown]
   
instance Arbitrary Lang where
    arbitrary = elements [English,Dutch]

instance Arbitrary Origin where
    arbitrary = oneof [
        fixed OriginUnknown,
        Origin <$> arbitrary,
        -- FileLoc <$> (String, Pos, String),
        DBLoc <$> arbitrary]

instance Arbitrary A_Markup where
    arbitrary = A_Markup
        <$> arbitrary -- Language
        <*> arbitrary -- PandocFormat
        <*> listOf arbitrary -- [Block]

instance Arbitrary Pattern where
    arbitrary = A_Pat
        <$> str1           -- ^ Name of this pattern
        <*> arbitrary      -- ^ the position in the file in which this pattern was declared.
        <*> arbitrary      -- ^ the end position in the file, elements with a position between pos and end are elements of this pattern.
        <*> none       --   , ptrls :: [Rule]        -- ^ The user defined rules in this pattern
        <*> none       --   , ptgns :: [A_Gen]       -- ^ The generalizations defined in this pattern
        <*> none       --   , ptdcs :: [Declaration] -- ^ The relations that are declared in this pattern
        <*> none       --   , ptups :: [Population]  -- ^ The user defined populations in this pattern
        <*> none       --   , ptids :: [IdentityDef] -- ^ The identity definitions defined in this pattern
        <*> none       --   , ptvds :: [ViewDef]     -- ^ The view definitions defined in this pattern
        <*> none       --   , ptxps :: [Purpose]     -- ^ The purposes of elements defined in this pattern

instance Arbitrary A_Context where
    arbitrary = ACtx
         <$> identifier        -- ^ The name of this context
         <*> listOf arbitrary  -- ^ The origin of the context. A context can be a merge of a file including other files c.q. a list of Origin.
         <*> arbitrary         -- ^ The default language used in this context.
         <*> arbitrary         -- ^ The default markup format for free text in this context (currently: LaTeX, ...)
         <*> listOf arbitrary  -- ^ Names of patterns/processes to be printed in the functional specification. (For partial documents.)
         <*> listOf arbitrary  -- ^ The patterns defined in this context
         <*> none          -- [Process]        -- ^ The processes defined in this context
         <*> none          -- [Rule]           -- ^ All user defined rules in this context, but outside patterns and outside processes
         <*> none          -- [Declaration]    -- ^ The relations that are declared in this context, outside the scope of patterns
         <*> none          -- [Population]     -- ^ The user defined populations of relations defined in this context, including those from patterns and processes
         <*> none          -- [ConceptDef]     -- ^ The concept definitions defined in this context, including those from patterns and processes
         <*> none          -- [IdentityDef]    -- ^ The identity definitions defined in this context, outside the scope of patterns
         <*> none          -- [ViewDef]        -- ^ The view definitions defined in this context, outside the scope of patterns
         <*> none          -- [A_Gen]          -- ^ The specialization statements defined in this context, outside the scope of patterns
         <*> none          -- [[A_Concept]]    -- ^ A partitioning of all concepts: the union of all these concepts contains all atoms, and the concept-lists are mutually distinct in terms of atoms in one of the mentioned concepts
         <*> none          -- [Interface]      -- ^ The interfaces defined in this context
         <*> none          -- [Purpose]        -- ^ The purposes of objects defined in this context, outside the scope of patterns and processes
         <*> none          -- [ObjectDef]      -- ^ user defined sqlplugs, taken from the Ampersand script
         <*> none          -- [ObjectDef]      -- ^ user defined phpplugs, taken from the Ampersand script
         <*> none          -- [Meta]           -- ^ used for Pandoc authors (and possibly other things)
       where identifier = suchThat str2 startUpper

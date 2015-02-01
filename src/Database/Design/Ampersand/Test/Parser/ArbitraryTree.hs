{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Design.Ampersand.Test.Parser.ArbitraryTree () where

import Test.QuickCheck hiding (Prop)
import Data.Char
import Control.Applicative

import Database.Design.Ampersand.Core.ParseTree

-- Useful functions to build on the quick check ones
fixed :: a -> Gen a
fixed x = elements [x]

none :: Gen [a]
none = elements [[]]

not_implemented :: Gen a
not_implemented = elements []

ascii :: Gen Char
ascii = elements (['a'..'z']++['A'..'Z']++['0'..'9']++"_")

str :: Gen String
str = listOf ascii

str1 :: Gen String
str1 = listOf1 ascii

str2 :: Gen String
str2 = suchThat str1 (\s -> length s > 1)

identifier :: Gen String
identifier = suchThat str2 startUpper
    where startUpper = isUpper . head

--- Now the arbitrary instances
instance Arbitrary P_Context where
    arbitrary = not_implemented

instance Arbitrary Meta where
    arbitrary = not_implemented

instance Arbitrary MetaObj where
    arbitrary = not_implemented

instance Arbitrary P_Process where
    arbitrary = not_implemented

instance Arbitrary P_RoleRelation where
    arbitrary = not_implemented

instance Arbitrary RoleRule where
    arbitrary = not_implemented

instance Arbitrary P_Pattern where
    arbitrary = not_implemented

instance Arbitrary P_Declaration where
    arbitrary = not_implemented

instance Arbitrary (Term a) where
    arbitrary = not_implemented

instance Arbitrary TermPrim where
    arbitrary = not_implemented

instance Arbitrary (PairView a) where
    arbitrary = not_implemented

instance Arbitrary (PairViewSegment a) where
    arbitrary = not_implemented

instance Arbitrary (PairViewTerm a) where
    arbitrary = not_implemented

instance Arbitrary (PairViewSegmentTerm a) where
    arbitrary = not_implemented

instance Arbitrary SrcOrTgt where
    arbitrary = not_implemented

instance Arbitrary (P_Rule a) where
    arbitrary = not_implemented

instance Arbitrary ConceptDef where
    arbitrary = not_implemented

instance Arbitrary P_Population where
    arbitrary = not_implemented

instance Arbitrary P_Interface where
    arbitrary = not_implemented

instance Arbitrary (P_ObjDef a) where
    arbitrary = not_implemented

instance Arbitrary (P_SubIfc a) where
    arbitrary = not_implemented

instance Arbitrary P_IdentDef where
    arbitrary = not_implemented

instance Arbitrary P_IdentSegment where
    arbitrary = not_implemented

instance Arbitrary (P_ViewD a) where
    arbitrary = not_implemented

instance Arbitrary (P_ViewSegmt a) where
    arbitrary = not_implemented

instance Arbitrary PPurpose where
    arbitrary = not_implemented

instance Arbitrary PRef2Obj where
    arbitrary = not_implemented

instance Arbitrary PMeaning where
    arbitrary = not_implemented

instance Arbitrary PMessage where
    arbitrary = not_implemented

instance Arbitrary P_Concept where
    arbitrary = not_implemented

instance Arbitrary P_Sign where
    arbitrary = not_implemented

instance Arbitrary P_Gen where
    arbitrary = not_implemented

instance Arbitrary Lang where
    arbitrary = not_implemented

instance Arbitrary P_Markup where
    arbitrary = not_implemented

instance Arbitrary PandocFormat where
    arbitrary = not_implemented

instance Arbitrary Label where
    arbitrary = not_implemented

instance Arbitrary Prop where
    arbitrary = not_implemented

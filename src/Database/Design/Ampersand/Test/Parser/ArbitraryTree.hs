{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Design.Ampersand.Test.Parser.ArbitraryTree () where

import Test.QuickCheck hiding (Prop)
import Data.Char
import Control.Applicative

import Database.Design.Ampersand.Core.ParseTree

-- Useful functions to build on the quick check ones
none :: Gen [a]
none = return []

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
instance Arbitrary Origin where
    arbitrary = return OriginUnknown

instance Arbitrary P_Context where
    arbitrary = PCtx <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                     <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                     <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                     <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Meta where
    arbitrary = Meta <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary MetaObj where
    arbitrary = return ContextMeta

instance Arbitrary P_Process where
    arbitrary = P_Prc <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary P_RoleRelation where
    arbitrary = P_RR <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary RoleRule where
    arbitrary = Maintain <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary P_Pattern where
    arbitrary = P_Pat <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary

instance Arbitrary P_Declaration where
    arbitrary = P_Sgn <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Term a) where
    arbitrary = oneof [
           Prim <$> arbitrary,
           PEqu <$> arbitrary <*> arbitrary <*> arbitrary,
           PImp <$> arbitrary <*> arbitrary <*> arbitrary,
           PIsc <$> arbitrary <*> arbitrary <*> arbitrary,
           PUni <$> arbitrary <*> arbitrary <*> arbitrary,
           PDif <$> arbitrary <*> arbitrary <*> arbitrary,
           PLrs <$> arbitrary <*> arbitrary <*> arbitrary,
           PRrs <$> arbitrary <*> arbitrary <*> arbitrary,
           PDia <$> arbitrary <*> arbitrary <*> arbitrary,
           PCps <$> arbitrary <*> arbitrary <*> arbitrary,
           PRad <$> arbitrary <*> arbitrary <*> arbitrary,
           PPrd <$> arbitrary <*> arbitrary <*> arbitrary,
           PKl0 <$> arbitrary <*> arbitrary,
           PKl1 <$> arbitrary <*> arbitrary,
           PFlp <$> arbitrary <*> arbitrary,
           PCpl <$> arbitrary <*> arbitrary,
           PBrk <$> arbitrary <*> arbitrary
        ]

instance Arbitrary TermPrim where
    arbitrary = oneof [
           PI <$> arbitrary,
           Pid <$> arbitrary <*> arbitrary,
           Patm <$> arbitrary <*> arbitrary <*> arbitrary,
           PVee <$> arbitrary,
           Pfull <$> arbitrary <*> arbitrary <*> arbitrary,
           Prel <$> arbitrary <*> arbitrary,
           PTrel <$> arbitrary <*> arbitrary <*> arbitrary
        ]

instance Arbitrary a => Arbitrary (PairView a) where
    arbitrary = PairView <$> arbitrary

instance Arbitrary a => Arbitrary (PairViewSegment a) where
    arbitrary = oneof [
            PairViewText <$> arbitrary,
            PairViewExp <$> arbitrary <*> arbitrary
        ]

instance Arbitrary a => Arbitrary (PairViewTerm a) where
    arbitrary = PairViewTerm <$> arbitrary -- should be only (PairView (Term a))

instance Arbitrary a => Arbitrary (PairViewSegmentTerm a) where
    arbitrary = PairViewSegmentTerm <$> arbitrary -- should be only PairViewSegment (Term a)

instance Arbitrary SrcOrTgt where
    arbitrary = elements[Src, Tgt]

instance Arbitrary a => Arbitrary (P_Rule a) where
    arbitrary = P_Ru <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                     <*> arbitrary

instance Arbitrary ConceptDef where
    arbitrary = Cd <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                   <*> arbitrary <*> arbitrary

instance Arbitrary P_Population where
    arbitrary = oneof [
          P_RelPopu <$> arbitrary <*> arbitrary <*> arbitrary,
          P_TRelPop <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
          P_CptPopu <$> arbitrary <*> arbitrary <*> arbitrary
        ]

instance Arbitrary P_Interface where
    arbitrary = P_Ifc <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (P_ObjDef a) where
    arbitrary = P_Obj <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (P_SubIfc a) where
    arbitrary = oneof [
            P_Box          <$> arbitrary <*> arbitrary,
            P_InterfaceRef <$> arbitrary <*> arbitrary
        ]

instance Arbitrary P_IdentDef where
    arbitrary = P_Id <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary P_IdentSegment where
    arbitrary = P_IdentExp <$> arbitrary

instance Arbitrary a => Arbitrary (P_ViewD a) where
    arbitrary = P_Vd <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (P_ViewSegmt a) where
    arbitrary = oneof [
            P_ViewExp <$> arbitrary,
            P_ViewText <$> arbitrary,
            P_ViewHtml <$> arbitrary
        ]

instance Arbitrary PPurpose where
    arbitrary = PRef2 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary PRef2Obj where
    arbitrary = oneof [
            PRef2ConceptDef <$> arbitrary,
            PRef2Declaration <$> arbitrary,
            PRef2Rule <$> arbitrary,
            PRef2IdentityDef <$> arbitrary,
            PRef2ViewDef <$> arbitrary,
            PRef2Pattern <$> arbitrary,
            PRef2Process <$> arbitrary,
            PRef2Interface <$> arbitrary,
            PRef2Context <$> arbitrary,
            PRef2Fspc <$> arbitrary
        ]

instance Arbitrary PMeaning where
    arbitrary = PMeaning <$> arbitrary

instance Arbitrary PMessage where
    arbitrary = PMessage <$> arbitrary

instance Arbitrary P_Concept where
    arbitrary = oneof [
            PCpt <$> arbitrary,
            return P_Singleton
        ]

instance Arbitrary P_Sign where
    arbitrary = P_Sign <$> arbitrary <*> arbitrary

instance Arbitrary P_Gen where
    arbitrary = oneof [
            P_Cy <$> arbitrary <*> arbitrary <*> arbitrary,
            PGen <$> arbitrary <*> arbitrary <*> arbitrary
        ]

instance Arbitrary Lang where
    arbitrary = elements [Dutch, English]

instance Arbitrary P_Markup where
    arbitrary = P_Markup <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary PandocFormat where
    arbitrary = elements [HTML, ReST, LaTeX, Markdown]

instance Arbitrary Label where
    arbitrary = Lbl <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Prop where
    arbitrary = elements [Uni, Inj, Sur, Tot, Sym, Asy, Trn, Rfx, Irf, Aut]

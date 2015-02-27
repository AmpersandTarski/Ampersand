{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Design.Ampersand.Test.Parser.ArbitraryTree () where

import Test.QuickCheck
import Data.Char
import Control.Applicative

import Database.Design.Ampersand.Core.ParseTree
import Database.Design.Ampersand.Input.ADL1.Parser (keywordstxt)
import Database.Design.Ampersand.ADL1.Pair (Paire(..))

-- Useful functions to build on the quick check functions

-- Generates a simple ascii character
printable :: Gen Char
printable = suchThat arbitrary isPrint -- printable ASCII characters

-- Generates a simple string of ascii characters
safeStr :: Gen String
safeStr = listOf printable

-- Generates a simple non-empty string of ascii characters
safeStr1 :: Gen String
safeStr1 = listOf1 printable

-- Genrates a valid ADL identifier
identifier :: Gen String
identifier = suchThat str2 noKeyword
    where noKeyword x = x `notElem` keywordstxt
          idChar = elements (['a'..'z']++['A'..'Z']++['0'..'9']++"_")
          str2   = suchThat (listOf1 idChar) (\s -> length s > 1)

-- Genrates a valid ADL upper-case identifier
upper_id :: Gen String
upper_id = suchThat identifier startUpper
    where startUpper = isUpper . head

-- Genrates a valid ADL lower-case identifier
lower_id :: Gen String
lower_id = suchThat identifier startLower
    where startLower = isLower . head

-- Generates an object
objTermPrim :: Gen (P_ObjDef TermPrim)
objTermPrim = P_Obj <$> lower_id  <*> arbitrary <*> arbitrary <*> subIfc <*> listOf (listOf1 safeStr1)
    where subIfc :: Gen (Maybe (P_SubIfc TermPrim))
          subIfc = Just <$> arbitrary

--- Now the arbitrary instances
instance Arbitrary Origin where
    arbitrary = return OriginUnknown

instance Arbitrary P_Context where
    arbitrary = PCtx <$> upper_id  <*> arbitrary <*> arbitrary <*> arbitrary <*> listOf upper_id
                     <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                     <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                     <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Meta where
    arbitrary = Meta <$> arbitrary <*> arbitrary <*>  safeStr  <*> safeStr

instance Arbitrary MetaObj where
    arbitrary = return ContextMeta

instance Arbitrary P_Process where
    arbitrary = P_Prc <$> identifier<*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary P_RoleRelation where
    arbitrary = P_RR <$> listOf1 safeStr <*> listOf1 arbitrary <*> arbitrary

instance Arbitrary RoleRule where
    arbitrary = Maintain <$> listOf1 safeStr <*> listOf1 safeStr <*> arbitrary

instance Arbitrary P_Pattern where
    arbitrary = P_Pat <$> safeStr1  <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary

instance Arbitrary P_Declaration where
    arbitrary = P_Sgn <$> lower_id  <*> arbitrary <*> arbitrary <*> safeStr1  <*> safeStr1
                      <*> safeStr1  <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Term a) where
    arbitrary = sized genTerm

genTerm :: Arbitrary a => Int -> Gen (Term a)
genTerm n = oneof (options!!idx)
        where idx = 6 - (n`div`7)
              gen = genTerm (n `div` 2)
              options = [
                -- level 0
                [PEqu <$> arbitrary <*> gen <*> gen,
                 PImp <$> arbitrary <*> gen <*> gen],
                -- level 1
                [PIsc <$> arbitrary <*> gen <*> gen,
                 PUni <$> arbitrary <*> gen <*> gen],
                -- level 2
                [PDif <$> arbitrary <*> gen <*> gen],
                -- level 3
                [PLrs <$> arbitrary <*> gen <*> gen,
                 PRrs <$> arbitrary <*> gen <*> gen,
                 PDia <$> arbitrary <*> gen <*> gen],
                -- level 4
                [PCps <$> arbitrary <*> gen <*> gen,
                 PRad <$> arbitrary <*> gen <*> gen,
                 PPrd <$> arbitrary <*> gen <*> gen],
                -- level 5
                [PKl0 <$> arbitrary <*> gen,
                 PKl1 <$> arbitrary <*> gen,
                 PFlp <$> arbitrary <*> gen,
                 PCpl <$> arbitrary <*> gen],
                -- level 6
                [PBrk <$> arbitrary <*> gen,
                 Prim <$> arbitrary]
                ]

instance Arbitrary TermPrim where
    arbitrary = oneof [
           --TODO: PI, Pid, Patm, PVee, Pfull
           Prel  <$> arbitrary <*> lower_id,
           PTrel <$> arbitrary <*> lower_id <*> arbitrary
        ]

instance Arbitrary a => Arbitrary (PairView a) where
    arbitrary = PairView <$> listOf1 arbitrary

instance Arbitrary a => Arbitrary (PairViewSegment a) where
    arbitrary = oneof [
            PairViewText <$> safeStr,
            PairViewExp <$> arbitrary <*> arbitrary
        ]

instance Arbitrary a => Arbitrary (PairViewTerm a) where
    arbitrary = PairViewTerm <$> arbitrary -- should be only (PairView (Term a))

instance Arbitrary a => Arbitrary (PairViewSegmentTerm a) where
    arbitrary = PairViewSegmentTerm <$> arbitrary -- should be only PairViewSegment (Term a)

instance Arbitrary SrcOrTgt where
    arbitrary = elements[Src, Tgt]

instance Arbitrary a => Arbitrary (P_Rule a) where
    arbitrary = P_Ru <$> safeStr <*> ruleTerm  <*> arbitrary <*> arbitrary <*> arbitrary
                     <*> arbitrary
              where ruleTerm = sized genTerm `suchThat` isRuleTerm
                    isRuleTerm (PEqu _ _ _) = True
                    isRuleTerm (PImp _ _ _) = True
                    isRuleTerm _            = False

instance Arbitrary ConceptDef where
    arbitrary = Cd <$> arbitrary <*> safeStr <*> arbitrary <*> safeStr <*> safeStr
                   <*>  safeStr  <*> safeStr

instance Arbitrary Paire where
    arbitrary = Paire <$> arbitrary <*> arbitrary

instance Arbitrary P_Population where
    arbitrary = oneof [
          P_RelPopu <$> lower_id <*> arbitrary <*> arbitrary,
          P_TRelPop <$> lower_id <*> arbitrary <*> arbitrary <*> arbitrary,
          P_CptPopu <$> lower_id <*> arbitrary <*> listOf safeStr
        ]

instance Arbitrary P_Interface where
    arbitrary = P_Ifc <$> safeStr1   <*> maybeSafeStr  <*> arbitrary <*> args <*> listOf safeStr1
                      <*>objTermPrim <*> arbitrary     <*> safeStr
                   where args = listOf $ listOf1 safeStr
                         maybeSafeStr = oneof[Just <$> safeStr, return Nothing]

instance Arbitrary a => Arbitrary (P_ObjDef a) where
    arbitrary = P_Obj <$> lower_id  <*> arbitrary <*> arbitrary <*> arbitrary <*> args
              where args = listOf $ listOf1 safeStr1

instance Arbitrary a => Arbitrary (P_SubIfc a) where
    arbitrary = oneof [
            P_Box          <$> arbitrary <*> boxKey   <*> listOf1 arbitrary,
            P_InterfaceRef <$> arbitrary <*> safeStr1
        ]
        where boxKey = elements [Nothing,Just "ROWS",Just "COLS",Just "TABS"]

instance Arbitrary P_IdentDef where
    arbitrary = P_Id <$> arbitrary <*> safeStr <*> arbitrary <*> listOf1 arbitrary

instance Arbitrary P_IdentSegment where
    arbitrary = P_IdentExp <$> arbitrary

instance Arbitrary a => Arbitrary (P_ViewD a) where
    arbitrary = P_Vd <$> arbitrary <*> safeStr <*> arbitrary <*> listOf1 arbitrary

instance Arbitrary a => Arbitrary (P_ViewSegmt a) where
    arbitrary = oneof [
            P_ViewExp <$> arbitrary,
            P_ViewText <$> safeStr,
            P_ViewHtml <$> safeStr
        ]

instance Arbitrary PPurpose where
    arbitrary = PRef2 <$> arbitrary <*> arbitrary <*> arbitrary <*> listOf safeStr1

instance Arbitrary PRef2Obj where
    arbitrary = oneof [
            PRef2ConceptDef <$> safeStr,
            PRef2Declaration <$> arbitrary,
            PRef2Rule <$> upper_id,
            PRef2IdentityDef <$> upper_id,
            PRef2ViewDef <$> upper_id,
            PRef2Pattern <$> upper_id,
            PRef2Process <$> upper_id,
            PRef2Interface <$> upper_id,
            PRef2Context <$> upper_id
            -- The PRef2Fspc is not used in the parser.
            -- PRef2Fspc <$> upper_id
        ]

instance Arbitrary PMeaning where
    arbitrary = PMeaning <$> arbitrary

instance Arbitrary PMessage where
    arbitrary = PMessage <$> arbitrary

instance Arbitrary P_Concept where
    arbitrary = oneof [
            PCpt <$> upper_id
            -- TODO: return P_Singleton
        ]

instance Arbitrary P_Sign where
    arbitrary = P_Sign <$> arbitrary <*> arbitrary

instance Arbitrary P_Gen where
    arbitrary = oneof [
            P_Cy <$> concept <*> listOf1 arbitrary <*> arbitrary,
            PGen <$> concept <*> concept <*> arbitrary
        ]
        where concept = PCpt <$> upper_id

instance Arbitrary Lang where
    arbitrary = elements [Dutch, English]

instance Arbitrary P_Markup where
    arbitrary = P_Markup <$> arbitrary <*> arbitrary <*> safeStr

instance Arbitrary PandocFormat where
    arbitrary = elements [HTML, ReST, LaTeX, Markdown]

instance Arbitrary Label where
    arbitrary = Lbl <$> safeStr1 <*> arbitrary <*> listOf(listOf1 safeStr1)

instance Arbitrary Prop where
    arbitrary = elements [Uni, Inj, Sur, Tot, Sym, Asy, Trn, Rfx, Irf, Aut]

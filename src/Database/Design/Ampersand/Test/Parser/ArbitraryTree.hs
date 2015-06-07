{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Database.Design.Ampersand.Test.Parser.ArbitraryTree () where

import Test.QuickCheck
import Data.Char
import Control.Applicative

import Database.Design.Ampersand.Core.ParseTree
import Database.Design.Ampersand.Input.ADL1.Lexer (keywords)
import Database.Design.Ampersand.ADL1.Pair (Paire(..))

-- Useful functions to build on the quick check functions

-- Generates a simple ascii character
printable :: Gen Char
printable = suchThat arbitrary isValid
    where isValid x = isPrint x && x <= 'Ñ' -- printable ASCII characters

-- Generates a simple string of ascii characters
safeStr :: Gen String
safeStr = listOf printable

-- Generates a simple non-empty string of ascii characters
safeStr1 :: Gen String
safeStr1 = listOf1 printable

-- Genrates a valid ADL identifier
identifier :: Gen String
identifier = suchThat str2 noKeyword
    where noKeyword x = x `notElem` keywords
          -- The prelude functions accept Unicode characters
          idChar = elements (['a'..'z']++['A'..'Z']++['0'..'9']++"_")
          str2   = suchThat (listOf1 idChar) (\s -> length s > 1)

-- Genrates a valid ADL upper-case identifier
upperId :: Gen String
upperId = suchThat identifier startUpper
    where startUpper = isUpper . head

-- Genrates a valid ADL lower-case identifier
lowerId :: Gen String
lowerId = suchThat identifier startLower
    where startLower = isLower . head

-- Generates an object
objTermPrim :: Int -> Gen (P_ObjDef TermPrim)
objTermPrim 0 = objTermPrim 1 -- minimum of 1 sub interface
objTermPrim i =
  makeObj genPrim ifc genView i
    where ifc :: Int -> Gen (P_SubIfc TermPrim)
          ifc n = subIfc objTermPrim (n`div`2)
          --TODO: The view is never tested like this
          genView = return Nothing
          genPrim :: Gen TermPrim
          genPrim = PNamedR <$> relationRef

--TODO: refactor obj/ifc generators
genObj :: Arbitrary a => Int -> Gen (P_ObjDef a)
genObj = makeObj arbitrary genIfc (return Nothing)

makeObj :: Gen a -> (Int -> Gen (P_SubIfc a)) -> Gen (Maybe String) -> Int -> Gen (P_ObjDef a)
makeObj genPrim ifcGen genView n =
        P_Obj <$> lowerId  <*> arbitrary <*> term <*> genView <*> ifc <*> args
              where args = listOf $ listOf1 safeStr1
                    term = Prim <$> genPrim
                    ifc  = if n == 0 then return Nothing
                           else Just <$> ifcGen (n`div`2)

genIfc :: Arbitrary a => Int -> Gen (P_SubIfc a)
genIfc = subIfc genObj

subIfc :: (Int -> Gen (P_ObjDef a)) -> Int -> Gen (P_SubIfc a)
subIfc objGen n =
    if n == 0 then P_InterfaceRef <$> arbitrary <*> safeStr1
    else P_Box          <$> arbitrary <*> boxKey   <*> vectorOf n (objGen$ n`div`2)
    where boxKey = elements [Nothing, Just "ROWS", Just "COLS", Just "TABS"]

genPairs :: Gen Pairs
genPairs = listOf genPaire

genPaire :: Gen Paire
genPaire = Paire <$> safeStr <*> safeStr

--- Now the arbitrary instances
instance Arbitrary Origin where
    arbitrary = return OriginUnknown

instance Arbitrary P_Context where
    arbitrary = PCtx
       <$> upperId   -- name
       <*> listOf arbitrary  -- pos
       <*> arbitrary  -- lang
       <*> arbitrary  -- markup
       <*> listOf upperId -- themes
       <*> listOf arbitrary -- patterns
       <*> listOf arbitrary -- rules
       <*> listOf arbitrary -- relations
       <*> listOf arbitrary -- concepts
       <*> listOf arbitrary -- identities
       <*> return []        -- role rules
       <*> return []        -- role relations
       <*> listOf arbitrary -- views
       <*> listOf arbitrary -- gen definitions
       <*> listOf arbitrary -- interfaces
       <*> listOf arbitrary -- purposes
       <*> listOf arbitrary -- populations
       <*> listOf arbitrary -- sqlplugs
       <*> listOf arbitrary -- phpplugs
       <*> listOf arbitrary -- generic meta information

instance Arbitrary Meta where
    arbitrary = Meta <$> arbitrary <*> arbitrary <*>  safeStr  <*> safeStr

instance Arbitrary MetaObj where
    arbitrary = return ContextMeta

instance Arbitrary P_RoleRelation where
    arbitrary = P_RR <$> arbitrary <*> listOf1 arbitrary <*> listOf1 relationRef

instance Arbitrary P_RoleRule where
    arbitrary = Maintain <$> arbitrary <*> listOf1 arbitrary <*> listOf1 safeStr

instance Arbitrary Role where
    arbitrary = Role <$> safeStr

instance Arbitrary P_Pattern where
    arbitrary = P_Pat <$> arbitrary <*> safeStr1  <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary P_Declaration where
    arbitrary = P_Sgn <$> lowerId         -- name
                      <*> arbitrary       -- sign
                      <*> arbitrary       -- props
                      <*> listOf safeStr1 -- pragma. Should be tree, but the grammar allows more.
                      <*> arbitrary       -- meaning
                      <*> genPairs        -- pairs
                      <*> arbitrary       -- origin
                      <*> arbitrary       -- plug

instance Arbitrary a => Arbitrary (Term a) where
    arbitrary = do lv <- choose (0,6)
                   sized (genTerm lv)

genTerm :: Arbitrary a => Int -> Int -> Gen (Term a)
genTerm lv n = if n == 0
               then Prim <$> arbitrary
               else oneof options
    where gen :: Arbitrary a => Int -> Gen (Term a)
          gen l = genTerm l (n`div`2)
  
          options :: Arbitrary a => [Gen (Term a)]
          options = concat $ drop lv levels
          
          levels :: Arbitrary a => [[Gen (Term a)]]
          levels = [
            -- level 0: pRule
            [PEqu <$> arbitrary <*> gen 1 <*> gen 1,
             PImp <$> arbitrary <*> gen 1 <*> gen 1],
            -- level 1: pTerm
            [PIsc <$> arbitrary <*> gen 2 <*> gen 2,
             PUni <$> arbitrary <*> gen 2 <*> gen 2],
            -- level 2: pTrm2
            [PDif <$> arbitrary <*> gen 3 <*> gen 3],
            -- level 3: pTrm3
            [PLrs <$> arbitrary <*> gen 4 <*> gen 4,
             PRrs <$> arbitrary <*> gen 4 <*> gen 4,
             PDia <$> arbitrary <*> gen 4 <*> gen 4],
            -- level 4: pTrm4
            [PCps <$> arbitrary <*> gen 5 <*> gen 5,
             PRad <$> arbitrary <*> gen 5 <*> gen 5,
             PPrd <$> arbitrary <*> gen 5 <*> gen 5],
            -- level 5: pTrm5
            [PKl0 <$> arbitrary <*> gen 6,
             PKl1 <$> arbitrary <*> gen 6,
             PFlp <$> arbitrary <*> gen 6,
             PCpl <$> arbitrary <*> gen 6],
            -- level 6: pTrm6
            [PBrk <$> arbitrary <*> gen 1,
             Prim <$> arbitrary]]

instance Arbitrary TermPrim where
    arbitrary =
        oneof [
           PI <$> arbitrary,
           Pid <$> arbitrary <*> genConceptOne,
           Patm <$> arbitrary <*> identifier <*> maybeConceptOne,
           PVee <$> arbitrary,
           Pfull <$> arbitrary <*> genConceptOne <*> genConceptOne,
           PNamedR <$> relationRef
       ]
      where maybeConceptOne = oneof [return Nothing, Just <$> genConceptOne]

relationRef :: Gen P_NamedRel
relationRef = PNamedRel <$> arbitrary <*> lowerId <*> arbitrary

instance Arbitrary a => Arbitrary (PairView (Term a)) where
    arbitrary = PairView <$> listOf1 arbitrary

instance Arbitrary a => Arbitrary (PairViewSegment (Term a)) where
    arbitrary = oneof [
            PairViewText <$> arbitrary <*> safeStr,
            PairViewExp <$> arbitrary <*> arbitrary <*> sized(genTerm 1) -- only accepts pTerm, no pRule.
        ]

instance Arbitrary a => Arbitrary (PairViewTerm a) where
    arbitrary = PairViewTerm <$> arbitrary -- should be only (PairView (Term a))

instance Arbitrary a => Arbitrary (PairViewSegmentTerm a) where
    arbitrary = PairViewSegmentTerm <$> arbitrary -- should be only PairViewSegment (Term a)

instance Arbitrary SrcOrTgt where
    arbitrary = elements[Src, Tgt]

instance Arbitrary a => Arbitrary (P_Rule a) where
    arbitrary = P_Ru <$> arbitrary <*> safeStr <*> ruleTerm  <*> arbitrary <*> arbitrary
                     <*> arbitrary
              where ruleTerm = sized $ genTerm 0 -- rule is a term level 0

instance Arbitrary ConceptDef where
    arbitrary = Cd <$> arbitrary <*> safeStr <*> arbitrary <*> safeStr <*> safeStr
                   <*>  safeStr  <*> safeStr

instance Arbitrary Paire where
    arbitrary = Paire <$> arbitrary <*> arbitrary

instance Arbitrary P_Population where
    arbitrary =
        oneof [
          P_RelPopu <$> arbitrary <*> lowerId <*> genPairs,
          P_TRelPop <$> arbitrary <*> lowerId <*> arbitrary <*> genPairs,
          P_CptPopu <$> arbitrary <*> lowerId <*> listOf safeStr
        ]

instance Arbitrary P_Interface where
    arbitrary = P_Ifc <$> safeStr1 <*> maybeSafeStr
                      <*> listOf relationRef <*> args <*> listOf arbitrary
                      <*> sized objTermPrim <*> arbitrary <*> safeStr
                   where args = listOf $ listOf1 safeStr
                         maybeSafeStr = oneof [Just <$> safeStr, return Nothing]

instance Arbitrary a => Arbitrary (P_ObjDef a) where
    arbitrary = sized genObj

instance Arbitrary a => Arbitrary (P_SubIfc a) where
    arbitrary = sized genIfc

instance Arbitrary P_IdentDef where
    arbitrary = P_Id <$> arbitrary <*> safeStr <*> arbitrary <*> listOf1 arbitrary

instance Arbitrary P_IdentSegment where
    arbitrary = P_IdentExp <$> sized objTermPrim

instance Arbitrary a => Arbitrary (P_ViewD a) where
    arbitrary =
        oneof [P_Vd <$> arbitrary <*> safeStr <*> genConceptOne
                    <*> return True <*> return Nothing <*> listOf1 arbitrary,
               P_Vd <$> arbitrary <*> safeStr <*> genConceptOne
                    <*> arbitrary <*> arbitrary <*> listOf1 (P_ViewExp <$> arbitrary)]

instance Arbitrary ViewHtmlTemplate where
    arbitrary = ViewHtmlTemplateFile <$> safeStr

instance Arbitrary a => Arbitrary (P_ViewSegmt a) where
    arbitrary =
        oneof [
            P_ViewExp <$> arbitrary,
            P_ViewText <$> safeStr,
            P_ViewHtml <$> safeStr
        ]

instance Arbitrary PPurpose where
    arbitrary = PRef2 <$> arbitrary <*> arbitrary <*> arbitrary <*> listOf safeStr1

instance Arbitrary PRef2Obj where
    arbitrary =
        oneof [
            PRef2ConceptDef <$> safeStr,
            PRef2Declaration <$> relationRef,
            PRef2Rule <$> upperId,
            PRef2IdentityDef <$> upperId,
            PRef2ViewDef <$> upperId,
            PRef2Pattern <$> upperId,
            PRef2Interface <$> upperId,
            PRef2Context <$> upperId
            -- The PRef2Fspc is not used in the parser.
            -- PRef2Fspc <$> upperId
        ]

instance Arbitrary PMeaning where
    arbitrary = PMeaning <$> arbitrary

instance Arbitrary PMessage where
    arbitrary = PMessage <$> arbitrary

instance Arbitrary P_Concept where
    arbitrary = PCpt <$> upperId

genConceptOne :: Gen P_Concept
genConceptOne = oneof [arbitrary, return P_Singleton]

instance Arbitrary P_Sign where
    arbitrary = P_Sign <$> arbitrary <*> arbitrary

instance Arbitrary P_Gen where
    arbitrary =
        oneof [
            P_Cy <$> arbitrary <*> concept <*> listOf1 arbitrary,
            PGen <$> arbitrary <*> concept <*> concept
        ]
        where concept = PCpt <$> upperId

instance Arbitrary Lang where
    arbitrary = elements [Dutch, English]

instance Arbitrary P_Markup where
    arbitrary = P_Markup <$> arbitrary <*> arbitrary <*> safeStr

instance Arbitrary PandocFormat where
    arbitrary = elements [HTML, ReST, LaTeX, Markdown]

instance Arbitrary Prop where
    arbitrary = elements [Uni, Inj, Sur, Tot, Sym, Asy, Trn, Rfx, Irf, Aut, Prop]

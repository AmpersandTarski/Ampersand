{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ampersand.Test.Parser.ArbitraryTree () where

import Ampersand.Basics
import Ampersand.Core.ParseTree
import Ampersand.Input.ADL1.Lexer (isSafeIdChar, keywords)
import RIO.Char
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import Test.QuickCheck hiding (listOf1)
import Test.QuickCheck.Instances ()

-- Useful functions to build on the quick check functions

-- | Generates a simple ascii character
printable :: Gen Char
printable = suchThat arbitrary isValid
  where
    isValid x = isPrint x && isAscii x

-- | Generates a simple string of ascii characters
safeStr :: Gen Text
safeStr = (T.pack <$> listOf printable) `suchThat` noEsc

-- Generates a simple non-empty string of ascii characters
safeStr1 :: Gen Text
safeStr1 = safeStr `suchThat` (not . T.null)

noEsc :: Text -> Bool
noEsc = not . T.any (== '\\')

listOf1 :: Gen a -> Gen (NE.NonEmpty a)
listOf1 p = (NE.:|) <$> p <*> listOf p

-- Generates a filePath
safeFilePath :: Gen FilePath
safeFilePath = T.unpack <$> safeStr

-- Genrates a valid ADL identifier
identifier :: Gen Text
identifier =
  (T.cons <$> firstChar <*> (T.pack <$> listOf restChar))
    `suchThat` noKeyword
  where
    firstChar :: Gen Char
    firstChar = idChar True
    restChar :: Gen Char
    restChar = idChar False
    noKeyword :: Text -> Bool
    noKeyword x = x `notElem` map T.pack keywords
    idChar :: Bool -> Gen Char
    idChar isFirst = arbitrary `suchThat` isAscii `suchThat` isSafeIdChar isFirst

-- Genrates a valid ADL upper-case identifier
upperId :: Gen Text
upperId = identifier `suchThat` startUpper
  where
    startUpper txt = case T.uncons txt of
      Nothing -> False
      Just (h, _) -> isUpper h

-- Genrates a valid ADL lower-case identifier
lowerId :: Gen Text
lowerId = identifier `suchThat` startLower
  where
    startLower txt = case T.uncons txt of
      Nothing -> False
      Just (h, _) -> isLower h

-- Generates an object
objTermPrim :: ObjectKind -> Int -> Gen P_BoxItemTermPrim
objTermPrim objectKind 0 = objTermPrim objectKind 1 -- minimum of 1 sub interface
objTermPrim objectKind i = makeObj objectKind i

data ObjectKind = InterfaceKind | SubInterfaceKind | IdentSegmentKind

makeObj :: ObjectKind -> Int -> Gen P_BoxItemTermPrim
makeObj objectKind maxDepth =
  oneof $
    (P_BxExpr <$> identifier <*> arbitrary <*> term <*> arbitrary <*> pure Nothing <*> ifc) :
      [P_BxTxt <$> identifier <*> arbitrary <*> safeStr | isTxtAllowed]
  where
    isTxtAllowed = case objectKind of
      InterfaceKind -> False
      SubInterfaceKind -> True
      IdentSegmentKind -> False
    term =
      -- depending on the kind, we need the term to be a namedRelation
      Prim <$> case objectKind of
        InterfaceKind -> PNamedR <$> arbitrary
        SubInterfaceKind -> arbitrary
        IdentSegmentKind -> PNamedR <$> arbitrary
    ifc =
      if maxDepth == 0
        then pure Nothing
        else
          Just <$> case objectKind of
            InterfaceKind -> subIfc SubInterfaceKind (maxDepth `div` 2)
            SubInterfaceKind -> subIfc SubInterfaceKind maxDepth
            IdentSegmentKind -> subIfc IdentSegmentKind (maxDepth `div` 2)

subIfc :: ObjectKind -> Int -> Gen P_SubInterface
subIfc objectKind n
  | n == 0 = P_InterfaceRef <$> arbitrary <*> arbitrary <*> identifier
  | otherwise = P_Box <$> arbitrary <*> arbitrary <*> vectorOf n (objGen $ n `div` 2)
  where
    objGen = case objectKind of
      InterfaceKind -> objTermPrim objectKind
      SubInterfaceKind -> makeObj SubInterfaceKind
      IdentSegmentKind -> objTermPrim objectKind

instance Arbitrary BoxHeader where
  arbitrary = BoxHeader <$> arbitrary <*> pure "BOX" <*> listOf arbitrary

instance Arbitrary TemplateKeyValue where
  arbitrary =
    TemplateKeyValue
      <$> arbitrary
      <*> identifier `suchThat` startsWithLetter
      <*> liftArbitrary safeStr1
    where
      startsWithLetter :: Text -> Bool
      startsWithLetter t = case T.uncons t of
        Nothing -> False
        Just (h, _) -> isLetter h

--- Now the arbitrary instances
instance Arbitrary P_Cruds where
  arbitrary =
    P_Cruds <$> arbitrary
      <*> (T.pack <$> suchThat (sublistOf "cCrRuUdD") isCrud)
    where
      isCrud str = L.nub (map toUpper str) == map toUpper str

instance Arbitrary Origin where
  arbitrary = pure OriginUnknown

instance Arbitrary P_Context where
  arbitrary =
    PCtx
      <$> identifier -- name
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary MetaData where
  arbitrary = MetaData <$> arbitrary <*> safeStr1 <*> safeStr

instance Arbitrary P_RoleRule where
  arbitrary = Maintain <$> arbitrary <*> arbitrary <*> listOf1 identifier

instance Arbitrary Representation where
  arbitrary =
    Repr <$> arbitrary
      <*> arbitrary `suchThat` noOne
      <*> arbitrary `suchThat` (TypeOfOne /=)

instance Arbitrary TType where
  arbitrary = elements [minBound ..]

instance Arbitrary Role where
  arbitrary =
    oneof
      [ Role <$> identifier,
        Service <$> identifier
      ]

instance Arbitrary P_Pattern where
  arbitrary =
    P_Pat
      <$> arbitrary
        <*> identifier
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary P_Relation where
  arbitrary =
    P_Relation
      <$> lowerId
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary Pragma where
  arbitrary =
    Pragma
      <$> arbitrary
      <*> safeStr
      <*> safeStr
      <*> safeStr

instance Arbitrary (Term TermPrim) where
  arbitrary = do
    infixLevel <- choose (0, 6)
    maxTreeDepth <- getSize
    treeDepth <- choose (0, maxTreeDepth)
    genTerm infixLevel treeDepth

genTerm ::
  -- | The minimum level of the operands. 0 means that rules are allowe, 1 excludes the operands at that level
  Int ->
  -- | The maximum depth of subexpressions
  Int ->
  Gen (Term TermPrim)
genTerm infixLevel treeDepth =
  if treeDepth == 0
    then Prim <$> arbitrary
    else oneof options
  where
    gen :: Int -> Gen (Term TermPrim)
    gen l = genTerm l (treeDepth `div` 2)

    options :: [Gen (Term TermPrim)]
    options = concat $ drop infixLevel levels

    levels :: [[Gen (Term TermPrim)]]
    levels =
      [ -- level 0: pRule
        [ PEqu <$> arbitrary <*> gen 1 <*> gen 1,
          PInc <$> arbitrary <*> gen 1 <*> gen 1
        ],
        -- level 1: pTerm
        [ PIsc <$> arbitrary <*> gen 2 <*> gen 2,
          PUni <$> arbitrary <*> gen 2 <*> gen 2
        ],
        -- level 2: pTrm2
        [PDif <$> arbitrary <*> gen 3 <*> gen 3],
        -- level 3: pTrm3
        [ PLrs <$> arbitrary <*> gen 4 <*> gen 4,
          PRrs <$> arbitrary <*> gen 4 <*> gen 4,
          PDia <$> arbitrary <*> gen 4 <*> gen 4
        ],
        -- level 4: pTrm4
        [ PCps <$> arbitrary <*> gen 5 <*> gen 5,
          PRad <$> arbitrary <*> gen 5 <*> gen 5,
          PPrd <$> arbitrary <*> gen 5 <*> gen 5
        ],
        -- level 5: pTrm5
        [ PKl0 <$> arbitrary <*> gen 6,
          PKl1 <$> arbitrary <*> gen 6,
          PFlp <$> arbitrary <*> gen 6,
          PCpl <$> arbitrary <*> gen 6
        ],
        -- level 6: pTrm6
        [ PBrk <$> arbitrary <*> gen 1,
          Prim <$> arbitrary
        ]
      ]

instance Arbitrary TermPrim where
  arbitrary =
    oneof
      [ PI <$> arbitrary,
        Pid <$> arbitrary <*> arbitrary,
        Patm <$> arbitrary <*> arbitrary <*> arbitrary,
        PVee <$> arbitrary,
        Pfull <$> arbitrary <*> arbitrary <*> arbitrary,
        PNamedR <$> arbitrary
      ]

instance Arbitrary (PairView (Term TermPrim)) where
  arbitrary = PairView <$> arbitrary

instance Arbitrary (PairViewSegment (Term TermPrim)) where
  arbitrary =
    oneof
      [ PairViewText <$> arbitrary <*> safeStr,
        PairViewExp <$> arbitrary <*> arbitrary <*> genNonRuleTerm
      ]

genRuleTerm :: Gen (Term TermPrim)
genRuleTerm = sized (genTerm 0) -- accepts both pTerm and pRule.

genNonRuleTerm :: Gen (Term TermPrim)
genNonRuleTerm = sized (genTerm 1) -- only accepts pTerm, no pRule.

instance Arbitrary (PairViewTerm TermPrim) where
  arbitrary = PairViewTerm <$> arbitrary

instance Arbitrary (PairViewSegmentTerm TermPrim) where
  arbitrary = PairViewSegmentTerm <$> arbitrary

instance Arbitrary SrcOrTgt where
  arbitrary = elements [minBound ..]

instance Arbitrary (P_Rule TermPrim) where
  arbitrary =
    P_Rule
      <$> arbitrary
      <*> identifier
      <*> genRuleTerm
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary (P_Enforce TermPrim) where
  arbitrary =
    P_Enforce <$> arbitrary
      <*> arbitrary `suchThat` isNamedRelation
      <*> arbitrary
      <*> genNonRuleTerm
    where
      isNamedRelation :: TermPrim -> Bool
      isNamedRelation PNamedR {} = True
      isNamedRelation _ = False

instance Arbitrary EnforceOperator where
  arbitrary =
    oneof
      [ IsSuperSet <$> arbitrary,
        IsSubSet <$> arbitrary,
        IsSameSet <$> arbitrary
      ]

instance Arbitrary PConceptDef where
  arbitrary =
    PConceptDef <$> arbitrary <*> identifier <*> arbitrary
      <*> arbitrary
      <*> identifier

instance Arbitrary PCDDef where
  arbitrary =
    oneof
      [ PCDDefLegacy <$> safeStr <*> safeStr --, Temporary workaround for ampersand 4. Is already fixed in 5.
      --      PCDDefNew <$> arbitrary
      ]

instance Arbitrary PAtomPair where
  arbitrary = PPair <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary P_Population where
  arbitrary =
    oneof
      [ P_RelPopu
          <$> arbitrary `suchThat` noOne
          <*> arbitrary `suchThat` noOne
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary,
        P_CptPopu
          <$> arbitrary
          <*> arbitrary `suchThat` notIsOne
          <*> arbitrary
      ]

instance Arbitrary P_NamedRel where
  arbitrary = PNamedRel <$> arbitrary <*> lowerId <*> arbitrary

instance Arbitrary PAtomValue where
  -- Arbitrary must produce valid input from an ADL-file, so no Xlsx stuff allowed here,
  -- otherwise it is likely that Quickcheck will fail because of it.
  arbitrary =
    oneof
      [ ScriptString <$> arbitrary <*> safeStr `suchThat` stringConstraints,
        ScriptInt <$> arbitrary <*> arbitrary `suchThat` (0 <=),
        ScriptFloat <$> arbitrary <*> arbitrary `suchThat` (0 <=),
        ScriptDate <$> arbitrary <*> arbitrary,
        ScriptDateTime <$> arbitrary <*> arbitrary,
        ComnBool <$> arbitrary <*> arbitrary
      ]
    where
      stringConstraints :: Text -> Bool
      stringConstraints = all isValid . T.unpack
      isValid :: Char -> Bool
      isValid c = c `notElem` ['\'', '"', '\\']

instance Arbitrary P_Interface where
  arbitrary =
    P_Ifc <$> arbitrary
      <*> identifier
      <*> arbitrary
      <*> interfaceObject
      <*> arbitrary
      <*> safeStr
    where
      interfaceObject :: Gen P_BoxItemTermPrim
      interfaceObject = do
        maxDepth <- getSize
        depth <- choose (1, maxDepth)
        makeObj InterfaceKind depth

instance Arbitrary P_SubInterface where
  arbitrary = sized (subIfc SubInterfaceKind)

instance Arbitrary P_IdentDef where
  arbitrary =
    P_Id <$> arbitrary
      <*> identifier
      <*> arbitrary `suchThat` notIsOne
      <*> arbitrary

instance Arbitrary P_IdentSegment where
  arbitrary = P_IdentExp <$> sized (objTermPrim IdentSegmentKind)

instance Arbitrary P_ViewDef where
  arbitrary =
    P_Vd <$> arbitrary <*> identifier <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary ViewHtmlTemplate where
  arbitrary = ViewHtmlTemplateFile <$> safeFilePath

instance Arbitrary (P_ViewSegment TermPrim) where
  arbitrary = P_ViewSegment <$> (Just <$> identifier) <*> arbitrary <*> arbitrary

instance Arbitrary (P_ViewSegmtPayLoad TermPrim) where
  arbitrary =
    oneof
      [ P_ViewExp
          <$> genNonRuleTerm,
        P_ViewText
          <$> safeStr
      ]

instance Arbitrary PPurpose where
  arbitrary = PRef2 <$> arbitrary <*> arbitrary <*> arbitrary <*> listOf safeStr1

instance Arbitrary PRef2Obj where
  arbitrary =
    oneof
      [ PRef2ConceptDef <$> identifier,
        PRef2Relation <$> arbitrary,
        PRef2Rule <$> identifier,
        PRef2IdentityDef <$> identifier,
        PRef2ViewDef <$> identifier,
        PRef2Pattern <$> identifier,
        PRef2Interface <$> identifier,
        PRef2Context <$> identifier
      ]

instance Arbitrary PMeaning where
  arbitrary = PMeaning <$> arbitrary

instance Arbitrary PMessage where
  arbitrary = PMessage <$> arbitrary

instance Arbitrary P_Concept where
  arbitrary =
    frequency
      [ (100, PCpt <$> upperId),
        (1, pure P_ONE)
      ]

instance Arbitrary P_Sign where
  arbitrary = P_Sign <$> arbitrary <*> arbitrary

instance Arbitrary PClassify where
  arbitrary =
    PClassify
      <$> arbitrary
      <*> arbitrary `suchThat` notIsOne
      <*> arbitrary `suchThat` noOne

instance Arbitrary Lang where
  arbitrary = elements [minBound ..]

instance Arbitrary P_Markup where
  arbitrary = P_Markup <$> arbitrary <*> arbitrary <*> safeStr `suchThat` noEndMarkup
    where
      noEndMarkup :: Text -> Bool
      noEndMarkup = not . T.isInfixOf "+}"

instance Arbitrary PandocFormat where
  arbitrary = elements [minBound ..]

instance Arbitrary PProp where
  arbitrary = elements [minBound ..]

instance Arbitrary PRelationDefault where
  arbitrary =
    oneof
      [ PDefAtom <$> arbitrary <*> arbitrary,
        PDefEvalPHP <$> arbitrary <*> safeStr
      ]

noOne :: Foldable t => t P_Concept -> Bool
noOne = all notIsOne

notIsOne :: P_Concept -> Bool
notIsOne = (P_ONE /=)

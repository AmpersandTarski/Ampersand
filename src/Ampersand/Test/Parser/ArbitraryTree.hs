{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ampersand.Test.Parser.ArbitraryTree () where

import Ampersand.Basics
import Ampersand.Core.ParseTree
import Ampersand.Input.ADL1.Lexer (keywords)
import RIO.Char
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import Test.QuickCheck hiding (listOf1)
import Test.QuickCheck.Instances ()

-- Useful functions to build on the quick check functions

-- | Generates a simple string of ascii characters
safeText :: Gen Text
safeText = (T.pack <$> listOf printable) `suchThat` noEsc
  where
    noEsc :: Text -> Bool
    noEsc = not . T.any (== '\\')
    printable :: Gen Char
    printable = suchThat arbitrary isValid
      where
        isValid x = isPrint x && isAscii x

-- Generates a simple non-empty string of ascii characters
safeStr1 :: Gen Text1
safeStr1 = toText1Unsafe <$> (safeText `suchThat` (not . T.null))

listOf1 :: Gen a -> Gen (NE.NonEmpty a)
listOf1 p = (NE.:|) <$> p <*> listOf p

-- Generates a filePath
safeFilePath :: Gen FilePath
safeFilePath = T.unpack <$> safeText

-- | An identifier consists of a single word that starts with a letter. The other
--   characters are all alphanumerical or '_'. Keywords are excluded from identifiers
identifier :: Gen Text1
identifier =
  ( Text1
      <$> arbitrary
      `suchThat` isSafeIdChar' True
      <*> (T.pack <$> listOf (arbitrary `suchThat` isSafeIdChar' False))
  )
    `suchThat` (not . isKeyword)
  where
    isKeyword :: Text1 -> Bool
    isKeyword x = x `elem` keywords
    isSafeIdChar' b c = isSafeIdChar b c && isAscii c -- Only use ascii to run the quickcheck. This prevents difficult-to-read error messages in the parser/prettyprinter roundtrip.

-- Genrates a valid ADL lower-case name
lowercaseName :: Gen Name
lowercaseName = arbitrary `suchThat` (firstLowercase . namePartToText1 . localName)
  where
    firstLowercase :: Text1 -> Bool
    firstLowercase (Text1 c _) = isLower c

-- Genrates a valid ADL upper-case name
uppercaseName :: Gen Name
uppercaseName = arbitrary `suchThat` (firstUppercase . namePartToText1 . localName)
  where
    firstUppercase :: Text1 -> Bool
    firstUppercase (Text1 c _) = isUpper c

makeObj :: ObjectKind -> Gen (P_BoxItem TermPrim)
makeObj objectKind =
  oneof
    $ ( P_BoxItemTerm
          <$> plainNameGenerator
          <*> arbitrary
          <*> arbitrary
          <*> term
          <*> cruds
          <*> view'
          <*> subInterface objectKind
      )
    : [ P_BxTxt
          <$> plainNameGenerator
          <*> arbitrary
          <*> safeText
        | isTxtAllowed
      ]
  where
    plainNameGenerator :: Gen (Maybe Text1)
    plainNameGenerator = case objectKind of
      InterfaceKind -> pure Nothing
      SubInterfaceKind _ -> Just <$> safePlainName
      IdentSegmentKind -> pure Nothing
      ViewSegmentKind -> pure Nothing
    cruds :: Gen (Maybe P_Cruds)
    cruds = case objectKind of
      InterfaceKind -> arbitrary
      SubInterfaceKind _ -> arbitrary
      IdentSegmentKind -> pure Nothing
      ViewSegmentKind -> pure Nothing
    view' :: Gen (Maybe Name)
    view' = case objectKind of
      InterfaceKind -> oneof [pure Nothing, Just <$> uppercaseName]
      SubInterfaceKind _ -> oneof [pure Nothing, Just <$> uppercaseName]
      IdentSegmentKind -> pure Nothing
      ViewSegmentKind -> oneof [pure Nothing, Just <$> uppercaseName]
    isTxtAllowed = case objectKind of
      InterfaceKind -> False
      SubInterfaceKind _ -> True
      IdentSegmentKind -> False
      ViewSegmentKind -> True
    term =
      -- depending on the kind, we need the term to be a namedRelation
      Prim <$> case objectKind of
        InterfaceKind -> PNamedR <$> arbitrary
        SubInterfaceKind _ -> arbitrary
        IdentSegmentKind -> PNamedR <$> arbitrary
        ViewSegmentKind -> arbitrary
    subInterface :: ObjectKind -> Gen (Maybe (P_SubIfc TermPrim))
    subInterface objectKind' = case objectKind' of
      InterfaceKind -> subInterface (SubInterfaceKind {siMaxDepth = 3})
      IdentSegmentKind -> pure Nothing
      SubInterfaceKind 0 -> pure Nothing
      SubInterfaceKind n ->
        Just
          <$> oneof
            [ P_Box
                <$> arbitrary
                <*> arbitrary
                <*> smallListOf (makeObj (SubInterfaceKind {siMaxDepth = min (n - 1) 2})),
              P_InterfaceRef
                <$> arbitrary
                <*> arbitrary
                <*> arbitrary
            ]
      ViewSegmentKind -> pure Nothing

smallListOf :: Gen a -> Gen [a]
smallListOf gen = do
  k <- choose (0, 3)
  vectorOf k gen

instance Arbitrary Name where
  arbitrary =
    mkName <$> arbitrary <*> listOf1 safeNamePart
    where
      safeNamePart :: Gen NamePart
      safeNamePart =
        ( \x -> case toNamePart1 x of
            Nothing -> fatal $ "Not a valid NamePart: " <> tshow x
            Just np -> np
        )
          <$> identifier
          `suchThat` requirements
      requirements = T.all (/= '.') . text1ToText

instance Arbitrary NameType where
  arbitrary = elements [minBound ..]

instance Arbitrary Label where
  arbitrary = Label <$> safeText

instance Arbitrary HTMLtemplateCall where
  arbitrary =
    HTMLtemplateCall
      <$> arbitrary
      <*> pure (toText1Unsafe "BOX")
      <*> listOf arbitrary

instance Arbitrary TemplateKeyValue where
  arbitrary =
    TemplateKeyValue
      <$> arbitrary
      <*> identifier
      <*> liftArbitrary safeText

instance Arbitrary P_Cruds where
  arbitrary =
    P_Cruds
      <$> arbitrary
      <*> (toText1Unsafe . T.pack <$> (sublistOf "cCrRuUdD" `suchThat` requirements))
    where
      requirements cs = length cs `elem` [1 .. 4] && map toUpper cs == (L.nub . map toUpper $ cs)

instance Arbitrary Origin where
  arbitrary = pure OriginUnknown

instance Arbitrary P_Context where
  arbitrary =
    PCtx
      <$> uppercaseName
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary

instance Arbitrary MetaData where
  arbitrary = MetaData <$> arbitrary <*> safeStr1 <*> safeText

instance Arbitrary P_RoleRule where
  arbitrary = Maintain <$> arbitrary <*> arbitrary <*> listOf1 arbitrary

-- | Only generate explicit representations. Implicit representations come from boxes in interfaces, so they must not be generated.
instance Arbitrary P_Representation where
  arbitrary =
    Repr
      <$> arbitrary
      <*> arbitrary
      `suchThat` all notIsONE
      <*> arbitrary
      `suchThat` (TypeOfOne /=)

instance Arbitrary TType where
  arbitrary = elements [minBound ..]

instance Arbitrary Role where
  arbitrary =
    Role
      <$> arbitrary
      <*> unrestrictedName
      <*> arbitrary
      <*> arbitrary
    where
      unrestrictedName = oneof [lowercaseName, uppercaseName]

instance Arbitrary P_Pattern where
  arbitrary =
    P_Pat
      <$> arbitrary
      <*> uppercaseName
      <*> arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> arbitrary
      <*> smallListOf arbitrary

instance Arbitrary P_Relation where
  arbitrary =
    P_Relation
      <$> lowercaseName
      <*> arbitrary
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
      <*> safeText
      <*> safeText
      <*> safeText

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
      [ PairViewText <$> arbitrary <*> safeText,
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
      <*> arbitrary
      <*> arbitrary
      <*> genRuleTerm
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary (P_Enforce TermPrim) where
  arbitrary =
    P_Enforce
      <$> arbitrary
      <*> arbitrary
      `suchThat` isNamedRelation
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
    PConceptDef
      <$> arbitrary
      <*> uppercaseName
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary DefinitionContainer where
  arbitrary =
    oneof
      [ CONTEXT <$> uppercaseName,
        PATTERN <$> uppercaseName,
        Module <$> arbitrary
      ]

instance Arbitrary PCDDef where
  arbitrary =
    oneof
      [ PCDDefLegacy <$> safeText <*> safeText,
        PCDDefNew <$> arbitrary
      ]

instance Arbitrary PAtomPair where
  arbitrary = PPair <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary P_Population where
  arbitrary =
    oneof
      [ P_RelPopu
          <$> arbitrary
          `suchThat` all notIsONE
          <*> arbitrary
          `suchThat` all notIsONE
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary,
        P_CptPopu
          <$> arbitrary
          <*> arbitrary
          `suchThat` notIsONE
          <*> arbitrary
      ]

instance Arbitrary P_NamedRel where
  arbitrary = PNamedRel <$> arbitrary <*> lowercaseName <*> arbitrary

instance Arbitrary PAtomValue where
  -- Arbitrary must produce valid input from an ADL-file, so no Xlsx stuff allowed here,
  -- otherwise it is likely that Quickcheck will fail because of it.
  arbitrary =
    oneof
      [ ScriptString <$> arbitrary <*> safeText `suchThat` stringConstraints,
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
    P_Ifc
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> makeObj InterfaceKind
      <*> arbitrary
      <*> safeText

instance Arbitrary P_IdentDef where
  arbitrary =
    P_Id
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary `suchThat` notIsONE
      <*> arbitrary `suchThat` notIsRuleTerm

instance Arbitrary P_ViewDef where
  arbitrary =
    P_Vd
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary ViewHtmlTemplate where
  arbitrary = ViewHtmlTemplateFile <$> safeFilePath

instance Arbitrary (P_ViewSegment TermPrim) where
  arbitrary = P_ViewSegment . Just <$> identifier <*> arbitrary <*> arbitrary

instance Arbitrary (P_ViewSegmtPayLoad TermPrim) where
  arbitrary =
    oneof
      [ P_ViewExp
          <$> genNonRuleTerm,
        P_ViewText
          <$> safeText
      ]

instance Arbitrary PPurpose where
  arbitrary = PPurpose <$> arbitrary <*> arbitrary <*> arbitrary <*> listOf safeText

instance Arbitrary PRef2Obj where
  arbitrary =
    oneof
      [ PRef2ConceptDef <$> uppercaseName,
        PRef2Relation <$> arbitrary,
        PRef2Rule <$> arbitrary,
        PRef2IdentityDef <$> arbitrary,
        PRef2ViewDef <$> arbitrary,
        PRef2Pattern <$> uppercaseName,
        PRef2Interface <$> arbitrary,
        PRef2Context <$> uppercaseName
      ]

instance Arbitrary PMeaning where
  arbitrary = PMeaning <$> arbitrary

instance Arbitrary PMessage where
  arbitrary = PMessage <$> arbitrary

instance Arbitrary P_Concept where
  arbitrary =
    frequency
      [ (100, PCpt <$> uppercaseName),
        (1, pure P_ONE)
      ]

instance Arbitrary P_Sign where
  arbitrary =
    P_Sign
      <$> arbitrary
      <*> arbitrary

instance Arbitrary PClassify where
  arbitrary =
    PClassify
      <$> arbitrary
      <*> arbitrary
      `suchThat` notIsONE
      <*> arbitrary
      `suchThat` all notIsONE

instance Arbitrary Lang where
  arbitrary = elements [minBound ..]

instance Arbitrary P_Markup where
  arbitrary = P_Markup <$> arbitrary <*> arbitrary <*> safeText `suchThat` noEndMarkup
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
        PDefEvalPHP <$> arbitrary <*> safeText
      ]

notIsONE :: P_Concept -> Bool
notIsONE cpt = case cpt of
  PCpt {} -> True
  P_ONE -> False

-- notIsRuleTerm :: NonEmpty (Term TermPrim) -> Bool
notIsRuleTerm :: NonEmpty (Term a) -> Bool
notIsRuleTerm = all noRule . NE.toList
  where
    noRule :: Term a -> Bool
    noRule trm = case trm of
                   PEqu {} -> False
                   PInc {} -> False
                   _ -> True

safePlainName :: Gen Text1
safePlainName =
  oneof
    [ identifier,
      safeStr1 `suchThat` (not . null . T.words . text1ToText)
    ]

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
safeStr1 :: Gen Text1
safeStr1 = toText1Unsafe <$> (safeStr `suchThat` (not . T.null))

noEsc :: Text -> Bool
noEsc = not . T.any (== '\\')

listOf1 :: Gen a -> Gen (NE.NonEmpty a)
listOf1 p = (NE.:|) <$> p <*> listOf p

-- Generates a filePath
safeFilePath :: Gen FilePath
safeFilePath = T.unpack <$> safeStr

-- | An identifier consists of a single word that starts with a letter. The other
--   characters are all alphanumerical or '_'. Keywords are excluded from identifiers
identifier :: Gen Text1
identifier =
  ( Text1 <$> arbitrary `suchThat` isSafeIdChar True
      <*> (T.pack <$> listOf (arbitrary `suchThat` isSafeIdChar False))
  )
    `suchThat` (not . isKeyword)
  where
    isKeyword :: Text1 -> Bool
    isKeyword x = x `elem` keywords

-- Genrates a valid ADL lower-case name
lowercaseName :: Gen Name
lowercaseName = arbitrary `suchThat` (firstLowercase . plainNameOf1)
  where
    firstLowercase :: Text1 -> Bool
    firstLowercase (Text1 c _) = isLower c

-- Genrates a valid ADL upper-case name
uppercaseName :: Gen Name
uppercaseName = arbitrary `suchThat` (firstUppercase . plainNameOf1)
  where
    firstUppercase :: Text1 -> Bool
    firstUppercase (Text1 c _) = isUpper c

data ObjectKind = InterfaceKind | SubInterfaceKind {siMaxDepth :: !Int} | IdentSegmentKind

makeObj :: ObjectKind -> Gen P_BoxBodyElement
makeObj objectKind =
  oneof $
    (P_BxExpr <$> labelGenerator <*> arbitrary <*> term <*> arbitrary <*> pure Nothing <*> subInterface objectKind) :
      [P_BxTxt <$> labelGenerator <*> arbitrary <*> safeStr | isTxtAllowed]
  where
    isTxtAllowed = case objectKind of
      InterfaceKind -> False
      SubInterfaceKind _ -> True
      IdentSegmentKind -> False
    labelGenerator :: Gen (Maybe Text1)
    labelGenerator = case objectKind of
      InterfaceKind -> Just <$> safeLabel
      SubInterfaceKind _ -> Just <$> safeLabel
      IdentSegmentKind -> pure Nothing
    term =
      -- depending on the kind, we need the term to be a namedRelation
      Prim <$> case objectKind of
        InterfaceKind -> PNamedR <$> arbitrary
        SubInterfaceKind _ -> arbitrary
        IdentSegmentKind -> PNamedR <$> arbitrary
    subInterface :: ObjectKind -> Gen (Maybe (P_SubIfc TermPrim))
    subInterface objectKind' = case objectKind' of
      InterfaceKind -> do
        maxDepth <- getSize
        depth <- choose (0, maxDepth)
        subInterface (SubInterfaceKind {siMaxDepth = depth})
      IdentSegmentKind -> pure Nothing
      SubInterfaceKind 0 -> pure Nothing
      SubInterfaceKind n ->
        Just
          <$> oneof
            [ P_Box <$> arbitrary <*> arbitrary <*> listOf (makeObj (SubInterfaceKind {siMaxDepth = n -1})),
              P_InterfaceRef <$> arbitrary <*> arbitrary <*> arbitrary
            ]

instance Arbitrary Name where
  arbitrary =
    toName <$> listOf safeNamePart <*> safeNamePart
    where
      safeNamePart :: Gen Text1
      safeNamePart = identifier `suchThat` requirements
      requirements t =
        T.all (/= '.') . text1ToText $ t

instance Arbitrary BoxHeader where
  arbitrary =
    BoxHeader
      <$> arbitrary
      <*> pure (toText1Unsafe "BOX")
      <*> listOf arbitrary

instance Arbitrary TemplateKeyValue where
  arbitrary =
    TemplateKeyValue
      <$> arbitrary
      <*> identifier
      <*> liftArbitrary safeStr

instance Arbitrary P_Cruds where
  arbitrary =
    P_Cruds <$> arbitrary
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
  arbitrary = Maintain <$> arbitrary <*> arbitrary <*> listOf1 arbitrary

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
      [ Role <$> arbitrary,
        Service <$> arbitrary
      ]

instance Arbitrary P_Pattern where
  arbitrary =
    P_Pat
      <$> arbitrary
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
      <*> arbitrary
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
    PConceptDef <$> arbitrary <*> uppercaseName <*> arbitrary
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
      [ PCDDefLegacy <$> safeStr <*> safeStr,
        PCDDefNew <$> arbitrary
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
  arbitrary = PNamedRel <$> arbitrary <*> lowercaseName <*> arbitrary

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
      <*> arbitrary
      <*> arbitrary
      <*> makeObj InterfaceKind
      <*> arbitrary
      <*> safeStr

instance Arbitrary P_IdentDef where
  arbitrary =
    P_Id <$> arbitrary
      <*> arbitrary
      <*> arbitrary `suchThat` notIsOne
      <*> arbitrary

instance Arbitrary P_IdentSegment where
  arbitrary = P_IdentExp <$> makeObj IdentSegmentKind

instance Arbitrary P_ViewDef where
  arbitrary =
    P_Vd <$> arbitrary <*> arbitrary <*> arbitrary
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
  arbitrary = PRef2 <$> arbitrary <*> arbitrary <*> arbitrary <*> listOf safeStr

instance Arbitrary PRef2Obj where
  arbitrary =
    oneof
      [ PRef2ConceptDef <$> arbitrary,
        PRef2Relation <$> arbitrary,
        PRef2Rule <$> arbitrary,
        PRef2IdentityDef <$> arbitrary,
        PRef2ViewDef <$> arbitrary,
        PRef2Pattern <$> arbitrary,
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

safeLabel :: Gen Text1
safeLabel =
  oneof
    [ identifier,
      toText1Unsafe . tshow <$> safeStr1,
      elements keywords
    ]
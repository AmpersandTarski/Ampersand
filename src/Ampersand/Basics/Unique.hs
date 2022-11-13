{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- The purpose of class Unique is to identify a Haskell object by means of a string.
E.g.
instance Unique Pattern where
 showUnique = tName
-}

module Ampersand.Basics.Unique
  ( Unique (..),
    Named (..),
    NameDef,
    --    NameRef,
    Name,
    NameSpace,
    Text1 (..),
    toName,
    nameOfONE,
    nameOfExecEngineRole,
    prependToPlainName,
    fullNameToName,
    urlEncodedName,
    toNameUnsafe,
  )
where

import Ampersand.Basics.Prelude
import Ampersand.Basics.String (escapeIdentifier, text1ToText, toText1Unsafe, urlEncode)
import Ampersand.Basics.Version (fatal)
import qualified Data.GraphViz.Printing as GVP
import Data.Hashable
import Data.Text1 (Text1 (..))
import qualified Data.Text1 as T1 hiding (Text1 (..))
import Data.Text1.Text1 ((<>.))
import Data.Typeable
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL

type NameSpace = [Text1]

type Name = NameDef --TODO: Get rid of this definition. Each use of it must either be a NameDef or a NameRef

-- newtype NameRef = ReferenceTo NameDef

data NameType = ConceptName | RelationName | RuleName | PatternName | ContextName | RoleName | TemporaryDummy
  deriving (Data)

data NameDef = NameDef
  { -- | The plain name
    plainName :: !Text1,
    -- | The namespace where the name resides in.
    nameSpace :: !NameSpace,
    -- | An optional label for showing the definition in things like the user-interface
    mLabel :: !(Maybe Text1),
    -- | The type of the thing that this name is for
    nameType :: !NameType
  }
  deriving (Data)

instance Ord NameDef where
  compare a b = compare (tshow a) (tshow b)

instance Eq NameDef where
  a == b = compare a b == EQ

instance Show NameDef where
  show x = T.unpack . mconcat . L.intersperse "." $ (toText <$> nameSpace x) <> [toText $ plainName x]
    where
      toText :: Text1 -> Text
      toText (Text1 c tl) = T.cons c tl

instance Hashable NameDef where
  hashWithSalt s = hashWithSalt s . text1ToText . tName

instance Named NameDef where
  name = id

instance GVP.PrintDot NameDef where
  unqtDot = GVP.text . TL.fromStrict . text1ToText . tName

nameOfExecEngineRole :: NameDef
nameOfExecEngineRole =
  NameDef
    { plainName = Text1 'E' "xecEngine",
      nameSpace = [],
      mLabel = Nothing,
      nameType = RoleName
    }

nameOfONE :: NameDef
nameOfONE =
  NameDef
    { plainName = Text1 'O' "NE",
      nameSpace = [],
      mLabel = Nothing,
      nameType = ConceptName
    }

toName :: NameSpace -> Text1 -> NameDef
toName space plainname =
  NameDef
    { plainName = mkValid plainname,
      nameSpace = space,
      mLabel = Nothing,
      nameType = TemporaryDummy
    }

toNameUnsafe :: [Text] -> Text -> NameDef
toNameUnsafe ns t = toName ns' t'
  where
    ns' = toSafe <$> ns
    t' = toSafe t
    toSafe :: Text -> Text1
    toSafe txt = case T.uncons txt of
      Nothing ->
        fatal $
          T.intercalate
            "/n  "
            [ "toNameUnsafe must not be used unless you are certain that it is safe!",
              tshow ns,
              tshow t
            ]
      Just (h, tl) -> Text1 h tl

-- We do not want points and spaces in the actual name, for this will conflict with namespaces
mkValid :: Text1 -> Text1
mkValid t1@(Text1 h tl) =
  if h `elem` [' ', '.', '_']
    then fatal $ "A name must not start with a forbiden character. `" <> tshow t1 <> "`."
    else Text1 h (mkValid' tl)
  where
    mkValid' t = case T.uncons t of
      Nothing -> mempty
      Just (h', tl') -> case h' of
        '.' -> T.cons '-' $ mkValid' tl'
        ' ' -> mkValid' tl'
        _ -> T.cons h' $ mkValid' tl'

-- | anything could have some label, can't it?
class Named a where
  name :: a -> NameDef
  tName :: a -> Text1
  tName = toText1Unsafe . tshow . name
  nameSpaceOf :: a -> [Text1]
  nameSpaceOf = nameSpace . name
  plainNameOf1 :: a -> Text1
  plainNameOf1 = plainName . name
  plainNameOf :: a -> Text
  plainNameOf nm = T.cons h tl
    where
      Text1 h tl = plainNameOf1 nm
  label :: a -> Text1
  label x =
    fromMaybe
      (plainName . name $ x)
      (mLabel . name $ x)

fullNameToName :: Text1 -> NameDef
fullNameToName t = case T.split (== '.') . text1ToText $ t of
  [] -> fatal $ "Name should contain chacters other than `.`: " <> tshow t
  (h : tl) -> toName (NE.init parts) (NE.last parts)
    where
      parts = tryToMakeValid <$> h NE.:| tl
      tryToMakeValid :: Text -> Text1
      tryToMakeValid part = case T.uncons part of
        Nothing -> fatal $ "part may not be empty (" <> text1ToText t <> ")."
        Just (h', tl') -> mkValid (Text1 h' tl')

prependToPlainName :: Text -> NameDef -> NameDef
prependToPlainName prefix nm = toName (nameSpaceOf nm) (prefix T1..<> plainNameOf1 nm)

urlEncodedName :: NameDef -> Text1
urlEncodedName = toText1Unsafe . urlEncode . text1ToText . tName

-- | In the context of the haskell code, things can be Unique.
class (Typeable e, Eq e) => Unique e where
  -- | a representation of a unique thing
  self :: e -> UniqueObj e
  self a =
    UniqueObj
      { theThing = a
      --    , theShow  = showUnique
      }

  -- | representation of a Unique thing into a Text.
  uniqueShowWithType :: e -> Text1
  uniqueShowWithType x = (toText1Unsafe . tshow . typeOf $ x) <>. "_" <> (text1ToText . showUnique $ x)

  -- | A function to show a unique instance. It is the responsability
  --   of the instance definition to make sure that for every a, b of
  --   an individual type:
  --        a == b  <==> showUnique a == showUnique b
  showUnique :: e -> Text1

  {-# MINIMAL showUnique #-}

  idWithoutType :: e -> Text1
  idWithoutType =
    uniqueButNotTooLong -- because it could be stored in an SQL database
      . escapeIdentifier -- escape because a character safe identifier is needed for use in URLs, filenames and database ids
      . showUnique

  idWithType :: e -> Text1
  idWithType e =
    uniqueButNotTooLong -- because it could be stored in an SQL database
      . addType e
      . escapeIdentifier -- escape because a character safe identifier is needed for use in URLs, filenames and database ids
      $ showUnique e

  addType :: e -> Text1 -> Text1
  addType x string = toText1Unsafe $ tshow (typeOf x) <> "_" <> text1ToText string

uniqueButNotTooLong :: Text1 -> Text1
uniqueButNotTooLong txt =
  let (prfx, rest) = T.splitAt safeLength (text1ToText txt)
   in if T.null rest
        then txt
        else toText1Unsafe $ prfx <> "#" <> tshow (hash . text1ToText $ txt) <> "#"
  where
    safeLength = 50 -- HJO, 20170812: Subjective value. This is based on the
    -- limitation that DirtyId's are stored in an sql database
    -- in a field that is normally 255 long. We store the
    -- prefix of the string but make sure we still have space
    -- left over for the hash. While theoretically this is a
    -- crappy solution, in practice this will prove to be good
    -- enough.

-- | this is the implementation of the abstract data type. It mustn't be exported
newtype UniqueObj a = UniqueObj
  { theThing :: a
  }
  deriving (Typeable)

instance Unique a => Unique [a] where
  showUnique [] = toText1Unsafe "[]"
  showUnique xs = toText1Unsafe $ "[" <> T.intercalate ", " (text1ToText . showUnique <$> xs) <> "]"

instance Unique a => Unique (Set.Set a) where
  showUnique = showUnique . Set.elems

instance Unique Bool where
  showUnique = toText1Unsafe . T.toLower . tshow
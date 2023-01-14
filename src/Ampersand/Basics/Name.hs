{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Ampersand.Basics.Name
  ( Named (..),
    Name,
    NameSpace,
    Label (..),
    Labeled (..),
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
import Ampersand.Basics.String (text1ToText, toText1Unsafe, urlEncode)
import Ampersand.Basics.Version (fatal)
import qualified Data.GraphViz.Printing as GVP
import Data.Hashable
import RIO.Char
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL

type NameSpace = [Text1]

data Name = Name
  { -- | A name in a namespace can be seen as a nonempty list of words.
    --   currently, we only deal with 'absolute' names.
    --   the separator that is inbetween the nameWords can be depending on the specific environment.
    --   in an .adl file, we will assume a dot `.` as separator.
    nameWords :: !(NonEmpty Text1),
    nameType :: !NameType
  }
  deriving (Data)

instance Ord Name where
  compare a b = compare (tshow a) (tshow b)

instance Eq Name where
  a == b = compare a b == EQ

instance Show Name where
  show =
    T.unpack
      . mconcat
      . L.intersperse "."
      . toList
      . fmap text1ToText
      . nameWords

instance Hashable Name where
  hashWithSalt s = hashWithSalt s . text1ToText . tName

instance Named Name where
  name = id

instance GVP.PrintDot Name where
  unqtDot = GVP.text . TL.fromStrict . text1ToText . tName

nameOfExecEngineRole :: Name
nameOfExecEngineRole =
  Name
    { nameWords = Text1 'E' "xecEngine" :| [],
      nameType = RoleName
    }

nameOfONE :: Name
nameOfONE =
  Name
    { nameWords = Text1 'O' "NE" :| [],
      nameType = ConceptName
    }

data NameType = ConceptName | RelationName | RuleName | PatternName | ContextName | RoleName | TemporaryDummy
  deriving (Data)

toName :: NameSpace -> Text1 -> Name
toName ns plainname =
  Name
    { nameWords = prependList ns (mkValid plainname :| []),
      nameType = TemporaryDummy
    }

toNameUnsafe :: [Text] -> Text -> Name
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

-- | Validation for the rules for the words in a Name. These rules are based on the rules for
--   mariaDB names for tables and columns. (https://mariadb.com/kb/en/columnstore-naming-conventions/)
--   1) Table and column names are restricted to alphanumeric and underscore only, i.e "A-Z a-z 0-9 _".
--   2) The first character of all table and column names should be an ASCII letter (a-z A-Z).
--   3) ColumnStore reserves certain words that MariaDB does not, such as SELECT, CHAR and TABLE, so even wrapped in backticks these cannot be used.
-- isValidNameWord ::

-- We do not want points and spaces in the actual name, for this will conflict with namespaces
mkValid :: Text1 -> Text1
mkValid t = case T.words $ text1ToText t of
  [] -> fatal "empty text seems to be parsed as a part of a name."
  [wrd] -> case T.uncons wrd of
    Nothing -> fatal "Impossible. How can a word be empty?"
    Just (h, tl) ->
      if isValidFirstCharacter h
        then Text1 h (mkValidTail tl)
        else fatal $ "Invalid first character: " <> wrd
  txts -> fatal $ "There cannot be whitespace in a nameword: " <> tshow txts
  where
    mkValidTail = T.map toValidChar
      where
        toValidChar c = if isValidOtherCharacter c then c else '_'
    isValidFirstCharacter c = isAlpha c
    isValidOtherCharacter c = isAlpha c || isDigit c || c == '_'

-- | anything could have some name, can't it?
class Named a where
  {-# MINIMAL name #-}
  name :: a -> Name
  tName :: a -> Text1
  tName = toText1Unsafe . tshow . name
  nameSpaceOf :: a -> [Text1]
  nameSpaceOf = NE.init . nameWords . name
  plainNameOf1 :: a -> Text1
  plainNameOf1 = NE.last . nameWords . name
  plainNameOf :: a -> Text
  plainNameOf nm = T.cons h tl
    where
      Text1 h tl = plainNameOf1 nm

newtype Label = Label Text

class Named a => Labeled a where
  {-# MINIMAL mLabel #-}
  mLabel :: a -> Maybe Label
  label :: a -> Text
  label x = case mLabel x of
    Nothing -> plainNameOf x
    Just (Label lbl) -> lbl

instance Show Label where
  show (Label x) = "LABEL " <> T.unpack x

fullNameToName :: Text1 -> Name
fullNameToName t = case T.split (== '.') . text1ToText $ t of
  [] -> fatal $ "Name should contain chacters other than `.`: " <> tshow t
  (h : tl) -> toName (NE.init parts) (NE.last parts)
    where
      parts = tryToMakeValid <$> h NE.:| tl
      tryToMakeValid :: Text -> Text1
      tryToMakeValid part = case T.uncons part of
        Nothing -> fatal $ "part may not be empty (" <> text1ToText t <> ")."
        Just (h', tl') -> mkValid (Text1 h' tl')

prependToPlainName :: Text -> Name -> Name
prependToPlainName prefix nm = toName (nameSpaceOf nm) (toText1Unsafe $ prefix <> text1ToText (plainNameOf1 nm))

urlEncodedName :: Name -> Text1
urlEncodedName = toText1Unsafe . urlEncode . text1ToText . tName

-- Should be in RIO.NonEmpty:
prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList ls ne = case ls of
  [] -> ne
  (x : xs) -> x :| xs <> toList ne
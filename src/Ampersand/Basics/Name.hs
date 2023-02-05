{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Ampersand.Basics.Name
  ( Named (..),
    Name,
    NameSpace,
    NameType (..),
    Label (..),
    Labeled (..),
    mkName,
    nameOfONE,
    nameOfExecEngineRole,
    withNameSpace,
    prependToPlainName,
    urlEncodedName,
    splitOnDots,
  )
where

import Ampersand.Basics.Prelude
import Ampersand.Basics.String (text1ToText, toText1Unsafe, urlEncode)
import Ampersand.Basics.Version (fatal)
import qualified Data.GraphViz.Printing as GVP
import Data.Hashable
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

mkName :: NameType -> NonEmpty Text1 -> Name
mkName typ xs =
  Name
    { nameWords = xs,
      nameType = typ
    }

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

data NameType
  = ConceptName
  | ContextName
  | IdentName
  | InterfaceName
  | PatternName
  | PropertyName
  | RelationName
  | RoleName
  | RuleName
  | SqlAttributeName
  | SqlTableName
  | ViewName
  deriving (Data, Enum, Bounded)

withNameSpace :: NameSpace -> Name -> Name
withNameSpace ns nm =
  Name
    { nameWords = prependList ns (nameWords nm),
      nameType = nameType nm
    }

-- | Validation for the rules for the words in a Name. These rules are based on the rules for
--   mariaDB names for tables and columns. (https://mariadb.com/kb/en/columnstore-naming-conventions/)
--   1) Table and column names are restricted to alphanumeric and underscore only, i.e "A-Z a-z 0-9 _".
--   2) The first character of all table and column names should be an ASCII letter (a-z A-Z).
--   3) ColumnStore reserves certain words that MariaDB does not, such as SELECT, CHAR and TABLE, so even wrapped in backticks these cannot be used.
-- isValidNameWord ::
splitOnDots :: Text1 -> NonEmpty Text1
splitOnDots t1 =
  case map toText1Unsafe
    . filter (not . T.null)
    . T.split (== '.')
    . text1ToText
    $ t1 of
    [] -> fatal "This should be impossible!"
    te : tes -> te NE.:| tes

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
  updatedName :: Text1 -> a -> Name
  updatedName txt1 x = Name ws' typ
    where
      Name ws typ = name x
      ws' = NE.reverse (txt1 NE.:| (reverse . NE.tail) ws)

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

prependToPlainName :: Text -> Name -> Name
prependToPlainName prefix nm =
  nm {nameWords = NE.reverse $ toText1Unsafe (prefix <> text1ToText h) NE.:| tl}
  where
    h NE.:| tl = NE.reverse . nameWords $ nm

urlEncodedName :: Name -> Text1
urlEncodedName = toText1Unsafe . urlEncode . text1ToText . tName

-- Should be in RIO.NonEmpty:
prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList ls ne = case ls of
  [] -> ne
  (x : xs) -> x :| xs <> toList ne
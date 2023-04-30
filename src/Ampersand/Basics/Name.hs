{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Ampersand.Basics.Name
  ( Named (..),
    Name,
    NamePart,
    NameSpace,
    NameType (..),
    Label (..),
    Labeled (..),
    Rename (..),
    mkName,
    nameOfONE,
    nameOfExecEngineRole,
    withNameSpace,
    prependToPlainName,
    urlEncodedName,
    splitOnDots,
    namePartToText,
    namePartToText1,
    toNamePartUnsafe,
    toNamePartUnsafe1,
    checkProperId,
  )
where

import Ampersand.Basics.Auxiliaries (eqCl)
import Ampersand.Basics.Prelude
import Ampersand.Basics.String (isSafeIdChar, text1ToText, toText1Unsafe, urlEncode)
import Ampersand.Basics.Version (fatal)
import qualified Data.GraphViz.Printing as GVP
import Data.Hashable
import qualified Data.Text1 as T1
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL

type NameSpace = [NamePart]

-- A namepart is a single word, that starts with an alphanumeric character
-- and may contain of alphanumeric characters and digits only.
newtype NamePart = NamePart Text1 deriving (Data)

instance Show NamePart where
  show (NamePart t1) = show t1

data Name = Name
  { -- | A name in a namespace can be seen as a nonempty list of words.
    --   currently, we only deal with 'absolute' names.
    --   the separator that is inbetween the nameWords can be depending on the specific environment.
    --   in an .adl file, we will assume a dot `.` as separator.
    nameWords :: !(NonEmpty NamePart),
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
      . fmap namePartToText
      . nameWords

instance Hashable Name where
  hashWithSalt s = hashWithSalt s . text1ToText . tName

instance Named Name where
  name = id

instance GVP.PrintDot Name where
  unqtDot = GVP.text . TL.fromStrict . text1ToText . tName

-- | toNamePartUnsafe will convert a Text to a NamePart. The Text must be a proper ID. (See checkProperId)
toNamePartUnsafe :: Text -> NamePart
toNamePartUnsafe txt = case T.uncons txt of
  Nothing -> fatal "toText1Unsafe must not be used unless you are certain that it is safe!"
  Just (h, tl) -> toNamePartUnsafe1 $ Text1 h tl

-- | toNamePartUnsafe1 will convert a Text1 to a NamePart. The Text1 must be a proper ID. (See checkProperId)
toNamePartUnsafe1 :: Text1 -> NamePart
toNamePartUnsafe1 = NamePart . checkProperId

-- | This function checks
checkProperId :: Text1 -> Text1
checkProperId t@(Text1 h tl) =
  if isProper
    then t
    else fatal $ "Not a proper Id: " <> text1ToText t
  where
    isProper = and (isSafeIdChar True h : (isSafeIdChar False <$> T.unpack tl))

namePartToText :: NamePart -> Text
namePartToText (NamePart x) = text1ToText x

namePartToText1 :: NamePart -> Text1
namePartToText1 (NamePart x) = x

mkName :: NameType -> NonEmpty NamePart -> Name
mkName typ xs =
  Name
    { nameWords = xs,
      nameType = typ
    }

nameOfExecEngineRole :: Name
nameOfExecEngineRole =
  Name
    { nameWords = NamePart (Text1 'E' "xecEngine") :| [],
      nameType = RoleName
    }

nameOfONE :: Name
nameOfONE =
  Name
    { nameWords = NamePart (Text1 'O' "NE") :| [],
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
  deriving (Show, Eq, Data, Enum, Bounded)

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
  nameSpaceOf :: a -> [NamePart]
  nameSpaceOf = NE.init . nameWords . name
  plainNameOf1 :: a -> NamePart
  plainNameOf1 = NE.last . nameWords . name
  plainNameOf :: a -> Text
  plainNameOf nm = T.cons h tl
    where
      NamePart (Text1 h tl) = plainNameOf1 nm
  updatedName :: NamePart -> a -> Name
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
prependToPlainName t nm =
  nm {nameWords = NE.reverse $ prepend t h NE.:| tl}
  where
    h NE.:| tl = NE.reverse . nameWords $ nm

prepend :: Text -> NamePart -> NamePart
prepend t (NamePart txt) = NamePart (t T1..<> txt)

postpend :: Text -> NamePart -> NamePart
postpend t (NamePart txt) = NamePart (txt T1.<>. t)

urlEncodedName :: Name -> Text1
urlEncodedName = toText1Unsafe . urlEncode . text1ToText . tName

-- Should be in RIO.NonEmpty:
prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList ls ne = case ls of
  [] -> ne
  (x : xs) -> x :| xs <> toList ne

class Named a => Rename a where
  rename :: a -> NamePart -> a

  -- | the function mkUniqueNames ensures case-insensitive unique names like sql plug names
  mkUniqueNames :: [Name] -> [a] -> [a]
  mkUniqueNames taken xs =
    [ p
      | cl <- eqCl (T.toLower . text1ToText . tName) xs,
        p <- -- each equivalence class cl contains (identified a) with the same map toLower (name p)
          if name (NE.head cl) `elem` taken || length cl > 1
            then [rename p (postpend (tshow i) (plainNameOf1 p)) | (p, i) <- zip (NE.toList cl) [(1 :: Int) ..]]
            else NE.toList cl
    ]

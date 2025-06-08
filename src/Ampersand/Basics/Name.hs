{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Ampersand.Basics.Name
  ( Named (..),
    Name(..),
    NamePart,
    NameSpace,
    NameType (..),
    Label (..),
    Labeled (..),
    mkName,
    nameOfONE,
    nameOfSESSION,
    nameOfExecEngineRole,
    withNameSpace,
    prependToPlainName,
    urlEncodedName,
    splitOnDots,
    namePartToText,
    namePartToText1,
    toNamePart,
    toNamePart1,
    toNamePartText1,
    postpend,
    checkProperId,
    suggestName,
    toMyDotNode,
    MyDotNode,
  )
where

import Ampersand.Basics.Hashing
import Ampersand.Basics.Prelude
import Ampersand.Basics.String (isSafeIdChar, text1ToText, toText1Unsafe, urlEncode)
import Ampersand.Basics.Version (fatal)
import qualified Data.GraphViz.Printing as GVP
import qualified Data.Text1 as T1
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL

type NameSpace = [NamePart]

-- A namepart is a single word, that starts with an alphanumeric character
-- and may contain of alphanumeric characters and digits only.
newtype NamePart = NamePart Text1 deriving (Data, Eq, Ord)

toNamePartText1 :: Text1 -> NamePart
toNamePartText1 = NamePart

instance Show NamePart where
  show (NamePart t1) = T.unpack $ text1ToText t1

data Name = Name
  { -- | A name in a namespace can be seen as a nonempty list of words.
    --   currently, we only deal with 'absolute' names.
    --   the separator that is inbetween the nameParts can be depending on the specific environment.
    --   in an .adl file, we will assume a dot `.` as separator.
    nameParts :: !(NonEmpty NamePart),
    nameType :: !NameType
  }
  deriving (Data)

instance Ord Name where
  compare a b = compare (nameParts a) (nameParts b)

instance Eq Name where
  a == b = compare a b == EQ

instance Show Name where
  show = T.unpack . fullName

instance Hashable Name where
  hashWithSalt s = hashWithSalt s . fullName

instance Named Name where
  name = id

instance GVP.PrintDot Name where
  unqtDot = GVP.text . TL.fromStrict . fullName

-- | toNamePart will convert a Text to a NamePart, iff the Text is a proper ID. (See checkProperId)
toNamePart :: Text -> Maybe NamePart
toNamePart txt = case T.uncons txt of
  Nothing -> Nothing
  Just (h, tl) -> toNamePart1 $ Text1 h tl

-- | toNamePart1 will convert a Text1 to a NamePart, iff the Text1 is a proper ID. (See checkProperId)
toNamePart1 :: Text1 -> Maybe NamePart
toNamePart1 x = case checkProperId x of
  Nothing -> Nothing
  Just np -> Just (NamePart np)

-- | suggestName checks if the given text is a proper name. If not, it proposes a proper name based
--   on the given text. In that case, the given text is converted into a Label. If the given text
--   was good enough for a proper name, no Label is returned.
suggestName :: NameType -> Text1 -> (Name, Maybe Label)
suggestName typ txt =
  ( mkName typ . fmap fst $ parts,
    if and . NE.toList . fmap snd $ parts
      then Nothing
      else Just . Label . text1ToText $ txt
  )
  where
    parts = suggestedPart <$> splitOnDots txt
    suggestedPart :: Text1 -> (NamePart, Bool)
    suggestedPart x@(Text1 h tl) = case checkProperId x of
      Just _ -> (NamePart x, True)
      Nothing -> (NamePart suggestion, False)
      where
        suggestion = toText1Unsafe $ pre <> rest
          where
            pre =
              if isSafeIdChar True h
                then T.singleton h
                else "X" <> substitute h
            rest = T.concat $ substitute <$> T.unpack tl
            substitute :: Char -> Text
            substitute c =
              if isSafeIdChar False c
                then T.singleton c
                else "_"

-- | This function checks if a text is a proper Id, if so, it returns the text
checkProperId :: Text1 -> Maybe Text1
checkProperId t@(Text1 h tl) =
  if isProper
    then Just t
    else Nothing
  where
    isProper = and (isSafeIdChar True h : (isSafeIdChar False <$> T.unpack tl))

namePartToText :: NamePart -> Text
namePartToText (NamePart x) = text1ToText x

namePartToText1 :: NamePart -> Text1
namePartToText1 (NamePart x) = x

mkName :: NameType -> NonEmpty NamePart -> Name
mkName typ xs =
  Name
    { nameParts = xs,
      nameType = typ
    }

nameOfExecEngineRole :: Name
nameOfExecEngineRole =
  Name
    { nameParts = NamePart (Text1 'E' "xecEngine") :| [],
      nameType = RoleName
    }

nameOfONE :: Name
nameOfONE =
  Name
    { nameParts = NamePart (Text1 'O' "NE") :| [],
      nameType = ConceptName
    }

nameOfSESSION :: Name
nameOfSESSION =
  Name
    { nameParts = NamePart (Text1 'S' "ESSION") :| [],
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
    { nameParts = prependList ns (nameParts nm),
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
  fullName1 :: a -> Text1
  fullName1 = toText1Unsafe . fullName
  fullName :: a -> Text
  fullName =
    mconcat
      . L.intersperse "."
      . toList
      . fmap namePartToText
      . nameParts
      . name
  nameSpaceOf :: a -> [NamePart]
  nameSpaceOf = NE.init . nameParts . name
  localName :: a -> NamePart
  localName = NE.last . nameParts . name
  localNameOf :: a -> Text
  localNameOf = namePartToText . localName
  updatedName :: NamePart -> a -> Name
  updatedName txt1 x = Name ws' typ
    where
      Name ws typ = name x
      ws' = NE.reverse (txt1 NE.:| (reverse . NE.tail) ws)

newtype Label = Label Text
  deriving (Data, Ord, Eq)

class (Named a) => Labeled a where
  {-# MINIMAL mLabel #-}
  mLabel :: a -> Maybe Label
  label :: a -> Text
  label x = case mLabel x of
    Nothing -> localNameOf x
    Just (Label lbl) -> lbl

instance Show Label where
  show (Label x) = T.unpack x

prependToPlainName :: Text -> Name -> Name
prependToPlainName
  txt
  Name
    { nameParts = ws,
      nameType = typ
    } =
    Name
      { nameParts =
          prependList (NE.init ws)
            . singleton
            . prepend txt
            . NE.last
            $ ws,
        nameType = typ
      }
    where
      prepend :: Text -> NamePart -> NamePart
      prepend t1 (NamePart (Text1 c t2)) =
        NamePart
          ( case T.uncons (t1 <> T.cons c t2) of
              Nothing -> fatal "impossible"
              Just (h, tl) -> Text1 h tl
          )
      singleton :: a -> NonEmpty a
      singleton = pure

postpend :: Text -> NamePart -> NamePart
postpend t (NamePart txt) = NamePart (txt T1.<>. t)

urlEncodedName :: Name -> Text1
urlEncodedName = toText1Unsafe . urlEncode . fullName

-- Should be in RIO.NonEmpty:
prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList ls ne = case ls of
  [] -> ne
  (x : xs) -> x :| xs <> toList ne

-- | Special type for use in Dot language (Graphviz). It needs to be an instance
--   of Show, but must be quoted, because of the '.' in the fullname.
newtype MyDotNode = MyDotNode Name

toMyDotNode :: (Named a) => a -> MyDotNode
toMyDotNode = MyDotNode . name

instance Show MyDotNode where
  show (MyDotNode nm) = show (fullName nm)

instance GVP.PrintDot MyDotNode where
  unqtDot (MyDotNode x) = GVP.unqtDot (abs . hash . fullName $ x)

instance Ord MyDotNode where
  a `compare` b = tshow a `compare` tshow b

instance Eq MyDotNode where
  a == b = compare a b == EQ

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Ampersand.Basics.Name
  ( Label (..),
    Labeled (..),
    --    mkName,
    MyDotNode,
    mkName,
    Name,
    Named (..),
    nameOfExecEngineRole,
    nameOfONE,
    nameOfSESSION,
    NamePart,
    namePartToText,
    namePartToText1,
    NameSpace,
    NameType (..),
    postpend,
    prependToPlainName,
    suggestName,
    toMyDotNode,
    try2Name,
    try2Namepart,
    urlEncodedName,
    withNameSpace,
  )
where

import Ampersand.Basics.Hashing
import Ampersand.Basics.Prelude
import Ampersand.Basics.String (isSafeIdChar, pascal, text1ToText, toText1Unsafe, urlEncode)
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
  compare = compare `on` nameParts

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

-- | This function checks if a text is a proper Namepart.
-- Validation for the rules for the words in a Name. These rules are based on the rules for
--   mariaDB names for tables and columns. (https://mariadb.com/kb/en/columnstore-naming-conventions/)
--   1) Table and column names are restricted to alphanumeric and underscore only, i.e "A-Z a-z 0-9 _".
--      However, the names are also used in math expressions in the generated documentation. Underscore
--      has a special meaning in math expressions, so we do not allow it.
--   2) The first character of all table and column names must be an ASCII letter (a-z A-Z).
--   3) ColumnStore reserves certain words that MariaDB does not, such as SELECT, CHAR and TABLE, so even wrapped in backticks these cannot be used.
-- There are three possible outcomes:
--   * Right (Name, Nothing). The text is a proper Name.
--   * Right (Name, Just Label). The text is not a proper Name, but well enough to suggest a name. The Label contains the original text.
--   * Left Text. The returned text contains a message suited as error message. Conversion to a Name failed.
try2Name :: NameType -> Text -> Either Text (Name, Maybe Label)
try2Name typ txt =
  case partitionEithers resultsOfParts of
    ([], []) -> Left "A name must not be empty."
    ([], p : ps) ->
      -- Standard case, no suggestions required.
      Right (mkName typ (p :| ps), Nothing)
    (otherParts, _) -> case partitionEithers otherParts of
      ([], _) ->
        -- Every non-proper namepart has a suggestion
        Right (mkName typ . harvestsuggested $ resultsOfParts, Just . Label $ txt)
      (err : _, _) -> Left err
  where
    harvestsuggested :: [Either (Either Text NamePart) NamePart] -> NonEmpty NamePart
    -- \| all nameparts must be either a proper NamePart or a suggestion for a NamePart
    harvestsuggested lst = case foo <$> lst of
      [] -> fatal "impossible"
      (h : tl) -> h NE.:| tl
      where
        foo :: Either (Either Text NamePart) NamePart -> NamePart
        foo (Right np) = np
        foo (Left (Right np)) = np
        foo (Left (Left err)) = fatal $ "all nameparts must be either a proper NamePart or a suggestion for a NamePart. " <> err
    resultsOfParts = try2Namepart <$> splitOnDots txt

-- | This function checks if a text is a proper Namepart. There are three possible outcomes:
--   * Right NamePart. The text is a proper NamePart.
--   * Left (Right NamePart. The text is not a proper NamePart, but a suggestion is returned.
--   * Left (Left Text). The text is not a proper NamePart. An text message is returned about the reason.
try2Namepart :: Text -> Either (Either Text NamePart) NamePart
try2Namepart t = case T.uncons t of
  Nothing -> Left $ Left "Empty namepart"
  Just (h, tl) ->
    if and (isSafeIdChar True h : (isSafeIdChar False <$> T.unpack tl))
      then Right (NamePart (Text1 h tl))
      else case T.uncons . T.filter (isSafeIdChar False) $ t of
        Nothing -> Left $ Left $ "No valid characters in namepart: " <> t
        Just (h', tl') ->
          if isSafeIdChar True h'
            then Right . NamePart $ Text1 h' tl'
            else Left . Left $ "First character of namepart must be alphanumeric: " <> t

suggestName :: NameType -> Text1 -> (Name, Maybe Label)
suggestName typ t =
  case try2Name typ . pascal . text1ToText $ t of
    Left msg -> fatal $ "suggestName: " <> msg
    Right (nm, _) ->
      ( nm,
        if text1ToText t == tshow nm then Nothing else Just . Label . text1ToText $ t
      )

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

splitOnDots :: Text -> [Text]
splitOnDots = T.split (== '.')

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
  compare = compare `on` tshow

instance Eq MyDotNode where
  a == b = compare a b == EQ

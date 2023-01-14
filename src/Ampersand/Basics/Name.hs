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
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL

type NameSpace = [Text1]

data Name = Name
  { -- | The plain name
    plainName :: !Text1,
    -- | The namespace where the name resides in.
    nameSpace :: !NameSpace,
    -- | The type of the thing that this name is for
    nameType :: !NameType
  }
  deriving (Data)

instance Ord Name where
  compare a b = compare (tshow a) (tshow b)

instance Eq Name where
  a == b = compare a b == EQ

instance Show Name where
  show x = T.unpack . mconcat . L.intersperse "." $ (text1ToText <$> nameSpace x) <> [text1ToText $ plainName x]

instance Hashable Name where
  hashWithSalt s = hashWithSalt s . text1ToText . tName

instance Named Name where
  name = id

instance GVP.PrintDot Name where
  unqtDot = GVP.text . TL.fromStrict . text1ToText . tName

nameOfExecEngineRole :: Name
nameOfExecEngineRole =
  Name
    { plainName = Text1 'E' "xecEngine",
      nameSpace = [],
      nameType = RoleName
    }

nameOfONE :: Name
nameOfONE =
  Name
    { plainName = Text1 'O' "NE",
      nameSpace = [],
      nameType = ConceptName
    }

data NameType = ConceptName | RelationName | RuleName | PatternName | ContextName | RoleName | TemporaryDummy
  deriving (Data)

toName :: NameSpace -> Text1 -> Name
toName space plainname =
  Name
    { plainName = mkValid plainname,
      nameSpace = space,
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

-- | anything could have some name, can't it?
class Named a where
  {-# MINIMAL name #-}
  name :: a -> Name
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

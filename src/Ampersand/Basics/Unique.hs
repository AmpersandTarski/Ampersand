{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- The purpose of class Unique is to identify a Haskell object by means of a string.
E.g.
instance Unique Pattern where
 showUnique = tName
-}

module Ampersand.Basics.Unique
  ( Unique (..),
  )
where

import Ampersand.Basics.Prelude
import Ampersand.Basics.String (isSafeIdChar, text1ToText, toText1Unsafe)
import Ampersand.Basics.Version (fatal)
import Data.Hashable
import Data.Typeable
import qualified RIO.Set as Set
import qualified RIO.Text as T

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
  uniqueShowWithType x = toText1Unsafe $ tshow (typeOf x) <> ("_" <> (text1ToText . showUnique $ x))

  -- | A function to show a unique instance. It is the responsability
  --   of the instance definition to make sure that for every a, b of
  --   an individual type:
  --        a == b  <==> showUnique a == showUnique b
  showUnique :: e -> Text1

  {-# MINIMAL showUnique #-}

  idWithoutType :: e -> Text1
  idWithoutType =
    uniqueButNotTooLong -- because it could be stored in an SQL database
    --  . escapeIdentifier -- escape because a character safe identifier is needed for use in URLs, filenames and database ids
      . checkProperId
      . showUnique

  idWithType :: e -> Text1
  idWithType e =
    uniqueButNotTooLong -- because it could be stored in an SQL database
      . addType e
      --  . escapeIdentifier -- escape because a character safe identifier is needed for use in URLs, filenames and database ids
      . checkProperId
      $ showUnique e

  addType :: e -> Text1 -> Text1
  addType x string = toText1Unsafe $ tshow (typeOf x) <> "_" <> text1ToText string

-- | This function
checkProperId :: Text1 -> Text1
checkProperId t@(Text1 h tl) =
  if isProper
    then t
    else fatal $ "Not a proper Id: " <> text1ToText t
  where
    isProper = and (isSafeIdChar True h : (isSafeIdChar False <$> T.unpack tl))

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
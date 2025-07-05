{-# LANGUAGE DeriveDataTypeable #-}

module Ampersand.Basics.Prelude
  ( module RIO,
    readUTF8File,
    readUTF8FileLenient,
    zipWith,
    openTempFile,
    Verbosity (..),
    FirstTrue (..),
    fromFirstTrue,
    reads,
    getChar,
    defaultFirstTrue,
    FirstFalse (..),
    fromFirstFalse,
    defaultFirstFalse,
    decodeUtf8,
    foldl,
    undefined,
    Text1 (..),
    unsnoc,
  )
where

-- Needs to be fixed later. See https://haskell.fpcomplete.com/library/rio we'll explain why we need this in logging

import qualified Data.Text.Encoding as T
import Data.Text1 (Text1 (..))
import RIO hiding (exitWith, undefined, zipWith)
import qualified RIO as WarnAbout (undefined)
import qualified RIO.ByteString as SB
import RIO.Directory (doesFileExist)
import qualified RIO.Text as T
import System.IO (openTempFile)
import Prelude (getChar, reads)

data Verbosity = Loud | Silent deriving (Eq, Data, Show)

readUTF8FileLenient :: FilePath -> RIO env (Either [Text] Text)
readUTF8FileLenient fp = do
  exists <- doesFileExist fp
  if exists
    then liftIO (Right . T.decodeUtf8With T.lenientDecode <$> SB.readFile fp)
    else return $ Left ["File does not exist: " <> T.pack fp]

-- Wrapper around readFileUtf8. It exits with an error:
readUTF8File :: FilePath -> RIO env (Either [Text] Text)
readUTF8File fp = (Right <$> readFileUtf8 fp) `catch` handler
  where
    handler :: IOException -> RIO env (Either [Text] a)
    handler err =
      return
        . Left
        $ [ "File could not be read: " <> T.pack fp,
            tshow err
          ]

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith fun = go
  where
    go [] _ = []
    go _ [] = []
    go (x' : xs) (y : ys) = fun x' y : go xs ys

-- Redefine foldl to ensure that we use foldl' everywhere. But make the Haskeller
-- aware that in fact you should use fold'.
foldl :: (Foldable t) => (b -> a -> b) -> b -> t a -> b
{-# WARNING foldl "Please do not use foldl. Use foldl' instead. It is more performant." #-}
foldl = foldl'

-- Redefine undefined to ensure that it isn't accidentally used.
undefined :: a
{-# WARNING undefined "Undefined statement left in code. Why not use fatal?" #-}
undefined = WarnAbout.undefined

-- Functions copied from stack

-- | Like @First Bool@, but the default is @True@.
newtype FirstTrue = FirstTrue {getFirstTrue :: Maybe Bool}
  deriving (Show, Eq, Ord)

instance Semigroup FirstTrue where
  FirstTrue (Just x) <> _ = FirstTrue (Just x)
  FirstTrue Nothing <> x = x

instance Monoid FirstTrue where
  mempty = FirstTrue Nothing
  mappend = (<>)

-- | Get the 'Bool', defaulting to 'True'
fromFirstTrue :: FirstTrue -> Bool
fromFirstTrue = fromMaybe True . getFirstTrue

-- | Helper for filling in default values
defaultFirstTrue :: (a -> FirstTrue) -> Bool
defaultFirstTrue _ = True

-- | Like @First Bool@, but the default is @False@.
newtype FirstFalse = FirstFalse {getFirstFalse :: Maybe Bool}
  deriving (Show, Eq, Ord)

instance Semigroup FirstFalse where
  FirstFalse (Just x) <> _ = FirstFalse (Just x)
  FirstFalse Nothing <> x = x

instance Monoid FirstFalse where
  mempty = FirstFalse Nothing
  mappend = (<>)

-- | Get the 'Bool', defaulting to 'False'
fromFirstFalse :: FirstFalse -> Bool
fromFirstFalse = fromMaybe False . getFirstFalse

-- | Helper for filling in default values
defaultFirstFalse :: (a -> FirstFalse) -> Bool
defaultFirstFalse _ = False

decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode

-- functions missing in RIO

-- | Returns all but the last character and the last character of a
-- 'Text', or 'Nothing' if empty.
--
-- @since 1.2.3.0
unsnoc :: Text -> Maybe (Text, Char)
unsnoc t = case T.uncons (T.reverse t) of
  Nothing -> Nothing
  Just (h, tl) -> Just (T.reverse tl, h)

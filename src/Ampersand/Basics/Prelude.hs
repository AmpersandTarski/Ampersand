{-# LANGUAGE DeriveDataTypeable #-}
module Ampersand.Basics.Prelude
  ( module RIO
--  , module Data.Monoid
  , say', sayLn
  , sayWhenLoudLn
  , writeFile
  , readUTF8File
  , zipWith
  , openTempFile
  , Verbosity (..)
  , FirstTrue (..)
  , fromFirstTrue
  , reads, getChar
  , defaultFirstTrue
  , FirstFalse (..)
  , fromFirstFalse
  , defaultFirstFalse
  , decodeUtf8
  )where
import Prelude (reads,getChar) -- Needs to be fixed later. See https://haskell.fpcomplete.com/library/rio we'll explain why we need this in logging
import RIO hiding (zipWith,exitWith)
import System.IO (openTempFile, stderr)
import qualified RIO.Text as T


data Verbosity = Loud | Silent deriving (Eq, Data, Show)

-- Functions to be upgraded later on:
sayLn :: (HasLogFunc env) => String -> RIO env ()
sayLn msg = do
  logInfo . display . T.pack $ msg
--  h <- view handleL
--  liftIO $ hPutStrLn h msg
say' :: (HasLogFunc env) => String -> RIO env ()
say' msg = do 
  logInfo . display . T.pack $ msg
sayWhenLoudLn :: (HasLogFunc env) => String -> RIO env ()
sayWhenLoudLn msg = do
  logDebug . display . T.pack $ msg
--  v <- view verbosityL
--  case v of
--    Loud   -> do
--        h <- view handleL
--        hSetBuffering h NoBuffering
--        mapM_ sayLn (lines msg)
--    Silent -> return ()

-- Functions to be replaced later on:
writeFile :: FilePath -> String -> IO ()
writeFile fp x = writeFileUtf8 fp . T.pack $ x
readUTF8File :: FilePath -> RIO env (Either [String] T.Text)
readUTF8File fp = (Right <$> readFileUtf8 fp) `catch` handler
  where 
     handler :: IOException -> RIO env (Either [String] a)
     handler err = return . Left $
         [ "Error reading "<> fp
         , show $ err
         ]

zipWith :: (a->b->c) -> [a]->[b]->[c]
zipWith fun = go
  where
    go [] _ = []
    go _ [] = []
    go (x':xs) (y:ys) = fun x' y : go xs ys


-- Functions copied from stack
-- | Like @First Bool@, but the default is @True@.
newtype FirstTrue = FirstTrue { getFirstTrue :: Maybe Bool }
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
newtype FirstFalse = FirstFalse { getFirstFalse :: Maybe Bool }
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
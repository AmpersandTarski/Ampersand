{-# LANGUAGE DeriveDataTypeable #-}
module Ampersand.Basics.Prelude
  ( module Prelude
  , module RIO
  , putStr, putStrLn
  , verbose, verboseLn
  , writeFile
  , readUTF8File
  , zipWith
  , openTempFile
  , HasHandle(..)
  , HasVerbosity(..), Verbosity (..)
  )where
import Prelude (reads,getChar) -- Needs to be fixed later. See https://haskell.fpcomplete.com/library/rio we'll explain why we need this in logging
import RIO
import System.IO (openTempFile,hPutStr,hPutStrLn, stderr)
import qualified RIO.Text as T
-- import Debug.Trace
-- import Prelude hiding ( 
--                    getContents
--                  , putStr
--                  , putStrLn
--                  , readFile
--                  , writeFile
--                       )

class HasHandle env where
  handleL :: Lens' env Handle
instance HasHandle Handle where
  handleL = id  

data Verbosity = Loud | Silent deriving (Eq, Data)
class HasVerbosity env where
  verbosityL :: Lens' env Verbosity  

-- Functions to be upgraded later on:
putStrLn :: HasHandle env => String -> RIO env ()
putStrLn msg = do
  h <- view handleL
  liftIO $ hPutStrLn h msg
putStr :: HasHandle env => String -> RIO env ()
putStr msg = do 
  h <- view handleL
  liftIO $ hPutStr h msg
verbose :: (HasHandle env, HasVerbosity env) => String -> RIO env ()
verbose msg = do
  v <- view verbosityL
  case v of
    Loud   -> putStr msg
    Silent -> return ()
verboseLn :: (HasHandle env, HasVerbosity env) => String -> RIO env ()
verboseLn msg = do
  v <- view verbosityL
  case v of
    Loud   -> do
        h <- view handleL
        hSetBuffering h NoBuffering
        mapM_ putStrLn (lines msg)
    Silent -> return ()

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

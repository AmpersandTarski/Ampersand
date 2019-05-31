module Ampersand.Basics.Prelude
  ( module Prelude
  , module RIO
  , putStr, putStrLn
  , verbose, verboseLn
  , writeFile
  , readUTF8File
  , zipWith
  , openTempFile
  , HasHandles(..)
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

class HasHandles env where
  getHandle :: env -> Handle
instance HasHandles Handle where
  getHandle = id  

data Verbosity = Loud | Silent deriving Eq
class HasVerbosity env where
 getVerbosity :: env -> Verbosity  
-- Functions to be upgraded later on:
putStrLn :: HasHandles env => String -> RIO env ()
putStrLn msg = do
  env <- ask
  liftIO $ hPutStrLn (getHandle env) msg
putStr :: HasHandles env => String -> RIO env ()
putStr msg = do 
  env <- ask
  liftIO $ hPutStr (getHandle env) msg
verbose :: (HasHandles env, HasVerbosity env) => String -> RIO env ()
verbose msg = do
  env <- ask
  case getVerbosity env of
    Loud   -> putStr msg
    Silent -> return ()
verboseLn :: (HasHandles env, HasVerbosity env) => String -> RIO env ()
verboseLn msg = do
  env <- ask
  case getVerbosity env of
    Loud   -> do
        hSetBuffering (getHandle env) NoBuffering
        mapM_ putStrLn (lines msg)
    Silent -> return ()

-- Functions to be replaced later on:
writeFile :: FilePath -> String -> IO ()
writeFile fp x = writeFileUtf8 fp . T.pack $ x
readUTF8File :: FilePath -> RIO env (Either String T.Text)
readUTF8File fp = (Right <$> readFileUtf8 fp) `catch` handler
  where 
     handler :: IOException -> RIO env (Either String a)
     handler err = return . Left . show $ err

zipWith :: (a->b->c) -> [a]->[b]->[c]
zipWith fun = go
  where
    go [] _ = []
    go _ [] = []
    go (x':xs) (y:ys) = fun x' y : go xs ys

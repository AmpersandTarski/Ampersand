module Ampersand.Basics.Prelude
  ( module Prelude
  , module RIO
  , writeFile
  , readUTF8File
  , zip3, zipWith
  , openTempFile
  )where
import Prelude (putStrLn,putStr,reads,getChar) -- Needs to be fixed later. See https://haskell.fpcomplete.com/library/rio we'll explain why we need this in logging
import RIO
import System.IO (openTempFile)

-- import Debug.Trace
-- import Prelude hiding ( 
--                    getContents
--                  , putStr
--                  , putStrLn
--                  , readFile
--                  , writeFile
--                       )

-- Functions to be replaced later on:
writeFile :: FilePath -> String -> IO ()
writeFile fp x = writeFileUtf8 fp . tshow $ x
readUTF8File :: FilePath -> IO (Either String String)
readUTF8File fp = (Right . show <$> readFileUtf8 fp) `catch` handler
  where 
     handler :: IOException -> IO (Either String String)
     handler err = return . Left . show $ err
-- | 'zip3' takes three lists and returns a list of triples, analogous to
-- 'zip'.
zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
-- Specification
-- zip3 =  zipWith3 (,,)
zip3 (a:as) (b:bs) (c:cs) = (a,b,c) : zip3 as bs cs
zip3 _      _      _      = []

zipWith :: (a->b->c) -> [a]->[b]->[c]
zipWith fun = go
  where
    go [] _ = []
    go _ [] = []
    go (x':xs) (y:ys) = fun x' y : go xs ys

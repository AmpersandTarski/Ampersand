module Ampersand.Basics.Prelude
  ( module Prelude
  , module RIO
  , writeFile
  , zip3
  , maybeRead
  )where
import Prelude (putStrLn,putStr,reads) -- Needs to be fixed later. See https://haskell.fpcomplete.com/library/rio we'll explain why we need this in logging
import RIO

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

-- | 'zip3' takes three lists and returns a list of triples, analogous to
-- 'zip'.
zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
-- Specification
-- zip3 =  zipWith3 (,,)
zip3 (a:as) (b:bs) (c:cs) = (a,b,c) : zip3 as bs cs
zip3 _      _      _      = []

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
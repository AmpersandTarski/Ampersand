module Ampersand.Basics.Prelude
  ( module Prelude
  , module RIO
  , writeFile
  )where
import Prelude (putStrLn,putStr) -- Needs to be fixed later. See https://haskell.fpcomplete.com/library/rio we'll explain why we need this in logging
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


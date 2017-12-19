module Ampersand.Basics.Prelude
  ( module Debug.Trace
  , module Prelude
  )where

import Debug.Trace
import Prelude hiding ( 
                   getContents
                 , putStr
                 , putStrLn
                 , readFile
                 , writeFile
                      )
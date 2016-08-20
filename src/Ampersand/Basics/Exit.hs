{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.Basics.Exit 
         ( exitWith
         , fatalExit
         ) where

import qualified System.Exit as SE
import System.IO.Unsafe

fatalExit :: [String] -> a
fatalExit = exitWith . Fatal

{-# NOINLINE exitWith #-}
exitWith :: AmpersandExit -> a
exitWith x = unsafePerformIO $ do
	exitIO (message x)
	SE.exitWith (exitcode x)

exitIO :: [String] -> IO()
exitIO = mapM_ putStrLn

data AmpersandExit 
  = Succes
  | Fatal [String]

exitcode :: AmpersandExit -> SE.ExitCode
exitcode Succes  = SE.ExitSuccess
exitcode Fatal{} = SE.ExitFailure 1

message :: AmpersandExit -> [String]
message Succes = []
message (Fatal xs) = xs

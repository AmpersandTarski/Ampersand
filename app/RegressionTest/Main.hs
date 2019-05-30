module Main(main) where
import MainApps

main :: IO ()
main = do
   x <- regressionTest
   exitWith x
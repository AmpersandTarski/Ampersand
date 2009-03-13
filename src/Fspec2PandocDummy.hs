
module Fspec2PandocDummy (fSpec2Pandoc,render2Pandoc{-,writeRTF,writeLaTeX-})
where

   import FspecDef
   import Options
   
   render2Pandoc :: Options -> Pandoc -> String
   render2Pandoc _ pandoc = show pandoc

   fSpec2Pandoc :: Fspc -> Options -> Pandoc
   fSpec2Pandoc fSpec flags = Pandoc "Helaas, Pandoc is nog niet in gebruik...."

   
   data Pandoc = Pandoc String deriving Show
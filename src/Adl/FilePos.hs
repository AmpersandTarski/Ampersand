{-# OPTIONS_GHC -Wall #-}
-- Deze module kan niet warningvrij worden gemaakt, omdat er twee orphan instances
-- in voorkomen. (Zie de discussie op http://lukepalmer.wordpress.com/2009/01/25/a-world-without-orphans/)
module Adl.FilePos 
where
   import UU_Scanner (Pos(Pos),noPos)

   newtype FilePos = FilePos (String, Pos, String) 
                          deriving (Eq,Ord)
   posNone :: FilePos
   posNone = FilePos ("",noPos,"")
   instance Ord Pos where
     a >= b = (show a) >= (show b)
     a <= b = (show a) <= (show b)
     
   instance Show Pos where
     show (Pos l c)
       = "line " ++ show l
         ++ ", column " ++ show c

   instance Show FilePos where
     show (FilePos (fn,Pos l _,_))
       = "line " ++ show l
         ++ ", file " ++ show fn
     
   class Numbered a where
    nr :: a->Int
    pos :: a->FilePos
    nr x = nr (pos x)

   instance Numbered FilePos where
    nr (FilePos (_,Pos l _,_)) = l
    pos p = p

     
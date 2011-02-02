{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.ADL.FilePos    ( FilePos(FilePos,Nowhere)
                      , Numbered(nr,pos))
where
   import UU_Scanner (Pos(Pos))

   data FilePos = FilePos ( String, Pos, String)
                | Nowhere 
                        --  deriving (Eq,Ord)

   instance Eq FilePos where
       fp == fp' = case (fp,fp') of
                 (Nowhere,Nowhere) -> True
                 (FilePos(a,b,c),FilePos(a',b',c'))
                                   -> a==a' && b==b' && c==c'
                 (_,_)             -> False

   instance Ord FilePos where
       compare f f' = compare (nr f) (nr f')

--   posNone :: FilePos
--   posNone = FilePos ("",noPos,"")
--   instance Ord Pos where
--     a >= b = (show a) >= (show b)
--     a <= b = (show a) <= (show b)
--     
--   instance Show Pos where
--     show (Pos l c)
--       = "line " ++ show l
--         ++ ", column " ++ show c
--
   instance Show FilePos where
     show (FilePos (fn,Pos l _,_))
       = "line " ++ show l
         ++ ", file " ++ show fn
     show Nowhere
       = "Nowhere"    -- Valt nog te bezien of je hier eigenlijk wel wat wilt laten zien...
       
   class Numbered a where
    nr :: a->Int
    pos :: a->FilePos
    nr x = nr (pos x)

   instance Numbered FilePos where
    nr (FilePos (_,Pos l _,_)) = l
    nr Nowhere = 0
    
    pos p = p

     

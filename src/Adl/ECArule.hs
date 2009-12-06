{-# OPTIONS_GHC -Wall #-}
module Adl.ECArule (InsDel(..)
                   ,ECArule(..)
                   ,Event(..)
                   ,PAclause(..)
                   ,isAll
                   ,isChc
                   ,isBlk
                   ,isDry
                   ,isNop
                   ,isDo
                   )
where
import Adl.Rule                      (Rule)
import Adl.FilePos                   (nr)
import Adl.Expression                (Expression)
import Adl.MorphismAndDeclaration    (Morphism)
import Adl.Concept                   (Concept)
import Strings                       (commaEng)


-- | The following datatypes form a process algebra. ADL derives the process logic from the static logic by interpreting an expression in relation algebra as an invariant.
--   An example: suppose you have large shoes, which means that there is no way you can fit you shoes through your trousers. What does this mean for the process of dressing in the morning? Well, if the shoes won't fit through your trousers, you must first put on your trousers, and then put on your shoes. So the order of putting on trousers and putting on shoes is dictated by the (static) fact that your shoes are too big to fit through your trousers. When undressing, the order is reversed: you must take off your shoes before taking off your trousers. This example ilustrates how the order of activities is restricted by an invariant property. So it is possible to derive some dynamic behaviour from static properties.

data InsDel   = Ins | Del
                deriving (Show,Eq)
data ECArule  = ECA { ecaTriggr :: Event         -- The event on which this rule is activated
                    , ecaDelta  :: Morphism      -- The delta to be inserted or deleted from this rule. It actually serves very much like a formal parameter.
                    , ecaAction :: PAclause      -- The action to be taken when triggered.
                    , ecaNum    :: Int           -- A unique number that identifies the ECArule within its scope.
                    } deriving Show
data Event    = On { eSrt :: InsDel
                   , eMhp :: Morphism
                   } deriving (Show,Eq)
data PAclause = Chc { paCls   :: [PAclause]
                    , paMotiv :: [(Expression,[Rule])] -- tells which conjunct from which rule is being maintained
                    }
              | All { paCls   :: [PAclause]
                    , paMotiv :: [(Expression,[Rule])]
                    }
              | Do  { paSrt   :: InsDel                -- do Insert or Delete
                    , paTo    :: Expression            -- into toExpr    or from toExpr
                    , paDelta :: Expression            -- delta
                    , paMotiv :: [(Expression,[Rule])]
                    }
              | Sel { paCpt   :: Concept               -- pick an existing instance of type c
                    , paExp   :: Expression            -- the expression to pick from
                    , paCl    :: String->PAclause      -- the completion of the clause
                    , paMotiv :: [(Expression,[Rule])]
                    }
              | New { paCpt   :: Concept               -- make a new instance of type c
                    , paCl    :: String->PAclause      -- to be done after creating the concept
                    , paMotiv :: [(Expression,[Rule])]
                    }
              | Rmv { paCpt   :: Concept               -- remove an instance of type c
                    , paCl    :: String->PAclause      -- to be done after removing the concept
                    , paMotiv :: [(Expression,[Rule])]
                    }
              | Nop { paMotiv :: [(Expression,[Rule])]  -- tells which conjunct from which rule is being maintained
                    }
              | Blk { paMotiv :: [(Expression,[Rule])]  -- tells which expression from which rule has caused the blockage
                    }
              | Dry { paMotiv :: [(Expression,[Rule])]  -- same as block, but for a lack of viable options to choose from.
                    }

isAll :: PAclause -> Bool
isAll All{} = True
isAll _     = False

isChc :: PAclause -> Bool
isChc Chc{} = True
isChc _     = False

isBlk :: PAclause -> Bool
isBlk Blk{} = True
isBlk _     = False

isDry :: PAclause -> Bool
isDry Dry{} = True
isDry _     = False

isNop :: PAclause -> Bool
isNop Nop{} = True
isNop _     = False

isDo :: PAclause -> Bool
isDo Do{}   = True
isDo _      = False

instance Eq PAclause where
 Chc ds _ == Chc ds' _ = ds==ds'
 All ds _ == All ds' _ = ds==ds'
 p@Do{}   ==   p'@Do{} = paSrt p==paSrt p' && paTo p==paTo p' && paDelta p==paDelta p'
 Nop _    ==     Nop _ = True
 p@New{}  ==  p'@New{} = paCpt p==paCpt p'
 p@Rmv{}  ==  p'@Rmv{} = paCpt p==paCpt p'
 _ == _ = False

      
instance Show PAclause where
    showsPrec _ p = showString (showFragm "\n" p)
     where
      showFragm indent pa@Do{}
       = ( case paSrt pa of
            Ins -> "INSERT INTO "
            Del -> "DELETE FROM ")++
         show (paTo pa)++
         " SELECTFROM "++
         show (paDelta pa)
         ++motivate indent "TO MAINTAIN" (paMotiv pa)
      showFragm indent (New c clause m) = "CREATE x:"++show c++";"++indent++"    "++show (clause "x")++motivate indent "MAINTAINING" m
      showFragm indent (Rmv c clause m) = "REMOVE x:"++show c++";"++indent++"    "++show (clause "x")++motivate indent "MAINTAINING" m
      showFragm indent (Sel c e r m)    = "SELECT x:"++show c++" FROM codomain("++show e++");"
                                          ++indent++"    "++show (r "x")++motivate indent "MAINTAINING" m
      showFragm indent (Chc ds m)       = "ONE of "++concat [indent++"       "++showFragm (indent++"       ") d| d<-ds]++motivate indent "MAINTAINING" m
      showFragm indent (All ds m)       = "ALL of "++concat [indent++"       "++showFragm (indent++"       ") d| d<-ds]++motivate indent "MAINTAINING" m
      showFragm indent (Nop m)          = "DO NOTHING"++motivate indent "TO MAINTAIN" m
      showFragm indent (Blk m)          = "BLOCK"++motivate indent "CANNOT CHANGE" m
      showFragm indent (Dry m)          = "BLOCK"++motivate indent "NO RULES TO HANDLE" m

      motivate indent motive motives = concat [ indent++showConj m | m<-motives ]
       where showConj (conj,rs) = "("++motive++" "++show conj++" FROM "++commaEng "" ["R"++show (nr r)| r<-rs]++")"
      
{-
instance Show ECArule where
 showsPrec p er = showString (show (ecaTriggr er)++" "++show (ecaAction er))
instance Show Event where
 showsPrec p (On Ins m) = showString ("ON INSERT Delta IN "++show m)
 showsPrec p (On Del m) = showString ("ON DELETE Delta FROM "++show m)
-}


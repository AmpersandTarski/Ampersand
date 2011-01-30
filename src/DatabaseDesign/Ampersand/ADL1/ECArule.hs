{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.ADL1.ECArule (InsDel(..)
                   ,ECArule(..)
                   ,Event(..)
                   ,PAclause(..)
                   ,isAll
                   ,isChc
                   ,isBlk
                   ,isNop
                   ,isDo
                   )
where
import DatabaseDesign.Ampersand.ADL1.Rule                      (Rules)
--import DatabaseDesign.Ampersand.Input.ADL1.FilePos                   (nr)
import DatabaseDesign.Ampersand.ADL1.Expression                (Expression)
import DatabaseDesign.Ampersand.ADL1.MorphismAndDeclaration    (Relation,Identified)
import DatabaseDesign.Ampersand.ADL1.Concept                   (Concept)
import Data.List


-- | The following datatypes form a process algebra. Ampersand derives the process logic from the static logic by interpreting an expression in relation algebra as an invariant.
--   An example: suppose you have large shoes, which means that there is no way you can fit you shoes through your trousers. What does this mean for the process of dressing in the morning? Well, if the shoes won't fit through your trousers, you must first put on your trousers, and then put on your shoes. So the order of putting on trousers and putting on shoes is dictated by the (static) fact that your shoes are too big to fit through your trousers. When undressing, the order is reversed: you must take off your shoes before taking off your trousers. This example ilustrates how the order of activities is restricted by an invariant property. So it is possible to derive some dynamic behaviour from static properties.

data InsDel   = Ins | Del
                deriving (Show,Eq)
data ECArule c= ECA { ecaTriggr :: Event c               -- The event on which this rule is activated
                    , ecaDelta  :: Relation c            -- The delta to be inserted or deleted from this rule. It actually serves very much like a formal parameter.
                    , ecaAction :: PAclause (Relation c) -- The action to be taken when triggered.
                    , ecaNum    :: Int                   -- A unique number that identifies the ECArule within its scope.
                    } deriving (Show,Eq)
data Event c  = On { eSrt :: InsDel
                   , eMhp :: Relation c
                   } deriving (Show,Eq)
data PAclause r
              = Chc { paCls   :: [PAclause r]
                    , paMotiv :: [(Expression r,Rules r )] -- tells which conjunct from which rule is being maintained
                    }
              | All { paCls   :: [PAclause r]
                    , paMotiv :: [(Expression r,Rules r )]
                    }
              | Do  { paSrt   :: InsDel                -- do Insert or Delete
                    , paTo    :: Expression r            -- into toExpr    or from toExpr
                    , paDelta :: Expression r            -- delta
                    , paMotiv :: [(Expression r,Rules r )]
                    }
              | Sel { paCpt   :: Concept               -- pick an existing instance of type c
                    , paExp   :: Expression r            -- the expression to pick from
                    , paCl    :: String->PAclause r      -- the completion of the clause
                    , paMotiv :: [(Expression r,Rules r )]
                    }
              | New { paCpt   :: Concept               -- make a new instance of type c
                    , paCl    :: String->PAclause r      -- to be done after creating the concept
                    , paMotiv :: [(Expression r,Rules r )]
                    }
              | Rmv { paCpt   :: Concept               -- remove an instance of type c
                    , paCl    :: String->PAclause r      -- to be done after removing the concept
                    , paMotiv :: [(Expression r,Rules r )]
                    }
              | Nop { paMotiv :: [(Expression r,Rules r )]  -- tells which conjunct from which rule is being maintained
                    }
              | Blk { paMotiv :: [(Expression r,Rules r )]  -- tells which expression from which rule has caused the blockage
                    }

isAll :: PAclause r -> Bool
isAll All{} = True
isAll _     = False

isChc :: PAclause r -> Bool
isChc Chc{} = True
isChc _     = False

isBlk :: PAclause r -> Bool
isBlk Blk{} = True
isBlk _     = False

isNop :: PAclause r -> Bool
isNop Nop{} = True
isNop _     = False

isDo :: PAclause r -> Bool
isDo Do{}   = True
isDo _      = False

instance (Show r, Identified r, Eq r) => Eq (PAclause r) where
 Chc ds _ == Chc ds' _ = ds==ds'
 All ds _ == All ds' _ = ds==ds'
 p@Do{}   ==   p'@Do{} = paSrt p==paSrt p' && paTo p==paTo p' && paDelta p==paDelta p'
 Nop _    ==     Nop _ = True
 p@New{}  ==  p'@New{} = paCpt p==paCpt p'
 p@Rmv{}  ==  p'@Rmv{} = paCpt p==paCpt p'
 _ == _ = False

      
instance (Show r) => Show (PAclause r) where
    showsPrec _ p = showString (showFragm "\n> " p)
     where
      showFragm indent pa@Do{}
       = ( case paSrt pa of
            Ins -> "INSERT INTO "
            Del -> "DELETE FROM ")++
         show (paTo pa)++
         " SELECTFROM "++
         show (paDelta pa)
         ++motivate indent "TO MAINTAIN" (paMotiv pa)
      showFragm indent (New c clause m) = "CREATE x:"++indent++"       "++show c++";"++indent++"    "++showFragm (indent++"    ") (clause "x")++motivate indent "MAINTAINING" m
      showFragm indent (Rmv c clause m) = "REMOVE x:"++indent++"       "++show c++";"++indent++"    "++showFragm (indent++"    ") (clause "x")++motivate indent "MAINTAINING" m
      showFragm indent (Sel c e r m)    = "SELECT x:"++indent++"       "++show c++" FROM codomain("++show e++");"
                                          ++indent++"    "++showFragm (indent++"    ") (r "x")++motivate indent "MAINTAINING" m
      showFragm indent (Chc ds m)       = "ONE of"++indent++"       "++intercalate (indent++"       ") [showFragm (indent++"       ") d| d<-ds]++motivate indent "MAINTAINING" m
      showFragm indent (All ds m)       = "ALL of"++indent++"       "++intercalate (indent++"       ") [showFragm (indent++"       ") d| d<-ds]++motivate indent "MAINTAINING" m
      showFragm indent (Nop m)          = "DO NOTHING"++motivate indent "TO MAINTAIN" m
      showFragm indent (Blk m)          = "BLOCK"++motivate indent "CANNOT CHANGE" m

      motivate _ _ _ = [] -- concat [ indent++showConj m | m<-motives ]
--       where showConj (conj,rs) = "("++motive++" "++show conj++" FROM "++commaEng "" ["R"++show (nr r)| r<-rs]++")"
      
{-
instance Show ECArule where
 showsPrec p er = showString (show (ecaTriggr er)++" "++show (ecaAction er))
instance Show Event where
 showsPrec p (On Ins m) = showString ("ON INSERT Delta IN "++show m)
 showsPrec p (On Del m) = showString ("ON DELETE Delta FROM "++show m)
-}


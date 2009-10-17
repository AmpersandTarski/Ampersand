{-# OPTIONS_GHC -Wall #-}
module Adl.ECArule (InsDel(..)
                   ,ECArule(..)
                   ,Event(..)
                   ,PAclause(..))
where
import Adl.Expression                (Expression)
import Adl.MorphismAndDeclaration    (Morphism,Declaration)
import Adl.Concept                   (Concept)


-- | The following datatypes form a process algebra. ADL derives the process logic from the static logic by interpreting an expression in relation algebra as an invariant.
--   An example: suppose you have large shoes, which means that there is no way you can fit you shoes through your trousers. What does this mean for the process of dressing in the morning? Well, if the shoes won't fit through your trousers, you must first put on your trousers, and then put on your shoes. So the order of putting on trousers and putting on shoes is dictated by the (static) fact that your shoes are too big to fit through your trousers. When undressing, the order is reversed: you must take off your shoes before taking off your trousers. This example ilustrates how the order of activities is restricted by an invariant property. So it is possible to derive some dynamic behaviour from static properties.

data InsDel   = Ins | Del
                deriving (Eq,Show)
data ECArule  = ECA { ecaTriggr :: Event
                    , ecaAction :: PAclause
                    , ecaNum    :: Int
                    }
                  deriving Eq
data Event    = On { eSrt :: InsDel
                   , eMhp :: Morphism
                   }
                  deriving Eq
data PAclause = Choice { paCls:: [PAclause]
                       }
              | All { paCls   :: [PAclause]}
              | Do  { paSrt   :: InsDel         -- do Insert or Delete
                    , paTo    :: Expression     -- into toExpr    or from toExpr
                    , paDelta ::Expression     -- delta
                    }
              | New { paNew :: Concept }        -- make a new instance of type c
              | Rmv { paNew :: Concept }        -- remove an instance of type c
                  deriving (Eq, Show)

instance Show ECArule where
 showsPrec p (ECA event pa n) = showString (show event++" "++show pa)
instance Show Event where
 showsPrec p (On Ins m) = showString ("ON INSERT Delta IN "++show m)
 showsPrec p (On Del m) = showString ("ON DELETE Delta FROM "++show m)

-- instance Show PAclause where
--  showsPrec p fragm = showString ("ON "++show "\n  " fragm)


module Database.Design.Ampersand.ADL1.ECArule ( isAll
                                             , isCHC
                                             , isBlk
                                             , isNop
                                             , isDo
                                             , eventsFrom
                                             )
where
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.Basics     (fatal)

  --   Ampersand derives the process logic from the static logic by interpreting an expression in relation algebra as an invariant.
  --   So how does Ampersand derive dynamic behaviour from static rules? An example may clarify this:
  --   Suppose you have large shoes that do not fit through your trousers in any way.
  --   Does this have any consequences for the process of dressing in the morning?
  --   Well sure it has!
  --   Since your shoes won't fit through your trousers, you must first put on your trousers, and then put on your shoes.
  --   So the order of putting on trousers and putting on shoes is dictated by the (static) fact that your shoes are too big to fit through your trousers.
  --   When undressing, the order is reversed: you must take off your shoes before taking off your trousers.
  --   This example ilustrates how the order of activities is restricted by an invariant property.
  --   So it is possible to derive some dynamic behaviour from static properties.
  --   The following datatypes form a process algebra.

isAll :: PAclause -> Bool
isAll ALL{} = True
isAll _     = False

isCHC :: PAclause -> Bool
isCHC CHC{} = True
isCHC _     = False

isBlk :: PAclause -> Bool
isBlk Blk{} = True
isBlk _     = False

isNop :: PAclause -> Bool
isNop Nop{} = True
isNop _     = False

isDo :: PAclause -> Bool
isDo Do{}   = True
isDo _      = False

{-
             | Do  { paSrt :: InsDel                     -- do Insert or Delete
                    , paTo :: Declaration                 -- into toExpr    or from toExpr
                    , paDelta :: Expression               -- delta
                    , paMotiv :: [(Expression,[Rule] )]

              | New { paCpt :: A_Concept                  -- make a new instance of type c
                    , paCl :: String->PAclause            -- to be done after creating the concept
                    , paMotiv :: [(Expression,[Rule] )]
                    }
              | Rmv { paCpt :: A_Concept                  -- Remove an instance of type c
                    , paCl :: String->PAclause            -- to be done afteremoving the concept
                    , paMotiv :: [(Expression,[Rule] )]
                    }
-}

-- | eventsFrom is written for constructing switchboard diagrams.
eventsFrom :: PAclause -> [Event]   -- gather all Do's from a PAclause
eventsFrom p@CHC{}          = concatMap eventsFrom (paCls p)
eventsFrom p@GCH{}          = concatMap eventsFrom [ paClause | (_,_,paClause)<-paGCls p]
eventsFrom p@ALL{}          = concatMap eventsFrom (paCls p)
eventsFrom (Do tOp dcl _ _) = [On tOp dcl]
eventsFrom p@New{}          = On Ins (Isn (paCpt p)): eventsFrom (paCl p (makePSingleton "x"))
eventsFrom p@Rmv{}          = On Del (Isn (paCpt p)): eventsFrom (paCl p (makePSingleton "x"))
eventsFrom Nop{}            = []
eventsFrom Blk{}            = []
eventsFrom Let{}            = fatal 56 "eventsFrom not defined for `Let` constructor of PAclause"
eventsFrom Ref{}            = fatal 57 "eventsFrom not defined for `Ref` constructor of PAclause"

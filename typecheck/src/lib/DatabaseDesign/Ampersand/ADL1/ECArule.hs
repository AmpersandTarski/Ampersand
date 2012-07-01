{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.ADL1.ECArule ( isAll
                                             , isChc
                                             , isBlk
                                             , isNop
                                             , isDo
                                             , dos
 
                                             )
where
--import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree       
--import DatabaseDesign.Ampersand.Basics 
import DatabaseDesign.Ampersand.Fspec.Fspec
import DatabaseDesign.Ampersand.Basics     (fatalMsg)

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
fatal :: Int -> String -> a
fatal = fatalMsg "ADL1.ECArule.hs"

  

isAll :: PAclause -> Bool
isAll All{} = True
isAll _     = False
  
isChc :: PAclause -> Bool
isChc Chc{} = True
isChc _     = False
  
isBlk :: PAclause -> Bool
isBlk Blk{} = True
isBlk _     = False
  
isNop :: PAclause -> Bool
isNop Nop{} = True
isNop _     = False
  
isDo :: PAclause -> Bool
isDo Do{}   = True
isDo _      = False

dos :: PAclause -> [PAclause]   -- gather all Do's from a PAclause
dos (p@Chc{}) = concatMap dos (paCls p)
dos (p@All{}) = concatMap dos (paCls p)
dos (p@Do{})  = [p]
dos (p@Sel{}) = dos (paCl p "x")
dos (p@New{}) = dos (paCl p "x")
dos (p@Rmv{}) = dos (paCl p "x")
dos (Nop{})   = []
dos (Blk{})   = []
dos (Let{})   = fatal 56 "dos not defined for `Let` constructor of PAclause"
dos (Ref{})   = fatal 57 "dos not defined for `Ref` constructor of PAclause"
 


  
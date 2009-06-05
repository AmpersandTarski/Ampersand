{-# OPTIONS_GHC -Wall #-}
module TypeInference.Statements where
import Adl.Concept
import TypeInference.AdlExpr
import Adl.MorphismAndDeclaration

import CommonClasses --for ghci only

data Statement = IsaStat  Concept Concept | --DESCR -> stating that the left concept IS-a right concepte
                 InfErr InfErrType   | --DESCR -> stating that the type of an expression cannot be inferred, because the inference attempt ended because of this error
                 EmptyStmt | --DESCR -> stating that there is no statement
                 BoundTo AdlExpr |
                 TypeOf AdlExpr 
                 deriving (Show)
                 
data InfErrType = UndeclRel AdlExpr | IErr Concept Concept AdlExpr  deriving (Show)
instance Eq InfErrType where
   (UndeclRel expr)==(UndeclRel expr') = expr==expr'
   (IErr c1 c2 expr)==(IErr c1' c2' expr') = expr==expr' && c1==c1' && c2==c2'
   _ == _ = False

--DESCR -> constructs an IsaStat statement from an isa definition (spc,gen)
fromIsa :: (Concept,Concept) -> Statement
fromIsa (c1,c2) = IsaStat c1 c2

iserrstmt :: Statement -> Bool
iserrstmt (InfErr {}) = True
iserrstmt _ = False

instance Eq Statement where
  (IsaStat c1 c2)==(IsaStat c1' c2') = c1==c1' && c2==c2'
  (InfErr tp )==(InfErr tp' ) = tp==tp'
  EmptyStmt==EmptyStmt = True
  (TypeOf expr)==(TypeOf expr') = expr==expr'
  (BoundTo expr)==(BoundTo expr') = expr==expr'
  _==_ = False
------------------------------------------


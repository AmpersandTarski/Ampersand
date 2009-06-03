{-# OPTIONS_GHC -Wall #-}
module TypeInference.Statements where
import Adl.Concept
import TypeInference.AdlExpr
import Adl.MorphismAndDeclaration

import CommonClasses --for ghci only

data Statement = DisjStat  Concept Concept | --DESCR -> stating that two concepts are disjunct
                 IsaStat  Concept Concept | --DESCR -> stating that the left concept IS-a right concept
                 TypeStat  AdlExpr Sign | --DESCR -> stating AdlExpr::Sign
                 BndStat  AdlExpr Sign | --DESCR -> stating AdlExpr[Sign]
                 LAndStat  Statement Statement | --DESCR -> stating that both statements are true
                 InfErr InfErrType String  | --DESCR -> stating that the type of an expression cannot be inferred, because the inference attempt ended because of this error
                 EmptyStmt | --DESCR -> stating that there is no statement
                 BoundTo AdlExpr |
                 TypeOf AdlExpr 
                 deriving (Show)
                 
data InfErrType = UndeclRel AdlExpr | IErr deriving (Show)

--DESCR -> constructs an IsaStat statement from an isa definition (spc,gen)
fromIsa :: (Concept,Concept) -> Statement
fromIsa (c1,c2) = IsaStat c1 c2

instance Eq Statement where
  (DisjStat c1 c2)==(DisjStat c1' c2') = (c1==c1' && c2==c2') || (c1==c2' && c2==c1')
  (IsaStat c1 c2)==(IsaStat c1' c2') = c1==c1' && c2==c2'
  (TypeStat expr sgn)==(TypeStat expr' sgn') = expr==expr' && sgn==sgn'
  --(BndStat expr sgn)==(BndStat expr' sgn') = expr==expr' && sgn==sgn'
  (LAndStat s1 s2)==(LAndStat s1' s2') = (s1==s1' && s2==s2') || (s1==s2' && s2==s1')
  (InfErr _ str)==(InfErr _ str') = str==str'
  EmptyStmt==EmptyStmt = True
  (TypeOf expr)==(TypeOf expr') = expr==expr'
  (BoundTo expr)==(BoundTo expr') = expr==expr'
  _==_ = False
------------------------------------------


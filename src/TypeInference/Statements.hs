{-# OPTIONS_GHC -Wall #-}
module TypeInference.Statements where
import Adl.Concept
import TypeInference.AdlExpr

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


------------------------
type Gamma = [Statement]
{-
evalTT :: Gamma -> TypeTerm -> Sign
evalTT gm (TT ct1 ct2) = (evalCT gm ct1,evalCT gm ct2)

evalCT :: Gamma -> ConceptTerm -> Concept
evalCT _ (CT c) = c
evalCT _ (CF (f,c1,c2)) 
  | f==Generic = c2
  | f==Specific = c1 
evalCT gm (CTake x) = takec gm x
inverseCT :: ConceptTerm -> ConceptTerm
inverseCT ct@(CT{}) = ct
inverseCT (CF (f,c1,c2)) 
  | f==Generic = CF (Specific,c1,c2)
  | f==Specific = CF (Generic,c1,c2)
inverseCT (CTake (f,cs))
  | f==Generic = CTake (Specific,cs)
  | f==Specific = CTake (Generic,cs)

takec :: Gamma -> (GenSpec,Concepts) -> Concept
takec gamma (Generic,cs) = case cs of
   [] -> Anything
   c:[] -> c
   c1:c2:cn -> if elem (fromIsa (c1,c2)) gamma
               then takec gamma (Generic,c2:cn)
               else
                 if elem (fromIsa (c2,c1)) gamma
                 then takec gamma (Generic,c1:cn)
                 else error $ "Error in Statements.hs module Typeinference.Statements function takec: " ++
                              "Concepts in list have no isa relation. C1: "++show c1++", C2: "++show c2++"\nGamma: "++show gamma++"."
takec gamma (Specific,cs) = case cs of
   [] -> NOthing
   c:[] -> c
   c1:c2:cn -> if elem (fromIsa (c1,c2)) gamma
               then takec gamma (Specific,c1:cn)
               else
                 if elem (fromIsa (c2,c1)) gamma
                 then takec gamma (Specific,c2:cn)
                 else error $ "Error in Statements.hs module Typeinference.Statements function takec: " ++
                              "Concepts in list have no isa relation. C1: "++show c1++", C2: "++show c2++"\nGamma: "++show gamma++"."
-----------------------------------------
-}


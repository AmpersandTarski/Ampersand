module Ampersand.Core.ShowAStruct
  (AStruct(..))
where

import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Core.ShowPStruct
import Ampersand.Core.A2P_Converters

class AStruct a where
 showA :: a -> String

instance AStruct A_Context where
 showA = showP . aCtx2pCtx

instance AStruct Expression where  
 showA = showP . aExpression2pTermPrim

instance AStruct A_Concept where
 showA = showP . aConcept2pConcept

instance AStruct A_Gen where
 showA = showP . aGen2pGen 

instance AStruct Rule where
 showA = showP . aRule2pRule

instance AStruct Declaration where
 showA = showP . aDeclaration2pDeclaration

instance AStruct AAtomPair where
 showA p = "("++showA (apLeft p)++","++ showA (apRight p)++")"

instance AStruct AAtomValue where
 showA at = case at of
              AAVString{} -> show (aavstr at)
              AAVInteger _ i   -> show i
              AAVFloat   _ f   -> show f
              AAVBoolean _ b   -> show b
              AAVDate _ day    -> show day
              AAVDateTime _ dt -> show dt
              AtomValueOfONE -> "1"

instance AStruct ExplObj where
 showA = showP . aExplObj2PRef2Obj


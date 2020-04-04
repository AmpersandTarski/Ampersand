{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Core.ShowAStruct
  (AStruct(..))
where

import Ampersand.Basics
import Ampersand.Core.A2P_Converters
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Core.ShowPStruct

class AStruct a where
 showA :: a -> Text

instance AStruct A_Context where
 showA = showP . aCtx2pCtx

instance AStruct Expression where  
 showA = showP . aExpression2pTermPrim

instance AStruct A_Concept where
 showA = showP . aConcept2pConcept

instance AStruct AClassify where
 showA = showP . aClassify2pClassify 

instance AStruct Rule where
 showA = showP . aRule2pRule

instance AStruct Relation where
 showA = showP . aRelation2pRelation

instance AStruct AAtomPair where
 showA p = "("<>showA (apLeft p)<>","<> showA (apRight p)<>")"

instance AStruct AAtomValue where
 showA at = case at of
              AAVString{} -> tshow (aavtxt at)
              AAVInteger _ i   -> tshow i
              AAVFloat   _ f   -> tshow f
              AAVBoolean _ b   -> tshow b
              AAVDate _ day    -> tshow day
              AAVDateTime _ dt -> tshow dt
              AtomValueOfONE -> "1"

instance AStruct ExplObj where
 showA = showP . aExplObj2PRef2Obj


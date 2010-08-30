module Prototype.CodeStatement (UseVar(..)) where
 import Prototype.CodeAuxiliaries (Named(..))
 
 data UseVar = UseVar {uvList::[Either String (Named UseVar)]}
 instance Show UseVar
 instance Eq UseVar
 
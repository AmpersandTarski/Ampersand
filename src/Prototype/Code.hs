{-# LANGUAGE Rank2Types, KindSignatures, GeneralizedNewtypeDeriving #-}

module Prototype.Code (getCodeFor,showCode,codeVariableForBinary,Statement(..)) where
 import Prototype.GetCode  (getCodeFor) -- writing the code
 import Prototype.ShowCode (showCode)   -- php specific stuff
 import Prototype.CodeVariables (codeVariableForBinary) -- manipulating variables
 import Prototype.CodeAuxiliaries (Statement(..)) 


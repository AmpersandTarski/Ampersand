{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Rank2Types, KindSignatures, GeneralizedNewtypeDeriving #-}

module Prototype.Code (getCodeFor,showCode,codeVariableForBinary,Statement(..),showCodeHeaders) where
 import Prototype.CodeStatement (Statement(..)) 
 import Prototype.CodeVariables (codeVariableForBinary) -- manipulating variables
 import Prototype.GetCode  (getCodeFor) -- writing the code
 import Prototype.ShowCode (showCode,showCodeHeaders)   -- php specific stuff


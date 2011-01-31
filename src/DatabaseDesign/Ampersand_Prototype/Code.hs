{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Rank2Types, KindSignatures, GeneralizedNewtypeDeriving #-}

module DatabaseDesign.Ampersand_Prototype.Code (getCodeFor,showCode,codeVariableForBinary,Statement(..),showCodeHeaders) where
 import DatabaseDesign.Ampersand_Prototype.CodeStatement (Statement(..)) 
 import DatabaseDesign.Ampersand_Prototype.CodeVariables (codeVariableForBinary) -- manipulating variables
 import DatabaseDesign.Ampersand_Prototype.GetCode  (getCodeFor) -- writing the code
 import DatabaseDesign.Ampersand_Prototype.ShowCode (showCode,showCodeHeaders)   -- php specific stuff


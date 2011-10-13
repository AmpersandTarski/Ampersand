{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module DatabaseDesign.Ampersand.ADL1.Concept ( Conceptual(..)
                   
                   
                   , newAcpt,cptos'
                   ) 
where
  import DatabaseDesign.Ampersand.Basics (fatalMsg)
  import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
  fatal :: Int -> String -> a
  fatal = fatalMsg "ADL1.Concept"


  class Eq c => Conceptual c where
   isSingleton :: c -> Bool

  instance Conceptual A_Concept where
   isSingleton ONE = True
   isSingleton _   = False

  cptos' :: A_Concept -> [String]
  cptos' C{cptos=x} = x
  cptos' ONE = fatal 126 "Asking for the value of the universal singleton"
  

  newAcpt :: String -> A_Concept
  newAcpt nm = C{ cptnm=nm, cptgE = (==), cptos = []}
  


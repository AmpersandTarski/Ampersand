{-# LANGUAGE Rank2Types, KindSignatures, GeneralizedNewtypeDeriving #-}

module Prototype.CodeAuxiliaries (Statement(..),CodeQuery(..),CodeVariable(..),atleastOne) where
 import Adl (Concept(..),Expression(..))
 
 {-
 data Function
  =  Function { fnname:: String
              , input :: [CodeVariable]
              , output:: CodeVariable
              , fncode:: [Statement]
              }
 -}
 -- | An abstract statement: this is the intermediate structure for going from an expression to an imperative program.
 data Statement
  =  Iteration   { preknowledge :: [CodeVariable]
                 , postknowledge:: [CodeVariable]
                 , loopOver     :: CodeVariable -- ^ this variable is in preknowledge, and a variable with the same name (but different content) is in postknowledge
                 , loopBy       :: CodeVariable -- ^ this is a fresh variable with which iterate: foreach(loopOver as loopBy=>loopValue) do stcode.
                 , loopValue    :: CodeVariable -- ^ this is subvariable of loopOver
                 , stcode       :: [Statement]  -- ^ this is the code we perform once every iteration
                 }
   | Assignment  { preknowledge :: [CodeVariable]
                 , postknowledge:: [CodeVariable]
                 , assignTo     :: [CodeVariable] -- ^ head assignTo indicates the postknowledge variable to assign this to, the second part of the list indicates which of its elements, which of its attributes, and so on
                 , query        :: CodeQuery
                 }
   | Forget      -- ^ in some languages, we need to clean up variables. In most languages, code for Forget will be []
                 { preknowledge :: [CodeVariable] -- ^ variables we used to know (before this statement)
                 , postknowledge:: [CodeVariable] -- ^ variables we still know (not cleaned up)
                 }
 data CodeQuery
  =  SQLBinary   {cqexpression::Expression, sqlquery::String } -- ^ get a binary relation from SQL (this can only be one expression). (Used to fill a scalar, usually)
   | SQLComposed {cqsource:: Concept, cqexpressions::[Expression], sqlquery::String } -- ^ get a couple of relations from SQL. They all share the same source, and there is one record per source item
   | PHPPlug -- ^ TODO: what does this look like?
   | PHPIntersect{cqfrom1::CodeVariable,cqfrom2::CodeVariable}
   | PHPJoin     {cqfrom1::CodeVariable,cqfrom2::CodeVariable}
   | PHPIsectComp{cqfrom1::CodeVariable,cqfrom2::CodeVariable} -- ^ cqfrom1 /\ -cqfrom2
   | PHPDagger   {cqfrom1::CodeVariable,cqfrom2::CodeVariable,cqAll::CodeVariable}
   | PHPUnion    {cqfrom1::CodeVariable,cqfrom2::CodeVariable}
   | CQCompose   {cqFrom::[(String,CodeVariable)]} -- ^ as SQLComposed, but this time using preknowledge variables. String indicates the outgoing name or method.
   deriving (Show)
 -- | A data type containing the description of some variable in the target language.
 -- | see for example the singletonCV, or codeVariableForBinary
 -- | note that cvname cannot be an empty string, unless it is the only variable nested in the content attribute of a CodeVarObject
 data CodeVariable
  =  CodeVarScalar { cvname     :: String  -- ^ the name of this scalar, as in name=value.
                   , cvtype     :: Concept -- ^ the (ADL) type of this scalar
                   , multiple   :: Bool    -- ^ whether this is a list or not (name=[value])
                   , sorted     :: Bool    -- ^ if this is a list, whether it is sorted
                   , cvexpression :: Expression -- ^ the value contained in this variable. We assume a value for the source of expression is known, so (source expression == S) or this variable is part of a CodeVarObject (in that case, the source here is just the target of the containing object). Should you require 'everything', use (V[S*Type];e) in the expression component
                   }
   | CodeVarObject { cvname     :: String
                   , content    :: [CodeVariable] -- ^ the code variables cv1,cv2... here give rise to an object such as {cv1=v1,cv2=v2..}
                   , cvtype     :: Concept
                   , multiple   :: Bool
                   , cvexpression :: Expression
                   }
 
 instance Show CodeVariable where
   show (CodeVarScalar a b False _ e   ) = "CodeVarScalar "++a++" "++show b++" "++show e++" (single)"
   show (CodeVarScalar a b True False e) = "CodeVarScalar "++a++" "++show b++" "++show e++" (multiple, unsorted)"
   show (CodeVarScalar a b True True e ) = "CodeVarScalar "++a++" "++show b++" "++show e++" (multiple, sorted)"
   show (CodeVarObject a b _ False e   ) = "CodeVarObject "++a++" "++show b++" "++show e++" (single)"
   show (CodeVarObject a b _ True  e   ) = "CodeVarObject "++a++" "++show b++" "++show e++" (multiple)"

 instance Eq CodeVariable where
   CodeVarScalar n1 t1 m1 s1 e1 == CodeVarScalar n2 t2 m2 s2 e2
    = (n1==n2) && (t1==t2) && (e1==e2) && if(m1==False) then (m2==False) else ((m1==m2) && (s1==s2))
   CodeVarObject n1 c1 t1 m1 e1 == CodeVarObject n2 c2 t2 m2 e2
    = (n1==n2) && (c1==c2) && (t1==t2) && (m1==m2) && (e1==e2)
   _ == _ = False
 
  -- | make sure a function returns at least one item (run-time check) or give a debug error
 atleastOne :: forall t. [Char] -> [t] -> [t]
 atleastOne errormsg [] = error errormsg
 atleastOne _ (a:as) = a:as

 {-
 -- | Suppose you have some code that you wish to execute within a certain environment where global variables already exist: make sure that variables within this code get new, unique names
 -- | As a side-effect, this function may rename variables such that any overlap already present will occur not or less often. Please don't depend on this feature, but instead write clean code in the first place.
 addKnowledge :: [CodeVariable] -- ^ knowledge you wish to add
              -> [Statement] -- ^ Code to add this knowledge to
              -> [Statement] -- ^ resulting code
 addKnowledge vars oldcode = map addTo oldcode
  where addTo s = (case s of
                    Iteration{}
                     -> (\x -> x{stcode=addKnowledge (preknowledge x) (stripKnowledge (preknowledge s) (stcode s))})
                    _ -> id
                  )
                  (knupd s)
        -- knowledge update
        knupd s = s{preknowledge=vars++map ku (preknowledge s),postknowledge=vars++map ku (postknowledge s)}
        ku var = var{cvname=noCollide allnames (cvname var)}
        allnames = map cvname vars ++ 
                   rd (map cvname (concat (map preknowledge oldcode++map postknowledge oldcode)))
 -- | Removes a variable from the knowledge of a piece of code. Function is used by addKnowledge and might be usefull for other purposes as well.
 -- | Does the opposite of addKnowledge, with the exception that no renaming of variables can occur
 stripKnowledge :: [CodeVariable] -- ^ knowledge to be removed
                -> [Statement] -- ^ Code to remove this knowledge from
                -> [Statement] -- ^ resulting code
 stripKnowledge vars oldcode = map stripFrom oldcode
   where stripFrom s = (case s of
                         Iteration{}
                          -> (\x -> x{stcode=map stripFrom (stcode s)})
                         _ -> id
                       )
                       s{preknowledge=strp (preknowledge s),postknowledge=strp (postknowledge s)}
         strp vs = [var|var<-vs,notElem var vars]
  -}
module Prototype.CodeStatement (Statement(..),CodeQuery(..),UseVar(..)) where
 import Adl (Concept(..),Expression(..))
 import Prototype.CodeVariables (CodeVar(..))
 import Prototype.CodeAuxiliaries (Named(..))

 -- | An abstract statement: this is the intermediate structure for going from an expression to an imperative program.
 data Statement
  =  Iteration   { preknowledge :: [Named CodeVar]
                 , postknowledge:: [Named CodeVar]
                 , loopOver     :: Named UseVar -- ^ this variable is in preknowledge, and a variable with the same name (but different content) is in postknowledge
                 , loopBy       :: Named CodeVar -- ^ this is a fresh variable with which iterate: foreach(loopOver as loopBy=>loopValue) do stcode.
                 , loopValue    :: Named CodeVar -- ^ this is subvariable of loopOver, possible renamed
                 , stcode       :: [Statement]  -- ^ this is the code we perform once every iteration
                 }
   | Assignment  { preknowledge :: [Named CodeVar]
                 , postknowledge:: [Named CodeVar]
                 , assignTo     :: Named UseVar -- ^ head assignTo indicates the postknowledge variable to assign this to, the second part of the list indicates which of its elements, which of its attributes, and so on
                 , query        :: CodeQuery
                 }
   -- | in some languages, we need to clean up variables. In most languages, code for Forget will be []
   | Forget      { preknowledge :: [Named CodeVar] -- ^ variables we used to know (before this statement)
                 , postknowledge:: [Named CodeVar] -- ^ variables we still know (not cleaned up)
                 }

 -- | The actual use of a variable. In practice, use Named UseVar.
 -- | Example: the PHP usage $people[$i]["Name"] becomes Named "people" [Right Named "i" [],Left "Name"]
 data UseVar = UseVar {uvList::[Either String (Named UseVar)]} deriving (Eq)
 instance Show UseVar where
   show (UseVar []) = ""
   show (UseVar (Left  s:xs)) = "["++show s++"]"++(show xs)
   show (UseVar (Right s:xs)) = "["++show s++"]"++(show xs)
 data CodeQuery
  =  SQLBinary   {cqexpression::Expression, sqlquery::String } -- ^ get a binary relation from SQL (this can only be one expression). (Used to fill a scalar, usually) Will fill target only
   | SQLComposed {cqsource:: Concept, cqExpressions::[Named Expression], sqlquery::String } -- ^ get a couple of relations from SQL. They all share the same source, and there is one record per source item
   | PHPPlug     {cqinput  ::[CodeQuery]  -- ^ list of arguments passed to the plug (must be verified!)
                 ,cqoutput ::CodeVar      -- ^ the output variable
                 ,cqphpplug::String       -- ^ the name of the plug
                 ,cqphpfile::Maybe String -- ^ the file name on where to find this plug
                 }
   | PHPBinCheck {cqinput  ::[CodeQuery]  -- ^ list of arguments passed to the plug
                 ,cqreturn ::(CodeQuery,CodeQuery) -- ^ what to return if the plug did not return False
                 ,cqphpplug::String       -- ^ the name of the plug (must be verified!)
                 ,cqphpfile::Maybe String -- ^ the file name on where to find this plug
                 }
   | PHPIntersect{cqfrom1::CodeQuery,cqfrom2::CodeQuery}
   | PHPJoin     {cqfrom1::CodeQuery,cqfrom2::CodeQuery}
   | PHPIsectComp{cqfrom1::CodeQuery,cqfrom2::CodeQuery} -- ^ cqfrom1 /\ -cqfrom2
   | PHPDagger   {cqfrom1::CodeQuery,cqfrom2::CodeQuery,cqAll::CodeQuery}
   | PHPUnion    {cqfrom1::CodeQuery,cqfrom2::CodeQuery}
   | PHPCompl1   {cqtuple::(Named UseVar,Named UseVar),cqfrom::CodeQuery}
   | CQCompose   {cqFrom::[Named CodeQuery]}-- ^ as SQLComposed: combine different codeQueries by name
   | CQPlain     (Named UseVar)             -- ^ simply get some variable and return it
   | CQConstant  {cqQuotedValue::String}    -- ^ a constant such as "Hello world", true, or date()
   deriving (Show,Eq)
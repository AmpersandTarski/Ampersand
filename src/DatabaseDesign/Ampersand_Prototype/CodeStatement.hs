{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand_Prototype.CodeStatement
      (Statement(..)
      ,CodeQuery(..)
      ,UseVar(..)
      ,CodeVar(..)
      ,CodeVarIndexed(..)
      ,useAttribute
      ,PHPconcept(..),php2conc,conc2php,phpsource,phptarget,phpflp,phpsign
      ) where
 import DatabaseDesign.Ampersand_Prototype.CodeAuxiliaries (Named(..), mapRelation, mapExpression)
 import DatabaseDesign.Ampersand_Prototype.CoreImporter

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
 data CodeVar = CodeVar
  { cvIndexed :: CodeVarIndexed
    -- | Content can either be a CodeVar, intended for indexed stuff: $var[$i] returns a codeVar,
    --                    OR  [Named CodeVar], intended for objects/associative arrays: $var["nName"] is a codeVar 
  , cvContent :: Either CodeVar [Named CodeVar] 
  , cvExpression :: Expression (Relation PHPconcept)
  }
 
 instance Eq CodeVar where
  cv==cv' = cvIndexed cv == cvIndexed cv' && cvContent cv == cvContent cv'
  
 data CodeVarIndexed = Indexed | NotIndexed | IndexByName deriving (Eq,Show)
 
 instance Show CodeVar where
   show (CodeVar i c e) = show i++" "++show c -- ++" '"++show e++"'"
 
 -- | The actual use of a variable. In practice, use Named UseVar.
 -- | Example: the PHP usage $people[$i]["Name"] becomes Named "people" [Right (Named "i" []),Left "Name"]
 data UseVar = UseVar {uvList::[Either String (Named UseVar)]} deriving (Eq)
 instance Show UseVar where
   show (UseVar []) = ""
   show (UseVar (Left  s:xs)) = "["++show s++"]"++(show xs)
   show (UseVar (Right s:xs)) = "["++show s++"]"++(show xs)
   
 useAttribute :: Either String (Named UseVar) -- ^ use this attribute
              -> Named UseVar -- ^ of this variable
              -> Named UseVar
 useAttribute s var = var{nObject=UseVar {uvList=varlist++[s]}}
   where  varlist=uvList(nObject var)

 -- | The following wrapper adds PHP-information to a concept.
 data PHPconcept
    = PHPC Concept -- ^Usual concept
    | PHPexp (Expression (Relation Concept)) -- ^A concept containing pairs representing the population in the expression.
    | PHPI1  { cpvar :: Named UseVar }
    deriving (Eq,Show)

 instance SpecHierarchy PHPconcept
 instance Ord PHPconcept where
   x <= y = x == y

 instance Identified PHPconcept where
  name (PHPC c)     = name c
  name (PHPexp _)   = "SomeExpression"
  name (PHPI1 x)    = nName x
 
 phpsource :: Expression (Relation PHPconcept) -> PHPconcept
 phpsource expr = case expr of
     (ERel rel)-> source rel
     (EBrk f)    -> phpsource f
     (ECps [])    -> error "!Fatal (module CodeStatement 74): ECps []"
     (ECps fs)    -> phpsource (head fs)
     (ERad [])  -> error "!Fatal (module CodeStatement 76): ERad []"
     (ERad fs)  -> phpsource (head fs)
     (EUni [])  -> error "!Fatal (module CodeStatement 78): EUni []"
     (EUni fs)  -> phpsource (head fs)
     (EIsc [])  -> error "!Fatal (module CodeStatement 80): EIsc []"
     (EIsc fs)  -> phpsource (head fs)
     (EKl0 e')  -> phpsource e'
     (EKl1 e')  -> phpsource e'
     (ECpl e')  -> phpsource e'
 phptarget  :: Expression (Relation PHPconcept) -> PHPconcept
 phptarget x = phpsource(phpflp x)
 phpsign :: Expression (Relation PHPconcept) -> (PHPconcept,PHPconcept)
 phpsign x = (phpsource x, phptarget x)
 phpflp  :: Expression (Relation PHPconcept) -> Expression (Relation PHPconcept)
 phpflp expr = case expr of
     (ERel rel)-> ERel (flp rel)
     (EBrk f)    -> EBrk (phpflp f)
     (ECps ts)    -> ECps (map phpflp (reverse ts))
     (ERad ts)  -> ERad (map phpflp (reverse ts))
     (EUni fs)  -> EUni (map phpflp fs)
     (EIsc fs)  -> EIsc (map phpflp fs)
     (EKl0 e')  -> EKl0 (phpflp e')
     (EKl1 e')  -> EKl1 (phpflp e')
     (ECpl e')  -> ECpl (phpflp e')

 conc2php :: Expression (Relation Concept) -> Expression (Relation PHPconcept)
 conc2php e = mapExpression (mapRelation PHPC) e

 php2conc :: Expression (Relation PHPconcept) -> Expression (Relation Concept)
 php2conc e = mapExpression (mapRelation f) e
              where f (PHPC c) = c
                    f _ = error("!Fatal (module CodeStatement 101): Non-exhaustive pattern for PHPconcept in php2conc")

 data CodeQuery
  =  SQLBinary   { cqexpression ::Expression (Relation PHPconcept)
                 , sqlquery     ::String } -- ^ get a binary relation from SQL (this can only be one expression). (Used to fill a scalar, usually) Will fill target only
   | SQLComposed { cqsource     :: Concept
                 , cqExpressions::[Named (Expression (Relation PHPconcept))]
                 , sqlquery ::String } -- ^ get a couple of relations from SQL. They all share the same source, and there is one record per source item
   | PHPPlug     { cqinput  ::[CodeQuery]  -- ^ list of arguments passed to the plug (must be verified!)
                 , cqoutput ::CodeVar      -- ^ the output variable
                 , cqphpplug::String       -- ^ the name of the plug
                 , cqphpfile::Maybe String -- ^ the file name on where  to find this plug
                 }
   | PHPBinCheck { cqinput  ::[CodeQuery]  -- ^ list of arguments passed to the plug
                 , cqreturn ::(CodeQuery,CodeQuery) -- ^ what to return if the plug did not return False
                 , cqphpplug::String       -- ^ the name of the plug (must be verified!)
                 , cqphpfile::Maybe String -- ^ the file name on where  to find this plug
                 }
   | PHPIntersect{ cqfrom1::CodeQuery
                 , cqfrom2::CodeQuery}
   | PHPJoin     { cqfrom1::CodeQuery
                 , cqfrom2::CodeQuery}
   | PHPIsectComp{ cqfrom1::CodeQuery
                 , cqfrom2::CodeQuery} -- ^ cqfrom1 /\ -cqfrom2
   | PHPDagger   { cqfrom1::CodeQuery
                 , cqfrom2::CodeQuery,cqAll::CodeQuery}
   | PHPUnion    { cqfrom1::CodeQuery
                 , cqfrom2::CodeQuery}
   | PHPAdd1     { cqfrom1::CodeQuery
                 , cqfrom2::CodeQuery}
   | PHPCompl1   { cqtuple::(Named UseVar,Named UseVar)
                 , cqfrom ::CodeQuery}
   | CQCompose   { cqFrom ::[Named CodeQuery]} -- ^ as SQLComposed: combine different codeQueries by name
   | CQPlain     (Named UseVar)                -- ^ simply get some variable and return it
   | CQConstant  {cqQuotedValue::String}       -- ^ a constant such as "Hello world", true, or date()
   deriving (Eq, Show)

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
 import DatabaseDesign.Ampersand_Prototype.CodeAuxiliaries (Named(..))
 import DatabaseDesign.Ampersand -- .ADL1 (Concept(..),SpecHierarchy(..),Expression(..),Relation(..),Identified(..))

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
  } deriving (Eq)
 data CodeVarIndexed = Indexed | NotIndexed | IndexByName deriving (Eq,Show)
 
 instance Show CodeVar where
   show (CodeVar i c e) = show i++" "++show c++" '"++show e++"'"
 
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
   where varlist=uvList(nObject var)

-- | The following wrapper adds PHP-information to a concept.
 data PHPconcept
    = PHPC Concept -- ^Usual concept
    | PHPexp (Expression (Relation Concept)) -- ^A concept containing pairs representing the population in the expression.
    | PHPI1  { cpvar :: Named UseVar }
    deriving (Eq,Show)

 instance Identified PHPconcept where
  name (PHPC c)     = name c
  name (PHPexp _)   = "SomeExpression"
  name (PHPI1 x)    = nName x
 
 phpsource :: Expression (Relation PHPconcept) -> PHPconcept
 phpsource expr = case expr of
     (Tm rel i)-> source rel
     (Tc f)    -> phpsource f
     (F [])    -> error "!Fatal (module CodeStatement 74): F []"
     (F fs)    -> phpsource (head fs)
     (Fdx [])  -> error "!Fatal (module CodeStatement 76): Fdx []"
     (Fdx fs)  -> phpsource (head fs)
     (Fux [])  -> error "!Fatal (module CodeStatement 78): Fux []"
     (Fux fs)  -> phpsource (head fs)
     (Fix [])  -> error "!Fatal (module CodeStatement 80): Fix []"
     (Fix fs)  -> phpsource (head fs)
     (K0x e')  -> phpsource e'
     (K1x e')  -> phpsource e'
     (Cpx e')  -> phpsource e'
 phptarget  :: Expression (Relation PHPconcept) -> PHPconcept
 phptarget x = phpsource(phpflp x)
 phpsign :: Expression (Relation PHPconcept) -> (PHPconcept,PHPconcept)
 phpsign x = (phpsource x, phptarget x)
 phpflp  :: Expression (Relation PHPconcept) -> Expression (Relation PHPconcept)
 phpflp expr = case expr of
     (Tm rel i)-> Tm (flp rel) i
     (Tc f)    -> Tc (phpflp f)
     (F ts)    -> F (map phpflp (reverse ts))
     (Fdx ts)  -> Fdx (map phpflp (reverse ts))
     (Fux fs)  -> Fux (map phpflp fs)
     (Fix fs)  -> Fix (map phpflp fs)
     (K0x e')  -> K0x (phpflp e')
     (K1x e')  -> K1x (phpflp e')
     (Cpx e')  -> Cpx (phpflp e')

 conc2php :: Expression (Relation Concept) -> Expression (Relation PHPconcept)
 conc2php e = mapExpression (mapMorphism PHPC) e

 php2conc :: Expression (Relation PHPconcept) -> Expression (Relation Concept)
 php2conc e = mapExpression (mapMorphism f) e
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
                 , cqphpfile::Maybe String -- ^ the file name on where to find this plug
                 }
   | PHPBinCheck { cqinput  ::[CodeQuery]  -- ^ list of arguments passed to the plug
                 , cqreturn ::(CodeQuery,CodeQuery) -- ^ what to return if the plug did not return False
                 , cqphpplug::String       -- ^ the name of the plug (must be verified!)
                 , cqphpfile::Maybe String -- ^ the file name on where to find this plug
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
   | CQCompose   { cqFrom ::[Named CodeQuery]}-- ^ as SQLComposed: combine different codeQueries by name
   | CQPlain     (Named UseVar)             -- ^ simply get some variable and return it
   | CQConstant  {cqQuotedValue::String}    -- ^ a constant such as "Hello world", true, or date()
   deriving (Show,Eq)

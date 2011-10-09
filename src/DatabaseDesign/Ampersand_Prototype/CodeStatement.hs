{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand_Prototype.CodeStatement
      (Statement(..)
      ,CodeQuery(..)
      ,UseVar(..)
      ,CodeVar(..)
      ,CodeVarIndexed(..)
      ,useAttribute
      ,PHPconcept(..)
      ,php2conc,conc2php
      ,phpsource
      ,phptarget,phpflp,phpsign
      ,PHPDeclaration(..),PHPRelation(..),PHPExpression(..)
      ) where
 import DatabaseDesign.Ampersand_Prototype.CodeAuxiliaries (Named(..), mapRelation, mapExpression)
 import DatabaseDesign.Ampersand_Prototype.CoreImporter

{- Expressions are evaluated preferrably in SQL, since the database should do the work wherever possible.
In some cases, however, a mixture of PHP and SQL is needed to evaluate an Expression.
Therefore, the Expression data structure also exists in a PHP-variant.
(Critical question, why is this necessary? It has the same structure, so WHY?)
For every Expression there is a corresponding PHPExpression,
for instance ECps has a corresponding PHPECps, and so on for every constructor in Expression.
The functions php2conc and conc2php define the correspondence betwee Expression PHPExpression.
-}

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
  , cvExpression :: PHPExpression
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
    = PHPC A_Concept -- ^Usual concept
    | PHPexp Expression -- ^A concept containing pairs representing the population in the expression.
    | PHPI1  { cpvar :: Named UseVar }
    deriving (Eq,Show)

 instance SpecHierarchy PHPconcept
 instance Ord PHPconcept where
   x <= y = x == y

 instance Identified PHPconcept where
  name (PHPC c)     = name c
  name (PHPexp _)   = "SomeExpression"
  name (PHPI1 x)    = nName x
 
 phpsource :: PHPExpression -> PHPconcept
 phpsource expr = case expr of
     (PHPEEqu (l,r)) -> phpsource l
     (PHPEImp (l,r)) -> phpsource l
     (PHPEIsc [])    -> error "!Fatal (module CodeStatement 80): EIsc []"
     (PHPEIsc es)    -> phpsource (head es)
     (PHPEUni [])    -> error "!Fatal (module CodeStatement 78): EUni []"
     (PHPEUni es)    -> phpsource (head es)
     (PHPEDif (l,r)) -> phpsource l
     (PHPELrs (l,r)) -> phpsource l
     (PHPERrs (l,r)) -> phptarget r
     (PHPECps [])    -> error "!Fatal (module CodeStatement 74): ECps []"
     (PHPECps es)    -> phpsource (head es)
     (PHPERad [])    -> error "!Fatal (module CodeStatement 76): ERad []"
     (PHPERad es)    -> phpsource (head es)
     (PHPEKl0 e)     -> phpsource e
     (PHPEKl1 e)     -> phpsource e
     (PHPEFlp e)     -> phptarget e
     (PHPECpl e)     -> phpsource e
     (PHPEBrk e)     -> phpsource e
     (PHPETyp e t)   -> phpsource e
     (PHPERel rel)   -> phprelsource rel
 phptarget  :: PHPExpression -> PHPconcept
 phptarget x = phpsource(phpflp x)
 phpsign :: PHPExpression -> (PHPconcept,PHPconcept)
 phpsign x = (phpsource x, phptarget x)
 phpflp  :: PHPExpression -> PHPExpression
 phpflp expr = case expr of
     (PHPEEqu (l,r))   -> PHPEEqu (phpflp l, phpflp r)
     (PHPEImp (l,r))   -> PHPEImp (phpflp l, phpflp r)
     (PHPEIsc es)      -> PHPEIsc (map phpflp (reverse es))
     (PHPEUni es)      -> PHPEUni (map phpflp (reverse es))
     (PHPEDif (l,r))   -> PHPEDif (phpflp l, phpflp r)
     (PHPELrs (l,r))   -> PHPELrs (phpflp r, phpflp l)
     (PHPERrs (l,r))   -> PHPERrs (phpflp r, phpflp l)
     (PHPECps es)      -> PHPECps (map phpflp (reverse es))
     (PHPERad es)      -> PHPERad (map phpflp (reverse es))
     (PHPEKl0 e)       -> PHPEKl0 (phpflp e)
     (PHPEKl1 e)       -> PHPEKl1 (phpflp e)
     (PHPEFlp e)       -> PHPEFlp (phpflp e)
     (PHPECpl e)       -> PHPECpl (phpflp e)
     (PHPEBrk e)       -> PHPEBrk (phpflp e)
     (PHPETyp e (s,t)) -> PHPETyp (phpflp e) (t,s)
     (PHPERel rel)     -> PHPERel (phprelflp rel)


 data CodeQuery
  =  SQLBinary   { cqexpression ::PHPExpression
                 , sqlquery     ::String } -- ^ get a binary relation from SQL (this can only be one expression). (Used to fill a scalar, usually) Will fill target only
   | SQLComposed { cqsource     :: A_Concept
                 , cqExpressions::[Named (PHPExpression)]
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




-------------------------------------------------
 data PHPExpression 
      = PHPEEqu (PHPExpression,PHPExpression)   
      | PHPEImp (PHPExpression,PHPExpression)   
      | PHPEIsc [PHPExpression]                 
      | PHPEUni [PHPExpression]                 
      | PHPEDif (PHPExpression,PHPExpression)   
      | PHPELrs (PHPExpression,PHPExpression)   
      | PHPERrs (PHPExpression,PHPExpression)   
      | PHPECps [PHPExpression]                 
      | PHPERad [PHPExpression]                 
      | PHPEKl0 PHPExpression                  
      | PHPEKl1 PHPExpression                  
      | PHPEFlp PHPExpression                   
      | PHPECpl PHPExpression                   
      | PHPEBrk PHPExpression                   
      | PHPETyp PHPExpression (PHPconcept,PHPconcept)
      | PHPERel PHPRelation                      
      deriving (Eq,Show)
 data PHPRelation = 
  PHPRel { phpnm   :: String 
         , phppos  :: Origin       
         , phpsgn  :: (PHPconcept,PHPconcept)            
         , phpdcl  :: Declaration     
         } |
  PHPI    { php1typ :: PHPconcept       
          } |
  PHPV    { phptyp  :: (PHPconcept,PHPconcept)            
          } |
  PHPMp1  { phpval  :: String       
          , php1typ :: PHPconcept       
          } deriving (Show)

 instance Eq PHPRelation where
  PHPRel nm _ sgn _ == PHPRel nm' _ sgn' _ = nm==nm' && sgn==sgn'
  PHPI c            == PHPI c'             = c==c'
  PHPV sgn          == PHPV sgn'           = sgn==sgn'
  PHPMp1 s c        == PHPMp1 s' c'        = s==s' && c==c'
  _ == _ = False

 instance Identified PHPRelation where
  name (PHPRel nm _ _ _) = nm
  name (PHPI{}) = "I"
  name (PHPV{}) = "V"
  name (PHPMp1{}) = "I"

 phprelsource (PHPRel _ _ (c,_) _) = c
 phprelsource (PHPI c) = c
 phprelsource (PHPV (c,_)) = c
 phprelsource (PHPMp1 s c) = c

 php2conc :: PHPExpression -> Expression
 php2conc (PHPEEqu (l,r)) = EEqu (php2conc l,php2conc r) 
 php2conc (PHPEImp (l,r)) = EImp (php2conc l,php2conc r) 
 php2conc (PHPEIsc es)    = EIsc (map (php2conc) es)             
 php2conc (PHPEUni es)    = EUni (map (php2conc) es)             
 php2conc (PHPEDif (l,r)) = EDif (php2conc l,php2conc r) 
 php2conc (PHPELrs (l,r)) = ELrs (php2conc l,php2conc r) 
 php2conc (PHPERrs (l,r)) = ERrs (php2conc l,php2conc r) 
 php2conc (PHPECps es)    = ECps (map (php2conc) es)             
 php2conc (PHPERad es)    = ERad (map (php2conc) es)             
 php2conc (PHPEKl0 e)     = EKl0 (php2conc e)                    
 php2conc (PHPEKl1 e)     = EKl1 (php2conc e)                    
 php2conc (PHPEFlp e)     = EFlp (php2conc e)                    
 php2conc (PHPECpl e)     = ECpl (php2conc e)                    
 php2conc (PHPEBrk e)     =     php2conc e                       
 php2conc (PHPETyp e t)   =     php2conc e
 php2conc (PHPERel rel)
  = let f (PHPC c) = c
        f _ = error("!Fatal (module CodeStatement 101): Non-exhaustive pattern for PHPconcept in php2conc")
    in case rel of
           PHPRel nm pos (s,t) d -> ERel (Rel nm pos (Sign (f s) (f t)) d)
           PHPI  c -> ERel (I (f c))
           PHPV  (s,t) -> ERel (V (Sign (f s) (f t)))
           PHPMp1 val c -> ERel (Mp1 val (f c)) 

 conc2php (EEqu (l,r)) = PHPEEqu (conc2php l,conc2php r) 
 conc2php (EImp (l,r)) = PHPEImp (conc2php l,conc2php r) 
 conc2php (EIsc es)    = PHPEIsc (map (conc2php) es)             
 conc2php (EUni es)    = PHPEUni (map (conc2php) es)             
 conc2php (EDif (l,r)) = PHPEDif (conc2php l,conc2php r) 
 conc2php (ELrs (l,r)) = PHPELrs (conc2php l,conc2php r) 
 conc2php (ERrs (l,r)) = PHPERrs (conc2php l,conc2php r) 
 conc2php (ECps es)    = PHPECps (map (conc2php) es)             
 conc2php (ERad es)    = PHPERad (map (conc2php) es)             
 conc2php (EKl0 e)     = PHPEKl0 (conc2php e)                    
 conc2php (EKl1 e)     = PHPEKl1 (conc2php e)                    
 conc2php (EFlp e)     = PHPEFlp (conc2php e)                    
 conc2php (ECpl e)     = PHPECpl (conc2php e)                    
 conc2php (EBrk e)     =     conc2php e                       
 conc2php (ETyp e _)   =     conc2php e                    
 conc2php (ERel rel)
  = case rel of
           Rel nm pos (Sign s t) d -> PHPERel (PHPRel nm pos (PHPC s,PHPC t) d)
           I  c -> PHPERel (PHPI (PHPC c))
           V  (Sign s t) -> PHPERel (PHPV (PHPC s,PHPC t))
           Mp1 val c -> PHPERel (PHPMp1 val (PHPC c))  



 data PHPDeclaration = 
  PHPSgn { phpdecnm   :: String  
      , phpdecsgn  :: (PHPconcept,PHPconcept)       
      --, decprps :: [Prop]     
      , phpdecprps_calc :: [Prop]
      --, decprL  :: String     
      --, decprM  :: String     
      --, decprR  :: String
      --, decMean :: String     
      --, decpopu :: Pairs      
      --, decfpos :: Origin     
      , phpdeciss  :: Bool       
      , phpdecusr  :: Bool       
      --, decpat  :: String     
      , phpdecplug :: Bool       
      } 

 phprelflp :: PHPRelation -> PHPRelation
 phprelflp (PHPRel nm pos (s,t) d) = PHPRel nm pos (t,s) d

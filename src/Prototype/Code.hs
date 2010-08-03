{-# LANGUAGE Rank2Types, KindSignatures, GeneralizedNewtypeDeriving #-}

module Prototype.Code where
 import Char(isDigit,digitToInt,intToDigit,isAlphaNum,toLower)
 import Strings (chain)
 import Adl
 import Prototype.RelBinGenBasics(selectExpr,sqlExprTrg,sqlExprSrc,phpShow,pDebug,noCollide)
 import Data.Fspec
 import ShowHS
 
 data Function
  =  Function { fnname:: String
              , input :: [CodeVariable]
              , output:: CodeVariable
              , fncode:: [Statement]
              }
 data Statement
  =  Iteration   { preknowledge :: [CodeVariable]
                 , postknowledge:: [CodeVariable]
                 , loopOver     :: CodeVariable
                 , stcode       :: [Statement]
                 }
   | Assignmment { preknowledge :: [CodeVariable]
                 , postknowledge:: [CodeVariable]
                 , assignTo     :: CodeVariable
                 , query        :: CodeQuery
                 }
 data CodeQuery
  =  SQLBinary { cqsource:: Concept, cqtarget:: Concept, cqexpression::Expression }
   | SQLComposed{cqsource:: Concept, cqexpressions::[Expression] }
   | PHPIntersect{cqfrom1::CodeVariable,cqfrom2::CodeVariable}
   | PHPJoin     {cqfrom1::CodeVariable,cqfrom2::CodeVariable}
   | PHPIsectComp{cqfrom1::CodeVariable,cqfrom2::CodeVariable}
   | PHPDagger   {cqfrom1::CodeVariable,cqfrom2::CodeVariable,cqAll::CodeVariable}
   | PHPUnion    {cqfrom1::CodeVariable,cqfrom2::CodeVariable}
 data CodeVariable
  =  CodeVarPlain  { cvname     :: String
                   , cvtype     :: Concept
                   , multiple   :: Bool
                   , sorted     :: Bool
                   , cvexpression :: Expression
                   }
   | CodeVarObject { cvname     :: String
                   , content    :: [CodeVariable]
                   , cvtype     :: Concept
                   , multiple   :: Bool
                   , cvexpression :: Expression
                   }
   deriving Eq
  
 getCodeFor :: [CodeVariable]->[CodeVariable]->(Maybe [Statement])
 getCodeFor pre post
  = Just [undefined]
 
 showCode :: Int -- indentation
          -> [Statement] -- code
          -> String
 showCode _ [] = ""
 showCode i (x:xs) = ""++"\n"++showCode i xs
 
 codeVariableForBinary :: Fspc -> String  -- name of the variable to be created
                          -> Expression   -- value of the variable is list of tuples in this Expression
                          -> CodeVariable
 codeVariableForBinary fSpec str expr
  = CodeVarObject { cvname=str
                  , content=[CodeVarPlain { cvname  ="0"
                                          , cvtype  =source expr
                                          , multiple=False
                                          , sorted  =undefined
                                          , cvexpression=Tm (srcRel) undefined
                                          }
                            ,CodeVarPlain { cvname  ="1"
                                          , cvtype  =target expr
                                          , multiple=False
                                          , sorted  =undefined
                                          , cvexpression=Tm (trgRel) undefined
                                          }
                            ]
                  , cvtype=newConcForExpr
                  , multiple=True
                  , cvexpression=Tm (mIs newConcForExpr) undefined
                  }
    where 
          newConcForExpr = source srcRel -- source srcRel == source trgRel
          srcRel = pairSource expr
          trgRel = pairTarget expr
 
 pairSource :: Expression -> Morphism
 pairSource e = makeMph (pairSourceDecl e)
 pairTarget :: Expression -> Morphism
 pairTarget e = makeMph (pairTargetDecl e)
 pairTargetDecl :: Expression -> Declaration
 pairTargetDecl e = (pairSourceDecl e){decnm="trg",detrg=target e}
 pairSourceDecl expr = Sgn { decnm="src", desrc=DExp expr, detrg=source expr
                           , decprps=[], decprps_calc=[Uni,Tot]
                           , decprL="", decprM="", decprR=""
                           , decpopu=undefined -- TODO? If so, get population from expr, and be sure to adjust pairTargetDecl as well.
                           , decexplain=""
                           , decfpos=undefined, decid=undefined -- cannot get a position or a unique number
                           , deciss=False -- not a signal-relation
                           , decusr=False -- defined by ADL (i.e. not user-defined)
                           , decpat=undefined
                           }
 
 --src<-[], trg<-[noCollide [src] (sqlExprTrg fSpec rule')]
  {- -- te genereren: 
     $accepted=DB_doquer_lookups('SELECT DISTINCT `Order1`.`I`, `Order1`.`accepted` FROM `Order1` ORDER BY `Order1`.`accepted`');
     $of=DB_doquer_lookups('SELECT DISTINCT `Delivery`.`of`, `Delivery`.`I` FROM `Order1` ORDER BY `Delivery`.`I`');
     // expr1 = accepted;of~
     $expr1=array();
     foreach($accepted as $i=>$acceptedvals){
       foreach($acceptedvals as $acceptedval){
         addLookups($expr1[$i],@$of[$acceptedval]);
       }
     }
     $provided=DB_doquer_lookups('SELECT DISTINCT `Delivery`.`I`, `Delivery`.`provided` FROM `Delivery` ORDER BY `Delivery`.`provided`');
     // expr2 = expr1/\-provided
     $expr2=array();
     foreach($expr1 as $i=>$expr1vals){
       if(isset($providedvals=@$provided[$i])){
           $providedvals=$provided[$expr1vals];
           $j=0;
           for($k=0;$k<count($expr1vals) and $j<count($providedvals);$k++){
             while($expr1vals[$k]>@$providedvals[$j]){
               $j++;
             }
             if($expr1vals[$k]==@$providedvals[$j]){}else addLookup($expr2[$i],$expr1vals[$k]);
           }
       } else $expr2[$i]=$expr1vals;
     }
     $v=$expr2;
     -}
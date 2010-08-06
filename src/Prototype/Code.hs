{-# LANGUAGE Rank2Types, KindSignatures, GeneralizedNewtypeDeriving #-}

module Prototype.Code where
 --import Char(isDigit,digitToInt,intToDigit,isAlphaNum,toLower)
 --import Strings (chain)
 import Adl 
 import Prototype.RelBinGenBasics(selectExpr,sqlExprTrg,sqlExprSrc,pDebug,noCollide,sqlRelPlugs,indentBlockBetween)
 import Data.Fspec
 import ShowHS

 import Data.Maybe
 import NormalForms (simplify)

 
 data Function
  =  Function { fnname:: String
              , input :: [CodeVariable]
              , output:: CodeVariable
              , fncode:: [Statement]
              }
 -- | An abstract statement: this is the intermediate structure for going from an expression to an imperative program.
 data Statement
  =  Iteration   { preknowledge :: [CodeVariable]
                 , postknowledge:: [CodeVariable]
                 , loopOver     :: CodeVariable -- ^ this variable is in preknowledge, and a variable with the same name (but different content) is in postknowledge
                 , stcode       :: [Statement]  -- ^ this is the code we perform once every iteration
                 }
   | Assignment  { preknowledge :: [CodeVariable]
                 , postknowledge:: [CodeVariable]
                 , assignTo     :: CodeVariable
                 , query        :: CodeQuery
                 }
   | Forget      -- ^ in some languages, we need to clean up variables. In most languages, code for Forget will be []
                 { preknowledge :: [CodeVariable] -- ^ variables we used to know (before this statement)
                 , postknowledge:: [CodeVariable] -- ^ variables we still know (not cleaned up)
                 }
 data CodeQuery
  =  SQLBinary   {cqexpression::Expression, sqlquery::String } -- ^ get a binary relation from SQL (this can only be one expression). (Used to fill a scalar, usually)
   | SQLComposed {cqsource:: Concept, cqexpressions::[Expression], sqlquery::String } -- ^ get a couple of relations from SQL. They all share the same source, and there is one record per source item
   | PHPPlug -- ^ todo, what does this look like?
   | PHPIntersect{cqfrom1::CodeVariable,cqfrom2::CodeVariable}
   | PHPJoin     {cqfrom1::CodeVariable,cqfrom2::CodeVariable}
   | PHPIsectComp{cqfrom1::CodeVariable,cqfrom2::CodeVariable} -- ^ cqfrom1 /\ -cqfrom2
   | PHPDagger   {cqfrom1::CodeVariable,cqfrom2::CodeVariable,cqAll::CodeVariable}
   | PHPUnion    {cqfrom1::CodeVariable,cqfrom2::CodeVariable}
 -- | A data type containing the description of some variable in the target language.
 -- | see for example the singletonCV, or codeVariableForBinary
 data CodeVariable
  =  CodeVarScalar { cvname     :: String  -- ^ the name of this scalar, as in name=value
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
 singletonCV :: CodeVariable
 singletonCV = CodeVarScalar ""
                            S
                            False
                            (error "Single variable: cannot test whether it is sorted (in Code.hs)")
                            (Tm (mIs S) (error "Term number undefined in Code.hs"))
 instance Eq CodeVariable where
   CodeVarScalar n1 t1 m1 s1 e1 == CodeVarScalar n2 t2 m2 s2 e2
    = (n1==n2) && (t1==t2) && (e1==e2) && if(m1==False) then (m2==False) else ((m1==m2) && (s1==s2))
   CodeVarObject n1 c1 t1 m1 e1 == CodeVarObject n2 c2 t2 m2 e2
    = (n1==n2) && (c1==c2) && (t1==t2) && (m1==m2) && (e1==e2)
   _ == _ = False
   
 getCodeFor :: Fspc->[CodeVariable]->[CodeVariable]->(Maybe [Statement])
 getCodeFor fSpec pre post
    = if null new then Just [] else
       case next of
        (Just a,Just as) -> Just (a ++ as)
        _ -> Nothing
  where
   new  = [p|p<-post,notElem p pre]
   next = (getCodeForSingle fSpec pre (head new),getCodeFor fSpec ((head new):pre) post)
 
 
 getCodeForSingle :: Fspc->[CodeVariable]->CodeVariable->(Maybe [Statement])
 getCodeForSingle _ pre post | elem post pre = Just [] -- allready known info
 getCodeForSingle fSpec pre o
  = listToMaybe
     -- to get code, we can try different strategies. Just concattenate all attempts
     -- listToMaybe will take the first, so put the things you want most first
     ([ -- here we try to find a partial overlap in pre:
        -- we already know pre, and it might be just what we're looking for
        -- there is no need to calculate it twice
      ]++ -- if that does not work, find Expression and iterate over it
      -- let's first try to find a singleton, those values we know already
      [ code | code<-getTarget e
      ]++
      -- try using some Mp1 value
      [ 
        [Iteration pre (pre++[o]) p code]
      | p<-pre,F ((Tm Mp1{mph1val=mval} _):_)<-[e]
      , mval==cvname p,code<-iterateOver p -- loop over something else
      ]
     )
  where e = cvexpression o
        getTarget (Tm (V{mphtyp=(s,t)}) _)
         = [case s of
             S -> allT
             _ -> error "Cannot calculate a cross-product (inexhaustive patterns) in Code.hs"
           | allT <- case t of DExp e' -> getAllInExpr fSpec pre o e'
                               _ -> error "Cannot get allT in Code.hs (inexhaustive patterns)"
           ]
        getTarget _ | True = error "Cannot getTarget in Code.hs (patterns are still inexhaustive as of august 6th 2010. Deleting this line in Code.hs might fix things when getCodeForSingle is somewhat more complete)"
        getTarget _ = []
        iterateOver _ = [] -- todo
 -- | Create code to fill a single variable with some expression
 getAllInExpr :: Fspc -- ^ contains information on what's in a DB and what's in a different kind of plug
                 ->[CodeVariable] -- ^ preknowledge (for administrative purposes)
                 ->CodeVariable -- ^ variable to assign Expression to
                 ->Expression   -- ^ expression we'd like to know
                 ->[[Statement]]-- ^ list of possible chunks of code that get Expression into CodeVariable, sorted from most efficient to least efficient (fastest way to get Expression)
 getAllInExpr fSpec pre var (Tc   e ) = getAllInExpr fSpec pre var e
 getAllInExpr fSpec pre var (F   [e]) = getAllInExpr fSpec pre var e
 getAllInExpr fSpec pre var (Fix [e]) = getAllInExpr fSpec pre var e
 getAllInExpr fSpec pre var (Fux [e]) = getAllInExpr fSpec pre var e
 getAllInExpr fSpec pre var (Fdx [e]) = getAllInExpr fSpec pre var e
 getAllInExpr fSpec pre var composed
  = -- we try to get the whole thing via SQL
    [[Assignment pre (var:pre) var (SQLBinary composed (sqlQuery fSpec composed))]
    | isExprInDB fSpec composed -- make sure we can
    ] ++
    -- divide: we try to get both sides of some operator, and then use a binary PHP composition
    [get1++get2++join++forget
    | (e1,e2,opr) <- case composed of (Fix (a:Cpx b:x)) -> [(F (a:x),b,PHPIsectComp)]
                                      (Fix (Cpx b:a:x)) -> [(F (a:x),b,PHPIsectComp)]
                                      (F   (f:fs))      -> [(f,(F   fs),PHPJoin)]
                                      (Fix (f:fs))      -> [(f,(Fix fs),PHPIntersect)]
                                      (Fux (f:fs))      -> [(f,(Fix fs),PHPUnion)]
                                      _ -> []
    , let var1=newVarFor (map cvname pre) e1
    , let var2=newVarFor (map cvname (var1:pre)) e2
    -- code below is correct, and should work when getCodeForSingle is OK
    --, get1<-maybeToList (getCodeForSingle fSpec pre var1)
    --, get2<-maybeToList (getCodeForSingle fSpec (var1:pre) var2)
    , get1<-getAllInExpr fSpec pre var1 e1
    , get2<-getAllInExpr fSpec (var1:pre) var2 e2
    , let join=[Assignment (var1:var2:pre) (var1:var2:var:pre) var (opr var1 var2)]
    , let forget=[Forget (var1:var2:var:pre) (var:pre)]
    ] ++
    error ("getAllInExpr returned no result in Code.hs (this is OK for certain expressions, and this error message is for testing purposes only, the failed expression was: "++(show composed)++")")

 -- | will get a straight-forward sql expression (binary) with a nice name for source and target
 sqlQuery :: Fspc -> Expression -> String
 sqlQuery fSpec expr
  = selectExpr fSpec 0 src (noCollide [src] (sqlExprTrg fSpec expr)) expr
  where src = sqlExprSrc fSpec expr
 -- | Will tell whether a certain expression is obtainable via the underlying database
 isExprInDB :: Fspc -- ^ info on the onderlying db
            -> Expression -- ^ expression needed
            -> Bool -- ^ True means that the expression can be calculated using only the database
 isExprInDB fSpec e = [] /= sqlRelPlugs fSpec e -- Todo: use the entire code-generator for SQL instead of just a part of it
 
 -- | Create a new variable with the value of some expression, ensure that its name does not exist yet
 newVarFor :: [String] -- ^ list of forbidden names
           -> Expression -- ^ value of the variable
           -> CodeVariable -- ^ the resulting variable
 newVarFor forbidden expr
  = CodeVarObject  (newVarNameFor forbidden expr) 
                   [CodeVarScalar "" (target expr) (isUni expr) False expr]
                   (source expr)
                   True
                   (Tm (V [] (S,source expr)) (error "did not assign number to Tm in Code.hs"))
                  
 -- | Create a new name with the value of some expression, ensure that its name does not exist yet
 newVarNameFor :: [String] -- ^ list of forbidden names
               -> Expression -- ^ value of the variable
               -> String -- ^ the resulting name
 newVarNameFor forbidden (Tm a _) = noCollide forbidden (name a)
 newVarNameFor forbidden (F _) = noCollide forbidden "join"
 newVarNameFor forbidden (Fix _) = noCollide forbidden "isct"
 newVarNameFor forbidden (Fux _) = noCollide forbidden "unio"
 newVarNameFor forbidden (Fdx _) = noCollide forbidden "dggr"
 newVarNameFor forbidden (K0x _) = noCollide forbidden "reflfixpt"
 newVarNameFor forbidden (K1x _) = noCollide forbidden "fixpt"
 newVarNameFor forbidden (Cpx _) = noCollide forbidden "cmplt"
 newVarNameFor forbidden _ = noCollide forbidden "expr"
 
 showCode :: Int -- indentation
          -> [Statement] -- code
          -> String
 showCode _ [] = ""
 showCode i (x:xs)
  = (case x of
      Assignment{assignTo=to,query=q}
       -> showAssignmentCode i to q
      Iteration{loopOver=lp,stcode=c}
       -> error "No way to generate iteration yet in Code.hs"
      Forget{} -> []
    )++"\n"++showCode i xs
   where
 -- | give the PHP code for an assignemt of some query to some variable
 showAssignmentCode :: Int -- ^ indentation
                    -> CodeVariable -- ^ the variable that will have a value after this
                    -> CodeQuery -- ^ the query
                    -> String -- ^ PHP code
 showAssignmentCode i var quer
  = case quer of
     SQLBinary{sqlquery=str}
      -> indentBlockBetween (sps ++ "$"++varname++" = DB_doquer_lookups('") "');" (lines str)
     PHPIntersect{}
      -> line "intersect"
     PHPJoin{}
      -> line "join"
     PHPIsectComp{}
      -> line "isectComp"
     _ -> error "cannot showCode in Assignment of query (inexhaustive patterns in Code.hs)"
    where
     varname = cvname var
     sps=take i (repeat ' ')
     line str = sps ++ "$"++varname++"="++str++"($"++cvname (cqfrom1 quer)++",$"++cvname (cqfrom2 quer)++");"
 -- | Creates a codeVariable that contains the pairs indicated by some expression.
 -- | If it is possible to calculate the expression, getCodeFor should be able to get a CodeVariable constructed via codeVariableForBinary
 codeVariableForBinary :: String  -- ^ name of the variable to be created
                          -> Expression   -- ^ value of the variable is list of tuples in this Expression
                          -> CodeVariable
 codeVariableForBinary str expr
  = CodeVarObject { cvname=str
                  , content=[CodeVarScalar { cvname  ="0"
                                          , cvtype  =source expr
                                          , multiple=False
                                          , sorted  =error "Sorted undefined (not multiple) in Code.hs"
                                          , cvexpression=Tm (srcRel) (error "Term number undefined in Code.hs")
                                          }
                            ,CodeVarScalar { cvname  ="1"
                                          , cvtype  =target expr
                                          , multiple=False
                                          , sorted  =error "Sorted undefined (not multiple) in Code.hs"
                                          , cvexpression=Tm (trgRel) (error "Term number undefined in Code.hs")
                                          }
                            ]
                  , cvtype=newConcForExpr
                  , multiple=True
                  , cvexpression=Tm (V [] (S,newConcForExpr)) 1
                  }
    where 
          newConcForExpr = source srcRel -- source srcRel == source trgRel
          srcRel = pairSource (simplify (noDaggers expr))
          trgRel = pairTarget (simplify (noDaggers expr))
 
 -- | removes all daggers from the given expression
 noDaggers :: Expression -> Expression
 noDaggers (F (fs)) = F (map noDaggers fs)
 noDaggers (Fix (fs)) = Fix (map noDaggers fs)
 noDaggers (Fux (fs)) = Fix (map noDaggers fs)
 noDaggers (Fdx (fs)) = Cpx (F (map noDaggers (map Cpx fs)))
 noDaggers (Cpx (Cpx a)) = a
 noDaggers (Cpx (Fdx fs)) = F (map noDaggers (map Cpx fs))
 noDaggers (K0x e) = K0x (noDaggers e)
 noDaggers (K1x e) = K1x (noDaggers e)
 noDaggers c = c
 
 pairSource :: Expression -> Morphism
 pairSource e = makeMph (pairSourceDecl e)
 pairTarget :: Expression -> Morphism
 pairTarget e = makeMph (pairTargetDecl e)
 pairTargetDecl :: Expression -> Declaration
 pairTargetDecl e = (pairSourceDecl e){decnm="trg",detrg=target e}
 pairSourceDecl :: Expression -> Declaration
 pairSourceDecl expr = Sgn { decnm="src", desrc=DExp expr, detrg=source expr
                           , decprps=[], decprps_calc=[Uni,Tot]
                           , decprL="", decprM="", decprR=""
                           , decpopu=error "Decl has no population (Code.hs)" -- TODO? If so, get population from expr, and be sure to adjust pairTargetDecl as well.
                           , decexplain=""
                           , decfpos=error "Decl is not on any position since it is ADL-defined (Code.hs)"
                           , decid=error "Decl has no number since it is ADL-defined (Code.hs)"
                           , deciss=False -- not a signal-relation
                           , decusr=False -- defined by ADL (i.e. not user-defined)
                           , decpat=error "Decl is not in any pattern since it is ADL-defined (Code.hs)"
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
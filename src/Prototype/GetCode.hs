module Prototype.GetCode (getCodeFor) where
 import Prototype.CodeAuxiliaries (Statement(..),CodeVariable(..),atleastOne,CodeQuery(..))
 import Adl (Concept(..),Expression(..),Morphism(..),mIs,source,target)
 import Prototype.RelBinGenBasics(selectExpr,sqlExprTrg,sqlExprSrc,noCollide,sqlRelPlugs)
 import Data.Fspec (Fspc)
 import Prototype.CodeVariables (newVarFor,newSingleton,freshSingleton,pairSourceExpr,pairTargetExpr)-- manipulating variables

 getCodeFor :: Fspc->[CodeVariable]->[CodeVariable]->(Maybe [Statement])
 getCodeFor fSpec pre post
    = if null new then Just [] else
       case next of
        (a:_,Just as) -> Just (a ++ as)
        _ -> Nothing
  where
   new  = [p|p<-post,notElem p pre]
   next = (getCodeForSingle fSpec pre (head new),getCodeFor fSpec ((head new):pre) post)
 
  
 getCodeForSingle :: Fspc->[CodeVariable]->CodeVariable->[[Statement]]
 getCodeForSingle _ pre post | elem post pre = [[]] -- allready known info
 getCodeForSingle fSpec pre o
 -- TODO: make sure that newVarFor variables are read OK
  =  atleastOne ("getCodeForSingle did not return anything for an object of expression "++show e)
     -- to get code, we can try different strategies. Just concattenate all attempts
     -- Put the things you want most first (efficient stuff first)
     ([ -- here we try to find a partial overlap in pre:
        -- we already know pre, and it might be just what we're looking for
        -- there is no need to calculate it twice
      ]++ -- if that does not work, find Expression and iterate over it
      -- let's first try to find a singleton, those values we know already
      [ code |
        code<-case e of
         (Tm (V{mphtyp=(S,t)}) _)
          -> getAllTarget t      
         _ -> error "Cannot get code in Code.hs (patterns are still inexhaustive as of august 6th 2010. Putting [] here might fix things when getCodeForSingle is somewhat more complete)"
      ]++
      -- try using some Mp1 value
      [ 
        [--Iteration pre (pre++[o]) p byvar valvar code
        ]
      | p<-pre,F ((Tm Mp1{mph1val=mval} _):_)<-[e]
      , mval==cvname p,False
      -- TODO: generate code (then use SERVICE to find a nice example to work and test this on)
      ]
     )
  where e = cvexpression o
        getAllTarget (DExp e')
         = atleastOne ("getAllTarget did not return something for (DExp e') with e'="++show e')
           [galines ++ renaming
           | tmpvar<-[newVarFor (map cvname (o:pre)) e']
           , galines<-getCodeForSingle fSpec pre tmpvar
           , renaming<-[[Iteration (tmpvar:pre) (o:tmpvar:pre) tmpvar s t
                                   [Assignment (s:t:tmpvar:pre)
                                               (s:t:o:tmpvar:pre)
                                               [o,newSingleton "" (DExp e')]
                                               (CQCompose fromTo)
                                   ]]
                       | let c = case o of
                                   CodeVarObject{content=x} -> x
                                   x@CodeVarScalar{} -> [x]
                       , let s = freshSingleton (tmpvar:pre) "source" (source e')
                       , let t = freshSingleton (s:tmpvar:pre) "target" (target e')
                       , let fromTo = [ (cvname f,t')
                                      | f <- c 
                                      , t' <- (case f of
                                                CodeVarScalar{cvexpression=expr}
                                                  ->    [s|expr==pairSourceExpr e']
                                                     ++ [t|expr==pairTargetExpr e']
                                                _ -> error ("case f does not match: "++show f)
                                              )
                                      ]
                       , (length fromTo == length c) || error ("Length does not match in Code.hs: "++show (fromTo,c))
                       ]
           ]
        getAllTarget tp
         = [[Assignment pre (o:pre) [o] (SQLComposed (source expr) [expr] (selectExpr fSpec 0 "" (sqlExprTrg fSpec expr) expr))]
           | let expr=Tm (mIs(tp)) (-1)
           , CodeVarScalar{} <-[o]
           , isExprInDB fSpec expr -- make sure we can do this
           ]++
           [ l
           | CodeVarObject{content=[c]}<-[o]
           , cvname c == ""
           , l<-getAllInExpr fSpec pre [o,c] (cvexpression c)
           ]++
           [ error ("TODO: create complex objects for getAllTarget in Code.hs "++show c)
           | CodeVarObject{content=c}<-[o]
           ]
 
 -- | Create code to fill a single variable with some expression
 getAllInExpr :: Fspc -- ^ contains information on what's in a DB and what's in a different kind of plug
                 ->[CodeVariable] -- ^ preknowledge (for administrative purposes)
                 ->[CodeVariable] -- ^ variable to assign Expression to (see Assignment for details)
                 ->Expression   -- ^ expression we'd like to know
                 ->[[Statement]]-- ^ list of possible chunks of code that get Expression into CodeVariable, sorted from most efficient to least efficient (fastest way to get Expression)
 getAllInExpr fSpec pre var (Tc   e ) = getAllInExpr fSpec pre var e
 getAllInExpr fSpec pre var (F   [e]) = getAllInExpr fSpec pre var e
 getAllInExpr fSpec pre var (Fix [e]) = getAllInExpr fSpec pre var e
 getAllInExpr fSpec pre var (Fux [e]) = getAllInExpr fSpec pre var e
 getAllInExpr fSpec pre var (Fdx [e]) = getAllInExpr fSpec pre var e
 getAllInExpr fSpec pre var composed
  = atleastOne ("getAllInExpr returned no result in Code.hs (this is OK for certain expressions, and this error message is for testing purposes only, the failed expression was:\n "++(show composed)++"\n it has to be put into:\n "++(show var)++")")
    -- we try to get the whole thing via SQL
    ( [[Assignment pre (head var:pre) var (SQLBinary composed (sqlQuery fSpec composed))]
      | isExprInDB fSpec composed -- make sure we can
      ] ++
      -- divide: we try to get both sides of some operator, and then use a binary PHP composition
      [get1++get2++join++forget
      | (e1,e2,opr) <- case composed of (Fix (a:Cpx b:x)) -> [(F (a:x),b,PHPIsectComp)]
                                        (Fix (Cpx b:a:x)) -> [(F (a:x),b,PHPIsectComp)]
                                        (F   (f:fs))      -> [(f,(F   fs),PHPJoin)]
                                        (Fix (f:fs))      -> [(f,(Fix fs),PHPIntersect)]
                                        (Fux (f:fs))      -> [(f,(Fix fs),PHPUnion)]
                                        _ -> [] -- error ("Failed composed namely "++show composed)
      , let var1=newVarFor (map cvname (head var:pre)) e1
      , let var2=newVarFor (map cvname (head var:var1:pre)) e2
      -- code below is correct, and should work when getCodeForSingle is OK
      , get1<- getCodeForSingle fSpec pre var1
      , get2<- getCodeForSingle fSpec (var1:pre) var2
      --, get1<-getAllInExpr fSpec pre var1 e1
      --, get2<-getAllInExpr fSpec (var1:pre) var2 e2
      , let join=[Assignment (var1:var2:pre) (var1:var2:head var:pre) var (opr var1 var2)]
      , let forget=[Forget (var1:var2:head var:pre) (head var:pre)]
      ]
    )
  -- | will get a straight-forward sql expression (binary) with a nice name for source and target
 sqlQuery :: Fspc -> Expression -> String
 sqlQuery fSpec expr
  = selectExpr fSpec 0 src (noCollide [src] (sqlExprTrg fSpec expr)) expr
  where src = sqlExprSrc fSpec expr
 -- | Will tell whether a certain expression is obtainable via the underlying database
 isExprInDB :: Fspc -- ^ info on the onderlying db
            -> Expression -- ^ expression needed
            -> Bool -- ^ True means that the expression can be calculated using only the database
 isExprInDB fSpec e = [] /= sqlRelPlugs fSpec e -- TODO: use the entire code-generator for SQL instead of just a part of it
 

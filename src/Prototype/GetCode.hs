{-# OPTIONS_GHC -Wall #-}
module Prototype.GetCode (getCodeFor) where
 import Prototype.CodeStatement (Statement(..),CodeQuery(..),UseVar(..),useAttribute)
 import Prototype.CodeVariables (CodeVar(..))
 import Prototype.CodeAuxiliaries (atleastOne,nameFresh,reName,noCollide)
 import Adl (Concept(..), Expression(..),Morphism(..),mIs,isIdent,flp,source,sign,target,Identified(..),singleton)
 import Prototype.RelBinGenSQL(selectExpr,sqlExprTrg,sqlExprSrc)
 import Prototype.RelBinGenBasics(zipnum)
 import Data.Fspec (Fspc(plugs))
 import Data.Plug (IsPlug(..),plname,PlugPHP(..))
 import Prototype.CodeAuxiliaries(Named(..))
 import Prototype.CodeVariables (newVarFor,freshSingleton,pairSourceExpr,pairTargetExpr,singletonCV)-- manipulating variables
 import Data.Maybe (listToMaybe)
 
 getCodeFor :: Fspc->[Named CodeVar]->[Named CodeVar]->(Maybe [Statement])
 getCodeFor fSpec pre post
    = if null new then Just [] else
       case next of
        (a:_,Just as) -> Just (a ++ as)
        _ -> Nothing
  where
   new  = [p|p<-post,notElem p pre]
   next = (getCodeForSingle fSpec pre (head new),getCodeFor fSpec ((head new):pre) post)
 
  
 -- | getCodeForSingle should pay attention to the structure of the object o, and get all attributes contained therein.
 -- | to do so, getCodeForSingle may call getAllInExpr, which only looks at the expression and returns an object of a default type.
 -- | currently, getCodeForSingle is very buggy. In particular, the object structure returned may differ from the desired structure.
 getCodeForSingle :: Fspc->[Named CodeVar]->Named CodeVar->[[Statement]]
 getCodeForSingle _ pre post | elem post pre = [[]] -- allready known info
 getCodeForSingle fSpec pre o
  | singleton (source e) -- als dit niet waar is, kan de variabele niet gevuld worden!!
 -- TODO: make sure that newVarFor variables are read OK. More specifically: o may have attributes, these should be given a value!!
  =  -- to get code, we can try different strategies. Just concattenate all attempts
     -- Put the things you want most first (efficient stuff first)
     -- We might try to find a partial overlap in pre here (there is no need to calculate it twice)
     
     -- We try to find a singleton, those values we know already
     [ code |
       code<-case e of
        (Tm (V{mphtyp=(_,t)}) _) -- source is al automatisch een singleton
         -> getAllTarget t
        _ -> error ("please fix getCodeForSingle, so that it will find objects holding expressions such as "++show e)
     ]
  | otherwise = error "getCodeForSingle requires that source(cvExpression o) is a singleton (in GetCode.hs)"
  where e = cvExpression obj
        obj = (nObject o)
        getAllTarget (DExp e') -- this makes the object very predicatble: it will have a source (0) and a target (1) relation
         = atleastOne ("getAllTarget did not return something for (DExp e') in GetCode.hs with e'="++show e')
           [galines ++ renaming
           | tmpvar<-[newVarFor (map nName (o:pre)) e']
           , galines <- atleastOne ("getCodeForSingle in GetCode.hs should return something for e'="++show e'++"\n"
                                   ) $
                        getCodeForSingle fSpec pre tmpvar
           , renaming<-[[Iteration (tmpvar:pre) (o:tmpvar:pre) (use tmpvar) s t'
                        [Iteration (o:s:t:tmpvar:pre) (o:s:t:tmpvar:pre) (use t') t'' t
                                   [Assignment (s:t:t':tmpvar:pre)
                                               (s:t:t':o:tmpvar:pre)
                                               (Named (nName o) (UseVar [Right (Named "" (UseVar []))]))
                                               (CQCompose (map (\x->Named (fst x) (CQPlain (snd x))) fromTo))
                                   ]]
                        ]
                       | let c = case obj of
                                   CodeVar{cvContent=Right []} -> [o]
                                   CodeVar{cvContent=Right x} -> (x)
                                   _ -> []
                       , let s = freshSingleton (tmpvar:pre) "source" (source e')
                       , let t' = nameFresh (s:tmpvar:pre) "t" singletonCV
                       , let t = freshSingleton (s:t':tmpvar:pre) "target" (target e')
                       , let t'' = nameFresh (s:t:tmpvar:pre) "i" singletonCV
                       , let fromTo = [ (nName f, to)
                                      | f <- c 
                                      , to <-    [use s|cvExpression (nObject f)==pairSourceExpr e']
                                              ++ [use t|cvExpression (nObject f)==pairTargetExpr e']
                                      ]
                       , (length fromTo == length c) || error ("Length does not match in GetCode.hs: "++show (fromTo,c)++", to fix this, start checking and verifying all attributes of obj and get a perfect match. Do NOT fix by just removing this error: this might cause an assignment to not-match")
                       ]
           ]
        getAllTarget tp
         = [[Assignment pre (o:pre) (use o) (SQLComposed (source expr) [Named (name$ source expr) expr] sql)]
           | let expr=Tm (mIs(tp)) (-1) -- get the type of the target of the expression just as I
           , CodeVar{cvContent=Right []} <-[obj]
           , Just sql <- [selectExpr fSpec 0 "" (sqlExprTrg fSpec expr) expr]
           ]++
           [ l
           | CodeVar{cvContent=Left c}<-[obj]
           , l<-getAllInExpr fSpec pre (use o) (cvExpression c)
           ]++
           [ error ("TODO: create complex objects for getAllTarget in Code.hs "++show c)
           | CodeVar{cvContent=Right c}<-[obj]
           ]
 
 -- | Create code to fill a single variable with some expression.
 -- | The resulting variable is of the most basic kind: $var[$everysource]=array($target1,$target2,...);
 -- | (see the definition of newVarFor for the exact type)
 getAllInExpr :: Fspc            -- ^ contains information on what's in a DB and what's in a different kind of plug
              -> [Named CodeVar] -- ^ preknowledge (for administrative purposes)
              -> Named UseVar    -- ^ variable to assign Expression to (see Assignment for details)
              -> Expression      -- ^ expression we'd like to know
              -> [[Statement]]   -- ^ list of possible chunks of code that get Expression into Named CodeVar, sorted from most efficient to least efficient (fastest way to get Expression)
 getAllInExpr fSpec pre var (Tc   e ) = getAllInExpr fSpec pre var e
 getAllInExpr fSpec pre var (F   [e]) = getAllInExpr fSpec pre var e
 getAllInExpr fSpec pre var (Fix [e]) = getAllInExpr fSpec pre var e
 getAllInExpr fSpec pre var (Fux [e]) = getAllInExpr fSpec pre var e
 getAllInExpr fSpec pre var (Fdx [e]) = getAllInExpr fSpec pre var e
 getAllInExpr fSpec pre var composed
  =  -- There are several approaches to get the expression
     -- 1. find the information in the preknowledge
     -- 2. get the whole expression via a single SQL query
     -- 3. find a php plug that returns at least the expression (possibly more) and use it
     -- 4. divide: split the expression into two sides that CAN be calculated
     
     -- Here we try to get the whole thing via a single SQL query
     [[Assignment pre (obj:pre) var (SQLBinary composed sql)]
     | Just sql<-[sqlQuery fSpec composed]
     ] ++
     
     -- Get an expression from a PHP plug
     [[Assignment pre (obj:pre) var p]
     | Just p <- [phpQuery fSpec pre composed]
     ] ++
     
     -- Here we try to divide.
     -- Divide: we try to get both sides of some operator, and then use a binary PHP composition
     -- We may do 'divide' in several ways.
     -- Either get both sides of the operator completely, which is done below
     [get1++get2++join++forget
     | (e1,e2,opr) <- case composed of (Fix (a:Cpx b:x)) -> [(F (a:x),b,PHPIsectComp)]
                                       (Fix (Cpx b:a:x)) -> [(F (a:x),b,PHPIsectComp)]
                                       (F   (f:fs))      -> [(f,(F   fs),PHPJoin)]
                                       (Fix (f:fs))      -> [(f,(Fix fs),PHPIntersect)]
                                       (Fux (f:fs))      -> [(f,(Fix fs),PHPUnion)]
                                       _ -> [] -- error ("Failed composed namely "++show composed)
     , let var1=getAVar pre e1
     , let var2=getAVar (var1:pre) e2
     -- code below is correct, and should work when getCodeForSingle is OK
     , get1<- getCodeForSingle fSpec pre var1
     , get2<- getCodeForSingle fSpec (var1:pre) var2
     -- we may decide to use this shortcut instead:
     --, get1<-getAllInExpr fSpec pre (use var1) e1
     --, get2<-getAllInExpr fSpec (var1:pre) (use var2) e2
     , let join=[Assignment (var1:var2:pre) (var1:var2:obj:pre) var (opr (CQPlain$use var1) (CQPlain$use var2))]
     , let forget=[Forget (var1:var2:obj:pre) (obj:pre)]
     ] ++
     -- Or we might get one side of the operator, and loop over the obtained information
     -- example: a;b;c
     -- here, b is a binary check-only plug
     -- we now try to get a;(b;c) by
     --  1. getting a completely (assuming we can)
     --  2. loop over a, and put the target in $tmpvar
     --  3. get (b;c)[$tmpvar*..], and put it in a;(b;c). We can get (b;c)[$..] because of the next example:
     -- another example: (b;c)[$tmpvar*..]
     --  1. get c completely (assuming we can)
     --  2. loop over c, and put the source in $tmpvar2
     --  3. get b[$tmpvar*$tmpvar2], and put it in (b;c)[$tmpvar*..]. We can get b[$s*$t] via the check-only plug!
     --  (the actual implementation of the latter works by flipping the entire example)
     [ code
     | code <- case composed of
                -- NOTE ON Cpx !!! When Cpx becomes typed, the pattern below should be changed, and changeSource as well!
                (F [Tm (I _ (I1 s) _ _) _,Cpx f,Tm (I _ (I1 t) _ _) _])
                       -> atleastOne ("getCodeForSingle in GetCode.hs should return something in Cpx for "++show f)$ -- SJC put this here
                          [ assignment++
                            [Assignment (var1:pre)
                                        (obj:var1:pre)
                                        (var)
                                        (PHPCompl1 (s,t) (CQPlain (useAttribute (Right s)$ use var1)))
                            ,Forget (var1:obj:pre) (obj:pre)]
                          | var1 <- [getAVar (obj:pre)$ (changeSource (I1 s) $ changeTarget (I1 t) f)
                                    ,getAVar (obj:pre)$ changeTarget (I1 t) f
                                    ,getAVar (obj:pre)$ changeSource (I1 s) f
                                    ]
                          , assignment <- getCodeForSingle fSpec pre var1
                          ] 
                (F fs) -> [assignment++
                           [Iteration (var1:pre) (obj:var1:pre) (use var1) loopby tmp
                                      [Iteration (obj:var1:loopby:tmp:pre) (obj:loopby:tmp:var1:pre)
                                                 (use tmp) tmp2 loopvalue
                                                 stcode'
                                      ]
                           ,Forget (var1:obj:pre) (obj:pre)]
                          | (assign,(f1,f2))
                              <- map (\x->(\pre' var2 s t->[Assignment pre' 
                                                                   (var2:pre')
                                                                   (Named (nName var ) (UseVar [Right s]))
                                                                   (CQPlain t)]
                                           ,x)) (splitAssoc F fs)
                              ++ map (\x->(\pre' var2 s t->[Assignment pre' 
                                                                   (var2:pre')
                                                                   (Named (nName var ) (UseVar [Right t]))
                                                                   (CQPlain s)]
                                           ,x)) (splitAssoc F (map flp (reverse fs)))
                          , let var1 = getAVar (obj:pre) f1
                          , assignment <- getCodeForSingle fSpec pre var1
                          , let loopby = getScalar (obj:var1:pre) "i" (source f1)
                          , let tmp = nameFresh (obj:loopby:var1:pre) "i" singletonCV
                          , let tmp2 = nameFresh (obj:loopby:tmp:var1:pre) "i" singletonCV
                          , let loopvalue = getScalar (obj:loopby:tmp:var1:tmp2:pre) "i" (target f1)
                          , stcode'<- [ get
                                        ++ assign pre' var2 (use loopby)
                                                            (Named (nName var2) (UseVar [Right (use loopvalue)]))
                                        ++ [Forget (var2:pre') pre']
                                      | let pre'=(obj:loopby:tmp:var1:tmp2:loopvalue:pre)
                                      , let var2=getAVar pre' (changeSource (I1 (use loopvalue)) f2)
                                      , get<-getCodeForSingle fSpec pre' var2
                                      ]
                          ]
                (Fix fs)->[assignment++
                           [Iteration (var1:pre) (obj:var1:pre) (use var1) loopby tmp
                                      [Iteration (obj:var1:loopby:tmp:pre) (obj:loopby:tmp:var1:pre)
                                                 (use tmp) tmp2 loopvalue
                                                 stcode'
                                      ]
                           ,Forget (var1:obj:pre) (obj:pre)]
                          | (i,f1) <- zipnum fs
                          , f2<-applyOprOnLists Fix$ take i fs ++ drop (i+1) fs
                          , let var1 = getAVar pre f1
                          , assignment <- getCodeForSingle fSpec pre var1
                          , let loopby = getScalar (obj:var1:pre) "i" (source f1)
                          , let tmp = nameFresh (obj:loopby:var1:pre) "i" singletonCV
                          , let tmp2 = nameFresh (obj:loopby:tmp:var1:pre) "i" singletonCV
                          , let loopvalue = getScalar (obj:loopby:tmp:var1:tmp2:pre) "i" (target f1)
                          , stcode' <- [ get ++ 
                                        [Assignment pre' 
                                                    (var2:pre')
                                                    addTo
                                                    (PHPAdd1 (CQPlain addTo) (CQPlain (Named (nName var2) (UseVar [Right (use loopby),Left "0"]))))
                                        ,Forget (var2:pre') pre']
                                      | let pre'=(obj:loopby:tmp:var1:tmp2:loopvalue:pre)
                                      , let var2=getAVar pre' (changeSource (I1 (use loopby)) (changeTarget (I1 (use loopvalue)) f2))
                                      , let addTo=(Named (nName var ) (UseVar [Right (use loopby)]))
                                      , get<-atleastOne ("getCodeForSingle in GetCode.hs should return something in Fi for "++show var2++" (just removing this error on line 231 might fix the problem)")$
                                             getCodeForSingle fSpec pre' var2
                                      ]
                          ]
                _ -> []
     ]
  where obj =reName (nName var) (newVarFor (map nName pre) composed)
        -- create a new variable, ensuring that no overlap occurs in the namespaces concerning preknowledge
        getAVar pre' = newVarFor (map nName (obj:pre'))
        getScalar pre' nm = freshSingleton (obj:pre') nm
        -- | split a list coming from the use of some associative operator returning two expressions
        splitAssoc opr as
         = [(a,b)|(i,_)<-zipnum as,let (a',b')=splitAt i as
           , a<-applyOprOnLists opr a', b<-applyOprOnLists opr b'
           ]
        -- will turn a list of arbitray length into a list of length at most 1, by applying opr::[a]->a to the list if necessary
        applyOprOnLists opr a@(_:_:_) = [opr a]
        applyOprOnLists _ [x] = [x]
        applyOprOnLists _ [] = []
 -- | use a variable
 use :: Named CodeVar -> Named UseVar
 use s = Named (nName s) (UseVar [])
 
 -- | will get a straight-forward php expression (binary)
 phpQuery :: Fspc -> [Named CodeVar] -> Expression -> Maybe (CodeQuery)
 phpQuery fSpec _ expr
  = listToMaybe
      [  PHPBinCheck {cqinput=map CQPlain [s,t] -- arguments passed to the plug
                     ,cqreturn=(CQPlain s,CQPlain t)
                     ,cqphpplug=plname plug
                     ,cqphpfile=phpfile plug
                     }
      | plug <- pickTypedPlug$ plugs fSpec
      , phpSafe plug
      , (I1 s,I1 t) <- [sign expr]
      , length (phpinArgs plug)==2
      , let pExpr = cvExpression (last (phpinArgs plug))
      , isIdent (cvExpression (head (phpinArgs plug)))
      , (changeSource (I1 s) (changeTarget (I1 t) pExpr) == expr)
      ]

 -- | will get a straight-forward sql expression (binary) with a nice name for source and target
 -- | if, of course, such a sql expression exists
 sqlQuery :: Fspc -> Expression -> Maybe String
 sqlQuery fSpec expr
  = selectExpr fSpec 0 src (noCollide [src] (sqlExprTrg fSpec expr)) expr
  where src = sqlExprSrc fSpec expr
 
 changeTarget :: Concept -> Expression -> Expression
 changeTarget c = flp . changeSource c . flp
 -- | change the source of some expression into c. We assume that c ISA source expression.
 changeSource :: Concept -> Expression -> Expression
 changeSource c (Tc (x)) =changeSource c x
 changeSource c (K0x (x))=changeSource c x
 changeSource c (K1x (x))=changeSource c x
 changeSource c (F (a:as))
   = case changeSource c a of
      F(bs) -> F (bs++as)
      expr -> F (expr:as)
 changeSource c (Fdx (a:as))
   = case changeSource c a of
      Fdx(bs) -> Fdx (bs++as)
      expr -> Fdx (expr:as)
 changeSource c (Fix as) = Fix$ map (changeSource c) as
 changeSource c (Fux as) = Fux$ map (changeSource c) as
 changeSource c (Cpx as) = F [Tm (mIs c)(-1),Cpx as]
 changeSource c (Tm m _)
  = case m of
     Mp1{} -> error "changeSource in getAllInExpr in GetCode.hs should compare whether the source of this Mp1 is equal to c, and either return -V (Nothing) or return the original Mp1. Currently, an error is placed here since I (SJC) don't think this will occur. I would rather see a I of type I1 here."
     I{} -> Tm (I [] c c True) (-1)
     V{mphtyp=(_,t)} -> Tm (V [] (c,t)) (-1)
     Mph{mphtyp=(_,t)} -> Tm m{mphtyp=(c,t)} (-1)
 changeSource _ (F []) = F []
 changeSource _ (Fdx []) = Fdx []

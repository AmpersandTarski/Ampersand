module Database.Design.Ampersand.FSpec.SQL
  (selectSrcTgt,QueryExpr
  ,prettySQLQuery
  , selectExprRelation  --exported to be able to validate the generated SQL for individual relations
  )
  
where


import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.Pretty
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Classes.ConceptStructure
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.ADL1.Expression
import Database.Design.Ampersand.Classes.Relational
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.FSpec.FSpecAux
import Database.Design.Ampersand.FSpec.ShowADL
import Database.Design.Ampersand.Misc

import Data.Char
import Data.List
import Data.Maybe

fatal :: Int -> String -> a
fatal = fatalMsg "FSpec.SQL"

-- | prettyprint ValueExpr and indent it with spaces
prettySQLQuery :: Int -> QueryExpr -> String
prettySQLQuery i =  intercalate ("\n"++replicate i ' ') .  lines . prettyQueryExpr

selectSrcTgt :: 
           FSpec       -- current context
        -> Expression  -- expression to be translated
        -> QueryExpr   -- resulting SQL expression
selectSrcTgt fSpec expr = toSQL (selectExpr fSpec sourceAlias targetAlias expr)


sourceAlias, targetAlias :: Name
sourceAlias = (Name "src") 
targetAlias = (Name "tgt")
selectExpr :: FSpec    -- current context
        -> Name        -- SQL name of the source of this expression, as assigned by the environment
        -> Name        -- SQL name of the target of this expression, as assigned by the environment
        -> Expression  -- expression to be translated
        -> BinQueryExpr   -- resulting info for the binary SQL expression
-- In order to translate all Expressions, code generators have been written for EUni ( \/ ), EIsc ( /\ ), EFlp ( ~ ), ECpl (unary - ), and ECps ( ; ),
-- each of which is supposed to generate correct code in 100% of the cases. (TODO: how do we establish that properly?)
-- The other operators, EEqu ( = ), EImp ( |- ), ERad ( ! ), EPrd ( * ), ELrs ( / ), ERrs ( \ ), and EDia ( <> ), have been implemented in terms of the previous ones,
-- in order to prevent mistakes in the code generator. It is possible that more efficient code may be generated in these cases.
-- Special cases are treated up front, so they will overrule the more general cases.
-- That allows more efficient code while retaining correctness and completeness as much as possible.
-- Code for the Kleene operators EKl0 ( * ) and EKl1 ( + ) is not done, because this cannot be expressed in SQL.
-- These operators must be eliminated from the Expression before using selectExpr, or else you will get fatals.
selectExpr fSpec src trg expr
 = case expr of
    EIsc{} -> 
    {- The story on the case of EIsc:
 This alternative of selectExpr compiles a conjunction of at least two subexpressions (code: EIsc lst'@(_:_:_))
 Each of these subexpressions are of one of the following types:
     1) positive and Mp1
     2) negative and Mp1
     3) not Mp1
    -}
          case posVals of
            ( _ {-a-} : _ {-b-} : _ )
                -> emptySet  --since a /= b, there can be no result. 
            [val]
                -> if val `elem` negVals then emptySet
                   else f (Just val) nonMp1Terms
            []  -> f Nothing nonMp1Terms
           where 
                  posVals :: [String]
                  posVals = nub (map atmValue posMp1Terms)
                  negVals :: [String]
                  negVals = nub (map (atmValue . notCpl) negMp1Terms)
                  atmValue (EMp1 a _) = a
                  atmValue _          = fatal 31 "atm error"
                  mp1Terms, nonMp1Terms :: [Expression]
                  (mp1Terms,nonMp1Terms) = partition isMp1 (exprIsc2list expr)
                  posMp1Terms, negMp1Terms :: [Expression]
                  (posMp1Terms,negMp1Terms) = partition isPos mp1Terms
                  f :: Maybe String   -- Optional the single atomvalue that might be found as the only possible value 
                      -> [Expression] -- subexpressions of the intersection.  Mp1{} nor ECpl(Mp1{}) are allowed elements of this list.  
                      -> BinQueryExpr
                  f specificValue subTerms 
                     = case subTerms of
                          [] -> case specificValue of 
                                 Nothing  -> emptySet -- case might occur with only negMp1Terms??
                                 Just str ->
                                    BSE { bseCmt = "case: (EIsc "++showADL expr++" ("++show (sign expr)++")"
                                        , bseSrc = StringLit str
                                        , bseTrg = StringLit str
                                        , bseTbl = []
                                        , bseWhr = Nothing
                                        }
                          ts  ->   BSE { bseCmt = "case: (EIsc "++showADL expr++" ("++show (sign expr)++")" 
                                        , bseSrc = theSrc
                                        , bseTrg = theTrg
                                        , bseTbl = theTbl
                                        , bseWhr = case catMaybes [mandatoryTuple,forbiddenTuples,theWhr] of
                                                    [] -> Nothing
                                                    vs -> Just (conjunctSQL vs)
                                        }
                                     where
                                       mandatoryTuple :: Maybe ValueExpr
                                       mandatoryTuple =
                                          case specificValue of
                                            Nothing  -> Nothing
                                            Just val -> Just $ equalToValueClause val
                                          where
                                            equalToValueClause :: String -> ValueExpr
                                            equalToValueClause str = conjunctSQL 
                                                               [ BinOp theSrc [Name "="] (StringLit str)
                                                               , BinOp theTrg [Name "="] (StringLit str)
                                                               ]

                                       forbiddenTuples :: Maybe ValueExpr
                                       forbiddenTuples = 
                                           case negVals of
                                            []  -> Nothing
                                            _   -> Just . conjunctSQL $
                                                     map notEqualToValueClause negVals
                                          where
                                            notEqualToValueClause :: String -> ValueExpr
                                            notEqualToValueClause str = conjunctSQL 
                                                               [ BinOp theSrc [Name "<>"] (StringLit str)
                                                               , BinOp theTrg [Name "<>"] (StringLit str)
                                                               ]

                                       theSrc = bseSrc (makeSelectable sResult)
                                       theTrg = bseTrg (makeSelectable sResult)
                                       theTbl = bseTbl (makeSelectable sResult)
                                       theWhr = bseWhr (makeSelectable sResult)
                                       sResult = makeIntersectSelectExpr ts
                                       makeSelectable :: BinQueryExpr -> BinQueryExpr
                                       makeSelectable x =
                                         case x of
                                           BSE{}   -> x
                                           BCQE{}  -> BSE { bseCmt = ""
                                                          , bseSrc = Iden [sourceAlias]
                                                          , bseTrg = Iden [targetAlias]
                                                          , bseTbl = [TRParens . TRQueryExpr . toSQL $ x]
                                                          , bseWhr = Nothing
                                                          }
                                       makeIntersectSelectExpr :: [Expression] -> BinQueryExpr
                                       makeIntersectSelectExpr exprs =
                                        case map (selectExpr fSpec sourceAlias targetAlias) exprs of 
                                          []  -> fatal 126 "makeIntersectSelectExpr must not be used on empty list"
                                          [e] -> e
                                          es  -> -- Note: We now have at least two subexpressions
                                                 BSE { bseCmt = "`intersect` does not work in MySQL, so this statement is generated:"
                                                     , bseSrc = Iden[iSect 0,sourceAlias]
                                                     , bseTrg = Iden[iSect 0,targetAlias]
                                                     , bseTbl = map tableRef (zip [0..] es)
                                                     , bseWhr = Just . conjunctSQL . concatMap constraintsOfTailExpression $ 
                                                                   [1..length (tail es)]     
                                                     }
                                                  where
                                                   iSect :: Int -> Name
                                                   iSect n = Name ("subIntersect"++show n)
                                                   tableRef :: (Int, BinQueryExpr) -> TableRef
                                                   tableRef (n, e) = TRQueryExpr (toSQL e) `as` iSect n
                                                   constraintsOfTailExpression :: Int -> [ValueExpr]
                                                   constraintsOfTailExpression n 
                                                      = [ BinOp (Iden[iSect n,sourceAlias]) [Name "="] (Iden[iSect 0,sourceAlias])
                                                        , BinOp (Iden[iSect n,targetAlias]) [Name "="] (Iden[iSect 0,targetAlias])
                                                        ]
--                                          (e:es) -> BCQE { bcqeCmt  = "intersect case"
--                                                         , bcqeOper = Intersect
--                                                         , bcqe0    = selectExpr fSpec sourceAlias targetAlias e
--                                                         , bcqe1    = makeIntersectSelectExpr es
--                                                         }

    EUni (l,r) -> BCQE { bcqeCmt  = "case: EUni (l,r)"++showADL expr++" ("++show (sign expr)++")"
                       , bcqeOper = Union
                       , bcqe0    = selectExpr fSpec src trg l
                       , bcqe1    = selectExpr fSpec src trg r
                       }
    
    
--    ECps (EDcV (Sign ONE _), ECpl expr')
--     -> case target expr' of
--         ONE -> fatal 137 "sqlConcept not defined for ONE"
--         _   -> let src'  = sqlAttConcept fSpec (source expr')
--                    trg'  = sqlAttConcept fSpec (target expr')
--                    trg2  = noCollide' [src'] (sqlAttConcept fSpec (target expr'))
--                    allAtoms = Name "allAtoms"
--                    complemented = Name "complemented"
--                in BSE { bseCmt = "case:  ECps (EDcV (Sign ONE _), ECpl expr') "++showADL expr
--                       , bseSrc = NumLit "1"
--                       , bseTrg = Iden [trg'    ]
--                       , bseTbl = [sqlConceptTable fSpec (target expr') `as` allAtoms]
--                       , bseWhr = Just $ selectNotExists 
--                                           (selectExprInFROM fSpec src' trg' expr' `as` complemented)
--                                           (Just (BinOp (Iden [complemented,trg2])
--                                                          [Name "="]
--                                                        (Iden [allAtoms,trg'])
--                                                 )
--                                           )
--                       }
                                 
    ECps{}  ->
       case exprCps2list expr of
          [] -> fatal 190 ("impossible outcome of exprCps2list: "++showADL expr)
          [e]-> selectExpr fSpec src trg e -- Even though this case cannot occur, it safeguards that there are two or more elements in exprCps2list expr in the remainder of this code.
{-  We can treat the ECps expressions as poles-and-fences, with at least two fences.
    We start numbering the fences with 0. Each fence is connected to the previous fence with a pole.
    the pole holds the constraints of the connection of the fence to the previous fence. Only pole 0 has no previous 
    fence, so ther are no constraints. 
    In general, at some pole i, the constraint is that fence(i-1).trg=fencei.src
    However, there are exceptions for the expressions V and Mp1 (and possibly I??).
    For V, we don not calculate V, and we also pose no restrictions at the pole. 
    For Mp1, we do not calculate Mp1, but we do pose a restriction at the pole.  
    
    Imagine subexpressions as "fences". The source and target of a "fence" are the "poles" between which that "fence" is mounted.
    In this metaphor, we create the FROM-clause directly from the "fences", and the WHERE-clause from the "poles" between "fences".
    The "outer poles" correspond to the source and target of the entire expression.
    To prevent name conflicts in SQL, each calculated subexpression is aliased in SQL by a unique the fenceName. ".
-}
{- TODO: Check these assumptions:
     1) We assume that: let exprCps2list = [e0, e1, ... , en],
                             for all i: 0<=i< n the following is true:
                                if ei == EDcV{}  then e(i+1) /= EDcV{}
                                Or, in plain english: two neighbouring expressions are not both `V`
     2) We assume that for all expressions e in the list: e /= I[ONE]
           (We don't like:  ... ;V[A*ONE];I[ONE];V[ONE*B];... . It should have been normalized to V[A*B])
     3) We assume that for all neighbouring expressions ei and e(i+1) in the list cannot be both EMp1
           (`value1`;`value2` can be normalized to `value1` iff value1 == value2. Otherwise it can be normalized to the empty set )
-}
          es -> let fenceName :: Int -> Name
                    fenceName n = Name ("fence"++show n)
                    firstNr, lastNr :: Int
                    firstNr = 0
                    lastNr = firstNr + length es - 1
                    fenceExpr :: Int -> Expression 
                    fenceExpr i = case lookup i (zip [firstNr..lastNr] es) of
                                    Nothing -> fatal 238 "i out of bound!"
                                    Just e -> e 
                    fences :: [Maybe TableRef]
                    fences = map fenceTable [firstNr..lastNr]
                    fenceTable :: Int -> Maybe TableRef
                    fenceTable i = 
                      -- The first and the last fence must always exist, because the source and target of the entire expression 
                      -- depend on them. 
                      if i == firstNr || i == lastNr 
                      then makeNormalFence
                      else 
                        case fenceExpr i of 
                       -- In some cases of a non-outer expression, a fence need not be generated, to get better SQL queries. 
                            EDcV{} -> Nothing  
                            EMp1{} -> Nothing
                            _      -> makeNormalFence
                     where
                       makeNormalFence = Just $ (TRQueryExpr . toSQL . selectExpr fSpec sourceAlias targetAlias) (fenceExpr i) `as` fenceName i
                       
                                                                  
                                    
                    -- | between two fences there is a pole. The pole holds the constraint(s) between these fences.
                    polesConstraints :: [Maybe ValueExpr]
                    polesConstraints = map makePole [firstNr..lastNr - 1] --there is one pole less than fences...
                      where 
                        makePole :: Int -> Maybe ValueExpr
                        makePole i 
                         = case (fenceTable i, fenceTable (i+1)) of
                             (Just _ , Just _ ) -> 
                                                 Just (BinOp (Iden [fenceName i,targetAlias])
                                                             [Name "="]
                                                             (Iden [fenceName (i+1) ,sourceAlias])) 
                             -- When one or both sides have no fenceTable, that is because of optimation of
                             -- the SQL statement. Check the code of fenceTable for more details
                             (Just _ , Nothing) -> 
                                  case fenceExpr (i+1) of 
                                    EDcV _    -> noConstraint  "..;EDcV"
                                    EMp1 a _  -> Just (BinOp (Iden [fenceName i,targetAlias])
                                                             [Name "="]
                                                             (StringLit a))
                                    _         -> fatal 294 "there is no reason for having no fenceTable!"
                             (Nothing, Just _ ) ->
                                  case fenceExpr i of 
                                    EDcV _    -> noConstraint  "EDcV;.."
                                    EMp1 a _  -> Just (BinOp (StringLit a)
                                                             [Name "="]
                                                             (Iden [fenceName (i+1) ,sourceAlias]))
                                    _         -> fatal 294 "there is no reason for having no fenceTable!"

                             (Nothing, Nothing) -> 
                                  fatal 286 $ intercalate "\n  " $
                                     ["Can this happen? Here is a case to analyse: (i = "++show i++")"
                                     , "expr: "++showADL expr
                                     ]++map show (zip (map (stringOfName . fenceName) [firstNr..]) es)
                          where
                            noConstraint str = Just $ BinOp x [Name "="] x
                                     where
                                       x = StringLit ("pole"++show i++"_"++str)                       
                             
                             
                          
                in BSE { bseCmt = "case: (ECps es), with two or more elements in es."++showADL expr
                       , bseSrc = if source (head es) == ONE -- the first expression is V[ONE*someConcept]
                                  then NumLit "1"
                                  else Iden [fenceName firstNr,sourceAlias]
                       , bseTrg = if target (last es) == ONE -- the last expression is V[someConcept*ONE]
                                  then NumLit "1"
                                  else Iden [fenceName lastNr, targetAlias]
                       , bseTbl = catMaybes fences
                       , bseWhr = case catMaybes polesConstraints of
                                    [] -> Nothing
                                    cs -> Just (conjunctSQL cs) 
                       }
    (EFlp x) -> case selectExpr fSpec src trg x of
                 se@BSE{} -> BSE { bseCmt = "(flipped): "++ bseCmt se
                                 , bseSrc = bseTrg se
                                 , bseTrg = bseSrc se
                                 , bseTbl = bseTbl se
                                 , bseWhr = bseWhr se
                                 }
                 BCQE {bcqeOper=oper} -> fatal 313 $ "Unexpected: Directly flip a `"++show oper++"` expression. (What happend to the brackets?)"
    (EMp1 atom _) -> BSE { bseCmt = "case: EMp1 atom."
                         , bseSrc = sqlAtomQuote atom
                         , bseTrg = sqlAtomQuote atom
                         , bseTbl = []
                         , bseWhr = Nothing
                         }
    (EDcV (Sign s t))    -> sqlcomment ("case: (EDcV (Sign s t))   V[ \""++show (Sign s t)++"\" ]") $
                            let (psrc,fsrc) = getConceptTableInfo fSpec s
                                (ptgt,ftgt) = getConceptTableInfo fSpec t
                                (src1,trg1,tbl1) 
                                  = case (s,t) of
                                       (ONE, ONE) -> fatal 309 "V[ONE*ONE] ???"
                                       (_  , ONE) -> ( Iden [QName (name psrc),QName (name fsrc)]
                                                     , NumLit "1"
                                                     , [TRSimple [QName (name psrc)]]
                                                     )
                                       (ONE, _  ) -> ( NumLit "1"
                                                     , Iden [QName (name ptgt),QName (name ftgt)]
                                                     , [TRSimple [QName (name ptgt)]]
                                                     )
                                       _     -> if s == t
                                                then let a0 = QName (name fsrc)
                                                         a1 = QName (name fsrc++"1")
                                                     in
                                                     ( Iden [a0,QName (name fsrc)]
                                                     , Iden [a1,QName (name ftgt)]
                                                     , [TRSimple [QName (name psrc)] `as` a0
                                                       ,TRSimple [QName (name ptgt)] `as` a1]
                                                     )
                                                else ( Iden [QName (name psrc),QName (name fsrc)]
                                                     , Iden [QName (name ptgt),QName (name ftgt)]
                                                     , [TRSimple [QName (name psrc)]
                                                       ,TRSimple [QName (name ptgt)]]
                                                     )
                            in 
                               BSE { bseCmt = ""
                                   , bseSrc = src1
                                   , bseTrg = trg1
                                   , bseTbl = tbl1
                                   , bseWhr = Nothing
                                   }
    
    
    (EDcI c)             -> case c of
                              ONE ->   BSE { bseCmt = "I[ONE]"
                                           , bseSrc = NumLit "1"
                                           , bseTrg = NumLit "1"
                                           , bseTbl = []
                                           , bseWhr = Nothing
                                           }
                              PlainConcept{} -> 
                                 let cAtt = sqlAttConcept fSpec c
                                 in    BSE { bseCmt = "I["++name c++"]"
                                           , bseSrc = selectSelItem' (cAtt)
                                           , bseTrg = selectSelItem' (cAtt)
                                           , bseTbl = [sqlConceptTable fSpec c]
                                           , bseWhr = Just (notNull (Iden [cAtt]))
                                           }
    -- EEps behaves like I. The intersects are semantically relevant, because all semantic irrelevant EEps expressions have been filtered from es.
    (EEps c sgn)     -> case c of -- select the population of the most specific concept, which is the source.
                              ONE ->   BSE { bseCmt = "epsilon "++name c++" "++showSign sgn
                                           , bseSrc = NumLit "1"
                                           , bseTrg = NumLit "1"
                                           , bseTbl = []
                                           , bseWhr = Nothing
                                           }
                              PlainConcept{} -> 
                                 let cAtt = sqlAttConcept fSpec c
                                 in    BSE { bseCmt = "epsilon "++name c++" "++showSign sgn
                                           , bseSrc = selectSelItem' (cAtt)
                                           , bseTrg = selectSelItem' (cAtt)
                                           , bseTbl = [sqlConceptTable fSpec c]
                                           , bseWhr = Just (notNull (Iden [cAtt]))
                                           }
    (EDcD d)             -> selectExprRelationNew fSpec d

    (EBrk e)             -> selectExpr fSpec src trg e

    (ECpl e)
      -> case e of
           EDcV _        -> emptySet
           EDcI ONE      -> fatal 254 "EDcI ONE must not be seen at this place."
           EDcI c        ->            BSE { bseCmt = "case: ECpl (EDcI "++name c++")"
                                           , bseSrc = Iden [QName "concept0", concpt]
                                           , bseTrg = Iden [QName "concept1", concpt]
                                           , bseTbl = [sqlConceptTable fSpec c `as` QName "concept0"
                                                      ,sqlConceptTable fSpec c `as` QName "concept1"
                                                      ]
                                           , bseWhr = Just (BinOp (Iden [QName "concept0", concpt])
                                                                  [Name "<>"]
                                                                  (Iden [QName "concept1", concpt])
                                                           )
                                           }
                             where concpt = sqlAttConcept fSpec c
           _ | otherwise       -> sqlcomment ("case: ECpl e"++"ECpl ( \""++showADL e++"\" )") $
                                       BSE { bseCmt = "case: ECpl e"++"ECpl ( \""++showADL e++"\" )"
                                           , bseSrc = Iden [closedWorld,sourceAlias]
                                           , bseTrg = Iden [closedWorld,targetAlias]
                                           , bseTbl = [(toTableRef . selectExprInFROM' fSpec) theClosedWorldExpression `as` closedWorld]
                                           , bseWhr = Just $ selectNotExists 
                                                               (toTableRef (selectExprInFROM' fSpec e)) Nothing
                                           }
              where closedWorld = Name ("all_"++case theClosedWorldExpression of 
                                                  (EDcI c) -> plur c
                                                  (EDcV (Sign s t)) -> plur s ++ "_" ++ plur t 
                                                  _  -> fatal 434 "closedWorldExpression is not supposed to be of this kind."
                                       ) 
                          where plur c = plural (fsLang fSpec) (name c)
                    theClosedWorldExpression =
                       case (source e, target e) of
                         (ONE, ONE) -> fatal 425 "The complement of I[ONE] ???"
                         (ONE, t  ) -> EDcI t
                         (s  , ONE) -> EDcI s
                         (s  , t  ) | s == t    -> EDcI s
                                    | otherwise -> EDcV (Sign s t) 
                        
    EKl0 _               -> fatal 249 "SQL cannot create closures EKl0 (`SELECT * FROM NotExistingKl0`)"
    EKl1 _               -> fatal 249 "SQL cannot create closures EKl1 (`SELECT * FROM NotExistingKl1`)"
    (EDif (EDcV _,x)) -> sqlcomment ("case: EDif V x"++"EDif V ( \""++showADL x++"\" ) \""++show (sign expr)++"\"")
                                    (selectExpr fSpec src trg (notCpl x))
-- The following definitions express code generation of the remaining cases in terms of the previously defined generators.
-- As a result of this way of working, code generated for =, |-, -, !, *, \, and / may not be efficient, but at least it is correct.
    EEqu (l,r)
      -> sqlcomment ("case: EEqu (l,r)"++showADL expr++" ("++show (sign expr)++")") $
         selectExpr fSpec src trg ((ECpl l .\/. r) ./\. (ECpl r .\/. l))
    EImp (l,r)
      -> sqlcomment ("case: EImp (l,r)"++showADL expr++" ("++show (sign expr)++")") $
         selectExpr fSpec src trg (ECpl l .\/. r)
    EDif (l,r)
      -> sqlcomment ("case: EDif (l,r)"++showADL expr++" ("++show (sign expr)++")") $
         selectExpr fSpec src trg (l ./\. ECpl r)
    ERrs (l,r) -- The right residual l\r is defined by: for all x,y:   x(l\r)y  <=>  for all z in X, z l x implies z r y.
{- In order to obtain an SQL-query, we make a Haskell derivation of the right residual:
             and [    (z,x)    `elem` contents l -> (z,y) `elem` contents r  | z<-contents (source l)]
   =
             and [    (z,x) `notElem` contents l || (z,y) `elem` contents r  | z<-contents (source l)]
   =
        not ( or [not((z,x) `notElem` contents l || (z,y) `elem` contents r) | z<-contents (source l)])
   =
        not ( or [    (z,x)  `elem` contents l && (z,y) `notElem` contents r | z<-contents (source l)])
   =
        null [ () | z<-contents (source l), (z,x)  `elem` contents l && (z,y) `notElem` contents r]
   =
        null [ () | z<-contents (source l), (z,x)  `elem` contents l, (z,y) `notElem` contents r]
   =
        null [ () | (z,x') <- contents l, x==x', (z,y) `notElem` contents r ]
   =
        null [ () | (z,x') <- contents l, x==x' && (z,y) `notElem` contents r ]

Based on this derivation:
  contents (l\r)
    = [(x,y) | x<-contents (target l), y<-contents (target r)
             , null [ () | (z,x') <- contents l, x==x', (z,y) `notElem` contents r ]
             ]
-}
      -> let rResiduClause
              | target l == ONE = fatal 332 ("ONE is unexpected as target of "++showADL l)
              | target r == ONE = fatal 333 ("ONE is unexpected as target of "++showADL r)
              | otherwise
                  = BSE { bseCmt = ""
                        , bseSrc = Iden [ srcAlias, mainSrc]
                        , bseTrg = Iden [ tgtAlias, mainTgt]
                        , bseTbl = [sqlConceptTable fSpec (target l) `as` srcAlias
                                  ,sqlConceptTable fSpec (target r) `as` tgtAlias]
                        , bseWhr = Just $ selectNotExists 
                                            (lCode `as` lhs)
                                            ( Just $ conjunctSQL
                                                [BinOp (Iden [srcAlias,mainSrc])
                                                       [Name "="]
                                                       (Iden [lhs,ltrg])
                                                ,selectNotExists 
                                                   (rCode `as` rhs)
                                                   ( Just $ conjunctSQL 
                                                      [BinOp (Iden [rhs,rsrc])
                                                             [Name "="]
                                                             (Iden [lhs,lsrc])
                                                      ,BinOp (Iden [rhs,rtrg])
                                                             [Name "="]
                                                             (Iden [tgtAlias,mainTgt])
                                                      ]
                                                   )
                                                ]
                                            )
                        }
             mainSrc = (sqlAttConcept fSpec.target) l  -- Note: this 'target' is not an error!!! It is part of the definition of right residu
             mainTgt = (sqlAttConcept fSpec.target) r
             relNames = foldrMapExpression uni (\decl->[QName (name decl)]) [] expr
             srcAlias = noCollide' relNames (QName "RResLeft")
             tgtAlias = noCollide' relNames (QName "RResRight")
             lhs  = QName "lhs"
             rhs  = QName "rhs"
             lsrc = sqlExprSrc fSpec l
             ltrg = sqlExprTgt fSpec l  -- shouldn't this be a noCollide? Apparently not. Introducing noCollide here has produced ticket #389
             rsrc = sqlExprSrc fSpec r
             rtrg = sqlExprTgt fSpec r  -- shouldn't this be a noCollide? (idem)
             lCode = selectExprInFROM fSpec lsrc ltrg l
             rCode = selectExprInFROM fSpec rsrc rtrg r
         in sqlcomment ("case: ERrs (l,r)"++showADL expr++" ("++show (sign expr)++")")
                         rResiduClause
    ELrs (l,r)
      -> sqlcomment ("case: ELrs (l,r)"++showADL expr++" ("++show (sign expr)++")") $
         selectExpr fSpec trg src (EFlp (flp r .\. flp l))
    EDia (l,r)
      -> sqlcomment ("case: EDia (l,r)"++showADL expr++" ("++show (sign expr)++")") $
         selectExpr fSpec trg src ((flp l .\. r) ./\. (l ./. flp r))
    ERad{}
      -> sqlcomment ("case: ERad (l,r)"++showADL expr++" ("++show (sign expr)++")") $
        selectExpr fSpec src trg (deMorganERad expr)
    EPrd (l,r)
     -> let v = EDcV (Sign (target l) (source r))
        in sqlcomment ("case: EPrd (l,r)"++showADL expr++" ("++show (sign expr)++")") $
           selectExpr fSpec src trg (foldr1 (.:.) [l,v,r])








-- | selectExprInFROM is meant for SELECT expressions inside a FROM clause.
--   It generates a simple table reference for primitive expressions (EDcD, EDcI, and EDcV) and a bracketed SQL expression in more complicated situations.
--   Note that selectExprInFROM makes sure that the attributes of the generated view correspond to the parameters src and trg.
--   Note that the resulting pairs do not contain any NULL values.
selectExprInFROM' :: FSpec -> Expression -> BinQueryExpr
selectExprInFROM' fSpec expr = selectExpr fSpec sourceAlias targetAlias expr
toTableRef :: BinQueryExpr -> TableRef
toTableRef b = TRQueryExpr . toSQL $ b
selectExprInFROM :: FSpec
                 -> Name      -- ^ source name (preferably quoted)
                 -> Name      -- ^ target name (preferably quoted)
                 -> Expression  -- ^ Whatever expression to generate an SQL query for
                 -> TableRef
selectExprInFROM fSpec src trg expr 
   | src == trg && (not.isIdent) expr = fatal 373 $ "selectExprInFrom must not be called with identical src and trg. ("++show src++") "++showADL expr
   | unquoted src = selectExprInFROM fSpec (toQName src) trg         expr
   | unquoted trg = selectExprInFROM fSpec src         (toQName trg) expr
   | otherwise    =
      case expr of
        EFlp e -> selectExprInFROM fSpec trg src e
        EBrk e -> selectExprInFROM fSpec src trg e
        EDcD d -> if sqlExprSrc fSpec expr === src && sqlExprTgt fSpec expr === trg
                  then ( if not mayContainNulls 
                         then TRSimple [declName]
                         else TRQueryExpr . toSQL $
                                       BSE { bseCmt = ""
                                           , bseSrc = selectSelItem' sAtt
                                           , bseTrg = selectSelItem' tAtt
                                           , bseTbl = [TRSimple [declName]]
                                           , bseWhr = Just $ conjunctSQL
                                               [notNull (Iden[src]), notNull (Iden[trg])]
                                           }
                       )
                  else TRQueryExpr . toSQL $   BSE { bseCmt = ""
                                           , bseSrc = selectSelItem' sAtt
                                           , bseTrg = selectSelItem' tAtt
                                           , bseTbl = [TRSimple [declName]]
                                           , bseWhr = if mayContainNulls
                                                      then (Just $ conjunctSQL
                                                              [notNull (Iden[sAtt]), notNull (Iden[tAtt])])
                                                      else Nothing
                                           }
                  where
                   sAtt = sqlExprSrc fSpec expr
                   tAtt = sqlExprTgt fSpec expr
                   (plug,_,_) = getDeclarationTableInfo fSpec d
                   (declName,mayContainNulls)
                      = (QName (name plug), case plug of 
                                              TblSQL{}  ->  True
                                              _         ->  False)
        EDcI ONE -> fatal 401 "ONE is unexpected at this place."
        EDcI c -> case (cpt, cptAlias) of
                    (cpt', (Iden [x])) -> if cpt'=== x
                                          then TRSimple [cpt']
                                          else sg
                    _                  -> sg
                 where  
                   sg = TRQueryExpr . toSQL $  BSE { bseCmt = " case: EDcI " ++ name c ++ " "
                                           , bseSrc = selectSelItem' (sqlAttConcept fSpec c)
                                           , bseTrg = Iden [sqlConcept fSpec c]
                                           , bseTbl = [sqlConceptTable fSpec c]
                                           , bseWhr = Nothing
                                           }
                   cptAlias = selectSelItem' (sqlAttConcept fSpec c)
                   cpt = sqlConcept fSpec c
        _      -> TRQueryExpr . toSQL $ selectExpr fSpec src trg expr
   where
     unquoted :: Name -> Bool
     unquoted (Name _ )   = True
     unquoted (QName _)   = False
     unquoted (UQName  _) = False --UQName = UNICODE quoted
     
(===) :: Name -> Name -> Bool
n === n' = stringOfName n == stringOfName n'

-- | does the same as noCollide, but ensures that all names used have `quotes` around them (for mySQL)
noCollide' :: [Name] -> Name -> Name
noCollide' nms nm = toQName (noCollide (map toUqName nms) (toUqName nm))
 where
   noCollide :: [Name] -- ^ forbidden names
             -> Name -- ^ preferred name
             -> Name -- ^ a unique name (does not occur in forbidden names)
   noCollide names nm' | nm'' `elem` map toUqName names = noCollide names (newNumber nm'')
                       | otherwise = nm'
    where
      nm''           = toUqName nm'
      newNumber :: Name -> Name
      newNumber nm1 = Name (reverse reverseNamePart++ changeNr (reverse reverseNumberpart))
        where
          ( reverseNumberpart, reverseNamePart) = span isDigit  . reverse . stringOfName $ nm1
      
      changeNr x     = int2string (string2int x+1)
      --  changeNr x = show (read x +1)
      string2int :: String -> Int
      string2int  = enc.reverse
       where enc "" = 0
             enc (c:cs) = digitToInt c + 10* enc cs
      int2string :: Int -> String
      int2string 0 = "0"
      int2string n = if n `div` 10 == 0 then [intToDigit (n `rem` 10) |n>0] else int2string (n `div` 10)++[intToDigit (n `rem` 10)]


-- | This function returns a (multy-lines) prettyprinted SQL qurey of a declaration. 
selectExprRelation :: FSpec
                   -> Declaration
                   -> String
selectExprRelation fSpec dcl
 = prettyQueryExpr . toSQL $ selectExprRelationNew fSpec dcl
     
selectExprRelationNew :: FSpec
         --          -> Name -- ^ Alias of source
         --          -> Name -- ^ Alias of target
                   -> Declaration
                   -> BinQueryExpr


selectExprRelationNew fSpec dcl =
  case dcl of
    Sgn{}  -> leafCode (getDeclarationTableInfo fSpec dcl)
    Isn{}  -> let (plug, c) = getConceptTableInfo fSpec (detyp dcl)
              in leafCode (plug, c, c)
    Vs sgn
     | source sgn == ONE -> fatal 468 "ONE is not expected at this place"
     | target sgn == ONE -> fatal 469 "ONE is not expected at this place"
     | otherwise
           -> let src,trg :: ValueExpr
                  src=Iden [Name "vfst", sqlAttConcept fSpec (source sgn)]
                  trg=Iden [Name "vsnd", sqlAttConcept fSpec (target sgn)]
              in BSE { bseCmt = ""
                     , bseSrc = src
                     , bseTrg = trg
                     , bseTbl = [sqlConceptTable fSpec (source sgn) `as` Name "vfst"
                                ,sqlConceptTable fSpec (target sgn) `as` Name "vsnd"]
                     , bseWhr = Just (conjunctSQL (map notNull [src,trg]))
                     }
   where
     leafCode :: (PlugSQL,SqlField,SqlField) -> BinQueryExpr
     leafCode (plug,s,t) = BSE { bseCmt = ""
                               , bseSrc = Iden [QName (name s)]
                               , bseTrg = Iden [QName (name t)]
                               , bseTbl = [TRSimple [QName (name plug)]]
                               , bseWhr = Just . conjunctSQL . map notNull $
                                            [Iden [QName (name c)] | c<-nub [s,t]]
                               }


selectExists, selectNotExists
     :: TableRef      -- ^ tables
     -> Maybe ValueExpr -- ^ the (optional) WHERE clause
     -> ValueExpr
selectNotExists tbl whr = PrefixOp [Name "NOT"] $ selectExists tbl whr
selectExists tbl whr = 
  SubQueryExpr SqExists
     Select { qeSetQuantifier = SQDefault
            , qeSelectList    = [(Star,Nothing)]   
            , qeFrom          = [tbl `as` Name "aDummyName" ] -- dummyname is required because MySQL requires you to label the "sub query" instead of just leaving it like many other implementations.
            , qeWhere         = whr
            , qeGroupBy       = []
            , qeHaving        = Nothing
            , qeOrderBy       = []
            , qeOffset        = Nothing
            , qeFetchFirst    = Nothing
            }

-- | a (local) data structure to hold SQL info for binary expressions
data BinQueryExpr = BSE  { bseCmt :: String          -- ^ Comment for the binary SQL SELECT statement
                         , bseSrc :: ValueExpr       -- ^ source field and table
                         , bseTrg :: ValueExpr       -- ^ target field and table
                         , bseTbl :: [TableRef]      -- ^ tables
                         , bseWhr :: Maybe ValueExpr -- ^ the (optional) WHERE clause
                         }
                  | BCQE { bcqeCmt  :: String          -- ^ Comment for the binary CombineQueryExpr statement (Union, Intersect)
                         , bcqeOper :: CombineOp      -- ^ The combine operator 
                         , bcqe0    :: BinQueryExpr    -- ^ Left  expression
                         , bcqe1    :: BinQueryExpr    -- ^ Right expression
                         }
                        
toSQL :: BinQueryExpr -> QueryExpr
toSQL bqe 
 = case bqe of
    BSE{} -> Select { qeSetQuantifier = Distinct
                    , qeSelectList    = [ (bseSrc bqe, Just sourceAlias)
                                        , (bseTrg bqe, Just targetAlias)]
                    , qeFrom          = bseTbl bqe
                    , qeWhere         = bseWhr bqe
                    , qeGroupBy       = []
                    , qeHaving        = Nothing
                    , qeOrderBy       = []
                    , qeOffset        = Nothing
                    , qeFetchFirst    = Nothing
                    }
    BCQE{} -> CombineQueryExpr 
                    { qe0 = toSQL (bcqe0 bqe)
                    , qeCombOp = bcqeOper bqe
                    , qeSetQuantifier = SQDefault
                    , qeCorresponding = Respectively  -- ??? What does this mean?
                    , qe1 = toSQL (bcqe1 bqe)
                    }
selectSelItem' :: Name -> ValueExpr
selectSelItem' att = Iden [att] 

--selectSelItem :: (Name, Name) -> (ValueExpr ,Maybe Name)
--selectSelItem (att,alias)
--  | att === alias           = (Iden [toQName att], Nothing)
--  | stringOfName att == "1" = fatal 778 "ONE should have no named string" -- otherwise use: (NumLit "1", Just alias)
--  | otherwise               = (Iden [toQName att], Just alias)


-- | sqlExprSrc gives the quoted SQL-string that serves as the attribute name in SQL.
--   Quotes are added to prevent collision with SQL reserved words (e.g. ORDER).
--   We want it to show the type, which is useful for readability. (Otherwise, just "SRC" and "TGT" would suffice)
sqlExprSrc :: FSpec -> Expression -> Name
--sqlExprSrc fSpec expr = sourceAlias
sqlExprSrc fSpec (EDcV (Sign a _))   = sqlAttConcept fSpec a
sqlExprSrc fSpec (EDcI c)            = sqlAttConcept fSpec c
sqlExprSrc fSpec (EEps i _)          = sqlAttConcept fSpec i
sqlExprSrc fSpec (EFlp e)            = sqlExprTgt fSpec e
sqlExprSrc fSpec (EDcD d)            = let (_,s,_) = getDeclarationTableInfo fSpec d
                                       in QName (name s)
sqlExprSrc _     expr                = QName (name (source expr))


-- | sqlExprTgt gives the quoted SQL-string that serves as the attribute name in SQL.
sqlExprTgt :: FSpec -> Expression -> Name
--sqlExprTgt fSpec expr = targetAlias
sqlExprTgt fSpec (EDcV (Sign _ b))   = sqlAttConcept fSpec b
sqlExprTgt fSpec (EDcI c)            = sqlAttConcept fSpec c
sqlExprTgt fSpec (EEps i _)          = sqlAttConcept fSpec i
sqlExprTgt fSpec (EFlp e)            = sqlExprSrc fSpec e
sqlExprTgt fSpec (EDcD d)            = let (_,_,t) = getDeclarationTableInfo fSpec d
                                       in QName (name t)
sqlExprTgt _     expr                = QName (name (target expr))

-- sqlConcept gives the name of the plug that contains all atoms of A_Concept c.
-- Quotes are added just in case an SQL reserved word (e.g. "ORDER", "SELECT", etc.) is used as a concept name.
sqlConceptTable :: FSpec -> A_Concept -> TableRef
sqlConceptTable fSpec a = TRSimple [sqlConcept fSpec a]
sqlConcept :: FSpec -> A_Concept -> Name
sqlConcept fSpec = QName . name . sqlConceptPlug fSpec
-- sqlConcept yields the plug that contains all atoms of A_Concept c. Since there may be more of them, the first one is returned.
sqlConceptPlug :: FSpec -> A_Concept -> PlugSQL
sqlConceptPlug fSpec c | c==ONE = fatal 583 "A_Concept ONE may not be represented in SQL."
                       | otherwise
             = if null ps then fatal 585 $ "A_Concept \""++show c++"\" does not occur in fSpec." else
               head ps
               where ps = [plug |InternalPlug plug<-plugInfos fSpec
                                , not (null (case plug of ScalarSQL{} -> [c |c==cLkp plug]; _ -> [c' |(c',_)<-cLkpTbl plug, c'==c]))]

sqlAttConcept :: FSpec -> A_Concept -> Name
sqlAttConcept fSpec c | c==ONE = QName "ONE"
                      | otherwise
             = if null cs then fatal 594 $ "A_Concept \""++show c++"\" does not occur in its plug in fSpec \""++name fSpec++"\"" else
               QName (head cs)
               where cs = [name f |f<-plugFields (sqlConceptPlug fSpec c), c'<-concs f,c==c']


toUqName :: Name -> Name
toUqName = Name . stringOfName

toQName :: Name -> Name
toQName = QName . stringOfName

stringOfName :: Name -> String
stringOfName (Name s)   =  s
stringOfName (QName s)  =  s
stringOfName (UQName s) =  s

sqlAtomQuote :: String -> ValueExpr
sqlAtomQuote s = StringLit s

-- | for the time untill comment is supported, we use a dummy function 
sqlcomment :: String -> a -> a 
sqlcomment _ a = a 


conjunctSQL :: [ValueExpr] -> ValueExpr
conjunctSQL [] = fatal 57 "nothing to `AND`."
conjunctSQL [ve] = ve
conjunctSQL (ve:ves) = BinOp ve [Name "AND"] (conjunctSQL ves)

as :: TableRef -> Name -> TableRef
as ve a = -- TRAlias ve (Alias a Nothing)
  case ve of 
    TRSimple [n] -> if n === a then withoutAlias else withAlias
    _            -> withAlias
 where
   withoutAlias = ve
   withAlias = TRAlias ve (Alias a Nothing)
    
notNull :: ValueExpr -> ValueExpr
notNull ve = PostfixOp [Name "IS NOT NULL"] ve                         

emptySet :: BinQueryExpr
emptySet = BSE { bseCmt = "this will quaranteed return 0 rows:"
               -- select 1 as src, 1 as trg from (select 1) dummy where false
               , bseSrc = Iden [a]
               , bseTrg = Iden [a]
               , bseTbl = [TRQueryExpr  Select { qeSetQuantifier = SQDefault
                                               , qeSelectList = [(NumLit "1", Just a)]
                                               , qeFrom = []
                                               , qeWhere = Nothing
                                               , qeGroupBy = []
                                               , qeHaving = Nothing
                                               , qeOrderBy = []
                                               , qeOffset = Nothing
                                               , qeFetchFirst = Nothing
                                               } `as` Name "dummy"]
               , bseWhr = Just (BinOp (Iden [a]) [Name "<>"] (NumLit "1"))
               }
            where a = Name "a"



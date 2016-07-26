module Database.Design.Ampersand.FSpec.SQL
  ( placeHolderSQL
  , prettySQLQuery               , sqlQuery
  , prettySQLQueryWithPlaceholder, sqlQueryWithPlaceholder 
  , prettyBroadQueryWithPlaceholder,broadQueryWithPlaceholder)
  
where
import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.Pretty
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Classes.ConceptStructure
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.ADL1.Expression
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.FSpec.FSpecAux
import Database.Design.Ampersand.FSpec.ShowADL
import Database.Design.Ampersand.Misc
import Data.List
import Data.Maybe

placeHolderSQL :: String
placeHolderSQL = "_SRCATOM"

broadQueryWithPlaceholder :: FSpec -> ObjectDef -> String
broadQueryWithPlaceholder fSpec
  = unwords . words 
     . prettyQueryExpr theDialect
     . broadQuery fSpec 
prettyBroadQueryWithPlaceholder :: Int -> FSpec -> ObjectDef -> String
prettyBroadQueryWithPlaceholder i fSpec
    =  intercalate ("\n"++replicate i ' ') 
     . lines
     . prettyQueryExpr theDialect 
     . broadQuery fSpec

class SQLAble a where
  -- | show SQL query without comments and not prettyprinted
  sqlQuery, sqlQueryWithPlaceholder :: FSpec -> a -> String
  sqlQuery                 = doNonPretty getBinQueryExpr
  sqlQueryWithPlaceholder  = doNonPretty getBinQueryExprPlaceholder

  doNonPretty :: (FSpec -> a -> BinQueryExpr) -> FSpec -> a -> String
  doNonPretty fun fSpec 
    =  unwords . words 
     . prettyQueryExpr theDialect . toSQL
     . stripComment
     . fun fSpec

  prettySQLQuery, prettySQLQueryWithPlaceholder 
          :: Int    -- Amount of indentation
          -> FSpec  -- The context
          -> a      
          -> String 
  prettySQLQueryWithPlaceholder = doPretty getBinQueryExprPlaceholder
  prettySQLQuery                = doPretty getBinQueryExpr
  doPretty :: (FSpec -> a -> BinQueryExpr) -> Int -> FSpec -> a -> String
  doPretty fun i fSpec 
    =  intercalate ("\n"++replicate i ' ') 
     . lines
     . prettyQueryExpr theDialect 
     . toSQL
     . fun fSpec
  
  getBinQueryExpr ::  FSpec -> a -> BinQueryExpr
  getBinQueryExprPlaceholder :: FSpec -> a -> BinQueryExpr
  getBinQueryExprPlaceholder fSpec = insertPlaceholder . getBinQueryExpr fSpec 
    where 
      insertPlaceholder :: BinQueryExpr -> BinQueryExpr
      insertPlaceholder bqe 
        = case bqe of
            BSE{} -> case (col2ValueExpr (bseSrc bqe),bseWhr bqe) of
                       (Iden [_] , _ ) 
                            -> bqeWithPlaceholder
                       (Iden [_,a], _ )
                         | a == sourceAlias
                            -> bqeWithPlaceholder
                         | otherwise -> bqeWithoutPlaceholder
                       _ -> bqeWithoutPlaceholder
            BCQE{} -> BCQE { bcqeOper = bcqeOper bqe
                           , bcqe0 = insertPlaceholder . bcqe0 $ bqe
                           , bcqe1 = insertPlaceholder . bcqe1 $ bqe
                           }
            BQEComment _ x -> insertPlaceholder x
        where 
          bqeWithoutPlaceholder = BQEComment [BlockComment "THERE IS NO PLACEHOLDER HERE"] bqe
          bqeWithPlaceholder = 
             BSE { bseSrc = bseSrc bqe
                 , bseTrg = bseTrg bqe
                 , bseTbl = bseTbl bqe
                 , bseWhr = Just $
                             case bseWhr bqe of
                              Nothing  -> placeHolder
                              Just whr -> conjunctSQL [ placeHolder, whr ]
                 }
          placeHolder = BinOp (col2ValueExpr (bseSrc bqe)) [Name "="] (StringLit placeHolderSQL) 
instance SQLAble Expression where  
  getBinQueryExpr = selectExpr
instance SQLAble Declaration where
  getBinQueryExpr = selectDeclaration
     
sourceAlias, targetAlias :: Name
sourceAlias = Name "src" 
targetAlias = Name "tgt"
selectExpr :: FSpec    -- current context
        -> Expression  -- expression to be translated
        -> BinQueryExpr   -- resulting info for the binary SQL expression
-- In order to translate all Expressions, code generators have been written for EUni ( \/ ), EIsc ( /\ ), EFlp ( ~ ), ECpl (unary - ), and ECps ( ; ),
-- each of which is supposed to generate correct code in 100% of the cases. (TODO: how do we establish that properly?)
-- The other operators, EEqu ( = ), EInc ( |- ), ERad ( ! ), EPrd ( * ), ELrs ( / ), ERrs ( \ ), and EDia ( <> ), have been implemented in terms of the previous ones,
-- in order to prevent mistakes in the code generator. It is possible that more efficient code may be generated in these cases.
-- Special cases are treated up front, so they will overrule the more general cases.
-- That allows more efficient code while retaining correctness and completeness as much as possible.
-- Code for the Kleene operators EKl0 ( * ) and EKl1 ( + ) is not done, because this cannot be expressed in SQL.
-- These operators must be eliminated from the Expression before using selectExpr, or else you will get fatals.
selectExpr fSpec expr 
 = fromMaybe (nonSpecialSelectExpr fSpec expr) (maybeSpecialCase fSpec expr) --special cases for optimized results.

-- Special cases for optimized SQL generation
-- Sometimes it is possible to generate queries that perform better. If this is the case for some 
-- expression, this function will return the optimized query. 
maybeSpecialCase :: FSpec -> Expression -> Maybe BinQueryExpr
maybeSpecialCase fSpec expr = 
  case expr of 
    EIsc (EDcI a , ECpl (ECps (EDcD r,EFlp (EDcD r')) )) 
      | r == r'   -> Just . BQEComment 
                              [ BlockComment $ "Optimized case for: "++name r++showSign r++" [TOT]."
                              , BlockComment $ "   "++showADL expr++" ("++show (sign expr)++")"
                              ] $ 
                                 let col = Col { cTable = [Name "notIns"]
                                               , cCol   = [sqlAttConcept fSpec a]
                                               , cAlias = []
                                               , cSpecial = Nothing}
                                     aAtt = col2ValueExpr col
                                     whereClause = 
                                       conjunctSQL [ aAtt `isNotIn` selectSource (selectExpr fSpec (EDcD r))
                                                   , notNull aAtt
                                                   ]
                                 in    
                                   BSE { bseSrc = col
                                       , bseTrg = col
                                       , bseTbl = [sqlConceptTable fSpec a `as` Name "notIns"]
                                       , bseWhr = Just whereClause
                                       }
      | otherwise -> Nothing
    EIsc (expr1 , ECpl expr2)
                  -> go False expr1 expr2
    EIsc (expr1 , EFlp (ECpl expr2))
                  -> go True expr1 expr2
    _ -> Nothing 
  where 
    go :: Bool -> Expression -> Expression -> Maybe BinQueryExpr
    go isFlipped expr1 expr2 = Just .
      BQEComment
        [ BlockComment . unlines $
             [ "Optimized case for: <expr1> intersect with the "
                   ++(if isFlipped then "flipped " else "")
                   ++"complement of "++(case expr2 of 
                                           EDcD (dcl@Sgn{}) -> "`"++name dcl++"`"
                                           _                -> "<expr2>"
                                        )++"."
             , "where "
             , "  <expr1> = "++showADL expr1++" ("++show (sign expr1)++")"
             , "  <expr2> = "++showADL expr2++" ("++show (sign expr2)++")"
             , "   "++showADL expr++" ("++show (sign expr)++")"
             ]
        ] $ BSE { bseSrc = Col { cTable = [table1]
                               , cCol   = [sourceAlias]
                               , cAlias = []
                               , cSpecial = Nothing}
                , bseTrg = Col { cTable = [table1]
                               , cCol   = [targetAlias]
                               , cAlias = []
                               , cSpecial = Nothing}
                , bseTbl = [TRJoin 
                              (TRQueryExpr (toSQL (selectExpr fSpec expr1)) `as` table1)
                              False -- Needs to be false in MySql
                              JLeft
                              leftTable
                              (Just . JoinOn . conjunctSQL $
                                       [ BinOp (Iden[table1,sourceAlias]) [Name "="] (Iden[table2,expr2Src])
                                       , BinOp (Iden[table1,targetAlias]) [Name "="] (Iden[table2,expr2trg])
                                       ])
                           ]
                , bseWhr = Just . disjunctSQL $
                                    [ isNull (Iden[table2,expr2Src])
                                    , isNull (Iden[table2,expr2trg])
                                    ]
                }
     where
      fun = if isFlipped then flp else id
      (expr2Src,expr2trg,leftTable) =
         case expr2 of
           EDcD (dcl@Sgn{}) -> 
               let (plug,s,t) = getDeclarationTableInfo fSpec dcl
                   lt = TRSimple [QName (name plug)] `as` table2
               in if isFlipped 
                  then (QName (name t), QName (name s), lt)
                  else (QName (name s), QName (name t), lt)
           _ -> ( sourceAlias, targetAlias
                , TRQueryExpr (toSQL (selectExpr fSpec (fun expr2))) `as` table2)
      table1 = Name "t1"
      table2 = Name "t2"

nonSpecialSelectExpr :: FSpec -> Expression -> BinQueryExpr
nonSpecialSelectExpr fSpec expr=
    case expr of
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
                  posVals :: [PSingleton]
                  posVals = nub (map atmValue posMp1Terms)
                  negVals :: [PSingleton]
                  negVals = nub (map (atmValue . notCpl) negMp1Terms)
                  atmValue (EMp1 a _) = a
                  atmValue _          = fatal 31 "atm error"
                  mp1Terms, nonMp1Terms :: [Expression]
                  (mp1Terms,nonMp1Terms) = partition isMp1 (exprIsc2list expr)
                  posMp1Terms, negMp1Terms :: [Expression]
                  (posMp1Terms,negMp1Terms) = partition isPos mp1Terms
                  f :: Maybe PSingleton   -- Optional the singleton value that might be found as the only possible value 
                      -> [Expression] -- subexpressions of the intersection.  Mp1{} nor ECpl(Mp1{}) are allowed elements of this list.  
                      -> BinQueryExpr
                  f specificValue subTerms 
                     = BQEComment [BlockComment . unlines $
                                       ("case: (EIsc "++showADL expr++" ("++show (sign expr)++")")
                                       :
                                       case expr of 
                                          EIsc (a,b) -> [show a, show b]
                                          _ -> fatal 148 $ "Not expecting anything else here than EIsc!\n  " ++ show expr
                                  ] $
                        case subTerms of
                          [] -> case specificValue of 
                                 Nothing  -> emptySet -- case might occur with only negMp1Terms??
                                 Just singleton -> selectExpr fSpec (EMp1 singleton (source expr))
                          ts  ->    BSE { bseSrc = theSr'
                                        , bseTrg = theTr'
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
                                            equalToValueClause :: PSingleton -> ValueExpr
                                            equalToValueClause singleton = conjunctSQL 
                                                               [ BinOp (col2ValueExpr theSr') [Name "="] (singleton2SQL (source expr) singleton)
                                                               , BinOp (col2ValueExpr theTr') [Name "="] (singleton2SQL (source expr) singleton)
                                                               ]

                                       forbiddenTuples :: Maybe ValueExpr
                                       forbiddenTuples = 
                                           case negVals of
                                            []  -> Nothing
                                            _   -> Just . conjunctSQL $
                                                     map notEqualToValueClause negVals
                                          where
                                            notEqualToValueClause :: PSingleton -> ValueExpr
                                            notEqualToValueClause singleton = conjunctSQL 
                                                               [ BinOp (col2ValueExpr theSr') [Name "<>"] (singleton2SQL (source expr) singleton)
                                                               , BinOp (col2ValueExpr theTr') [Name "<>"] (singleton2SQL (source expr) singleton)
                                                               ]

                                       theSr' = bseSrc (makeSelectable sResult)
                                       theTr' = bseTrg (makeSelectable sResult)
                                       theTbl = bseTbl (makeSelectable sResult)
                                       theWhr = case makeSelectable sResult of
                                                  e@BSE{}      -> bseWhr e
                                                  BCQE{}       -> fatal 129 "makeSelectable is not doing what it is supposed to do!" 
                                                  BQEComment{} -> fatal 130 "makeSelectable is not doing what it is supposed to do!" 
                                       sResult = makeIntersectSelectExpr ts
                                       dummy = Name "someDummyNameBecauseMySQLNeedsOne"
                                       makeSelectable :: BinQueryExpr -> BinQueryExpr
                                       makeSelectable x =
                                         case x of
                                           BSE{}   -> x
                                           _       -> BSE { bseSrc = Col { cTable = [dummy]
                                                                         , cCol   = [sourceAlias]
                                                                         , cAlias = []
                                                                         , cSpecial = Nothing}
                                                          , bseTrg = Col { cTable = [dummy]
                                                                         , cCol   = [targetAlias]
                                                                         , cAlias = []
                                                                         , cSpecial = Nothing}
                                                          , bseTbl = [TRQueryExpr (toSQL x) `as` dummy]
                                                          , bseWhr = Nothing
                                                          }
                                       makeIntersectSelectExpr :: [Expression] -> BinQueryExpr
                                       makeIntersectSelectExpr exprs =
                                        case map (selectExpr fSpec) exprs of 
                                          []  -> fatal 126 "makeIntersectSelectExpr must not be used on empty list"
                                          [e] -> e
                                          es  -> -- Note: We now have at least two subexpressions
                                                 BQEComment [BlockComment "`intersect` does not work in MySQL, so this statement is generated:"]
                                                 BSE { bseSrc = Col { cTable = [iSect 0]
                                                                    , cCol   = [sourceAlias]
                                                                    , cAlias = []
                                                                    , cSpecial = Nothing}
                                                     , bseTrg = Col { cTable = [iSect 0]
                                                                    , cCol   = [targetAlias]
                                                                    , cAlias = []
                                                                    , cSpecial = Nothing}
                                                     , bseTbl = zipWith tableRef [0 ..] es
                                                     , bseWhr = Just . conjunctSQL . concatMap constraintsOfTailExpression $ 
                                                                   [1..length (tail es)]     
                                                     }
                                                  where
                                                   iSect :: Int -> Name
                                                   iSect n = Name ("subIntersect"++show n)
                                                   tableRef :: Int -> BinQueryExpr -> TableRef
                                                   tableRef n e = TRQueryExpr (toSQL e) `as` iSect n
                                                   constraintsOfTailExpression :: Int -> [ValueExpr]
                                                   constraintsOfTailExpression n 
                                                      = [ BinOp (Iden[iSect n,sourceAlias]) [Name "="] (Iden[iSect 0,sourceAlias])
                                                        , BinOp (Iden[iSect n,targetAlias]) [Name "="] (Iden[iSect 0,targetAlias])
                                                        ]

    EUni (l,r) -> BQEComment [BlockComment $ "case: EUni (l,r)"++showADL expr++" ("++show (sign expr)++")"]
                  BCQE { bcqeOper = Union
                       , bcqe0    = selectExpr fSpec l
                       , bcqe1    = selectExpr fSpec r
                       }
                                 
    ECps{}  ->
       case exprCps2list expr of
          [] -> fatal 190 ("impossible outcome of exprCps2list: "++showADL expr)
          [e]-> selectExpr fSpec e -- Even though this case cannot occur, it safeguards that there are two or more elements in exprCps2list expr in the remainder of this code.
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
                    fenceExpr i = fromMaybe (fatal 238 "i out of bound!")
                                . lookup i . zip [firstNr .. lastNr] $ es
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
                            _      -> makeNormalFence
                     where
                       makeNormalFence = Just $ (TRQueryExpr . toSQL . selectExpr fSpec) (fenceExpr i) `as` fenceName i
                       
                                                                  
                                    
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
                                    EDcV _    -> Nothing
                                    _         -> fatal 251 "there is no reason for having no fenceTable!"
                             (Nothing, Just _ ) ->
                                  case fenceExpr i of 
                                    EDcV _    -> Nothing
                                    _         -> fatal 258 "there is no reason for having no fenceTable!"

                             (Nothing, Nothing) -> 
                                  -- This must be the special case: ...;V[A*B];V[B*C];....
                                 Just . SubQueryExpr SqExists . toSQL 
                                  . BQEComment [BlockComment "Case: ...;V[A*B];V[B*C];...."]
                                  . selectExpr fSpec . EDcI . target . fenceExpr $ i

                in BQEComment [BlockComment $ "case: (ECps es), with two or more elements in es."++showADL expr]
                   BSE { bseSrc = if source (head es) == ONE -- the first expression is V[ONE*someConcept]
                                  then theONESingleton
                                  else Col { cTable = [fenceName firstNr]
                                           , cCol   = [sourceAlias]
                                           , cAlias = []
                                           , cSpecial = Nothing}
                       , bseTrg = if target (last es) == ONE -- the last expression is V[someConcept*ONE]
                                  then theONESingleton
                                  else Col { cTable = [fenceName lastNr]
                                           , cCol   = [targetAlias]
                                           , cAlias = []
                                           , cSpecial = Nothing}
                       , bseTbl = catMaybes fences
                       , bseWhr = case catMaybes polesConstraints of
                                    [] -> Nothing
                                    cs -> Just (conjunctSQL cs) 
                       }
    (EFlp x) -> flipped (selectExpr fSpec x)
                 where 
                   fTable = Name "flipped"
                   flipped se =
                     BQEComment [BlockComment ("Flipped: "++show x)] $
                        case se of 
                         BSE{}  -> BSE { bseSrc = bseTrg se
                                       , bseTrg = bseSrc se
                                       , bseTbl = bseTbl se
                                       , bseWhr = bseWhr se
                                       }
                         BCQE { bcqeOper = Union }
                               -> BCQE { bcqeOper = Union 
                                       , bcqe0    = flipped (bcqe0 se)
                                       , bcqe1    = flipped (bcqe1 se)
                                       }
                         BCQE{} -> BSE { bseSrc = Col { cTable = [fTable]
                                                      , cCol   = [targetAlias]
                                                      , cAlias = []
                                                      , cSpecial = Nothing}
                                       , bseTrg = Col { cTable = [fTable]
                                                      , cCol   = [sourceAlias]
                                                      , cAlias = []
                                                      , cSpecial = Nothing}
                                       , bseTbl = [toTableRef se `as` fTable] -- MySQL requires you to label the "sub query" instead of just leaving it like many other implementations.
                                       , bseWhr = Nothing
                                       }
                         (BQEComment c e) 
                                -> case flipped e of
                                    BQEComment (_:c') fe -> BQEComment (c++c') fe
                                    _ -> fatal 309 "A flipped expression will always start with the comment `Flipped: ..."
    (EMp1 val c) -> let cAtt = Iden [sqlAttConcept fSpec c]
                    in BQEComment [BlockComment "case: EMp1 val c"]
                         BSE { bseSrc = Col { cTable = []
                                            , cCol   = [sqlAttConcept fSpec c]
                                            , cAlias = []
                                            , cSpecial = Nothing}
                             , bseTrg = Col { cTable = []
                                            , cCol   = [sqlAttConcept fSpec c]
                                            , cAlias = []
                                            , cSpecial = Nothing}
                             , bseTbl = [sqlConceptTable fSpec c]
                             , bseWhr = Just $ BinOp cAtt [Name "="] (singleton2SQL c val)
                             } 
    (EDcV (Sign s t))    -> 
                 let (psrc,fsrc) = (QName (name plug), QName (name att))
                                     where (plug,att) = getConceptTableInfo fSpec s
                     (ptgt,ftgt) = (QName (name plug), QName (name att))
                                     where (plug,att) = getConceptTableInfo fSpec t
                 in BQEComment [BlockComment $ "case: (EDcV (Sign s t))   V"++show (Sign s t)++""] $
                    case (s,t) of
                     (ONE, ONE) -> one
                     (_  , ONE) -> BSE { bseSrc = Col { cTable = [psrc]
                                                      , cCol   = [fsrc]
                                                      , cAlias = []
                                                      , cSpecial = Nothing}
                                       , bseTrg = theONESingleton
                                       , bseTbl = [TRSimple [psrc]]
                                       , bseWhr = Just (notNull (Iden [psrc, fsrc]))
                                                              
                                       }
                     (ONE, _  ) -> BSE { bseSrc = theONESingleton
                                       , bseTrg = Col { cTable = [ptgt]
                                                      , cCol   = [ftgt]
                                                      , cAlias = []
                                                      , cSpecial = Nothing}
                                       , bseTbl = [TRSimple [ptgt]]
                                       , bseWhr = Just (notNull (Iden [ptgt, ftgt]))
                                       }
                     _     -> BSE { bseSrc = Col { cTable = [first]
                                                 , cCol   = [fsrc]
                                                 , cAlias = []
                                                 , cSpecial = Nothing}
                                  , bseTrg = Col { cTable = [secnd]
                                                 , cCol   = [ftgt]
                                                 , cAlias = []
                                                 , cSpecial = Nothing}
                                  , bseTbl = [TRSimple [psrc] `as` first
                                             ,TRSimple [ptgt] `as` secnd]
                                  , bseWhr = Just $ conjunctSQL
                                          [notNull (Iden[first, fsrc]), notNull (Iden[secnd, ftgt])]
                                  }
                                where
                                  first = Name "fst"
                                  secnd = Name "snd"
    
    (EDcI c)             -> BQEComment [BlockComment $ "I["++name c++"]"] $
                             case c of
                              ONE ->   BSE { bseSrc = theONESingleton
                                           , bseTrg = theONESingleton
                                           , bseTbl = []
                                           , bseWhr = Nothing
                                           }
                              PlainConcept{} -> 
                                 let cAtt = Iden [sqlAttConcept fSpec c]
                                 in    BSE { bseSrc = Col { cTable = []
                                                          , cCol   = [sqlAttConcept fSpec c]
                                                          , cAlias = []
                                                          , cSpecial = Nothing}
                                           , bseTrg = Col { cTable = []
                                                          , cCol   = [sqlAttConcept fSpec c]
                                                          , cAlias = []
                                                          , cSpecial = Nothing}
                                           , bseTbl = [sqlConceptTable fSpec c]
                                           , bseWhr = Just (notNull cAtt)
                                           }


    -- EEps behaves like I. The intersects are semantically relevant, because all semantic irrelevant EEps expressions have been filtered from es.
    (EEps c sgn)     -> BQEComment [BlockComment $ "epsilon "++name c++" "++showSign sgn] $
                         case c of -- select the population of the most specific concept, which is the source.
                              ONE ->   BSE { bseSrc = theONESingleton
                                           , bseTrg = theONESingleton
                                           , bseTbl = []
                                           , bseWhr = Nothing
                                           }
                              PlainConcept{} -> 
                                 let cAtt = Iden [sqlAttConcept fSpec c]
                                 in    BSE { bseSrc = Col { cTable = []
                                                          , cCol   = [sqlAttConcept fSpec c]
                                                          , cAlias = []
                                                          , cSpecial = Nothing}
                                           , bseTrg = Col { cTable = []
                                                          , cCol   = [sqlAttConcept fSpec c]
                                                          , cAlias = []
                                                          , cSpecial = Nothing}
                                           , bseTbl = [sqlConceptTable fSpec c]
                                           , bseWhr = Just (notNull cAtt)
                                           }
    (EDcD d)             -> selectDeclaration fSpec d

    (EBrk e)             -> selectExpr fSpec e

    (ECpl e)
      -> case e of
           EDcV _        -> emptySet
           EDcI ONE      -> fatal 254 "EDcI ONE must not be seen at this place."
           EDcI c        -> BQEComment [BlockComment $ "case: ECpl (EDcI "++name c++")"]
                             BSE { bseSrc = Col { cTable = [QName "concept0"]
                                                , cCol   = [concpt]
                                                , cAlias = []
                                                , cSpecial = Nothing}
                                 , bseTrg = Col { cTable = [QName "concept1"]
                                                , cCol   = [concpt]
                                                , cAlias = []
                                                , cSpecial = Nothing}
                                 , bseTbl = [sqlConceptTable fSpec c `as` QName "concept0"
                                            ,sqlConceptTable fSpec c `as` QName "concept1"
                                            ]
                                 , bseWhr = Just (BinOp (Iden [QName "concept0", concpt])
                                                        [Name "<>"]
                                                        (Iden [QName "concept1", concpt])
                                                 )
                                 }
                             where concpt = sqlAttConcept fSpec c
           _             -> BQEComment (map BlockComment [ "case: ECpl e", "ECpl ( \""++showADL e++"\" )"])
                            BSE { bseSrc = Col { cTable = [closedWorldName]
                                               , cCol   = [sourceAlias]
                                               , cAlias = []
                                               , cSpecial = Nothing}
                                , bseTrg = Col { cTable = [closedWorldName]
                                               , cCol   = [targetAlias]
                                               , cAlias = []
                                               , cSpecial = Nothing}
                                , bseTbl = [(toTableRef . selectExpr fSpec) theClosedWorldExpression `as` closedWorldName]
                                , bseWhr = Just $ selectNotExists 
                                                    (toTableRef (selectExpr fSpec e) `as` posName) 
                                                                (Just . conjunctSQL $ 
                                                                   [BinOp (Iden [closedWorldName,sourceAlias])
                                                                          [Name "="]
                                                                          (Iden [posName,sourceAlias])
                                                                   ,BinOp (Iden [closedWorldName,targetAlias])
                                                                          [Name "="]
                                                                          (Iden [posName,targetAlias])
                                                                   ]
                                                                )
                                }
              where posName = Name "pos"
                    closedWorldName = QName ("cartesian product of "++plur (source e) ++ " and " ++ plur (target e)) 
                                       
                          where plur c = plural (fsLang fSpec) (name c)
                    theClosedWorldExpression = EDcV (sign e) 
                        
    EKl0 _               -> fatal 249 "SQL cannot create closures EKl0 (`SELECT * FROM NotExistingKl0`)"
    EKl1 _               -> fatal 249 "SQL cannot create closures EKl1 (`SELECT * FROM NotExistingKl1`)"
    (EDif (EDcV _,x)) -> BQEComment [BlockComment $ "case: EDif V x"++"EDif V ( \""++showADL x++"\" ) \""++show (sign expr)++"\""]
                                    (selectExpr fSpec (notCpl x))
-- The following definitions express code generation of the remaining cases in terms of the previously defined generators.
-- As a result of this way of working, code generated for =, |-, -, !, *, \, and / may not be efficient, but at least it is correct.
    EEqu (l,r)
      -> BQEComment [BlockComment $ "case: EEqu (l,r) "++showADL expr++" ("++show (sign expr)++")"] $
         selectExpr fSpec ((ECpl l .\/. r) ./\. (ECpl r .\/. l))
    EInc (l,r)
      -> BQEComment [BlockComment $ "case: EInc (l,r) "++showADL expr++" ("++show (sign expr)++")"] $
         selectExpr fSpec (ECpl l .\/. r)
    EDif (l,r)
      -> BQEComment [BlockComment $ "case: EDif (l,r) "++showADL expr++" ("++show (sign expr)++")"] $
         selectExpr fSpec (l ./\. ECpl r)
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
                  = BSE { bseSrc = Col { cTable = [resLeft]
                                       , cCol   = [mainSrc]
                                       , cAlias = []
                                       , cSpecial = Nothing}
                        , bseTrg = Col { cTable = [resRight]
                                       , cCol   = [mainTgt]
                                       , cAlias = []
                                       , cSpecial = Nothing}
                        , bseTbl = [ sqlConceptTable fSpec (target l) `as` resLeft
                                   , sqlConceptTable fSpec (target r) `as` resRight]
                        , bseWhr = Just . VEComment [BlockComment $ "Left hand side: "++showADL l] $ 
                                          selectNotExists 
                                            (lCode `as` lhs)
                                            ( Just $ conjunctSQL
                                                [BinOp (Iden [resLeft,mainSrc])
                                                       [Name "="]
                                                       (Iden [lhs,targetAlias])
                                                ,VEComment [BlockComment $ "Right hand side: "++showADL r] $ 
                                                 selectNotExists 
                                                   (rCode `as` rhs)
                                                   ( Just $ conjunctSQL 
                                                      [BinOp (Iden [rhs,sourceAlias])
                                                             [Name "="]
                                                             (Iden [lhs,sourceAlias])
                                                      ,BinOp (Iden [rhs,targetAlias])
                                                             [Name "="]
                                                             (Iden [resRight,mainTgt])
                                                      ]
                                                   )
                                                ]
                                            ) 
                        }
             mainSrc = (sqlAttConcept fSpec.target) l  -- Note: this 'target' is not an error!!! It is part of the definition of right residu
             mainTgt = (sqlAttConcept fSpec.target) r
             resLeft  = Name "RResLeft"
             resRight = Name "RResRight"
             lhs  = Name "lhs"
             rhs  = Name "rhs"
             lCode = toTableRef $ selectExpr fSpec l -- selectExprInFROM fSpec sourceAlias targetAlias l
             rCode = toTableRef $ selectExpr fSpec r -- selectExprInFROM fSpec sourceAlias targetAlias r
         in BQEComment [BlockComment $ "case: ERrs (l,r)"++showADL expr++" ("++show (sign expr)++")"]
                         rResiduClause
    ELrs (l,r)
      -> BQEComment [BlockComment $ "case: ELrs (l,r)"++showADL expr++" ("++show (sign expr)++")"] $
         selectExpr fSpec (EFlp (flp r .\. flp l))
    EDia (l,r)
      -> BQEComment [BlockComment $ "case: EDia (l,r)"++showADL expr++" ("++show (sign expr)++")"] $
         selectExpr fSpec ((flp l .\. r) ./\. (l ./. flp r))
    ERad{}
      -> BQEComment [BlockComment $ "case: ERad (l,r)"++showADL expr++" ("++show (sign expr)++")"] $
        selectExpr fSpec (deMorganERad expr)
    EPrd (l,r)
     -> let v = EDcV (Sign (target l) (source r))
        in BQEComment [BlockComment $ "case: EPrd (l,r)"++showADL expr++" ("++show (sign expr)++")"] $
           selectExpr fSpec (foldr1 (.:.) [l,v,r])

  where
   singleton2SQL :: A_Concept -> PSingleton -> ValueExpr
   singleton2SQL cpt singleton = 
     atomVal2InSQL (safePSingleton2AAtomVal (fcontextInfo fSpec) cpt singleton)

atomVal2InSQL :: AAtomValue -> ValueExpr
atomVal2InSQL val =
 case val of 
   AAVString{}         -> StringLit (aavstr val)
   AAVInteger _ int    -> NumLit (show int)
   AAVFloat _ d        -> NumLit (show d)
   AAVBoolean _ b      -> NumLit $ if b then "1" else "0"
   _                   -> fatal 621 $ "Building a query with a literal `"
                                    ++show (aavtyp val)++"` is not implemented (jet?)."
toTableRef :: BinQueryExpr -> TableRef
toTableRef = TRQueryExpr . toSQL
     

selectDeclaration :: FSpec -> Declaration -> BinQueryExpr
selectDeclaration fSpec dcl =
  case dcl of
    Sgn{}  -> leafCode (getDeclarationTableInfo fSpec dcl)
    Isn{}  -> let (plug, c) = getConceptTableInfo fSpec (detyp dcl)
              in leafCode (plug, c, c)
    Vs sgn
     | source sgn == ONE -> fatal 468 "ONE is not expected at this place"
     | target sgn == ONE -> fatal 469 "ONE is not expected at this place"
     | otherwise
           -> BSE { bseSrc = Col { cTable = [Name "vfst"]
                                 , cCol   = [sqlAttConcept fSpec (source sgn)]
                                 , cAlias = []
                                 , cSpecial = Nothing}
                  , bseTrg = Col { cTable = [Name "vsnd"]
                                 , cCol   = [sqlAttConcept fSpec (target sgn)]
                                 , cAlias = []
                                 , cSpecial = Nothing}
                  , bseTbl = [sqlConceptTable fSpec (source sgn) `as` Name "vfst"
                             ,sqlConceptTable fSpec (target sgn) `as` Name "vsnd"]
                  , bseWhr = Just . conjunctSQL . map notNull $
                               [ Iden [Name "vfst", sqlAttConcept fSpec (source sgn)]
                               , Iden [Name "vsnd", sqlAttConcept fSpec (target sgn)]]
                  }
   where
     leafCode :: (PlugSQL,SqlAttribute,SqlAttribute) -> BinQueryExpr
     leafCode (plug,s,t) 
         = BSE { bseSrc = Col { cTable = []
                              , cCol   = [QName (name s)]
                              , cAlias = []
                              , cSpecial = Nothing}
               , bseTrg = Col { cTable = []
                              , cCol   = [QName (name t)]
                              , cAlias = []
                              , cSpecial = Nothing}
               , bseTbl = [TRSimple [QName (name plug)]]
               , bseWhr = Just . conjunctSQL . map notNull $
                                [Iden [QName (name c)] | c<-nub [s,t]]
               }

isNotIn :: ValueExpr -> QueryExpr -> ValueExpr
isNotIn value = In False value . InQueryExpr 
-- | select only the source of a binary expression
selectSource :: BinQueryExpr -> QueryExpr
selectSource = selectSorT sourceAlias

selectSorT :: Name -> BinQueryExpr -> QueryExpr
selectSorT att binExp =
     Select { qeSetQuantifier = SQDefault
            , qeSelectList    = [(Iden [att],Nothing)]   
            , qeFrom          = [TRQueryExpr (toSQL binExp) `as` att]
            , qeWhere         = Nothing
            , qeGroupBy       = []
            , qeHaving        = Nothing
            , qeOrderBy       = []
            , qeOffset        = Nothing
            , qeFetchFirst    = Nothing
            } 

selectExists, selectNotExists
     :: TableRef        -- ^ tables
     -> Maybe ValueExpr -- ^ the (optional) WHERE clause
     -> ValueExpr
selectNotExists tbl whr = PrefixOp [Name "NOT"] $ selectExists tbl whr
selectExists tbl whr = 
  SubQueryExpr SqExists
     Select { qeSetQuantifier = SQDefault
            , qeSelectList    = [(Star,Nothing)]   
            , qeFrom          = [case tbl of 
                                   TRAlias{} -> tbl
                                   _  -> tbl `as` Name "aDummyName" -- MySQL requires you to label the "sub query" instead of just leaving it like many other implementations.
                                ]
            , qeWhere         = whr
            , qeGroupBy       = []
            , qeHaving        = Nothing
            , qeOrderBy       = []
            , qeOffset        = Nothing
            , qeFetchFirst    = Nothing
            }

-- | a (local) data structure to hold SQL info for binary expressions
data BinQueryExpr = BSE  { bseSrc :: Col
                         , bseTrg :: Col
                         , bseTbl :: [TableRef]      -- ^ tables
                         , bseWhr :: Maybe ValueExpr -- ^ the (optional) WHERE clause
                         }
                  | BCQE { bcqeOper :: CombineOp     -- ^ The combine operator 
                         , bcqe0 ::    BinQueryExpr  -- ^ Left  expression
                         , bcqe1 ::    BinQueryExpr  -- ^ Right expression
                         }
                  | BQEComment [Comment] BinQueryExpr
data Col = Col { cTable :: [Name]
               , cCol   :: [Name]
               , cAlias :: [Name]
               , cSpecial :: Maybe ValueExpr 
               }  
col2ValueExpr :: Col -> ValueExpr
col2ValueExpr col = 
  case cSpecial col of 
    Nothing -> Iden x
    Just ve  
      | null x    -> ve
      | otherwise -> fatal 826 "cSpecial should only be used in special cases, e.g. with ONE"
  where
    x = cTable col++cCol col++cAlias col
stripComment :: BinQueryExpr -> BinQueryExpr
stripComment bqe 
  = case bqe of
       BSE{} -> BSE { bseSrc = bseSrc bqe
                    , bseTrg = bseTrg bqe
                    , bseTbl = map stripCommentTableRef . bseTbl $ bqe
                    , bseWhr = bseWhr bqe
                    }
       BCQE{} -> BCQE { bcqeOper = bcqeOper bqe
                      , bcqe0    = stripComment (bcqe0 bqe)
                      , bcqe1    = stripComment (bcqe1 bqe)
                      }
       BQEComment _ x -> stripComment x 
stripCommentTableRef :: TableRef -> TableRef
stripCommentTableRef tr =
  case tr of
    TRSimple _ -> tr 
    TRJoin tr1 b jt tr2 x -> TRJoin (stripCommentTableRef tr1) b jt (stripCommentTableRef tr2) x 
    TRParens tr1 -> TRParens (stripCommentTableRef tr1)
    TRAlias tr1 x -> TRAlias (stripCommentTableRef tr1) x
    TRQueryExpr qe -> TRQueryExpr (stripCommentQueryExpr qe)
    TRFunction _ _ -> tr 
    TRLateral tr1 -> TRLateral (stripCommentTableRef tr1)
stripCommentQueryExpr :: QueryExpr -> QueryExpr
stripCommentQueryExpr qe = 
   case qe of
     QEComment _ qe' -> stripCommentQueryExpr qe'
     _               -> qe
toSQL :: BinQueryExpr -> QueryExpr
toSQL bqe 
 = case bqe of
    BSE{} -> Select { qeSetQuantifier = Distinct
                    , qeSelectList    = [ (col2ValueExpr (bseSrc bqe), Just sourceAlias)
                                        , (col2ValueExpr (bseTrg bqe), Just targetAlias)]
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
    (BQEComment c (BQEComment c' e)) -> toSQL $ BQEComment (c++c') e
    (BQEComment c e) -> QEComment c (toSQL e)
    
sqlConceptTable :: FSpec -> A_Concept -> TableRef
sqlConceptTable fSpec a = TRSimple [sqlConcept fSpec a]

-- sqlConcept gives the name of the plug that contains all atoms of A_Concept c.
sqlConcept :: FSpec -> A_Concept -> Name
sqlConcept fSpec = QName . name . sqlConceptPlug fSpec
-- sqlConcept yields the plug that contains all atoms of A_Concept c. Since there may be more of them, the first one is returned.
sqlConceptPlug :: FSpec -> A_Concept -> PlugSQL
sqlConceptPlug fSpec c 
             = case lookupCpt fSpec c of
                 []   ->  fatal 585 $ "A_Concept \""++show c++"\" does not occur in fSpec." 
                 (plug,_):_ -> plug

sqlAttConcept :: FSpec -> A_Concept -> Name
sqlAttConcept fSpec c | c==ONE = QName "ONE"
                      | otherwise
             = if null cs then fatal 594 $ "A_Concept \""++show c++"\" does not occur in its plug in fSpec \""++name fSpec++"\"" else
               QName (head cs)
               where cs = [name f |f<-plugAttributes (sqlConceptPlug fSpec c), c'<-concs f,c==c']


stringOfName :: Name -> String
stringOfName (Name s)   =  s
stringOfName (QName s)  =  s
stringOfName (UQName s) =  s
stringOfName _          = fatal 659 "This kind of a Name wasn't used before in Ampersand. "

conjunctSQL :: [ValueExpr] -> ValueExpr
conjunctSQL [] = fatal 57 "nothing to `and`."
conjunctSQL [ve] = bracketsSQL ve
conjunctSQL (ve:ves) = BinOp (bracketsSQL ve) [Name "and"] (conjunctSQL ves)

disjunctSQL :: [ValueExpr] -> ValueExpr
disjunctSQL [] = fatal 57 "nothing to `or`."
disjunctSQL [ve] = bracketsSQL ve
disjunctSQL (ve:ves) = BinOp (bracketsSQL ve) [Name "or"] (conjunctSQL ves)

bracketsSQL :: ValueExpr -> ValueExpr
bracketsSQL = Parens

as :: TableRef -> Name -> TableRef
as ve a = -- TRAlias ve (Alias a Nothing)
  case ve of 
    TRSimple [n] -> if stringOfName n == stringOfName a then withoutAlias else withAlias
    _            -> withAlias
 where
   withoutAlias = ve
   withAlias = TRAlias ve (Alias a Nothing)
    
notNull :: ValueExpr -> ValueExpr
notNull = PostfixOp [Name "is not null"]
isNull  :: ValueExpr -> ValueExpr
isNull = PostfixOp [Name "is null"]
emptySet :: BinQueryExpr
emptySet = BQEComment [BlockComment "this will quaranteed return 0 rows:"]
           BSE { 
               -- select 1 as src, 1 as trg from (select 1) dummy where false
                 bseSrc = Col { cTable = [nothing]
                              , cCol   = [a]
                              , cAlias = []
                              , cSpecial = Nothing}
               , bseTrg = Col { cTable = [nothing]
                              , cCol   = [a]
                              , cAlias = []
                              , cSpecial = Nothing}
               , bseTbl = [TRQueryExpr (QEComment [BlockComment "Select nothing..."] 
                                        Select { qeSetQuantifier = SQDefault
                                               , qeSelectList = [(NumLit "1", Just a)]
                                               , qeFrom = []
                                               , qeWhere = Nothing
                                               , qeGroupBy = []
                                               , qeHaving = Nothing
                                               , qeOrderBy = []
                                               , qeOffset = Nothing
                                               , qeFetchFirst = Nothing
                                               }
                                       ) `as` nothing]
               , bseWhr = Just (BinOp (Iden [a]) [Name "<>"] (NumLit "1"))
               }
            where a = Name "a"
                  nothing = Name "nothing"


one :: BinQueryExpr
one = BQEComment [BlockComment "Just ONE"]
      BSE {  -- select distinct 1 as src, 1 as tgt from (select 1) as a
            bseSrc = theONESingleton
          , bseTrg = theONESingleton
          , bseTbl = [ TRQueryExpr Select { qeSetQuantifier = SQDefault
                                          , qeSelectList = [(NumLit "1", Nothing)]
                                          , qeFrom = []
                                          , qeWhere = Nothing
                                          , qeGroupBy = []
                                          , qeHaving = Nothing
                                          , qeOrderBy = []
                                          , qeOffset = Nothing
                                          , qeFetchFirst = Nothing
                                          } `as` Name "ONE" ]
          , bseWhr = Nothing
          }

theDialect :: Dialect 
theDialect = MySQL  -- maybe in the future other dialects will be supported. This depends on package `simple-sql-parser`

broadQuery :: FSpec -> ObjectDef -> QueryExpr
broadQuery fSpec obj = 
  -- The idea is to fetch all columns that are available in the same Plug as the conceptTable of the 
  -- target of the object's context-expression. This dramatically reduces the number of queries required,
  -- and hence will boost performance at runtime. 
  --
  -- So here is the plan:. 
  --   a) The ObjectDef has a contextExpression. A BinQueryExpr can be built using selectExpr. 
  --   b) For all expressions in the subinterface, when they are in the same table as the conceptTable of 
  --     the target of the contextExpression, we want to fetch them in this single query.
  --   c) We know the table that is used to get the tgt of the result of a) This could be some intermediate table!
  --   d) we know the conceptTable of the target concept of the expression. 
  --There are the following cases to consider:
  -- 1) There is no subinterface, or the subinterface contains no expressions to consider
  -- 2) The only expression to consider is I[<target of contextExpression>]
  -- 3) The plug used to fetch the contextExpression is the same plug as the conceptTable of the target of that expression.
  -- 4) None of the above
  case objmsub obj of
   Nothing                -> toSQL baseBinExpr
   Just InterfaceRef{}    -> toSQL baseBinExpr
   Just Box{siObjs=sObjs} -> 
                    case filter isInBroadQuery sObjs of
                       [] -> toSQL baseBinExpr
                       xs -> extendWithCols xs baseBinExpr
       --                xs -> extendWithCols xs (toSQL baseBinExpr)
 where  
  baseBinExpr = getBinQueryExprPlaceholder fSpec . objctx $ obj

  extendWithCols :: [ObjectDef] -> BinQueryExpr -> QueryExpr
  extendWithCols objs bqe 
    | null objs = plainQE
    | otherwise =
        case bqe of
          BSE{}  -> newSelect (newSelectList,newFrom,newWhere)
           where
            (newSelectList,newFrom,newWhere) =
              case qeFrom plainQE of
                [TRSimple [n]] 
                  -> if n == sqlConcept fSpec tableCpt
                     then ( qeSelectList plainQE ++ map (makeCol Nothing) objs
                          , qeFrom plainQE
                          , qeWhere plainQE
                          )
                     else subThings
                _ -> subThings
          BCQE{} -> newSelect subThings
          BQEComment _ x -> extendWithCols objs x 
   where 
     newSelect (sl,f,w) =
        Select { qeSetQuantifier = Distinct
               , qeSelectList    = sl
               , qeFrom          = f
               , qeWhere         = w
               , qeGroupBy       = []
               , qeHaving        = Nothing
               , qeOrderBy       = []
               , qeOffset        = Nothing
               , qeFetchFirst    = Nothing
               }
     plainQE = toSQL bqe
     makeCol :: Maybe Name -> ObjectDef -> (ValueExpr, Maybe Name)
     makeCol tableName col =
       case attThatisInTableOf (target . objctx $ obj) col of
            Nothing  -> fatal 1104 $ "this is unexpected behaviour. "++show col  
            Just att -> ( Iden ( case tableName of
                                   Nothing -> [QName (name att)]
                                   Just tab -> [tab,QName (name att)]
                               )
                        , Just ( QName $ -- The name is not sufficient for two reasons:
                                         --   1) the columname must be unique. For that reason, it is prefixed:
                                         "ifc_"++ 
                                         --   2) It must be injective. Because SQL deletes trailing spaces,
                                         --      we have to cope with that:
                                         escapeIdentifier (name col)
                               )
                        )
     subThings :: ( [(ValueExpr, Maybe Name)]
                  , [TableRef]
                  , Maybe ValueExpr
                  )
     subThings = ( [ (Iden [org,sourceAlias] , Just sourceAlias)
                   , (Iden [org,targetAlias] , Just targetAlias)
                   ]++ map (makeCol . Just $ ct) objs
                 , [ TRQueryExpr plainQE `as` org
                   , sqlConceptTable fSpec tableCpt `as` ct
                   ]
                 , Just (BinOp (Iden [org, targetAlias])
                                  [Name "="]
                               (Iden [ct, sqlAttConcept fSpec tableCpt])
                        )
                 )
       where 
         org = Name "org"
         ct  = Name "cptTbl"
     tableCpt = source . objctx . head $ objs

  isInBroadQuery :: ObjectDef -> Bool
  isInBroadQuery sObj = 
     (isUni . objctx $ sObj) && 
     (isJust . attThatisInTableOf (target . objctx $ obj) $ sObj)
      
  attThatisInTableOf :: A_Concept -> ObjectDef -> Maybe SqlAttribute
  attThatisInTableOf cpt od = 
          case theDcl of
            Nothing -> Nothing
            Just (p,att) -> if plug == p
                            then Just att
                            else Nothing
         where
            (plug, _ ) = getConceptTableInfo fSpec cpt
            theDcl :: Maybe (PlugSQL, SqlAttribute)
            theDcl = case objctx od of
                       EFlp (EDcD d) -> let (p, s, _) = getDeclarationTableInfo fSpec d
                                        in Just (p, s)
                       EDcD d        -> let (p, _, t) = getDeclarationTableInfo fSpec d
                                        in Just (p, t)
                       EDcI c        -> Just $ getConceptTableInfo fSpec c
                       _             -> Nothing

theONESingleton :: Col
theONESingleton = Col { cTable = []
                      , cCol   = []
                      , cAlias = []
                      , cSpecial = Just $ NumLit "1"
                      }

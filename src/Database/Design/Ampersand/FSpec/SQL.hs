module Database.Design.Ampersand.FSpec.SQL 
  (selectExpr,QueryExpr)
where


import Language.SQL.SimpleSQL.Syntax
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Classes.ConceptStructure
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.ADL1.Expression
import Database.Design.Ampersand.Classes.Relational
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.FSpec.FSpecAux
import Database.Design.Ampersand.FSpec.ShowADL

import Data.Char
import Data.List
import Data.Maybe

fatal :: Int -> String -> a
fatal = fatalMsg "RelBinGenSQL"


selectExpr :: FSpec    -- current context
        -> Name        -- SQL name of the source of this expression, as assigned by the environment
        -> Name        -- SQL name of the target of this expression, as assigned by the environment
        -> Expression  -- expression to be translated
        -> QueryExpr   -- resulting SQL expression
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
    EIsc{} -> let lst'       = exprIsc2list expr
                  sgn        = sign expr
                  src'       = sqlExprSrc fSpec firstTerm
                  trg'       = sqlExprTgt fSpec firstTerm
                  firstTerm  = head posTms  -- always defined, because length posTms>0 (ensured in definition of posTms)
                  mp1Tm      = take 1 [t | t@EMp1{}<-lst']++[t | t<-lst', [EMp1{},EDcV _,EMp1{}] <- [exprCps2list t]]
                  lst        = [t |t<-lst', t `notElem` mp1Tm]
                  posTms     = if null posTms' then map notCpl (take 1 negTms') else posTms' -- we take a term out of negTms' if we have to, to ensure length posTms>0
                  negTms     = if null posTms' then tail negTms' else negTms' -- if the first term is in posTms', don't calculate it here
                  posTms'    = [t | t<-lst, isPos t && not (isIdent t)]++[t | t<-lst, isPos t && isIdent t] -- the code to calculate I is better if it is not the first term
                  negTms'    = [notCpl t | t<-lst, isNeg t && isIdent t]++[notCpl t | t<-lst, isNeg t && not (isIdent t)] -- should a negTerm become a posTerm (for reasons described above), it can best be an -I.
                  exprbracs  = [TRAlias (TRQueryExpr (selectExprInFROM fSpec src'' trg'' l)) (Alias (Name ("isect"++show n)) Nothing)
                               | (n,l)<-zip [(0::Int)..] posTms
                               , let src'' = sqlExprSrc fSpec l
                               , let trg'' =noCollide' [src''] (sqlExprTgt fSpec l)
                               ]
                  wherecl :: Maybe ValueExpr
                  wherecl   = Just . conjunctSQL . concat $
                               ([if isIdent l
                                 then -- this is the code to calculate ../\I. The code below will work, but is longer
                                      [BinOp (Iden [Name "isect0", src'])
                                             [Name "="]
                                             (Iden [Name "isect0", trg'])
                                      ]
                                 else [BinOp (Iden [Name "isect0", src'])
                                             [Name "="]
                                             (Iden [Name $ "isect"++show n, src''])
                                      ,BinOp (Iden [Name "isect0",trg'])
                                             [Name "="]
                                             (Iden [Name $ "isect"++show n, trg''])
                                      ]
                                | (n,l)<-tail (zip [(0::Int)..] posTms) -- not empty because of definition of posTms
                                , let src''=sqlExprSrc fSpec l
                                , let trg''=noCollide' [src''] (sqlExprTgt fSpec l)
                                ]++
                                [ [ BinOp (Iden [Name "isect0",src'])
                                          [Name "="]
                                          (sqlAtomQuote atom)
                                  , BinOp (Iden [Name "isect0",trg'])
                                          [Name "="]
                                          (sqlAtomQuote atom)
                                  ]
                                | EMp1 atom _ <- mp1Tm
                                ]++
                                [ [ BinOp (Iden [Name "isect0",src'])
                                          [Name "="]
                                          (sqlAtomQuote atom1) -- source and target are unequal
                                  , BinOp (Iden [Name "isect0",trg'])
                                          [Name "="]
                                          (sqlAtomQuote atom2) -- source and target are unequal
                                  ]
                                | t@ECps{} <- mp1Tm, [EMp1 atom1 _, EDcV _, EMp1 atom2 _]<-[exprCps2list t]
                                ]++
                                [if isIdent l
                                 then -- this is the code to calculate ../\I. The code below will work, but is longer
                                      [BinOp (Iden [Name "isect0", src'])
                                             [Name "="]
                                             (Iden [Name "isect0", trg'])
                                      ]
                                 else [PrefixOp [Name "NOT"] 
                                        ( SubQueryExpr SqExists
                                            ( selectExists' 
                                                 [TRAlias 
                                                    (TRQueryExpr (selectExprInFROM fSpec src'' trg'' l)
                                                    ) (Alias (Name "cp") Nothing)]
                                                 (Just . conjunctSQL $
                                                   [ BinOp (Iden [Name "isect0",src'])
                                                           [Name "="]
                                                           (Iden [Name "cp",src''])
                                                   , BinOp (Iden [Name "isect0",trg'])
                                                           [Name "="]
                                                           (Iden [Name "cp",trg''])
                                                   ]
                                                 )
                                            )
                                        )
                                      ]
                                | (_,l)<-zip [(0::Int)..] negTms
                                , let src''=sqlExprSrc fSpec l
                                , let trg''=noCollide' [src''] (sqlExprTgt fSpec l)
                                ]++
                                [nub (map notNull [src',trg'])]
                               )
                  notNull :: Name -> ValueExpr
                  notNull tm = PostfixOp [Name "IS NOT NULL"] (Iden [tm])                         

              in case lst' of
{- The story:
 This alternative of selectExpr compiles a conjunction of at least two subexpressions (code: EIsc lst'@(_:_:_))
 For now, we explain only the otherwise clause (code: selectGeneric i ("isect0", ...)
 Suppose we have an expression, plaats~;locatie/\-hoofdplaats~/\-neven
 It has two negative terms (code: negTms), which are (in this example): hoofdplaats~ and neven,
 It has one positive term (code posTms), which is plaats~;locatie.
 In the resulting SQL code, the first term of posTms is taken as the basis.
 All other positive terms are added as EXISTS subexpressions in the WHERE clause and
 all negative terms are added as NOT EXISTS subexpressions in the WHERE clause.
 So our example will look like:
                    SELECT DISTINCT isect0.`A`, isect0.`B`
                     FROM ( SELECT bladibla) AS isect0       representing plaats~;locatie
                    WHERE NOT EXISTS (SELECT foo)            representing hoofdplaats~
                      AND NOT EXISTS (SELECT bar)            representing neven
-}
                  (_:_:_) -> sqlcomment ("case: (EIsc lst'@(_:_:_))"++showADL expr++" ("++show sgn++")") $
                             selectGeneric (Iden [Name "isect0",src'],Just src)
                                           (Iden [Name "isect0",trg'],Just trg)            
                                           exprbracs
                                           wherecl
                  _       -> fatal 123 "A list shorter than 2 cannot occur in the query at all! If it does, we have made a mistake earlier."
    EUni{} -> sqlcomment ("case: EUni (l,r)"++showADL expr++" ("++show (sign expr)++")") $
              combineQueryExprs Union [selectExpr fSpec src trg e | e<-exprUni2list expr]
    ECps (EDcV (Sign ONE _), ECpl expr')
     -> case target expr' of
         ONE -> fatal 137 "sqlConcept not defined for ONE"
         _   -> let src'  = sqlAttConcept fSpec (source expr')
                    trg'  = sqlAttConcept fSpec (target expr')
                    trg2  = noCollide' [src'] (sqlAttConcept fSpec (target expr'))
                in sqlcomment ("case:  ECps (EDcV (Sign ONE _), ECpl expr') "++showADL expr) $
                   selectGeneric (Iden [Name "1"], Just src)
                                 (Iden [trg'    ], Just trg)
                                 [TRAlias (TRQueryExpr (sqlConcept fSpec (target expr'))
                                          ) (Alias (Name "allAtoms") Nothing) ]
                                 (Just $ PrefixOp [Name "NOT"] 
                                    ( SubQueryExpr SqExists
                                       ( selectExists' 
                                           [TRAlias 
                                              (TRQueryExpr (selectExprInFROM fSpec src' trg' expr')
                                              ) (Alias (Name "complemented") Nothing)
                                           ] (Just (BinOp (Iden [Name "complemented",trg2])
                                                          [Name "="]
                                                          (Iden [Name "allAtoms",trg'])
                                                   )
                                             )
                                       )
                                    )
                                 )
    ECps{}  ->
       case exprCps2list expr of
          (EDcV (Sign ONE _):fs@(_:_))
             -> let expr' = foldr1 (.:.) fs
                    src'  = noCollide' [trg'] (sqlExprSrc fSpec expr')
                    trg'  = sqlExprTgt fSpec expr'
                in sqlcomment ("case:  (EDcV (Sign ONE _): fs@(_:_))"++showADL expr) $
                   selectGeneric (Iden [Name "1"], Just src)
                                 (Iden [Name "fst",trg'    ], Just trg)
                                 [TRAlias
                                    (TRQueryExpr (selectExprInFROM fSpec src' trg' expr')
                                    ) (Alias (Name "fst") Nothing)
                                 ] 
                                 (Just (PostfixOp [Name "is not null"] (Iden [Name "fst", trg'])))
          (s1@EMp1{}: s2@(EDcV _): s3@EMp1{}: fx@(_:_)) -- to make more use of the thing below
             -> sqlcomment ("case:  (s1@EMp1{}: s2@(EDcV _): s3@EMp1{}: fx@(_:_))"
                            ++showADL expr)
                (selectExpr fSpec src trg (foldr1 (.:.) [s1,s2,s3] .:. foldr1 (.:.) fx))
          [EMp1 atomSrc _, EDcV _, EMp1 atomTgt _]-- this will occur quite often because of doSubsExpr
             -> sqlcomment ("case:  [EMp1 atomSrc _, EDcV _, EMp1 atomTgt _]"++showADL expr) $
                Select { qeSetQuantifier = SQDefault
                       , qeSelectList    = [ (sqlAtomQuote atomSrc, Just src)
                                           , (sqlAtomQuote atomTgt, Just trg)
                                           ]
                       , qeFrom          = []
                       , qeWhere         = Nothing
                       , qeGroupBy       = []
                       , qeHaving        = Nothing
                       , qeOrderBy       = []
                       , qeOffset        = Nothing
                       , qeFetchFirst    = Nothing
                       }
          (e@(EMp1 atom _):f:fx)
             -> let expr' = foldr1 (.:.) (f:fx)
                    src' = sqlExprSrc fSpec e
                    trg' = noCollide' [src'] (sqlExprTgt fSpec expr')
                in sqlcomment ("case:  (EMp1{}: f: fx)"++showADL expr) $
                   selectGeneric (Iden [Name "fst",src'], Just src)
                                 (Iden [Name "fst",trg'], Just trg)
                                 [TRAlias
                                    (TRQueryExpr (selectExprInFROM fSpec src' trg' expr')
                                    ) (Alias (Name "fst") Nothing)
                                 ]
                                 (Just (BinOp (Iden [Name "fst",src'])
                                              [Name "="]
                                              (sqlAtomQuote atom)))
          (e:EDcV _:f:fx) -- prevent calculating V in this case
             | src==trg && not (isProp e) -> fatal 172 $ "selectExpr 2 src and trg are equal ("++stringOfName src++") in "++showADL e
             | otherwise -> let expr' = foldr1 (.:.) (f:fx)
                                src' = sqlExprSrc fSpec e
                                mid' = noCollide' [src'] (sqlExprTgt fSpec e)
                                mid2'= sqlExprSrc fSpec f
                                trg' = noCollide' [mid2'] (sqlExprTgt fSpec expr')
                            in sqlcomment ("case:  (e:ERel (EDcV _) _:f:fx)"++showADL expr) $
                               selectGeneric (Iden [Name "fst",src'], Just src)
                                             (Iden [Name "snd",trg'], Just trg)
                                             [TRAlias
                                                (TRQueryExpr (selectExprInFROM fSpec src' mid' e)
                                                ) (Alias (Name "fst") Nothing)
                                             ,TRAlias
                                                (TRQueryExpr (selectExprInFROM fSpec mid2' trg' expr')
                                                ) (Alias (Name "snd") Nothing)
                                             ]
                                             Nothing
          [] -> fatal 190 ("impossible outcome of exprCps2list: "++showADL expr)
          [e]-> selectExpr fSpec src trg e -- Even though this case cannot occur, it safeguards that there are two or more elements in exprCps2list expr in the remainder of this code.
{-  We can treat the ECps expressions as poles-and-fences, with at least two fences.
    Imagine subexpressions as "fences". The source and target of a "fence" are the "poles" between which that "fence" is mounted.
    In this metaphor, we create the FROM-clause directly from the "fences", and the WHERE-clause from the "poles" between "fences".
    The "outer poles" correspond to the source and target of the entire expression.
    To prevent name conflicts in SQL, each subexpression is aliased in SQL by the name "ECps<n>".
SELECT DISTINCT ECps0.`C` AS `SrcC`, ECps0.`A` AS `TgtA`
FROM `r` AS ECps0, `A`AS ECps2
WHERE ECps0.`A`<>ECps2.`A
-}
          es -> let selects = [mainSrc,mainTgt]
                      where
                        mainSrc = (Iden [Name ("ECps"++show n),sqlSrc], Just src)
                                  where
                                    (n,_,sqlSrc,_) = head fenceExprs
                        mainTgt = (Iden [Name ("ECps"++show n),sqlTgt], Just trg)
                                  where
                                    (n,_,_,sqlTgt) = last fenceExprs
                    froms = [ TRAlias
                               (TRQueryExpr lSQLexp
                               ) (Alias (Name ("ECps"++show n)) Nothing) 
                           | (n,lSQLexp,_,_) <- fenceExprs ]
                    wheres = Just . conjunctSQL $
                                [ BinOp (Iden [Name ("ECps"++show n), lSQLtrg])
                                        [Name (if m==n+1 then "=" else "<>")]
                                        (Iden [Name ("ECps"++show m), rSQLsrc])
                                | ((n,_,_,lSQLtrg),(m,_,rSQLsrc,_))<-zip (init fenceExprs) (tail fenceExprs)
                                ]++
                                [notNull mainSrc,notNull mainTgt]
                      where
                        mainSrc = Iden [Name ("ECps"++show n), sqlSrc]
                                  where (n,_,sqlSrc,_) = head fenceExprs
                        mainTgt = Iden [Name ("ECps"++show n), sqlTgt]
                                  where (n,_,_,sqlTgt) = last fenceExprs
                        notNull :: ValueExpr -> ValueExpr
                        notNull ve = PostfixOp [Name "IS NOT NULL"] ve                         
                    -- fenceExprs lists the expressions and their SQL-fragments.
                    -- In the poles-and-fences metaphor, they serve as the fences between the poles.
                    fenceExprs :: [(Int,QueryExpr,Name,Name)]
                    fenceExprs = -- the first part introduces a 'pole' that consists of the source concept.
                                 [ ( length es
                                   , sqlConcept fSpec c
                                   , cAtt
                                   , cAtt
                                   )
                                 | length es>1, e@(ECpl (EDcI c)) <- [head es]
                                 , let cAtt = (sqlAttConcept fSpec.source) e
                                 ]++
                                 -- the second part is the main part, which does most of the work (parts 1 and 3 are rarely used)
                                 [ ( n                              -- the serial number of this fence (in between poles n and n+1)
                                   , selectExprInFROM fSpec srcAtt tgtAtt e
                                   , srcAtt
                                   , tgtAtt
                                   )
                                 | (n, e) <- zip [(0::Int)..] es
                                 , case e of
                                    ECpl (EDcI _) -> False
                                    EDcI _        -> False  -- if the normalizer works correctly, this case will never be visited.
                                    _             -> True
                                 , let srcAtt = sqlExprSrc fSpec e
                                 , let tgtAtt = noCollide' [srcAtt] (sqlExprTgt fSpec e)
                                 ]++
                                 -- the third part introduces a 'pole' that consists of the target concept.
                                 [ ( length es
                                   , sqlConcept fSpec c
                                   , cAtt
                                   , cAtt
                                   )
                                 | length es>1, e@(ECpl (EDcI c)) <- [last es]
                                 , let cAtt = (sqlAttConcept fSpec.target) e
                                 ]
                in sqlcomment ("case: (ECps es), with two or more elements in es."++showADL expr) $
                   Select { qeSetQuantifier = Distinct
                          , qeSelectList    = selects
                          , qeFrom          = froms
                          , qeWhere         = wheres
                          , qeGroupBy       = []
                          , qeHaving        = Nothing
                          , qeOrderBy       = []
                          , qeOffset        = Nothing
                          , qeFetchFirst    = Nothing
                          }
    (EFlp x) -> sqlcomment "case: EFlp x." $
                 selectExpr fSpec trg src x
    (EMp1 atom _) -> sqlcomment "case: EMp1 atom."
                   Select { qeSetQuantifier = Distinct
                          , qeSelectList    = [ (sqlAtomQuote atom , Just src)
                                              , (sqlAtomQuote atom , Just trg)
                                              ]
                          , qeFrom          = []
                          , qeWhere         = Nothing
                          , qeGroupBy       = []
                          , qeHaving        = Nothing
                          , qeOrderBy       = []
                          , qeOffset        = Nothing
                          , qeFetchFirst    = Nothing
                          }
    (EDcV (Sign s t))    -> let concNames ::  String -> A_Concept -> Maybe ( Name --Name of the plug
                                                                           , Name --Name of the alias of the plug
                                                                           , Name --Name of the field in the plug
                                                                           )
                                concNames pfx c 
                                  = case c of
                                     PlainConcept{}
                                         -> case sqlRelPlugs fSpec (EDcI c) of
                                              []         -> fatal 344 $ "Problem in selectExpr (EDcV (Sign \""++show s++"\" \""++show t++"\"))"
                                                                      ++"\nNo plug relations found."
                                              [(p,s',_)] -> Just ( QName (name p)
                                                                 , QName pfx
                                                                 , QName (fldname s')
                                                                 )  
                                              xs         -> fatal 349 $ "Problem in selectExpr (EDcV (Sign \""++show s++"\" \""++show t++"\"))"
                                                                      ++"\nMultiple plug relations found: "++show xs
                                     ONE -> Nothing
                                (src1, tgt1, tbl1) =
                                 case ( concNames (if name s==name t then "cfst0" else  (name s)) s
                                      , concNames (if name s==name t then "cfst1" else  (name t)) t
                                      ) of
                                 (Nothing        , Nothing        ) 
                                          -> ( (NumLit "1", Just src)
                                             , (NumLit "1", Just trg)
                                             , []
                                             )
                                 (Just (s1,s2,s3), Nothing        )
                                          -> ( (Iden [s2,s3] , Just src)
                                             , (NumLit "1", Just trg)
                                             , [TRAlias (TRSimple [s1]) (Alias s2 Nothing)]
                                              )
                                 (Nothing        , Just (t1,t2,t3)) 
                                          -> ( (NumLit "1", Just src)
                                             , (Iden [t2,t3] , Just trg)
                                             , [TRAlias (TRSimple [t1]) (Alias t2 Nothing)]
                                              )
                                 (Just (s1,s2,s3), Just (t1,t2,t3)) 
                                          -> ( (Iden [s2,s3] , Just src)
                                             , (Iden [t2,t3] , Just trg)
                                             , [TRAlias (TRSimple [s1]) (Alias s2 Nothing)
                                               ,TRAlias (TRSimple [t1]) (Alias t2 Nothing)
                                               ]
                                              )
                            in sqlcomment ("case: (EDcV (Sign s t))"++"V[ \""++show (Sign s t)++"\" ]") $
                               selectGeneric src1 tgt1 tbl1 Nothing







-- | selectExprInFROM is meant for SELECT expressions inside a FROM clause.
--   It generates a simple table reference for primitive expressions (EDcD, EDcI, and EDcV) and a bracketed SQL expression in more complicated situations.
--   Note that selectExprInFROM makes sure that the attributes of the generated view correspond to the parameters src and trg.
--   Note that the resulting pairs do not contain any NULL values.
selectExprInFROM :: FSpec
                 -> Name      -- ^ source name (preferably quoted)
                 -> Name      -- ^ target name (preferably quoted)
                 -> Expression  -- ^ Whatever expression to generate an SQL query for
                 -> QueryExpr
selectExprInFROM fSpec src trg expr = undefined --TODO


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




selectExists' :: [TableRef]              -- ^ tables
              -> Maybe ValueExpr         -- ^ the WHERE clause
              -> QueryExpr
selectExists' tbl whr
  =  Select { qeSetQuantifier = Distinct
            , qeSelectList    = [(Star,Nothing)]
            , qeFrom          = tbl
            , qeWhere         = whr
            , qeGroupBy       = []
            , qeHaving        = Nothing
            , qeOrderBy       = []
            , qeOffset        = Nothing
            , qeFetchFirst    = Nothing
            }
selectGeneric :: (ValueExpr, Maybe Name) -- ^ (source field and table, alias)
              -> (ValueExpr, Maybe Name) -- ^ (target field and table, alias)
              -> [TableRef]              -- ^ tables
              -> Maybe ValueExpr         -- ^ the WHERE clause
              -> QueryExpr
selectGeneric src tgt tbl whr 
  =  Select { qeSetQuantifier = Distinct
            , qeSelectList    = [src,tgt]
            , qeFrom          = tbl
            , qeWhere         = whr
            , qeGroupBy       = []
            , qeHaving        = Nothing
            , qeOrderBy       = []
            , qeOffset        = Nothing
            , qeFetchFirst    = Nothing
            }

selectSelItem :: (Name, Name) -> TableRef
selectSelItem (att,alias)
  = TRAlias (TRSimple [if stringOfName att == "1"
                       then UQName "1"
                       else toQName att
                      ]
            ) (Alias alias Nothing)


-- | sqlExprSrc gives the quoted SQL-string that serves as the attribute name in SQL.
--   Quotes are added to prevent collision with SQL reserved words (e.g. ORDER).
--   We want it to show the type, which is useful for readability. (Otherwise, just "SRC" and "TGT" would suffice)
sqlExprSrc :: FSpec -> Expression -> Name
sqlExprSrc fSpec (EDcV (Sign a _))   = sqlAttConcept fSpec a
sqlExprSrc fSpec (EDcI c)            = sqlAttConcept fSpec c
sqlExprSrc fSpec (EEps i _)          = sqlAttConcept fSpec i
sqlExprSrc fSpec (EFlp e)            = sqlExprTgt fSpec e
sqlExprSrc fSpec expr@EDcD{}         = toQName $     --  quotes are added just in case the result happens to be an SQL reserved word.
                                       case sqlRelPlugs fSpec expr of
                                        [(_,s,_)] -> Name (fldname s)
                                        []        -> fatal 614 ("No plug for relation "++showADL expr)
                                        [(p,s,t), (p',s',t')] | p==p' && s==t' && t==s'-> Name (fldname s)
                                        _  -> fatal 616 ("Multiple plugs for relation "++showADL expr)
sqlExprSrc _     expr                = QName ("Src"++name (source expr))


-- | sqlExprTgt gives the quoted SQL-string that serves as the attribute name in SQL.
sqlExprTgt :: FSpec -> Expression -> Name
sqlExprTgt fSpec (EDcV (Sign _ b))   = sqlAttConcept fSpec b
sqlExprTgt fSpec (EDcI c)            = sqlAttConcept fSpec c
sqlExprTgt fSpec (EEps i _)          = sqlAttConcept fSpec i
sqlExprTgt fSpec (EFlp e)            = sqlExprSrc fSpec e
sqlExprTgt fSpec expr@EDcD{}         = toQName $     --  quotes are added just in case the result happens to be an SQL reserved word.
                                       case sqlRelPlugs fSpec expr of
                                        [(_,_,t)] -> Name (fldname t)
                                        []        -> fatal 628 ("No plug for relation "++showADL expr)
                                        [(p,s,t), (p',s',t')] | p==p' && s==t' && t==s'-> Name (fldname t)
                                        _  -> fatal 630 ("Multiple plugs for relation "++showADL expr)
sqlExprTgt _     expr                = QName ("Tgt"++name (target expr))

-- sqlConcept gives the name of the plug that contains all atoms of A_Concept c.
-- Quotes are added just in case an SQL reserved word (e.g. "ORDER", "SELECT", etc.) is used as a concept name.
sqlConcept :: FSpec -> A_Concept -> QueryExpr
sqlConcept fSpec a = Table [(QName . name . sqlConceptPlug fSpec) a]

-- sqlConcept yields the plug that contains all atoms of A_Concept c. Since there may be more of them, the first one is returned.
sqlConceptPlug :: FSpec -> A_Concept -> PlugSQL
sqlConceptPlug fSpec c | c==ONE = fatal 583 "A_Concept ONE may not be represented in SQL."
                       | otherwise
             = if null ps then fatal 585 $ "A_Concept \""++show c++"\" does not occur in fSpec (sqlConcept in module Database.Design.Ampersand.Prototype.RelBinGenSQL)" else
               head ps
               where ps = [plug |InternalPlug plug<-plugInfos fSpec
                                , not (null (case plug of ScalarSQL{} -> [c |c==cLkp plug]; _ -> [c' |(c',_)<-cLkpTbl plug, c'==c]))]

sqlAttConcept :: FSpec -> A_Concept -> Name
sqlAttConcept fSpec c | c==ONE = Name "ONE"
                      | otherwise
             = if null cs then fatal 594 $ "A_Concept \""++show c++"\" does not occur in its plug in fSpec \""++name fSpec++"\" (sqlAttConcept in module Database.Design.Ampersand.Prototype.RelBinGenSQL)" else
               QName (head cs)
               where cs = [fldname f |f<-plugFields (sqlConceptPlug fSpec c), c'<-concs f,c==c']


toUqName :: Name -> Name
toUqName = UQName . stringOfName

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


combineQueryExprs :: CombineOp -> [QueryExpr] -> QueryExpr
combineQueryExprs op exprs
 = case exprs of
    []     -> fatal 300 "Nothing to combine!"
    [e]    -> e
    (e:es) -> CombineQueryExpr { qe0 = e
                               , qeCombOp = op
                               , qeSetQuantifier = SQDefault
                               , qeCorresponding = Respectively
                               , qe1 = combineQueryExprs op es
                               }

conjunctSQL :: [ValueExpr] -> ValueExpr
conjunctSQL [] = fatal 57 "nothing to `AND`."
conjunctSQL [ve] = ve
conjunctSQL (ve:ves) = BinOp ve [Name "AND"] (conjunctSQL ves)

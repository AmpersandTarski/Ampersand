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

import Data.Char
import Data.List

fatal :: Int -> String -> a
fatal = fatalMsg "FSpec.SQL"

-- | prettyprint ValueExpr and indent it with spaces
prettySQLQuery :: Int -> QueryExpr -> String
prettySQLQuery i =  intercalate ("\n"++replicate i ' ') .  lines . prettyQueryExpr

selectSrcTgt :: 
           FSpec       -- current context
        -> Expression  -- expression to be translated
        -> QueryExpr   -- resulting SQL expression
selectSrcTgt fSpec expr = selectExpr fSpec (QName "src") (QName "tgt") expr


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
                  exprbracs  = [ selectExprInFROM fSpec src'' trg'' l `as` (Name ("isect"++show n)) 
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
                                             [Name "<>"]
                                             (Iden [Name "isect0", trg'])
                                      ]
                                 else [selectNotExists 
                                         [selectExprInFROM fSpec src'' trg'' l `as` Name "cp"]
                                         (Just . conjunctSQL $
                                           [ BinOp (Iden [Name "isect0",src'])
                                                   [Name "="]
                                                   (Iden [Name "cp",src''])
                                           , BinOp (Iden [Name "isect0",trg'])
                                                   [Name "="]
                                                   (Iden [Name "cp",trg''])
                                           ]
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
                                 [sqlConceptTable fSpec (target expr') `as` Name "allAtoms"]
                                 (Just $ selectNotExists 
                                           [selectExprInFROM fSpec src' trg' expr' `as` Name "complemented"]
                                           (Just (BinOp (Iden [Name "complemented",trg2])
                                                          [Name "="]
                                                        (Iden [Name "allAtoms",trg'])
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
                                 [selectExprInFROM fSpec src' trg' expr' `as` Name "fst"] 
                                 (Just (PostfixOp [Name "is not null"] (Iden [Name "fst", trg'])))
          (s1@EMp1{}: s2@(EDcV _): s3@EMp1{}: fx@(_:_)) -- to make more use of the thing below
             -> sqlcomment ("case:  (s1@EMp1{}: s2@(EDcV _): s3@EMp1{}: fx@(_:_))"
                            ++showADL expr)
                (selectExpr fSpec src trg (foldr1 (.:.) [s1,s2,s3] .:. foldr1 (.:.) fx))
          [EMp1 atomSrc _, EDcV _, EMp1 atomTgt _]-- this will occur quite often because of doSubsExpr
             -> sqlcomment ("case:  [EMp1 atomSrc _, EDcV _, EMp1 atomTgt _]"++showADL expr) $
                Select { qeSetQuantifier = Distinct
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
                                 [selectExprInFROM fSpec src' trg' expr' `as` Name "fst"]
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
                                             [selectExprInFROM fSpec src' mid' e      `as` Name "fst"
                                             ,selectExprInFROM fSpec mid2' trg' expr' `as` Name "snd"
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
                    froms = [ lSQLexp `as` Name ("ECps"++show n) 
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
                    fenceExprs :: [(Int,TableRef,Name,Name)]
                    fenceExprs = -- the first part introduces a 'pole' that consists of the source concept.
                                 [ ( length es
                                   , sqlConceptTable fSpec c
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
                                   , sqlConceptTable fSpec c
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
                                                                      ++"\nMultiple plug relations found: "++concatMap myShow xs
                                                              where
                                                                myShow :: (PlugSQL, SqlField, SqlField) -> String
                                                                myShow (p,sf,tf) = "\n   "++show [name p,fldname sf, fldname tf]        
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
                                             , [TRSimple [s1] `as` s2]
                                              )
                                 (Nothing        , Just (t1,t2,t3)) 
                                          -> ( (NumLit "1", Just src)
                                             , (Iden [t2,t3] , Just trg)
                                             , [TRSimple [t1] `as` t2]
                                             )
                                 (Just (s1,s2,s3), Just (t1,t2,t3)) 
                                          -> ( (Iden [s2,s3] , Just src)
                                             , (Iden [t2,t3] , Just trg)
                                             , [TRSimple [s1] `as` s2
                                               ,TRSimple [t1] `as` t2
                                               ]
                                              )
                            in sqlcomment ("case: (EDcV (Sign s t))"++"V[ \""++show (Sign s t)++"\" ]") $
                               selectGeneric src1 tgt1 tbl1 Nothing
    (EDcI c)             -> sqlcomment ("I["++name c++"]")
                                ( case c of
                                    ONE            -> selectGeneric (NumLit "1", Just src)
                                                                    (NumLit "1", Just trg)
                                                                     [] Nothing
                                    PlainConcept{} -> let cAtt = sqlAttConcept fSpec c in
                                                      selectGeneric (selectSelItem (cAtt, src))
                                                                    (selectSelItem (cAtt, trg))
                                                                    [sqlConceptTable fSpec c]
                                                                    (Just (PostfixOp [Name "IS NOT NULL"] (Iden [cAtt])))
                                )
    -- EEps behaves like I. The intersects are semantically relevant, because all semantic irrelevant EEps expressions have been filtered from es.
    (EEps inter sgn)     -> sqlcomment ("epsilon "++name inter++" "++showSign sgn)  -- showSign yields:   "["++(name.source) sgn++"*"++(name.target) sgn++"]"
                                ( case inter of -- select the population of the most specific concept, which is the source.
                                    ONE            -> selectGeneric (NumLit "1", Just src)
                                                                    (NumLit "1", Just trg)
                                                                     [] Nothing
                                    PlainConcept{} -> let cAtt = sqlAttConcept fSpec inter in
                                                      selectGeneric (selectSelItem (cAtt, src))
                                                                    (selectSelItem (cAtt, trg))
                                                                    [sqlConceptTable fSpec inter]
                                                                    (Just (PostfixOp [Name "IS NOT NULL"] (Iden [cAtt])))
                                )
    (EDcD d)             -> selectExprRelationNew fSpec src trg d

    (EBrk e)             -> selectExpr fSpec src trg e

    (ECpl e)
      -> case e of
           EDcV _        -> sqlcomment ("case: ECpl (EDcV _)  with signature "++show (sign expr)) $  -- yields empty
                            selectGeneric (NumLit "1", Just src)
                                          (NumLit "1", Just trg)
                                          [TRQueryExpr
                                            (Select { qeSetQuantifier = Distinct
                                                    , qeSelectList    = [(NumLit "0",Nothing)]
                                                    , qeFrom          = []
                                                    , qeWhere         = Nothing
                                                    , qeGroupBy       = []
                                                    , qeHaving        = Nothing
                                                    , qeOrderBy       = []
                                                    , qeOffset        = Nothing
                                                    , qeFetchFirst    = Nothing
                                                    } ) `as` Name "a"]
                                          (Just (NumLit "0"))
           EDcI ONE      -> fatal 254 "EDcI ONE must not be seen at this place."
           EDcI c        -> sqlcomment ("case: ECpl (EDcI "++name c++")") $
                            selectGeneric (Iden [Name "concept0", concpt], Just src)
                                          (Iden [Name "concept1", concpt], Just trg)
                                          [sqlConceptTable fSpec c `as` Name "concept0"
                                          ,sqlConceptTable fSpec c `as` Name "concept1"
                                          ]
                                          (Just (BinOp (Iden [Name "concept0", concpt])
                                                       [Name "<>"]
                                                       (Iden [Name "concept1", concpt])
                                                )
                                          )
                             where concpt = sqlAttConcept fSpec c
           _ | source e == ONE -> sqlcomment ("case: source e == ONE"++"ECpl ( \""++showADL e++"\" )") $
                                  selectGeneric (src', Just src)
                                                (trg', Just trg)
                                                [sqlConceptTable fSpec (target e)]
                                                (Just $ selectNotExists 
                                                          [selectExprInFROM fSpec src2 trg2 e `as` Name "cp"]
                                                          Nothing
                                                )
                                    where src' = NumLit "1"
                                          trg' = Iden [sqlAttConcept fSpec (target e)]
                                          src2 = sqlExprSrc fSpec e
                                          trg2 = noCollide' [src2] (sqlExprTgt fSpec e)
           _ | target e == ONE -> sqlcomment ("case: target e == ONE"++"ECpl ( \""++showADL e++"\" )") $
                                  selectGeneric (src', Just src)
                                                (trg', Just trg)
                                                [sqlConceptTable fSpec (source e)]
                                                (Just $ selectNotExists 
                                                          [selectExprInFROM fSpec src2 trg2 e `as`Name "cp"]
                                                          Nothing
                                                )
                                  where src' = Iden [sqlAttConcept fSpec (source e)]
                                        trg' = NumLit "1"
                                        src2 = sqlExprSrc fSpec e
                                        trg2 = noCollide' [src2] (sqlExprTgt fSpec e)
           _ | otherwise       -> sqlcomment ("case: ECpl e"++"ECpl ( \""++showADL e++"\" )") $
                                  selectGeneric (src', Just src)
                                                (trg', Just trg)
                                                [sqlConceptTable fSpec (source e) `as` Name "cfst"
                                                ,sqlConceptTable fSpec (target e) `as` Name "csnd"]
                                                (Just $ selectNotExists 
                                                          [selectExprInFROM fSpec src2 trg2 e `as` Name "cp"]
                                                          (Just . conjunctSQL $
                                                             [ BinOp src'
                                                                     [Name "="]
                                                                     (Iden [Name "cp",src2])
                                                             , BinOp trg'
                                                                     [Name "="]
                                                                     (Iden [Name "cp",trg2])
                                                             ]
                                                          )
                                                )
                                    where src' = Iden [Name "cfst",sqlAttConcept fSpec (source e)]
                                          trg' = Iden [Name "csnd",sqlAttConcept fSpec (target e)]
                                          src2 = sqlExprSrc fSpec e
                                          trg2 = noCollide' [src2] (sqlExprTgt fSpec e)
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
                  = selectGeneric (Iden [ srcAlias, mainSrc], Just src)
                                  (Iden [ tgtAlias, mainTgt], Just trg)
                                  [sqlConceptTable fSpec (target l) `as` srcAlias
                                  ,sqlConceptTable fSpec (target r) `as` tgtAlias]
                                  (Just $ selectNotExists 
                                            [lCode `as` lhs]
                                            ( Just $ conjunctSQL
                                                [BinOp (Iden [srcAlias,mainSrc])
                                                       [Name "="]
                                                       (Iden [lhs,ltrg])
                                                ,selectNotExists 
                                                   [rCode `as` rhs]
                                                   ( Just $ conjunctSQL 
                                                      [BinOp (Iden [rhs,rsrc])
                                                             [Name "="]
                                                             (Iden [Name "lhs"])
                                                      ,BinOp (Iden [Name "rhs",rtrg])
                                                             [Name "="]
                                                             (Iden [tgtAlias,mainTgt])
                                                      ]
                                                   )
                                                ]
                                            )
                                  )
             mainSrc = (sqlAttConcept fSpec.target) l  -- Note: this 'target' is not an error!!! It is part of the definition of right residu
             mainTgt = (sqlAttConcept fSpec.target) r
             relNames = foldrMapExpression uni (\decl->[Name (name decl)]) [] expr
             srcAlias = noCollide' relNames (Name "RResLeft")
             tgtAlias = noCollide' relNames (Name "RResRight")
             lhs  = Name "lhs"
             rhs  = Name "rhs"
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
        EDcD{} -> if sqlExprSrc fSpec expr === src && sqlExprTgt fSpec expr === trg
                  then ( if not mayContainNulls 
                         then TRSimple [declName]
                         else TRQueryExpr $
                              selectGeneric (selectSelItem (sqlExprSrc fSpec expr, src))
                                            (selectSelItem (sqlExprTgt fSpec expr, trg))
                                            [TRSimple [declName]]
                                            (Just $ conjunctSQL
                                               [notNull src, notNull trg])
                       )
                  else TRQueryExpr $
                       selectGeneric (selectSelItem (sqlExprSrc fSpec expr, src))
                                     (selectSelItem (sqlExprTgt fSpec expr, trg))
                                     [TRSimple [declName]]
                                     (if mayContainNulls
                                      then (Just $ conjunctSQL
                                              [notNull src, notNull trg])
                                      else Nothing
                                     )
                  where
                   (declName,mayContainNulls)
                     = case sqlRelPlugs fSpec expr of
                         []           -> fatal 371 ("No plug found for expression "++showADL expr)
                         [(plug@TblSQL{},_,_)] -> (QName (name plug), True)
                         [(plug,_,_)]          -> (QName (name plug), False )
                         [(plug,s,t),(plug',s',t')]  --This can be the case for a Prop -relation.
                                      -> if plug == plug' && s == t' && t == s'
                                         then (QName (name plug), True)
                                         else fatal 390 ("Multiple plugs found for expression "++showADL expr)
                         _            -> fatal 371 ("Multiple plugs found for expression "++showADL expr)
        EDcI ONE -> fatal 401 "ONE is unexpected at this place."
        EDcI c -> sqlcomment ( " case: EDcI " ++ name c ++ " ") $
                  (
                  case (cpt, cptAlias) of
                    (cpt', (Iden [x],Nothing)) -> if cpt'=== x
                                                  then TRSimple [cpt']
                                                  else sg
                    _                          -> sg
                  )
                  where  
                   sg = TRQueryExpr $
                       selectGeneric (selectSelItem (sqlAttConcept fSpec c, src))
                                     (Iden [sqlConcept fSpec c],Nothing)
                                     [sqlConceptTable fSpec c]
                                     Nothing
                   cptAlias = selectSelItem (sqlAttConcept fSpec c, src)  -- Alias to src if needed.
                   cpt = sqlConcept fSpec c
--        EDcV{}
--          | source expr == ONE && target expr == ONE -> fatal 410 "The V of WHAT???"
--          | source expr == ONE
--               -> TRQueryExpr $ 
--                  selectGeneric (NumLit "1",Nothing)
--                                (selectSelItem (sqlExprTgt fSpec expr, trg))
--                                [rightConceptTable]
--                                Nothing
--          | target expr == ONE
--               -> TRQueryExpr $ 
--                  selectGeneric (selectSelItem (sqlExprSrc fSpec expr, src))
--                                (NumLit "1",Nothing)
--                                [leftConceptTable]
--                                Nothing
--
--          | otherwise
--               -> TRQueryExpr $ 
--                  selectGeneric (selectSelItem (sqlExprSrc fSpec expr, src))
--                                (NumLit "1",Nothing)
--                                [leftConceptTable, if rightConcept===rC
--                                                   then rightConceptTable
--                                                   else rightConceptTable `as` rC]
--                                Nothing
--
--
--                  where
--                    leftConcept       = sqlConcept      fSpec (source expr)
--                    leftConceptTable  = sqlConceptTable fSpec (source expr)
--                    rightConcept      = sqlConcept      fSpec (target expr)
--                    rightConceptTable = sqlConceptTable fSpec (target expr)
--                    rC  = noCollide' [sqlConcept fSpec (source expr)] (sqlConcept fSpec (target expr))
        _      -> TRQueryExpr $ selectExpr fSpec src trg expr


   where
     unquoted :: Name -> Bool
     unquoted (Name _ )   = True
     unquoted (QName _)   = False
     unquoted (UQName  _) = True
     notNull :: Name -> ValueExpr
     notNull tm = PostfixOp [Name "IS NOT NULL"] (Iden [tm])
     
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
 = prettyQueryExpr $ selectExprRelationNew fSpec s t dcl
     where s = QName "src"
           t = QName "trg"
     
selectExprRelationNew :: FSpec
                   -> Name -- ^ Alias of source
                   -> Name -- ^ Alias of target
                   -> Declaration
                   -> QueryExpr


selectExprRelationNew fSpec srcAS trgAS dcl =
  case dcl of
    Sgn{}  -> leafCode (EDcD dcl)
    Isn{}  -> leafCode (EDcI (source dcl))
    Vs sgn
     | source sgn == ONE -> fatal 468 "ONE is not expected at this place"
     | target sgn == ONE -> fatal 469 "ONE is not expected at this place"
     | otherwise
           -> let src,trg :: [Name]
                  src=[Name "vfst", sqlAttConcept fSpec (source sgn)]
                  trg=[Name "vsnd", sqlAttConcept fSpec (target sgn)]
              in selectGeneric (Iden src, Just srcAS)
                               (Iden trg, Just trgAS)
                               [sqlConceptTable fSpec (source sgn) `as` Name "vfst"
                               ,sqlConceptTable fSpec (target sgn) `as` Name "vsnd"]
                               (Just (conjunctSQL (map notNull [src,trg]))) 


                  
   where
     notNull :: [Name] -> ValueExpr
     notNull tm = PostfixOp [Name "IS NOT NULL"] (Iden tm)                         
     leafCode :: Expression -> QueryExpr
     leafCode expr =  -- made for both Rel and I
       case sqlRelPlugs fSpec expr of
         []           -> fatal 344 $ "No plug for expression "++show expr
         (plug,s,t):_ -> selectGeneric (Iden [Name (fldname s)], Just srcAS)
                                       (Iden [Name (fldname t)], Just trgAS)
                                       [TRSimple [QName (name plug)]]
                                       (Just . conjunctSQL . map notNull $
                                          [ [QName (fldname c)] | c<-nub [s,t]])
  -- TODO: "NOT NULL" checks could be omitted if column is non-null, but the
  -- code for computing table properties is currently unreliable.

selectExists, selectNotExists
     :: [TableRef]      -- ^ tables
     -> Maybe ValueExpr -- ^ the (optional) WHERE clause
     -> ValueExpr
selectNotExists tbls whr = PrefixOp [Name "NOT"] $ selectExists tbls whr
selectExists tbls whr = 
  SubQueryExpr SqExists
     Select { qeSetQuantifier = Distinct
            , qeSelectList    = [(Star,Nothing)]
            , qeFrom          = tbls
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
              -> Maybe ValueExpr         -- ^ the (optional) WHERE clause
              -> QueryExpr
selectGeneric src tgt tbls whr 
  =  Select { qeSetQuantifier = Distinct
            , qeSelectList    = [src,tgt]
            , qeFrom          = tbls
            , qeWhere         = whr
            , qeGroupBy       = []
            , qeHaving        = Nothing
            , qeOrderBy       = []
            , qeOffset        = Nothing
            , qeFetchFirst    = Nothing
            }

selectSelItem :: (Name, Name) -> (ValueExpr,Maybe Name)
selectSelItem (att,alias)
  | att === alias           = (Iden [toQName att], Nothing)
  | stringOfName att == "1" = (Iden [UQName "1" ], Just alias)
  | otherwise               = (Iden [toQName att], Just alias)


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
sqlAttConcept fSpec c | c==ONE = Name "ONE"
                      | otherwise
             = if null cs then fatal 594 $ "A_Concept \""++show c++"\" does not occur in its plug in fSpec \""++name fSpec++"\"" else
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
                               , qeSetQuantifier = Distinct
                               , qeCorresponding = Respectively
                               , qe1 = combineQueryExprs op es
                               }

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
    




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

import Data.Maybe
import Data.Char

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
                  wherecl   = if isIdent l
                              then Just (BinOp
                                           (BinOp (Iden (Name "isect0")) (Name ".") (Iden src'))
                                           (Name "=")
                                           (BinOp (Iden (Name "isect0")) (Name ".") (Iden trg'))
                                        )
                              else Nothing
                  conjunct :: [ValueExpr] -> ValueExpr
                  conjunct [] = fatal 57 "nothing to `AND`."
                  conjunct [ve] = ve
                  conjunct (ve:ves) = BinOp ve (Name "and") (conjunct ves)
                                            

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
                  (_:_:_) -> selectGeneric (Iden [Name "isect0",src'],src)
                                           (Iden [Name "isect0",trg'],trg)            
                                           exprbracs
                                           wherecl
--                    _       -> fatal 69 "TODO"


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
            , qeFrom = tbl
            , qeWhere = whr
            , qeGroupBy = []
            , qeHaving = Nothing
            , qeOrderBy = []
            , qeOffset = Nothing
            , qeFetchFirst = Nothing
            }
selectGeneric :: Maybe (ValueExpr, Maybe Name) -- ^ (source field and table, alias)
              -> Maybe (ValueExpr, Maybe Name) -- ^ (target field and table, alias)
              -> [TableRef]              -- ^ tables
              -> Maybe ValueExpr         -- ^ the WHERE clause
              -> QueryExpr
selectGeneric src tgt tbl whr 
  =  Select { qeSetQuantifier = Distinct
            , qeSelectList    = catMaybes  [src,tgt]
            , qeFrom = tbl
            , qeWhere = whr
            , qeGroupBy = []
            , qeHaving = Nothing
            , qeOrderBy = []
            , qeOffset = Nothing
            , qeFetchFirst = Nothing
            }

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


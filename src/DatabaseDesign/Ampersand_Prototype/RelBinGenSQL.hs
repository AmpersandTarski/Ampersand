{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
module DatabaseDesign.Ampersand_Prototype.RelBinGenSQL
 (sqlRelPlugs,sqlExprTrg,sqlExprSrc,sqlPlugFields,getDeclarationTableInfo,selectExpr,selectExprRelation,isOne,isOne'
 ) where 
import DatabaseDesign.Ampersand_Prototype.CoreImporter

import DatabaseDesign.Ampersand_Prototype.RelBinGenBasics (zipnum,Concatable(..),(+++),quote
                                 ,cChain,addSlashes,phpIndent)
import Data.Maybe
import Data.Char(isDigit,digitToInt,intToDigit)
import Data.List
import DatabaseDesign.Ampersand_Prototype.Version 

fatal :: Int -> String -> a
fatal = fatalMsg "RelBinGenSQL"

-- isOne: het is niet voldoende om alleen te controleren of: source (contextOf o) == ONE
-- De interface op V[ONE*SomeConcept] moet immers nog voor ieder SomeConcept iets aanbieden
-- de vraag die we hier stellen is: komen we steeds op eenzelfde A_Concept uit
-- als dit zo is, hoeven we alleen dat ene A_Concept te tonen
--
-- Bovenstaand commentaar snap ik niet (gmi)
-- in de php code heb je een instantie van een A_Concept (ID=,$id,etc.) 
-- soms is het id constant i.e. source (contextOf o) == ONE.
-- In SQL code generatie (doSqlGet) wordt volgens mij bovenstaande betekenis aan "is One" gegeven (was: isOne'= isOne objOut)
-- daarom heb ik ze opgesplitst
-- isOneExpr :: Expression -> Bool
-- isOneExpr e' = (isUni.conjNF.ECps) [vExpr (Sign (source e') (source e')),e']
isOne' :: ObjectDef -> Bool
isOne' = isOne     -- isOneExpr$contextOf o
                   --TODO: isOneExpr zorgt sowieso voor slechte select queries (doSqlGet), misschien kan deze wel weg.
                   --      isOneExpr (rev:771) kan in de problemen komen bij doSqlGet (error "line 578 Object.hs"). 
                   --      in dat geval komt isOne (huidige rev) ook in de problemen (error iets met viewgroup)
                   --      Dit heeft te maken met ingewikkelde kernels.
                   --      case waarbij INJ cruciale rol speelt:
                   --  value1 :: Obj -> Datatype [INJ].
                   --  attr1 :: Obj -> Attr.
                   --  attr2 :: Obj * Attr[UNI].
                   --  value2 :: Attr -> Datatype.
                   --  volgens mij was deze case ook al problematisch
                   --  value1 :: Obj * Datatype [INJ].
                   --  attr1 :: Obj * Attr [UNI].
                   --  value2 :: Attr -> Datatype.
isOne :: ObjectDef -> Bool
isOne o = source (contextOf o) == ONE

-- | add comments to Maybe sql strings, for example for debugging purposes
sqlcomment :: Int -> String -> Maybe String -> Maybe String 
sqlcomment _ _    Nothing   = Nothing
sqlcomment i cmt (Just sql) = Just ("/* "++addSlashes cmt++" */"++phpIndent i++sql)

{- selectExpr translates an Expression (which is in Ampersand's A-structure) into an SQL expression in textual form.
-}
selectExpr ::    Fspc       -- current context
              -> Int        -- indentation
              -> String     -- SQL name of the source of this expression, as assigned by the environment 
              -> String     -- SQL name of the target of this expression, as assigned by the environment
              -> Expression -- expression to be translated
              -> Maybe String     -- resulting SQL expression
-- In order to translate all Expressions, code generators have been written for EUni ( \/ ), EIsc ( /\ ), EFlp ( ~ ), ECpl (unary - ), and ECps ( ; ),
-- each of which is supposed to generate correct code in 100% of the cases. (TODO: how do we establish that properly?)
-- The other operators, EEqu ( = ), EImp ( |- ), ERad ( ! ), EPrd ( * ), ELrs ( / ), and ERrs ( \ ), have been implemented in terms of the previous ones,
-- in order to prevent mistakes in the code generator. It is possible that more efficient code may be generated in these cases.
-- Special cases are treated up front, so they will overrule the more general cases.
-- That allows more efficient code while retaining correctness and completeness as much as possible.
-- Code for the Kleene operators EKl0 ( * ) and EKl1 ( + ) is not done, because this cannot be expressed in SQL.
-- These operators must be eliminated from the Expression before using selectExpr, or else you will get fatals.

--TODO
selectExpr fSpec i src trg expr
 = case expr of
    EIsc{} -> let lst'       = exprIsc2list expr
                  sgn        = sign expr
                  sqlOk      = all isJust exprbracs'
                  exprbracs  = catMaybes exprbracs'
                  src'       = sqlExprSrc fSpec fstm
                  trgC       = sqlExprTrg fSpec fstm -- can collide with src', for example in case fst==r~;r, or if fst is a property (or identity)
                  trg'       = noCollideUnlessTm' fstm [src'] trgC
                  fstm       = head posTms  -- always defined, because length posTms>0 (ensured in definition of posTms)
                  mp1Tm      = take 1 [t | t@EMp1{}<-lst']++[t | t<-lst', [EMp1{},EDcV _,EMp1{}] <- [exprCps2list t]]
                  lst        = [t |t<-lst', t `notElem` mp1Tm]
                  posTms     = if null posTms' then map (notCpl sgn) (take 1 negTms') else posTms' -- we take a term out of negTms' if we have to, to ensure length posTms>0
                  negTms     = if null posTms' then tail negTms' else negTms' -- if the first term is in posTms', don't calculate it here
                  posTms'    = [t | t<-lst, isPos t && not (isIdent t)]++[t | t<-lst, isPos t && isIdent t] -- the code to calculate I is better if it is not the first term
                  negTms'    = [notCpl sgn t | t<-lst, isNeg t && isIdent t]++[notCpl sgn t | t<-lst, isNeg t && not (isIdent t)] -- should a negTerm become a posTerm (for reasons described above), it can best be an -I.
                  exprbracs' = [ case brc of
                                  Just s->Just (s ++ " AS isect"++show n)
                                  Nothing->Nothing
                               | (n,l)<-zipnum posTms
                               , src''<-[sqlExprSrc fSpec l]
                               , trg''<-[noCollideUnlessTm' l [src''] (sqlExprTrg fSpec l)]
                               , let brc = selectExprBrac fSpec i src'' trg'' l
                               ]
                  wherecl   = [Just$ if isIdent l
                               then  "isect0."++src'++" = isect0."++trg' -- this is the code to calculate ../\I. The code below will work, but is longer
                               else "(isect0."++src'++" = isect"++show n++"."++src''
                               ++ " AND isect0."++trg'++" = isect"++show n++"."++trg''++")"
                              | (n,l)<-tail (zipnum posTms) -- not empty because of definition of posTms
                              , src''<-[sqlExprSrc fSpec l]
                              , trg''<-[noCollideUnlessTm' l [src''] (sqlExprTrg fSpec l)]
                              ]++
                              [Just$ "isect0."++src'++" = \\'"++atom++"\\'" -- source and target are equal because this is the case with EMp1{}
                              | (EMp1 atom _) <- mp1Tm
                              ]++
                              [Just$ "isect0."++src'++" = \\'"++atom1++"\\'" -- source and target are unequal
                                ++ " AND isect0."++trg'++" = \\'"++atom2++"\\'" -- source and target are unequal
                              | t@ECps{} <- mp1Tm, [EMp1 atom1 _, EDcV _, EMp1 atom2 _]<-[exprCps2list t]
                              ]++
                              [if isIdent l
                               then  Just ("isect0."++src'++" <> isect0."++trg') -- this code will calculate ../\-I
                               else  "NOT EXISTS ("+++(selectExists' (i+12)
                                                                    ((selectExprBrac fSpec (i+12) src'' trg'' l) +++ " AS cp")
                                                                    ("isect0."++src' ++ "=cp."++src''++" AND isect0."++ trg'++"=cp."++trg'')
                                                  )+++")"
                              | (_,l)<-zipnum negTms
                              , src''<-[sqlExprSrc fSpec l]
                              , trg''<-[noCollideUnlessTm' l [src''] (sqlExprTrg fSpec l)]
                              ]++[Just$ "isect0."++src'++" IS NOT NULL",Just$ "isect0."++trg'++" IS NOT NULL"]
              in case lst' of
{- The story:
 This alternative of selectExpr compiles a conjunction of at least two subexpressions (code: EIsc lst'@(_:_:_))
 For now, we explain only the otherwise clause (code: selectGeneric i ("isect0."++ ...)
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
                      AND NOT EXISTS (SELECT foo)            representing neven
-}
                   (_:_:_)   | sqlOk ->  sqlcomment i ("case: (EIsc lst'@(_:_:_) _)"++phpIndent (i+3)++showADL expr++" ("++show sgn++")") $
                                         selectGeneric i ("isect0."++src',src) ("isect0."++trg',trg)
                                                         (cChain ", " exprbracs) (cChain " AND " wherecl)
                             | otherwise -> fatal 71 "sqlOk does not hold"
                   _     -> fatal 123 "A list shorter than 2 cannot occur in the query at all! If it does, we have made a mistake earlier."


    EUni{}   -> sqlcomment i ("case: EUni (l,r)"++phpIndent (i+3)++showADL expr++" ("++show (sign expr)++")") $
                Just (intercalate " UNION " [ "(" ++ str ++ phpIndent i ++ ")" | e<-exprUni2list expr , Just str<-[selectExpr fSpec (i+4) src trg e]] )
    ECps{}  ->
       let es = exprCps2list expr in
       case es of 
          (EDcV (Sign ONE _):fs@(_:_))
             -> let expr' = foldr1 (.:.) fs
                    src'  = noCollideUnlessTm' expr' [trg'] (sqlExprSrc fSpec expr')
                    trg'  = sqlExprTrg fSpec expr'
                in sqlcomment i ("case:  (EDcV (Sign ONE _): fs@(_:_))"++phpIndent (i+3)++showADL expr) $
                   selectGeneric i ("1",src) ("fst."++trg',trg)
                      (selectExprBrac fSpec i src' trg' expr' +++ " AS fst")
                      (Just$ "fst."++trg'++" IS NOT NULL")
          (s1@EMp1{}: s2@(EDcV _): s3@EMp1{}: fx@(_:_)) -- to make more use of the thing below
             -> sqlcomment i ("case:  (s1@EMp1{}: s2@(EDcV _): s3@EMp1{}: fx@(_:_))"
                ++
                phpIndent (i+3)++showADL expr) (selectExpr fSpec i src trg (foldr1 (.:.) [s1,s2,s3] .:. foldr1 (.:.) fx))
          [EMp1 atomSrc _, EDcV _, EMp1 atomTrg _]-- this will occur quite often because of doSubsExpr
             -> sqlcomment i ("case:  [EMp1 atomSrc _, EDcV _, EMp1 atomTrg _]"++phpIndent (i+3)++showADL expr) $
                 Just$ "SELECT \\'"++atomSrc++"\\' AS "++src++", \\'"++atomTrg++"\\' AS "++trg

          (e@(EMp1 atom _):f:fx)
             -> let expr' = foldr1 (.:.) (f:fx)
                    src' = sqlExprSrc fSpec e
                    trg' = noCollideUnlessTm' expr' [src'] (sqlExprTrg fSpec expr')
                in sqlcomment i ("case:  (EMp1{}: f: fx)"++phpIndent (i+3)++showADL expr) $
                   selectGeneric i ("fst."++src',src) ("fst."++trg',trg)
                                   (selectExprBrac fSpec i src' trg' expr'+++" AS fst")
                                   (Just$"fst."++src'++" = \\'"++atom++"\\'")
          (e:EDcV _:f:fx) -- prevent calculating V in this case
             | src==trg && not (isProp e) -> fatal 146 $ "selectExpr 2 src and trg are equal ("++src++") in "++showADL e
             | otherwise -> let expr' = foldr1 (.:.) (f:fx)
                                src' = sqlExprSrc fSpec e
                                mid' = sqlExprTrg fSpec e
                                mid2'= sqlExprSrc fSpec f
                                trg' = noCollideUnlessTm' expr' [mid2'] (sqlExprTrg fSpec expr')
                            in sqlcomment i ("case:  (e:ERel (V _) _:f:fx)"++phpIndent (i+3)++showADL e) $
                                   selectGeneric i ("fst."++src',src) ("snd."++trg',trg)
                                                    ((selectExprBrac fSpec i src' mid' e)+++" AS fst,"++phpIndent (i+5)+++(selectExprBrac fSpec i mid2' trg' f)+++" AS snd")
                                                    ("fst."++src'+++" IS NOT NULL")
{-  The ECps is treated as poles-and-fences.
    Imagine subexpressions as "fences". The source and target of a "fence" are the "poles" between which that "fence" is mounted.
    In this metaphor, we create the FROM-clause directly from the "fences", and the WHERE-clause from the "poles" between "fences".
    The "outer poles" correspond to the source and target of the entire expression.
    To prevent name conflicts in SQL, each subexpression is aliased in SQL by the name "ECps<n>".
-}
          _:_:_  -- in this case, it is certain that there are at least two elements in es.
            -> let mainSrc = selectSelItem ("ECps"++show n++"."++sqlSrc,src)
                             where (n,_,sqlSrc,_) = head fenceExprs
                   mainTrg = selectSelItem ("ECps"++show n++"."++sqlTrg,trg) 
                             where (n,_,_,sqlTrg) = last fenceExprs
                   selectClause = "SELECT DISTINCT " ++ mainSrc ++ ", " ++mainTrg
                   fromClause   = "FROM " ++ intercalate (","++phpIndent (i+5)) [ lSQLexp | (_,lSQLexp,_,_)<-fenceExprs ]
                   whereClause
                           = "WHERE " ++ intercalate (phpIndent i++"  AND ")
                             [ "ECps"++show n++"."++lSQLtrg++"=ECps"++show (n+1)++"."++rSQLsrc
                             | ((n,_,_,lSQLtrg),(_,_,rSQLsrc,_))<-zip (init fenceExprs) (tail fenceExprs)
                             ]
                   -- fenceExprs lists the expressions and their SQL-fragments.
                   -- In the poles-and-fences metaphor, they serve as the fences between the poles.
                   fenceExprs = [ ( n                                                                                -- the serial number of this fence (in between poles n and n+1)
                                  , sqlExpr ++ " AS ECps"++show n
                                  , srcAtt
                                  , trgAtt
                                  )
                                | (n, e) <- zip [(0::Int)..] es
                                , srcAtt<-[sqlExprSrc fSpec e]
                                , trgAtt<-[noCollideUnlessTm' e [srcAtt] (sqlExprTrg fSpec e)]
                                , let Just sqlExpr = selectExprBrac fSpec i srcAtt trgAtt e
                                ]
               in sqlcomment i ("case: (ECps es), with two or more elements in es."++phpIndent (i+3)++showADL expr) $ Just $ phpIndent i++
                  selectClause ++phpIndent i++
                  fromClause   ++phpIndent i++
                  whereClause 
          _  -> fatal 215 "impossible outcome of exprCps2list"

    (EFlp x _) -> sqlcomment i "case: EFlp x." $
                 selectExpr fSpec i trg src x
    (EMp1 atom _) -> sqlcomment i "case: EMp1 atom."
                      (Just $ "SELECT "++show atom++" AS "++src++", "++show atom++" AS "++trg)
    (EDcV (Sign s t))    -> let concNames pfx c = [([],"1") |c==ONE]++[([quote (name p) ++ " AS "++pfx],pfx++"."++quote (fldname s')) | (p,s',_) <- sqlRelPlugs fSpec (iExpr c)]
                            in sqlcomment i ("case: (vExpr (Sign s t))"++phpIndent (i+3)++"V [ \""++show (Sign s t)++"\" ]") $
                               case [selectGeneric i (src',src) (trg',trg) tbls "1"
                                    | (s',src') <- concNames (if name s==name t then "cfst0" else quote (name s)) s
                                    , (t',trg') <- concNames (if name s==name t then "cfst1" else quote (name t)) t
                                    , let tbls = if null (s'++t') then "(SELECT 1) AS csnd" else intercalate ", " (s'++t')
                                    ] of
                                 []    -> fatal 216 $ "Problem in selectExpr (vExpr (Sign \""++show s++"\" \""++show t++"\"))"
                                 sql:_ -> Just sql 
    (EDcI     sgn)       -> sqlcomment i ("I["++(name.source) sgn++"]") 
                                ( case source sgn of
                                    ONE -> Just ( "SELECT 1 AS "++src++", 1 AS "++trg)
                                    c   -> selectExprRelation fSpec i src trg (Isn c)
                                )
    (EDcD d   _)         -> selectExprRelation fSpec i src trg d

    (EBrk e) -> selectExpr fSpec i src trg e

    (ECpl (EDcV _) sgn)  -> sqlcomment i ("case: ECpl (EDcV _)  with signature "++show sgn)$   -- yields empty
                                 toM(selectGeneric i ("1",src) ("1",trg) "(SELECT 1) AS a" "0")
    (ECpl e sgn)
      -> sqlcomment i ("case: ECpl e"++phpIndent (i+3)++"ECpl ( \""++showADL e++"\" ) \""++show sgn++"\"") $
         selectGeneric i ("cfst."++src',src) ("csnd."++trg',trg)
                         (sqlConcept fSpec (source e) ++ " AS cfst,"++phpIndent (i+5)+++selectExprBrac fSpec (i+5) trg' trg' (iExpr (target e))+++" AS csnd")
                         ("NOT EXISTS"++phpIndent i++" ("+++
                             selectExists' (i+2) (selectExprBrac fSpec (i + 2) src2 trg2 e +++ " AS cp")
                                                 ("cfst." ++ src' ++ "=cp."++src2++" AND csnd."++ trg'++"=cp."++trg2)
                              +++ ")"
                         )
                         where src' = quote $ sqlAttConcept fSpec (source e) 
                               trg' = quote $ noCollide [src'] (sqlAttConcept fSpec (target e))
                               src2 = sqlExprSrc fSpec e
                               trg2 = noCollideUnlessTm' e [src2] (sqlExprTrg fSpec e)
    (EKl0 _ _) -> fatal 249 "SQL cannot create closures EKl0" (Just "SELECT * FROM NotExistingKl0")
    (EKl1 _ _) -> fatal 249 "SQL cannot create closures EKl1" (Just "SELECT * FROM NotExistingKl1")
    (EDif (EDcV _,x) sgn) -> sqlcomment i ("case: EDif V x"++phpIndent (i+3)++"EDif V ( \""++showADL x++"\" ) \""++show sgn++"\"") $
                                 selectExpr fSpec i src trg (notCpl sgn x) 
-- The following definitions express code generation of the remaining cases in terms of the previously defined generators.
-- As a result of this way of working, code generated for =, |-, -, !, *, \, and / may not be efficient, but at least it is correct.
    EEqu (l,r) sgn
      -> sqlcomment i ("case: EEqu (l,r)"++phpIndent (i+3)++showADL expr++" ("++show sgn++")") $
         selectExpr fSpec i src trg ((ECpl l sgn .\/. r) ./\. (ECpl r sgn .\/. l))
    EImp (l,r) sgn
      -> sqlcomment i ("case: EImp (l,r)"++phpIndent (i+3)++showADL expr++" ("++show sgn++")") $
         selectExpr fSpec i src trg (ECpl l sgn .\/. r)
    EDif (l,r) sgn
      -> sqlcomment i ("case: EDif (l,r)"++phpIndent (i+3)++showADL expr++" ("++show sgn++")") $
         selectExpr fSpec i src trg (l ./\. ECpl r sgn)
    ERrs (l,r) _ sgn
      -> sqlcomment i ("case: ERrs (l,r)"++phpIndent (i+3)++showADL expr++" ("++show sgn++")") $
         selectExpr fSpec i src trg (notCpl sgn (flp l) .!. r)
    ELrs (l,r) _ sgn
      -> sqlcomment i ("case: ELrs (l,r)"++phpIndent (i+3)++showADL expr++" ("++show sgn++")") $
         selectExpr fSpec i src trg (l .!. notCpl sgn (flp r))
    ERad{}
      -> sqlcomment i ("case: ERad (l,r)"++phpIndent (i+3)++showADL expr++" ("++show (sign expr)++")") $
        selectExpr fSpec i src trg (deMorgan (sign expr) expr)
    EPrd (l,r) sgn
     -> let v = vExpr (Sign (target l) (source r))
        in sqlcomment i ("case: EPrd (l,r)"++phpIndent (i+3)++showADL expr++" ("++show sgn++")") $
           selectExpr fSpec i src trg (foldr1 (.:.) [l,v,r])
    ETyp x sgn -> sqlcomment i ("case: ETyp x _"++phpIndent (i+3)++showADL expr++" (possible mistake: has the population been restricted using signature "++show sgn++"?)") $
                  selectExpr fSpec i src trg x  -- TODO, this is wrong. Delimit the population using the sgn of ETyp!!!

selectExprBrac :: Fspc
               -> Int         -- ^ indentation
               -> String      -- ^ source name (preferably quoted)
               -> String      -- ^ target name (preferably quoted)
               -> Expression  -- ^ Whatever expression to generate an SQL query for
               -> Maybe String
selectExprBrac fSpec i src trg expr
   | unquoted src = selectExprBrac fSpec i (quote src) trg         expr
   | unquoted trg = selectExprBrac fSpec i src         (quote trg) expr
   | otherwise = 
      case expr of 
        EBrk  e  -> selectExprBrac fSpec i src trg e
        EDcD{}   -> leafCode
        EDcI{}   -> leafCode
        EDcV{}   -> leafCode
        _        -> phpIndent (i+5) ++ "( " +++ selectExpr fSpec (i+7) src trg expr+++ phpIndent(i+5)++")"
   where 
     leafCode = listToMaybe ([quote (name p) |(p,s,t)<-sqlRelPlugs fSpec expr,quote (fldname s)==quote src,quote (fldname t)==quote trg]
             ++ maybeToList ("( " +++selectExpr fSpec (i+2) src trg expr+++" )"))
     unquoted [] = False
     unquoted (x:_) = x /= '`'
     
-- | does the same as noCollide, but ensures that all names used have `quotes` around them (for mySQL)
noCollide' :: [String] -> String -> String
noCollide' nms nm = quote$noCollide (map unquote nms) (unquote nm)
unquote :: String->String
unquote ('`':xs) = init xs
unquote xs = xs
-- | changes its second argument by appending a digit, such that it does not occur in its first argument 
noCollide :: [String] -- ^ forbidden names
          -> String -- ^ preferred name
          -> String -- ^ a unique name (does not occur in forbidden names)
noCollide names nm | nm `elem` names = noCollide names (namepart (reverse nm) ++ changeNr (numberpart (reverse nm)))
                   | otherwise = nm
 where
   namepart str   = reverse (dropWhile isDigit str)
   numberpart str = reverse (takeWhile isDigit str)
   changeNr x     = int2string (string2int x+1)
   --  changeNr x = show (read x +1)
   string2int :: String -> Int
   string2int  = enc.reverse
    where enc "" = 0
          enc (c:cs) = digitToInt c + 10* enc cs
   int2string :: Int -> String
   int2string 0 = "0"
   int2string n = if n `div` 10 == 0 then [intToDigit (n `rem` 10) |n>0] else int2string (n `div` 10)++[intToDigit (n `rem` 10)]

noCollideUnlessTm' :: Expression 
                  -> [String]
                  -> String
                  -> String

noCollideUnlessTm' (EDcD{}) _ nm = quote nm
noCollideUnlessTm' (EDcI{}) _ nm = quote nm
noCollideUnlessTm' (EDcV{}) _ nm = quote nm
noCollideUnlessTm' _  names nm = noCollide' names nm

selectExprRelation :: Fspc
                   -> Int    -- ^ Indentation
                   -> String -- ^ Alias of source
                   -> String -- ^ Alias of target
                   -> Declaration
                   -> Maybe String

selectExprRelation fSpec i srcAS trgAS dcl =
  case dcl of
    Sgn{}  -> leafCode (EDcD dcl (sign dcl))
    Isn{}  -> leafCode (EDcI     (sign dcl))
    Vs sgn -> let src="vfst."++sqlAttConcept fSpec (source sgn)
                  trg="vsnd."++sqlAttConcept fSpec (target sgn)
              in selectGeneric i (src,srcAS) (trg,trgAS)
                  (sqlConcept fSpec (source sgn) +++ " AS vfst, "++sqlConcept fSpec (target sgn)++ " AS vsnd")
                  (src+++" IS NOT NULL AND "++trg++" IS NOT NULL")
   where
     leafCode expr =  -- made for both Rel and I
       case sqlRelPlugs fSpec expr of
         []        -> fatal 344 $ "No plug for expression "++show expr
         (p,s,t):_ -> Just $ selectGeneric i (quote (fldname s),srcAS) (quote (fldname t),trgAS) (quote (name p)) (quote (fldname s)++" IS NOT NULL AND "++quote (fldname t)++" IS NOT NULL")
  -- TODO: "NOT NULL" checks could be omitted if column is non-null, but the
  -- code for computing table properties is currently unreliable.
               
selectExists' :: (Concatable a,Concatable b) => Int -> a -> b -> Maybe String
selectExists' i tbl whr
 = "SELECT * FROM " +++ tbl +++
   (phpIndent i ++ "WHERE ") +++ whr

selectGeneric :: (Concatable a) =>
                 Int             -- ^ indentation
              -> (String,String) -- ^ (source field,source table)
              -> (String,String) -- ^ (target field,target table)
              -> a               -- ^ tables
              -> a               -- ^ the WHERE clause
              -> a
selectGeneric i src trg tbl whr
 = selectcl ++
   phpIndent i ++ "FROM " +>+ 
   (if toM whr==Just "1" then tbl else tbl+|+(phpIndent i ++ "WHERE "+>+whr))
   where  selectcl | snd src=="" && snd trg=="" = fatal 421 "Source and target are \"\", use selectExists' for this purpose"
                   | snd src==snd trg  = "SELECT DISTINCT " ++ selectSelItem src
                   | snd src==""       = "SELECT DISTINCT " ++ selectSelItem trg
                   | snd trg==""       = "SELECT DISTINCT " ++ selectSelItem src
                   | snd src=="tgt"    = "SELECT DISTINCT " ++ selectSelItem trg ++", "++selectSelItem src  -- added because of flipped expression (see ticket #342) 
                   | otherwise         = "SELECT DISTINCT " ++ selectSelItem src ++", "++selectSelItem trg
selectSelItem :: (String, String) -> String
selectSelItem (att,alias)
  | unquote (afterPoint att) == unquote alias = att
  | otherwise                                 = att++" AS "++alias
 where myafterPoint ('.':xs) = xs
       myafterPoint ( _ :xs) = myafterPoint xs
       myafterPoint []       = []
       afterPoint s = if myafterPoint s == "" then s else myafterPoint s

--WHY bestaat sqlRelPlugs?
-- | sqlRelPlugs levert alle mogelijkheden om een plug met twee velden te vinden waarin expressie e is opgeslagen.
-- | Als (plug,sf,tf) `elem` sqlRelPlugs fSpec e, dan geldt e = (fldexpr sf)~;(fldexpr tf)
-- | Als sqlRelPlugs fSpec e = [], dan volstaat een enkele tabel lookup niet om e te bepalen
sqlRelPlugs :: Fspc -> Expression  -> [(PlugSQL,SqlField,SqlField)] --(plug,source,target)
sqlRelPlugs fSpec e
   =  
     [ (plug,fld0,fld1)
     | InternalPlug plug<-plugInfos fSpec
     , (fld0,fld1)<-sqlPlugFields plug e
     ]
 
-- return table name and source and target column names for relation rel, or nothing if the relation is not found
getDeclarationTableInfo :: Fspc -> Declaration -> Maybe (PlugSQL,SqlField,SqlField)
getDeclarationTableInfo fSpec decl =
 case decl of 
   Sgn{} -> 
      case sqlRelPlugs fSpec (EDcD decl (sign decl)) of
            [plugInfo] -> Just plugInfo
            []         -> Nothing
            plugInfo:_ -> Just plugInfo -- fatal 62 $ "Multiple plugs for relation "++ show decl
                      -- TODO: currently this fatal is disabled because some relations return multiple plugs
                      --       (see ticket #217)
   _     -> fatal 420 $ "getDeclarationTableInfo must not be used on this type of declaration!"
--iff proven that e is equivalent to plugexpr
--   AND not proven that e is not equivalent to plugexpr
--then return (fld0,fld1)
--TODO -> can you prove for all e whether e is equivalent to plugexpr or not?
sqlPlugFields :: PlugSQL -> Expression  -> [(SqlField, SqlField)]
sqlPlugFields p e' =
    let e = disjNF e'
    in nub
        [(fld0,fld1)
        | fld0<-[f |f<-plugFields p,target (fldexpr f)==source e] --fld0 must be a field matching the source of e
        , fld1<-[f |f<-plugFields p,target (fldexpr f)==target e] --fld1 must be a field matching the target of e
        , let plugexpr = plugpath p fld0 fld1 --the smallest expression from fld0 to fld1 (both in same plug)
        , let se = fldexpr fld0
              te = fldexpr fld1
              bs = (isTrue.disjNF) (notCpl (sign e) e .\/. flp se .:. te)    --       e |- se~;te
              bt = (isTrue.disjNF) (notCpl (sign e) (flp se .:. te) .\/. e)  --       se~;te |- e
        , --reasons why e is equivalent to plugexpr:
           --because e and plugexpr are equal
           e==plugexpr
     -- || because1 e fld0 fld1              
     --OR e is equivalent to plugexpr for some other reason (requires reasoning)
        || bs && bt     ]                                          --       e = se~;te
        {- the above should be enough.. but the relation algebra calculations
           are not good enough yet. In particular:
             isFalse ((I/\x);e /\ -e)
           and
             isTrue  ((I/\e;e~);e \/ -e)
           do not work (these should yield True instead of False in both cases)
           
           The code below fixes exactly these ommissions
      --  
        || (isProp (se) && (te == e)
           && (isTrue$disjNF$ let c = source e in (iExpr c ./\. simplF [e,flp e] ) .\/. notCpl (Sign c c) se))
        || (isProp (te) && se==flp e
           && (isTrue$disjNF$ let c = source e in (iExpr c ./\. simplF [e,flp e] ) .\/. notCpl (Sign c c) te))
        -- found another exception:
        --     isFalse (I;I /\ -I)
        --   and
        --     isTrue  (I;I \/ -I)
        --   yield False, but should yield True
        --
        || (  (se == te) && isIdent e && (isSur se)  )
        , --TODO -> reasons why e is not equivalent to plugexpr:
        True 
        ]
  where
  -- simplF: replace a;a~ by I if INJ&TOT
  simplF ks = simplify ( if null fs || null (head fs) then replF ks else replF $ head fs )
    where fs = [ts | ECps ts <- [simplify $ ECps ks]] -- if null, replF will probably not do a lot.
  simplF ks = case simplify (foldr1 .:. ks) of
                 t@ECps{} -> simplify (replF (exprCps2list t))
                 _        -> simplify (replF ks)
           -- null occurs especialy in cases of [I;e] and [e;I]
  
  replF [k:k2]    | k == flp k2 && isInj k && isTot k = iExpr (source k)
  replF (k:k2:ks) | k == flp k2 && isInj k && isTot k = replF ks
  replF [a]                                           = a
  replF (k:k2:ks) | fs /= [k2:ks]
   = case res of ECps{} -> replF (exprCps2list res) ; _ -> ECps (k,res)
     where res = replF (k2:ks)
           fs  = case res of ECps{} -> [exprCps2list res] ; _ -> []

  replF [] -- this should not occur here, and if it does, it might cause errors in other code that should be solved here
   = fatal 542 "Could not define a properly typed I for ECps[] in replF in sqlPlugFields in Prototype/RelBinGenSQL.hs"
           -- this error does not guarantee, however, that simplF yields no ECps []. In particular: simplify (ECps [I;I]) == ECps []
  replF ks = ECps (ks)
  -----------------
  -}
  
-- | sqlExprSrc gives the quoted SQL-string that serves as the attribute name in SQL.
--   we want it to show the type, which is useful for readability. (Otherwise, just "SRC" and "TRG" would suffice)
sqlExprSrc :: Fspc->Expression -> String
sqlExprSrc _ expr = quote ("Src"++name (source expr))  -- The quotes are added just in case the result of (ses expr) happens to be an SQL reserved word.

-- | sqlExprTrg gives the quoted SQL-string that serves as the attribute name in SQL.
sqlExprTrg :: Fspc->Expression -> String
sqlExprTrg _ expr = quote ("Trg"++name (target expr))


-- sqlConcept gives the name of the plug that contains all atoms of A_Concept c.
sqlConcept :: Fspc -> A_Concept -> String
sqlConcept fSpec = quote.name.sqlConceptPlug fSpec
   
-- sqlConcept yields the plug that contains all atoms of A_Concept c. Since there may be more of them, the first one is returned.
sqlConceptPlug :: Fspc -> A_Concept -> PlugSQL
sqlConceptPlug fSpec c | c==ONE = fatal 583 "A_Concept ONE may not be represented in SQL."
                       | otherwise
             = if null ps then fatal 585 $ "A_Concept \""++show c++"\" does not occur in fSpec (sqlConcept in module DatabaseDesign.Ampersand_Prototype.RelBinGenSQL)" else
               head ps
               where ps = [plug |InternalPlug plug<-plugInfos fSpec
                                , not (null (case plug of ScalarSQL{} -> [c |c==cLkp plug]; _ -> [c' |(c',_)<-cLkpTbl plug, c'==c]))]


sqlAttConcept :: Fspc -> A_Concept -> String
sqlAttConcept fSpec c | c==ONE = "ONE"
                      | otherwise
             = if null cs then fatal 594 $ "A_Concept \""++show c++"\" does not occur in its plug in fSpec \""++appname++"\" (sqlAttConcept in module DatabaseDesign.Ampersand_Prototype.RelBinGenSQL)" else
               head cs
               where cs = [fldname f |f<-plugFields (sqlConceptPlug fSpec c), c'<-concs f,c==c']
                     appname =  name fSpec

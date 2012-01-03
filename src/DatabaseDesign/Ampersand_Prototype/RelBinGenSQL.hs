{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
module DatabaseDesign.Ampersand_Prototype.RelBinGenSQL
 (sqlRelPlugs,sqlExprTrg,sqlExprSrc,sqlPlugFields,sqlRelPlugNames,getRelationTableInfo,selectExpr,selectExprMorph,selectExprBrac,isOne,isOne'
 ) where 
import DatabaseDesign.Ampersand_Prototype.CoreImporter
import DatabaseDesign.Ampersand.Core.Poset (Poset(..))
import Prelude hiding (Ord(..))

import DatabaseDesign.Ampersand_Prototype.RelBinGenBasics (zipnum,Concatable(..),(+++),quote
                                 ,cChain,addSlashes,phpIndent)
import Data.Maybe
import Char(isDigit,digitToInt,intToDigit)
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
-- isOneExpr e' = (isUni.conjNF.ECps) [ERel (V (Sign (source e') (source e'))),e']
isOne' :: ObjectDef -> Bool
isOne' o = isOne o -- isOneExpr$contextOf o
                   --TODO: isOneExpr zorgt sowieso voor slechte select queries (doSqlGet), misschien kan deze wel weg.
                   --      isOneExpr (rev:771) kan in de problemen komen bij doSqlGet (error "line 578 Object.hs"). 
                   --      in dat geval komt isOne (huidige rev) ook in de problemen (error iets met keygroup)
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
selectExpr fSpec i src trg (EIsc lst'@(_:_:_))
 | sqlOk =  sqlcomment i ("case: (EIsc lst@(_:_:_))"++phpIndent (i+3)++"EIsc "++show (map showADL lst')) $
            selectGeneric i ("isect0."++src',src) ("isect0."++trg',trg)
                            (cChain ", " exprbracs) (cChain " AND " wherecl)
 | otherwise = fatal 71 "sqlOk does not hold"
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
   where sqlOk   = and (map isJust exprbracs')
         exprbracs=catMaybes exprbracs'
         src'    = sqlExprSrc fSpec fstm
         trgC    = sqlExprTrg fSpec fstm -- can collide with src', for example in case fst==r~;r, or if fst is a property (or identity)
         trg'    = noCollideUnlessTm' fstm [src'] trgC
         fstm    = head posTms  -- always defined, because length posTms>0 (ensured in definition of posTms)
         mp1Tm   = take 1 ([t | t@(ERel (Mp1{}))<-lst']++[t | t@(ECps ((ERel (Mp1{})):(ERel (V _)):(ERel (Mp1{})):[])) <- lst'])
         lst     = [t |t<-lst', not (elem t mp1Tm)]
         posTms  = if null posTms' then map notCpl (take 1 negTms') else posTms' -- we take a term out of negTms' if we have to, to ensure length posTms>0
         negTms  = if null posTms' then tail negTms' else negTms' -- if the first term is in posTms', don't calculate it here
         posTms' = [t | t<-lst, isPos t && not (isI t)]++[t | t<-lst, isPos t && isI t] -- the code to calculate I is better if it is not the first term
         negTms' = [notCpl t | t<-lst, isNeg t && isI t]++[notCpl t | t<-lst, isNeg t && not (isI t)] -- should a negTerm become a posTerm (for reasons described above), it can best be an -I.
         exprbracs' = [ case brc of
                         Just s->Just (s ++ " AS isect"++show n)
                         Nothing->Nothing
                      | (n,l)<-zipnum posTms
                      , src''<-[sqlExprSrc fSpec l]
                      , trg''<-[noCollideUnlessTm' l [src''] (sqlExprTrg fSpec l)]
                      , let brc = selectExprBrac fSpec i src'' trg'' l
                      ]
         wherecl   = [Just$ if isI l
                      then  "isect0."++src'++" = isect0."++trg' -- this is the code to calculate ../\I. The code below will work, but is longer
                      else "(isect0."++src'++" = isect"++show n++"."++src''
                      ++ " AND isect0."++trg'++" = isect"++show n++"."++trg''++")"
                     | (n,l)<-tail (zipnum posTms) -- not empty because of definition of posTms
                     , src''<-[sqlExprSrc fSpec l]
                     , trg''<-[noCollideUnlessTm' l [src''] (sqlExprTrg fSpec l)]
                     ]++
                     [Just$ "isect0."++src'++" = \\'"++relval r++"\\'" -- source and target are equal because this is the case with Mp1
                     | (ERel r@(Mp1{})) <- mp1Tm
                     ]++
                     [Just$ "isect0."++src'++" = \\'"++relval m1++"\\'" -- source and target are unequal
                       ++ " AND isect0."++trg'++" = \\'"++relval m2++"\\'" -- source and target are unequal
                     | (ECps ((ERel m1@(Mp1{})):(ERel (V _)):(ERel m2@(Mp1{})):[])) <- mp1Tm
                     ]++
                     [if isI l
                      then  Just ("isect0."++src'++" <> isect0."++trg') -- this code will calculate ../\-I
                      else  "NOT EXISTS ("+++(selectExists' (i+12)
                                                           ((selectExprBrac fSpec (i+12) src'' trg'' l) +++ " AS cp")
                                                           ("isect0."++src' ++ "=cp."++src''++" AND isect0."++ trg'++"=cp."++trg'')
                                         )+++")"
                     | (_,l)<-zipnum negTms
                     , src''<-[sqlExprSrc fSpec l]
                     , trg''<-[noCollideUnlessTm' l [src''] (sqlExprTrg fSpec l)]
                     ]++[Just$ "isect0."++src'++" IS NOT NULL",Just$ "isect0."++trg'++" IS NOT NULL"]
selectExpr fSpec i src trg (EIsc [e]) = sqlcomment i ("case: (EIsc [e])"++phpIndent (i+3)++"(EIsc [ \""++showADL e++"\" ])") $
                                         selectExpr fSpec i src trg e
-- Why not return Nothing?
-- Reason: EIsc [] should not occur in the query at all! If it does, we have made a mistake earlier.
selectExpr _     _ _   _   (EIsc [] ) = fatal 123 "Cannot create query for EIsc [] because it is wrong"
selectExpr _ i src trg (EUni [] ) = sqlcomment i "EUni []"$ toM$ selectGeneric i ("1",src) ("1",trg) ("(SELECT 1) AS a") ("0")
selectExpr fSpec i src trg (EUni es') = sqlcomment i ("case: EUni es"++phpIndent (i+3)++"EUni "++show (map showADL es')) $
                                        "(" +++ (selectExprInUnion fSpec i src trg (EUni es')) +++ (phpIndent i) ++ ")"
selectExpr _     _ _   _   (ECps [] ) = fatal 140 "Cannot create query for ECps [] because it is wrong"
selectExpr fSpec i src trg (ECps es@(ERel (V (Sign ONE _)):fs@(_:_)))
  = sqlcomment i ("case: (ECps (ERel (V (Sign ONE _)):fs@(_:_)))"++phpIndent (i+3)++"ECps  "++show (map showADL es)) $
    selectGeneric i ("1",src) ("fst."++trg',trg)
                    (selectExprBrac fSpec i src' trg' (ECps fs) +++ " AS fst")
                    (Just$ "fst."++trg'++" IS NOT NULL")
                    where src' = noCollideUnlessTm' (ECps fs) [trg'] (sqlExprSrc fSpec (ECps fs))
                          trg' = sqlExprTrg fSpec (ECps fs)
selectExpr fSpec i src trg (ECps e@(s1@(ERel (Mp1{})):(s2@(ERel (V _)):(s3@(ERel (Mp1{})):fx@(_:_))))) -- to make more use of the thing below
  = sqlcomment i ("case: (ECps (s1@(ERel (Mp1{})):(s2@(ERel (V _)):(s3@(ERel (Mp1{})):fx@(_:_)))))"++phpIndent (i+3)++"ECps "++show (map showADL e)) $
    selectExpr fSpec i src trg (ECps ((ECps (s1:s2:s3:[])):fx))

selectExpr _ i src trg (ECps e@((ERel sr@(Mp1{})):((ERel (V _)):((ERel tr@(Mp1{})):[])))) -- this will occur quite often because of doSubsExpr
  = sqlcomment i ("case: (ECps ((ERel sr@(Mp1{})):((ERel (V _)):((ERel tr@(Mp1{})):[]))))"++phpIndent (i+3)++"ECps "++show (map showADL e)) $
    Just$ "SELECT \\'"++relval sr++"\\' AS "++src++", \\'"++relval tr++"\\' AS "++trg

selectExpr fSpec i src trg (ECps es@(e@(ERel sr@(Mp1{})):f:fx))
   = sqlcomment i ("case: (ECps (ERel Mp1{}:f:fx))"++phpIndent (i+3)++"ECps "++show (map showADL es)) $
     selectGeneric i ("fst."++src',src) ("fst."++trg',trg)
                     (selectExprBrac fSpec i src' trg' (ECps (f:fx))+++" AS fst")
                     (Just$"fst."++src'++" = \\'"++relval sr++"\\'")
                     where src' = sqlExprSrc fSpec e
                           trg' = noCollideUnlessTm' (ECps (f:fx)) [src'] (sqlExprTrg fSpec (ECps (f:fx)))

selectExpr fSpec i src trg (ECps es@(e:ERel (V _):f:fx)) -- prevent calculating V in this case
 | src==trg && not (isProp e) = fatal 146 $ "selectExpr 2 src and trg are equal ("++src++") in "++showADL e
 | otherwise = sqlcomment i ("case: ECps (e:ERel (V _):f:fx)"++phpIndent (i+3)++"ECps "++show (map showADL es)) $
    selectGeneric i ("fst."++src',src) ("snd."++trg',trg)
                     ((selectExprBrac fSpec i src' mid' e)+++" AS fst,"++phpIndent (i+5)+++(selectExprBrac fSpec i mid2' trg' f)+++" AS snd")
                     ("fst."++src'+++" IS NOT NULL")
         where src' = sqlExprSrc fSpec e
               mid' = sqlExprTrg fSpec e
               mid2'= sqlExprSrc fSpec f
               trg' = noCollideUnlessTm' (ECps (f:fx)) [mid2'] (sqlExprTrg fSpec (ECps (f:fx)))
selectExpr fSpec i src trg (ECps    [e]       ) = selectExpr fSpec i src trg e
selectExpr fSpec i src trg (ECps es)  -- in this case, it is certain that there are at least two elements in es.
 = sqlcomment i ("case: (ECps es), with two or more elements in es."++phpIndent (i+3)++"ECps "++show (map showADL es)) $ Just $ phpIndent i++
   selectClause ++phpIndent i++
   fromClause   ++phpIndent i++
   whereClause 
{-  The ECps is treated as poles-and-fences.
    Imagine subexpressions as "fences". The source and target of a "fence" are the "poles" between which that "fence" is mounted.
    In this metaphor, we create the FROM-clause directly from the "fences", and the WHERE-clause from the "poles" between "fences".
    The "outer poles" correspond to the source and target of the entire expression.
    To prevents name conflicts in SQL, each subexpression is aliased in SQL by the name "ECps<n>".
-}
   where mainSrc = "ECps0."++selectSelItem (sqlSrc,src)
                   where (_,_,sqlSrc,_) = head fenceExprs
         mainTrg = "ECps"++show (length es-1)++"."++selectSelItem (sqlTrg,trg) 
                   where (_,_,_,sqlTrg) = last fenceExprs
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
                      | (n, expr) <- zip [(0::Int)..] es
                      , srcAtt<-[sqlExprSrc fSpec expr]
                      , trgAtt<-[noCollideUnlessTm' expr [srcAtt] (sqlExprTrg fSpec expr)]
                      , let Just sqlExpr = selectExprBrac fSpec i srcAtt trgAtt expr
                      ]
selectExpr fSpec i src trg (EFlp x)    = sqlcomment i ("case: EFlp x.") $
                                         selectExpr fSpec i trg src x

selectExpr fSpec i src trg (ERel (V (Sign s t))   ) 
 = sqlcomment i ("case: (ERel (V (Sign s t)))"++phpIndent (i+3)++"ERel [ \""++showADL (V (Sign s t))++"\" ]") $
   case [selectGeneric i (src',src) (trg',trg) tbls "1"
        | (s',src') <- concNames (if name s==name t then "cfst0" else quote (name s)) s
        , (t',trg') <- concNames (if name s==name t then "cfst1" else quote (name t)) t
        , let tbls = if length (s'++t') == 0 then "(SELECT 1) AS csnd" else intercalate ", " (s'++t')
        ] of
     []    -> fatal 216 $ "Problem in selectExpr (ERel (V (Sign \""++show s++"\" \""++show t++"\")))"
     sql:_ -> Just sql 
 where concNames pfx c = [([],"1") |c==ONE]++[([quote p ++ " AS "++pfx],pfx++"."++quote s') | (p,s',_) <- sqlRelPlugNames fSpec (ERel (I c))]

selectExpr fSpec i src trg (ERel (I ONE)) = sqlcomment i "I[ONE]"$ selectExpr fSpec i src trg (ERel (V (Sign ONE ONE)))

selectExpr fSpec i src trg (ERel mrph) = selectExprMorph fSpec i src trg mrph
selectExpr fSpec i src trg (EBrk expr) = selectExpr fSpec i src trg expr

selectExpr   _   i src trg (ECpl (ERel (V _))) = sqlcomment i "case: ECpl (ERel (V _))"$   -- yields empty
                                                 toM(selectGeneric i ("1",src) ("1",trg) ("(SELECT 1) AS a") ("0"))
selectExpr fSpec i src trg (ECpl e )
   = sqlcomment i ("case: ECpl e"++phpIndent (i+3)++"ECpl [ \""++showADL e++"\" ]") $
     selectGeneric i ("cfst."++src',src) ("csnd."++trg',trg)
                     (sqlConcept fSpec (source e) ++ " AS cfst,"++phpIndent (i+5)+++selectExprBrac fSpec (i+5) trg' trg' (ERel (I (target e)))+++" AS csnd")
                     ("NOT EXISTS"++phpIndent i++" ("+++
                        (selectExists' (i+2) ((selectExprBrac fSpec (i+2) src2 trg2 e) +++ " AS cp")
                                             ("cfst." ++ src' ++ "=cp."++src2++" AND csnd."++ trg'++"=cp."++trg2)
                        ) +++ ")"
                     )
                     where src' = quote$sqlAttConcept fSpec (source e) 
                           trg' = noCollide [src'] (sqlAttConcept fSpec (target e))
                           src2 = sqlExprSrc fSpec e
                           trg2 = noCollideUnlessTm' e [src2] (sqlExprTrg fSpec e)
selectExpr _ i _ _ (EKl0 _)
   = sqlcomment i "SQL cannot create closures EKl0" (Just "SELECT * FROM NotExistingKl0")
selectExpr _ i _ _ (EKl1 _)
   = sqlcomment i "SQL cannot create closures EKl1" (Just "SELECT * FROM NotExistingKl1")
selectExpr fSpec i src trg (EDif (ERel V{},x)) = sqlcomment i ("case: EDif V x"++phpIndent (i+3)++"EDif V ( \""++showADL x++"\" )") $
                                                 selectExpr fSpec i src trg (ECpl x) 

-- The following definitions express code generation of the remaining cases in terms of the previously defined generators.
-- As a result of this way of working, code generated for =, |-, -, !, *, \, and / may not be efficient, but at least it is correct.
selectExpr fSpec i src trg (EEqu (l,r))
 =  sqlcomment i ("case: (EEqu (l,r))"++phpIndent (i+3)++"EEqu ("++showADL l++", "++showADL r++")") $
    selectExpr fSpec i src trg (EIsc [EUni [ECpl l,r],EUni [ECpl r,l]])
selectExpr fSpec i src trg (EImp (l,r))
 =  sqlcomment i ("case: (EImp (l,r))"++phpIndent (i+3)++"EImp ("++showADL l++", "++showADL r++")") $
    selectExpr fSpec i src trg (EUni [ECpl l,r])
selectExpr fSpec i src trg (EDif (l,r))
 =  sqlcomment i ("case: (EDif (l,r))"++phpIndent (i+3)++"EDif ("++showADL l++", "++showADL r++")") $
    selectExpr fSpec i src trg (EIsc [l,ECpl r])
selectExpr fSpec i src trg (ERrs (l,r))
 =  sqlcomment i ("case: (ERrs (l,r))"++phpIndent (i+3)++"ERrs ("++showADL l++", "++showADL r++")") $
    selectExpr fSpec i src trg (ERad [ECpl (EFlp l),r])
selectExpr fSpec i src trg (ELrs (l,r))
 =  sqlcomment i ("case: (ELrs (l,r))"++phpIndent (i+3)++"ELrs ("++showADL l++", "++showADL r++")") $
    selectExpr fSpec i src trg (ERad [l,ECpl (EFlp r)])
selectExpr _     _ _   _   (ERad [] )  = fatal 310 "Cannot create query for ERad []. This should never occur."
selectExpr fSpec i src trg (ERad [e])  = selectExpr fSpec i src trg e
selectExpr fSpec i src trg (ERad es)   = sqlcomment i ("case: ERad es@(_:_:_)"++phpIndent (i+3)++"ERad "++show (map showADL es)) $
                                         (selectExpr fSpec i src trg . ECpl . ECps . map notCpl) es
selectExpr _     _ _   _   (EPrd [] )  = fatal 310 "Cannot create query for ERad []. This should never occur."
selectExpr fSpec i src trg (EPrd [e])  = selectExpr fSpec i src trg e
selectExpr fSpec i src trg (EPrd es)   = sqlcomment i ("case: EPrd es@(_:_:_)"++phpIndent (i+3)++"EPrd "++show (map showADL es)) $
                                         selectExpr fSpec i src trg (ECps [l,v,r])
                                         where l = head es
                                               r = last es
                                               v = ERel (V (Sign (target l) (source r)))
selectExpr fSpec i src trg (ETyp x _)  = sqlcomment i ("case: ETyp x _"++phpIndent (i+3)++"ETyp ( \""++showADL x++"\" ) _") $
                                         selectExpr fSpec i src trg x
--selectExpr _     _ _   _   x           = fatal 332 ("Cannot create query for "++showADL x)

-- selectExprInUnion is om de recursie te verbergen (deze veroorzaakt sql fouten)
selectExprInUnion :: Fspc
                  -> Int
                  -> String
                  -> String
                  -> Expression 
                  -> Maybe String
selectExprInUnion fSpec i src trg (EBrk  e        ) =  selectExprInUnion fSpec i src trg e
selectExprInUnion fSpec i src trg (ECps [e]       ) =  selectExprInUnion fSpec i src trg e
selectExprInUnion fSpec i src trg (EIsc [e]       ) =  selectExprInUnion fSpec i src trg e
selectExprInUnion fSpec i src trg (EUni [e]       ) =  selectExprInUnion fSpec i src trg e
selectExprInUnion fSpec i src trg (EUni (e:(f:fx))) = (selectExprInUnion fSpec i src trg e) +++ (phpIndent i) ++ ") UNION (" +++ (selectExprInUnion fSpec i src trg (EUni (f:fx))) +++ (phpIndent i) ++ ""
selectExprInUnion fSpec i src trg e                 =  selectExpr        fSpec (i+4) src trg e

selectExprBrac :: Fspc
               -> Int         -- ^ indentation
               -> String      -- ^ source name (preferably quoted)
               -> String      -- ^ target name (preferably quoted)
               -> Expression  -- ^ Whatever expression to generate an SQL query for
               -> Maybe String
selectExprBrac    f i s@(_:_)   t         e | head s /= '`' = selectExprBrac f i (quote s) t         e
selectExprBrac    f i s         t@(_:_)   e | head t /= '`' = selectExprBrac f i s         (quote t) e
selectExprBrac fSpec i src trg (EBrk  e )                   = selectExprBrac fSpec i src trg e
selectExprBrac fSpec i src trg (ECps [e])                   = selectExprBrac fSpec i src trg e
selectExprBrac fSpec i src trg (ERad [e])                   = selectExprBrac fSpec i src trg e
selectExprBrac fSpec i src trg (EIsc [e])                   = selectExprBrac fSpec i src trg e
selectExprBrac fSpec i src trg (EUni [e])                   = selectExprBrac fSpec i src trg e
selectExprBrac fSpec i src trg e@(ERel{})
 = listToMaybe ([quote$p |(p,s,t)<-sqlRelPlugNames fSpec e,quote s==quote src,quote t==quote trg]
             ++ maybeToList ("( " +++selectExpr fSpec (i+2) src trg e+++" )"))
selectExprBrac fSpec i src trg expr
 = phpIndent (i+5) ++ "( " +++ selectExpr fSpec (i+7) src trg expr+++ phpIndent(i+5)++")"

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

noCollideUnlessTm' (ERel _) _ nm = quote nm
noCollideUnlessTm' _  names nm = noCollide' names nm

selectExprMorph :: Fspc
                -> Int
                -> String -- ^ source
                -> String -- ^ target
                -> Relation
                -> Maybe String

selectExprMorph fSpec i src trg rel@V{}
 = selectGeneric i (src',src) (trg',trg)
                   (sqlConcept fSpec (source rel) +++ " AS vfst, "++sqlConcept fSpec (target rel)++ " AS vsnd")
                   (src'+++" IS NOT NULL AND "++trg'++" IS NOT NULL")
 where src'="vfst."++sqlAttConcept fSpec (source rel)
       trg'="vsnd."++sqlAttConcept fSpec (target rel)
selectExprMorph _ _ src trg rel@Mp1{}
 | src == ""&&trg=="" = fatal 394 "Source and target are \"\", use selectExists' for this purpose"
 | src == ""  = Just$ "SELECT \\'"++relval rel++"\\' AS "++trg
 | trg == ""  = Just$ "SELECT \\'"++relval rel++"\\' AS "++src
 | src == trg = Just$ "SELECT \\'"++relval rel++"\\' AS "++src
 | otherwise  = Just$ "SELECT \\'"++relval rel++"\\' AS "++src++", \\'"++relval rel++"\\' AS "++trg
selectExprMorph fSpec i src trg rel -- made for both Rel and I
 = case sqlRelPlugNames fSpec (ERel rel) of
     []        -> fatal 344 $ "No plug for relation "++show rel
     (p,s,t):_ -> Just $ selectGeneric i (quote s,src) (quote t,trg) (quote p) (quote s++" IS NOT NULL AND "++quote t++" IS NOT NULL")
  -- TODO: "NOT NULL" checks could be omitted if column is non-null, but the
  -- code for computing table properties is currently unreliable.
               
selectExists' :: (Concatable a,Concatable b) => Int -> a -> b -> (Maybe String)
selectExists' i tbl whr
 = ("SELECT * FROM ") +++ tbl +++
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
                   | snd src==""   = "SELECT DISTINCT " ++ selectSelItem trg
                   | snd trg==""   = "SELECT DISTINCT " ++ selectSelItem src
                   | otherwise = "SELECT DISTINCT " ++ selectSelItem src ++", "++selectSelItem trg
selectSelItem :: (String, String) -> String
selectSelItem (att,alias)
  | unquote (afterPoint att) == unquote alias = att
  | otherwise                                 = att++" AS "++alias
 where myafterPoint ('.':xs) = xs
       myafterPoint ( _ :xs) = myafterPoint xs
       myafterPoint []       = []
       afterPoint s = if (myafterPoint s == "") then s else myafterPoint s

--WHY bestaat sqlRelPlugs?
-- | sqlRelPlugs levert alle mogelijkheden om een plug met twee velden te vinden waarin expressie e is opgeslagen.
-- | Als (plug,sf,tf) `elem` sqlRelPlugs fSpec e, dan geldt e = (fldexpr sf)~;(fldexpr tf)
-- | Als sqlRelPlugs fSpec e = [], dan volstaat een enkele tabel lookup niet om e te bepalen
sqlRelPlugs :: Fspc -> Expression  -> [(PlugSQL,SqlField,SqlField)] --(plug,source,target)
sqlRelPlugs fSpec e
 = [ (plug,fld0,fld1)
   | InternalPlug plug<-plugInfos fSpec
   , (fld0,fld1)<-sqlPlugFields plug e
   ]

sqlRelPlugNames :: Fspc -> Expression  -> [(String,String,String)] --(plug,source,target)
sqlRelPlugNames f e = [(name p,fldname s,fldname t) |(p,s,t)<-sqlRelPlugs f e]

-- return table name and source and target column names for relation rel, or nothing if the relation is not found
getRelationTableInfo :: Fspc -> Relation -> Maybe (String,String,String)
getRelationTableInfo fSpec rel = case sqlRelPlugNames fSpec (ERel rel) of
                                   [plugInfo] -> Just plugInfo
                                   []         -> Nothing
                                   plugInfo:_ -> Just plugInfo -- fatal 62 $ "Multiple plugs for relation "++ show rel
                                   -- TODO: currently this fatal is disabled because some relations return multiple plugs
                                   --       (see ticket #217)

--iff proven that e is equivalent to plugexpr
--   AND not proven that e is not equivalent to plugexpr
--then return (fld0,fld1)
--TODO -> can you prove for all e whether e is equivalent to plugexpr or not?
sqlPlugFields :: PlugSQL -> Expression  -> [(SqlField, SqlField)]
sqlPlugFields p e'
  = let e= disjNF e'
    in
    nub [(fld0,fld1)
        | fld0<-[f |f<-tblfields p,target (fldexpr f)==source e] --fld0 must be a field matching the source of e
        , fld1<-[f |f<-tblfields p,target (fldexpr f)==target e] --fld1 must be a field matching the target of e
        , let plugexpr = plugpath p fld0 fld1 --the smallest expression from fld0 to fld1 (both in same plug)
        , let se = fldexpr fld0
              te = fldexpr fld1
              bs = (isTrue.disjNF) (EUni [ECpl e, ECps [flp se,te] ])  --       e |- se~;te
              bt = (isTrue.disjNF) (EUni [ECpl (ECps [flp se,te]),e])  --       se~;te |- e
        , --reasons why e is equivalent to plugexpr:
           --because e and plugexpr are equal
           show e==show plugexpr
     -- || because1 e fld0 fld1              
        || --OR e is equivalent to plugexpr for some other reason (requires reasoning)
           bs && bt                                               --       e = se~;te
        {- the above should be enough.. but the relation algebra calculations
           are not good enough yet. In particular:
             isFalse ((I/\x);e /\ -e)
           and
             isTrue  ((I/\e;e~);e \/ -e)
           do not work (these should yield True instead of False in both cases)
           
           The code below fixes exactly these ommissions
        -}
        || (isProp (se) && (te == e)
           && (isTrue$disjNF$EUni [EIsc [ ERel (I (source e)), simplF [e,flp e] ]
                                ,ECpl$se]))
        || (isProp (te) && (se == flp e)
           && (isTrue$disjNF$EUni [EIsc [ ERel (I (source e)), simplF [flp e,e] ]
                                ,ECpl$te]))
        {- found another exception:
             isFalse (I;I /\ -I)
           and
             isTrue  (I;I \/ -I)
           yield False, but should yield True
        -}
        || (  (se == te) && isIdent e
           && (isSur se)
          )
        , --TODO -> reasons why e is not equivalent to plugexpr:
        True 
        ]
  where
  {- is because1 obsolete? Please remove if it is, otherwise explain why we need it in the future...
  because1 e fld0 fld1=
     --if e=r;m1;s;m2;t 
     --   where
     --   m1 and m2 are morphisms in p
     --   optional r, s and t are compositions of (fldexpr kernelfield)s of p (i.e. at least uni+inj(+sur or tot))
     --           (note: fldexpr kernelfields are assumed to be morphisms)
     --   r  is stored in p from fldx to fldr (maybe fldx==fldr i.e. r=I)
     --   m1 is stored in p from fldr to fld1 (maybe fldr==fld1 i.e. m1=I)
     --   s  is stored in p from fld1 to flds (maybe fld1==flds i.e. s=I)
     --   m2 is stored in p from flds to fld2 (maybe flds==fld2 i.e. m2=I) 
     --   t  is stored in p from fld2 to fldt (maybe fld2==fldt i.e. t=I) 
     --   if p is TblSQL then
     --      m1 and m2 are at least uni
     --      r = plugpath p fldx fldr
     --      s = plugpath p fld1 flds
     --      t = plugpath p fld2 fldt
     --   if p is BinSQL then 
     --      r,s,t = I because BinSQL has no kernel
     --      m1==m2~ (assuming that BinSQL stores at exactly one morphism and no concepts)
     --then
     --   plugpath p fldx fldt =  r;m1;m2;t = r;m1;s;m2;t (TODO)  
     --   plugpath p fldr fldt =  m1;m2;t   =   m1;s;m2;t (TODO)
     --   plugpath p fldx fld2 =  r;m1;m2   = r;m1;s;m2   (TODO)
     --   plugpath p fldr fld2 == m1;m2     =   m1;s;m2   (only m1;m2 IMPLEMENTED)
     case e of 
       ECps [m1,m2] -> let fldrs=map fst (sqlPlugFields p m1)
                        fld2s=map snd (sqlPlugFields p m2)
                     in elem fld0 fldrs && elem fld1 fld2s
       _ -> False   
  -}
  -- simplF: replace a;a~ by I if INJ&TOT
  simplF ks = simplify ( if null fs || null (head fs) then replF ks else replF $ head fs )
    where fs = [ts | ECps ts <- [simplify $ ECps ks]] -- if null, replF will probably not do a lot.
           -- null occurs especialy in cases of [I;e] and [e;I]
  replF (k:k2:ks) | k == flp k2 && isInj k && isTot k
         = if null ks then ERel(I$source k) else replF ks
  replF [a] = ECps [a]
  replF (k:k2:ks) | fs /= [k2:ks] -- ie: if something is replaced by replF
    = if null fs then ECps [k,res] else replF (k:head fs) -- we might replace something again!
    where res = replF (k2:ks)
          fs  = [ts | ECps ts <- [res]]
  replF [] -- this should not occur here, and if it does, it might cause errors in other code that should be solved here
   = fatal 542 "Could not define a properly typed I for ECps[] in replF in sqlPlugFields in Prototype/RelBinGenSQL.hs"
           -- this error does not guarantee, however, that simplF yields no ECps []. In particular: simplify (ECps [I;I]) == ECps []
  replF ks = ECps (ks)
  -----------------
  
-- | sqlExprSrc gives the quoted SQL-string that serves as the attribute name in SQL.
--   we want it to show the type, which is useful for readability. (Otherwise, just "SRC" and "TRG" would suffice)
--   It is not clear why the recursion over expressions is repeated here...
--   WHY not ask for the type of the expression and assemble a nice name? (TODO)
sqlExprSrc :: Fspc->Expression -> String
sqlExprSrc fSpec expr = quote (ses expr)  -- The quotes are added just in case the result of (ses expr) happens to be an SQL reserved word.
 where
   ses (EEqu (l,_)) = ses l
   ses (EImp (l,_)) = ses l
   ses (EIsc [])    = fatal 560 (if expr==ECps[] then "calling sqlExprSrc (EIsc [])" else "evaluating (EIsc []) in sqlExprSrc ("++showADL expr++")")
   ses (EIsc [f])   = ses f
   ses (EIsc fs)    = ses (head fs) --all subexprs have the same type --was:(head (filter l fs)) where  l = (==foldr1 lub (map source fs)).source
   ses (EUni [])    = fatal 557 (if expr==ECps[] then "calling sqlExprSrc (EUni [])" else "evaluating (EUni []) in sqlExprSrc ("++showADL expr++")")
   ses (EUni [f])   = ses f
   ses (EUni fs)    = ses (head fs) --all subexprs have the same type --was: (head (filter l fs)) where  l = (==foldr1 lub (map source fs)).source
   ses (EDif (l,_)) = ses l
{- It's not obvious for ELrs and ERrs. As a help to motivate ses, here are the type definitions (copied from sign::Expression)
 sign (ELrs (l,r))   = if target l `comparable` target r
                     then Sign (source l) (source r)
                     else fatal 252 $ "type checker failed to verify "++show (ELrs (l,r))++"."
 sign (ERrs (l,r))   = if source l `comparable` source r
                     then Sign (target l) (target r)
                     else fatal 255 $ "type checker failed to verify "++show (ERrs (l,r))++"."
-}
   ses (ELrs (l,_)) = ses l
   ses (ERrs (_,r)) = sqlExprTrg fSpec r
   ses (ECps [])    = fatal 554 (if expr==ECps[] then "calling sqlExprSrc (ECps [])" else "evaluating (ECps []) in sqlExprSrc ("++showADL expr++")")
   ses (ECps [f])   = ses f
   ses (ECps fs)    = ses (head fs)
   ses (ERad [])    = fatal 563 (if expr==ERad[] then "calling sqlExprSrc (ERad [])" else "evaluating (ERad []) in sqlExprSrc ("++showADL expr++")")
   ses (ERad [f])   = ses f
   ses (ERad fs)    = ses (head fs)
   ses (EPrd [])    = fatal 602 (if expr==EPrd[] then "calling sqlExprSrc (EPrd [])" else "evaluating (EPrd []) in sqlExprSrc ("++showADL expr++")")
   ses (EPrd [f])   = ses f
   ses (EPrd fs)    = ses (head fs)
   ses (ECpl e)     = ses e
   ses (EKl0 e)     = ses e
   ses (EKl1 e)     = ses e
   ses (EBrk e)     = ses e
   ses (ETyp e _)   = ses e
   ses (ERel r)     = case r of
                      Mp1{} -> "Mp"++(name (rel1typ r))
                      V{} -> ses (ERel I{rel1typ=source r})
                      _ -> head ([s |(_,s,_)<-sqlRelPlugNames fSpec (ERel r)]++[show r])
   ses (EFlp (ERel r))
                    = case r of
                      Mp1{} -> "Mp"++(name (rel1typ r))
                      V{} -> ses (ERel I{rel1typ=target r})
                      _ -> head ([t |(_,_,t)<-sqlRelPlugNames fSpec (ERel r)]++[show r])
   ses (EFlp e)     = ses (flp e)
-- | sqlExprTrg gives the quoted SQL-string that serves as the attribute name in SQL.
sqlExprTrg :: Fspc->Expression -> String
sqlExprTrg fSpec e = sqlExprSrc fSpec (flp e)


-- sqlConcept gives the name of the plug that contains all atoms of A_Concept c.
sqlConcept :: Fspc -> A_Concept -> String
sqlConcept fSpec c = (quote.name.sqlConceptPlug fSpec) c
   
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
               where cs = [fldname f |f<-tblfields (sqlConceptPlug fSpec c), c'<-concs f,c==c']
                     appname =  name fSpec

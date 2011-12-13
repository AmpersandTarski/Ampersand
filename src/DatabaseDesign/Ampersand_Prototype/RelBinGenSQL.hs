{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
module DatabaseDesign.Ampersand_Prototype.RelBinGenSQL
 (sqlRelPlugs,sqlExprTrg,sqlExprSrc,sqlPlugFields,sqlRelPlugNames,selectExpr,selectExprBrac,isOne,isOne',InPlug(..),showsql,SqlSelect(..)
 ) where 
import DatabaseDesign.Ampersand_Prototype.CoreImporter
import DatabaseDesign.Ampersand.Core.Poset (Poset(..))
import Prelude hiding (Ord(..))

import DatabaseDesign.Ampersand_Prototype.RelBinGenBasics (zipnum,Concatable(..),(+++),quote
                                 ,cChain,filterEmpty,phpIndent)
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
sqlcomment i cmt (Just sql) = Just ("/* "++cmt++" */"++phpIndent i++sql)

{- selectExpr translates an Expression (which is in Ampersand's A-structure) into an SQL expression in textual form.
-}
selectExpr ::    Fspc       -- current context
              -> Int        -- indentation
              -> String     -- SQL name of the source of this expression, as assigned by the environment 
              -> String     -- SQL name of the target of this expression, as assigned by the environment
              -> Expression -- expression to be translated
              -> Maybe String     -- resulting SQL expression
-- quote the attributes (such that column-names such as `Right` or `in` won't yield errors)
selectExpr fSpec i src@(_:_) trg       e | head src /= '`'
 = selectExpr fSpec i ('`':src++"`") trg            e
selectExpr fSpec i src       trg@(_:_) e | head trg /= '`'
 = selectExpr fSpec i src            ('`':trg++"`") e

--TODO
selectExpr fSpec i src trg (EIsc lst'@(_:_:_))
 | sqlOk =  sqlcomment i ("matches pattern: (EIsc lst@(_:_:_))"++phpIndent (i+3)++"EIsc "++show (map showADL lst')) $
            selectGeneric i ("isect0."++src',src) ("isect0."++trg',trg)
                            (cChain ", " exprbracs) (cChain " AND " wherecl)
 | otherwise = Nothing
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
         src'    = quote$sqlExprSrc fSpec fstm
         trgC    = quote$sqlExprTrg fSpec fstm -- can collide with src', for example in case fst==r~;r, or if fst is a property (or identity)
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
                      , src''<-[quote$sqlExprSrc fSpec l]
                      , trg''<-[noCollideUnlessTm' l [src''] (quote$sqlExprTrg fSpec l)]
                      , let brc = selectExprBrac fSpec i src'' trg'' l
                      ]
         wherecl   = [Just$ if isI l
                      then  "isect0."++src'++" = isect0."++trg' -- this is the code to calculate ../\I. The code below will work, but is longer
                      else "(isect0."++src'++" = isect"++show n++"."++src''
                      ++ " AND isect0."++trg'++" = isect"++show n++"."++trg''++")"
                     | (n,l)<-tail (zipnum posTms) -- not empty because of definition of posTms
                     , src''<-[quote$sqlExprSrc fSpec l]
                     , trg''<-[noCollideUnlessTm' l [src''] (quote$sqlExprTrg fSpec l)]
                     ]++
                     [Just$ "isect0."++src'++" = "++relval r -- sorce and target are equal because this is the case with Mp1
                     | (ERel r@(Mp1{})) <- mp1Tm
                     ]++
                     [Just$ "isect0."++src'++" = "++relval m1 -- sorce and target are unequal
                       ++ " AND isect0."++trg'++" = "++relval m2 -- sorce and target are unequal
                     | (ECps ((ERel m1@(Mp1{})):(ERel (V _)):(ERel m2@(Mp1{})):[])) <- mp1Tm
                     ]++
                     [if isI l
                      then  Just ("isect0."++src'++" <> isect0."++trg') -- this code will calculate ../\-I
                      else  "NOT EXISTS ("+++(selectExists' (i+12)
                                                           ((selectExprBrac fSpec (i+12) src'' trg'' l) +++ " AS cp")
                                                           ("isect0."++src' ++ "=cp."++src''++" AND isect0."++ trg'++"=cp."++trg'')
                                         )+++")"
                     | (_,l)<-zipnum negTms
                     , src''<-[quote$sqlExprSrc fSpec l]
                     , trg''<-[noCollideUnlessTm' l [src''] (quote$sqlExprTrg fSpec l)]
                     ]++[Just$ "isect0."++src'++" IS NOT NULL",Just$ "isect0."++trg'++" IS NOT NULL"]
selectExpr fSpec i src trg (EIsc [e]) = sqlcomment i ("matches pattern: (EIsc [e])"++phpIndent (i+3)++"(EIsc [ \""++showADL e++"\" ])") $
                                         selectExpr fSpec i src trg e
-- Why not return Nothing?
-- Reason: EIsc [] should not occur in the query at all! If it does, we have made a mistake earlier.
selectExpr _     _ _   _   (EIsc [] ) = fatal 123 "Cannot create query for EIsc [] because it is wrong"

selectExpr fSpec i src trg (ECps es@(ERel (V (Sign ONE _)):fs@(_:_)))
  = sqlcomment i ("matches pattern: (ECps (ERel (V (Sign ONE _)):fs@(_:_)))"++phpIndent (i+3)++"ECps  "++show (map showADL es)) $
    selectGeneric i ("1",src) ("fst."++trg',trg)
                    (selectExprBrac fSpec i src' trg' (ECps fs) +++ " AS fst")
                    (Just$ "fst."++trg'++" IS NOT NULL")
                    where src' = noCollideUnlessTm' (ECps fs) [trg'] (quote$sqlExprSrc fSpec (ECps fs))
                          trg' = quote$sqlExprTrg fSpec (ECps fs)
selectExpr fSpec i src trg (ECps e@(s1@(ERel (Mp1{})):(s2@(ERel (V _)):(s3@(ERel (Mp1{})):fx@(_:_))))) -- to make more use of the thing below
  = sqlcomment i ("matches pattern: (ECps (s1@(ERel (Mp1{})):(s2@(ERel (V _)):(s3@(ERel (Mp1{})):fx@(_:_)))))"++phpIndent (i+3)++"ECps "++show (map showADL e)) $
    selectExpr fSpec i src trg (ECps ((ECps (s1:s2:s3:[])):fx))

selectExpr _ i src trg (ECps e@((ERel sr@(Mp1{})):((ERel (V _)):((ERel tr@(Mp1{})):[])))) -- this will occur quite often because of doSubsExpr
  = sqlcomment i ("matches pattern: (ECps ((ERel sr@(Mp1{})):((ERel (V _)):((ERel tr@(Mp1{})):[]))))"++phpIndent (i+3)++"ECps "++show (map showADL e)) $
    Just$ "SELECT "++relval sr++" AS "++src++", "++relval tr++" AS "++trg

selectExpr fSpec i src trg (ECps es@(e@(ERel sr@(Mp1{})):f:fx))
   = sqlcomment i ("matches pattern: (ECps (ERel Mp1{}:f:fx))"++phpIndent (i+3)++"ECps "++show (map showADL es)) $
     selectGeneric i ("fst."++src',src) ("fst."++trg',trg)
                     (selectExprBrac fSpec i src' trg' (ECps (f:fx))+++" AS fst")
                     (Just$"fst."++src'++" = "++relval sr)
                     where src' = quote$sqlExprSrc fSpec e
                           trg' = noCollideUnlessTm' (ECps (f:fx)) [src'] (quote$sqlExprTrg fSpec (ECps (f:fx)))

selectExpr fSpec i src trg (ECps es@(e:ERel (V _):f:fx)) -- prevent calculating V in this case
 | src==trg && not (isProp e) = fatal 146 $ "selectExpr 2 src and trg are equal ("++src++") in "++showADL e
 | otherwise = sqlcomment i ("matches pattern: ECps (e:ERel (V _):f:fx)"++phpIndent (i+3)++"ECps "++show (map showADL es)) $
    selectGeneric i ("fst."++src',src) ("snd."++trg',trg)
                     ((selectExprBrac fSpec i src' mid' e)+++" AS fst,"++phpIndent (i+5)+++(selectExprBrac fSpec i mid2' trg' f)+++" AS snd")
                     ("fst."++src'+++" IS NOT NULL")
         where src' = quote$sqlExprSrc fSpec e
               mid' = quote$sqlExprTrg fSpec e
               mid2'= quote$sqlExprSrc fSpec f
               trg' = noCollideUnlessTm' (ECps (f:fx)) [mid2'] (quote$sqlExprTrg fSpec (ECps (f:fx)))
selectExpr fSpec i src trg (ECps    [e]       ) = selectExpr fSpec i src trg e
selectExpr fSpec i src trg (ECps es@(fstm:_:_))
 = sqlcomment i ("matches pattern: (ECps lst@(fstm:_:_))"++phpIndent (i+3)++"ECps "++show (map showADL es)) $
   selectGeneric i (mainSrc,src) (mainTrg,trg)
                        (cChain ", " (concExprs++exprbracs)) (cChain (phpIndent i++"  AND ") wherecl)
{-  De ECps gedraagt zich als een join. Het is dus zaak om enigszins efficiente code te genereren.
    Dat doen we door de complement-operatoren van de elementen uit es te betrekken in de codegeneratie.
    De concepten in es noemen we c0, c1, ... cn (met n de lengte van es)
    De elementen in es zelf noemen we F0, F1, ... ECps(n-1).
    Deze namen worden aangehouden in de SQL-aliasing. Dat voorkomt naamconflicten op een wat ruwe manier, maar wel overzichtelijk en effectief.
-}
   where mainSrc = (if isNeg (head es) then "c" else "ECps")++"0."++src'
         mainTrg = (if isNeg (last es) then "c"++show (length es) else "ECps"++show (length es-1))++"."++trg'
         -- the strings that represent source and target of ECps es. When their names are equal, disambiguation of names happens in noCollideUnlessTm'
         src'    = if isNeg (head es)
                   then (quote.sqlExprSrc fSpec.ERel . I .source.head) es
                   else (quote.sqlExprSrc fSpec.head) es
         trg'    = if isNeg (last es)
                   then (quote.sqlExprTrg fSpec.ERel . I .target.last) es
                   else (quote.sqlExprTrg fSpec.last) es
         -- poleConcepts lists the concepts that come in between those subexpressions that are used in the FROM clause of the SQL-expression.
         -- These intermediate concepts are called 'pole concepts', because they represent the poles in the poles-and-fences metaphor.
         poleConcepts
                 = [ pole 0 (source (head es)) | isNeg (head es) ] ++                                      
                   [ pole n c
                   | (leftExpr,(n,rightExpr))<-zip (init es) (tail (zipnum es))
                   , isNeg leftExpr && isNeg rightExpr
                   , let c=if target leftExpr<=source rightExpr then target leftExpr else source rightExpr
                   ]++
                   [ pole (length es) (target (last es)) | isNeg (last es) ]
                   where
                   -- each "pole" consists of a serial number, concept, and two SQL code fragments.
                    pole n c  = ( n                                                                       -- the serial number of this pole,  
                                , c                                                                       -- the type of this pole            
                                , selectExprBrac fSpec i sqlAtt sqlAtt (ERel (I c)) +++ " AS "++concNm n  -- the SQL expression of this pole
                                , quote$sqlExprSrc fSpec (ERel (I c))                                     -- the string that serves as attribute name in SQL
                                ) where sqlAtt = quote$sqlExprSrc fSpec (ERel (I c))
         -- the SQL-expressions for the intermediate concepts, maar nu in SQL
         concExprs = [e | (_,e,_)<-concExpr]
         concExpr  = [ (n,selectExprBrac fSpec i srcE srcE trgE +++ " AS "++concNm n, srcE)
                     | (n,c,sqlExpr,sqlAtt)<-poleConcepts, trgE<-[ERel (I c)], srcE<-[quote$sqlExprSrc fSpec trgE] ]
         concTp n  = head ([t | (i',_,t)<-concExpr, n==i']++fatal 182 "concTp")
         concNm n  = head (["c"++show n | (i',_,_)<-concExpr, n==i']++fatal 183 "concNm")
         -- de SQL-expressies voor de elementen uit es, die elk een Ampersand expressie representeren
         exprbracs = [e | (_,e,_,_)<-exprbrac]
         exprbrac  = [ (n,selectExprBrac fSpec i src'' trg'' l +++ " AS "++exprNm n , src'' , trg'' )
                     | (n,l)<-zipnum es
                     , not (isNeg l)
                     , src''<-[quote$sqlExprSrc fSpec l]
                     , trg''<-[noCollideUnlessTm' l [src''] (quote$sqlExprTrg fSpec l)]
                     ]
         exprS n  = head ([s | (i',_,s,_)<-exprbrac, n==i']++fatal 192 "exprS")  -- source type
         exprT n  = head ([t | (i',_,_,t)<-exprbrac, n==i']++fatal 193 "exprT")  -- target type
         exprNm n = head (["ECps"++show n | (i',_,_,_)<-exprbrac, n==i']++fatal 194 "exprNm")
         -- the 'where'  expressions contain all "magic".
         wherecl  = filterEmpty
                     [ if isNeg l
                       then "NOT EXISTS/**/"++phpIndent (i+6)++"( "+++
                              selectExists' (i+8) (selectExprBrac fSpec (i+6) src'' trg'' (notCpl l) +++ " AS ECps"++show n)
                                                  (cChain "  AND/**/ " ([ concNm  n   ++"."++concTp n   ++ "=ECps"++show n      ++"."++src''         | inCs n ]++
                                                                    [ exprNm (n-1)++"."++exprT (n-1)++ "=ECps"++show n      ++"."++src''         | n>0, not (inCs n) ]++
                                                                    [ "ECps"++show n ++"."++trg''       ++ "=" ++concNm (n+1)++"."++concTp (n+1) | inCs (n+1) ]++
                                                                    [ "ECps"++show n ++"."++trg''       ++ "=" ++exprNm (n+1)++"."++exprS (n+1)  | not (inCs (n+1)) ]++
                                                                    []))
                                                     +++")"
                       else cChain "  AND " (["c"++show n++"."++src'' ++ "=ECps"++show  n   ++"."++src'' | inCs n]++
                                             ["ECps"++show n++"."++trg'' ++ "=c"++show (n+1)++"."++trg'' | inCs (n+1)])
                     | (n,l)<-zipnum es
                     , src''<-[quote$sqlExprSrc fSpec l]
                     , trg''<-[noCollideUnlessTm' l [src''] (quote$sqlExprTrg fSpec l)]
                     ]++
                     [ Just$ "ECps"++show n++"."++trg''' ++ "=ECps"++show n'++"."++src''
                     | ((n,l),(n',l'))<-zip (init (zipnum es)) (tail (zipnum es))
                     , not (isNeg l), not (isNeg l')
                     , src'''<-[quote$sqlExprSrc fSpec l]
                     , trg'''<-[noCollideUnlessTm' l [src'''] (quote$sqlExprTrg fSpec l)]
                     , src''<-[quote$sqlExprSrc fSpec l']
                     ]++
                     [ Just$ "c"++show n ++"."++(quote$sqlExprSrc fSpec (ERel (I c)))++" IS NOT NULL"
                     | (n,c,sqlExpr,sqlAtt)<-poleConcepts
                     ]
                     where inCs n = n `elem` [n | (n,_,_,_)<-poleConcepts]
selectExpr _     _ _   _   (ECps  [] ) = fatal 223 "Cannot create query for ECps [] because type is unknown"

selectExpr fSpec i src trg (ERel (V (Sign s t))   ) 
 = sqlcomment i ("matches pattern: (ERel (V (Sign s t)))"++phpIndent (i+3)++"ERel [ \""++showADL (V (Sign s t))++"\" ]") $
   listToMaybe [selectGeneric i (src',src) (trg',trg) tbls "1"
               | (s',src') <- concNames (if name s==name t then "cfst0" else quote (name s)) s
               , (t',trg') <- concNames (if name s==name t then "cfst1" else quote (name t)) t
               , let tbls = if length (s'++t') == 0 then "(SELECT 1) AS csnd" else intercalate ", " (s'++t')
               ]
 where concNames pfx c = [([],"1") |c==ONE]++[([quote p ++ " AS "++pfx],pfx++"."++quote s') | (p,s',_) <- sqlRelPlugNames fSpec (ERel (I c))]

selectExpr fSpec i src trg (ERel (I ONE)) = sqlcomment i "I[ONE]"$ selectExpr fSpec i src trg (ERel (V (Sign ONE ONE)))

selectExpr fSpec i src trg (ERel mrph) = selectExprMorph fSpec i src trg mrph
selectExpr fSpec i src trg (EBrk expr) = selectExpr fSpec i src trg expr

 --src*trg zijn strings die aangeven wat de gewenste uiteindelijke typering van de query is (naar php of hoger in de recursie)
 --het is dus wel mogelijk om een -V te genereren van het gewenste type, maar niet om een V te genereren (omdat de inhoud niet bekend is)
selectExpr _ i src trg (EUni [] ) = sqlcomment i "EUni []"$ toM$ selectGeneric i ("1",src) ("1",trg) ("(SELECT 1) AS a") ("0")
selectExpr fSpec i src trg (EUni es') = sqlcomment i ("matches pattern: EUni es"++phpIndent (i+3)++"EUni "++show (map showADL es')) $
                                        "(" +++ (selectExprInUnion fSpec i src trg (EUni es')) +++ (phpIndent i) ++ ")"
selectExpr fSpec i src trg (ECpl (ERel (V _))) = sqlcomment i "matches pattern: ECpl (ERel (V _))"$ selectExpr fSpec i src trg (EUni [])
selectExpr fSpec i src trg (ECpl e )
   = sqlcomment i ("matches pattern: ECpl e"++phpIndent (i+3)++"ECpl [ \""++showADL e++"\" ]") $
     selectGeneric i ("cfst."++src',src) ("csnd."++trg',trg)
                     (quote (sqlConcept fSpec (source e)) ++ " AS cfst,"++phpIndent (i+5)+++selectExprBrac fSpec (i+5) trg' trg' (ERel (I (target e)))+++" AS csnd")
                     ("NOT EXISTS"++phpIndent i++" ("+++
                        (selectExists' (i+2) ((selectExprBrac fSpec (i+2) src2 trg2 e) +++ " AS cp")
                                             ("cfst." ++ src' ++ "=cp."++src2++" AND csnd."++ trg'++"=cp."++trg2)
                        ) +++ ")"
                     )
                     where src' = quote$sqlAttConcept fSpec (source e) 
                           trg' = noCollide [src'] (sqlAttConcept fSpec (target e))
                           src2 = quote$sqlExprSrc fSpec e
                           trg2 = noCollideUnlessTm' e [src2] (quote$sqlExprTrg fSpec e)
selectExpr _ i _ _ (EKl0 _)
   = sqlcomment i "SQL cannot create closures EKl0" (Just "SELECT * FROM NotExistingKl0")
selectExpr _ i _ _ (EKl1 _)
   = sqlcomment i "SQL cannot create closures EKl1" (Just "SELECT * FROM NotExistingKl1")
selectExpr fSpec i src trg (ERad  [e]       ) = sqlcomment i ("matches pattern: ERad [e]"++phpIndent (i+3)++"ERad [ \""++showADL e++"\" ]") $
                                                 selectExpr fSpec i src trg e
selectExpr fSpec i src trg (ERad lst'@(fstm:_:_))
 = sqlcomment i ("matches pattern: ERad lst'@(fstm:_:_)"++phpIndent (i+3)++"ERad "++show (map showADL lst')) $
   selectGeneric i (mainSrc,src) (mainTrg,trg)
                        (cChain ", " (concExprs i ncs)) (cChain (phpIndent i++"  AND ") (inner: cclauses ncs))
{-  The concepts in lst' are called c0, c1, ... cn (with n being the length of lst')
    The elements in lst' are called reladd0, reladd1, ... reladdd(n-1).
    These names are also used in the SQL-aliasing. This prevents name conflicts in a somewhat rough manner, yet insightful and effective.
-}
   where mainSrc = "c0."++src'
         mainTrg = "c"++show (length lst')++"."++trg'
         -- de strings die de source en target van ERad lst' weergeven. Bij gelijke namen vindt ontdubbeling van naam plaats met noCollideUnlessTm'
         src'    = quote$sqlExprSrc fSpec fstm
         trg'    = noCollideUnlessTm' fstm [src'] (quote$sqlExprTrg fSpec (last lst'))
         -- ncs geeft alleen de concepten uit lst', die in SQL doorlopen moeten worden, inclusief rangnummer
         ncs     = [ (0,source (head lst')), (length lst',target (last lst'))  ]
         ncs'    = [ (n',c)
                   | (l,(n',l'))<-zip (init lst') (tail (zipnum lst'))
                   , not (isNeg l) || not (isNeg l')
                   , c<-[if target l<=source l' then target l else source l']
                   ]
         -- de SQL-expressies voor de concepten van lst', maar nu in SQL
         concExprs i' ncs'' = [ selectExprBrac fSpec i' sm sm tm +++ " AS c"++show n
                              | (n,c)<-ncs'', tm<-[ERel (I c)], sm<-[quote$sqlExprSrc fSpec tm] ]
         -- de SQL-expressies voor de elementen uit lst', die elk een Ampersand expressie representeren
         inner     = "NOT EXISTS ("+++selectExists' (i+19)
                                                          (cChain ", " (concExprs (i+19) ncs'))
                                                          (cChain (phpIndent (i+19)++"  AND ") (wherecl++cclauses ncs'))
                                            +++")"
         -- de where  expressies bevatten alle "magie". Dit is zgn. "terse code", die omzichtig behandeld moet worden.
         -- TODO: de volgende code is incorrect. Ook loopt ze uit de pas met de code voor ECps.
         wherecl   = filterEmpty
                     [ (if isNeg l then "   " else "NOT")++
                       " EXISTS ("+++selectExists' (i+38)
                                                  (selectExprBrac fSpec (i+38) src''  trg''  (if isNeg l  then notCpl l  else l ) +++ " AS reladd"++show n)
                                                  (cChain " AND " ([(if inCs n then "c" else "reladd")++show n++"."++src'' ++ "=reladd"++show  n   ++"."++src'']++
                                                                  ["reladd"++show n++"."++trg'' ++ (if inCs (n+1) then "=c" else "=reladd")++show (n+1)++"."++trg'']))
                              +++")"
                     | (n,l)<-zipnum lst' 
                     , src''<-[quote$sqlExprSrc fSpec l]
                     , trg''<-[noCollideUnlessTm' l [src''] (quote$sqlExprTrg fSpec l)]
                     ]++
                     [ Just $ "reladd"++show n++"."++trg'' ++ "=reladd"++show n'++"."++src''
                     | ((n,l),(n',l'))<-zip (init (zipnum lst')) (tail (zipnum lst'))
                     , isNeg l, isNeg l'
                     , trg''<-[(quote$sqlExprTrg fSpec l)]
                     , src''<-[quote$sqlExprSrc fSpec l']
                     ] where inCs n = n `elem` map fst (ncs++ncs')
         cclauses ncs'' = [ Just$ "c"++show n ++"."++(quote$sqlExprSrc fSpec (ERel ( I c)))++" IS NOT NULL"
                          | (n,c)<-ncs''
                          ]
                     
selectExpr _     _ _   _   (ERad  [] ) = fatal 310 "Cannot create query for ERad []. This should never occur."
selectExpr fSpec i src trg (ETyp x _)  = sqlcomment i ("matches pattern: ETyp x _"++phpIndent (i+3)++"ETyp ( \""++showADL x++"\" ) _") $
                                         selectExpr fSpec i src trg x
selectExpr fSpec i src trg (EFlp x)    = sqlcomment i ("matches pattern: EFlp x.") $
                                         selectExpr fSpec i trg src x
selectExpr fSpec i src trg (EDif (ERel V{},x)) = sqlcomment i ("matches pattern: EDif V x"++phpIndent (i+3)++"EDif V ( \""++showADL x++"\" )") $
                                                 selectExpr fSpec i src trg (ECpl x) 
selectExpr _     _ _   _   x           = fatal 332 ("Cannot create query for "++showADL x)

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
               -> Int        -- ^ indentation
               -> String     -- ^ source name (preferably quoted)
               -> String     -- ^ target name (preferably quoted)
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
                   (quote (sqlConcept fSpec (source rel)) +++ " AS vfst, "++quote (sqlConcept fSpec (target rel)) ++ " AS vsnd")
                   (src'+++" IS NOT NULL AND "++trg'++" IS NOT NULL")
 where src'="vfst."++sqlAttConcept fSpec (source rel)
       trg'="vsnd."++sqlAttConcept fSpec (target rel)
selectExprMorph _ _ src trg rel@Mp1{}
 | src == ""&&trg=="" = fatal 394 "Source and target are \"\", use selectExists' for this purpose"
 | src == ""  = Just$ "SELECT "++relval rel++" AS "++trg
 | trg == ""  = Just$ "SELECT "++relval rel++" AS "++src
 | src == trg = Just$ "SELECT "++relval rel++" AS "++src
 | otherwise  = Just$ "SELECT "++relval rel++" AS "++src++", "++relval rel++" AS "++trg
selectExprMorph fSpec i src trg rel -- made for both Rel and I
 = listToMaybe [selectGeneric i (quote s,src) (quote t,trg) (quote p) (quote s++" IS NOT NULL AND "++quote t++" IS NOT NULL")
               | (p,s,t)<-sqlRelPlugNames fSpec (ERel rel)  -- "NOT NULL" checks could be omitted if column is non-null, but the
               ]                                            -- code for computing table properties is currently unreliable.
               
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
  
-- | sqlExprSrc gives the SQL-string that serves as the attribute name in SQL.
--   we want it to show the type, which is useful for readability. (Otherwise, just "SRC" and "TRG" would suffice)
--   It is not clear why the recursion over expressions is repeated here...
--   WHY not ask for the type of the expression and assemble a nice name? (TODO)
sqlExprSrc :: Fspc->Expression -> String
sqlExprSrc fSpec expr = ses expr
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
sqlExprTrg :: Fspc->Expression -> String
sqlExprTrg fSpec e = sqlExprSrc fSpec (flp e)


-- sqlConcept gives the name of the plug that contains all atoms of A_Concept c.
sqlConcept :: Fspc -> A_Concept -> String
sqlConcept fSpec c = name (sqlConceptPlug fSpec c)
   
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

----------------------------------------

data SqlFld = SqlFld {sfdfrom::SqlSelect, sfdfld::String, sfdas::String}
data SqlVal = SqlVal String | SqlNul | SqlVar SqlFld
data SqlClause = SqlEQ SqlVal SqlVal | SqlAND SqlClause SqlClause | SqlNOT SqlClause | SqlOR SqlClause SqlClause | SqlEXISTS SqlSelect
data SqlSelect = SqlSel1 CptSelect | SqlSel2 RelSelect | SqlFrom SqlTbl
data CptSelect = CptSel {cslfld::SqlFld, cslfrom::SqlSelect, cslwhere::Maybe SqlClause, cslas::String}
data RelSelect = RelSel {rslfld::(SqlFld,SqlFld), rslfrom::[SqlSelect],rsljoin::Maybe(SqlSelect,SqlFld,SqlFld), rslwhere::Maybe SqlClause, rslas::String}
data SqlTbl = SqlTbl {tblnm::String, tblas::String}
instance Identified SqlFld where
   name x = if null (sfdas x) then sfdfld x else sfdas x
   rename x nm = x{sfdfld=if null(sfdas x) then sfdfld x else sfdas x, sfdas=nm}
instance Identified SqlSelect where
   name (SqlSel1 x)= if null (cslas x) then fatal 611 "no alias for select query" else cslas x
   name (SqlSel2 x)= if null (rslas x) then fatal 612 "no alias for select query" else rslas x
   name (SqlFrom x)= if null (tblas x) then tblnm x else tblas x 
   rename (SqlSel1 x) nm = SqlSel1$x{cslas=nm}
   rename (SqlSel2 x) nm = SqlSel2$x{rslas=nm}
   rename (SqlFrom x) nm = SqlFrom$x{tblas=nm}

showsql :: SqlSelect -> String
showsql (SqlSel1 csel)
 = "SELECT " ++ selectfld(cslfld csel)
   ++ " FROM " ++ selectfrm(cslfrom csel)
   ++ selectwhere(cslwhere csel)
showsql (SqlSel2 rsel)
 = "SELECT " ++ selectflds[fst(rslfld rsel),snd(rslfld rsel)]
   ++ " FROM " ++ selectfrms(rslfrom rsel)
   ++ selectjoin(rsljoin rsel)
   ++ selectwhere(rslwhere rsel)
showsql (SqlFrom tbl) = "SELECT * FROM `"++ tblnm tbl ++"`"

selectflds :: [SqlFld] -> String
selectflds flds = intercalate ", " (map selectfld flds)
selectfrms :: [SqlSelect] -> String
selectfrms frms = intercalate ", " (map selectfrm frms)
selectfld :: SqlFld -> String
selectfld fld = "`"++ name(sfdfrom fld) ++"`.`"++ sfdfld fld ++"`"
                   ++ (if null(sfdas fld) then [] else " AS " ++ sfdas fld)
selectfrm :: SqlSelect -> String
selectfrm (SqlFrom tbl) = "`"++ tblnm tbl ++"`" 
                             ++ (if null(tblas tbl) then [] else " AS " ++ tblas tbl)
selectfrm sel = "("++ showsql sel ++") AS " ++ name sel
selectwhere :: Maybe SqlClause -> String
selectwhere Nothing = []
selectwhere (Just w) = " WHERE " ++ showsqlwhere w
selectjoin :: Maybe (SqlSelect, SqlFld, SqlFld) -> String
selectjoin Nothing = []
selectjoin (Just (sel,f1,f2)) = " JOIN "++ selectfrm sel ++" ON " ++ wherevar f1 ++ "=" ++ wherevar f2

showsqlwhere :: SqlClause -> String
showsqlwhere (SqlEQ SqlNul y)                    = showsqlwhere (SqlEQ y SqlNul)
showsqlwhere (SqlEQ (SqlVar fld) SqlNul)         = wherevar fld  ++" IS NULL"
showsqlwhere (SqlEQ (SqlVal val) y)              = showsqlwhere (SqlEQ y (SqlVal val))
showsqlwhere (SqlEQ (SqlVar fld) (SqlVal val))   = wherevar fld  ++"="++ whereval val
showsqlwhere (SqlEQ (SqlVar fld1) (SqlVar fld2)) = wherevar fld1 ++"="++ wherevar fld2       
showsqlwhere (SqlAND w1 w2)                      = "("++ showsqlwhere w1 ++") AND ("++ showsqlwhere w2 ++")"
showsqlwhere (SqlNOT w)                          = "NOT("++ showsqlwhere w ++")"
showsqlwhere (SqlOR w1 w2)                       = "("++ showsqlwhere w1 ++") OR ("++ showsqlwhere w2 ++")"
showsqlwhere (SqlEXISTS sel)                     = "EXISTS ("++ showsql sel ++")"

wherevar :: SqlFld -> String
wherevar fld = "`"++ name(sfdfrom fld) ++"`.`"++ name fld ++"`"
whereval :: String -> String
whereval val = "'"++ val ++"'"

--NAMING CONVENTION
--select queries AS (sel++typeof(rel)++uniquename(rel))
--selected fields AS => [fld1,fld2,..]
--name A_Concept => unique
--name declaration => not unique >> uniquename declaration = hash(name,source,target)
--name expression => not unique >> uniquename expression = hash(show,source,target)
--examples: `cptOrder`.`fld1`, `dcl12`.`fld2`
--ALIAS CONVENTION
--when constructing new fields, use rename to give them an alias => the old alias will become the actual name of the new field
--nested select queries must have an alias to avoid fatals <= use rename to set an alias
class (Show rel) => InPlug rel where
   selectbinary :: Fspc -> rel -> RelSelect
   selectdomain :: Fspc -> rel -> CptSelect
   selectrange :: Fspc -> rel -> CptSelect
   selectvector :: Fspc -> String -> rel -> CptSelect
   domainloc :: Fspc -> rel -> (SqlField,PlugSQL)  --a location in the InternalPlugs::[InternalPlug SqlPlug]
   domainloc fs r 
    | null (domainlocs fs r) = fatal 626 ("rel not found InPlug" ++ show r)
    | otherwise = head (domainlocs fs r)
   domainlocs :: Fspc -> rel -> [(SqlField,PlugSQL)]  --all locations in the InternalPlugs::[InternalPlug SqlPlug]
   targetloc :: Fspc -> rel -> (SqlField,PlugSQL)  --a location in the InternalPlugs::[InternalPlug SqlPlug]
   targetloc fs r 
    | null (targetlocs fs r) = fatal 631 "rel not found InPlug"
    | otherwise = head (targetlocs fs r)
   targetlocs :: Fspc -> rel -> [(SqlField,PlugSQL)]  --all locations in the InternalPlugs::[InternalPlug SqlPlug]
   --default implementation for the class of relations and relation expressions
   selectdomain fs r =
      let rsel = selectbinary fs r
          sel1 = rename (SqlSel2 rsel) "relr"
          fld = (fst(rslfld rsel)){sfdfrom=sel1}
      in CptSel {cslfld=rename fld "fld1",cslfrom=sel1,cslwhere=Nothing,cslas=[]}
   selectrange fs r =
      let rsel = selectbinary fs r
          sel1 = rename (SqlSel2 rsel) "relr"
          fld = (snd(rslfld rsel)){sfdfrom=sel1}
      in CptSel {cslfld=rename fld "fld1",cslfrom=sel1,cslwhere=Nothing,cslas=[]}
   selectvector fs x r = --the target of r given an instance of the source of r
      let rsel = selectbinary fs r 
          sel1 = rename (SqlSel2 rsel) "relr"
          fld1 = (fst(rslfld rsel)){sfdfrom=sel1}
          fld2 = (snd(rslfld rsel)){sfdfrom=sel1}
          w = SqlEQ (SqlVar fld1) (SqlVal x)
      in CptSel {cslfld=rename fld2 "fld1", cslfrom=sel1,cslwhere=Just w,cslas=[]}

makesqlfld :: (Identified a) => SqlField -> a -> SqlFld
makesqlfld pf p = SqlFld {sfdfrom=makesqltbl p, sfdfld=fldname pf, sfdas=[]}
makesqltbl :: (Identified a) => a -> SqlSelect
makesqltbl p = SqlFrom(SqlTbl {tblnm=name p, tblas=[]})

instance InPlug A_Concept where
   selectbinary fs c --the identity relation of c
    = let csel = selectdomain fs c 
          cfld = cslfld csel
          sel = rename (SqlSel1 csel) ("cpt" ++ name c)
          fld1 = cfld{sfdfrom=sel} 
          fld2 = cfld{sfdfrom=sel}
      in RelSel {rslfld=(rename fld1 "fld1",rename fld2 "fld2"),rslfrom=[sel],rsljoin=Nothing,rslwhere=Nothing,rslas=[]}  
   selectdomain fs c = --the domain of c
      let (pf,p) = domainloc fs c
          fld = makesqlfld pf p
          tbl = makesqltbl p
          w = SqlNOT(SqlEQ (SqlVar fld) SqlNul)
      in CptSel {cslfld=rename fld "fld1",cslfrom=tbl,cslwhere=Just w,cslas=[]}
   selectrange = selectdomain --the domain and range of the identity relation of c are the same
   selectvector fs x c = --a A_Concept instance x may or may not exist
      let csel = selectdomain fs c 
          sel1 = rename (SqlSel1 csel) ("cpt" ++ name c)
          fld = (cslfld csel){sfdfrom=sel1}
          w = SqlEQ (SqlVar fld) (SqlVal x)
      in CptSel {cslfld=rename fld "fld1",cslfrom=sel1,cslwhere=Just w,cslas=[]}
   domainlocs fs c = [(f,p) |(p,_,f)<-sqlRelPlugs fs (ERel(I c))]
   targetlocs = domainlocs
   
instance InPlug Relation where
   selectbinary fs r --the relation r
    = let (sf,sp) = domainloc fs r
          sfld = makesqlfld sf sp
          (tf,tp) = targetloc fs r
          tfld = makesqlfld tf tp
      in RelSel {rslfld=(rename sfld "fld1",rename tfld "fld2")
                ,rslfrom=if sp==tp then [makesqltbl sp] else fatal 714 "relation declarations must be stored in one plug"
                ,rsljoin=Nothing
                ,rslwhere=Just( SqlAND  (SqlNOT(SqlEQ (SqlVar sfld) SqlNul))
                                        (SqlNOT(SqlEQ (SqlVar tfld) SqlNul))   )
                ,rslas=[]} 
   domainlocs fs r = [(f,p) |(p,f,_)<-sqlRelPlugs fs (ERel r)]
   targetlocs fs r = [(f,p) |(p,_,f)<-sqlRelPlugs fs (ERel r)]
   
instance InPlug Expression where
   domainlocs fs ex = [(f,p) |(p,f,_)<-sqlRelPlugs fs ex]
   targetlocs fs ex = [(f,p) |(p,_,f)<-sqlRelPlugs fs ex]
   selectbinary fs (ERel r) 
    = case r of
        Rel{} -> selectbinary fs r
        V{} -> let ssel=selectdomain fs (source r)
                   sel1=rename (SqlSel1 ssel) "sourcer"
                   tsel=selectdomain fs (target r)
                   sel2=rename (SqlSel1 tsel) "targetr"
                   sfld=(cslfld ssel){sfdfrom=sel1}
                   tfld=(cslfld tsel){sfdfrom=sel2}
               in RelSel{rslfld=(rename sfld "fld1", rename tfld "fld2")
                        ,rslfrom=[sel1,sel2],rsljoin=Nothing,rslwhere=Nothing,rslas=[]}
        I{} -> selectbinary fs (source r)
        Mp1{} -> let csel = selectvector fs (relval r) (source r)
                     cfld = cslfld csel
                     sel = rename (SqlSel1 csel) "sourcer"
                     fld1 = cfld{sfdfrom=sel} 
                     fld2 = cfld{sfdfrom=sel}
                 in RelSel {rslfld=(rename fld1 "fld1",rename fld2 "fld2"),rslfrom=[sel],rsljoin=Nothing,rslwhere=Nothing,rslas=[]}  
   selectbinary fs (EBrk ex)  = selectbinary fs ex
   selectbinary fs (ECpl(ECpl ex)) = selectbinary fs ex --TODO -> not(EXISTS (not EXISTS selex) seems to be wrong (e.g. r |- s;t)
   selectbinary fs (ECpl ex) 
    = let vex = ERel (V (sign ex))
          vsel = selectbinary fs vex
          sel1=rename (SqlSel2 vsel) "vsel"
          sfld=(fst$rslfld vsel){sfdfrom=sel1}
          tfld=(snd$rslfld vsel){sfdfrom=sel1}
          w = SqlNOT(SqlEXISTS sel2)
          selex = selectbinary fs ex
          selexf1 = (fst(rslfld selex)){sfdas=sfdfld(fst(rslfld selex))} --use the name as alias
          selexf2 = (snd(rslfld selex)){sfdas=sfdfld(snd(rslfld selex))} --use the name as alias
          cl2 = SqlAND (SqlEQ (SqlVar sfld) (SqlVar selexf1)) (SqlEQ (SqlVar tfld) (SqlVar selexf2))
          w2 = case (rslwhere selex) of
             Nothing -> cl2
             Just wex -> SqlAND wex cl2
          sel2 = SqlSel2(selex{rslwhere=Just w2})
      in RelSel{rslfld=(rename sfld "fld1", rename tfld "fld2")
                        ,rslfrom=[sel1],rsljoin=Nothing,rslwhere=Just w,rslas=[]}
   selectbinary _ (ECps []) = fatal 783 "ECps[]"
   selectbinary fs (ECps (ex:[])) = selectbinary fs ex
   selectbinary fs (ECps (ex:exs))
    = let selex = selectbinary fs ex
          sel1=rename (SqlSel2 selex) "relr"
          s1fld=(fst$rslfld selex){sfdfrom=sel1}
          t1fld=(snd$rslfld selex){sfdfrom=sel1}
          selexs = selectbinary fs (ECps exs)
          sel2=rename (SqlSel2 selexs) "relS"
          s2fld=(fst$rslfld selexs){sfdfrom=sel2}
          t2fld=(snd$rslfld selexs){sfdfrom=sel2}
      in RelSel{rslfld=(rename s1fld "fld1", rename t2fld "fld2")
                        ,rslfrom=[sel1],rsljoin=Just (sel2,s2fld,t1fld),rslwhere=Nothing,rslas=[]}
   selectbinary fs (ERad exs) = selectbinary fs (ECpl(ECps(map ECpl exs))) --TODO
   selectbinary fs (EUni exs) = selectbinary fs (ECpl(EIsc(map ECpl exs))) --TODO
   selectbinary _ (EIsc []) = fatal 783 "EIsc[]"
   selectbinary fs (EIsc (ex:[])) = selectbinary fs ex
   selectbinary fs (EIsc (ex:exs)) 
    = let selex = selectbinary fs ex
          sel1=rename (SqlSel2 selex) "selr"
          sfld=(fst$rslfld selex){sfdfrom=sel1}
          tfld=(snd$rslfld selex){sfdfrom=sel1}
          w = SqlEXISTS sel2
          selexs = selectbinary fs (EIsc exs)
          selexsf1 = (fst(rslfld selexs)){sfdas=sfdfld(fst(rslfld selexs))} --use the name as alias
          selexsf2 = (snd(rslfld selexs)){sfdas=sfdfld(snd(rslfld selexs))} --use the name as alias
          cl2 = SqlAND (SqlEQ (SqlVar sfld) (SqlVar selexsf1)) (SqlEQ (SqlVar tfld) (SqlVar selexsf2))
          w2 = case (rslwhere selexs) of
             Nothing -> cl2
             Just wex -> SqlAND wex cl2
          sel2 = SqlSel2(selexs{rslwhere=Just w2})
      in RelSel{rslfld=(rename sfld "fld1", rename tfld "fld2")
                        ,rslfrom=[sel1],rsljoin=Nothing,rslwhere=Just w,rslas=[]}
--select j1.sKlasse,gerelateerdAan.tKlasse from  gerelateerdAan JOIN (select erftVan.sKlasse,gerelateerdAan.tKlasse from gerelateerdAan JOIN erftVan ON erftVan.tKlasse=gerelateerdAan.sKlasse) as j1 ON j1.tKlasse=gerelateerdAan.sKlasse;
  --  = let selex = selectbinary fs ex
          
    --  | ECps  (Expressions rel)   -- ts   ^ composition                             ;
--      | ERad (Expressions rel)  -- ts   ^ relative addition                       !
  --    | EIsc (Expressions rel)  -- fs   ^ intersection                            /\
    --  | EUni (Expressions rel)  -- fs   ^ union                                   \/     
   selectbinary fs (EFlp x) = let sb=selectbinary fs x in sb{rslfld=(fst(rslfld sb),snd(rslfld sb))}
   selectbinary fs (ETyp x _) = selectbinary fs x
   selectbinary _ (EKl0{}) = RelSel (SqlFld (SqlFrom (SqlTbl "NotExistingKl0" "")) "x" ""
                                     ,SqlFld (SqlFrom (SqlTbl "NotExistingKl0" "")) "y" "")
                                     [SqlFrom (SqlTbl "NotExistingKl0" "")] Nothing Nothing ""
   selectbinary _ (EKl1{}) = RelSel (SqlFld (SqlFrom (SqlTbl "NotExistingKl1" "")) "x" ""
                                     ,SqlFld (SqlFrom (SqlTbl "NotExistingKl1" "")) "y" "")
                                     [SqlFrom (SqlTbl "NotExistingKl1" "")] Nothing Nothing ""
   selectbinary fs (EDif (ERel V{},x)) = selectbinary fs (ECpl x)
   selectbinary _ x = fatal 748 ("not supported" ++ show x)

 

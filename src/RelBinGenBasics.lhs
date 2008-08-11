> module RelBinGenBasics where
>  import Char
>  import Auxiliaries
>  import CC_aux
>  import CommonClasses
>  import Calc(shrink, conjNF, informalRule, computeOrder, homogeneous, ComputeRule)
>--  import MultRules

>  pDebug   = True
>  pNoDebug = not pDebug


sqlCodeComputeRule: 4th argument is (as of sept 25th 07) empty. It used to be:
           substitutions = [ if inline m
                             then (m,phpMorSrc context m,phpMorTrg context m)
                             else (m,phpMorTrg context m,phpMorSrc context m)
                           | m<-mors e, declaration m==r]
Het substitueren van een hele relatie met de ingetikte atomen vanuit de interface mag alleen maar onder condities, die we nog niet begrijpen. Vandaar.
Zonder substituties rekent sqlCodeComputeRule dus met de hele relatie, en is daardoor minder efficient (maar wel correct).
Een substitutie (m,s,t) betekent dat de relatie m wordt vervangen door twee atomen uit de interface, aangeduid met s en t.
Let op: als m geflipt is, dan moeten de twee ingetikte atomen op de juiste plaats komen, en dus worden verwisseld.

Als Tm [m'] = frExpr dan hoeft alleen het veld uit de interface (opgeslagen in de PHP variabele attrs) te worden overgenomen

>-- Pre: oneMorphism toExpr
>  sqlCodeComputeRule :: String -> Int -> Context -> [(Morphism,String,String)] -> ComputeRule -> String
>  sqlCodeComputeRule attrs i context subs hc@(fOp, e, "INSERT INTO", toExpr, frExpr, rule)
>   = if srcTo==trgTo && not (isProperty toExpr) then error ("(Module RelBinGenBasics) Fatal: srcTo and trgTo are equal ("++srcTo++") in sqlCodeComputeRule.\n"++show hc) else
>   {-if Tm [m'] == frExpr
>     then "'INSERT IGNORE INTO "++sqlMorName context m++
>          phpIndent i ++ "VALUES (\\''.addslashes("++attrs++"['"++srcFr++"']).'\\',"++
>                                " \\''.addslashes("++attrs++"['"++trgFr++"']).'\\')'"
>     else -}
>     "'INSERT IGNORE INTO "++sqlMorName context m++
>          selectExpr context i srcTo trgTo frExpr'++"'"
>     where m    = head (mors toExpr)
>           m'   = head (mors frExpr)
>           srcFr | isIdent frExpr     = sqlConcept context (source frExpr)
>                 | otherwise          = if inline m' then sqlMorSrc context m else sqlMorTrg context m
>           srcTo = sqlExprSrc toExpr
>           trgFr | isIdent frExpr     = sqlConcept context (source frExpr)
>                 | otherwise          = if inline m' then sqlMorTrg context m else sqlMorSrc context m
>           trgTo = sqlExprTrg toExpr -- may not collide with srcTo, but what if toExpr is a property (or identity)? (Bas?)
>           frExpr' = doSubsExpr context attrs subs frExpr
>  sqlCodeComputeRule attrs i context subs hc@(fOp, e, "DELETE FROM", toExpr, (Cp frExpr), rule)
>   = if null froms
>     then "'DELETE FROM "++tbl++phpIndent i++
>          "WHERE "++tbl++"."++src++"<>"++doSubSrc context subs "$qa[0]" m (sqlMorName context m') (sqlMorSrc context m')++
>          " OR "  ++tbl++"."++trg++"<>"++doSubTrg context subs "$qa[0]" m (sqlMorName context m') (sqlMorTrg context m')++"'"
>     else "'DELETE FROM "++tbl++phpIndent i++
>          "WHERE NOT EXISTS "++
>          "("++(selectExists' i
>                              ((selectExprBrac context (i+2) src trg (doSubsExpr context attrs subs frExpr))++" AS f")
>                              ("f."++src++"="++tbl++"."++src++" AND f."++trg++"="++tbl++"."++trg)
>               )++
>          ")'"
>     where m  = head (mors toExpr)
>           m' = head (mors frExpr)
>           tbl   = sqlMorName context m
>           src   = sqlMorSrc context m
>           trg   = sqlMorTrg context m
>           frExpr' = doSubsExpr context attrs subs frExpr
>           froms = [m| m<-mors frExpr, not (isSub context subs (sqlMorName context m))]
>  sqlCodeComputeRule attrs i context subs hc@(fOp, e, "DELETE FROM", toExpr, frExpr, rule)
>   = if null froms
>     then "'DELETE FROM "++tbl++phpIndent i++
>          "WHERE "++tbl++"."++src++"="++doSubSrc context subs "$qa[0]" m (sqlMorName context m') (sqlMorSrc context m')++
>          " AND " ++tbl++"."++trg++"="++doSubTrg context subs "$qa[0]" m (sqlMorName context m') (sqlMorTrg context m')++"'"
>     else "'DELETE FROM "++tbl++phpIndent i++"WHERE EXISTS "++
>          "("++(selectExists' i
>                              ((selectExprBrac context (i+2) src trg (doSubsExpr context attrs subs frExpr))++" AS f")
>                              ("f."++src++"="++tbl++"."++src++" AND f."++trg++"="++tbl++"."++trg)
>               )++
>          ")'"
>     where m  = head (mors toExpr)
>           m' = head (mors frExpr)
>           tbl   = sqlMorName context m
>           src   = sqlMorSrc context m
>           trg   = sqlMorTrg context m
>           froms = [m| m<-mors frExpr, not (isSub context subs (sqlMorName context m))]
>
>  doSubsExpr ::    Context
>                -> String   -- substitute by this PHP variable (e.g. "$x")
>                -> [(Morphism,String,String)] -- substitutions
>                -> Expression -- without substitutions made
>                -> Expression -- with substitutions made

Example: doSubsExpr ctx "$x" [(a,_,_)]  <a;b;a~\/c>
 yields: <"$x";b;"$x"~\/c>

Explanation: a relation r, which occurs in expression expr, is replaced by an element of r (which is a Pair).
Reason: prevent double code.

>  doSubsExpr _ _ [] expr = expr
>  doSubsExpr ctx  -- the current context
>             to   -- "$attrs"
>             s    -- the list of substitutions. Each substitution is an (r::Morphism,a,q)
>             expr -- 
>   = sub expr
>     where sub (F  e) = F  (map sub e)
>           sub (Fd e) = Fd (map sub e)
>           sub (Fi e) = Fi (map sub e)
>           sub (Fu e) = Fu (map sub e)
>           sub (Cp e) = Cp (sub e)
>           sub (K0 e) = K0 (sub e)
>           sub (K1 e) = K1 (sub e)
>           sub (Tm m@(Mph _ _ _ _ _ _)) = subMorph [ x | x@(r,_,_)<-s, sqlMorName ctx m == sqlMorName ctx r] m
>           sub (Tm m) = Tm m
>           sub (Tc e) = Tc (sub e)
>           subMorph trs m | isIdent m = mp1 to (head [a|(r,a,_)<-s,sqlMorName ctx m == sqlMorName ctx r]) (source m)
>           subMorph []  m             = Tm m -- nothing to replace
> --          subMorph ((_,p,q):trs)   m = F [mp1 to p (source m),v (source m,target m),mp1 to q (target m)]
>           subMorph ((r,p,q):trs)   m | inline m  = F [mp1 to p (source m),v (source m,target m),mp1 to q (target m)]
>                                      | otherwise = F [mp1 to q (source m),v (source m,target m),mp1 to p (target m)]
>           mp1 var a = Tm . Mp1 ("\\''.addslashes("++var++"['"++a++"']).'\\'")


Is this still useful?? (SJ, 2 aug 2007)
  sqlExpr ctx   -- context
          i     -- indentation
          rule  -- oorspronkelijke regel
          alias -- naam die achter de AS moet komen
          subs  -- substituties voor PHP variabelen
          expr  -- expressie die gecompiled moet worden
   = if isClos expr
     then (alias, sqlClosName ctx expr, sqlExprSrc expr, sqlExprTrg expr)
     else if oneMorphism expr
          then ( if single rule m then sqlMorName ctx m else alias
               , sqlMorName ctx m
               , sqlMorSrc ctx m
               , sqlMorTrg ctx m
               )
          else (alias
               , "("++selectNormFiExpr "$attrs" ctx (i+6) rule (sqlExprSrc expr, sqlExprTrg expr) subs expr++phpIndent (i+6)++")"
               , sqlExprSrc expr, sqlExprTrg expr)
     where m = head (morlist expr)

The practical version of selectRule does the same, but generates code in which the rule is recognized better.

>  selectRule :: Context -- current context
>                -> Int  -- indentation
>                -> Rule -- rule to be translated
>                -> String -- resulting SQL expression
>  selectRule ctx i r@(Ru _ _ _ _ _ _ _ _ _)
>   = if src==trg && not (isProperty e) then error ("(Module RelBinGenBasics) Fatal: src and trg are equal ("++src++") in selectRule.\n"++showADL r) else
>     selectExpr ctx i src trg e
>     where src  = sqlExprSrc e
>           trg  = sqlExprTrg e -- may not collide with src, but what if toExpr is a property (or identity)? (Bas?)
> -- was:   trg  = noCollide [src] (sqlExprTrg e)
> -- might be?: trg  = noCollideUnlessTm e [src] (sqlExprTrg e)
>           e    = (shrink.conjNF.Cp .normExpr) r
>  selectRule ctx i r
>   = error ("(module RelBinGenBasics) Fatal: This rule should never occur in selectRule ctx i ("++showHS "" r++")") -- verified in AGtry.ag vs. 0.7.6 on May 1st 2006 (by SJO)

TODO: de volgende functie, selectExpr, geeft een fout antwoord als de expressie -V is.

>  selectExpr ::    Context    -- current context
>                -> Int        -- indentation
>                -> String     -- SQL name of the source of this expression, as assigned by the environment 
>                -> String     -- SQL name of the target of this expression, as assigned by the environment
>                -> Expression -- expression to be translated
>                -> String     -- resulting SQL expression

>  selectExpr ctx i src trg (Fi lst'@(_:_:_))
>   = selectGeneric i ("isect0."++src',src) ("isect0."++trg',trg)
>                          (chain ", " exprbracs) (chain " AND " (wherecl))
>     where src'    = sqlExprSrc fst
>           trgC    = sqlExprTrg fst -- can collide with src', for example in case fst==r~;r, or if fst is a property (or identity)
>           trg'    = noCollideUnlessTm fst [src'] trgC
>           fst     = head posTms  -- always defined, because length posTms>0 (ensured in definition of posTms)
>           mp1Tm   = take 1 ([t| t@(Tm (Mp1 _ _))<-lst']++[t| t@(F ((Tm (Mp1 _ _)):(Tm (V _ _)):(Tm (Mp1 _ _)):[])) <- lst'])
>           lst     = [t|t<-lst', not (elem t mp1Tm)]
>           posTms  = if null posTms' then map notCp (take 1 negTms') else posTms' -- we take a term out of negTms' if we have to, to ensure length posTms>0
>           negTms  = if null posTms' then tail negTms' else negTms' -- if the first term is in posTms', don't calculate it here
>           posTms' = [t| t<-lst, isPos t && not (isIdent t)]++[t| t<-lst, isPos t && isIdent t] -- the code to calculate I is better if it is not the first term
>           negTms' = [notCp t| t<-lst, isNeg t && isIdent t]++[notCp t| t<-lst, isNeg t && not (isIdent t)] -- should a negTerm become a posTerm (for reasons described above), it can best be an -I.
>           exprbracs = [ (selectExprBrac ctx (i) src'' trg'' l) ++ " AS isect"++show n 
>                       | (n,l)<-zip [0..] posTms
>                       , src''<-[sqlExprSrc l]
>                       , trg''<-[noCollideUnlessTm l [src''] (sqlExprTrg l)]
>                       ]
>           wherecl   = [if isIdent l
>                        then  "isect0."++src'++" = isect0."++trg' -- this is the code to calculate ../\I. The code below will work, but is longer
>                        else "(isect0."++src'++" = isect"++show n++"."++src''
>                        ++ " AND isect0."++trg'++" = isect"++show n++"."++trg''++")"
>                       | (n,l)<-tail (zip [0..] posTms) -- not empty because of definition of posTms
>                       , src''<-[sqlExprSrc l]
>                       , trg''<-[noCollideUnlessTm l [src''] (sqlExprTrg l)]
>                       ]++
>                       [ "isect0."++src'++" = "++s -- sorce and target are equal because this is the case with Mp1
>                       | (Tm (Mp1 s _)) <- mp1Tm
>                       ]++
>                       [ "isect0."++src'++" = "++s1 -- sorce and target are unequal
>                         ++ " AND isect0."++trg'++" = "++s2 -- sorce and target are unequal
>                       | (F ((Tm (Mp1 s1 _)):(Tm (V _ _)):(Tm (Mp1 s2 _)):[])) <- mp1Tm
>                       ]++
>                       [if isIdent l
>                        then  "isect0."++src'++" <> isect0."++trg' -- this code will calculate ../\-I
>                        else  "NOT EXISTS ("++(selectExists' (i+12)
>                                                             ((selectExprBrac ctx (i+12) src'' trg'' l) ++ " AS cp")
>                                                             ("isect0."++src' ++ "=cp."++src''++" AND isect0."++ trg'++"=cp."++trg'')
>                                           )++")"
>                       | (n,l)<-zip [0..] negTms
>                       , src''<-[sqlExprSrc l]
>                       , trg''<-[noCollideUnlessTm l [src''] (sqlExprTrg l)]
>                       ]

More optimal F code

>  selectExpr ctx i src trg (F (s1@(Tm (Mp1 _ _)):(s2@(Tm (V _ _)):(s3@(Tm (Mp1 _ _)):fx@(_:_))))) -- to make more use of the thing below
>    =  selectExpr ctx i src trg (F ((F (s1:s2:s3:[])):fx))

>  selectExpr ctx _ src trg (F ((Tm s@(Mp1 sr _)):((Tm (V _ _)):((Tm t@(Mp1 tr _)):[])))) -- this will occur quite often because of doSubsExpr
>    = "SELECT "++sr++" AS "++src++", "++tr++" AS "++trg

>  selectExpr ctx i src trg (F (e@(Tm (Mp1 sr _)):(f:fx))) = -- this will occur because of ObjBinGenObject's way of doing a read
>       selectGeneric i ("fst."++src',src) ("fst."++trg',trg)
>                       (selectExprBrac ctx (i) src' trg' (F (f:fx))++" AS fst")
>                       ("fst."++src'++" = "++sr)
>                       where src' = sqlExprSrc e
>                             trg' = noCollideUnlessTm (F (f:fx)) [src'] (sqlExprTrg (F (f:fx)))

>  selectExpr ctx i src trg (F (e:((Tm (V _ _)):(f:fx)))) = -- prevent calculating V in this case
>      if src==trg && not (isProperty e) then error ("(Module RelBinGenBasics: selectExpr 2) src and trg are equal ("++src++") in "++showADL e) else
>      selectGeneric i ("fst."++src',src) ("snd."++trg',trg)
>                       ((selectExprBrac ctx i src' mid' e)++" AS fst, "++(selectExprBrac ctx i mid2' trg' f)++" AS snd")
>                       "1"
>                       where src' = sqlExprSrc e
>                             mid' = sqlExprTrg e
>                             mid2'= sqlExprSrc f
>                             trg' = noCollideUnlessTm (F (f:fx)) [mid2'] (sqlExprTrg (F (f:fx)))

Code below is basic functionality, and should be changed only when absolutely necessary.

>  selectExpr ctx i src trg (Tm mrph      ) = selectExprMorph ctx i src trg mrph
>  selectExpr ctx i src trg (Tc expr      ) = selectExpr ctx i src trg expr
>  selectExpr ctx i src trg (F  (e:(f:fx))) =
>       selectGeneric i ("fst."++src',src) ("snd."++trg',trg)
>                       (selectExprBrac ctx (i) src' mid' e++" AS fst, "++selectExprBrac ctx (i) mid2' trg' (F (f:fx))++" AS snd")
>                       ("fst."++mid'++" = snd."++mid2')
>  --  The values of  src', mid', mid2', and trg' might be fantasies (albeit distinct), but have been chosen to be meaningful names derived from source and targets.
>                       where src' = sqlExprSrc e
>                             mid' = noCollideUnlessTm e [src'] (sqlExprTrg e)
>                             mid2'= sqlExprSrc f
>                             trg' = noCollideUnlessTm (F (f:fx)) [mid2'] (sqlExprTrg (F (f:fx)))
>  selectExpr ctx i src trg (F  [e]       ) = selectExpr ctx i src trg e
>  selectExpr ctx i src trg (Fi (e:(f:fx))) = selectGeneric i ("fst."++src',src) ("fst."++trg',trg) ((selectExprBrac ctx (i) src' trg' e)++" AS fst, "++(selectExprBrac ctx (i) src'' trg'' (Fi (f:fx)))++" AS snd") ("fst."++src'++" = snd."++src''++" AND fst."++trg'++"=snd."++trg'')
>                       where src'  = sqlExprSrc e
>                             trg'  = noCollide [src'] (sqlExprTrg e)
>                             src'' = sqlExprSrc f
>                             trg'' = noCollide [src''] (sqlExprTrg f)
>  selectExpr ctx i src trg (Fi [e]) = selectExpr ctx i src trg e
>  selectExpr ctx i src trg (Fi [] ) = error ("RelBinGenBasics.lhs: Cannot create query for Fi [] because type is unknown")
>--src*trg zijn strings die aangeven wat de gewenste uiteindelijke typering van de query is (naar php of hoger in de recursie)
>--het is dus wel mogelijk om een -V te genereren van het gewenste type, maar niet om een V te genereren (omdat de inhoud niet bekend is)
>  selectExpr ctx i src trg (Fu [] ) = selectGeneric i ("''",src) ("''",trg) ("(SELECT 1) AS a") ("0")
>  selectExpr ctx i src trg (Fu es ) = (phpIndent i) ++ "(" ++ (selectExprInUnion ctx (i) src trg (Fu es)) ++ (phpIndent i) ++ ")"
>    where  -- selectExprInUnion is om de recursie te verbergen (deze veroorzaakt sql fouten)
>      selectExprInUnion ctx i src trg (Tc e         ) = selectExprInUnion ctx i src trg e
>      selectExprInUnion ctx i src trg (F  [e]       ) = selectExprInUnion ctx i src trg e
>      selectExprInUnion ctx i src trg (Fi [e]       ) = selectExprInUnion ctx i src trg e
>      selectExprInUnion ctx i src trg (Fu (e:(f:fx))) = (selectExprInUnion ctx (i) src trg e) ++ (phpIndent i) ++ ") UNION (" ++ (selectExprInUnion ctx (i) src trg (Fu (f:fx))) ++ (phpIndent i) ++ ""
>      selectExprInUnion ctx i src trg (Fu [e]       ) = selectExprInUnion ctx i src trg e
>      selectExprInUnion ctx i src trg e               = selectExpr ctx (i+4) src trg e
>  selectExpr ctx i src trg (Cp e  ) =
>       selectGeneric i ("cfst."++src',src) ("csnd."++trg',trg)
>                       (sqlConcept ctx (source e) ++ " AS cfst, "++sqlConcept ctx (target e) ++ " AS csnd")
>                       ("NOT EXISTS ("++ (selectExists' (i+12)
>                                                        ((selectExprBrac ctx (i+12) src2 trg2 e) ++ " AS cp")
>                                                        ("cfst." ++ src' ++ "=cp."++src2++" AND csnd."++ trg'++"=cp."++trg2)
>                                         ) ++ ")"
>                       )
>                       where src' = sqlAttConcept ctx (source e) 
>                             trg' = sqlAttConcept ctx (target e)
>                             src2 = sqlExprSrc e
>                             trg2 = noCollideUnlessTm e [src2] (sqlExprTrg e)
>  selectExpr ctx i src trg cl@(K0 e) = selectGeneric i (sqlExprSrc cl,src) (sqlExprTrg cl,trg) (sqlRelName ctx cl) "1"
>  selectExpr ctx i src trg cl@(K1 e) = selectGeneric i (sqlExprSrc cl,src) (sqlExprTrg cl,trg) (sqlRelName ctx cl) "1"
>  selectExpr ctx i src trg (Fd []  ) = error ("RelBinGenBasics.lhs: Cannot create query for Fd [] because type is unknown")
>  selectExpr ctx i src trg (Fd [e] ) = selectExpr ctx i src trg e
>  selectExpr ctx i src trg (Fd (e:(f:fx))) =
>       selectGeneric i ("dagger1."++src',src) ("dagger2."++trg',trg)
>                       ((selectExprBrac ctx (i) src' mid' e)++" AS dagger1, "++(selectExprBrac ctx (i) mid2' trg' (F (f:fx)))++" AS dagger2")
>                       ("NOT EXISTS ("
>                           ++ (selectExists' (i+6)
>                                             (selectSelItem (cpt, cptname))
>                                             ("dagger1."++mid' ++"<>"++cptattr++" AND dagger2."++mid2'++"<>"++cptattr)
>                              ) ++ ")"
>                       )
>                       where src' = sqlExprSrc e
>                             mid' = noCollide [src'] (sqlExprTrg e)
>                             cpt  = sqlConcept ctx (target e)
>                             cptname = noCollide [mid', mid2'] (sqlConcept ctx (target e))
>                             cptattr = cptname++"."++(sqlAttConcept ctx (target e))
>                             mid2'= noCollide [trg'] (sqlExprSrc f)
>                             trg' = sqlExprTrg (Fd (f:fx))

>--  selectExpr _ _ _ _ e = error ("RelBinGenBasics.lhs: Non-exhaustive patterns in function selectExpr for "++(showADL e))

selectExprBrac verkort de query van termen als deze in de FROM clause voorkomt:
--> SELECT .. FROM (SELECT a AS a,b AS b FROM c) AS t1
wat tussen haakjes staat kan worden vervangen door c:
--> SELECT .. FROM c AS t1
dit kan echter niet als er een hernoeming plaats vindt:
--> SELECT .. FROM (SELECT a AS a1,b AS b1 FROM c) AS t1
daarom wordt hier op getest

>  selectExprBrac ctx i src trg (Tc e  )                             = selectExprBrac ctx i src trg e
>  selectExprBrac ctx i src trg (F  [e])                             = selectExprBrac ctx i src trg e
>  selectExprBrac ctx i src trg (Fi [e])                             = selectExprBrac ctx i src trg e
>  selectExprBrac ctx i src trg (Fu [e])                             = selectExprBrac ctx i src trg e
>  selectExprBrac ctx i src trg (Tm m@(Mph _ _ _ _ _ _))
>   | (sqlMorSrc ctx m,sqlMorTrg ctx m)==(src,trg) = sqlMorName ctx m
>  selectExprBrac ctx i src trg (K0 e)
>   | (sqlExprSrc e,sqlExprTrg e)==(src,trg)                         = sqlRelName ctx (K0 e)
>  selectExprBrac ctx i src trg (K1 e)
>   | (sqlExprSrc e,sqlExprTrg e)==(src,trg)                 = sqlRelName ctx (K1 e)
>  selectExprBrac ctx i src trg expr                                 = phpIndent (i+4) ++ "( " ++ selectExpr ctx (i+6) src trg expr++ phpIndent(i+4)++")"

>  noCollide :: [String] -> String -> String
>  noCollide names name | name `elem` names = noCollide names (namepart (reverse name) ++ changeNr (numberpart (reverse name)))
>                       | otherwise = name
>                       where namepart str   = reverse (dropWhile isDigit str)
>                             numberpart str = reverse (takeWhile isDigit str)
>                             changeNr x     = decode (encode x+1)

When two attributes from the same relation share the same sql column-name, they have the same value.
In this case, it would make no sense to generate two different names. To prevent this, 'noCollideUnlessTm' is used.
This function should return diffrent names, unless they are names originating from the same table (one term) with the same value.
For instance: Tm I may return the same names, but cp(Tm I) should not since the values don't match.

>  noCollideUnlessTm (Tm _) _ name = name
>  noCollideUnlessTm _  names name = noCollide names name

>  selectExprMorph ctx i src trg mph@(V _ _)
>   = selectGeneric i ("vfst."++sqlAttConcept ctx (source mph),src) ("vsnd."++sqlAttConcept ctx (source mph),trg)
>                     (sqlConcept ctx (source mph) ++ " AS vfst, "++sqlConcept ctx (target mph) ++ " AS vsnd")
>                     "1"
>  selectExprMorph ctx i src trg mph@(Mp1 str _)
>   | src == trg = "SELECT "++str++" AS "++src
>   | otherwise  = "SELECT "++str++" AS "++src++", "++str++" AS "++trg
>  selectExprMorph ctx i src trg mph -- made for both Mph and I
>   | isIdent mph = selectGeneric i (sqlAttConcept ctx (source mph),src) (sqlAttConcept ctx (target mph),trg) (sqlConcept ctx (source mph)) "1"
>   | otherwise   = selectGeneric i (sqlMorSrc ctx mph,src) (sqlMorTrg ctx mph,trg) (sqlMorName ctx mph) "1"
>--   | otherwise   = selectGeneric i (sqlMorSrc ctx mph,trg) (sqlMorTrg ctx mph,src) (sqlMorName ctx mph) "1"

>  selectExists' i tbl whr
>   = "SELECT *" ++
>     phpIndent i ++ "  FROM " ++ tbl ++
>     phpIndent i ++ " WHERE " ++ whr
>  selectGeneric :: Int -> (String,String) -> (String,String) -> String -> String -> String
>  selectGeneric i src trg tbl whr
>   = selectcl ++
>     phpIndent i ++ "  FROM "++tbl++
>     phpIndent i ++ " WHERE "++whr
>     where selectcl | src==trg  = "SELECT DISTINCT " ++ selectSelItem src
>                    | otherwise = "SELECT DISTINCT " ++ selectSelItem src ++", "++selectSelItem trg
>  selectSelItem (att,alias) | selectSameName (att,alias) = att
>                            | otherwise                  = att++" AS "++alias
>  selectSameName (att,alias) = afterPoint att == alias
>   where afterPoint s = if (myafterPoint s == "") then s else myafterPoint s
>         myafterPoint ('.':xs) = xs
>         myafterPoint ( x :xs) = myafterPoint xs
>         myafterPoint []       = []

>  selectNormFiExpr :: String -> Context -> Int -> Expression -> (String,String) -> [(Morphism,String,String)] -> Expression -> String
>  selectNormFiExpr var ctx i expr (src,trg) subs e
>   = selectExpr ctx i src trg expr'
>     where expr' = doSubsExpr ctx var subs (shrink (conjNF e))


Temporary class 'IsClos'. The types Expression and Morphism ought to be unified in one type, so this monstrum can be resolved.

>  class IsClos e where
>   isClos :: e -> Bool
>   sqlClosName :: Context -> e -> String

>  instance IsClos Expression where
>   isClos (K0 e) = True
>   isClos (K1 e) = True
>   isClos (F [e]) = isClos e
>   isClos (Fu [e]) = isClos e
>   isClos (Fi [e]) = isClos e
>   isClos (Tm m) = isClos m
>   isClos _      = False
>   sqlClosName context (K0 e)
>    = head (["Clos"++show i++"_"++enc False (name s)| (c,i)<-zip (closE context) [0..], K0 e == c, s<-take 1 (declarations e)]++
>            [error ("(module RelBinGenBasics) Illegal closure expression in \"sqlClosName context ("++showHS "" (K0 e)++")\"\n with closE context = "++showHS "" (closE context))]
>           )
>   sqlClosName context (K1 e)
>    = head (["Clos"++show i++"_"++enc False (name s)| (c,i)<-zip (closE context) [0..], K1 e == c, s<-take 1 (declarations e)]++
>            [error ("(module RelBinGenBasics) Illegal closure expression in \"sqlClosName context ("++showHS "" (K1 e)++")\"")]
>           )
>   sqlClosName context (F [e])  = sqlClosName context e
>   sqlClosName context (Fu [e]) = sqlClosName context e
>   sqlClosName context (Fi [e]) = sqlClosName context e

>  instance IsClos Morphism where
>   isClos (Mph _ _ _ _ _ d) = isClos d
>   isClos _ = False
>   sqlClosName ctx mph@(Mph _ _ _ _ False _) = sqlClosName ctx (K0 (Tm (flp mph)))
>   sqlClosName ctx mph@(Mph _ _ _ _ _ _) = sqlClosName ctx (K0 (Tm mph))

>  instance IsClos Declaration where
>   isClos s = take 4 (name s) == "Clos"
>   sqlClosName context s | isClos s = name s

Auxiliaries:

>  phpIndent i = "\n"++[' '|n<-[1..i]]

>  closE = map (shrink.conjNF) . closExprs
>  clos0 (K0 e) = True
>  clos0 _ = False

phpShow adds backslashes to quotes in order to make a string readable for MySQL

>  phpShow :: String -> String
>  phpShow str = "'" ++ addslashes str ++ "'"

>  addslashes ('\'': cs) = "\\'"++addslashes cs
>  addslashes ('\\': cs) = "\\\\"++addslashes cs
>  addslashes (c:cs) = c:addslashes cs
>  addslashes "" = ""

>  phpRelName :: Context -> Declaration -> String
>  phpRelName context s
>   = sqlRelName context s

>  phpRelSrc :: Morphic a => Context -> a -> String
>  phpRelSrc context s
>   | homogeneous s = "Src"++phpEncode (name (source s))
>   | otherwise     = "Att"++phpEncode (name (source s))

>  phpRelTrg :: Morphic a => Context -> a -> String
>  phpRelTrg context s
>   | homogeneous s = "Trg"++phpEncode (name (target s))
>   | otherwise     = "Att"++phpEncode (name (target s))

>  phpMorName, phpMorSrc, phpMorTrg :: Context -> Morphism -> String
>  phpMorName = sqlMorName
>  phpMorSrc  = sqlMorSrc
>  phpMorTrg  = sqlMorTrg

>  phpConcept :: Context -> Concept -> String
>  phpConcept context c
>   = if null cs then error ("(module RelBinGenBasics) Concept \""++show c++"\" does not occur in context \""++name context++"\" (phpConcept in module RelBinGenBasics)") else
>     if length cs>1 then error ("(module RelBinGenBasics) Concept \""++show c++"\" is not unique in context \""++name context++"\" (phpConcept in module RelBinGenBasics)") else
>     head cs
>     where cs = ["C"++show i++"_"++phpEncode (name c')|(c',i)<-zip (concs context) [1..], c==c']

>  sqlRuleName :: Context -> Rule -> String
>  sqlRuleName context (Sg p rule expla sgn nr pn signal) = sqlRelName context signal
>  sqlRuleName context r = error ("Illegal call to sqlRuleName ("++showADL r++" on "++show (CC_aux.pos r)++")")

TODO: de nummering van declaraties geschiedt niet consequent. Dus moet het maar opnieuw genummerd worden

>  sqlRelName :: (ShowHS m,Morphic m,Morphical m,IsClos m) => Context -> m -> String
>  sqlRelName context m
>   = if isIdent m then sqlConcept context (source m) else
>     if isClos m then sqlClosName context m else
>     if isTrue m then "V" else
>     if null as then error ("(module RelBinGenBasics) Fatal error in RelBinGen.lhs (sqlRelName): No declarations in "++showHS "" m) else
>     if length as>1 then error ("(module RelBinGenBasics) Fatal error in RelBinGen.lhs (sqlRelName): Multiple declarations in "++showHS "" m) else
>     if null ts then error ("(module RelBinGenBasics) Declaration \""++showHS "" m++"\" does not occur in context \""++name context++"\" (sqlRelName in module RelBinGenBasics)\n"++showHS "" (declarations (closExprs context) ++ declarations context)) else
> --  if length ts>1 then error ("(module RelBinGenBasics) Declaration \""++show a++"\" is not unique in context \""++name context++"\" (sqlRelName in module RelBinGenBasics) "++show ts) else
>     head ts
>     where ts = ["T"++show i++"_"++enc False (name s)
>                |(i,s)<-zip [1..] (declarations context), a==s]
>              -- error(chain "\n" (map (showHS "") (filter isSgnl (declarations context)))) --
>           as = declarations m
>           a = head as

sqlExprSrc and sqlExprTrg provide attribute names for SQL queries of an entire expression.
These names may be fantasy, as long as source and target get distinct names.
The following implementation should therefore work:
  sqlExprSrc e = "Src"
  sqlExprTrg e = "Trg"
However, we provide slightly more meaningful names, in order to facilitate reading of the SQL code.

>  sqlExprSrc :: Expression -> String
>  sqlExprSrc (F [])   = error ("(Module RelBinGenBasics: ) calling sqlExprSrc (F [])")
>  sqlExprSrc (F [f])  = sqlExprSrc f
>  sqlExprSrc (F fs)   = sqlExprSrc (head fs)
>  sqlExprSrc (Fu [])  = error ("(Module RelBinGenBasics: ) calling sqlExprSrc (Fu [])")
>  sqlExprSrc (Fu [f]) = sqlExprSrc f
>  sqlExprSrc (Fu fs)  = sqlExprSrc (head (filter l fs)) where l = (==foldr1 lub (map source fs)).source
>  sqlExprSrc (Fi [])  = error ("(Module RelBinGenBasics: ) calling sqlExprSrc (Fi [])")
>  sqlExprSrc (Fi [f]) = sqlExprSrc f
>  sqlExprSrc (Fi fs)  = sqlExprSrc (head (filter l fs)) where l = (==foldr1 lub (map source fs)).source
>  sqlExprSrc (Fd [])  = error ("(Module RelBinGenBasics: ) calling sqlExprSrc (Fd [])")
>  sqlExprSrc (Fd [f]) = sqlExprSrc f
>  sqlExprSrc (Fd fs)  = sqlExprSrc (head fs)
>  sqlExprSrc (Cp e)   = sqlExprSrc e
>  sqlExprSrc (K0 e)   = sqlExprSrc e
>  sqlExprSrc (K1 e)   = sqlExprSrc e
>  sqlExprSrc (Tm (Mp1 _ t)) = "Att"++phpEncode (name t)
>  sqlExprSrc (Tm m) | inline m  = sqlRelSrc (declaration m)
>                    | otherwise = sqlRelTrg (declaration m)
>  sqlExprSrc e        = error ("unexhaustive pattern in RelBinGenBasics.lhs in sqlExprSrc for "++showHS "" e)

>  sqlExprTrg :: Expression -> String
>  sqlExprTrg e = sqlExprSrc (flp e)


>  sqlRelSrc :: Declaration -> String
>  sqlRelSrc s
>   | isIdent s = "Att"++phpEncode (name src)
>   | src==trg  = "Src"++phpEncode (name src)
>   | otherwise = "Att"++phpEncode (name src)
>   where src = source s
>         trg = target s

>  sqlRelTrg :: Declaration -> String
>  sqlRelTrg s
>   | isIdent s = "Att"++phpEncode (name trg)
>   | src==trg  = "Trg"++phpEncode (name trg)
>   | otherwise = "Att"++phpEncode (name trg)
>   where src = source s
>         trg = target s

>  sqlMorName :: Context -> Morphism -> String
>  sqlMorName context (Mph nm pos atts sgn yin s) = sqlRelName context s
>  sqlMorName context (I atts g s yin)            = sqlConcept context s
>  sqlMorName context m = error ("(module RelBinGenBasics) sqlMorName: illegal argument: "++showHS "" m)

sqlMorSrc and sqlMorTrg are exclusively meant to generate code

 >  sqlMorSrc :: Context -> Morphism -> String
 >  sqlMorSrc context m
 >   | isIdent m                 = "Att"++phpEncode (name (source m))
 >   | homogeneous m && inline m = "Src"++phpEncode (name (source m))
 >   | homogeneous m             = "Trg"++phpEncode (name (source m))
 >   | otherwise                 = "Att"++phpEncode (name (source m))

sqlMorSrc (r :A*A) = "SrcA"
sqlMorSrc (r~:A*A) = "TrgA"
sqlMorSrc (r :A*B) = "AttA"
sqlMorSrc (r~:A*B) = "AttB"
sqlMorTrg (r :A*A) = "TrgA"
sqlMorTrg (r~:A*A) = "SrcA"
sqlMorTrg (r :A*B) = "AttB"
sqlMorTrg (r~:A*B) = "AttA"

 >  sqlMorTrg :: Context -> Morphism -> String
 >  sqlMorTrg context m
 >   | isIdent m                 = "Att"++phpEncode (name (target m))
 >   | homogeneous m && inline m = "Trg"++phpEncode (name (target m))
 >   | homogeneous m             = "Src"++phpEncode (name (target m))
 >   | otherwise                 = "Att"++phpEncode (name (target m))

>  sqlMorSrc :: Context -> Morphism -> String
>  sqlMorSrc context m
>   | isIdent m                 = "Att"++phpEncode (name (source m))
>   | homogeneous d && inline m = "Src"++phpEncode (name (source d))
>   | homogeneous d             = "Trg"++phpEncode (name (target d))
>   | inline m                  = "Att"++phpEncode (name (source d))
>   | otherwise                 = "Att"++phpEncode (name (target d))
>   where d = declaration m

sqlMorSrc (r :A*A) = "SrcA"
sqlMorSrc (r~:A*A) = "TrgA"
sqlMorSrc (r :A*B) = "AttA"
sqlMorSrc (r~:A*B) = "AttB"
sqlMorTrg (r :A*A) = "TrgA"
sqlMorTrg (r~:A*A) = "SrcA"
sqlMorTrg (r :A*B) = "AttB"
sqlMorTrg (r~:A*B) = "AttA"

>  sqlMorTrg :: Context -> Morphism -> String
>  sqlMorTrg context m
>   | isIdent m                 = "Att"++phpEncode (name (target m))
>   | homogeneous d && inline m = "Trg"++phpEncode (name (target d))
>   | homogeneous d             = "Src"++phpEncode (name (source d))
>   | inline m                  = "Att"++phpEncode (name (target d))
>   | otherwise                 = "Att"++phpEncode (name (source d))
>   where d = declaration m


>  sqlConcept :: Context -> Concept -> String
>  sqlConcept = sqlConc "C"
>  sqlEConcept :: Context -> Concept -> String
>  sqlEConcept = sqlConc "E"

>  sqlConc prefix context c
>   | name c == "ONE" = "ConcONE"
>   | otherwise = if null cs then error ("(module RelBinGenBasics) Concept \""++show c++"\" does not occur in context \""++name context++"\" (sqlConcept in module RelBinGenBasics)") else
>                 if length cs>1 then error ("(module RelBinGenBasics) Concept \""++show c++"\" is not unique in context \""++name context++"\" (sqlConcept in module RelBinGenBasics)") else
>                 head cs
>                 where cs = [prefix++show i++"_"++phpEncode (name c')|(c',i)<-zip (concs context) [1..], c==c']

>  sqlAttConcept :: Context -> Concept -> String
>  sqlAttConcept context c
>   | name c == "ONE" = "AttONE" -- eventually, this concept should be implemented by other means
>   | otherwise = if null cs then error ("(module RelBinGenBasics) Concept \""++show c++"\" does not occur in context \""++name context++"\" (sqlAttConcept in module RelBinGenBasics)") else
>                 if length cs>1 then error ("(module RelBinGenBasics) Concept \""++show c++"\" is not unique in context \""++name context++"\" (sqlAttConcept in module RelBinGenBasics)") else
>                 head cs
>                 where cs = ["Att"++phpEncode (name c')|(c',i)<-zip (concs context) [1..], c==c']

>  ifAs str str' = if str == str' then str else str'++" AS "++str


For all ASCII characters c, htmlDec (htmlEnc c) = c

>  phpCount var context m = "count("++var++"['"++sqlMorName context m++"'])"
>  phpNtCnt var context m = "!count("++var++"['"++sqlMorName context m++"'])"

>  phpIsset var context m = "isset("++var++"['"++sqlMorName context m++"'])"
>  phpIsnet var context m = "!isset("++var++"['"++sqlMorName context m++"'])"

>  isSub ctx subs a               = a `elem` [sqlMorName ctx r| (r,x,y)<-subs]
>  doSubSrc ctx subs var x a t    = if isSub ctx subs a then "\\''.addslashes("++var++"['"++doSrc ctx subs x a++"']).'\\'"         else a++"."++t
>  doSubTrg ctx subs var x a t    = if isSub ctx subs a then "\\''.addslashes("++var++"['"++doTrg ctx subs x a++"']).'\\'"         else a++"."++t
>  dASubSrc ctx subs var x a as t = if isSub ctx subs a then "\\''.addslashes("++var++"['"++doSrc ctx subs x a++"']).'\\' AS "++as else a++"."++ifAs as t
>  dASubTrg ctx subs var x a as t = if isSub ctx subs a then "\\''.addslashes("++var++"['"++doTrg ctx subs x a++"']).'\\' AS "++as else a++"."++ifAs as t
>  doSrc ctx subs x a             = head [if inline x then p else q| (r,p,q)<-subs, a==sqlMorName ctx r]
>  doTrg ctx subs x a             = head [if inline x then q else p| (r,p,q)<-subs, a==sqlMorName ctx r]


For the following functions, enc and dec, we have:
 for all c:Bool and str:String ::     dec c (enc c str) == str
  
>  phpEncode = enc True

  phpDecode = dec True


>  phpCodeTransactionStart context noTransactions
>   = if noTransactions then "" else
>     chain "\n   "
>     [ "DB_doquer('START TRANSACTION');"
>     , "//DB_doquer('LOCK TABLES "
>       ++ chain ", " ([sqlRelName context s ++ " WRITE" | s<-declarations context ]++
>                      [sqlConcept context c ++ " WRITE" | c<-concs context ]        ) ++
>                   "');"
>     , "// transaction started"
>     ]

>  phpCodeTransactionClose context noTransactions okVar
>   = if noTransactions then "     return("++okVar++");" else
>     if(null (rules context))
>        then chain "\n"
>               [ "DB_doquer('COMMIT');"
>               , "   // DB_doquer('UNLOCK TABLES');"
>               , "  return("++okVar++");"
>               ]
>        else chain "\n   "
>               [ "if(" ++ (chain " && " [ "checkRule"++show (nr r)++"()" | r<-rules context {-TODO: ++multRules context -} ])  ++ "){"
>               , "     DB_doquer('COMMIT');"
>               , "  // DB_doquer('UNLOCK TABLES');"
>               , "     return("++okVar++");"
>               , "}else{"
>               , "     DB_doquer('ROLLBACK');"
>               , "  // DB_doquer('UNLOCK TABLES');"
>               , "     return(false);"
>               , "}" ]

>  insConcept context c selStmt
>   = chain "\n        "
>      [ "DB_doquer('INSERT IGNORE INTO "++sqlConcept context c'++" ("++sqlAttConcept context c'++") "++selStmt++"');"
>      | c'<-concs context, c' <= c]
>  insConcepts context hcs n c str excludeRels
>   = [' '|i<-[1..n]]++
>     chain ("\n"++[' '|i<-[1..n]])
>      [ "DB_doquer('INSERT IGNORE INTO "++sqlConcept context c'++" ("++sqlAttConcept context c'++") VALUES (\\''.addslashes($attrs['"++str++"']).'\\')');"
>      | c'<-concs context, c' <= c]++
>     concat
>      [ "\n"++[' '|i<-[1..n-3]]++"// "++informalRule {-[sIs c]-} (fOp, Tm (mIs c), bOp, toExpr, frExpr, rule)++"\n"++[' '|i<-[1..n]]++
>        "if(isset($attrs['"++str++"']))" ++
>        "DB_doquer('INSERT IGNORE INTO "++sqlMorName context s++" VALUES (\\''.addslashes($attrs['"++str++"']).'\\', \\''.addslashes($attrs['"++str++"']).'\\')');"
>      | (fOp, e, bOp, toExpr, frExpr, rule)<-computeOrder hcs "INSERT INTO" [sIs c], isIdent toExpr
>      , sign frExpr <= sign c
>      , oneMorphism toExpr
>      , s<-mors toExpr, not (declaration s `elem` excludeRels)]

>  dbDelConcept context i c atomVar
>   = [' '| x<-[1..i]]++"DB_doquer('DELETE FROM "++sqlConcept context c++" WHERE "++sqlAttConcept context c++"=\\''.addslashes("++atomVar++").'\\' AND NOT EXISTS ("++chain " UNION " (["SELECT "++sqlRelSrc s++" FROM "++sqlRelName context s++" WHERE "++sqlRelSrc s++"=\\''.addslashes("++atomVar++").'\\'" | s<-declarations context, source s==c]++["SELECT "++sqlRelTrg s++" FROM "++sqlRelName context s++" WHERE "++sqlRelTrg s++"=\\''.addslashes("++atomVar++").'\\'" | s<-declarations context, target s==c])++")');"


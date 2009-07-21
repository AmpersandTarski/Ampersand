module Prototype.RelBinGenBasics(phpIdentifier,
                         selectExpr,sqlExprTrg,sqlExprSrc,addSlashes,sqlMorName
                        ,sqlConcept,sqlAttConcept,sqlMorSrc
                        ,sqlClosName,closE,sqlRelName,sqlRelSrc,sqlRelTrg
                        ,phpShow,insConcept
                        ,selectNormFiExpr,clos0,pDebug,noCollide,plugs) where
   import Char(isDigit,digitToInt,intToDigit,isAlphaNum)
   --import Auxiliaries(chain,enc) --TODO -> see Garbage
   import Strings (chain) --TODO -> is this correct instead of chain from Auxiliaries?
   import Adl
   import ShowADL(showADL)
   import ShowHS(showHS,ShowHS()) --TODO -> export ShowHS() enabled, See comment at function sqlRelName
 --  import CC_aux ( isProperty,oneMorphism)  TODO-> see Garbage
   import CommonClasses(lub)
   import Calc(informalRule, computeOrder)
   import ComputeRule (ComputeRule(..))
   import NormalForms (conjNF)
 --  import MultRules
   import Data.Fspec
   import Data.Plug
   import Auxiliaries
   import Collection (rd,(>-))
   import Collection (Collection(rd,uni))
   import Prototype.Garbage --TODO -> clean up Garbage

   pDebug   = True
   pNoDebug = not pDebug

   --TODO -> is this a correct translation ánd clean-up of Morphical Context? 
   --I think its better to make morphical redundant all along, and replace it with fspec and functions on fspec
   --Gerard talked to Stef on the phone:
   --   Fspc functions like vrels exist for the same reason as Morphical exists for Adl data types -> a common function for a certain information set
   --   The difference is that Fspc as data type is introduced as the central data type with (precalculated) functions like vrels
   --   Besides that the implementation of a Morphical instance is not clear anymore
   --   For that reason the Morphical instance of Fspc is placed here instead of in Morphical.hs.
   --   TODO -> We have to think about a structure to agree upon
   instance Morphical Fspc where
     concs fspc = concs (vrels fspc) `uni` concs (vrules fspc) --I assumed that rules contains every expression from kd, obj, etc
     mors  fspc = mors (vrules fspc)
     morlist fspc = morlist (vrules fspc)
     declarations fspc = vrels fspc
     closExprs fspc = closExprs (vrules fspc)

 --sqlCodeComputeRule: 4th argument is (as of sept 25th 07) empty. It used to be:
 --           substitutions = [ if inline m
 --                             then (m,phpMorSrc context m,phpMorTrg context m)
 --                             else (m,phpMorTrg context m,phpMorSrc context m)
 --                           | m<-mors e, makeDeclaration m==r]
 --Het substitueren van een hele relatie met de ingetikte atomen vanuit de interface mag alleen maar onder condities, die we nog niet begrijpen. Vandaar.
 --Zonder substituties rekent sqlCodeComputeRule dus met de hele relatie, en is daardoor minder efficient (maar wel correct).
 --Een substitutie (m,s,t) betekent dat de relatie m wordt vervangen door twee atomen uit de interface, aangeduid met s en t.
 --Let op: als m geflipt is, dan moeten de twee ingetikte atomen op de juiste plaats komen, en dus worden verwisseld.
 --
 --Als Tm [m'] = frExpr dan hoeft alleen het veld uit de interface (opgeslagen in de PHP variabele attrs) te worden overgenomen

 -- Pre: oneMorphism toExpr
   sqlCodeComputeRule :: String -> Int -> Fspc -> [(Morphism,String,String)] -> ComputeRule -> String
   sqlCodeComputeRule attrs i fSpec subs hc@(CR fOp e "INSERT INTO" toExpr frExpr rule)
    = if srcTo==trgTo && not (isProperty toExpr) then error ("(Module RelBinGenBasics) Fatal: srcTo and trgTo are equal ("++srcTo++") in sqlCodeComputeRule.\n"++show hc) else
    {-if Tm [m'] == frExpr
      then "'INSERT IGNORE INTO "++sqlMorName context m++
           phpIndent i ++ "VALUES (\\''.addSlashes("++attrs++"['"++srcFr++"']).'\\',"++
                                 " \\''.addSlashes("++attrs++"['"++trgFr++"']).'\\')'"
      else -}
      "'INSERT IGNORE INTO "++sqlMorName fSpec m++
           selectExpr fSpec i srcTo trgTo frExpr'++"'"
      where m    = head (mors toExpr)
            m'   = head (mors frExpr)
            srcFr | isIdent frExpr     = sqlConcept fSpec (source frExpr)
                  | otherwise          = if inline m' then sqlMorSrc fSpec m else sqlMorTrg fSpec m
            srcTo = sqlExprSrc fSpec toExpr
            trgFr | isIdent frExpr     = sqlConcept fSpec (source frExpr)
                  | otherwise          = if inline m' then sqlMorTrg fSpec m else sqlMorSrc fSpec m
            trgTo = sqlExprTrg fSpec toExpr -- may not collide with srcTo, but what if toExpr is a property (or identity)? (Bas?)
            frExpr' = doSubsExpr fSpec attrs subs frExpr
   sqlCodeComputeRule attrs i fSpec subs hc@(CR fOp e "DELETE FROM" toExpr (Cp frExpr) rule)
    = if null froms
      then "'DELETE FROM "++tbl++phpIndent i++
           "WHERE "++tbl++"."++src++"<>"++doSubSrc fSpec subs "$qa[0]" m (sqlMorName fSpec m') (sqlMorSrc fSpec m')++
           " OR "  ++tbl++"."++trg++"<>"++doSubTrg fSpec subs "$qa[0]" m (sqlMorName fSpec m') (sqlMorTrg fSpec m')++"'"
      else "'DELETE FROM "++tbl++phpIndent i++
           "WHERE NOT EXISTS "++
           "("++(selectExists' i
                               ((selectExprBrac fSpec (i+2) src trg (doSubsExpr fSpec attrs subs frExpr))++" AS f")
                               ("f."++src++"="++tbl++"."++src++" AND f."++trg++"="++tbl++"."++trg)
                )++
           ")'"
      where m  = head (mors toExpr)
            m' = head (mors frExpr)
            tbl   = sqlMorName fSpec m
            src   = sqlMorSrc fSpec m
            trg   = sqlMorTrg fSpec m
            frExpr' = doSubsExpr fSpec attrs subs frExpr
            froms = [m| m<-mors frExpr, not (isSub fSpec subs (sqlMorName fSpec m))]
   sqlCodeComputeRule attrs i fSpec subs hc@(CR fOp e "DELETE FROM" toExpr frExpr rule)
    = if null froms
      then "'DELETE FROM "++tbl++phpIndent i++
           "WHERE "++tbl++"."++src++"="++doSubSrc fSpec subs "$qa[0]" m (sqlMorName fSpec m') (sqlMorSrc fSpec m')++
           " AND " ++tbl++"."++trg++"="++doSubTrg fSpec subs "$qa[0]" m (sqlMorName fSpec m') (sqlMorTrg fSpec m')++"'"
      else "'DELETE FROM "++tbl++phpIndent i++"WHERE EXISTS "++
           "("++(selectExists' i
                               ((selectExprBrac fSpec (i+2) src trg (doSubsExpr fSpec attrs subs frExpr))++" AS f")
                               ("f."++src++"="++tbl++"."++src++" AND f."++trg++"="++tbl++"."++trg)
                )++
           ")'"
      where m  = head (mors toExpr)
            m' = head (mors frExpr)
            tbl   = sqlMorName fSpec m
            src   = sqlMorSrc fSpec m
            trg   = sqlMorTrg fSpec m
            froms = [m| m<-mors frExpr, not (isSub fSpec subs (sqlMorName fSpec m))]
 
   doSubsExpr ::    Fspc
                 -> String   -- substitute by this PHP variable (e.g. "$x")
                 -> [(Morphism,String,String)] -- substitutions
                 -> Expression -- without substitutions made
                 -> Expression -- with substitutions made

   doSubsExpr _ _ [] expr = expr
   doSubsExpr fSpec  -- the current context
              to   -- "$attrs"
              s    -- the list of substitutions. Each substitution is an (r::Morphism,a,q)
              expr -- 
    = sub expr
      where sub (F  e) = F  (map sub e)
            sub (Fd e) = Fd (map sub e)
            sub (Fi e) = Fi (map sub e)
            sub (Fu e) = Fu (map sub e)
            sub (Cp e) = Cp (sub e)
            sub (K0 e) = K0 (sub e)
            sub (K1 e) = K1 (sub e)
            sub (Tm m@(Mph{})) = subMorph [ x | x@(r,_,_)<-s, sqlMorName fSpec m == sqlMorName fSpec r] m
            sub (Tm m) = Tm m
            sub (Tc e) = Tc (sub e)
            subMorph trs m | isIdent m = mp1 to (head [a|(r,a,_)<-s,sqlMorName fSpec m == sqlMorName fSpec r]) (source m)
            subMorph []  m             = Tm m -- nothing to replace
  --          subMorph ((_,p,q):trs)   m = F [mp1 to p (source m),v (source m,target m),mp1 to q (target m)]
            subMorph ((r,p,q):trs)   m | inline m  = F [mp1 to p (source m),v (source m,target m),mp1 to q (target m)]
                                       | otherwise = F [mp1 to q (source m),v (source m,target m),mp1 to p (target m)]
            mp1 var a = Tm . Mp1 ("\\''.addSlashes("++var++"['"++a++"']).'\\'")

   selectRule :: Fspc -- current context
                 -> Int  -- indentation
                 -> Rule -- rule to be translated
                 -> String -- resulting SQL expression
   selectRule fSpec i r@(Ru _ _ _ _ _ _ _ _ _)
    = if src==trg && not (isProperty e) then error ("(Module RelBinGenBasics) Fatal: src and trg are equal ("++src++") in selectRule.\n"++showADL r) else
      selectExpr fSpec i src trg e
      where src  = sqlExprSrc fSpec e
            trg  = sqlExprTrg fSpec e -- may not collide with src, but what if toExpr is a property (or identity)? (Bas?)
  -- was:   trg  = noCollide [src] (sqlExprTrg fSpec e)
  -- might be?: trg  = noCollideUnlessTm e [src] (sqlExprTrg fSpec e)
            e    = (conjNF.Cp .normExpr) r
   selectRule fSpec i r
    = error ("(module RelBinGenBasics) Fatal: This rule should never occur in selectRule fSpec i ("++showHS "" r++")") -- verified in AGtry.ag vs. 0.7.6 on May 1st 2006 (by SJO)



   selectExpr ::    Fspc    -- current context
                 -> Int        -- indentation
                 -> String     -- SQL name of the source of this expression, as assigned by the environment 
                 -> String     -- SQL name of the target of this expression, as assigned by the environment
                 -> Expression -- expression to be translated
                 -> String     -- resulting SQL expression

   selectExpr fSpec i src trg (Fi lst'@(_:_:_))
    = selectGeneric i ("isect0."++src',src) ("isect0."++trg',trg)
                           (chain ", " exprbracs) (chain " AND " (wherecl))
      where src'    = sqlExprSrc fSpec fst
            trgC    = sqlExprTrg fSpec fst -- can collide with src', for example in case fst==r~;r, or if fst is a property (or identity)
            trg'    = noCollideUnlessTm fst [src'] trgC
            fst     = head posTms  -- always defined, because length posTms>0 (ensured in definition of posTms)
            mp1Tm   = take 1 ([t| t@(Tm (Mp1 _ _))<-lst']++[t| t@(F ((Tm (Mp1 _ _)):(Tm (V _ _)):(Tm (Mp1 _ _)):[])) <- lst'])
            lst     = [t|t<-lst', not (elem t mp1Tm)]
            posTms  = if null posTms' then map notCp (take 1 negTms') else posTms' -- we take a term out of negTms' if we have to, to ensure length posTms>0
            negTms  = if null posTms' then tail negTms' else negTms' -- if the first term is in posTms', don't calculate it here
            posTms' = [t| t<-lst, isPos t && not (isIdent t)]++[t| t<-lst, isPos t && isIdent t] -- the code to calculate I is better if it is not the first term
            negTms' = [notCp t| t<-lst, isNeg t && isIdent t]++[notCp t| t<-lst, isNeg t && not (isIdent t)] -- should a negTerm become a posTerm (for reasons described above), it can best be an -I.
            exprbracs = [ (selectExprBrac fSpec (i) src'' trg'' l) ++ " AS isect"++show n 
                        | (n,l)<-zip [0..] posTms
                        , src''<-[sqlExprSrc fSpec l]
                        , trg''<-[noCollideUnlessTm l [src''] (sqlExprTrg fSpec l)]
                        ]
            wherecl   = [if isIdent l
                         then  "isect0."++src'++" = isect0."++trg' -- this is the code to calculate ../\I. The code below will work, but is longer
                         else "(isect0."++src'++" = isect"++show n++"."++src''
                         ++ " AND isect0."++trg'++" = isect"++show n++"."++trg''++")"
                        | (n,l)<-tail (zip [0..] posTms) -- not empty because of definition of posTms
                        , src''<-[sqlExprSrc fSpec l]
                        , trg''<-[noCollideUnlessTm l [src''] (sqlExprTrg fSpec l)]
                        ]++
                        [ "isect0."++src'++" = "++s -- sorce and target are equal because this is the case with Mp1
                        | (Tm (Mp1 s _)) <- mp1Tm
                        ]++
                        [ "isect0."++src'++" = "++s1 -- sorce and target are unequal
                          ++ " AND isect0."++trg'++" = "++s2 -- sorce and target are unequal
                        | (F ((Tm (Mp1 s1 _)):(Tm (V _ _)):(Tm (Mp1 s2 _)):[])) <- mp1Tm
                        ]++
                        [if isIdent l
                         then  "isect0."++src'++" <> isect0."++trg' -- this code will calculate ../\-I
                         else  "NOT EXISTS ("++(selectExists' (i+12)
                                                              ((selectExprBrac fSpec (i+12) src'' trg'' l) ++ " AS cp")
                                                              ("isect0."++src' ++ "=cp."++src''++" AND isect0."++ trg'++"=cp."++trg'')
                                            )++")"
                        | (n,l)<-zip [0..] negTms
                        , src''<-[sqlExprSrc fSpec l]
                        , trg''<-[noCollideUnlessTm l [src''] (sqlExprTrg fSpec l)]
                        ]



   selectExpr fSpec i src trg (F (Tm (V _ (s,t)):fs)) | s==cptS = selectExpr fSpec i src trg (F (Tm (Mp1 "1" t):fs))
   selectExpr fSpec i src trg (F (s1@(Tm (Mp1 _ _)):(s2@(Tm (V _ _)):(s3@(Tm (Mp1 _ _)):fx@(_:_))))) -- to make more use of the thing below
     =  selectExpr fSpec i src trg (F ((F (s1:s2:s3:[])):fx))

   selectExpr fSpec _ src trg (F ((Tm s@(Mp1 sr _)):((Tm (V _ _)):((Tm t@(Mp1 tr _)):[])))) -- this will occur quite often because of doSubsExpr
     = "SELECT "++sr++" AS "++src++", "++tr++" AS "++trg

   selectExpr fSpec i src trg (F (e@(Tm (Mp1 sr _)):(f:fx))) = -- this will occur because of ObjBinGenObject's way of doing a read
        selectGeneric i ("fst."++src',src) ("fst."++trg',trg)
                        (selectExprBrac fSpec (i) src' trg' (F (f:fx))++" AS fst")
                        ("fst."++src'++" = "++sr)
                        where src' = sqlExprSrc fSpec e
                              trg' = noCollideUnlessTm (F (f:fx)) [src'] (sqlExprTrg fSpec (F (f:fx)))

   selectExpr fSpec i src trg (F (e:((Tm (V _ _)):(f:fx)))) = -- prevent calculating V in this case
       if src==trg && not (isProperty e) then error ("(Module RelBinGenBasics: selectExpr 2) src and trg are equal ("++src++") in "++showADL e) else
       selectGeneric i ("fst."++src',src) ("snd."++trg',trg)
                        ((selectExprBrac fSpec i src' mid' e)++" AS fst, "++(selectExprBrac fSpec i mid2' trg' f)++" AS snd")
                        "1"
                        where src' = sqlExprSrc fSpec e
                              mid' = sqlExprTrg fSpec e
                              mid2'= sqlExprSrc fSpec f
                              trg' = noCollideUnlessTm (F (f:fx)) [mid2'] (sqlExprTrg fSpec (F (f:fx)))


   selectExpr fSpec i src trg (Tm (V _ (s,t))   ) 
         | s==cptS && t==cptS = selectGeneric i ("1",src) ("1",trg)
                                                ("(SELECT 1) AS csnd")
                                                ("1"
                                                )
         | s==cptS            = selectGeneric i ("1",src) ("csnd."++trg',trg) 
                                                (sqlConcept fSpec t ++ " AS csnd")
                                                ("1"
                                                )
         | t==cptS            = selectGeneric i ("cfst."++src',src) ("1",trg)
                                                (sqlConcept fSpec s ++ " AS cfst")
                                                ("1"
                                                )
         | otherwise          = selectGeneric i ("cfst."++src',src) ("csnd."++trg'',trg)
                                                (sqlConcept fSpec s ++ " AS cfst, "++selectExprBrac fSpec (i) trg'' trg'' (Tm (mIs t))++" AS csnd")
                                                ("1"
                                                )
                        where src'  = if s==Anything
                                      then error ("!Fatal in module RelBinGenBasics: selectExpr fSpec i "++src++" "++trg++" (Tm (V _ ("++name s++","++name t++"))   )")
                                      else sqlAttConcept fSpec s
                              trg'  = if t==Anything
                                      then error ("!Fatal in module RelBinGenBasics: selectExpr fSpec i "++src++" "++trg++" (Tm (V _ ("++name s++","++name t++"))   )")
                                      else sqlAttConcept fSpec t
                              trg'' = noCollide [src'] trg'

   selectExpr fSpec i src trg (Tm (I _ s _ _)   ) | s == cptS = selectExpr fSpec i src trg (Tm (V [] (s,s)))

   selectExpr fSpec i src trg (Tm mrph      ) = selectExprMorph fSpec i src trg mrph
   selectExpr fSpec i src trg (Tc expr      ) = selectExpr fSpec i src trg expr
   selectExpr fSpec i src trg (F  (e:(f:fx))) =
        selectGeneric i ("fst."++src',src) ("snd."++trg',trg)
                        (selectExprBrac fSpec (i) src' mid' e++" AS fst, "++selectExprBrac fSpec (i) mid2' trg' (F (f:fx))++" AS snd")
                        ("fst."++mid'++" = snd."++mid2')
   --  The values of  src', mid', mid2', and trg' can be anything, as long as they are distinct. Here they have been chosen (arbitrarily) to be meaningful names derived from source and targets.
                        where src' = sqlExprSrc fSpec e
                              mid' = noCollideUnlessTm e [src'] (sqlExprTrg fSpec e)
                              mid2'= sqlExprSrc fSpec f
                              trg' = noCollideUnlessTm (F (f:fx)) [mid2'] (sqlExprTrg fSpec (F (f:fx)))
   selectExpr fSpec i src trg (F  [e]       ) = selectExpr fSpec i src trg e
   selectExpr fSpec i src trg (Fi (e:(f:fx))) = selectGeneric i ("fst."++src',src) ("fst."++trg',trg) ((selectExprBrac fSpec (i) src' trg' e)++" AS fst, "++(selectExprBrac fSpec (i) src'' trg'' (Fi (f:fx)))++" AS snd") ("fst."++src'++" = snd."++src''++" AND fst."++trg'++"=snd."++trg'')
                        where src'  = sqlExprSrc fSpec e
                              trg'  = noCollide [src'] (sqlExprTrg fSpec e)
                              src'' = sqlExprSrc fSpec f
                              trg'' = noCollide [src''] (sqlExprTrg fSpec f)
   selectExpr fSpec i src trg (Fi [e]) = selectExpr fSpec i src trg e
   selectExpr fSpec i src trg (Fi [] ) = error ("RelBinGenBasics.lhs: Cannot create query for Fi [] because type is unknown")
 --src*trg zijn strings die aangeven wat de gewenste uiteindelijke typering van de query is (naar php of hoger in de recursie)
 --het is dus wel mogelijk om een -V te genereren van het gewenste type, maar niet om een V te genereren (omdat de inhoud niet bekend is)
   selectExpr fSpec i src trg (Fu [] ) = selectGeneric i ("\\'\\'",src) ("\\'\\'",trg) ("(SELECT 1) AS a") ("0")
   selectExpr fSpec i src trg (Fu es ) = (phpIndent i) ++ "(" ++ (selectExprInUnion fSpec (i) src trg (Fu es)) ++ (phpIndent i) ++ ")"
     where  -- selectExprInUnion is om de recursie te verbergen (deze veroorzaakt sql fouten)
       selectExprInUnion fSpec i src trg (Tc e         ) = selectExprInUnion fSpec i src trg e
       selectExprInUnion fSpec i src trg (F  [e]       ) = selectExprInUnion fSpec i src trg e
       selectExprInUnion fSpec i src trg (Fi [e]       ) = selectExprInUnion fSpec i src trg e
       selectExprInUnion fSpec i src trg (Fu (e:(f:fx))) = (selectExprInUnion fSpec (i) src trg e) ++ (phpIndent i) ++ ") UNION (" ++ (selectExprInUnion fSpec (i) src trg (Fu (f:fx))) ++ (phpIndent i) ++ ""
       selectExprInUnion fSpec i src trg (Fu [e]       ) = selectExprInUnion fSpec i src trg e
       selectExprInUnion fSpec i src trg e               = selectExpr fSpec (i+4) src trg e
   selectExpr fSpec i src trg (Cp (Tm (V _ _))) = selectExpr fSpec i src trg (Fu [])
   selectExpr fSpec i src trg (Cp e  ) =
        selectGeneric i ("cfst."++src',src) ("csnd."++trg',trg)
                        (sqlConcept fSpec (source e) ++ " AS cfst, "++selectExprBrac fSpec (i) trg' trg' (Tm (mIs (target e)))++" AS csnd")
                        ("NOT EXISTS ("++ (selectExists' (i+12)
                                                         ((selectExprBrac fSpec (i+12) src2 trg2 e) ++ " AS cp")
                                                         ("cfst." ++ src' ++ "=cp."++src2++" AND csnd."++ trg'++"=cp."++trg2)
                                          ) ++ ")"
                        )
                        where src' = sqlAttConcept fSpec (source e) 
                              trg' = noCollide [src'] (sqlAttConcept fSpec (target e))
                              src2 = sqlExprSrc fSpec e
                              trg2 = noCollideUnlessTm e [src2] (sqlExprTrg fSpec e)
   selectExpr fSpec i src trg cl@(K0 e) = selectGeneric i (sqlExprSrc fSpec cl,src) (sqlExprTrg fSpec cl,trg) (sqlRelName fSpec cl) "1"
   selectExpr fSpec i src trg cl@(K1 e) = selectGeneric i (sqlExprSrc fSpec cl,src) (sqlExprTrg fSpec cl,trg) (sqlRelName fSpec cl) "1"
   selectExpr fSpec i src trg (Fd []  ) = error ("RelBinGenBasics.lhs: Cannot create query for Fd [] because type is unknown")
   selectExpr fSpec i src trg (Fd [e] ) = selectExpr fSpec i src trg e
   selectExpr fSpec i src trg (Fd fxs) = selectExpr fSpec i src trg $ Cp {e=F (map addcompl fxs)}
         where
         addcompl fx@(Cp{}) = e fx
         addcompl fx = Cp{e=fx}


   selectExprBrac fSpec i src trg (Tc e  )                             = selectExprBrac fSpec i src trg e
   selectExprBrac fSpec i src trg (F  [e])                             = selectExprBrac fSpec i src trg e
   selectExprBrac fSpec i src trg (Fi [e])                             = selectExprBrac fSpec i src trg e
   selectExprBrac fSpec i src trg (Fu [e])                             = selectExprBrac fSpec i src trg e
   selectExprBrac fSpec i src trg (Tm m@(Mph{}))
    | (sqlMorSrc fSpec m,sqlMorTrg fSpec m)==(src,trg) = sqlMorName fSpec m
   selectExprBrac fSpec i src trg (K0 e)
    | (sqlExprSrc fSpec e,sqlExprTrg fSpec e)==(src,trg)                         = sqlRelName fSpec (K0 e)
   selectExprBrac fSpec i src trg (K1 e)
    | (sqlExprSrc fSpec e,sqlExprTrg fSpec e)==(src,trg)                 = sqlRelName fSpec (K1 e)
   selectExprBrac fSpec i src trg expr                                 = phpIndent (i+4) ++ "( " ++ selectExpr fSpec (i+6) src trg expr++ phpIndent(i+4)++")"

   noCollide :: [String] -> String -> String
   noCollide names name | name `elem` names = noCollide names (namepart (reverse name) ++ changeNr (numberpart (reverse name)))
                        | otherwise = name
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
      int2string n = if n `div` 10 == 0 then [intToDigit (n `rem` 10)|n>0] else int2string (n `div` 10)++[intToDigit (n `rem` 10)]

   noCollideUnlessTm (Tm _) _ name = name
   noCollideUnlessTm _  names name = noCollide names name

   selectExprMorph fSpec i src trg mph@(V _ _)
    = selectGeneric i ("vfst."++sqlAttConcept fSpec (source mph),src) ("vsnd."++sqlAttConcept fSpec (target mph),trg)
                      (sqlConcept fSpec (source mph) ++ " AS vfst, "++sqlConcept fSpec (target mph) ++ " AS vsnd")
                      "1"
   selectExprMorph fSpec i src trg mph@(Mp1 str _)
    | src == trg = "SELECT "++str++" AS "++src
    | otherwise  = "SELECT "++str++" AS "++src++", "++str++" AS "++trg
   selectExprMorph fSpec i src trg mph -- made for both Mph and I
    | isIdent mph = selectGeneric i (sqlAttConcept fSpec (source mph),src) (sqlAttConcept fSpec (target mph),trg) (sqlConcept fSpec (source mph)) "1"
    | otherwise   = selectGeneric i (sqlMorSrc fSpec mph,src) (sqlMorTrg fSpec mph,trg) (sqlMorName fSpec mph) "1"
 --   | otherwise   = selectGeneric i (sqlMorSrc fSpec mph,trg) (sqlMorTrg fSpec mph,src) (sqlMorName fSpec mph) "1"

   selectExists' i tbl whr
    = "SELECT *" ++
      phpIndent i ++ "  FROM " ++ tbl ++
      phpIndent i ++ " WHERE " ++ whr
   selectGeneric :: Int -> (String,String) -> (String,String) -> String -> String -> String
   selectGeneric i src trg tbl whr
    = selectcl ++
      phpIndent i ++ "  FROM "++tbl++
      phpIndent i ++ " WHERE "++whr
      where selectcl | src==trg  = "SELECT DISTINCT " ++ selectSelItem src
                     | otherwise = "SELECT DISTINCT " ++ selectSelItem src ++", "++selectSelItem trg
   selectSelItem (att,alias) | selectSameName (att,alias) = att
                             | otherwise                  = att++" AS "++alias
   selectSameName (att,alias) = afterPoint att == alias
    where afterPoint s = if (myafterPoint s == "") then s else myafterPoint s
          myafterPoint ('.':xs) = xs
          myafterPoint ( x :xs) = myafterPoint xs
          myafterPoint []       = []

   selectNormFiExpr :: String -> Fspc -> Int -> Expression -> (String,String) -> [(Morphism,String,String)] -> Expression -> String
   selectNormFiExpr var fSpec i expr (src,trg) subs e
    = selectExpr fSpec i src trg expr'
      where expr' = doSubsExpr fSpec var subs (conjNF e)




   class IsClos e where
    isClos :: e -> Bool
    sqlClosName :: Fspc -> e -> String

   instance IsClos Expression where
    isClos (K0 e) = True
    isClos (K1 e) = True
    isClos (F [e]) = isClos e
    isClos (Fu [e]) = isClos e
    isClos (Fi [e]) = isClos e
    isClos (Tm m) = isClos m
    isClos _      = False
    sqlClosName fSpec (K0 e)
     = head (["Clos"++show i++"_"++enc False (name s)| (c,i)<-zip (closE fSpec) [0..], K0 e == c, s<-take 1 (declarations e)]++
             [error ("(module RelBinGenBasics) Illegal closure expression in \"sqlClosName fSpec ("++showHS "" (K0 e)++")\"\n with closE fSpec = "++showHS "" (closE fSpec))]
            )
    sqlClosName fSpec (K1 e)
     = head (["Clos"++show i++"_"++enc False (name s)| (c,i)<-zip (closE fSpec) [0..], K1 e == c, s<-take 1 (declarations e)]++
             [error ("(module RelBinGenBasics) Illegal closure expression in \"sqlClosName fSpec ("++showHS "" (K1 e)++")\"")]
            )
    sqlClosName fSpec (F [e])  = sqlClosName fSpec e
    sqlClosName fSpec (Fu [e]) = sqlClosName fSpec e
    sqlClosName fSpec (Fi [e]) = sqlClosName fSpec e

   instance IsClos Morphism where
    isClos m@(Mph{}) = isClos (mphdcl m)
    isClos _ = False
    sqlClosName fSpec mph@(Mph{}) = sqlClosName fSpec (K0 (Tm (if mphyin mph then mph else flp mph)))
    sqlClosName _ _ = error ("Module RelBinGenBasics: incomplete pattern in sqlClosName.")
   instance IsClos Declaration where
    isClos s = take 4 (name s) == "Clos"
    sqlClosName fSpec s | isClos s  = name s
                          | otherwise = error ("Module RelBinGenBasics: incomplete condition in sqlClosName.")


   phpIndent i = "\n"++[' '|n<-[1..i]]

   closE = map conjNF . closExprs
   clos0 (K0 e) = True
   clos0 _ = False



   phpIdentifier :: String -> String
   phpIdentifier str = [if c==' ' then '_' else c| c<-str, isAlphaNum c || c==' ' || c=='_']



   phpShow :: String -> String
   phpShow str = "'" ++ addSlashes str ++ "'"

   addSlashes :: String -> String
   addSlashes ('\'': cs) = "\\'"++addSlashes cs
   addSlashes ('"': cs) = "\\\""++addSlashes cs
   addSlashes ('\\': cs) = "\\\\"++addSlashes cs
   addSlashes (c:cs) = c:addSlashes cs
   addSlashes "" = ""

   phpConcept :: Fspc -> Concept -> String
   phpConcept fSpec c | c == cptS = "ONE"
                        | otherwise 
    = if null cs then error ("(module RelBinGenBasics) Concept \""++show c++"\" does not occur in fSpec \""++appname++"\" (phpConcept in module RelBinGenBasics)") else
      if length cs>1 then error ("(module RelBinGenBasics) Concept \""++show c++"\" is not unique in fSpec \""++appname++"\" (phpConcept in module RelBinGenBasics)") else
      head cs
      where cs = ["C"++show i++"_"++phpEncode (name c')|(c',i)<-zip (concs fSpec) [1..], c==c']
            FS_id appname = fsfsid fSpec

   sqlRuleName :: Fspc -> Rule -> String
   sqlRuleName fSpec (Sg p rule expla sgn nr pn signal) = sqlRelName fSpec signal
   sqlRuleName fSpec r = error ("Illegal call to sqlRuleName ("++showADL r++" on "++show (pos r)++")")


   -- WAAROM?? Onderstaande declaratie mag wel wat verduidelijking.
   -- Het lijkt me ook niet onderhoudbaar.
   -- Overigens is deze module de ENIGE die het noodzakelijk maakt om de Class ShowHS te exporteren in ShowHS. Jammer!
   sqlRelName :: (Show m,Morphic m,MorphicId m,Morphical m,IsClos m) => Fspc -> m -> String
   sqlRelName fSpec m
    = if isIdent m then sqlConcept fSpec (source m) else
      if isClos m then sqlClosName fSpec m else
      if isTrue m then "V" else
      if null as then error ("(module RelBinGenBasics) Fatal error in RelBinGen.lhs (sqlRelName): No declarations in "++show m) else
      if length as>1 then error ("(module RelBinGenBasics) Fatal error in RelBinGen.lhs (sqlRelName): Multiple declarations in "++show m) else
      name plug
      where (plug,_,_) = sqlRelPlug fSpec (Tm (makeMph a))
            as = declarations m
            a = head as
            FS_id appname = fsfsid fSpec
    
    --[PlugSql {fields = [Fld {fldname = "Application", fldexpr = I, fldtype = SQLId, fldnull = False, flduniq = True},Fld {fldname = "Decision", fldexpr = leadsto, fldtype = SQLId, fldnull = True, flduniq = True},Fld {fldname = "Employee", fldexpr = assigned, fldtype = SQLId, fldnull = True, flduniq = False},Fld {fldname = "Person", fldexpr = applicant, fldtype = SQLId, fldnull = False, flduniq = False},Fld {fldname = "Product", fldexpr = kind, fldtype = SQLId, fldnull = False, flduniq = False},Fld {fldname = "Product1", fldexpr = kind, fldtype = SQLId, fldnull = False, flduniq = False}], database = CurrentDb, plname = "Application"},PlugSql {fields = [Fld {fldname = "Application", fldexpr = I, fldtype = SQLId, fldnull = False, flduniq = True},Fld {fldname = "Decision", fldexpr = leadsto, fldtype = SQLId, fldnull = True, flduniq = True},Fld {fldname = "Decision1", fldexpr = leadsto;leadsto, fldtype = SQLId, fldnull = True, flduniq = True},Fld {fldname = "Employee", fldexpr = leadsto;assigned, fldtype = SQLId, fldnull = True, flduniq = False},Fld {fldname = "Person", fldexpr = leadsto;applicant, fldtype = SQLId, fldnull = True, flduniq = False},Fld {fldname = "Product", fldexpr = leadsto;kind, fldtype = SQLId, fldnull = True, flduniq = False},Fld {fldname = "Product1", fldexpr = leadsto;kind, fldtype = SQLId, fldnull = True, flduniq = False}], database = CurrentDb, plname = "ApplicationApplication"},PlugSql {fields = [Fld {fldname = "IDdocument", fldexpr = I, fldtype = SQLId, fldnull = False, flduniq = True},Fld {fldname = "Person", fldexpr = authentic~, fldtype = SQLId, fldnull = False, flduniq = False}], database = CurrentDb, plname = "authentic"},PlugSql {fields = [Fld {fldname = "Application", fldexpr = I/\checked;checked~, fldtype = SQLId, fldnull = False, flduniq = False},Fld {fldname = "IDdocument", fldexpr = checked, fldtype = SQLId, fldnull = False, flduniq = False}], database = CurrentDb, plname = "checked"},PlugSql {fields = [Fld {fldname = "Employee", fldexpr = I/\auth;auth~, fldtype = SQLId, fldnull = False, flduniq = False},Fld {fldname = "Product", fldexpr = auth, fldtype = SQLId, fldnull = False, flduniq = False}], database = CurrentDb, plname = "auth"},PlugSql {fields = [Fld {fldname = "Person", fldexpr = I, fldtype = SQLId, fldnull = False, flduniq = True},Fld {fldname = "Area", fldexpr = inhabitant, fldtype = SQLId, fldnull = False, flduniq = False}], database = CurrentDb, plname = "inhabitant"},PlugSql {fields = [Fld {fldname = "Area", fldexpr = I, fldtype = SQLId, fldnull = False, flduniq = False},Fld {fldname = "Employee", fldexpr = area~, fldtype = SQLId, fldnull = False, flduniq = False}], database = CurrentDb, plname = "area"},PlugSql {fields = [Fld {fldname = "Employee", fldexpr = I, fldtype = SQLId, fldnull = False, flduniq = True}], database = CurrentDb, plname = "Employee"},PlugSql {fields = [Fld {fldname = "Product", fldexpr = I, fldtype = SQLId, fldnull = False, flduniq = True}], database = CurrentDb, plname = "Product"}]

    
   sqlRelPlug :: Fspc -> Expression -> (Plug,SqlField,SqlField) --(plug,source,target)
   sqlRelPlug fSpec expr = if null cs then error ("\n(module RelBinGenBasics) Expression \""++show expr++"\" does not occur in plugs of fSpec (sqlRelPlug in module RelBinGenBasics)") else
                           head cs
                           where cs = [ (plug,sf,tf)
                                      | plug@PlugSql{}<-plugs fSpec
                                      , sf<-[f|f<-fields plug,target (fldexpr f)==source e]
                                      , tf<-[f|f<-fields plug,target (fldexpr f)==target e]
                                      ,  ((Sur `elem` multiplicities (fldexpr sf))
                                          &&(conjNF (F [fldexpr  sf,    e])==fldexpr tf)
                                         )
                                      || ((Sur `elem` multiplicities (fldexpr tf))
                                          &&(conjNF (F [fldexpr  tf,flp e])==fldexpr sf)
                                         )
                                      || (source (fldexpr sf)==source e && fldexpr tf == e)
                                      || (source (fldexpr tf)==target e && fldexpr sf == flp e)
                                      ]
                                 e  = expr
   
   sqlRelSrc :: Fspc->Declaration -> String
   sqlRelSrc fSpec i@Vs{}      = name (target i)
   sqlRelSrc fSpec i@Isn{}     = name (target i)
   sqlRelSrc fSpec i@Iscompl{} = name (target i)
   sqlRelSrc fSpec s = fldname src
    where (_,src,_) = sqlRelPlug fSpec (Tm $makeMph s)

   sqlRelTrg :: Fspc->Declaration -> String
   sqlRelTrg fSpec i@Vs{}      = name (target i)
   sqlRelTrg fSpec i@Isn{}     = name (target i)
   sqlRelTrg fSpec i@Iscompl{} = name (target i)
   sqlRelTrg fSpec s = fldname trg
    where (_,_,trg) = sqlRelPlug fSpec (Tm $makeMph s)

   sqlExprSrc :: Fspc->Expression -> String
   sqlExprSrc fSpec (F [])   = error ("(Module RelBinGenBasics: ) calling sqlExprSrc (F [])")
   sqlExprSrc fSpec (F [f])  = sqlExprSrc fSpec f
   sqlExprSrc fSpec (F fs)   = sqlExprSrc fSpec (head fs)
   sqlExprSrc fSpec (Fu [])  = error ("(Module RelBinGenBasics: ) calling sqlExprSrc (Fu [])")
   sqlExprSrc fSpec (Fu [f]) = sqlExprSrc fSpec f
   sqlExprSrc fSpec (Fu fs)  = sqlExprSrc fSpec (head fs) --all subexprs have the same type --was: (head (filter l fs)) where l = (==foldr1 lub (map source fs)).source
   sqlExprSrc fSpec (Fi [])  = error ("(Module RelBinGenBasics: ) calling sqlExprSrc (Fi [])")
   sqlExprSrc fSpec (Fi [f]) = sqlExprSrc fSpec f
   sqlExprSrc fSpec (Fi fs)  = sqlExprSrc fSpec (head fs) --all subexprs have the same type --was:(head (filter l fs)) where l = (==foldr1 lub (map source fs)).source
   sqlExprSrc fSpec (Fd [])  = error ("(Module RelBinGenBasics: ) calling sqlExprSrc (Fd [])")
   sqlExprSrc fSpec (Fd [f]) = sqlExprSrc fSpec f
   sqlExprSrc fSpec (Fd fs)  = sqlExprSrc fSpec (head fs)
   sqlExprSrc fSpec (Cp e)   = sqlExprSrc fSpec e
   sqlExprSrc fSpec (K0 e)   = sqlExprSrc fSpec e
   sqlExprSrc fSpec (K1 e)   = sqlExprSrc fSpec e
   sqlExprSrc fSpec (Tm (Mp1 _ t)) = "Att"++phpEncode (name t)
   sqlExprSrc fSpec (Tm m) = sqlMorSrc fSpec m
   {-
   sqlExprSrc fSpec e@(Tm m) | inline m  = sqlRelSrc fSpec (makeDeclaration m)
                             | otherwise = sqlRelTrg fSpec (makeDeclaration m)
   -}
   
   sqlExprTrg :: Fspc->Expression -> String
   sqlExprTrg fSpec e = sqlExprSrc fSpec (flp e)

   sqlMorName :: Fspc -> Morphism -> String
   sqlMorName fSpec (Mph nm pos atts sgn yin s) = sqlRelName fSpec s
   sqlMorName fSpec (I atts g s yin)            = sqlConcept fSpec s
   sqlMorName fSpec m = error ("(module RelBinGenBasics) sqlMorName: illegal argument: "++showHS "" m)
   
   -- these functions are exact compies of sqlRelSrc and sqlRelTrg!
   sqlMorSrc :: Fspc -> Morphism -> String
   sqlMorSrc fSpec i@V{}   = name (source i)
   sqlMorSrc fSpec i@I{}   = name (source i)
   sqlMorSrc fSpec i@Mph{} = name (source i)
   sqlMorSrc fSpec i@Mp1{} = name (source i)
   sqlMorSrc fSpec s = fldname src
    where (_,src,_) = sqlRelPlug fSpec (Tm s)

   sqlMorTrg :: Fspc -> Morphism -> String
   sqlMorTrg fSpec i@V{}   = name (target i)
   sqlMorTrg fSpec i@I{}   = name (target i)
   sqlMorTrg fSpec i@Mph{} = name (target i)
   sqlMorTrg fSpec i@Mp1{} = name (target i)
   sqlMorTrg fSpec s = fldname trg
    where (_,_,trg) = sqlRelPlug fSpec (Tm s)


   sqlConcept :: Fspc -> Concept -> String
   sqlConcept fSpec c | c==cptS = "ONE" -- todo: throw error
                      | otherwise
                = if null cs then error ("(module RelBinGenBasics) Concept \""++show c++"\" does not occur in fSpec (sqlConcept in module RelBinGenBasics)") else
                  head cs
                  where cs = [name plug|let plug=sqlConceptPlug fSpec c, c'<-concs plug, c'==c]
   
   sqlConceptPlug :: Fspc -> Concept -> Plug
   sqlConceptPlug fSpec c = if null cs then error ("(module RelBinGenBasics) Concept \""++show c++"\" does not occur in fSpec (sqlConceptPlug in module RelBinGenBasics)") else
                            head cs
                            where cs = [plug | plug@PlugSql{}<-plugs fSpec, c'<-concs plug, c'==c]

   sqlAttConcept :: Fspc -> Concept -> String
   sqlAttConcept fSpec c | c==cptS = "ONE"
                           | otherwise
                = if null cs then error ("(module RelBinGenBasics) Concept \""++show c++"\" does not occur in its plug in fSpec \""++appname++"\" (sqlAttConcept in module RelBinGenBasics)") else
                  head cs
                  where cs = [fldname f|f<-fields (sqlConceptPlug fSpec c), c'<-concs f,c==c']
                        FS_id appname =  fsfsid fSpec

   ifAs str str' = if str == str' then str else str'++" AS "++str

   phpCount var fSpec m = "count("++var++"['"++sqlMorName fSpec m++"'])"
   phpNtCnt var fSpec m = "!count("++var++"['"++sqlMorName fSpec m++"'])"

   phpIsset var fSpec m = "isset("++var++"['"++sqlMorName fSpec m++"'])"
   phpIsnet var fSpec m = "!isset("++var++"['"++sqlMorName fSpec m++"'])"

   isSub fSpec subs a               = a `elem` [sqlMorName fSpec r| (r,x,y)<-subs]
   doSubSrc fSpec subs var x a t    = if isSub fSpec subs a then "\\''.addSlashes("++var++"['"++doSrc fSpec subs x a++"']).'\\'"         else a++"."++t
   doSubTrg fSpec subs var x a t    = if isSub fSpec subs a then "\\''.addSlashes("++var++"['"++doTrg fSpec subs x a++"']).'\\'"         else a++"."++t
   dASubSrc fSpec subs var x a as t = if isSub fSpec subs a then "\\''.addSlashes("++var++"['"++doSrc fSpec subs x a++"']).'\\' AS "++as else a++"."++ifAs as t
   dASubTrg fSpec subs var x a as t = if isSub fSpec subs a then "\\''.addSlashes("++var++"['"++doTrg fSpec subs x a++"']).'\\' AS "++as else a++"."++ifAs as t
   doSrc fSpec subs x a             = head [if inline x then p else q| (r,p,q)<-subs, a==sqlMorName fSpec r]
   doTrg fSpec subs x a             = head [if inline x then q else p| (r,p,q)<-subs, a==sqlMorName fSpec r]

   phpEncode = enc True

   phpCodeTransactionStart fSpec noTransactions
    = if noTransactions then "" else
      chain "\n   "
      [ "DB_doquer('START TRANSACTION');"
      , "//DB_doquer('LOCK TABLES "
        ++ chain ", " ([sqlRelName fSpec s ++ " WRITE" | s<-declarations fSpec ]++
                       [sqlConcept fSpec c ++ " WRITE" | c<-concs fSpec ]        ) ++
                    "');"
      , "// transaction started"
      ]

   phpCodeTransactionClose fSpec noTransactions okVar
    = if noTransactions then "     return("++okVar++");" else
      if(null (vrules fSpec))
         then chain "\n"
                [ "DB_doquer('COMMIT');"
                , "   // DB_doquer('UNLOCK TABLES');"
                , "  return("++okVar++");"
                ]
         else chain "\n   "
                [ "if(" ++ (chain " && " [ "checkRule"++show (runum r)++"()" | r<-vrules fSpec ])  ++ "){"
                , "     DB_doquer('COMMIT');"
                , "  // DB_doquer('UNLOCK TABLES');"
                , "     return("++okVar++");"
                , "}else{"
                , "     DB_doquer('ROLLBACK');"
                , "  // DB_doquer('UNLOCK TABLES');"
                , "     return(false);"
                , "}" ]

   insConcept fSpec c selStmt
    = chain "\n        "
       [ "DB_doquer('INSERT IGNORE INTO "++sqlConcept fSpec c'++" ("++sqlAttConcept fSpec c'++") "++selStmt++"');"
       | c'<-concs fSpec, c' <= c]
   insConcepts fSpec hcs n c str excludeRels
    = take n (repeat ' ')++
      chain ("\n"++take n (repeat ' '))
       [ "DB_doquer('INSERT IGNORE INTO "++sqlConcept fSpec c'++" ("++sqlAttConcept fSpec c'++") VALUES (\\''.addSlashes($attrs['"++str++"']).'\\')');"
       | c'<-concs fSpec, c' <= c]++
      concat
       [ "\n"++take (n-3) (repeat ' ')++"// "++informalRule {-[sIs c]-} (CR fOp (Tm (mIs c)) bOp toExpr frExpr rule)++"\n"++[' '|i<-[1..n]]++
         "if(isset($attrs['"++str++"']))" ++
         "DB_doquer('INSERT IGNORE INTO "++sqlMorName fSpec s++" VALUES (\\''.addSlashes($attrs['"++str++"']).'\\', \\''.addSlashes($attrs['"++str++"']).'\\')');"
       | (CR fOp e bOp toExpr frExpr rule)<-computeOrder hcs "INSERT INTO" [Isn c c], isIdent toExpr
       , sign frExpr <= sign c
       , oneMorphism toExpr
       , s<-mors toExpr, not (makeDeclaration s `elem` excludeRels)]

   dbDelConcept fSpec i c atomVar
    = take i (repeat ' ')++"DB_doquer('DELETE FROM "++sqlConcept fSpec c++" WHERE "++sqlAttConcept fSpec c++"=\\''.addSlashes("++atomVar++").'\\' AND NOT EXISTS ("++chain " UNION " (["SELECT "++sqlRelSrc fSpec s++" FROM "++sqlRelName fSpec s++" WHERE "++sqlRelSrc fSpec s++"=\\''.addSlashes("++atomVar++").'\\'" | s<-declarations fSpec, source s==c]++["SELECT "++sqlRelTrg fSpec s++" FROM "++sqlRelName fSpec s++" WHERE "++sqlRelTrg fSpec s++"=\\''.addSlashes("++atomVar++").'\\'" | s<-declarations fSpec, target s==c])++")');"


{-
Code below is meant for inside the Fspec structure.
In particular the function plugs 
-}
   plugs :: Fspc -> [Plug]
   plugs spc = neededPlugs given spc ++ given
     where given = []
   
   neededPlugs :: [Plug] -> Fspc -> [Plug] -- given [Plug], what plugs are still needed to implement Fspc?
   neededPlugs given spec
    = theplugs
      where
       otherRels      = looseRels >- mors (given ++ complexPlugs)
       looseRels      = map makeMph (vrels spec) >- mors given
       looseConcs     = concs (vrels spec) -- todo: we can make this less, since V[conc] isn't allways asked for..
                        >- concs (given ++ relPlugs ++ complexPlugs)
       -- complexPlugs are the plugs that consist of multiple relations
       uniSurRels :: Morphisms
       uniSurRels     = [r | r <- rd (looseRels++map flp looseRels) , null ([Uni,Sur,Inj] >- multiplicities r)]
       complexPlugs   = [] {-
       complexPlugs   = error$show [ show rclos
                        | rclos <- [ foldl1 (\x y -> if length x <= length y then y else x) cl -- longest list in cl
                                   | cl <- eqClass (\x y -> length x < length (x >- y)) -- there is overlap between x and y
                                                   (clos source target uniSurRels)
                                   ]
                        ] -}
       mkdataset rclos= error "mkdataset function not done yet"
                        
       relPlugs       = map morplug otherRels
       theplugs       = (uniqueNames . joinPlugs) (complexPlugs ++ relPlugs ++ map concplug looseConcs)
       
   
   -- todo, this algorithm is supposidly O(n^3), but this has never been measured
   -- the orders O(..) are mere estimations
   -- if the this algorithm is slower than n^3, it should be made faster
   joinPlugs :: [Plug] -> [Plug]
   joinPlugs ps = doJoin ps (joins inCols outCols) -- O(n^3)
     where
     outCols  = sort [(c,(p,f))|p<-ps,f<-fields p,flduniq f,Sur `elem` (multiplicities (fldexpr f)),let c=target(fldexpr f)]
     inCols   = sort [(c,(p,f))|p<-ps,f<-fields p,not (fldnull f),flduniq f,let c=source (fldexpr f),c==target (fldexpr f)]
     joins [] _ = []
     joins _ [] = []
     joins (i@(ci,pi):ins) (o@(co,po):outs) = -- joins is O(n)
       if ci<co then joins ins (o:outs) else if ci>co then joins (i:ins) outs
       else if fst pi == fst po then joins ins (o:outs) else (pi,po):joins ins (o:outs)
   
   doJoin :: [Plug] -> [((Plug, SqlField), (Plug, SqlField))] -> [Plug]
   doJoin ps [] = ps -- O(n^3)
   doJoin ps (j:js) = pair : doJoin rest js'
     where
     myIns  p = [fst i|(i,o)<-(j:js),fst o==p] -- O(n)
     ins      = myIns (fst addTo) -- O(n)
     addTo    = snd j --O(1)
     pair     = (fst addTo){fields = (fields (fst addTo)) ++ [joinFields f|o<-ins,f<-tail (fields o)] --O(n^2)
                           ,plname = (name . source) expr }
     expr     = fldexpr (snd addTo) --O(1)
     joinFields fld = fld{fldexpr = conjNF (F [expr,fldexpr fld]) -- O(1) ?
                         ,fldnull = fldnull (snd addTo) || fldnull fld}
     js'      = [(i',o')|(i,o)<-js,o /= addTo
                        ,let i'=if (fst i) `elem` ins then (pair,joinFields (snd i)) else i
                        ,let o'=if (fst o) `elem` ins then (pair,joinFields (snd o)) else o] -- O(n^2)
     rest     = [p|p<-ps, p `notElem` (fst addTo:ins)] -- O(n^2)
     --newFields = addFields (fields p) ps
   
   {-
   addFields fs (p:ps) = (first newFields,unused ++ second newFields ns)
     where
     newFields    = addFields fs' ns
     concexprs    = [e|e<-(map fldexpr (fs)),Sur `elem` (multiplicities e)]
     matchCols    = [c|
   -}
   
   uniqueNames :: [Plug]->[Plug]
   {-
   uniqueNames plgs = uN [] (map uniqueCols plgs) -- todo: de velden in elke plug moeten ook unieke namen hebben!
    where uN :: [String] -> [Plug] -> [Plug]
          uN _ [] = []
          uN lst (p:ps) | (name p) `notElem` lst = p:(uN (name p:lst) ps)
          uN lst (p:ps) | (n1 p)   `notElem` lst = p{plname=n1 p}:(uN (n1 p:lst) ps)
          uN lst (p:ps) | (n2 p)   `notElem` lst = p{plname=n2 p}:(uN (n2 p:lst) ps)
          uN lst ps     | otherwise              = (uNn lst ps 1)
          uNn :: [String] -> [Plug] -> Integer -> [Plug]
          uNn lst (p:ps) n | name p ++ show n `notElem` lst = p{plname=name p ++ show n}:(uN (name p:lst) ps)
          uNn lst ps n | otherwise = uNn lst ps (n+1)
          n1 p = name p ++ plsource p
          n2 p = name p ++ pltarget p
          plsource p = name (source (fldexpr (head (fields (p)))))
          pltarget p = name (target (fldexpr (last (fields (p)))))
          uniqueCols p = p{fields=uniqueFields [] $ fields p}
          uniqueFields _ [] = []
          --uniqueFields lst (f:fs) | name f `notElem` lst = 
   -}
   uniqueNames plgs = naming (\x y->x{plname=y}) -- renaming function for plugs
                             (name:n1:n2:[(\x->name x ++ show n)|n<-[1..]]) -- ways to name a plug
                             []                  -- no plug-names taken yet
                             (map uniqueFields plgs)
     where n1 p = name p ++ plsource p
           n2 p = name p ++ pltarget p
           plsource p = name (source (fldexpr (head (fields (p)))))
           pltarget p = name (target (fldexpr (last (fields (p)))))
           uniqueFields plug = plug{fields = naming (\x y->x{fldname=y}) -- renaming function for fields
                                             (fldname:[(\x->fldname x ++ show n)|n<-[1..]])
                                             []
                                             (fields plug)
                                   }
   
   {- naming - a naming function
      The objective is to name all items in a list uniquely
      
      The call below will label allItems as 1,2,3 etc, skipping 4:
      naming nameIt [(\x->show n)|n<-[1..]] ["4"] allItems
      
      Naming one item is done by: nameIt unnamedItem someName -> namedItem
      There should be a list of functions to name an item,
          the resulting names should form an infinite set.
   -}
   naming :: Eq a => (b->a->c) -- function used to asign name a to element b
                  -> [b->a]    -- infinite list of functions to create a name for b
                  -> [a]       -- list of forbidden names (names already taken)
                  -> [b]       -- list of elements b that need a name
                  -> [c]       -- result: named alements (matches [b])
   naming _ _ _ [] = []
   naming _ [] _ _ = error "(RelBinGenBasics) no naming functions given"
   naming assignFunc as taken (l:ls)
                   = head [assignFunc l (a l):naming assignFunc as (a l:taken) ls
                          | a<-as, a l `notElem` taken]
   
--- uniqueNames p:ps | ((name p++(name source p)) `elem` (names ps)) = p:(uniqueNames ps)
   
   concplug :: Concept -> Plug
   concplug c = plugsql (name c) [field (name c) (Tm (mIs c)) Nothing False True]
   
   morplug :: Morphism -> Plug
   morplug  m
    = {- If the morphism is UNI, INJ and SUR
       we can identify the target by its source alone
       we shoud, however do this afterwards, transforming ALL plugs
      if ( isUni && isInj && isSur && fldtyp (target m) == SQLId ) || (isIdent m)
      then plugsql (name (source m)) [field (name (source m)) (Tm (mIs (source m))) Nothing False True
                                     ,field (name (target m)) (Tm m) (Just SQLBool) (not isTot) True]
      else -}
      if isInj && not isUni then morplug (flp m)
      else if isUni || isTot
      then plugsql (name m) [field (name (source m)) (Tm (mIs (source m))) Nothing False isUni
                            ,field (name (target m)) (Tm m) Nothing (not isTot) isInj]
      else if isInj || isSur then morplug (flp m)
      else plugsql (name m) [field (name (source m)) (Fi {es=[Tm (mIs (source m)),F {es=[Tm m,flp (Tm m)]}]}
                                                     )      Nothing False False
                            ,field (name (target m)) (Tm m) Nothing False False]
      where
        mults = multiplicities m
        isTot = Tot `elem` mults
        isUni = Uni `elem` mults
        isSur = Sur `elem` mults
        isInj = Inj `elem` mults
   
   plugsql :: String -> [SqlField] -> Plug
   plugsql nm fld = PlugSql {plname=nm,database=CurrentDb,fields=fld}
   field :: String
            -> Expression
            -> Maybe SqlType
            -> Bool
            -> Bool
            -> SqlField
   field nm expr Nothing   nul uniq = Fld {fldname = nm, fldexpr=expr, fldtype=fldtyp (target expr),fldnull=nul,flduniq=uniq}
   field nm expr (Just tp) nul uniq = Fld {fldname = nm, fldexpr=expr, fldtype=tp,fldnull=nul,flduniq=uniq}

   
   fldtyp :: (Identified a) => a -> SqlType
   fldtyp nm = case name nm of { "BLOB"   -> SQLBlob;
                                 "PASS"   -> SQLPass;
                                 "STRING" -> SQLVarchar 255;
                                 _        -> SQLId
                               }

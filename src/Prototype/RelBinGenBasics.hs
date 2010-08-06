{-  TODO: Warningvrij maken # OPTIONS_GHC -Wall #-}
module Prototype.RelBinGenBasics(phpIdentifier,naming,sqlRelPlugs,commentBlock,strReplace
 ,selectExpr,selectExprBrac,addSlashes,sqlExprTrg,sqlExprSrc,sqlAttConcept
 ,sqlPlugFields,indentBlock,phpShow,isOne,addToLast
 ,pDebug,noCollide,indentBlockBetween
 ) where
   import Char(isDigit,digitToInt,intToDigit,isAlphaNum,toLower)
   import Strings (commaEng,chain)
   import Adl
   import ShowADL
   import ShowHS
   import NormalForms (conjNF,disjNF,simplify,nfProof)
   import Data.Fspec
   import Data.Plug
   import List(isPrefixOf)
   import Collection (Collection(rd,uni))
   import Auxiliaries (naming)
   import Calc

--   import Debug.Trace

   pDebug :: Bool
   pDebug = True
   
   quote :: String->String
   quote [] = []
   quote ('`':s) = ('`':s)
   quote s = "`"++s++"`"
   
   commentBlock :: [String]->[String]
   commentBlock ls = ["/*"++take lnth (repeat '*')++"*\\"]
                        ++ ["* "++(strReplace "*/" "**" line)++take (lnth - length line) (repeat ' ')++" *" | line <- ls]
                        ++ ["\\*"++take lnth (repeat '*')++"*/"]
      where
        lnth = foldl max 0 (map length ls)
   indentBlock :: Int -> [String] -> [String]
   indentBlock i = map ((++) (take i (repeat ' ')))
   
   -- | will put the block after the first string, and put the second after the block
   -- | If the block is just 1 line, indentBlockBetween will return just 1 line as well
   indentBlockBetween :: String -- ^ precedes the block
                      -> String -- ^ comes at the end of the block
                      -> [String] -- ^ the block itself, (will be indented)
                      -> String -- ^ result
   indentBlockBetween pre post [] = pre++post
   indentBlockBetween pre post [s] = pre++s++post
   indentBlockBetween pre post block
    = chain (phpIndent (length pre)) ((pre++head block):(init rest++[last rest++post]))
    where rest = tail block
   -- isOne: het is niet voldoende om alleen te controleren of: source (ctx o) == ONE
   -- De service op V[ONE*SomeConcept] moet immers nog voor ieder SomeConcept iets aanbieden
   -- de vraag die we hier stellen is: komen we steeds op eenzelfde concept uit
   -- als dit zo is, hoeven we alleen dat ene concept te tonen
   isOneExpr :: Expression -> Bool
   isOneExpr e' = (isUni.conjNF.F) [v (source (e'),source (e')),e']
   isOne :: ObjectDef -> Bool
   isOne o = isOneExpr$ctx o

   
   strReplace :: String -> String -> String -> String
   strReplace _ _ "" = ""
   strReplace "" _ str = str
   strReplace src dst inp
       = process inp
         where
           n = length src
           process "" = ""
           process st@(c:cs)
             | src `isPrefixOf` st = dst ++ process (drop n st)
             | otherwise           = c:process cs

   selectExpr ::    Fspc    -- current context
                 -> Int        -- indentation
                 -> String     -- SQL name of the source of this expression, as assigned by the environment 
                 -> String     -- SQL name of the target of this expression, as assigned by the environment
                 -> Expression -- expression to be translated
                 -> String     -- resulting SQL expression
   -- quote the attributes (such that column-names such as `Right` or `in` won't yield errors)
   selectExpr fSpec i src@(_:_) trg       e' | head src /= '`'
    = selectExpr fSpec i ('`':src++"`") trg            e'
   selectExpr fSpec i src       trg@(_:_) e' | head trg /= '`'
    = selectExpr fSpec i src            ('`':trg++"`") e'
   
   --TODO
   selectExpr fSpec i src trg iex@(Fix lst'@(_:_:_))
    | length lst'==length simplectxbinding && source iex==target iex && length(snd$vctxenv fSpec)==length simplectxbinding
        = let phpvar1 = (snd.head)(snd$vctxenv fSpec) 
          in concat$ 
               ["SELECT DISTINCT TODO.`I`, TODO.`I` AS i1 FROM `"++name(source iex)++"` AS TODO "
               , "WHERE TODO.`"++phpvar1++"`='\".$GLOBALS['ctxenv']['"++phpvar1++"'].\"'"]
               ++["AND TODO.`"++x++"`='\".$GLOBALS['ctxenv']['"++x++"'].\"'"|(_,x)<-tail (snd$vctxenv fSpec)]
    | otherwise = selectGeneric i ("isect0."++src',src) ("isect0."++trg',trg)
                           (chain ", " exprbracs) (chain " AND " wherecl)
{- The story:
 This alternative of selectExpr compiles a conjunction of at least two subexpressions (code: Fi lst'@(_:_:_))
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
      where simplectxbinding = [name$source s|(F [Tm m1 _,Tm s@(Mph{mphnm="s"}) _,Tm m2 _])<-lst',source s==target s,m1==flp m2]
            src'    = quote$sqlExprSrc fSpec fstm
            trgC    = quote$sqlExprTrg fSpec fstm -- can collide with src', for example in case fst==r~;r, or if fst is a property (or identity)
            trg'    = noCollideUnlessTm' fstm [src'] trgC
            fstm    = head posTms  -- always defined, because length posTms>0 (ensured in definition of posTms)
            mp1Tm   = take 1 ([t| t@(Tm (Mp1{})_)<-lst']++[t| t@(F ((Tm (Mp1{})_):(Tm (V _ _)_):(Tm (Mp1{})_):[])) <- lst'])
            lst     = [t|t<-lst', not (elem t mp1Tm)]
            posTms  = if null posTms' then map notCp (take 1 negTms') else posTms' -- we take a term out of negTms' if we have to, to ensure length posTms>0
            negTms  = if null posTms' then tail negTms' else negTms' -- if the first term is in posTms', don't calculate it here
            posTms' = [t| t<-lst, isPos t && not (isIdent t)]++[t| t<-lst, isPos t && isIdent t] -- the code to calculate I is better if it is not the first term
            negTms' = [notCp t| t<-lst, isNeg t && isIdent t]++[notCp t| t<-lst, isNeg t && not (isIdent t)] -- should a negTerm become a posTerm (for reasons described above), it can best be an -I.
            exprbracs = [ (selectExprBrac fSpec i src'' trg'' l) ++ " AS isect"++show n 
                        | (n,l)<-zip [0..] posTms
                        , src''<-[quote$sqlExprSrc fSpec l]
                        , trg''<-[noCollideUnlessTm' l [src''] (quote$sqlExprTrg fSpec l)]
                        ]
            wherecl   = [if isIdent l
                         then  "isect0."++src'++" = isect0."++trg' -- this is the code to calculate ../\I. The code below will work, but is longer
                         else "(isect0."++src'++" = isect"++show n++"."++src''
                         ++ " AND isect0."++trg'++" = isect"++show n++"."++trg''++")"
                        | (n,l)<-tail (zip [0..] posTms) -- not empty because of definition of posTms
                        , src''<-[quote$sqlExprSrc fSpec l]
                        , trg''<-[noCollideUnlessTm' l [src''] (quote$sqlExprTrg fSpec l)]
                        ]++
                        [ "isect0."++src'++" = "++mph1val m -- sorce and target are equal because this is the case with Mp1
                        | (Tm m@(Mp1{}) _) <- mp1Tm
                        ]++
                        [ "isect0."++src'++" = "++mph1val m1 -- sorce and target are unequal
                          ++ " AND isect0."++trg'++" = "++mph1val m2 -- sorce and target are unequal
                        | (F ((Tm m1@(Mp1{}) _):(Tm (V _ _)_):(Tm m2@(Mp1{})_):[])) <- mp1Tm
                        ]++
                        [if isIdent l
                         then  "isect0."++src'++" <> isect0."++trg' -- this code will calculate ../\-I
                         else  "NOT EXISTS ("++(selectExists' (i+12)
                                                              ((selectExprBrac fSpec (i+12) src'' trg'' l) ++ " AS cp")
                                                              ("isect0."++src' ++ "=cp."++src''++" AND isect0."++ trg'++"=cp."++trg'')
                                            )++")"
                        | (_,l)<-zip [0..] negTms
                        , src''<-[quote$sqlExprSrc fSpec l]
                        , trg''<-[noCollideUnlessTm' l [src''] (quote$sqlExprTrg fSpec l)]
                        ]++["isect0."++src'++" IS NOT NULL", "isect0."++trg'++" IS NOT NULL"]
   selectExpr fSpec i src trg (Fix [e']) = selectExpr fSpec i src trg e'
   selectExpr _     _ _   _   (Fix [] ) = error ("!Fatal (module RelBinGenBasics 140): Cannot create query for Fi [] because type is unknown")

   selectExpr fSpec i src trg (F (Tm (V _ (s,_))_:fs@(_:_))) | s==cptS
     = selectGeneric i ("1",src) ("fst."++trg',trg)
                       (selectExprBrac fSpec i src' trg' (F fs) ++ " AS fst")
                       ("fst."++trg'++" IS NOT NULL")
                       where src' = noCollideUnlessTm' (F fs) [trg'] (quote$sqlExprSrc fSpec (F fs))
                             trg' = quote$sqlExprTrg fSpec (F fs)
   selectExpr fSpec i src trg (F (s1@(Tm (Mp1{})_):(s2@(Tm (V _ _)_):(s3@(Tm (Mp1{})_):fx@(_:_))))) -- to make more use of the thing below
     =  selectExpr fSpec i src trg (F ((F (s1:s2:s3:[])):fx))

   selectExpr fSpec i src trg (F ((Tm sr@(Mp1{})_):((Tm (V _ _)_):((Tm tr@(Mp1{})_):[])))) -- this will occur quite often because of doSubsExpr
     = "SELECT "++mph1val sr++" AS "++src++", "++mph1val tr++" AS "++trg

   selectExpr fSpec i src trg (F (e'@(Tm sr@(Mp1{})_):(f:fx)))
      = selectGeneric i ("fst."++src',src) ("fst."++trg',trg)
                        (selectExprBrac fSpec i src' trg' (F (f:fx))++" AS fst")
                        ("fst."++src'++" = "++mph1val sr)
                        where src' = quote$sqlExprSrc fSpec e'
                              trg' = noCollideUnlessTm' (F (f:fx)) [src'] (quote$sqlExprTrg fSpec (F (f:fx)))

   selectExpr fSpec i src trg (F (e':((Tm (V _ _)_):(f:fx)))) = -- prevent calculating V in this case
       if src==trg && not (isProp e')
       then error ("!Fatal (module RelBinGenBasics 163): selectExpr 2 src and trg are equal ("++src++") in "++showADL e')
       else
       selectGeneric i ("fst."++src',src) ("snd."++trg',trg)
                        ((selectExprBrac fSpec i src' mid' e')++" AS fst, "++(selectExprBrac fSpec i mid2' trg' f)++" AS snd")
                        ("fst."++src'++" IS NOT NULL")
            where src' = quote$sqlExprSrc fSpec e'
                  mid' = quote$sqlExprTrg fSpec e'
                  mid2'= quote$sqlExprSrc fSpec f
                  trg' = noCollideUnlessTm' (F (f:fx)) [mid2'] (quote$sqlExprTrg fSpec (F (f:fx)))
   selectExpr fSpec i src trg (F  [e']       ) = selectExpr fSpec i src trg e'
   selectExpr fSpec i src trg (F lst'@(fstm:_:_))
    = selectGeneric i (mainSrc,src) (mainTrg,trg)
                           (chain ", " (concExprs++exprbracs)) (chain ("\n"++[' '|n<-[0..i+1]]++" AND ") wherecl)
{-  De F gedraagt zich als een join. Het is dus zaak om enigszins efficiente code te genereren.
    Dat doen we door de complement-operatoren van de elementen uit lst' te betrekken in de codegeneratie.
    De concepten in lst' noemen we c0, c1, ... cn (met n de lengte van lst')
    De elementen in lst' zelf noemen we F0, F1, ... F(n-1).
    Deze namen worden aangehouden in de SQL-aliasing. Dat voorkomt naamconflicten op een wat ruwe manier, maar wel overzichtelijk en effectief.
-}
      where mainSrc = (if isNeg (head lst') then "c" else "F")++"0."++src'
            mainTrg = (if isNeg (last lst') then "c"++show (length lst') else "F"++show (length lst'-1))++"."++trg'
            -- de strings die de source en target van F lst' weergeven. Bij gelijke namen vindt ontdubbeling van naam plaats met noCollideUnlessTm'
            src'    = quote$sqlExprSrc fSpec fstm
            trg'    = noCollideUnlessTm' fstm [src'] (quote$sqlExprTrg fSpec (last lst'))
            -- ncs geeft alleen de concepten uit lst', die in SQL doorlopen moeten worden, inclusief rangnummer
            ncs     = [ (0,source (head lst'))  | isNeg (head lst') ]++
                      [ (n',c)
                      | ((n,l),(n',l'))<-zip (init (zip [0..] lst')) (tail (zip [0..] lst'))
                      , isNeg l && isNeg l'
                      , c<-[if target l<=source l' then target l else source l']
                      ]++
                      [ (length lst',target (last lst'))  | isNeg (last lst') ]
            -- de SQL-expressies voor de concepten van lst', maar nu in SQL
            concExprs = [e| (_,e,_)<-concExpr]
            concExpr  = [ (n,selectExprBrac fSpec i sm sm tm ++ " AS "++concNm n, sm)
                        | (n,c)<-ncs, tm<-[Tm (mIs c) (-1)], sm<-[quote$sqlExprSrc fSpec tm] ]
            concTp n = head ([t| (i,_,t)<-concExpr, n==i]++error("!Fatal (module RelBinGenBasics 199) concTp"))
            concNm n = head (["c"++show n| (i,_,_)<-concExpr, n==i]++error("!Fatal (module RelBinGenBasics 200) concNm"))
            -- de SQL-expressies voor de elementen uit lst', die elk een ADL-expressie representeren
            exprbracs = [e| (_,e,_,_)<-exprbrac]
            exprbrac  = [ (n,selectExprBrac fSpec i src' trg' l ++ " AS "++exprNm n , src' , trg' )
                        | (n,l)<-zip [0..] lst'
                        , not (isNeg l)
                        , src'<-[quote$sqlExprSrc fSpec l]
                        , trg'<-[noCollideUnlessTm' l [src'] (quote$sqlExprTrg fSpec l)]
                        ]
            exprE n  = head ([e| (i,e,_,_)<-exprbrac, n==i]++error("!Fatal (module RelBinGenBasics 209) exprE"))  -- the expression itself
            exprS n  = head ([s| (i,_,s,_)<-exprbrac, n==i]++error("!Fatal (module RelBinGenBasics 210) exprS"))  -- source type
            exprT n  = head ([t| (i,_,_,t)<-exprbrac, n==i]++error("!Fatal (module RelBinGenBasics 211) exprT"))  -- target type
            exprNm n = head (["F"++show n| (i,_,_,_)<-exprbrac, n==i]++error("!Fatal (module RelBinGenBasics 212) exprNm"))
            -- de where expressies bevatten alle "magie".
            wherecl   = (filter (not.null))
                        [ if isNeg l
                          then "NOT EXISTS ("++selectExists' (i+12)
                                                             (selectExprBrac fSpec i src' trg' (if isNeg l then notCp l else l) ++ " AS F"++show n)
                                                             (chain " AND " ([ concNm  n   ++"."++concTp n   ++ "=F"++show n      ++"."++src'        | inCs n ]++
                                                                             [ exprNm (n-1)++"."++exprT (n-1)++ "=F"++show n      ++"."++src'        | n>0, not (inCs n) ]++
                                                                             [ "F"++show n ++"."++trg'       ++ "=" ++concNm (n+1)++"."++concTp (n+1)| inCs (n+1) ]++
                                                                             [ "F"++show n ++"."++trg'       ++ "=" ++exprNm (n+1)++"."++exprS (n+1) | n>0, not (inCs (n+1)) ]++
                                                                             []))
                                               ++")"
                          else chain " AND " (["c"++show n++"."++src' ++ "=F"++show  n   ++"."++src'| inCs n]++
                                              ["F"++show n++"."++trg' ++ "=c"++show (n+1)++"."++trg'| inCs (n+1)])
                        | (n,l)<-zip [0..] lst'
                        , src'<-[quote$sqlExprSrc fSpec l]
                        , trg'<-[noCollideUnlessTm' l [src'] (quote$sqlExprTrg fSpec l)]
                        ]++
                        [ "F"++show n++"."++trg' ++ "=F"++show n'++"."++src''
                        | ((n,l),(n',l'))<-zip (init (zip [0..] lst')) (tail (zip [0..] lst'))
                        , not (isNeg l), not (isNeg l')
                        , src'<-[quote$sqlExprSrc fSpec l]
                        , trg'<-[noCollideUnlessTm' l [src'] (quote$sqlExprTrg fSpec l)]
                        , src''<-[quote$sqlExprSrc fSpec l']
                        , trg''<-[noCollideUnlessTm' l' [src''] (quote$sqlExprTrg fSpec l')]
                        ]++
                        [ "c"++show n ++"."++(quote$sqlExprSrc fSpec (Tm (mIs c)(-1)))++" IS NOT NULL"
                        | (n,c)<-ncs
                        ]
                        where inCs n = n `elem` map fst ncs
   selectExpr _     _ _   _   (F  [] ) = error ("!Fatal (module RelBinGenBasics 242): Cannot create query for F [] because type is unknown")

   selectExpr fSpec i src trg (Tm (V _ (s,t))_   ) 
         | s==cptS && t==cptS = selectGeneric i ("1",src) ("1",trg)
                                                ("(SELECT 1) AS csnd")
                                                ("1"
                                                )
         | s==cptS            = selectGeneric i ("1",src) ("csnd."++trg',trg) 
                                                (quote (sqlConcept fSpec t) ++ " AS csnd")
                                                ("1"
                                                )
         | t==cptS            = selectGeneric i ("cfst."++src',src) ("1",trg)
                                                (quote (sqlConcept fSpec s) ++ " AS cfst")
                                                ("1"
                                                )
         | otherwise          = selectGeneric i ("cfst."++src',src) ("csnd."++trg'',trg)
                                                (quote (sqlConcept fSpec s) ++ " AS cfst, "++selectExprBrac fSpec i trg'' trg'' (Tm (mIs t)(-1))++" AS csnd")
                                                ("1"
                                                )
                        where src'  = if s==Anything
                                      then error ("!Fatal (module RelBinGenBasics 262): selectExpr fSpec i "++src++" "++trg++" (Tm (V _ ("++name s++","++name t++"))   )")
                                      else sqlAttConcept fSpec s
                              trg'  = if t==Anything
                                      then error ("!Fatal (module RelBinGenBasics 265): selectExpr fSpec i "++src++" "++trg++" (Tm (V _ ("++name s++","++name t++"))   )")
                                      else sqlAttConcept fSpec t
                              trg'' = noCollide' [src'] trg'

   selectExpr fSpec i src trg (Tm (I _ s _ _) _  ) | s == cptS = selectExpr fSpec i src trg (Tm (V [] (s,s))(-1))

   selectExpr fSpec i src trg (Tm mrph   _   ) = selectExprMorph fSpec i src trg mrph
   selectExpr fSpec i src trg (Tc expr      ) = selectExpr fSpec i src trg expr

 --src*trg zijn strings die aangeven wat de gewenste uiteindelijke typering van de query is (naar php of hoger in de recursie)
 --het is dus wel mogelijk om een -V te genereren van het gewenste type, maar niet om een V te genereren (omdat de inhoud niet bekend is)
   selectExpr fSpec i src trg (Fux [] ) = selectGeneric i ("1",src) ("1",trg) ("(SELECT 1) AS a") ("0")
   selectExpr fSpec i src trg (Fux es') = (phpIndent i) ++ "(" ++ (selectExprInUnion fSpec i src trg (Fux es')) ++ (phpIndent i) ++ ")"
   selectExpr fSpec i src trg (Cpx (Tm (V _ _)_)) = selectExpr fSpec i src trg (Fux [])
   selectExpr fSpec i src trg (Cpx e' )
      = selectGeneric i ("cfst."++src',src) ("csnd."++trg',trg)
                        (quote (sqlConcept fSpec (source e')) ++ " AS cfst, "++selectExprBrac fSpec i trg' trg' (Tm (mIs (target e'))(-1))++" AS csnd")
                        ("NOT EXISTS ("++ (selectExists' (i+12)
                                                         ((selectExprBrac fSpec (i+12) src2 trg2 e') ++ " AS cp")
                                                         ("cfst." ++ src' ++ "=cp."++src2++" AND csnd."++ trg'++"=cp."++trg2)
                                          ) ++ ")"
                        )
                        where src' = quote$sqlAttConcept fSpec (source e') 
                              trg' = noCollide' [src'] (sqlAttConcept fSpec (target e'))
                              src2 = quote$sqlExprSrc fSpec e'
                              trg2 = noCollideUnlessTm' e' [src2] (quote$sqlExprTrg fSpec e')
   selectExpr fSpec i src trg (K0x _)
      = error ("!Fatal (module RelBinGenBasics 292): SQL cannot create closures K0")
   selectExpr fSpec i src trg (K1x _)
      = error ("!Fatal (module RelBinGenBasics 294): SQL cannot create closures K1")
   selectExpr fSpec i src trg (Fdx  [e']       ) = selectExpr fSpec i src trg e'
   selectExpr fSpec i src trg (Fdx lst'@(fstm:_:_))
    = selectGeneric i (mainSrc,src) (mainTrg,trg)
                           (chain ", " (concExprs i ncs)) (chain ("\n"++[' '|n<-[0..i]]++"  AND ") (inner: cclauses ncs))
{-  De concepten in lst' noemen we c0, c1, ... cn (met n de lengte van lst')
    De elementen in lst' zelf noemen we reladd0, reladd1, ... reladdd(n-1).
    Deze namen worden aangehouden in de SQL-aliasing. Dat voorkomt naamconflicten op een wat ruwe manier, maar wel overzichtelijk en effectief.
-}
      where mainSrc = "c0."++src'
            mainTrg = "c"++show (length lst')++"."++trg'
            -- de strings die de source en target van Fd lst' weergeven. Bij gelijke namen vindt ontdubbeling van naam plaats met noCollideUnlessTm'
            src'    = quote$sqlExprSrc fSpec fstm
            trg'    = noCollideUnlessTm' fstm [src'] (quote$sqlExprTrg fSpec (last lst'))
            -- ncs geeft alleen de concepten uit lst', die in SQL doorlopen moeten worden, inclusief rangnummer
            ncs     = [ (0,source (head lst')), (length lst',target (last lst'))  ]
            ncs'    = [ (n',c)
                      | ((n,l),(n',l'))<-zip (init (zip [0..] lst')) (tail (zip [0..] lst'))
                      , not (isNeg l) || not (isNeg l')
                      , c<-[if target l<=source l' then target l else source l']
                      ]
            -- de SQL-expressies voor de concepten van lst', maar nu in SQL
            concExprs i ncs = [ selectExprBrac fSpec i sm sm tm ++ " AS c"++show n
                              | (n,c)<-ncs, tm<-[Tm (mIs c) (-1)], sm<-[quote$sqlExprSrc fSpec tm] ]
            -- de SQL-expressies voor de elementen uit lst', die elk een ADL-expressie representeren
            exprbracs = [ selectExprBrac fSpec i src' trg' (if isNeg l then notCp l else l) ++ " AS reladd"++show n 
                        | (n,l)<-zip [0..] lst'
                        , src'<-[quote$sqlExprSrc fSpec l]
                        , trg'<-[noCollideUnlessTm' l [src'] (quote$sqlExprTrg fSpec l)]
                        ]
            inner     = "NOT EXISTS ("++selectExists' (i+19)
                                                             (chain ", " (concExprs (i+19) ncs'))
                                                             (chain ("\n"++[' '|j<-[0..i+19]]++"  AND ") (wherecl++cclauses ncs'))
                                               ++")"
            -- de where expressies bevatten alle "magie". Dit is zgn. "terse code", die omzichtig behandeld moet worden.
            -- TODO: de volgende code is incorrect. Ook loopt ze uit de pas met de code voor F.
            wherecl   = (filter (not.null))
                        [ (if isNeg l then "   " else "NOT")++
                          " EXISTS ("++selectExists' (i+38)
                                                     (selectExprBrac fSpec (i+38) src'  trg'  (if isNeg l  then notCp l  else l ) ++ " AS reladd"++show n)
                                                     (chain " AND " ([(if inCs n then "c" else "reladd")++show n++"."++src' ++ "=reladd"++show  n   ++"."++src']++
                                                                     ["reladd"++show n++"."++trg' ++ (if inCs (n+1) then "=c" else "=reladd")++show (n+1)++"."++trg']))
                                 ++")"
                        | (n,l)<-zip [0..] lst'
                        , src'<-[quote$sqlExprSrc fSpec l]
                        , trg'<-[noCollideUnlessTm' l [src'] (quote$sqlExprTrg fSpec l)]
                        ]++
                        [ "reladd"++show n++"."++trg' ++ "=reladd"++show n'++"."++src''
                        | ((n,l),(n',l'))<-zip (init (zip [0..] lst')) (tail (zip [0..] lst'))
                        , isNeg l, isNeg l'
                        , src'<-[quote$sqlExprSrc fSpec l]
                        , trg'<-[noCollideUnlessTm' l [src'] (quote$sqlExprTrg fSpec l)]
                        , src''<-[quote$sqlExprSrc fSpec l']
                        ] where inCs n = n `elem` map fst (ncs++ncs')
            cclauses ncs = [ "c"++show n ++"."++(quote$sqlExprSrc fSpec (Tm ( mIs c)(-1)))++" IS NOT NULL"
                           | (n,c)<-ncs
                           ]
                        
   selectExpr _     _ _   _   (Fdx  [] ) = error ("!Fatal (module RelBinGenBasics 352): Cannot create query for Fd [] because type is unknown")

   -- selectExprInUnion is om de recursie te verbergen (deze veroorzaakt sql fouten)
   selectExprInUnion :: Fspc
                     -> Int
                     -> String
                     -> String
                     -> Expression
                     -> [Char]
   selectExprInUnion fSpec i src trg (Tc  e'        ) =  selectExprInUnion fSpec i src trg e'
   selectExprInUnion fSpec i src trg (F  [e']       ) =  selectExprInUnion fSpec i src trg e'

   selectExprInUnion fSpec i src trg (Fix [e']       ) =  selectExprInUnion fSpec i src trg e'
-- WAAROM? Stef, waarom is onderstaand niet:
-- selectExprInUnion fSpec i src trg (Fu (e':f     )) = (selectExprInUnion fSpec i src trg e') ++ (phpIndent i) ++ ") UNION (" ++ (selectExprInUnion fSpec i src trg (Fu f     )) ++ (phpIndent i) ++ ""

   selectExprInUnion fSpec i src trg (Fux (e':(f:fx))) = (selectExprInUnion fSpec i src trg e') ++ (phpIndent i) ++ ") UNION (" ++ (selectExprInUnion fSpec i src trg (Fux (f:fx))) ++ (phpIndent i) ++ ""
   selectExprInUnion fSpec i src trg (Fux [e']       ) =  selectExprInUnion fSpec i src trg e'
   selectExprInUnion fSpec i src trg e'               =  selectExpr        fSpec (i+4) src trg e'

   selectExprBrac :: Fspc
                  -> Int
                  -> String
                  -> String
                  -> Expression
                  -> [Char]
   selectExprBrac    f i s@(_:_)   t         e' | head s /= '`'
    = selectExprBrac f i (quote s) t         e'
   selectExprBrac    f i s         t@(_:_)   e' | head t /= '`'
    = selectExprBrac f i s         (quote t) e'
   selectExprBrac fSpec i src trg (Tc  e' )                             = selectExprBrac fSpec i src trg e'
   selectExprBrac fSpec i src trg (F  [e'])                             = selectExprBrac fSpec i src trg e'
   selectExprBrac fSpec i src trg (Fdx [e'])                             = selectExprBrac fSpec i src trg e'
   selectExprBrac fSpec i src trg (Fix [e'])                             = selectExprBrac fSpec i src trg e'
   selectExprBrac fSpec i src trg (Fux [e'])                             = selectExprBrac fSpec i src trg e'
   selectExprBrac fSpec _ src trg (Tm m@I{} _)
    | lowerCase(quote$sqlMorSrc fSpec m)==(quote$lowerCase$src)
      && lowerCase(quote$sqlMorTrg fSpec m)==(quote$lowerCase$trg) 
      = quote (sqlConcept fSpec (source m))
   selectExprBrac fSpec _ src trg (Tm m@Mph{} _)
    | lowerCase(quote$sqlMorSrc fSpec m)==(quote$lowerCase$src)
      && lowerCase(quote$sqlMorTrg fSpec m)==(quote$lowerCase$trg) 
      = quote$sqlMorName fSpec m
   selectExprBrac fSpec i src trg expr
    = phpIndent (i+5) ++ "( " ++ selectExpr fSpec (i+7) src trg expr++ phpIndent(i+5)++")"
   
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
      int2string n = if n `div` 10 == 0 then [intToDigit (n `rem` 10)|n>0] else int2string (n `div` 10)++[intToDigit (n `rem` 10)]
   
   noCollideUnlessTm' :: Expression
                     -> [String]
                     -> String
                     -> String

   noCollideUnlessTm' (Tm _ _) _ nm = quote nm
   noCollideUnlessTm' _  names nm = noCollide' names nm

   selectExprMorph :: Fspc
                   -> Int
                   -> String
                   -> String
                   -> Morphism
                   -> String

   selectExprMorph fSpec i src trg mph@V{}
    = selectGeneric i (src',src) (trg',trg)
                      (quote (sqlConcept fSpec (source mph)) ++ " AS vfst, "++quote (sqlConcept fSpec (target mph)) ++ " AS vsnd")
                      (src'++" IS NOT NULL AND "++trg'++" IS NOT NULL")
    where src'="vfst."++sqlAttConcept fSpec (source mph)
          trg'="vsnd."++sqlAttConcept fSpec (target mph)
   selectExprMorph _ _ src trg mph@Mp1{}
    | src == ""&&trg=="" = error ("!Fatal (module RelBinGenBasics 441): Source and target are \"\", use selectExists' for this purpose")
    | src == ""  = "SELECT "++mph1val mph++" AS "++trg
    | trg == ""  = "SELECT "++mph1val mph++" AS "++src
    | src == trg = "SELECT "++mph1val mph++" AS "++src
    | otherwise  = "SELECT "++mph1val mph++" AS "++src++", "++mph1val mph++" AS "++trg
   selectExprMorph fSpec i src trg mph -- made for both Mph and I
    | isIdent mph = selectGeneric i (quote$sqlAttConcept fSpec (source mph),src) (quote$sqlAttConcept fSpec (target mph),trg) (quote (sqlConcept fSpec (source mph))) "1"-- (quote (sqlConcept fSpec (source mph))++" IS NOT NULL")
-- note that sqlMorSrc may be called only with I{} and Mph{} patterns, which happens to be the case. Please take care when editing...
    | otherwise   = selectGeneric i (quote$sqlMorSrc fSpec mph,src) (quote$sqlMorTrg fSpec mph,trg) (quote$sqlMorName fSpec mph) "1"

   selectExists' :: Int -> String -> String -> String
   selectExists' i tbl whr
    = "SELECT *" ++
      phpIndent i ++ "  FROM " ++ tbl ++
      phpIndent i ++ " WHERE " ++ whr
   selectGeneric :: Int -> (String,String) -> (String,String) -> String -> String -> String
   selectGeneric i src trg tbl whr
    = selectcl ++
      phpIndent i ++ "  FROM "++tbl++
      (if whr=="1" then "" else phpIndent i ++ " WHERE "++whr)
      where selectcl | snd src=="" && snd trg=="" = error ("!Fatal (module RelBinGenBasics 461): Source and target are \"\", use selectExists' for this purpose")
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
   
   phpIndent :: Int -> [Char]
   phpIndent i = "\n"++take i (repeat ' ')

   phpIdentifier :: String -> String
   phpIdentifier (s:str) | isDigit s = "I"++phpIdentifier (s:str)
   phpIdentifier str = [c| c<-str, isAlphaNum c]

   phpShow :: String -> String
   phpShow str = "'" ++ addSlashes str ++ "'"

   addSlashes :: String -> String
   addSlashes ('\'': cs) = "\\'"++addSlashes cs
   addSlashes ('"': cs) = "\\\""++addSlashes cs
   addSlashes ('\\': cs) = "\\\\"++addSlashes cs
   addSlashes (c:cs) = c:addSlashes cs
   addSlashes "" = ""
   

   sqlRel :: Fspc -> Morphism -> (Plug,SqlField,SqlField)
   sqlRel fSpec m@Mph{}
    = if null pms then error ("!Fatal (module RelBinGenBasics 495): no plug found for morphism "++showADLcode fSpec m) else
      if not (null (tail pms)) then error ("!Fatal (module RelBinGenBasics 496): multiple plugs found for morphism "++showADLcode fSpec m) else
      head pms
      where pms = [(p,ms,mt)|p@PlugSql{}<-plugs fSpec, (m',ms,mt)<-mLkpTbl p, makeDeclaration m==makeDeclaration m']
   sqlRel fSpec m@I{}
    = if null pcs
      then error ("!Fatal (module RelBinGenBasics 501): no plug found for concept "++showADLcode fSpec (target m))
      else head pcs
      where pcs = [(p,col,col)|p@PlugSql{}<-plugs fSpec, (c,col)<-cLkpTbl p, c==target m]
   sqlRel fSpec m
    = error ("!Fatal (module RelBinGenBasics 505): no plug exists for "++showADLcode fSpec m) 

-- WAAROM bestaat sqlRelPlugs?
-- | sqlRelPlugs levert alle mogelijkheden om een plug met twee velden te vinden waarin expressie e is opgeslagen.
-- | Als (plug,sf,tf) `elem` sqlRelPlugs fSpec e, dan geldt e = (fldexpr sf)~;(fldexpr tf)
-- | Als sqlRelPlugs fSpec e = [], dan volstaat een enkele tabel lookup niet om e te bepalen
   sqlRelPlugs :: Fspc -> Expression -> [(Plug,SqlField,SqlField)] --(plug,source,target)
   sqlRelPlugs fSpec e = rd [ (plug,fld0,fld1)
                            | plug@PlugSql{}<-plugs fSpec
                            , (fld0,fld1)<-sqlPlugFields plug e
                            ] 
   sqlPlugFields :: Plug -> Expression -> [(SqlField, SqlField)]
   sqlPlugFields plug e' 
    = [(fld0,fld1)| fld0<-[f|f<-fields plug,target (fldexpr f)==source e']
                  , fld1<-[f|f<-fields plug,target (fldexpr f)==target e']
                  , let se = fldexpr fld0
                        te = fldexpr fld1
                        bs = (isTrue.disjNF) (Fux [Cpx e', F [flp se,te] ])  --       e' |- se~;te
                        bt = (isTrue.disjNF) (Fux [Cpx (F [flp se,te]),e'])  --       se~;te |- e'
                  , bs && bt                                               --       e' = se~;te
                  {- the above should be enough.. but the relation algebra calculations
                     are not good enough yet. In particular:
                       isFalse ((I/\x);e /\ -e)
                     and
                       isTrue  ((I/\e;e~);e \/ -e)
                     do not work (these should yield True instead of False in both cases)
                     
                     The code below fixes exactly these ommissions
                  -}
                  || (isProp (se) && (te == e')
                     && (isTrue$disjNF$Fux [Fix [ Tm (mIs (source e'))(-1), simplF [e',flp e'] ]
                                          ,Cpx$se]))
                  || (isProp (te) && (se == flp e')
                     && (isTrue$disjNF$Fux [Fix [ Tm (mIs (source e'))(-1), simplF [flp e',e'] ]
                                          ,Cpx$te]))
                  {- found another exception:
                       isFalse (I;I /\ -I)
                     and
                       isTrue  (I;I \/ -I)
                     yield False, but should yield True
                  -}
                  || (  (se == te) && isIdent e'
                     && (isSur se)
                     )
{- obsolete?      , --IF the expression is the identity relation, then I want a plug designed for the concept of the identity i.e. the source of the plug must be equal to the type of the identity.
                    --REMARK -> while user-defined plugs are on top of the list, they are applied before generated plugs , as wanted ("head cs"). But it's a bit coincidential if you ask GMI.
                    case e' of (Tm (I{}) _) -> elem (source e') (map (source.fldexpr) (fields plug))
                               _ -> True -}
                  ]
     where -- simplF: replace a;a~ by I if INJ&TOT
      simplF ks = simplify ( if null fs || null (head fs) then replF ks else replF $ head fs )
        where fs = [m' | F m' <- [simplify $ F ks]] -- if null, replF will probably not do a lot.
               -- null occurs especialy in cases of [I;e] and [e;I]
      replF (k:k2:ks) | k == flp k2 && isInj k && isTot k
             = if null ks then Tm(mIs$source k)(-1) else replF ks
      replF [a] = F [a]
      replF (k:k2:ks) | fs /= [k2:ks] -- ie: if something is replaced by replF
        = if null fs then F [k,res] else replF (k:head fs) -- we might replace something again!
        where res = replF (k2:ks)
              fs  = [m' | F m' <- [res]]
      replF [] -- this should not occur here, and if it does, it might cause errors in other code that should be solved here
       = error ("!Fatal (module RelBinGenBasics 566): Could not define a properly typed I for F[] in replF in sqlPlugFields in RelBinGenBasics.hs")
               -- this error does not guarantee, however, that simplF yields no F []. In particular: simplify (F [I;I]) == F []
      replF ks = F (ks)
   sqlExprSrc :: Fspc->Expression -> String
   sqlExprSrc fSpec expr = ses expr
    where
      ses (F [])         = error ("!Fatal (module RelBinGenBasics 572): "++if expr==F[] then "calling sqlExprSrc (F [])" else "evaluating (F []) in sqlExprSrc ("++showADLcode fSpec expr++")")
      ses (F [f])        = ses f
      ses (F fs)         = ses (head fs)
      ses (Fux [])        = error ("!Fatal (module RelBinGenBasics 575): "++if expr==F[] then "calling sqlExprSrc (Fu [])" else "evaluating (Fu []) in sqlExprSrc ("++showADLcode fSpec expr++")")
      ses (Fux [f])       = ses f
      ses (Fux fs)        = ses (head fs) --all subexprs have the same type --was: (head (filter l fs)) where l = (==foldr1 lub (map source fs)).source
      ses (Fix [])        = error ("!Fatal (module RelBinGenBasics 578): "++if expr==F[] then "calling sqlExprSrc (Fi [])" else "evaluating (Fi []) in sqlExprSrc ("++showADLcode fSpec expr++")")
      ses (Fix [f])       = ses f
      ses (Fix fs)        = ses (head fs) --all subexprs have the same type --was:(head (filter l fs)) where l = (==foldr1 lub (map source fs)).source
      ses (Fdx [])        = error ("!Fatal (module RelBinGenBasics 581): "++if expr==F[] then "calling sqlExprSrc (Fd [])" else "evaluating (Fd []) in sqlExprSrc ("++showADLcode fSpec expr++")")
      ses (Fdx [f])       = ses f
      ses (Fdx fs)        = ses (head fs)
      ses (Cpx e)         = ses e
      ses (K0x e)         = ses e
      ses (K1x e)         = ses e
      ses (Tc e)         = ses e
      ses (Tm m@Mp1{} _) = "Att"++(name (mph1typ m))
      ses (Tm m@I{} _)   = sqlMorSrc fSpec m
      ses (Tm m@Mph{} _) = sqlMorSrc fSpec m
      ses (Tm m@V{} _)   = sqlMorSrc fSpec (mIs (source m)) -- There is no sql representation of V[s,t], but the one we need is I[s].

   sqlExprTrg :: Fspc->Expression -> String
   sqlExprTrg fSpec e' = sqlExprSrc fSpec (flp e')

   sqlMorName :: Fspc -> Morphism -> String
   sqlMorName fSpec m@I{}   =    name p where (p,_,_) = sqlRel fSpec m
   sqlMorName fSpec m@Mph{} =    name p where (p,_,_) = sqlRel fSpec m
   sqlMorName fSpec m       = error ("!Fatal (module RelBinGenBasics 599): sqlMorName fSpec ("++showADLcode fSpec m++") has no representation.")
   
   -- these functions (USED TO BE: GMI 4 mrt 2010) exact copies of sqlRelSrc and sqlRelTrg!
   sqlMorSrc  :: Fspc -> Morphism -> String
   sqlMorSrc  fSpec m@I{}   = fldname s where (_,s,_) = sqlRel fSpec m
   sqlMorSrc  fSpec m@Mph{} = fldname (if inline m then s else t) where (_,s,t) = sqlRel fSpec m
   sqlMorSrc  fSpec m       = error ("!Fatal (module RelBinGenBasics 605): sqlMorSrc fSpec ("++showADLcode fSpec m++") has no representation.")

   sqlMorTrg  :: Fspc -> Morphism -> String
   sqlMorTrg  fSpec m@I{}   = fldname t where (_,_,t) = sqlRel fSpec m
   sqlMorTrg  fSpec m@Mph{} = fldname (if inline m then t else s) where (_,s,t) = sqlRel fSpec m
   sqlMorTrg  fSpec m       = error ("!Fatal (module RelBinGenBasics 610): sqlMorTrg fSpec ("++showADLcode fSpec m++") has no representation.")

-- sqlConcept gives the name of the plug that contains all atoms of concept c.
   sqlConcept :: Fspc -> Concept -> String
   sqlConcept fSpec c = name (sqlConceptPlug fSpec c)
   
-- sqlConcept yields the plug that contains all atoms of concept c. Since there may be more of them, the fist one is returned.
   sqlConceptPlug :: Fspc -> Concept -> Plug
   sqlConceptPlug fSpec c | c==cptS = error ("!Fatal (module RelBinGenBasics 618): Concept ONE may not be represented in SQL.")
                          | otherwise
                = if null ps then error ("!Fatal (module RelBinGenBasics 620): Concept \""++show c++"\" does not occur in fSpec (sqlConcept in module RelBinGenBasics)") else
                  head ps
                  where ps = [plug|plug@PlugSql{}<-plugs fSpec, not (null [c'|(c',fld)<-cLkpTbl plug, c'==c])]

-- was:                  
-- sqlConceptPlug fSpec c  = if null cs then error ("!Fatal (module RelBinGenBasics 703): Concept \""++show c++"\" does not occur in fSpec (sqlConceptPlug in module RelBinGenBasics)") else
--                           head cs
--                           where cs = [plug | plug@PlugSql{}<-plugs fSpec, c'<-concs plug, c'==c]

   sqlAttConcept :: Fspc -> Concept -> String
   sqlAttConcept fSpec c | c==cptS = "ONE"
                         | otherwise
                = if null cs then error ("!Fatal (module RelBinGenBasics 632): Concept \""++show c++"\" does not occur in its plug in fSpec \""++appname++"\" (sqlAttConcept in module RelBinGenBasics)") else
                  head cs
                  where cs = [fldname f|f<-fields (sqlConceptPlug fSpec c), c'<-concs f,c==c']
                        appname =  name fSpec

   lowerCase :: String->String
   lowerCase = map toLower -- from Char



--- uniqueNames p:ps | ((name p++(name source p)) `elem` (names ps)) = p:(uniqueNames ps)
   
   addToLast :: [a] -> [[a]] -> [[a]]
   addToLast _ [] = error "!Fatal (module RelBinGenBasics 645): addToLast: empty list"
   addToLast s as = (init as)++[last as++s]
{-
   --copied from revision 569, and isolated these functions for composing SELECT queries only
   sqlRelPlugs_sel :: Fspc -> Expression -> [(Plug,SqlField,SqlField)] --(plug,source,target)
   sqlRelPlugs_sel fSpec e' = rd [ (plug,sf,tf)
                             | plug@PlugSql{}<-plugs fSpec
                             , (sf,tf)<-sqlPlugFields_sel plug e'
                             ]
   sqlPlugFields_sel :: Plug -> Expression -> [(SqlField, SqlField)]
   sqlPlugFields_sel plug e'
                = [ (sf,tf)
                  | sf<-[f|f<-fields plug,target (fldexpr f)==source e']
                  , let se = fldexpr sf
                  , tf<-[f|f<-fields plug,target (fldexpr f)==target e']
                  , let te = fldexpr tf
                  , (  (isTrue $disjNF$Fu [Cp e',simplF [flp se,te]])
                    && (isFalse$disjNF$Fi [Cp e',simplF [flp se,te]])
                    )
                  {- the above should be enough.. but the relation algebra calculations
                     are not good enough yet. In particular:
                       isFalse ((I/\x);e /\ -e)
                     and
                       isTrue  ((I/\e;e~);e \/ -e)
                     do not work (these should yield True instead of False in both cases)
                     
                     The code below fixes exactly these ommissions
                  -}
                  || (isProp (se) && (te == e')
                     && (isTrue$disjNF$Fu [Fi [ Tm (mIs (source e')) (-1), simplF [e',flp e'] ]
                                          ,Cp$se]))
                  || (isProp (te) && (se == flp e')
                     && (isTrue$disjNF$Fu [Fi [ Tm (mIs (source e')) (-1), simplF [flp e',e'] ]
                                          ,Cp$te]))
                  {- found another exception:
                       isFalse (I;I /\ -I)
                     and
                       isTrue  (I;I \/ -I)
                     yield False, but should yield True
                  -}
                  || (  (se == te) && isIdent e'
                     && (isSur se)
                     )
                  ]
     where -- simplF: replace a;a~ by I if INJ&TOT
      simplF ks = simplify ( if null fs || null (head fs) then replF ks else replF $ head fs )
        where fs = [m' | F m' <- [simplify $ F ks]] -- if null, replF will probably not do a lot.
               -- null occurs especialy in cases of [I;e] and [e;I]
      replF (k:k2:ks) | k == flp k2 && isInj k && isTot k
             = if null ks then Tm(mIs$source k)(-1) else replF ks
      replF [a] = F [a]
      replF (k:k2:ks) | fs /= [k2:ks] -- ie: if something is replaced by replF
        = if null fs then F [k,res] else replF (k:head fs) -- we might replace something again!
        where res = replF (k2:ks)
              fs  = [m' | F m' <- [res]]
      replF [] -- this should not occur here, and if it does, it might cause errors in other code that should be solved here
       = error ("!Fatal (module RelBinGenBasics 701): Could not define a properly typed I for F[] in replF in sqlPlugFields_sel in RelBinGenBasics.hs")
               -- this error does not guarantee, however, that simplF yields no F []. In particular: simplify (F [I;I]) == F []
      replF ks = F (ks)
-}

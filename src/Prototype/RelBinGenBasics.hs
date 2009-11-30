module Prototype.RelBinGenBasics(phpIdentifier,naming,sqlRelPlugs,commentBlock,strReplace
 ,selectExpr,selectExprBrac,addSlashes,sqlExprTrg,sqlExprSrc,sqlAttConcept
 ,sqlPlugFields,indentBlock,phpShow,isOne,addToLast
 ,pDebug,noCollide -- both are used in ObjBinGenConnectToDatabase
 ) where
   import Char(isDigit,digitToInt,intToDigit,isAlphaNum,toLower)
   import Strings (chain) --TODO -> is this correct instead of chain from Auxiliaries?
   import Adl
   import ShowADL(showADL)
   import NormalForms (conjNF,disjNF,simplify)
   import Data.Fspec
   import Data.Plug
   import List(isPrefixOf)
   import Collection (Collection(rd,uni))
   import Auxiliaries (naming)
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
   
   selectExpr fSpec i src trg (Fi lst'@(_:_:_))
    = selectGeneric i ("isect0."++src',src) ("isect0."++trg',trg)
                           (chain ", " exprbracs) (chain " AND " wherecl)
      where src'    = quote$sqlExprSrc fSpec fstm
            trgC    = quote$sqlExprTrg fSpec fstm -- can collide with src', for example in case fst==r~;r, or if fst is a property (or identity)
            trg'    = noCollideUnlessTm fstm [src'] trgC
            fstm    = head posTms  -- always defined, because length posTms>0 (ensured in definition of posTms)
            mp1Tm   = take 1 ([t| t@(Tm (Mp1{}))<-lst']++[t| t@(F ((Tm (Mp1{})):(Tm (V _ _)):(Tm (Mp1{})):[])) <- lst'])
            lst     = [t|t<-lst', not (elem t mp1Tm)]
            posTms  = if null posTms' then map notCp (take 1 negTms') else posTms' -- we take a term out of negTms' if we have to, to ensure length posTms>0
            negTms  = if null posTms' then tail negTms' else negTms' -- if the first term is in posTms', don't calculate it here
            posTms' = [t| t<-lst, isPos t && not (isIdent t)]++[t| t<-lst, isPos t && isIdent t] -- the code to calculate I is better if it is not the first term
            negTms' = [notCp t| t<-lst, isNeg t && isIdent t]++[notCp t| t<-lst, isNeg t && not (isIdent t)] -- should a negTerm become a posTerm (for reasons described above), it can best be an -I.
            exprbracs = [ (selectExprBrac fSpec i src'' trg'' l) ++ " AS isect"++show n 
                        | (n,l)<-zip [0..] posTms
                        , src''<-[quote$sqlExprSrc fSpec l]
                        , trg''<-[noCollideUnlessTm l [src''] (quote$sqlExprTrg fSpec l)]
                        ]
            wherecl   = [if isIdent l
                         then  "isect0."++src'++" = isect0."++trg' -- this is the code to calculate ../\I. The code below will work, but is longer
                         else "(isect0."++src'++" = isect"++show n++"."++src''
                         ++ " AND isect0."++trg'++" = isect"++show n++"."++trg''++")"
                        | (n,l)<-tail (zip [0..] posTms) -- not empty because of definition of posTms
                        , src''<-[quote$sqlExprSrc fSpec l]
                        , trg''<-[noCollideUnlessTm l [src''] (quote$sqlExprTrg fSpec l)]
                        ]++
                        [ "isect0."++src'++" = "++mph1val m -- sorce and target are equal because this is the case with Mp1
                        | (Tm m@(Mp1{})) <- mp1Tm
                        ]++
                        [ "isect0."++src'++" = "++mph1val m1 -- sorce and target are unequal
                          ++ " AND isect0."++trg'++" = "++mph1val m2 -- sorce and target are unequal
                        | (F ((Tm m1@(Mp1{})):(Tm (V _ _)):(Tm m2@(Mp1{})):[])) <- mp1Tm
                        ]++
                        [if isIdent l
                         then  "isect0."++src'++" <> isect0."++trg' -- this code will calculate ../\-I
                         else  "NOT EXISTS ("++(selectExists' (i+12)
                                                              ((selectExprBrac fSpec (i+12) src'' trg'' l) ++ " AS cp")
                                                              ("isect0."++src' ++ "=cp."++src''++" AND isect0."++ trg'++"=cp."++trg'')
                                            )++")"
                        | (_,l)<-zip [0..] negTms
                        , src''<-[quote$sqlExprSrc fSpec l]
                        , trg''<-[noCollideUnlessTm l [src''] (quote$sqlExprTrg fSpec l)]
                        ]++["isect0."++src'++" IS NOT NULL", "isect0."++trg'++" IS NOT NULL"]
   selectExpr fSpec i src trg (Fi [e']) = selectExpr fSpec i src trg e'
   selectExpr _     _ _   _   (Fi [] ) = error ("RelBinGenBasics: Cannot create query for Fi [] because type is unknown")

   selectExpr fSpec i src trg (F (Tm (V _ (s,_)):fs@(_:_))) | s==cptS
     = selectGeneric i ("1",src) ("fst."++trg',trg)
                       (selectExprBrac fSpec i src' trg' (F fs) ++ " AS fst")
                       ("fst."++trg'++" IS NOT NULL")
                       where src' = noCollideUnlessTm (F fs) [trg'] (quote$sqlExprSrc fSpec (F fs))
                             trg' = quote$sqlExprTrg fSpec (F fs)
   selectExpr fSpec i src trg (F (s1@(Tm (Mp1{})):(s2@(Tm (V _ _)):(s3@(Tm (Mp1{})):fx@(_:_))))) -- to make more use of the thing below
     =  selectExpr fSpec i src trg (F ((F (s1:s2:s3:[])):fx))

   selectExpr fSpec i src trg (F ((Tm sr@(Mp1{})):((Tm (V _ _)):((Tm tr@(Mp1{})):[])))) -- this will occur quite often because of doSubsExpr
     = "SELECT "++mph1val sr++" AS "++src++", "++mph1val tr++" AS "++trg

   selectExpr fSpec i src trg (F (e'@(Tm sr@(Mp1{})):(f:fx)))
      = selectGeneric i ("fst."++src',src) ("fst."++trg',trg)
                        (selectExprBrac fSpec i src' trg' (F (f:fx))++" AS fst")
                        ("fst."++src'++" = "++mph1val sr)
                        where src' = quote$sqlExprSrc fSpec e'
                              trg' = noCollideUnlessTm (F (f:fx)) [src'] (quote$sqlExprTrg fSpec (F (f:fx)))

   selectExpr fSpec i src trg (F (e':((Tm (V _ _)):(f:fx)))) = -- prevent calculating V in this case
       if src==trg && not (isProp e')
       then error ("(Module RelBinGenBasics: selectExpr 2) src and trg are equal ("++src++") in "++showADL e')
       else
       selectGeneric i ("fst."++src',src) ("snd."++trg',trg)
                        ((selectExprBrac fSpec i src' mid' e')++" AS fst, "++(selectExprBrac fSpec i mid2' trg' f)++" AS snd")
                        ("fst."++src'++" IS NOT NULL")
            where src' = quote$sqlExprSrc fSpec e'
                  mid' = quote$sqlExprTrg fSpec e'
                  mid2'= quote$sqlExprSrc fSpec f
                  trg' = noCollideUnlessTm (F (f:fx)) [mid2'] (quote$sqlExprTrg fSpec (F (f:fx)))
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
            -- de strings die de source en target van F lst' weergeven. Bij gelijke namen vindt ontdubbeling van naam plaats met noCollideUnlessTm
            src'    = quote$sqlExprSrc fSpec fstm
            trg'    = noCollideUnlessTm fstm [src'] (quote$sqlExprTrg fSpec (last lst'))
            -- ncs geeft alleen de concepten uit lst', die in SQL doorlopen moeten worden, inclusief rangnummer
            ncs     = [ (0,source (head lst'))  | isNeg (head lst') ]++
                      [ (n',c)
                      | ((n,l),(n',l'))<-zip (init (zip [0..] lst')) (tail (zip [0..] lst'))
                      , isNeg l && isNeg l'
                      , c<-[if target l<=source l' then target l else source l']
                      ]++
                      [ (length lst',target (last lst'))  | isNeg (last lst') ]
            -- de SQL-expressies voor de concepten van lst', maar nu in SQL
            concExprs = [ selectExprBrac fSpec i sm sm tm ++ " AS c"++show n
                        | (n,c)<-ncs, tm<-[Tm $ mIs c], sm<-[quote$sqlExprSrc fSpec tm] ]
            -- de SQL-expressies voor de elementen uit lst', die elk een ADL-expressie representeren
            exprbracs = [ selectExprBrac fSpec i src' trg' (if isNeg l then notCp l else l) ++ " AS F"++show n 
                        | (n,l)<-zip [0..] lst'
                        , not (isNeg l)
                        , src'<-[quote$sqlExprSrc fSpec l]
                        , trg'<-[noCollideUnlessTm l [src'] (quote$sqlExprTrg fSpec l)]
                        ]
            -- de where expressies bevatten alle "magie". Dit is zgn. "terse code", die omzichtig behandeld moet worden.
            wherecl   = (filter (not.null))
                        [ if isNeg l
                          then "NOT EXISTS ("++selectExists' (i+12)
                                                             (selectExprBrac fSpec i src' trg' (if isNeg l then notCp l else l) ++ " AS F"++show n)
                                                             (chain " AND " ([(if inCs n then "c" else "F")++show n++"."++src' ++ "=F"++show  n   ++"."++src']++
                                                                             ["F"++show n++"."++trg' ++ (if inCs (n+1) then "=c" else "=F")++show (n+1)++"."++trg']))
                                               ++")"
                          else chain " AND " (["c"++show n++"."++src' ++ "=F"++show  n   ++"."++src'| inCs n]++
                                              ["F"++show n++"."++trg' ++ "=c"++show (n+1)++"."++trg'| inCs (n+1)])
                        | (n,l)<-zip [0..] lst'
                        , src'<-[quote$sqlExprSrc fSpec l]
                        , trg'<-[noCollideUnlessTm l [src'] (quote$sqlExprTrg fSpec l)]
                        ]++
                        [ "F"++show n++"."++trg' ++ "=F"++show n'++"."++src''
                        | ((n,l),(n',l'))<-zip (init (zip [0..] lst')) (tail (zip [0..] lst'))
                        , not (isNeg l), not (isNeg l')
                        , src'<-[quote$sqlExprSrc fSpec l]
                        , trg'<-[noCollideUnlessTm l [src'] (quote$sqlExprTrg fSpec l)]
                        , src''<-[quote$sqlExprSrc fSpec l']
                        ]++
                        [ "c"++show n ++"."++(quote$sqlExprSrc fSpec (Tm $ mIs c))++" IS NOT NULL"
                        | (n,c)<-ncs
                        ]
                        where inCs n = n `elem` map fst ncs
   selectExpr _     _ _   _   (F  [] ) = error ("RelBinGenBasics: Cannot create query for F [] because type is unknown")

   selectExpr fSpec i src trg (Tm (V _ (s,t))   ) 
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
                                                (quote (sqlConcept fSpec s) ++ " AS cfst, "++selectExprBrac fSpec i trg'' trg'' (Tm (mIs t))++" AS csnd")
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

 --src*trg zijn strings die aangeven wat de gewenste uiteindelijke typering van de query is (naar php of hoger in de recursie)
 --het is dus wel mogelijk om een -V te genereren van het gewenste type, maar niet om een V te genereren (omdat de inhoud niet bekend is)
   selectExpr fSpec i src trg (Fu [] ) = selectGeneric i ("1",src) ("1",trg) ("(SELECT 1) AS a") ("0")
   selectExpr fSpec i src trg (Fu es') = (phpIndent i) ++ "(" ++ (selectExprInUnion fSpec i src trg (Fu es')) ++ (phpIndent i) ++ ")"
   selectExpr fSpec i src trg (Cp (Tm (V _ _))) = selectExpr fSpec i src trg (Fu [])
   selectExpr fSpec i src trg (Cp e' )
      = selectGeneric i ("cfst."++src',src) ("csnd."++trg',trg)
                        (quote (sqlConcept fSpec (source e')) ++ " AS cfst, "++selectExprBrac fSpec i trg' trg' (Tm (mIs (target e')))++" AS csnd")
                        ("NOT EXISTS ("++ (selectExists' (i+12)
                                                         ((selectExprBrac fSpec (i+12) src2 trg2 e') ++ " AS cp")
                                                         ("cfst." ++ src' ++ "=cp."++src2++" AND csnd."++ trg'++"=cp."++trg2)
                                          ) ++ ")"
                        )
                        where src' = quote$sqlAttConcept fSpec (source e') 
                              trg' = noCollide [src'] (sqlAttConcept fSpec (target e'))
                              src2 = quote$sqlExprSrc fSpec e'
                              trg2 = noCollideUnlessTm e' [src2] (quote$sqlExprTrg fSpec e')
   selectExpr fSpec i src trg (K0 _)
      = error ("error in selectExpr - RelBinGenBasics: SQL cannot create closures K0")
   selectExpr fSpec i src trg (K1 _)
      = error ("error in selectExpr - RelBinGenBasics: SQL cannot create closures K1")
{- obsolete:
   selectExpr _     _ _   _   (Fd []  ) = error ("RelBinGenBasics: Cannot create query for Fd [] because type is unknown")
   selectExpr fSpec i src trg (Fd [e']) = selectExpr fSpec i src trg e'
   selectExpr fSpec i src trg (Fd fxs ) = selectExpr fSpec i src trg $ Cp {e=F (map addcompl fxs)}
         where
         addcompl fx@(Cp{}) = e fx
         addcompl fx = Cp{e=fx}
-}
   selectExpr fSpec i src trg (Fd  [e']       ) = selectExpr fSpec i src trg e'
   selectExpr fSpec i src trg (Fd lst'@(fstm:_:_))
    = selectGeneric i (mainSrc,src) (mainTrg,trg)
                           (chain ", " (concExprs i ncs)) (chain ("\n"++[' '|n<-[0..i]]++"  AND ") (inner: cclauses ncs))
{-  De concepten in lst' noemen we c0, c1, ... cn (met n de lengte van lst')
    De elementen in lst' zelf noemen we reladd0, reladd1, ... reladdd(n-1).
    Deze namen worden aangehouden in de SQL-aliasing. Dat voorkomt naamconflicten op een wat ruwe manier, maar wel overzichtelijk en effectief.
-}
      where mainSrc = "c0."++src'
            mainTrg = "c"++show (length lst')++"."++trg'
            -- de strings die de source en target van Fd lst' weergeven. Bij gelijke namen vindt ontdubbeling van naam plaats met noCollideUnlessTm
            src'    = quote$sqlExprSrc fSpec fstm
            trg'    = noCollideUnlessTm fstm [src'] (quote$sqlExprTrg fSpec (last lst'))
            -- ncs geeft alleen de concepten uit lst', die in SQL doorlopen moeten worden, inclusief rangnummer
            ncs     = [ (0,source (head lst')), (length lst',target (last lst'))  ]
            ncs'    = [ (n',c)
                      | ((n,l),(n',l'))<-zip (init (zip [0..] lst')) (tail (zip [0..] lst'))
                      , not (isNeg l) || not (isNeg l')
                      , c<-[if target l<=source l' then target l else source l']
                      ]
            -- de SQL-expressies voor de concepten van lst', maar nu in SQL
            concExprs i ncs = [ selectExprBrac fSpec i sm sm tm ++ " AS c"++show n
                              | (n,c)<-ncs, tm<-[Tm $ mIs c], sm<-[quote$sqlExprSrc fSpec tm] ]
            -- de SQL-expressies voor de elementen uit lst', die elk een ADL-expressie representeren
            exprbracs = [ selectExprBrac fSpec i src' trg' (if isNeg l then notCp l else l) ++ " AS reladd"++show n 
                        | (n,l)<-zip [0..] lst'
                        , src'<-[quote$sqlExprSrc fSpec l]
                        , trg'<-[noCollideUnlessTm l [src'] (quote$sqlExprTrg fSpec l)]
                        ]
            inner     = "NOT EXISTS ("++selectExists' (i+19)
                                                             (chain ", " (concExprs (i+19) ncs'))
                                                             (chain ("\n"++[' '|j<-[0..i+19]]++"  AND ") (wherecl++cclauses ncs'))
                                               ++")"
            -- de where expressies bevatten alle "magie". Dit is zgn. "terse code", die omzichtig behandeld moet worden.
            wherecl   = (filter (not.null))
                        [ (if isNeg l then "   " else "NOT")++
                          " EXISTS ("++selectExists' (i+38)
                                                     (selectExprBrac fSpec (i+38) src'  trg'  (if isNeg l  then notCp l  else l ) ++ " AS reladd"++show n)
                                                     (chain " AND " ([(if inCs n then "c" else "reladd")++show n++"."++src' ++ "=reladd"++show  n   ++"."++src']++
                                                                     ["reladd"++show n++"."++trg' ++ (if inCs (n+1) then "=c" else "=reladd")++show (n+1)++"."++trg']))
                                 ++")"
                        | (n,l)<-zip [0..] lst'
                        , src'<-[quote$sqlExprSrc fSpec l]
                        , trg'<-[noCollideUnlessTm l [src'] (quote$sqlExprTrg fSpec l)]
                        ]++
                        [ "reladd"++show n++"."++trg' ++ "=reladd"++show n'++"."++src''
                        | ((n,l),(n',l'))<-zip (init (zip [0..] lst')) (tail (zip [0..] lst'))
                        , isNeg l, isNeg l'
                        , src'<-[quote$sqlExprSrc fSpec l]
                        , trg'<-[noCollideUnlessTm l [src'] (quote$sqlExprTrg fSpec l)]
                        , src''<-[quote$sqlExprSrc fSpec l']
                        ] where inCs n = n `elem` map fst (ncs++ncs')
            cclauses ncs = [ "c"++show n ++"."++(quote$sqlExprSrc fSpec (Tm $ mIs c))++" IS NOT NULL"
                           | (n,c)<-ncs
                           ]
                        
   selectExpr _     _ _   _   (Fd  [] ) = error ("RelBinGenBasics: Cannot create query for Fd [] because type is unknown")

   -- selectExprInUnion is om de recursie te verbergen (deze veroorzaakt sql fouten)
   selectExprInUnion :: Fspc
                     -> Int
                     -> String
                     -> String
                     -> Expression
                     -> [Char]
   selectExprInUnion fSpec i src trg (Tc  e'        ) =  selectExprInUnion fSpec i src trg e'
   selectExprInUnion fSpec i src trg (F  [e']       ) =  selectExprInUnion fSpec i src trg e'

   selectExprInUnion fSpec i src trg (Fi [e']       ) =  selectExprInUnion fSpec i src trg e'
-- WAAROM? Stef, waarom is onderstaand niet:
-- selectExprInUnion fSpec i src trg (Fu (e':f     )) = (selectExprInUnion fSpec i src trg e') ++ (phpIndent i) ++ ") UNION (" ++ (selectExprInUnion fSpec i src trg (Fu f     )) ++ (phpIndent i) ++ ""

   selectExprInUnion fSpec i src trg (Fu (e':(f:fx))) = (selectExprInUnion fSpec i src trg e') ++ (phpIndent i) ++ ") UNION (" ++ (selectExprInUnion fSpec i src trg (Fu (f:fx))) ++ (phpIndent i) ++ ""
   selectExprInUnion fSpec i src trg (Fu [e']       ) =  selectExprInUnion fSpec i src trg e'
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
   selectExprBrac fSpec i src trg (Fd [e'])                             = selectExprBrac fSpec i src trg e'
   selectExprBrac fSpec i src trg (Fi [e'])                             = selectExprBrac fSpec i src trg e'
   selectExprBrac fSpec i src trg (Fu [e'])                             = selectExprBrac fSpec i src trg e'
   selectExprBrac fSpec _ src trg (Tm m'@(I{}))
    | lowerCase(quote$sqlMorSrc fSpec m')==(quote$lowerCase$src)
      && lowerCase(quote$sqlMorTrg fSpec m')==(quote$lowerCase$trg) 
      = quote (sqlConcept fSpec (source m'))
   selectExprBrac fSpec _ src trg (Tm m'@(Mph{}))
    | lowerCase(quote$sqlMorSrc fSpec m')==(quote$lowerCase$src)
      && lowerCase(quote$sqlMorTrg fSpec m')==(quote$lowerCase$trg) 
      = quote$sqlMorName fSpec m'
   selectExprBrac fSpec i src trg expr
    = phpIndent (i+5) ++ "( " ++ selectExpr fSpec (i+7) src trg expr++ phpIndent(i+5)++")"
   
   noCollide :: [String] -> String -> String
   noCollide nms nm = quote$noCollide' (map unquote nms) (unquote nm)
   unquote :: String->String
   unquote ('`':xs) = init xs
   unquote xs = xs
   noCollide' :: [String] -> String -> String
   noCollide' names nm | nm `elem` names = noCollide names (namepart (reverse nm) ++ changeNr (numberpart (reverse nm)))
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
   
   noCollideUnlessTm :: Expression
                     -> [String]
                     -> String
                     -> String

   noCollideUnlessTm (Tm _) _ nm = nm
   noCollideUnlessTm _  names nm = noCollide names nm

   selectExprMorph :: Fspc
                   -> Int
                   -> String
                   -> String
                   -> Morphism
                   -> String

   selectExprMorph fSpec i src trg mph@(V _ _)
    = selectGeneric i (src',src) (trg',trg)
                      (quote (sqlConcept fSpec (source mph)) ++ " AS vfst, "++quote (sqlConcept fSpec (target mph)) ++ " AS vsnd")
                      (src'++" IS NOT NULL AND "++trg'++" IS NOT NULL")
    where src'="vfst."++sqlAttConcept fSpec (source mph)
          trg'="vsnd."++sqlAttConcept fSpec (target mph)
   selectExprMorph _ _ src trg mph@(Mp1{})
    | src == ""&&trg=="" = error ("Fatal in selectExprMorph (RelBinGenBasics): Source and target are \"\", use selectExists' for this purpose")
    | src == ""  = "SELECT "++mph1val mph++" AS "++trg
    | trg == ""  = "SELECT "++mph1val mph++" AS "++src
    | src == trg = "SELECT "++mph1val mph++" AS "++src
    | otherwise  = "SELECT "++mph1val mph++" AS "++src++", "++mph1val mph++" AS "++trg
   selectExprMorph fSpec i src trg mph -- made for both Mph and I
    | isIdent mph = selectGeneric i (quote$sqlAttConcept fSpec (source mph),src) (quote$sqlAttConcept fSpec (target mph),trg) (quote (sqlConcept fSpec (source mph))) "1"-- (quote (sqlConcept fSpec (source mph))++" IS NOT NULL")
    | otherwise   = selectGeneric i (sqlMorSrc fSpec mph,src) (sqlMorTrg fSpec mph,trg) (quote$sqlMorName fSpec mph) "1"

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
      where selectcl | snd src=="" && snd trg=="" = error ("Fatal in selectGeneric (RelBinGenBasics): Source and target are \"\", use selectExists' for this purpose")
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
   
   -- WAAROM?? Onderstaande declaratie mag wel wat verduidelijking.
   -- Het lijkt me ook niet onderhoudbaar.
   sqlRelName :: (Show m,Morphic m,MorphicId m,Morphical m) => Fspc -> m -> String
   sqlRelName fSpec m'
    = if isIdent m' then sqlConcept fSpec (source m') else
      if isTrue m' then "V" else
      if null as then error ("(module RelBinGenBasics) Fatal error in RelBinGen.lhs (sqlRelName): No declarations in "++show m') else
      if length as>1 then error ("(module RelBinGenBasics) Fatal error in RelBinGen.lhs (sqlRelName): Multiple declarations in "++show m') else
      name plug
      where (plug,_,_) = sqlRelPlug fSpec (Tm (makeMph a))
            as = declarations m'
            a = head as
   
    
   sqlRelPlug :: Fspc -> Expression -> (Plug,SqlField,SqlField) --(plug,source,target)
   sqlRelPlug fSpec expr = if null cs then (mError "Plug"
                                           ,field (name (source expr)) (mError "Source Expression") Nothing (mError "Null") (mError "isUniq")
                                           ,field (name (target expr)) (mError "Target Expression") Nothing (mError "Null") (mError "isUniq")
                                           )
                           else head cs
                           where cs = sqlRelPlugs fSpec expr
                                 mError tp = error ("\n(module RelBinGenBasics) Expression \""++show expr++"\" does not occur in plugs of fSpec, cannot give "++tp++" (sqlRelPlug in module RelBinGenBasics)")
   
   sqlRelPlugs :: Fspc -> Expression -> [(Plug,SqlField,SqlField)] --(plug,source,target)
   sqlRelPlugs fSpec e' = rd [ (plug,sf,tf)
                             | plug@PlugSql{}<-plugs fSpec
                             , (sf,tf)<-sqlPlugFields plug e'
                             ]
   sqlPlugFields :: Plug -> Expression -> [(SqlField, SqlField)]
   sqlPlugFields plug e'
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
                     && (isTrue$disjNF$Fu [Fi [ Tm (mIs (source e')), simplF [e',flp e'] ]
                                          ,Cp$se]))
                  || (isProp (te) && (se == flp e')
                     && (isTrue$disjNF$Fu [Fi [ Tm (mIs (source e')), simplF [flp e',e'] ]
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
             = if null ks then Tm$mIs$source k else replF ks
      replF [a] = F [a]
      replF (k:k2:ks) | fs /= [k2:ks] -- ie: if something is replaced by replF
        = if null fs then F [k,res] else replF (k:head fs) -- we might replace something again!
        where res = replF (k2:ks)
              fs  = [m' | F m' <- [res]]
      replF [] -- this should not occur here, and if it does, it might cause errors in other code that should be solved here
       = error ("Could not define a properly typed I for F[] in replF in sqlPlugFields in RelBinGenBasics.hs")
               -- this error does not guarantee, however, that simplF yields no F []. In particular: simplify (F [I;I]) == F []
      replF ks = F (ks)
   sqlExprSrc :: Fspc->Expression -> String
   sqlExprSrc _     (F [])         = error ("(Module RelBinGenBasics: ) calling sqlExprSrc (F [])")
   sqlExprSrc fSpec (F [f])        = sqlExprSrc fSpec f
   sqlExprSrc fSpec (F fs)         = sqlExprSrc fSpec (head fs)
   sqlExprSrc _     (Fu [])        = error ("(Module RelBinGenBasics: ) calling sqlExprSrc (Fu [])")
   sqlExprSrc fSpec (Fu [f])       = sqlExprSrc fSpec f
   sqlExprSrc fSpec (Fu fs)        = sqlExprSrc fSpec (head fs) --all subexprs have the same type --was: (head (filter l fs)) where l = (==foldr1 lub (map source fs)).source
   sqlExprSrc _     (Fi [])        = error ("(Module RelBinGenBasics: ) calling sqlExprSrc (Fi [])")
   sqlExprSrc fSpec (Fi [f])       = sqlExprSrc fSpec f
   sqlExprSrc fSpec (Fi fs)        = sqlExprSrc fSpec (head fs) --all subexprs have the same type --was:(head (filter l fs)) where l = (==foldr1 lub (map source fs)).source
   sqlExprSrc _     (Fd [])        = error ("(Module RelBinGenBasics: ) calling sqlExprSrc (Fd [])")
   sqlExprSrc fSpec (Fd [f])       = sqlExprSrc fSpec f
   sqlExprSrc fSpec (Fd fs)        = sqlExprSrc fSpec (head fs)
   sqlExprSrc fSpec (Cp e')        = sqlExprSrc fSpec e'
   sqlExprSrc fSpec (K0 e')        = sqlExprSrc fSpec e'
   sqlExprSrc fSpec (K1 e')        = sqlExprSrc fSpec e'
   sqlExprSrc fSpec (Tc e')        = sqlExprSrc fSpec e'
   sqlExprSrc _     (Tm m@(Mp1{})) = "Att"++(name (mph1typ m))
   sqlExprSrc fSpec (Tm m')        = sqlMorSrc fSpec m'

   sqlExprTrg :: Fspc->Expression -> String
   sqlExprTrg fSpec e' = sqlExprSrc fSpec (flp e')

   sqlMorName :: Fspc -> Morphism -> String
   sqlMorName fSpec (Mph _ _ _ _ _ s) = sqlRelName fSpec s
   sqlMorName fSpec (I _ _ s _)            = sqlConcept fSpec s
   sqlMorName _ m' = error ("(module RelBinGenBasics) sqlMorName: illegal argument: "++show m')
   
   -- these functions are exact compies of sqlRelSrc and sqlRelTrg!
   sqlMorSrc :: Fspc -> Morphism -> String
   {- sqlMorSrc _ i@V{}   = name (source i)
   sqlMorSrc _ i@I{}   = name (source i)
   sqlMorSrc _ i@Mph{} = name (source i)
   sqlMorSrc _ i@Mp1{} = name (source i) -}
   sqlMorSrc fSpec s = fldname src
    where (_,src,_) = sqlRelPlug fSpec (Tm s)

   sqlMorTrg :: Fspc -> Morphism -> String
   {- sqlMorTrg _ i@V{}   = name (target i)
   sqlMorTrg _ i@I{}   = name (target i)
   sqlMorTrg _ i@Mph{} = name (target i)
   sqlMorTrg _ i@Mp1{} = name (target i) -}
   sqlMorTrg fSpec s = fldname trg
    where (_,_,trg) = sqlRelPlug fSpec (Tm s)


   sqlConcept :: Fspc -> Concept -> String
   sqlConcept fSpec c | c==cptS = "ONE"
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

   lowerCase :: String->String
   lowerCase = map toLower -- from Char



--- uniqueNames p:ps | ((name p++(name source p)) `elem` (names ps)) = p:(uniqueNames ps)
   
   addToLast :: [a] -> [[a]] -> [[a]]
   addToLast _ [] = error "(RelBinGenBasics) addToLast: empty list"
   addToLast s as = (init as)++[last as++s]
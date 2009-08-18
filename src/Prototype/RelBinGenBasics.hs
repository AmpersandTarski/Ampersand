module Prototype.RelBinGenBasics(phpIdentifier,naming,sqlRelPlugs,commentBlock,strReplace
 ,selectExpr,selectExprBrac,addSlashes,sqlExprTrg,sqlExprSrc,sqlMorName,sqlConcept,sqlAttConcept
 ,sqlMorSrc,indentBlock,phpShow,isOne,addToLast
 ,pDebug,noCollide -- both are used in ObjBinGenConnectToDatabase
 ,plugs) where
   import Char(isDigit,digitToInt,intToDigit,isAlphaNum,toLower)
   import Strings (chain) --TODO -> is this correct instead of chain from Auxiliaries?
   import Adl
   import ShowADL(showADL)
   import CC_aux(fun)
   import NormalForms (conjNF)
   import Data.Fspec
   import Data.Plug
   import List(isPrefixOf)
   import Collection ((>-),Collection(rd,uni))

   pDebug :: Bool
   pDebug = True
   
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
   isOne :: ObjectDef -> Bool
   isOne o = (fun.multiplicities.conjNF.F) [v (source (ctx o),source (ctx o)),ctx o]

   
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

   selectExpr ::    Fspc    -- current context
                 -> Int        -- indentation
                 -> String     -- SQL name of the source of this expression, as assigned by the environment 
                 -> String     -- SQL name of the target of this expression, as assigned by the environment
                 -> Expression -- expression to be translated
                 -> String     -- resulting SQL expression

   selectExpr fSpec i src trg (Fi lst'@(_:_:_))
    = selectGeneric i ("isect0."++src',src) ("isect0."++trg',trg)
                           (chain ", " exprbracs) (chain " AND " (wherecl))
      where src'    = sqlExprSrc fSpec fstm
            trgC    = sqlExprTrg fSpec fstm -- can collide with src', for example in case fst==r~;r, or if fst is a property (or identity)
            trg'    = noCollideUnlessTm fstm [src'] trgC
            fstm     = head posTms  -- always defined, because length posTms>0 (ensured in definition of posTms)
            mp1Tm   = take 1 ([t| t@(Tm (Mp1 _ _))<-lst']++[t| t@(F ((Tm (Mp1 _ _)):(Tm (V _ _)):(Tm (Mp1 _ _)):[])) <- lst'])
            lst     = [t|t<-lst', not (elem t mp1Tm)]
            posTms  = if null posTms' then map notCp (take 1 negTms') else posTms' -- we take a term out of negTms' if we have to, to ensure length posTms>0
            negTms  = if null posTms' then tail negTms' else negTms' -- if the first term is in posTms', don't calculate it here
            posTms' = [t| t<-lst, isPos t && not (isIdent t)]++[t| t<-lst, isPos t && isIdent t] -- the code to calculate I is better if it is not the first term
            negTms' = [notCp t| t<-lst, isNeg t && isIdent t]++[notCp t| t<-lst, isNeg t && not (isIdent t)] -- should a negTerm become a posTerm (for reasons described above), it can best be an -I.
            exprbracs = [ (selectExprBrac fSpec (i) src'' trg'' l) ++ " AS isect"++show n 
                        | (n,l)<-zip [(0::Integer)..] posTms
                        , src''<-[sqlExprSrc fSpec l]
                        , trg''<-[noCollideUnlessTm l [src''] (sqlExprTrg fSpec l)]
                        ]
            wherecl   = [if isIdent l
                         then  "isect0."++src'++" = isect0."++trg' -- this is the code to calculate ../\I. The code below will work, but is longer
                         else "(isect0."++src'++" = isect"++show n++"."++src''
                         ++ " AND isect0."++trg'++" = isect"++show n++"."++trg''++")"
                        | (n,l)<-tail (zip [(0::Integer)..] posTms) -- not empty because of definition of posTms
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
                        | (_,l)<-zip [(0::Integer)..] negTms
                        , src''<-[sqlExprSrc fSpec l]
                        , trg''<-[noCollideUnlessTm l [src''] (sqlExprTrg fSpec l)]
                        ]

   selectExpr fSpec i src trg (F (Tm (V _ (s,_)):fs@(_:_))) | s==cptS
     = selectGeneric i ("1",src) ("fst."++trg',trg)
                       (selectExprBrac fSpec i src' trg' (F fs) ++ " AS fst")
                       ("1")
                       where src' = noCollideUnlessTm (F fs) [trg'] (sqlExprSrc fSpec (F fs))
                             trg' = sqlExprTrg fSpec (F fs)
   selectExpr fSpec i src trg (F (s1@(Tm (Mp1 _ _)):(s2@(Tm (V _ _)):(s3@(Tm (Mp1 _ _)):fx@(_:_))))) -- to make more use of the thing below
     =  selectExpr fSpec i src trg (F ((F (s1:s2:s3:[])):fx))

   selectExpr _ _ src trg (F ((Tm (Mp1 sr _)):((Tm (V _ _)):((Tm (Mp1 tr _)):[])))) -- this will occur quite often because of doSubsExpr
     = "SELECT "++sr++" AS "++src++", "++tr++" AS "++trg

   selectExpr fSpec i src trg (F (e'@(Tm (Mp1 sr _)):(f:fx)))
      = selectGeneric i ("fst."++src',src) ("fst."++trg',trg)
                        (selectExprBrac fSpec (i) src' trg' (F (f:fx))++" AS fst")
                        ("fst."++src'++" = "++sr)
                        where src' = sqlExprSrc fSpec e'
                              trg' = noCollideUnlessTm (F (f:fx)) [src'] (sqlExprTrg fSpec (F (f:fx)))

   selectExpr fSpec i src trg (F (e':((Tm (V _ _)):(f:fx)))) = -- prevent calculating V in this case
       if src==trg && not (isProp e')
       then error ("(Module RelBinGenBasics: selectExpr 2) src and trg are equal ("++src++") in "++showADL e')
       else
       selectGeneric i ("fst."++src',src) ("snd."++trg',trg)
                        ((selectExprBrac fSpec i src' mid' e')++" AS fst, "++(selectExprBrac fSpec i mid2' trg' f)++" AS snd")
                        "1"
            where src' = sqlExprSrc fSpec e'
                  mid' = sqlExprTrg fSpec e'
                  mid2'= sqlExprSrc fSpec f
                  trg' = noCollideUnlessTm (F (f:fx)) [mid2'] (sqlExprTrg fSpec (F (f:fx)))
{-
   selectExpr fSpec i src trg (F (e':(f:fx))) | null vs && mid'==mid2' && b1==b2 =
        selectGeneric i ("joined."++src',src) ("joined."++trg',trg)
                        (b1++" AS joined")
                        "1"
   --  The values of  src', mid', mid2', and trg' can be anything, as long as they are distinct. Here they have been chosen (arbitrarily) to be meaningful names derived from source and targets.
                        where src' = noCollideUnlessTm e' [mid'] (sqlExprSrc fSpec e')
                              mid' = sqlExprTrg fSpec e'
                              mid2'= sqlExprSrc fSpec f
                              trg' = noCollideUnlessTm (F (f:fx)) [mid2'] (sqlExprTrg fSpec (F (f:fx)))
                              b1 = selectExprBrac fSpec (i) mid' src' (flp e')
                              b2 = selectExprBrac fSpec (i) mid2' trg' (F (f:fx))
                              vs = [v'|(Tm v'@V{})<-[e',f]++fx]
-}

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
   selectExpr fSpec i src trg (F  (e':(f:fx)))
      = selectGeneric i ("fst."++src',src) ("snd."++trg',trg)
                        (selectExprBrac fSpec (i) src' mid' e'++" AS fst, "++selectExprBrac fSpec (i) mid2' trg' (F (f:fx))++" AS snd")
                        ("fst."++mid'++" = snd."++mid2')
   --  The values of  src', mid', mid2', and trg' can be anything, as long as they are distinct. Here they have been chosen (arbitrarily) to be meaningful names derived from source and targets.
                        where src' = sqlExprSrc fSpec e'
                              mid' = noCollideUnlessTm e' [src'] (sqlExprTrg fSpec e')
                              mid2'= sqlExprSrc fSpec f
                              trg' = noCollideUnlessTm (F (f:fx)) [mid2'] (sqlExprTrg fSpec (F (f:fx)))
   selectExpr fSpec i src trg (F  [e']       ) = selectExpr fSpec i src trg e'
   selectExpr fSpec i src trg (Fi (e':(f:fx))) = selectGeneric i ("fst."++src',src) ("fst."++trg',trg) ((selectExprBrac fSpec (i) src' trg' e')++" AS fst, "++(selectExprBrac fSpec (i) src'' trg'' (Fi (f:fx)))++" AS snd") ("fst."++src'++" = snd."++src''++" AND fst."++trg'++"=snd."++trg'')
                        where src'  = sqlExprSrc fSpec e'
                              trg'  = noCollide [src'] (sqlExprTrg fSpec e')
                              src'' = sqlExprSrc fSpec f
                              trg'' = noCollide [src''] (sqlExprTrg fSpec f)
   selectExpr fSpec i src trg (Fi [e']) = selectExpr fSpec i src trg e'
   selectExpr _     _ _   _   (F  [] ) = error ("RelBinGenBasics.lhs: Cannot create query for F [] because type is unknown")
   selectExpr _     _ _   _   (Fi [] ) = error ("RelBinGenBasics.lhs: Cannot create query for Fi [] because type is unknown")
 --src*trg zijn strings die aangeven wat de gewenste uiteindelijke typering van de query is (naar php of hoger in de recursie)
 --het is dus wel mogelijk om een -V te genereren van het gewenste type, maar niet om een V te genereren (omdat de inhoud niet bekend is)
   selectExpr _     i src trg (Fu [] ) = selectGeneric i ("\\'\\'",src) ("\\'\\'",trg) ("(SELECT 1) AS a") ("0")
   selectExpr fSpec i src trg (Fu es') = (phpIndent i) ++ "(" ++ (selectExprInUnion fSpec (i) src trg (Fu es')) ++ (phpIndent i) ++ ")"
   selectExpr fSpec i src trg (Cp (Tm (V _ _))) = selectExpr fSpec i src trg (Fu [])
   selectExpr fSpec i src trg (Cp e' )
      = selectGeneric i ("cfst."++src',src) ("csnd."++trg',trg)
                        (sqlConcept fSpec (source e') ++ " AS cfst, "++selectExprBrac fSpec (i) trg' trg' (Tm (mIs (target e')))++" AS csnd")
                        ("NOT EXISTS ("++ (selectExists' (i+12)
                                                         ((selectExprBrac fSpec (i+12) src2 trg2 e') ++ " AS cp")
                                                         ("cfst." ++ src' ++ "=cp."++src2++" AND csnd."++ trg'++"=cp."++trg2)
                                          ) ++ ")"
                        )
                        where src' = sqlAttConcept fSpec (source e') 
                              trg' = noCollide [src'] (sqlAttConcept fSpec (target e'))
                              src2 = sqlExprSrc fSpec e'
                              trg2 = noCollideUnlessTm e' [src2] (sqlExprTrg fSpec e')
   selectExpr fSpec i src trg cl@(K0 _) = selectGeneric i (sqlExprSrc fSpec cl,src) (sqlExprTrg fSpec cl,trg) (sqlRelName fSpec cl) "1"
   selectExpr fSpec i src trg cl@(K1 _) = selectGeneric i (sqlExprSrc fSpec cl,src) (sqlExprTrg fSpec cl,trg) (sqlRelName fSpec cl) "1"
   selectExpr _     _ _   _   (Fd []  ) = error ("RelBinGenBasics.lhs: Cannot create query for Fd [] because type is unknown")
   selectExpr fSpec i src trg (Fd [e']) = selectExpr fSpec i src trg e'
   selectExpr fSpec i src trg (Fd fxs ) = selectExpr fSpec i src trg $ Cp {e=F (map addcompl fxs)}
         where
         addcompl fx@(Cp{}) = e fx
         addcompl fx = Cp{e=fx}
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
   selectExprInUnion fSpec i src trg (Fu (e':(f:fx))) = (selectExprInUnion fSpec i src trg e') ++ (phpIndent i) ++ ") UNION (" ++ (selectExprInUnion fSpec (i) src trg (Fu (f:fx))) ++ (phpIndent i) ++ ""
   selectExprInUnion fSpec i src trg (Fu [e']       ) =  selectExprInUnion fSpec i src trg e'
   selectExprInUnion fSpec i src trg e'               =  selectExpr        fSpec (i+4) src trg e'

   selectExprBrac :: Fspc
                  -> Int
                  -> String
                  -> String
                  -> Expression
                  -> [Char]
   selectExprBrac fSpec i src trg (Tc  e' )                             = selectExprBrac fSpec i src trg e'
   selectExprBrac fSpec i src trg (F  [e'])                             = selectExprBrac fSpec i src trg e'
   selectExprBrac fSpec i src trg (Fi [e'])                             = selectExprBrac fSpec i src trg e'
   selectExprBrac fSpec i src trg (Fu [e'])                             = selectExprBrac fSpec i src trg e'
   selectExprBrac fSpec _ src trg (Tm m'@(Mph{}))
    | lowerCase(sqlMorSrc fSpec m')==(lowerCase$src) && lowerCase(sqlMorTrg fSpec m')==(lowerCase$trg)  = sqlMorName fSpec m'
   selectExprBrac fSpec _ src trg (K0 e')
    | (lowerCase(sqlExprSrc fSpec e'),(lowerCase$sqlExprTrg fSpec e'))==(lowerCase$src,lowerCase$trg) = sqlRelName fSpec (K0 e')
   selectExprBrac fSpec _ src trg (K1 e')
    | (lowerCase(sqlExprSrc fSpec e')==(lowerCase$src) || src=="")
    &&(lowerCase(sqlExprTrg fSpec e')==(lowerCase$trg) || trg=="") = sqlRelName fSpec (K1 e')
   selectExprBrac fSpec i src trg expr                                 = phpIndent (i+4) ++ "( " ++ selectExpr fSpec (i+6) src trg expr++ phpIndent(i+4)++")"

   noCollide :: [String] -> String -> String
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
    = selectGeneric i ("vfst."++sqlAttConcept fSpec (source mph),src) ("vsnd."++sqlAttConcept fSpec (target mph),trg)
                      (sqlConcept fSpec (source mph) ++ " AS vfst, "++sqlConcept fSpec (target mph) ++ " AS vsnd")
                      "1"
   selectExprMorph _ _ src trg (Mp1 str _)
    | src == ""&&trg=="" = error ("Fatal in selectExprMorph (RelBinGenBasics.hs): Source and target are \"\", use selectExists' for this purpose")
    | src == ""  = "SELECT "++str++" AS "++trg
    | trg == ""  = "SELECT "++str++" AS "++src
    | src == trg = "SELECT "++str++" AS "++src
    | otherwise  = "SELECT "++str++" AS "++src++", "++str++" AS "++trg
   selectExprMorph fSpec i src trg mph -- made for both Mph and I
    | isIdent mph = selectGeneric i (sqlAttConcept fSpec (source mph),src) (sqlAttConcept fSpec (target mph),trg) (sqlConcept fSpec (source mph)) "1"
    | otherwise   = selectGeneric i (sqlMorSrc fSpec mph,src) (sqlMorTrg fSpec mph,trg) (sqlMorName fSpec mph) "1"

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
      where selectcl | snd src=="" && snd trg=="" = error ("Fatal in selectGeneric (RelBinGenBasics.hs): Source and target are \"\", use selectExists' for this purpose")
                     | snd src==snd trg  = "SELECT DISTINCT " ++ selectSelItem src
                     | snd src==""   = "SELECT DISTINCT " ++ selectSelItem trg
                     | snd trg==""   = "SELECT DISTINCT " ++ selectSelItem src
                     | otherwise = "SELECT DISTINCT " ++ selectSelItem src ++", "++selectSelItem trg
   selectSelItem :: (String, String) -> String
   selectSelItem (att,alias) | selectSameName (att,alias) = att
                             | otherwise                  = att++" AS "++alias
   selectSameName :: (String,String)->Bool
   selectSameName (att,alias) = afterPoint att == alias
    where afterPoint s = if (myafterPoint s == "") then s else myafterPoint s
          myafterPoint ('.':xs) = xs
          myafterPoint ( _ :xs) = myafterPoint xs
          myafterPoint []       = []
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
                             , sf<-[f|f<-fields plug,target (fldexpr f)==source e']
                             , tf<-[f|f<-fields plug,target (fldexpr f)==target e']
                             ,  ((Sur `elem` multiplicities (fldexpr sf))
                                 &&(conjNF (F [fldexpr  sf,    e'])==fldexpr tf)
                                )
                             || ((Sur `elem` multiplicities (fldexpr tf))
                                 &&(conjNF (F [fldexpr  tf,flp e'])==fldexpr sf)
                                )
                             || (source (fldexpr sf)==source e' && fldexpr tf ==     e')
                             || (source (fldexpr tf)==target e' && fldexpr sf == flp e')
                             ]
   
   sqlExprSrc :: Fspc->Expression -> String
   sqlExprSrc _     (F [])    = error ("(Module RelBinGenBasics: ) calling sqlExprSrc (F [])")
   sqlExprSrc fSpec (F [f])   = sqlExprSrc fSpec f
   sqlExprSrc fSpec (F fs)    = sqlExprSrc fSpec (head fs)
   sqlExprSrc _     (Fu [])   = error ("(Module RelBinGenBasics: ) calling sqlExprSrc (Fu [])")
   sqlExprSrc fSpec (Fu [f])  = sqlExprSrc fSpec f
   sqlExprSrc fSpec (Fu fs)   = sqlExprSrc fSpec (head fs) --all subexprs have the same type --was: (head (filter l fs)) where l = (==foldr1 lub (map source fs)).source
   sqlExprSrc _     (Fi [])   = error ("(Module RelBinGenBasics: ) calling sqlExprSrc (Fi [])")
   sqlExprSrc fSpec (Fi [f])  = sqlExprSrc fSpec f
   sqlExprSrc fSpec (Fi fs)   = sqlExprSrc fSpec (head fs) --all subexprs have the same type --was:(head (filter l fs)) where l = (==foldr1 lub (map source fs)).source
   sqlExprSrc _     (Fd [])   = error ("(Module RelBinGenBasics: ) calling sqlExprSrc (Fd [])")
   sqlExprSrc fSpec (Fd [f])  = sqlExprSrc fSpec f
   sqlExprSrc fSpec (Fd fs)   = sqlExprSrc fSpec (head fs)
   sqlExprSrc fSpec (Cp e')   = sqlExprSrc fSpec e'
   sqlExprSrc fSpec (K0 e')   = sqlExprSrc fSpec e'
   sqlExprSrc fSpec (K1 e')   = sqlExprSrc fSpec e'
   sqlExprSrc fSpec (Tc e')   = sqlExprSrc fSpec e'
   sqlExprSrc _     (Tm (Mp1 _ t)) = "Att"++(name t)
   sqlExprSrc fSpec (Tm m') = sqlMorSrc fSpec m'
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
   sqlConcept fSpec c | c==cptS = "ONEsqlConcept"
                      | otherwise
                = if null cs then error ("(module RelBinGenBasics) Concept \""++show c++"\" does not occur in fSpec (sqlConcept in module RelBinGenBasics)") else
                  head cs
                  where cs = [name plug|let plug=sqlConceptPlug fSpec c, c'<-concs plug, c'==c]
   
   sqlConceptPlug :: Fspc -> Concept -> Plug
   sqlConceptPlug fSpec c = if null cs then error ("(module RelBinGenBasics) Concept \""++show c++"\" does not occur in fSpec (sqlConceptPlug in module RelBinGenBasics)") else
                            head cs
                            where cs = [plug | plug@PlugSql{}<-plugs fSpec, c'<-concs plug, c'==c]

   sqlAttConcept :: Fspc -> Concept -> String
   sqlAttConcept fSpec c | c==cptS = "ONEsqlAttConcept"
                         | otherwise
                = if null cs then error ("(module RelBinGenBasics) Concept \""++show c++"\" does not occur in its plug in fSpec \""++appname++"\" (sqlAttConcept in module RelBinGenBasics)") else
                  head cs
                  where cs = [fldname f|f<-fields (sqlConceptPlug fSpec c), c'<-concs f,c==c']
                        FS_id appname =  fsfsid fSpec

{-
Code below is meant for inside the Fspec structure.
In particular the function plugs 
-}
   plugs :: Fspc -> [Plug]
   plugs spc = neededPlugs given spc ++ given
     where given = vplugs spc
   
   neededPlugs :: [Plug] -> Fspc -> [Plug] -- given [Plug], what plugs are still needed to implement Fspc?
   neededPlugs given spec
    = theplugs
      where
       otherRels      = looseRels >- mors (given ++ complexPlugs)
       looseRels      = map makeMph (vrels spec) >- mors given
       looseConcs     = concs (vrels spec) -- todo: we can make this less, since V[conc] isn't allways asked for..
                        >- concs (given ++ relPlugs ++ complexPlugs)
       -- complexPlugs are the plugs that consist of multiple relations
       {-uniSurRels :: Morphisms
       uniSurRels     = [r | r <- rd (looseRels++map flp looseRels) , null ([Uni,Sur,Inj] >- multiplicities r)]
       -}
       complexPlugs   = [] {-
       complexPlugs   = [ mkdataset rclos
                        | rclos <- [ foldl1 (\x y -> if length x <= length y then y else x) cl -- longest list in cl
                                   | cl <- eqClass (\x y -> length x < length (x >- y)) -- there is overlap between x and y
                                                   (clos source target uniSurRels)
                                   ]
                        ] -}
       --mkdataset rclos= error "mkdataset function not done yet"
                        
       relPlugs       = map mor2plug otherRels
       theplugs       = uniqueNames (map name given) (complexPlugs ++ relPlugs ++ map conc2plug looseConcs)
   
   uniqueNames :: [String]->[Plug]->[Plug]
   -- MySQL is case insensitive! (hence the lowerCase)
   uniqueNames given plgs = naming (\x y->x{plname=y}) -- renaming function for plugs
                                   (map ((.) lowerCase) -- functions that name a plug (lowercase!)
                                        (name:n1:n2:[(\x->lowerCase(name x ++ show n))
                                                    |n<-[(1::Integer)..]])
                                   )
                                   (map lowerCase given) -- the plug-names taken
                                   (map uniqueFields plgs)
     where n1 p = name p ++ plsource p
           n2 p = name p ++ pltarget p
           plsource p = name (source (fldexpr (head (fields (p)))))
           pltarget p = name (target (fldexpr (last (fields (p)))))
           uniqueFields plug = plug{fields = naming (\x y->x{fldname=y}) -- renaming function for fields
                                             (map ((.) lowerCase) -- lowercase-yielding
                                                  (fldname:[(\x->fldname x ++ show n)
                                                           |n<-[(1::Integer)..]])
                                             )
                                             [] -- no field-names are taken
                                             (fields plug)
                                   }
   lowerCase :: String->String
   lowerCase = map toLower -- from Char
   {- naming - a naming function
      The objective is to name all items in a list uniquely
      
      The call below will label allItems as 1,2,3 etc, skipping 4:
      naming nameIt [(\x->show n)|n<-[(1::Integer)..]] ["4"] allItems
      
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
   
   conc2plug :: Concept -> Plug
   conc2plug c = plugsql (name c) [field (name c) (Tm (mIs c)) Nothing False True]
   
   mor2plug :: Morphism -> Plug
   mor2plug  m'
    = {- If the morphism is UNI, INJ and SUR
       we can identify the target by its source alone
       we shoud, however do this afterwards, transforming ALL plugs
      if ( isUni && isInj && isSur && fldtyp (target m) == SQLId ) || (isIdent m)
      then plugsql (name (source m)) [field (name (source m)) (Tm (mIs (source m))) Nothing False True
                                     ,field (name (target m)) (Tm m) (Just SQLBool) (not isTot) True]
      else -}
      if isInj && not isUni then mor2plug (flp m')
      else if isUni || isTot
      then plugsql (name m') [field (name (source m')) (Tm (mIs (source m'))) Nothing False isUni
                            ,field (name (target m')) (Tm m') Nothing (not isTot) isInj]
      else if isInj || isSur then mor2plug (flp m')
      else plugsql (name m') [field (name (source m')) (Fi {es=[Tm (mIs (source m')),F {es=[Tm m',flp (Tm m')]}]}
                                                     )      Nothing False False
                            ,field (name (target m')) (Tm m') Nothing False False]
      where
        mults = multiplicities m'
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
                                 _        -> SQLVarchar 255
                               }
   
   addToLast :: [a] -> [[a]] -> [[a]]
   addToLast _ [] = error "(RelBinGenBasics) addToLast: empty list"
   addToLast s as = (init as)++[last as++s]
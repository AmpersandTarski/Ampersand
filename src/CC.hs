{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# LANGUAGE FlexibleContexts #-}
module CC (pArchitecture, keywordstxt, keywordsops, specialchars, opchars) where
   import UU_Scanner  ( Token(..),TokenType(..),noPos
                      , pKey,pConid,pString,pSpec,pAtom,pExpl,pVarid,pComma)
   import UU_Parsing  (Parser
                      , (<$>) , (<$), (<*>), (<*) , (*>), (<|>)
                      ,pList,pListSep,pList1,pList1Sep,pSym
                      ,pSucceed
                      ,opt, Sequence,Alternative, IsParser
                      )
   import Collection  (Collection(..))
   import Auxiliaries (sort)
   import Adl         (Architecture(..)
                      ,Concept(..)
                      ,ConceptDef(..)
                      ,Context(..)
                      ,Declaration(..)
                      ,Expression(..)
                      ,Gen(..)
                      ,KeyDef(..)
                      ,Label(..)
                      ,Morphism(..)
                      ,Service(..),ObjectDef(..),ObjectDefs
                      ,RoleRelation(..),RoleService(..)
                      ,Pairs,Paire,mkPair
                      ,Pattern(..)
                      ,PExplanation(..)
                      ,Population(..)
                      ,Prop(..)
                      ,Rule(..),RuleType(..)
                      ,PExplObj(..)
                      ,PExpression(..)
                      ,FilePos(..)
                      ,Sign
                      ,UnOp(..),MulOp(..)
                      ,cptAnything
                      ,cptnew
                      ,flp
                      ,cptS
                      )
   import ShowADL     (showADL)
   import Languages
   import Strings     (trim)
   import Options     (defaultFlags, Options(..))
   import Data.Explain(AutoExplain,string2AutoExplain)
   keywordstxt :: [String]
   keywordstxt       = [ "CONTEXT", "ENDCONTEXT", "EXTENDS"
                       , "PATTERN", "ENDPATTERN"
                       , "SERVICE", "INITIAL", "SQLPLUG", "PHPPLUG"
                       , "POPULATION", "CONTAINS"
                       , "UNI", "INJ", "SUR", "TOT", "SYM", "ASY", "TRN", "RFX", "PROP", "ALWAYS"
                       , "RULE", "MAINTAINS", "SIGNALS", "SIGNAL", "ON","TEST"
                       , "RELATION", "CONCEPT", "KEY"
                       , "IMPORT", "GEN", "ISA", "I", "V"
                       , "PRAGMA", "EXPLANATION", "EXPLAIN", "PURPOSE", "IN", "REF", "ENGLISH", "DUTCH"
                       , "ONE", "BIND", "TOPHP", "BINDING"
                       , "BYPLUG"
                       , "ROLE", "EDITS", "USES"
                       ]
   keywordsops :: [String]
   keywordsops       = [ "-|", "|-", "-", "->", ">", "=", "~", "+", ";", "!", "*", "::", ":", "\\/", "/\\", "\\", "/", "<>" ]
   specialchars :: String
   specialchars      = "()[].,{}"
   opchars :: String
   opchars           = rd (sort (concat keywordsops))

   pArchitecture        :: Parser Token Architecture
   pArchitecture = Arch <$> pList1 pContext

   pBind             :: Parser Token (Declaration,String)
   pBind              = rebuild <$ pKey "BIND" <*> pDeclaration <* pKey "TOPHP" <*> (pConid <|> pString)
                       where rebuild d s = (d,s)

   pContext         :: Parser Token Context
   pContext  = rebuild <$ pKey "CONTEXT" <*> pConid <*>
                                  ((rebexpr <$ pKey ":" <*> pExpr <*> 
                                  --     (pSpec '{' *> pList1Sep (pSpec ';') pBind <* pSpec '}')
                                       ((pKey "BINDING" *> pList1Sep (pSpec ',') pBind) `opt` [])
                                    ) `opt` universe) <*>
                                  ((pKey "EXTENDS" *> pList1Sep (pSpec ',') pConid) `opt` []) <*>
                                  pList pContextElement <* pKey "ENDCONTEXT"
                       where  
                       rebexpr x y = (x,y)
                       universe = (Tm(V [] (cptAnything,cptAnything)) (-1),[]) --default: the universe
                       rebuild nm env on ces = 
                          Ctx { ctxnm   = nm
                              , ctxon   = on
                              , ctxisa  = empty
                              , ctxwrld = []
                              , ctxpats = [p| CPat p<-ces]
                              , ctxrs   = []
                              , ctxds   = [d| CDcl d<-ces]
                              , ctxcs   = [c| CCon c<-ces]
                              , ctxks   = [k| CKey k<-ces]
                              , ctxsvcs = [s| Csvc s<-ces]
                              , ctxps   = [e| CXpl e<-ces]
                              , ctxros  = [r| CRos r<-ces]
                              , ctxmed  = [r| CMed r<-ces]
                              , ctxpops = [Popu mph prs| CPop mph prs<-ces]
                              , ctxsql  = [plug| CSqlPlug plug<-ces]
                              , ctxphp  = [plug| CPhpPlug plug<-ces]
                              , ctxenv  = env
                              }

   data ContextElement = CPat Pattern
                       | CDcl Declaration
                       | CCon ConceptDef
                       | CKey KeyDef
                       | Csvc Service
                       | CPop Morphism Pairs
                       | CSqlPlug ObjectDef
                       | CPhpPlug ObjectDef
                       | CXpl PExplanation
                       | CRos RoleService
                       | CMed RoleRelation

   pLanguageID        :: Parser Token Lang
   pLanguageID         = lang <$> (pKey "IN" *> (pKey "DUTCH" <|> pKey "ENGLISH")) `opt` Dutch
                         where
                          lang str = case str of
                                      "DUTCH"      -> Dutch
                                      "ENGLISH"    -> English
                                      _ -> error ("!Fatal (module CC 89): "++if null str then "must specify a language in pLanguageID" else "language "++str++" is not supported")

   pRefID             :: Parser Token String
   pRefID              = (pKey "REF" *> pString) `opt` []

   pExplain           :: Parser Token PExplanation
   pExplain            = PExpl <$ pKey "EXPLAIN" <*> pExplObj <*> pLanguageID <*> pRefID <*> pExpl      <|>  -- syntax will become obsolete
                         PExpl <$ pKey "PURPOSE" <*> pExplObj <*> pLanguageID <*> pRefID <*> pExpl

   pExplObj           :: Parser Token PExplObj
   pExplObj            = PExplConceptDef  <$ pKey "CONCEPT"    <*> (pConid <|> pString) <|>
                         PExplDeclaration <$ pKey "RELATION"   <*> pMorphism            <|>
                         PExplRule        <$ pKey "RULE"       <*> pADLid               <|>
                         PExplKeyDef      <$ pKey "KEY"        <*> pADLid               <|>  
                         PExplObjectDef   <$ pKey "SERVICE"    <*> pADLid               <|>
                         PExplPattern     <$ pKey "PATTERN"    <*> pADLid               <|>
                         PExplContext     <$ pKey "CONTEXT"    <*> pADLid


   pContextElement    :: Parser Token ContextElement
   pContextElement     = CPat     <$> pPattern      <|>
                         CDcl     <$> pDeclaration  <|>
                         CCon     <$> pConceptDef   <|>
                         CKey     <$> pKeyDef       <|>
                         Csvc     <$> pService      <|>
                         CSqlPlug <$> pSqlplug      <|>
                         CPhpPlug <$> pPhpplug      <|>
                         CXpl     <$> pExplain      <|>
                         CRos     <$> pRoleService  <|>
                         CMed     <$> pRoleRelation <|>
                         CPop     <$  pKey "POPULATION" <*> pMorphism <* pKey "CONTAINS" <*> pContent

   pPattern         :: Parser Token Pattern
   pPattern  = rebuild <$  pKey "PATTERN" <*> (pConid <|> pString)
                       <*> pList pPatElem
                       <*  pKey "ENDPATTERN"
                       where
                         rebuild :: String -> [PatElem] -> Pattern
                         rebuild nm pes = Pat { ptnm        = nm
                                              , ptrls       = [r{r_pat=nm}|Pr r<-pes]
                                              , ptgns       = [gen{genpat=nm} |Pg gen<-pes]
                                              , ptdcs       = [mph{decpat=nm}| Pm mph@(Sgn{})<-pes]
                                              , ptcds       = [c| Pc c<-pes]
                                              , ptkds       = [k| Pk k<-pes]
                                              , ptxps       = [e| Pe e<-pes]
                                              , testexpr    = [e|Ptest e<-pes]
                                              , inftestexpr = []
                                              } 

   data PatElem      = Pr Rule
                     | Pg Gen
                     | Pm Declaration
                     | Pc ConceptDef
                     | Pk KeyDef
                     | Pe PExplanation
                     | Ptest (PExpression Morphism (Maybe Sign))

   pPatElem         :: Parser Token PatElem
   pPatElem          = Pr <$> pRuleDef      <|>
                       Pg <$> pGen          <|>
                       Pm <$> pDeclaration  <|>
                       Pc <$> pConceptDef   <|>
                       Pk <$> pKeyDef       <|>
                       Pe <$> pExplain      <|>
                       Ptest <$ pKey "TEST" <*> pPExpression


   pSignal          :: Parser Token (String, FilePos)
   pSignal           = --pKey "SIGNAL" *> pADLid_val_pos <* pKey "ON"       <|> obsolete syntax
                         pKey "RULE" *> pADLid_val_pos <* pKey "SIGNALS"
   pAlways          :: Parser Token (String, FilePos)
   pAlways           = ( pKey "RULE" *> pADLid_val_pos <* pKey "MAINTAINS" ) `opt` ("",Nowhere)

   pRuleDef         :: Parser Token Rule
   pRuleDef          = hc True            <$>   -- This boolean tells whether this rule will be a signal rule or a maintaining rule.
                          pSignal         <*>   -- "RULE m SIGNALS" (or "RULE m MAINTAINS in other cases)
                          pExpr           <*>   -- the antecedent
                          pKey_pos "|-"   <*>   -- parse the implication (or "=" in other cases)
                          pExpr           <*>   -- the consequent
                          ((pKey "EXPLANATION" *> pString) `opt` [])
                       <|>
                       kc True  <$> pSignal <*> pExpr <*> pKey_pos "-|" <*> pExpr <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
                       dc True  <$> pSignal <*> pExpr <*> pKey_pos "="  <*> pExpr <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
                       ac True  <$> pSignal <*>                             pExpr <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
                       hc False <$> pAlways <*> pExpr <*> pKey_pos "|-" <*> pExpr <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
                       kc False <$> pAlways <*> pExpr <*> pKey_pos "-|" <*> pExpr <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
                       dc False <$> pAlways <*> pExpr <*> pKey_pos "="  <*> pExpr <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
                       ac False <$> pAlways <*>                             pExpr <*> ((pKey "EXPLANATION" *> pString) `opt` [])
                       where
                        hc isSg (lbl,po) antc po' cons expl
                          = Ru { rrsrt = Implication
                               , rrant = antc
                               , rrfps = rulepos (lbl,po) po'
                               , rrcon = cons
                               , rrxpl = string2ExplainAllLang expl  
                               , rrtyp = (cptAnything,cptAnything)
                               , rrtyp_proof = Nothing
                               , rrdcl = Nothing
                               , runum = 0
                               , r_pat = ""
                               , r_usr = True
                               , r_sgl = isSg
                               , srrel = emptySignalDeclaration lbl po isSg
                               }
                        kc isSg (lbl,po) cons po' antc expl = hc isSg (lbl,po) antc po' cons expl
                        dc isSg (lbl,po) defd po' expr expl
                          = Ru { rrsrt = Equivalence
                               , rrant = defd
                               , rrfps = rulepos (lbl,po) po'
                               , rrcon = expr
                               , rrxpl = string2ExplainAllLang expl
                               , rrtyp = (cptAnything,cptAnything)
                               , rrtyp_proof = Nothing
                               , rrdcl = Nothing
                               , runum = 0
                               , r_pat = ""
                               , r_usr = True
                               , r_sgl = isSg
                               , srrel = emptySignalDeclaration lbl po isSg
                               }
                        ac isSg (lbl,po) expr expl
                          = Ru { rrsrt = Truth
                               , rrant = defd
                               , rrfps = po
                               , rrcon = expr
                               , rrxpl = string2ExplainAllLang expl
                               , rrtyp = (cptAnything,cptAnything)
                               , rrtyp_proof = Nothing
                               , rrdcl = Nothing
                               , runum = 0
                               , r_pat = ""
                               , r_usr = True
                               , r_sgl = isSg
                               , srrel = emptySignalDeclaration lbl po isSg
                               }
                         where defd=error ("!Fatal (module CC 222): defd undefined in pRuleDef "++showADL expr)
                        emptySignalDeclaration lbl po isSg
                         = Sgn lbl         -- decnm
                               cptAnything -- desrc
                               cptAnything -- detrg
                               []          -- decprps
                               []          -- decprps_calc
                               ""          -- decprL
                               ""          -- decprM
                               ""          -- decprR
                               []          -- decpopu
                               po          -- decfpos
                               0           -- decid
                               isSg        -- deciss
                               False       -- decusr
                               ""          -- decpat
                               True        -- decplug
                        rulepos (lbl,po) po' = if null lbl then po' else po -- position of the label is preferred. In its absence, take the position of the root operator of this rule's expression.
                        string2ExplainAllLang :: String -> [AutoExplain]      -- TODO: This is a workaround to cope with the fact that in the current ADL syntax, it cannot be determined in what language the EXPLANATION part of the rule is written in. 
                        string2ExplainAllLang str = [string2AutoExplain (defaultFlags {language=Dutch}) str]
                                                 ++ [string2AutoExplain (defaultFlags {language=English}) str]
                        
   pGen             :: Parser Token Gen
   pGen              = rebuild <$ pKey "GEN" <*> (pConid <|> pString) <*> pKey_pos "ISA" <*> (pConid <|> pString)
                       where rebuild spc p gen = G p (cptnew gen ) (cptnew spc ) ""

   postStr          :: Parser Token String
   postStr           = f <$> pList1 (pKey "~" <|> pKey "+" <|> pKey "-" <|> pKey "*")
                       where
                        f xs = g ['~'|'~'<-concat xs] ++ g ['-'|'-'<-concat xs] ++ eat [x|x<-concat xs,x/='~',x/='-']
                        g xs = if odd (length xs) then take 1 xs else []

   eat :: [Char] -> [Char]
   eat ('*':'*':xs) = eat ('*':xs)
   eat ('+':'*':xs) = eat ('*':xs)
   eat ('*':'+':xs) = eat ('*':xs)
   eat ('+':'+':xs) = eat ('+':xs)
   eat (x:xs)       = x:eat xs
   eat []           = []

   preStr          :: Parser Token String
   preStr           = g <$> pList1 (pKey "-")
                       where
                        g xs = if odd (length cs) then take 1 cs else [] where cs = concat xs

                              
   --Morphisms, or expressions in parentheses are terms with optional pre and post unary operators and optional type directive.
   --pExpression parses expressions composed of these terms and (>1)-ary operators.
   --pMorphism has already parsed the first type directive after a morphism without post-operator i.e. r[A*B][C*D] is possible.
     --type correct examples given r::A*B i.e. [Y*Y] is irrelevant.
        -- r[A*B]
        -- r[Y*Y][A*B]
        -- r[A*B]~
        -- r[A*B]~[B*A]
        -- -r[A*B]
        -- -r[Y*Y][A*B]
        -- -r[A*B]~
        -- -r[Y*Y]~[B*A]  
   pPTerm :: Parser Token (PExpression Morphism (Maybe Sign))
   pPTerm  = pe <$> preOp <*> (pSpec '(' *> pPExpression <* pSpec ')') <*> postOp
                    <*> pType
         <|> pm <$> preOp <*> pMorphism <*> postOp
                    <*> pType
     where 
     pType = hm <$ pSpec '[' <*> pConcept <* pSpec ']'  
         <|> ht <$ pSpec '[' <*> pConcept <* pKey "*" <*> pConcept <* pSpec ']'
         `opt` Nothing
         where hm c    = Just (c,c)
               ht c1 c2 = Just (c1,c2)
     preOp = pList1 (pKey (showADL Cp))  `opt` []
     postOp = pList1 (pKey (showADL Co) <|> pKey (showADL K0) <|> pKey (showADL K1))  `opt` []
     --operators on e are evaluated: first post from the inside out, then pre (from the inside out) (convention p.50 Maddux).
     --(GM) Is this correct with respect to K0 and K1 (post) i.c.w. the complement (pre)?
     pe pre e post t = settype(construct [op|op<-pUnOp(pre++reverse post)])
         where
         settype (TPExp m _) = TPExp m t
         settype (MulPExp op xs _) = MulPExp op xs t
         settype (UnPExp op x _) = UnPExp op x t       
         construct [] = e
         construct (x:xs) = UnPExp x (construct xs) Nothing
     pUnOp ops = [op |opc<-ops,op<-[Cp,Co,K0,K1],opc==showADL op]
     pm pre m post t = pe pre (TPExp m mtp') post t'
         where 
         mtp' = if null pre && t==Nothing then mtp else Nothing
         t'   = if t==Nothing && mtp/=Nothing 
                then if (even.length) [()|Co<-pUnOp post] then mtp else fmap (\(x,y)->(y,x)) mtp 
                else t
         mtp = case mphats m of 
           [x] -> Just (x,x) 
           [x,y] -> Just (x,y) 
           _ -> Nothing
   pPExpression :: Parser Token (PExpression Morphism (Maybe Sign))
   pPExpression  = foldr pMultOp pPTerm [Re,Ri,Fu,Fi,Fd,Fc] --The order of these operators is relevant (convention p.50 Maddux).
     where 
     pMultOp mop pnext = let g [x]= x
                             g xs = MulPExp mop xs Nothing
                         in g <$> pList1Sep (pKey (showADL mop)) pnext   

   pExpr            :: Parser Token Expression
   pExpr             = f <$> pList1Sep (pKey "\\/") pFactorI
                       where f [x] = x
                             f  xs = Fux xs



   pFactorI         :: Parser Token Expression
   pFactorI          = f <$> pList1Sep (pKey "/\\") pFactor
                       where f [x] = x
                             f  xs = Fix xs



   pFactor          :: Parser Token Expression
   pFactor           = f <$> pList1Sep (pKey "!") pTermD
                       where f [t]     = t
                             f ts      = Fdx ts



   pTermD           :: Parser Token Expression
   pTermD            = f <$> pList1Sep (pKey ";") pTerm
                       where f [Tc f'] = f'
                             f [t]     = t
                             f ts      = F ts

   pTerm            :: Parser Token Expression
   pTerm             = tm <$> (preStr `opt` []) <*> pMorphism <*> (postStr `opt` [])                            <|>
                       tc <$> (preStr `opt` []) <*> (pSpec '(' *> pExpr <* pSpec ')') <*> (postStr `opt` [])
                       where
                        tm xs pm ys   = f (Tm pm (-1)) (xs++ys)
                        tc xs pc ys   = f pc (xs++ys)
                        f t ('~':xs) = flp (f t xs)
                        f t ('*':xs) = K0x (f t xs)
                        f t ('+':xs) = K1x (f t xs)
                        f t ('-':xs) = Cpx (f t xs)
                        f _ (_:_)    = error ("!Fatal (module CC 357). Consult your dealer!")
                        f t []       = t

   pMorphism        :: Parser Token Morphism
   pMorphism         = iden <$ pKey "I" <*> ((pSpec '[' *> pConcept <* pSpec ']') `opt` cptAnything)             <|>
                       v'   <$ pKey "V" <*> pTwo                                                                 <|>
                       rebuild <$> pVarid_val_pos <*> pTwo                                                       <|>
                       single  <$> pAtom 
                               <*> ((pSpec '[' *> pConcept <* pSpec ']') `opt` cptAnything)
                       where rebuild (nm,pos') atts = Mph nm pos' (take 2 (atts++atts)) (cptAnything,cptAnything) True
                                                      (Sgn nm cptAnything cptAnything [] [] "" "" "" [] Nowhere 0 (nm/="") False [] True)
                             single nm c = Mp1 nm                   -- mph1val
                                               [c|c/=Anything]      -- mphats 
                                               c                    -- mph1typ
                             iden a | a ==cptAnything = I [] cptAnything cptAnything True
                                    | otherwise       = I [c|c/=cptAnything] c c True where c=emp a
                             v' []                  = V [] (cptAnything, cptAnything)
                             v' [a]                 = V [c|c/=cptAnything] (c,c) where c=emp a
                             v' [a,b]               = V [c|c<-[emp a,emp b],c/=cptAnything] (emp a,emp b)
                             v' _  = error ("!Fatal (module CC 376): relation cannot have more than two concepts as type")
                             emp c | c == cptnew ""     = cptAnything
                                   | otherwise          = c
                             pTwo = (one' <$ pSpec '[' <*> pConcept <* pSpec ']'  <|>
                                     two  <$ pSpec '[' <*> pConcept <* pKey "*" <*> pConcept <* pSpec ']')
                                     `opt` []
                                    where one' c    = [c]
                                          two  c c' = [c,c']

   pConcept         :: Parser Token Concept
   pConcept          = (cptS <$ pKey "ONE") <|> (cptnew <$> (pConid <|> pString))
                      -- where c str = C str (==) []

-- BECAUSE:
--  (SJ) Waarom heeft een label (optioneel) strings?
--  (GM) Dit is bedoeld als binding mechanisme voor implementatiespecifieke (SQL/PHP plug,PHP web app,etc) properties
--  (SJ) Met het invoeren van referenties (t.b.v losse Explanations) bestaat er een variant met props en eentje zonder.
   pLabelProps      :: Parser Token Label
   pLabelProps       = lbl <$> pADLid_val_pos
                           <*> (pArgs `opt` [])
                           <*  pKey_pos ":"
                       where lbl :: (String, FilePos) -> [[String]] -> Label
                             lbl (nm,pos') strs = Lbl nm pos' strs
                             pArgs = pSpec '{' *> pList1Sep (pSpec ',') (pList1 pADLid) <* pSpec '}'


   pADLid           :: Parser Token String
   pADLid            = pVarid <|> pConid <|> pString

   pADLid_val_pos   :: Parser Token (String, FilePos)
   pADLid_val_pos    = pVarid_val_pos <|> pConid_val_pos <|> pString_val_pos

   pConceptDef      :: Parser Token ConceptDef
   pConceptDef       = Cd <$> pKey_pos "CONCEPT"
                          <*> (pConid <|> pString)   -- the concept name
                          <*> ((True <$ pKey "BYPLUG") `opt` False)
                          <*> pString                -- the definition text
                          <*> (pString `opt` "")     -- a reference to the source of this definition.


-- A key definition looks like:   KEY Person(name, address),
-- which means that name<>name~ /\ address<>addres~ |- I[Person].
-- You may also use an expression on each attribute place, for example: KEY onpassport: Person(nationality, passport;documentnr),
-- which means that nationality<>nationality~ /\ passport;documentnr<>(passport;documentnr)~ |- I[Person].
-- For the sake of a proper user interface, you can assign labels to the attributes in a key, for example:
-- KEY onSSN: Person("social security number":ssn)
   pKeyDef          :: Parser Token KeyDef
   pKeyDef           = kd <$ pKey "KEY" <*> pLabelProps <*> pConcept <* pSpec '(' <*> pList1Sep (pSpec ',') pKeyAtt <* pSpec ')'
                        where kd :: Label -> Concept -> ObjectDefs -> KeyDef 
                              kd (Lbl nm p _) c ats = Kd p nm c ats

   pKeyAtt          :: Parser Token ObjectDef
   pKeyAtt           = attL <$> pLabelProps <*> pExpr <|>
                       att <$> pExpr
                       where attL (Lbl nm p strs) attexpr = Obj nm p attexpr Nothing [] strs
                             att attexpr = Obj "" Nowhere attexpr Nothing [] []

   pRoleService     :: Parser Token RoleService
   pRoleService      = rs <$> pKey_pos "ROLE"               <*>
                              pList1Sep (pSpec ',') pString <*
                              pKey "USES"                   <*>
                              pList1Sep (pSpec ',') pString 
                       where rs p r s = RS r s p

   pRoleRelation    :: Parser Token RoleRelation
   pRoleRelation      = rr <$> pKey_pos "ROLE"              <*>
                              pList1Sep (pSpec ',') pString <*
                              pKey "EDITS"                  <*>
                              pList1Sep (pSpec ',') pMorphism
                       where rr p r m = RR r m p

   pSqlplug         :: Parser Token ObjectDef
   pSqlplug          = pKey_pos "SQLPLUG" *> pObj

   pPhpplug         :: Parser Token ObjectDef
   pPhpplug          = pKey_pos "PHPPLUG" *> pObj

   pService         :: Parser Token Service
   pService          = lbl <$> (pKey "SERVICE" *> pADLid_val_pos)    <*>
                               (pParams `opt` [])                    <*>
                               (pKey ":" *> pExpr)                   <*>       -- the context expression (mostly: I[c])
                               (pAttrs `opt` [])                               -- the subobjects
                       where lbl :: (String, FilePos) -> [Morphism] -> Expression -> [ObjectDef] -> Service
                             lbl (nm,p) params  expr ats
                              = Serv { svName   = nm
                                     , svParams = params
                                     , svObj    = Obj nm p expr Nothing ats []
                                     , svPos    = p
                                     }
                             pParams = pSpec '(' *> pList1Sep (pSpec ',') pMorphism <* pSpec ')' 
                             pAttrs  = pKey "=" *> pSpec '[' *> pListSep (pSpec ',') pObj <* pSpec ']'

   pObj             :: Parser Token ObjectDef
   pObj              = obj <$> pLabelProps
                           <*> pExpr                                             -- the context expression (mostly: I[c])
                           <*> (optional (pKey "ALWAYS" *> pProps') )            -- uni of tot of prop WHY (SJ) does this exist? It is not used, so for what future use is this intended?
                           <*> ((pKey "=" *> pSpec '[' *> pListSep (pSpec ',') pObj <* pSpec ']') `opt` [])  -- the subobjects
                       where obj (Lbl nm pos' strs) expr _ ats = Obj nm pos' expr Nothing ats strs


   optional :: (Sequence p, Alternative p) => p a -> p (Maybe a)
   optional a        = Just <$> a <|> pSucceed Nothing


   pDeclaration     :: Parser Token Declaration
   pDeclaration      = rebuild <$> pVarid 
                               <*> pKey_pos "::" 
                               <*> pConcept 
                               <*> (pKey "*" <|> pKey "->" ) 
                               <*> pConcept
                               <*> ((True <$ pKey "BYPLUG") `opt` False)
                               <*> (pProps `opt` [])
                               <*> ((True <$ pKey "BYPLUG") `opt` False)
                               <*> (pPragma `opt` [])
                               <*> ((pKey "=" *> pContent) `opt` []) <* pSpec '.'
                       where rebuild :: String
                                     -> FilePos
                                     -> Concept
                                     -> [Char]
                                     -> Concept
                                     -> Bool
                                     -> [Prop]
                                     -> Bool
                                     -> [String]
                                     -> Pairs
                                     -> Declaration
                             rebuild nm pos' s fun' t bp1 props bp2 pragma content
                               = Sgn nm s t props' props' (pr!!0) (pr!!1) (pr!!2) content pos' 0 False True [](bp1||bp2)
                                 where pr = pragma++["","",""]
                                       props'= rd props `uni` if fun'=="->" then [Uni,Tot] else []

   pContent         :: Parser Token Pairs
   pContent          = pSpec '[' *> pListSep (pKey ";") pRecord <* pSpec ']'

   -- | pProps is bedoeld voor gebruik in relatie-declaraties.
   pProps           :: Parser Token [Prop]
   pProps            = pSpec '['  *> pListSep (pSpec ',') pProp <* pSpec ']'

   pProp            :: Parser Token Prop
   pProp             = k Uni "UNI" <|> k Inj "INJ" <|> k Sur "SUR" <|> k Tot "TOT"
                       <|> k Sym "SYM" <|> k Asy "ASY" <|> k Trn "TRN" <|> k Rfx "RFX"
                       where k obj str = f <$> pKey str where f _ = obj

   -- | De pProps' is identiek aan pProps, maar werkt alleen op UNI en TOT. Ze is bedoeld voor de Service definities.

   pProps'          :: Parser Token [Prop]
   pProps'           = f <$> pList pProp'
                       where f :: [String] -> [Prop]
                             f ps = [k p | p<-ps, p/="PROP"]++[p' | p<-ps, p=="PROP", p'<-[Sym, Asy]]
                             k "TOT" = Tot
                             k "UNI" = Uni
                             k s = error ("!Fatal (module CC 499): Unknown property tag has been used: " ++ show s)

   pProp'           :: Parser Token String
   pProp'            = pKey "UNI" <|> pKey "TOT" <|> pKey "PROP"

   pPragma          :: Parser Token [String]
   pPragma           = pKey "PRAGMA" *> pList1 pString

   pRecord          :: Parser Token Paire
   pRecord           = mkPair<$ pSpec '(' <*> (trim <$> pString)  <* pComma   <*> (trim <$> pString)  <* pSpec ')'
                     --where trimpair (x,y) = (trim x,trim y)
                                
   get_tok_pos :: Token -> FilePos
   get_tok_pos     (Tok _ _ s l f) = FilePos (f,l,s)
   get_tok_val_pos :: Token -> (String, FilePos)
   get_tok_val_pos (Tok _ _ s l f) = (s,FilePos (f,l,s))



   gsym_pos :: IsParser p Token => TokenType -> String -> String -> p FilePos
   gsym_pos kind val' val2' = get_tok_pos <$> pSym (Tok kind val' val2' noPos "")

   gsym_val_pos :: IsParser p Token => TokenType -> String -> String -> p (String,FilePos)
   gsym_val_pos kind val' val2' = get_tok_val_pos <$> pSym (Tok kind val' val2' noPos "")

   pKey_pos :: String -> Parser Token FilePos
   pKey_pos  keyword  =   gsym_pos TkKeyword   keyword   keyword

   pString_val_pos, {- pAtom_val_pos, -}pVarid_val_pos, pConid_val_pos
                      ::  IsParser p Token => p (String,FilePos)
   pString_val_pos    =   gsym_val_pos TkString    ""        "?STR?"
   pVarid_val_pos     =   gsym_val_pos TkVarid     ""        "?LC?"
   pConid_val_pos     =   gsym_val_pos TkConid     ""        "?UC?"
                                

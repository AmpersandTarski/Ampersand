{-# OPTIONS_GHC -Wall #-}
 module CC (pArchitecture, keywordstxt, keywordsops, specialchars, opchars) where
   import UU_Scanner
   import UU_Parsing
   import Collection  (Collection(..))
   import Auxiliaries (sort)
   import Adl         
   import ShowADL     (showADL)
   import CC_aux      (pKey_pos ,pVarid_val_pos, pConid_val_pos, pString_val_pos )

   keywordstxt :: [String]
   keywordstxt       = [ "RULE", "CONTEXT", "ENDCONTEXT", "EXTENDS"
                       , "PATTERN", "ENDPATTERN"
                       , "SERVICE", "INITIAL", "SQLPLUG", "PHPPLUG"
                       , "POPULATION", "CONTAINS"
                       , "UNI", "INJ", "SUR", "TOT", "SYM", "ASY", "TRN", "RFX", "PROP"
                       , "ALWAYS", "RELATION", "CONCEPT", "KEY"
                       , "IMPORT", "GEN", "ISA", "I", "V", "S"
                       , "PRAGMA", "EXPLANATION", "SIGNAL", "ON", "COMPUTING", "INSERTING", "DELETING"
                       , "ONE", "BIND", "TOPHP", "BINDING"
                       ]
   keywordsops :: [String]
   keywordsops       = [ "-|", "|-", "-", "->", ">", "=", "~", "+", ";", "!", "*", "::", ":", "\\/", "/\\" ]
   specialchars :: String
   specialchars      = "()[].,{}"
   opchars :: String
   opchars           = rd (sort (concat keywordsops))





   pArchitecture    :: Bool -> Parser Token Architecture
   pArchitecture beep = Arch <$> pList1 (pContext beep)

   pBind             :: Parser Token (Declaration,String)
   pBind              = rebuild <$ pKey "BIND" <*> pDeclaration <* pKey "TOPHP" <*> (pConid <|> pString)
                       where rebuild d s = (d,s)

   pContext         :: Bool -> Parser Token Context
   pContext beep     = rebuild <$ pKey "CONTEXT" <*> pConid <*>
                                  ((rebexpr <$ pKey ":" <*> pExpr <*> 
                                  --     (pSpec '{' *> pList1Sep (pSpec ';') pBind <* pSpec '}')
                                       ((pKey "BINDING" *> pList1Sep (pSpec ',') pBind) `opt` [])
                                    ) `opt` universe) <*>
                                  ((pKey "EXTENDS" *> pList1Sep (pSpec ',') pConid) `opt` []) <*>
                                  pList (pContextElement beep) <* pKey "ENDCONTEXT"
                       where  
                       rebexpr x y = (x,y)
                       universe = (Tm$V [] (cptAnything,cptAnything),[]) --default: the universe
                       rebuild nm env on ces = Ctx nm on empty [] pats [] ds cs ks os pops sqlplugs phpplugs env
                              where
                               ps   = [p| CPat p<-ces]
                               ds   = [d| CDcl d<-ces]
                               cs   = [c| CCon c<-ces]
                               ks   = [k| CKey k<-ces]
                               os   = [o| CObj o<-ces]
                               pops = [Popu mph prs| CPop mph prs<-ces]
                               pats = ps++[Pat "CONTEXT" [] [] ds cs ks| not (null ds && null cs && null ks)]
                               sqlplugs = [plug| CSqlPlug plug<-ces]
                               phpplugs = [plug| CPhpPlug plug<-ces]

   data ContextElement = CPat Pattern
                       | CDcl Declaration
                       | CCon ConceptDef
                       | CKey KeyDef
                       | CObj ObjectDef
                       | CPop Morphism Pairs
                       | CSqlPlug ObjectDef
                       | CPhpPlug ObjectDef

   pContextElement  :: Bool -> Parser Token ContextElement
   pContextElement beep = CPat <$> pPattern beep <|>
                          CDcl <$> pDeclaration  <|>
                          CCon <$> pConceptDef   <|>
                          CKey <$> pKeyDef       <|>
                          CObj <$> pObjDef       <|>
                          CSqlPlug<$> pSqlplug      <|>
                          CPhpPlug<$> pPhpplug      <|>
                          CPop <$ pKey "POPULATION" <*> pMorphism <* pKey "CONTAINS" <*> pContent

   pPattern         :: Bool -> Parser Token Pattern
   pPattern beep     = rebuild <$ pKey "PATTERN" <*> (pConid <|> pString)
                               <*> pList (pPatElem beep)
                               <* pKey "ENDPATTERN"
                       where
                         rebuild :: String -> [PatElem] -> Pattern
                         rebuild nm pes = Pat nm [r{r_pat=nm}|Pr r<-pes] [gen{genpat=nm} |Pg gen<-pes] [mph{decpat=nm}| Pm mph@(Sgn{})<-pes] [c| Pc c<-pes] [k| Pk k<-pes]

   data PatElem      = Pr Rule
                     | Pg Gen
                     | Pm Declaration
                     | Pc ConceptDef
                     | Pk KeyDef

   pPatElem         :: Bool -> Parser Token PatElem
   pPatElem beep     = Pr <$> pRule beep   <|>
                       Pg <$> pGen         <|>
                       Pm <$> pDeclaration <|>
                       Pc <$> pConceptDef  <|>
                       Pk <$> pKeyDef

   pSignal          :: Parser Token Morphism
   pSignal           = ( pKey "SIGNAL" *> pMorphism <* pKey "ON" ) `opt` (Mph "" Nowhere [] (cptAnything,cptAnything) True (Sgn "" cptAnything cptAnything [] "" "" "" [] "" Nowhere 0 False []))





   pRule            :: Bool -> Parser Token Rule
   pRule beep        = hc <$> pSignal <*> pExpr <*> pKey_pos "|-" <*> pExpr <*> pComputing <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
                       kc <$> pSignal <*> pExpr <*> pKey_pos "-|" <*> pExpr <*> pComputing <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
                       dc <$> pSignal <*> pExpr <*> pKey_pos "="  <*> pExpr <*> pComputing <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
                       ac <$> pSignal <*> pKey_pos "RULE"         <*> pExpr <*> pComputing <*> ((pKey "EXPLANATION" *> pString) `opt` [])
                       where
                        hc m' antc pos' cons cpu' expl
                         | not beep && name m'=="" = Ru Implication antc pos' cons expl (cptAnything,cptAnything) Nothing 0 "" True
                         | otherwise  = Sg pos' (Ru Implication antc pos' cons expl (cptAnything,cptAnything) Nothing 0 "" True) expl (cptAnything,cptAnything) 0 "" (Sgn (name m') cptAnything cptAnything [] "" "" "" [] expl pos' 0 True [])
                        kc m' cons pos' antc cpu' expl = hc m' antc pos' cons cpu' expl
                        dc m' defd pos' expr cpu' expl
   {- diagnosis          | (\(FilePos (_,Pos l c,_))->l==diagl && c>diagc) pos' = error ("Diag: "++showADL (Ru 'E' defd pos' expr cpu' expl (cptAnything,cptAnything) 0 "" True))  -}
                         | not beep && name m'=="" = Ru Equivalence defd pos' expr expl (cptAnything,cptAnything) Nothing 0 "" True
                         | otherwise  = Sg pos' (Ru Equivalence defd pos' expr expl (cptAnything,cptAnything) Nothing 0 "" True) expl (cptAnything,cptAnything) 0 "" (Sgn (name m') cptAnything cptAnything [] "" "" "" [] "" pos' 0 True [])
                        ac m'      pos' expr cpu' expl
                         | not beep && name m'=="" = Ru Truth defd pos' expr expl (cptAnything,cptAnything) Nothing 0 "" True
                         | otherwise  = Sg pos' (Ru Truth defd pos' expr expl (cptAnything,cptAnything) Nothing 0 "" True) expl (cptAnything,cptAnything) 0 "" (Sgn (name m') cptAnything cptAnything [] "" "" "" [] "" pos' 0 True [])
                         where defd=error ("defd undefined in CC.lhs in pRule "++showADL expr)

--   data PCompu       = Uc [Morphism]
--                     | Ui [Morphism]
--                     | Ud [Morphism]
--                     | Un

   pComputing       :: Parser Token Expressions
   pComputing        = (f <$ pKey "COMPUTING" <*> pList1Sep (pSpec ',') (pExpr) {- <|>
                        f <$ pKey "INSERTING" <*> pList1Sep (pSpec ',') (pExpr) <|>
                        f <$ pKey "DELETING" <*> pList1Sep (pSpec ',') (pExpr) -} ) `opt` []
                       where f ms = ms

   pGen             :: Parser Token Gen
   pGen              = rebuild <$ pKey "GEN" <*> (pConid <|> pString) <*> pKey_pos "ISA" <*> (pConid <|> pString)
                       where rebuild spec pos' genus = G pos' (cptnew genus ) (cptnew spec ) []

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



   pExpr            :: Parser Token Expression
   pExpr             = f <$> pList1Sep (pKey "\\/") pFactorI
                       where f [x] = x
                             f  xs = Fu xs



   pFactorI         :: Parser Token Expression
   pFactorI          = f <$> pList1Sep (pKey "/\\") pFactor
                       where f [x] = x
                             f  xs = Fi xs



   pFactor          :: Parser Token Expression
   pFactor           = f <$> pList1Sep (pKey "!") pTermD
                       where f [t]     = t
                             f ts      = Fd ts



   pTermD           :: Parser Token Expression
   pTermD            = f <$> pList1Sep (pKey ";") pTerm
                       where f [Tc f'] = f'
                             f [t]     = t
                             f ts      = F ts

   pTerm            :: Parser Token Expression
   pTerm             = tm <$> (preStr `opt` []) <*> pMorphism <*> (postStr `opt` [])                            <|>
                       tc <$> (preStr `opt` []) <*> (pSpec '(' *> pExpr <* pSpec ')') <*> (postStr `opt` [])
                       where
                        tm xs pm ys   = f (Tm pm) (xs++ys)
                        tc xs pc ys   = f pc (xs++ys)
                        f t ('~':xs) = flp (f t xs)
                        f t ('*':xs) = K0 (f t xs)
                        f t ('+':xs) = K1 (f t xs)
                        f t ('-':xs) = Cp (f t xs)
                        f _ (_:_)   = undefined     -- WAAROM? Stef, waarom ontbrak dit? Is dat vergeten? TODO Deze match is toegevoegd om de warning kwijt te raken. Maar is dit ook op deze manier bedoeld?
                        f t []       = t

   pMorphism        :: Parser Token Morphism
   pMorphism         = iden <$ pKey "I" <*> ((pSpec '[' *> pConcept <* pSpec ']') `opt` cptAnything)                <|>
                       v'   <$ pKey "V" <*> pTwo                                                                 <|>
                       rebuild <$> pVarid_val_pos <*> pTwo
                       where rebuild (nm,pos') atts = Mph nm pos' (take 2 (atts++atts)) (cptAnything,cptAnything) True
                                                      (Sgn nm cptAnything cptAnything [] "" "" "" [] "" Nowhere 0 (nm/="") [])
                             iden a | a ==cptAnything = I [] cptAnything cptAnything True
                                    | otherwise       = I [c|c/=cptAnything] c c True where c=emp a
                             v' []                  = V [] (cptAnything, cptAnything)
                             v' [a]                 = V [c|c/=cptAnything] (c,c) where c=emp a
                             v' [a,b]               = V [c|c<-[emp a,emp b],c/=cptAnything] (emp a,emp b)
                             v' _  = error ("!Fatal (module CC 205): relation cannot have more than two concepts as type")
                             emp c | c == cptnew ""     = cptAnything
                                   | otherwise          = c
                             pTwo = (one' <$ pSpec '[' <*> pConcept <* pSpec ']'  <|>
                                     two  <$ pSpec '[' <*> pConcept <* pKey "*" <*> pConcept <* pSpec ']')
                                     `opt` []
                                    where one' c    = [c]
                                          two  c c' = [c,c']

   pConcept         :: Parser Token Concept
   pConcept          = (cptS <$ (pKey "ONE")) <|> (cptnew <$> (pConid <|> pString))
                      -- where c str = C str (==) []

-- WAAROM (SJ) heeft een label (optioneel) strings?
-- ANTWOORD (GM) omdat we nog geen fatsoenlijk binding mechanisme hebben voor implementatiespecifieke (SQL/PHP plug,PHP web app,etc) properties
   pLabel           :: Parser Token Label
   pLabel            = lbl <$> (pVarid_val_pos <|> pConid_val_pos <|> pString_val_pos)
                           <*> ((pSpec '{' *> pList1Sep (pSpec ',') (pList1 phpId) <* pSpec '}') `opt` [])
                           <*  pKey_pos ":"
                       where lbl :: (String, FilePos) -> [[String]] -> Label
                             lbl (nm,pos') strs = Lbl nm pos' strs

   phpId            :: Parser Token String
   phpId             = pVarid <|> pConid <|> pString

   pConceptDef      :: Parser Token ConceptDef
   pConceptDef       = Cd <$> pKey_pos "CONCEPT" <*> (pConid <|> pString) <*> pString <*> (pString `opt` "")


-- A key definition normally looks like:   KEY onaddress: Person(name, address),
-- which means that name<>name~ /\ address<>addres~ |- I[Person].
-- You may also use an expression on each attribute place, for example: KEY onpassport: Person(nationality, passport;documentnr),
-- which means that nationality<>nationality~ /\ passport;documentnr<>(passport;documentnr)~ |- I[Person].
-- For the sake of a proper user interface, you can assign labels to the attributes in a key, for example:
-- KEY onSSN: Person("social security number":ssn)
   pKeyDef          :: Parser Token KeyDef
   pKeyDef           = kd <$ pKey "KEY" <*> pLabel <*> pConcept <* pSpec '(' <*> pList1Sep (pSpec ',') pKeyAtt <* pSpec ')'
                        where kd :: Label -> Concept -> ObjectDefs -> KeyDef 
                              kd (Lbl nm p _) c ats = Kd p nm (Tm $ mIs c) ats

   pKeyAtt          :: Parser Token ObjectDef
   pKeyAtt           = attL <$> pLabel <*> pExpr <|>
                       att <$> pExpr
                       where attL (Lbl nm p strs) attexpr = Obj nm p attexpr [] strs
                             att attexpr = Obj "" Nowhere attexpr [] []

   pObjDef          :: Parser Token ObjectDef
   pObjDef           = pKey_pos "SERVICE" *> pObj

   pSqlplug          :: Parser Token ObjectDef
   pSqlplug           = pKey_pos "SQLPLUG" *> pObj

   pPhpplug          :: Parser Token ObjectDef
   pPhpplug           = pKey_pos "PHPPLUG" *> pObj



   optional :: (Sequence p, Alternative p) => p a -> p (Maybe a)
   optional a        = Just <$> a <|> pSucceed Nothing

   pObj             :: Parser Token ObjectDef
   pObj              = obj <$> pLabel
                           <*> pExpr                                             -- de contextexpressie (default: I[c])
                           <*> (optional (pKey "ALWAYS" *> pProps') )            -- uni of tot of prop
                           <*> ((pKey "=" *> pSpec '[' *> pListSep (pSpec ',') pObj <* pSpec ']') `opt` [])  -- de subobjecten
                       where obj (Lbl nm pos' strs) expr _ ats = Obj nm pos' expr ats strs

   pDeclaration     :: Parser Token Declaration
   pDeclaration      = rebuild <$> pVarid 
                               <*> pKey_pos "::" 
                               <*> pConcept 
                               <*> (pKey "*" <|> pKey "->" ) 
                               <*> pConcept
                               <*> (pProps `opt` []) <*> (pPragma `opt` [])
                               <*> ((pKey "EXPLANATION" *> pString ) `opt` [])
                               <*> ((pKey "=" *> pContent) `opt` []) <* pSpec '.'
                       where rebuild :: String
                                     -> FilePos
                                     -> Concept
                                     -> [Char]
                                     -> Concept
                                     -> [Prop]
                                     -> [String]
                                     -> String
                                     -> Pairs
                                     -> Declaration
                             rebuild nm pos' s fun' t props pragma expla content
                               = Sgn nm s t (rd props `uni` if fun'=="->" then [Uni,Tot] else []) (pr!!0) (pr!!1) (pr!!2) content expla pos' 0 False []
                                 where pr = pragma++["","",""]

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
                             k _ = undefined  --TODO : Moet nog een foutmelding, dat er een onbekende tag is gebruikt.

   pProp'           :: Parser Token String
   pProp'            = pKey "UNI" <|> pKey "TOT" <|> pKey "PROP"

   pPragma          :: Parser Token [String]
   pPragma           = pKey "PRAGMA" *> pList1 pString

   pRecord          :: Parser Token Paire
   pRecord           = mkPaire<$ pSpec '(' <*> pString  <* pComma   <*> pString  <* pSpec ')'
                                
                                

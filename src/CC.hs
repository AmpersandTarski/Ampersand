{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS -XFlexibleContexts #-}
 module CC (pArchitecture, keywordstxt, keywordsops, specialchars, opchars) where
   import UU_Scanner  ( Token(..),TokenType(..),noPos
                      , pKey,pConid,pString,pSpec,pAtom,pVarid,pComma)
   import UU_Parsing  (Parser
                      , (<$>) , (<$), (<*>), (<*) , (*>), (<|>)
                      ,pList,pListSep,pList1,pList1Sep,pSym
                      ,pSucceed
                      ,opt, Sequence,Alternative, IsParser
                      )
   import Collection  (Collection(..))
   import Auxiliaries (sort)
   import Adl         
   import ShowADL     (showADL)

   keywordstxt :: [String]
   keywordstxt       = [ "CONTEXT", "ENDCONTEXT", "EXTENDS"
                       , "PATTERN", "ENDPATTERN"
                       , "SERVICE", "INITIAL", "SQLPLUG", "PHPPLUG"
                       , "POPULATION", "CONTAINS"
                       , "UNI", "INJ", "SUR", "TOT", "SYM", "ASY", "TRN", "RFX", "PROP", "ALWAYS"
                       , "RULE", "MAINTAINS", "SIGNALS", "SIGNAL", "ON"
                       , "RELATION", "CONCEPT", "KEY"
                       , "IMPORT", "GEN", "ISA", "I", "V", "S"
                       , "PRAGMA", "EXPLANATION"
                       , "ONE", "BIND", "TOPHP", "BINDING"
                       ]
   keywordsops :: [String]
   keywordsops       = [ "-|", "|-", "-", "->", ">", "=", "~", "+", ";", "!", "*", "::", ":", "\\/", "/\\" ]
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

   pContextElement        :: Parser Token ContextElement
   pContextElement = CPat <$> pPattern         <|>
                     CDcl <$> pDeclaration     <|>
                     CCon <$> pConceptDef      <|>
                     CKey <$> pKeyDef          <|>
                     CObj <$> pObjDef          <|>
                     CSqlPlug<$> pSqlplug      <|>
                     CPhpPlug<$> pPhpplug      <|>
                     CPop <$ pKey "POPULATION" <*> pMorphism <* pKey "CONTAINS" <*> pContent

   pPattern         :: Parser Token Pattern
   pPattern  = rebuild <$  pKey "PATTERN" <*> (pConid <|> pString)
                       <*> pList pPatElem
                       <*  pKey "ENDPATTERN"
                       where
                         rebuild :: String -> [PatElem] -> Pattern
                         rebuild nm pes = Pat nm [r{r_pat=nm}|Pr r<-pes] [gen{genpat=nm} |Pg gen<-pes] [mph{decpat=nm}| Pm mph@(Sgn{})<-pes] [c| Pc c<-pes] [k| Pk k<-pes]

   data PatElem      = Pr Rule
                     | Pg Gen
                     | Pm Declaration
                     | Pc ConceptDef
                     | Pk KeyDef

   pPatElem         :: Parser Token PatElem
   pPatElem          = Pr <$> pRule <|>
                       Pg <$> pGen          <|>
                       Pm <$> pDeclaration  <|>
                       Pc <$> pConceptDef   <|>
                       Pk <$> pKeyDef

   pSignal          :: Parser Token Morphism
   pSignal           = pKey "SIGNAL" *> pMorphism <* pKey "ON"       <|>
                         pKey "RULE" *> pMorphism <* pKey "SIGNALS"
   pAlways          :: Parser Token Morphism
   pAlways           = ( pKey "RULE" *> pMorphism <* pKey "MAINTAINS" ) `opt` (Mph "" Nowhere [] (cptAnything,cptAnything) True (Sgn "" cptAnything cptAnything [] "" "" "" [] "" Nowhere 0 False False ""))

   pRule            :: Parser Token Rule
   pRule             = hc True  <$> pSignal <*> pExpr <*> pKey_pos "|-" <*> pExpr <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
                       kc True  <$> pSignal <*> pExpr <*> pKey_pos "-|" <*> pExpr <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
                       dc True  <$> pSignal <*> pExpr <*> pKey_pos "="  <*> pExpr <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
                       ac True  <$> pSignal <*>                             pExpr <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
                       hc False <$> pAlways <*> pExpr <*> pKey_pos "|-" <*> pExpr <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
                       kc False <$> pAlways <*> pExpr <*> pKey_pos "-|" <*> pExpr <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
                       dc False <$> pAlways <*> pExpr <*> pKey_pos "="  <*> pExpr <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
                       ac False <$> pAlways <*>                             pExpr <*> ((pKey "EXPLANATION" *> pString) `opt` [])
                       where
                        hc isSg m' antc pos' cons expl
                          = Ru Implication antc pos' cons expl (cptAnything,cptAnything) Nothing 0 "" True isSg (Sgn (name m') cptAnything cptAnything [] "" "" "" [] "" pos' 0 isSg False "")
                        kc isSg m' cons pos' antc expl = hc isSg m' antc pos' cons expl
                        dc isSg m' defd pos' expr expl
                          = Ru Equivalence defd pos' expr expl (cptAnything,cptAnything) Nothing 0 "" True isSg (Sgn (name m') cptAnything cptAnything [] "" "" "" [] "" pos' 0 isSg False "")
                        ac isSg m' expr expl
                          = Ru Truth defd (Adl.pos m') expr expl (cptAnything,cptAnything) Nothing 0 "" True isSg (Sgn (name m') cptAnything cptAnything [] "" "" "" [] "" (Adl.pos m') 0 isSg False "")
                         where defd=error ("!Fatal (module CC 127): defd undefined in pRule "++showADL expr)

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
                        f _ (_:_)    = error ("!Fatal (module CC 189). Consult your dealer!")
                        f t []       = t

   pMorphism        :: Parser Token Morphism
   pMorphism         = iden <$ pKey "I" <*> ((pSpec '[' *> pConcept <* pSpec ']') `opt` cptAnything)             <|>
                       v'   <$ pKey "V" <*> pTwo                                                                 <|>
                       rebuild <$> pVarid_val_pos <*> pTwo                                                       <|>
                       single  <$> pAtom 
                               <*> ((pSpec '[' *> pConcept <* pSpec ']') `opt` cptAnything)
                       where rebuild (nm,pos') atts = Mph nm pos' (take 2 (atts++atts)) (cptAnything,cptAnything) True
                                                      (Sgn nm cptAnything cptAnything [] "" "" "" [] "" Nowhere 0 (nm/="") False [])
                             single nm c = Mp1 nm                   -- mph1val
                                               [c|c/=Anything]      -- mphats 
                                               c                    -- mph1typ
                             iden a | a ==cptAnything = I [] cptAnything cptAnything True
                                    | otherwise       = I [c|c/=cptAnything] c c True where c=emp a
                             v' []                  = V [] (cptAnything, cptAnything)
                             v' [a]                 = V [c|c/=cptAnything] (c,c) where c=emp a
                             v' [a,b]               = V [c|c<-[emp a,emp b],c/=cptAnything] (emp a,emp b)
                             v' _  = error ("!Fatal (module CC 216): relation cannot have more than two concepts as type")
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
-- DAAROM (GM) omdat we nog geen fatsoenlijk binding mechanisme hebben voor implementatiespecifieke (SQL/PHP plug,PHP web app,etc) properties
   pLabel           :: Parser Token Label
   pLabel            = lbl <$> (pVarid_val_pos <|> pConid_val_pos <|> pString_val_pos)
                           <*> ((pSpec '{' *> pList1Sep (pSpec ',') (pList1 phpId) <* pSpec '}') `opt` [])
                           <*  pKey_pos ":"
                       where lbl :: (String, FilePos) -> [[String]] -> Label
                             lbl (nm,pos') strs = Lbl nm pos' strs

   phpId            :: Parser Token String
   phpId             = pVarid <|> pConid <|> pString

   pConceptDef      :: Parser Token ConceptDef
   pConceptDef       = Cd <$> pKey_pos "CONCEPT"
                          <*> (pConid <|> pString)   -- the concept name
                          <*> pString                -- the definition text
                          <*> (pString `opt` "")     -- a reference to the source of this definition.


-- A key definition looks like:   KEY Person(name, address),
-- which means that name<>name~ /\ address<>addres~ |- I[Person].
-- You may also use an expression on each attribute place, for example: KEY onpassport: Person(nationality, passport;documentnr),
-- which means that nationality<>nationality~ /\ passport;documentnr<>(passport;documentnr)~ |- I[Person].
-- For the sake of a proper user interface, you can assign labels to the attributes in a key, for example:
-- KEY onSSN: Person("social security number":ssn)
   pKeyDef          :: Parser Token KeyDef
   pKeyDef           = kd <$ pKey "KEY" <*> pLabel <*> pConcept <* pSpec '(' <*> pList1Sep (pSpec ',') pKeyAtt <* pSpec ')'
                        where kd :: Label -> Concept -> ObjectDefs -> KeyDef 
                              kd (Lbl nm p _) c ats = Kd p nm c ats

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
                               = Sgn nm s t (rd props `uni` if fun'=="->" then [Uni,Tot] else []) (pr!!0) (pr!!1) (pr!!2) content expla pos' 0 False True []
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
                             k s = error ("!Fatal (module CC 316): Unknown property tag has been used: " ++ show s)

   pProp'           :: Parser Token String
   pProp'            = pKey "UNI" <|> pKey "TOT" <|> pKey "PROP"

   pPragma          :: Parser Token [String]
   pPragma           = pKey "PRAGMA" *> pList1 pString

   pRecord          :: Parser Token Paire
   pRecord           = mkPair<$ pSpec '(' <*> pString  <* pComma   <*> pString  <* pSpec ')'
                                
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
                                

> module CC (pArchitecture, keywordstxt, keywordsops, specialchars, opchars) where
>  import UU_Scanner
>  import UU_Parsing
>  import CommonClasses ( Identified(name))
>  import Collection (Collection(empty,uni,(>-),rd))
>  import Auxiliaries (sort, upCap)
>  import ADLdef ( Architecture(..)
>                , Context(..)
>                , Concept,cptAnything,cptC, cptS, cptnew
>                )
>  import CC_aux 
>            ( -- Architecture(Arch), 
>              -- Context(Ctx)
>              FilePos(FilePos)
>            , Pattern(Pat)
>            , Declaration(Sgn)
>            , ConceptDef(Cd)
>            , ObjectDef(Obj), ObjDefs, KeyDef(Kd), KeyDefs, Population(Popu), Populations
>            , Rule(Ru,Sg,Gc)
>            , Gen(G)
>            , Pairs
>            , Morphism(Mph,I,V)
>            , Morphical( concs, conceptDefs, mors, morlist, declarations, genE, closExprs, objDefs, keyDefs )
>         --   , Concept(Anything,C,S)
>            , Prop(Uni,Tot,Inj,Sur,Sym,Asy,Trn,Rfx,Aut)
>            , Expressions
>            , Expression(Fu,Fi,Fd,Tc,F,Tm,K0,K1,Cp)
>            , posNone
>            , pKey_pos
>            , showADL
>            , flp
>            , pVarid_val_pos, pConid_val_pos
>            , mIs, v)

>  diagl = 52
>  diagc = 0

TODO: Declaraties buiten Patterns niet meer toestaan.
(Experiment: sta het tijdelijk toe om fouten uit RelBinGen te halen.)

TODO: afdwingen dat alle morphismen binnen een pattern worden gedeclareerd.

VERSION, AUTHOR, PURPOSE, STAKEHOLDER

>  keywordstxt       = [ "RULE", "CONTEXT", "ENDCONTEXT", "EXTENDS"
>                      , "PATTERN", "ENDPATTERN"
>                      , "VIEW", "INITIAL"
>                      , "POPULATION", "CONTAINS"
>                      , "UNI", "INJ", "SUR", "TOT", "SYM", "ASY", "TRN", "RFX"
>                      , "RELATION", "CONCEPT", "KEY"
>                      , "IMPORT", "GEN", "ISA", "I", "V"
>                      , "PRAGMA", "EXPLANATION", "SIGNAL", "ON", "COMPUTING", "INSERTING", "DELETING"
>                      , "ONE"
>                      ]
>  keywordsops       = [ "-|", "|-", ":-", "-:", "-", "->", ">", "=", "~", "+", ";", "!", "*", "::", ":", "\\/", "/\\" ]
>  specialchars      = "()[].,"
>  opchars           = rd (sort (concat keywordsops))

If beep==True, then all rules are to be interpreted as beep-rules.
No automatic computation rules will be derived.
This will be achieved by generating signal rules only.

>  pArchitecture    :: Bool -> Parser Token Architecture
>  pArchitecture beep = ZZZArch <$> pList1 (pContext beep)

>  pContext         :: Bool -> Parser Token Context
>  pContext beep     = rebuild <$ pKey "CONTEXT" <*> pConid <*>
>                                 ((pKey "EXTENDS" *> pList1Sep (pSpec ',') pConid) `opt` []) <*>
>                                 pList (pContextElement beep) <* pKey "ENDCONTEXT"
>                      where rebuild nm on ces = ZZZCtx nm on empty [] pats [] ds cs ks os pops
>                             where
>                              ps   = [p| CPat p<-ces]
>                              ds   = [d| CDcl d<-ces]
>                              cs   = [c| CCon c<-ces]
>                              ks   = [k| CKey k<-ces]
>                              os   = [o| CObj o<-ces]
>                              pops = [Popu m ps| CPop m ps<-ces]
>                              pats = ps++[Pat "CONTEXT" [] [] ds cs ks| not (null ds && null cs && null ks)]

>  data ContextElement = CPat Pattern
>                      | CDcl Declaration
>                      | CCon ConceptDef
>                      | CKey KeyDef
>                      | CObj ObjectDef
>                      | CPop Morphism Pairs

>  pContextElement  :: Bool -> Parser Token ContextElement
>  pContextElement beep = CPat <$> pPattern beep <|>
>                         CDcl <$> pDeclaration  <|>
>                         CCon <$> pConceptDef   <|>
>                         CKey <$> pKeyDef       <|>
>                         CObj <$> pObjDef       <|>
>                         CPop <$ pKey "POPULATION" <*> pMorphism <* pKey "CONTAINS" <*> pContent

>  pPattern         :: Bool -> Parser Token Pattern
>  pPattern beep     = rebuild <$ pKey "PATTERN" <*> (pConid <|> pString)
>                              <*> pList (pPatElem beep)
>                              <* pKey "ENDPATTERN"
>                      where
>                        rebuild nm pes = Pat nm [r|Pr r<-pes] [gen |Pg gen<-pes] [m| Pm m<-pes] [c| Pc c<-pes] [k| Pk k<-pes]

>  data PatElem      = Pr Rule
>                    | Pg Gen
>                    | Pm Declaration
>                    | Pc ConceptDef
>                    | Pk KeyDef

>  pPatElem         :: Bool -> Parser Token PatElem
>  pPatElem beep     = Pr <$> pRule beep   <|>
>                      Pg <$> pGen         <|>
>                      Pm <$> pDeclaration <|>
>                      Pc <$> pConceptDef  <|>
>                      Pk <$> pKeyDef

>  pSignal          :: Parser Token Morphism
>  pSignal           = ( pKey "SIGNAL" *> pMorphism <* pKey "ON" ) `opt` (Mph "" posNone [] (cptAnything,cptAnything) True (Sgn "" cptAnything cptAnything [] "" "" "" [] "" posNone 0 False))

For a beep-machine, all rules are interpreted as signals. For this reason, a boolean 'beep' is used.
Beep means that only signal rules are used, and no automatic computation is done. The most effective way
to achieve that is to arrange that in the parser: All Ru-rules are wrapped in Sg-rules and the cpu is kept empty.

>  pRule            :: Bool -> Parser Token Rule
>  pRule beep        = hc <$> pSignal <*> pExpr <*> pKey_pos "-:" <*> pExpr <*> pComputing <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
>                      kc <$> pSignal <*> pExpr <*> pKey_pos ":-" <*> pExpr <*> pComputing <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
>                      hc <$> pSignal <*> pExpr <*> pKey_pos "|-" <*> pExpr <*> pComputing <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
>                      kc <$> pSignal <*> pExpr <*> pKey_pos "-|" <*> pExpr <*> pComputing <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
>                      dc <$> pSignal <*> pExpr <*> pKey_pos "="  <*> pExpr <*> pComputing <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
>                      ac <$> pSignal <*> pKey_pos "RULE"         <*> pExpr <*> pComputing <*> ((pKey "EXPLANATION" *> pString) `opt` []) <|>
>                      gc <$> pSignal <*> pKey_pos "GLUE" <*> pMorphism <* pKey "=" <*> pExpr <*> pComputing 
>                      where
>                       hc m antc pos cons cpu expl
>                        | not beep && name m=="" = Ru 'I' antc pos cons (if beep then [] else cpu) expl (cptAnything,cptAnything) 0 ""
>                        | otherwise  = Sg pos (Ru 'I' antc pos cons (if beep then [] else cpu) expl (cptAnything,cptAnything) 0 "") expl (cptAnything,cptAnything) 0 "" (Sgn (name m) cptAnything cptAnything [] "" "" "" [] expl pos 0 True)
>                       kc m cons pos antc cpu expl = hc m antc pos cons cpu expl
>                       dc m defd pos expr cpu expl
>  {- diagnosis          | (\(FilePos (_,Pos l c,_))->l==diagl && c>diagc) pos = error ("Diag: "++showADL (Ru 'E' defd pos expr cpu expl (cptAnything,cptAnything) 0 ""))  -}
>                        | not beep && name m=="" = Ru 'E' defd pos expr (if beep then [] else cpu) expl (cptAnything,cptAnything) 0 ""
>                        | otherwise  = Sg pos (Ru 'E' defd pos expr (if beep then [] else cpu) expl (cptAnything,cptAnything) 0 "") expl (cptAnything,cptAnything) 0 "" (Sgn (name m) cptAnything cptAnything [] "" "" "" [] "" pos 0 True)
>                       ac m      pos expr cpu expl
>                        | not beep && name m=="" = Ru 'A' defd pos expr (if beep then [] else cpu) expl (cptAnything,cptAnything) 0 ""
>                        | otherwise  = Sg pos (Ru 'A' defd pos expr (if beep then [] else cpu) expl (cptAnything,cptAnything) 0 "") expl (cptAnything,cptAnything) 0 "" (Sgn (name m) cptAnything cptAnything [] "" "" "" [] "" pos 0 True)
>                        where defd=error ("defd undefined in CC.lhs in pRule "++showADL expr)
>                       gc m      pos pm pf cpu     = Gc pos pm pf (if beep then [] else cpu) (cptAnything,cptAnything) 0 ""

>  data PCompu       = Uc [Morphism]
>                    | Ui [Morphism]
>                    | Ud [Morphism]
>                    | Un

>  pComputing       :: Parser Token Expressions
>  pComputing        = (f <$ pKey "COMPUTING" <*> pList1Sep (pSpec ',') (pExpr) {- <|>
>                       f <$ pKey "INSERTING" <*> pList1Sep (pSpec ',') (pExpr) <|>
>                       f <$ pKey "DELETING" <*> pList1Sep (pSpec ',') (pExpr) -} ) `opt` []
>                      where f ms = ms

>  pGen             :: Parser Token Gen
>  pGen              = rebuild <$ pKey "GEN" <*> (pConid <|> pString) <*> pKey_pos "ISA" <*> (pConid <|> pString)
>                      where rebuild spec pos genus = G pos (cptnew genus ) (cptnew spec )

>  postStr          :: Parser Token String
>  postStr           = f <$> pList1 (pKey "~" <|> pKey "+" <|> pKey "-" <|> pKey "*")
>                      where
>                       f xs = g ['~'|'~'<-concat xs] ++ g ['-'|'-'<-concat xs] ++ eat [x|x<-concat xs,x/='~',x/='-']
>                       g xs = if odd (length xs) then take 1 xs else []

>  eat ('*':'*':xs) = eat ('*':xs)
>  eat ('+':'*':xs) = eat ('*':xs)
>  eat ('*':'+':xs) = eat ('*':xs)
>  eat ('+':'+':xs) = eat ('+':xs)
>  eat (x:xs)       = x:eat xs
>  eat []           = []

>  preStr          :: Parser Token String
>  preStr           = g <$> pList1 (pKey "-")
>                      where
>                       g xs = if odd (length cs) then take 1 cs else [] where cs = concat xs

There are always two or more factors in a factorU.

>  pExpr            :: Parser Token Expression
>  pExpr             = f <$> pList1Sep (pKey "\\/") pFactorI
>                      where f [x] = x
>                            f  xs = Fu xs

There are always two or more factors in a factorU.

>  pFactorI         :: Parser Token Expression
>  pFactorI          = f <$> pList1Sep (pKey "/\\") pFactor
>                      where f [x] = x
>                            f  xs = Fi xs

There are always two or more terms in a factor. F [] cannot occur

>  pFactor          :: Parser Token Expression
>  pFactor           = f <$> pList1Sep (pKey "!") pTermD
>                      where f [t]     = t
>                            f ts      = Fd ts

There are always one or more terms in a factor. F [] cannot occur

>  pTermD           :: Parser Token Expression
>  pTermD            = f <$> pList1Sep (pKey ";") pTerm
>                      where f [Tc f'] = f'
>                            f [t]     = t
>                            f ts      = F ts

>  pTerm            :: Parser Token Expression
>  pTerm             = tm <$> (preStr `opt` []) <*> pMorphism <*> (postStr `opt` [])                            <|>
>                      tc <$> (preStr `opt` []) <*> (pSpec '(' *> pExpr <* pSpec ')') <*> (postStr `opt` [])
>                      where
>                       tm xs pm ys   = f (Tm pm) (xs++ys)
>                       tc xs pc ys   = f pc (xs++ys)
>                       f t ('~':xs) = flp (f t xs)
>                       f t ('*':xs) = K0 (f t xs)
>                       f t ('+':xs) = K1 (f t xs)
>                       f t ('-':xs) = Cp (f t xs)
>                       f t []       = t

>  pMorphism        :: Parser Token Morphism
>  pMorphism         = iden <$ pKey "I" <*> ((pSpec '[' *> pConcept <* pSpec ']') `opt` cptAnything)                <|>
>                      v    <$ pKey "V" <*> pTwo                                                                 <|>
>                      rebuild <$> pVarid_val_pos <*> pTwo
>                      where rebuild (nm,pos) atts = Mph nm pos (take 2 (atts++atts)) (cptAnything,cptAnything) True
>                                                     (Sgn nm cptAnything cptAnything [] "" "" "" [] "" posNone 0 (nm/=""))
>                            iden cptAnything      = I [] cptAnything cptAnything True
>                            iden a                = I [c|c/=cptAnything] c c True where c=emp a
>                            v []                  = V [] (cptAnything, cptAnything)
>                            v [a]                 = V [c|c/=cptAnything] (c,c) where c=emp a
>                            v [a,b]               = V [c|c<-[emp a,emp b],c/=cptAnything] (emp a,emp b)
>                            emp c | c == cptnew ""     = cptAnything
>                                  | otherwise          = c
>                            pTwo = (one <$ pSpec '[' <*> pConcept <* pSpec ']'  <|>
>                                    two <$ pSpec '[' <*> pConcept <* pKey "*" <*> pConcept <* pSpec ']')
>                                    `opt` []
>                                   where one c    = [c]
>                                         two c c' = [c,c']

>  pConcept         :: Parser Token Concept
>  pConcept          = (cptS <$ (pKey "ONE")) <|> (cptnew <$> (pConid <|> pString))
>                     -- where c str = C str (==) []

>  pLabel           :: Parser Token (String, FilePos)
>  pLabel            = (phpId <* pKey ">" ) `opt` ("", posNone)

>  phpId            :: Parser Token (String, FilePos)
>  phpId             = pVarid_val_pos <|> pConid_val_pos

>  pConceptDef      :: Parser Token ConceptDef
>  pConceptDef       = Cd <$> pKey_pos "CONCEPT" <*> (pConid <|> pString) <*> pString <*> (pString `opt` "")

>  pKeyDef          :: Parser Token KeyDef
>  pKeyDef           = kd <$ pKey "KEY" <*> pLabel <* pSpec ':' <*> pExpr <* pSpec '[' <*> pList1Sep (pSpec ',') pAtt <* pSpec ']'
>                       where kd (nm,pos) e ats = Kd pos nm e ats

>  pObjDef          :: Parser Token ObjectDef
>  pObjDef           = pKey_pos "VIEW" *> pObj

Bas: het volgende is nog incorrect....
 -- justPars :: (Parser Token anytype) -> Token -> anytype -> Parser Token (Maybe anytype)
  justPars parser token expr = parser token (Just expr)

>  optional a        = Just <$> a <|> pSucceed Nothing

>  pObj             :: Parser Token ObjectDef
>  pObj              = obj <$> pConid_val_pos                                    -- de naam van het object
>                          <*> (optional (pSpec '[' *> pConid <* pSpec ']') )    -- optioneel: het type van het object (een concept)
>                          <*> (optional (pKey ":" *> pExpr) )                   -- de contextexpressie (default: I[c])
>                          <*> ((pKey "=" *> pSpec '[' *> pListSep (pSpec ',') pObj <* pSpec ']') `opt` [])  -- de subobjecten
>                      <|>
>                      vbj <$> pVarid_val_pos                                     -- de naam van het object
>                          <*> (optional (pSpec '[' *>  pConid <* pSpec ']') )    -- optioneel: het type van het object (een concept)
>                          <*> (optional (pKey ":" *>  pExpr) )                   -- de contextexpressie (default: I[c])
>                          <*> ((pKey "=" *> pSpec '[' *> pListSep (pSpec ',') pObj <* pSpec ']') `opt` [])  -- de subobjecten
>                      where obj (nm,pos) Nothing  Nothing  ats = Obj nm pos (v (cptAnything, cptnew (nm))) ats
>                            obj (nm,pos) Nothing  (Just e) ats = Obj nm pos e ats
>                            obj (nm,pos) (Just c) Nothing  ats = Obj nm pos (v (cptAnything, cptnew c )) ats
>                            obj (nm,pos) (Just c) (Just e) ats = Obj nm pos (F[e,Tm (mIs (cptnew c ))]) ats
>                            vbj (nm,pos) Nothing  Nothing  ats = Obj nm pos (Tm (Mph nm pos [] (cptAnything,cptAnything) True (error "CC.lhs: vbj (nm,pos) Nothing Nothing has no declaration"))) ats
>                            vbj (nm,pos) Nothing  (Just e) ats = Obj nm pos e ats
>                            vbj (nm,pos) (Just c) Nothing  ats = Obj nm pos (Tm (Mph nm pos [] (cptAnything,cptnew c ) True (error "CC.lhs: vbj (nm,pos) Nothing (Just c) has no declaration"))) ats
>                            vbj (nm,pos) (Just c) (Just e) ats = Obj nm pos (F[e,Tm (mIs (cptnew c ))]) ats

>  pAtt             :: Parser Token ObjectDef
>  pAtt              = att <$> phpId <* pKey ":" <*>  pExpr
>                      where att (nm,pos) ctx = Obj nm pos ctx []

>  pDeclaration     :: Parser Token Declaration
>  pDeclaration      = rebuild <$> pVarid <*> pKey_pos "::" <*> pConcept <*> (pKey "*" <|> pKey "->" ) <*> pConcept
>                              <*> (pProps `opt` []) <*> (pPragma `opt` [])
>                              <*> ((pKey "EXPLANATION" *> pString ) `opt` [])
>                              <*> ((pKey "=" *> pContent) `opt` []) <* pSpec '.'
>                      where rebuild nm pos s fun t props pragma expla content
>                              = Sgn nm s t (rd props `uni` if fun=="->" then [Uni,Tot] else []) (pr!!0) (pr!!1) (pr!!2) content expla pos 0 False
>                                where pr = pragma++["","",""]

>  pContent         :: Parser Token Pairs
>  pContent          = pSpec '[' *> pListSep (pKey ";") pRecord <* pSpec ']'

>  pProps           :: Parser Token [Prop]
>  pProps            = pSpec '['  *> pListSep (pSpec ',') pProp <* pSpec ']'

>  pProp            :: Parser Token Prop
>  pProp             = k Uni "UNI" <|> k Inj "INJ" <|> k Sur "SUR" <|> k Tot "TOT"
>                      <|> k Sym "SYM" <|> k Asy "ASY" <|> k Trn "TRN" <|> k Rfx "RFX"
>                      where k obj str = f <$> pKey str where f _ = obj

>  pPragma          :: Parser Token [String]
>  pPragma           = pKey "PRAGMA" *> pList1 pString

>  pRecord          :: Parser Token [String]
>  pRecord           = pSpec '(' *> pListSep (pSpec ',') pString <* pSpec ')'

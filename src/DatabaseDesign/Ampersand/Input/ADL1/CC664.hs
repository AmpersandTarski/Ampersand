{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
--THIS IS THE PARSER VERSION (rev.664 in svnou) AS USED IN THE PREVIOUS RELEASE OF STUDENT TOOL.
--IT HAS BEEN MODIFIED TO FIT CHANGES IN module Adl
--THIS PARSER ENABLES STUDENTS TO LOAD SCRIPTS WITH SYNTAX OF THE PREVIOUS TOOL.
--note: relations outside a pattern (context elements) are put in a dummy pattern
module DatabaseDesign.Ampersand.Input.ADL1.CC664 (pContext, keywordstxt, keywordsops, specialchars, opchars) where
   import DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner  ( Token(..),TokenType(..),noPos
                      , pKey,pConid,pString,pSpec,pAtom,pExpl,pVarid,pComma)
   import DatabaseDesign.Ampersand.Input.ADL1.UU_Parsing  (Parser
                      , (<$>) , (<$), (<*>), (<*) , (*>), (<|>)
                      ,pList,pListSep,pList1,pList1Sep,pSym
                      ,pSucceed
                      ,opt, Sequence,Alternative, IsParser
                      )
   import DatabaseDesign.Ampersand.Basics  (fatalMsg,Collection(..))
   import DatabaseDesign.Ampersand.Core.ParseTree
   import Data.List (nub,sort)
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "ADL1.CC664"


   keywordstxt :: [String]
   keywordstxt       = [ "CONTEXT", "ENDCONTEXT", "EXTENDS"
                       , "PATTERN", "ENDPATTERN"
                       , "SERVICE", "INITIAL", "SQLPLUG", "PHPPLUG"
                       , "POPULATION", "CONTAINS"
                       , "UNI", "INJ", "SUR", "TOT", "SYM", "ASY", "TRN", "RFX", "PROP", "ALWAYS"
                       , "RULE", "MAINTAINS", "SIGNALS", "SIGNAL", "ON"
                       , "RELATION", "CONCEPT", "KEY"
                       , "IMPORT", "GEN", "ISA", "I", "V", "S"
                       , "PRAGMA", "EXPLANATION", "EXPLAIN", "IN", "REF", "ENGLISH", "DUTCH"
                       , "ONE", "BIND", "TOPHP", "BINDING"
                       ]
   keywordsops :: [String]
   keywordsops       = [ "-|", "|-", "-", "->", ">", "=", "~", "+", ";", "!", "*", "::", ":", "\\/", "/\\", "\\", "/", "<>" ]
   specialchars :: String
   specialchars      = "()[].,{}"
   opchars :: String
   opchars           = nub (sort (concat keywordsops))





--   pBind             :: Parser Token (P_Declaration,String)
--   pBind              = rebuild <$ pKey "BIND" <*> pDeclaration <* pKey "TOPHP" <*> (pConid <|> pString)
--                       where rebuild d s = (d,s)

   pContext         :: Parser Token P_Context
   pContext  = rebuild <$ pKey "CONTEXT" <*> pConid <*>
--                                  ((rebexpr <$ pKey ":" <*> pExpr <*> 
--                                  --     (pSpec '{' *> pList1Sep (pSpec ';') pBind <* pSpec '}')
--                                       ((pKey "BINDING" *> pList1Sep (pSpec ',') pBind) `opt` [])
--                                    ) `opt` universe) <*>
--                                  ((pKey "EXTENDS" *> pList1Sep (pSpec ',') pConid) `opt` []) <*>
                                  pList pContextElement <* pKey "ENDCONTEXT"
                       where  
--                       rebexpr x y = (x,y)
--                       universe = (ERel(V (Anything,Anything)) ,[]) --default: the universe
                       rebuild nm ces = 
                          PCtx{ ctx_nm    = nm                            -- The name of this context
                              , ctx_lang  = Just Dutch
                              , ctx_markup= ReST
                              , ctx_pats  = ps                            -- The patterns defined in this context
                              , ctx_PPrcs = []                            -- The processes as defined by the parser
                              , ctx_rs    = []                            -- All user defined rules in this context, but outside patterns and outside processes
                              , ctx_ds    = []                            -- The declarations defined in this context, outside the scope of patterns
                              , ctx_cs    = [c | CCon c<-ces]             -- The concept definitions defined in this context, outside the scope of patterns
                              , ctx_ks    = [k | CKey k<-ces]             -- The key definitions defined in this context, outside the scope of patterns
                              , ctx_gs    = []                            -- The gen definitions defined in this context, outside the scope of patterns
                              , ctx_ifcs  = [o | CObj o<-ces]             -- The interfaces defined in this context, outside the scope of patterns
                              , ctx_ps    = [e | CXpl e<-ces]             -- The pre-explanations defined in this context, outside the scope of patterns
                              , ctx_pops  = [p | CPop p<-ces]             -- The populations defined in this context
                              , ctx_sql   = [plug | CSqlPlug plug<-ces]   -- user defined sqlplugs, taken from the Ampersand script
                              , ctx_php   = [plug | CPhpPlug plug<-ces]   -- user defined phpplugs, taken from the Ampersand script
                              , ctx_env   = Nothing
                              }
                          where
                               ps = [p | CPat p<-ces]
                       pContextElement    :: Parser Token ContextElement
                       pContextElement     = CPat     <$> pPattern      <|>
                                             CDcl     <$> pDeclaration  <|>
                                             CCon     <$> pConceptDef   <|>
                                             CKey     <$> pKeyDef       <|>
                                             CObj     <$> pService      <|>
                                             CSqlPlug <$> pSqlplug      <|>
                                             CPhpPlug <$> pPhpplug      <|>
                                             CXpl     <$> pExplain      <|>
                                             CPop     <$> pPopulation  


   data ContextElement = CPat P_Pattern
                       | CDcl P_Declaration
                       | CCon ConceptDef
                       | CKey P_KeyDef
                       | CObj P_Interface
                       | CPop P_Population
                       | CSqlPlug P_ObjectDef
                       | CPhpPlug P_ObjectDef
                       | CXpl PExplanation

   pExplain           :: Parser Token PExplanation
   pExplain            = PExpl <$> pKey_pos "EXPLAIN" <*> pExplObj <*> pLanguageID <*> pRefID <*> pExpl 
                          where pRefID :: Parser Token String
                                pRefID = (pKey "REF" *> pString) `opt` []
                                pExplObj :: Parser Token PExplObj
                                pExplObj = PExplConceptDef  <$ pKey "CONCEPT"  <*> (pConid <|> pString) <|>
                                           PExplDeclaration <$ pKey "RELATION" <*> pRelation <*> pSign  <|>
                                           PExplRule        <$ pKey "RULE"     <*> pADLid               <|>
                                           PExplKeyDef      <$ pKey "KEY"      <*> pADLid               <|>  
                                           PExplPattern     <$ pKey "PATTERN"  <*> pADLid               <|>
                                    --       PExplProcess     <$ pKey "PROCESS"  <*> pADLid               <|>
                                           PExplInterface   <$ pKey "SERVICE"  <*> pADLid               <|>
                                           PExplContext     <$ pKey "CONTEXT"  <*> pADLid 
                                pLanguageID :: Parser Token (Maybe Lang)
                                pLanguageID = lang <$> (pKey "IN" *> (pKey "DUTCH" <|> pKey "ENGLISH")) `opt` Nothing
                                               where
                                                lang str = case str of
                                                            "DUTCH"      -> Just Dutch
                                                            "ENGLISH"    -> Just English
                                                            _ -> fatal 122 (if null str then "must specify a language in pLanguageID" else "language "++str++" is not supported")



   pPopulation         :: Parser Token P_Population
   pPopulation = ppop <$ pKey "POPULATION" <*> pRelation <*> optional pSign <* pKey "CONTAINS" <*> pContent
                 where ppop r  Nothing   c = P_Popu r [] c
                       ppop r (Just sgn) c = P_Popu r (psign sgn) c

   pPattern         :: Parser Token P_Pattern
   pPattern  = rebuild <$> pKey_pos "PATTERN" <*> (pConid <|> pString)
                       <*> pList pPatElem
                       <*> pKey_pos "ENDPATTERN"
                       where
                         rebuild :: Origin -> String -> [PatElem] -> Origin -> P_Pattern
                         rebuild pos' nm pes end
                          = P_Pat { pt_nm      = nm
                                  , pt_pos       = pos'
                                  , pt_end       = end
                                  , pt_rls       = [r | Pr r<-pes] --was ooit: [r{r_env=nm} |Pr r<-pes]
                                  , pt_gns       = [g | Pg g<-pes]
                                  , pt_dcs       = [d | Pm d<-pes] --was ooit: [mph{decpat=nm} | Pm mph@(Sgn{})<-pes]
                                  , pt_cds       = [c | Pc c<-pes]
                                  , pt_kds       = [k | Pk k<-pes]
                                  , pt_xps       = [e | Pe e<-pes]
                                  , pt_pop       = []
                                  } 
                         pPatElem :: Parser Token PatElem
                         pPatElem = Pr <$> pRule         <|>
                                    Pg <$> pGen          <|>
                                    Pm <$> pDeclaration  <|>
                                    Pc <$> pConceptDef   <|>
                                    Pk <$> pKeyDef       <|>
                                    Pe <$> pExplain


   data PatElem      = Pr P_Rule
                     | Pg P_Gen
                     | Pm P_Declaration
                     | Pc ConceptDef
                     | Pk P_KeyDef
                     | Pe PExplanation

   pRule            :: Parser Token P_Rule
   pRule             = hc True            <$>   -- This boolean tells whether this rule will be a signal rule or a maintaining rule.
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
                        rulid (FileLoc(FilePos (_,Pos l _,_))) = "rule@line"++show l
                        rulid _ = fatal 179 "rulid is expecting a file location."
                        hc _ (lbl,po) antc po' cons expl
                          = P_Ru { rr_nm   = if null lbl then rulid po' else lbl
                                 , rr_exp  = Pimp(antc,cons)
                                 , rr_fps  = rulepos (lbl,po) po'
                                 , rr_mean = (Just Dutch, expl)
                                 }      
                        kc isSg (lbl,po) cons po' antc = hc isSg (lbl,po) antc po' cons
                        dc _ (lbl,po) defd po' expr expl
                          = P_Ru { rr_nm   = if null lbl then rulid po' else lbl
                                 , rr_exp  = Pequ (defd,expr)
                                 , rr_fps  = rulepos (lbl,po) po'
                                 , rr_mean = (Just Dutch, expl)
                               }
                        ac _ (lbl,po) expr expl
                          = P_Ru { rr_nm  = if null lbl then rulid po else lbl
                                 , rr_exp = expr
                                 , rr_fps = po
                                 , rr_mean = (Just Dutch, expl)
                                 }
                        rulepos (lbl,po) po' = if null lbl then po' else po -- position of the label is preferred. In its absence, take the position of the root operator of this rule's expression.
                        pSignal          :: Parser Token (String, Origin)
                        pSignal           = pKey "SIGNAL" *> pADLid_val_pos <* pKey "ON"       <|>
                                              pKey "RULE" *> pADLid_val_pos <* pKey "SIGNALS"
                        pAlways          :: Parser Token (String, Origin)
                        pAlways           = ( pKey "RULE" *> pADLid_val_pos <* pKey "MAINTAINS" ) `opt` ("",fatal 160 "no location")


   pGen             :: Parser Token P_Gen
   pGen              = rebuild <$ pKey "GEN" <*> (pConid <|> pString) <*> pKey_pos "ISA" <*> (pConid <|> pString)
                       where rebuild spc p gen = PGen p (PCpt gen ) (PCpt spc )

   pExpr            :: Parser Token P_Expression
   pExpr             = f <$> pList1Sep (pKey "\\/") pFactorI
                       where f [x] = x
                             f  xs = PUni xs

   pFactorI         :: Parser Token P_Expression
   pFactorI          = f <$> pList1Sep (pKey "/\\") pFactor
                       where f [x]     = x
                             f  xs     = Pisc xs

   pFactor          :: Parser Token P_Expression
   pFactor           = f <$> pList1Sep (pKey "!") pTermD
                       where f [t]     = t
                             f ts      = PRad ts

   pTermD           :: Parser Token P_Expression
   pTermD            = f <$> pList1Sep (pKey ";") pTermP
                       where f [t]     = t
                             f ts      = PCps ts

   pTermP           :: Parser Token P_Expression
   pTermP            = f <$> pList1Sep (pKey "*") pTerm
                       where f [t]     = t
                             f ts      = PPrd ts

   pTerm            :: Parser Token P_Expression
   pTerm             = tm <$> (preStr `opt` []) <*> pRelation <*> optional pSign <*> (postStr `opt` [])                            <|>
                       tc <$> (preStr `opt` []) <*> (pSpec '(' *> pExpr <* pSpec ')') <*> (postStr `opt` [])
                       where
                        tm xs pm (Just sgn) ys  = PTyp (f (Prel pm ) (xs++ys)) sgn
                        tm xs pm Nothing  ys = f (Prel pm ) (xs++ys)
                        tc xs pc ys      = f pc (xs++ys)
                        f t ('~':xs) = PFlp (f t xs)
                        f t ('*':xs) = PKl0 (f t xs)
                        f t ('+':xs) = PKl1 (f t xs)
                        f t ('-':xs) = PCpl (f t xs)
                        f _ (_:_)    = fatal 245 "Consult your dealer!"
                        f t []       = t
                        preStr :: Parser Token String
                        preStr  = g <$> pList1 (pKey "-")
                                   where
                                    g xs = if odd (length cs) then take 1 cs else [] where cs = concat xs
                        postStr :: Parser Token String
                        postStr  = f' <$> pList1 (pKey "~" <|> pKey "+" <|> pKey "-" <|> pKey "*")
                                   where
                                    f' xs = g ['~' |'~'<-concat xs] ++ g ['-' |'-'<-concat xs] ++ eat [x |x<-concat xs,x/='~',x/='-']
                                    g xs = if odd (length xs) then take 1 xs else []
                                    eat :: String -> String
                                    eat ('*':'*':xs) = eat ('*':xs)
                                    eat ('+':'*':xs) = eat ('*':xs)
                                    eat ('*':'+':xs) = eat ('*':xs)
                                    eat ('+':'+':xs) = eat ('+':xs)
                                    eat (x:xs)       = x:eat xs
                                    eat []           = []




   pRelation        :: Parser Token P_Relation
   pRelation         = P_I <$ pKey "I"      <|>
                       P_V <$ pKey "V"      <|>
                       rebuild <$> pVarid_val_pos      <|>
                       single  <$> pAtom 
                       where rebuild :: (String, Origin) -> P_Relation
                             rebuild (nm,pos') =
                               P_Rel  { rel_nm = nm
                                      , rel_pos = pos'
                                      }
                             single :: String -> P_Relation
                             single nm = P_Mp1 { rel_1val = nm
                                               }     

   pSign :: Parser Token P_Sign
   pSign = rebuild <$ pSpec '[' <*> pConcept <*> optional (pKey "*" *> pConcept) <* pSpec ']'
      where
        rebuild :: P_Concept -> Maybe P_Concept -> P_Sign
        rebuild a mb = case mb of 
                        Just b -> P_Sign { psign = [a,b] }
                        Nothing -> P_Sign { psign = [a] }


   pConcept         :: Parser Token P_Concept
   pConcept          = (P_Singleton <$ pKey "ONE") <|> (PCpt <$> (pConid <|> pString))
                      -- where c str = C str (==) []

-- DAAROM:
--  (SJ) Waarom heeft een label (optioneel) strings?
--  (GM) Dit is bedoeld als binding mechanisme voor implementatiespecifieke (SQL/PHP plug,PHP web app,etc) properties
--  (SJ) Met het invoeren van referenties (t.b.v. losse explanations) bestaat er een variant met props en eentje zonder.
   pLabelProps      :: Parser Token Label
   pLabelProps       = lbl <$> pADLid_val_pos
                           <*> ((pSpec '{' *> pList1Sep (pSpec ',') (pList1 pADLid) <* pSpec '}') `opt` [])
                           <*  pKey_pos ":"
                       where lbl :: (String, Origin) -> [[String]] -> Label
                             lbl (nm,pos') = Lbl nm pos'

   pADLid           :: Parser Token String
   pADLid            = pVarid <|> pConid <|> pString

   pADLid_val_pos   :: Parser Token (String, Origin)
   pADLid_val_pos    = pVarid_val_pos <|> pConid_val_pos <|> pString_val_pos

   pConceptDef      :: Parser Token ConceptDef
   pConceptDef       = cd <$> pKey_pos "CONCEPT"
                          <*> (pConid <|> pString)   -- the concept name
                          <*> pString                -- the definition text
                          <*> (pString `opt` "")     -- a reference to the source of this definition.
                       where cd po nm x ref = Cd po nm False x "" ref


-- A key definition looks like:   KEY Person(name, address),
-- which means that name<>name~ /\ address<>addres~ |- I[Person].
-- You may also use an expression on each attribute place, for example: KEY onpassport: Person(nationality, passport;documentnr),
-- which means that nationality<>nationality~ /\ passport;documentnr<>(passport;documentnr)~ |- I[Person].
-- For the sake of a proper user interface, you can assign labels to the attributes in a key, for example:
-- KEY onSSN: Person("social security number":ssn)
   pKeyDef          :: Parser Token P_KeyDef
   pKeyDef           = kd <$ pKey "KEY" <*> pLabelProps <*> pConcept <* pSpec '(' <*> pList1Sep (pSpec ',') pKeyAtt <* pSpec ')'
                        where kd :: Label -> P_Concept -> P_ObjectDefs -> P_KeyDef 
                              kd (Lbl nm p _) c ats = P_Kd { kd_pos = p
                                                           , kd_lbl = nm
                                                           , kd_cpt = c
                                                           , kd_ats = ats
                                                           }

                              pKeyAtt :: Parser Token P_ObjectDef
                              pKeyAtt  = attL <$> pLabelProps <*> pExpr <|>
                                         att <$> pExpr
                                  where attL (Lbl nm p strs) attexpr =
                                           P_Obj { obj_nm   = nm
                                                 , obj_pos  = p
                                                 , obj_ctx  = attexpr 
                                                 , obj_ats  = []
                                                 , obj_strs = strs
                                                 }
                                        att attexpr =
                                            P_Obj { obj_nm   = ""
                                                  , obj_pos  = Origin "pKeyAtt CC664"
                                                  , obj_ctx  = attexpr 
                                                  , obj_ats  = []
                                                  , obj_strs = []
                                                  }

   pSqlplug          :: Parser Token P_ObjectDef
   pSqlplug           = pKey_pos "SQLPLUG" *> pObj

   pPhpplug          :: Parser Token P_ObjectDef
   pPhpplug           = pKey_pos "PHPPLUG" *> pObj


   pService         :: Parser Token P_Interface
   pService          = lbl <$> (pKey "SERVICE" *> pADLid_val_pos) <*>
                               (pParams `opt` [])                 <*>  -- a list of relations, which are editable within this service.
                               (pArgs `opt` [])                   <*>  -- a list of arguments for code generation.
                               (pKey ":" *> pExpr)                <*>  -- the context expression (mostly: I[c])
                               (pAttrs `opt` [])                       -- the subobjects
                       where lbl :: (String, Origin) -> [(P_Relation,P_Sign)] -> [[String]] -> P_Expression -> [P_ObjectDef] -> P_Interface
                             lbl (nm,p) params args expr ats
                              = P_Ifc { ifc_Name   = nm
                                      , ifc_Params = params
                                  --  , ifcViols  = []
                                      , ifc_Args   = args
                                      , ifc_Obj    = P_Obj { obj_nm    = nm    
                                                           , obj_pos   = p
                                                           , obj_ctx   = expr
                                                           , obj_ats   = ats
                                                           , obj_strs  = args
                                                           }
                                      , ifc_Pos    = p
                                      , ifc_Expl   = ""
                                    }
                             pParams = pSpec '(' *> pList1Sep (pSpec ',') pRelSign        <* pSpec ')' 
                             pArgs   = pSpec '{' *> pList1Sep (pSpec ',') (pList1 pADLid) <* pSpec '}'
                             pAttrs  = pKey "=" *> pSpec '[' *> pListSep (pSpec ',') pObj <* pSpec ']'
                             pRelSign :: Parser Token (P_Relation, P_Sign)
                             pRelSign = f <$> pRelation <*> optional pSign
                                         where f rel Nothing    = (rel,P_Sign [])
                                               f rel (Just sgn) = (rel,sgn)



   optional :: (Sequence p, Alternative p) => p a -> p (Maybe a)
   optional a        = Just <$> a <|> pSucceed Nothing

   pObj             :: Parser Token P_ObjectDef
   pObj              = obj <$> pLabelProps
                           <*> pExpr                                             -- de contextexpressie (default: I[c])
                           <*> optional (pKey "ALWAYS" *> pProps')             -- uni of tot of prop
                           <*> ((pKey "=" *> pSpec '[' *> pListSep (pSpec ',') pObj <* pSpec ']') `opt` [])  -- de subobjecten
                       where 
                         obj (Lbl nm pos' strs) expr _ ats = 
                              P_Obj { obj_nm   = nm
                                    , obj_pos  = pos'
                                    , obj_ctx  = expr
                                    , obj_ats  = ats
                                    , obj_strs = strs
                                    }
                         -- | De pProps' is identiek aan pProps, maar werkt alleen op UNI en TOT. Ze is bedoeld voor de Service definities.
                         pProps' :: Parser Token [Prop]
                         pProps' = f <$> pList pProp'
                           where f :: [String] -> [Prop]
                                 f ps = [k p | p<-ps, p/="PROP"]++[p' | p<-ps, p=="PROP", p'<-[Sym, Asy]]
                                 k "TOT" = Tot
                                 k "UNI" = Uni
                                 k s = fatal 413 ("Unknown property tag has been used: " ++ show s)

                         pProp' :: Parser Token String
                         pProp' = pKey "UNI" <|> pKey "TOT" <|> pKey "PROP"

   pDeclaration     :: Parser Token P_Declaration
   pDeclaration      = rebuild <$> pVarid 
                               <*> pKey_pos "::" 
                               <*> pConcept 
                               <*> (pKey "*" <|> pKey "->" ) 
                               <*> pConcept
                               <*> (pProps `opt` []) <*> (pPragma `opt` [])
                               <*> ((pKey "EXPLANATION" *> pString ) `opt` [])
                               <*> ((pKey "=" *> pContent) `opt` []) <* pSpec '.'
                       where rebuild :: String
                                     -> Origin
                                     -> P_Concept
                                     -> String
                                     -> P_Concept
                                     -> [Prop]
                                     -> [String]
                                     -> String
                                     -> Pairs
                                     -> P_Declaration
                             rebuild nm pos' s fun' t props pragma meaning content
                               = P_Sgn { dec_nm = nm
                                       , dec_sign = P_Sign [s,t]
                                       , dec_prps = props'
                                       , dec_prL = head pr
                                       , dec_prM = pr!!1
                                       , dec_prR = pr!!2
                                       , dec_Mean = meaning
                                       , dec_popu = content
                                       , dec_fpos = pos'
                                       , dec_plug = False
                                       }
                                 where pr = pragma++["","",""]
                                       props'= nub props `uni` if fun'=="->" then [Uni,Tot] else []
                             pPragma :: Parser Token [String]
                             pPragma = pKey "PRAGMA" *> pList1 pString
                             pProps  :: Parser Token [Prop]
                             pProps  = pSpec '['  *> pListSep (pSpec ',') pProp <* pSpec ']'

                             pProp   :: Parser Token Prop
                             pProp   = k Uni "UNI" <|> k Inj "INJ" <|> k Sur "SUR" <|> k Tot "TOT"
                                   <|> k Sym "SYM" <|> k Asy "ASY" <|> k Trn "TRN" <|> k Rfx "RFX"
                                  where k obj str = f <$> pKey str where f _ = obj



   pContent         :: Parser Token Pairs
   pContent          = pSpec '[' *> pListSep (pKey ";") pRecord <* pSpec ']'


   pRecord          :: Parser Token Paire
   pRecord           = mkPair<$ pSpec '(' <*> pString  <* pComma   <*> pString  <* pSpec ')'
                                
   get_tok_pos :: Token -> Origin
   get_tok_pos     (Tok _ _ s l f) = FileLoc(FilePos (f,l,s))
   get_tok_val_pos :: Token -> (String, Origin)
   get_tok_val_pos (Tok _ _ s l f) = (s,FileLoc(FilePos (f,l,s)))



   gsym_pos :: IsParser p Token => TokenType -> String -> String -> p Origin
   gsym_pos kind val' val2' = get_tok_pos <$> pSym (Tok kind val' val2' noPos "")

   gsym_val_pos :: IsParser p Token => TokenType -> String -> String -> p (String,Origin)
   gsym_val_pos kind val' val2' = get_tok_val_pos <$> pSym (Tok kind val' val2' noPos "")

   pKey_pos :: String -> Parser Token Origin
   pKey_pos  keyword  =   gsym_pos TkKeyword   keyword   keyword

   pString_val_pos, {- pAtom_val_pos, -}pVarid_val_pos, pConid_val_pos
                      ::  IsParser p Token => p (String,Origin)
   pString_val_pos    =   gsym_val_pos TkString    ""        "?STR?"
   pVarid_val_pos     =   gsym_val_pos TkVarid     ""        "?LC?"
   pConid_val_pos     =   gsym_val_pos TkConid     ""        "?UC?"
                                

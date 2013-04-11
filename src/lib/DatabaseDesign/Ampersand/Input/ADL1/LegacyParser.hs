{-# OPTIONS_GHC -Wall -fno-enable-rewrite-rules #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
--THIS IS THE PARSER VERSION (rev.664 in svnou) AS USED IN THE PREVIOUS RELEASE OF STUDENT TOOL.
--IT HAS BEEN MODIFIED TO FIT CHANGES IN module Adl
--THIS PARSER ENABLES STUDENTS TO LOAD SCRIPTS WITH SYNTAX OF THE PREVIOUS TOOL.
--note: relations outside a pattern (context elements) are put in a dummy pattern
module DatabaseDesign.Ampersand.Input.ADL1.LegacyParser (pContext, keywordstxt, keywordsops, specialchars, opchars) where
   import DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner  ( Token(..),TokenType(..),noPos
                      , pKey,pConid,pString,pSpec,pExpl,pVarid,pComma)
   import DatabaseDesign.Ampersand.Input.ADL1.UU_Parsing  (Parser
                      , (<$>) , (<$), (<*>), (<*) , (*>), (<|>), (<??>)
                      ,pList,pListSep,pList1,pList1Sep,pSym
                      ,pSucceed
                      ,opt, Sequence,Alternative, IsParser
                      )
   import DatabaseDesign.Ampersand.Basics  (fatalMsg,Collection(..))
   import DatabaseDesign.Ampersand.Core.ParseTree
   import Data.List (nub,sort)
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "ADL1.LegacyParser"


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





--   pBind :: Parser Token (P_Declaration,String)
--   pBind              = rebuild <$ pKey "BIND" <*> pDeclaration <* pKey "TOPHP" <*> (pConid <|> pString)
--                       where rebuild d s = (d,s)

   pContext :: Parser Token P_Context
   pContext  = rebuild <$ pKey "CONTEXT" <*> pConid <*>
--                                  ((rebexpr <$ pKey ":" <*> pExpr <*> 
--                                  --     (pSpec '{' *> pList1Sep (pSpec ';') pBind <* pSpec '}')
--                                       ((pKey "BINDING" *> pList1Sep (pSpec ',') pBind) `opt` [])
--                                    ) `opt` universe) <*>
--                                  ((pKey "EXTENDS" *> pList1Sep (pSpec ',') pConid) `opt` []) <*>
                                  pList pContextElement <* pKey "ENDCONTEXT"
                       where  
--                       rebexpr x y = (x,y)
--                       universe = (vExpr (Sign Anything Anything),[]) --default: the universe
                       rebuild nm ces = 
                          PCtx{ ctx_nm    = nm                            -- The name of this context
                              , ctx_pos   = []
                              , ctx_lang  = Just Dutch
                              , ctx_markup= Just ReST
                              , ctx_thms  = []                            -- Names of patterns/processes to be printed in the functional specification. (not applicable in this version.)
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
                              , ctx_metas = []
                              }
                          where
                               ps = [p | CPat p<-ces]
                       pContextElement :: Parser Token ContextElement
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
                       | CXpl PPurpose

   pExplain :: Parser Token PPurpose
   pExplain            = rebuild <$> pKey_pos "EXPLAIN" <*> pExplObj <*> optional pLanguageID <*> pRefID <*> pExpl 
                          where rebuild orig obj lang ref str =
                                  PRef2 orig obj (P_Markup lang Nothing str) ref
                                pRefID :: Parser Token String
                                pRefID = (pKey "REF" *> pString) `opt` []
                                pExplObj :: Parser Token PRef2Obj
                                pExplObj = PRef2ConceptDef  <$ pKey "CONCEPT"  <*> (pConid <|> pString) <|>
                                           PRef2Declaration <$ pKey "RELATION" <*> pRelSign             <|>
                                           PRef2Rule        <$ pKey "RULE"     <*> pADLid               <|>
                                           PRef2KeyDef      <$ pKey "KEY"      <*> pADLid               <|>  
                                           PRef2Pattern     <$ pKey "PATTERN"  <*> pADLid               <|>
                                    --       PRef2Process     <$ pKey "PROCESS"  <*> pADLid               <|>
                                           PRef2Interface   <$ pKey "SERVICE"  <*> pADLid               <|>
                                           PRef2Context     <$ pKey "CONTEXT"  <*> pADLid 
                                pLanguageID :: Parser Token Lang
                                pLanguageID = lang <$> (pKey "IN" *> (pKey "DUTCH" <|> pKey "ENGLISH"))
                                               where
                                                lang str = case str of
                                                            "DUTCH"      -> Dutch
                                                            "ENGLISH"    -> English
                                                            _ -> fatal 122 (if null str then "must specify a language in pLanguageID" else "language "++str++" is not supported")

   pPopulation :: Parser Token P_Population
   pPopulation = ppop <$> pKey_pos "POPULATION" <*> pRelSign <* pKey "CONTAINS" <*> pContent
       where
         ppop orig (PTrel _ nm sgn) content = P_RelPopu { p_rnme   = nm
                                                        , p_type   = sgn
                                                        , p_orig   = orig
                                                        , p_popps  = content
                                                        }
         ppop _ x _ = fatal 138 ("ppop must have an argument of the form PTrel _ nm sgn, but has been called with\n"++show x)

   pPattern :: Parser Token P_Pattern
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
                                  , pt_dcs       = [d | Pm d<-pes] --was ooit: [mph{decpat=nm} | Pm mph@Sgn{}<-pes]
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
                     | Pe PPurpose

   pRule :: Parser Token P_Rule
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
                                 , rr_exp  = Pimp po' antc cons
                                 , rr_fps  = rulepos (lbl,po) po'
                                 , rr_mean = meaning expl
                                 , rr_msg = []
                                 , rr_viol = Nothing
                                 }      
                        kc isSg (lbl,po) cons po' antc = hc isSg (lbl,po) antc po' cons
                        dc _ (lbl,po) defd po' expr expl
                          = P_Ru { rr_nm   = if null lbl then rulid po' else lbl
                                 , rr_exp  = Pequ po' defd expr
                                 , rr_fps  = rulepos (lbl,po) po'
                                 , rr_mean = meaning expl
                                 , rr_msg = []
                                 , rr_viol = Nothing
                                 }
                        ac _ (lbl,po) expr expl
                          = P_Ru { rr_nm  = if null lbl then rulid po else lbl
                                 , rr_exp = expr
                                 , rr_fps = po
                                 , rr_mean = meaning expl
                                 , rr_msg = []
                                 , rr_viol = Nothing
                                 }
                        meaning str = [PMeaning (P_Markup Nothing Nothing str)]
                        rulepos (lbl,po) po' = if null lbl then po' else po -- position of the label is preferred. In its absence, take the position of the root operator of this rule's expression.
                        pSignal :: Parser Token (String, Origin)
                        pSignal           = pKey "SIGNAL" *> pADLid_val_pos <* pKey "ON"       <|>
                                              pKey "RULE" *> pADLid_val_pos <* pKey "SIGNALS"
                        pAlways :: Parser Token (String, Origin)
                        pAlways           = ( pKey "RULE" *> pADLid_val_pos <* pKey "MAINTAINS" ) `opt` ("",fatal 160 "no location")
   pGen :: Parser Token P_Gen
   pGen              = rebuild <$ pKey "GEN" <*> (pConid <|> pString) <*> pKey_pos "ISA" <*> (pConid <|> pString)
                       where rebuild spc p gen = PGen p (PCpt gen ) (PCpt spc )

   pExpr :: Parser Token Term
   pExpr             = pFactorI <??> (f <$> pKey_pos "\\/" <*> pExpr)
                       where f orig y x = PUni orig x y

   pFactorI :: Parser Token Term
   pFactorI          = pFactor <??> (f <$> pKey_pos "/\\" <*> pFactorI)
                       where f orig y x = PIsc orig x y

   pFactor :: Parser Token Term
   pFactor           = pTermD <??> (f <$> pKey_pos "!" <*> pFactor)
                       where f orig y x = PRad orig x y

   pTermD :: Parser Token Term
   pTermD            = pTermP <??> (f <$> pKey_pos ";" <*> pTermD)
                       where f orig y x = PCps orig x y

   pTermP :: Parser Token Term
   pTermP            = pExp5 <??> (f <$> pKey_pos "*" <*> pTermP)
                       where f orig y x = PPrd orig x y

   pExp5 :: Parser Token Term
   pExp5  =  f <$> pList (pKey_pos "-") <*> pExp6  <*> pList ( pKey_val_pos "~" <|> pKey_val_pos "*" <|> pKey_val_pos "+" )
             where f ms pe (("~",orig):ps) = PFlp orig (f ms pe ps)
                   f ms pe (("*",orig):ps) = PKl0 orig (f ms pe ps)
                   f ms pe (("+",orig):ps) = PKl1 orig (f ms pe ps)
                   f (orig:ms) pe ps = PCpl orig (f ms pe ps)
                   f _ pe _          = pe

   pExp6 :: Parser Token Term
   pExp6             =  pRelationRef                                    <|>
                        PBrk <$>  pSpec_pos '('  <*>  pExpr  <*  pSpec ')'

   pRelationRef :: Parser Token Term
   pRelationRef      = pid   <$> pKey_pos "I" <*> optional (pSpec '[' *> pConceptRef <* pSpec ']') <|>
                       pfull <$> pKey_pos "V" <*> optional pSign                                   <|>
                       pRelSign                                                                    <|>
                       singl <$> pAtom_val_pos <*> optional (pSpec '[' *> pConceptRef <* pSpec ']')
                       where pid orig Nothing = PI orig
                             pid  _  (Just c) = Pid c
                             pfull orig Nothing = PVee orig
                             pfull _ (Just (P_Sign cs, _)) = if null cs then fatal 273 "null source and target in pRelationRef" else Pfull (head cs) (last cs)
                             singl (nm,orig) Nothing  = Patm orig nm []
                             singl (nm,orig) (Just c) = Patm orig nm [c]

   pRelSign :: Parser Token Term
   pRelSign          = prel  <$> pVarid_val_pos <*> optional pSign
                       where prel (nm,orig) Nothing = Prel orig nm
                             prel (nm,_) (Just (sgn,orig)) = PTrel orig nm sgn

   pSign :: Parser Token (P_Sign,Origin)
   pSign = rebuild <$> pSpec_pos '[' <*> pConceptRef <*> optional (pKey "*" *> pConceptRef) <* pSpec ']'
      where
        rebuild :: Origin -> P_Concept -> Maybe P_Concept -> (P_Sign,Origin)
        rebuild orig a mb
         = case mb of 
             Just b  -> (P_Sign { psign = [a,b] }, orig)
             Nothing -> (P_Sign { psign = [a] }  , orig)


   pConceptRef :: Parser Token P_Concept
   pConceptRef       = (P_Singleton <$ pKey "ONE") <|> (PCpt <$> (pConid <|> pString))

-- DAAROM:
--  (SJ) Waarom heeft een label (optioneel) strings?
--  (GM) Dit is bedoeld als binding mechanisme voor implementatiespecifieke (SQL/PHP plug,PHP web app,etc) properties
--  (SJ) Met het invoeren van referenties (t.b.v. losse explanations) bestaat er een variant met props en eentje zonder.
   pLabelProps :: Parser Token Label
   pLabelProps       = lbl <$> pADLid_val_pos
                           <*> ((pSpec '{' *> pList1Sep (pSpec ',') (pList1 pADLid) <* pSpec '}') `opt` [])
                           <*  pKey_pos ":"
                       where lbl :: (String, Origin) -> [[String]] -> Label
                             lbl (nm,pos') = Lbl nm pos'

   pADLid :: Parser Token String
   pADLid            = pVarid <|> pConid <|> pString

   pADLid_val_pos :: Parser Token (String, Origin)
   pADLid_val_pos    = pVarid_val_pos <|> pConid_val_pos <|> pString_val_pos

   pConceptDef :: Parser Token ConceptDef
   pConceptDef       = cd <$> pKey_pos "CONCEPT"
                          <*> (pConid <|> pString)   -- the concept name
                          <*> pString                -- the definition text
                          <*> (pString `opt` "")     -- a reference to the source of this definition.
                       where cd po nm x = Cd po nm False x ""


-- A key definition looks like:   KEY onNameAdress : Person(name, address),
-- which means that name<>name~ /\ address<>addres~ |- I[Person].
-- The label 'onNameAddress' is used to refer to this key.
-- You may also use an expression on each attribute place, for example: KEY onpassport: Person(nationality, passport;documentnr),
-- which means that nationality<>nationality~ /\ passport;documentnr<>(passport;documentnr)~ |- I[Person].
-- For the sake of a proper user interface, you can assign labels to the attributes in a key, for example:
-- KEY onSSN: Person("social security number":ssn)
   pKeyDef :: Parser Token P_KeyDef
   pKeyDef           = kd <$ pKey "KEY" <*> pLabelProps <*> pConceptRef <* pSpec '(' <*> pList1Sep (pSpec ',') pKeySegment <* pSpec ')'
                        where kd :: Label -> P_Concept -> [P_KeySegment] -> P_KeyDef 
                              kd (Lbl nm p _) c ats = P_Kd { kd_pos = p
                                                           , kd_lbl = nm
                                                           , kd_cpt = c
                                                           , kd_ats = ats
                                                           }

                              pKeySegment :: Parser Token P_KeySegment
                              pKeySegment = P_KeyExp <$> pKeyAtt <|> P_KeyText <$ pKey "S" <*> pString
                              
                              pKeyAtt :: Parser Token P_ObjectDef
                              pKeyAtt  = attL <$> pLabelProps <*> pExpr <|>
                                         att <$> pExpr
                                  where attL (Lbl nm p strs) attexpr =
                                           P_Obj { obj_nm   = nm
                                                 , obj_pos  = p
                                                 , obj_ctx  = attexpr 
                                                 , obj_msub = Nothing
                                                 , obj_strs = strs
                                                 }
                                        att attexpr =
                                            P_Obj { obj_nm   = ""
                                                  , obj_pos  = Origin "pKeyAtt CC664"
                                                  , obj_ctx  = attexpr 
                                                  , obj_msub = Nothing
                                                  , obj_strs = []
                                                  }

   pSqlplug :: Parser Token P_ObjectDef
   pSqlplug           = pKey_pos "SQLPLUG" *> pObj

   pPhpplug :: Parser Token P_ObjectDef
   pPhpplug           = pKey_pos "PHPPLUG" *> pObj


   pService :: Parser Token P_Interface
   pService          = lbl <$> (pKey "SERVICE" *> pADLid_val_pos) <*>
                               (pParams `opt` [])                 <*>  -- a list of expressions, which say which relations are editable within this service.
                                                                       -- either  Prel _ nm
                                                                       --       or  PTrel _ nm sgn
                               (pArgs `opt` [])                   <*>  -- a list of arguments for code generation.
                               (pKey ":" *> pExpr)                <*>  -- the context expression (mostly: I[c])
                               (pAttrs `opt` [])                       -- the subobjects
                       where lbl :: (String, Origin) -> [Term] -> [[String]] -> Term -> [P_ObjectDef] -> P_Interface
                             lbl (nm,p) params args expr ats
                              = P_Ifc { ifc_Name   = nm
                                      , ifc_Params = params
                                      , ifc_Args   = args
                                      , ifc_Roles  = []
                                      , ifc_Obj    = P_Obj { obj_nm    = nm    
                                                           , obj_pos   = p
                                                           , obj_ctx   = expr
                                                           , obj_msub  = Just . P_Box $ ats
                                                           , obj_strs  = args
                                                           }
                                      , ifc_Pos    = p
                                      , ifc_Prp    = ""
                                    }
                             pParams = pSpec '(' *> pList1Sep (pSpec ',') pRelSign        <* pSpec ')' 
                             pArgs   = pSpec '{' *> pList1Sep (pSpec ',') (pList1 pADLid) <* pSpec '}'
                             pAttrs  = pKey "=" *> pSpec '[' *> pListSep (pSpec ',') pObj <* pSpec ']'



   optional :: (Sequence p, Alternative p) => p a -> p (Maybe a)
   optional a        = Just <$> a <|> pSucceed Nothing

   pObj :: Parser Token P_ObjectDef
   pObj              = obj <$> pLabelProps
                           <*> pExpr                                             -- de contextexpressie (default: I[c])
                           <*> optional (pKey "ALWAYS" *> pProps')             -- uni of tot of prop
                           <*> ((pKey "=" *> pSpec '[' *> pListSep (pSpec ',') pObj <* pSpec ']') `opt` [])  -- de subobjecten
                       where 
                         obj (Lbl nm pos' strs) expr _ ats = 
                              P_Obj { obj_nm   = nm
                                    , obj_pos  = pos'
                                    , obj_ctx  = expr
                                    , obj_msub = Just . P_Box $ ats
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

   pDeclaration :: Parser Token P_Declaration
   pDeclaration      = rebuild <$> pVarid 
                               <*> pKey_pos "::" 
                               <*> pConceptRef 
                               <*> (pKey "*" <|> pKey "->" ) 
                               <*> pConceptRef
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
                             rebuild nm pos' s fun' t props pragma mean content
                               = P_Sgn { dec_nm = nm
                                       , dec_sign = P_Sign [s,t]
                                       , dec_prps = props'
                                       , dec_prL = head pr
                                       , dec_prM = pr!!1
                                       , dec_prR = pr!!2
                                       , dec_Mean = if mean == ""
                                                    then []
                                                    else [PMeaning (P_Markup Nothing Nothing mean)]
                                       , dec_conceptDef = Nothing
                                       , dec_popu = content
                                       , dec_fpos = pos'
                                       , dec_plug = False
                                       }
                                 where pr = pragma++["","",""]
                                       props'= nub props `uni` if fun'=="->" then [Uni,Tot] else []
                             pPragma :: Parser Token [String]
                             pPragma = pKey "PRAGMA" *> pList1 pString
                             pProps :: Parser Token [Prop]
                             pProps  = pSpec '['  *> pListSep (pSpec ',') pProp <* pSpec ']'

                             pProp :: Parser Token Prop
                             pProp   = k Uni "UNI" <|> k Inj "INJ" <|> k Sur "SUR" <|> k Tot "TOT"
                                   <|> k Sym "SYM" <|> k Asy "ASY" <|> k Trn "TRN" <|> k Rfx "RFX"
                                  where k obj str = f <$> pKey str where f _ = obj


   pContent :: Parser Token Pairs
   pContent          = pSpec '[' *> pListSep (pKey ";") pRecord <* pSpec ']'

   pRecord :: Parser Token Paire
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
   pKey_pos keyword  =   gsym_pos TkKeyword   keyword   keyword
   pSpec_pos :: Char -> Parser Token Origin
   pSpec_pos s       =   gsym_pos TkSymbol    [s]       [s]

   pString_val_pos, pVarid_val_pos, pConid_val_pos, pAtom_val_pos ::  IsParser p Token => p (String,Origin)
   pString_val_pos    =   gsym_val_pos TkString    ""        "?STR?"
   pVarid_val_pos     =   gsym_val_pos TkVarid     ""        "?LC?"
   pConid_val_pos     =   gsym_val_pos TkConid     ""        "?UC?"
   pAtom_val_pos      =   gsym_val_pos TkAtom      ""        ""
   pKey_val_pos ::  IsParser p Token => String -> p (String,Origin)
   pKey_val_pos keyword = gsym_val_pos TkKeyword   keyword   keyword
--   pSpec_val_pos ::  IsParser p Token => Char -> p (String,Origin)
--   pSpec_val_pos s      = gsym_val_pos TkSymbol    [s]       [s]

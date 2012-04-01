{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
module DatabaseDesign.Ampersand.Input.ADL1.Parser 
   (pContext, pPopulations,pExpr, keywordstxt, keywordsops, specialchars, opchars) where
   import DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner
            ( Token(..),TokenType(..),noPos
            , pKey,pConid,pString,pSpec,pAtom,pExpl,pVarid,pComma,pInteger)
   import DatabaseDesign.Ampersand.Input.ADL1.UU_Parsing
            (Parser
            , (<$>) , (<$), (<*>), (<*), (*>), (<|>), (<??>)
            ,pList,pListSep,pList1,pList1Sep,pSym
            ,pSucceed
            ,opt, Sequence,Alternative, IsParser
            )
   import DatabaseDesign.Ampersand.Basics  (fatalMsg,Collection(..),trim)
   import DatabaseDesign.Ampersand.Core.ParseTree    
   import Data.List (nub,sort)
   import Data.Maybe
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "ADL1.Parser"

--  The Ampersand scanner takes the file name (String) for documentation and error messaging.
--   scanner :: String -> String -> [Token]
--   scanner fn str = scan keywordstxt keywordsops specialchars opchars fn initPos str

   keywordstxt :: [String]
   keywordstxt       = [ "INCLUDE"
                       , "CONTEXT", "ENDCONTEXT", "EXTENDS", "TEXTMARKUP", "THEMES"
                       , "META"
                       , "PATTERN", "ENDPATTERN"
                       , "PROCESS", "ENDPROCESS"
                       , "INTERFACE", "FOR", "BOX", "INITIAL", "SQLPLUG", "PHPPLUG", "TYPE"
                       , "POPULATION", "CONTAINS"
                       , "UNI", "INJ", "SUR", "TOT", "SYM", "ASY", "TRN", "RFX", "IRF", "PROP", "ALWAYS"
                       , "RULE", "MESSAGE", "VIOLATION", "SRC", "TGT", "TEST"
                       , "RELATION", "MEANING", "DEFINE", "CONCEPT", "KEY", "TXT", "PRIMHTML"
                       , "IMPORT", "SPEC", "ISA", "I", "V"
                       , "PRAGMA", "EXPLAIN", "PURPOSE", "IN", "REF", "ENGLISH", "DUTCH"
                       , "REST", "HTML", "LATEX", "MARKDOWN"
                       , "ONE"
                       , "BYPLUG"
                       , "ROLE", "EDITS", "MAINTAINS"
                       ]
   keywordsops :: [String]
   keywordsops       = [ "-|", "|-", "-", "->", ">", "=", "~", "+", ";", "!", "*", "::", ":", "\\/", "/\\", "\\", "/", "<>"
                       , "..", "." , "0", "1"]
   specialchars :: String
   specialchars      = "()[],{}"
   opchars :: String
   opchars           = nub (sort (concat keywordsops))

   --to parse files containing only populations
   pPopulations :: Parser Token [P_Population]
   pPopulations = pList1 pPopulation

   pContext         :: Parser Token (P_Context, [String]) -- the result is the parsed context and a list of include filenames
   pContext  = rebuild <$> pKey_pos "CONTEXT" <*> pConid
                            <*> pList pIncludeStatement 
                            <*> optional pLanguageRef 
                            <*> optional pTextMarkup 
                            <*> pList pContextElement <* pKey "ENDCONTEXT"
     where
       rebuild :: Origin -> String -> [String] -> Maybe Lang -> Maybe PandocFormat -> [ContextElement] -> (P_Context, [String])
       rebuild pos' nm includeFileNames lang fmt ces = 
          (PCtx{ ctx_nm    = nm
               , ctx_pos   = [pos']
               , ctx_lang  = lang
               , ctx_markup= fmt
               , ctx_thms  = (nub.concat) [xs | CThm xs<-ces] -- Names of patterns/processes to be printed in the functional specification. (For partial documents.)
               , ctx_pats  = [p | CPat p<-ces]       -- The patterns defined in this context
               , ctx_PPrcs = [p | CPrc p<-ces]       -- The processes as defined by the parser
               , ctx_rs    = [p | CRul p<-ces]       -- All user defined rules in this context, but outside patterns and outside processes
               , ctx_ds    = [p | CRel p<-ces]       -- The declarations defined in this context, outside the scope of patterns
               , ctx_cs    = [c | CCon c<-ces]       -- The concept definitions defined in this context, outside the scope of patterns
               , ctx_gs    = [g | CGen g<-ces]       -- The gen definitions defined in this context, outside the scope of patterns
               , ctx_ks    = [k | CKey k<-ces]       -- The key definitions defined in this context, outside the scope of patterns
               , ctx_ifcs  = [s | Cifc s<-ces]       -- The interfaces defined in this context, outside the scope of patterns
               , ctx_sql   = [p | CSqlPlug p<-ces]   -- user defined sqlplugs, taken from the Ampersand scriptplug<-ces]  
               , ctx_php   = [p | CPhpPlug p<-ces]   -- user defined phpplugs, taken from the Ampersand script
               , ctx_ps    = [e | CPrp e<-ces]       -- The purposes defined in this context, outside the scope of patterns
               , ctx_pops  = [p | CPop p<-ces]       -- The populations defined in this contextplug<-ces]  
               , ctx_metas = [m | CMeta m <-ces]
               , ctx_experimental = False -- is set in Components.hs
               }
          , includeFileNames)

       pContextElement :: Parser Token ContextElement
       pContextElement = CMeta    <$> pMeta         <|>
                         CPat     <$> pPatternDef   <|>
                         CPrc     <$> pProcessDef   <|>
                         CRul     <$> pRuleDef      <|>
                         CRel     <$> pRelationDef  <|>
                         CCon     <$> pConceptDef   <|>
                         CGen     <$> pGenDef       <|>
                         CKey     <$> pKeyDef       <|>
                         Cifc     <$> pInterface    <|>
                         CSqlPlug <$> pSqlplug      <|>
                         CPhpPlug <$> pPhpplug      <|>
                         CPrp     <$> pPurpose      <|>
                         CPop     <$> pPopulation   <|>
                         CThm     <$> pPrintThemes


   data ContextElement = CMeta P_Meta
                       | CPat P_Pattern
                       | CPrc P_Process
                       | CRul P_Rule
                       | CRel P_Declaration
                       | CCon ConceptDef
                       | CGen P_Gen
                       | CKey P_KeyDef
                       | Cifc P_Interface
                       | CSqlPlug P_ObjectDef
                       | CPhpPlug P_ObjectDef
                       | CPrp PPurpose
                       | CPop P_Population
                       | CThm [String]    -- a list of themes to be printed in the functional specification. These themes must be PATTERN or PROCESS names.

   pIncludeStatement :: Parser Token String
   pIncludeStatement = pKey "INCLUDE" *> pString

   pLanguageRef :: Parser Token Lang
   pLanguageRef = pKey "IN" *> 
                  (( Dutch   <$ pKey "DUTCH"  ) <|>
                   ( English <$ pKey "ENGLISH")
                  )
   pTextMarkup :: Parser Token PandocFormat
   pTextMarkup = ( ReST     <$ pKey "REST"     ) <|>
                 ( HTML     <$ pKey "HTML"     ) <|>
                 ( LaTeX    <$ pKey "LATEX"    ) <|>
                 ( Markdown <$ pKey "MARKDOWN" )

   pMeta :: Parser Token P_Meta
   pMeta = P_Meta <$> pKey_pos "META" <*> pMetaObj <*> pString <*> pString
    where pMetaObj = pSucceed ContextMeta -- for the context meta we don't need a keyword
    
   pPatternDef    :: Parser Token P_Pattern
   pPatternDef = rebuild <$> pKey_pos "PATTERN" <*> (pConid <|> pString)
                         <*> pList pPatElem
                         <*> pKey_pos "ENDPATTERN"
     where
       rebuild :: Origin -> String -> [PatElem] -> Origin -> P_Pattern
       rebuild pos' nm pes end
        = P_Pat { pt_nm  = nm
                , pt_pos = pos'
                , pt_end = end
                , pt_rls = [r | Pr r<-pes]
                , pt_gns = [g | Pg g<-pes]
                , pt_dcs = [d | Pd d<-pes]
                , pt_cds = [c | Pc c<-pes]
                , pt_kds = [k | Pk k<-pes]
                , pt_xps = [e | Pe e<-pes]
                , pt_pop = [p | Pp p<-pes]
                } 
       pPatElem :: Parser Token PatElem
       pPatElem = Pr <$> pRuleDef      <|>
                  Pd <$> pRelationDef  <|>
                  Pc <$> pConceptDef   <|>
                  Pg <$> pGenDef       <|>
                  Pk <$> pKeyDef       <|>
                  Pe <$> pPurpose      <|>
                  Pp <$> pPopulation

   data PatElem = Pr P_Rule
                | Pd P_Declaration 
                | Pc ConceptDef
                | Pg P_Gen
                | Pk P_KeyDef
                | Pe PPurpose
                | Pp P_Population
                              
   pProcessDef :: Parser Token P_Process
   pProcessDef = rebuild <$> pKey_pos "PROCESS" <*> (pConid <|> pString)
                         <*> pList pProcElem
                         <*> pKey_pos "ENDPROCESS"
      where
       rebuild :: Origin -> String -> [ProcElem] -> Origin -> P_Process
       rebuild pos' nm pes end
         = P_Prc { procNm    = nm
                 , procPos   = pos'
                 , procEnd   = end
                 , procRules = [rr | PrR rr<-pes]
                 , procGens  = [g  | PrG g <-pes]
                 , procDcls  = [d  | PrD d <-pes]
                 , procRRuls = [rr | PrM rr<-pes]
                 , procRRels = [rr | PrL rr<-pes]
                 , procCds   = [cd | PrC cd<-pes]
                 , procKds   = [kd | PrK kd<-pes]
                 , procXps   = [e  | PrE e <-pes]
                 , procPop   = [p  | PrP p <-pes]
                 }
       pProcElem :: Parser Token ProcElem
       pProcElem = PrR <$> pRuleDef      <|>
                   PrD <$> pRelationDef  <|>
                   PrM <$> pRoleRule     <|>
                   PrL <$> pRoleRelation <|>
                   PrC <$> pConceptDef   <|>
                   PrG <$> pGenDef       <|>
                   PrK <$> pKeyDef       <|>
                   PrE <$> pPurpose      <|>
                   PrP <$> pPopulation

   data ProcElem = PrR P_Rule
                 | PrD P_Declaration
                 | PrM RoleRule
                 | PrL P_RoleRelation
                 | PrC ConceptDef
                 | PrG P_Gen
                 | PrK P_KeyDef
                 | PrE PPurpose
                 | PrP P_Population

   pRuleDef :: Parser Token P_Rule
   pRuleDef = rebuild <$> pKey_pos "RULE"
                      <*> optional (pADLid <* pKey ":" )
                      <*> pExpr
                      <*> pList pMeaning                 
                      <*> pList pMessage
                      <*> optional pViolation
                 where
                   rebuild po mn rexp mean msg mViolation
                     = P_Ru { rr_nm   = fromMaybe (rulid po) mn
                            , rr_exp  = rexp
                            , rr_fps  = po
                            , rr_mean = mean
                            , rr_msg  = msg
                            , rr_viol = mViolation
                            }
                   rulid (FileLoc(FilePos (_,Pos l _,_))) = "rule@line"++show l
                   rulid _ = fatal 226 "rulid is expecting a file location."
                   pMessage :: Parser Token P_Markup
                   pMessage = P_Markup <$ pKey "MESSAGE" 
                                      <*> optional pLanguageRef
                                      <*> optional pTextMarkup
                                      <*> (pString <|> pExpl)
                   pViolation :: Parser Token P_PairView
                   pViolation = id <$ pKey "VIOLATION" <*> pPairView

                   pPairView :: Parser Token P_PairView
                   pPairView = P_PairView <$ pSpec '(' <*> pList1Sep (pSpec ',') pPairViewSegment <* pSpec ')'
   
                   pPairViewSegment :: Parser Token P_PairViewSegment
                   pPairViewSegment = P_PairViewExp <$> pSrcOrTgt <*>  pExpr
                                  <|> P_PairViewText <$ pKey "TXT" <*> pString
   
   pSrcOrTgt :: Parser Token SrcOrTgt                    
   pSrcOrTgt = Src <$ pKey "SRC" <|> Tgt <$ pKey "TGT"


   pRelationDef     :: Parser Token P_Declaration
   pRelationDef      = ( rebuild <$> pVarid  <*> pKey_pos "::"  <*> pConceptRef  <*> pFun  <*> pConceptRef
                         <|>
                         rbd <$> pKey_pos "RELATION" <*> pVarid  <*> pSign
                       )
                         <*> ((True <$ pKey "BYPLUG") `opt` False)
                         <*> (pProps `opt` [])
                         <*> ((True <$ pKey "BYPLUG") `opt` False)
                         <*> (pPragma `opt` [])
                         <*> pList pMeaning
                         <*> ((\st d -> Just $ RelConceptDef st d) <$ pKey "DEFINE" <*> pSrcOrTgt <*> pString `opt` Nothing)
                         <*> ((pKey "=" *> pContent) `opt` [])
                         <* (pKey "." `opt` "")         -- in the syntax before 2011, a dot was required. This optional dot is there to save user irritation during the transition to a dotless era  :-) .
                       where rebuild nm pos' s fun' t bp1 props --bp2 pragma meanings conceptDef content
                               = rbd pos' nm (P_Sign [s,t]) bp1 props' --bp2 pragma meanings conceptDef content
                                 where props'= nub (props `uni` fun')
                             rbd pos' nm sgn bp1 props bp2 pragma meanings conceptDef content
                               = P_Sgn { dec_nm   = nm
                                       , dec_sign = sgn
                                       , dec_prps = props
                                       , dec_prL  = head pr
                                       , dec_prM  = pr!!1
                                       , dec_prR  = pr!!2
                                       , dec_Mean = meanings
                                       , dec_conceptDef = conceptDef
                                       , dec_popu = content
                                       , dec_fpos = pos'
                                       , dec_plug = bp1 || bp2
                                       }
                                 where pr = pragma++["","",""]

                             pProps :: Parser Token [Prop]
                             pProps  = (f.concat) <$> (pSpec '[' *> pListSep (pSpec ',') pProp <* pSpec ']')
                                 where f ps = nub (ps ++ concat [[Uni, Inj] | null ([Sym, Asy]>-ps)])
                             pProp  :: Parser Token [Prop]
                             pProp   = k [Uni] "UNI" <|> k [Inj] "INJ" <|> k [Sur] "SUR" <|> k [Tot] "TOT" <|>
                                       k [Sym] "SYM" <|> k [Asy] "ASY" <|> k [Trn] "TRN" <|>
                                       k [Rfx] "RFX" <|> k [Irf] "IRF" <|> k [Sym, Asy] "PROP"
                                 where k obj str = f <$> pKey str where f _ = obj
                             pPragma :: Parser Token [String]
                             pPragma = pKey "PRAGMA" *> pList1 pString
                             pFun    :: Parser Token [Prop]
                             pFun    = []        <$ pKey "*"  <|> 
                                       [Uni,Tot] <$ pKey "->" <|>
                                       (rbld     <$  pSpec '['  
                                                 <*> (pMult (Tot,Uni) `opt` [])
                                                 <*  pKey "-"
                                                 <*> (pMult (Sur,Inj) `opt` [])
                                                 <*  pSpec ']'
                                       )       
                                 where 
                                   pMult :: (Prop,Prop) -> Parser Token [Prop]
                                   pMult (ts,ui) = rbld  <$> (( []   <$ pKey "0") <|> ([ts] <$ pKey "1") ) 
                                                         <*  pKey ".."
                                                         <*> (( [ui] <$ pKey "1") <|> ([]   <$ pKey "*" )) <|>
                                                   [] <$ pKey "*"  <|>
                                                   [ts,ui] <$ pKey "1"
                                   rbld a b = a++b 
                                       
   pConceptDef      :: Parser Token ConceptDef
   pConceptDef       = Cd <$> pKey_pos "CONCEPT"
                          <*> (pConid <|> pString)   -- the concept name
                          <*> ((True <$ pKey "BYPLUG") `opt` False)
                          <*> pString                -- the definition text
                          <*> ((pKey "TYPE" *> pString) `opt` "")     -- the type of the concept.
                          <*> (pString `opt` "")     -- a reference to the source of this definition.

   pGenDef          :: Parser Token P_Gen
   pGenDef           = rebuild <$ pKey "SPEC" <*> (pConid <|> pString) <*> pKey_pos "ISA" <*> (pConid <|> pString)
                       where rebuild spc p gen = PGen p (PCpt gen) (PCpt spc)

   -- | A key definition looks like:   KEY Person(name, address),
   -- which means that name<>name~ /\ address<>addres~ |- I[Person].
   -- You may also use an expression on each attribute place, for example: KEY onpassport: Person(nationality, passport;documentnr),
   -- which means that nationality<>nationality~ /\ passport;documentnr<>(passport;documentnr)~ |- I[Person].
   -- For the sake of a proper user interface, you can assign labels to the attributes in a key, for example:
   -- KEY onSSN: Person("social security number":ssn)
   pKeyDef :: Parser Token P_KeyDef
   pKeyDef  = kd <$ pKey "KEY" <*> pLabelProps <*> pConceptRef <* pSpec '(' <*> pList1Sep (pSpec ',') pKeySegment <* pSpec ')'
       where kd :: Label -> P_Concept -> [P_KeySegment] -> P_KeyDef 
             kd (Lbl nm p _) c ats = P_Kd { kd_pos = p
                                          , kd_lbl = nm
                                          , kd_cpt = c
                                          , kd_ats = [ case keySeg of
                                                          P_KeyExp x       -> if null (obj_nm x) then P_KeyExp $ x{obj_nm=show i} else P_KeyExp x 
                                                          P_KeyText _ -> keySeg 
                                                          P_KeyHtml _ -> keySeg 
                                                     | (i,keySeg)<-zip [(1::Integer)..] ats]
                                          } -- nrs also count text segments but they're are not important anyway
             pKeySegment :: Parser Token P_KeySegment
             pKeySegment = P_KeyExp  <$> pKeyAtt <|> 
                           P_KeyText <$ pKey "TXT" <*> pString <|>
                           P_KeyHtml <$ pKey "PRIMHTML" <*> pString
             pKeyAtt :: Parser Token P_ObjectDef
             pKeyAtt = rebuild <$> optional pLabelProps <*> pExpr
                 where
                   rebuild mLbl attexpr =
                     case mLbl of
                       Just (Lbl nm p strs) ->
                               P_Obj { obj_nm   = nm
                                     , obj_pos  = p
                                     , obj_ctx  = attexpr 
                                     , obj_msub = Nothing
                                     , obj_strs = strs
                                     }
                       Nothing ->
                               P_Obj { obj_nm   = ""
                                     , obj_pos  = Origin "pKeyAtt CCv221.hs"
                                     , obj_ctx  = attexpr 
                                     , obj_msub = Nothing
                                     , obj_strs = []
                                     }
                       
   pInterface :: Parser Token P_Interface
   pInterface = lbl <$> (pKey "INTERFACE" *> pADLid_val_pos) <*>
                        (pParams `opt` [])                   <*>  
                        (pArgs   `opt` [])                   <*>  
                        (pRoles  `opt` [])                   <*>  
                        (pKey ":" *> pExpr)                  <*>  
                        pSubInterface
       where lbl :: (String, Origin) -> [(P_Relation,P_Sign)] -> [[String]] -> [String] -> P_Expression -> P_SubInterface -> P_Interface
             lbl (nm,p) params args roles expr sub
                = P_Ifc { ifc_Name   = nm
                        , ifc_Params = params
                        , ifc_Args   = args
                        , ifc_Roles  = roles
                        , ifc_Obj    = P_Obj { obj_nm   = nm    
                                             , obj_pos  = p
                                             , obj_ctx  = expr
                                             , obj_msub = Just sub
                                             , obj_strs = args
                                             }
                        , ifc_Pos    = p
                        , ifc_Expl   = ""   --TODO: Nothing in syntax defined for the purpose of the interface.
                                    }
             pParams = pSpec '(' *> pList1Sep (pSpec ',') pRelSign          <* pSpec ')' 
             pArgs   = pSpec '{' *> pList1Sep (pSpec ',') (pList1 pADLid)   <* pSpec '}'
             pRoles  = pKey "FOR" *> pList1Sep (pSpec ',') pADLid

   pSubInterface :: Parser Token P_SubInterface 
   pSubInterface = rebuild <$ pKey "INTERFACE" <*> pADLid_val_pos  
               <|> P_Box <$> pBox 
      where
        rebuild (n,p) = P_InterfaceRef p n

   pSqlplug         :: Parser Token P_ObjectDef
   pSqlplug          = pKey_pos "SQLPLUG" *> pObjDef

   pPhpplug         :: Parser Token P_ObjectDef
   pPhpplug          = pKey_pos "PHPPLUG" *> pObjDef

   pPurpose         :: Parser Token PPurpose
   pPurpose          = rebuild <$> ( pKey_pos "EXPLAIN" <|> pKey_pos "PURPOSE" )  -- "EXPLAIN" will become obsolete
                               <*> pRef2Obj
                               <*> optional pLanguageRef
                               <*> optional pTextMarkup
                               <*> ((pKey "REF" *> pString) `opt` [])
                               <*> pExpl      
        where
          rebuild orig obj lang fmt ref str
              = PRef2 orig obj (P_Markup lang fmt str) ref
          pRef2Obj :: Parser Token PRef2Obj
          pRef2Obj = PRef2ConceptDef  <$ pKey "CONCEPT"   <*> (pConid <|> pString) <|>
                     PRef2Declaration <$ pKey "RELATION"  <*> pRelSign             <|>
                     PRef2Rule        <$ pKey "RULE"      <*> pADLid               <|>
                     PRef2KeyDef      <$ pKey "KEY"       <*> pADLid               <|>  
                     PRef2Pattern     <$ pKey "PATTERN"   <*> pADLid               <|>
                     PRef2Process     <$ pKey "PROCESS"   <*> pADLid               <|>
                     PRef2Interface   <$ pKey "INTERFACE" <*> pADLid               <|>
                     PRef2Context     <$ pKey "CONTEXT"   <*> pADLid

   pPopulation :: Parser Token P_Population
   pPopulation = ppop <$ pKey "POPULATION" <*> pRelSign <* pKey "CONTAINS" <*> pContent
       where
         ppop (r,sgn) = P_Popu r sgn
         
   pRoleRelation    :: Parser Token P_RoleRelation
   pRoleRelation      = rr <$> pKey_pos "ROLE"              <*>
                               pList1Sep (pSpec ',') pADLid <*
                               pKey "EDITS"                 <*>
                               pList1Sep (pSpec ',') pRelSign
                        where rr p roles rels = P_RR roles rels p

   pRoleRule        :: Parser Token RoleRule
   pRoleRule         = rr <$> pKey_pos "ROLE"               <*>
                              pList1Sep (pSpec ',') pADLid  <*
                              pKey "MAINTAINS"              <*>
                              pList1Sep (pSpec ',') pADLid 
                       where rr p roles rulIds = Maintain roles rulIds p

   pPrintThemes :: Parser Token [String]
   pPrintThemes = pKey "THEMES" 
               *> pList1Sep (pSpec ',') (pConid <|> pString)

   pMeaning :: Parser Token PMeaning
   pMeaning = rebuild <$  pKey "MEANING" 
                      <*> optional pLanguageRef
                      <*> optional pTextMarkup
                      <*> (pString <|> pExpl)
      where rebuild lang fmt mkup =
               PMeaning (P_Markup lang fmt mkup)

                              
{-  Basically we would have the following expression syntax:
pExpr ::= pExp1   "="    pExp1                           |
          pExp1   "|-"   pExp1                           |
          pExp1 .
pExp1 ::= pList1Sep "/\\" pExp2                          |
          pList1Sep "\\/" pExp2                          |
          pExp2 .
pExp2 ::= pExp3    "-"    pExp3                          |
          pExp3 .
pExp3 ::= pExp4   "\\"   pExp4                           |
          pExp4   "/"    pExp4                           |
          pExp4 .
pExp4 ::= pList1Sep ";" pExp5                            |
          pList1Sep "!" pExp5                            |
          pList1Sep "*" pExp5                            |
          pExp5 .
pExp5 ::= "-"     pExp6                                  |
          pExp6   pSign                                  |
          pExp6   "~"                                    |
          pExp6   "*"                                    |
          pExp6   "+"                                    |
          pExp6 .
pExp6 ::= pRelation                                      |
          "("   pExpr   ")" .
In practice, we have it a little different.
 - In order to avoid "associative" brackets, we parse the associative operators "\/", "/\", ";", and "!" with pList1Sep. That works.
 - We would like the user to disambiguate between "=" and "|-" by using brackets. 
-}

{- In theory, the expression is parsed by:
   pExpr  :: Parser Token (P_Expression)
   pExpr  =  fEequ <$> pExp1  <*  pKey "="   <*>  pExp1   <|>
             fEimp <$> pExp1  <*  pKey "|-"  <*>  pExp1   <|>
             pExp1
             where fEequ lExp rExp = Pequ (lExp, rExp)
                   fEimp lExp rExp = Pimp (lExp, rExp)
-- However elegant, this solution needs to be left-factored in order to get a performant parser.
-}
   pExpr  :: Parser Token P_Expression
   pExpr  =  pExp1 <??> (f <$>  (pKey "=" <|> pKey "|-") <*> pExp1 )
             where f "="  rExp lExp = Pequ (lExp, rExp)
                   f _    rExp lExp = Pimp (lExp, rExp)
             
{- The union and intersect are parsed as lists. The obvious thing to do might be:
   pExp1  :: Parser Token P_Expression
   pExp1   = fEuni <$> pList1Sep (pKey "\\/") pExp1a
             where fEuni [x] = x
                   fEuni xs  = PUni xs

   pExp1a :: Parser Token P_Expression
   pExp1a  = fEisc <$> pList1Sep (pKey "/\\") pExp2
             where fEisc [x] = x
                   fEisc xs  = Pisc xs
However, we want intersect and union to be of equal precedence. It has to be left-factored
and the grammar must be disambiguated in order to get a performant parser...
-}

   pExp1  :: Parser Token P_Expression
   pExp1   = f <$> pExp2 <*> (pLuni <|> pLisc)
             where f x (PUni []) = x
                   f x (Pisc []) = x
                   f x (PUni xs) = PUni (x:xs)
                   f x (Pisc xs) = Pisc (x:xs)
                   f _ _ = fatal 284 "PUni Pisc expected"
                   pLuni = PUni <$> pList1 (pKey "\\/" *> pExp2)
                   pLisc = Pisc <$> pList  (pKey "/\\" *> pExp2)

-- The left factored version of difference:
   pExp2  :: Parser Token P_Expression
   pExp2   = pExp3 <??> (f <$> pKey "-" <*> pExp3)
             where f _ rExp lExp = PDif (lExp, rExp)

-- The left factored version of right- and left residuals:
   pExp3  :: Parser Token P_Expression
   pExp3  =  pExp4 <??> (f <$>  (pKey "\\" <|> pKey "/") <*> pExp4 )
             where f "\\" rExp lExp = PRrs (lExp, rExp)
                   f _    rExp lExp = PLrs (lExp, rExp)

-- composition and relational addition are associative, and parsed similar to union and intersect...
   pExp4  :: Parser Token P_Expression
   pExp4   = f <$> pExp5 <*> (pLrad <|> pLcps <|> pLprd)
             where f x (PCps []) = x
                   f x (PRad []) = x
                   f x (PPrd []) = x
                   f x (PCps xs) = PCps (x:xs)
                   f x (PRad xs) = PRad (x:xs)
                   f x (PPrd xs) = PPrd (x:xs)
                   f _ _ = fatal 301 "PRad PCps expected"
                   pLrad = PRad <$> pList1 (pKey "!" *> pExp5)
                   pLcps = PCps <$> pList1 (pKey ";" *> pExp5)
                   pLprd = PPrd <$> pList  (pKey "*" *> pExp5)

   pExp5  :: Parser Token P_Expression
   pExp5  =  f <$> pList (pKey "-") <*> pExp6  <*> pList ( pKey "~" <|> pKey "*" <|> pKey "+" )
             where f ms pe ("~":ps) = PFlp (f ms pe ps)
                   f ms pe ("*":ps) = PKl0 (f ms pe ps)
                   f ms pe ("+":ps) = PKl1 (f ms pe ps)
                   f ("-":ms) pe ps = PCpl (f ms pe ps)
                   f _ pe _         = pe

   pExp6  :: Parser Token P_Expression
   pExp6  =  f <$> pExp7 <*> optional pSign
             where f e Nothing = e
                   f e (Just sgn) = PTyp e sgn
-- Alternatively, this works too:    pExp6  =  pExp7 <??> (flip PTyp <$> pSign)

   pExp7  :: Parser Token P_Expression
   pExp7  =  Prel <$> pRelationRef                                                <|>
             PBrk <$  pSpec '('  <*>  pExpr  <*  pSpec ')'


   pRelationRef :: Parser Token P_Relation
   pRelationRef      = P_I   <$  pKey "I"   <|>
                       P_V   <$  pKey "V"   <|>
                       P_Mp1 <$> pAtom      <|>
                       rbld  <$> pVarid_val_pos
        where rbld (nm,pos') = P_Rel {rel_nm = nm, rel_pos = pos'}

   pRelSign         :: Parser Token (P_Relation, P_Sign)
   pRelSign          = f <$> pRelationRef 
                         <*> optional pSign
                        where f rel Nothing    = (rel,P_Sign [])
                              f rel (Just sgn) = (rel,sgn)
                                                                 
   pSign :: Parser Token P_Sign
   pSign = rebuild <$ pSpec '[' <*> pConceptRef <*> optional (pKey "*" *> pConceptRef) <* pSpec ']'
      where
        rebuild :: P_Concept -> Maybe P_Concept -> P_Sign
        rebuild a mb = case mb of 
                        Just b  -> P_Sign { psign = [a,b] }
                        Nothing -> P_Sign { psign = [a] }
   
   pConceptRef      :: Parser Token P_Concept
   pConceptRef       = (P_Singleton <$ pKey "ONE") 
                   <|> (PCpt <$> (pConid <|> pString))

-- BECAUSE:
--  (SJ) Waarom heeft een label (optioneel) strings?
--  (GM) Dit is bedoeld als binding mechanisme voor implementatiespecifieke (SQL/PHP plug,PHP web app,etc) properties
--  (SJ) Met het invoeren van referenties (t.b.v losse Explanations) bestaat er een variant met props en eentje zonder.
   pLabelProps      :: Parser Token Label
   pLabelProps       = lbl <$> pADLid_val_pos
                           <*> (pArgs `opt` [])
                           <*  pKey_pos ":"
                       where lbl :: (String, Origin) -> [[String]] -> Label
                             lbl (nm,pos')  = Lbl nm pos' 
                             pArgs = pSpec '{' *> pList1Sep (pSpec ',') (pList1 pADLid) <* pSpec '}'

   pObjDef          :: Parser Token P_ObjectDef
   pObjDef           = obj <$> pLabelProps
                           <*> pExpr            -- the context expression (for example: I[c])
                           <*> optional pSubInterface  -- the optional subinterface 
                       where obj (Lbl nm pos' strs) expr msub  = 
                               P_Obj { obj_nm   = nm
                                     , obj_pos  = pos'
                                     , obj_ctx  = expr
                                     , obj_msub = msub 
                                     , obj_strs = strs
                                     }
   pBox            :: Parser Token [P_ObjectDef]
   pBox            = pKey "BOX" *> pSpec '[' *> pList1Sep (pSpec ',') pObjDef <* pSpec ']'

   pContent         :: Parser Token Pairs
   pContent          = pSpec '[' *> pListSep pComma pRecord <* pSpec ']'
                   <|> pSpec '[' *> pListSep (pKey ";") pRecordObs <* pSpec ']' --obsolete
       where
       pRecord = mkPair<$> pValue <* pKey "*" <*> pValue
       pValue  = pAtom <|> pConid <|> pVarid <|> pInteger <|> ((++)<$>pInteger<*>pConid) <|> ((++)<$>pInteger<*>pVarid)
       pRecordObs = mkPair<$ pSpec '(' <*> (trim <$> pString)  <* pComma   <*> (trim <$> pString)  <* pSpec ')' --obsolete



   pADLid           :: Parser Token String
   pADLid            = pVarid <|> pConid <|> pString

   pADLid_val_pos   :: Parser Token (String, Origin)
   pADLid_val_pos    = pVarid_val_pos <|> pConid_val_pos <|> pString_val_pos

   optional :: (Sequence p, Alternative p) => p a -> p (Maybe a)
   optional a        = Just <$> a <|> pSucceed Nothing


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
   

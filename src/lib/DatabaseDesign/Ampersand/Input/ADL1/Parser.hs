{-# OPTIONS_GHC -Wall -fno-enable-rewrite-rules #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
module DatabaseDesign.Ampersand.Input.ADL1.Parser 
   (pContext, pPopulations,pTerm, keywordstxt, keywordsops, specialchars, opchars) where
   import DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner
            ( Token(..),TokenType(..),noPos
            , pKey,pConid,pString,pSpec,pAtom,pExpl,pVarid,pComma,pInteger,pSemi)
   import DatabaseDesign.Ampersand.Input.ADL1.UU_Parsing
            (Parser
            , (<$>) , (<$), (<*>), (<*), (*>), (<|>), (<??>)
            ,pList,pListSep,pList1,pList1Sep,pSym
            ,pSucceed
            ,opt, Sequence,Alternative, IsParser
            )
   import DatabaseDesign.Ampersand.Basics  (fatalMsg,Collection(..))
   import DatabaseDesign.Ampersand.Core.ParseTree    
   import Data.List
   import Data.Maybe
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "Input.ADL1.Parser"

--  The Ampersand scanner takes the file name (String) for documentation and error messaging.
--   scanner :: String -> String -> [Token]
--   scanner fn str = scan keywordstxt keywordsops specialchars opchars fn initPos str

   keywordstxt :: [String]
   keywordstxt       = [ "INCLUDE"
                       , "CONTEXT", "ENDCONTEXT", "EXTENDS", "THEMES"
                       , "META"
                       , "PATTERN", "ENDPATTERN"
                       , "PROCESS", "ENDPROCESS"
                       , "INTERFACE", "FOR", "BOX", "INITIAL", "SQLPLUG", "PHPPLUG", "TYPE"
                       , "POPULATION", "CONTAINS"
                       , "UNI", "INJ", "SUR", "TOT", "SYM", "ASY", "TRN", "RFX", "IRF", "PROP", "ALWAYS"
                       , "RULE", "MESSAGE", "VIOLATION", "SRC", "TGT", "TEST"
                       , "RELATION", "MEANING", "DEFINE", "CONCEPT", "IDENT"
                       , "VIEW", "TXT", "PRIMHTML"
                       , "KEY" -- HJO, 20130605: Obsolete. Only usefull as long as the old prototype generator is still in use.
                       , "IMPORT", "SPEC", "ISA", "IS", "I", "V"
                       , "CLASSIFY"
                       , "PRAGMA", "PURPOSE", "IN", "REF", "ENGLISH", "DUTCH"
                       , "REST", "HTML", "LATEX", "MARKDOWN"
                       , "ONE"
                       , "BYPLUG"
                       , "ROLE", "EDITS", "MAINTAINS"
                       ]
   keywordsops :: [String]
   keywordsops       = [ "|-", "-", "->", "<-", ">", "=", "~", "+", "*", ";", "!", "#", "::", ":", "\\/", "/\\", "\\", "/", "<>"
                       , "..", "." , "0", "1"]
   specialchars :: String
   specialchars      = "()[],{}"
   opchars :: String
   opchars           = nub (sort (concat keywordsops))

   --to parse files containing only populations
   pPopulations :: Parser Token [P_Population]
   pPopulations = pList1 pPopulation

   pContext :: Parser Token (P_Context, [String]) -- the result is the parsed context and a list of include filenames
   pContext  = rebuild <$> pKey_pos "CONTEXT" <*> pConceptName
                            <*> optional pLanguageRef 
                            <*> optional pTextMarkup 
                            <*> pList pContextElement <* pKey "ENDCONTEXT"
     where
       rebuild :: Origin -> String -> Maybe Lang -> Maybe PandocFormat -> [ContextElement] -> (P_Context, [String])
       rebuild    pos'      nm        lang          fmt                   ces
        = (PCtx{ ctx_nm     = nm
               , ctx_pos    = [pos']
               , ctx_lang   = lang
               , ctx_markup = fmt
               , ctx_thms   = (nub.concat) [xs | CThm xs<-ces] -- Names of patterns/processes to be printed in the functional specification. (For partial documents.)
               , ctx_pats   = [p | CPat p<-ces]       -- The patterns defined in this context
               , ctx_PPrcs  = [p | CPrc p<-ces]       -- The processes as defined by the parser
               , ctx_rs     = [p | CRul p<-ces]       -- All user defined rules in this context, but outside patterns and outside processes
               , ctx_ds     = [p | CRel p<-ces]       -- The relations defined in this context, outside the scope of patterns
               , ctx_cs     = [c ("CONTEXT "++nm) | CCon c<-ces]    -- The concept definitions defined in this context, outside the scope of patterns
               , ctx_gs     = [g | CGen g<-ces]       -- The gen definitions defined in this context, outside the scope of patterns
               , ctx_ks     = [k | CIndx k<-ces]      -- The identity definitions defined in this context, outside the scope of patterns
               , ctx_vs     = [v | CView v<-ces]      -- The view definitions defined in this context, outside the scope of patterns
               , ctx_ifcs   = [s | Cifc s<-ces]       -- The interfaces defined in this context, outside the scope of patterns -- fatal 78 ("Diagnostic: "++concat ["\n\n   "++show ifc | Cifc ifc<-ces])
               , ctx_sql    = [p | CSqlPlug p<-ces]   -- user defined sqlplugs, taken from the Ampersand scriptplug<-ces]  
               , ctx_php    = [p | CPhpPlug p<-ces]   -- user defined phpplugs, taken from the Ampersand script
               , ctx_ps     = [e | CPrp e<-ces]       -- The purposes defined in this context, outside the scope of patterns
               , ctx_pops   = [p | CPop p<-ces]       -- The populations defined in this contextplug<-ces]  
               , ctx_metas  = [m | CMeta m <-ces]
               }
          , [s | CIncl s<-ces]) -- the INCLUDE filenames

       pContextElement :: Parser Token ContextElement
       pContextElement = CMeta    <$> pMeta         <|>
                         CPat     <$> pPatternDef   <|>
                         CPrc     <$> pProcessDef   <|>
                         CRul     <$> pRuleDef      <|>
                         CCfy     <$> pClassify     <|>
                         CRel     <$> pRelationDef  <|>
                         CCon     <$> pConceptDef   <|>
                         CGen     <$> pGenDef       <|>
                         CIndx    <$> pIndex        <|>
                         CView    <$> pViewDef      <|>
                         Cifc     <$> pInterface    <|>
                         CSqlPlug <$> pSqlplug      <|>
                         CPhpPlug <$> pPhpplug      <|>
                         CPrp     <$> pPurpose      <|>
                         CPop     <$> pPopulation   <|>
                         CThm     <$> pPrintThemes  <|>
                         CIncl    <$> pIncludeStatement


   data ContextElement = CMeta Meta
                       | CPat P_Pattern
                       | CPrc P_Process
                       | CRul (P_Rule TermPrim)
                       | CCfy P_Gen
                       | CRel P_Declaration
                       | CCon (String->ConceptDef)
                       | CGen P_Gen
                       | CIndx P_IdentDef
                       | CView P_ViewDef
                       | Cifc P_Interface
                       | CSqlPlug P_ObjectDef
                       | CPhpPlug P_ObjectDef
                       | CPrp PPurpose
                       | CPop P_Population
                       | CThm [String]    -- a list of themes to be printed in the functional specification. These themes must be PATTERN or PROCESS names.
                       | CIncl String     -- an INCLUDE statement

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

   pMeta :: Parser Token Meta
   pMeta = Meta <$> pKey_pos "META" <*> pMetaObj <*> pString <*> pString
    where pMetaObj = pSucceed ContextMeta -- for the context meta we don't need a keyword
    
   pPatternDef :: Parser Token P_Pattern
   pPatternDef = rebuild <$> pKey_pos "PATTERN" <*> pConceptName   -- The name spaces of patterns, processes and concepts are shared.
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
                , pt_rus = [r | Pm r<-pes]
                , pt_res = [r | Pl r<-pes]
                , pt_cds = [c nm | Pc c<-pes]
                , pt_ids = [k | Pk k<-pes]
                , pt_vds = [v | Pv v<-pes]
                , pt_xps = [e | Pe e<-pes]
                , pt_pop = [p | Pp p<-pes]
                } 
       pPatElem :: Parser Token PatElem
       pPatElem = Pr <$> pRuleDef      <|>
                  Py <$> pClassify     <|>
                  Pd <$> pRelationDef  <|>
                  Pm <$> pRoleRule     <|>
                  Pl <$> pRoleRelation <|>
                  Pc <$> pConceptDef   <|>
                  Pg <$> pGenDef       <|>
                  Pk <$> pIndex        <|>
                  Pv <$> pViewDef      <|>
                  Pe <$> pPurpose      <|>
                  Pp <$> pPopulation

   data PatElem = Pr (P_Rule TermPrim)
                | Py P_Gen
                | Pd P_Declaration 
                | Pm RoleRule
                | Pl P_RoleRelation
                | Pc (String->ConceptDef)
                | Pg P_Gen
                | Pk P_IdentDef
                | Pv P_ViewDef
                | Pe PPurpose
                | Pp P_Population
                              
   pProcessDef :: Parser Token P_Process
   pProcessDef = rebuild <$> pKey_pos "PROCESS" <*> pConceptName   -- The name spaces of patterns, processes and concepts are shared.
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
                 , procCds   = [cd nm | PrC cd<-pes]
                 , procIds   = [ix | PrI ix<-pes]
                 , procVds   = [vd | PrV vd<-pes]
                 , procXps   = [e  | PrE e <-pes]
                 , procPop   = [p  | PrP p <-pes]
                 }
       pProcElem :: Parser Token ProcElem
       pProcElem = PrR <$> pRuleDef      <|>
                   PrY <$> pClassify     <|>
                   PrD <$> pRelationDef  <|>
                   PrM <$> pRoleRule     <|>
                   PrL <$> pRoleRelation <|>
                   PrC <$> pConceptDef   <|>
                   PrG <$> pGenDef       <|>
                   PrI <$> pIndex        <|>
                   PrV <$> pViewDef      <|>
                   PrE <$> pPurpose      <|>
                   PrP <$> pPopulation

   data ProcElem = PrR (P_Rule TermPrim)
                 | PrY P_Gen
                 | PrD P_Declaration
                 | PrM RoleRule
                 | PrL P_RoleRelation
                 | PrC (String->ConceptDef)
                 | PrG P_Gen
                 | PrI P_IdentDef
                 | PrV P_ViewDef
                 | PrE PPurpose
                 | PrP P_Population

   pClassify :: Parser Token P_Gen
   pClassify = rebuild <$> pKey_pos "CLASSIFY"
                       <*> pConceptRef
                       <*  pKey "IS"
                       <*> pCterm
                  where
                    rebuild po lhs rhs
                      = P_Cy { gen_spc  = lhs             --  Left hand side concept expression 
                             , gen_rhs  = rhs             --  Right hand side concept expression
                             , gen_fp   = po
                             }
                    pCterm  = f <$> pList1Sep (pKey "/\\") pCterm1
                    pCterm1 = g <$> pConceptRef                        <|>
                              h <$> (pSpec '(' *> pCterm <* pSpec ')')  -- brackets are allowed for educational reasons.
                    f ccs = concat ccs
                    g c = [c]
                    h cs = cs

   
   pRuleDef :: Parser Token (P_Rule TermPrim)
   pRuleDef =  rebuild <$> pKey_pos "RULE"
                       <*> optional (pADLid <* pKey ":" )
                       <*> pRule
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
                    rulid _ = fatal 226 "pRuleDef is expecting a file location."
                    pViolation :: Parser Token (PairView (Term TermPrim))
                    pViolation = id <$ pKey "VIOLATION" <*> pPairView
               
                    pPairView :: Parser Token (PairView (Term TermPrim))
                    pPairView = PairView <$ pSpec '(' <*> pList1Sep (pSpec ',') pPairViewSegment <* pSpec ')'
               
                    pPairViewSegment :: Parser Token (PairViewSegment (Term TermPrim))
                    pPairViewSegment = PairViewExp <$> pSrcOrTgt <*>  pTerm
                                   <|> PairViewText <$ pKey "TXT" <*> pString
   
   pSrcOrTgt :: Parser Token SrcOrTgt                    
   pSrcOrTgt = Src <$ pKey "SRC" <|> Tgt <$ pKey "TGT"


   pRelationDef :: Parser Token P_Declaration
   pRelationDef      = ( rebuild <$> pVarid  <*> pKey_pos "::"  <*> pConceptRef  <*> pFun  <*> pConceptRef
                         <|> rbd <$> pKey_pos "RELATION" <*> pVarid  <*> pSign
                       )
                         <*> ((True <$ pKey "BYPLUG") `opt` False)
                         <*> (pProps `opt` [])
                         <*> ((True <$ pKey "BYPLUG") `opt` False)
                         <*> (pPragma `opt` [])
                         <*> pList pMeaning
                         <*> ((\st d -> Just $ RelConceptDef st d) <$ pKey "DEFINE" <*> pSrcOrTgt <*> pString `opt` Nothing)
                         <*> ((pKey "=" *> pContent) `opt` [])
                         <* (pKey "." `opt` "")         -- in the syntax before 2011, a dot was required. This optional dot is there to save user irritation during the transition to a dotless era  :-) .
                       where rebuild nm pos' src fun' trg bp1 props --bp2 pragma meanings conceptDef content
                               = rbd pos' nm (P_Sign src trg,pos') bp1 props' --bp2 pragma meanings conceptDef content
                                 where props'= nub (props `uni` fun')
                             rbd pos' nm (sgn,_) bp1 props bp2 pragma meanings conceptDef content
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
                             pProp :: Parser Token [Prop]
                             pProp   = k [Uni] "UNI" <|> k [Inj] "INJ" <|> k [Sur] "SUR" <|> k [Tot] "TOT" <|>
                                       k [Sym] "SYM" <|> k [Asy] "ASY" <|> k [Trn] "TRN" <|>
                                       k [Rfx] "RFX" <|> k [Irf] "IRF" <|> k [Sym, Asy] "PROP"
                                 where k obj str = f <$> pKey str where f _ = obj
                             pPragma :: Parser Token [String]
                             pPragma = pKey "PRAGMA" *> pList1 pString
                             pFun :: Parser Token [Prop]
                             pFun    = []        <$ pKey "*"  <|> 
                                       [Uni,Tot] <$ pKey "->" <|>
                                       [Sur,Inj] <$ pKey "<-" <|>
                                       (rbld     <$  pSpec '['  
                                                 <*> (pMult (Sur,Inj) `opt` [])
                                                 <*  pKey "-"
                                                 <*> (pMult (Tot,Uni) `opt` [])
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
                                       
   pConceptDef :: Parser Token (String->ConceptDef)
   pConceptDef       = Cd <$> pKey_pos "CONCEPT"
                          <*> pConceptName           -- the concept name
                          <*> ((True <$ pKey "BYPLUG") `opt` False)
                          <*> pString                -- the definition text
                          <*> ((pKey "TYPE" *> pString) `opt` "")     -- the type of the concept.
                          <*> (pString `opt` "")     -- a reference to the source of this definition.

   pGenDef :: Parser Token P_Gen
   pGenDef           = rebuild <$> pKey_pos "SPEC"     <*> pConceptRef <* pKey "ISA" <*> pConceptRef <|>  -- SPEC is obsolete syntax. Should disappear!
                       rebuild <$> pKey_pos "CLASSIFY" <*> pConceptRef <* pKey "ISA" <*> pConceptRef <|>
                       pClassify
                       where rebuild p spc gen = PGen{gen_spc=spc, gen_gen=gen, gen_fp =p}

   -- | A identity definition looks like:   IDENT onNameAdress : Person(name, address),
   -- which means that name<>name~ /\ address<>addres~ |- I[Person].
   -- The label 'onNameAddress' is used to refer to this identity.
   -- You may also use an expression on each attribute place, for example: IDENT onpassport: Person(nationality, passport;documentnr),
   -- which means that nationality<>nationality~ /\ passport;documentnr<>(passport;documentnr)~ |- I[Person].
   pIndex :: Parser Token P_IdentDef
   pIndex  = identity <$ pKey "IDENT" <*> pLabel <*> pConceptRefPos <* pSpec '(' <*> pList1Sep (pSpec ',') pIndSegment <* pSpec ')'
       where identity :: Label -> (P_Concept, Origin) -> [P_IdentSegment] -> P_IdentDef 
             identity (Lbl nm _ _) (c, orig) ats
              = P_Id { ix_pos = orig
                     , ix_lbl = nm
                     , ix_cpt = c
                     , ix_ats = ats
                     }

             pIndSegment :: Parser Token P_IdentSegment
             pIndSegment = P_IdentExp <$> pIndAtt
             
             pIndAtt :: Parser Token P_ObjectDef
             pIndAtt  = attL <$> pLabelProps <*> pTerm <|>
                        att <$> pTerm
                 where attL (Lbl nm p strs) attexpr =
                          P_Obj { obj_nm   = nm
                                , obj_pos  = p
                                , obj_ctx  = attexpr 
                                , obj_msub = Nothing
                                , obj_strs = strs
                                }
                       att attexpr =
                           P_Obj { obj_nm   = ""
                                 , obj_pos  = Origin "pIndAtt CC664"
                                 , obj_ctx  = attexpr 
                                 , obj_msub = Nothing
                                 , obj_strs = []
                                 }

   -- | A view definition looks like:
   --      VIEW onSSN: Person("social security number":ssn)
   -- or
   --      VIEW SaveAdlFile: SaveAdlFile(PRIMHTML "<a href='../../index.php?operation=2&file=", filepath , filename
   --      ,PRIMHTML "&userrole=", savecontext~;sourcefile;uploaded~;userrole
   --      ,PRIMHTML "'>", filename/\V[SaveAdlFile*FileName], PRIMHTML "</a>")
   -- which can be used to define a proper user interface by assigning labels and markup to the attributes in a view.
   pViewDef :: Parser Token P_ViewDef
   pViewDef  = vd <$ (pKey "VIEW" <|> pKey "KEY") <*> pLabelProps <*> pConceptOneRefPos <* pSpec '(' <*> pList1Sep (pSpec ',') pViewSegment <* pSpec ')'
       where vd :: Label -> (P_Concept, Origin) -> [P_ViewSegment] -> P_ViewDef 
             vd (Lbl nm _ _) (c, orig) ats
                 = P_Vd { vd_pos = orig
                        , vd_lbl = nm
                        , vd_cpt = c
                        , vd_ats = [ case viewSeg of
                                        P_ViewExp x       -> if null (obj_nm x) then P_ViewExp $ x{obj_nm=show i} else P_ViewExp x 
                                        P_ViewText _ -> viewSeg 
                                        P_ViewHtml _ -> viewSeg 
                                   | (i,viewSeg)<-zip [(1::Integer)..] ats]
                        } -- nrs also count text segments but they're are not important anyway
             pViewSegment :: Parser Token P_ViewSegment
             pViewSegment = P_ViewExp  <$> pViewAtt <|> 
                            P_ViewText <$ pKey "TXT" <*> pString <|>
                            P_ViewHtml <$ pKey "PRIMHTML" <*> pString
             pViewAtt :: Parser Token P_ObjectDef
             pViewAtt = rebuild <$> optional pLabelProps <*> pTerm
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
                                     , obj_pos  = origin attexpr
                                     , obj_ctx  = attexpr 
                                     , obj_msub = Nothing
                                     , obj_strs = []
                                     }

   pInterface :: Parser Token P_Interface
   pInterface = lbl <$> (pKey "INTERFACE" *> pADLid_val_pos) <*>
                        (pParams `opt` [])                   <*>       -- a list of expressions, which say which relations are editable within this service.
                                                                       -- either  Prel _ nm
                                                                       --       or  PTrel _ nm sgn
                        (pArgs   `opt` [])                   <*>  
                        (pRoles  `opt` [])                   <*>  
                        (pKey ":" *> pTerm)                  <*>  
                        pSubInterface
       where lbl :: (String, Origin) -> [TermPrim] -> [[String]] -> [String] -> (Term TermPrim) -> P_SubInterface -> P_Interface
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
                        , ifc_Prp    = ""   --TODO: Nothing in syntax defined for the purpose of the interface.
                        }
             pParams = pSpec '(' *> pList1Sep (pSpec ',') pRelSign          <* pSpec ')' 
             pArgs   = pSpec '{' *> pList1Sep (pSpec ',') (pList1 pADLid)   <* pSpec '}'
             pRoles  = pKey "FOR" *> pList1Sep (pSpec ',') pADLid

   pSubInterface :: Parser Token P_SubInterface 
   pSubInterface = P_Box <$> pKey_pos "BOX" <*> pBox 
                   <|> rebuild <$ pKey "INTERFACE" <*> pADLid_val_pos  
      where
        rebuild (n,p) = P_InterfaceRef p n

   pObjDef :: Parser Token P_ObjectDef
   pObjDef            = obj <$> pLabelProps
                            <*> pTerm            -- the context expression (for example: I[c])
                            <*> optional pSubInterface  -- the optional subinterface 
                        where obj (Lbl nm pos' strs) expr msub  = 
                                P_Obj { obj_nm   = nm
                                      , obj_pos  = pos'
                                      , obj_ctx  = expr
                                      , obj_msub = msub 
                                      , obj_strs = strs
                                      }
   pBox :: Parser Token [P_ObjectDef]
   pBox              = pSpec '[' *> pList1Sep (pSpec ',') pObjDef <* pSpec ']'

   pSqlplug :: Parser Token P_ObjectDef
   pSqlplug          = pKey_pos "SQLPLUG" *> pObjDef

   pPhpplug :: Parser Token P_ObjectDef
   pPhpplug          = pKey_pos "PHPPLUG" *> pObjDef

   pPurpose :: Parser Token PPurpose
   pPurpose          = rebuild <$> pKey_pos "PURPOSE"  -- "EXPLAIN" has become obsolete
                               <*> pRef2Obj
                               <*> optional pLanguageRef
                               <*> optional pTextMarkup
                               <*> ((pKey "REF" *> (pList1Sep pSemi pString)) `opt` [])
                               <*> pExpl      
        where
          rebuild :: Origin -> PRef2Obj -> Maybe Lang -> Maybe PandocFormat -> [String] -> String -> PPurpose
          rebuild    orig      obj         lang          fmt                   refs       str
              = PRef2 orig obj (P_Markup lang fmt str) (concatMap (splitOn ";") refs)
                 where splitOn :: Eq a => [a] -> [a] -> [[a]]
                       splitOn [] s = [s]
                       splitOn s t  = case findIndex (isPrefixOf s) (tails t) of
                                        Nothing -> [t]
                                        Just i  -> [take i t]  ++ splitOn s (drop (i+length s) t)

          pRef2Obj :: Parser Token PRef2Obj
          pRef2Obj = PRef2ConceptDef  <$ pKey "CONCEPT"   <*> pConceptName <|>
                     PRef2Declaration <$ pKey "RELATION"  <*> pRelSign     <|>
                     PRef2Rule        <$ pKey "RULE"      <*> pADLid       <|>
                     PRef2IdentityDef <$ pKey "IDENT"     <*> pADLid       <|>  
                     PRef2ViewDef     <$ pKey "VIEW"      <*> pADLid       <|>  
                     PRef2Pattern     <$ pKey "PATTERN"   <*> pADLid       <|>
                     PRef2Process     <$ pKey "PROCESS"   <*> pADLid       <|>
                     PRef2Interface   <$ pKey "INTERFACE" <*> pADLid       <|>
                     PRef2Context     <$ pKey "CONTEXT"   <*> pADLid

   pPopulation :: Parser Token P_Population
   pPopulation = prelpop <$> pKey_pos "POPULATION" <*> pRelSign     <* pKey "CONTAINS" <*> pContent <|>
                 pcptpop <$> pKey_pos "POPULATION" <*> pConceptName <* pKey "CONTAINS" <*> (pSpec '[' *> pListSep pComma pValue <* pSpec ']')
       where
         prelpop :: Origin -> TermPrim -> Pairs -> P_Population
         prelpop    orig     (Prel _ nm)  contents
          = P_RelPopu { p_rnme   = nm
                      , p_orig   = orig
                      , p_popps  = contents
                      }
         prelpop orig (PTrel _ nm sgn) contents
          = P_TRelPop { p_rnme   = nm
                      , p_type   = sgn
                      , p_orig   = orig
                      , p_popps  = contents
                      }
         prelpop _ expr _ = fatal 429 ("Expression "++show expr++" should never occur in prelpop.")
         pcptpop :: Origin -> String -> [String] -> P_Population
         pcptpop    orig      cnm       contents 
          = P_CptPopu { p_cnme   = cnm
                      , p_orig   = orig
                      , p_popas  = contents
                      }

   pRoleRelation :: Parser Token P_RoleRelation
   pRoleRelation      = rr <$> pKey_pos "ROLE"              <*>
                               pList1Sep (pSpec ',') pADLid <*
                               pKey "EDITS"                 <*>
                               pList1Sep (pSpec ',') pRelSign
                        where rr p roles rels = P_RR roles rels p

   pRoleRule :: Parser Token RoleRule
   pRoleRule         = rr <$> pKey_pos "ROLE"               <*>
                              pList1Sep (pSpec ',') pADLid  <*
                              pKey "MAINTAINS"              <*>
                              pList1Sep (pSpec ',') pADLid 
                       where rr p roles rulIds = Maintain roles rulIds p

   pPrintThemes :: Parser Token [String]
   pPrintThemes = pKey "THEMES" 
               *> pList1Sep (pSpec ',') pConceptName  -- Patterns, processes and concepts share the same name space, so these names must be checked whether the processes and patterns exist.
   pMeaning :: Parser Token PMeaning
   pMeaning = rebuild <$  pKey "MEANING" 
                      <*> optional pLanguageRef
                      <*> optional pTextMarkup
                      <*> (pString <|> pExpl)
      where rebuild :: Maybe Lang -> Maybe PandocFormat -> String -> PMeaning
            rebuild    lang          fmt                   mkup   =
               PMeaning (P_Markup lang fmt mkup)
   pMessage :: Parser Token PMessage
   pMessage = rebuild <$ pKey "MESSAGE" 
                      <*> optional pLanguageRef
                      <*> optional pTextMarkup
                      <*> (pString <|> pExpl)
      where rebuild :: Maybe Lang -> Maybe PandocFormat -> String -> PMessage
            rebuild    lang          fmt                   mkup   =
               PMessage (P_Markup lang fmt mkup)
                              
{-  Basically we would have the following expression syntax:
pRule ::= pTrm1   "="    pTerm                           |  -- equivalence
          pTrm1   "|-"   pTerm                           |  -- implication or subset
          pTrm1 .
pTerm ::= pList1Sep "/\\" pTrm2                          |  -- intersection
          pList1Sep "\\/" pTrm2                          |  -- union
          pTrm2 .
pTrm2 ::= pTrm3    "-"    pTrm3                          |  -- set difference
          pTrm3 .
pTrm3 ::= pTrm4   "\\"   pTrm4                           |  -- right residual
          pTrm4   "/"    pTrm4                           |  -- left residual
          pTrm4 .
pTrm4 ::= pList1Sep ";" pTrm5                            |  -- composition       (semicolon)
          pList1Sep "!" pTrm5                            |  -- relative addition (dagger)
          pList1Sep "#" pTrm5                            |  -- cartesian product (asterisk)
          pTrm5 .
pTrm5 ::= "-"     pTrm6                                  |  -- unary complement
          pTrm6   pSign                                  |  -- unary type cast
          pTrm6   "~"                                    |  -- unary flip
          pTrm6   "*"                                    |  -- unary Kleene star
          pTrm6   "+"                                    |  -- unary Kleene plus
          pTrm6 .
pTrm6 ::= pRelation                                      |
          "("   pTerm   ")" .
In practice, we have it a little different.
 - In order to avoid "associative" brackets, we parse the associative operators "\/", "/\", ";", and "!" with pList1Sep. That works.
 - We would like the user to disambiguate between "=" and "|-" by using brackets. 
-}


{- In theory, the expression is parsed by:
   pRule :: Parser Token (Term TermPrim)
   pRule  =  fEequ <$> pTrm1  <*>  pKey_pos "="   <*>  pTerm   <|>
             fEimp <$> pTrm1  <*>  pKey_pos "|-"  <*>  pTerm   <|>
             pTrm1
             where fequ  lExp orig rExp = Pequ orig lExp rExp
                   fEimp lExp orig rExp = Pimp orig lExp rExp
-- However elegant, this solution needs to be left-factored in order to get a performant parser.
-}
   pRule :: Parser Token (Term TermPrim)
   pRule  =  pTerm <??> (fEqu  <$> pKey_pos "="  <*> pTerm <|>
                         fImpl <$> pKey_pos "|-" <*> pTerm )
             where fEqu  orig rExp lExp = Pequ orig lExp rExp
                   fImpl orig rExp lExp = Pimp orig lExp rExp

{-
   pTrm1 is slightly more complicated, for the purpose of avoiding "associative" brackets.
   The idea is that each operator ("/\\" or "\\/") can be parsed as a sequence without brackets.
   However, as soon as they are combined, brackets are needed to disambiguate the combination.
   There is no natural precedence of one operator over the other.
   Brackets are enforced by parsing the subexpression as pTrm5.
   In order to maintain performance standards, the parser is left factored.
   The functions pars and f have arguments 'combinator' and 'operator' only to avoid writing the same code twice.
-}
   pTerm :: Parser Token (Term TermPrim)
   pTerm   = pTrm2 <??> (f PIsc <$> pars PIsc "/\\" <|> f PUni <$> pars PUni "\\/")
             where pars combinator operator
                    = g <$> pKey_pos operator <*> pTrm2 <*> optional (pars combinator operator)
                             where g orig y Nothing  = (orig, y)
                                   g orig y (Just (org,z)) = (orig, combinator org y z)
                   f combinator (orig, y) x = combinator orig x y

-- The left factored version of difference: (Actually, there is no need for left-factoring here, but no harm either)
   pTrm2 :: Parser Token (Term TermPrim)
   pTrm2   = pTrm3 <??> (f <$> pKey_pos "-" <*> pTrm3)
             where f orig rExp lExp = PDif orig lExp rExp

-- The left factored version of right- and left residuals:
   pTrm3 :: Parser Token (Term TermPrim)
   pTrm3  =  pTrm4 <??> (fLrs <$> pKey_pos "/" <*> pTrm4 <|> fRrs <$> pKey_pos "\\"  <*> pTrm4 <|> fDia <$> pKey_pos "<>" <*> pTrm4 )
             where fLrs orig rExp lExp = PLrs orig lExp rExp
                   fRrs orig rExp lExp = PRrs orig lExp rExp
                   fDia orig rExp lExp = PDia orig lExp rExp

{- by the way, a slightly different way of getting exactly the same result is:
   pTrm3 :: Parser Token (Term TermPrim)
   pTrm3  =  pTrm4 <??> (f <$>  (pKey_val_pos "/" <|> pKey_val_pos "\\" <|> pKey_val_pos "<>") <*> pTrm4 )
             where f ("\\", orig) rExp lExp = PRrs orig lExp rExp
                   f ("/" , orig) rExp lExp = PLrs orig lExp rExp
                   f (_   , orig) rExp lExp = PDia orig lExp rExp
-}

-- composition and relational addition are associative, and parsed similar to union and intersect...
   pTrm4 :: Parser Token (Term TermPrim)
   pTrm4   = pTrm5 <??> (f PCps <$> pars PCps ";" <|> f PRad <$> pars PRad "!" <|> f PPrd <$> pars PPrd "#")
             where pars combinator operator
                    = g <$> pKey_pos operator <*> pTrm5 <*> optional (pars combinator operator)
                             where g orig y Nothing  = (orig, y)
                                   g orig y (Just (org,z)) = (orig, combinator org y z)
                   f combinator (orig, y) x = combinator orig x y

   pTrm5 :: Parser Token (Term TermPrim)
   pTrm5  =  f <$> pList (pKey_val_pos "-") <*> pTrm6  <*> pList ( pKey_val_pos "~" <|> pKey_val_pos "*" <|> pKey_val_pos "+" )
             where f ms pe (("~",_):("~",_):ps) = f ms pe ps                     -- e~  converse       (flip, wok)
                   f ms pe (("~",_):ps) = let x=f ms pe ps in PFlp (origin x) x  -- the type checker requires that the origin of x is equal to the origin of its converse.
                   f ms pe (("*",orig):ps) = PKl0 orig (f ms pe ps)              -- e*  Kleene closure (star)
                   f ms pe (("+",orig):ps) = PKl1 orig (f ms pe ps)              -- e+  Kleene closure (plus)
                   f (_:_:ms) pe ps        = f ms pe ps                          -- -e  complement     (unary minus)
                   f ((_,orig):ms) pe ps   = let x=f ms pe ps in PCpl orig x     -- the type checker requires that the origin of x is equal to the origin of its complement.
                   f _ pe _                = pe

   pTrm6 :: Parser Token (Term TermPrim)
   pTrm6  =  (Prim <$> pRelationRef)  <|>
             PBrk <$>  pSpec_pos '('  <*>  pTerm  <*  pSpec ')'

   pRelationRef :: Parser Token TermPrim
   pRelationRef      = pRelSign                                                                         <|>
                       pid   <$> pKey_pos "I"  <*> optional (pSpec '[' *> pConceptOneRef <* pSpec ']')  <|>
                       pfull <$> pKey_pos "V"  <*> optional pSign                                       <|>
                       singl <$> pAtom_val_pos <*> optional (pSpec '[' *> pConceptOneRef <* pSpec ']')
                       where pid orig Nothing = PI orig
                             pid orig (Just c)= Pid orig c
                             pfull orig Nothing = PVee orig
                             pfull orig (Just (P_Sign src trg, _)) = Pfull orig src trg
                             singl (nm,orig) x  = Patm orig nm x

   pRelSign :: Parser Token TermPrim
   pRelSign          = prel  <$> pVarid_val_pos <*> optional pSign
                       where prel (nm,orig) Nothing = Prel orig nm
                             prel (nm,_) (Just (sgn,orig)) = PTrel orig nm sgn
                                                                 
   pSign :: Parser Token (P_Sign,Origin)
   pSign = rebuild <$> pSpec_pos '[' <*> pConceptOneRef <*> optional (pKey "*" *> pConceptOneRef) <* pSpec ']'
      where
        rebuild :: Origin -> P_Concept -> Maybe P_Concept -> (P_Sign,Origin)
        rebuild orig a mb
         = case mb of 
             Just b  -> (P_Sign a b, orig)
             Nothing -> (P_Sign a a, orig)
   
   pConceptName ::   Parser Token String
   pConceptName    = pConid <|> pString

   pConceptRef ::    Parser Token P_Concept
   pConceptRef     = PCpt <$> pConceptName

   pConceptOneRef :: Parser Token P_Concept
   pConceptOneRef  = (P_Singleton <$ pKey "ONE") <|> pConceptRef

   pConceptRefPos :: Parser Token (P_Concept, Origin)
   pConceptRefPos     = conid <$> pConid_val_pos   <|>   conid <$> pString_val_pos
                        where conid :: (String, Origin) ->  (P_Concept, Origin)
                              conid (c,orig) = (PCpt c, orig)

   pConceptOneRefPos :: Parser Token (P_Concept, Origin)
   pConceptOneRefPos  = singl <$> pKey_pos "ONE"   <|>   conid <$> pConid_val_pos   <|>   conid <$> pString_val_pos
                        where singl :: Origin ->  (P_Concept, Origin)
                              singl orig     = (P_Singleton, orig)
                              conid :: (String, Origin) ->  (P_Concept, Origin)
                              conid (c,orig) = (PCpt c, orig)

--  (SJ) Why does a label have (optional) strings?
--  (GM) This is a binding mechanism for implementation specific properties, such as SQL/PHP plug,PHP web app,etc.
--  (SJ April 15th, 2013) Since KEY has been replaced by IDENT and VIEW, there is a variant with props  (pLabelProps) and one without (pLabel).
   pLabelProps :: Parser Token Label
   pLabelProps       = lbl <$> pADLid_val_pos
                           <*> (pArgs `opt` [])
                           <*  pKey_pos ":"
                       where lbl :: (String, Origin) -> [[String]] -> Label
                             lbl (nm,pos') strs = Lbl nm pos' strs
                             pArgs = pSpec '{' *> pList1Sep (pSpec ',') (pList1 pADLid) <* pSpec '}'

   pLabel :: Parser Token Label
   pLabel       = lbl <$> pADLid_val_pos <*  pKey ":"
                  where lbl :: (String, Origin) -> Label
                        lbl (nm,pos') = Lbl nm pos' []

   pContent :: Parser Token Pairs
   pContent          = pSpec '[' *> pListSep pComma pRecord <* pSpec ']'
                   <|> pSpec '[' *> pListSep (pKey ";") pRecordObs <* pSpec ']' --obsolete
       where
       pRecord = mkPair<$> pValue <* pKey "*" <*> pValue
       pRecordObs = mkPair<$ pSpec '(' <*> pString <* pComma   <*> pString <* pSpec ')' --obsolete
   pValue :: Parser Token String
   pValue  = pAtom <|> pConid <|> pVarid <|> pInteger <|> ((++)<$>pInteger<*>pConid) <|> ((++)<$>pInteger<*>pVarid)



   pADLid :: Parser Token String
   pADLid            = pVarid <|> pConid <|> pString

   pADLid_val_pos :: Parser Token (String, Origin)
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

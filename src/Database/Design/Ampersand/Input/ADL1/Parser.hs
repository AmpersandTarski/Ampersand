{-# OPTIONS_GHC -fno-enable-rewrite-rules #-} -- Disable rewrite rules to drastically improve compilation speed
{-# LANGUAGE FlexibleContexts #-}
module Database.Design.Ampersand.Input.ADL1.Parser(
    AmpParser, pContext, pPopulations, pTerm, pRule
) where

import Database.Design.Ampersand.Basics (fatalMsg)
import Database.Design.Ampersand.Core.ParseTree
import Database.Design.Ampersand.Input.ADL1.ParsingLib
import Data.List
import Data.Maybe

--TODO! After converting the parser to Parsec, we had to add some try-calls.
--We gotta check the try's to see if we can refactor them, or at least pay attention to the error messages.

fatal :: Int -> String -> a
fatal = fatalMsg "Input.ADL1.Parser"

--to parse files containing only populations
--- Populations ::= Population+
pPopulations :: AmpParser [P_Population]
pPopulations = many1 pPopulation

--- Context ::= 'CONTEXT' ConceptName LanguageRef TextMarkup? ContextElement* 'ENDCONTEXT'
pContext :: AmpParser (P_Context, [String]) -- the result is the parsed context and a list of include filenames
pContext  = rebuild <$> posOf (pKey "CONTEXT") <*> pConceptName
                         <*> pLanguageRef
                         <*> pMaybe pTextMarkup
                         <*> many pContextElement <* pKey "ENDCONTEXT"
  where
    rebuild :: Origin -> String -> Lang -> Maybe PandocFormat -> [ContextElement] -> (P_Context, [String])
    rebuild    pos       nm        lang          fmt                   ces
     = (PCtx{ ctx_nm     = nm
            , ctx_pos    = [pos]
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
            , ctx_rrules = []  -- TODO: Allow MAINTAINS statements in the context
            , ctx_rrels  = []  -- TODO: Allow EDITS statements in the context
            , ctx_vs     = [v | CView v<-ces]      -- The view definitions defined in this context, outside the scope of patterns
            , ctx_ifcs   = [s | Cifc s<-ces]       -- The interfaces defined in this context, outside the scope of patterns -- fatal 78 ("Diagnostic: "++concat ["\n\n   "++show ifc | Cifc ifc<-ces])
            , ctx_sql    = [p | CSqlPlug p<-ces]   -- user defined sqlplugs, taken from the Ampersand scriptplug<-ces]
            , ctx_php    = [p | CPhpPlug p<-ces]   -- user defined phpplugs, taken from the Ampersand script
            , ctx_ps     = [e | CPrp e<-ces]       -- The purposes defined in this context, outside the scope of patterns
            , ctx_pops   = [p | CPop p<-ces]       -- The populations defined in this contextplug<-ces]
            , ctx_metas  = [meta | CMeta meta <-ces]
            }
       , [s | CIncl s<-ces]) -- the INCLUDE filenames

    --- ContextElement ::= Meta | PatternDef | ProcessDef | RuleDef | Classify | RelationDef | ConceptDef | GenDef | Index | ViewDef | Interface | Sqlplug | Phpplug | Purpose | Population | PrintThemes | IncludeStatement
    pContextElement :: AmpParser ContextElement
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
                    | CPrc P_Pattern
                    | CRul (P_Rule TermPrim)
                    | CCfy P_Gen
                    | CRel P_Declaration
                    | CCon (String -> ConceptDef)
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

--- IncludeStatement ::= 'INCLUDE' String
pIncludeStatement :: AmpParser String
pIncludeStatement = pKey "INCLUDE" *> pString

--- LanguageRef ::= 'IN' ('DUTCH' | 'ENGLISH')
pLanguageRef :: AmpParser Lang
pLanguageRef = pKey "IN" *>
               (Dutch   <$ pKey "DUTCH"   <|>
                English <$ pKey "ENGLISH")

--- TextMarkup ::= 'REST' | 'HTML' | 'LATEX' | 'MARKDOWN'
pTextMarkup :: AmpParser PandocFormat
pTextMarkup = ReST     <$ pKey "REST"     <|>
              HTML     <$ pKey "HTML"     <|>
              LaTeX    <$ pKey "LATEX"    <|>
              Markdown <$ pKey "MARKDOWN"

--- Meta ::= 'META' String String
pMeta :: AmpParser Meta
pMeta = Meta <$> currPos <* pKey "META" <*> pMetaObj <*> pString <*> pString
 where pMetaObj = pSucceed ContextMeta -- for the context meta we don't need a keyword

--- PatternDef ::= 'PATTERN' ConceptName PatElem* 'ENDPATTERN'
pPatternDef :: AmpParser P_Pattern
pPatternDef = rebuild <$> currPos
                      <*  pKey "PATTERN"
                      <*> pConceptName   -- The name spaces of patterns, processes and concepts are shared.
                      <*> many pPatElem
                      <*> currPos
                      <*  pKey "ENDPATTERN"
  where
    rebuild :: Origin -> String -> [PatElem] -> Origin -> P_Pattern
    rebuild pos' nm pes end
     = P_Pat { pt_nm  = nm
             , pt_pos = pos'
             , pt_end = end
             , pt_rls = [r | Pr r<-pes]
             , pt_gns = [g | Pg g<-pes]
             , pt_dcs = [d | Pd d<-pes]
             , pt_RRuls = [rr | Pm rr<-pes]
             , pt_RRels = [rr | Pl rr<-pes]
             , pt_cds = [c nm | Pc c<-pes]
             , pt_ids = [k | Pk k<-pes]
             , pt_vds = [v | Pv v<-pes]
             , pt_xps = [e | Pe e<-pes]
             , pt_pop = [p | Pp p<-pes]
             }

--- ProcessDef ::= 'PROCESS' ConceptName ProcElem* 'ENDPROCESS'
pProcessDef :: AmpParser P_Pattern
pProcessDef = rebuild <$> currPos
                      <*  pKey "PROCESS"
                      <*> pConceptName   -- The name spaces of patterns, processes and concepts are shared.
                      <*> many pPatElem
                      <*> currPos
                      <*  pKey "ENDPROCESS"
  where
    rebuild :: Origin -> String -> [PatElem] -> Origin -> P_Pattern
    rebuild pos' nm pes end
     = P_Pat { pt_nm  = nm
             , pt_pos = pos'
             , pt_end = end
             , pt_rls = [r | Pr r<-pes]
             , pt_gns = [g | Pg g<-pes]
             , pt_dcs = [d | Pd d<-pes]
             , pt_RRuls = [rr | Pm rr<-pes]
             , pt_RRels = [rr | Pl rr<-pes]
             , pt_cds = [c nm | Pc c<-pes]
             , pt_ids = [k | Pk k<-pes]
             , pt_vds = [v | Pv v<-pes]
             , pt_xps = [e | Pe e<-pes]
             , pt_pop = [p | Pp p<-pes]
             }			 

--- PatElem used by PATTERN and PROCESS
--- PatElem ::= RuleDef | Classify | RelationDef | ConceptDef | GenDef | Index | ViewDef | Purpose | Population
pPatElem :: AmpParser PatElem
pPatElem = Pr <$> pRuleDef          <|>
           Py <$> pClassify         <|>
           Pd <$> pRelationDef      <|>
           Pm <$> try pRoleRule     <|>
           Pl <$> try pRoleRelation <|>
           Pc <$> pConceptDef       <|>
           Pg <$> pGenDef           <|>
           Pk <$> pIndex            <|>
           Pv <$> pViewDef          <|>
           Pe <$> pPurpose          <|>
           Pp <$> pPopulation

data PatElem = Pr (P_Rule TermPrim)
             | Py P_Gen
             | Pd P_Declaration
             | Pm P_RoleRule
             | Pl P_RoleRelation
             | Pc (String -> ConceptDef)
             | Pg P_Gen
             | Pk P_IdentDef
             | Pv P_ViewDef
             | Pe PPurpose
             | Pp P_Population 

--- Classify ::= 'CLASSIFY' ConceptRef 'IS' Cterm
pClassify :: AmpParser P_Gen   -- Example: CLASSIFY A IS B /\ C /\ D
pClassify = try (rebuild <$> currPos <* pKey "CLASSIFY" <*> pConceptRef <*  pKey "IS")
                    <*> pCterm
               where
                 rebuild po lhs rhs
                   = P_Cy { gen_spc  = lhs             --  Left hand side concept expression
                          , gen_rhs  = rhs             --  Right hand side concept expression
                          , gen_fp   = po
                          }
                 --- Cterm ::= Cterm1 ('/\' Cterm1)*
                 --- Cterm1 ::= ConceptRef | ('('? Cterm ')'?)
                 pCterm  = concat <$> pCterm1 `sepBy1` pOperator "/\\"
                 pCterm1 = single <$> pConceptRef                        <|>
                           id     <$> pParens pCterm  -- brackets are allowed for educational reasons.
                 single x = [x]

--- RuleDef ::= 'RULE' (ADLid ':')? Rule Meaning* Message* Violation?
pRuleDef :: AmpParser (P_Rule TermPrim)
pRuleDef =  rebuild <$> currPos 
                    <*  pKey "RULE"
                    <*> pMaybe (try pLabel)
                    <*> pRule
                    <*> many pMeaning
                    <*> many pMessage
                    <*> pMaybe pViolation
               where
                 rebuild po mn rexp mean msg mViolation
                   = P_Ru { rr_nm   = fromMaybe (rulid po) mn
                          , rr_exp  = rexp
                          , rr_fps  = po
                          , rr_mean = mean
                          , rr_msg  = msg
                          , rr_viol = mViolation
                          }
                 rulid (FileLoc pos _) = "rule@" ++ show (show pos)
                 rulid _ = fatal 226 "pRuleDef is expecting a file location."
                 
                 --- Violation ::= 'VIOLATION' PairView
                 pViolation :: AmpParser (PairView (Term TermPrim))
                 pViolation = id <$ pKey "VIOLATION" <*> pPairView

                 --- PairView ::= '(' PairViewSegmentList ')'
                 pPairView :: AmpParser (PairView (Term TermPrim))
                 pPairView = PairView <$> pParens (pPairViewSegment `sepBy1` pComma)

                 --- PairViewSegmentList  ::= PairViewSegment (',' PairViewSegment)*
                 --- PairViewSegment ::= 'SRC' Term | 'TGT' Term | 'TXT' String
                 pPairViewSegment :: AmpParser (PairViewSegment (Term TermPrim))
                 pPairViewSegment = PairViewExp  <$> posOf (pKey "SRC") <*> return Src <*> pTerm 
                                <|> PairViewExp  <$> posOf (pKey "TGT") <*> return Tgt <*> pTerm
                                <|> PairViewText <$> posOf (pKey "TXT") <*> pString

--- RelationDef ::= (RelationNew | RelationOld) 'BYPLUG'? Props? 'BYPLUG'? ('PRAGMA' String+)? Meaning* ('=' Content)? '.'?
pRelationDef :: AmpParser P_Declaration
pRelationDef = reorder <$> currPos
                       <*> (pRelationNew <|> pRelationOld)
                       <*> pIsThere (pKey "BYPLUG")
                       <*> optList pProps
                       <*> pIsThere (pKey "BYPLUG")
                       <*> optList (pKey "PRAGMA" *> many1 pString)
                       <*> many pMeaning
                       <*> optList (pOperator "=" *> pContent)
                       <*  optList (pOperator ".")
            where reorder pos (nm,sign,fun) bp1 prop bp2 pragma meanings popu =
                    let plug = bp1 || bp2
                        props = prop ++ fun
                    in P_Sgn nm sign props pragma meanings popu pos plug

--- RelationNew ::= 'RELATION' Varid Sign
pRelationNew :: AmpParser (String,P_Sign,Props)
pRelationNew = (,,) <$  pKey "RELATION"
                    <*> pVarid
                    <*> pSign
                    <*> return []

--- RelationOld ::= Varid '::' ConceptRef Fun ConceptRef
pRelationOld :: AmpParser (String,P_Sign,Props)
pRelationOld = relOld <$> pVarid
                      <*  pOperator "::"
                      <*> pConceptRef
                      <*> pFun
                      <*> pConceptRef
            where relOld nm src fun tgt = (nm,P_Sign src tgt,fun)

--- Props ::= '[' PropList? ']'
pProps :: AmpParser [Prop]
pProps  = normalizeProps <$> pBrackets (pProp `sepBy` pComma)
  where --- PropList ::= Prop (',' Prop)*
        --- Prop ::= 'UNI' | 'INJ' | 'SUR' | 'TOT' | 'SYM' | 'ASY' | 'TRN' | 'RFX' | 'IRF' | 'AUT' | 'PROP'
        pProp :: AmpParser Prop
        pProp = choice [ p <$ pKey (show p) | p <- [minBound..] ]

--- Fun ::= '*' | '->' | '<-' | '[' Mults ']'
pFun :: AmpParser [Prop]
pFun  = []        <$ pOperator "*"  <|>
        [Uni,Tot] <$ pOperator "->" <|>
        [Sur,Inj] <$ pOperator "<-" <|>
        pBrackets pMults
  where --- Mults ::= Mult '-' Mult
        pMults :: AmpParser [Prop]
        pMults = (++) <$> optList (pMult (Sur,Inj))
                      <*  pDash
                      <*> optList (pMult (Tot,Uni))
        
        --- Mult ::= ('0' | '1') '..' ('1' | '*') | '*' | '1'
        --- Mult ::= '0' '..' ('1' | '*') | '1'('..' ('1' | '*'))? | '*'
        --TODO! refactor
        pMult :: (Prop,Prop) -> AmpParser [Prop]
        pMult (ts,ui) = (++) <$> ([]    <$ pZero   <|> [ts] <$ try pOne)
                             <*  pOperator ".."
                             <*> ([ui] <$ try pOne <|> ([]   <$ pOperator "*" )) <|>
                        [] <$ pOperator "*"  <|>
                        [ts,ui] <$ try pOne

--- ConceptDef ::= 'CONCEPT' ConceptName 'BYPLUG'? String ('TYPE' String)? String?
pConceptDef :: AmpParser (String->ConceptDef)
pConceptDef       = Cd <$> currPos
                       <*  pKey "CONCEPT"
                       <*> pConceptName           -- the concept name
                       <*> pIsThere (pKey "BYPLUG")
                       <*> pString                -- the definition text
                       <*> optList (pKey "TYPE" *> pString)     -- the type of the concept.
                       <*> (pString `opt` "")     -- a reference to the source of this definition.

--- GenDef ::= ('CLASSIFY' | 'SPEC') ConceptRef 'ISA' ConceptRef
pGenDef :: AmpParser P_Gen 
-- pGenDef = try (rebuild <$> currPos <* pKey "CLASSIFY" <*> pConceptRef <* pKey "ISA") <*> pConceptRef -- 
--          where rebuild p spc gen = PGen { gen_spc = spc, gen_gen = gen, gen_fp = p}

--Old version 
pGenDef = try (rebuild <$> currPos <* key <*> pConceptRef <* pKey "ISA") <*> pConceptRef -- 
          where rebuild p spc gen = PGen { gen_spc = spc, gen_gen = gen, gen_fp = p}
                key = pKey "CLASSIFY" <|> pKey "SPEC"

-- | A identity definition looks like:   IDENT onNameAdress : Person(name, address),
-- which means that name<>name~ /\ address<>addres~ |- I[Person].
-- The label 'onNameAddress' is used to refer to this identity.
-- You may also use an expression on each attribute place, for example: IDENT onpassport: Person(nationality, passport;documentnr),
-- which means that nationality<>nationality~ /\ passport;documentnr<>(passport;documentnr)~ |- I[Person].
--- Index ::= 'IDENT' Label ConceptRefPos '(' IndSegmentList ')'
pIndex :: AmpParser P_IdentDef
pIndex  = P_Id <$> currPos
               <*  pKey "IDENT"
               <*> pLabel
               <*> pConceptRef
               <*> pParens (pIndSegment `sepBy1` pComma)
    where
          --- IndSegmentList ::= IndSegment (',' IndSegment)
          --- IndSegment ::= IndAtt
          pIndSegment :: AmpParser P_IdentSegment
          pIndSegment = P_IdentExp <$> pIndAtt

          --- IndAtt ::= LabelProps Term | Term
          pIndAtt :: AmpParser P_ObjectDef
          --TODO! Use `opt` and remove record syntax (create optLabelProps)
          -- There's an ambiguity in the grammar here: If we see an identifier, we don't know whether it's a label followed by ':' or a term name.
          pIndAtt  = attL <$> currPos <*> (try pLabelProps `opt` ("",[])) <*> try pTerm
              where mView = Nothing
                    msub = Nothing 
                    --TODO: What does this origin mean? It's not used, can we remove it?
                    attL p (nm, strs) ctx = let pos = if null nm then Origin "pIndAtt CC664" else p
                                in P_Obj nm pos ctx mView msub strs

-- | A view definition looks like:
--      VIEW onSSN: Person("social security number":ssn)
-- or
--      VIEW SaveAdlFile: SaveAdlFile(PRIMHTML "<a href='../../index.php?operation=2&file=", filepath , filename
--      ,PRIMHTML "&userrole=", savecontext~;sourcefile;uploaded~;userrole
--      ,PRIMHTML "'>", filename/\V[SaveAdlFile*FileName], PRIMHTML "</a>")
-- which can be used to define a proper user interface by assigning labels and markup to the attributes in a view.

--- ViewDef ::= FancyViewDef | ViewDefLegacy
pViewDef :: AmpParser P_ViewDef
pViewDef = try pFancyViewDef <|> try pViewDefLegacy -- introduces a bit of harmless backtracking, but is more elegant than rewriting pViewDefLegacy to disallow "KEY ... ENDVIEW".

--- FancyViewDef ::= 'VIEW' pLabel ConceptOneRefPos 'DEFAULT'? '{' ViewObjList '}' HtmlView? 'ENDVIEW'
pFancyViewDef :: AmpParser P_ViewDef
pFancyViewDef  = mkViewDef <$> currPos
                      <*  pKey "VIEW"
                      <*> pLabel
                      <*> pConceptOneRef
                      <*> pIsThere (pKey "DEFAULT")
                      <*> pBraces ((P_ViewExp <$> pViewObj) `sepBy1` pComma)
                      <*> pMaybe pHtmlView 
                      <*  pKey "ENDVIEW"
    where mkViewDef pos nm cpt isDef ats html =
            P_Vd { vd_pos = pos
                 , vd_lbl = nm
                 , vd_cpt = cpt
                 , vd_isDefault = isDef
                 , vd_html = html
                 , vd_ats = ats
                 }
          
          --- ViewObjList ::= ViewObj (',' ViewObj)*
          --- ViewObj ::= Label Term
          pViewObj :: AmpParser P_ObjectDef
          pViewObj = P_Obj <$> pLabel
                           <*> currPos
                           <*> pTerm
                           <*> return Nothing
                           <*> return Nothing
                           <*> return []
                          
          --- HtmlView ::= 'HTML' 'TEMPLATE' String
          pHtmlView :: AmpParser ViewHtmlTemplate                 
          pHtmlView = ViewHtmlTemplateFile <$ pKey "HTML" <* pKey "TEMPLATE" <*> pString

--- ViewDefLegacy ::= ('VIEW' | 'KEY') LabelProps ConceptOneRefPos '(' ViewSegmentList ')'
pViewDefLegacy :: AmpParser P_ViewDef
pViewDefLegacy = P_Vd <$> currPos
                      <*  (pKey "VIEW" <|> pKey "KEY")
                      <*> pLabel
                      <*> pConceptOneRef
                      <*> return True
                      <*> return Nothing
                      <*> pParens(ats <$> pViewSegment `sepBy1` pComma)
    --TODO! Numbering should not happen in the parser
    where ats xs = [ case viewSeg of
                         P_ViewExp x  -> if null (obj_nm x) then P_ViewExp $ x{obj_nm="seg_"++show i} else viewSeg
                         _            -> viewSeg
                    | (i,viewSeg) <- zip [(1::Integer)..] xs]
                    -- counter is used to name anonymous segments (may skip numbers because text/html segments are also counted)
          --- ViewSegmentList ::= ViewSegment (',' ViewSegment)*
          --- ViewSegment ::= ViewAtt | 'TXT' String | 'PRIMHTML' String
          pViewSegment :: AmpParser P_ViewSegment
          pViewSegment = P_ViewExp  <$> pViewAtt <|>
                         P_ViewText <$ pKey "TXT" <*> pString <|>
                         P_ViewHtml <$ pKey "PRIMHTML" <*> pString
          --- ViewAtt ::= LabelProps? Term
          pViewAtt :: AmpParser P_ObjectDef
          pViewAtt = rebuild <$> currPos <*> (try pLabelProps `opt` ("",[])) <*> pTerm
              where rebuild pos (nm, strs) ctx = P_Obj nm pos ctx mView msub strs
                    mView = Nothing
                    msub  = Nothing

--- Interface ::= 'INTERFACE' ADLid 'CLASS'? (Conid | String) Params? InterfaceArgs? Roles? ':' Term SubInterface
pInterface :: AmpParser P_Interface
pInterface = lbl <$> currPos                                       <*>
                     (pKey "INTERFACE" *> pADLid)                  <*>
                     pMaybe (pKey "CLASS" *> (pConid <|> pString)) <*> -- the class is an upper-case identifier or a quoted string
                     optList pParams                               <*> -- a list of expressions, which say which relations are editable within this service.
                     optList pArgs                                 <*> -- either  Prel _ nm or  PNamedRel _ nm sgn
                     optList pRoles                                <*>
                     (pColon *> pTerm)                             <*>
                     pSubInterface
    where lbl :: Origin -> String -> Maybe String -> [P_NamedRel] -> [[String]] -> [Role] -> Term TermPrim -> P_SubInterface -> P_Interface
          lbl p nm iclass params args roles term sub
             = P_Ifc { ifc_Name   = nm
                     , ifc_Class  = iclass
                     , ifc_Params = params
                     , ifc_Args   = args
                     , ifc_Roles  = roles
                     , ifc_Obj    = P_Obj { obj_nm   = nm
                                          , obj_pos  = p
                                          , obj_ctx  = term
                                          , obj_mView = Nothing
                                          , obj_msub = Just sub
                                          , obj_strs = args
                                          }
                     , ifc_Pos    = p
                     , ifc_Prp    = ""   --TODO: Nothing in syntax defined for the purpose of the interface.
                     }
          --- Params ::= '(' NamedRel ')'
          pParams = pParens(pNamedRel `sepBy1` pComma)
          --- InterfaceArgs ::= '{' ADLidListList '}'
          pArgs   = pBraces(many1 pADLid `sepBy1` pComma)
          --- Roles ::= 'FOR' RoleList
          pRoles  = pKey "FOR" *> pRole `sepBy1` pComma

--- SubInterface ::= ('BOX' ('<' Conid '>')? | 'ROWS' | 'COLS') Box | 'INTERFACE' ADLid
pSubInterface :: AmpParser P_SubInterface
pSubInterface = P_Box          <$> currPos <*> pBoxKey <*> pBox
            <|> P_InterfaceRef <$> currPos <*  pKey "INTERFACE" <*> pADLid
  where pBoxKey :: AmpParser (Maybe String)
        pBoxKey = pKey "BOX" *> pMaybe (pChevrons pConid)
              <|> Just <$> pKey "ROWS"
              <|> Just <$> pKey "COLS"
              <|> Just <$> pKey "TABS"

--- ObjDef ::= LabelProps Term ('<' Conid '>')? SubInterface?
--- ObjDefList ::= ObjDef (',' ObjDef)*
pObjDef :: AmpParser P_ObjectDef
pObjDef = obj <$> currPos
              <*> pLabelProps
              <*> pTerm            -- the context expression (for example: I[c])
              <*> pMaybe (pChevrons pConid)
              <*> pMaybe pSubInterface  -- the optional subinterface
         where obj pos (nm, strs) term mView msub  =
                 P_Obj { obj_nm   = nm
                       , obj_pos  = pos
                       , obj_ctx  = term
                       , obj_mView = mView
                       , obj_msub = msub
                       , obj_strs = strs
                       }

--- Box ::= '[' ObjDefList ']'
pBox :: AmpParser [P_ObjectDef]
pBox = pBrackets $ pObjDef `sepBy1` pComma

--- Sqlplug ::= 'SQLPLUG' ObjDef
pSqlplug :: AmpParser P_ObjectDef
pSqlplug = pKey "SQLPLUG" *> pObjDef

--- Phpplug ::= 'PHPPLUG' ObjDef
pPhpplug :: AmpParser P_ObjectDef
pPhpplug = pKey "PHPPLUG" *> pObjDef

--- Purpose ::= 'PURPOSE' Ref2Obj LanguageRef? TextMarkup? ('REF' StringListSemi)? Expl
pPurpose :: AmpParser PPurpose
pPurpose = rebuild <$> currPos
                   <*  pKey "PURPOSE"  -- "EXPLAIN" has become obsolete
                   <*> pRef2Obj
                   <*> pMaybe pLanguageRef
                   <*> pMaybe pTextMarkup
                   <*> optList (pKey "REF" *> pString `sepBy1` pSemi)
                   <*> pExpl
     where
       rebuild :: Origin -> PRef2Obj -> Maybe Lang -> Maybe PandocFormat -> [String] -> String -> PPurpose
       rebuild    orig      obj         lang          fmt                   refs       str
           = PRef2 orig obj (P_Markup lang fmt str) (concatMap (splitOn ";") refs)
              -- TODO! Maybe this separation should not happen in the parser
              where splitOn :: Eq a => [a] -> [a] -> [[a]]
                    splitOn [] s = [s]
                    splitOn s t  = case findIndex (isPrefixOf s) (tails t) of
                                     Nothing -> [t]
                                     Just i  -> take i t : splitOn s (drop (i+length s) t)
       --- Ref2Obj ::= 'CONCEPT' ConceptName | 'RELATION' RelSign | 'RULE' ADLid | 'IDENT' ADLid | 'VIEW' ADLid | 'PATTERN' ADLid | 'PROCESS' ADLid | 'INTERFACE' ADLid | 'CONTEXT' ADLid
       pRef2Obj :: AmpParser PRef2Obj
       pRef2Obj = PRef2ConceptDef  <$ pKey "CONCEPT"   <*> pConceptName <|>
                  PRef2Declaration <$ pKey "RELATION"  <*> pNamedRel    <|>
                  PRef2Rule        <$ pKey "RULE"      <*> pADLid       <|>
                  PRef2IdentityDef <$ pKey "IDENT"     <*> pADLid       <|>
                  PRef2ViewDef     <$ pKey "VIEW"      <*> pADLid       <|>
                  PRef2Pattern     <$ pKey "PATTERN"   <*> pADLid       <|>
                  PRef2Pattern     <$ pKey "PROCESS"   <*> pADLid       <|>
                  PRef2Interface   <$ pKey "INTERFACE" <*> pADLid       <|>
                  PRef2Context     <$ pKey "CONTEXT"   <*> pADLid

--- Population ::= 'POPULATION' NamedRel 'CONTAINS' Content | 'POPULATION' ConceptName 'CONTAINS' '[' ValueList ']'
pPopulation :: AmpParser P_Population
-- TODO! Refactor grammar, consider removing pNamedRel or adding it to the parse tree.
pPopulation = try (prelpop <$> currPos <* pKey "POPULATION" <*> pNamedRel    <* pKey "CONTAINS" <*> pContent) <|>
              try (pcptpop <$> currPos <* pKey "POPULATION" <*> pConceptName <* pKey "CONTAINS" <*> pBrackets (pString `sepBy` pComma))
    where
      prelpop :: Origin -> P_NamedRel -> Pairs -> P_Population
      prelpop orig (PNamedRel _ nm mSgn)  contents =
        case mSgn of Nothing  -> P_RelPopu { p_rnme   = nm
                   , p_orig   = orig
                   , p_popps  = contents
                   }
                     Just sgn -> P_TRelPop { p_rnme   = nm
                   , p_type   = sgn
                   , p_orig   = orig
                   , p_popps  = contents
                   }
                                           
      pcptpop :: Origin -> String -> [String] -> P_Population
      pcptpop    orig      cnm       contents
       = P_CptPopu { p_cnme   = cnm
                   , p_orig   = orig
                   , p_popas  = contents
                   }

--- RoleRelation ::= 'ROLE' RoleList 'EDITS' NamedRelList
pRoleRelation :: AmpParser P_RoleRelation
pRoleRelation      = rr <$> currPos
                        <*  pKey "ROLE"
                        <*> pRole `sepBy1` pComma
                        <*  pKey "EDITS"
                        <*> pNamedRel `sepBy1` pComma
                     where rr p roles rels = P_RR roles rels p

--- RoleRule ::= 'ROLE' RoleList 'MAINTAINS' ADLidList
--TODO! Rename the RoleRule to RoleMantains and RoleRelation to RoleEdits.
pRoleRule :: AmpParser P_RoleRule
pRoleRule         = rr <$> currPos
                       <*  pKey "ROLE"
                       <*> pRole `sepBy1` pComma
                       <*  pKey "MAINTAINS"
                       <*> pADLid `sepBy1` pComma
                    where rr p roles rulIds = Maintain roles rulIds p

--- Role ::= ADLid
--- RoleList ::= Role (',' Role)*
pRole :: AmpParser Role
pRole =  Role <$> pADLid

--- PrintThemes ::= 'THEMES' ConceptNameList
pPrintThemes :: AmpParser [String]
pPrintThemes = pKey "THEMES"
            *> pConceptName `sepBy1` pComma -- Patterns, processes and concepts share the same name space, so these names must be checked whether the processes and patterns exist.

--- Meaning ::= 'MEANING' LanguageRef? TextMarkup? (String | Expl)
pMeaning :: AmpParser PMeaning
pMeaning = rebuild <$  pKey "MEANING"
                   <*> pMaybe pLanguageRef
                   <*> pMaybe pTextMarkup
                   <*> (pString <|> pExpl)
   where rebuild :: Maybe Lang -> Maybe PandocFormat -> String -> PMeaning
         rebuild    lang          fmt                   mkup   =
            PMeaning (P_Markup lang fmt mkup)

--- Message ::= 'MESSAGE' Markup
pMessage :: AmpParser PMessage
pMessage = PMessage <$ pKey "MESSAGE" <*> pMarkup

-- Markup ::= LanguageRef? TextMarkup? (String | Expl)
pMarkup :: AmpParser P_Markup
pMarkup = P_Markup
           <$> pMaybe pLanguageRef
           <*> pMaybe pTextMarkup
           <*> (pString <|> pExpl)

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
pRule :: AmpParser (Term TermPrim)
pRule  =  fEequ <$> pTrm1  <*>  posOf (pOperator "=")  <*>  pTerm   <|>
          fEimp <$> pTrm1  <*>  posOf (pOperator "|-") <*>  pTerm   <|>
          pTrm1
          where fequ  lExp orig rExp = PEqu orig lExp rExp
                fEimp lExp orig rExp = PImp orig lExp rExp
-- However elegant, this solution needs to be left-factored in order to get a performant parser.
-}
--- Rule ::= Term ('=' Term | '|-' Term)?
pRule :: AmpParser (Term TermPrim)
pRule  =  pTerm <??> (invert PEqu  <$> currPos <* pOperator "="  <*> pTerm <|>
                      invert PImp  <$> currPos <* pOperator "|-" <*> pTerm)
                      

{-
pTrm1 is slightly more complicated, for the purpose of avoiding "associative" brackets.
The idea is that each operator ("/\\" or "\\/") can be parsed as a sequence without brackets.
However, as soon as they are combined, brackets are needed to disambiguate the combination.
There is no natural precedence of one operator over the other.
Brackets are enforced by parsing the subexpression as pTrm5.
In order to maintain performance standards, the parser is left factored.
The functions pars and f have arguments 'combinator' and 'operator' only to avoid writing the same code twice.
-}
--- Term ::= Trm2 (('/\' Trm2)+ | ('\/' Trm2)+)?
pTerm :: AmpParser (Term TermPrim)
pTerm = rebuild <$> pTrm2 <*> optList (many1 (invert PIsc <$> currPos <* pOperator "/\\" <*> pTrm2) <|>
                                       many1 (invert PUni <$> currPos <* pOperator "\\/" <*> pTrm2))
    where rebuild = foldl (\a f -> f a)

-- The left factored version of difference: (Actually, there is no need for left-factoring here, but no harm either)
--- Trm2 ::= Trm3 ('-' Trm3)?
pTrm2 :: AmpParser (Term TermPrim)
pTrm2   = pTrm3 <??> (invert PDif <$> posOf pDash <*> pTrm3)

-- The left factored version of right- and left residuals:
--- Trm3 ::= Trm4 ('/' Trm4 | '\' Trm4 | '<>' Trm4)?
pTrm3 :: AmpParser (Term TermPrim)
pTrm3  =  pTrm4 <??> (invert PLrs <$> currPos <* pOperator "/"  <*> pTrm4 <|>
                      invert PRrs <$> currPos <* pOperator "\\" <*> pTrm4 <|>
                      invert PDia <$> currPos <* pOperator "<>" <*> pTrm4 )

{- by the way, a slightly different way of getting exactly the same result is:
pTrm3 :: AmpParser (Term TermPrim)
pTrm3  =  pTrm4 <??> (f <$>  (valPosOf (pOperator "/") <|> valPosOf (pOperator "\\") <|> valPosOf (pOperator "<>")) <*> pTrm4 )
          where f ("\\", orig) rExp lExp = PRrs orig lExp rExp
                f ("/" , orig) rExp lExp = PLrs orig lExp rExp
                f (_   , orig) rExp lExp = PDia orig lExp rExp
-}

-- composition and relational addition are associative, and parsed similar to union and intersect...
--- Trm4 ::= Trm5 ((';' Trm5)+ | ('!' Trm5)+ | ('#' Trm5)+)?
pTrm4 :: AmpParser (Term TermPrim)
pTrm4 = rebuild <$> pTrm5 <*> optList (many1 (invert PCps <$> currPos <* pOperator ";" <*> pTrm5) <|>
                                       many1 (invert PRad <$> currPos <* pOperator "!" <*> pTrm5) <|>
                                       many1 (invert PPrd <$> currPos <* pOperator "#" <*> pTrm5))
    where rebuild = foldl (\a f -> f a)

--- Trm5 ::= '-'* Trm6 ('~' | '*' | '+')*
pTrm5 :: AmpParser (Term TermPrim)
--TODO! Separate into prefix and postfix top-level functions
pTrm5  =  f <$> many (valPosOf pDash) <*> pTrm6  <*> many (valPosOf (pOperator "~" <|> pOperator "*" <|> pOperator "+" ))
          where f ms pe (("~",_):ps) = let x=f ms pe ps in PFlp (origin x) x  -- the type checker requires that the origin of x is equal to the origin of its converse.
                f ms pe (("*",orig):ps) = PKl0 orig (f ms pe ps)              -- e*  Kleene closure (star)
                f ms pe (("+",orig):ps) = PKl1 orig (f ms pe ps)              -- e+  Kleene closure (plus)
                f (_:_:ms) pe ps        = f ms pe ps                          -- -e  complement     (unary minus)
                f ((_,orig):ms) pe ps   = let x=f ms pe ps in PCpl orig x     -- the type checker requires that the origin of x is equal to the origin of its complement.
                f _ pe _                = pe

--- Trm6 ::= RelationRef | '(' Term ')'
pTrm6 :: AmpParser (Term TermPrim)
pTrm6 = Prim <$> pRelationRef  <|>
        PBrk <$> currPos <*> pParens pTerm

-- Help function for several expressions. The type 't' is each of the left and right terms, while t is the result type.
invert :: (Origin -> t -> t -> r) -> Origin -> t -> t -> r
invert constructor position rightTerm leftTerm = constructor position leftTerm rightTerm

--- RelationRef ::= RelSign | 'I' ('[' ConceptOneRef ']')? | 'V' Sign? | Atom ('[' ConceptOneRef ']')?
pRelationRef :: AmpParser TermPrim
pRelationRef      = PNamedR <$> pNamedRel                                                           <|>
                    pid   <$> currPos <* pKey "I" <*> pMaybe (pBrackets pConceptOneRef)  <|>
                    pfull <$> currPos <* pKey "V" <*> pMaybe pSign                       <|>
                    Patm  <$> currPos <*> pAtom   <*> pMaybe (pBrackets pConceptOneRef)
                    where pid orig Nothing = PI orig
                          pid orig (Just c)= Pid orig c
                          pfull orig Nothing = PVee orig
                          pfull orig (Just (P_Sign src trg)) = Pfull orig src trg

--- NamedRelList ::= NamedRel (',' NamedRel)*
--- NamedRel ::= Varid Sign?
pNamedRel :: AmpParser P_NamedRel
--TODO! Remove valPosOf
pNamedRel = pnamedrel  <$> valPosOf pVarid <*> pMaybe pSign
                    where pnamedrel (nm, orig) = PNamedRel orig nm

--- Sign ::= '[' ConceptOneRef ('*' ConceptOneRef)? ']'
pSign :: AmpParser P_Sign
pSign = pBrackets sign
   where
     sign = mkSign <$> pConceptOneRef <*> pMaybe (pOperator "*" *> pConceptOneRef)
     mkSign src mTgt = P_Sign src $ fromMaybe src mTgt

--- ConceptName ::= Conid | String
--- ConceptNameList ::= ConceptName (',' ConceptName)
pConceptName ::   AmpParser String
pConceptName    = pConid <|> pString

--- ConceptRef ::= ConceptName
pConceptRef ::    AmpParser P_Concept
pConceptRef     = PCpt <$> pConceptName

--- ConceptOneRef ::= 'ONE' | ConceptRef
pConceptOneRef :: AmpParser P_Concept
pConceptOneRef  = (P_Singleton <$ pKey "ONE") <|> pConceptRef

--  (SJ) Why does a label have (optional) strings?
--  (GM) This is a binding mechanism for implementation specific properties, such as SQL/PHP plug,PHP web app,etc.
--  (SJ April 15th, 2013) Since KEY has been replaced by IDENT and VIEW, there is a variant with props  (pLabelProps) and one without (pLabel).
--- LabelProps ::= ADLid ('{' ADLidListList '}')? ':'
pLabelProps :: AmpParser (String, [[String]])
pLabelProps       = (,) <$> pADLid
                        <*> optList pArgs
                        <*  posOf pColon
                    where pArgs = pBraces $ many1 pADLid `sepBy1` pComma

--- Label ::= ADLid ':'
pLabel :: AmpParser String
pLabel = pADLid <* pColon

--- Content ::= '[' RecordList? ']' | '[' RecordObsList? ']'
pContent :: AmpParser Pairs
pContent      = try (pBrackets (pRecord `sepBy` pComma))   <|>
                try (pBrackets (pRecordObs `sepBy` pSemi))
    where
    --- RecordList ::= Record (',' Record)*
    --- Record ::= String '*' String
    pRecord :: AmpParser Paire
    pRecord = mkPair<$> pString <* pOperator "*" <*> pString
    --- RecordObsList ::= RecordObsList (';' RecordObsList)
    --- RecordObs ::= '(' String ',' String ')'
    pRecordObs :: AmpParser Paire
    pRecordObs = pParens (mkPair <$> pString <* pComma <*> pString)

--- ADLid ::= Varid | Conid | String
--- ADLidList ::= ADLid (',' ADLid)*
--- ADLidListList ::= ADLid+ (',' ADLid+)*
pADLid :: AmpParser String
pADLid            = pVarid <|> pConid <|> pString

pMaybe :: AmpParser a -> AmpParser (Maybe a)
pMaybe p = Just <$> p <|> pSucceed Nothing

{-# OPTIONS_GHC -fno-enable-rewrite-rules #-} -- Disable rewrite rules to drastically improve compilation speed
{-# LANGUAGE FlexibleContexts #-}
module Database.Design.Ampersand.Input.ADL1.Parser
    ( AmpParser
    , pContext
    , pPopulations
    , pTerm
    , pRule
    ) where

import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Core.ParseTree
import Database.Design.Ampersand.Input.ADL1.ParsingLib
import Data.List
import Data.Maybe
import Control.Applicative(pure)

fatal :: Int -> String -> a
fatal = fatalMsg "Input.ADL1.Parser"

--- Populations ::= Population+
-- | Parses a list of populations
pPopulations :: AmpParser [P_Population] -- ^ The population list parser
pPopulations = many1 pPopulation

--- Context ::= 'CONTEXT' ConceptName LanguageRef TextMarkup? ContextElement* 'ENDCONTEXT'
-- | Parses a context
pContext :: AmpParser (P_Context, [String]) -- ^ The result is the parsed context and a list of include filenames
pContext  = rebuild <$> posOf (pKey "CONTEXT")
                    <*> pConceptName
                    <*> pLanguageRef
                    <*> pMaybe pTextMarkup
                    <*> many pContextElement
                    <*  pKey "ENDCONTEXT"
  where
    rebuild :: Origin -> String -> Lang -> Maybe PandocFormat -> [ContextElement] -> (P_Context, [String])
    rebuild    pos       nm        lang          fmt                   ces
     = (PCtx{ ctx_nm     = nm
            , ctx_pos    = [pos]
            , ctx_lang   = lang
            , ctx_markup = fmt
            , ctx_thms   = (nub.concat) [xs | CThm xs<-ces] -- Names of patterns to be printed in the functional specification. (For partial documents.)
            , ctx_pats   = [p | CPat p<-ces]       -- The patterns defined in this context
            , ctx_rs     = [p | CRul p<-ces]       -- All user defined rules in this context, but outside patterns
            , ctx_ds     = [p | CRel p<-ces]       -- The relations defined in this context, outside the scope of patterns
            , ctx_cs     = [c ("CONTEXT "++nm) | CCon c<-ces]    -- The concept definitions defined in this context, outside the scope of patterns
            , ctx_gs     = [g | CGen g<-ces] ++ [y | CCfy y<-ces] -- The gen definitions defined in this context, outside the scope of patterns
            , ctx_ks     = [k | CIndx k<-ces]      -- The identity definitions defined in this context, outside the scope of patterns
            , ctx_rrules = [x | Cm x <-ces]        -- The MAINTAINS statements in the context
            , ctx_rrels  = [x | Cl x <-ces]        -- The EDITS statements in the context
            , ctx_reprs  = [r | CRep r<-ces] 
            , ctx_vs     = [v | CView v<-ces]      -- The view definitions defined in this context, outside the scope of patterns
            , ctx_ifcs   = [s | Cifc s<-ces]       -- The interfaces defined in this context, outside the scope of patterns -- fatal 78 ("Diagnostic: "++concat ["\n\n   "++show ifc | Cifc ifc<-ces])
            , ctx_sql    = [p | CSqlPlug p<-ces]   -- user defined sqlplugs, taken from the Ampersand scriptplug<-ces]
            , ctx_php    = [p | CPhpPlug p<-ces]   -- user defined phpplugs, taken from the Ampersand script
            , ctx_ps     = [e | CPrp e<-ces]       -- The purposes defined in this context, outside the scope of patterns
            , ctx_pops   = [p | CPop p<-ces]       -- The populations defined in this contextplug<-ces]
            , ctx_metas  = [meta | CMeta meta <-ces]
            }
       , [s | CIncl s<-ces] -- the INCLUDE filenames
       ) 
      
    --- ContextElement ::= Meta | PatternDef | ProcessDef | RuleDef | Classify | RelationDef | ConceptDef | GenDef | Index | ViewDef | Interface | Sqlplug | Phpplug | Purpose | Population | PrintThemes | IncludeStatement
    pContextElement :: AmpParser ContextElement
    pContextElement = CMeta    <$> pMeta         <|>
                      CPat     <$> pPatternDef   <|>
                      CPat     <$> pProcessDef   <|>
                      CRul     <$> pRuleDef      <|>
                      CCfy     <$> pClassify     <|>
                      CRel     <$> pRelationDef  <|>
                      CCon     <$> pConceptDef   <|>
                      CRep     <$> pRepresentation <|>
                      Cm       <$> pRoleRule     <|>
                      Cm       <$> pServiceRule  <|>
                      Cl       <$> pRoleRelation <|>
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
                    | CRul (P_Rule TermPrim)
                    | CCfy P_Gen
                    | CRel P_Declaration
                    | CCon (String -> ConceptDef)
                    | CRep Representation
                    | Cm P_RoleRule
                    | Cl P_RoleRelation
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
 where pMetaObj = return ContextMeta -- for the context meta we don't need a keyword

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
     = P_Pat { pt_pos = pos'
             , pt_nm  = nm
             , pt_rls = [r | Pr r<-pes]
             , pt_gns = [y | Py y<-pes] ++ [g | Pg g<-pes]
             , pt_dcs = [d | Pd d<-pes]
             , pt_RRuls = [rr | Pm rr<-pes]
             , pt_RRels = [rr | Pl rr<-pes]
             , pt_cds = [c nm | Pc c<-pes]
             , pt_Reprs = [x | Prep x<-pes]
             , pt_ids = [k | Pk k<-pes]
             , pt_vds = [v | Pv v<-pes]
             , pt_xps = [e | Pe e<-pes]
             , pt_pop = [p | Pp p<-pes]
             , pt_end = end
             }

--- ProcessDef ::= 'PROCESS' ConceptName PatElem* 'ENDPROCESS'
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
     = P_Pat { pt_pos = pos'
             , pt_nm  = nm
             , pt_rls = [r | Pr r<-pes]
             , pt_gns = [y | Py y<-pes] ++ [g | Pg g<-pes]
             , pt_dcs = [d | Pd d<-pes]
             , pt_RRuls = [rr | Pm rr<-pes]
             , pt_RRels = [rr | Pl rr<-pes]
             , pt_Reprs = [r | Prep r<-pes]
             , pt_cds = [c nm | Pc c<-pes]
             , pt_ids = [k | Pk k<-pes]
             , pt_vds = [v | Pv v<-pes]
             , pt_xps = [e | Pe e<-pes]
             , pt_pop = [p | Pp p<-pes]
             , pt_end = end
             }

-- PatElem used by PATTERN and PROCESS
--- PatElem ::= RuleDef | Classify | RelationDef | ConceptDef | GenDef | Index | ViewDef | Purpose | Population
pPatElem :: AmpParser PatElem
pPatElem = Pr <$> pRuleDef          <|>
           Py <$> pClassify         <|>
           Pd <$> pRelationDef      <|>
                   -- the syntax of pRoleRule and pRoleRelation shows an ambiguity
                   -- Syntax review can be considered
           Pm <$> pRoleRule         <|>
           Pm <$> pServiceRule      <|>
           Pl <$> pRoleRelation     <|>
           Pc <$> pConceptDef       <|>
           Pg <$> pGenDef           <|>
           Prep <$> pRepresentation <|>
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
             | Prep Representation
             | Pk P_IdentDef
             | Pv P_ViewDef
             | Pe PPurpose
             | Pp P_Population

--- Classify ::= 'CLASSIFY' ConceptRef 'IS' Cterm
pClassify :: AmpParser P_Gen   -- Example: CLASSIFY A IS B /\ C /\ D
pClassify = try (P_Cy <$> currPos
                      <* pKey "CLASSIFY"
                      <*> pConceptRef
                      <*  pKey "IS")
                 <*> pCterm
               where
                 --- Cterm ::= Cterm1 ('/\' Cterm1)*
                 --- Cterm1 ::= ConceptRef | ('('? Cterm ')'?)
                 pCterm  = concat <$> pCterm1 `sepBy1` pOperator "/\\"
                 pCterm1 = pure   <$> pConceptRef <|>
                           id     <$> pParens pCterm  -- brackets are allowed for educational reasons.

--- RuleDef ::= 'RULE' Label? Rule Meaning* Message* Violation?
pRuleDef :: AmpParser (P_Rule TermPrim)
pRuleDef =  P_Ru <$> currPos
                 <*  pKey "RULE"
                 <*> (try pLabel <|> rulid <$> currPos)
                 <*> pRule
                 <*> many pMeaning
                 <*> many pMessage
                 <*> pMaybe pViolation
           where rulid (FileLoc pos _) = show("rule@" ++(show pos))
                 rulid _ = fatal 226 "pRuleDef is expecting a file location."

                 --- Violation ::= 'VIOLATION' PairView
                 pViolation :: AmpParser (PairView (Term TermPrim))
                 pViolation = id <$ pKey "VIOLATION" <*> pPairView

                 --- PairView ::= '(' PairViewSegmentList ')'
                 pPairView :: AmpParser (PairView (Term TermPrim))
                 pPairView = PairView <$> pParens (pPairViewSegment `sepBy1` pComma)

                 --- PairViewSegmentList ::= PairViewSegment (',' PairViewSegment)*
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

--- RelationNew ::= 'RELATION' Varid Signature
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
        --- PropList ::= Prop (',' Prop)*
        --- Prop ::= 'UNI' | 'INJ' | 'SUR' | 'TOT' | 'SYM' | 'ASY' | 'TRN' | 'RFX' | 'IRF' | 'AUT' | 'PROP'
  where pProp :: AmpParser Prop
        pProp = choice [ p <$ pKey (show p) | p <- [minBound..] ]

--- Fun ::= '*' | '->' | '<-' | '[' Mults ']'
pFun :: AmpParser [Prop]
pFun  = []        <$ pOperator "*"  <|>
        [Uni,Tot] <$ pOperator "->" <|>
        [Sur,Inj] <$ pOperator "<-" <|>
        pBrackets pMults
        --- Mults ::= Mult '-' Mult
  where pMults :: AmpParser [Prop]
        pMults = (++) <$> optList (pMult (Sur,Inj))
                      <*  pDash
                      <*> optList (pMult (Tot,Uni))

        --- Mult ::= ('0' | '1') '..' ('1' | '*') | '*' | '1'
        --TODO: refactor to Mult ::= '0' '..' ('1' | '*') | '1'('..' ('1' | '*'))? | '*'
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
                       <*> pConceptName
                       <*> pIsThere (pKey "BYPLUG")
                       <*> (pString <?> "concept definition (string)")
                       <*> (pString `opt` "") -- a reference to the source of this definition.

--- Representation ::= 'REPRESENT' ConceptNameList 'TYPE' AdlTType
pRepresentation :: AmpParser Representation
pRepresentation 
  = Repr <$> currPos
         <*  pKey "REPRESENT"
         <*> pConceptName `sepBy1` pComma
         <*  pKey "TYPE"
         <*> pAdlTType
--- AdlTType = ...<enumeration>
pAdlTType :: AmpParser TType
pAdlTType
        = k Alphanumeric     "ALPHANUMERIC"
      <|> k BigAlphanumeric  "BIGALPHANUMERIC"
      <|> k HugeAlphanumeric "HUGEALPHANUMERIC"
      <|> k Password         "PASSWORD"
      <|> k Binary           "BINARY"
      <|> k BigBinary        "BIGBINARY"
      <|> k HugeBinary       "HUGEBINARY"
      <|> k Date             "DATE"
      <|> k DateTime         "DATETIME"
      <|> k Boolean          "BOOLEAN"
      <|> k Integer          "INTEGER"
      <|> k Float            "FLOAT"
      
  where
   k tt str = f <$> pKey str where f _ = tt

--- GenDef ::= ('CLASSIFY' | 'SPEC') ConceptRef 'ISA' ConceptRef
pGenDef :: AmpParser P_Gen
pGenDef = try (PGen <$> currPos <* key <*> pConceptRef <* pKey "ISA") <*> pConceptRef --
          where key = pKey "CLASSIFY" <|> pKey "SPEC"

-- | A identity definition looks like:   IDENT onNameAdress : Person(name, address),
-- which means that name<>name~ /\ address<>addres~ |- I[Person].
-- The label 'onNameAddress' is used to refer to this identity.
-- You may also use an expression on each attribute place, for example: IDENT onpassport: Person(nationality, passport;documentnr),
-- which means that nationality<>nationality~ /\ passport;documentnr<>(passport;documentnr)~ |- I[Person].
--- Index ::= 'IDENT' Label ConceptRef '(' IndSegmentList ')'
pIndex :: AmpParser P_IdentDef
pIndex  = P_Id <$> currPos
               <*  pKey "IDENT"
               <*> pLabel
               <*> pConceptRef
               <*> pParens (pIndSegment `sepBy1` pComma)
    where
          --- IndSegmentList ::= Att (',' Att)*
          pIndSegment :: AmpParser P_IdentSegment
          pIndSegment = P_IdentExp <$> pAtt

-- | A view definition looks like:
--      VIEW onSSN: Person("social security number":ssn)
-- or
--      VIEW SaveAdlFile: SaveAdlFile(PRIMHTML "<a href='../../index.php?operation=2&file=", filepath , filename
--      ,PRIMHTML "&userrole=", savecontext~;sourcefile;uploaded~;userrole
--      ,PRIMHTML "'>", filename/\V[SaveAdlFile*FileName], PRIMHTML "</a>")
-- which can be used to define a proper user interface by assigning labels and markup to the attributes in a view.

--- ViewDef ::= FancyViewDef | ViewDefLegacy
pViewDef :: AmpParser P_ViewDef
pViewDef = try pFancyViewDef <|> try pViewDefLegacy -- introduces backtracking, but is more elegant than rewriting pViewDefLegacy to disallow "KEY ... ENDVIEW".

--- FancyViewDef ::= 'VIEW' Label ConceptOneRef 'DEFAULT'? ('{' ViewObjList? '}')?  HtmlView? 'ENDVIEW'
pFancyViewDef :: AmpParser P_ViewDef
pFancyViewDef  = mkViewDef <$> currPos
                      <*  pKey "VIEW"
                      <*> pLabel
                      <*> pConceptOneRef
                      <*> pIsThere (pKey "DEFAULT")
                      <*> (pBraces ((P_ViewExp fatl <$> pViewObj) `sepBy` pComma)) `opt` []
                      <*> pMaybe pHtmlView
                      <*  pKey "ENDVIEW"
    where mkViewDef pos nm cpt isDef ats html =
            P_Vd { vd_pos = pos
                 , vd_lbl = nm
                 , vd_cpt = cpt
                 , vd_isDefault = isDef
                 , vd_html = html
                 , vd_ats = numbered ats
                 }
          fatl = fatal 363 "Numbering of segment goes wrong."
          numbered xs = map numbr (zip [1..] xs)
              where numbr (i,x)= x{vs_nr=i}
          --- ViewObjList ::= ViewObj (',' ViewObj)*
          --- ViewObj ::= Label Term
          pViewObj :: AmpParser P_ObjectDef
          pViewObj = P_Obj <$> pLabel
                           <*> currPos
                           <*> pTerm
                           <*> return Nothing
                           <*> return Nothing
                           <*> return Nothing
                           <*> return []

          --- HtmlView ::= 'HTML' 'TEMPLATE' String
          pHtmlView :: AmpParser ViewHtmlTemplate
          pHtmlView = ViewHtmlTemplateFile <$ pKey "HTML" <* pKey "TEMPLATE" <*> pString

--- ViewDefLegacy ::= 'VIEW' LabelProps ConceptOneRef '(' ViewSegmentList ')'
pViewDefLegacy :: AmpParser P_ViewDef
pViewDefLegacy = P_Vd <$> currPos
                      <*  pKey "VIEW"
                      <*> pLabel
                      <*> pConceptOneRef
                      <*> return True
                      <*> return Nothing
                      <*> pParens(ats <$> pViewSegment `sepBy1` pComma)
    --TODO: Numbering should not happen in the parser
    where ats xs = [ case viewSeg of
                         P_ViewExp _ x  -> if null (obj_nm x) then P_ViewExp i $ x{obj_nm="seg_"++show i} 
                                                              else P_ViewExp i x 
                         P_ViewText _ x -> P_ViewText i x
                         P_ViewHtml _ x -> P_ViewHtml i x
                         
                    | (i,viewSeg) <- zip [(1::Integer)..] xs]
                    -- counter is used to name anonymous segments (may skip numbers because text/html segments are also counted)
          --- ViewSegmentList ::= ViewSegment (',' ViewSegment)*
          --- ViewSegment ::= Att | 'TXT' String | 'PRIMHTML' String
          pViewSegment :: AmpParser P_ViewSegment
          pViewSegment = P_ViewExp  fat <$> pAtt <|>
                         P_ViewText fat <$ pKey "TXT" <*> pString <|>
                         P_ViewHtml fat <$ pKey "PRIMHTML" <*> pString
               where fat = fatal 399 "numbering is done a little later."

--- Interface ::= 'INTERFACE' ADLid 'CLASS'? (Conid | String) Params? InterfaceArgs? Roles? ':' Term (ADLid | Conid)? SubInterface
pInterface :: AmpParser P_Interface
pInterface = lbl <$> currPos                                       <*>
                     (pKey "INTERFACE" *> pADLid)                  <*>
                     pMaybe (pKey "CLASS" *> (pConid <|> pString)) <*> -- the class is an upper-case identifier or a quoted string
                     optList pParams                               <*> -- a list of expressions, which say which relations are editable within this service.
                     optList pArgs                                 <*> -- either  Prel _ nm or  PNamedRel _ nm sgn
                     optList pRoles                                <*>
                     (pColon *> pTerm)                             <*> -- the expression of the interface object
                     pMaybe pCruds                                 <*> -- The Crud-string (will later be tested, that it can contain only characters crud (upper/lower case)
                     pSubInterface
    where lbl :: Origin -> String -> Maybe String -> [P_NamedRel] -> [[String]] -> [Role] -> Term TermPrim -> Maybe P_Cruds -> P_SubInterface -> P_Interface
          lbl p nm iclass params args roles term mCrud sub
             = P_Ifc { ifc_Name   = nm
                     , ifc_Class  = iclass
                     , ifc_Params = params
                     , ifc_Args   = args
                     , ifc_Roles  = roles
                     , ifc_Obj    = P_Obj { obj_nm   = nm
                                          , obj_pos  = p
                                          , obj_ctx  = term
                                          , obj_crud = mCrud
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
          pRoles  = pKey "FOR" *> (pRole False) `sepBy1` pComma

--- SubInterface ::= ('BOX' ('<' Conid '>')? | 'ROWS' | 'COLS') Box | 'LINKTO'? 'INTERFACE' ADLid
pSubInterface :: AmpParser P_SubInterface
pSubInterface = P_Box          <$> currPos <*> pBoxKey <*> pBox
            <|> P_InterfaceRef <$> currPos <*> pIsThere (pKey "LINKTO") <*  pKey "INTERFACE" <*> pADLid
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
              <*> pMaybe pCruds
              <*> pMaybe (pChevrons pConid)
              <*> pMaybe pSubInterface  -- the optional subinterface
         where obj pos (nm, args) ctx mCrud mView msub =
                 P_Obj nm pos ctx mCrud mView msub args
--- Cruds ::= ADLid | Conid
pCruds :: AmpParser P_Cruds
pCruds = P_Cruds <$> currPos 
                 <*> (pADLid <|> pConid)
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
              -- TODO: This separation should not happen in the parser
              where splitOn :: Eq a => [a] -> [a] -> [[a]]
                    splitOn [] s = [s]
                    splitOn s t  = case findIndex (isPrefixOf s) (tails t) of
                                     Nothing -> [t]
                                     Just i  -> take i t : splitOn s (drop (i+length s) t)
       --- Ref2Obj ::= 'CONCEPT' ConceptName | 'RELATION' NamedRel | 'RULE' ADLid | 'IDENT' ADLid | 'VIEW' ADLid | 'PATTERN' ADLid | 'PROCESS' ADLid | 'INTERFACE' ADLid | 'CONTEXT' ADLid
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

--- Population ::= 'POPULATION' (NamedRel 'CONTAINS' Content | ConceptName 'CONTAINS' '[' ValueList ']')
-- | Parses a population
pPopulation :: AmpParser P_Population -- ^ The population parser
pPopulation = pKey "POPULATION" *> (
                  (P_RelPopu Nothing Nothing) <$> currPos <*> pNamedRel    <* pKey "CONTAINS" <*> pContent <|>
                  P_CptPopu <$> currPos <*> pConceptName <* pKey "CONTAINS" <*> pBrackets (pAtomValue `sepBy` pComma))

--- RoleRelation ::= 'ROLE' RoleList 'EDITS' NamedRelList
pRoleRelation :: AmpParser P_RoleRelation
pRoleRelation = try (P_RR <$> currPos
                          <*  pKey "ROLE"
                          <*> (pRole False) `sepBy1` pComma
                          <*  pKey "EDITS")
                    <*> pNamedRel `sepBy1` pComma

--- RoleRule ::= 'ROLE' RoleList 'MAINTAINS' ADLidList
--TODO: Rename the RoleRule to RoleMantains and RoleRelation to RoleEdits.
pRoleRule :: AmpParser P_RoleRule
pRoleRule = try (Maintain <$> currPos
                          <*  pKey "ROLE"
                          <*> (pRole False) `sepBy1` pComma
                          <*  pKey "MAINTAINS")
                <*> pADLid `sepBy1` pComma
--- ServiceRule ::= 'SERVICE' RoleList 'MAINTAINS' ADLidList
--TODO: Rename the RoleRule to RoleMantains and RoleRelation to RoleEdits.
pServiceRule :: AmpParser P_RoleRule
pServiceRule = try (Maintain <$> currPos
                          <*  pKey "SERVICE"
                          <*> (pRole True) `sepBy1` pComma
                          <*  pKey "MAINTAINS")
                <*> pADLid `sepBy1` pComma

--- Role ::= ADLid
--- RoleList ::= Role (',' Role)*
pRole :: Bool -> AmpParser Role
pRole isService =  (if isService then Service else Role) <$> pADLid

--- PrintThemes ::= 'THEMES' ConceptNameList
pPrintThemes :: AmpParser [String]
pPrintThemes = pKey "THEMES"
            *> pConceptName `sepBy1` pComma -- Patterns, processes and concepts share the same name space, so these names must be checked whether the processes and patterns exist.

--- Meaning ::= 'MEANING' LanguageRef? TextMarkup? (String | Expl)
pMeaning :: AmpParser PMeaning
pMeaning = PMeaning <$> (
           P_Markup <$  pKey "MEANING"
                    <*> pMaybe pLanguageRef
                    <*> pMaybe pTextMarkup
                    <*> (pString <|> pExpl))

--- Message ::= 'MESSAGE' Markup
pMessage :: AmpParser PMessage
pMessage = PMessage <$ pKey "MESSAGE" <*> pMarkup

--- Markup ::= LanguageRef? TextMarkup? (String | Expl)
pMarkup :: AmpParser P_Markup
pMarkup = P_Markup
           <$> pMaybe pLanguageRef
           <*> pMaybe pTextMarkup
           <*> (pString <|> pExpl)

--- Rule ::= Term ('=' Term | '|-' Term)?
-- | Parses a rule
pRule :: AmpParser (Term TermPrim) -- ^ The rule parser
pRule  =  pTerm <??> (invert PEqu  <$> currPos <* pOperator "="  <*> pTerm <|>
                      invert PInc  <$> currPos <* pOperator "|-" <*> pTerm)


{-
pTerm is slightly more complicated, for the purpose of avoiding "associative" brackets.
The idea is that each operator ("/\\" or "\\/") can be parsed as a sequence without brackets.
However, as soon as they are combined, brackets are needed to disambiguate the combination.
There is no natural precedence of one operator over the other.
Brackets are enforced by parsing the subexpression as pTrm5.
In order to maintain performance standards, the parser is left factored.
The functions pars and f have arguments 'combinator' and 'operator' only to avoid writing the same code twice.
-}
--- Term ::= Trm2 (('/\' Trm2)+ | ('\/' Trm2)+)?
-- | Parses a term
pTerm :: AmpParser (Term TermPrim) -- ^ The term parser
pTerm = pTrm2 <??> (invertT PIsc <$> rightAssociate PIsc "/\\" pTrm2 <|>
                    invertT PUni <$> rightAssociate PUni "\\/" pTrm2)

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

-- composition and relational addition are associative, and parsed similar to union and intersect...
--- Trm4 ::= Trm5 ((';' Trm5)+ | ('!' Trm5)+ | ('#' Trm5)+)?
pTrm4 :: AmpParser (Term TermPrim)
pTrm4   = pTrm5 <??> (invertT PCps <$> rightAssociate PCps ";" pTrm5 <|>
                      invertT PRad <$> rightAssociate PRad "!" pTrm5 <|>
                      invertT PPrd <$> rightAssociate PPrd "#" pTrm5)

--- Trm5 ::= '-'* Trm6 ('~' | '*' | '+')*
pTrm5 :: AmpParser (Term TermPrim)
--TODO: Separate into prefix and postfix top-level functions
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

-- Help function for several expressions. The type 't' is each of the terms.
invert :: (Origin -> t -> t -> t) -> Origin -> t -> t -> t
invert constructor position rightTerm leftTerm = constructor position leftTerm rightTerm

-- Variant for the above function with a tuple, for usage with right association
invertT :: (Origin -> t -> t -> t) -> (Origin, t) -> t -> t
invertT constructor (position,rightTerm) leftTerm = constructor position leftTerm rightTerm

-- Help function for pTerm and pTrm4, to allow right association
rightAssociate :: (Origin -> t -> t -> t) -> String -> AmpParser t -> AmpParser (Origin, t)
rightAssociate combinator operator term
                 = g <$> currPos <* pOperator operator <*> term <*> pMaybe (rightAssociate combinator operator term)
                          where g orig y Nothing  = (orig, y)
                                g orig y (Just (org,z)) = (orig, combinator org y z)

--- RelationRef ::= NamedRel | 'I' ('[' ConceptOneRef ']')? | 'V' Signature? | Singleton ('[' ConceptOneRef ']')?
pRelationRef :: AmpParser TermPrim
pRelationRef      = PNamedR <$> pNamedRel                                                
                <|> pid   <$> currPos <* pKey "I" <*> pMaybe (pBrackets pConceptOneRef)
                <|> pfull <$> currPos <* pKey "V" <*> pMaybe pSign
                <|> Patm  <$> currPos <*> pSingleton <*> pMaybe (pBrackets pConceptOneRef)
                    where pid orig Nothing = PI orig
                          pid orig (Just c)= Pid orig c
                          pfull orig Nothing = PVee orig
                          pfull orig (Just (P_Sign src trg)) = Pfull orig src trg

pSingleton :: AmpParser PSingleton
pSingleton = value2PAtomValue <$> currPos <*> pAtomInExpression

pAtomValue :: AmpParser PAtomValue
pAtomValue = value2PAtomValue <$> currPos <*> pAtomValInPopulation

value2PAtomValue :: Origin -> Value -> PAtomValue
value2PAtomValue o v = case v of 
         VSingleton s x -> PSingleton o s (fmap (value2PAtomValue o) x)
         VRealString s  -> ScriptString o s
         VInt i         -> ScriptInt o (toInteger i)
         VFloat x       -> ScriptFloat o x
         VBoolean b     -> ComnBool o b
         VDateTime x    -> ScriptDateTime o x
         VDate x        -> ScriptDate o x
         
--- Att ::= LabelProps? Term
pAtt :: AmpParser P_ObjectDef
-- There's an ambiguity in the grammar here: If we see an identifier, we don't know whether it's a label followed by ':' or a term name.
pAtt = rebuild <$> currPos <*> try pLabelProps `opt` ("",[]) <*> try pTerm
  where rebuild pos (nm, strs) ctx = P_Obj nm pos ctx mCrud mView msub strs
        mCrud = Nothing
        mView = Nothing
        msub = Nothing

--- NamedRelList ::= NamedRel (',' NamedRel)*
--- NamedRel ::= Varid Signature?
pNamedRel :: AmpParser P_NamedRel
pNamedRel = PNamedRel  <$> currPos <*> pVarid <*> pMaybe pSign

--- Signature ::= '[' ConceptOneRef ('*' ConceptOneRef)? ']'
pSign :: AmpParser P_Sign
pSign = pBrackets sign
   where sign = mkSign <$> pConceptOneRef <*> pMaybe (pOperator "*" *> pConceptOneRef)
         mkSign src mTgt = P_Sign src (fromMaybe src mTgt)

--- ConceptName ::= Conid | String
--- ConceptNameList ::= ConceptName (',' ConceptName)
pConceptName ::   AmpParser String
pConceptName = pConid <|> pString

--- ConceptRef ::= ConceptName
pConceptRef ::    AmpParser P_Concept
pConceptRef = PCpt <$> pConceptName

--- ConceptOneRef ::= 'ONE' | ConceptRef
pConceptOneRef :: AmpParser P_Concept
pConceptOneRef = (P_Singleton <$ pKey "ONE") <|> pConceptRef

--  (SJ) Why does a label have (optional) strings?
--  (GM) This is a binding mechanism for implementation specific properties, such as SQL/PHP plug,PHP web app,etc.
--  (SJ April 15th, 2013) Since KEY has been replaced by IDENT and VIEW, there is a variant with props  (pLabelProps) and one without (pLabel).
--- LabelProps ::= ADLid ('{' ADLidListList '}')? ':'
pLabelProps :: AmpParser (String, [[String]])
pLabelProps = (,) <$> pADLid
                  <*> optList pArgs
                  <*  posOf pColon
              where pArgs = pBraces $ many1 pADLid `sepBy1` pComma

--- Label ::= ADLid ':'
pLabel :: AmpParser String
pLabel = pADLid <* pColon

--- Content ::= '[' RecordList? ']'
pContent :: AmpParser [PAtomPair]
pContent = pBrackets (pRecord `sepBy` (pComma <|> pSemi))
          --- RecordList ::= Record ((','|';') Record)*
          --- Record ::= String ',' String
    where pRecord :: AmpParser PAtomPair
          pRecord = 
             pParens (PPair <$> currPos
                            <*> pAtomValue
                            <*  pComma
                            <*> pAtomValue
                     )
                     
--- ADLid ::= Varid | Conid | String
--- ADLidList ::= ADLid (',' ADLid)*
--- ADLidListList ::= ADLid+ (',' ADLid+)*
pADLid :: AmpParser String
pADLid = pVarid <|> pConid <|> pString



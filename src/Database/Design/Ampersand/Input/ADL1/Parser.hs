{-# OPTIONS_GHC -fno-enable-rewrite-rules #-} -- Disable rewrite rules to drastically improve compilation speed
{-# LANGUAGE FlexibleContexts #-}
module Database.Design.Ampersand.Input.ADL1.Parser
  (AmpParser, pContext, pPopulations,pTerm, pRule, keywordstxt, keywordsops, specialchars, opchars) where

import Database.Design.Ampersand.Input.ADL1.UU_Scanner
         ( Token(..),TokenType(..),noPos
         , pKey,pConid,pString,pSpec,pExpl,pVarid,pComma,pSemi)
import UU.Parsing hiding (Parser)
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Core.ParseTree
import Data.List
import Data.Maybe

fatal :: Int -> String -> a
fatal = fatalMsg "Input.ADL1.Parser"

type AmpParser a = AnaParser [Token] Pair Token (Maybe Token) a


--  The Ampersand scanner takes the file name (String) for documentation and error messaging.
--   scanner :: String -> String -> [Token]
--   scanner fn str = scan keywordstxt keywordsops specialchars opchars fn initPos str

keywordstxt :: [String]
keywordstxt       = [ "INCLUDE"
                    , "CONTEXT", "ENDCONTEXT", "EXTENDS", "THEMES"
                    , "META"
                    , "PATTERN", "ENDPATTERN"
                    , "PROCESS", "ENDPROCESS"
                    , "INTERFACE", "CLASS", "FOR", "BOX", "ROWS", "TABS", "COLS", "INITIAL", "SQLPLUG", "PHPPLUG"
                    , "REPRESENT", "TYPE"
                    , "POPULATION", "CONTAINS"
                    , "UNI", "INJ", "SUR", "TOT", "SYM", "ASY", "TRN", "RFX", "IRF", "AUT", "PROP", "ALWAYS"
                    , "RULE", "MESSAGE", "VIOLATION", "SRC", "TGT", "TEST"
                    , "RELATION", "MEANING", "CONCEPT", "IDENT"
                    , "VIEW", "ENDVIEW", "DEFAULT", "TXT", "PRIMHTML", "TEMPLATE"
                    , "IMPORT", "SPEC", "ISA", "IS", "I", "V"
                    , "CLASSIFY"
                    , "PRAGMA", "PURPOSE", "IN", "REF", "ENGLISH", "DUTCH"
                    , "REST", "HTML", "LATEX", "MARKDOWN"
                    , "ONE"
                    , "BYPLUG"
                    , "ROLE", "EDITS", "MAINTAINS"
                    -- Keywords for TType:
                    , "ALPHANUMERIC", "BIGALPHANUMERIC", "HUGEALPHANUMERIC", "PASSWORD"
                    , "BINARY", "BIGBINARY", "HUGEBINARY"
                    , "DATE", "DATETIME", "BOOLEAN", "INTEGER", "FLOAT", "AUTOINCREMENT"
                    ]
keywordsops :: [String]
keywordsops       = [ "|-", "-", "->", "<-", "=", "~", "+", "*", ";", "!", "#", "::", ":", "\\/", "/\\", "\\", "/", "<>"
                    , "..", "." , "0", "1"]
specialchars :: String
specialchars      = "()[],{}<>"
opchars :: String
opchars           = nub (sort (concat keywordsops))

--to parse files containing only populations
pPopulations :: AmpParser [P_Population]
pPopulations = pList1 pPopulation

pContext :: AmpParser (P_Context, [String]) -- the result is the parsed context and a list of include filenames
pContext  = rebuild <$> pKey_pos "CONTEXT" <*> pConceptName
                         <*> pLanguageRef
                         <*> pMaybe pTextMarkup
                         <*> pList pContextElement <* pKey "ENDCONTEXT"
  where
    rebuild :: Origin -> String -> Lang -> Maybe PandocFormat -> [ContextElement] -> (P_Context, [String])
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
            , ctx_rrules = []  -- TODO: Allow MAINTAINS statements in the context
            , ctx_rrels  = []  -- TODO: Allow EDITS statements in the context
            , ctx_reprs  = [r | CRep r<-ces] 
            , ctx_vs     = [v | CView v<-ces]      -- The view definitions defined in this context, outside the scope of patterns
            , ctx_ifcs   = [s | Cifc s<-ces]       -- The interfaces defined in this context, outside the scope of patterns -- fatal 78 ("Diagnostic: "++concat ["\n\n   "++show ifc | Cifc ifc<-ces])
            , ctx_sql    = [p | CSqlPlug p<-ces]   -- user defined sqlplugs, taken from the Ampersand scriptplug<-ces]
            , ctx_php    = [p | CPhpPlug p<-ces]   -- user defined phpplugs, taken from the Ampersand script
            , ctx_ps     = [e | CPrp e<-ces]       -- The purposes defined in this context, outside the scope of patterns
            , ctx_pops   = [p | CPop p<-ces]       -- The populations defined in this contextplug<-ces]
            , ctx_metas  = [meta | CMeta meta <-ces]
            }
       , [s | CIncl s<-ces]) -- the INCLUDE filenames

    pContextElement :: AmpParser ContextElement
    pContextElement = CMeta    <$> pMeta         <|>
                      CPat     <$> pPatternDef   <|>
                      CPrc     <$> pProcessDef   <|>
                      CRul     <$> pRuleDef      <|>
                      CCfy     <$> pClassify     <|>
                      CRel     <$> pRelationDef  <|>
                      CCon     <$> pConceptDef   <|>
                      CRep     <$> pRepresentation <|>
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
                    | CCon (String->ConceptDef)
                    | CRep Representation
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

pIncludeStatement :: AmpParser String
pIncludeStatement = pKey "INCLUDE" *> pString

pLanguageRef :: AmpParser Lang
pLanguageRef = pKey "IN" *>
               (( Dutch   <$ pKey "DUTCH"  ) <|>
                ( English <$ pKey "ENGLISH")
               )
pTextMarkup :: AmpParser PandocFormat
pTextMarkup = ( ReST     <$ pKey "REST"     ) <|>
              ( HTML     <$ pKey "HTML"     ) <|>
              ( LaTeX    <$ pKey "LATEX"    ) <|>
              ( Markdown <$ pKey "MARKDOWN" )

pMeta :: AmpParser Meta
pMeta = Meta <$> pKey_pos "META" <*> pMetaObj <*> pString <*> pString
 where pMetaObj = pSucceed ContextMeta -- for the context meta we don't need a keyword

pPatternDef :: AmpParser P_Pattern
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
             , pt_RRuls = []  -- TODO: Add P_RoleRule to Pattern
             , pt_RRels = []  -- TODO: Add P_RoleRelation to Pattern
             , pt_Reprs = [r | Prep r<-pes]
             , pt_cds = [c nm | Pc c<-pes]
             , pt_ids = [k | Pk k<-pes]
             , pt_vds = [v | Pv v<-pes]
             , pt_xps = [e | Pe e<-pes]
             , pt_pop = [p | Pp p<-pes]
             }
    pPatElem :: AmpParser PatElem
    pPatElem = Pr <$> pRuleDef      <|>
               Py <$> pClassify     <|>
               Pd <$> pRelationDef  <|>
               Pc <$> pConceptDef   <|>
               Pg <$> pGenDef       <|>
               Prep <$> pRepresentation <|>
               Pk <$> pIndex        <|>
               Pv <$> pViewDef      <|>
               Pe <$> pPurpose      <|>
               Pp <$> pPopulation

data PatElem = Pr (P_Rule TermPrim)
             | Py P_Gen
             | Pd P_Declaration
             | Pc (String->ConceptDef)
             | Pg P_Gen
             | Prep Representation
             | Pk P_IdentDef
             | Pv P_ViewDef
             | Pe PPurpose
             | Pp P_Population

pProcessDef :: AmpParser P_Pattern
pProcessDef = rebuild <$> pKey_pos "PROCESS" <*> pConceptName   -- The name spaces of patterns, processes and concepts are shared.
                      <*> pList pProcElem
                      <*> pKey_pos "ENDPROCESS"
   where
    rebuild :: Origin -> String -> [ProcElem] -> Origin -> P_Pattern
    rebuild pos' nm pes end
      = P_Pat { pt_nm    = nm
              , pt_pos   = pos'
              , pt_end   = end
              , pt_rls = [rr | PrR rr<-pes]
              , pt_gns  = [g  | PrG g <-pes]
              , pt_dcs  = [d  | PrD d <-pes]
              , pt_RRuls = [rr | PrM rr<-pes]
              , pt_RRels = [rr | PrL rr<-pes]
              , pt_Reprs = [r  | PrRep r<-pes]
              , pt_cds   = [cd nm | PrC cd<-pes]
              , pt_ids   = [ix | PrI ix<-pes]
              , pt_vds   = [vd | PrV vd<-pes]
              , pt_xps   = [e  | PrE e <-pes]
              , pt_pop   = [p  | PrP p <-pes]
              }
    pProcElem :: AmpParser ProcElem
    pProcElem = PrR <$> pRuleDef      <|>
                PrY <$> pClassify     <|>
                PrD <$> pRelationDef  <|>
                PrM <$> pRoleRule     <|>
                PrL <$> pRoleRelation <|>
                PrRep <$> pRepresentation <|>
                PrC <$> pConceptDef   <|>
                PrG <$> pGenDef       <|>
                PrI <$> pIndex        <|>
                PrV <$> pViewDef      <|>
                PrE <$> pPurpose      <|>
                PrP <$> pPopulation

data ProcElem = PrR (P_Rule TermPrim)
              | PrY P_Gen
              | PrD P_Declaration
              | PrM P_RoleRule
              | PrL P_RoleRelation
              | PrRep Representation
              | PrC (String->ConceptDef)
              | PrG P_Gen
              | PrI P_IdentDef
              | PrV P_ViewDef
              | PrE PPurpose
              | PrP P_Population

pClassify :: AmpParser P_Gen   -- Example: CLASSIFY A IS B /\ C /\ D
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

pRuleDef :: AmpParser (P_Rule TermPrim)
pRuleDef =  rebuild <$> pKey_pos "RULE"
                    <*> pMaybe (pADLid <* pKey ":" )
                    <*> pRule
                    <*> pList pMeaning
                    <*> pList pMessage
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
                 rulid (FileLoc(FilePos (_,Pos l _,_))) = "rule@line"++show l
                 rulid _ = fatal 226 "pRuleDef is expecting a file location."
                 pViolation :: AmpParser (PairView (Term TermPrim))
                 pViolation = id <$ pKey "VIOLATION" <*> pPairView

                 pPairView :: AmpParser (PairView (Term TermPrim))
                 pPairView = PairView <$ pSpec '(' <*> pList1Sep (pSpec ',') pPairViewSegment <* pSpec ')'

                 pPairViewSegment :: AmpParser (PairViewSegment (Term TermPrim))
                 pPairViewSegment = rebuildSrc <$> pKey_pos "SRC" <*> pTerm 
                                <|> rebuildTgt <$> pKey_pos "TGT" <*> pTerm
                                <|> PairViewText <$> pKey_pos "TXT" <*> pString
                   where rebuildSrc p t = PairViewExp p Src t
                         rebuildTgt p t = PairViewExp p Tgt t

pRelationDef :: AmpParser P_Declaration
pRelationDef      = ( rebuild <$> pVarid  <*> pKey_pos "::"  <*> pConceptRef  <*> pFun  <*> pConceptRef
                      <|> rbd <$> pKey_pos "RELATION" <*> pVarid  <*> pSign
                    )
                      <*> ((True <$ pKey "BYPLUG") `opt` False)
                      <*> (pProps `opt` [])
                      <*> ((True <$ pKey "BYPLUG") `opt` False)
                      <*> (pPragma `opt` [])
                      <*> pList pMeaning
                      <*> ((pKey "=" *> pContent) `opt` [])
                      <* (pKey "." `opt` "")         -- in the syntax before 2011, a dot was required. This optional dot is there to save user irritation during the transition to a dotless era  :-) .
                    where rebuild nm pos' src fun' trg bp1 props --bp2 pragma meanings content
                            = rbd pos' nm (P_Sign src trg) bp1 props' --bp2 pragma meanings content
                              where props'= nub (props `uni` fun')
                          rbd pos' nm sgn bp1 props bp2 pragma meanings content
                            = P_Sgn { dec_nm   = nm
                                    , dec_sign = sgn
                                    , dec_prps = props
                                    , dec_prL  = head pr
                                    , dec_prM  = pr!!1
                                    , dec_prR  = pr!!2
                                    , dec_Mean = meanings
                                    , dec_popu = content
                                    , dec_fpos = pos'
                                    , dec_plug = bp1 || bp2
                                    }
                              where pr = pragma++["","",""]

                          pProps :: AmpParser [Prop]
                          pProps  = (f.concat) <$> (pSpec '[' *> pListSep (pSpec ',') pProp <* pSpec ']')
                              where f ps = nub (ps ++ concat [[Uni, Inj] | null ([Sym, Asy]>-ps)])
                          pProp :: AmpParser [Prop]
                          pProp   = k [Uni] "UNI" <|> k [Inj] "INJ" <|> k [Sur] "SUR" <|> k [Tot] "TOT" <|>
                                    k [Sym] "SYM" <|> k [Asy] "ASY" <|> k [Trn] "TRN" <|>
                                    k [Rfx] "RFX" <|> k [Irf] "IRF" <|> k [Aut] "AUT" <|> k [Sym, Asy] "PROP"
                              where k obj str = f <$> pKey str where f _ = obj
                          pPragma :: AmpParser [String]
                          pPragma = pKey "PRAGMA" *> pList1 pString
                          pFun :: AmpParser [Prop]
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
                                pMult :: (Prop,Prop) -> AmpParser [Prop]
                                pMult (ts,ui) = rbld  <$> (( []   <$ pKey "0") <|> ([ts] <$ pKey "1") )
                                                      <*  pKey ".."
                                                      <*> (( [ui] <$ pKey "1") <|> ([]   <$ pKey "*" )) <|>
                                                [] <$ pKey "*"  <|>
                                                [ts,ui] <$ pKey "1"
                                rbld a b = a++b

pConceptDef :: AmpParser (String->ConceptDef)
pConceptDef       = Cd <$> pKey_pos "CONCEPT"
                       <*> pConceptName           -- the concept name
                       <*> ((True <$ pKey "BYPLUG") `opt` False)
                       <*> pString                -- the definition text
                  --     <*> ((pKey "TYPE" *> pString) `opt` "")     -- the type of the concept.
                       <*> (pString `opt` "")     -- a reference to the source of this definition.

pRepresentation :: AmpParser Representation
pRepresentation 
  = Repr <$> pKey_pos "REPRESENT"
         <*> pList1Sep (pSpec ',') pConceptName  -- the concept names
         <*  pKey "TYPE"
         <*> pAdlTType

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
      <|> k AutoIncrement    "AUTOINCREMENT"
  where
   k tt str = f <$> pKey str where f _ = tt

pGenDef :: AmpParser P_Gen
pGenDef           = rebuild <$> pKey_pos "SPEC"     <*> pConceptRef <* pKey "ISA" <*> pConceptRef <|>  -- SPEC is obsolete syntax. Should disappear!
                    rebuild <$> pKey_pos "CLASSIFY" <*> pConceptRef <* pKey "ISA" <*> pConceptRef <|>
                    pClassify
                    where rebuild p spc gen = PGen{gen_spc=spc, gen_gen=gen, gen_fp =p}

-- | A identity definition looks like:   IDENT onNameAdress : Person(name, address),
-- which means that name<>name~ /\ address<>addres~ |- I[Person].
-- The label 'onNameAddress' is used to refer to this identity.
-- You may also use an expression on each attribute place, for example: IDENT onpassport: Person(nationality, passport;documentnr),
-- which means that nationality<>nationality~ /\ passport;documentnr<>(passport;documentnr)~ |- I[Person].
pIndex :: AmpParser P_IdentDef
pIndex  = identity <$ pKey "IDENT" <*> pLabel <*> pConceptRefPos <* pSpec '(' <*> pList1Sep (pSpec ',') pIndSegment <* pSpec ')'
    where identity :: Label -> (P_Concept, Origin) -> [P_IdentSegment] -> P_IdentDef
          identity (Lbl nm _ _) (c, orig) ats
           = P_Id { ix_pos = orig
                  , ix_lbl = nm
                  , ix_cpt = c
                  , ix_ats = ats
                  }

          pIndSegment :: AmpParser P_IdentSegment
          pIndSegment = P_IdentExp <$> pIndAtt

          pIndAtt :: AmpParser P_ObjectDef
          pIndAtt  = attL <$> pLabelProps <*> pTerm <|>
                     att <$> pTerm
              where attL (Lbl nm p strs) attexpr =
                       P_Obj { obj_nm   = nm
                             , obj_pos  = p
                             , obj_ctx  = attexpr
                             , obj_mView = Nothing
                             , obj_msub = Nothing
                             , obj_strs = strs
                             }
                    att attexpr =
                        P_Obj { obj_nm   = ""
                              , obj_pos  = Origin "pIndAtt CC664"
                              , obj_ctx  = attexpr
                              , obj_mView = Nothing
                              , obj_msub = Nothing
                              , obj_strs = []
                              }

pViewDef :: AmpParser P_ViewDef
pViewDef  = pFancyViewDef <|> pViewDefLegacy -- introduces a bit of harmless backtracking, but is more elegant than rewriting pViewDefLegacy to disallow "KEY ... ENDVIEW".

pFancyViewDef :: AmpParser P_ViewDef
pFancyViewDef  = mkViewDef <$  pKey "VIEW" <*> pLabel <*> pConceptOneRefPos <*> pMaybe (pKey "DEFAULT") <* pSpec '{' <*> pList1Sep (pSpec ',') pViewObj <* pSpec '}'
                           <*> pMaybe pHtmlView 
                           <*  pKey "ENDVIEW"
    where mkViewDef :: Label -> (P_Concept, Origin) -> Maybe String -> [P_ObjectDef] -> Maybe ViewHtmlTemplate -> P_ViewDef
          mkViewDef (Lbl nm _ _) (c, orig) mDefault objs mHtml =
            P_Vd { vd_pos = orig
                 , vd_lbl = nm
                 , vd_cpt = c
                 , vd_isDefault = isJust mDefault
                 , vd_html = mHtml
                 , vd_ats = map P_ViewExp objs
                 }

          pViewObj :: AmpParser P_ObjectDef
          pViewObj = mkObj <$> pLabel <*> pTerm
            where mkObj (Lbl nm p strs) attexpr = 
                    P_Obj { obj_nm    = nm
                          , obj_pos   = p
                          , obj_ctx   = attexpr
                          , obj_mView = Nothing
                          , obj_msub  = Nothing
                          , obj_strs  = strs -- will always be []
                          }
          
          pHtmlView :: AmpParser ViewHtmlTemplate                 
          pHtmlView = ViewHtmlTemplateFile <$ pKey "HTML" <* pKey "TEMPLATE" <*> pString
          

-- | A view definition looks like:
--      VIEW onSSN: Person("social security number":ssn)
-- or
--      VIEW SaveAdlFile: SaveAdlFile(PRIMHTML "<a href='../../index.php?operation=2&file=", filepath , filename
--      ,PRIMHTML "&userrole=", savecontext~;sourcefile;uploaded~;userrole
--      ,PRIMHTML "'>", filename/\V[SaveAdlFile*FileName], PRIMHTML "</a>")
-- which can be used to define a proper user interface by assigning labels and markup to the attributes in a view.
pViewDefLegacy :: AmpParser P_ViewDef
pViewDefLegacy  = vd <$ pKey "VIEW" <*> pLabelProps <*> pConceptOneRefPos <* pSpec '(' <*> pList1Sep (pSpec ',') pViewSegment <* pSpec ')'
    where vd :: Label -> (P_Concept, Origin) -> [P_ViewSegment] -> P_ViewDef
          vd (Lbl nm _ _) (c, orig) ats
              = P_Vd { vd_pos = orig
                     , vd_lbl = nm
                     , vd_cpt = c
                     , vd_isDefault = True -- Legacy view defs are always assumed to be the default (only one is allowed)
                     , vd_html = Nothing
                     , vd_ats = [ case viewSeg of
                                     P_ViewExp x       -> if null (obj_nm x) then P_ViewExp $ x{obj_nm="seg_"++show i} else P_ViewExp x
                                     P_ViewText _ -> viewSeg
                                     P_ViewHtml _ -> viewSeg
                                | (i,viewSeg) <- zip [(1::Integer)..] ats]
                     }  -- counter is used to name anonymous segments (may skip numbers because text/html segments are also counted)
          pViewSegment :: AmpParser P_ViewSegment
          pViewSegment = P_ViewExp  <$> pViewAtt <|>
                         P_ViewText <$ pKey "TXT" <*> pString <|>
                         P_ViewHtml <$ pKey "PRIMHTML" <*> pString
          pViewAtt :: AmpParser P_ObjectDef
          pViewAtt = rebuild <$> pMaybe pLabelProps <*> pTerm
              where
                rebuild mLbl attexpr =
                  case mLbl of
                    Just (Lbl nm p strs) ->
                            P_Obj { obj_nm   = nm
                                  , obj_pos  = p
                                  , obj_ctx  = attexpr
                                  , obj_mView = Nothing
                                  , obj_msub = Nothing
                                  , obj_strs = strs
                                  }
                    Nothing ->
                            P_Obj { obj_nm   = ""
                                  , obj_pos  = origin attexpr
                                  , obj_ctx  = attexpr
                                  , obj_mView = Nothing
                                  , obj_msub = Nothing
                                  , obj_strs = []
                                  }

pInterface :: AmpParser P_Interface
pInterface = lbl <$> (pKey "INTERFACE" *> pADLid_val_pos) <*>
                     (pMaybe $ pKey "CLASS" *> (pConid <|> pString)) <*> -- the class is an upper-case identifier or a quoted string
                     (pParams `opt` [])                   <*>       -- a list of expressions, which say which relations are editable within this service.
                                                                    -- either  Prel _ nm
                                                                    --       or  PNamedRel _ nm sgn
                     (pArgs   `opt` [])                   <*>
                     (pRoles  `opt` [])                   <*>
                     (pKey ":" *> pTerm)                  <*>
                     pSubInterface
    where lbl :: (String, Origin) -> Maybe String -> [P_NamedRel] -> [[String]] -> [Role] -> (Term TermPrim) -> P_SubInterface -> P_Interface
          lbl (nm,p) iclass params args roles expr sub
             = P_Ifc { ifc_Name   = nm
                     , ifc_Class  = iclass
                     , ifc_Params = params
                     , ifc_Args   = args
                     , ifc_Roles  = roles
                     , ifc_Obj    = P_Obj { obj_nm   = nm
                                          , obj_pos  = p
                                          , obj_ctx  = expr
                                          , obj_mView = Nothing
                                          , obj_msub = Just sub
                                          , obj_strs = args
                                          }
                     , ifc_Pos    = p
                     , ifc_Prp    = ""   --TODO: Nothing in syntax defined for the purpose of the interface.
                     }
          pParams = pSpec '(' *> pList1Sep (pSpec ',') pNamedRel          <* pSpec ')'
          pArgs   = pSpec '{' *> pList1Sep (pSpec ',') (pList1 pADLid)   <* pSpec '}'
          pRoles  = pKey "FOR" *> pList1Sep (pSpec ',') pRole

pSubInterface :: AmpParser P_SubInterface
pSubInterface = (\(o,cl) objs -> P_Box o cl objs) <$> pBoxKey <*> pBox
            <|> (\(n,p) -> P_InterfaceRef p n) <$ pKey "INTERFACE" <*> pADLid_val_pos
  where pBoxKey :: AmpParser (Origin, Maybe String)
        pBoxKey = (\o mCl -> (o,mCl))     <$> pKey_pos "BOX" <*> pMaybe (pSpec '<' *> pConid <* pSpec '>')
              <|> (\o -> (o,Just "ROWS")) <$> pKey_pos "ROWS"
              <|> (\o -> (o,Just "COLS")) <$> pKey_pos "COLS"
              <|> (\o -> (o,Just "TABS")) <$> pKey_pos "TABS"

pObjDef :: AmpParser P_ObjectDef
pObjDef            = obj <$> pLabelProps
                         <*> pTerm            -- the context expression (for example: I[c])
                         <*> pMaybe (pSpec '<' *> pConid <* pSpec '>')
                         <*> pMaybe pSubInterface  -- the optional subinterface
                     where obj (Lbl nm pos' strs) expr mView msub  =
                             P_Obj { obj_nm   = nm
                                   , obj_pos  = pos'
                                   , obj_ctx  = expr
                                   , obj_mView = mView
                                   , obj_msub = msub
                                   , obj_strs = strs
                                   }
pBox :: AmpParser [P_ObjectDef]
pBox              = pSpec '[' *> pList1Sep (pSpec ',') pObjDef <* pSpec ']'

pSqlplug :: AmpParser P_ObjectDef
pSqlplug          = pKey_pos "SQLPLUG" *> pObjDef

pPhpplug :: AmpParser P_ObjectDef
pPhpplug          = pKey_pos "PHPPLUG" *> pObjDef

pPurpose :: AmpParser PPurpose
pPurpose          = rebuild <$> pKey_pos "PURPOSE"  -- "EXPLAIN" has become obsolete
                            <*> pRef2Obj
                            <*> pMaybe pLanguageRef
                            <*> pMaybe pTextMarkup
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

pPopulation :: AmpParser P_Population
pPopulation = prelpop <$> pKey_pos "POPULATION" <*> pNamedRel    <* pKey "CONTAINS" <*> pContent <|>
              pcptpop <$> pKey_pos "POPULATION" <*> pConceptName <* pKey "CONTAINS" <*> (pSpec '[' *> pListSep pComma pString <* pSpec ']')
    where
      prelpop :: Origin -> P_NamedRel -> [PAtomPair] -> P_Population
      prelpop orig nrel contents =
         P_RelPopu { p_nmdr   = nrel
                   , p_orig   = orig
                   , p_popps  = contents
                   }
      pcptpop :: Origin -> String -> [String] -> P_Population
      pcptpop    orig      cnm       contents
       = P_CptPopu { p_cnme   = cnm
                   , p_orig   = orig
                   , p_popas  = map (PAVString orig) contents
                   }

pRoleRelation :: AmpParser P_RoleRelation
pRoleRelation      = rr <$> pKey_pos "ROLE"              <*>
                            pList1Sep (pSpec ',') pRole <*
                            pKey "EDITS"                 <*>
                            pList1Sep (pSpec ',') pNamedRel
                     where rr p roles rels = P_RR roles rels p

pRoleRule :: AmpParser P_RoleRule
pRoleRule         = rr <$> pKey_pos "ROLE"               <*>
                           pList1Sep (pSpec ',') pRole  <*
                           pKey "MAINTAINS"              <*>
                           pList1Sep (pSpec ',') pADLid
                    where rr p roles rulIds = Maintain roles rulIds p
pRole :: AmpParser Role
pRole =  Role <$> pADLid

pPrintThemes :: AmpParser [String]
pPrintThemes = pKey "THEMES"
            *> pList1Sep (pSpec ',') pConceptName  -- Patterns, processes and concepts share the same name space, so these names must be checked whether the processes and patterns exist.
pMeaning :: AmpParser PMeaning
pMeaning = rebuild <$  pKey "MEANING"
                   <*> pMaybe pLanguageRef
                   <*> pMaybe pTextMarkup
                   <*> (pString <|> pExpl)
   where rebuild :: Maybe Lang -> Maybe PandocFormat -> String -> PMeaning
         rebuild    lang          fmt                   mkup   =
            PMeaning (P_Markup lang fmt mkup)
pMessage :: AmpParser PMessage
pMessage = rebuild <$ pKey "MESSAGE"
                   <*> pMaybe pLanguageRef
                   <*> pMaybe pTextMarkup
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
pRule :: AmpParser (Term TermPrim)
pRule  =  fEequ <$> pTrm1  <*>  pKey_pos "="   <*>  pTerm   <|>
          fEimp <$> pTrm1  <*>  pKey_pos "|-"  <*>  pTerm   <|>
          pTrm1
          where fequ  lExp orig rExp = PEqu orig lExp rExp
                fEimp lExp orig rExp = PImp orig lExp rExp
-- However elegant, this solution needs to be left-factored in order to get a performant parser.
-}
pRule :: AmpParser (Term TermPrim)
pRule  =  pTerm <??> (fEqu  <$> pKey_pos "="  <*> pTerm <|>
                      fImpl <$> pKey_pos "|-" <*> pTerm )
          where fEqu  orig rExp lExp = PEqu orig lExp rExp
                fImpl orig rExp lExp = PImp orig lExp rExp

{-
pTrm1 is slightly more complicated, for the purpose of avoiding "associative" brackets.
The idea is that each operator ("/\\" or "\\/") can be parsed as a sequence without brackets.
However, as soon as they are combined, brackets are needed to disambiguate the combination.
There is no natural precedence of one operator over the other.
Brackets are enforced by parsing the subexpression as pTrm5.
In order to maintain performance standards, the parser is left factored.
The functions pars and f have arguments 'combinator' and 'operator' only to avoid writing the same code twice.
-}
pTerm :: AmpParser (Term TermPrim)
pTerm   = pTrm2 <??> (f PIsc <$> pars PIsc "/\\" <|> f PUni <$> pars PUni "\\/")
          where pars combinator operator
                 = g <$> pKey_pos operator <*> pTrm2 <*> pMaybe (pars combinator operator)
                          where g orig y Nothing  = (orig, y)
                                g orig y (Just (org,z)) = (orig, combinator org y z)
                f combinator (orig, y) x = combinator orig x y

-- The left factored version of difference: (Actually, there is no need for left-factoring here, but no harm either)
pTrm2 :: AmpParser (Term TermPrim)
pTrm2   = pTrm3 <??> (f <$> pKey_pos "-" <*> pTrm3)
          where f orig rExp lExp = PDif orig lExp rExp

-- The left factored version of right- and left residuals:
pTrm3 :: AmpParser (Term TermPrim)
pTrm3  =  pTrm4 <??> (fLrs <$> pKey_pos "/" <*> pTrm4 <|> fRrs <$> pKey_pos "\\"  <*> pTrm4 <|> fDia <$> pKey_pos "<>" <*> pTrm4 )
          where fLrs orig rExp lExp = PLrs orig lExp rExp
                fRrs orig rExp lExp = PRrs orig lExp rExp
                fDia orig rExp lExp = PDia orig lExp rExp

{- by the way, a slightly different way of getting exactly the same result is:
pTrm3 :: AmpParser (Term TermPrim)
pTrm3  =  pTrm4 <??> (f <$>  (pKey_val_pos "/" <|> pKey_val_pos "\\" <|> pKey_val_pos "<>") <*> pTrm4 )
          where f ("\\", orig) rExp lExp = PRrs orig lExp rExp
                f ("/" , orig) rExp lExp = PLrs orig lExp rExp
                f (_   , orig) rExp lExp = PDia orig lExp rExp
-}

-- composition and relational addition are associative, and parsed similar to union and intersect...
pTrm4 :: AmpParser (Term TermPrim)
pTrm4   = pTrm5 <??> (f PCps <$> pars PCps ";" <|> f PRad <$> pars PRad "!" <|> f PPrd <$> pars PPrd "#")
          where pars combinator operator
                 = g <$> pKey_pos operator <*> pTrm5 <*> pMaybe (pars combinator operator)
                          where g orig y Nothing  = (orig, y)
                                g orig y (Just (org,z)) = (orig, combinator org y z)
                f combinator (orig, y) x = combinator orig x y

pTrm5 :: AmpParser (Term TermPrim)
pTrm5  =  f <$> pList (pKey_val_pos "-") <*> pTrm6  <*> pList ( pKey_val_pos "~" <|> pKey_val_pos "*" <|> pKey_val_pos "+" )
          where f ms pe (("~",_):ps) = let x=f ms pe ps in PFlp (origin x) x  -- the type checker requires that the origin of x is equal to the origin of its converse.
                f ms pe (("*",orig):ps) = PKl0 orig (f ms pe ps)              -- e*  Kleene closure (star)
                f ms pe (("+",orig):ps) = PKl1 orig (f ms pe ps)              -- e+  Kleene closure (plus)
                f (_:_:ms) pe ps        = f ms pe ps                          -- -e  complement     (unary minus)
                f ((_,orig):ms) pe ps   = let x=f ms pe ps in PCpl orig x     -- the type checker requires that the origin of x is equal to the origin of its complement.
                f _ pe _                = pe

pTrm6 :: AmpParser (Term TermPrim)
pTrm6  =  (Prim <$> pRelationRef)  <|>
          PBrk <$>  pSpec_pos '('  <*>  pTerm  <*  pSpec ')'

pRelationRef :: AmpParser TermPrim
pRelationRef      = PNamedR <$> pNamedRel                                                           <|>
                    pid   <$> pKey_pos "I"  <*> pMaybe (pSpec '[' *> pConceptOneRef <* pSpec ']')  <|>
                    pfull <$> pKey_pos "V"  <*> pMaybe pSign                                       <|>
                    singl <$> pAtom_val_pos <*> pMaybe (pSpec '[' *> pConceptOneRef <* pSpec ']')
                    where pid orig Nothing = PI orig
                          pid orig (Just c)= Pid orig c
                          pfull orig Nothing = PVee orig
                          pfull orig (Just (P_Sign src trg)) = Pfull orig src trg
                          singl (nm,orig) x  = Patm orig nm x

pNamedRel :: AmpParser P_NamedRel
pNamedRel = pnamedrel  <$> pVarid_val_pos <*> pMaybe pSign
            where pnamedrel (nm,orig) mSgn = PNamedRel orig nm mSgn

pSign :: AmpParser P_Sign
pSign = mkSign <$ pSpec '[' <*> pConceptOneRef <*> pMaybe (pKey "*" *> pConceptOneRef) <* pSpec ']'
   where mkSign :: P_Concept -> Maybe P_Concept -> P_Sign
         mkSign src mTgt =
           case mTgt of Just tgt -> P_Sign src tgt
                        Nothing  -> P_Sign src src

pConceptName ::   AmpParser String
pConceptName    = pConid <|> pString

pConceptRef ::    AmpParser P_Concept
pConceptRef     = PCpt <$> pConceptName

pConceptOneRef :: AmpParser P_Concept
pConceptOneRef  = (P_Singleton <$ pKey "ONE") <|> pConceptRef

pConceptRefPos :: AmpParser (P_Concept, Origin)
pConceptRefPos     = conid <$> pConid_val_pos   <|>   conid <$> pString_val_pos
                     where conid :: (String, Origin) ->  (P_Concept, Origin)
                           conid (c,orig) = (PCpt c, orig)

pConceptOneRefPos :: AmpParser (P_Concept, Origin)
pConceptOneRefPos  = singl <$> pKey_pos "ONE"   <|>   conid <$> pConid_val_pos   <|>   conid <$> pString_val_pos
                     where singl :: Origin ->  (P_Concept, Origin)
                           singl orig     = (P_Singleton, orig)
                           conid :: (String, Origin) ->  (P_Concept, Origin)
                           conid (c,orig) = (PCpt c, orig)

--  (SJ) Why does a label have (optional) strings?
--  (GM) This is a binding mechanism for implementation specific properties, such as SQL/PHP plug,PHP web app,etc.
--  (SJ April 15th, 2013) Since KEY has been replaced by IDENT and VIEW, there is a variant with props  (pLabelProps) and one without (pLabel).
pLabelProps :: AmpParser Label
pLabelProps       = lbl <$> pADLid_val_pos
                        <*> (pArgs `opt` [])
                        <*  pKey_pos ":"
                    where lbl :: (String, Origin) -> [[String]] -> Label
                          lbl (nm,pos') strs = Lbl nm pos' strs
                          pArgs = pSpec '{' *> pList1Sep (pSpec ',') (pList1 pADLid) <* pSpec '}'

pLabel :: AmpParser Label
pLabel       = lbl <$> pADLid_val_pos <*  pKey ":"
               where lbl :: (String, Origin) -> Label
                     lbl (nm,pos') = Lbl nm pos' []

pContent :: AmpParser [PAtomPair]
pContent          = pSpec '[' *> pListSep pComma pRecord <* pSpec ']'
                <|> pSpec '[' *> pListSep (pKey ";") pRecordObs <* pSpec ']' --obsolete
    where
    build1 :: String -> Origin -> String -> PAtomPair
    build1 l o s = mkPair o l s 
    pRecord = build1 <$> pString <*> pKey_pos "*" <*> pString
    pRecord, pRecordObs :: AmpParser PAtomPair
    build2 :: Origin -> String -> String -> PAtomPair
    build2 = mkPair
    pRecordObs = build2 <$> pSpec_pos '(' <*> pString <* pComma   <*> pString <* pSpec ')' --obsolete
 
pADLid :: AmpParser String
pADLid            = pVarid <|> pConid <|> pString

pADLid_val_pos :: AmpParser (String, Origin)
pADLid_val_pos    = pVarid_val_pos <|> pConid_val_pos <|> pString_val_pos

pMaybe :: IsParser p s => p a -> p (Maybe a)
pMaybe p = Just <$> p <|> pSucceed Nothing


get_tok_pos :: Token -> Origin
get_tok_pos     (Tok _ _ s l f) = FileLoc(FilePos (f,l,s))
get_tok_val_pos :: Token -> (String, Origin)
get_tok_val_pos (Tok _ _ s l f) = (s,FileLoc(FilePos (f,l,s)))

gsym_pos :: IsParser p Token => TokenType -> String -> String -> p Origin
gsym_pos kind val' val2' = get_tok_pos <$> pSym (Tok kind val' val2' noPos "")

gsym_val_pos :: IsParser p Token => TokenType -> String -> String -> p (String,Origin)
gsym_val_pos kind val' val2' = get_tok_val_pos <$> pSym (Tok kind val' val2' noPos "")

pKey_pos :: String -> AmpParser Origin
pKey_pos keyword  =   gsym_pos TkKeyword   keyword   keyword
pSpec_pos :: Char -> AmpParser Origin
pSpec_pos s       =   gsym_pos TkSymbol    [s]       [s]

pString_val_pos, pVarid_val_pos, pConid_val_pos, pAtom_val_pos ::  IsParser p Token => p (String,Origin)
pString_val_pos    =   gsym_val_pos TkString    ""        "?STR?"
pVarid_val_pos     =   gsym_val_pos TkVarid     ""        "?LC?"
pConid_val_pos     =   gsym_val_pos TkConid     ""        "?UC?"
pAtom_val_pos      =   gsym_val_pos TkAtom      ""        ""    -- TODO: does not escape, i.e. 'Mario\'s Pizzas' will fail to parse
pKey_val_pos ::  IsParser p Token => String -> p (String,Origin)
pKey_val_pos keyword = gsym_val_pos TkKeyword   keyword   keyword
--   pSpec_val_pos ::  IsParser p Token => Char -> p (String,Origin)
--   pSpec_val_pos s      = gsym_val_pos TkSymbol    [s]       [s]

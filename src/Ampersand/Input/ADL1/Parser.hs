{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Ampersand.Input.ADL1.Parser
  ( AmpParser,
    Include (..),
    pContext,
    pContent,
    pPopulations,
    pTerm,
    pRule,
  )
where

import Ampersand.Basics hiding (many, try)
import Ampersand.Core.ParseTree
import Ampersand.Input.ADL1.Lexer (keywords)
import Ampersand.Input.ADL1.ParsingLib
import qualified RIO.NonEmpty as NE
import qualified RIO.NonEmpty.Partial as PARTIAL
import qualified RIO.Set as Set
import qualified RIO.Text as T
import qualified RIO.Text.Partial as PARTIAL

--- Populations ::= Population+

-- | Parses a list of populations
pPopulations ::
  NameSpace ->
  -- | The population list parser
  AmpParser [P_Population]
pPopulations = many1 . pPopulation

--- Context ::= 'CONTEXT' ConceptName LanguageRef? TextMarkup? ContextElement* 'ENDCONTEXT'

-- | Parses a context
pContext ::
  -- |
  NameSpace ->
  -- | The result is the parsed context and a list of include filenames
  AmpParser (P_Context, [Include])
pContext ns =
  rebuild <$> (posOf . pKey . toText1Unsafe $ "CONTEXT")
    <*> pConceptName ns
    <*> pMaybe pLanguageRef
    <*> pMaybe pTextMarkup
    <*> many pContextElement
    <* (pKey . toText1Unsafe $ "ENDCONTEXT")
  where
    rebuild :: Origin -> Name -> Maybe Lang -> Maybe PandocFormat -> [ContextElement] -> (P_Context, [Include])
    rebuild pos' nm lang fmt ces =
      ( PCtx
          { ctx_nm = nm,
            ctx_pos = [pos'],
            ctx_lang = lang,
            ctx_markup = fmt,
            ctx_pats = [p | CPat p <- ces], -- The patterns defined in this context
            ctx_rs = [p | CRul p <- ces], -- All user defined rules in this context, but outside patterns
            ctx_ds = [p | CRel (p, _) <- ces], -- The relations defined in this context, outside the scope of patterns
            ctx_cs = [c (CONTEXT nm) | CCon c <- ces], -- The concept definitions defined in this context, outside the scope of patterns
            ctx_gs = concat [ys | CCfy ys <- ces], -- The Classify definitions defined in this context, outside the scope of patterns
            ctx_ks = [k | CIndx k <- ces], -- The identity definitions defined in this context, outside the scope of patterns
            ctx_rrules = [x | Cm x <- ces], -- The MAINTAINS statements in the context
            ctx_reprs = [r | CRep r <- ces],
            ctx_vs = [v | CView v <- ces], -- The view definitions defined in this context, outside the scope of patterns
            ctx_ifcs = [s | Cifc s <- ces], -- The interfaces defined in this context, outside the scope of patterns -- fatal ("Diagnostic: "<>concat ["\n\n   "<>show ifc | Cifc ifc<-ces])
            ctx_ps = [e | CPrp e <- ces], -- The purposes defined in this context, outside the scope of patterns
            ctx_pops = [p | CPop p <- ces] <> concat [p | CRel (_, p) <- ces], -- The populations defined in this contextplug, from POPULATION statements as well as from Relation declarations.
            ctx_metas = [meta | CMeta meta <- ces],
            ctx_enfs = [x | CEnf x <- ces]
          },
        [s | CIncl s <- ces] -- the INCLUDE filenames
      )

    --- ContextElement ::= MetaData | PatternDef | ProcessDef | RuleDef | Classify | RelationDef | ConceptDef | Index | ViewDef | Interface | Sqlplug | Phpplug | Purpose | Population | PrintThemes | IncludeStatement | Enforce
    pContextElement :: AmpParser ContextElement
    pContextElement =
      CMeta <$> pMeta
        <|> CPat <$> pPatternDef ns
        <|> CRul <$> pRuleDef ns
        <|> CCfy <$> pClassify ns
        <|> CRel <$> pRelationDef ns
        <|> CCon <$> pConceptDef ns
        <|> CRep <$> pRepresentation ns
        <|> Cm <$> pRoleRule ns
        <|> Cm <$> pServiceRule ns
        <|> CIndx <$> pIdentDef ns
        <|> CView <$> pViewDef ns
        <|> Cifc <$> pInterface ns
        <|> CPrp <$> pPurpose ns
        <|> CPop <$> pPopulation ns
        <|> CIncl <$> pIncludeStatement
        <|> CEnf <$> pEnforce ns

data ContextElement
  = CMeta MetaData
  | CPat P_Pattern
  | CRul (P_Rule TermPrim)
  | CCfy [PClassify]
  | CRel (P_Relation, [P_Population])
  | CCon (DefinitionContainer -> PConceptDef)
  | CRep Representation
  | Cm P_RoleRule
  | CIndx P_IdentDef
  | CView P_ViewDef
  | Cifc P_Interface
  | CPrp PPurpose
  | CPop P_Population
  | CIncl Include -- an INCLUDE statement
  | CEnf (P_Enforce TermPrim)

data Include = Include Origin FilePath [Text]

--- IncludeStatement ::= 'INCLUDE' Text
pIncludeStatement :: AmpParser Include
pIncludeStatement =
  Include <$> currPos
    <* (pKey . toText1Unsafe) "INCLUDE"
    <*> (T.unpack <$> pDoubleQuotedString)
    <*> (pBrackets (pDoubleQuotedString `sepBy` pComma) <|> return [])

--- LanguageRef ::= 'IN' ('DUTCH' | 'ENGLISH')
pLanguageRef :: AmpParser Lang
pLanguageRef =
  (pKey . toText1Unsafe) "IN"
    *> ( Dutch <$ (pKey . toText1Unsafe) "DUTCH"
           <|> English <$ (pKey . toText1Unsafe) "ENGLISH"
       )

--- TextMarkup ::= 'REST' | 'HTML' | 'LATEX' | 'MARKDOWN'
pTextMarkup :: AmpParser PandocFormat
pTextMarkup =
  ReST <$ (pKey . toText1Unsafe) "REST"
    <|> HTML <$ (pKey . toText1Unsafe) "HTML"
    <|> LaTeX <$ (pKey . toText1Unsafe) "LATEX"
    <|> Markdown <$ (pKey . toText1Unsafe) "MARKDOWN"

--- MetaData ::= 'META' Text Text
pMeta :: AmpParser MetaData
pMeta = MetaData <$> currPos <* (pKey . toText1Unsafe) "META" <*> pDoubleQuotedString1 <*> pDoubleQuotedString

--- PatternDef ::= 'PATTERN' ConceptName PatElem* 'ENDPATTERN'
pPatternDef :: NameSpace -> AmpParser P_Pattern
pPatternDef ns =
  rebuild <$> currPos
    <* (pKey . toText1Unsafe) "PATTERN"
    <*> pConceptName ns -- The name spaces of patterns and concepts are shared.
    <*> many (pPatElem ns)
    <*> currPos
    <* (pKey . toText1Unsafe) "ENDPATTERN"
  where
    rebuild :: Origin -> Name -> [PatElem] -> Origin -> P_Pattern
    rebuild pos' nm pes end =
      P_Pat
        { pos = pos',
          pt_nm = nm,
          pt_rls = [r | Pr r <- pes],
          pt_gns = concat [ys | Py ys <- pes],
          pt_dcs = [d | Pd (d, _) <- pes],
          pt_RRuls = [rr | Pm rr <- pes],
          pt_cds = [c (PATTERN nm) | Pc c <- pes],
          pt_Reprs = [x | Prep x <- pes],
          pt_ids = [k | Pk k <- pes],
          pt_vds = [v | Pv v <- pes],
          pt_xps = [e | Pe e <- pes],
          pt_pop = [p | Pp p <- pes] <> concat [p | Pd (_, p) <- pes],
          pt_end = end,
          pt_enfs = [e | Penf e <- pes]
        }

-- PatElem used by PATTERN
--- PatElem ::= RuleDef | Classify | RelationDef | ConceptDef | Index | ViewDef | Purpose | Population | Enforce
pPatElem :: NameSpace -> AmpParser PatElem
pPatElem ns =
  Pr <$> pRuleDef ns
    <|> Py <$> pClassify ns
    <|> Pd <$> pRelationDef ns
    <|> Pm <$> pRoleRule ns
    <|> Pm <$> pServiceRule ns
    <|> Pc <$> pConceptDef ns
    <|> Prep <$> pRepresentation ns
    <|> Pk <$> pIdentDef ns
    <|> Pv <$> pViewDef ns
    <|> Pe <$> pPurpose ns
    <|> Pp <$> pPopulation ns
    <|> Penf <$> pEnforce ns

data PatElem
  = Pr (P_Rule TermPrim)
  | Py [PClassify]
  | Pd (P_Relation, [P_Population])
  | Pm P_RoleRule
  | Pc (DefinitionContainer -> PConceptDef)
  | Prep Representation
  | Pk P_IdentDef
  | Pv P_ViewDef
  | Pe PPurpose
  | Pp P_Population
  | Penf (P_Enforce TermPrim)

--- Enforce ::= 'ENFORCE' Relation (':=' | ':<' | '>:' ) Expression
pEnforce :: NameSpace -> AmpParser (P_Enforce TermPrim)
pEnforce ns =
  P_Enforce <$> currPos
    <* (pKey . toText1Unsafe) "ENFORCE"
    <*> (PNamedR <$> pNamedRel ns)
    <*> pEnforceOperator
    <*> pTerm ns
  where
    pEnforceOperator :: AmpParser EnforceOperator
    pEnforceOperator =
      fun <$> currPos
        <*> ( (pOperator . toText1Unsafe) ":="
                <|> (pOperator . toText1Unsafe) ":<"
                <|> (pOperator . toText1Unsafe) ">:"
            )
      where
        fun orig op =
          ( case text1ToText op of
              ":=" -> IsSameSet
              ":<" -> IsSubSet
              ">:" -> IsSuperSet
              t -> fatal $ "This operator is not known: " <> t
          )
            orig

--- Classify ::= 'CLASSIFY' ConceptRef ('IS' Cterm | 'ISA' ConceptRef)
pClassify :: NameSpace -> AmpParser [PClassify] -- Example: CLASSIFY A IS B /\ C /\ D
pClassify ns =
  fun <$> currPos
    <* (pKey . toText1Unsafe) "CLASSIFY"
    <*> pConceptRef ns `sepBy1` pComma
    <*> ( (is <$ (pKey . toText1Unsafe) "IS" <*> pCterm)
            <|> (isa <$ (pKey . toText1Unsafe) "ISA" <*> pConceptRef ns)
        )
  where
    fun :: Origin -> NE.NonEmpty P_Concept -> (Bool, [P_Concept]) -> [PClassify]
    fun p lhs (isISA, rhs) = NE.toList $ fmap f lhs
      where
        f s =
          PClassify
            { pos = p,
              specific = s,
              generics = if isISA then s NE.:| rhs else PARTIAL.fromList rhs
            }
    --- Cterm ::= Cterm1 ('/\' Cterm1)*
    --- Cterm1 ::= ConceptRef | ('('? Cterm ')'?)
    pCterm = concat <$> pCterm1 `sepBy1` (pOperator . toText1Unsafe) "/\\"
    pCterm1 =
      pure <$> pConceptRef ns
        <|> pParens pCterm -- brackets are allowed for educational reasons.
    is :: [P_Concept] -> (Bool, [P_Concept])
    is gens = (False, gens)
    isa :: P_Concept -> (Bool, [P_Concept])
    isa gen = (True, [gen])

--- RuleDef ::= 'RULE' Label? Rule Meaning* Message* Violation?
pRuleDef :: NameSpace -> AmpParser (P_Rule TermPrim)
pRuleDef ns =
  build <$> currPos
    <* (pKey . toText1Unsafe) "RULE"
    <*> pMaybe pLabelAndColon
    <*> pRule ns
    <*> many pMeaning
    <*> many pMessage
    <*> pMaybe pViolation
  where
    build orig mlabel term meanings messages mViolation =
      P_Rule
        { pos = orig,
          rr_nm = toName ns $
            case mlabel of
              Nothing -> rulid orig
              Just lbl -> lbl,
          rr_exp = term,
          rr_mean = meanings,
          rr_msg = messages,
          rr_viol = mViolation
        }
    rulid :: Origin -> Text1
    rulid (FileLoc pos' _) = toText1Unsafe $ "rule@" <> tshow pos'
    rulid _ = fatal "pRuleDef is expecting a file location."

    --- Violation ::= 'VIOLATION' PairView
    pViolation :: AmpParser (PairView (Term TermPrim))
    pViolation = id <$ (pKey . toText1Unsafe) "VIOLATION" <*> pPairView

    --- PairView ::= '(' PairViewSegmentList ')'
    pPairView :: AmpParser (PairView (Term TermPrim))
    pPairView = PairView <$> pParens (pPairViewSegment `sepBy1` pComma)
    --    where f xs = PairView {ppv_segs = xs}

    --- PairViewSegmentList ::= PairViewSegment (',' PairViewSegment)*
    --- PairViewSegment ::= 'SRC' Term | 'TGT' Term | 'TXT' Text
    pPairViewSegment :: AmpParser (PairViewSegment (Term TermPrim))
    pPairViewSegment =
      PairViewExp <$> posOf ((pKey . toText1Unsafe) "SRC") <*> return Src <*> pTerm ns
        <|> PairViewExp <$> posOf ((pKey . toText1Unsafe) "TGT") <*> return Tgt <*> pTerm ns
        <|> PairViewText <$> posOf ((pKey . toText1Unsafe) "TXT") <*> pDoubleQuotedString

--- RelationDef ::= (RelationNew | RelationOld) Props? RelDefaults? ('PRAGMA' Text+)? Meaning* ('=' Content)? '.'?
pRelationDef :: NameSpace -> AmpParser (P_Relation, [P_Population])
pRelationDef ns =
  reorder <$> currPos
    <*> (pRelationNew ns <|> pRelationOld ns)
    <*> optSet pProps
    <*> optList pRelDefaults
    <*> pMaybe pPragma
    <*> many pMeaning
    <*> optList ((pOperator . toText1Unsafe) "=" *> pContent)
    <* optional ((pOperator . toText1Unsafe) ".")
  where
    reorder pos' (nm, sign, fun) prop dflts pragma meanings prs =
      (P_Relation nm sign props dflts pragma meanings pos', map pair2pop prs)
      where
        props = prop `Set.union` fun
        pair2pop :: PAtomPair -> P_Population
        pair2pop a = P_RelPopu Nothing Nothing (origin a) rel [a]
        rel :: P_NamedRel -- the named relation
        rel = PNamedRel pos' nm (Just sign)

--- Pragma ::'PRAGMA' Text+
pPragma :: AmpParser Pragma
pPragma =
  build
    <$> currPos <* (pKey . toText1Unsafe) "PRAGMA"
    <*> pMaybe pDoubleQuotedString
    <*> pMaybe pDoubleQuotedString
    <*> pMaybe pDoubleQuotedString
  where
    build :: Origin -> Maybe Text -> Maybe Text -> Maybe Text -> Pragma
    build orig a b c =
      Pragma
        { pos = orig,
          praLeft = fromMaybe "" a,
          praMid = fromMaybe "" b,
          praRight = fromMaybe "" c
        }

--- RelDefaults ::= 'DEFAULT' RelDefault*
pRelDefaults :: AmpParser [PRelationDefault]
pRelDefaults = (pKey . toText1Unsafe) "DEFAULT" *> (toList <$> many1 pRelDefault)

--- RelDefault ::= ( 'SRC' | 'TGT' ) ( ('VALUE' AtomValue (',' AtomValue)*) | ('EVALPHP' '<DoubleQuotedString>') )
pRelDefault :: AmpParser PRelationDefault
pRelDefault =
  build <$> pSrcOrTgt
    <*> pDef
  where
    build :: SrcOrTgt -> Either (NE.NonEmpty PAtomValue) Text -> PRelationDefault
    build st (Left vals) = PDefAtom st vals
    build st (Right txt) = PDefEvalPHP st txt
    pDef :: AmpParser (Either (NE.NonEmpty PAtomValue) Text)
    pDef = pAtom <|> pPHP
    pAtom =
      Left <$ (pKey . toText1Unsafe) "VALUE"
        <*> sepBy1 pAtomValue pComma
    pPHP =
      Right <$ (pKey . toText1Unsafe) "EVALPHP"
        <*> pDoubleQuotedString
    pSrcOrTgt =
      Src <$ (pKey . toText1Unsafe) "SRC"
        <|> Tgt <$ (pKey . toText1Unsafe) "TGT"

--- RelationNew ::= 'RELATION' Varid Signature
pRelationNew :: NameSpace -> AmpParser (Name, P_Sign, PProps)
pRelationNew ns =
  (,,) <$ (pKey . toText1Unsafe) "RELATION"
    <*> pLowerCaseName ns
    <*> pSign ns
    <*> return Set.empty

--- RelationOld ::= Varid '::' ConceptRef Fun ConceptRef
pRelationOld :: NameSpace -> AmpParser (Name, P_Sign, PProps)
pRelationOld ns =
  relOld <$> pLowerCaseName ns
    <* (pOperator . toText1Unsafe) "::"
    <*> pConceptRef ns
    <*> pFun
    <*> pConceptRef ns
  where
    relOld nm src fun tgt = (nm, P_Sign src tgt, fun)

--- Props ::= '[' PropList? ']'
pProps :: AmpParser (Set.Set PProp)
pProps = normalizeProps <$> pBrackets (pProp `sepBy` pComma)
  where
    --- PropList ::= Prop (',' Prop)*
    --- Prop ::= 'UNI' | 'INJ' | 'SUR' | 'TOT' | 'SYM' | 'ASY' | 'TRN' | 'RFX' | 'IRF' | 'PROP'
    pProp :: AmpParser PProp
    pProp = choice [p <$ pKey (toText1Unsafe $ tshow p) | p <- [minBound ..]]
    normalizeProps :: [PProp] -> PProps
    normalizeProps = conv . rep . Set.fromList
      where
        -- replace PROP by SYM, ASY
        rep :: PProps -> PProps
        rep ps
          | P_Prop `elem` ps = Set.fromList [P_Sym, P_Asy] `Set.union` (P_Prop `Set.delete` ps)
          | otherwise = ps
        -- add Uni and Inj if ps has neither Sym nor Asy
        conv :: PProps -> PProps
        conv ps =
          ps
            `Set.union` if P_Sym `elem` ps && P_Asy `elem` ps
              then Set.fromList [P_Uni, P_Inj]
              else Set.empty

--- Fun ::= '*' | '->' | '<-' | '[' Mults ']'
pFun :: AmpParser PProps
pFun =
  Set.empty <$ (pOperator . toText1Unsafe) "*"
    <|> Set.fromList [P_Uni, P_Tot] <$ (pOperator . toText1Unsafe) "->"
    <|> Set.fromList [P_Sur, P_Inj] <$ (pOperator . toText1Unsafe) "<-"
    <|> pBrackets pMults
  where
    --- Mults ::= Mult '-' Mult
    pMults :: AmpParser PProps
    pMults =
      Set.union <$> optSet (pMult (P_Sur, P_Inj))
        <* pDash
        <*> optSet (pMult (P_Tot, P_Uni))

    --- Mult ::= ('0' | '1') '..' ('1' | '*') | '*' | '1'
    --TODO: refactor to Mult ::= '0' '..' ('1' | '*') | '1'('..' ('1' | '*'))? | '*'
    pMult :: (PProp, PProp) -> AmpParser PProps
    pMult (ts, ui) =
      Set.union <$> (Set.empty <$ pZero <|> Set.singleton ts <$ try pOne)
        <* (pOperator . toText1Unsafe) ".."
        <*> (Set.singleton ui <$ try pOne <|> (Set.empty <$ (pOperator . toText1Unsafe) "*"))
          <|> Set.empty <$ (pOperator . toText1Unsafe) "*"
          <|> Set.fromList [ts, ui] <$ try pOne

--- ConceptDef ::= 'CONCEPT' ConceptName Text ('TYPE' Text)? Text?
pConceptDef :: NameSpace -> AmpParser (DefinitionContainer -> PConceptDef)
pConceptDef ns =
  PConceptDef <$> currPos
    <* (pKey . toText1Unsafe) "CONCEPT"
    <*> pConceptName ns
    <*> pPCDDef2
    <*> many pMeaning
  where
    pPCDDef2 :: AmpParser PCDDef
    pPCDDef2 =
      ( PCDDefLegacy <$> (pDoubleQuotedString <?> "concept definition (string)")
          <*> (pDoubleQuotedString `opt` "") -- a reference to the source of this definition.
      )
        <|> (PCDDefNew <$> pMeaning)

--- Representation ::= 'REPRESENT' ConceptNameList 'TYPE' AdlTType
pRepresentation :: NameSpace -> AmpParser Representation
pRepresentation ns =
  Repr <$> currPos
    <* (pKey . toText1Unsafe) "REPRESENT"
    <*> pConceptRef ns `sepBy1` pComma
    <* (pKey . toText1Unsafe) "TYPE"
    <*> pAdlTType

--- AdlTType = ...<enumeration>
pAdlTType :: AmpParser TType
pAdlTType =
  k Alphanumeric "ALPHANUMERIC"
    <|> k BigAlphanumeric "BIGALPHANUMERIC"
    <|> k HugeAlphanumeric "HUGEALPHANUMERIC"
    <|> k Password "PASSWORD"
    <|> k Binary "BINARY"
    <|> k BigBinary "BIGBINARY"
    <|> k HugeBinary "HUGEBINARY"
    <|> k Date "DATE"
    <|> k DateTime "DATETIME"
    <|> k Boolean "BOOLEAN"
    <|> k Integer "INTEGER"
    <|> k Float "FLOAT"
    <|> k Object "OBJECT"
  where
    k tt str = f <$> (pKey . toText1Unsafe) str where f _ = tt

-- | A identity definition looks like:   IDENT onNameAdress : Person(name, address),
-- which means that name<>name~ /\ address<>addres~ |- I[Person].
-- The label 'onNameAddress' is used to refer to this identity.
-- You may also use an expression on each attribute place, for example: IDENT onpassport: Person(nationality, passport;documentnr),
-- which means that nationality<>nationality~ /\ passport;documentnr<>(passport;documentnr)~ |- I[Person].

--- IdentDef ::= 'IDENT' Label ConceptRef '(' IndSegmentList ')'
pIdentDef :: NameSpace -> AmpParser P_IdentDef
pIdentDef ns =
  P_Id <$> currPos
    <* (pKey . toText1Unsafe) "IDENT"
    <*> pNameAndColon ns
    <*> pConceptRef ns
    <*> pParens (pIdentSegment `sepBy1` pComma)
  where
    --- IndSegmentList ::= Attr (',' Attr)*
    pIdentSegment :: AmpParser P_IdentSegment
    pIdentSegment = P_IdentExp <$> pAtt
    --- Attr ::= Term
    pAtt :: AmpParser P_BoxBodyElement
    pAtt = rebuild <$> currPos <*> pTerm ns
      where
        rebuild pos' ctx =
          P_BxExpr
            { box_label = Nothing,
              pos = pos',
              obj_ctx = ctx,
              obj_crud = Nothing,
              obj_mView = Nothing,
              obj_msub = Nothing
            }

--- ViewDef ::= FancyViewDef | ViewDefLegacy
pViewDef :: NameSpace -> AmpParser P_ViewDef
pViewDef ns = try (pViewDefImproved ns) <|> pViewDefLegacy ns -- introduces backtracking, but is more elegant than rewriting pViewDefLegacy to disallow "KEY ... ENDVIEW".

--- FancyViewDef ::= 'VIEW' Label ConceptOneRef 'DEFAULT'? ('{' ViewObjList '}')?  HtmlView? 'ENDVIEW'
pViewDefImproved :: NameSpace -> AmpParser P_ViewDef
pViewDefImproved ns =
  mkViewDef <$> currPos
    <* (pKey . toText1Unsafe) "VIEW"
    <*> pNameAndColon ns
    <*> pConceptOneRef ns
    <*> pIsThere ((pKey . toText1Unsafe) "DEFAULT")
    <*> pBraces (pViewSegment ns `sepBy` pComma) `opt` []
    <*> pMaybe pHtmlView
    <* (pKey . toText1Unsafe) "ENDVIEW"
  where
    mkViewDef pos' nm cpt isDef ats html =
      P_Vd
        { pos = pos',
          vd_lbl = nm,
          vd_cpt = cpt,
          vd_isDefault = isDef,
          vd_html = html,
          vd_ats = ats
        }
    --- ViewSegmentList ::= ViewSegment (',' ViewSegment)*
    --- HtmlView ::= 'HTML' 'TEMPLATE' Text
    pHtmlView :: AmpParser ViewHtmlTemplate
    pHtmlView =
      ViewHtmlTemplateFile
        <$ (pKey . toText1Unsafe) "HTML"
        <* (pKey . toText1Unsafe) "TEMPLATE"
        <*> (T.unpack <$> pDoubleQuotedString)

--- ViewSegmentLoad ::= Term | 'TXT' Text
pViewSegmentLoad :: NameSpace -> AmpParser (P_ViewSegmtPayLoad TermPrim)
pViewSegmentLoad ns =
  P_ViewExp <$> pTerm ns
    <|> P_ViewText <$ (pKey . toText1Unsafe) "TXT" <*> pDoubleQuotedString

--- ViewSegment ::= Label ViewSegmentLoad
pViewSegment :: NameSpace -> AmpParser (P_ViewSegment TermPrim)
pViewSegment ns =
  build <$> currPos
    <*> pMaybe pLabelAndColon
    <*> pViewSegmentLoad ns
  where
    build :: Origin -> Maybe Text1 -> P_ViewSegmtPayLoad TermPrim -> P_ViewSegment TermPrim
    build orig lbl load =
      P_ViewSegment
        { vsm_labl = lbl,
          pos = orig,
          vsm_load = load
        }

--- ViewDefLegacy ::= 'VIEW' Label ConceptOneRef '(' ViewSegmentList ')'
pViewDefLegacy :: NameSpace -> AmpParser P_ViewDef
pViewDefLegacy ns =
  P_Vd <$> currPos
    <* (pKey . toText1Unsafe) "VIEW"
    <*> pNameAndColon ns
    <*> pConceptOneRef ns
    <*> return True
    <*> return Nothing
    <*> pParens (pViewSegment ns `sepBy` pComma)

--- Interface ::= 'INTERFACE' ADLid Params? Roles? ':' Term (ADLid | Conid)? SubInterface?
pInterface :: NameSpace -> AmpParser P_Interface
pInterface ns =
  lbl <$> currPos
    <*> pInterfaceIsAPI
    <*> pUnrestrictedName ns
    <*> pMaybe pParams
    <*> pMaybe pRoles
    <*> (pColon *> pTerm ns) -- the expression of the interface object
    <*> pMaybe pCruds -- The Crud-string (will later be tested, that it can contain only characters crud (upper/lower case)
    <*> pMaybe (pChevrons $ pUpperCaseName ns) -- The view that should be used for this object
    <*> pSubInterface ns
  where
    lbl :: Origin -> Bool -> Name -> a -> Maybe (NE.NonEmpty Role) -> Term TermPrim -> Maybe P_Cruds -> Maybe Name -> P_SubInterface -> P_Interface
    lbl p isAPI nm _params roles ctx mCrud mView sub =
      P_Ifc
        { ifc_IsAPI = isAPI,
          ifc_Name = nm,
          ifc_Roles = maybe [] NE.toList roles,
          ifc_Obj =
            P_BxExpr
              { box_label = Just (tName nm),
                pos = p,
                obj_ctx = ctx,
                obj_crud = mCrud,
                obj_mView = mView,
                obj_msub = Just sub
              },
          pos = p,
          ifc_Prp = "" --TODO: Nothing in syntax defined for the purpose of the interface.
        }
    --- Params ::= '(' NamedRel ')'
    pParams = pParens (pNamedRel ns `sepBy1` pComma)
    --- Roles ::= 'FOR' RoleList
    pRoles = (pKey . toText1Unsafe) "FOR" *> pRole ns False `sepBy1` pComma

--- SubInterface ::= 'BOX' BoxHeader? Box | 'LINKTO'? 'INTERFACE' ADLid
pSubInterface :: NameSpace -> AmpParser P_SubInterface
pSubInterface ns =
  P_Box <$> currPos <*> pBoxHeader <*> pBoxBody
    <|> P_InterfaceRef
      <$> currPos
      <*> pIsThere ((pKey . toText1Unsafe) "LINKTO") <* pInterfaceKey
      <*> pUnrestrictedName ns
  where
    --- Box ::= '[' ObjDefList ']'
    pBoxBody :: AmpParser [P_BoxBodyElement]
    pBoxBody = pBrackets $ pBoxBodyElement ns `sepBy` pComma
    pBoxHeader :: AmpParser BoxHeader
    pBoxHeader =
      build <$> currPos <* (pKey . toText1Unsafe) "BOX" <*> optional pBoxSpecification
    build :: Origin -> Maybe (Text1, [TemplateKeyValue]) -> BoxHeader
    build o x = BoxHeader o typ keys
      where
        (typ, keys) = case x of
          Nothing -> (toText1Unsafe "FORM", [])
          Just (boxtype, atts) -> (boxtype, atts)
    pBoxSpecification :: AmpParser (Text1, [TemplateKeyValue])
    pBoxSpecification =
      pChevrons $
        (,) <$> (pSingleWord <|> anyKeyWord)
          <*> many pTemplateKeyValue

    pTemplateKeyValue :: AmpParser TemplateKeyValue
    pTemplateKeyValue =
      TemplateKeyValue
        <$> currPos
        <*> (pSingleWord <|> anyKeyWord)
        <*> optional (id <$ (pOperator . toText1Unsafe) "=" <*> pDoubleQuotedString)

anyKeyWord :: AmpParser Text1
anyKeyWord = case map pKey keywords of
  [] -> fatal "We should have keywords."
  h : tl -> foldr (<|>) h tl

--- ObjDef ::= Label Term ('<' Conid '>')? SubInterface?
--- ObjDefList ::= ObjDef (',' ObjDef)*
pBoxBodyElement :: NameSpace -> AmpParser P_BoxBodyElement
pBoxBodyElement ns =
  try pBoxItemTerm
    <|> try pBoxItemText -- We need `try` becausein the Term, the label is mandatory, while in Text it is optional.
  where
    pBoxItemTerm :: AmpParser P_BoxBodyElement
    pBoxItemTerm =
      build
        <$> currPos
        <*> pLabelAndColon
        <*> pTerm ns -- the context expression (for example: I[c])
        <*> pMaybe pCruds
        <*> pMaybe (pChevrons $ pUpperCaseName ns) --for the view
        <*> pMaybe (pSubInterface ns) -- the optional subinterface
      where
        build orig lab term mCrud mView msub =
          P_BxExpr
            { box_label = Just lab,
              pos = orig,
              obj_ctx = term,
              obj_crud = mCrud,
              obj_mView = mView,
              obj_msub = msub
            }
    pBoxItemText :: AmpParser P_BoxBodyElement
    pBoxItemText =
      build
        <$> currPos
        <*> pMaybe pLabelAndColon
        <* (pKey . toText1Unsafe) "TXT"
        <*> pDoubleQuotedString1
      where
        build orig lab txt =
          P_BxTxt
            { box_label = lab,
              pos = orig,
              box_txt = txt
            }

--- Cruds ::= crud in upper /lowercase combinations
pCruds :: AmpParser P_Cruds
pCruds = P_Cruds <$> currPos <*> pCrudString

--- Purpose ::= 'PURPOSE' Ref2Obj LanguageRef? TextMarkup? ('REF' StringListSemi)? Expl
pPurpose :: NameSpace -> AmpParser PPurpose
pPurpose ns =
  rebuild <$> currPos
    <* (pKey . toText1Unsafe) "PURPOSE"
    <*> pRef2Obj
    <*> pMaybe pLanguageRef
    <*> pMaybe pTextMarkup
    <*> pMaybe ((pKey . toText1Unsafe) "REF" *> pDoubleQuotedString `sepBy1` pSemi)
    <*> pAmpersandMarkup
  where
    rebuild :: Origin -> PRef2Obj -> Maybe Lang -> Maybe PandocFormat -> Maybe (NE.NonEmpty Text) -> Text -> PPurpose
    rebuild orig obj lang fmt refs str =
      PRef2 orig obj (P_Markup lang fmt str) (concatMap splitOnSemicolon (maybe [] NE.toList refs))
      where
        -- TODO: This separation should not happen in the parser
        splitOnSemicolon :: Text -> [Text]
        splitOnSemicolon = PARTIAL.splitOn ";" -- This is safe: The first argument of splitOn must not be empty.
        --- Ref2Obj ::= 'CONCEPT' ConceptName | 'RELATION' NamedRel | 'RULE' ADLid | 'IDENT' ADLid | 'VIEW' ADLid | 'PATTERN' ADLid | 'INTERFACE' ADLid | 'CONTEXT' ADLid
    pRef2Obj :: AmpParser PRef2Obj
    pRef2Obj =
      PRef2ConceptDef <$ (pKey . toText1Unsafe) "CONCEPT" <*> pConceptName ns
        <|> PRef2Relation <$ (pKey . toText1Unsafe) "RELATION" <*> pNamedRel ns
        <|> PRef2Rule <$ (pKey . toText1Unsafe) "RULE" <*> pUnrestrictedName ns
        <|> PRef2IdentityDef <$ (pKey . toText1Unsafe) "IDENT" <*> pUnrestrictedName ns
        <|> PRef2ViewDef <$ (pKey . toText1Unsafe) "VIEW" <*> pUnrestrictedName ns
        <|> PRef2Pattern <$ (pKey . toText1Unsafe) "PATTERN" <*> pUnrestrictedName ns
        <|> PRef2Interface <$ pInterfaceKey <*> pUnrestrictedName ns
        <|> PRef2Context <$ (pKey . toText1Unsafe) "CONTEXT" <*> pUnrestrictedName ns

pInterfaceKey :: AmpParser Text1
pInterfaceKey = pKey (toText1Unsafe "INTERFACE") <|> pKey (toText1Unsafe "API") -- On special request of Rieks, the keyword "API" is allowed everywhere where the keyword "INTERFACE" is used. https://github.com/AmpersandTarski/Ampersand/issues/789

pInterfaceIsAPI :: AmpParser Bool
pInterfaceIsAPI = (toText1Unsafe "API" ==) <$> pInterfaceKey

--- Population ::= 'POPULATION' (NamedRel 'CONTAINS' Content | ConceptName 'CONTAINS' '[' ValueList ']')

-- | Parses a population
pPopulation ::
  NameSpace ->
  -- | The population parser
  AmpParser P_Population
pPopulation ns =
  (pKey . toText1Unsafe) "POPULATION"
    *> ( P_RelPopu Nothing Nothing <$> currPos <*> pNamedRel ns <* (pKey . toText1Unsafe) "CONTAINS" <*> pContent
           <|> P_CptPopu <$> currPos <*> pConceptRef ns <* (pKey . toText1Unsafe) "CONTAINS" <*> pBrackets (pAtomValue `sepBy` pComma)
       )

--- RoleRule ::= 'ROLE' RoleList 'MAINTAINS' ADLidList
--TODO: Rename the RoleRule to RoleMantains and RoleRelation to RoleEdits.
pRoleRule :: NameSpace -> AmpParser P_RoleRule
pRoleRule ns =
  try
    ( Maintain <$> currPos
        <* (pKey . toText1Unsafe) "ROLE"
        <*> pRole ns False `sepBy1` pComma
        <* (pKey . toText1Unsafe) "MAINTAINS"
    )
    <*> pUnrestrictedName ns `sepBy1` pComma

--- ServiceRule ::= 'SERVICE' RoleList 'MAINTAINS' ADLidList
--TODO: Rename the RoleRule to RoleMantains and RoleRelation to RoleEdits.
pServiceRule :: NameSpace -> AmpParser P_RoleRule
pServiceRule ns =
  try
    ( Maintain <$> currPos
        <* (pKey . toText1Unsafe) "SERVICE"
        <*> pRole ns True `sepBy1` pComma
        <* (pKey . toText1Unsafe) "MAINTAINS"
    )
    <*> pUnrestrictedName ns `sepBy1` pComma

--- Role ::= ADLid
--- RoleList ::= Role (',' Role)*
pRole :: NameSpace -> Bool -> AmpParser Role
pRole ns isService = (if isService then Service else Role) <$> pUnrestrictedName ns

--- Meaning ::= 'MEANING' Markup
pMeaning :: AmpParser PMeaning
pMeaning =
  PMeaning <$ (pKey . toText1Unsafe) "MEANING"
    <*> pMarkup

--- Message ::= 'MESSAGE' Markup
pMessage :: AmpParser PMessage
pMessage = PMessage <$ (pKey . toText1Unsafe) "MESSAGE" <*> pMarkup

--- Markup ::= LanguageRef? TextMarkup? (Text | Expl)
pMarkup :: AmpParser P_Markup
pMarkup =
  P_Markup
    <$> pMaybe pLanguageRef
    <*> pMaybe pTextMarkup
    <*> (pDoubleQuotedString <|> pAmpersandMarkup)

--- Rule ::= Term ('=' Term | '|-' Term)?

-- | Parses a rule
pRule ::
  NameSpace ->
  -- | The rule parser
  AmpParser (Term TermPrim)
pRule ns =
  pTerm ns
    <??> ( invert PEqu <$> currPos <* (pOperator . toText1Unsafe) "=" <*> pTerm ns
             <|> invert PInc <$> currPos <* (pOperator . toText1Unsafe) "|-" <*> pTerm ns
         )

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
pTerm ::
  NameSpace ->
  -- | The term parser
  AmpParser (Term TermPrim)
pTerm ns =
  pTrm2 ns
    <??> ( invertT PIsc <$> rightAssociate PIsc (toText1Unsafe "/\\") (pTrm2 ns)
             <|> invertT PUni <$> rightAssociate PUni (toText1Unsafe "\\/") (pTrm2 ns)
         )

-- The left factored version of difference: (Actually, there is no need for left-factoring here, but no harm either)
--- Trm2 ::= Trm3 ('-' Trm3)?
pTrm2 :: NameSpace -> AmpParser (Term TermPrim)
pTrm2 ns = pTrm3 ns <??> (invert PDif <$> posOf pDash <*> pTrm3 ns)

-- The left factored version of right- and left residuals:
--- Trm3 ::= Trm4 ('/' Trm4 | '\' Trm4 | '<>' Trm4)?
pTrm3 :: NameSpace -> AmpParser (Term TermPrim)
pTrm3 ns =
  pTrm4 ns
    <??> ( invert PLrs <$> currPos <* (pOperator . toText1Unsafe) "/" <*> pTrm4 ns
             <|> invert PRrs <$> currPos <* (pOperator . toText1Unsafe) "\\" <*> pTrm4 ns
             <|> invert PDia <$> currPos <* (pOperator . toText1Unsafe) "<>" <*> pTrm4 ns
         )

-- composition and relational addition are associative, and parsed similar to union and intersect...
--- Trm4 ::= Trm5 ((';' Trm5)+ | ('!' Trm5)+ | ('#' Trm5)+)?
pTrm4 :: NameSpace -> AmpParser (Term TermPrim)
pTrm4 ns =
  pTrm5 ns
    <??> ( invertT PCps <$> rightAssociate PCps (toText1Unsafe ";") (pTrm5 ns)
             <|> invertT PRad <$> rightAssociate PRad (toText1Unsafe "!") (pTrm5 ns)
             <|> invertT PPrd <$> rightAssociate PPrd (toText1Unsafe "#") (pTrm5 ns)
         )

--- Trm5 ::= '-'* Trm6 ('~' | '*' | '+')*
pTrm5 :: NameSpace -> AmpParser (Term TermPrim)
--TODO: Separate into prefix and postfix top-level functions
pTrm5 ns =
  f <$> many (valPosOf pDash)
    <*> pTrm6 ns
    <*> many
      ( valPosOf
          ( (pOperator . toText1Unsafe) "~"
              <|> (pOperator . toText1Unsafe) "*"
              <|> (pOperator . toText1Unsafe) "+"
          )
      )
  where
    f :: [(a, Origin)] -> Term TermPrim -> [(Text1, Origin)] -> Term TermPrim
    f ms pe ((Text1 '~' _, _) : ps) = let x = f ms pe ps in PFlp (origin x) x -- the type checker requires that the origin of x is equal to the origin of its converse.
    f ms pe ((Text1 '*' _, orig) : ps) = PKl0 orig (f ms pe ps) -- e*  Kleene closure (star)
    f ms pe ((Text1 '+' _, orig) : ps) = PKl1 orig (f ms pe ps) -- e+  Kleene closure (plus)
    f (_ : _ : ms) pe ps = f ms pe ps -- -e  complement     (unary minus)
    f ((_, orig) : ms) pe ps = let x = f ms pe ps in PCpl orig x -- the type checker requires that the origin of x is equal to the origin of its complement.
    f _ pe _ = pe

--- Trm6 ::= RelationRef | '(' Term ')'
pTrm6 :: NameSpace -> AmpParser (Term TermPrim)
pTrm6 ns =
  Prim <$> pRelationRef ns
    <|> PBrk <$> currPos <*> pParens (pTerm ns)

-- Help function for several expressions. The type 't' is each of the terms.
invert :: (Origin -> t -> t -> t) -> Origin -> t -> t -> t
invert constructor position rightTerm leftTerm = constructor position leftTerm rightTerm

-- Variant for the above function with a tuple, for usage with right association
invertT :: (Origin -> t -> t -> t) -> (Origin, t) -> t -> t
invertT constructor (position, rightTerm) leftTerm = constructor position leftTerm rightTerm

-- Help function for pTerm and pTrm4, to allow right association
rightAssociate :: (Origin -> t -> t -> t) -> Text1 -> AmpParser t -> AmpParser (Origin, t)
rightAssociate combinator operator term =
  g <$> currPos <* pOperator operator <*> term <*> pMaybe (rightAssociate combinator operator term)
  where
    g orig y Nothing = (orig, y)
    g orig y (Just (org, z)) = (orig, combinator org y z)

--- RelationRef ::= NamedRel | 'I' ('[' ConceptOneRef ']')? | 'V' Signature? | Singleton ('[' ConceptOneRef ']')?
pRelationRef :: NameSpace -> AmpParser TermPrim
pRelationRef ns =
  PNamedR <$> pNamedRel ns
    <|> pid <$> currPos <* (pKey . toText1Unsafe) "I" <*> (pMaybe . pBrackets $ pConceptOneRef ns)
    <|> pfull <$> currPos <* (pKey . toText1Unsafe) "V" <*> pMaybe (pSign ns)
    <|> Patm <$> currPos <*> pSingleton <*> (pMaybe . pBrackets $ pConceptOneRef ns)
  where
    pid orig Nothing = PI orig
    pid orig (Just c) = Pid orig c
    pfull orig Nothing = PVee orig
    pfull orig (Just (P_Sign src trg)) = Pfull orig src trg

pSingleton :: AmpParser PAtomValue
pSingleton =
  value2PAtomValue <$> currPos
    <*> ( pAtomValInPopulation True
            <|> pBraces (pAtomValInPopulation False)
        )

pAtomValue :: AmpParser PAtomValue
pAtomValue = value2PAtomValue <$> currPos <*> pAtomValInPopulation False

value2PAtomValue :: Origin -> Value -> PAtomValue
value2PAtomValue o v = case v of
  VSingleton s x -> PSingleton o s (fmap (value2PAtomValue o) x)
  VRealString s -> ScriptString o s
  VInt i -> ScriptInt o (toInteger i)
  VFloat x -> ScriptFloat o x
  VBoolean b -> ComnBool o b
  VDateTime x -> ScriptDateTime o x
  VDate x -> ScriptDate o x

--- NamedRelList ::= NamedRel (',' NamedRel)*
--- NamedRel ::= Varid Signature?
pNamedRel :: NameSpace -> AmpParser P_NamedRel
pNamedRel ns = PNamedRel <$> currPos <*> pLowerCaseName ns <*> pMaybe (pSign ns)

--- Signature ::= '[' ConceptOneRef ('*' ConceptOneRef)? ']'
pSign :: NameSpace -> AmpParser P_Sign
pSign ns = pBrackets sign
  where
    sign = mkSign <$> pConceptOneRef ns <*> pMaybe ((pOperator . toText1Unsafe) "*" *> pConceptOneRef ns)
    mkSign src mTgt = P_Sign src (fromMaybe src mTgt)

--- ConceptName ::= Conid | Text
--- ConceptNameList ::= ConceptName (',' ConceptName)
pConceptName :: NameSpace -> AmpParser Name
pConceptName = pUpperCaseName

--- ConceptRef ::= ConceptName
pConceptRef :: NameSpace -> AmpParser P_Concept
pConceptRef ns = PCpt <$> pConceptName ns

--- ConceptOneRef ::= 'ONE' | ConceptRef
pConceptOneRef :: NameSpace -> AmpParser P_Concept
pConceptOneRef ns = (P_ONE <$ (pKey . toText1Unsafe) "ONE") <|> pConceptRef ns

--- Label ::= ADLid ':'
pLabelAndColon :: AmpParser Text1
pLabelAndColon = pUnrestrictedLabel <* pColon

pUnrestrictedLabel :: AmpParser Text1
pUnrestrictedLabel = pSingleWord <|> anyKeyWord <|> pDoubleQuotedString1

pNameAndColon :: NameSpace -> AmpParser Name --TODO: Dit moet subtieler, omdat er ook namen kunnen bestaan met een NameSpace-gedeelte als prefix.
pNameAndColon ns = toName ns <$> pLabelAndColon

--- Content ::= '[' RecordList? ']'
pContent :: AmpParser [PAtomPair]
pContent = pBrackets (pRecord `sepBy` (pComma <|> pSemi))
  where
    --- RecordList ::= Record ((','|';') Record)*
    --- Record ::= Text ',' Text
    pRecord :: AmpParser PAtomPair
    pRecord =
      pParens
        ( PPair <$> currPos
            <*> pAtomValue
            <* pComma
            <*> pAtomValue
        )

--- ADLid ::= Varid | Conid
--- ADLidList ::= ADLid (',' ADLid)*
--- ADLidListList ::= ADLid+ (',' ADLid+)*
pUnrestrictedName :: NameSpace -> AmpParser Name
pUnrestrictedName ns = pLowerCaseName ns <|> pUpperCaseName ns

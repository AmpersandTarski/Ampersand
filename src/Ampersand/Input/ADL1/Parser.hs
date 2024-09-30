{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Ampersand.Input.ADL1.Parser
  ( AmpParser,
    ParserState,
    Include (..),
    initialParserState,
    pContext,
    pContent,
    pPopulations,
    pTerm,
    pRule,
  )
where

import Ampersand.Basics hiding (many, try)
import Ampersand.Core.ParseTree
import Ampersand.Input.ADL1.ParsingLib
import qualified RIO.NonEmpty as NE
import qualified RIO.NonEmpty.Partial as PARTIAL
import qualified RIO.Set as Set
import qualified RIO.Text as T
import qualified RIO.Text.Partial as PARTIAL
import Text.Casing

--- Populations ::= Population+

-- | Parses a list of populations
pPopulations ::
  -- | The population list parser
  AmpParser [P_Population]
pPopulations = many1 pPopulation

--- Context ::= 'CONTEXT' ConceptName LanguageRef? TextMarkup? ContextElement* 'ENDCONTEXT'

-- | Parses a context
pContext ::
  -- | The result is the parsed context and a list of include filenames
  AmpParser (P_Context, [Include])
pContext =
  rebuild
    <$> (posOf . pKey . toText1Unsafe $ "CONTEXT")
    <*> pNameWithOptionalLabel ContextName
    <*> pMaybe pLanguageRef
    <*> pMaybe pTextMarkup
    <*> many pContextElement
    <* (pKey . toText1Unsafe $ "ENDCONTEXT")
  where
    rebuild :: Origin -> (Name, Maybe Label) -> Maybe Lang -> Maybe PandocFormat -> [ContextElement] -> (P_Context, [Include])
    rebuild pos' (nm, lbl) lang fmt ces =
      ( PCtx
          { ctx_nm = nm,
            ctx_lbl = lbl,
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
      CMeta
        <$> pMeta
        <|> CPat
        <$> pPatternDef
        <|> CRul
        <$> pRuleDef
        <|> CCfy
        <$> pClassify
        <|> CRel
        <$> pRelationDef
        <|> CCon
        <$> pConceptDef
        <|> CRep
        <$> pRepresentation
        <|> Cm
        <$> pRoleRule
        <|> Cm
        <$> pServiceRule
        <|> CIndx
        <$> pIdentDef
        <|> CView
        <$> pViewDef
        <|> Cifc
        <$> pInterface
        <|> CPrp
        <$> pPurpose
        <|> CPop
        <$> pPopulation
        <|> CIncl
        <$> pIncludeStatement
        <|> CEnf
        <$> pEnforce

pNameWithoutLabel :: NameType -> AmpParser Name
pNameWithoutLabel typ
  | typ == RelationName = properParser
  | otherwise = properParser <|> depricatedParser
  where
    properParser :: AmpParser Name
    properParser = pName typ
    depricatedParser :: AmpParser Name
    depricatedParser = do
      orig <- currPos
      txt <- pDoubleQuotedString1
      let suggestedNamePart = suggestNamePart typ txt
          warn = mkWarnText typ txt
      addParserWarning orig warn
      return
        ( mkName
            typ
            ( case toNamePart1 suggestedNamePart of
                Nothing -> fatal $ "Not a valid NamePart: " <> tshow suggestedNamePart
                Just np -> np NE.:| []
            )
        )

pNameWithOptionalLabel :: NameType -> AmpParser (Name, Maybe Label)
pNameWithOptionalLabel typ = properParser <|> depricatedParser
  where
    properParser :: AmpParser (Name, Maybe Label)
    properParser = do
      nm <- pName typ
      mLab <- pMaybe pLabel
      return (nm, mLab)
    depricatedParser :: AmpParser (Name, Maybe Label)
    depricatedParser =
      do
        orig <- currPos
        txt <- pDoubleQuotedString1
        let suggestedNamePart = suggestNamePart typ txt
            mLab =
              if suggestedNamePart == txt
                then Nothing
                else Just . Label . text1ToText $ txt
            warn = mkWarnText typ txt
        addParserWarning orig warn
        return
          ( mkName
              typ
              ( case toNamePart1 suggestedNamePart of
                  Nothing -> fatal $ "Not a valid NamePart: " <> tshow suggestedNamePart
                  Just np -> np NE.:| []
              ),
            mLab
          )

mkWarnText :: NameType -> Text1 -> Text
mkWarnText typ txt =
  T.intercalate
    "\n  "
    [ "The doublequoted string is deprecated as " <> T.toLower (tshow typ) <> ".",
      tshow . text1ToText $ txt,
      "   should be replaced by something like:",
      text1ToText suggestedNamePart
        <> ( case mLab of
               Nothing -> mempty
               Just (Label lbl) -> " LABEL " <> tshow lbl
           )
    ]
  where
    suggestedNamePart = suggestNamePart typ txt
    mLab =
      if txt == suggestedNamePart
        then Nothing
        else Just . Label . text1ToText $ txt

suggestNamePart :: NameType -> Text1 -> Text1
suggestNamePart typ = toText1Unsafe . T.pack . casing . T.unpack . T.map crapToSpace . text1ToText
  where
    crapToSpace :: Char -> Char
    crapToSpace c =
      if isSafeIdChar False c
        then c
        else ' '
    casing :: String -> String
    casing = case typ of
      ConceptName -> upper
      ContextName -> upper
      IdentName -> dontcare
      InterfaceName -> dontcare
      PatternName -> upper
      PropertyName -> upper
      RelationName -> lower
      RoleName -> dontcare
      RuleName -> dontcare
      SqlAttributeName -> dontcare
      SqlTableName -> dontcare
      ViewName -> dontcare
    upper = pascal
    lower = camel
    dontcare = pascal

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
  Include
    <$> currPos
    <* (pKey . toText1Unsafe) "INCLUDE"
    <*> (T.unpack <$> pDoubleQuotedString)
    <*> (pBrackets (pDoubleQuotedString `sepBy` pComma) <|> return [])

--- LanguageRef ::= 'IN' ('DUTCH' | 'ENGLISH')
pLanguageRef :: AmpParser Lang
pLanguageRef =
  (pKey . toText1Unsafe) "IN"
    *> ( Dutch
           <$ (pKey . toText1Unsafe) "DUTCH"
           <|> English
           <$ (pKey . toText1Unsafe) "ENGLISH"
       )

--- TextMarkup ::= 'REST' | 'HTML' | 'LATEX' | 'MARKDOWN'
pTextMarkup :: AmpParser PandocFormat
pTextMarkup =
  ReST
    <$ (pKey . toText1Unsafe) "REST"
    <|> HTML
    <$ (pKey . toText1Unsafe) "HTML"
    <|> LaTeX
    <$ (pKey . toText1Unsafe) "LATEX"
    <|> Markdown
    <$ (pKey . toText1Unsafe) "MARKDOWN"

--- MetaData ::= 'META' Text Text
pMeta :: AmpParser MetaData
pMeta = MetaData <$> currPos <* (pKey . toText1Unsafe) "META" <*> pDoubleQuotedString1 <*> pDoubleQuotedString

--- PatternDef ::= 'PATTERN' ConceptName PatElem* 'ENDPATTERN'
pPatternDef :: AmpParser P_Pattern
pPatternDef =
  rebuild
    <$> currPos
    <* (pKey . toText1Unsafe) "PATTERN"
    <*> pNameWithOptionalLabel PatternName -- The name spaces of patterns and concepts are shared.
    <*> many pPatElem
    <*> currPos
    <* (pKey . toText1Unsafe) "ENDPATTERN"
  where
    rebuild :: Origin -> (Name, Maybe Label) -> [PatElem] -> Origin -> P_Pattern
    rebuild pos' (nm, lbl) pes end =
      P_Pat
        { pos = pos',
          pt_nm = nm,
          pt_lbl = lbl,
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
pPatElem :: AmpParser PatElem
pPatElem =
  Pr
    <$> pRuleDef
    <|> Py
    <$> pClassify
    <|> Pd
    <$> pRelationDef
    <|> Pm
    <$> pRoleRule
    <|> Pm
    <$> pServiceRule
    <|> Pc
    <$> pConceptDef
    <|> Prep
    <$> pRepresentation
    <|> Pk
    <$> pIdentDef
    <|> Pv
    <$> pViewDef
    <|> Pe
    <$> pPurpose
    <|> Pp
    <$> pPopulation
    <|> Penf
    <$> pEnforce

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
pEnforce :: AmpParser (P_Enforce TermPrim)
pEnforce =
  P_Enforce
    <$> currPos
    <* (pKey . toText1Unsafe) "ENFORCE"
    <*> (PNamedR <$> pNamedRel)
    <*> pEnforceOperator
    <*> pTerm
  where
    pEnforceOperator :: AmpParser EnforceOperator
    pEnforceOperator =
      fun
        <$> currPos
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
pClassify :: AmpParser [PClassify] -- Example: CLASSIFY A IS B /\ C /\ D
pClassify =
  fun
    <$> currPos
    <* (pKey . toText1Unsafe) "CLASSIFY"
    <*> pConceptRef
    `sepBy1` pComma
    <*> ( is
            <$ (pKey . toText1Unsafe) "IS"
            <*> pCterm
            <|> isa
            <$ (pKey . toText1Unsafe) "ISA"
            <*> pConceptRef
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
      pure
        <$> pConceptRef
        <|> pParens pCterm -- brackets are allowed for educational reasons.
    is :: [P_Concept] -> (Bool, [P_Concept])
    is gens = (False, gens)
    isa :: P_Concept -> (Bool, [P_Concept])
    isa gen = (True, [gen])

--- RuleDef ::= 'RULE' Label? Rule Meaning* Message* Violation?
pRuleDef :: AmpParser (P_Rule TermPrim)
pRuleDef =
  build
    <$> currPos
    <* (pKey . toText1Unsafe) "RULE"
    <*> pNameLabelTerm
    <*> many pMeaning
    <*> many pMessage
    <*> pMaybe pViolation
  where
    pNameLabelTerm :: AmpParser (Maybe (Name, Maybe Label), Term TermPrim)
    pNameLabelTerm = try pNameAndLabel <|> pNoName
      where
        pNoName :: AmpParser (Maybe (Name, Maybe Label), Term TermPrim)
        pNoName = buildWithoutName <$> pRule
          where
            buildWithoutName :: Term TermPrim -> (Maybe (Name, Maybe Label), Term TermPrim)
            buildWithoutName term = (Nothing, term)
        pNameAndLabel :: AmpParser (Maybe (Name, Maybe Label), Term TermPrim)
        pNameAndLabel = buildWithName <$> pNameWithOptionalLabelAndColon RuleName <*> pRule
          where
            buildWithName :: (Name, Maybe Label) -> Term TermPrim -> (Maybe (Name, Maybe Label), Term TermPrim)
            buildWithName (nm, lbl) term = (Just (nm, lbl), term)
    build ::
      Origin ->
      (Maybe (Name, Maybe Label), Term a) ->
      [PMeaning] ->
      [PMessage] ->
      Maybe (PairView (Term a)) ->
      P_Rule a
    build orig (maybeNameDefLbl, term) meanings messages mViolation =
      P_Rule
        { pos = orig,
          rr_nm = nameDef,
          rr_lbl = lbl,
          rr_exp = term,
          rr_mean = meanings,
          rr_msg = messages,
          rr_viol = mViolation
        }
      where
        (nameDef, lbl) =
          fromMaybe
            (origToName, Just . Label $ "The rule defined at " <> tshow orig)
            maybeNameDefLbl
        localNm = "Rule_" <> (tshow . abs . hash . tshow) orig
        origToName =
          mkName
            RuleName
            $ ( case toNamePart localNm of
                  Nothing -> fatal $ "Not a valid NamePart: " <> localNm
                  Just np -> np
              )
            NE.:| []
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
      PairViewExp
        <$> posOf ((pKey . toText1Unsafe) "SRC")
        <*> return Src
        <*> pTerm
        <|> PairViewExp
        <$> posOf ((pKey . toText1Unsafe) "TGT")
        <*> return Tgt
        <*> pTerm
        <|> PairViewText
        <$> posOf ((pKey . toText1Unsafe) "TXT")
        <*> pDoubleQuotedString

--- RelationDef ::= (RelationNew | RelationOld) Props? RelDefaults? ('PRAGMA' Text+)? Meaning* ('=' Content)? '.'?
pRelationDef :: AmpParser (P_Relation, [P_Population])
pRelationDef =
  reorder
    <$> currPos
    <*> (pRelationNew <|> pRelationOld)
    <*> optSet pProps
    <*> optList pRelDefaults
    <*> pMaybe pPragma
    <*> many pMeaning
    <*> optList ((pOperator . toText1Unsafe) "=" *> pContent)
    <* optional ((pOperator . toText1Unsafe) ".")
  where
    reorder pos' (nm, sign, lbl, fun) prop dflts pragma meanings prs =
      (P_Relation nm sign lbl props dflts pragma meanings pos', map pair2pop prs)
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
    <$> currPos
    <* (pKey . toText1Unsafe) "PRAGMA"
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
  build
    <$> pSrcOrTgt
    <*> pDef
  where
    build :: SrcOrTgt -> Either (NE.NonEmpty PAtomValue) Text -> PRelationDefault
    build st (Left vals) = PDefAtom st vals
    build st (Right txt) = PDefEvalPHP st txt
    pDef :: AmpParser (Either (NE.NonEmpty PAtomValue) Text)
    pDef = pAtom <|> pPHP
    pAtom =
      Left
        <$ (pKey . toText1Unsafe) "VALUE"
        <*> sepBy1 pAtomValue pComma
    pPHP =
      Right
        <$ (pKey . toText1Unsafe) "EVALPHP"
        <*> pDoubleQuotedString
    pSrcOrTgt =
      Src
        <$ (pKey . toText1Unsafe) "SRC"
        <|> Tgt
        <$ (pKey . toText1Unsafe) "TGT"

--- RelationNew ::= 'RELATION' Varid Signature
pRelationNew :: AmpParser (Name, P_Sign, Maybe Label, PProps)
pRelationNew =
  (,,,)
    <$ (pKey . toText1Unsafe) "RELATION"
    <*> pNameWithoutLabel RelationName
    <*> pSign
    <*> optional pLabel
    <*> return Set.empty

--- RelationOld ::= Varid '::' ConceptRef Fun ConceptRef
pRelationOld :: AmpParser (Name, P_Sign, Maybe Label, PProps)
pRelationOld =
  relOld
    <$> pNameWithoutLabel RelationName
    <* (pOperator . toText1Unsafe) "::"
    <*> pConceptRef
    <*> pFun
    <*> pConceptRef
    <*> optional pLabel
  where
    relOld nm src fun tgt lbl = (nm, P_Sign src tgt, lbl, fun)

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
  Set.empty
    <$ (pOperator . toText1Unsafe) "*"
    <|> Set.fromList [P_Uni, P_Tot]
    <$ (pOperator . toText1Unsafe) "->"
    <|> Set.fromList [P_Sur, P_Inj]
    <$ (pOperator . toText1Unsafe) "<-"
    <|> pBrackets pMults
  where
    --- Mults ::= Mult '-' Mult
    pMults :: AmpParser PProps
    pMults =
      Set.union
        <$> optSet (pMult (P_Sur, P_Inj))
        <* pDash
        <*> optSet (pMult (P_Tot, P_Uni))

    --- Mult ::= ('0' | '1') '..' ('1' | '*') | '*' | '1'
    -- TODO: refactor to Mult ::= '0' '..' ('1' | '*') | '1'('..' ('1' | '*'))? | '*'
    pMult :: (PProp, PProp) -> AmpParser PProps
    pMult (ts, ui) =
      Set.union
        <$> (Set.empty <$ pZero <|> Set.singleton ts <$ try pOne)
        <* (pOperator . toText1Unsafe) ".."
        <*> (Set.singleton ui <$ try pOne <|> Set.empty <$ (pOperator . toText1Unsafe) "*")
        <|> Set.empty
        <$ (pOperator . toText1Unsafe) "*"
        <|> Set.fromList [ts, ui]
        <$ try pOne

--- ConceptDef ::= 'CONCEPT' ConceptName Text ('TYPE' Text)? Text?
pConceptDef :: AmpParser (DefinitionContainer -> PConceptDef)
pConceptDef =
  build
    <$> currPos
    <* (pKey . toText1Unsafe) "CONCEPT"
    <*> pNameWithOptionalLabel ConceptName
    <*> pPCDDef2
    <*> many pMeaning
  where
    build :: Origin -> (Name, Maybe Label) -> PCDDef -> [PMeaning] -> (DefinitionContainer -> PConceptDef)
    build orig (nm, mLab) = PConceptDef orig nm mLab
    pPCDDef2 :: AmpParser PCDDef
    pPCDDef2 =
      PCDDefLegacy
        <$> (pDoubleQuotedString <?> "concept definition (string)")
        <*> (pDoubleQuotedString `opt` "")
        <|> PCDDefNew
        <$> pMeaning

--- Representation ::= 'REPRESENT' ConceptNameList 'TYPE' AdlTType
pRepresentation :: AmpParser Representation
pRepresentation =
  Repr
    <$> currPos
    <* (pKey . toText1Unsafe) "REPRESENT"
    <*> pConceptRef
    `sepBy1` pComma
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
-- You may also use a term on each attribute place, for example: IDENT onpassport: Person(nationality, passport;documentnr),
-- which means that nationality<>nationality~ /\ passport;documentnr<>(passport;documentnr)~ |- I[Person].

--- IdentDef ::= 'IDENT' Label ConceptRef '(' IndSegmentList ')'
pIdentDef :: AmpParser P_IdentDef
pIdentDef =
  build
    <$> currPos
    <* (pKey . toText1Unsafe) "IDENT"
    <*> pNameWithOptionalLabelAndColon IdentName
    <*> pConceptRef
    <*> pParens (pIdentSegment `sepBy1` pComma)
  where
    build orig (nm, lbl) cpt lst =
      P_Id
        { ix_label = lbl,
          ix_name = nm,
          ix_cpt = cpt,
          ix_ats = lst,
          pos = orig
        }
    --- IndSegmentList ::= Attr (',' Attr)*
    pIdentSegment :: AmpParser P_IdentSegment
    pIdentSegment = P_IdentExp <$> pAtt
    --- Attr ::= Term
    pAtt :: AmpParser P_BoxBodyElement
    pAtt = rebuild <$> currPos <*> pTerm
      where
        rebuild pos' ctx =
          P_BoxItemTerm
            { pos = pos',
              obj_PlainName = Nothing,
              obj_lbl = Nothing,
              obj_ctx = ctx,
              obj_crud = Nothing,
              obj_mView = Nothing,
              obj_msub = Nothing
            }

--- ViewDef ::= FancyViewDef | ViewDefLegacy
pViewDef :: AmpParser P_ViewDef
pViewDef = try pViewDefImproved <|> pViewDefLegacy -- introduces backtracking, but is more elegant than rewriting pViewDefLegacy to disallow "KEY ... ENDVIEW".

--- FancyViewDef ::= 'VIEW' Name Label? ConceptOneRef 'DEFAULT'? ('{' ViewObjList '}')?  HtmlView? 'ENDVIEW'
pViewDefImproved :: AmpParser P_ViewDef
pViewDefImproved =
  mkViewDef
    <$> currPos
    <* (pKey . toText1Unsafe) "VIEW"
    <*> pNameWithOptionalLabelAndColon ViewName
    <*> pConceptOneRef
    <*> pIsThere ((pKey . toText1Unsafe) "DEFAULT")
    <*> pBraces (pViewSegment Improved `sepBy` pComma)
    `opt` []
    <*> pMaybe pHtmlView
    <* (pKey . toText1Unsafe) "ENDVIEW"
  where
    mkViewDef pos' (nm, lbl) cpt isDef ats html =
      P_Vd
        { pos = pos',
          vd_nm = nm,
          vd_label = lbl,
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
pViewSegmentLoad :: AmpParser (P_ViewSegmtPayLoad TermPrim)
pViewSegmentLoad =
  P_ViewExp
    <$> pTerm
    <|> P_ViewText
    <$ (pKey . toText1Unsafe) "TXT"
    <*> pDoubleQuotedString

data ViewKind = Legacy | Improved

--- ViewSegment ::= Label ViewSegmentLoad
pViewSegment :: ViewKind -> AmpParser (P_ViewSegment TermPrim)
pViewSegment viewKind =
  build
    <$> currPos
    <*> case viewKind of
      Legacy -> pure Nothing
      Improved -> pMaybe pTex1AndColon
    <*> pViewSegmentLoad
  where
    build :: Origin -> Maybe Text1 -> P_ViewSegmtPayLoad TermPrim -> P_ViewSegment TermPrim
    build orig lbl load =
      P_ViewSegment
        { vsm_labl = lbl,
          pos = orig,
          vsm_load = load
        }

--- ViewDefLegacy ::= 'VIEW' Label ConceptOneRef '(' ViewSegmentList ')'
pViewDefLegacy :: AmpParser P_ViewDef
pViewDefLegacy =
  build
    <$> currPos
    <* (pKey . toText1Unsafe) "VIEW"
    <*> pNameWithOptionalLabelAndColon ViewName
    <*> pConceptOneRef
    <*> pParens (pViewSegment Legacy `sepBy` pComma)
  where
    build ::
      Origin ->
      (Name, Maybe Label) ->
      P_Concept ->
      [P_ViewSegment a] ->
      P_ViewD a
    build orig (nm, lbl) cpt segments =
      P_Vd
        { vd_label = lbl,
          vd_nm = nm,
          vd_isDefault = True,
          vd_html = Nothing,
          vd_cpt = cpt,
          vd_ats = segments,
          pos = orig
        }

--- Interface ::= 'INTERFACE' ADLid Params? Roles? ':' Term (ADLid | Conid)? SubInterface?
pInterface :: AmpParser P_Interface
pInterface =
  build
    <$> currPos
    <*> pInterfaceIsAPI
    <*> pNameWithOptionalLabel InterfaceName
    <*> pMaybe pRoles
    <*> (pColon *> pTerm) -- the term of the interface object
    <*> pMaybe pCruds -- The Crud-string (will later be tested, that it can contain only characters crud (upper/lower case)
    <*> pMaybe (pChevrons (pNameWithoutLabel ViewName)) -- The view that should be used for this object
    <*> pSubInterface
  where
    build ::
      Origin ->
      Bool ->
      (Name, Maybe Label) ->
      Maybe (NE.NonEmpty Role) ->
      Term TermPrim ->
      Maybe P_Cruds ->
      Maybe Name ->
      P_SubInterface ->
      P_Interface
    build p isAPI (nm, lbl) roles ctx mCrud mView sub =
      P_Ifc
        { ifc_IsAPI = isAPI,
          ifc_Name = nm,
          ifc_lbl = lbl,
          ifc_Roles = maybe [] NE.toList roles,
          ifc_Obj =
            P_BoxItemTerm
              { obj_PlainName = Nothing,
                obj_lbl = Nothing,
                pos = p,
                obj_ctx = ctx,
                obj_crud = mCrud,
                obj_mView = mView,
                obj_msub = Just sub
              },
          pos = p,
          ifc_Prp = "" -- TODO: Nothing in syntax defined for the purpose of the interface.
        }
    --- Roles ::= 'FOR' RoleList
    pRoles = (pKey . toText1Unsafe) "FOR" *> pRole False `sepBy1` pComma

--- SubInterface ::= 'BOX' HTMLtemplateCall? Box | 'LINKTO'? 'INTERFACE' ADLid
pSubInterface :: AmpParser P_SubInterface
pSubInterface =
  P_Box
    <$> currPos
    <*> pBoxHeader
    <*> pBoxBody
    <|> P_InterfaceRef
    <$> currPos
    <*> pIsThere ((pKey . toText1Unsafe) "LINKTO")
    <* pInterfaceKey
    <*> pNameWithoutLabel InterfaceName
  where
    pBoxHeader :: AmpParser HTMLtemplateCall
    pBoxHeader =
      build <$> currPos <* (pKey . toText1Unsafe) "BOX" <*> optional pBoxSpecification
      where
        build :: Origin -> Maybe (Text1, [TemplateKeyValue]) -> HTMLtemplateCall
        build o x = HTMLtemplateCall o typ keys
          where
            (typ, keys) = fromMaybe (toText1Unsafe "FORM", []) x
        pBoxSpecification :: AmpParser (Text1, [TemplateKeyValue])
        pBoxSpecification =
          pChevrons
            $ (,)
            <$> (pSingleWord <|> pAnyKeyWord)
            <*> many pTemplateKeyValue
        pTemplateKeyValue :: AmpParser TemplateKeyValue
        pTemplateKeyValue =
          TemplateKeyValue
            <$> currPos
            <*> (pSingleWord <|> pAnyKeyWord)
            <*> optional (id <$ (pOperator . toText1Unsafe) "=" <*> pDoubleQuotedString)
    --- Box ::= '[' ObjDefList ']'
    pBoxBody :: AmpParser [P_BoxBodyElement]
    pBoxBody = pBrackets $ pBoxBodyElement `sepBy` pComma

--- ObjDef ::= Label Term ('<' Conid '>')? SubInterface?
--- ObjDefList ::= ObjDef (',' ObjDef)*
pBoxBodyElement :: AmpParser P_BoxBodyElement
pBoxBodyElement =
  try pBoxItemTerm
    <|> try pBoxItemText -- We need `try` because in the Term, the local name is mandatory, while in Text it is optional.
  where
    pBoxItemTerm :: AmpParser P_BoxBodyElement
    pBoxItemTerm =
      build
        <$> currPos
        <*> pUnrestrictedText1
        <*> pMaybe pLabel
        <* pColon
        <*> pTerm -- the context term (for example: I[c])
        <*> pMaybe pCruds
        <*> pMaybe (pChevrons (pNameWithoutLabel ViewName)) -- for the view
        <*> pMaybe pSubInterface -- the optional subinterface
      where
        build orig localNm lbl term mCrud mView msub =
          P_BoxItemTerm
            { obj_PlainName = Just localNm,
              obj_lbl = lbl,
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
        <*> pMaybe pTex1AndColon
        <* (pKey . toText1Unsafe) "TXT"
        <*> pDoubleQuotedString
      where
        build orig lab txt =
          P_BxTxt
            { obj_PlainName = lab,
              pos = orig,
              box_txt = txt
            }

--- Cruds ::= crud in upper /lowercase combinations
pCruds :: AmpParser P_Cruds
pCruds = P_Cruds <$> currPos <*> pCrudString

--- Purpose ::= 'PURPOSE' Ref2Obj LanguageRef? TextMarkup? ('REF' StringListSemi)? Expl
pPurpose :: AmpParser PPurpose
pPurpose =
  rebuild
    <$> currPos
    <* (pKey . toText1Unsafe) "PURPOSE"
    <*> pRef2Obj
    <*> pMaybe pLanguageRef
    <*> pMaybe pTextMarkup
    <*> pMaybe ((pKey . toText1Unsafe) "REF" *> pDoubleQuotedString `sepBy1` pSemi)
    <*> pAmpersandMarkup
  where
    rebuild :: Origin -> PRef2Obj -> Maybe Lang -> Maybe PandocFormat -> Maybe (NE.NonEmpty Text) -> Text -> PPurpose
    rebuild orig obj lang fmt refs str =
      PPurpose orig obj (P_Markup lang fmt str) (concatMap splitOnSemicolon (maybe [] NE.toList refs))
      where
        -- TODO: This separation should not happen in the parser
        splitOnSemicolon :: Text -> [Text]
        splitOnSemicolon = PARTIAL.splitOn ";" -- This is safe: The first argument of splitOn must not be empty.
        --- Ref2Obj ::= 'CONCEPT' ConceptName | 'RELATION' NamedRel | 'RULE' ADLid | 'IDENT' ADLid | 'VIEW' ADLid | 'PATTERN' ADLid | 'INTERFACE' ADLid | 'CONTEXT' ADLid
    pRef2Obj :: AmpParser PRef2Obj
    pRef2Obj =
      PRef2ConceptDef
        <$ (pKey . toText1Unsafe) "CONCEPT"
        <*> pNameWithoutLabel ConceptName
        <|> PRef2Relation
        <$ (pKey . toText1Unsafe) "RELATION"
        <*> pNamedRel
        <|> PRef2Rule
        <$ (pKey . toText1Unsafe) "RULE"
        <*> pNameWithoutLabel RuleName
        <|> PRef2IdentityDef
        <$ (pKey . toText1Unsafe) "IDENT"
        <*> pNameWithoutLabel IdentName
        <|> PRef2ViewDef
        <$ (pKey . toText1Unsafe) "VIEW"
        <*> pNameWithoutLabel ViewName
        <|> PRef2Pattern
        <$ (pKey . toText1Unsafe) "PATTERN"
        <*> pNameWithoutLabel PatternName
        <|> PRef2Interface
        <$ pInterfaceKey
        <*> pNameWithoutLabel InterfaceName
        <|> PRef2Context
        <$ (pKey . toText1Unsafe) "CONTEXT"
        <*> pNameWithoutLabel ContextName

pInterfaceKey :: AmpParser Text1
pInterfaceKey = pKey (toText1Unsafe "INTERFACE") <|> pKey (toText1Unsafe "API") -- On special request of Rieks, the keyword "API" is allowed everywhere where the keyword "INTERFACE" is used. https://github.com/AmpersandTarski/Ampersand/issues/789

pInterfaceIsAPI :: AmpParser Bool
pInterfaceIsAPI = (toText1Unsafe "API" ==) <$> pInterfaceKey

--- Population ::= 'POPULATION' (NamedRel 'CONTAINS' Content | ConceptName 'CONTAINS' '[' ValueList ']')

-- | Parses a population
pPopulation ::
  -- | The population parser
  AmpParser P_Population
pPopulation =
  (pKey . toText1Unsafe) "POPULATION"
    *> (try pPopulationCpt <|> pPopulationRel) -- FIXME: Adding try solved the problem of parsing POPULATION statements. However, it significantly slowed down the quickCheck tests.
  where
    pPopulationRel =
      P_RelPopu Nothing Nothing
        <$> currPos
        <*> pNamedRel
        <* (pKey . toText1Unsafe) "CONTAINS"
        <*> pContent
    pPopulationCpt =
      P_CptPopu
        <$> currPos
        <*> pConceptRef
        <* (pKey . toText1Unsafe) "CONTAINS"
        <*> pBrackets (pAtomValue `sepBy` pComma)

--- RoleRule ::= 'ROLE' RoleList 'MAINTAINS' ADLidList
-- TODO: Rename the RoleRule to RoleMantains.
pRoleRule :: AmpParser P_RoleRule
pRoleRule =
  try
    ( Maintain
        <$> currPos
        <* (pKey . toText1Unsafe) "ROLE"
        <*> pRole False
        `sepBy1` pComma
        <* (pKey . toText1Unsafe) "MAINTAINS"
    )
    <*> pNameWithoutLabel RuleName
    `sepBy1` pComma

--- ServiceRule ::= 'SERVICE' RoleList 'MAINTAINS' ADLidList
-- TODO: Rename the RoleRule to RoleMantains.
pServiceRule :: AmpParser P_RoleRule
pServiceRule =
  try
    ( Maintain
        <$> currPos
        <* (pKey . toText1Unsafe) "SERVICE"
        <*> pRole True
        `sepBy1` pComma
        <* (pKey . toText1Unsafe) "MAINTAINS"
    )
    <*> pNameWithoutLabel RuleName
    `sepBy1` pComma

--- Role ::= ADLid (LABEL doublequotedstring)?
--- RoleList ::= Role (',' Role)*
pRole :: Bool -> AmpParser Role
pRole isService =
  build <$> currPos <*> pNameWithOptionalLabel RoleName
  where
    build :: Origin -> (Name, Maybe Label) -> Role
    build orig (nm, lbl) =
      Role
        { pos = orig,
          rlName = nm,
          rlLbl = lbl,
          rlIsService = isService
        }

-- pNameWithoutLabel RoleName

--- Meaning ::= 'MEANING' Markup
pMeaning :: AmpParser PMeaning
pMeaning =
  PMeaning
    <$ (pKey . toText1Unsafe) "MEANING"
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
  -- | The rule parser
  AmpParser (Term TermPrim)
pRule =
  pTerm
    <??> ( invert PEqu
             <$> currPos
             <* (pOperator . toText1Unsafe) "="
             <*> pTerm
             <|> invert PInc
             <$> currPos
             <* (pOperator . toText1Unsafe) "|-"
             <*> pTerm
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
  -- | The term parser
  AmpParser (Term TermPrim)
pTerm =
  pTrm2
    <??> ( invertT PIsc
             <$> rightAssociate PIsc (toText1Unsafe "/\\") pTrm2
             <|> invertT PUni
             <$> rightAssociate PUni (toText1Unsafe "\\/") pTrm2
         )

-- The left factored version of difference: (Actually, there is no need for left-factoring here, but no harm either)
--- Trm2 ::= Trm3 ('-' Trm3)?
pTrm2 :: AmpParser (Term TermPrim)
pTrm2 = pTrm3 <??> (invert PDif <$> posOf pDash <*> pTrm3)

-- The left factored version of right- and left residuals:
--- Trm3 ::= Trm4 ('/' Trm4 | '\' Trm4 | '<>' Trm4)?
pTrm3 :: AmpParser (Term TermPrim)
pTrm3 =
  pTrm4
    <??> ( invert PLrs
             <$> currPos
             <* (pOperator . toText1Unsafe) "/"
             <*> pTrm4
             <|> invert PRrs
             <$> currPos
             <* (pOperator . toText1Unsafe) "\\"
             <*> pTrm4
             <|> invert PDia
             <$> currPos
             <* (pOperator . toText1Unsafe) "<>"
             <*> pTrm4
         )

-- composition and relational addition are associative, and parsed similar to union and intersect...
--- Trm4 ::= Trm5 ((';' Trm5)+ | ('!' Trm5)+ | ('#' Trm5)+)?
pTrm4 :: AmpParser (Term TermPrim)
pTrm4 =
  pTrm5
    <??> ( invertT PCps
             <$> rightAssociate PCps (toText1Unsafe ";") pTrm5
             <|> invertT PRad
             <$> rightAssociate PRad (toText1Unsafe "!") pTrm5
             <|> invertT PPrd
             <$> rightAssociate PPrd (toText1Unsafe "#") pTrm5
         )

--- Trm5 ::= '-'* Trm6 ('~' | '*' | '+')*
pTrm5 :: AmpParser (Term TermPrim)
-- TODO: Separate into prefix and postfix top-level functions
pTrm5 =
  f
    <$> many (valPosOf pDash)
    <*> pTrm6
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
pTrm6 :: AmpParser (Term TermPrim)
pTrm6 =
  Prim
    <$> pRelationRef
    <|> PBrk
    <$> currPos
    <*> pParens pTerm

-- Help function for several terms. The type 't' is each of the terms.
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
pRelationRef :: AmpParser TermPrim
pRelationRef =
  PNamedR
    <$> pNamedRel
    <|> pid
    <$> currPos
    <* (pKey . toText1Unsafe) "I"
    <*> (pMaybe . pBrackets $ pConceptOneRef)
    <|> pfull
    <$> currPos
    <* (pKey . toText1Unsafe) "V"
    <*> pMaybe pSign
    <|> Patm
    <$> currPos
    <*> pSingleton
    <*> (pMaybe . pBrackets $ pConceptOneRef)
  where
    pid orig Nothing = PI orig
    pid orig (Just c) = Pid orig c
    pfull orig Nothing = PVee orig
    pfull orig (Just (P_Sign src trg)) = Pfull orig src trg

pSingleton :: AmpParser PAtomValue
pSingleton =
  value2PAtomValue
    <$> currPos
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
pNamedRel :: AmpParser P_NamedRel
pNamedRel = PNamedRel <$> currPos <*> pNameWithoutLabel RelationName <*> pMaybe pSign

--- Signature ::= '[' ConceptOneRef ('*' ConceptOneRef)? ']'
pSign :: AmpParser P_Sign
pSign = pBrackets sign
  where
    sign = mkSign <$> pConceptOneRef <*> pMaybe ((pOperator . toText1Unsafe) "*" *> pConceptOneRef)
    mkSign src mTgt = P_Sign src (fromMaybe src mTgt)

--- ConceptRef ::= ConceptName
pConceptRef :: AmpParser P_Concept
pConceptRef = PCpt <$> pNameWithoutLabel ConceptName

--- ConceptOneRef ::= 'ONE' | ConceptRef
pConceptOneRef :: AmpParser P_Concept
pConceptOneRef = P_ONE <$ (pKey . toText1Unsafe) "ONE" <|> pConceptRef

pTex1AndColon :: AmpParser Text1
pTex1AndColon = pUnrestrictedText1 <* pColon

pUnrestrictedText1 :: AmpParser Text1
pUnrestrictedText1 = (pSingleWord <|> pAnyKeyWord <|> pDoubleQuotedString1) <?> "identifier"

pNameWithOptionalLabelAndColon ::
  NameType ->
  AmpParser (Name, Maybe Label)
pNameWithOptionalLabelAndColon typ = pNameWithOptionalLabel typ <* pColon

--- Content ::= '[' RecordList? ']'
pContent :: AmpParser [PAtomPair]
pContent = pBrackets (pRecord `sepBy` (pComma <|> pSemi))
  where
    --- RecordList ::= Record ((','|';') Record)*
    --- Record ::= Text ',' Text
    pRecord :: AmpParser PAtomPair
    pRecord =
      pParens
        ( PPair
            <$> currPos
            <*> pAtomValue
            <* pComma
            <*> pAtomValue
        )

pLabel :: AmpParser Label
pLabel =
  Label
    <$ (pKey . toText1Unsafe $ "LABEL")
    <*> pDoubleQuotedString

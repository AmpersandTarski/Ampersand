{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Ampersand.Input.IFC.IFCAnalyze
-- Description : Binds STEP/Part-21 instances to an EXPRESS schema and emits a sliced P_Context.
-- Maintainer  : stef.joosten@ou.nl
--
-- This is the IFC binder (handoff WP3). It is the only module of the three IFC
-- reader layers that is /not/ schema-agnostic: it knows how to combine
--
--   * @[StepInstance]@ from the STEP reader ('Ampersand.Input.Step.Parser'), and
--   * an @ExpressSchema@ from the EXPRESS reader ('Ampersand.Input.Express.Parser')
--
-- into a 'P_Context', exactly as @archi2PContext@ does for ArchiMate
-- (@Ampersand.Input.Archi.ArchiAnalyze@). The result feeds the rest of the
-- compiler (type-checker, FSpec generation) unchanged.
--
-- It works in four steps:
--
--   1. /Bind/: give the positional 'siArgs' of every instance the names from the
--      schema's inherited attribute list ('fullAttrs'). An arity mismatch produces
--      a 'Guarded' warning, never a fatal error.
--   2. /Slice/ (handoff vraag 2): collect the used entity types and close the set
--      upward over supertype chains, attribute target types and the SELECTs that
--      occur as targets — mirroring @gen_subset.py::build_subset@. Only this slice
--      of the (huge) schema reaches Ampersand.
--   3. /Emit relations and inheritance/: one 'P_Relation' per used attribute and one
--      'PClassify' per supertype edge.
--   4. /Emit population/: 'P_CptPopu' for type membership and 'P_RelPopu' for the
--      attribute pairs.
--
-- == Design decisions (deliberate, see handoff §4-WP3) ==
--
--   * __SELECT becomes a relation, not ISA.__ We keep single inheritance so the
--     typologies stay small and clean (one root per tree, no universal synthetic
--     root). A SELECT-typed reference is the /same atom/, typed through a relation
--     to the SELECT concept. This is what avoids the typology blow-up (and the
--     out-of-memory) seen in the Python prototype's @selects='isa'@ mode.
--   * __Everything is OBJECT__ for now (no @REPRESENT@): every concept is an object
--     concept. Value types (measures, enums, strings) will later get their own
--     typology plus @ctx_reprs@; until then their atoms are plain object atoms.
--     This is a deliberate, temporary simplification.
--   * __Known limitation — LIST\/ARRAY order is lost.__ The STEP reader preserves
--     order in 'SVList', but a relation in Ampersand is a /set/ of pairs, so the
--     positional order of e.g. polyline points or geometry vertices does not
--     survive into 'ctx_pops'. For geometry this matters; it is flagged as future
--     work (an index relation or a position concept).
module Ampersand.Input.IFC.IFCAnalyze
  ( ifc2PContext,
    ifc2PContextWithSchema,
    defaultSchemaPath,
  )
where

import Ampersand.Basics
import Ampersand.Core.ParseTree
import Ampersand.Input.ADL1.CtxError
import Ampersand.Input.Express.Parser (parseExpressFile)
import Ampersand.Input.Express.Schema
import Ampersand.Input.Step.Parser (parseStepFile)
import Ampersand.Input.Step.Types
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T

-- | Default EXPRESS schema used when no schema is supplied. Proper static
-- bundling plus @FILE_SCHEMA@-header selection is WP4; here we fall back to the
-- IFC4.3 schema on the author's machine (handoff §6), skipped if absent (see the
-- test). WP4 replaces this with a bundled resource.
defaultSchemaPath :: FilePath
defaultSchemaPath =
  "/Users/stef/Library/CloudStorage/GoogleDrive-stefjoosten1@gmail.com/"
    <> "Mijn Drive/cloudDrive/Rijksvastgoedbedrijf/BIM/bronnen/ifc-schemas/IFC4.3/IFC4X3_ADD2.exp"

-- | Read an @.ifc@ file and produce a 'P_Context', using 'defaultSchemaPath' for
-- the EXPRESS schema. Analogous to @archi2PContext@.
ifc2PContext :: FilePath -> RIO env (Guarded P_Context)
ifc2PContext = ifc2PContextWithSchema defaultSchemaPath

-- | Like 'ifc2PContext', but with an explicit EXPRESS schema path. WP4 chooses the
-- path from the @FILE_SCHEMA@ header and a bundled resource; WP3 keeps it explicit.
ifc2PContextWithSchema :: FilePath -> FilePath -> RIO env (Guarded P_Context)
ifc2PContextWithSchema schemaPath ifcPath = do
  eInsts <- parseStepFile ifcPath
  eSchema <- parseExpressFile schemaPath
  pure $ case (eInsts, eSchema) of
    (Left errs, _) -> mkErr ifcPath (T.unlines errs)
    (_, Left err) -> mkErr schemaPath (T.pack err)
    (Right insts, Right schema) -> mkIfcContext schema insts
  where
    mkErr fp msg =
      Errors . pure $
        CTXE (Origin ("While reading " <> T.pack fp)) msg

--------------------------------------------------------------------------------
-- Binding: positional args -> named attribute pairs
--------------------------------------------------------------------------------

-- | A bound attribute occurrence: the owning entity that /declares/ the attribute,
-- the attribute itself, and the value found at its position in the instance.
data Bound = Bound
  { bOwner :: !Text, -- entity declaring the attribute (the relation source concept)
    bAttr :: !Attr, -- the schema attribute
    bId :: !Text, -- "#id" of the instance
    bVal :: !StepValue -- the positional value
  }

-- | One ordered attribute together with the entity that declares it. This is the
-- piece of information that 'fullAttrs' alone does not carry, but @json2pop.py@
-- needs as @owner@; we recompute it by walking the supertype chain.
fullAttrsWithOwner :: ExpressSchema -> Text -> [(Text, Attr)]
fullAttrsWithOwner schema = go []
  where
    go seen nm
      | nm `elem` seen = [] -- guard against cyclic supertypes
      | otherwise =
          case Map.lookup nm (esEntities schema) of
            Nothing -> []
            Just ent ->
              maybe [] (go (nm : seen)) (enSupertype ent)
                <> [(nm, a) | a <- enAttrs ent]

-- | Bind one instance, collecting its bound attribute occurrences plus any arity
-- warning. Unknown types yield no bindings (and a warning).
bindInstance :: ExpressSchema -> StepInstance -> ([Bound], [Warning])
bindInstance schema inst =
  case Map.lookup pascalType (esEntities schema) of
    Nothing -> ([], [warnUnknownType])
    Just _ ->
      let attrs = fullAttrsWithOwner schema pascalType
          nArgs = length (siArgs inst)
          nAttrs = length attrs
          arityWarn
            | nArgs == nAttrs = []
            | otherwise = [warnArity nArgs nAttrs]
          bounds =
            [ Bound owner a (siId inst) v
              | ((owner, a), v) <- zip attrs (siArgs inst)
            ]
       in (bounds, arityWarn)
  where
    -- The schema keys are PascalCase; STEP types are UPPERCASE. We match
    -- case-insensitively via the schema's own upper-cased key index.
    pascalType = Map.findWithDefault (siType inst) (T.toUpper (siType inst)) upperIndex
    upperIndex = schemaUpperIndex schema
    warnUnknownType =
      mkParserStateWarning
        (Origin "While binding an IFC instance")
        ("Unknown entity type " <> siType inst <> " (" <> siId inst <> "); instance skipped.")
    warnArity nArgs nAttrs =
      mkParserStateWarning
        (Origin "While binding an IFC instance")
        ( siId inst
            <> " "
            <> siType inst
            <> ": "
            <> tshow nArgs
            <> " args but schema expects "
            <> tshow nAttrs
            <> " attributes."
        )

-- | UPPERCASE entity name -> PascalCase schema key, computed once per schema use.
schemaUpperIndex :: ExpressSchema -> Map Text Text
schemaUpperIndex schema =
  Map.fromList [(T.toUpper k, k) | k <- Map.keys (esEntities schema)]

--------------------------------------------------------------------------------
-- Value flattening: SVList nesting -> flat scalar/ref values
--------------------------------------------------------------------------------

-- | Flatten nested lists to a flat list of scalar/ref values, mirroring
-- @json2pop.py::flatten@. 'SVNull' is dropped; typed values keep their inner
-- payload (a measure wrapper carries its single scalar).
flattenValue :: StepValue -> [StepValue]
flattenValue v = case v of
  SVList xs -> concatMap flattenValue xs
  SVTyped _ xs -> concatMap flattenValue xs
  SVNull -> []
  _ -> [v]

-- | The atom text of a flattened scalar/ref value (the @str(elem)@ of the Python).
atomText :: StepValue -> Maybe Text
atomText v = case v of
  SVRef r -> Just r
  SVStr s -> Just s
  SVEnum e -> Just e
  SVInt i -> Just (tshow i)
  SVReal d -> Just (showReal d)
  SVBin b -> Just b
  SVList _ -> Nothing -- already flattened away
  SVTyped _ _ -> Nothing -- already flattened away
  SVNull -> Nothing

-- | Render a Double the way the value will appear as an atom. We keep it simple
-- and deterministic; exact numeric round-tripping is out of scope for WP3.
showReal :: Double -> Text
showReal = tshow

--------------------------------------------------------------------------------
-- Slice + context assembly
--------------------------------------------------------------------------------

-- | A used relation, keyed by (relation-name, owner concept, target concept).
data RelKey = RelKey
  { rkName :: !Text, -- PascalCase attribute name (relation name made lowercase later)
    rkOwner :: !Text, -- owner entity (source concept)
    rkTarget :: !Text -- target concept (after primConcept / select mapping)
  }
  deriving (Eq, Ord)

-- | Assemble the sliced 'P_Context' from a schema and the bound instances.
mkIfcContext :: ExpressSchema -> [StepInstance] -> Guarded P_Context
mkIfcContext schema insts =
  addWarnings allWarnings (pure context)
  where
    -- 1. Bind every instance.
    (boundLists, warningLists) = L.unzip (map (bindInstance schema) insts)
    bounds = concat boundLists
    allWarnings = concat warningLists

    -- Used entity types (instances whose type is known) -> their atoms.
    knownInsts =
      [ (pascalKey, siId i)
        | i <- insts,
          Just pascalKey <- [Map.lookup (T.toUpper (siType i)) upperIdx]
      ]
    upperIdx = schemaUpperIndex schema

    typeAtoms :: Map Text [Text]
    typeAtoms =
      Map.map (L.nub . reverse) $
        Map.fromListWith (<>) [(t, [a]) | (t, a) <- knownInsts]

    -- 2. Relation pairs, deduplicated (relations are sets).
    relPairs :: Map RelKey [(Text, Text)]
    relPairs =
      Map.map (L.nub . reverse) $
        Map.fromListWith (<>) $
          [ (key, [(bId b, tgtAtom)])
            | b <- bounds,
              let key =
                    RelKey
                      { rkName = atName (bAttr b),
                        rkOwner = bOwner b,
                        rkTarget = targetConcept (atTarget (bAttr b))
                      },
              elemV <- flattenValue (bVal b),
              Just tgtAtom <- [atomText elemV]
          ]

    usedRelKeys = Map.keys relPairs

    -- 3. Slice: close the used-type set upward (handoff vraag 2 / gen_subset.py).
    usedTypes0 = Set.fromList (Map.keys typeAtoms)
    -- add owners of used relations
    withOwners = Set.union usedTypes0 (Set.fromList (map rkOwner usedRelKeys))
    -- close over supertype chains
    sliceTypes = closeSupertypes withOwners

    -- SELECTs that occur as a relation target (kept as concepts, NOT as ISA).
    usedSelects =
      Set.fromList
        [ rkTarget k
          | k <- usedRelKeys,
            isSelectType schema (rkTarget k)
        ]

    -- 4. CLASSIFY edges: only genuine single-inheritance supertype edges within
    --    the slice. SELECTs are deliberately NOT turned into ISA (design choice).
    classifyEdges :: [(Text, Text)]
    classifyEdges =
      L.nub
        [ (sub, sup)
          | sub <- Set.toList sliceTypes,
            Just ent <- [Map.lookup sub (esEntities schema)],
            Just sup <- [enSupertype ent],
            sub /= sup
        ]

    -- Concepts that need to exist: slice entity types + used select concepts +
    -- relation targets (so primitive/value concepts are introduced too).
    allTargets = Set.fromList (map rkTarget usedRelKeys)
    sliceConcepts =
      Set.unions [sliceTypes, usedSelects, allTargets]

    closeSupertypes :: Set Text -> Set Text
    closeSupertypes = go Set.empty . Set.toList
      where
        go acc [] = acc
        go acc (x : xs)
          | x `Set.member` acc = go acc xs
          | otherwise =
              let acc' = Set.insert x acc
                  sup = Map.lookup x (esEntities schema) >>= enSupertype
               in go acc' (maybe xs (: xs) sup)

    -- Build the P_Context fragments.
    pClassifies :: [PClassify]
    pClassifies =
      [ PClassify
          { pos = orig,
            specific = mkConcept sub,
            generics = mkConcept sup NE.:| []
          }
        | (sub, sup) <- classifyEdges
      ]

    pRelations :: [P_Relation]
    pRelations =
      [ mkRelation k
        | k <- usedRelKeys
      ]

    pRelPops :: [P_Population]
    pRelPops =
      [ P_RelPopu
          { p_src = Nothing,
            p_tgt = Nothing,
            pos = orig,
            p_nmdr = relNamedRel k,
            p_popps = map mkPair pairs
          }
        | (k, pairs) <- Map.toList relPairs
      ]

    pCptPops :: [P_Population]
    pCptPops =
      [ P_CptPopu
          { pos = orig,
            p_cpt = mkConcept t,
            p_popas = map mkAtomValue atoms
          }
        | (t, atoms) <- Map.toList typeAtoms
      ]

    -- Concept membership for select/target concepts is implied by the relation
    -- population; we only emit explicit type membership for entity instances.
    _ = sliceConcepts -- documents the full concept set (used for clarity)

    context =
      PCtx
        { ctx_nm = ifcContextName (esName schema),
          ctx_pos = [orig],
          ctx_lbl = Nothing,
          ctx_lang = Just English,
          ctx_markup = Nothing,
          ctx_pats = [],
          ctx_rs = [],
          ctx_ds = pRelations,
          ctx_cs = [],
          ctx_ks = [],
          ctx_rrules = [],
          ctx_reprs = [], -- everything OBJECT for now (deliberate, see header)
          ctx_vs = [],
          ctx_gs = pClassifies,
          ctx_ifcs = [],
          ctx_ps = [],
          ctx_pops = pCptPops <> pRelPops,
          ctx_metas = [],
          ctx_enfs = []
        }

    orig = Origin "Somewhere during reading an IFC (STEP/Part-21) file."

    -- Name helpers ----------------------------------------------------------
    mkConcept :: Text -> P_Concept
    mkConcept t = mkPConcept (conceptName t)

    conceptName :: Text -> Name
    conceptName t = fst (suggestName ConceptName (toText1Unsafe (nonEmpty t)))

    relationName :: Text -> Name
    relationName t = fst (suggestName RelationName (toText1Unsafe (nonEmpty t)))

    nonEmpty :: Text -> Text
    nonEmpty t = if T.null t then "X" else t

    mkRelation :: RelKey -> P_Relation
    mkRelation k =
      P_Relation
        { dec_sign = relSign k,
          dec_prps = Set.empty,
          dec_pragma = Nothing,
          dec_nm = relationName (rkName k),
          dec_label = Nothing,
          dec_defaults = [],
          dec_Mean = [],
          dec_pos = orig
        }

    relSign :: RelKey -> P_Sign
    relSign k = P_Sign (mkConcept (rkOwner k)) (mkConcept (rkTarget k))

    relNamedRel :: RelKey -> P_NamedRel
    relNamedRel k = PNamedRel orig (relationName (rkName k)) (Just (relSign k))

    mkPair :: (Text, Text) -> PAtomPair
    mkPair (x, y) =
      PPair orig (ScriptString orig x) (ScriptString orig y)

    mkAtomValue :: Text -> PAtomValue
    mkAtomValue a = ScriptString orig a

-- | True if the named type is a SELECT in the schema.
isSelectType :: ExpressSchema -> Text -> Bool
isSelectType schema t =
  case Map.lookup t (esTypes schema) of
    Just (TSelect _) -> True
    _ -> False

-- | Context name derived from the schema name (e.g. @IFC4X3_ADD2@).
ifcContextName :: Text -> Name
ifcContextName sName =
  fst (suggestName ContextName (toText1Unsafe nm))
  where
    nm = if T.null sName then "IFC" else sName

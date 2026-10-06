-- | Systems of contexts.
--
--   The statement @CONTEXT A INCLUDES B@ makes everything that context @B@ declares
--   available in context @A@, under the name of @B@ (or an alias) as a prefix.
--   Every context has one database. So, a context that is reached along two paths is one context.
--
--   The compiler compiles one context at a time, which we call the viewer.
--   It joins the declarations of the viewer and of every context that the viewer reaches
--   into a single context, in which every thing of another context carries one prefix:
--   the label that the viewer has for that context. This module contains that renaming,
--   as a traversal over the names of a 'P_Context', and the checks on the use of prefixes.
--   The specification is @AmpersandData/FormalAmpersand/MultiContext.adl@.
module Ampersand.Input.Qualify
  ( qualifyContext,
    conceptNamesOf,
    traverseNames,
    NameUse (..),

    -- * Systems of contexts
    ContextKey,
    SystemNode (..),
    SystemEdge (..),
    System (..),
    flattenSystem,
    systemLabels,
  )
where

import Ampersand.Basics
import Ampersand.Core.ParseTree
import Ampersand.Input.ADL1.CtxError
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T

-- | The way in which a name occurs in a script.
--   The traversal reports it, so that a caller can treat the kinds of names differently.
data NameUse
  = -- | the name of a concept, where it is defined or where it is used
    ConceptUse
  | -- | the name of a concept in a CLASSIFY statement
    ClassifyUse
  | -- | the name of a concept in a REPRESENT statement
    RepresentUse
  | -- | the name of an interface, where it is defined or where it is referred to
    InterfaceUse
  | -- | the name of any other thing that a context declares:
    --   a relation, rule, identity, view, pattern or enforcement
    ThingUse
  deriving (Eq, Show)

-- | Give every name that the context declares or uses the given name space as a prefix.
--   Names of roles, and the name of the context itself, are left as they are:
--   a role is a function of a person in an organisation, which no context owns.
qualifyContext :: NameSpace -> P_Context -> P_Context
qualifyContext ns = runIdentity . traverseNames (const (Identity . qualify))
  where
    qualify nm
      | isReservedName nm = nm
      | otherwise = withNameSpace ns nm

-- | All names of concepts that occur in a context.
conceptNamesOf :: P_Context -> Set.Set Name
conceptNamesOf = getConst . traverseNames collect
  where
    collect use nm = Const $ case use of
      ThingUse -> Set.empty
      InterfaceUse -> Set.empty
      _ -> Set.singleton nm

-- | A context is identified by the file in which it is found and by its name.
--   Two versions of one context carry the same name and are found in different files.
type ContextKey = (FilePath, Text)

-- | One context of a system, as its script states it.
data SystemNode = SystemNode
  { nodeKey :: !ContextKey,
    -- | the declarations of the context, with the names as its own script writes them
    nodeCtx :: !P_Context
  }

-- | One inclusion: an item of a statement @CONTEXT A INCLUDES B FROM "file" AS alias@.
data SystemEdge = SystemEdge
  { edgeOrigin :: !Origin,
    edgeTarget :: !ContextKey,
    -- | the name of the included context
    edgeName :: !NamePart,
    edgeAlias :: !(Maybe NamePart)
  }

-- | A system of contexts, seen from one of them.
data System = System
  { -- | the context that is being compiled
    sysViewer :: !ContextKey,
    sysNodes :: !(Map.Map ContextKey SystemNode),
    sysEdges :: !(Map.Map ContextKey [SystemEdge])
  }

-- | The prefixes by which the script of a context can call the contexts it includes.
prefixesOf :: System -> ContextKey -> [(NamePart, ContextKey)]
prefixesOf sys k =
  L.nub
    [ (p, edgeTarget e)
      | e <- Map.findWithDefault [] k (sysEdges sys),
        p <- maybeToList (edgeAlias e) <> [edgeName e]
    ]

-- | A prefix is proper if it fits one included context.
properPrefixes :: System -> ContextKey -> [(NamePart, ContextKey)]
properPrefixes sys k = [(p, t) | (p, t) <- ps, length (L.nub [t' | (p', t') <- ps, p' == p]) == 1]
  where
    ps = prefixesOf sys k

-- | The contexts that the viewer reaches, the viewer excluded, nearest first.
--   The result is an error if a context includes itself, directly or indirectly.
reached :: System -> Guarded [ContextKey]
reached sys = cycles *> pure (drop 1 (bfs [sysViewer sys] [sysViewer sys]))
  where
    succs k = L.nub (map edgeTarget (Map.findWithDefault [] k (sysEdges sys)))
    bfs seen frontier = case frontier of
      [] -> seen
      k : ks ->
        let new = [t | t <- succs k, t `notElem` seen]
         in bfs (seen <> new) (ks <> new)
    cycles = dfs [] (sysViewer sys)
    dfs :: [ContextKey] -> ContextKey -> Guarded ()
    dfs path k
      | k `elem` path =
          mkSystemError
            [origin e | e <- Map.findWithDefault [] k (sysEdges sys)]
            [ "The context " <> snd k <> " includes itself:",
              "  " <> T.intercalate " includes " (map snd ([k] <> reverse (takeWhile (/= k) path) <> [k])) <> ".",
              "  A context must be deployable without the contexts that include it, so inclusion has no cycles."
            ]
      | otherwise = traverse_ (dfs (k : path)) (succs k)

instance Traced SystemEdge where
  origin = edgeOrigin

-- | The label that the viewer has for every context it reaches.
--   The label is the prefix that the things of that context carry in the joined context.
--   For a context that the viewer includes, it is the alias, or else the name of that context.
--   A context that the viewer reaches in more steps gets its name, with a number if that name is taken.
systemLabels :: System -> Guarded [(ContextKey, NamePart)]
systemLabels sys = do
  ks <- reached sys
  traverse_ canBeNamed (sysViewer sys : ks)
  pure (foldl' assign [] ks)
  where
    direct = Map.findWithDefault [] (sysViewer sys) (sysEdges sys)
    proper = properPrefixes sys (sysViewer sys)
    assign :: [(ContextKey, NamePart)] -> ContextKey -> [(ContextKey, NamePart)]
    assign acc k = acc <> [(k, fresh (preferred k))]
      where
        taken = map snd acc
        fresh p = case [c | c <- p : [postpend (tshow n) p | n <- [2 :: Int ..]], c `notElem` taken, not (isReservedNameSpace c)] of
          c : _ -> c
          [] -> fatal "An infinite list has a first element."
    preferred :: ContextKey -> NamePart
    preferred k =
      case [p | e <- direct, edgeTarget e == k, p <- maybeToList (edgeAlias e) <> [edgeName e], (p, k) `elem` proper] of
        p : _ -> p
        [] -> case [edgeName e | es <- Map.elems (sysEdges sys), e <- es, edgeTarget e == k] of
          n : _ -> n
          [] -> fatal ("The context " <> snd k <> " is reached without an inclusion.")
    -- Every inclusion needs a prefix that fits no other context that the same context includes.
    canBeNamed :: ContextKey -> Guarded ()
    canBeNamed k = traverse_ check (Map.findWithDefault [] k (sysEdges sys))
      where
        check e =
          traverse_ notReserved (edgeAlias e)
            *> hasProperPrefix e
        notReserved alias =
          when (isReservedNameSpace alias)
            $ mkSystemError
              [origin e | e <- Map.findWithDefault [] k (sysEdges sys), edgeAlias e == Just alias]
              [ "The alias " <> namePartToText alias <> " is a name space of the Ampersand system.",
                "  Choose another alias."
              ]
        hasProperPrefix e =
          when (null [() | (_, t) <- properPrefixes sys k, t == edgeTarget e])
            $ mkSystemError
              [edgeOrigin e]
              [ "The context " <> snd k <> " includes two contexts with the name " <> namePartToText (edgeName e) <> ".",
                "  Give each of them a name of its own, by adding AS followed by an alias."
              ]

-- | Join the declarations of the viewer and of every context it reaches into one context.
--   A thing of the viewer keeps its name. A thing of another context gets the label of that context as a prefix.
--   A context that is reached along two paths contributes its declarations once.
--   The joined context carries, as metadata, the label and the name of every context it contains besides the viewer.
flattenSystem :: System -> Guarded P_Context
flattenSystem sys = do
  labels <- systemLabels sys
  let labelOf k = [l | Just l <- [L.lookup k labels]]
      node k = case Map.lookup k (sysNodes sys) of
        Just n -> n
        Nothing -> fatal ("The context " <> snd k <> " has not been read.")
      renamed k = relabel sys labelOf k (nodeCtx (node k))
  viewer <- renamed (sysViewer sys)
  others <- traverse (fmap withoutInterfaces . renamed . fst) labels
  let joined = foldl' mergeContexts viewer others
  views <- traverse (\(k, l) -> (,) l <$> systemLabels sys {sysViewer = k}) labels
  pure
    joined
      { ctx_metas =
          ctx_metas joined
            <> [mkForeignMeta (posOf k) l (snd k) | (k, l) <- labels]
            <> [mkFileMeta (posOf k) l (fst k) | (k, l) <- labels]
            <> [ mkLabelViewMeta (posOf g) owner here there
                 | (owner, theirs) <- views,
                   (g, there) <- theirs,
                   Just here <- [L.lookup g labels]
               ]
      }
  where
    posOf k = fromMaybe OriginUnknown (listToMaybe (maybe [] (ctx_pos . nodeCtx) (Map.lookup k (sysNodes sys))))
    -- The interfaces of a context are the user interface of its own application.
    -- They stay in the joined context until the types have been checked, because an interface
    -- tells that the atoms of a concept are objects, and a concept has one type in every context.
    -- The type checker leaves them out of its result.
    withoutInterfaces ctx =
      ctx
        { ctx_ps = filter (not . isInterfacePurpose) (ctx_ps ctx),
          ctx_pats = [pat {pt_xps = filter (not . isInterfacePurpose) (pt_xps pat)} | pat <- ctx_pats ctx]
        }
    isInterfacePurpose p = case pexObj p of
      PRef2Interface _ -> True
      _ -> False

-- | Rename the names in the script of one context to the names of the joined context, and check its prefixes.
relabel :: System -> (ContextKey -> NameSpace) -> ContextKey -> P_Context -> Guarded P_Context
relabel sys labelOf k ctx = addWarnings unused (checks *> pure (runIdentity (traverseNames (const (Identity . rename)) ctx)))
  where
    proper = properPrefixes sys k
    improper = [p | (p, _) <- prefixesOf sys k, p `notElem` map fst proper]
    targetOf nm = case nameSpaceOf nm of
      h : _ -> L.lookup h proper
      [] -> Nothing
    rename nm
      | isReservedName nm = nm
      | otherwise = case (nameSpaceOf nm, targetOf nm) of
          (_ : rest, Just t) -> withNameSpace (labelOf t <> rest) (plain nm)
          _ -> withNameSpace (labelOf k) nm
    plain nm = mkName (nameType nm) (localName nm NE.:| [])
    withoutPrefix nm = withNameSpace (drop 1 (nameSpaceOf nm)) (plain nm)
    uses = getConst (traverseNames (\use nm -> Const [(use, nm)]) ctx)
    ownConcepts t = case Map.lookup t (sysNodes sys) of
      Nothing -> Set.empty
      Just n -> Set.filter (isNothing . targetOfIn t) (conceptNamesOf (nodeCtx n))
    targetOfIn t nm = case nameSpaceOf nm of
      h : _ -> L.lookup h (properPrefixes sys t)
      [] -> Nothing
    -- An included context that no name of this script refers to. A name with the alias counts for the context.
    unused =
      [ mkUnusedInclusionWarning (edgeOrigin e) (snd k) (namePartToText (edgeName e))
        | e : _ <- L.groupBy ((==) `on` edgeTarget) (L.sortOn edgeTarget (Map.findWithDefault [] k (sysEdges sys))),
          edgeTarget e `notElem` [t | (_, nm) <- uses, Just t <- [targetOf nm]]
      ]
    checks :: Guarded ()
    checks =
      traverse_ ambiguous (L.nub [nm | (_, nm) <- uses, take 1 (nameSpaceOf nm) `elem` map pure improper])
        *> traverse_ unknownConcept (L.nub [(nm, t) | (use, nm) <- uses, use `notElem` [ThingUse, InterfaceUse], Just t <- [targetOf nm]])
        *> traverse_ foreignInterface (L.nub [nm | (InterfaceUse, nm) <- uses, isJust (targetOf nm)])
        *> traverse_ foreignRepresent (L.nub [nm | (RepresentUse, nm) <- uses, isJust (targetOf nm)])
    ambiguous nm =
      mkSystemError
        (ctx_pos ctx)
        [ "The name " <> fullName nm <> " is ambiguous.",
          "  The context " <> snd k <> " includes two contexts with the name " <> T.intercalate "." (map namePartToText (take 1 (nameSpaceOf nm))) <> ".",
          "  Use the alias of the context you mean."
        ]
    -- Ampersand declares a concept by using it. Without this check, a misspelled name
    -- such as @Registry.Persn@ would silently become a new concept.
    unknownConcept (nm, t) =
      when (withoutPrefix nm `Set.notMember` ownConcepts t)
        $ mkSystemError
          (ctx_pos ctx)
          [ "The name " <> fullName nm <> " does not denote a concept.",
            "  The context " <> snd t <> " has no concept " <> fullName (withoutPrefix nm) <> "."
          ]
    -- The interfaces of a context are the user interface of its own application.
    foreignInterface nm =
      mkSystemError
        (ctx_pos ctx)
        [ "The name " <> fullName nm <> " refers to an interface of another context.",
          "  The interfaces of a context belong to its own application, so another context cannot use them."
        ]
    -- A REPRESENT statement determines how the atoms of a concept are stored,
    -- so only the context that owns a concept can state it.
    foreignRepresent nm =
      mkSystemError
        (ctx_pos ctx)
        [ "A REPRESENT statement mentions the concept " <> fullName nm <> ", which belongs to another context.",
          "  Only the context that declares a concept can state how it is represented."
        ]

mkSystemError :: [Origin] -> [Text] -> Guarded a
mkSystemError origs = mkErrorReadingINCLUDE (listToMaybe origs)

-- | Visit every name in a context that refers to a concept or to another thing
--   that a context declares. Roles and the name of the context itself are not visited.
traverseNames :: (Applicative f) => (NameUse -> Name -> f Name) -> P_Context -> f P_Context
traverseNames f ctx =
  ( \pats rs ds cs ks rrules reprs vs gs ifcs ps pops enfs ->
      ctx
        { ctx_pats = pats,
          ctx_rs = rs,
          ctx_ds = ds,
          ctx_cs = cs,
          ctx_ks = ks,
          ctx_rrules = rrules,
          ctx_reprs = reprs,
          ctx_vs = vs,
          ctx_gs = gs,
          ctx_ifcs = ifcs,
          ctx_ps = ps,
          ctx_pops = pops,
          ctx_enfs = enfs
        }
  )
    <$> traverse pattern' (ctx_pats ctx)
    <*> traverse rule (ctx_rs ctx)
    <*> traverse relation (ctx_ds ctx)
    <*> traverse conceptDef (ctx_cs ctx)
    <*> traverse identDef (ctx_ks ctx)
    <*> traverse roleRule (ctx_rrules ctx)
    <*> traverse representation (ctx_reprs ctx)
    <*> traverse viewDef (ctx_vs ctx)
    <*> traverse classify (ctx_gs ctx)
    <*> traverse interface (ctx_ifcs ctx)
    <*> traverse purpose (ctx_ps ctx)
    <*> traverse population (ctx_pops ctx)
    <*> traverse enforce (ctx_enfs ctx)
  where
    thing = f ThingUse
    concept = conceptAs ConceptUse
    conceptAs _ P_ONE = pure P_ONE
    conceptAs use (PCpt nm) = PCpt <$> f use nm
    sign (P_Sign s t) = P_Sign <$> concept s <*> concept t
    namedRel (PNamedRel o nm mSgn) = PNamedRel o <$> thing nm <*> traverse sign mSgn
    termPrim prim = case prim of
      PI _ -> pure prim
      Pid o c -> Pid o <$> concept c
      Patm o v mc -> Patm o v <$> traverse concept mc
      PVee _ -> pure prim
      Pfull o s t -> Pfull o <$> concept s <*> concept t
      PBin _ _ -> pure prim
      PBind o op c -> PBind o op <$> concept c
      PNamedR r -> PNamedR <$> namedRel r
      PFlipped p -> PFlipped <$> termPrim p
    term = traverse termPrim
    pattern' pat =
      ( \nm rls gns dcs rruls cds reprs ids vds xps pops enfs ->
          pat
            { pt_nm = nm,
              pt_rls = rls,
              pt_gns = gns,
              pt_dcs = dcs,
              pt_RRuls = rruls,
              pt_cds = cds,
              pt_Reprs = reprs,
              pt_ids = ids,
              pt_vds = vds,
              pt_xps = xps,
              pt_pop = pops,
              pt_enfs = enfs
            }
      )
        <$> thing (pt_nm pat)
        <*> traverse rule (pt_rls pat)
        <*> traverse classify (pt_gns pat)
        <*> traverse relation (pt_dcs pat)
        <*> traverse roleRule (pt_RRuls pat)
        <*> traverse conceptDef (pt_cds pat)
        <*> traverse representation (pt_Reprs pat)
        <*> traverse identDef (pt_ids pat)
        <*> traverse viewDef (pt_vds pat)
        <*> traverse purpose (pt_xps pat)
        <*> traverse population (pt_pop pat)
        <*> traverse enforce (pt_enfs pat)
    rule r =
      (\nm r' -> r' {rr_nm = nm})
        <$> thing (rr_nm r)
        <*> traverse termPrim r
    relation d =
      (\nm sgn -> d {dec_nm = nm, dec_sign = sgn})
        <$> thing (dec_nm d)
        <*> sign (dec_sign d)
    conceptDef cd =
      (\nm from -> cd {cdname = nm, cdfrom = from})
        <$> f ConceptUse (cdname cd)
        <*> container (cdfrom cd)
    container c = case c of
      CONTEXT _ -> pure c
      PATTERN nm -> PATTERN <$> thing nm
      Module _ -> pure c
    identDef i =
      (\nm cpt i' -> i' {ix_name = nm, ix_cpt = cpt})
        <$> thing (ix_name i)
        <*> concept (ix_cpt i)
        <*> traverse termPrim i
    roleRule rr = (\rules -> rr {mRules = rules}) <$> traverse thing (mRules rr)
    representation r = (\cpts -> r {reprcpts = cpts}) <$> traverse (conceptAs RepresentUse) (reprcpts r)
    viewDef v =
      (\nm cpt v' -> v' {vd_nm = nm, vd_cpt = cpt})
        <$> thing (vd_nm v)
        <*> concept (vd_cpt v)
        <*> traverse termPrim v
    classify g =
      (\spc gens -> g {specific = spc, generics = gens})
        <$> conceptAs ClassifyUse (specific g)
        <*> traverse (conceptAs ClassifyUse) (generics g)
    interface ifc =
      (\nm obj -> ifc {ifc_Name = nm, ifc_Obj = obj})
        <$> f InterfaceUse (ifc_Name ifc)
        <*> boxItem (ifc_Obj ifc)
    boxItem item = case item of
      P_BxTxt {} -> pure item
      P_BoxItemTerm {} ->
        (\trm mView msub -> item {obj_term = trm, obj_mView = mView, obj_msub = msub})
          <$> term (obj_term item)
          <*> traverse thing (obj_mView item)
          <*> traverse subIfc (obj_msub item)
    subIfc si = case si of
      P_Box {} -> (\items -> si {si_box = items}) <$> traverse boxItem (si_box si)
      P_InterfaceRef {} -> (\nm -> si {si_str = nm}) <$> f InterfaceUse (si_str si)
    purpose p = (\obj -> p {pexObj = obj}) <$> ref2Obj (pexObj p)
    ref2Obj r = case r of
      PRef2ConceptDef nm -> PRef2ConceptDef <$> f ConceptUse nm
      PRef2Relation nr -> PRef2Relation <$> namedRel nr
      PRef2Rule nm -> PRef2Rule <$> thing nm
      PRef2IdentityDef nm -> PRef2IdentityDef <$> thing nm
      PRef2ViewDef nm -> PRef2ViewDef <$> thing nm
      PRef2Pattern nm -> PRef2Pattern <$> thing nm
      PRef2Interface nm -> PRef2Interface <$> f InterfaceUse nm
      PRef2Context _ -> pure r
      PRef2Enforce nm -> PRef2Enforce <$> thing nm
      PRef2Role _ -> pure r
    population p = case p of
      P_RelPopu {} ->
        (\s t nr -> p {p_src = s, p_tgt = t, p_nmdr = nr})
          <$> traverse concept (p_src p)
          <*> traverse concept (p_tgt p)
          <*> namedRel (p_nmdr p)
      P_CptPopu {} -> (\c -> p {p_cpt = c}) <$> concept (p_cpt p)
    enforce e =
      (\mNm rel expr -> e {penfNm = mNm, penfRel = rel, penfExpr = expr})
        <$> traverse thing (penfNm e)
        <*> termPrim (penfRel e)
        <*> term (penfExpr e)

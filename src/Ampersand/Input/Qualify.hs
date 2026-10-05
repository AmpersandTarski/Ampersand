-- | Qualifying the names of an included context.
--
--   The statement @INCLUDE "foo.adl" AS x@ makes everything that the context in
--   @foo.adl@ declares available in the including context under the prefix @x.@.
--   The compiler realises this by renaming: every name of the included context gets
--   the alias as its name space, after which the two contexts can be merged without
--   any of their names coinciding. So, the included context contributes a disjoint
--   set of concepts, relations and rules, where a plain @INCLUDE@ contributes to a union.
--
--   This module contains that renaming, as a traversal over the names of a 'P_Context',
--   and the checks on the use of aliases that can be done before type checking.
module Ampersand.Input.Qualify
  ( qualifyContext,
    conceptNamesOf,
    checkForeignConcepts,
    checkOwnership,
    traverseNames,
    NameUse (..),
  )
where

import Ampersand.Basics
import Ampersand.Core.ParseTree
import Ampersand.Input.ADL1.CtxError
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
  | -- | the name of any other thing that a context declares:
    --   a relation, rule, identity, view, interface, pattern or enforcement
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
      _ -> Set.singleton nm

-- | A concept name that starts with an alias must exist in the context of that alias.
--   Without this check, a misspelled name such as @Reg.Persn@ would silently
--   introduce a new concept, because Ampersand declares concepts by using them.
checkForeignConcepts ::
  -- | the alias, with the concept names of the (qualified) included context
  [(NamePart, Set.Set Name)] ->
  -- | a context as it was parsed from the including script
  P_Context ->
  Guarded ()
checkForeignConcepts aliases ctx =
  case concatMap unknowns aliases of
    [] -> pure ()
    h : tl -> Errors (h NE.:| tl)
  where
    used = Set.toList (conceptNamesOf ctx)
    unknowns (alias, known) =
      [ mkAliasError
          (ctx_pos ctx)
          [ "The name " <> fullName nm <> " does not denote a concept.",
            "  The context that is included as " <> namePartToText alias <> " has no concept " <> localNameOf nm <> "."
          ]
        | nm <- used,
          take 1 (nameSpaceOf nm) == [alias],
          nm `Set.notMember` known
      ]

-- | A REPRESENT statement determines how the atoms of a concept are stored.
--   So, only the context that owns a concept may state it.
--   A CLASSIFY statement may relate concepts of different contexts.
--   The including context needs that to say which concepts of two included contexts correspond,
--   for instance that every atom of a concept in an existing system is an atom of
--   the concept with the same name in the system that replaces it.
checkOwnership :: [NamePart] -> P_Context -> Guarded ()
checkOwnership aliases ctx =
  case getConst (traverseNames collect ctx) of
    [] -> pure ()
    h : tl -> Errors (h NE.:| tl)
  where
    isForeign nm = take 1 (nameSpaceOf nm) `elem` map pure aliases
    collect use nm = Const $ case use of
      RepresentUse
        | isForeign nm ->
            [ mkAliasError
                (ctx_pos ctx)
                [ "A REPRESENT statement mentions the concept " <> fullName nm <> ", which belongs to an included context.",
                  "  Only the context that declares a concept can state how it is represented."
                ]
            ]
      _ -> []

mkAliasError :: [Origin] -> [Text] -> CtxError
mkAliasError origs msg = case mkErrorReadingINCLUDE (listToMaybe origs) msg :: Guarded () of
  Errors (e NE.:| _) -> e
  Checked _ _ -> fatal ("mkErrorReadingINCLUDE is supposed to yield an error: " <> T.unlines msg)

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
        <$> thing (ifc_Name ifc)
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
      P_InterfaceRef {} -> (\nm -> si {si_str = nm}) <$> thing (si_str si)
    purpose p = (\obj -> p {pexObj = obj}) <$> ref2Obj (pexObj p)
    ref2Obj r = case r of
      PRef2ConceptDef nm -> PRef2ConceptDef <$> f ConceptUse nm
      PRef2Relation nr -> PRef2Relation <$> namedRel nr
      PRef2Rule nm -> PRef2Rule <$> thing nm
      PRef2IdentityDef nm -> PRef2IdentityDef <$> thing nm
      PRef2ViewDef nm -> PRef2ViewDef <$> thing nm
      PRef2Pattern nm -> PRef2Pattern <$> thing nm
      PRef2Interface nm -> PRef2Interface <$> thing nm
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

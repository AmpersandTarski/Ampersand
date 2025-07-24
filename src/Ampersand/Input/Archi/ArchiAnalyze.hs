{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Module      : ArchiAnalyze
-- Description : Interprets an ArchiMate(r) repository as Ampersand context.
-- Maintainer  : stef.joosten@ou.nl
-- Stability   : experimental
--
-- The purpose of this module is to load ArchiMate content into an Ampersand context.
-- This module parses an Archi-repository by means of function `archi2PContext`, which produces a `P_Context` for merging into Ampersand.
-- That `P_Context` contains both the ArchiMate-metamodel (in the form of declarations) and the ArchiMate population that represents the model.
-- In this way, `archi2PContext ` deals with the fact that ArchiMate produces a mix of model and metamodel.
--
-- It works as follows:
-- 1. A parser transforms an Archi-file (*.archimate) to a Haskell data structure of type ArchiRepo.
-- 2. A grinder, grindArchi, turns this data structure in relations, populations and patterns to assemble a metamodel of the ArchiMate repository. This yields a context.
-- 3. This context is absorbed as though it were a separate file, so it can be included into another Ampersand script by the statement:
--    INCLUDE "<foo>.archimate"
module Ampersand.Input.Archi.ArchiAnalyze (archi2PContext) where

import Ampersand.Basics
import Ampersand.Core.ParseTree
import Ampersand.Core.ShowPStruct
import Ampersand.Input.ADL1.CtxError
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Tree.NTree.TypeDefs
import RIO.Char
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import Text.XML.HXT.Core hiding (fatal, mkName, trace, utf8)

-- | Function `archi2PContext` is meant to grind the contents of an Archi-repository into declarations and population inside a fresh Ampersand P_Context.
--   The process starts by parsing an XML-file by means of function `processStraight` into a data structure called `archiRepo`. This function uses arrow-logic from the HXT-module.
--   The resulting data structure contains the folder structure of the tool Archi (https://github.com/archimatetool/archi) and represents the model-elements and their properties.
--   A lookup-function, `typeLookup`,is derived from `archiRepo`.
--   It assigns the Archi-type (e.g. Business Process) to the identifier of an arbitrary Archi-object (e.g. "0957873").
--   Then, the properties have to be provided with identifiers (see class `WithProperties`), because Archi represents them just as key-value pairs.
--   The function `grindArchi` retrieves the population of meta-relations
--   It produces the P_Populations and P_Declarations that represent the ArchiMate model.
--   Finally, the function `mkArchiContext` produces a `P_Context` ready to be merged into the rest of Ampersand's population.
archi2PContext :: FilePath -> RIO env (Guarded P_Context)
archi2PContext archiRepoFilename -- e.g. "CArepository.archimate"
  =
  do
    -- hSetEncoding stdout utf8
    archiRepo <- liftIO $ runX (processStraight archiRepoFilename)
    let typeLookup :: Text1 -> Maybe Text1
        typeLookup atom = (Map.lookup atom . typeMap Nothing) archiRepo

    let archiRepoWithProps = (grindArchi (Nothing, typeLookup, Nothing) . identifyProps []) archiRepo
    --     let relPops = (filter (not.null.p_popps) . sortRelPops . map fst4) archiRepoWithProps
    --     let cptPops = (filter (not.null.p_popas) . sortCptPops . map fst4) archiRepoWithProps
    --     let elemCount archiConcept = (Map.lookup archiConcept . Map.fromList . atomCount . atomMap) relPops
    --     let countPop :: P_Population -> Text
    --         countPop pop = let sig = ((\(Just sgn)->sgn).p_mbSign.p_nmdr) pop in
    --                        (tshow.length.p_popps) pop              <>"\t"<>
    --                        (p_nrnm.p_nmdr) pop                     <>"\t"<>
    --                        (p_cptnm.pSrc) sig                      <>"\t"<>
    --                        (tshow.length.eqCl ppLeft.p_popps) pop  <>"\t"<>
    --                        (showMaybeInt.elemCount.pSrc) sig       <>"\t"<>
    --                        (p_cptnm.pTgt) sig                      <>"\t"<>
    --                        (tshow.length.eqCl ppRight.p_popps) pop <>"\t"<>
    --                        (showMaybeInt.elemCount.pTgt) sig
    -- --  logInfo (displayShow archiRepo<>"\n")  -- for debugging
    --     writeFileUtf8 "ArchiCount.txt"
    --      (T.intercalate "\n" $
    --          (fmap countPop relPops)
    --       <> ((fmap showArchiElems) . atomCount . atomMap $ relPops<>cptPops)
    --      )
    --     logInfo ("ArchiCount.txt written")
    return (mkArchiContext archiRepo archiRepoWithProps)

--   where sortRelPops, sortCptPops :: [P_Population] -> [P_Population] -- assembles P_Populations with the same signature into one
--         sortRelPops pops = [ (NE.head cl){p_popps = foldr L.union [] [p_popps decl | decl<-NE.toList cl]} | cl<-eqClass samePop [pop | pop@P_RelPopu{}<-pops] ]
--         sortCptPops pops = [ (NE.head cl){p_popas = foldr L.union [] [p_popas cpt  | cpt <-NE.toList cl]} | cl<-eqClass samePop [pop | pop@P_CptPopu{}<-pops] ]
--         atomMap :: [P_Population] -> Map.Map P_Concept [PAtomValue]
--         atomMap pops = Map.fromListWith L.union
--                           ([ (pSrc sgn, (L.nub.map ppLeft.p_popps) pop) | pop@P_RelPopu{}<-pops, Just sgn<-[(p_mbSign.p_nmdr) pop] ]<>
--                            [ (pTgt sgn, (L.nub.map ppRight.p_popps) pop) | pop@P_RelPopu{}<-pops, Just sgn<-[(p_mbSign.p_nmdr) pop] ]<>
--                            [ (p_cpt pop, (L.nub.p_popas) pop) | pop@P_CptPopu{}<-pops ]
--                           )
--         atomCount :: Map.Map c [a] -> [(c,Int)]
--         atomCount am = [ (archiElem,length atoms) | (archiElem,atoms)<-Map.toList am ]
--         showMaybeInt :: Show a => Maybe a -> Text
--         showMaybeInt (Just n) = tshow n
--         showMaybeInt Nothing = "Err"
--         showArchiElems :: (P_Concept,Int) -> Text
--         showArchiElems (c,n) = "\n"<>p_cptnm c<>"\t"<>tshow n

-- | function `samePop` is used to merge concepts with the same name into one concept,
--   and to merge pairs of `the same` relations into one.
--   It compares name and signature of a relation, because two relations with the same name and signature have the same set of pairs.
--   Similarly for concepts: two concepts with the same name are the same concept.
samePop :: P_Population -> P_Population -> Bool
samePop pop@P_RelPopu {} pop'@P_RelPopu {} =
  same (p_nmdr pop) (p_nmdr pop')
  where
    same nr nr' =
      case (p_mbSign nr, p_mbSign nr') of
        (Just sgn, Just sgn') -> p_nrnm nr == p_nrnm nr' && sgn == sgn'
        _ -> fatal ("Cannot compare partially defined populations of\n" <> tshow nr <> " and\n" <> tshow nr')
samePop pop@P_CptPopu {} pop'@P_CptPopu {} = p_cpt pop == p_cpt pop'
samePop _ _ = False

sameRel :: P_Relation -> P_Relation -> Bool
sameRel rel rel' = dec_nm rel == dec_nm rel' && dec_sign rel == dec_sign rel'

samePurp :: PPurpose -> PPurpose -> Bool
samePurp prp prp' = pexObj prp == pexObj prp' && mString (pexMarkup prp) == mString (pexMarkup prp')

-- | Namespace for archi models. For a first attempt, I now leave it as simple as can be:
archiNameSpace :: NameSpace
archiNameSpace = []

-- | Function `mkArchiContext` defines the P_Context that has been constructed from the ArchiMate repo
mkArchiContext :: [ArchiRepo] -> [ArchiGrain] -> Guarded P_Context
mkArchiContext [archiRepo] pops = do
  let orig = Origin "Somewhere during reading an ArchiMate file."
  nm <- case try2Name PatternName . text1ToText . archRepoName $ archiRepo of
    Left msg -> mustBeValidName orig msg
    Right (nm', _) -> pure nm'
  pure
    PCtx
      { ctx_nm = withNameSpace archiNameSpace nm,
        ctx_pos = [orig],
        ctx_lbl = Nothing,
        ctx_lang = Just Dutch,
        ctx_markup = Nothing,
        ctx_pats = pats,
        ctx_rs = [],
        ctx_ds = archiDecls,
        ctx_cs = [],
        ctx_ks = [],
        ctx_rrules = [],
        ctx_reprs = [],
        ctx_vs = [],
        ctx_gs = [],
        ctx_ifcs = [],
        ctx_ps = archiPurps,
        ctx_pops = archiPops,
        ctx_metas = [],
        ctx_enfs = []
      }
  where
    -- vwAts picks ArchiGrains that belong to one view, to assemble a pattern for that view.
    vwAts :: ArchiObj -> [ArchiGrain]
    vwAts vw = case vw of
      View {} ->
        filter participatingRel
          . filter isRelevant
          . filter inView
          $ pops
        where
          isRelevant :: ArchiGrain -> Bool
          isRelevant ag = (fullName . grainRel) ag `L.notElem` ["inside", "inView"]
          inView :: ArchiGrain -> Bool
          inView ag = case archiViewname ag of
            Nothing -> False
            Just nm -> nm == viewName vw
          participatingRel :: ArchiGrain -> Bool
          participatingRel ag = (pSrc . dec_sign . grainRel) ag `L.notElem` map (mkArchiConcept . toText1Unsafe) ["Relationship", "Property", "View"]
          mkArchiConcept :: Text1 -> P_Concept
          mkArchiConcept x =
            PCpt
              { p_cptnm = withNameSpace archiNameSpace nm
              }
            where
              nm = fst . suggestName ConceptName $ x
      _ -> fatal "May not call vwAts on a non-view element"
    -- viewpoprels contains all triples that are picked by vwAts, for all views,
    -- to compute the triples that are not assembled in any pattern.
    viewpoprels :: [ArchiGrain]
    viewpoprels =
      removeDoubles
        [ popRelVw
          | folder <- allFolders archiRepo,
            vw@View {} <- fldObjs folder,
            popRelVw <- vwAts vw
        ]
    removeDoubles :: [ArchiGrain] -> [ArchiGrain]
    removeDoubles = map NE.head . eqCl (Set.fromList . p_popps . grainPop)
    -- to compute the left-over triples, we must use L.deleteFirstsBy because we do not have Ord P_Population.
    leftovers = L.deleteFirstsBy f pops viewpoprels
      where
        f :: ArchiGrain -> ArchiGrain -> Bool
        f ag ag' = (Set.fromList . p_popps . grainPop) ag == (Set.fromList . p_popps . grainPop) ag'
    archiPops :: [P_Population]
    archiPops =
      sortRelPops --  The populations that are local to this pattern
        (map grainPop leftovers)
    archiDecls :: [P_Relation]
    archiDecls =
      sortDecls --  The relations that are declared in this pattern
        (map grainRel leftovers)
    archiPurps :: [PPurpose]
    archiPurps =
      (map NE.head . eqClass samePurp) --  The relations that are declared in this pattern
        (map grainPurp leftovers)
    pats = map mkPattern . concatMap (filter isView . fldObjs) . allFolders $ archiRepo
      where
        isView :: ArchiObj -> Bool
        isView View {} = True
        isView _ = False
        mkPattern vw =
          P_Pat
            { pos = OriginUnknown,
              pt_nm = withNameSpace archiNameSpace nm,
              pt_lbl = lbl,
              pt_rls = [],
              pt_gns = [],
              pt_dcs = sortDecls . map grainRel . vwAts $ vw,
              pt_RRuls = [],
              pt_cds = [],
              pt_Reprs = [],
              pt_ids = [],
              pt_vds = [],
              pt_xps = map grainPurp (vwAts vw),
              pt_pop = sortRelPops . map grainPop . vwAts $ vw,
              pt_end = OriginUnknown,
              pt_enfs = []
            }
          where
            (nm, lbl) = suggestName PatternName (viewName vw)
    sortRelPops :: [P_Population] -> [P_Population] -- assembles P_Populations with the same signature into one
    sortRelPops popus =
      [ (NE.head cl) {p_popps = foldr L.union [] [p_popps decl | decl <- NE.toList cl]}
        | cl <- eqClass samePop [pop | pop@P_RelPopu {} <- popus]
      ]
    sortDecls :: [P_Relation] -> [P_Relation] -- assembles P_Relations with the same signature into one
    sortDecls decls = [NE.head cl | cl <- eqClass sameRel decls]
mkArchiContext _ _ = fatal "Something dead-wrong with mkArchiContext."

-- The following code defines a data structure (called ArchiRepo) that corresponds to an Archi-repository in XML.

-- | `data ArchiRepo` represents an entire ArchiMate repository in one Haskell data structure.
data ArchiRepo = ArchiRepo
  { archRepoId :: Text1,
    archRepoName :: Text1,
    archFolders :: [Folder],
    archProperties :: [ArchiProp],
    archPurposes :: [ArchiPurpose]
  }
  deriving (Show, Eq)

-- | The smallest grain of data produced by grinding the archimate model.
data ArchiGrain = ArchiGrain
  { grainPop :: P_Population,
    grainRel :: P_Relation,
    archiViewname :: Maybe Text1,
    grainPurp :: PPurpose
  }

-- | Where 'archFolders' gives the top level folders, allFolders provides all subfolders as well.
allFolders :: ArchiRepo -> [Folder]
allFolders = concatMap recur . archFolders
  where
    recur :: Folder -> [Folder]
    recur fld = fld : (concatMap recur . fldFolders) fld

-- | `data Folder` represents the folder structure of the ArchiMate Tool.
data Folder = Folder
  { fldName :: Text, -- the name of the folder
    fldId :: Text, -- the Archi-id (e.g. "b12f3af5")
    fldType :: Text, -- the xsi:type of the folder
    fldLevel :: Int, -- the nesting level: 0=top level, 1=subfolder, 2=subsubfolder, etc.
    fldObjs :: [ArchiObj], -- the elements in the current folder, without the subfolders
    fldFolders :: [Folder] -- the subfolders
  }
  deriving (Show, Eq)

-- | `data ArchiObj` represents every ArchiMate element in the ArchiMate repo
data ArchiObj
  = Element
      { elemId :: Text1,
        elemName :: Text,
        elemType :: Text1,
        elemDocu :: Text,
        elemProps :: [ArchiProp],
        elemDocus :: [ArchiDocu]
      }
  | Relationship
      { relId :: Text1,
        relName :: Text,
        relType :: Text1,
        relDocu :: Text,
        relProps :: [ArchiProp],
        relDocus :: [ArchiDocu],
        relSrc :: Text1,
        relTgt :: Text1,
        relAccTp :: Text
      }
  | View
      { viewId :: Text1,
        viewName :: Text1,
        --  , viewType   :: Text       -- this is always "archimate:ArchimateDiagramModel" in Archi
        viewDocu :: Text,
        viewProps :: [ArchiProp],
        viewDocus :: [ArchiDocu],
        viewPoint :: Text,
        viewChilds :: [Child]
      }
  deriving (Show, Eq)

-- | We do not analyze all information that is available in views.
--   Still, the omitted information is written below, but commented out so you can follow the structure in the ArchiMate-file.
data Child = Child
  { --  chldType       :: Text
    -- , chldId         :: Text
    -- , chldAlgn       :: Text
    -- , chldFCol       :: Text
    chldElem :: Maybe Text1,
    -- , trgtConn       :: Text
    -- , bound          :: Bound
    srcConns :: [SourceConnection],
    childs :: [Child]
  }
  deriving (Show, Eq)

data Bound = Bound
  { bnd_x :: Text,
    bnd_y :: Text,
    bnd_width :: Text,
    bnd_height :: Text
  }
  deriving (Show, Eq)

newtype SourceConnection = SrcConn
  { --  sConType       :: Text,
    --  sConId         :: Text,
    --  sConSrc        :: Text,
    --  sConTgt        :: Text,
    sConRel :: Text1
    --  sConRelat      :: [Relation],
    --  sCbendPts      :: [BendPoint]
  }
  deriving (Show, Eq)

{-
data BendPoint = BendPt
  { bpStartX       :: Text
  , bpStartY       :: Text
  , bpEndX         :: Text
  , bpEndY         :: Text
  } deriving (Show, Eq)
-}

data ArchiProp = ArchiProp
  { archPropId :: Maybe Text1,
    archPropKey :: Text,
    archPropVal :: Text
  }
  deriving (Show, Eq)

newtype ArchiPurpose = ArchiPurpose
  { archPurpVal :: Text
  }
  deriving (Show, Eq)

newtype ArchiDocu = ArchiDocu
  { archDocuVal :: Text
  }
  deriving (Show, Eq)

-- | The class WithProperties is defined to generate keys for properties,
--   to be inserted in the grinding process.
--   Properties in ArchiMate have no identifying key.
--   The only data structures with properties in the inner structure of Archi
--   (i.e. in the repository minus the Views) are folders and elements.
--   In Ampersand, that key is necessary to get objects that represent an ArchiMate-property.
--   For this reason, the types ArchiRepo, Folder, and Element are instances
--   of class WithProperties.
class WithProperties a where
  allProps :: a -> [ArchiProp] -- takes all properties from an ArchiRepo, a Folder, or an Element
  identifyProps :: [Text1] -> a -> a -- distributes identifiers ( [Text1] ) over an ArchiRepo, a Folder, or an Element, in order to assign a unique identifier to each property in it.

instance WithProperties ArchiRepo where
  allProps archiRepo = allProps (archFolders archiRepo) <> archProperties archiRepo
  identifyProps _ archiRepo =
    archiRepo
      { archProperties = [prop {archPropId = Just propId} | (prop, propId) <- zip (archProperties archiRepo) propIds],
        archFolders = identifyProps fldrIds (archFolders archiRepo)
      }
    where
      identifiers = map (\i -> toText1Unsafe $ "pr-" <> tshow i) [0 :: Integer ..]
      len = (length . allProps . archFolders) archiRepo
      fldrIds = take len identifiers
      propIds = drop len identifiers

instance WithProperties Folder where
  allProps folder = allProps (fldObjs folder) <> allProps (fldFolders folder)
  identifyProps identifiers folder =
    folder
      { fldObjs = identifyProps elemsIdentifiers (fldObjs folder),
        fldFolders = identifyProps foldersIdentifiers (fldFolders folder)
      }
    where
      elemsIdentifiers = take ((length . allProps . fldObjs) folder) identifiers
      foldersIdentifiers = drop ((length . allProps . fldObjs) folder) identifiers

instance WithProperties ArchiObj where
  allProps element@Element {} = elemProps element
  allProps relation@Relationship {} = relProps relation
  allProps vw@View {} = viewProps vw
  identifyProps identifiers element@Element {} =
    element
      { elemProps = [prop {archPropId = Just propId} | (propId, prop) <- zip identifiers (elemProps element)]
      }
  identifyProps identifiers relation@Relationship {} =
    relation
      { relProps = [prop {archPropId = Just propId} | (propId, prop) <- zip identifiers (relProps relation)]
      }
  identifyProps identifiers vw@View {} =
    vw
      { viewProps = [prop {archPropId = Just propId} | (propId, prop) <- zip identifiers (viewProps vw)]
      }

instance (WithProperties a) => WithProperties [a] where
  allProps = concatMap allProps
  identifyProps identifiers xs =
    [identifyProps ids x | (ids, x) <- zip idss xs]
    where
      countProperties :: [Int] -- a list that contains the lengths of property lists in `folder`
      countProperties = map (length . allProps) xs
      idss = distr countProperties identifiers
      distr :: [Int] -> [a] -> [[a]] -- distribute identifiers in order to allocate them to items in `archiRepo`
      distr (n : ns) idents = take n idents : distr ns (drop n idents)
      distr [] _ = []

-- | In order to populate an Archi-metamodel with the contents of an Archi-repository,
--   we must grind that contents into binary tables. For that purpose, we define the
--   class MetaArchi, and instantiate it on ArchiRepo and all its constituent types.
class MetaArchi a where
  typeMap :: Maybe Text1 -> a -> Map Text1 Text1 -- the map that determines the type (xsi:type) of every atom (id-field) in the repository
  -- grindArchi takes two parameters:
  --  1. the view name (Maybe Text), just used when scanning inside a view to link an ArchiMate object to a view;
  --  2. a lookup function (Text->Maybe Text) called typeLookup, that looks up the type of an ArchiMate object. E.g. typeLookup ("702221af-2740-46e2-a0ae-c64d0226ff95") = "BusinessRole"

  grindArchi ::
    (Maybe Text1, Text1 -> Maybe Text1, Maybe Text1) ->
    a -> -- create population and the corresponding metamodel for the P-structure in Ampersand
    [ArchiGrain]

instance MetaArchi ArchiRepo where
  typeMap _ archiRepo =
    typeMap Nothing (archFolders archiRepo)
  grindArchi env archiRepo =
    [ translateArchiElem
        (toText1Unsafe "name")
        (toText1Unsafe "ArchiRepo", toText1Unsafe "Text")
        Nothing
        (Set.singleton P_Uni)
        [(text1ToText $ archRepoId archiRepo, text1ToText $ archRepoName archiRepo)]
    ]
      <> [ translateArchiElem
             (toText1Unsafe "purpose")
             (toText1Unsafe "ArchiRepo", toText1Unsafe "Text")
             Nothing
             (Set.singleton P_Uni)
             [(text1ToText $ archRepoId archiRepo, archPurpVal purp) | purp <- archPurposes archiRepo]
           | (not . null . archPurposes) archiRepo
         ]
      <> [ translateArchiElem (toText1Unsafe "propOf") (toText1Unsafe "Property", toText1Unsafe "ArchiRepo") Nothing (Set.singleton P_Uni) [(text1ToText propid, text1ToText $ archRepoId archiRepo)]
           | prop <- archProperties archiRepo,
             Just propid <- [archPropId prop]
         ]
      <> concatMap (grindArchi env) (archFolders archiRepo)
      <> (concatMap (grindArchi env) . archProperties) archiRepo

instance MetaArchi Folder where
  typeMap _ folder =
    (typeMap Nothing . fldObjs) folder
      <> (typeMap Nothing . fldFolders) folder
  grindArchi env folder =
    (concatMap (grindArchi env) . fldObjs) folder
      <> (concatMap (grindArchi env) . fldFolders) folder

-- A type map is constructed for Archi-objects only. Taking relationships into this map brings Archi into higher order logic, and may cause black holes in Haskell.
instance MetaArchi ArchiObj where
  typeMap maybeViewName element@Element {} =
    Map.fromList [(elemId element, elemType element)]
      <> typeMap maybeViewName (elemProps element)
  typeMap maybeViewName relation@Relationship {} =
    Map.fromList [(relId relation, toText1Unsafe "Relationship")]
      <> typeMap maybeViewName (relProps relation)
  typeMap _ diagram@View {} =
    Map.fromList [(viewId diagram, toText1Unsafe "View")]
      <> typeMap (Just (viewName diagram)) (viewProps diagram)
  grindArchi env@(_, _, maybeViewname) element@Element {} =
    [ translateArchiElem (toText1Unsafe "name") (elemType element, toText1Unsafe "Text") maybeViewname (Set.singleton P_Uni) [(text1ToText $ elemId element, elemName element)]
      | (not . T.null . elemName) element
    ]
      <> [ translateArchiElem (toText1Unsafe "docu") (elemType element, toText1Unsafe "Text") maybeViewname (Set.singleton P_Uni) [(text1ToText $ elemId element, elemDocu element)] -- documentation in the XML-tag
           | (not . T.null . elemDocu) element
         ]
      <> [ translateArchiElem (toText1Unsafe "docu") (elemType element, toText1Unsafe "Text") maybeViewname (Set.singleton P_Uni) [(text1ToText $ elemId element, archDocuVal eldo)] -- documentation with <documentation/> tags.
           | eldo <- elemDocus element
         ]
      <> [ translateArchiElem (toText1Unsafe "propOf") (toText1Unsafe "Property", toText1Unsafe "ArchiObject") maybeViewname (Set.singleton P_Uni) [(text1ToText propid, text1ToText $ elemId element)]
           | prop <- elemProps element,
             Just propid <- [archPropId prop]
         ]
      <> (concatMap (grindArchi env) . elemProps) element
  grindArchi env@(_, typeLookup, maybeViewname) relation@Relationship {} =
    [translateArchiElem relLabel (xType, yType) maybeViewname Set.empty [(text1ToText $ relSrc relation, text1ToText $ relTgt relation)]]
      <> [translateArchiElem (toText1Unsafe "name") (toText1Unsafe "Relationship", toText1Unsafe "Text") maybeViewname (Set.singleton P_Uni) [(text1ToText $ relId relation, text1ToText relLabel)]]
      <> [translateArchiElem (toText1Unsafe "type") (toText1Unsafe "Relationship", toText1Unsafe "Text") maybeViewname (Set.singleton P_Uni) [(text1ToText $ relId relation, text1ToText relTyp)]]
      <> [translateArchiElem (toText1Unsafe "source") (toText1Unsafe "Relationship", xType) maybeViewname (Set.singleton P_Uni) [(text1ToText $ relId relation, text1ToText $ relSrc relation)]]
      <> [translateArchiElem (toText1Unsafe "target") (toText1Unsafe "Relationship", yType) maybeViewname (Set.singleton P_Uni) [(text1ToText $ relId relation, text1ToText $ relTgt relation)]]
      <> [ translateArchiElem (toText1Unsafe "docu") (toText1Unsafe "Relationship", toText1Unsafe "Text") maybeViewname (Set.singleton P_Uni) [(text1ToText $ relId relation, relDocu relation)] -- documentation in the XML-tag
           | (not . T.null . relDocu) relation
         ]
      <> [ translateArchiElem (toText1Unsafe "docu") (toText1Unsafe "Relationship", toText1Unsafe "Text") maybeViewname (Set.singleton P_Uni) [(text1ToText $ relId relation, archDocuVal reldo)] -- documentation with <documentation/> tags.
           | reldo <- relDocus relation
         ]
      <> [ translateArchiElem (toText1Unsafe "accessType") (toText1Unsafe "Relationship", toText1Unsafe "AccessType") maybeViewname (Set.singleton P_Uni) [(text1ToText $ relId relation, relAccTp relation)]
           | (not . T.null . relAccTp) relation
         ]
      <> [ translateArchiElem (toText1Unsafe "propOf") (toText1Unsafe "Property", toText1Unsafe "Relationship") maybeViewname (Set.singleton P_Uni) [(text1ToText propid, text1ToText $ relId relation)]
           | prop <- relProps relation,
             Just propid <- [archPropId prop]
         ]
      <> (concatMap (grindArchi env) . relProps) relation
    where
      relTyp :: Text1
      relTyp = (relCase . unFix . relType) relation -- the relation type,  e.g. "access"
        where
          unFix :: Text1 -> Text1
          unFix str
            | str == toText1Unsafe "Relationship" = str
            | "Relationship" `T.isSuffixOf` text1ToText str =
                toText1Unsafe . T.reverse . T.drop 12 . T.reverse . text1ToText $ str
            | otherwise = str
      relLabel :: Text1
      relLabel = case text1ToText relTyp of
        "association" ->
          case T.uncons (relName relation) of
            Nothing -> relTyp
            Just (h, tl) -> relCase $ Text1 h tl -- the name given by the user, e.g. "create/update"
        _ -> relTyp
      xType = case typeLookup (relSrc relation) of
        Just str -> str
        Nothing -> fatal ("No Archi-object found for Archi-identifier " <> tshow (relSrc relation))
      yType = case typeLookup (relTgt relation) of
        Just str -> str
        Nothing -> fatal ("No Archi-object found for Archi-identifier " <> tshow (relTgt relation))
  grindArchi (_, typeLookup, _) diagram@View {} =
    [translateArchiElem (toText1Unsafe "name") (toText1Unsafe "View", toText1Unsafe "Text") maybeViewName (Set.singleton P_Uni) [(text1ToText $ viewId diagram, text1ToText $ viewName diagram)]]
      <> [ translateArchiElem (toText1Unsafe "propOf") (toText1Unsafe "Property", toText1Unsafe "View") maybeViewName (Set.singleton P_Uni) [(text1ToText propid, text1ToText $ viewId diagram)]
           | prop <- viewProps diagram,
             Just propid <- [archPropId prop]
         ]
      <> [ translateArchiElem (toText1Unsafe "docu") (toText1Unsafe "View", toText1Unsafe "Text") maybeViewName (Set.singleton P_Uni) [(text1ToText $ viewId diagram, viewDocu diagram)] -- documentation in the XML-tag
           | (not . T.null . viewDocu) diagram
         ]
      <> [ translateArchiElem (toText1Unsafe "docu") (toText1Unsafe "View", toText1Unsafe "Text") maybeViewName (Set.singleton P_Uni) [(text1ToText $ viewId diagram, archDocuVal viewdoc)] -- documentation with <documentation/> tags.
           | viewdoc <- viewDocus diagram
         ]
      <> [ translateArchiElem (toText1Unsafe "inView") (chldType, toText1Unsafe "View") maybeViewName Set.empty [(text1ToText chld, text1ToText $ viewId diagram)] -- register the views in which an element is used.
           | viewelem <- viewChilds diagram,
             Just chld <- [chldElem viewelem],
             Just chldType <- [typeLookup chld]
         ]
      <> [ translateArchiElem (toText1Unsafe "viewpoint") (toText1Unsafe "View", toText1Unsafe "ViewPoint") maybeViewName (Set.singleton P_Uni) [(text1ToText $ viewId diagram, viewPoint diagram)] -- documentation with <documentation/> tags.
           | (not . T.null . viewPoint) diagram
         ]
      <> (concatMap (grindArchi (Nothing, typeLookup, maybeViewName)) . viewProps) diagram
      <> (concatMap (grindArchi (Just (viewId diagram), typeLookup, maybeViewName)) . viewChilds) diagram
    where
      maybeViewName = Just (viewName diagram)

instance MetaArchi Child where
  typeMap _ _ =
    Map.empty
  grindArchi env@(Just viewid, typeLookup, maybeViewName) diagrObj =
    [ translateArchiElem (toText1Unsafe "inView") (elType, viewtype) maybeViewName Set.empty [(text1ToText chld, text1ToText viewid)]
      | child <- childs diagrObj,
        Just chld <- [chldElem child],
        Just elType <- [typeLookup chld],
        Just viewtype <- [typeLookup viewid]
    ]
      <> [ translateArchiElem (toText1Unsafe "inView") (connType, viewtype) maybeViewName Set.empty [(text1ToText $ sConRel conn, text1ToText viewid)]
           | conn <- srcConns diagrObj,
             Just connType <- [typeLookup (sConRel conn)],
             Just viewtype <- [typeLookup viewid]
         ]
      <> [ translateArchiElem (toText1Unsafe "inside") (childtype, objtype) maybeViewName Set.empty [(text1ToText chld, text1ToText chlDiag)]
           | child <- childs diagrObj,
             Just chld <- [chldElem child],
             Just childtype <- [typeLookup chld],
             Just chlDiag <- [chldElem diagrObj],
             Just objtype <- [typeLookup chlDiag]
         ]
      <> (concatMap (grindArchi env) . childs) diagrObj
  grindArchi (maybeViewid, _, maybeViewName) _ = fatal ("\nmaybeViewid = " <> tshow maybeViewid <> "\nmaybeViewName = " <> tshow maybeViewName)

instance MetaArchi ArchiProp where
  typeMap _ property =
    Map.fromList [(propid, toText1Unsafe "Property") | Just propid <- [archPropId property]]
  grindArchi (_, _, maybeViewname) property =
    [ translateArchiElem
        (toText1Unsafe "key")
        (toText1Unsafe "Property", toText1Unsafe "Text")
        maybeViewname
        (Set.singleton P_Uni)
        [ (text1ToText propid, archPropKey property)
          | (not . T.null . archPropKey) property,
            Just propid <- [archPropId property]
        ],
      translateArchiElem
        (toText1Unsafe "value")
        (toText1Unsafe "Property", toText1Unsafe "Text")
        maybeViewname
        (Set.singleton P_Uni)
        [ (text1ToText propid, archPropVal property)
          | (not . T.null . archPropVal) property,
            Just propid <- [archPropId property]
        ]
    ]

instance (MetaArchi a) => MetaArchi [a] where
  typeMap maybeViewName xs = Map.unions [typeMap maybeViewName x | x <- xs]
  grindArchi typeLookup xs = concat [grindArchi typeLookup x | x <- xs]

-- | The function `translateArchiElem` does the actual compilation of data objects from archiRepo into the Ampersand structure.
--   It looks redundant to produce both a `P_Population` and a `P_Relation`, but the first contains the population and the second is used to
--   include the metamodel of ArchiMate in the population. This saves the author the effort of maintaining an ArchiMate-metamodel.
translateArchiElem ::
  Text1 ->
  (Text1, Text1) ->
  Maybe Text1 ->
  Set.Set PProp ->
  [(Text, Text)] ->
  ArchiGrain
translateArchiElem plainNm (plainSrcName, plainTgtName) maybeViewName props tuples =
  ArchiGrain
    { grainPop = P_RelPopu Nothing Nothing OriginUnknown ref_to_relation (tuples2PAtomPairs tuples),
      grainRel =
        P_Relation
          { dec_sign = ref_to_signature,
            dec_prps = props,
            dec_pragma = Nothing,
            dec_nm = relName',
            dec_label = Nothing,
            dec_defaults = [],
            dec_Mean = [],
            dec_pos = OriginUnknown
          },
      archiViewname = maybeViewName,
      grainPurp =
        PPurpose
          { pos = OriginUnknown, -- the position in the Ampersand script of this purpose definition
            pexObj = PRef2Relation ref_to_relation, -- the reference to the object whose purpose is explained
            pexMarkup = P_Markup Nothing Nothing purpText, -- the piece of text, including markup and language info
            pexRefIDs = [] -- the references (for traceability)
          }
    }
  where
    -- This is unsafe, for not all Text1 is valid for a Name
    toNameUnsafe :: NameType -> Text1 -> Name
    toNameUnsafe typ x = withNameSpace archiNameSpace nm
      where
        (nm, _) = suggestName typ x
    relName' = toNameUnsafe RelationName plainNm
    srcName = toNameUnsafe ConceptName plainSrcName
    tgtName = toNameUnsafe ConceptName plainTgtName
    purpText :: Text
    purpText = showP ref_to_relation <> " serves to embody the ArchiMate metamodel"
    ref_to_relation :: P_NamedRel
    ref_to_relation = PNamedRel OriginUnknown relName' (Just ref_to_signature)
    ref_to_signature :: P_Sign
    ref_to_signature = P_Sign (PCpt srcName) (PCpt tgtName)

-- | Function `relCase` is used to generate relation identifiers that are syntactically valid in Ampersand.
relCase :: Text1 -> Text1
relCase (Text1 c cs) = escapeIdentifier $ Text1 (toLower c) cs

-- | Function `tuples2PAtomPairs` is used to save ourselves some writing effort
tuples2PAtomPairs :: [(Text, Text)] -> [PAtomPair]
tuples2PAtomPairs = map tuple2PAtomPair

tuple2PAtomPair :: (Text, Text) -> PAtomPair
tuple2PAtomPair (x, y) =
  PPair
    OriginUnknown
    (ScriptString OriginUnknown x)
    (ScriptString OriginUnknown y)

-- | The function `processStraight` derives an ArchiRepo from an Archi-XML-file.
processStraight :: FilePath -> IOSLA (XIOState s0) XmlTree ArchiRepo
processStraight absFilePath =
  readDocument
    [ withRemoveWS yes, -- purge redundant white spaces
      withCheckNamespaces yes, -- propagates name spaces into QNames
      withTrace 0 -- if >0 gives trace messages.
    ]
    uri
    >>> analArchiRepo
  where
    uri = "file://" <> n (g <$> absFilePath)
      where
        g x = if x == '\\' then '/' else x
        n [] = fatal "absFilePath is an empty list."
        n x@(h : _) = if h /= '/' then '/' : x else x
    analArchiRepo :: (ArrowXml a) => a XmlTree ArchiRepo
    analArchiRepo =
      (atTag "archimate:model" <+> atTag "archimate:ArchimateModel")
        >>> proc l -> do
          repoNm <- getAttrValue "name" -< l
          repoId <- getAttrValue "id" -< l
          purposes <- listA (getChildren >>> getPurpose) -< l
          folders <- listA (getChildren >>> getFolder 0) -< l
          props <- listA (getChildren >>> getProp) -< l
          returnA
            -<
              ArchiRepo
                { archRepoName = case T.uncons $ T.pack repoNm of
                    Nothing -> toText1Unsafe "ArchimateModel"
                    Just (h, tl) -> Text1 h tl,
                  archRepoId = case T.uncons $ T.pack repoId of
                    Nothing -> toText1Unsafe "ArchimateModel"
                    Just (h, tl) -> Text1 h tl,
                  archFolders = folders,
                  archProperties = [prop {archPropId = Just . toText1Unsafe $ "pr-" <> tshow i} | (prop, i) <- zip props [length (allProps folders) ..]],
                  archPurposes = purposes
                }

    getFolder :: (ArrowXml a) => Int -> a XmlTree Folder
    getFolder level =
      isElem
        >>> (hasName "folder" <+> hasName "folders")
        >>> proc l -> do
          fldNm' <- getAttrValue "name" -< l
          fldId' <- getAttrValue "id" -< l
          fldType' <- getAttrValue "type" -< l
          objects <- listA (getChildren >>> getArchiObj) -< l
          subFlds <- listA (getChildren >>> getFolder (level + 1)) -< l
          returnA
            -<
              Folder
                { fldName = T.pack fldNm',
                  fldId = T.pack fldId',
                  fldType = T.pack fldType',
                  fldLevel = level,
                  fldObjs = objects,
                  fldFolders = subFlds
                }

    getArchiObj :: (ArrowXml a) => a XmlTree ArchiObj
    getArchiObj =
      isElem
        >>> (hasName "element" <+> hasName "elements")
        >>> proc l -> do
          -- don't use atTag, because there is recursion in getFolder.
          objId <- getAttrValue "id" -< l
          let objId' = case T.uncons $ T.pack objId of
                Nothing -> fatal "id is empty"
                Just (h, tl) -> Text1 h tl
          objName <- getAttrValue "name" -< l
          objType <- getAttrValue "xsi:type" -< l
          let objType' = case T.uncons $ T.pack objType of
                Nothing -> fatal $ "Type is empty for object " <> T.pack objId
                Just (h, tl) -> Text1 h tl
          objDocu <- getAttrValue "documentation" -< l -- This accommodates Archi vs. 2
          objDocus <- listA (getChildren >>> getDocu) -< l
          objProps <- listA (getChildren >>> getProp) -< l
          objSrc <- getAttrValue "source" -< l -- specific for Relationships
          objTgt <- getAttrValue "target" -< l -- specific for Relationships
          objAccTp <- getAttrValue "accessType" -< l -- specific for Relationships
          objVwPt <- getAttrValue "viewpoint" -< l -- specific for Views
          objChilds <- listA (getChildren >>> getChild) -< l -- specific for Views
          returnA
            -<
              if objType == "archimate:ArchimateDiagramModel"
                then
                  View
                    { viewId = objId',
                      viewName = case T.uncons $ T.pack objName of
                        Nothing -> fatal $ "View name is empty for view " <> T.pack objId
                        Just (h, tl) -> Text1 h tl,
                      viewDocu = T.pack objDocu,
                      viewDocus = objDocus,
                      viewProps = objProps,
                      viewPoint = T.pack objVwPt,
                      viewChilds = objChilds
                    }
                else case objSrc of
                  [] ->
                    Element
                      { elemId = objId',
                        elemName = T.pack objName,
                        elemType = unPrefix objType',
                        elemDocu = T.pack objDocu,
                        elemDocus = objDocus,
                        elemProps = objProps
                      }
                  (h : tl) ->
                    Relationship
                      { relId = objId',
                        relName = T.pack objName,
                        relType = unPrefix objType',
                        relDocu = T.pack objDocu,
                        relDocus = objDocus,
                        relProps = objProps,
                        relSrc = Text1 h $ T.pack tl,
                        relTgt = case objTgt of
                          [] -> fatal $ "No target defined for relation " <> text1ToText objId'
                          (c : s) -> Text1 c (T.pack s),
                        relAccTp = T.pack objAccTp
                      }

    unPrefix :: Text1 -> Text1
    unPrefix str
      | text1ToText str == thePrefix = fatal $ text1ToText str <> " found without continuation text."
      | thePrefix `T.isPrefixOf` text1ToText str = toText1Unsafe . T.drop (T.length thePrefix) . text1ToText $ str
      | otherwise = str
      where
        thePrefix = "archimate:"
    getProp :: (ArrowXml a) => a XmlTree ArchiProp
    getProp =
      isElem
        >>> (hasName "property" <+> hasName "properties")
        >>> proc l -> do
          propKey <- getAttrValue "key" -< l
          propVal <- getAttrValue "value" -< l
          returnA
            -<
              ArchiProp
                { archPropKey = T.pack propKey,
                  archPropId = Nothing, -- error "fatal 315: archPropId not yet defined"
                  archPropVal = T.pack propVal
                }
    getPurpose :: (ArrowXml a) => a XmlTree ArchiPurpose
    getPurpose =
      isElem
        >>> hasName "purpose"
        >>> proc l -> do
          purpVal <- text -< l
          returnA -< ArchiPurpose {archPurpVal = T.pack purpVal}
    getDocu :: (ArrowXml a) => a XmlTree ArchiDocu
    getDocu =
      isElem
        >>> hasName "documentation"
        >>> proc l -> do
          docuVal <- text -< l
          returnA -< ArchiDocu {archDocuVal = T.pack docuVal}

    getChild :: (ArrowXml a) => a XmlTree Child
    getChild =
      atTag "child"
        <+> atTag "children"
        >>> proc l -> do
          --  chldType'  <- getAttrValue "xsi:type"            -< l
          --  chldId'    <- getAttrValue "id"                  -< l
          --  chldName'  <- getAttrValue "name"                -< l -- defined, but not used.
          --  chldFCol'  <- getAttrValue "fillColor"           -< l
          --  chldAlgn'  <- getAttrValue "textAlignment"       -< l
          chldElem' <- getAttrValue "archimateElement" -< l
          --  trgtConn'  <- getAttrValue "targetConnections"   -< l
          --  bound'     <- getChildren >>> getBound           -< l
          srcConns' <- listA (getChildren >>> getSrcConn) -< l
          childs' <- listA (getChildren >>> getChild) -< l
          returnA
            -<
              Child
                { --                        chldType = unPrefix (T.pack chldType')
                  --                      , chldId   = T.pack chldId'
                  --                      , chldAlgn = T.pack chldAlgn'
                  --                      , chldFCol = T.pack chldFCol'
                  chldElem = case chldElem' of
                    [] -> Nothing
                    c : s -> Just (Text1 c (T.pack s)),
                  --                      , trgtConn = T.pack trgtConn'
                  --                      , bound    = bound'
                  srcConns = srcConns',
                  childs = childs'
                }

    -- The following does not work yet for recent versions of Archi
    -- which should parse with hasName "sourceConnection", but doesn't. TODO
    -- However, forget about this after the ArchiMate Exchange Format can be parsed.
    getSrcConn :: (ArrowXml a) => a XmlTree SourceConnection
    getSrcConn =
      isElem
        >>> hasName "sourceConnection"
        <+> hasName "sourceConnections"
        >>> proc l -> do
          --  sConType'  <- getAttrValue "xsi:type"              -< l
          --  sConId'    <- getAttrValue "id"                    -< l
          --  sConSrc'   <- getAttrValue "source"                -< l
          --  sConTgt'   <- getAttrValue "target"                -< l
          sConRel' <- getAttrValue "archimateRelationship" -< l
          --  sConRelat' <- listA (getChildren>>>getRelation)    -< l
          --  bendPts'   <- listA (getChildren>>>getBendPt)      -< l
          returnA
            -<
              SrcConn
                { --                          sConType  = T.pack sConType'
                  --                          sConId    = T.pack sConId'
                  --                          sConSrc   = T.pack sConSrc'
                  --                          sConTgt   = T.pack sConTgt'
                  sConRel = case sConRel' of
                    [] -> fatal "sourceConnection found without archimateRelationship"
                    c : s -> Text1 c (T.pack s)
                    --                          sConRelat = sConRelat'
                    --                          sCbendPts = bendPts'
                }

{- We no longer analyze all information that is available.
     getRelation :: ArrowXml a => a XmlTree Relation
     getRelation = isElem >>> hasName "relationship" >>>
         proc l -> do relType'   <- getAttrValue "xsi:type"          -< l
                      relHref'   <- getAttrValue "href"              -< l
                      returnA    -< Relation{ relType = T.pack relType'
                                            , relHref = T.pack relHref'
                                            }
     getBound :: ArrowXml a => a XmlTree Bound
     getBound = isElem >>> hasName "bounds" >>>
         proc l -> do bnd_x'     <- getAttrValue "x"                 -< l
                      bnd_y'     <- getAttrValue "y"                 -< l
                      bndWidth'  <- getAttrValue "width"             -< l
                      bndHeight' <- getAttrValue "height"            -< l
                      returnA    -< Bound   { bnd_x      = T.pack bnd_x'
                                            , bnd_y      = T.pack bnd_y'
                                            , bnd_width  = T.pack bndWidth'
                                            , bnd_height = T.pack bndHeight'
                                            }
     getBendPt :: ArrowXml a => a XmlTree BendPoint
     getBendPt = isElem >>> hasName "bendpoints" >>>
         proc l -> do bpStartX'  <- getAttrValue "startX"              -< l
                      bpStartY'  <- getAttrValue "startY"              -< l
                      bpEndX'    <- getAttrValue "endX"                -< l
                      bpEndY'    <- getAttrValue "endY"                -< l
                      returnA    -< BendPt  { bpStartX  = T.pack bpStartX'
                                            , bpStartY  = T.pack bpStartY'
                                            , bpEndX    = T.pack bpEndX'
                                            , bpEndY    = T.pack bpEndY'
                                            }
-}

-- | Auxiliaries `atTag` and `text` have been copied from the tutorial papers about arrows
atTag :: (ArrowXml a) => Text -> a (NTree XNode) XmlTree
atTag tag = deep (isElem >>> hasName (T.unpack tag))

text :: (ArrowXml a) => a (NTree XNode) String
text = getChildren >>> getText

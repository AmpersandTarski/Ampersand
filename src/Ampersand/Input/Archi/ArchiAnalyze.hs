{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings, DuplicateRecordFields #-}
{-|
Module      : ArchiAnalyze
Description : Interprets an ArchiMate(r) repository as Ampersand context.
Maintainer  : stef.joosten@ou.nl
Stability   : experimental

The purpose of this module is to load Archimate content into an Ampersand context.
This module parses an Archi-repository by means of function `archi2PContext`, which produces a `P_Context` for merging into Ampersand.
That `P_Context` contains both the Archimate-metamodel (in the form of declarations) and the Archimate population that represents the model.
In this way, `archi2PContext ` deals with the fact that Archimate produces a mix of model and metamodel.
-}
module Ampersand.Input.Archi.ArchiAnalyze (archi2PContext) where

import           Ampersand.Basics
import           Ampersand.Core.ParseTree
import           Ampersand.Input.ADL1.CtxError
import           RIO.Char
import qualified RIO.NonEmpty as NE
import qualified Data.Map.Strict as Map -- import qualified, to avoid name clashes with Prelude functions
import qualified Data.Set as Set
import           Data.Tree.NTree.TypeDefs
import qualified RIO.List as L
import qualified RIO.Text as T
import           Text.XML.HXT.Core hiding (utf8, fatal,trace)

-- | Function `archi2PContext` is meant to grind the contents of an Archi-repository into declarations and population inside a fresh Ampersand P_Context.
--   The process starts by parsing an XML-file by means of function `processStraight` into a data structure called `archiRepo`. This function uses arrow-logic from the HXT-module.
--   The resulting data structure contains the folder structure of the tool Archi (https://github.com/archimatetool/archi) and represents the model-elements and their properties.
--   A lookup-function, `typeLookup`,is derived from `archiRepo`.
--   It assigns the Archi-type (e.g. Business Process) to the identifier of an arbitrary Archi-object (e.g. "0957873").
--   Then, the properties have to be provided with identifiers (see class `WithProperties`), because Archi represents them just as key-value pairs.
--   The function `grindArchi` retrieves the population of meta-relations
--   It produces the P_Populations and P_Declarations that represent the Archimate model.
--   Finally, the function `mkArchiContext` produces a `P_Context` ready to be merged into the rest of Ampersand's population.
archi2PContext :: (HasLogFunc env) => FilePath -> RIO env (Guarded P_Context)
archi2PContext archiRepoFilename  -- e.g. "CArepository.archimate"
 = do -- hSetEncoding stdout utf8
      archiRepo <- liftIO $ runX (processStraight archiRepoFilename)
      let fst3 (x,_,_) = x
      let typeLookup atom = (Map.lookup atom . typeMap) archiRepo
      let archiRepoWithProps = (grindArchi typeLookup.identifyProps []) archiRepo
      let relPops = (filter (not.null.p_popps) . sortRelPops . map fst3) archiRepoWithProps
      let cptPops = (filter (not.null.p_popas) . sortCptPops . map fst3) archiRepoWithProps
      let elemCount archiConcept = (Map.lookup archiConcept . Map.fromList . atomCount . atomMap) relPops
      let countPop :: P_Population -> Text
          countPop pop = let sig = ((\(Just sgn)->sgn).p_mbSign.p_nmdr) pop in
                         (tshow.length.p_popps) pop             <>"\t"<>
                         (p_nrnm.p_nmdr) pop                    <>"\t"<>
                         (p_cptnm.pSrc) sig                     <>"\t"<>
                         (tshow.length.eqCl ppLeft.p_popps) pop  <>"\t"<>
                         (showMaybeInt.elemCount.pSrc) sig      <>"\t"<>
                         (p_cptnm.pTgt) sig                     <>"\t"<>
                         (tshow.length.eqCl ppRight.p_popps) pop <>"\t"<>
                         (showMaybeInt.elemCount.pTgt) sig
      logInfo (displayShow archiRepo<>"\n")  -- for debugging
      writeFileUtf8 "ArchiCount.txt"
       (T.intercalate "\n" $
           (fmap countPop relPops)
        <> ((fmap showArchiElems) . atomCount . atomMap $ relPops<>cptPops) 
       )
      logInfo ("ArchiCount.txt written")
      return (mkArchiContext archiRepoWithProps)
   where sortRelPops, sortCptPops :: [P_Population] -> [P_Population] -- assembles P_Populations with the same signature into one
         sortRelPops pops = [ (NE.head cl){p_popps = foldr L.union [] [p_popps decl | decl<-NE.toList cl]} | cl<-eqClass samePop [pop | pop@P_RelPopu{}<-pops] ]
         sortCptPops pops = [ (NE.head cl){p_popas = foldr L.union [] [p_popas cpt  | cpt <-NE.toList cl]} | cl<-eqClass samePop [pop | pop@P_CptPopu{}<-pops] ]
         atomMap :: [P_Population] -> Map.Map P_Concept [PAtomValue]
         atomMap pops = Map.fromListWith L.union
                           ([ (pSrc sgn, (L.nub.map ppLeft.p_popps) pop) | pop@P_RelPopu{}<-pops, Just sgn<-[(p_mbSign.p_nmdr) pop] ]<>
                            [ (pTgt sgn, (L.nub.map ppRight.p_popps) pop) | pop@P_RelPopu{}<-pops, Just sgn<-[(p_mbSign.p_nmdr) pop] ]<>
                            [ (p_cpt pop, (L.nub.p_popas) pop) | pop@P_CptPopu{}<-pops ]
                           )
         atomCount :: Map.Map c [a] -> [(c,Int)]
         atomCount am = [ (archiElem,length atoms) | (archiElem,atoms)<-Map.toList am ]
         showMaybeInt :: Show a => Maybe a -> Text
         showMaybeInt (Just n) = tshow n
         showMaybeInt Nothing = "Err"
         showArchiElems :: (P_Concept,Int) -> Text
         showArchiElems (c,n) = "\n"<>p_cptnm c<>"\t"<>tshow n

-- | function `samePop` is used to merge concepts with the same name into one concept,
--   and to merge pairs of `the same` relations into one.
--   It compares name and signature of a relation, because two relations with the same name and signature have the same set of pairs.
--   Similarly for concepts: two concepts with the same name are the same concept.
samePop :: P_Population -> P_Population -> Bool
samePop pop@P_RelPopu{} pop'@P_RelPopu{}
 = same (p_nmdr pop) (p_nmdr pop')
   where same nr nr'
          = case (p_mbSign nr, p_mbSign nr') of
                 (Just sgn,    Just sgn') -> p_nrnm nr==p_nrnm nr' && sgn==sgn'
                 _                        -> fatal ("Cannot compare partially defined populations of\n"<>tshow nr<>" and\n"<>tshow nr')
samePop pop@P_CptPopu{} pop'@P_CptPopu{} = p_cpt pop == p_cpt pop'
samePop _ _ = False

-- | Function `mkArchiContext` defines the P_Context that has been constructed from the ArchiMate repo
mkArchiContext :: [(P_Population,Maybe P_Relation,[PClassify])] -> Guarded P_Context
mkArchiContext pops = pure
  PCtx{ ctx_nm     = "Archimate"
      , ctx_pos    = []
      , ctx_lang   = Just Dutch  -- fatal "No language because of Archi-import hack. Please report this as a bug"
      , ctx_markup = Nothing
      , ctx_pats   = []
      , ctx_rs     = []
      , ctx_ds     = (fmap NE.head . eqCl nameSign) [ ad | Just ad<-archiDecls ]
      , ctx_cs     = []
      , ctx_ks     = []
      , ctx_rrules = []
      , ctx_reprs  = []
      , ctx_vs     = []
      , ctx_gs     = L.nub (concat archiGenss)
      , ctx_ifcs   = []
      , ctx_ps     = []
      , ctx_pops   = sortRelPops archiPops <> sortCptPops archiPops
      , ctx_metas  = []
      }
  where archiPops ::  [P_Population]
        archiDecls :: [Maybe P_Relation]
        archiGenss :: [[PClassify]]
        (archiPops, archiDecls, archiGenss) = L.unzip3 pops
        sortRelPops, sortCptPops :: [P_Population] -> [P_Population] -- assembles P_Populations with the same signature into one
        sortRelPops popus = [ (NE.head cl){p_popps = foldr L.union [] [p_popps decl | decl<-NE.toList cl]} | cl<-eqClass samePop [pop | pop@P_RelPopu{}<-popus] ]
        sortCptPops popus = [ (NE.head cl){p_popas = foldr L.union [] [p_popas cpt  | cpt <-NE.toList cl]} | cl<-eqClass samePop [pop | pop@P_CptPopu{}<-popus] ]
        nameSign decl = (name decl, dec_sign decl)
-- The following code defines a data structure (called ArchiRepo) that corresponds to an Archi-repository in XML.

-- | `data ArchiRepo` represents an entire ArchiMate repository in one Haskell data structure.
data ArchiRepo = ArchiRepo
  { archRepoName   :: Text
  , archRepoId     :: Text
  , archFolders    :: [Folder]
  , archProperties :: [ArchiProp]
  , archPurposes   :: [ArchiPurpose]
  } deriving (Show, Eq)

-- | `data Folder` represents the folder structure of the ArchiMate Tool.
data Folder = Folder
  { fldName        :: Text      -- the name of the folder
  , fldId          :: Text      -- the Archi-id (e.g. "b12f3af5")
  , fldType        :: Text      -- the xsi:type of the folder
  , fldLevel       :: Int       -- the nesting level: 0=top level, 1=subfolder, 2=subsubfolder, etc.
  , fldElems       :: [Element] -- the elements in the current folder, without the subfolders
  , fldFolders     :: [Folder]  -- the subfolders
  , fldViews       :: [View]    -- Archi stores views in a designated folder called "Views"
  } deriving (Show, Eq)

-- | `data Element` represents every ArchiMate element in the ArchiMate repo
data Element = Element
  { elemType       :: Text
  , elemId         :: Text
  , elemName       :: Text
  , elemSrc        :: Text
  , elemTgt        :: Text
  , elemAccTp      :: Text
  , elemDocu       :: Text
  , elChilds       :: [Child]
  , elProps        :: [ArchiProp]
  , elDocus        :: [ArchiDocu]
  } deriving (Show, Eq)

data View = View
  { viewType       :: Text
  , viewId         :: Text
  , viewName       :: Text
  , viewPoint      :: Text
  , viewDocu       :: Text
  , viewChilds     :: [Child]
  , viewProps      :: [ArchiProp]
  , viewDocus      :: [ArchiDocu]
  } deriving (Show, Eq)

-- | Children occur in views only.
-- | We do not analyze all information that is available in views.
data Child = Child
  { chldType       :: Text
  , chldId         :: Text
--, chldAlgn       :: Text
--, chldFCol       :: Text
  , chldElem       :: Text
--, trgtConn       :: Text
--, bound          :: Bound
--, srcConns       :: [SourceConnection]
  , childs         :: [Child]
  } deriving (Show, Eq)

data Relation = Relation
  { relType        :: Text
  , relHref        :: Text
  } deriving (Show, Eq)

data Bound = Bound
  { bnd_x          :: Text
  , bnd_y          :: Text
  , bnd_width      :: Text
  , bnd_height     :: Text
  } deriving (Show, Eq)

data SourceConnection = SrcConn
  { sConType       :: Text
  , sConId         :: Text
  , sConSrc        :: Text
  , sConTgt        :: Text
  , sConRel        :: Text
  , sConRelat      :: [Relation]
  , sCbendPts      :: [BendPoint]
  } deriving (Show, Eq)

data BendPoint = BendPt
  { bpStartX       :: Text
  , bpStartY       :: Text
  , bpEndX         :: Text
  , bpEndY         :: Text
  } deriving (Show, Eq)

data ArchiProp = ArchiProp
  { archPropId     :: Maybe Text
  , archPropKey    :: Text
  , archPropVal    :: Text
  } deriving (Show, Eq)

data ArchiPurpose = ArchiPurpose
  { archPurpVal    :: Text
  } deriving (Show, Eq)

data ArchiDocu = ArchiDocu
  { archDocuVal    :: Text
  } deriving (Show, Eq)

-- | The class WithProperties is defined to generate keys for properties,
--   to be inserted in the grinding process.
--   Properties in Archimate have no identifying key.
--   The only data structures with properties in the inner structure of Archi
--   (i.e. in the repository minus the Views) are folders and elements.
--   In Ampersand, that key is necessary to get objects that represent an Archimate-property.
--   For this reason, the types ArchiRepo, Folder, and Element are instances
--   of class WithProperties.
class WithProperties a where
  allProps      :: a -> [ArchiProp]   -- takes all properties from an ArchiRepo, a Folder, or an Element
  identifyProps :: [Text] -> a -> a -- distributes identifiers ( [Text] ) over an ArchiRepo, a Folder, or an Element, in order to assign a unique identifier to each property in it.

instance WithProperties ArchiRepo where
  allProps archiRepo = allProps (archFolders archiRepo) <> archProperties archiRepo
  identifyProps _ archiRepo
   = archiRepo
      { archProperties = [ prop{archPropId=Just propId} | (prop,propId)<- zip (archProperties archiRepo) propIds ]
      , archFolders    = identifyProps fldrIds (archFolders archiRepo)
      }
     where
      identifiers = map (\i -> "pr-"<>tshow i) [0::Integer ..]
      len = (length.allProps.archFolders) archiRepo
      fldrIds = take len identifiers
      propIds = drop len identifiers

instance WithProperties Folder where
  allProps folder = allProps (fldElems folder) <> allProps (fldFolders folder)
  identifyProps identifiers folder = folder
    { fldElems   = identifyProps elemsIdentifiers (fldElems folder)
    , fldFolders = identifyProps foldersIdentifiers (fldFolders folder)
    }
    where
      elemsIdentifiers   = take ((length.allProps.fldElems) folder) identifiers
      foldersIdentifiers = drop ((length.allProps.fldElems) folder) identifiers

instance WithProperties Element where
  allProps element = elProps element
--                    <> allProps (elChilds element)   -- children are not (yet) being analyzed, so we skip the elChilds of the element.
  identifyProps identifiers element = element
    { elProps = [ prop{archPropId=Just propId} | (propId,prop)<- zip identifiers (elProps element) ] }

instance WithProperties a => WithProperties [a] where
  allProps xs = concatMap allProps xs
  identifyProps identifiers xs
   = [ identifyProps ids x | (ids,x) <- zip idss xs ]
     where
      countProperties :: [Int] -- a list that contains the lengths of property lists in `folder`
      countProperties = map (length.allProps) xs
      idss = distr countProperties identifiers
      distr :: [Int] -> [a] -> [[a]]  -- distribute identifiers in order to allocate them to items in `archiRepo`
      distr (n:ns) idents = take n idents: distr ns (drop n idents)
      distr []     _      = []


-- | In order to populate an Archi-metamodel with the contents of an Archi-repository,
--   we must grind that contents into binary tables. For that purpose, we define the
--   class MetaArchi, and instantiate it on ArchiRepo and all its constituent types.
class MetaArchi a where
  typeMap    :: a -> Map Text Text           -- the map that determines the type (xsi:type) of every atom (id-field) in the repository
  grindArchi :: (Text->Maybe Text) -> a ->   -- create population and the corresponding metamodel for the P-structure in Ampersand
                   [(P_Population, Maybe P_Relation, [PClassify])]
  keyArchi   :: a -> Text                      -- get the key value (dirty identifier) of an a.

instance MetaArchi ArchiRepo where
  typeMap archiRepo
   = typeMap (archFolders archiRepo)  <> 
     (typeMap.archProperties) archiRepo
  grindArchi typeLookup archiRepo
   = [ translateArchiObj "purpose" "ArchiRepo"
        [(keyArchi archiRepo, archPurpVal purp) | purp<-archPurposes archiRepo]
     | (not.null.archPurposes) archiRepo ] <>
     (concat.map (grindArchi typeLookup)) (archFolders archiRepo)  <> 
     (concat.map (grindArchi typeLookup).archProperties) archiRepo
  keyArchi = archRepoId

instance MetaArchi Folder where
  typeMap folder
   = (typeMap.fldElems)   folder <> 
     (typeMap.fldViews)   folder <> 
     (typeMap.fldFolders) folder
  grindArchi typeLookup folder
   = (concat.map (grindArchi typeLookup)               .fldElems)   folder  <> 
     (concat.map (grindArchi typeLookup)               .fldViews)   folder  <> 
     (concat.map (grindArchi typeLookup.insType folder).fldFolders) folder
  keyArchi = fldId

-- | If a folder has a fldType, all subfolders without a type are meant to have the same fldType.
--   For this purpose, the fldType is transported recursively to subfolders.
insType :: Folder -> Folder -> Folder
insType super sub
 = case (fldType super, fldType sub) of
        ("",_)    -> sub
        (ftyp,"") -> sub{fldType=ftyp}
        _         -> sub

-- A type map is constructed for Archi-objects only. Taking relationships into this map brings Archi into higher order logic, and may cause black holes in Haskell. 
instance MetaArchi Element where
  typeMap element
   = Map.fromList [(keyArchi element, elemType element) | (not.T.null.elemName) element, (T.null.elemSrc) element] <>
     typeMap (elProps element)
  grindArchi typeLookup element
   = [ translateArchiObj "name" (elemType element) [(keyArchi element, elemName element)]
     | (not . T.null . elemName) element, (T.null . elemSrc) element] <>
     [ translateArchiObj "docu" (elemType element) [(keyArchi element, elemDocu element)] -- documentation in the XML-tag
     | (not . T.null . elemDocu) element, (T.null . elemSrc) element] <>
     [ translateArchiObj "docu" (elemType element) [(keyArchi element, archDocuVal eldo)] -- documentation with <documentation/> tags.
     | eldo<-elDocus element] <>
     (if isRelationship element then translateArchiRel typeLookup element else [] ) <>
     [ translateArchiObj "accessType" (elemType element) [(keyArchi element, elemAccTp element)]
     | (not . T.null . elemAccTp) element] <>
     [ translateArchiObj "elprop" (elemType element) [(keyArchi prop, keyArchi element)]
     | prop<-elProps element] <>
     (concat.map (grindArchi typeLookup).elProps) element
  keyArchi = elemId

-- | Function `isRelationship` can tell whether this XML-element is an Archimate Relationship.
isRelationship :: Element -> Bool
isRelationship element = (not . T.null . elemSrc) element

-- isView :: Element -> Bool
-- isView element = elemType element == "ArchimateDiagramModel"

instance MetaArchi View where
  typeMap diagram
   = Map.fromList [(keyArchi diagram, viewType diagram) | (not.T.null.viewName) diagram] <>
     typeMap (viewProps diagram)
  grindArchi typeLookup diagram
   = [ translateArchiView "name" (viewType diagram,"Text") [(keyArchi diagram, viewName diagram)]
     | (not . T.null . viewName) diagram] <>
     [ translateArchiView "docu" (viewType diagram,"Text") [(keyArchi diagram, viewDocu diagram)] -- documentation in the XML-tag
     | (not . T.null . viewDocu) diagram] <>
     [ translateArchiView "docu" (viewType diagram,"Text") [(keyArchi diagram, archDocuVal viewdoc)] -- documentation with <documentation/> tags.
     | viewdoc<-viewDocus diagram] <>
     [ translateArchiView "inView" (srcType,viewType diagram) [(chldElem viewelem, keyArchi diagram)] -- register the views in which an element is used.
     | viewelem<-viewChilds diagram, Just srcType<-[typeLookup (chldElem viewelem)]] <>
     [ translateArchiView "viewpoint" (viewType diagram,"ViewPoint") [(keyArchi diagram, viewPoint diagram)] -- documentation with <documentation/> tags.
     | (not . T.null . viewPoint) diagram]
  keyArchi = viewId

instance MetaArchi ArchiProp where
  typeMap _
   = Map.empty
  grindArchi _ property
   = [ translateArchiObj "key" "Property"
         [(keyArchi property, archPropKey property) | (not . T.null . archPropKey) property ]
     , translateArchiObj "value" "Property"
         [(keyArchi property, archPropVal property) | (not . T.null . archPropVal) property ]
     ]
  keyArchi = fromMaybe (error "fatal: No key defined yet") . archPropId

instance MetaArchi a => MetaArchi [a] where
  typeMap               xs = Map.unions [ typeMap               x | x<-xs ]
  grindArchi typeLookup xs = concat [ grindArchi typeLookup x | x<-xs ]
  keyArchi = error "fatal: cannot use keyArchi on a list"

-- | The function `translateArchiObj` does the actual compilation of data objects from archiRepo into the Ampersand structure.
--   It looks redundant to produce both a `P_Population` and a `P_Relation`, but the first contains the population and the second is used to
--   include the metamodel of Archimate in the population. This saves the author the effort of maintaining an Archimate-metamodel.
translateArchiObj :: Text -> Text -> [(Text, Text)] -> (P_Population,Maybe P_Relation,[PClassify])
translateArchiObj "name" typeLabel tuples
 = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "name" (Just (P_Sign (PCpt typeLabel) (PCpt "Text")))) (transTuples tuples)
   , Just $ P_Sgn "name" (P_Sign (PCpt typeLabel) (PCpt "Text")) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
translateArchiObj "docu" typeLabel tuples
 = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "documentation" (Just (P_Sign (PCpt typeLabel) (PCpt "Text")))) (transTuples tuples)
   , Just $ P_Sgn "documentation" (P_Sign (PCpt typeLabel) (PCpt "Text")) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
translateArchiObj "key" "Property" tuples
 = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "key" (Just (P_Sign (PCpt "Property") (PCpt "Text")))) (transTuples tuples)
   , Just $ P_Sgn "key" (P_Sign (PCpt "Property") (PCpt "Text")) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
translateArchiObj "value" "Property" tuples
 = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "value" (Just (P_Sign (PCpt "Property") (PCpt "Text")))) (transTuples tuples)
   , Just $ P_Sgn "value" (P_Sign (PCpt "Property") (PCpt "Text")) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
translateArchiObj "elprop" _ tuples
 = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "propOf" (Just (P_Sign (PCpt "Property") (PCpt "ArchiObject")))) (transTuples tuples)
   , Just $ P_Sgn "propOf" (P_Sign (PCpt "Property") (PCpt "ArchiObject")) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
translateArchiObj "accessType" typeLabel tuples
 = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "accessType" (Just (P_Sign (PCpt typeLabel) (PCpt "AccessType")))) (transTuples tuples)
   , Just $ P_Sgn "accessType" (P_Sign (PCpt typeLabel) (PCpt "AccessType")) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
translateArchiObj a b c = error ("!fatal: non-exhaustive pattern in translateArchiObj\ntranslateArchiObj "<> show a<>" "<>show b<>" "<>show c)

-- | Purpose: To generate relationships from archiRepo as elements the Ampersand P-structure
--   Pre:     isRelationship element  -- this guarantees only that elemSrc is not empty.
translateArchiRel :: (Text -> Maybe Text) -> Element -> [(P_Population, Maybe P_Relation, [PClassify])]
translateArchiRel typeLookup element
 = [ ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown relLabel (Just (P_Sign (PCpt xType) (PCpt yType)))) (transTuples [(x,y)])
     , Just $ P_Sgn relLabel (P_Sign (PCpt xType) (PCpt yType)) (Set.fromList []) [] [] OriginUnknown
     , []
     )
   , ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "source" (Just (P_Sign (PCpt "Relationship") (PCpt "ArchiObject")))) (transTuples [(relId,x)])
     , Just $ P_Sgn "source" (P_Sign (PCpt "Relationship") (PCpt "ArchiObject")) (Set.fromList [Uni]) [] [] OriginUnknown
     , [] -- [ PGen OriginUnknown (PCpt xType) (PCpt "ArchiObject") ]
     )
   , ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "isa" (Just (P_Sign (PCpt xType) (PCpt "ArchiObject")))) (transTuples [(x,x)])
     , Just $ P_Sgn "isa" (P_Sign (PCpt xType) (PCpt "ArchiObject")) (Set.fromList [Uni,Inj]) [] [] OriginUnknown
     , []
     )
   , ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "target" (Just (P_Sign (PCpt "Relationship") (PCpt "ArchiObject")))) (transTuples [(relId,y)])
     , Just $ P_Sgn "target" (P_Sign (PCpt "Relationship") (PCpt "ArchiObject")) (Set.fromList [Uni]) [] [] OriginUnknown
     , [] -- [ PGen OriginUnknown (PCpt yType) (PCpt "ArchiObject") ]
     )
   , ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "isa" (Just (P_Sign (PCpt yType) (PCpt "ArchiObject")))) (transTuples [(y,y)])
     , Just $ P_Sgn "isa" (P_Sign (PCpt yType) (PCpt "ArchiObject")) (Set.fromList [Uni,Inj]) [] [] OriginUnknown
     , []
     )
   ] <>
   [ ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "datatype" (Just (P_Sign (PCpt "Relationship") (PCpt "Text")))) (transTuples [(relId,relLabel)])
     , Just $ P_Sgn "datatype" (P_Sign (PCpt "Relationship") (PCpt "Text")) (Set.fromList [Uni]) [] [] OriginUnknown
     , []
     )
   | xType=="ApplicationComponent" && yType=="ApplicationComponent" ]
     where
       relId    = keyArchi element                        -- the key from Archi, e.g. "693"                 
       relTyp   = (relCase . unFix . elemType) element    -- the relation type,  e.g. "Access"  
       relLabel = case relTyp of
                    "Association"
                      -> if (T.null . elemName) element          
                         then relTyp
                         else relCase (elemName element)  -- the name given by the user, e.g. "create/update"
                    _ -> relTyp
       (x,y)    = (elemSrc element, elemTgt element)
       xType    = case typeLookup x of
                    Just str -> str
                    Nothing -> fatal ("No Archi-object found for Archi-identifier "<>tshow x)
       yType    = case typeLookup y of
                    Just str -> str
                    Nothing -> fatal ("No Archi-object found for Archi-identifier "<>tshow y)

-- | Function `relCase` is used to generate relation identifiers that are syntactically valid in Ampersand.
       relCase :: Text -> Text
       relCase str = case T.uncons str of
         Nothing -> fatal "fatal empty relation identifier."
         Just (c,cs) -> escapeIdentifier . T.cons (toLower c) $ cs
-- | Function `unFix` is used to remove the "Relationship" suffix, which is specific to Archi.
       unFix :: Text -> Text
       unFix str = if "Relationship" `T.isSuffixOf` str
                   then (T.reverse . T.drop 12 . T.reverse) str else str

transTuples :: [(Text, Text)] -> [PAtomPair]
transTuples tuples = 
     [ PPair OriginUnknown (ScriptString OriginUnknown x) (ScriptString OriginUnknown y) 
     | (x,y)<-tuples
--     , (not.T.null) x
--     , (not.T.null) y 
     ]

-- | The function `translateArchiObj` does the actual compilation of data objects from archiRepo into the Ampersand structure.
--   It looks redundant to produce both a `P_Population` and a `P_Relation`, but the first contains the population and the second is used to
--   include the metamodel of Archimate in the population. This saves the author the effort of maintaining an Archimate-metamodel.
translateArchiView :: Text -> (Text,Text) -> [(Text, Text)] -> (P_Population,Maybe P_Relation,[PClassify])
translateArchiView "name" (srcLabel,tgtLabel) tuples
 = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "name" (Just (P_Sign (PCpt srcLabel) (PCpt tgtLabel)))) (transTuples tuples)
   , Just $ P_Sgn "name" (P_Sign (PCpt srcLabel) (PCpt tgtLabel)) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
translateArchiView "docu" (srcLabel,tgtLabel) tuples
 = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "documentation" (Just (P_Sign (PCpt srcLabel) (PCpt tgtLabel)))) (transTuples tuples)
   , Just $ P_Sgn "documentation" (P_Sign (PCpt srcLabel) (PCpt tgtLabel)) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
translateArchiView "inView" (srcLabel,tgtLabel) tuples
 = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "inView" (Just (P_Sign (PCpt srcLabel) (PCpt tgtLabel)))) (transTuples tuples)
   , Just $ P_Sgn "inView" (P_Sign (PCpt srcLabel) (PCpt tgtLabel)) (Set.fromList []) [] [] OriginUnknown, [] )
translateArchiView "viewpoint" (srcLabel,tgtLabel) tuples
 = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "viewpoint" (Just (P_Sign (PCpt srcLabel) (PCpt "ViewPoint")))) (transTuples tuples)
   , Just $ P_Sgn "viewpoint" (P_Sign (PCpt srcLabel) (PCpt tgtLabel)) (Set.fromList []) [] [] OriginUnknown, [] )
translateArchiView a b c = error ("!fatal: non-exhaustive pattern in translateArchiView\ntranslateArchiView "<> show a<>" "<>show b<>" "<>show c)

-- | The function `processStraight` derives an ArchiRepo from an Archi-XML-file.
processStraight :: FilePath -> IOSLA (XIOState s0) XmlTree ArchiRepo
processStraight absFilePath
 = readDocument [ withRemoveWS  yes        -- purge redundant white spaces
                , withCheckNamespaces yes  -- propagates name spaces into QNames
                , withTrace 0]             -- if >0 gives trace messages.
                uri
   >>>
   analArchiRepo
    where
     uri = "file://" <> n (g <$> absFilePath)
       where
         g x = if x == '\\' then '/' else x
         n [] = fatal "absFilePath is an empty list."
         n x@(h:_) = if h /= '/' then '/' : x else x
     analArchiRepo :: ArrowXml a => a XmlTree ArchiRepo
     analArchiRepo
       = (atTag "archimate:model"<+>atTag "archimate:ArchimateModel") >>>
         proc l -> do repoNm'   <- getAttrValue "name"                  -< l
                      repoId'   <- getAttrValue "id"                    -< l
                      purposes' <- listA (getChildren >>> getPurpose)   -< l
                      folders'  <- listA (getChildren >>> getFolder 0)  -< l
                      props'    <- listA (getChildren >>> getProp)      -< l
                      returnA   -< ArchiRepo { archRepoName   = T.pack repoNm'
                                             , archRepoId     = T.pack repoId'
                                             , archFolders    = folders'
                                             , archProperties = [ prop{archPropId=Just $ "pr-"<>tshow i} | (prop,i)<- zip props' [length (allProps folders')..] ]
                                             , archPurposes   = purposes'
                                             }

     getFolder :: ArrowXml a => Int -> a XmlTree Folder
     getFolder level
      = isElem >>> (hasName "folder"<+>hasName "folders") >>>
         proc l -> do fldNm'     <- getAttrValue "name"                 -< l
                      fldId'     <- getAttrValue "id"                   -< l
                      fldType'   <- getAttrValue "type"                 -< l
                      elems'     <- listA (getChildren >>> getElement)  -< l
                      subFlds'   <- listA (getChildren >>> getFolder (level+1)) -< l
                      views'     <- listA (getChildren >>> getView)     -< l
                      returnA    -< Folder { fldName    = T.pack fldNm'
                                           , fldId      = T.pack fldId'
                                           , fldType    = T.pack fldType'
                                           , fldLevel   = level
                                           , fldElems   = [e| e<-elems', elemType e/="ArchimateDiagramModel"]
                                           , fldFolders = subFlds'
                                           , fldViews   = [v| v<-views', viewType v=="ArchimateDiagramModel"]
                                           }

     getView :: ArrowXml a => a XmlTree View
     getView
      = isElem >>> (hasName "element") >>>
         proc l -> do viewNm'    <- getAttrValue "name"              -< l
                      viewId'    <- getAttrValue "id"                -< l
                      viewType'  <- getAttrValue "xsi:type"          -< l
                      viewPoint' <- getAttrValue "viewpoint"         -< l
                      viewChilds'<- listA (getChildren >>> getChild) -< l
                      viewDocu'  <- getAttrValue "documentation"     -< l
                      props'     <- listA (getChildren >>> getProp)  -< l
                      docus'     <- listA (getChildren >>> getDocu)  -< l
                      returnA    -< View { viewName    = T.pack viewNm'
                                         , viewId      = T.pack viewId'
                                         , viewType    = unPrefix (T.pack viewType')
                                         , viewDocu    = T.pack viewDocu'
                                         , viewPoint   = T.pack viewPoint'
                                         , viewChilds  = viewChilds'
                                         , viewProps   = props'
                                         , viewDocus   = docus'
                                         }

     -- | drops the prefix "archimate:", which is specific for Archi types.
     unPrefix :: Text -> Text
     unPrefix str = if "archimate:" `T.isPrefixOf` str
                    then T.drop 10 str else str

     getProp :: ArrowXml a => a XmlTree ArchiProp
     getProp = isElem >>> (hasName "property"<+>hasName "properties") >>>
         proc l -> do propKey    <- getAttrValue "key"   -< l
                      propVal    <- getAttrValue "value" -< l
                      returnA    -< ArchiProp { archPropKey = T.pack propKey
                                              , archPropId  = Nothing -- error "fatal 315: archPropId not yet defined"
                                              , archPropVal = T.pack propVal
                                              }
     getPurpose :: ArrowXml a => a XmlTree ArchiPurpose
     getPurpose = isElem >>> hasName "purpose" >>>
         proc l -> do purpVal    <- text -< l
                      returnA    -< ArchiPurpose { archPurpVal = T.pack purpVal }
     getDocu :: ArrowXml a => a XmlTree ArchiDocu
     getDocu = isElem >>> hasName "documentation" >>>
         proc l -> do docuVal    <- text -< l
                      returnA    -< ArchiDocu { archDocuVal = T.pack docuVal }

     getChild :: ArrowXml a => a XmlTree Child
     getChild
      = atTag "child"<+>atTag "children" >>>     
         proc l -> do chldType'  <- getAttrValue "xsi:type"            -< l
                      chldId'    <- getAttrValue "id"                  -< l
                  --  chldName'  <- getAttrValue "name"                -< l -- defined, but not used.
                  --  chldFCol'  <- getAttrValue "fillColor"           -< l
                  --  chldAlgn'  <- getAttrValue "textAlignment"       -< l
                      chldElem'  <- getAttrValue "archimateElement"    -< l
                  --  trgtConn'  <- getAttrValue "targetConnections"   -< l
                  --  bound'     <- getChildren >>> getBound           -< l
                  --  srcConns'  <- listA (getChildren >>> getSrcConn) -< l
                      childs'    <- listA (getChildren >>> getChild)   -< l
                      returnA    -< Child { chldType = unPrefix (T.pack chldType')
                                          , chldId   = T.pack chldId'
                  --                      , chldAlgn = T.pack chldAlgn'
                  --                      , chldFCol = T.pack chldFCol'
                                          , chldElem = T.pack chldElem'
                  --                      , trgtConn = T.pack trgtConn'
                  --                      , bound    = bound'
                  --                      , srcConns = srcConns'
                                          , childs   = childs'
                                          }

     getElement :: ArrowXml a => a XmlTree Element
     getElement = isElem >>> (hasName "element"<+>hasName "elements") >>>  -- don't use atTag, because recursion is in getFolder.
         proc l -> do elemType'  <- getAttrValue "xsi:type"           -< l
                      elemId'    <- getAttrValue "id"                 -< l
                      elemName'  <- getAttrValue "name"               -< l
                      elemSrc'   <- getAttrValue "source"             -< l
                      elemTgt'   <- getAttrValue "target"             -< l
                      elemAccTp' <- getAttrValue "accessType"         -< l
                      elemDocu'  <- getAttrValue "documentation"      -< l
                      childs'    <- listA (getChildren >>> getChild)  -< l
                      props'     <- listA (getChildren >>> getProp)   -< l
                      docus'     <- listA (getChildren >>> getDocu)   -< l
                      returnA    -< Element { elemType  = unPrefix (T.pack elemType')
                                            , elemId    = T.pack elemId'
                                            , elemName  = T.pack elemName'
                                            , elemSrc   = T.pack elemSrc'
                                            , elemTgt   = T.pack elemTgt'
                                            , elemAccTp = T.pack elemAccTp'
                                            , elemDocu  = T.pack elemDocu'
                                            , elChilds  = childs'
                                            , elProps   = props'
                                            , elDocus   = docus'
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
-- The following does not work yet for recent versions of Archi
-- which should parse with hasName "sourceConnection", but doesn't. TODO
-- However, forget about this after the ArchiMate Exchange Format can be parsed.
     getSrcConn :: ArrowXml a => a XmlTree SourceConnection
     getSrcConn = isElem >>> hasName "sourceConnections" >>>
         proc l -> do sConType'  <- getAttrValue "xsi:type"          -< l
                      sConId'    <- getAttrValue "id"                -< l
                      sConSrc'   <- getAttrValue "source"            -< l
                      sConTgt'   <- getAttrValue "target"            -< l
                      sConRel'   <- getAttrValue "relationship"      -< l
                      sConRelat' <- listA (getChildren>>>getRelation)-< l
                      bendPts'   <- listA (getChildren>>>getBendPt)  -< l
                      returnA    -< SrcConn { sConType  = T.pack sConType'
                                            , sConId    = T.pack sConId'
                                            , sConSrc   = T.pack sConSrc'
                                            , sConTgt   = T.pack sConTgt'
                                            , sConRel   = T.pack sConRel'
                                            , sConRelat = sConRelat'
                                            , sCbendPts = bendPts'
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
atTag :: ArrowXml a => Text -> a (NTree XNode) XmlTree
atTag tag = deep (isElem >>> hasName (T.unpack tag))

text :: ArrowXml a => a (NTree XNode) String
text = getChildren >>> getText
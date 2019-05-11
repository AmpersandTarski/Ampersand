{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings, DuplicateRecordFields #-}
module Ampersand.Input.Archi.ArchiAnalyze (archi2PContext)
   -- The purpose of this module is to load Archimate content into an Ampersand context.
   -- This module parses an Archi-repository by means of function `archi2PContext`, which produces a `P_Context` for merging into Ampersand.
   -- That `P_Context` contains both the Archimate-metamodel (in the form of declarations) and the Archimate population that represents the model.
   -- In this way, `archi2PContext ` deals with the fact that Archimate produces a mix of model and metamodel.
where
   import Ampersand.Basics  -- for things such as fatal, eqClass
   import Data.Char                                                    -- for things such as toLower
   import qualified Data.Map.Strict as Map -- import qualified, to avoid name clashes with Prelude functions
   import Data.Tree.NTree.TypeDefs
   import Text.XML.HXT.Core hiding (utf8, fatal,trace)
   import Ampersand.Core.ParseTree
   import qualified RIO.List as L
   import qualified Data.Set as Set
   import qualified Data.List.NonEmpty as NEL

   -- | Function `archi2PContext` is meant to grind the contents of an Archi-repository into declarations and population inside a fresh Ampersand P_Context.
   --   The process starts by parsing an XML-file by means of function `processStraight` into a data structure called `archiRepo`. This function uses arrow-logic from the HXT-module.
   --   The resulting data structure contains the folder structure of the tool Archi (https://github.com/archimatetool/archi) and represents the model-elements and their properties.
   --   A lookup-function, `elemLookup`,is derived from `archiRepo`.
   --   It assigns the Archi-type (e.g. Business Process) to the identifier of an arbitrary Archi-object (e.g. "0957873").
   --   Then, the properties have to be provided with identifiers (see class `WithProperties`), because Archi represents them just as key-value pairs.
   --   The function `grindArchi` retrieves the population of meta-relations
   --   It produces the P_Populations and P_Declarations that represent the Archimate model.
   --   Finally, the function `mkArchiContext` produces a `P_Context` ready to be merged into the rest of Ampersand's population.
   archi2PContext :: String -> IO P_Context
   archi2PContext archiRepoFilename  -- e.g. "CA repository.xml"
    = do -- hSetEncoding stdout utf8
         archiRepo <- runX (processStraight archiRepoFilename)
         let fst3 (x,_,_) = x
         let elemLookup atom = (Map.lookup atom . Map.fromList . typeMap) archiRepo
         let archiRepoWithProps = (grindArchi elemLookup.identifyProps []) archiRepo
         let relPops = (filter (not.null.p_popps) . sortRelPops . map fst3) archiRepoWithProps
         let cptPops = (filter (not.null.p_popas) . sortCptPops . map fst3) archiRepoWithProps
         let elemCount archiConcept = (Map.lookup archiConcept . Map.fromList . atomCount . atomMap) relPops
         let countPop pop = let signature = ((\(Just sgn)->sgn).p_mbSign.p_nmdr) pop in
                            (show.length.p_popps) pop              ++"\t"++
                            (p_nrnm.p_nmdr) pop                    ++"\t"++
                            (p_cptnm.pSrc) signature               ++"\t"++
                            (show.length.eqCl ppLeft.p_popps) pop  ++"\t"++
                            (showMaybeInt.elemCount.pSrc) signature++"\t"++
                            (p_cptnm.pTgt) signature               ++"\t"++
                            (show.length.eqCl ppRight.p_popps) pop ++"\t"++
                            (showMaybeInt.elemCount.pTgt) signature
         writeFile "ArchiCount.txt"
          (   (L.intercalate "\n" . map countPop) relPops
           <> (concat . map showArchiElems . atomCount . atomMap ) (relPops++cptPops ) 
          )
         putStrLn ("ArchiCount.txt written")
         return (mkArchiContext archiRepoWithProps)
{- reminder:
data P_Population
  = P_RelPopu { p_src   :: Maybe String -- a separate src and tgt instead of "Maybe Sign", such that it is possible to specify only one of these.
              , p_tgt   :: Maybe String -- these src and tgt must be more specific than the P_NamedRel
              , p_orig  :: Origin       -- the origin
              , p_nmdr  :: P_NamedRel   -- the named relation
              , p_popps :: [PAtomPair]  -- the contents
              }
  | P_CptPopu { p_orig  :: Origin  -- the origin
              , p_cnme  :: String  -- the name of a concept
              , p_popas :: [PAtomValue]  -- atoms in the initial population of that concept
              }
data P_NamedRel = PNamedRel { p_nrpos :: Origin, p_nrnm :: String, p_mbSign :: Maybe P_Sign }

data P_Sign = P_Sign {pSrc :: P_Concept, pTgt :: P_Concept } deriving (Ord,Eq)
data PAtomPair
  = PPair { pppos :: Origin
          , ppLeft  :: PAtomValue
          , ppRight :: PAtomValue
          } deriving (Eq,Ord,Show) -- Show is for QuickCheck error messages and/or input redundancy removal only!
-}
      where sortRelPops, sortCptPops :: [P_Population] -> [P_Population] -- assembles P_Populations with the same signature into one
            sortRelPops pops = [ (NEL.head cl){p_popps = foldr L.union [] [p_popps decl | decl<-NEL.toList cl]} | cl<-eqClass samePop [pop | pop@P_RelPopu{}<-pops] ]
            sortCptPops pops = [ (NEL.head cl){p_popas = foldr L.union [] [p_popas cpt  | cpt <-NEL.toList cl]} | cl<-eqClass samePop [pop | pop@P_CptPopu{}<-pops] ]
            atomMap :: [P_Population] -> Map.Map P_Concept [PAtomValue]
            atomMap pops = Map.fromListWith L.union
                              ([ (pSrc sgn, (L.nub.map ppLeft.p_popps) pop) | pop@P_RelPopu{}<-pops, Just sgn<-[(p_mbSign.p_nmdr) pop] ]++
                               [ (pTgt sgn, (L.nub.map ppRight.p_popps) pop) | pop@P_RelPopu{}<-pops, Just sgn<-[(p_mbSign.p_nmdr) pop] ]++
                               [ ((PCpt . p_cnme) pop, (L.nub.p_popas) pop) | pop@P_CptPopu{}<-pops ]
                              )
            atomCount :: Map.Map c [a] -> [(c,Int)]
            atomCount am = [ (archiElem,length atoms) | (archiElem,atoms)<-Map.toList am ]
            showMaybeInt (Just n) = show n
            showMaybeInt Nothing = "Err"
            showArchiElems :: (P_Concept,Int) -> String
            showArchiElems (c,n) = "\n"++p_cptnm c++"\t"++show n

   samePop :: P_Population -> P_Population -> Bool
   samePop pop@P_RelPopu{} pop'@P_RelPopu{}
    = same (p_nmdr pop) (p_nmdr pop')
      where same nr nr'
             = case (p_mbSign nr, p_mbSign nr') of
                    (Just sgn,    Just sgn') -> p_nrnm nr==p_nrnm nr' && sgn==sgn'
                    _                        -> fatal ("Cannot compare partially defined populations of\n"++show nr++" and\n"++show nr')
   samePop pop@P_CptPopu{} pop'@P_CptPopu{} = p_cnme pop == p_cnme pop'
   samePop _ _ = False

   mkArchiContext :: [(P_Population,Maybe P_Relation,[PClassify])] -> P_Context
   mkArchiContext pops =
     PCtx{ ctx_nm     = "Archimate"
         , ctx_pos    = []
         , ctx_lang   = Just Dutch  -- fatal "No language because of Archi-import hack. Please report this as a bug"
         , ctx_markup = Nothing
         , ctx_pats   = []
         , ctx_rs     = []
         , ctx_ds     = L.nub [ ad | Just ad<-archiDecls ]
         , ctx_cs     = []
         , ctx_ks     = []
         , ctx_rrules = []
         , ctx_rrels  = []
         , ctx_reprs  = []
         , ctx_vs     = []
         , ctx_gs     = L.nub (concat archiGenss)
         , ctx_ifcs   = []
         , ctx_ps     = []
         , ctx_pops   = sortRelPops archiPops ++ sortCptPops archiPops
         , ctx_metas  = []
         }
     where archiPops ::  [P_Population]
           archiDecls :: [Maybe P_Relation]
           archiGenss :: [[PClassify]]
           (archiPops, archiDecls, archiGenss) = L.unzip3 pops
           sortRelPops, sortCptPops :: [P_Population] -> [P_Population] -- assembles P_Populations with the same signature into one
           sortRelPops popus = [ (NEL.head cl){p_popps = foldr L.union [] [p_popps decl | decl<-NEL.toList cl]} | cl<-eqClass samePop [pop | pop@P_RelPopu{}<-popus] ]
           sortCptPops popus = [ (NEL.head cl){p_popas = foldr L.union [] [p_popas cpt  | cpt <-NEL.toList cl]} | cl<-eqClass samePop [pop | pop@P_CptPopu{}<-popus] ]

-- The following code defines a data structure (called ArchiRepo) that corresponds to an Archi-repository in XML.

   data ArchiRepo = ArchiRepo
     { archRepoName   :: String
     , archRepoId     :: String
     , archFolders    :: [Folder]
     , archProperties :: [ArchiProp]
     , archPurposes   :: [ArchiPurpose]
     } deriving (Show, Eq)
 
   data Folder = Folder
     { fldName        :: String    -- the name of the folder
     , fldId          :: String    -- the Archi-id (e.g. "b12f3af5")
     , fldType        :: String    -- the xsi:type of the folder
     , fldLevel       :: Int       -- the nesting level: 0=top level, 1=subfolder, 2=subsubfolder, etc.
     , fldElems       :: [Element] -- the elements in the current folder, without the subfolders
     , fldFolders     :: [Folder]  -- the subfolders
     } deriving (Show, Eq)

   data Element = Element
     { elemType       :: String
     , elemId         :: String
     , elemName       :: String
     , elemSrc        :: String
     , elemTgt        :: String
     , elemAccTp      :: String
     , elemDocu       :: String
     , elChilds       :: [Child]
     , elProps        :: [ArchiProp]
     , elDocus        :: [ArchiDocu]
     } deriving (Show, Eq)

-- Children occur in views only.
   data Child = Child
     { chldType       :: String
     , chldId         :: String
     , chldAlgn       :: String
     , chldFCol       :: String
     , chldElem       :: String
     , trgtConn       :: String
     , bound          :: Bound
     , srcConns       :: [SourceConnection]
     , childs         :: [Child]
     } deriving (Show, Eq)

   data Relation = Relation
     { relType        :: String
     , relHref        :: String
     } deriving (Show, Eq)

   data Bound = Bound
     { bnd_x          :: String
     , bnd_y          :: String
     , bnd_width      :: String
     , bnd_height     :: String
     } deriving (Show, Eq)

   data SourceConnection = SrcConn
     { sConType       :: String
     , sConId         :: String
     , sConSrc        :: String
     , sConTgt        :: String
     , sConRel        :: String
     , sConRelat      :: [Relation]
     , sCbendPts      :: [BendPoint]
     } deriving (Show, Eq)

   data BendPoint = BendPt
     { bpStartX       :: String
     , bpStartY       :: String
     , bpEndX         :: String
     , bpEndY         :: String
     } deriving (Show, Eq)

   data ArchiProp = ArchiProp
     { archPropId     :: Maybe String
     , archPropKey    :: String
     , archPropVal    :: String
     } deriving (Show, Eq)

   data ArchiPurpose = ArchiPurpose
     { archPurpVal    :: String
     } deriving (Show, Eq)

   data ArchiDocu = ArchiDocu
     { archDocuVal    :: String
     } deriving (Show, Eq)


-- | Properties in Archimate have no identifying key. In Ampersand, that key is necessary to get objects that represent an Archimate-property.
--   So the class WithProperties is defined to generate keys for properties, to be inserted in the grinding process.
--   The only data structures with properties in the inner structure of Archi (i.e. in the repository minus the Views),
--   are folders and elements.
--   For this reason, the types ArchiRepo, Folder, and Element are instances of class WithProperties.

   class WithProperties a where
     allProps      :: a -> [ArchiProp]   -- takes all properties from an ArchiRepo, a Folder, or an Element
     identifyProps :: [String] -> a -> a -- distributes identifiers ( [String] ) over an ArchiRepo, a Folder, or an Element, in order to assign a unique identifier to each property in it.

   instance WithProperties ArchiRepo where
     allProps archiRepo = allProps (archFolders archiRepo) ++ archProperties archiRepo
     identifyProps _ archiRepo
      = archiRepo
         { archProperties = [ prop{archPropId=Just propId} | (prop,propId)<- zip (archProperties archiRepo) propIds ]
         , archFolders    = identifyProps fldrIds (archFolders archiRepo)
         }
        where
         identifiers = [ "pr-"++show (i::Integer) | i<-[0..] ] -- infinitely many unique keys to identify properties.
         len = (length.allProps.archFolders) archiRepo
         fldrIds = take len identifiers
         propIds = drop len identifiers

   instance WithProperties Folder where
     allProps folder = allProps (fldElems folder) ++ allProps (fldFolders folder)
     identifyProps identifiers folder = folder
       { fldElems   = identifyProps elemsIdentifiers (fldElems folder)
       , fldFolders = identifyProps foldersIdentifiers (fldFolders folder)
       }
       where
         elemsIdentifiers   = take ((length.allProps.fldElems) folder) identifiers
         foldersIdentifiers = drop ((length.allProps.fldElems) folder) identifiers

   instance WithProperties Element where
     allProps element = elProps element
--                      ++ allProps (elChilds element)   -- children are not (yet) being analyzed, so we skip the elChilds of the element.
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
     typeMap    :: a -> [(String,String)]           -- the map that determines the type (xsi:type) of every atom (id-field) in the repository
     grindArchi :: (String->Maybe String) -> a ->   -- create population and the corresponding metamodel for the P-structure in Ampersand
                      [(P_Population, Maybe P_Relation, [PClassify])]
     keyArchi   :: a -> String                      -- get the key value (dirty identifier) of an a.

{- I'm not quite sure what the purpose of having eqAsy was in the first place...
   instance Flippable P_Relation where
     flp decl
      = decl{ dec_sign =      flp  (dec_sign decl)
            , dec_prps = (map flp) (dec_prps decl)
            , dec_popu = (map flp) (dec_popu decl)
            }

   instance Flippable P_Population where
     flp pop
      = case pop of
         P_RelPopu{} -> pop { p_src = p_tgt pop
                            , p_tgt = p_src pop
                            , p_nmdr = flp (p_nmdr pop)
                            , p_popps = (map flp) (p_popps pop)
                            }

   instance Flippable P_NamedRel where
     flp rel = rel{p_mbSign = flp (p_mbSign rel)}

   eqAsy :: [(P_Population,  Maybe P_Relation, [PClassify])] -> [(P_Population, Maybe P_Relation, [PClassify])]
   eqAsy [] = []
   eqAsy ((pPop, maybeDecl, pgens): rest)
    = ( foldr1 squash
        ( (pPop, maybeDecl, pgens) : [ (flp pPop', flp maybeDecl', pgens')
                                     | (pPop'@P_RelPopu{}, maybeDecl', pgens')<-rest
                                     , name pPop'==name pPop, p_src pPop'==p_tgt pPop, p_tgt pPop'==p_src pPop, flp maybeDecl'==maybeDecl
                                     ] ) )
      : eqAsy [ (pPop', maybeDecl', pgens')
              | (pPop'@P_RelPopu{}, maybeDecl', pgens')<-rest, name pPop'/=name pPop||p_src pPop'/=p_tgt pPop||p_tgt pPop'/=p_src pPop||flp maybeDecl'/=maybeDecl ]
      where
       squash :: (P_Population, Maybe P_Relation, [PClassify]) ->
                 (P_Population, Maybe P_Relation, [PClassify]) ->
                 (P_Population, Maybe P_Relation, [PClassify])
       (pPp@P_RelPopu{}, mDecl, pgns) `squash` (pPp'@P_RelPopu{}, mDecl', pgns')
        = ( pPp{p_popps = p_popps pPp `union` p_popps pPp'}
          , case (mDecl, mDecl') of
                 (Just decl, Just _)     -> Just (decl {dec_nm = dec_nm decl++"!"})
                 (Just decl, Nothing)    -> Just (decl {dec_nm = dec_nm decl++"!"})
                 (Nothing,   Just decl') -> Just (decl'{dec_nm = dec_nm decl'++"!"})
                 _                       -> Nothing
          , pgns `union` pgns'
          )
       (pPp@P_CptPopu{}, mDecl, pgns) `squash` (pPp'@P_CptPopu{}, mDecl', pgns')
        = ( pPp{p_popas = p_popas pPp `union` p_popas pPp'}
          , case (mDecl, mDecl') of
                 (Just decl, Just _)     -> Just (decl {dec_nm = dec_nm decl++"!"})
                 (Just decl, Nothing)    -> Just (decl {dec_nm = dec_nm decl++"!"})
                 (Nothing,   Just decl') -> Just (decl'{dec_nm = dec_nm decl'++"!"})
                 _                       -> Nothing
          , pgns `union` pgns'
          )
       _ `squash` _ = fatal "error in squash"
-}

   instance MetaArchi ArchiRepo where
     typeMap archiRepo
      = typeMap [ folder | folder<-archFolders archiRepo, fldName folder/="Views"]  ++ 
        (typeMap.archProperties) archiRepo
     grindArchi elemLookup archiRepo
      = [ translateArchiObj "purpose" "ArchiRepo"
           [(keyArchi archiRepo, archPurpVal purp) | purp<-archPurposes archiRepo]
        | (not.null.archPurposes) archiRepo ] ++
        (concat.map (grindArchi elemLookup)) backendFolders  ++ 
        (concat.map (grindArchi elemLookup).archProperties) archiRepo
        where backendFolders = [ folder | folder<-archFolders archiRepo, fldName folder/="Views"]
     keyArchi = archRepoId

   instance MetaArchi Folder where
     typeMap folder
      = (typeMap.fldElems)   folder ++ 
        (typeMap.fldFolders) folder
     grindArchi elemLookup folder
      = [ translateArchiObj "folderName" "ArchiFolder" [(keyArchi folder, fldName folder)]] ++
        [ translateArchiObj "type" "ArchiFolder" [(keyArchi folder, fldType folder)]] ++
        [ translateArchiObj "level" "ArchiFolder" [(keyArchi folder, (show.fldLevel) folder)]] ++
        [ translateArchiObj "sub"  "ArchiFolder"
           [(keyArchi subFolder, keyArchi folder) | subFolder<-fldFolders folder]
        | (not.null.fldFolders) folder ] ++
        [ translateArchiObj "in"   "ArchiFolder"
           [(keyArchi element, keyArchi folder) | element<-fldElems folder]
        | (not.null.fldElems) folder ] ++
        [ translateArchiObj "cat" (elemType element) [(keyArchi element, fldName folder)]
        | fldLevel folder>1, element<-fldElems folder] ++
        [ translateArchiObj "archiLayer" (elemType element)
           [(keyArchi element, fldType folder)]
        | element<-fldElems folder] ++
        [ translateArchiObj "inFolder" (fldName folder)
           [(keyArchi element, fldName folder)]
        | element<-fldElems folder] ++
        (concat.map (grindArchi elemLookup)               .fldElems)   folder  ++ 
        (concat.map (grindArchi elemLookup.insType folder).fldFolders) folder
     keyArchi = fldId


-- | If a folder has a fldType, all subfolders without a type are meant to have the same fldType.
--   For this purpose, the fldType is transported recursively to subfolders.
   insType :: Folder -> Folder -> Folder
   insType super sub
    = case (fldType super, fldType sub) of
           ("",_)    -> sub
           (ftyp,"") -> sub{fldType=ftyp}
           _         -> sub

   instance MetaArchi Element where
-- A type map is constructed for Archi-objects only. Taking relationships into this map brings Archi into higher order logic, and may cause black holes in Haskell. 
     typeMap element
      = [(keyArchi element, elemType element) | (not.null.elemName) element, (null.elemSrc) element] ++
        typeMap (elProps element)
     grindArchi elemLookup element
      = [ translateArchiObj "name" (elemType element) [(keyArchi element, elemName element)]
        | (not.null.elemName) element, (null.elemSrc) element] ++
        [ translateArchiObj "docu" (elemType element) [(keyArchi element, elemDocu element)] -- documentation in the XML-tag
        | (not.null.elemDocu) element, (null.elemSrc) element] ++
        [ translateArchiObj "docu" (elemType element) [(keyArchi element, archDocuVal eldo)] -- documentation with <documentation/> tags.
        | eldo<-elDocus element] ++
        (if isRelationship element then translateArchiRel elemLookup element else [] ) ++
        [ translateArchiObj "accessType" (elemType element) [(keyArchi element, elemAccTp element)]
        | (not.null.elemAccTp) element] ++
        [ translateArchiObj "elprop" (elemType element) [(keyArchi prop, keyArchi element)]
        | prop<-elProps element] ++
        (concat.map (grindArchi elemLookup).elProps) element
     keyArchi = elemId

   isRelationship :: Element -> Bool  -- figure out whether this XML-element is an Archimate Relationship.
   isRelationship element = (not.null.elemSrc) element

   instance MetaArchi ArchiProp where
     typeMap _
      = []
     grindArchi _ property
      = [ translateArchiObj "key" "Property"
            [(keyArchi property, archPropKey property) | (not.null.archPropKey) property ]
        , translateArchiObj "value" "Property"
            [(keyArchi property, archPropVal property) | (not.null.archPropVal) property ]
        ]
     keyArchi = fromMaybe (error "fatal: No key defined yet") . archPropId

   instance MetaArchi a => MetaArchi [a] where
     typeMap               xs = concat [ typeMap                  x | x<-xs ]
     grindArchi elemLookup xs = concat [ grindArchi elemLookup x | x<-xs ]
     keyArchi = error "fatal: cannot use keyArchi on a list"

-- | The function `translateArchiObj` does the actual compilation of data objects from archiRepo into the Ampersand structure.
--   It looks redundant to produce both a `P_Population` and a `P_Relation`, but the first contains the population and the second is used to
--   include the metamodel of Archimate in the population. This saves the author the effort of maintaining an Archimate-metamodel.
   translateArchiObj :: String -> String -> [(String, String)] -> (P_Population,Maybe P_Relation,[PClassify])
   translateArchiObj "purpose" "ArchiRepo" tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "purpose" (Just (P_Sign (PCpt "ArchiFolder") (PCpt "Tekst")))) (transTuples tuples)
      , Just $ P_Sgn "purpose" (P_Sign (PCpt "ArchiFolder") (PCpt "Tekst")) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
   translateArchiObj "folderName" _ tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "naam" (Just (P_Sign (PCpt "ArchiFolder") (PCpt "FolderName")))) (transTuples tuples)
      , Just $ P_Sgn "naam" (P_Sign (PCpt "ArchiFolder") (PCpt "FolderName")) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
   translateArchiObj "name" typeLabel tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "naam" (Just (P_Sign (PCpt typeLabel) (PCpt "Tekst")))) (transTuples tuples)
      , Just $ P_Sgn "naam" (P_Sign (PCpt typeLabel) (PCpt "Tekst")) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
   translateArchiObj "type" typeLabel tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "type" (Just (P_Sign (PCpt typeLabel) (PCpt "Tekst")))) (transTuples tuples)
      , Just $ P_Sgn "type" (P_Sign (PCpt typeLabel) (PCpt "Tekst")) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
   translateArchiObj "level" _ tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "level" (Just (P_Sign (PCpt "ArchiFolder") (PCpt "Tekst")))) (transTuples tuples)
      , Just $ P_Sgn "level" (P_Sign (PCpt "ArchiFolder") (PCpt "Tekst")) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
   translateArchiObj "sub" typeLabel tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "sub" (Just (P_Sign (PCpt typeLabel) (PCpt typeLabel)))) (transTuples tuples)
      , Just $ P_Sgn "sub" (P_Sign (PCpt typeLabel) (PCpt typeLabel)) (Set.fromList []) [] [] OriginUnknown, [] )
   translateArchiObj "in" _ tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "folder" (Just (P_Sign (PCpt "ArchiObject") (PCpt "ArchiFolder")))) (transTuples tuples)
      , Just $ P_Sgn "folder" (P_Sign (PCpt "ArchiObject") (PCpt "ArchiFolder")) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
   translateArchiObj "cat" typeLabel tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "inFolderName" (Just (P_Sign (PCpt typeLabel) (PCpt "FolderName")))) (transTuples tuples)
      , Just $ P_Sgn "inFolderName" (P_Sign (PCpt typeLabel) (PCpt "FolderName")) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
   translateArchiObj "docu" typeLabel tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "documentatie" (Just (P_Sign (PCpt typeLabel) (PCpt "Tekst")))) (transTuples tuples)
      , Just $ P_Sgn "documentatie" (P_Sign (PCpt typeLabel) (PCpt "Tekst")) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
   translateArchiObj "inFolder" typeLabel tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "inFolder" (Just (P_Sign (PCpt typeLabel) (PCpt "ArchiObject")))) (transTuples tuples)
      , Just $ P_Sgn "inFolder" (P_Sign (PCpt typeLabel) (PCpt "ArchiObject")) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
   translateArchiObj "key" "Property" tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "key" (Just (P_Sign (PCpt "Property") (PCpt "Tekst")))) (transTuples tuples)
      , Just $ P_Sgn "key" (P_Sign (PCpt "Property") (PCpt "Tekst")) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
   translateArchiObj "value" "Property" tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "value" (Just (P_Sign (PCpt "Property") (PCpt "Tekst")))) (transTuples tuples)
      , Just $ P_Sgn "value" (P_Sign (PCpt "Property") (PCpt "Tekst")) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
   translateArchiObj "elprop" _ tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "propOf" (Just (P_Sign (PCpt "Property") (PCpt "ArchiObject")))) (transTuples tuples)
      , Just $ P_Sgn "propOf" (P_Sign (PCpt "Property") (PCpt "ArchiObject")) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
   translateArchiObj "archiLayer" typeLabel tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "archiLayer" (Just (P_Sign (PCpt typeLabel) (PCpt "ArchiLayer")))) (transTuples tuples)
      , Just $ P_Sgn "archiLayer" (P_Sign (PCpt typeLabel) (PCpt "ArchiLayer")) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
   translateArchiObj "accessType" typeLabel tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "accessType" (Just (P_Sign (PCpt typeLabel) (PCpt "AccessType")))) (transTuples tuples)
      , Just $ P_Sgn "accessType" (P_Sign (PCpt typeLabel) (PCpt "AccessType")) (Set.fromList [Uni]) [] [] OriginUnknown, [] )
   translateArchiObj a b c = error ("!fatal: non-exhaustive pattern in translateArchiObj\ntranslateArchiObj "++ show a++" "++show b++" "++show c)


-- | Purpose: To generate relationships from archiRepo as elements the Ampersand P-structure
-- | Pre:     isRelationship element
   translateArchiRel :: (String -> Maybe String) -> Element -> [(P_Population, Maybe P_Relation, [PClassify])]
   translateArchiRel elemLookup element
    = [ ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown relNm (Just (P_Sign (PCpt xType) (PCpt yType)))) (transTuples [(x,y)])
        , Just $ P_Sgn relNm (P_Sign (PCpt xType) (PCpt yType)) (Set.fromList []) [] [] OriginUnknown
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
      ] ++
      [ (   P_CptPopu { pos     = OriginUnknown
                      , p_cnme  = relTyp 
                      , p_popas = [ScriptString OriginUnknown relId] 
                      }
        , Nothing
        , [ PClassify
                { pos     = OriginUnknown
                , specific = PCpt relTyp           -- ^ specific concept
                , generics = NEL.fromList [PCpt "Relationship"]   -- ^ generic concepts
                } ]
        )
      | relTyp/="Relationship" ] ++
      [ ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "datatype" (Just (P_Sign (PCpt "Relationship") (PCpt "Tekst")))) (transTuples [(relId,relLabel)])
        , Just $ P_Sgn "datatype" (P_Sign (PCpt "Relationship") (PCpt "Tekst")) (Set.fromList [Uni]) [] [] OriginUnknown
        , []
        )
      | xType=="ApplicationComponent" && yType=="ApplicationComponent" ]
        where
          relId    = keyArchi element                 -- the key from Archi, e.g. "693"                 
          relTyp   = elemType element                 -- the relation type,  e.g. "AccessRelationship"  
          relLabel = if (null.elemName) element          
                     then unfixRel (elemType element)
                     else relCase (elemName element)  -- the name given by the user, e.g. "create/update"
          (x,y)    = (elemSrc element, elemTgt element)
          xType    = case elemLookup x of
                       Just str -> str
                       Nothing -> fatal ("No Archi-object found for Archi-identifier "++show x)
          yType    = case elemLookup y of
                       Just str -> str
                       Nothing -> fatal ("No Archi-object found for Archi-identifier "++show y)
          relNm    = relCase relLabel  --  ++"["++xType++"*"++yType++"]"

   unfixRel :: String -> String
   unfixRel cs = (reverse.drop 1.dropWhile (/='R').reverse.relCase) cs
   relCase :: String -> String
   relCase (c:cs) = (toLower c): cs
   relCase "" = error "fatal empty relation identifier."
   transTuples :: [(String, String)] -> [PAtomPair]
   transTuples tuples = [ PPair OriginUnknown (ScriptString OriginUnknown x) (ScriptString OriginUnknown y) | (x,y)<-tuples, (not.null) x, (not.null) y ]

-- The function `processStraight` derives an ArchiRepo from an Archi-XML-file.
   processStraight :: String -> IOSLA (XIOState s0) XmlTree ArchiRepo
   processStraight absFilePath
    = readDocument [ withRemoveWS  yes        -- purge redundant white spaces
                   , withCheckNamespaces yes  -- propagates name spaces into QNames
                   , withTrace 0]             -- if >0 gives trace messages.
                   uri
      >>>
      analArchiRepo
       where
        uri = "file://" ++ n (g <$> absFilePath)
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
                         returnA   -< ArchiRepo { archRepoName   = repoNm'
                                                , archRepoId     = repoId'
                                                , archFolders    = folders'
                                                , archProperties = [ prop{archPropId=Just $ "pr-"++show i} | (prop,i)<- zip props' [length (allProps folders')..] ]
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
                         returnA    -< Folder { fldName    = fldNm'
                                              , fldId      = fldId'
                                              , fldType    = fldType'
                                              , fldLevel   = level
                                              , fldElems   = elems'
                                              , fldFolders = subFlds'
                                              }

        getProp :: ArrowXml a => a XmlTree ArchiProp
        getProp = isElem >>> (hasName "property"<+>hasName "properties") >>>
            proc l -> do propKey    <- getAttrValue "key"   -< l
                         propVal    <- getAttrValue "value" -< l
                         returnA    -< ArchiProp { archPropKey = propKey
                                                 , archPropId  = Nothing -- error "fatal 315: archPropId not yet defined"
                                                 , archPropVal = propVal
                                                 }

        getPurpose :: ArrowXml a => a XmlTree ArchiPurpose
        getPurpose = isElem >>> hasName "purpose" >>>
            proc l -> do purpVal    <- text -< l
                         returnA    -< ArchiPurpose { archPurpVal = purpVal }

        getDocu :: ArrowXml a => a XmlTree ArchiDocu
        getDocu = isElem >>> hasName "documentation" >>>
            proc l -> do docuVal    <- text -< l
                         returnA    -< ArchiDocu { archDocuVal = docuVal }

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
                         returnA    -< Element { elemType  = drop 1 (dropWhile (/=':') elemType')  -- drop the prefix "archimate:"
                                               , elemId    = elemId'
                                               , elemName  = elemName'
                                               , elemSrc   = elemSrc'
                                               , elemTgt   = elemTgt'
                                               , elemAccTp = elemAccTp'
                                               , elemDocu  = elemDocu'
                                               , elChilds  = childs'
                                               , elProps   = props'
                                               , elDocus   = docus'
                                               }
                                  
        getRelation :: ArrowXml a => a XmlTree Relation
        getRelation = isElem >>> hasName "relationship" >>>
            proc l -> do relType'   <- getAttrValue "xsi:type"          -< l
                         relHref'   <- getAttrValue "href"              -< l
                         returnA    -< Relation{ relType = relType'
                                               , relHref = relHref'
                                               }

        getBound :: ArrowXml a => a XmlTree Bound
        getBound = isElem >>> hasName "bounds" >>>
            proc l -> do bnd_x'     <- getAttrValue "x"                 -< l
                         bnd_y'     <- getAttrValue "y"                 -< l
                         bndWidth'  <- getAttrValue "width"             -< l
                         bndHeight' <- getAttrValue "height"            -< l
                         returnA    -< Bound   { bnd_x      = bnd_x'
                                               , bnd_y      = bnd_y'
                                               , bnd_width  = bndWidth'
                                               , bnd_height = bndHeight'
                                               }

        getSrcConn :: ArrowXml a => a XmlTree SourceConnection
        getSrcConn = isElem >>> hasName "sourceConnections" >>>
            proc l -> do sConType'  <- getAttrValue "xsi:type"          -< l
                         sConId'    <- getAttrValue "id"                -< l
                         sConSrc'   <- getAttrValue "source"            -< l
                         sConTgt'   <- getAttrValue "target"            -< l
                         sConRel'   <- getAttrValue "relationship"      -< l
                         sConRelat' <- listA (getChildren>>>getRelation)-< l
                         bendPts'   <- listA (getChildren>>>getBendPt)  -< l
                         returnA    -< SrcConn { sConType  = sConType'
                                               , sConId    = sConId'
                                               , sConSrc   = sConSrc'
                                               , sConTgt   = sConTgt'
                                               , sConRel   = sConRel'
                                               , sConRelat = sConRelat'
                                               , sCbendPts = bendPts'
                                               }

        getBendPt :: ArrowXml a => a XmlTree BendPoint
        getBendPt = isElem >>> hasName "bendpoints" >>>
            proc l -> do bpStartX'  <- getAttrValue "startX"              -< l
                         bpStartY'  <- getAttrValue "startY"              -< l
                         bpEndX'    <- getAttrValue "endX"                -< l
                         bpEndY'    <- getAttrValue "endY"                -< l
                         returnA    -< BendPt  { bpStartX  = bpStartX'
                                               , bpStartY  = bpStartY'
                                               , bpEndX    = bpEndX'  
                                               , bpEndY    = bpEndY'  
                                               }
                                    
        getChild                    
         = atTag "children" >>>     
            proc l -> do chldType'  <- getAttrValue "xsi:type"            -< l
                         chldId'    <- getAttrValue "id"                  -< l
--                         chldName'  <- getAttrValue "name"                -< l -- defined, but not used.
                         chldFCol'  <- getAttrValue "fillColor"           -< l
                         chldAlgn'  <- getAttrValue "textAlignment"       -< l
                         chldElem'  <- getAttrValue "archimateElement"    -< l
                         trgtConn'  <- getAttrValue "targetConnections"   -< l
                         bound'     <- getChildren >>> getBound           -< l
                         srcConns'  <- listA (getChildren >>> getSrcConn) -< l
                         childs'    <- listA (getChildren >>> getChild)   -< l
                         returnA    -< Child { chldType = chldType'
                                             , chldId   = chldId'
                                             , chldAlgn = chldAlgn'
                                             , chldFCol = chldFCol'
                                             , chldElem = chldElem'
                                             , trgtConn = trgtConn'
                                             , bound    = bound'
                                             , srcConns = srcConns'
                                             , childs   = childs'
                                             }

-- Auxiliaries

   atTag :: ArrowXml a => String -> a (NTree XNode) XmlTree
   atTag tag = deep (isElem >>> hasName tag)

   text :: ArrowXml a => a (NTree XNode) String
   text = getChildren >>> getText
{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings #-}
module Database.Design.Ampersand.Input.Archi.ArchiAnalyze (archi2PContext)
   -- The purpose of this module is to load Archimate content into an Ampersand context.
   -- This module parses an Archi-repository by means of function `archi2PContext`, which produces a `P_Context` for merging into Ampersand.
   -- That `P_Context` contains both the Archimate-metamodel (in the form of declarations) and the Archimate population that represents the model.
   -- In this way, `archi2PContext ` deals with the fact that Archimate produces a mix of model and metamodel.
where
   import Database.Design.Ampersand.Basics  -- for things such as fatal, eqClass
   import Data.Char                         -- for things such as toLower
   import qualified Data.Map.Strict as Map  -- import qualified, to avoid name clashes with Prelude functions
   import Data.Tree.NTree.TypeDefs
   import Text.XML.HXT.Core hiding (utf8, fatal,trace)
   import Database.Design.Ampersand.Core.ParseTree
   import Data.List  -- for things such as nub
   import Data.Maybe

   -- | Function `archi2PContext` is meant to grind the contents of an Archi-repository into declarations and population inside a fresh Ampersand P_Context.
   --   The process starts by parsing an XML-file by means of function `processStraight` into a data structure called `archiRepo`. This function uses arrow-logic from the HXT-module.
   --   The resulting data structure contains the folder structure of the tool Archi (https://github.com/archimatetool/archi) and represents the model-elements and their properties.
   --   A lookup-function, `elemLookup`,is derived from `archiRepo`.
   --   It assigns the Archi-type (e.g. Business Process) to the identifier of an arbitrary Archi-object (e.g. "0957873").
   --   Then, the properties have to be provided with identifiers (see class `WithProperties`), because Archi represents them just as key-value pairs.
   --   The function `grindArchiPop` retrieves the population of meta-relations
   --   It produces the P_Populations and P_Declarations that represent the Archimate model.
   --   Finally, the function `mkArchiContext` produces a `P_Context` ready to be merged into the rest of Ampersand's population.
   archi2PContext :: String -> IO P_Context
   archi2PContext archiRepoFilename  -- e.g. "CA repository.xml"
    = do archiRepo <- runX (processStraight archiRepoFilename)
         let elemLookup atom = Map.lookup atom (Map.fromList (typeMap archiRepo))
         return ((mkArchiContext . grindArchiPop elemLookup . identifyProps []) archiRepo)

   mkArchiContext :: [(P_Population,P_Declaration,[P_Gen])] -> P_Context
   mkArchiContext pops =
     PCtx{ ctx_nm     = "Archimate"
         , ctx_pos    = []
         , ctx_lang   = Dutch  -- fatal 686 "No language because of Archi-import hack. Please report this as a bug"
         , ctx_markup = Nothing
         , ctx_thms   = []
         , ctx_pats   = []
         , ctx_rs     = []
         , ctx_ds     = nub archiDecls
         , ctx_cs     = []
         , ctx_ks     = []
         , ctx_rrules = []
         , ctx_rrels  = []
         , ctx_reprs  = []
         , ctx_vs     = []
         , ctx_gs     = nub (concat archiGenss)
         , ctx_ifcs   = []
         , ctx_ps     = []
         , ctx_pops   = archiPops
         , ctx_sql    = []
         , ctx_php    = []
         , ctx_metas  = []
         }
     where archiPops ::  [P_Population]
           archiDecls :: [P_Declaration]
           archiGenss :: [[P_Gen]]
           (archiPops, archiDecls, archiGenss) = unzip3 pops

-- The following code defines a data structure (called ArchiRepo) that corresponds to an Archi-repository in XML.

   data ArchiRepo = ArchiRepo
     { archRepoName   :: String
     , archRepoId     :: String
     , archFolders    :: [Folder]
     , archProperties :: [ArchiProp]
     } deriving (Show, Eq)
 
   data Folder = Folder
     { fldName        :: String
     , fldId          :: String
     , fldType        :: String
     , fldElems       :: [Element]
     , fldFolders     :: [Folder]
     } deriving (Show, Eq)

   data Element = Element
     { elemType       :: String
     , elemId         :: String
     , elemName       :: String
     , elemSrc        :: String
     , elemTgt        :: String
     , elemDocu       :: String
     , elChilds       :: [Child]
     , elProps        :: [ArchiProp]
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


-- | Properties in Archimate have no identifying key. In Ampersand, that key is necessary. So the class WithProperties is defined to
--   generate keys for properties, to be inserted in the grinding process. The only data structures with properties in the inner structure
--   of Archi (i.e. in the repository minus the Views), are folders and elements. For this reason, the types ArchiRepo, Folder, and Element
--   are instances of class WithProperties.

   class WithProperties a where
     allProps      :: a -> [ArchiProp]        -- takes all properties from an ArchiRepo, a Folder, or an Element
     identifyProps :: [String] -> a -> a -- distributes identifiers ( [String] ) over an ArchiRepo, a Folder, or an Element, in order to assign a unique identifier to each property in it.

   instance WithProperties ArchiRepo where
     allProps archiRepo = allProps (archFolders archiRepo) ++ archProperties archiRepo
     identifyProps _ archiRepo = archiRepo
       { archProperties = [ prop{archPropId=Just propId} | (prop,propId)<- zip (archProperties archiRepo) propIds ]
       , archFolders    = identifyProps fldrIds (archFolders archiRepo)
       }
       where
         identifiers = [ "pr-"++show (i::Integer) | i<-[0..] ] -- infinitely many unique keys to identify properties.
         fldrIds = take ((length.allProps.archFolders) archiRepo) identifiers
         propIds = drop ((length.allProps.archFolders) archiRepo) identifiers

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
     typeMap ::        a -> [(String,String)]        -- the map that determines the type (xsi:type) of every atom (id-field) in the repository
     grindArchiPop :: (String->Maybe String) -> a -> -- create population and the corresponding metamodel for the metamodel of Archi (i.e. BusinessProcess, DataObject, etc.)
                      [(P_Population,P_Declaration,[P_Gen])]
     keyArchi ::       a -> String                   -- get the key value (dirty identifier) of an a.

   instance MetaArchi ArchiRepo where
     typeMap archiRepo
      = typeMap [ folder | folder<-archFolders archiRepo, fldName folder/="Views"]  ++ 
        (typeMap.archProperties) archiRepo
     grindArchiPop elemLookup archiRepo
      = (concat.map (grindArchiPop elemLookup)) backendFolders  ++ 
        (concat.map (grindArchiPop elemLookup).archProperties) archiRepo
        where backendFolders = [ folder | folder<-archFolders archiRepo, fldName folder/="Views"]
     keyArchi = archRepoId

   instance MetaArchi Folder where
     typeMap folder
      = (typeMap.fldElems)   folder  ++ 
        (typeMap.fldFolders) folder
     grindArchiPop elemLookup folder
      = (concat.map (grindArchiPop elemLookup).fldElems)   folder  ++ 
        (concat.map (grindArchiPop elemLookup).fldFolders) folder
     keyArchi = fldId

   instance MetaArchi Element where
-- A type map is constructed for Archi-objects only. Taking relationships into this map brings Archi into higher order logic, and may cause black holes in Haskell. 
     typeMap element
      = [(keyArchi element, elemType element) | (not.null.elemName) element, (null.elemSrc) element] ++
        typeMap (elProps element)
     grindArchiPop elemLookup element
      = [ translateArchiObj "name" (elemType element) [(keyArchi element, elemName element)]
        | (not.null.elemName) element, (null.elemSrc) element] ++
        [ translateArchiObj "docu" (elemType element) [(keyArchi element, elemDocu element)]
        | (not.null.elemDocu) element, (null.elemSrc) element] ++
        (if isRelationship element then []  else    -- do this only for elements which are a relationship.
         translateArchiRel
                    elemLookup (keyArchi element)
                    (if (null.elemName) element
                     then unfixRel (elemType element)
                     else elemName element
                    )
                    (elemSrc element) (elemTgt element)
        ) ++
        [ translateArchiObj "elprop" (elemType element) [(keyArchi prop, elemSrc element)]
        | prop<-elProps element] ++
        (concat.map (grindArchiPop elemLookup).elProps) element
     keyArchi = elemId

   isRelationship :: Element -> Bool  -- figure out whether this XML-element is an Archimate Relationship.
   isRelationship element = (null.elemSrc) element

   instance MetaArchi ArchiProp where
     typeMap _
      = []
     grindArchiPop _ property
      = [ translateArchiObj "key" "Property"
            [(keyArchi property, archPropKey property) | (not.null.archPropKey) property ]
        , translateArchiObj "value" "Property"
            [(keyArchi property, archPropVal property) | (not.null.archPropVal) property ]
        ]
     keyArchi = fromMaybe (error "fatal 234: No key defined yet") . archPropId

   instance MetaArchi a => MetaArchi [a] where
     typeMap                  xs = concat [ typeMap                  x | x<-xs ]
     grindArchiPop elemLookup xs = concat [ grindArchiPop elemLookup x | x<-xs ]
     keyArchi = error "fatal 269: cannot use keyArchi on a list"

-- | The function `translateArchiObj` does the actual compilation of data objects from archiRepo into the Ampersand structure.
--   It looks redundant to produce both a `P_Population` and a `P_Declaration`, but the first contains the population and the second is used to
--   include the metamodel of Archimate in the population. This save the author the effort of maintaining an Archimate-metamodel.
   translateArchiObj :: String -> String -> [(String, String)] -> (P_Population,P_Declaration,[P_Gen])
   translateArchiObj "name" typeLabel tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "naam" (Just (P_Sign (PCpt typeLabel) (PCpt "Tekst")))) (transTuples tuples)
      , P_Sgn "naam" (P_Sign (PCpt typeLabel) (PCpt "Tekst")) [Uni] [] [] [] OriginUnknown False, [] )
   translateArchiObj "docu" typeLabel tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "documentatie" (Just (P_Sign (PCpt typeLabel) (PCpt "Tekst")))) (transTuples tuples)
      , P_Sgn "documentatie" (P_Sign (PCpt typeLabel) (PCpt "Tekst")) [Uni] [] [] [] OriginUnknown False, [] )
   translateArchiObj "key" "Property" tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "key" (Just (P_Sign (PCpt "Property") (PCpt "Tekst")))) (transTuples tuples)
      , P_Sgn "key" (P_Sign (PCpt "Property") (PCpt "Tekst")) [Uni] [] [] [] OriginUnknown False, [] )
   translateArchiObj "value" "Property" tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "value" (Just (P_Sign (PCpt "Property") (PCpt "Tekst")))) (transTuples tuples)
      , P_Sgn "value" (P_Sign (PCpt "Property") (PCpt "Tekst")) [Uni] [] [] [] OriginUnknown False, [] )
   translateArchiObj "elprop" _ tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "propOf" (Just (P_Sign (PCpt "Property") (PCpt "ArchiObject")))) (transTuples tuples)
      , P_Sgn "propOf" (P_Sign (PCpt "Property") (PCpt "ArchiObject")) [Uni] [] [] [] OriginUnknown False, [] )
   translateArchiObj _ _ _ = error "fatal 328 non-exhaustive pattern in translateArchiObj"

-- | The function `translateArchiRel` does the actual compilation of relationships from archiRepo into the Ampersand structure.
   translateArchiRel :: (String -> Maybe String) -> String -> String -> String -> String -> [(P_Population, P_Declaration, [P_Gen])]
   translateArchiRel elemLookup relId relLabel x y
    = [ ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown relNm (Just (P_Sign (PCpt xType) (PCpt yType)))) (transTuples [(x,y)])
        , P_Sgn relNm (P_Sign (PCpt xType) (PCpt yType)) [] [] [] [] OriginUnknown False
        , []
        )
-- To obtain a readable Archimate Metamodel in graphviz, remove the following 4 lines from the code.
-- In that way, the source- and target functions (from the meta-metamodel) do not pollute the diagram, whereas the relation is still visible.
      , ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "source" (Just (P_Sign (PCpt "Relationship") (PCpt "ArchiObject")))) (transTuples [(relId,x)])
        , P_Sgn "source" (P_Sign (PCpt "Relationship") (PCpt "ArchiObject")) [Uni] [] [] [] OriginUnknown False
        , [] -- [ PGen OriginUnknown (PCpt xType) (PCpt "ArchiObject") ]
        )
      , ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "target" (Just (P_Sign (PCpt "Relationship") (PCpt "ArchiObject")))) (transTuples [(relId,y)])
        , P_Sgn "target" (P_Sign (PCpt "Relationship") (PCpt "ArchiObject")) [Uni] [] [] [] OriginUnknown False
        , [] -- [ PGen OriginUnknown (PCpt yType) (PCpt "ArchiObject") ]
        )
      ] ++
      [ ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "datatype" (Just (P_Sign (PCpt "Relationship") (PCpt "Tekst")))) (transTuples [(relId,relLabel)])
        , P_Sgn "datatype" (P_Sign (PCpt "Relationship") (PCpt "Tekst")) [Uni] [] [] [] OriginUnknown False
        , []
        )
      | xType=="ApplicationComponent" && yType=="ApplicationComponent" && relLabel/="flow" ]
      where xType = case elemLookup x of
                      Just str -> str
                      Nothing -> fatal 292 ("No Archi-object found for Archi-identifier "++show x)
            yType = case elemLookup y of
                      Just str -> str
                      Nothing -> fatal 293 ("No Archi-object found for Archi-identifier "++show y)
            relNm = if xType=="ApplicationComponent" && yType=="ApplicationComponent"
                    then "flow"
                    else relLabel  --  ++"["++xType++"*"++yType++"]"

   unfixRel :: String -> String
   unfixRel cs = (reverse.drop 1.dropWhile (/='R').reverse.relCase) cs
   relCase :: String -> String
   relCase (c:cs) = (toUpper c): cs
   relCase "" = error "fatal 325 empty relation identifier."
   transTuples :: [(String, String)] -> [PAtomPair]
   transTuples tuples = [ PPair OriginUnknown (ScriptString OriginUnknown x) (ScriptString OriginUnknown y) | (x,y)<-tuples ]


-- The function `processStraight` derives an ArchiRepo from an Archi-XML-file.
   processStraight :: String -> IOSLA (XIOState s0) XmlTree ArchiRepo
   processStraight infile
    = readDocument [ withRemoveWS  yes        -- purge redundant white spaces
                   , withCheckNamespaces yes  -- propagates name spaces into QNames
                   , withTrace 0]             -- if >0 gives trace messages.
                   infile
      >>>
      analArchiRepo
       where
        analArchiRepo :: ArrowXml a => a XmlTree ArchiRepo
        analArchiRepo
          = atTag "archimate:ArchimateModel" >>>
            proc l -> do repoNm'   <- getAttrValue "name"               -< l
                         repoId'   <- getAttrValue "id"                 -< l
                         folders'  <- listA (getChildren >>> getFolder) -< l
                         props'    <- listA (getChildren >>> getProp)   -< l
                         returnA   -< ArchiRepo { archRepoName   = repoNm'
                                                , archRepoId     = repoId'
                                                , archFolders    = folders'
                                                , archProperties = [ prop{archPropId=Just $ "pr-"++show i} | (prop,i)<- zip props' [length (allProps folders')..] ]
                                                }

        getFolder :: ArrowXml a => a XmlTree Folder
        getFolder
         = isElem >>> hasName "folders" >>>
            proc l -> do fldNm'     <- getAttrValue "name"                 -< l
                         fldId'     <- getAttrValue "id"                   -< l
                         fldType'   <- getAttrValue "type"                 -< l
                         elems'     <- listA (getChildren >>> getElement)  -< l
                         subFlds'   <- listA (getChildren >>> getFolder)   -< l
                         returnA    -< Folder { fldName    = fldNm'
                                              , fldId      = fldId'
                                              , fldType    = fldType'
                                              , fldElems   = elems'
                                              , fldFolders = subFlds'
                                              }

        getProp :: ArrowXml a => a XmlTree ArchiProp
        getProp = isElem >>> hasName "properties" >>>
            proc l -> do propKey    <- getAttrValue "key"   -< l
                         propVal    <- getAttrValue "value" -< l
                         returnA    -< ArchiProp { archPropKey = propKey
                                                 , archPropId  = Nothing -- error "fatal 315: archPropId not yet defined"
                                                 , archPropVal = propVal
                                                 }

        getElement :: ArrowXml a => a XmlTree Element
        getElement = atTag "elements" >>>
            proc l -> do elemType'  <- getAttrValue "xsi:type"           -< l
                         elemId'    <- getAttrValue "id"                 -< l
                         elemName'  <- getAttrValue "name"               -< l
                         elemSrc'   <- getAttrValue "source"             -< l
                         elemTgt'   <- getAttrValue "target"             -< l
                         elemDocu'  <- getAttrValue "documentation"      -< l
                         childs'    <- listA (getChildren >>> getChild)  -< l
                         props'     <- listA (getChildren >>> getProp)   -< l
                         returnA    -< Element  { elemType = drop 1 (dropWhile (/=':') elemType')  -- drop the prefix "archimate:"
                                               , elemId   = elemId'
                                               , elemName = elemName'
                                               , elemSrc  = elemSrc'
                                               , elemTgt  = elemTgt'
                                               , elemDocu = elemDocu'
                                               , elChilds = childs'
                                               , elProps  = props'
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
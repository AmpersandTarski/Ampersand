{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings, DuplicateRecordFields #-}
module Ampersand.Input.Archi.ArchiAnalyze (doArchiAnalyze)
   -- The purpose of this module is to load Archimate content into an Ampersand context.
   -- This module parses an Archi-repository by means of function `doArchiAnalyze`, which produces a `P_Context` for merging into Ampersand.
   -- That `P_Context` contains both the Archimate-metamodel (in the form of declarations) and the Archimate population that represents the model.
   -- In this way, `doArchiAnalyze ` deals with the fact that Archimate produces a mix of model and metamodel.
where
   import           Ampersand.Basics                 -- for things such as fatal, eqClass
   import           RIO.Char                         -- for things such as toLower
   import qualified Data.Map                  as M
   import           Data.Tree.NTree.TypeDefs
   import           GHC.Exts (groupWith)
   import           Text.XML.HXT.Core  hiding (utf8,fatal,trace)
--   import           Ampersand.Input.ADL1.CtxError
   import           Ampersand.Core.ParseTree
-- import qualified RIO.Text                  as T   -- for things such as pack
   import qualified RIO.List                  as L
   import qualified Data.List.NonEmpty        as NEL
   import qualified RIO.Set                   as Set

   -- | Function `doArchiAnalyze` is meant to grind the contents of an Archi-repository into declarations and population inside a fresh Ampersand P_Context.
   --   The process starts by parsing an XML-file by means of function `processStraight` into a data structure called `archiRepo`.
   doArchiAnalyze :: HasHandle env => String -> RIO env P_Context
   doArchiAnalyze archiRepoFilename  -- e.g. "CA repository.xml"
    = do -- hSetEncoding stdout utf8
   --   This function uses arrow-logic from the HXT-module.
   --   The resulting data structure represents the model-elements, the relations, and their properties.
   --   Two lookup-functions, `elLookup` and `relLookup`,are derived from `archiRepo`.
   --   The function `grind2Rels` retrieves the population of meta-relations
   --   It produces the P_Populations and P_Declarations that represent the Archimate model.
   --   Finally, the function `mkArchiContext` produces a `P_Context` ready to be merged into the rest of Ampersand's population.
         archiModels <- liftIO $ runX (processStraight archiRepoFilename)
         let elLookup :: ElemRef -> Maybe Element
             elLookup atom = (M.lookup atom . elNameMap) archiModels
         let relLookup :: RelRef -> Maybe Relation
             relLookup atom = (M.lookup atom . relNameMap) archiModels
         let patterns :: [P_Pattern]
             patterns = grind2Pats elLookup relLookup archiModels
         sayLn (warnDuplicateViewNames patterns)
         (pure (mkArchiContext patterns)) -- list of Warnings is empty

   warnDuplicateViewNames :: [P_Pattern] -> String
   warnDuplicateViewNames pats 
     = case groups of
        []    -> ""
        [g@(pat:_)] -> "Warning:\nOne view name, \""++show (pt_nm pat)++"\", has been given to multiple ("++show (length g)++") views."
        _           -> "Warnings:\n"++"There are views with duplicate names:\n   "++
                       L.intercalate ",\n   " [show (pt_nm pat) | pat:_<-groups]++"."
     where
      groups = (filter moreThanOne . groupWith pt_nm) pats
      moreThanOne (_:_:_) = True
      moreThanOne  _      = False

   mkArchiContext :: [P_Pattern] -> P_Context
   mkArchiContext pats =
     PCtx{ ctx_nm     = "Archimate"
         , ctx_pos    = []
         , ctx_lang   = Just Dutch  -- fatal "No language because of Archi-import hack. Please report this as a bug"
         , ctx_markup = Nothing
         , ctx_pats   = pats
         , ctx_rs     = []
         , ctx_ds     = []
         , ctx_cs     = []
         , ctx_ks     = []
         , ctx_rrules = []
         , ctx_reprs  = []
         , ctx_vs     = []
         , ctx_gs     = []
         , ctx_ifcs   = []
         , ctx_ps     = []
         , ctx_pops   = []
         , ctx_metas  = []
         }

-- The following code defines a data structure (called ArchiModel) that mimics the structure of an Archi-repository in XML.

   data ArchiModel = ArchiModel
        { modelName       :: Cstring
        , archModelId     :: String
        , archDocu        :: String
        , archElements    :: [Element]
        , archRelations   :: [Relation]
        , archProperties  :: [Property]
        , archOrgItems    :: [Item]
        , archPropdefs    :: [PropertyDef]
        , archViews       :: [View]
        } deriving (Show, Eq)
     
   data ElemRef = ER String  deriving (Show, Eq, Ord)  -- The string represents the key (e.g. id-40192) of an element
   data Element = Element
        { elemName        :: Maybe Cstring
        , elemId          :: String
        , elemType        :: Cstring
        , elemDocu        :: String
        , elemProps       :: [Property]
        } deriving (Show, Eq)

   data RelRef = RR String  deriving (Show, Eq, Ord)  -- The string represents the key (e.g. id-10382) of an element
   data Relation = Relation
        { relName         :: Maybe Cstring
        , relId           :: String
        , source          :: ElemRef
        , target          :: ElemRef
        , relType         :: String
        , relDocu         :: String
        , relProps        :: [Property]
        } deriving (Show, Eq)

   data Property = Property
        { propId          :: String
        , propName        :: Cstring
        } deriving (Show, Eq)

   data PropertyDef = PropDef
        { pdType          :: Cstring
        , pdId            :: String
        , pdName          :: Cstring
        , pdDocu          :: String
        , props           :: [Property]
        } deriving (Show, Eq)

   data View = View
        { viewId          :: String
        , viewPoint       :: Cstring
        , viewName        :: String
        , viewDocu        :: String
        , viewNodes       :: [Node]
        , viewConnections :: [Connection]
        } deriving (Show, Eq)

   data NodeRef = NR String  deriving (Show, Eq, Ord)  -- The string represents the key (e.g. id-34491) of a node
   data Node = Node
        { nodeId          :: String
        , nodeElem        :: ElemRef
        , nodeType        :: String
        , subnodes        :: [Node]
        } deriving (Show, Eq)

   data ConnRef = CR String  deriving (Show, Eq, Ord)  -- The string represents the key (e.g. id-24876) of a connection
   data Connection = Conn
        { connId          :: String
        , connType        :: String
        , connRel         :: RelRef  -- refers to the corresponding relation in the model
        , connSrc         :: String  -- refers to the source node in the view
        , connTgt         :: String  -- refers to the source node in the view
        } deriving (Show, Eq)

   data Item = Item
        { itemId          :: String
        , itemIdRf        :: String
        , label           :: String
        , itemDocu        :: String
        , items           :: [Item]
        } deriving (Show, Eq)

   data Organization = Organization
        { itemId          :: String
        , itemIdRf        :: String
        , label           :: String
        , itemDocu        :: String
        , items           :: [Item]
        } deriving (Show, Eq)

   data Cstring = C String    -- meant for concept names, to make them case insensitive. (Not used for archimate atoms)

   instance Eq Cstring where
     C a == C b = camelCaseLow a == camelCaseLow b

   instance Show Cstring where
     show (C str) = str

   -- We make a lookup table for all elements, relations, nodes, and connections from the Archimate file.
   class ElMap a where
     elNameMap  :: a -> M.Map ElemRef Element
   instance ElMap ArchiModel where
     elNameMap archModel = elNameMap (archElements archModel)
   instance ElMap a => ElMap [a] where
     elNameMap  = M.unions . map elNameMap
   instance ElMap Element where
     elNameMap element = M.fromList [(ER (elemId element), element)]

   class RelMap a where
     relNameMap :: a -> M.Map RelRef Relation
   instance RelMap ArchiModel where
     relNameMap archModel = relNameMap (archRelations archModel)
   instance RelMap a => RelMap [a] where
     relNameMap = M.unions . map relNameMap
   instance RelMap Relation where
     relNameMap rel = M.fromList [(RR (relId rel), rel)]

-- | class Grind2Pop serves to populate an Archi-metamodel with the contents
--   of an Archi-repository. Function grind2Pops grinds the contents into populations.
--   These populations contribute to a P-structure, which is then fed to Ampersand.
   class Grind2Pop a where
     grind2Pops :: (ElemRef->Maybe Element) -> (RelRef->Maybe Relation) -> a -> [P_Population]

   instance Grind2Pop a => Grind2Pop [a] where
     grind2Pops elLookup relLookup = concat . map (grind2Pops elLookup relLookup)

   instance Grind2Pop ArchiModel where
     grind2Pops elLookup relLookup archiModel
      = (concat . map (grind2Pops elLookup relLookup) . archRelations) archiModel ++
        (concat . map (grind2Pops elLookup relLookup) . archViews) archiModel

   instance Grind2Pop View where
     grind2Pops elLookup relLookup vw
      = ( sortRelPops $
          grind2Pops elLookup relLookup (relationsFrom relLookup vw)
          -- besides the population from explicit Archimate relations,
          -- we treat grouping as an implicit form of Archmate relation.
          -- In the absence of grouping relations in the XML-source, we generate the population directly.
          ++ concat [ translateArchiObj (C "grouping") relSrc relTgt [(s,t)]
                    | nRoot<-viewNodes vw, (n,sn)<-recur nRoot, relSrc<-tpLkp n, relTgt<-tpLkp sn
                    , s<-nmLkp n, t<-nmLkp sn
                    ]
        ) ++
        ( sortCptPops $
          grind2Pops elLookup relLookup
          [ el | Just el<-(map elLookup . map nodeElem . allNodes . viewNodes) vw ]
        )
       where
         sortRelPops :: [P_Population] -> [P_Population] -- assembles P_Populations with the same signature into one
         sortRelPops popus
          = [ (NEL.head cl){p_popps = pairs}
            | cl<-eqCl p_nmdr [pop | pop@P_RelPopu{}<-popus]
            , pairs<-[foldr uni [] (NEL.map p_popps cl)]
            , not (null pairs)
            ]
         sortCptPops :: [P_Population] -> [P_Population] -- assembles P_Populations with the same signature into one
         sortCptPops popus
          = [ (NEL.head cl){p_popas = foldr uni [] (NEL.map p_popas cl)}
            | cl<-eqCl p_cnme [pop | pop@P_CptPopu{}<-popus]
            ]
         uni :: Eq a => [a] -> [a] -> [a]
         uni xs ys = L.nub (xs ++ ys)
         recur :: Node -> [(Node,Node)]
         recur n = [(n,s)|s<-subnodes n] ++ (concat . map recur . subnodes) n
         nmLkp, tpLkp :: Node -> [Cstring]
         nmLkp n = [ case elemName e of Just cnm -> cnm; _ -> C "!NoName" | Just e<-[elLookup (nodeElem n)]]
         tpLkp n = [ elemType e | Just e<-[elLookup (nodeElem n)] ]
         allNodes :: [Node] -> [Node]
         allNodes [] = []
         allNodes nodes = nodes++(allNodes . concat . map subnodes) nodes
   relationsFrom :: (RelRef->Maybe Relation) -> View -> [Relation]
   relationsFrom relLookup vw = [ r | Just r <- (map relLookup . map connRel . viewConnections) vw ]

   elementsFrom :: (ElemRef->Maybe Element) -> View -> [Element]
   elementsFrom elLookup vw = [ e | Just e <- (map elLookup . map nodeElem . viewNodes) vw ]

   instance Grind2Pop Element where
     grind2Pops _ _ el
      = [ P_CptPopu OriginUnknown (camelCaseUp typName) [ScriptString OriginUnknown nm] ]
        where nm=case elemName el of Just (C str) -> str; _ -> "!NoName"
              C typName = elemType el
   instance Grind2Pop Relation where
     grind2Pops elLookup _ rel
      = if relType rel=="Specialization"
        then let relSrc = (nmLookup.source) rel
                 relTgt = (nmLookup.target) rel in
             translateArchiObj (C "specialize") relTgt relSrc []
        else let relNm  = C (if relType rel=="Association"
                             then relNameType rel
                             else relType rel)
                 relSrc = (tpLookup.source) rel
                 relTgt = (tpLookup.target) rel in
              concat [ translateArchiObj relNm relSrc relTgt [(s,t)] | s<-(nmLkp.source) rel, t<-(nmLkp.target) rel]
        where nmLookup, tpLookup :: ElemRef -> Cstring
              nmLookup eref = case elLookup eref of Just e->(case elemName e of Just cnm -> cnm; _ -> C "!NoName") ; _ -> C ("No element named "++show eref++" in "++show rel)
              tpLookup eref = case elLookup eref of Just e->elemType e ; _ -> C ("No element named "++show eref++" in "++show rel)
              nmLkp eref = [ case elemName e of Just cnm -> cnm; _ -> C "!NoName" | Just e<-[elLookup eref]]

-- | class Grind2Rel serves to populate an Archi-metamodel with the contents
--   of an Archi-repository. Function grind2Rels grinds the contents into populations.
--   These populations contribute to a P-structure, which is then fed to Ampersand.
   class Grind2Rel a where
     grind2Rels :: (ElemRef->Maybe Element) -> (RelRef->Maybe Relation) -> a -> [P_Relation]

   instance Grind2Rel a => Grind2Rel [a] where
     grind2Rels elLookup relLookup = concat . map (grind2Rels elLookup relLookup)

   instance Grind2Rel Connection where
     grind2Rels elLookup relLookup conn
      = concat [grind2Rels elLookup relLookup rel| Just rel<-[(relLookup . connRel) conn] ]

   instance Grind2Rel Relation where
     grind2Rels elLookup _ rel
      = if relType rel=="Specialization"
        then let relSrc = (nmLookup.source) rel
                 relTgt = (nmLookup.target) rel in
             [ mkPRelation (C "specialize") relTgt relSrc ]
        else let relNm  = C (if relType rel=="Association"
                             then relNameType rel
                             else relType rel)
                 relSrc = (tpLookup.source) rel
                 relTgt = (tpLookup.target) rel in
              [ mkPRelation relNm relSrc relTgt ]
        where nmLookup, tpLookup :: ElemRef -> Cstring
              nmLookup eref = case elLookup eref of Just e->(case elemName e of Just cnm -> cnm; _ -> C "!NoName") ; _ -> C ("No element named "++show eref++" in "++show rel)
              tpLookup eref = case elLookup eref of Just e->elemType e ; _ -> C ("No element named "++show eref++" in "++show rel)

   mkPRelation :: Cstring -> Cstring -> Cstring -> P_Relation
   mkPRelation (C relNm) (C relSrc) (C relTgt)
     = P_Sgn { dec_nm     = camelCaseLow relNm
             , dec_sign   = P_Sign (PCpt (camelCaseUp relSrc)) (PCpt (camelCaseUp relTgt))
             , dec_prps   = Set.empty       -- ^ the user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx)
             , dec_pragma = [] 
             , dec_Mean   = []              -- ^ the optional meaning of a relation, possibly more than one for different languages.
             , pos        = OriginUnknown   -- ^ the position in the Ampersand source file where this relation is declared. Not all relations come from the ampersand souce file.
             }

   -- | assigns the given name to a relation. If no name is given, the Archimate name (relType) is used as name.
   relNameType :: Relation -> String
   relNameType r = case relName r of Just (C nm) -> nm ; _ -> relType r

   instance Grind2Rel View where
    -- besides the explicit Archimate relations, which are built in `instance Grind2Rel Relation`,
    -- we treat grouping as an implicit form of Archmate relation.
    -- So we generate these relations where we can see them: in each View.
     grind2Rels elLookup relLookup vw
      = Set.toList . Set.fromList $
        grind2Rels elLookup relLookup (viewConnections vw) ++
        [ mkPRelation (C "grouping") relSrc relTgt
        | nRoot<-viewNodes vw, (n,sn)<-recur nRoot, relSrc<-tpLkp n, relTgt<-tpLkp sn
        ]
       where
         recur :: Node -> [(Node,Node)]
         recur n = [(n,s)|s<-subnodes n] ++ (concat . map recur . subnodes) n
         tpLkp :: Node -> [Cstring]
         tpLkp n = [ elemType e | Just e<-[elLookup (nodeElem n)] ]

   class Grind2Purpose a where
     grind2Purposes :: (ElemRef -> Maybe Element) -> (RelRef -> Maybe Relation) -> a -> [PPurpose]

   instance Grind2Purpose a => Grind2Purpose [a] where
     grind2Purposes elLookup relLookup = concat . map (grind2Purposes elLookup relLookup)

   instance Grind2Purpose Element where
     grind2Purposes _ _ el
      = [ mkPurp (PRef2ConceptDef nm) (elemDocu el) | (not . null . elemDocu) el]
        where nm=case elemName el of Just (C str) -> str; _ -> "!NoName"
   instance Grind2Purpose Relation where
     grind2Purposes elLookup _ rel
      = [ mkPurp (PRef2Relation (PNamedRel{ pos      = OriginUnknown
                                          , p_nrnm   = camelCaseLow (relNameType rel)
                                          , p_mbSign = Just (P_Sign (PCpt (camelCaseUp srcNm)) (PCpt (camelCaseUp tgtNm)))
                                          })) (relDocu rel)
        | (not . null . relDocu) rel
        , Just relSrc<-[elLookup (source rel)], Just relTgt<-[elLookup (target rel)]
        , C srcNm<-[elemType relSrc], C tgtNm<-[elemType relTgt]
        ]
   instance Grind2Purpose View where
     grind2Purposes elLookup relLookup vw
      = [ mkPurp (PRef2Pattern (viewName vw)) (viewDocu vw) | (not . null . viewDocu) vw] ++
        grind2Purposes elLookup relLookup (elementsFrom elLookup vw) ++
        grind2Purposes elLookup relLookup (relationsFrom relLookup vw)

   mkPurp :: PRef2Obj -> String -> PPurpose
   mkPurp obj str
    = PRef2 { pos       = OriginUnknown
            , pexObj    = obj
            , pexMarkup = P_Markup{mLang=Nothing, mFormat=Nothing, mString=str}
            , pexRefIDs = []
            }

   class Grind2Pat a where
     grind2Pats :: (ElemRef -> Maybe Element) -> (RelRef -> Maybe Relation) -> a -> [P_Pattern]
   instance Grind2Pat ArchiModel where
     grind2Pats elLookup relLookup archiModel = (concat . map (grind2Pats elLookup relLookup) . archViews) archiModel
   instance Grind2Pat a => Grind2Pat [a] where
     grind2Pats elLookup relLookup = concat . map (grind2Pats elLookup relLookup)
   instance Grind2Pat View where
     grind2Pats elLookup relLookup vw = 
       [ P_Pat { pos      = OriginUnknown
               , pt_nm    = viewName vw
               , pt_rls   = []
               , pt_gns   = []
               , pt_dcs   = grind2Rels elLookup relLookup vw
               , pt_RRuls = []
               , pt_cds   = []
               , pt_Reprs = []
               , pt_ids   = []
               , pt_vds   = []
               , pt_xps   = grind2Purposes elLookup relLookup vw
               , pt_pop   = grind2Pops elLookup relLookup vw
               , pt_end   = OriginUnknown  -- was ptend pat
               }
       ]

   translateArchiObj :: Cstring -> Cstring -> Cstring   -- name, source, and target
                           -> [(Cstring, Cstring)]     -- pairs of archimate keys (e.g. "id-38772")
                           -> [P_Population]
   translateArchiObj (C relTyp) (C s) (C t) tuples   -- e.g. translateArchiObj (C "Aggregation") (C "Product") (C "Grouping") [("id-30330","id-28500")]
    = case transTuples tuples of
       [] -> []
       ts -> [ P_RelPopu Nothing Nothing OriginUnknown
                        (PNamedRel OriginUnknown (camelCaseLow relTyp) (Just (P_Sign (PCpt (camelCaseUp s)) (PCpt (camelCaseUp t)))))
                        ts ]

--   translateInView 

   -- | The camelCase functions are meant to let identifiers satisfy the Ampersand syntax.
   --   However, it does not disambiguate case-insensitive identifiers. So "PERSOON" and "Persoon" are treated differently.
   camelCaseUp, camelCaseLow :: String -> String
   camelCaseUp  str = cmCase toUpper (unPrefix str)
   camelCaseLow str = cmCase toLower (unPrefix str)

   cmCase :: (Char->Char) -> String -> String
   cmCase toCase (c:cs) = toCase c : takeWhile (not.pred) cs ++ camelCaseUp (dropWhile (not.pred) cs)
   cmCase _ _ = ""

   unPrefix :: String -> String
   unPrefix str = dropWhile pred str
   pred :: Char -> Bool
   pred c = or [c==x | x<-" -_:;,<.>/?\\[]{}~!@#$%^&*()+=`'\"" ]

   transTuples :: [(Cstring, Cstring)] -> [PAtomPair]
   transTuples tuples = [ PPair OriginUnknown (ScriptString OriginUnknown x) (ScriptString OriginUnknown y) | (C x,C y)<-tuples, (not.null) x, (not.null) y ]

-- The function `processStraight` derives an ArchiRepo from an Archi-XML-file.
-- The way of working is inspired on the examples given in https://wiki.haskell.org/HXT/Practical/Simple2
   processStraight :: String -> IOSLA (XIOState s0) XmlTree ArchiModel
   processStraight absFilePath
    = readDocument [ withRemoveWS  yes        -- purge redundant white spaces
                   , withCheckNamespaces yes  -- propagates name spaces into QNames
                   , withValidate yes
                   , withTrace 0]             -- if >0 gives trace messages.
                   uri
      >>>
      analArchiModel
       where
        uri = "file://" ++ n (g <$> absFilePath)
          where
            g x = if x == '\\' then '/' else x
            n x@(h:_) = if h /= '/' then '/' : x else x
            n [] = fatal "Empty absFilePath"
        analArchiModel :: ArrowXml a => a XmlTree ArchiModel
        analArchiModel
          = atTag "model" >>>
          -- (For future code inspection)  The types are systematic:
          --  repoId :: String, getAttrValue "identifier" :: ArrowXml a => a XmlTree String, and l :: XmlTree
          --  names :: [String],  listA (getChildren >>> getContent) :: ArrowXml a => a XmlTree [String], and l :: XmlTree
          --  elems :: [Element],  listA getElement :: ArrowXml a => a XmlTree [Element], and elements :: XmlTree
            proc l -> do repoId     <- getAttrValue "identifier"                           -< l           -- repoId :: String
                         names      <- listA (getChildren >>> getContent "name")           -< l           -- names::[String]
                         docus      <- listA (getChildren >>> getContent "documentation")  -< l           -- names::[String]
                         properties <- getProperties                                       -< l           -- properties :: [Property]
                         elemTag    <- atTag "elements"                                    -< l           -- elemTag :: XmlTree
                         elements   <- listA getElement                                    -< elemTag     -- elements :: [Element]
                         relTag     <- atTag "relationships"                               -< l           -- relTag :: XmlTree
                         rels       <- listA getRelation                                   -< relTag      -- rels :: [Relation]
           -- TBD        orgItems   <- getOrganizations                                    -< l           -- orgItems :: [Item]
           -- TBD        propDefTag <- (atTag "propertyDefinitions" <+> atTag "propertyDefs") -< l           -- propDefTag :: XmlTree
           -- TBD        propDefs   <- listA getPropertyDef                                -< propDefTag  -- propdefs :: [PropertyDef]
                         viewsTag   <- atTag "views"                                       -< l           -- viewsTag :: XmlTree
                         views      <- listA getView                                       -< viewsTag    -- views :: [View]
                         returnA   -<  ArchiModel { archModelId    = repoId
                                                  , modelName      = C (testSingleton "Archimate model" names)
                                                  , archDocu       = case testZeroOrOne "documentation" docus of Just d -> d; _ -> ""
                                                  , archProperties = properties
                                                  , archElements   = elements
                                                  , archRelations  = rels
                                                  , archOrgItems   = [] -- orgItems
                                                  , archPropdefs   = [] -- propDefs
                                                  , archViews      = views
                                                  }

        getProperties :: ArrowXml a => a XmlTree [Property]
        getProperties = 
          (atTag "properties" >>> listA getProperty) `orElse` constA []

-- TBD  getOrganizations :: ArrowXml a => a XmlTree [Item]
-- TBD  getOrganizations = 
-- TBD    ((atTag "organizations" <+> atTag "organization") >>> listA getItem) `orElse` constA []

        getElement :: ArrowXml a => a XmlTree Element
        getElement = atTag "element" >>> 
            proc l -> do elemId'   <- getAttrValue "identifier"                          -< l
                         elemType' <- getAttrValue "xsi:type"                            -< l
                         names     <- listA (getChildren >>> (getContent "name"<+>getContent "label")) -< l
                         docus     <- listA (getChildren >>> getContent "documentation") -< l
                         properties<- listA getProperty                                  -< l
                         returnA  -<  Element { elemId    = elemId'
                                              , elemName  = testZeroOrOne "name" (map C names)
                                              , elemType  = C elemType'
                                              , elemDocu  = L.intercalate "; " docus
                                              , elemProps = properties
                                              }

        getRelation :: ArrowXml a => a XmlTree Relation
        getRelation = atTag "relationship" >>> 
            proc l -> do relId'    <- getAttrValue "identifier"                          -< l
                         relType'  <- getAttrValue "xsi:type"                            -< l
                         source'   <- getAttrValue "source"                              -< l
                         target'   <- getAttrValue "target"                              -< l
                         names     <- listA (getChildren >>> (getContent "name"<+>getContent "label"))  -< l
                         docus     <- listA (getChildren >>> getContent "documentation") -< l
                         properties<- listA getProperty                                  -< l
                         returnA  -<  Relation { relType   = relType'
                                               , relId     = relId'
                                               , source    = ER source'
                                               , target    = ER target'
                                               , relName   = testZeroOrOne "relation name" (map C names)
                                               , relDocu   = L.intercalate "; " docus
                                               , relProps  = properties
                                               }
  
        getProperty :: ArrowXml a => a XmlTree Property
        getProperty = atTag "property" >>> 
            proc l -> do propId'   <- getAttrValue "identifierref"                       -< l
                         values    <- listA (getChildren >>> getContent "value")          -< l
                         returnA  -<  Property { propId   = propId'
                                               , propName = testSingleton "value" (map C values)
                                               }

        getView :: ArrowXml a => a XmlTree View
        getView = atTag "view" >>> 
            proc l -> do viewId'    <- getAttrValue "identifier"                       -< l
                         viewPoint' <- getAttrValue "viewpoint"                        -< l
                         names      <- listA (getChildren >>> getContent "name")       -< l
                         docus      <- listA (getChildren >>> getContent "documentation") -< l
                         nodes      <- listA (getChildren >>> getnode)                 -< l
                         connections<- listA (getChildren >>> getConnection)           -< l
                         returnA   -<  View { viewId          = viewId'
                                            , viewPoint       = C viewPoint'
                                            , viewName        = case testZeroOrOne "name" (map C names) of Just (C d) -> d; _ -> ""
                                            , viewDocu        = L.intercalate "; " docus
                                            , viewNodes       = nodes
                                            , viewConnections = connections
                                            }

        getnode :: ArrowXml a => a XmlTree Node
        getnode = atTag "node" >>> 
            proc l -> do nodeId'    <- getAttrValue "identifier"                       -< l
                         nodeElem'  <- getAttrValue "elementRef"                       -< l
                         nodeType'  <- getAttrValue "xsi:type"                         -< l
                         nodes      <- listA (getChildren >>> getnode)                 -< l
                         returnA   -<  Node { nodeId          = nodeId'
                                            , nodeElem        = ER nodeElem'
                                            , nodeType        = nodeType'
                                            , subnodes        = nodes
                                            }

        getConnection :: ArrowXml a => a XmlTree Connection
        getConnection = atTag "connection" >>> 
            proc l -> do connId'    <- getAttrValue "identifier"                       -< l
                         connType'  <- getAttrValue "xsi:type"                         -< l
                         connRel'   <- getAttrValue "relationshipRef"                  -< l
                         src        <- getAttrValue "source"                           -< l
                         tgt        <- getAttrValue "target"                           -< l
                         returnA   -<  Conn { connId          = connId'
                                            , connType        = connType'
                                            , connRel         = RR connRel'
                                            , connSrc         = src
                                            , connTgt         = tgt
                                            }

-- TBD  getPropertyDef :: ArrowXml a => a XmlTree PropertyDef
-- TBD  getPropertyDef = atTag "propertyDefinition" >>> 
-- TBD      proc l -> do propId'    <- getAttrValue "identifier"                          -< l
-- TBD      --           propName'  <- getAttrValue "name"                                -< l  -- for compatibility with Archimate 2.1
-- TBD                   propType'  <- getAttrValue "type"                                -< l
-- TBD                   nameTag    <- atTag "name"                                       -< l           -- nameTag :: XmlTree
-- TBD                   propName'  <- text                                               -< nameTag     -- name :: String
-- TBD                   docuTag    <- atTag "documentation"                              -< l           -- docuTag :: XmlTree
-- TBD                   docu       <- text                                               -< docuTag     -- docu :: String
-- TBD                   propsTag   <- atTag "properties"                                 -< l           -- propsTag :: XmlTree
-- TBD                   properties <- listA getProperty                                  -< propsTag    -- properties :: [Property]
-- TBD                   returnA   -<  PropDef { pdType  = C propType'
-- TBD                                         , pdId    = propId'
-- TBD                                         , pdName  = propName'
-- TBD                                         , pdDocu  = docu
-- TBD                                         , props   = properties
-- TBD                                         }

-- TBD  getItem :: ArrowXml a => a XmlTree Item
-- TBD  getItem = atTag "item" >>> 
-- TBD      proc l -> do itemId'   <- getAttrValue "identifier"                           -< l
-- TBD                   itemIdRf' <- getAttrValue "identifierref"                        -< l
-- TBD                   labelTag  <- atTag "label"                                       -< l           -- labelTag :: XmlTree
-- TBD                   label'    <- text                                                -< labelTag    -- label :: String
-- TBD                   docuTag   <- atTag "documentation"                               -< l           -- docuTag :: XmlTree
-- TBD                   docu      <- text                                                -< docuTag     -- docu :: String
-- TBD                   subitems  <- listA (getChildren >>> getItem)                     -< l           -- subitems :: [Item]
-- TBD                   returnA  -<  Item { itemId   = itemId'
-- TBD                                     , itemIdRf = itemIdRf'
-- TBD                                     , label    = label'
-- TBD                                     , itemDocu = docu
-- TBD                                     , items    = subitems
-- TBD                                     }

        testSingleton :: Show a => String -> [a] -> a
        testSingleton _    [nm] = nm
        testSingleton kind nms  = fatal ("!Archimate wants one and one only "++kind++"! However, I got "++show (length nms)++": "++show nms)

        testZeroOrOne :: Show a => String -> [a] -> Maybe a
        testZeroOrOne _    [nm] = Just nm
        testZeroOrOne _    []   = Nothing
        testZeroOrOne kind nms  = fatal ("!Archimate wants at most one "++kind++"! However, I got "++show (length nms)++": "++show nms)

        -- | The code of getContent is more verbose than necessary to expose the types in the arrow structure, for documentation purposes.
        getContent :: ArrowXml a => String -> a XmlTree String
        getContent lbl = isElem >>> hasName lbl >>>
            proc l -> do nm      <- (let aap :: ArrowXml a => a XmlTree String ; aap=text in aap)  -< (l :: XmlTree) 
                         returnA -< (nm:: String)

-- Auxiliaries

   atTag :: ArrowXml a => String -> a (NTree XNode) XmlTree
   atTag tag = deep (isElem >>> hasName tag)

   text :: ArrowXml a => a (NTree XNode) String
   text = getChildren >>> getText
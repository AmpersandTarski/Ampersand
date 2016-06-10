{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings #-}
module Main -- Database.Design.Ampersand.FSpec.ArchiAnalyze
where
--   import Database.Design.Ampersand.Core.ParseTree
--   import Database.Design.Ampersand.Core.AbstractSyntaxTree
--   import Database.Design.Ampersand.Basics      (fatal,Collection(..),Named(..))
--   import Database.Design.Ampersand.Classes
--   import Database.Design.Ampersand.ADL1 (insParentheses)
--   import Database.Design.Ampersand.FSpec.FSpec
--   import Prelude
--   import Data.Char

   import System.IO
   import System.Environment
   import System.Exit
   import System.Console.GetOpt
   import Data.List              -- for things such as intercalate
   import Data.Maybe
   import Data.Tree.NTree.TypeDefs
   import Text.XML.HXT.Core
   import Text.Pandoc
   import Text.Pandoc.Builder

   -- This module parses an Archi-repository by means of function `processStraight`.
   -- That produces an object of type `ArchiRepo`
   -- The function `grindArchi` transforms an `ArchiRepo` into binary tables, which is a list of `Pop` object.
   -- The purpose of it all is to load Archimate content into an Ampersand context.

   main :: IO () -- This main program is meant to test the grinding process of an Archi-repository
   main = do archiRepo <- runX (processStraight "CA repository.xml")
             (writeFile "output.html" . writeHtmlString def .  archi2Pandoc . concatMap analyze) archiRepo
             (putStr . intercalate "\n\n" . map show .
              filter (not.null.popPairs) . sortPops .
              grindArchi . identifyProps []) archiRepo
          where sortPops pops = [ (head cl){popPairs = concatMap popPairs cl} | cl<-eqCl f pops ]
                f p = popName p++"["++popSource p++"*"++popTarget p++"]"

-- The following code defines a data structure (called ArchiRepo) that corresponds to an Archi-repository in XML.

   data ArchiRepo = ArchiRepo
     { archRepoName   :: String
     , archRepoId     :: String
     , archFolders    :: [Folder]
     , archProperties :: [Prop]
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
     , elemDocu       :: String
     , elChilds       :: [Child]
     , elProps        :: [Prop]
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

   data Prop = Prop
     { archPropId     :: String
     , archPropKey    :: String
     , archPropVal    :: String
     } deriving (Show, Eq)


-- | Properties in Archimate have no identifying key. In Ampersand, that key is necessary. So the class WithProperties is defined to
--   generate keys for properties, to be inserted in the grinding process. The only data structures with properties in the inner structure
--   of Archi (i.e. in the repository minus the Views), are folders and elements. For this reason, the types ArchiRepo, Folder, and Element
--   are instances of class WithProperties.

   class WithProperties a where
     allProps      :: a -> [Prop]        -- takes all properties from an ArchiRepo, a Folder, or an Element
     identifyProps :: [String] -> a -> a -- distributes identifiers ( [String] ) over an ArchiRepo, a Folder, or an Element, in order to assign a unique identifier to each property in it.

   instance WithProperties ArchiRepo where
     allProps archiRepo = allProps (archFolders archiRepo) ++ archProperties archiRepo
     identifyProps _ archiRepo = archiRepo
       { archProperties = [ prop{archPropId=propId} | (prop,propId)<- zip (archProperties archiRepo) propIds ]
       , archFolders    = identifyProps fldrIds (archFolders archiRepo)
       }
       where
         identifiers = [ "pr-"++show i | i<-[0..] ] -- infinitely many unique keys to identify properties.
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
       { elProps = [ prop{archPropId=propId} | (propId,prop)<- zip identifiers (elProps element) ] }

   instance WithProperties a => WithProperties [a] where
     allProps xs = concatMap allProps xs
     identifyProps identifiers xs
      = [ identifyProps ids x | (ids,x) <- zip idss xs ]
        where
         countProperties :: [Int] -- a list that contains the lengths of property lists in `folder`
         countProperties = map (length.allProps) xs
         idss = distr countProperties identifiers
         distr :: [Int] -> [a] -> [[a]]  -- distribute identifiers in order to allocate them to items in `archiRepo`
         distr (n:ns) identifiers = take n identifiers: distr ns (drop n identifiers)
         distr []     identifiers = []


   showElements archiTypes = intercalate "\n\n" (map showOneArchiType archiTypes)
    where
     showOneArchiType (str,elems)
       = str++"\n"++intercalate "\n" ( nub [ elemId e ++ "\t" ++ elemName e | e<-elems ])

   data Configuration = Conf
     { elements :: [Element]
--     , associations :: [Association]
     }

-- | The following analysis makes the content of an Archi-repository visible.
--   In order to prove that the repository is being read, use this together with archi2Pandoc
--   to display the contents that has been read by `processStraight`.
   analyze :: ArchiRepo -> [(String,[Element])]
   analyze aRepo = [ (elemType (head xs), nub xs) | xs<- eqCl elemType (extractElements aRepo) ]
     where
      extractElements :: ArchiRepo -> [Element]
      extractElements aRepo = concat [ elemsFromFolder folder | folder<-archFolders aRepo ]

      elemsFromFolder :: Folder -> [Element]
      elemsFromFolder folder
       = fldElems folder ++ concat [ elemsFromFolder folder | folder<-fldFolders folder ]

   archi2Pandoc :: [(String,[Element])] -> Pandoc
   archi2Pandoc archiTypes = myDoc
-- [(Inlines, [Blocks])]
     where
       myDoc :: Pandoc
       myDoc = (setTitle "Analyse van Archimate" . doc)
               (  simpleTable [ para "element type", para "number of elements"]                     -- headers
                              [ map (para.text) [(drop 1 . dropWhile (/=':')) label, (show.length) elems]    -- rows
                              | (label,elems)<-archiTypes, takeWhile (/=':') label=="archimate", isConcept label
                              ]
               <> simpleTable [ para "Relationship type", para "number of elements"]                     -- headers
                              [ map (para.text) [(unfixRel . drop 1 . dropWhile (/=':')) label, (show.length) elems]    -- rows
                              | (label,elems)<-archiTypes, takeWhile (/=':') label=="archimate", isRelationship label
                              ]
               <> definitionList [ (text label, showOneArchiType elems) | (label,elems)<-archiTypes ]
               )
       showOneArchiType :: [Element] -> [Blocks]
       showOneArchiType elems
           =  [ para (text (elemName e)) | cl<-eqCl elemId elems, e<-nubBy eqName cl ]
       testOneArchiType :: [Element] -> [Blocks]
       testOneArchiType elems
           =  [ (para.text) ("One Archi-element, " ++ elemId (head cl) ++ ", has multiple names: " ++ intercalate ", " (map elemName cl'))
              | cl<-eqCl elemId elems, let cl'=nubBy eqName cl, length cl'>1 ]
       x `eqName` y = elemName x == elemName y
       unfixRel str = (reverse.drop 1.dropWhile (/='R').reverse) str
       isRelationship str = (reverse.takeWhile (/='R').reverse) str == "elationship"
       isConcept str = not (isRelationship str)


-- The following code generates Ampersand population from an ArchiRepo
   data Pop = Pop { popName ::   String
                  , popSource :: String
                  , popTarget :: String
                  , popPairs ::  [(String,String)]
                  }
            | Comment { comment :: String  -- Not-so-nice way to get comments in a list of populations. Since it is local to this module, it is not so bad, I guess...
                      }

   instance Show Pop where
     show p = popName p++"["++popSource p++"*"++popTarget p++"]"++
              concat [ "\n   ("++x++", "++y++")" | (x,y)<-popPairs p ]

-- | In order to populate an Archi-metamodel with the contents of an Archi-repository,
--   we must grind that contents into binary tables. For that purpose, we define the
--   class MetaArchi, and instantiate it on ArchiRepo and all its constituent types.
   class MetaArchi a where
     grindArchi :: a -> [Pop]  -- create population for a datastructure of type a.
     keyArchi :: a->String     -- get the key value (dirty identifier) of an a.

   instance MetaArchi ArchiRepo where
     grindArchi archiRepo
      = [ Pop "name" "ArchiRepo" "Text"
            [(keyArchi archiRepo, archRepoName archiRepo)]
        , Pop "folders" "ArchiRepo" "Folder"
            (nub [(keyArchi archiRepo, keyArchi folder) | folder<-backendFolders])
        , Pop "properties" "ArchiRepo" "Property"
            (nub [(keyArchi archiRepo, keyArchi property) | property<-archProperties archiRepo])
        ] ++ (concat.map grindArchi) backendFolders
          ++ (concat.map grindArchi.archProperties) archiRepo
        where backendFolders = [ folder | folder<-archFolders archiRepo, fldName folder/="Views"]
        
     keyArchi = archRepoId

   instance MetaArchi Folder where
     grindArchi folder
      = [ Pop "name" "Folder" "Text"
            [(keyArchi folder, fldName folder)]
        , Pop "type" "Folder" "Text"
            [(keyArchi folder, fldType folder) | (not.null.fldType) folder]
        , Pop "elements" "Folder" "Element"
            (nub [(keyArchi folder, keyArchi element) | element<-fldElems folder])
        , Pop "folders" "Folder" "Folder"
            (nub [(keyArchi folder, keyArchi subFolder) | subFolder<-fldFolders folder])
        ] ++ (concat.map grindArchi.fldElems)   folder
          ++ (concat.map grindArchi.fldFolders) folder
     keyArchi = fldId

   instance MetaArchi Element where
     grindArchi element
      = [ Pop "type" "Element" "Text"   -- Archi ensures totality (I hope...)
            [(keyArchi element, elemType element)]
        , Pop "name" "Element" "Text"
            [(keyArchi element, elemName element) | (not.null.elemName) element]
        , Pop "docu" "Element" "Text"
            [(keyArchi element, elemDocu element) | (not.null.elemDocu) element]
        , Pop "childs" "Element" "Property"
            (nub [(keyArchi element, keyArchi property) | property<-elProps element])
        ] ++ (concat.map grindArchi.elProps) element
     keyArchi = elemId

   instance MetaArchi Prop where
     grindArchi property
      = [ Pop "key" "Property" "Text"
            [(keyArchi property, archPropKey property)]
        , Pop "val" "Property" "Text"
            [(keyArchi property, archPropVal property)]
        ]
     keyArchi = archPropId

   instance MetaArchi a => MetaArchi [a] where
     grindArchi xs = concat [ grindArchi x | x<-xs ]
     keyArchi = error "fatal 269: cannot use keyArchi on a list"


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
            proc l -> do repoNm    <- getAttrValue "name"               -< l
                         repoId    <- getAttrValue "id"                 -< l
                         folders   <- listA (getChildren >>> getFolder) -< l
                         props     <- listA (getChildren >>> getProp)   -< l
                         returnA -< ArchiRepo { archRepoName   = repoNm
                                              , archRepoId     = repoId
                                              , archFolders    = folders
                                              , archProperties = [ prop{archPropId="pr-"++show i} | (prop,i)<- zip props [length (allProps folders)..] ]
                                              }

        getFolder :: ArrowXml a => a XmlTree Folder
        getFolder
         = isElem >>> hasName "folders" >>>
            proc l -> do fldNm   <- getAttrValue "name"                 -< l
                         fldId   <- getAttrValue "id"                   -< l
                         fldType <- getAttrValue "type"                 -< l
                         elems   <- listA (getChildren >>> getElement)  -< l
                         subFlds <- listA (getChildren >>> getFolder)   -< l
                         returnA -< Folder { fldName    = fldNm
                                           , fldId      = fldId
                                           , fldType    = fldType
                                           , fldElems   = elems
                                           , fldFolders = subFlds
                                           }

        getProp :: ArrowXml a => a XmlTree Prop
        getProp = isElem >>> hasName "properties" >>>
            proc l -> do propKey <- getAttrValue "key"   -< l
                         propVal <- getAttrValue "value" -< l
                         returnA -< Prop { archPropKey = propKey
                                         , archPropId  = error "fatal 315: archPropId not yet defined"
                                         , archPropVal = propVal
                                         }

        getElement :: ArrowXml a => a XmlTree Element
        getElement = atTag "elements" >>>
            proc l -> do elemType  <- getAttrValue "xsi:type"           -< l
                         elemId    <- getAttrValue "id"                 -< l
                         elemName  <- getAttrValue "name"               -< l
                         elemDocu  <- getAttrValue "documentation"      -< l
                         childs    <- listA (getChildren >>> getChild)  -< l
                         props     <- listA (getChildren >>> getProp)   -< l
                         returnA   -< Element  { elemType = elemType
                                               , elemId   = elemId
                                               , elemName = elemName
                                               , elemDocu = elemDocu
                                               , elChilds = childs
                                               , elProps  = props
                                               }

        getRelation :: ArrowXml a => a XmlTree Relation
        getRelation = isElem >>> hasName "relationship" >>>
            proc l -> do relType    <- getAttrValue "xsi:type"          -< l
                         relHref    <- getAttrValue "href"              -< l
                         returnA    -< Relation{ relType = relType
                                               , relHref = relHref
                                               }

        getBound :: ArrowXml a => a XmlTree Bound
        getBound = isElem >>> hasName "bounds" >>>
            proc l -> do bnd_x      <- getAttrValue "x"                 -< l
                         bnd_y      <- getAttrValue "y"                 -< l
                         bnd_width  <- getAttrValue "width"             -< l
                         bnd_height <- getAttrValue "height"            -< l
                         returnA    -< Bound   { bnd_x      = bnd_x
                                               , bnd_y      = bnd_y
                                               , bnd_width  = bnd_width
                                               , bnd_height = bnd_height
                                               }

        getSrcConn :: ArrowXml a => a XmlTree SourceConnection
        getSrcConn = isElem >>> hasName "sourceConnections" >>>
            proc l -> do sConType   <- getAttrValue "xsi:type"          -< l
                         sConId     <- getAttrValue "id"                -< l
                         sConSrc    <- getAttrValue "source"            -< l
                         sConTgt    <- getAttrValue "target"            -< l
                         sConRel    <- getAttrValue "relationship"      -< l
                         sConRelat  <- listA (getChildren>>>getRelation)-< l
                         bendPts    <- listA (getChildren>>>getBendPt)  -< l
                         returnA    -< SrcConn { sConType  = sConType
                                               , sConId    = sConId
                                               , sConSrc   = sConSrc
                                               , sConTgt   = sConTgt
                                               , sConRel   = sConRel
                                               , sConRelat = sConRelat
                                               , sCbendPts = bendPts
                                               }

        getBendPt :: ArrowXml a => a XmlTree BendPoint
        getBendPt = isElem >>> hasName "bendpoints" >>>
            proc l -> do bpStartX   <- getAttrValue "startX"            -< l
                         bpStartY   <- getAttrValue "startY"            -< l
                         bpEndX     <- getAttrValue "endX"              -< l
                         bpEndY     <- getAttrValue "endY"              -< l
                         returnA    -< BendPt  { bpStartX  = bpStartX
                                               , bpStartY  = bpStartY
                                               , bpEndX    = bpEndX  
                                               , bpEndY    = bpEndY  
                                               }

        getChild
         = atTag "children" >>>
            proc l -> do chldType <- getAttrValue "xsi:type"            -< l
                         chldId   <- getAttrValue "id"                  -< l
                         chldName <- getAttrValue "name"                -< l
                         chldFCol <- getAttrValue "fillColor"           -< l
                         chldAlgn <- getAttrValue "textAlignment"       -< l
                         chldElem <- getAttrValue "archimateElement"    -< l
                         trgtConn <- getAttrValue "targetConnections"   -< l
                         bound    <- getChildren >>> getBound           -< l
                         srcConns <- listA (getChildren >>> getSrcConn) -< l
                         childs   <- listA (getChildren >>> getChild)   -< l
                         returnA  -< Child { chldType = chldType
                                           , chldId   = chldId
                                           , chldAlgn = chldAlgn
                                           , chldFCol = chldFCol
                                           , chldElem = chldElem
                                           , trgtConn = trgtConn
                                           , bound    = bound
                                           , srcConns = srcConns
                                           , childs   = childs
                                           }

-- Auxiliaries

   atTag :: ArrowXml a => String -> a (NTree XNode) XmlTree
   atTag tag = deep (isElem >>> hasName tag)

-- | eqCl is used for gathering things that are equal wrt some criterion f.
--   For instance, if you want to have persons with the same name:
--    'eqCl name persons' produces a list,in which each element is a list of persons with the same name.
-- Example> eqCl (=='s') "Mississippi" = "ssss"

   eqCl :: Eq b => (a -> b) -> [a] -> [[a]]
   eqCl _ [] = []
   eqCl f (x:xs) = (x:[e |e<-xs, f x==f e]) : eqCl f [e |e<-xs, f x/=f e]
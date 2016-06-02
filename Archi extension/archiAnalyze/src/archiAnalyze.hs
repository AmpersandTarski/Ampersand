{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings #-}
module Main where
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

   -- This example demonstrates a more complex XML parse,
   -- involving multiple levels, attributes, inner lists,
   -- and dealing with optional data.

   main :: IO ()
   main = do archiRepo <- runX (processStraight "CA repository.xml")
             (writeFile "output.html" . writeHtmlString def . archi2Pandoc . concatMap analyze) archiRepo

   showElements archiTypes = intercalate "\n\n" (map showOneArchiType archiTypes)
    where
     showOneArchiType (str,elems)
       = str++"\n"++intercalate "\n" ( nub [ elemId e ++ "\t" ++ elemName e | e<-elems ])

   data Configuration = Conf
     { elements :: [Element]
--     , associations :: [Association]
     }

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

{-
chptHeader lang chap
 = header 1 (chptTitle lang chap ) <> (para (xrefLabel chap))

chptTitle :: Lang -> Chapter -> Inlines
chptTitle lang cpt =
     (case (cpt,lang) of
        (Intro                 , Dutch  ) -> text "Inleiding"
        (Intro                 , English) -> text "Introduction"
-}

-- The following code derives a data structure (called ArchiRepo) from an Archi-XML-file.

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
     { archPropKey    :: String
     , archPropVal    :: String
     } deriving (Show, Eq)

   data Element = Element
     { elemType       :: String
     , elemId         :: String
     , elemName       :: String
     , elemDocu       :: String
     , elChilds       :: [Child]
     , elProps        :: [Prop]
     } deriving (Show, Eq)

{- inspiration for adding the data structure to XML conversion ->
   main = do [rc] <- runX (processStruct "CArepo.xml" >>> getErrStatus)
             exitWith ( if rc >= c_err
                        then ExitFailure (-1)
                        else ExitSuccess
                      )

   xmlArchiRepo :: ArrowXml a => ArchiRepo -> a XmlTree XmlTree
   xmlArchiRepo archiRepo
       = root [] [mkelem "archimate:ArchimateModel"
                         [ sattr "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance"
                         , sattr "xmlns:archimate" "http://www.archimatetool.com/archimate"
                         , sattr "name" (archRepoName archiRepo)
                         , sattr "id" (archRepoId   archiRepo)
                         ]
                         [ txt "to be done" ]
                 ]

-- processStruct shows how to process an Archimate file into an HTML-list of elements without defining an intermediary data structure.
   processStruct outfile
    = xmlArchiRepo archiRepo
      >>>
      writeDocument [withIndent yes] outfile
-}

-- processStraight shows how to process an Archimate file into an HTML-list of elements without defining an intermediary data structure.
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
                                              , archProperties = props
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
                                         , archPropVal = propVal
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


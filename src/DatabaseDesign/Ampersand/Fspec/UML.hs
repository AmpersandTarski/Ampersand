module DatabaseDesign.Ampersand.Fspec.UML where

import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec.Graphic.ClassDiagram
import DatabaseDesign.Ampersand.Fspec

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

-- TODO: escape
-- TODO: names of model, package, assoc (empty?), etc.

generateUML :: Fspc -> Options -> String
generateUML fSpec opts = showUML (genUMLClassDiag $ cdAnalysis fSpec opts)

data UMLState = UMLState { idCounter :: Int
                         , labelIdMap :: Map String String
                         , diagramEltIds :: [String]
                         }
                         
type StateUML a = State UMLState a
                           

type UML = StateUML [String]

showUML :: UML -> String
showUML uml = unlines $ evalState uml $ UMLState 0 Map.empty []
{-OOclassdiagram {classes     :: [Class]            --
                                   ,assocs      :: [Association]      --
                                   ,aggrs       :: [Aggregation]      --
                                   ,geners      :: [Generalization]   --
                                   ,nameandcpts :: (String,[A_Concept])}-}
genUMLClassDiag :: ClassDiag -> UML
genUMLClassDiag (OOclassdiagram classes assocs _ _ (contextName, allConcepts)) =
 do { packageId <- getUnlabeledId "Package" 
    ; diagramId <- getUnlabeledId "Diagram" 
    ; datatypesUML <- mapM genUMLDatatype $ map name allConcepts >- [ name | OOClass name _ _ <-  classes ] 
    ; classesUML <- mapM genUMLClass $ classes 
    ; assocsUML <- mapM genUMLAssociation $ {- [OOAssoc "Document" "" "" "Dossier" "" ""] -- -} assocs 
    ; diagramElements <- genDiagramElements
    ; return $ [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
               , "<xmi:XMI xmi:version=\"2.1\" xmlns:uml=\"http://schema.omg.org/spec/UML/2.1\" xmlns:xmi=\"http://schema.omg.org/spec/XMI/2.1\">"
               , " <xmi:Documentation exporter=\"Enterprise Architect\" exporterVersion=\"6.5\"/>"
               , " <uml:Model xmi:type=\"uml:Model\" name=\""++contextName++"\" visibility=\"public\">"
               , "  <packagedElement xmi:type=\"uml:Package\" xmi:id=\""++packageId++"\" name=\""++contextName++"\" visibility=\"public\">" ] ++
               concat datatypesUML ++
               concat classesUML ++ 
               concat assocsUML ++ 
               [ "  </packagedElement>"
               , " </uml:Model>"
               , " <xmi:Extension>"
               , "  <diagrams>"
               , "   <diagram xmi:id=\""++diagramId++"\">"
               , "    <model package=\""++packageId++"\" owner=\""++packageId++"\"/>"
               , "    <properties name=\"Data Model\" type=\"Logical\"/>"
               , "    <elements>" ] ++
               diagramElements ++
               [ "    </elements>"
               , "   </diagram>"
               , "  </diagrams>"
               , " </xmi:Extension>"
               , "</xmi:XMI>" ]
    }
genUMLDatatype :: String -> UML
genUMLDatatype name =
 do { classId <- getLabeledId "Class" name
    ; addToDiagram classId
    ; return $ [ "   <packagedElement xmi:type=\"uml:DataType\" xmi:id=\""++classId++"\" name=\""++name++"\" visibility=\"public\"/> " ]
    }

genUMLClass :: Class -> UML
genUMLClass (OOClass name attrs methods) =
 do { classId <- getLabeledId "Class" name
    ; addToDiagram classId
    ; attributesUML <- mapM genUMAttribute attrs
    ; return $ [ "   <packagedElement xmi:type=\"uml:Class\" xmi:id=\""++classId++"\" name=\""++name++"\" visibility=\"public\">"] ++
               concat attributesUML ++
               [ "   </packagedElement>"]
    }

genUMAttribute :: Attribute -> UML
genUMAttribute  (OOAttr name attrType leftOpen) =
 do { attrId <- getUnlabeledId "Attr"
    ; classId <- getLabeledId "Class" attrType
    ; return [ "      <ownedAttribute xmi:type=\"uml:Property\" xmi:id=\""++attrId++"\" name=\""++name++"\" visibility=\"public\" isStatic=\"false\""++
                                    " isReadOnly=\"false\" isDerived=\"false\" isOrdered=\"false\" isUnique=\"true\" isDerivedUnion=\"false\">"
             , "       <lowerValue xmi:type=\"uml:LiteralInteger\" xmi:id=\"INTID1\" value=\"1\"/>"
             , "       <upperValue xmi:type=\"uml:LiteralInteger\" xmi:id=\"INTID2\" value=\"1\"/>"
             , "       <type xmi:idref=\""++classId++"\"/>"
             , "      </ownedAttribute>"]
    }

genUMLAssociation :: Association -> UML
genUMLAssociation (OOAssoc lClass lMults lRole rClass rMults rRole) =
 do { assocId <- getUnlabeledId "Assoc"
    ; lClassId <- getLabeledId "Class" lClass
    ; rClassId <- getLabeledId "Class" rClass
    ; lEndId <- getUnlabeledId "MemberEnd"
    ; rEndId <- getUnlabeledId "MemberEnd"
    ; return $
        [ "   <packagedElement xmi:type=\"uml:Association\" xmi:id=\""++assocId++"\" name=\""++rRole++"\" visibility=\"public\">"
        , "    <memberEnd xmi:idref=\""++lEndId++"\"/>"
        , "    <memberEnd xmi:idref=\""++rEndId++"\"/>"
        ] ++
        genOwnedEnd assocId lEndId lClassId ++
        genOwnedEnd assocId rEndId rClassId ++
        [ "   </packagedElement>"
        ]
    }
 where genOwnedEnd assocId endId classId =
         [ "    <ownedEnd xmi:type=\"uml:Property\" xmi:id=\""++endId++"\" visibility=\"public\" association=\""++assocId++"\" isStatic=\"false\" isReadOnly=\"false\" isDerived=\"false\" isOrdered=\"false\" isUnique=\"true\" isDerivedUnion=\"false\" aggregation=\"none\">"
         , "     <type xmi:idref=\""++classId++"\"/>"
         , "    </ownedEnd>"
         ]
                     
genDiagramElements :: UML
genDiagramElements =
 do { state <- get
    ; return [ "     <element subject=\""++elementId++"\"/>" | elementId <- diagramEltIds state ]
    }

addToDiagram :: String -> StateUML ()
addToDiagram elementId =
 do { modify $ \state -> state { diagramEltIds = elementId : diagramEltIds state} 
    }
    
getUnlabeledId :: String -> StateUML String
getUnlabeledId tag =
 do { idC <- gets idCounter 
    ; modify $ \state -> state { idCounter = idCounter state + 1}
    ; let classId = tag++"ID_"++show idC 
    ; return classId
    }

getLabeledId :: String -> String -> StateUML String
getLabeledId tag label =
 do { lidMap <- gets labelIdMap
    ; classId <- case Map.lookup label lidMap of
                   Just lid -> return lid
                   Nothing  -> do { let classId = tag++"ID_"++label 
                                  ; modify $ \state -> state { labelIdMap =  Map.insert label classId (labelIdMap state) }
                                  ; return classId
                                  }
    ; return classId
    }
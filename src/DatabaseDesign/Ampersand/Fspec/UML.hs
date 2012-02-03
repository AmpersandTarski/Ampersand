module DatabaseDesign.Ampersand.Fspec.UML (generateUML) where

import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec.Graphic.ClassDiagram
import DatabaseDesign.Ampersand.Fspec

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

fatal :: Int -> String -> a
fatal = fatalMsg "UML"

-- TODO: escape
-- TODO: names of model, package, assoc (empty?), etc.

generateUML :: Fspc -> Options -> String
generateUML fSpec opts = showUML (genUMLClassDiag $ cdAnalysis fSpec opts)

showUML :: UML -> String
showUML uml = unlines $ evalState uml $ UMLState 0 Map.empty []

genUMLClassDiag :: ClassDiag -> UML
genUMLClassDiag (OOclassdiagram classes assocs _ _ (contextName, allConcepts)) =
 do { packageId <- mkUnlabeledId "Package" 
    ; diagramId <- mkUnlabeledId "Diagram"
    
    ; sequence $ map (mkLabeledId "Datatype") datatypeNames
    ; sequence $ map (mkLabeledId "Class") classNames

    ; datatypesUML <- mapM genUMLDatatype datatypeNames
    ; classesUML <- mapM genUMLClass classes 
    ; assocsUML <- mapM genUMLAssociation assocs 
    ; diagramElements <- genDiagramElements
    ; return $ [ "<?xxxml version=\"1.0\" encoding=\"UTF-8\"?>"
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
 where classNames    = [ name | OOClass name _ _ <-  classes ]
       datatypeNames = map name allConcepts >- classNames
       
genUMLDatatype :: String -> UML
genUMLDatatype name =
 do { datatypeId <- refLabeledId name
    ; addToDiagram datatypeId
    ; return $ [ "   <packagedElement xmi:type=\"uml:DataType\" xmi:id=\""++datatypeId++"\" name=\""++name++"\" visibility=\"public\"/> " ]
    }

genUMLClass :: Class -> UML
genUMLClass (OOClass name attrs methods) =
 do { classId <- refLabeledId name
    ; addToDiagram classId
    ; attributesUML <- mapM genUMAttribute attrs
    ; return $ [ "   <packagedElement xmi:type=\"uml:Class\" xmi:id=\""++classId++"\" name=\""++name++"\" visibility=\"public\">"] ++
               concat attributesUML ++
               [ "   </packagedElement>"]
    }

genUMAttribute :: Attribute -> UML
genUMAttribute  (OOAttr name attrType leftOpen) =
 do { attrId <- mkUnlabeledId "Attr"
    ; lIntId <- mkUnlabeledId "Int"
    ; uIntId <- mkUnlabeledId "Int"
    ; classId <- refLabeledId attrType
    ; return [ "      <ownedAttribute xmi:type=\"uml:Property\" xmi:id=\""++attrId++"\" name=\""++name++"\" visibility=\"public\" isStatic=\"false\""++
                                    " isReadOnly=\"false\" isDerived=\"false\" isOrdered=\"false\" isUnique=\"true\" isDerivedUnion=\"false\">"
             , "       <lowerValue xmi:type=\"uml:LiteralInteger\" xmi:id=\""++lIntId++"\" value=\"1\"/>"
             , "       <upperValue xmi:type=\"uml:LiteralInteger\" xmi:id=\""++uIntId++"\" value=\"1\"/>"
             , "       <type xmi:idref=\""++classId++"\"/>"
             , "      </ownedAttribute>"]
    }

genUMLAssociation :: Association -> UML
genUMLAssociation (OOAssoc lClass lMults lRole rClass rMults rRole) =
 do { assocId <- mkUnlabeledId "Assoc"
    ; lClassId <- refLabeledId lClass
    ; rClassId <- refLabeledId rClass
    ; lEndId <- mkUnlabeledId "MemberEnd"
    ; rEndId <- mkUnlabeledId "MemberEnd"
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
         [ "    <ownedEnd xmi:type=\"uml:Property\" xmi:id=\""++endId++"\" visibility=\"public\" association=\""++assocId++"\" isStatic=\"false\""++
                        " isReadOnly=\"false\" isDerived=\"false\" isOrdered=\"false\" isUnique=\"true\" isDerivedUnion=\"false\" aggregation=\"none\">"
         , "     <type xmi:idref=\""++classId++"\"/>"
         , "    </ownedEnd>"
         ]
                     
genDiagramElements :: UML
genDiagramElements =
 do { elementIds <- gets diagramEltIds
    ; return [ "     <element subject=\""++elementId++"\"/>" | elementId <- elementIds ]
    }


-- State and Monad

data UMLState = UMLState { idCounter :: Int
                         , labelIdMap :: Map String String
                         , diagramEltIds :: [String]
                         }
                         
type StateUML a = State UMLState a
                           

type UML = StateUML [String]


addToDiagram :: String -> StateUML ()
addToDiagram elementId =
 do { modify $ \state -> state { diagramEltIds = elementId : diagramEltIds state} 
    }
    
mkUnlabeledId :: String -> StateUML String
mkUnlabeledId tag =
 do { idC <- gets idCounter 
    ; modify $ \state -> state { idCounter = idCounter state + 1}
    ; let unlabeledId = tag++"ID_"++show idC 
    ; return unlabeledId
    }

refLabeledId :: String -> StateUML String
refLabeledId label =
 do { lidMap <- gets labelIdMap
    ; labeledId <- case Map.lookup label lidMap of
                   Just lid -> return lid
                   Nothing  -> fatal 147 $ "Requesting non-existent label "++label
    ; return labeledId
    }
    
mkLabeledId :: String -> String -> StateUML ()
mkLabeledId tag label =
 do { let classId = tag++"ID_"++label 
    ; modify $ \state -> state { labelIdMap =  Map.insert label classId (labelIdMap state) }
    }
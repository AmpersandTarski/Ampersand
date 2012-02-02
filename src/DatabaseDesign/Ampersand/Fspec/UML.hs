module DatabaseDesign.Ampersand.Fspec.UML where

import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec.Graphic.ClassDiagram
import DatabaseDesign.Ampersand.Fspec

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

-- TODO: no IO
-- TODO: escape
-- TODO: memberEnd vs ownedEnd?
-- TODO: names of model, package, assoc (empty?), etc.
-- TODO: fix id's (maybe put ids in ClassDiag type)

generateUML :: Fspc -> Options -> IO String
generateUML fSpec opts =
    do { return $ showUML (genUMLClassDiag $ cdAnalysis fSpec opts) 
       }

type StateUML a = State (Int, Map (String,String) String) a
-- Simple state monad for maintaining counter for id's and keeping track of class id's. 

type UML = StateUML [String]

showUML :: UML -> String
showUML uml = unlines $ evalState uml (0, Map.empty)

genUMLClassDiag :: ClassDiag -> UML
genUMLClassDiag diagram =
 do { classesUML <- mapM genUMLClass $ classes diagram 
    ; assocsUML <- mapM genUMLAssociation $ {- [OOAssoc "Document" "" "" "Dossier" "" ""] -- -} assocs diagram 
    ; diagramElements <- genDiagramElements
    ; return $ [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
               , "<xmi:XMI xmi:version=\"2.1\" xmlns:uml=\"http://schema.omg.org/spec/UML/2.1\" xmlns:xmi=\"http://schema.omg.org/spec/XMI/2.1\">"
               , " <xmi:Documentation exporter=\"Enterprise Architect\" exporterVersion=\"6.5\"/>"
               , " <uml:Model xmi:type=\"uml:Model\" name=\"Ampersand Import\" visibility=\"public\">"
               , "  <packagedElement xmi:type=\"uml:Package\" xmi:id=\"PACKAGEID\" name=\"INDOORS\" visibility=\"public\">" ] ++
               concat classesUML ++ 
               concat assocsUML ++ 
               [ "  </packagedElement>"
               , " </uml:Model>"
               , " <xmi:Extension>"
               , "  <diagrams>"
               , "   <diagram xmi:id=\"DIAGRAMID\">"
               , "    <model package=\"PACKAGEID\" owner=\"PACKAGEID\"/>"
               , "    <properties name=\"Diagramm\" type=\"Logical\"/>"
               , "    <elements>"
               ] ++
               diagramElements ++
               [ "    </elements>"
               , "   </diagram>"
               , "  </diagrams>"
               , " </xmi:Extension>"
               , "</xmi:XMI>"
                ]
    }

genUMLClass :: Class -> UML
genUMLClass  (OOClass name attrs methods) =
 do { classId <- getClassId name 
    ; return ["   <packagedElement xmi:type=\"uml:Class\" xmi:id=\""++classId++"\" name=\""++name++"\" visibility=\"public\"/>"]
    }

genUMLAssociation :: Association -> UML
genUMLAssociation (OOAssoc lClass lMults lRole rClass rMults rRole) =
 do { assocId <- getAssocId $ lClass ++ rClass
    ; lClassId <- getClassId lClass
    ; rClassId <- getClassId rClass
    ; lEndId <- getGenericId
    ; rEndId <- getGenericId
    ; lUpperId <- getGenericId
    ; lUpperId <- getGenericId
    ; lLowerId <- getGenericId
    ; rUpperId <- getGenericId
    ; rLowerId <- getGenericId
    ; return $
        [ "   <packagedElement xmi:type=\"uml:Association\" xmi:id=\""++assocId++"\" name=\"\" visibility=\"public\">"
        , "    <memberEnd xmi:idref=\""++lEndId++"\"/>"
        , "    <memberEnd xmi:idref=\""++rEndId++"\"/>"
        ] ++
        genOwnedEnd assocId lEndId lClassId ++
        genOwnedEnd assocId rEndId rClassId ++
        
       {-  UML 1
        , "    <ownedEnd xmi:id=\""++lEndId++"\" name=\"klant\" type=\""++lClassId++"\" association=\""++assocId++"\" visibility=\"public\">"
        , "     <upperValue xmi:type=\"uml:LiteralUnlimitedNatural\" xmi:id=\""++lUpperId++"\" value=\"1\"/>"
        , "     <lowerValue xmi:type=\"uml:LiteralInteger\" xmi:id=\""++lLowerId++"\" value=\"1\"/>"
        , "    </ownedEnd>"
        , "    <ownedEnd xmi:id=\""++rEndId++"\" name=\"leverancier\" type=\""++rClassId++"\" association=\""++assocId++"\" visibility=\"public\">"
        , "     <upperValue xmi:type=\"uml:LiteralUnlimitedNatural\" xmi:id=\""++rUpperId++"\" value=\"1\"/>"
        , "     <lowerValue xmi:type=\"uml:LiteralInteger\" xmi:id=\""++rLowerId++"\" value=\"1\"/>"
        , "    </ownedEnd>" -}
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
 do { (_, classIdMap) <- get
    ; return [ "     <element subject=\""++elementId++"\"/>" | elementId <- Map.elems classIdMap ]
    }

getGenericId :: StateUML String
getGenericId =
 do { (idCounter, classIdMap) <- get
    ; let classId = "Generic_"++show idCounter 
    ; put (idCounter + 1, classIdMap)
    ; return classId
    }
    
getClassId name = getId "Class" name
getAssocId name = getId "Assoc" name
--getId name = getId "" name
--getId name = getId "" name
--getId name = getId "" name

getId :: String -> String -> StateUML String
getId tag name =
 do { (idCounter, classIdMap) <- get
    ; classId <- case Map.lookup (tag,name) classIdMap of
                   Just cid -> return cid
                   Nothing  -> do { let classId = tag++"_"++show idCounter 
                                  ; put (idCounter + 1, Map.insert (tag,name) classId classIdMap)
                                  ; return classId
                                  }
    ; return classId
    }
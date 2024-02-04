module Ampersand.FSpec.GenerateUML (generateUML) where

import Ampersand.ADL1
import Ampersand.Basics
--TODO: Replace by RIO state
import Ampersand.Basics.BuildInfo_Generated (cabalVersionStr)
import Ampersand.FSpec
import Ampersand.Graphic.ClassDiagram
import Ampersand.Graphic.Fspec2ClassDiagrams
import Ampersand.Misc.HasClasses
import Ampersand.Output.PandocAux
import Control.Monad.State.Lazy (State, evalState, gets, modify)
import qualified RIO.Map as Map
import qualified RIO.Text as T

-- TODO: escape
-- TODO: names of model, package, assoc (empty?), etc.

generateUML :: (HasOutputLanguage env) => env -> FSpec -> Text
generateUML env = showUML . fSpec2UML env

showUML :: UML -> Text
showUML uml = T.unlines $ evalState uml $ UMLState 0 Map.empty [] []

fSpec2UML :: (HasOutputLanguage env) => env -> FSpec -> UML
fSpec2UML env fSpec =
  do
    packageId0 <- mkUnlabeledId "TopPackage"
    packageId1 <- mkUnlabeledId "PackageClasses"
    packageId2 <- mkUnlabeledId "PackageReqs"
    diagramId <- mkUnlabeledId "Diagram"

    mapM_ (mkLabeledId "Datatype" . fullName) datatypeNames
    mapM_ (mkLabeledId "Class" . fullName) classNames

    datatypesUML <- mapM genUMLDatatype datatypeNames
    classesUML <- mapM genUMLClass (classes classDiag)
    assocsUML <- mapM genUMLAssociation (assocs classDiag)
    requirementsUML <- mapM genUMLRequirement (requirements env fSpec)
    diagramElements <- genDiagramElements
    customProfileElements <- genCustomProfileElements
    customReqElements <- genCustomReqElements env fSpec packageId2
    return $ -- The following XMI-template has been borrowed from Enterprise Architect vs. 6.5
      [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
        "<!-- Generated by " <> longVersion appVersion <> " -->",
        "<xmi:XMI xmi:version=\"2.1\" xmlns:uml=\"http://schema.omg.org/spec/UML/2.1\" xmlns:xmi=\"http://schema.omg.org/spec/XMI/2.1\" xmlns:thecustomprofile=\"http://www.sparxsystems.com/profiles/thecustomprofile/1.0\">",
        -- WHY is the exporter not something like `Ampersand` (in the string below)?
        -- BECAUSE then for some reason the importer doesn't show the properties of the requirements.
        " <xmi:Documentation exporter=\"Ampersand\" exporterVersion=\"" <> cabalVersionStr <> "\"/>",
        " <uml:Model xmi:type=\"uml:Model\" name=\"" <> fullName contextName <> "\" visibility=\"public\">",
        "  <packagedElement xmi:type=\"uml:Package\" xmi:id=" <> tshow packageId0 <> " name=" <> fullName contextName <> " visibility=\"public\">"
      ]
        <> ["   <packagedElement xmi:type=\"uml:Package\" xmi:id=" <> tshow packageId1 <> " name=" <> fullName (prependToPlainName "classesOf_" contextName) <> " visibility=\"public\">"]
        <> concat datatypesUML
        <> concat classesUML
        <> concat assocsUML
        <> ["   </packagedElement>"]
        <> ["   <packagedElement xmi:type=\"uml:Package\" xmi:id=" <> tshow packageId2 <> " name=" <> fullName (prependToPlainName "RequirementsOf_" contextName) <> " visibility=\"public\">"]
        <> concat requirementsUML
        <> ["   </packagedElement>"]
        <> ["  </packagedElement>"]
        <> customProfileElements
        <> [ " </uml:Model>",
             " <xmi:Extension extender=\"Enterprise Architect\" extenderID=\"6.5\">",
             "  <elements>"
           ]
        <> ["    <element xmi:idref=" <> tshow packageId0 <> " xmi:type=\"uml:Package\" name=" <> fullName contextName <> " scope=\"public\">"]
        <> ["    </element>"]
        <> ["    <element xmi:idref=" <> tshow packageId1 <> " xmi:type=\"uml:Package\" name=" <> fullName (prependToPlainName "classesOf_" contextName) <> " scope=\"public\">"]
        <> ["     <model package2=" <> tshow packageId1 <> " package=" <> tshow packageId0 <> " tpos=\"0\" ea_eleType=\"package\"/>"]
        <> ["    </element>"]
        <> ["    <element xmi:idref=" <> tshow packageId2 <> " xmi:type=\"uml:Package\" name=" <> fullName (prependToPlainName "RequirementsOf_" contextName) <> " scope=\"public\">"]
        <> ["     <model package2=" <> tshow packageId2 <> " package=" <> tshow packageId0 <> " tpos=\"0\" ea_eleType=\"package\"/>"]
        <> ["    </element>"]
        <> customReqElements
        <> [ "  </elements>",
             "  <diagrams>",
             "   <diagram xmi:id=\"" <> diagramId <> "\">",
             "    <model package=\"" <> packageId1 <> "\" owner=\"" <> packageId1 <> "\"/>",
             "    <properties name=\"Data Model\" type=\"Logical\"/>",
             "    <elements>"
           ]
        <> diagramElements
        <> [ "    </elements>",
             "   </diagram>",
             "  </diagrams>",
             " </xmi:Extension>",
             "</xmi:XMI>"
           ]
  where
    classDiag = cdAnalysis False fSpec fSpec
    contextName = name classDiag
    allConcs = ooCpts classDiag
    classNames = map name (classes classDiag)
    datatypeNames :: [Name]
    datatypeNames = filter (`notElem` classNames) $ map name allConcs

genUMLRequirement :: Req -> UML
genUMLRequirement req =
  do
    reqLId <- mkUnlabeledId "Req"
    addReqToState (reqLId, req)
    return ["    <packagedElement xmi:type=\"uml:Class\" xmi:id=\"" <> reqLId <> "\" name=\"" <> reqId req <> "\" visibility=\"public\"/> "]

genUMLDatatype :: Name -> UML
genUMLDatatype nm =
  do
    datatypeId <- refLabeledId . fullName $ nm
    addToDiagram datatypeId
    return ["    <packagedElement xmi:type=\"uml:DataType\" xmi:id=\"" <> datatypeId <> "\" name=\"" <> fullName nm <> "\" visibility=\"public\"/> "]

genUMLClass :: Class -> UML
genUMLClass cl =
  do
    classId <- refLabeledId . fullName $ clName cl
    addToDiagram classId
    attributesUML <- mapM genUMAttribute (clAtts cl)
    return $
      ["    <packagedElement xmi:type=\"uml:Class\" xmi:id=\"" <> classId <> "\" name=\"" <> fullName (clName cl) <> "\" visibility=\"public\">"]
        <> concat attributesUML
        <> ["    </packagedElement>"]

genUMAttribute :: CdAttribute -> UML
genUMAttribute (OOAttr nm attrType isOptional) =
  do
    attrId <- mkUnlabeledId "Attr"
    lIntId <- mkUnlabeledId "Int"
    uIntId <- mkUnlabeledId "Int"
    classId <- refLabeledId . fullName $ attrType
    return
      [ "       <ownedAttribute xmi:type=\"uml:Property\" xmi:id=\"" <> attrId <> "\" name=\"" <> fullName nm <> "\" visibility=\"public\" isStatic=\"false\""
          <> " isReadOnly=\"false\" isDerived=\"false\" isOrdered=\"false\" isUnique=\"true\" isDerivedUnion=\"false\">",
        "        <type xmi:idref=\"" <> classId <> "\"/>",
        "        <lowerValue xmi:type=\"uml:LiteralInteger\" xmi:id=\"" <> lIntId <> "\" value=\"" <> (if isOptional then "0" else "1") <> "\"/>",
        "        <upperValue xmi:type=\"uml:LiteralInteger\" xmi:id=\"" <> uIntId <> "\" value=\"1\"/>",
        "       </ownedAttribute>"
      ]

genUMLAssociation :: Association -> UML
genUMLAssociation ass =
  do
    assocId <- mkUnlabeledId "Assoc"
    lMemberAndOwnedEnd <- genMemberAndOwnedEnd (asslhm ass) assocId (fullName $ assSrc ass)
    rMemberAndOwnedEnd <- genMemberAndOwnedEnd (assrhm ass) assocId (fullName $ assTgt ass)
    return $
      [ "    <packagedElement xmi:type=\"uml:Association\" xmi:id=\"" <> assocId <> "\" name=\"" <> maybe "" fullName (assrhr ass) <> "\" visibility=\"public\">"
      ]
        <> lMemberAndOwnedEnd
        <> rMemberAndOwnedEnd
        <> [ "    </packagedElement>"
           ]
  where
    genMemberAndOwnedEnd (Mult minVal maxVal) assocId type' =
      do
        endId <- mkUnlabeledId "MemberEnd"
        typeId <- refLabeledId type'
        lIntId <- mkUnlabeledId "Int"
        uIntId <- mkUnlabeledId "Int"
        return
          [ "     <memberEnd xmi:idref=\"" <> endId <> "\"/>",
            "     <ownedEnd xmi:type=\"uml:Property\" xmi:id=\"" <> endId <> "\" visibility=\"public\" association=\"" <> assocId <> "\" isStatic=\"false\""
              <> " isReadOnly=\"false\" isDerived=\"false\" isOrdered=\"false\" isUnique=\"true\" isDerivedUnion=\"false\" aggregation=\"none\">",
            "      <type xmi:idref=\"" <> typeId <> "\"/>",
            "      <lowerValue xmi:type=\"uml:LiteralInteger\" xmi:id=\"" <> lIntId <> "\" value=\"" <> (if minVal == MinZero then "0" else "1") <> "\"/>",
            case maxVal of
              MaxOne -> "      <upperValue xmi:type=\"uml:LiteralInteger\" xmi:id=\"" <> uIntId <> "\" value=\"1\"/>"
              MaxMany -> "      <upperValue xmi:type=\"uml:LiteralUnlimitedNatural\" xmi:id=\"" <> uIntId <> "\" value=\"-1\"/>",
            "     </ownedEnd>"
          ]

genDiagramElements :: UML
genDiagramElements =
  do
    elementIds <- gets diagramEltIds
    return ["      <element subject=\"" <> elementId <> "\"/>" | elementId <- elementIds]

genCustomProfileElements :: UML
genCustomProfileElements =
  do
    reqVals <- gets reqValues
    return [reqUML req | req <- reverse reqVals]
  where
    reqUML :: ReqValue2 -> Text
    reqUML (xmiId, req) =
      T.intercalate
        "\n"
        ( ("   <thecustomprofile:Functional base_Requirement=" <> tshow xmiId <> "/>") :
            [ tagUML xmiId count' puprtxt reftxt
              | (count', (puprtxt, reftxt)) <- zip [0 :: Int ..] [(aMarkup2String (explMarkup p), T.intercalate ";" (explRefIds p)) | p <- reqPurposes req]
            ]
        )
    tagUML xmiId nr value reftxt =
      T.intercalate
        "\n"
        [ "     <thecustomprofile:" <> keyMeaning <> " base_Requirement=" <> tshow xmiId <> " " <> keyMeaning <> "=" <> tshow value <> "/>",
          "     <thecustomprofile:" <> keyRef <> " base_Requirement=" <> tshow xmiId <> " " <> keyRef <> "=" <> tshow reftxt <> "/>"
        ]
      where
        keyMeaning = "Meaning" <> tshow nr
        keyRef = "Reference" <> tshow nr

genCustomReqElements :: (HasOutputLanguage env) => env -> FSpec -> Text -> UML
genCustomReqElements env fSpec parentPackageId =
  do
    reqVals <- gets reqValues
    return [reqUML req | req <- reverse reqVals]
  where
    reqUML :: ReqValue2 -> Text
    reqUML (xmiId, req) =
      T.intercalate
        "\n"
        ( [ "    <element xmi:idref=" <> tshow xmiId <> " xmi:type=\"uml:Requirement\" name=" <> tshow (reqId req) <> " scope=\"public\"" <> ">",
            "      <model package=" <> tshow parentPackageId <> " ea_eleType=\"element\"/>",
            "      <properties documentation=" <> tshow (maybe "" (aMarkup2String . ameaMrk) . meaning (outputLang env fSpec) $ req) <> " isSpecification=\"false\" sType=\"Requirement\" nType=\"0\" scope=\"public\" stereotype=\"Functional\"/>",
            "      <tags>"
          ]
            <> ["         <tag name=\"Purpose" <> nr <> "\" value=" <> tshow p <> " modelElement=" <> tshow xmiId <> "/>" | (nr, p) <- zip (map tshow [1 :: Int ..]) (map (aMarkup2String . explMarkup) $ reqPurposes req)]
            <> [ "      </tags>",
                 "    </element>"
               ]
        )

-- Requirements

data Req = Req
  { reqId :: Text,
    --   , reqRef :: Text
    reqOrig :: Either Rule Relation,
    reqPurposes :: [Purpose]
  }

instance HasMeaning Req where
  meanings r = case reqOrig r of
    Right rul -> meanings rul
    Left dcl -> meanings dcl

instance Named Req where
  name r = case reqOrig r of
    Right rul -> name rul
    Left dcl -> name dcl -- fmap name (reqOrig r)

requirements :: (HasOutputLanguage env) => env -> FSpec -> [Req]
requirements env fSpec =
  map decl2req (toList $ vrels fSpec)
    <> map rule2req (toList $ vrules fSpec)
  where
    decl2req d =
      Req
        { reqId = fullName d,
          reqOrig = Right d,
          reqPurposes = purposesOf fSpec (outputLang env fSpec) d
        }
    rule2req r =
      Req
        { reqId = fullName r,
          reqOrig = Left r,
          reqPurposes = purposesOf fSpec (outputLang env fSpec) r
        }

-- State and Monad

data UMLState = UMLState
  { idCounter :: Int,
    labelIdMap :: Map.Map Text Text,
    diagramEltIds :: [Text],
    reqValues :: [ReqValue2]
  }

type StateUML a = State UMLState a

type UML = StateUML [Text]

type ReqValue2 =
  ( Text, -- the xmi-id
    Req
  )

addToDiagram :: Text -> StateUML ()
addToDiagram elementId =
  modify $ \state' -> state' {diagramEltIds = elementId : diagramEltIds state'}

addReqToState :: ReqValue2 -> StateUML ()
addReqToState reqVal =
  modify $ \state' -> state' {reqValues = reqVal : reqValues state'}

mkUnlabeledId :: Text -> StateUML Text
mkUnlabeledId tag =
  do
    idC <- gets idCounter
    modify $ \state' -> state' {idCounter = idCounter state' + 1}
    pure $ tag <> "ID_" <> tshow idC

refLabeledId :: Text -> StateUML Text
refLabeledId lbl =
  do
    lidMap <- gets labelIdMap
    case Map.lookup lbl lidMap of
      Just lid -> return lid
      Nothing -> fatal ("Requesting non-existent label " <> lbl)

mkLabeledId :: Text -> Text -> StateUML ()
mkLabeledId tag lbl =
  do
    let classId = tag <> "ID_" <> lbl
    modify $ \state' -> state' {labelIdMap = Map.insert lbl classId (labelIdMap state')}

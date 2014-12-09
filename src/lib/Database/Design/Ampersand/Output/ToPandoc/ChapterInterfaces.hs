module Database.Design.Ampersand.Output.ToPandoc.ChapterInterfaces
  ( chpInterfacesPics
  , chpInterfacesBlocks
  )
where
import Database.Design.Ampersand.Output.ToPandoc.SharedAmongChapters
import Database.Design.Ampersand.ADL1
import Data.List
import Database.Design.Ampersand.Fspec.Fspec
import Database.Design.Ampersand.Output.PandocAux
import Database.Design.Ampersand.Classes.Relational
import Database.Design.Ampersand.Fspec.FPA

fatal :: Int -> String -> a
fatal = fatalMsg "Output.ToPandoc.ChapterInterfaces"

chpInterfacesPics :: Fspc -> [Picture]
chpInterfacesPics fSpec = []

chpInterfacesBlocks :: Int -> Fspc -> Blocks
chpInterfacesBlocks lev fSpec = -- lev is the header level (0 is chapter level)
  mconcat $ map interfaceChap regularInterfaces ++ [ messagesChap messageInterfaces | not (null messageInterfaces) ]
  where
    messageInterfaces :: [Interface]
    regularInterfaces :: [Interface]
    (messageInterfaces, regularInterfaces) = partition (\i -> ifcClass i == Just "Message") $ interfaceS fSpec
    
    lang = Dutch -- TODO: add English labels and use (fsLang fSpec) here
      
    interfaceChap :: Interface -> Blocks
    interfaceChap ifc
     =  (labeledThing (flags fSpec) (lev) ("chapIfc_"++name ifc) ("Interface: " ++ quoteName (name ifc)))  <>
        ifcIntro ifc <>
        docInterface ifc
      
    ifcIntro :: Interface -> Blocks
    ifcIntro ifc
     =  introBlocks <>
        purposes2Blocks (flags fSpec) purps        
       where purps = purposesDefinedIn fSpec lang ifc
 
             introBlocks :: Blocks
             introBlocks = fromList $
               case lang of
                 Dutch   -> [Para
                             [ Str $ "Dit hoofdstuk bevat de documentatie voor de interface "++ quoteName (name ifc)++"."
                             ]]
                 English -> [Para
                             [ Str $ "This chapter contains the documentation for the interface "++ quoteName (name ifc)++"."
                             ]]

    docInterface :: Interface -> Blocks
    docInterface ifc =
      (plainText $ "De interface is beschikbaar voor " ++ showRoles (ifcRoles ifc) ++ ".") <>
      (if null $ ifcControls ifc
       then plainText "Voor deze interface hoeven geen regels gecontroleerd te worden."
       else plainText "Voorafgaand aan het afsluiten van een transactie (commit), moet aan de volgende regels voldaan zijn:" <>  
             bulletList [ plainText $ rc_rulename rule | rule <- ifcControls ifc]) <>
      (if genFPAChap (flags fSpec)
       then (plain . strong . text) "Functiepunten:" <>
            plainText ("Deze interface is gerubriceerd als " ++ showLang lang (fpType interfaceFP) ++
                       " " ++ showLang lang (fpComplexity interfaceFP) ++ 
                       ", en is daarmee " ++ show (fpVal interfaceFP) ++ " functiepunten waard.")
       else plainText "" -- TODO: vervangen door de Pandoc-aanduiding voor "niks"
      ) <>
      (plain . strong . text) "Interfacestructuur:" <>
      docInterfaceObjects (ifcParams ifc) (ifcRoles ifc) [] (ifcObj ifc)
      where interfaceFP = fpaInterface ifc

    docInterfaceObjects :: [Expression] -> [String] -> [Int] -> ObjectDef -> Blocks
    docInterfaceObjects editableRels roles hierarchy object =
      case hierarchy of
        [] -> plain . text $ "Interface voor een waarde van type " ++ quoteName (name (target iExp)) ++ "."
              -- TODO: unclear what we want to do here. Probably want to hide "ONE". Do we need to take multiplicites into account? (e.g. waarden)  
        _  -> plain . strong . fromList $ [Str $ (intercalate "." $ map show hierarchy) ++ " " ++ objectName]
      <> interfaceObjDoc <>
      mconcat subInterfaceDocs
      where objectName = let nm = name object in if null nm then "<Naamloos>" else nm
      
            interfaceObjDoc :: Blocks
            interfaceObjDoc =
              mconcat $
                [ plainText  $ fieldDescr ++ quoteName (name (target iExp)) ++ ". (" ++ maybe "niet-" (const "") editableRelM ++ "editable)"              
                ] ++
                
                case navigationDocs of
                  []               -> [ plainText $ "Hiervandaan kan niet genavigeerd worden." ]
                  navDocs@(_:rest) -> 
                    [ plainText $ "Hiervandaan kan genavigeerd worden naar interface"++(if null rest then "" else "s")++":"] ++
                    [ bulletList navDocs ]
                ++
                [ plainText $ "De bijbehorende ADL expressie is: ", plain . code $ showADL iExp ] ++
                [ plainText $ fieldRef ++ " bestaat uit " ++ show (length subInterfaceDocs) ++ " deelveld"++ (if len>1 then "en" else "") ++":"
                | let len = length subInterfaceDocs, len > 0 ] ++
                
                if not $ development (flags fSpec) then [] else -- some debug info shown on --dev
                  [ plainText $ "DEBUG: Props: ["++props++"]" | development (flags fSpec) ] ++
                  case editableRelM of
                    Nothing -> []
                    Just (srcConcept, d, tgtConcept, isFlipped) -> 
                      [ plainText $ "DEBUG: Declaration "++ name d ++ (if isFlipped then "~" else "")
                      , plainText $ "DEBUG: showADL: " ++ showADL d
                      ] 
              where (fieldDescr,fieldRef) = 
                      if isSur iExp then if isUni iExp then ("Een verplicht veld van type ", "Dit veld")
                                                       else ("Een lijst van 1 of meer velden van type ", "Elk veld")
                                    else if isUni iExp then ("Een optioneel veld van type ", "Dit veld")
                                                       else ("Een lijst van 0 of meer velden van type ", "Elk veld")
                    props = intercalate "," $ [ "INJ" | isInj iExp] ++ [ "SUR" | isSur iExp] ++ [ "TOT" | isTot iExp] ++ [ "UNI" | isUni iExp]
                    
                    editableRelM = getEditableRelation editableRels iExp
                    
                    navigationDocs = [ plainText $ quoteName (name navIfc) ++ " (voor " ++ showRoles sharedRoles ++ ")" 
                                     | navIfc <- regularInterfaces
                                     , source (objctx . ifcObj $ navIfc) == target iExp
                                     , let sharedRoles = ifcRoles navIfc `intersect` roles
                                     , not . null $ sharedRoles
                                     ]

            iExp = conjNF (flags fSpec) $ objctx object
                    
            fieldType = name (target iExp)
            subInterfaceDocs = docMSubInterface editableRels roles hierarchy (objmsub object)

    docMSubInterface :: [Expression] -> [String] -> [Int] -> Maybe SubInterface -> [Blocks]
    docMSubInterface editableRels roles hierarchy subIfc =
      case subIfc of
        Nothing                -> []
        Just (InterfaceRef nm) -> [ plainText $ "REF "++nm ] -- TODO: handle InterfaceRef
        Just (Box _ objects)   -> [ docInterfaceObjects editableRels roles (hierarchy ++[i]) obj | (obj,i) <- zip objects [1..] ]

    -- shorthand for easy localizing    
    l :: LocalizedStr -> String
    l lstr = localize (fsLang fSpec) lstr

    messagesChap :: [Interface] -> Blocks
    messagesChap ifcs = mconcat
      [ labeledThing (flags fSpec) (lev) "chapMessages" $ l (NL "Berichten", EN "Messages")
      , para . text $ l ( NL "Dit hoofdstuk geeft een overzicht van alle berichten."
                        , EN "This chapter lists all messages." )
      , simpleTable [ plainText $ l (NL "Eigenschap term (TODO: naam ok?)", EN "Property term"), plainText $ l (NL "Card.", EN "Card.")
                    , plainText $ l (NL "Expressie", EN "Expression"), plainText $ l (NL "Definitie", EN "Definition") ] $
          intercalate [[]] . map mkTableRows $ genEntity_Interfaces fSpec ifcs
      ]
      
    mkTableRows :: Entity -> [[Blocks]]
    mkTableRows (Entity nm dpth expr card def refTp props) = 
      [plain $ (fromList $ replicate (dpth*3) nbsp) <> (if dpth == 0 then strong else id) (text nm) , plainText card, plainText expr, plainText def]
      : concatMap mkTableRows props
      where nbsp :: Inline
            nbsp = RawInline (Format "latex") "~"

-- TODO: copied from prototype GenBericht.hs, if that module is kept, we should move this to a shared module.  
data Entity = Entity { entName ::     String
                     , depth ::       Int
                     , expr ::         String
                     , cardinality :: String
                     , definition ::  String
                     , refType ::     String
                     , properties ::  [Entity]
                     } deriving Show

genEntity_Interfaces :: Fspc -> [Interface] -> [Entity]
genEntity_Interfaces fSpec interfaces = map genEntity_Interface interfaces
  where 
    genEntity_Interface ::Interface -> Entity
    genEntity_Interface interface = genEntity_ObjDef 0 (ifcObj interface)
     where
       genEntity_ObjDef :: Int -> ObjectDef -> Entity
       genEntity_ObjDef dpth objDef =
           Entity { entName = name objDef
                  , depth = dpth
                  , expr = showADL $ objctx objDef
                  , cardinality = card $ objctx objDef
                  , definition  = def $ objctx objDef
                  , refType     = name (target $ objctx objDef)
                  , properties  =
                      case objmsub objDef of
                        Nothing -> []
                        Just (Box _ objs)      -> map (genEntity_ObjDef (dpth+1)) objs
                        Just (InterfaceRef nm) -> map (genEntity_ObjDef (dpth+1)) $ objsForInterfaceNamed nm
                  }
        where card e = (if isTot e then "1" else "0")++".."++(if isUni e then "1" else "*")
  
              def rel = case concDefs fSpec (target rel) of
                            Cd {cddef=def'} : _ | def' /= "" -> def'
                            _                                -> "** NO DEFINITION **"

              objsForInterfaceNamed :: String -> [ObjectDef]
              objsForInterfaceNamed nm =
                case objmsub $ ifcObj $ getInterfaceByName (interfaceS fSpec) nm of
                  Just (Box _ objs) -> objs
                  _               -> fatal 81 "Bericht interfaces have wrong format"

-- TODO: Maybe we should create a more general version of this function (might already exist, but I couldn't find it)
showRoles :: [String] -> String
showRoles roles = case roles of
                    [] -> "alle rollen"
                    [role] -> "de rol " ++ quoteName role
                    [role1,role2] -> "de rollen " ++ quoteName role1 ++ " en " ++ quoteName role2
                    (role1:roles) -> "de rollen " ++ quoteName role1 ++ 
                                         (concat . reverse $ zipWith (++) (", en ": repeat ", ") $ reverse $ map quoteName roles)

quoteName :: String -> String
quoteName role = "``"++role++"''"

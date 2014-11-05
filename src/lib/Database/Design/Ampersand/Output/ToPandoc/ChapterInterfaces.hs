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

-- TODO: use views?
-- TODO: refactor shared code with prototype (editability of fields and navigation is quite fragile now)
-- TODO: prototype/Generate.hs also generates for interfaceG fSpec, but this seems to be empty most of the time. Include them in the docs?

chpInterfacesPics :: Fspc -> [Picture]
chpInterfacesPics fSpec = []

chpInterfacesBlocks :: Int -> Fspc -> Blocks
chpInterfacesBlocks lev fSpec = -- lev is the header level (0 is chapter level)
  mconcat $ map interfaceChap allInterfaces
  where
    allInterfaces :: [Interface]
    allInterfaces = interfaceS fSpec
    
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
      docInterfaceObjects (ifcParams ifc) [] (ifcObj ifc)
      where interfaceFP = fpaInterface ifc

    docInterfaceObjects :: [Expression] -> [Int] -> ObjectDef -> Blocks
    docInterfaceObjects editableRels hierarchy object =
      case hierarchy of
        [] -> plain . text $ "Interface voor een waarde van type " ++ quoteName (name (target iExp)) 
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
                [ plainText $ fieldRef ++ " bestaat uit " ++ show (length subInterfaceDocs) ++ " deelveld"++ (if len>1 then "en" else "") ++":"
                | let len = length subInterfaceDocs, len > 0 ] ++
                 -- TODO: Maybe we want to show the expression? Maybe only when it is a declaration, but in that case we might want to show
                 --       it even when it is not in editableRels (requires refactoring of getEditableRelation).
                [ plainText $ "DEBUG: Props: ["++props++"]" | development (flags fSpec) ] ++
                case editableRelM of 
                  Nothing -> []
                  Just (srcConcept, d, tgtConcept, isFlipped) -> if not $ development (flags fSpec) then [] else
                    [ plainText $ "DEBUG: Declaration "++ name d ++ (if isFlipped then "~" else "")
                    , plainText $ "DEBUG: showADL: " ++ showADL d ++ ")"
                    ] 
              where (fieldDescr,fieldRef) = 
                      if isSur iExp then if isUni iExp then ("Een verplicht veld van type ", "Dit veld")
                                                       else ("Een lijst van 1 of meer velden van type ", "Elk veld")
                                    else if isUni iExp then ("Een optioneel veld van type ", "Dit veld")
                                                       else ("Een lijst van 0 of meer velden van type ", "Elk veld")
                    props = intercalate "," $ [ "INJ" | isInj iExp] ++ [ "SUR" | isSur iExp] ++ [ "TOT" | isTot iExp] ++ [ "UNI" | isUni iExp]
                    
                    editableRelM = getEditableRelation editableRels iExp
                    
                    navigationDocs = [ plainText $ quoteName (name navIfc) ++ " (voor " ++ showRoles (ifcRoles navIfc) ++ ")" 
                                     | navIfc <- allInterfaces, source (objctx . ifcObj $ navIfc) == target iExp 
                                     ]
                                    -- TODO: fragile: copying computation from Generate.hs
            iExp = conjNF (flags fSpec) $ objctx object
                    
            fieldType = name (target iExp)
            subInterfaceDocs = docMSubInterface editableRels hierarchy (objmsub object)

    docMSubInterface :: [Expression] -> [Int] -> Maybe SubInterface -> [Blocks]
    docMSubInterface editableRels hierarchy subIfc =
      case subIfc of
        Nothing                -> []
        Just (InterfaceRef nm) -> [ plainText $ "REF "++nm ] -- TODO: handle InterfaceRef
        Just (Box _ objects)   -> [ docInterfaceObjects editableRels (hierarchy ++[i]) obj | (obj,i) <- zip objects [1..] ]


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

plainText :: String -> Blocks 
plainText str = plain . text $ str

-- TODO: copied from ampersand-prototype Generate.hs for now. We need this in ampersand itself
getEditableRelation :: [Expression] -> Expression -> Maybe (A_Concept, Declaration, A_Concept, Bool)
getEditableRelation editableRels exp = case getRelation exp of
   Just (s,Just d,t,isFlipped)  -> if EDcD d `elem` editableRels then Just (s,d,t,isFlipped) else Nothing
   _                            -> Nothing
 where
   -- getRelation produces a declaration and the narrowest possible concepts
   -- at the left- and right hand sides of an expression.
   -- Additionally, a boolean is produced to state whether the relation is flipped.
   getRelation :: Expression -> Maybe (A_Concept, Maybe Declaration, A_Concept, Bool)
   getRelation (ECps (e, EDcI{})) = getRelation e
   getRelation (ECps (EDcI{}, e)) = getRelation e
   getRelation (ECps (e1, e2))
     = case (getRelation e1, getRelation e2) of --note: target e1==source e2
        (Just (_,Nothing,i1,_), Just (i2,Nothing,_,_)) -> if i1==target e1 && i2==source e2 then Just (i1, Nothing, i2, False) else -- i1==i2
                                                          if i1==target e1 && i2/=source e2 then Just (i2, Nothing, i2, False) else
                                                          if i1/=target e1 && i2==source e2 then Just (i1, Nothing, i1, False) else
                                                          Nothing
        (Just (_,Nothing,i,_), Just (s,d,t,isFlipped)) -> if i==target e1                 then Just (s,d,t,isFlipped) else                       
                                                          if i/=target e1 && s==target e1 then Just (i,d,t,isFlipped) else                       
                                                          Nothing                                                     
        (Just (s,d,t,isFlipped), Just (i,Nothing,_,_)) -> if i==source e2                 then Just (s,d,t,isFlipped) else
                                                          if i/=source e2 && t==source e2 then Just (s,d,i,isFlipped) else        
                                                          Nothing                                                                 
        _                                              -> Nothing
   getRelation (EFlp e)
    = case getRelation e of
        Just (s,d,t,isFlipped) -> Just (t,d,s,not isFlipped)
        Nothing                -> Nothing
   getRelation (EDcD d)   = Just (source d, Just d, target d, False)
   getRelation (EEps i _) = Just (i, Nothing, i, False)
   getRelation _ = Nothing

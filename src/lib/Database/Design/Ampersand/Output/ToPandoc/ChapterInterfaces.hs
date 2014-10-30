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

-- TODO: use views?
-- TODO: refactor shared code with prototype

chpInterfacesPics :: Fspc -> [Picture]
chpInterfacesPics fSpec = []

chpInterfacesBlocks :: Int -> Fspc -> Blocks
chpInterfacesBlocks lev fSpec = -- lev is the header level (0 is chapter level)
  mconcat (map interfaceChap (interfaceS fSpec))
  where
    lang = Dutch -- TODO: add English labels and use (fsLang fSpec) here
  
    interfaceChap :: Interface -> Blocks
    interfaceChap ifc
     =  (labeledThing (flags fSpec) (lev) ("chapIfc_"++name ifc) ("Interface: " ++ name ifc))  <>
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
                             [ Str $ "Dit hoofdstuk bevat de documentatie voor interface ``"++name ifc++"''."
                             ]]
                 English -> [Para
                             [ Str $ "This chapter contains the documentation for interface "++name ifc++"."
                             ]]

    docInterface :: Interface -> Blocks
    docInterface ifc =
      (plain . text $ "De interface is beschikbaar voor " ++
                    case ifcRoles ifc of
                      [] -> "geen enkele rol."
                      [role] -> "de rol " ++ showRole role ++ "."
                      [role1,role2] -> "de rollen " ++ showRole role1 ++ " en " ++ showRole role2 ++ "."
                      (role1:roles) -> "de rollen " ++ showRole role1 ++ 
                                       (concat . reverse $ zipWith (++) (", en ": repeat ", ") $ reverse $ map showRole roles) ++ ".") <>
      (if null $ ifcControls ifc
       then plain . text $ "Voor deze interface hoeven geen regels gecontroleerd te worden."
       else (plain . text $ "Om een transactie af te mogen sluiten, moeten de volgende regels gecontroleerd worden:") <>  
             bulletList [singleton $ Plain [Str $ rc_rulename rule] | rule <- ifcControls ifc]) <>
      (plain . strong . text $ "Interfacestructuur:") <>
      docInterfaceObjects (ifcParams ifc) [] (ifcObj ifc)
        where showRole role = "``"++role++"''"
       
    docInterfaceObjects :: [Expression] -> [Int] -> ObjectDef -> Blocks
    docInterfaceObjects editableRels hierarchy object =
      case hierarchy of
        [] -> plain . text $ "Interface voor een waarde van type ``" ++ name (target iExp) ++ "''" 
              -- TODO: unclear what we want to do here. Probably want to hide "ONE". Do we need to take multiplicites into account? (e.g. waarden)  
        _  -> (plain . strong . fromList $ [Str $ (intercalate "." $ map show hierarchy) ++ " " ++ objectName])
      <> interfaceObjDoc <>
      mconcat subInterfaceDocs
      where objectName = let nm = name object in if null nm then "<Naamloos>" else nm
      
            interfaceObjDoc :: Blocks
            interfaceObjDoc =
              fromList . map (\str -> Plain [Str str]) $
                [ fieldDescr ++ "``" ++ name (target iExp) ++ "''. (" ++ maybe "niet-" (const "") editableRelM ++ "editable)"
                ] ++
                [ fieldRef ++ " bestaat uit " ++ show (length subInterfaceDocs) ++ " deelveld"++ (if len >1 then "en" else "") ++":"
                | let len = length subInterfaceDocs, len > 0 ] ++
                --, "DEBUG: Props: ["++props++"]"
                case editableRelM of 
                  Nothing -> 
                    [
                    ] -- TODO: maybe we do want to show the expression? (or maybe only if it is a declaration? (that is not in editableRels))
   
                  Just (srcConcept, d, tgtConcept, isFlipped) ->
                    [ --"DEBUG: Relatie "++ name d ++ (if isFlipped then "~" else "")
                    --, "DEBUG(showADL: " ++ showADL d ++ ")" ]
                    ]
              where (fieldDescr,fieldRef) = 
                      if isSur iExp then if isUni iExp then ("Een verplicht veld van type ", "Dit veld")
                                                       else ("Een lijst van 1 of meer velden van type ", "Elk veld")
                                    else if isUni iExp then ("Een optioneel veld van type ", "Dit veld")
                                                       else ("Een lijst van 0 of meer velden van type ", "Elk veld")
                    props = intercalate "," $ [ "INJ" | isInj iExp] ++ [ "SUR" | isSur iExp] ++ [ "TOT" | isTot iExp] ++ [ "UNI" | isUni iExp]
                    
                    editableRelM = getEditableRelation editableRels iExp
            iExp = conjNF (flags fSpec) $ objctx object
                    
            fieldType = name (target iExp)
            subInterfaceDocs = docMSubInterface editableRels hierarchy (objmsub object)

    docMSubInterface :: [Expression] -> [Int] -> Maybe SubInterface -> [Blocks]
    docMSubInterface editableRels hierarchy subIfc =
      case subIfc of
        Nothing                -> []
        Just (InterfaceRef nm) -> [singleton $ Plain $ [Str $ "REF "++nm]] -- TODO: handle InterfaceRef
        Just (Box _ objects)   -> [docInterfaceObjects editableRels (hierarchy ++[i]) obj | (obj,i) <- zip objects [1..]]

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

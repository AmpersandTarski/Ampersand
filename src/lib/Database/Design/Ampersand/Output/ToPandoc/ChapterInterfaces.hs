{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
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

chpInterfacesPics :: Fspc -> [Picture]
chpInterfacesPics fSpec =
   concat [picKnowledgeGraph fSpec act : [picSwitchboard fSpec act | graphic (flags fSpec)] | act <- fActivities fSpec ]
chpInterfacesBlocks :: Int -> Fspc -> Blocks
chpInterfacesBlocks lev fSpec =
   foldr (<>) mempty (map interfaceChap (fActivities fSpec))
 where
   interfaceChap :: Activity -> Blocks
   interfaceChap act
   -- TODO: This should be one chapter for all interfaces.
    =  (labeledThing (flags fSpec) lev ("chpIfc"++name act) (name act)) <>
       ifcIntro act <>
       (if genGraphics (flags fSpec) then txtKnowledgeGraph act else mempty) <>
       (if graphic     (flags fSpec) then txtSwitchboard act else mempty)
   ifcIntro :: Activity -> Blocks
   ifcIntro act
    =   purposes2Blocks (flags fSpec) purps
      <> fromList (ifcAutoRules act)
      <> fromList (if genEcaDoc (flags fSpec) then ifcEcaRules act else [])
      where purps = purposesDefinedIn fSpec (fsLang fSpec) fSpec

{-
  ifcInsDelConcepts :: [Block]
  ifcInsDelConcepts
   = let ics = fsv_creating act>-fsv_deleting act
         dcs = fsv_deleting act>-fsv_creating act
         ucs = fsv_deleting act `isc` fsv_creating act
     in case fsLang fSpec of
      Dutch -> [Plain [Space]]++
          if null ics && null dcs && null ucs then [Plain $ [Str "Deze interface maakt of verwijdert geen objecten langs geautomatiseerde weg."]] else
          if null ics && null dcs             then [Plain $ [Str "Om regels te handhaven, mogen instanties van "]++commaNLPandoc (Str "en") (map (Str . name) ucs)++[Str " door deze interface geautomatiseerd worden aangemaakt en verwijderd."]] else
          if null ics       &&       null ucs then [Plain $ [Str "Om regels te handhaven, mag deze interface instanties van "]++commaNLPandoc (Str "en") (map (Str . name) ucs)++[Str " geautomatiseerd verwijderen."]] else
          if             null dcs && null ucs then [Plain $ [Str "Om regels te handhaven, mogen instanties van "]++commaNLPandoc (Str "en") (map (Str . name) ucs)++[Str " geautomatiseerd worden aangemaakt door deze interface."]] else
          if                         null ucs then [Plain $ [Str "Instanties van "]++commaNLPandoc (Str "en") (map (Str . name) ucs)++[Str " mogen worden toegevoegd en instanties van "]++f dcs++[Str " mogen worden verwijderd door deze interface. Dat gebeurt geautomatiseerd en uitsluitend waar nodig om regels te handhaven."]] else
          if             null dcs             then [Plain $ [Str "Deze interface mag instanties van "]++commaNLPandoc (Str "en") (map (Str . name) ics)++[Str " creeren, terwijl instanties van "]++commaNLPandoc (Str "en") (map (Str . name) ucs)++[Str " ook verwijderd mogen worden. Alleen waar nodig mag dit plaatsvinden om regels te handhaven."]] else
          if null ics                         then [Plain $ [Str "Deze interface mag instanties van "]++commaNLPandoc (Str "en") (map (Str . name) ucs)++[Str " wijzigen, maar instanties van "]++commaNLPandoc (Str "en") (map (Str . name) dcs)++[Str " mogen alleen worden verwijderd. Dat mag slechts dan gebeuren wanneer dat nodig is voor het handhaven van regels."]] else
          [Plain $ [Str "Deze interface maakt nieuwe instanties van concept"]++f ics++[Str ". Hij mag instanties van concept"]++f dcs++[Str " verwijderen, terwijl instanties van "]++commaNLPandoc (Str "en") (map (Str . name) ucs)++[Str " zowel gemaakt als vernietigd mogen worden."]]
          where f [x] = [Space, Str (name x), Space]
                f xs  = [Str "en "]++commaNLPandoc (Str "en") (map (Str . name) xs)
      English -> [Plain [Space]]++
          if null ics && null dcs && null ucs then [Plain $ [Str "In this interface, no objects are made or removed automatically."]] else
          if null ics && null dcs             then [Plain $ [Str "In order to maintain rules, instances of "]++f ucs++[Str " may be created or deleted by this interface automatically."]] else
          if null ics       &&       null ucs then [Plain $ [Str "In order to maintain rules, instances of "]++f dcs++[Str " may be deleted automatically by this interface."]] else
          if             null dcs && null ucs then [Plain $ [Str "In order to maintain rules, instances of "]++f ics++[Str " may be automatically inserted by this interface."]] else
          if                         null ucs then [Plain $ [Str "Concept"]++f ics++[Str " may be inserted, and concept"]++f dcs++[Str " may be deleted by this interface. This happens only if necessary for maintaining rules."]] else
          if             null dcs             then [Plain $ [Str "By this interface, concept"]++f ucs++[Str " may be changed, but"]++f ics++[Str " may be created but not deleted. This happens only if necessary for maintaining rules."]] else
          if null ics                         then [Plain $ [Str "By this interface, concept"]++f ucs++[Str " may be changed, but"]++f dcs++[Str " may only be deleted. This happens only if necessary for maintaining rules."]] else
          [Plain $ [Str "This interface can create new instances of concept"]++f ics++[Str ". It may delete instances of concept"]++f dcs++[Str ", and instances of concept"]++f ucs++[Str " may be either created and removed. Such actions will take place only in order to maintain rules."]]
          where f [x] = [Space, Str (name x)]
                f xs  = [Str "s "]++commaEngPandoc (Str "and") (map (Str . name) xs)
-}

   ifcAutoRules :: Activity -> [Block]
   ifcAutoRules act
    = case fsLang fSpec of
       Dutch ->   [Plain ([Str "Activiteit",Space, Quoted SingleQuote [(Str . name . actRule) act], Space,Str "moet door een gebruiker met rol "]++commaNLPandoc (Str "of") rols++[Str" worden uitgevoerd."] ++
                          case length auts of
                           0 -> []
                           1 -> [Space,Str "Daarbij wordt regel",Space]++auts++[Space,Str "gehandhaafd zonder interventie van de gebruiker."]
                           _ -> [Space,Str "Daarbij worden regels",Space]++commaNLPandoc (Str "en") auts++[Space,Str "gehandhaafd zonder interventie van de gebruiker."]
                  )]
       English -> [Plain ([Str "Activity",Space, Quoted SingleQuote [(Str . name . actRule) act], Space,Str "must be performed by a user with role "]++commaEngPandoc (Str "or") rols++[Str"."] ++
                          case length auts of
                           0 -> []
                           1 -> [Space,Str "During that activity, rule",Space]++auts++[Space,Str "will be maintained without intervention of a user."]
                           _ -> [Space,Str "During that activity, rules",Space]++commaEngPandoc (Str "and") auts++[Space,Str "will be maintained without intervention of a user."]
                  )]
      where
         auts = nub [ Quoted  SingleQuote [Str (name r)] | q<-actQuads act, let r=qRule q, r_usr r == UserDefined]
         rols = nub [Str r | (r,rul)<-fRoleRuls fSpec, rul==actRule act]

   ifcEcaRules :: Activity -> [Block]
   ifcEcaRules act
    = ( case (fsLang fSpec, actEcas act) of
         (Dutch,[])   -> [Plain [Str "Alle veranderingen die een gebruiker uitvoert zijn handmatig. Er is geen geautomatiseerde functionaliteit in deze activiteit."]]
         (English,[]) -> [Plain [Str "All changes a user makes are done by hand. There is no automated functionality in this activity."]]
         (Dutch,_)    -> [Plain [Str "De volgende tabel laat zien welke edit-acties welke functie aanroepen."]]
         (English,_)  -> [Plain [Str "The following table shows which edit actions invoke which function."]]
      )++
      if length (actEcas act)<1 then [] else
      [ Table [] [AlignLeft,AlignLeft,AlignLeft] [0.0,0.0,0.0]
        ( case fsLang fSpec of
           Dutch   ->
               [[Plain [Str "actie"]]
               ,[Plain [Str "relatie"]]
               ,[Plain [Str "regel"]]]
           English   ->
               [[Plain [Str "action"]]
               ,[Plain [Str "relation"]]
               ,[Plain [Str "rule"]]] )
        [ [ [Plain [ (Str . show . eSrt.ecaTriggr) eca]]
          , [Plain [ (Str . show . eDcl.ecaTriggr) eca]]
          , [Plain (shwEca eca)]
          ]
        | eca<-actEcas act  -- tail haalt het eerste veld, zijnde I[c], eruit omdat die niet in deze tabel thuishoort.
        ]]
     where
      shwEca :: ECArule -> [Inline]
      shwEca eca
       | isBlk (ecaAction eca)
            = Str "error: rule ":
              commaEngPandoc (Str "and")
                       [ Quoted  SingleQuote [Str (let nrRules = length rs
                                                       s  | nrRules == 0  = ""
                                                          | nrRules == 1  = name (head rs)
                                                          | otherwise     = name (head rs)++" (and "++show(nrRules - 1)++" other rules)."
                                                   in s
                                                  )]
                       | (_,rs)<-paMotiv (ecaAction eca)
                       ]
       | isNop (ecaAction eca)
            = [ Str "no op"]
       | otherwise = [ Str "ECA rule ", Str ((show . ecaNum ) eca) ]
{-
  ifcFieldTables
   = if null (fsv_fields act) then [] else
     if length (fsv_fields act)==1
     then [ Para  $ [ case fsLang fSpec of
                       Dutch   -> Str "In deze interface is het volgende veld zichtbaar: "
                       English -> Str "This interface has one field: "
                    ]
          , head [b | BulletList [bs]<-[flds], b<-bs]
          ]
     else [ Para  $ [ case fsLang fSpec of
                       Dutch   -> Str "In deze interface zijn de volgende velden zichtbaar. "
                       English -> Str "This interface has the following fields. "
                    ]
          , flds
          ]
     where flds :: Block
           flds = BulletList [recur ((objctx.ifcObj.fsv_ifcdef) act) f | f<-fsv_fields act]
            where recur :: Expression -> Field -> [Block]
                  recur e f | null (fld_sub f) = fld e f
                            | otherwise        = fld e f ++
                                                 [ BulletList [recur (ECps [e,fld_expr f']) f' | f'<-fld_sub f] ]
           fld e f = [ Para [ Str (dealWithUnderscores (fld_name f)++if null cols then "" else "("++intercalate ", " cols++")") ]
                     , Para [ Str "display on start: ", Math InlineMath $ showMath (conjNF (flags fSpec) e) ]
                     ]
            where cols = ["lijst"         | fld_list    f]++
                         ["verplicht"     | fld_must    f]++
                         ["nieuw"         | fld_insAble f]++
                         ["verwijderbaar" | fld_delAble f]

           dealWithUnderscores :: String -> String
           dealWithUnderscores x =
                  case x of
                    []     -> []
                    '_':cs -> "\\_" ++ dealWithUnderscores cs
                    c:cs   -> c : dealWithUnderscores cs
-}

   txtKnowledgeGraph :: Activity -> Blocks
   txtKnowledgeGraph act
    = (case fsLang fSpec of                                     -- announce the knowledge graph
           Dutch   -> para ( "Figuur " <> (xRefReference (flags fSpec) (picKnowledgeGraph fSpec act))
                           <>" geeft de kennisgraaf weer voor deze interface."
                           )
           English -> para ( "Figure " <> (xRefReference (flags fSpec) (picKnowledgeGraph fSpec act))
                           <>" shows the knowledge graph of this interface."
                           )
      ) <>
      ( (para . showImage (flags fSpec)) (picKnowledgeGraph fSpec act))    -- draw the knowledge graph

   txtSwitchboard :: Activity -> Blocks
   txtSwitchboard act
    = (if name act==name (head (fActivities fSpec)) then fromList switchboardIntro else mempty
      ) <>
      (case fsLang fSpec of                                     -- announce the switchboard diagram
           Dutch   -> para ( "Figuur " <> (xRefReference (flags fSpec) (picSwitchboard fSpec act))
                           <>" geeft het schakelpaneel (switchboard diagram) weer voor deze interface."
                           )
           English -> para ( "Figure " <> (xRefReference (flags fSpec) (picSwitchboard fSpec act))
                           <>" shows the switchboard diagram of this interface."
                           )
      ) <>
      ( (para . showImage (flags fSpec)) (picSwitchboard fSpec act))        -- draw the switchboard

   switchboardIntro :: [Block]
   switchboardIntro
    = if not (graphic (flags fSpec)) then [] else
     [ Para $ case fsLang fSpec of                             -- tells us for who this interface exists
        Dutch   -> [ Str "Iedere sectie in dit hoofdstuk beschrijft één activiteit. "
                   , Str "Tijdens het uitvoeren van een activiteit zal een gebruiker populatie invoegen of verwijderen in verschillende relaties. "
                   , Str "Hierdoor kunnen invarianten potentieel worden overtreden. "
                   , Str "(Een invariant is een bedrijfsregel die op ieder moment waar moet blijven.) "
                   , Str "De software die nodig is om invarianten waar te maken wordt automatisch gegenereerd. "
                   , Str "De structuur van deze software wordt geïllustreerd door een zogenaamd schakelpaneel (switchboard-diagram), "
                   , Str "waarvan u de eerste in figuur X aantreft. "
                   , Str "Elk switchboard diagram bestaat uit drie kolommen: "
                   , Str "Invariante regels staan in het midden en relaties staan aan de (linker en rechter) zijkanten. "
                   , Str "Een pijl ter linkerzijde wijst van een relatie die ge-edit wordt naar een regel die daardoor mogelijk overtreden wordt. "
                   , Str "Elke pijl ter rechterzijde van een regel representeert een edit-actie die nodig is om het waar-zijn ervan te herstellen. "
                   , Str "Deze pijl wijst naar de relatie waarin deze herstel-actie moet worden uitgevoerd. "
                   , Str "Een pijl gelabeled met '+' duidt op een insert event; een pijl met '-' op  "
                   , Str "Hierdoor onstaat een accuraat beeld op welke manier de activiteit alle invarianten handhaaft. "
                   ]
        English -> [ Str "Every section in this chapter describes one activity. "
                   , Str "While performing an activity, users will insert or delete population in various relations. "
                   , Str "This may potentially violate invariants. "
                   , Str "(An invariant is a business rule rules that must remain true at all times.) "
                   , Str "The software to maintain the truth of invariant rules is generated automatically. "
                   , Str "The structure of that software is illustrated by a so called switchboard diagram, "
                   , Str "the first of which you will find in figure X. "
                   , Str "Each switchboard diagram consists of three columns: "
                   , Str "Invariant rules are drawn in the middle, and relations occur on the (right and left hand) sides. "
                   , Str "An arrow on the left hand side points from a relation that may be edited to a rule that may be violated as a consequence thereof. "
                   , Str "Each arrow on the right hand side of a rule represents an edit action that is required to restore its truth. "
                   , Str "It points to the relation that is edited for that purpose. "
                   , Str "If that arrow is labeled '+', it involves an insert event; if labeled '-' it refers to a delete event. "
                   , Str "This yields an accurate perspective on the way in which invariants are maintained. "
                   ]
     ]

picKnowledgeGraph :: Fspc -> Activity -> Picture
picKnowledgeGraph fSpec act = makePicture fSpec (PTFinterface act)

picSwitchboard :: Fspc -> Activity -> Picture
picSwitchboard fSpec act
    = makePicture fSpec (PTSwitchBoard act) -- the Picture that represents this interface's knowledge graph

graphic :: Options -> Bool
graphic opts = genGraphics opts && theme opts /= StudentTheme


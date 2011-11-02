{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.ChapterDataAnalysis
where 
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
import DatabaseDesign.Ampersand.Basics  
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Classes
import Data.List
import DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms
import DatabaseDesign.Ampersand.Fspec.ToFspec.Calc (reprAsRule)
import DatabaseDesign.Ampersand.Fspec.FPA (fpa) 
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Fspec.Fspec
import Text.Pandoc
import Text.Pandoc.Builder  (toList, codeBlock)
import DatabaseDesign.Ampersand.Output.PredLogic        (PredLogicShow(..), showLatex)
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec.Switchboard      (SwitchBdDiagram(..), switchboardAct,sbDiagram)
import DatabaseDesign.Ampersand.Output.AdlExplanation (purpose,meaning,Explainable(..))
import DatabaseDesign.Ampersand.Output.Statistics (Statistics(..))
import DatabaseDesign.Ampersand.Output.PandocAux

fatal :: Int -> String -> a
fatal = fatalMsg "ChapterDataAnalysis.hs"

------------------------------------------------------------
--DESCR -> the data analysis contains a section for each class diagram in the fspec
--         the class diagram and multiplicity rules are printed
chpDataAnalysis :: Int -> Fspc -> Options -> ([Block],[Picture])
chpDataAnalysis lev fSpec flags
 = ( header ++ 
     daContents ++
     daAssociations remainingDecls ++
     [b | InternalPlug p<-plugInfos fSpec, b<-daPlug p] ++
     [Header (lev+1) [Str $ case language flags of
                              Dutch   ->  "Schakelpaneel"   
                              English ->  "Switchboard"
                     ]] ++
     txtSwitchboard

   , [classificationPicture, classDiagramPicture, picSwitchboard] )
 where 
  remainingDecls = mors fSpec >- [r | p<-plugInfos fSpec, r<-mors p]

  header :: [Block]
  header = labeledHeader lev (xLabel DataAnalisys) (case language flags of
                                              Dutch   ->  "Gegevensstructuur"   
                                              English ->  "Data structure"
                                        )
  -- | a short introduction
  daContents :: [Block]
  daContents = 
   (case language flags of
     Dutch   -> [Para $
                  ( if genGraphics flags 
                    then 
                     ( if null (gens fSpec) then [] else
                       [ Str "Een aantal concepten uit hoofdstuk "
                       , xrefReference FunctionalRequirements
                       , Str " zit in een classificatiestructuur. Deze is in figuur "
                       , xrefReference classificationPicture
                       , Str " weergegeven. " ] 
                     ) ++
                       [ Str "De eisen, die in hoofdstuk "
                       , xrefReference FunctionalRequirements
                       , Str " beschreven zijn, zijn in een gegevensanalyse vertaald naar het gegevensmodel van figuur "
                       , xrefReference classDiagramPicture
                       , Str ". " ]
                    else []
                  )++
                  [ Str (case length (classes classDiagram) of
                            0 -> "Er zijn"
                            1 -> "Er is Ã©Ã©n gegevensverzameling,"
                            _ -> "Er zijn "++count flags (length (classes classDiagram)) "gegevensverzameling"++","
                        )
                  , Str $ " "++count flags (length (assocs classDiagram)) "associatie"++","
                  , Str $ " "++count flags (length (geners classification)) "generalisatie"++" en"
                  , Str $ " "++count flags (length (aggrs classDiagram)) "aggregatie"++"."
                  , Str $ " "++name fSpec++" kent in totaal "++count flags (length (concs fSpec)) "concept"++"."
                  ]]
     English -> [Para $
                  ( if genGraphics flags 
                    then 
                     ( if null (gens fSpec) then [] else
                       [ Str "A number of concepts from chapter "
                       , xrefReference FunctionalRequirements
                       , Str " are organized in a classification structure. This is represented in figure "
                       , xrefReference classificationPicture
                       , Str ". " ] ) ++
                       [ Str "The requirements, which are listed in chapter "
                       , xrefReference FunctionalRequirements
                       , Str ", have been translated into the data model in figure "
                       , xrefReference classDiagramPicture
                       , Str ". " ]
                    else []
                  )++
                  [ Str (case length (classes classDiagram) of
                            0 -> "There are"
                            1 -> "There is one data set,"
                            _ -> "There are "++count flags (length (classes classDiagram)) "data set"++","
                        )
                  , Str $ " "++count flags (length (assocs classDiagram)) "association"++","
                  , Str $ " "++count flags (length (geners classification)) "generalisation"++", and"
                  , Str $ " "++count flags (length (aggrs classDiagram)) "aggregation"++"."
                  , Str $ " "++name fSpec++" has a total of "++count flags (length (concs fSpec)) "concept"++"."
                  ]] --TODO
   ) ++ [ Plain $ xrefFigure1 classificationPicture, Plain $ xrefFigure1 classDiagramPicture ]  -- TODO: explain all multiplicities]

  classDiagram :: ClassDiag
  classDiagram = cdAnalysis fSpec flags

  classification :: ClassDiag
  classification = clAnalysis fSpec flags

  classificationPicture :: Picture
  classificationPicture
    = (makePicture flags fSpec Gen_CG classification)
        {caption = case language flags of
                    Dutch   ->"Classificatie van "++name fSpec
                    English ->"Classification of "++name fSpec}

  classDiagramPicture :: Picture
  classDiagramPicture
   = (makePicture flags fSpec Plain_CG classDiagram)
        {caption = case language flags of
                    Dutch   ->"Datamodel van "++name fSpec
                    English ->"Data model of "++name fSpec}

  picSwitchboard :: Picture
  picSwitchboard
   = (makePicture flags fSpec Plain_CG sb) 
        {caption = case language flags of
                    Dutch   ->"Schakelpaneel van "++name fSpec
                    English ->"Switchboard of "++name fSpec}
     where
       sb :: SwitchBdDiagram
       sb = (sbDiagram fSpec . fSwitchboard) fSpec -- the Picture that represents this interface's knowledge graph

  txtSwitchboard :: [Block]
  txtSwitchboard
   = (case language flags of                                     -- announce the switchboard diagram
           Dutch   -> [Para [ Str "Figuur ", xrefReference picSwitchboard
                            , Str " geeft het schakelpaneel (switchboard diagram) weer. "
                            , Str "Dit wordt gebruikt bij het ontwerpen van de database functionaliteit."] ]
           English -> [Para [ Str "Figure ", xrefReference picSwitchboard
                            , Str " shows the switchboard diagram."
                            , Str "This is used in designing the database functionality."] ]
     )
     ++ [Plain (xrefFigure1 picSwitchboard)]                     -- draw the switchboard

-- The properties of various declations are documented in different tables.
-- First, we document the heterogeneous properties of all relations
-- Then, the endo-poperties are given, and finally
-- the signals are documented.
  daAssociations :: [Relation] -> [Block]
  daAssociations rs = heteroMultiplicities ++ endoProperties ++ keyDocumentation
   where
    heteroMultiplicities
     = case [r | r@Rel{}<-rs, not (isProp r), not (isAttribute r)] of
        []  -> []
        [r] -> [ case language flags of
                   Dutch   ->
                      Para [ Str $ upCap (name fSpec)++" heeft Ã©Ã©n associatie: "++showADL r++". Deze associatie "++ 
                                   case (isTot r, isSur r) of
                                    (False, False) -> "heeft geen beperkingen ten aanzien van multipliciteit."
                                    (True,  False) -> "is totaal."
                                    (False, True ) -> "is surjectief."
                                    (True,  True ) -> "is totaal en surjectief."
                           ]
                   English ->
                      Para [ Str $ upCap (name fSpec)++" has one association: "++showADL r++". This association "++ 
                                   case (isTot r, isSur r) of
                                    (False, False) -> "has no restrictions with respect to multiplicities. "
                                    (True,  False) -> "is total."
                                    (False, True ) -> "is surjective."
                                    (True,  True ) -> "is total and surjective."
                           ]]
        rs' -> case [r | r<-rs', (not.null) ([Tot,Sur] `isc` multiplicities r) ] of
               []  -> []
               [r] -> [ case language flags of
                          Dutch   ->
                             Para [ Str $ upCap (name fSpec)++" heeft "++count flags (length rs) "associatie"++". "
                                  , Str $ " Daarvan is "++showADL r++if isTot r then "totaal" else "surjectief"
                                  ]
                          English   ->
                            Para [ Str $ upCap (name fSpec)++" has "++count flags (length rs) "association"++". "
                                  , Str $ " Association "++showADL r++" is "++if isTot r then "total" else "surjective"
                                  ]
                      ]
               _   -> [ case language flags of
                          Dutch   ->
                             Para [ Str $ upCap (name fSpec)++" heeft de volgende associaties en multipliciteitsrestricties. "
                                  ]
                          English   ->
                             Para [ Str $ upCap (name fSpec)++" has the following associations and multiplicity constraints. "
                                  ]
                      , Table [] [AlignLeft,AlignCenter,AlignCenter] [0.0,0.0,0.0]
                        ( case language flags of
                          Dutch   ->
                               [ [Plain [Str "relatie"]]
                               , [Plain [Str "totaal"]]
                               , [Plain [Str "surjectief"]]]
                          English   ->
                               [ [Plain [Str "relation"]]
                               , [Plain [Str "total"]]
                               , [Plain [Str "surjective"]]]
                        )
                        [[[Plain [Math InlineMath (showMath r)]] -- r is a relation, and therefore isTypeable r. So  showMath r  exists.
                         ,[Plain [Math InlineMath "\\surd" | isTot r]]
                         ,[Plain [Math InlineMath "\\surd" | isSur r]]]
                        | r<-rs', not (isAttribute r)
                        ]
                      ]
    isAttribute r = (not.null) ([Uni,Inj] `isc` multiplicities r)
    endoProperties
     = if null [ m | d<-declarations fSpec, m<-multiplicities d, m `elem` [Rfx,Irf,Trn,Sym,Asy]]
       then []
       else [ Para [ case language flags of
                          Dutch   ->
                            Str $ "Er is Ã©Ã©n endorelatie, "++texOnly_Id(name d)++" met de volgende eigenschappen: "
                          English   ->
                            Str $ "There is one endorelation, "++texOnly_Id(name d)++" with the following properties: "]
            | length hMults==1, d<-hMults ]++
            [ Para [ case language flags of
                          Dutch   ->
                            Str $ "In aanvulling daarop hebben de endorelaties de volgende eigenschappen: "
                          English   ->
                            Str $ "Additionally, the endorelations come with the following properties: "]
            | length hMults>1 ]++
            [Table [] [AlignLeft,AlignCenter,AlignCenter,AlignCenter,AlignCenter,AlignCenter,AlignCenter] [0.0,0.0,0.0,0.0,0.0,0.0,0.0]
             [[case language flags of
                          Dutch   -> Plain [Str "relatie"] 
                          English -> Plain [Str "relation"]  ]
             ,[Plain [Str "Rfx"]]
             ,[Plain [Str "Irf"]]
             ,[Plain [Str "Trn"]]
             ,[Plain [Str "Sym"]]
             ,[Plain [Str "Asy"]]
             ,[Plain [Str "Prop"]]]
             [[[Plain [Math InlineMath (showMath d)]] -- d is a declaration, and therefore typeable. So  showMath d  exists.
              ,[Plain [Math InlineMath "\\surd" | isRfx d ]]
              ,[Plain [Math InlineMath "\\surd" | isIrf d ]]
              ,[Plain [Math InlineMath "\\surd" | isTrn d ]]
              ,[Plain [Math InlineMath "\\surd" | isSym d ]]
              ,[Plain [Math InlineMath "\\surd" | isAsy d ]]
              ,[Plain [Math InlineMath "\\surd" | isAsy d && isSym d ]]]
             | d<-hMults]
            | length hMults>0 ]
       where
        hMults  = [r | r@Rel{}<- mors fSpec, isEndo r]
    keyDocumentation
     = if null (keyDefs fSpec)
       then []
       else [ case language flags of
               Dutch   ->
                 Para  [Str "Er is ",Str preciesEen,Str " key: ",Str (texOnly_Id(name k)),Str "."]
               English ->
                 Para  [Str "There is but one key: ",Str (texOnly_Id(name k)),Str "." ]
            | length keyds==1, k<-keyds ]++
            [ case language flags of
               Dutch   ->
                 Para $ Str "De volgende keys bestaan: ": commaNLPandoc (Str "en") [Str $ texOnly_Id(name k) | k<-keyds]
               English ->
                 Para $ Str "The following keys exist: ": commaEngPandoc (Str "and") [Str $ texOnly_Id(name k) | k<-keyds]
            | length keyds>1 ]
       where
        keyds   = keyDefs fSpec -- all key definitions
-- The properties of various declations are documented in different tables.
-- First, we document the heterogeneous properties of all relations
-- Then, the endo-poperties are given, and finally
-- the process rules are documented.

  daAttributes :: PlugSQL -> [Block]
  daAttributes p
   = if length (tblfields p)<=1 then [] else
     [ case language flags of
               Dutch   ->
                 Para [ Str $ "De attributen van "++name p++" hebben de volgende multipliciteitsrestricties. "
                 ]
               English ->
                 Para [ Str $ "The attributes in "++name p++" have the following multiplicity constraints. "
                 ]
     ,Table [] [AlignLeft,AlignLeft,AlignCenter,AlignCenter] [0.0,0.0,0.0,0.0]
      ( case language flags of
          Dutch   ->
             [[Plain [Str "attribuut"]]
             ,[Plain [Str "type"]]
             ,[Plain [Str "verplicht"]]
             ,[Plain [Str "uniek"]]]
          English ->
             [[Plain [Str "attribute"]]
             ,[Plain [Str "type"]]
             ,[Plain [Str "mandatory"]]
             ,[Plain [Str "unique"]]] )
      [ if isProp (fldexpr fld) && fld/=head (tblfields p)
        then [ [Plain [Str (fldname fld)]]
             , [Plain [ Str "Bool"]]
             , [Plain [Math InlineMath "\\surd"]]
             , []
             ]
        else [ [Plain [if fld==head (tblfields p) || null ([Uni,Inj,Sur]>-multiplicities (fldexpr fld))
                       then Str  "key "
                       else Str (fldname fld)]]
             , [Plain [ (Str . latexEscShw.name.target.fldexpr) fld]]
             , [Plain [Math InlineMath "\\surd" | not (fldnull fld)]]
             , [Plain [Math InlineMath "\\surd" | flduniq fld]]
             ]
      | fld<-tblfields p  -- tail haalt het eerste veld, zijnde I[c], eruit omdat die niet in deze tabel thuishoort.
      ]
      
     ]
-- the endo-properties have already been reported in the general section of this chapter.
{-     where
--  voorgestelde multipliciteitenanalyse....
      clauses = nub [clause | Quad _ ccrs<-vquads fSpec, (_,shifts)<-cl_conjNF ccrs, clause<-shifts]
      is = nub [r | EUni fus<-clauses
                  , isIdent (EIsc [notCpl f | f<-fus, isPos f])
                  , f<-filter isNeg fus
                  , s<-strands f
                  , e<-[head s, flp (last s)]
                  , r<-mors e
                ]
      ts = nub [r | EUni fus<-clauses
                  , isIdent (EIsc [notCpl f | f<-fus, isNeg f])
                  , f<-filter isPos fus
                  , s<-strands f
                  , e<-[head s, flp (last s)]
                  , r<-mors e
                  ]
      strands (ECps fs) = [fs]
      strands _      = []    -- <--  we could maybe do better than this...
      tots = [d | t<-ts, inline t, d<-map makeDeclaration (mors t)]
      unis = [d | t<-is, inline t, d<-map makeDeclaration (mors t)]
      surs = [d | t<-ts, not (inline t), d<-map makeDeclaration (mors t)]
      injs = [d | t<-is, not (inline t), d<-map makeDeclaration (mors t)]
-}

  -- daPlugs describes data sets.
  -- These can be recognized by:
  --    1. the first field has the "unique" attribute on (otherwise it is a binary association)
  --    2. there is more than one field (otherwise it is a scalar).
  -- The text gives all rules that are maintained internally within the data structure,
  -- because they might very well be implemented as database integrity rules.
  -- Multiplicity rules are not reported separately, because they are already taken care of in the multiplicity tables.
  -- Plugs that are associations between data sets and scalars are ignored.

  daPlug :: PlugSQL -> [Block]
  daPlug p
   = if null content then [] else plugHeader ++ content
     where
       plugHeader = labeledHeader (lev+1) ("sct:plug "++name p) (name p)
       content = daAttributes p ++ plugRules ++ plugSignals ++ plugKeydefs ++ iRules
       plugRules
        = case language flags of
           English -> case [r | r<-invariants fSpec, null (mors r >- mors p)] of
                       []  -> []
                       [r] -> [ Para [ Str "Within this data set, the following integrity rule shall be true at all times. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic r)) ]
                                else if isTypeable (rrexp r)
                                     then Para [ Math DisplayMath $ showMath r]
                                     else fatal 1635 ("Untypeable "++show r)
                              ]
                       rs  -> [ Para [ Str "Within this data set, the following integrity rules shall be true at all times. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic r)) ]] | r<-rs ]
                                else BulletList [ if isTypeable (rrexp r)
                                                  then [Para [Math DisplayMath $ showMath r]]
                                                  else fatal 1642 ("Untypeable "++show r)
                                                | r<-rs ]
                              ]
           Dutch   -> case [r | r<-invariants fSpec, null (mors r >- mors p)] of
                       []  -> []
                       [r] -> [ Para [ Str "Binnen deze gegevensverzameling dient de volgende integriteitsregel te allen tijde waar te zijn. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic r)) ]
                                else if isTypeable (rrexp r)
                                     then Para [ Math DisplayMath $ showMath r]
                                     else fatal 1652 ("Untypeable "++show r)
                              ]
                       rs  -> [ Para [ Str "Binnen deze gegevensverzameling dienen de volgende integriteitsregels te allen tijde waar te zijn. " ]
                              , if showPredExpr flags
                                then BulletList [ [Para [ Math DisplayMath (showLatex (toPredLogic r)) ]] | r<-rs ]
                                else BulletList [ if isTypeable (rrexp r)
                                                  then [Para [Math DisplayMath $ showMath r]]
                                                  else fatal 1659 ("Untypeable "++show r)
                                                | r<-rs ]
                              ]
       plugKeydefs
        = case language flags of
           English -> case [k | k<-keyrules fSpec, null (mors k >- mors p)] of
                       []  -> []
                       [s] -> [ Para [ Str "This data set contains one key. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic s)) ]
                                else if isTypeable (rrexp s)
                                     then Para [ Math DisplayMath $ showMath s]
                                     else fatal 1671 ("Untypeable "++show s)
                              ]
                       ss  -> [ Para [ Str "This data set contains the following keys. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic s)) ]] | s<-ss ]
                                else BulletList [ if isTypeable (rrexp s)
                                                  then [Para [Math DisplayMath $ showMath s]]
                                                  else fatal 1678 ("Untypeable "++show s)
                                                | s<-ss ]
                              ]
           Dutch   -> case [k | k<-keyrules fSpec, null (mors k >- mors p)] of
                       []  -> []
                       [s] -> [ Para [ Str ("Deze gegevensverzameling genereert "++preciesEen++" key. ") ] 
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic s)) ]
                                else if isTypeable (rrexp s)
                                     then Para [ Math DisplayMath $ showMath s]
                                     else fatal 1688 ("Untypeable "++show s)
                              ]
                       ss  -> [ Para [ Str "Deze gegevensverzameling genereert de volgende keys. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic s)) ]] | s<-ss ]
                                else BulletList [ if isTypeable (rrexp s)
                                                  then [Para [Math DisplayMath $ showMath s]]
                                                  else fatal 1695 ("Untypeable "++show s)
                                                | s<-ss ]
                              ]
       plugSignals
        = case (language flags, [r | r<-vrules fSpec, isSignal r , null (mors r >- mors p)]) of
    --       English -> case [r | r<-vrules fSpec, isSignal r , null (mors r >- mors p)] of
            (_      , [])  -> []
            (English, [s]) -> [ Para [ Str "This data set generates one process rule. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic s)) ]
                                else if isTypeable (rrexp s)
                                     then Para [ Math DisplayMath $ showMath s]
                                     else fatal 1707 ("Untypeable "++show s)
                              ] 
            (English, ss)  -> [  Para [ Str "This data set generates the following process rules. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic s)) ]] | s<-ss ]
                                else BulletList [ if isTypeable (rrexp s)
                                                  then [Para [Math DisplayMath $ showMath s]]
                                                  else fatal 1714 ("Untypeable "++show s)
                                                | s<-ss ]
                              ]
            (Dutch  ,  [s]) -> [ Para [ Str ("Deze gegevensverzameling genereert "++preciesEen++" procesregel. ") ] 
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic s)) ]
                                else if isTypeable (rrexp s)
                                     then Para [ Math DisplayMath $ showMath s]
                                     else fatal 1724 ("Untypeable "++show s)
                              ]
            (Dutch  ,  ss ) -> [ Para [ Str "Deze gegevensverzameling genereert de volgende procesregels. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic s)) ]] | s<-ss ]
                                else BulletList [ if isTypeable (rrexp s)
                                                  then [Para [Math DisplayMath $ showMath s]]
                                                  else fatal 1731 ("Untypeable "++show s)
                                                | s<-ss ]
                              ]
       iRules
        = case language flags of
           English -> case irs of
                       []  -> []
                       [e] -> [ Para [ Str "The following rule defines the integrity of data within this data set. It must remain true at all times. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic e)) ]
                                else if isTypeable e
                                     then Para [ Math DisplayMath $ showMath e]
                                     else fatal 1743 ("Untypeable "++show e)
                              ]
                       es  -> [ Para [ Str "The following rules define the integrity of data within this data set. They must remain true at all times. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic e)) ]] | e<-es ]
                                else BulletList [ if isTypeable e
                                                  then [Para [Math DisplayMath $ showMath e]]
                                                  else fatal 1750 ("Untypeable "++show e)
                                                | e<-es ]
                              ]
           Dutch   -> case irs of
                       []  -> []
                       [e] -> [ Para [ Str "De volgende regel definieert de integriteit van gegevens binnen deze gegevensverzameling. Hij moet te allen tijde blijven gelden. " ]
                              , if showPredExpr flags
                                then Para [ Math DisplayMath (showLatex (toPredLogic e)) ]
                                else if isTypeable e
                                     then Para [ Math DisplayMath $ showMath e]
                                     else fatal 1760 ("Untypeable "++show e)
                              ]
                       es  -> [ Para [ Str "De volgende regels definiÃ«ren de integriteit van gegevens binnen deze gegevensverzameling. Zij moeten te allen tijde blijven gelden. " ]
                              , if showPredExpr flags
                                then BulletList [[Para [ Math DisplayMath (showLatex (toPredLogic e)) ]] | e<-es ]
                                else BulletList [ if isTypeable e
                                                  then [Para [Math DisplayMath $ showMath e]]
                                                  else fatal 1767 ("Untypeable "++show e)
                                                | e<-es ]
                              ]
          where irs = [EUni fs | Quad r ccrs<-vquads fSpec
                             , r_usr (cl_rule ccrs), isIdent r, source r `elem` pcpts
                             , (_,shifts)<-cl_conjNF ccrs
                             , EUni fs<-shifts
                             , let ns=[t | ECpl t<-fs], length ns==1, ERel nega<-ns
                             , r==nega
                             ]
                pcpts = case p of
                  ScalarSQL{} -> [cLkp p]
                  _           -> map fst (cLkpTbl p)


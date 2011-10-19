{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module DatabaseDesign.Ampersand.Fspec.ShowXMLtiny (showXML)
where
--   import Text.XML.HaXml
--     --Als de compiler hierover struikelt, dan moet je xml installeren. Dat is overigens in de volgende 3 stappen:
--                             -- 1) Eerst installeer je Cabal (zie http://www.haskell.org/cabal/) en dan roep je op je command line: 
--                             -- 2) cabal-install HaXml  (onder windows: cabal install HaXml)
--                             -- 3) er is geen stap 3!
--                             
--     -- Motivatie voor keuze van XML Light. (HJO, 6 feb 2009)
--     -- Oorspronkelijk heb ik gemeend om de Haskell XML Toolbox (hxt) te gebruiken. Die is uitgebreid en 
--     --  maakt het mogelijk om in de toekomst allerlei leuke dingen te gaan doen met de gegenereerde XML. 
--     -- Helaas was het niet mogelijk om dit te gebruiken: Onder Windows kreeg ik het niet aan de praat, omdat
--     --  het gebruik maakt van packages die nog niet standaard beschikbaar zijn.
--     -- Onder debian kreeg ik het ook niet voor elkaar met de standaard installatie manier, omdat in de standaard
--     -- distributie voor debian nog gebruik wordt gemaakt van ghc 6.6 
--     -- Wie weet komt dit later dus nog wel een keer....


-- TODO: Als het Ampersand bestand strings bevat met speciale characters als '&' en '"', dan wordt nu nog foute XML-code gegenereerd...

   import DatabaseDesign.Ampersand.ADL1
--   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import DatabaseDesign.Ampersand.Classes
--   import Languages
   import DatabaseDesign.Ampersand.Fspec.ShowADL 
   import DatabaseDesign.Ampersand.Basics
   import DatabaseDesign.Ampersand.Fspec.Fspec
--   import DatabaseDesign.Ampersand.Fspec.FPA     (FPA(..))
   import Time                                   (ClockTime)
   import DatabaseDesign.Ampersand.Fspec.Plug 
   import DatabaseDesign.Ampersand.Misc.TinyXML 
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "Fspec.ShowXMLtiny"

   showXML :: Fspc -> ClockTime -> String
   showXML fSpec now 
            = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" ++
              "<tns:ADL xmlns:tns=\"http://ampersand.sourceforge.net/ADL\" "++
              "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "++
              "xsi:schemaLocation=\"http://ampersand.sourceforge.net/AdlDocs "++
              "ADL.xsd \">"++
              "<!-- Generated with "++ ampersandVersionStr ++", at "++ show now ++" -->" ++
              showXTree ( mkXmlTree fSpec) ++
              "</tns:ADL>"   

   nameToAttr :: Identified x => x -> XAtt 
   nameToAttr x = mkAttr "name" (name x)

   ----------------------------------------------------------------------
  
   class XML a where 
    mkTag     :: a -> XTag
    mkXmlTree :: a -> XTree
   
   still2bdone :: String -> XTree
   still2bdone worktxt = Node (Tag "NotImplementedYet" [mkAttr "work2do_in_ShowXML.hs"  worktxt])     


   instance XML Fspc where
     mkTag f = Tag "Fspec" [nameToAttr f] 
     mkXmlTree f@(Fspc{})
        = Elem (mkTag f) ( 
             []
--          ++ [ Elem (simpleTag "Plugs-In-Ampersand-Script")     (map mkXmlTree (vplugs f))]
--          ++ [ Elem (simpleTag "Plugs-also-derived-ones") (map mkXmlTree (plugs f))]
          ++ [ Elem (simpleTag "Patterns")     (map mkXmlTree (patterns f))] 
          ++ [ Elem (simpleTag "InterfaceS")   (map mkXmlTree (interfaceS f))] 
          ++ [ Elem (simpleTag "InterfaceG")   (map mkXmlTree (interfaceG f))] 
      --    ++ [ Elem (simpleTag "Activities")   (map mkXmlTree (interfaces f))] 
          ++ [ Elem (simpleTag "Rules")        (map mkXmlTree (vrules f))] 
          ++ [ Elem (simpleTag "GRules")       (map mkXmlTree (grules f))] 
          ++ [ Elem (simpleTag "Declarations") (map mkXmlTree (vrels f))] 
          ++ [ Elem (simpleTag "Violations")   (map violation2XmlTree (violations f))]
          ++ [ still2bdone "Ontology" ] -- ++ [ Elem (simpleTag "Ontology") [mkXmlTree hhh] 
          ++ [ Elem (simpleTag "Explanations") (map mkXmlTree (fSexpls f))]
                 )
             where violation2XmlTree :: (Rule,Paire) -> XTree
                   violation2XmlTree (r,p) = 
                     Elem (Tag "Violation" [] )
                      (
                       Elem (simpleTag "ViolatedRule") [mkXmlTree r]
                       :[Elem (simpleTag "Culprit")[mkXmlTree p]]
                      )
                 
   instance XML Activity where
     mkTag _ = Tag "Activity" [] 
     mkXmlTree act
        = Elem (mkTag act) (  
             [ Elem (simpleTag "Rule")               [mkXmlTree (actRule act)]]
          ++ [ Elem (simpleTag "Editable Relations") (map mkXmlTree (actTrig   act)) |not (null (actTrig   act))] 
          ++ [ Elem (simpleTag "Affected Relations") (map mkXmlTree (actAffect act)) |not (null (actAffect act))] 
          ++ [ Elem (simpleTag "Affected Quads")     []] -- TODO
          ++ [ Elem (simpleTag "ECArules")           (map mkXmlTree (actEcas   act)) |not (null (actEcas   act))] 
          ++ [ Elem (simpleTag "FPA")                [mkXmlTree (actFPA act)]]
          ++ [ Elem (simpleTag "Explanations")       (map mkXmlTree (actXpls   act)) |not (null (actXpls   act))] 
           )

   instance XML FPA where
     mkTag _ = Tag "FPA" [] 
     mkXmlTree fpa'
        = Elem (mkTag fpa') []  -- TODO make content for this XML field

   instance XML Field where
     mkTag _ = Tag "Field" [] 
     mkXmlTree f
        = Elem (Tag "Field"
                    [ mkAttr "Editable" (show (fld_editable f))
                    , mkAttr "list"     (show (fld_list     f))
                    , mkAttr "Must"     (show (fld_must     f))
                    , mkAttr "New"      (show (fld_new      f))
                    , mkAttr "sLevel"   (show (fld_sLevel   f))
                    ])
               ( Elem (simpleTag "Expression") [mkXmlTree (fld_expr f)] :
                 [ Elem (simpleTag "Relation")   [mkXmlTree (fld_rel f)]]
               ) 

   instance XML Pattern where
     mkTag pat = Tag "Pattern" [ nameToAttr pat]
     mkXmlTree pat
        = Elem (mkTag pat) (  
             [ Elem (simpleTag "Rules")        (map mkXmlTree (ptrls pat)) |not (null (ptrls pat))] 
          ++ [ Elem (simpleTag "Gens")         (map mkXmlTree (ptgns pat)) |not (null (ptgns pat))] 
          ++ [ Elem (simpleTag "Declarations") (map mkXmlTree (ptdcs pat)) |not (null (ptdcs pat))] 
          ++ [ Elem (simpleTag "Concepts")     (map mkXmlTree (ptcds pat)) |not (null (ptcds pat))] 
          ++ [ Elem (simpleTag "Keys")         (map mkXmlTree (ptkds pat)) |not (null (ptkds pat))] 
          ++ [ Elem (simpleTag "Explanations") (map mkXmlTree (ptxps pat)) |not (null (ptxps pat))] 
           )

   instance XML Rule where
     mkTag r = Tag "Rule" [mkAttr "ruleId" (name r)]
     mkXmlTree r
      = Elem (mkTag r)
             [Elem (simpleTag "Expression")   [PlainText (showADL (rrexp r))]]
   
   instance XML KeyDef where
     mkTag k = Tag "KeyDef" [nameToAttr k]
     mkXmlTree k = Elem (mkTag k)
                        ( Elem (simpleTag "Key on") [mkXmlTree (kdcpt k)] :
                          attributesTree (kdats k)
                        )


   instance XML Interface where
     mkTag x = Tag "Interface" [ nameToAttr x]
     mkXmlTree x
           = Elem (mkTag x) []
                      --TODO: moet nog verder uitgewerkt.

   
   instance XML ObjectDef where
     mkTag x = Tag "ObjectDef" [ nameToAttr x]
     mkXmlTree x@(Obj{}) 
           = Elem (mkTag x)
                      ( descriptionTree (objctx x)
                     ++ attributesTree (objats x)
                     ++ [Elem (simpleTag "Directives")
                              [PlainText (show (objstrs x))] |not (null (objstrs x))]
                      )    --TODO: De directieven moeten waarschijnlijk nog verder uitgewerkt.


   instance XML Expression where
     mkTag _  = fatal 184 "mkTag should not be used for expressions."
     mkXmlTree expr 
         = case expr of
               (EEqu (l,r))   -> Elem (simpleTag equi) (map mkXmlTree [l,r])
               (EImp (l,r))   -> Elem (simpleTag impl) (map mkXmlTree [l,r])
               (EIsc [])      -> Node (Tag rl [mkAttr "Name" "V"])
               (EIsc [e])     -> mkXmlTree e
               (EIsc es)      -> Elem (simpleTag inter) (map mkXmlTree es)
               (EUni [])      -> Elem (simpleTag compl)  [ Node (Tag rl [mkAttr "Name" "V"])]
               (EUni [e])     -> mkXmlTree e
               (EUni es)      -> Elem (simpleTag union') (map mkXmlTree es)
               (EDif (l,r))   -> Elem (simpleTag diff) (map mkXmlTree [l,r])
               (ELrs (l,r))   -> Elem (simpleTag lres) (map mkXmlTree [l,r])
               (ERrs (l,r))   -> Elem (simpleTag rres) (map mkXmlTree [l,r])
               (ECps [])      -> Node (Tag rl [mkAttr "Name" "I"])
               (ECps [e])     -> mkXmlTree e
               (ECps es)      -> Elem (simpleTag rMul) (map mkXmlTree es)
               (ERad [])      -> Elem (simpleTag compl) [ Node (Tag rl [mkAttr "Name" "I"])]
               (ERad [e])     -> mkXmlTree e
               (ERad es)      -> Elem (simpleTag rAdd) (map mkXmlTree es)
               (EKl0 e)       -> Elem (simpleTag clos0) [mkXmlTree e]
               (EKl1 e)       -> Elem (simpleTag clos1') [mkXmlTree e]
               (EFlp e)       -> Elem (simpleTag flip') [mkXmlTree e]
               (ECpl e)       -> Elem (simpleTag compl) [mkXmlTree e]
               (EBrk e)       -> mkXmlTree e
               (ETyp e sgn)   -> Elem (simpleTag cast) [mkXmlTree e,mkXmlTree (source sgn),mkXmlTree (target sgn)]
               (ERel rel)     -> Elem (simpleTag flip') [mkXmlTree (EFlp (ERel rel))]


      where
      (equi,impl,inter,union',diff,lres,rres,rAdd,rMul,clos0,clos1',compl,flip',cast,rl)
       = ("EQUI","IMPL","CONJ","DISJ","DIFF","LRES","RRES","RADD","RMUL","CLS0","CLS1","CMPL","CONV","CAST","REL")



   instance XML PExplanation where
     mkTag expl =
       Tag "PExpl" atts
        
--        = case expl of
--                PExplConceptDef{}  -> Tag "ExplConceptDef"  atts
--                PExplDeclaration{} -> Tag "ExplDeclaration" atts
--                PExplRule{}        -> Tag "ExplRule"        atts
--                PExplKeyDef{}      -> Tag "ExplKeyDef"      atts
--                PExplPattern{}     -> Tag "ExplPattern"     atts
--                PExplProcess{}     -> Tag "ExplProcess"     atts
--                PExplInterface{}   -> Tag "ExplInterface"   atts
--                PExplContext{}     -> Tag "ExplContext"     atts
--                PExplFspc{}        -> Tag "ExplFspc"        atts
           where
            atts ::  [XAtt]
            atts = [mkAttr "Explains" (name expl)
                   ,mkAttr "Lang" (show(pexLang expl))
                   ,mkAttr "Ref" (pexRefID expl)]
     mkXmlTree expl 
         = Elem (mkTag expl) [PlainText (show (pexExpl expl))]

   instance XML Explanation where
     mkTag expl = Tag "Expl" [mkAttr "Explains" (show expl)
                             ,mkAttr "Lang" (show (explLang expl))
                             ,mkAttr "Ref" (explRefId expl)]

--        = case expl of
--                ExplConceptDef  cdef  lang ref _ -> Tag "ExplConceptDef"  (atts cdef lang ref)
--                ExplDeclaration d     lang ref _ -> Tag "ExplDeclaration" (atts (name d++name(source d)++name(target d)) lang ref)
--                ExplRule        r     lang ref _ -> Tag "ExplRule"        (atts (name r) lang ref)
--                ExplKeyDef      k     lang ref _ -> Tag "ExplKeyDef"      (atts (name k) lang ref)
--                ExplPattern     pname lang ref _ -> Tag "ExplPattern"     (atts pname lang ref)
--                ExplProcess     pname lang ref _ -> Tag "ExplProcess"     (atts pname lang ref)
--                ExplInterface   cname lang ref _ -> Tag "ExplInterface"   (atts cname lang ref)
--                ExplContext     cname lang ref _ -> Tag "ExplContext"     (atts cname lang ref)
--                ExplFspc        cname lang ref _ -> Tag "ExplFspc"        (atts cname lang ref)
--           where
--            atts :: String -> Lang -> String -> [XAtt]
--            atts str lang ref = [mkAttr "Explains" str
--                                ,mkAttr "Lang" (show lang)
--                                ,mkAttr "Ref" ref]
     mkXmlTree expl 
         = Elem (mkTag expl) [PlainText (show (explCont expl))]


   instance XML A_Gen where
     mkTag g = Tag "Gen" (mkAttr "Generic" (show (gengen g))
                          :[mkAttr "Specific" (show (genspc g))]
                         )
     mkXmlTree g = Node (mkTag g) 
   

   instance XML Relation where
     mkTag f = Tag "Relation" [nameToAttr f] 
     mkXmlTree rel = Elem (mkTag rel) 
      (case rel of  
          Rel{} ->  Elem (simpleTag "Source") [mkXmlTree (source rel)]
                    :[Elem (simpleTag "Target") [mkXmlTree (target rel)]]                  
          I{}   ->  [still2bdone "Relation_I"]
          V{}   ->  [still2bdone "Relation_V"]
          Mp1{} ->  [still2bdone "Relation_ONE"]
           ) 


   instance XML Declaration where
     mkTag d = Tag "Association" ([nameToAttr d]
                                ++[ mkAttr "type" t]
                                ++ extraAtts )
            where t = case d of
                        Sgn{} -> "Sgn"
                        Isn{} -> "Isn"
                        Iscompl{} -> "Iscompl"
                        Vs{} -> "Vs"
                  extraAtts = case d of
                                Sgn{} -> [mkAttr "IsSignal" (show (deciss d))]
                                _     -> []
            
     mkXmlTree d = Elem (mkTag d)
        (case d of  
          Sgn{} 
                ->  [Node (Tag "Source" [mkAttr "concept" (name(source d))])]
                  ++[Node (Tag "Target" [mkAttr "concept" (name(target d))])]
                  ++[Elem (simpleTag "MultFrom") [PlainText (multiplicity (multiplicities d))]]
                  ++[Elem (simpleTag "MultTo") [PlainText (multiplicity (flipProps (multiplicities d)))]]
                  ++[Elem (simpleTag "Pragma") 
                             [PlainText (show (prL++"%f"++prM++"%t"++prR))] 
                                | not (null (prL++prM++prR))]
                  ++[Elem (simpleTag "Meaning") 
                             [PlainText (decMean d)]
                                | not (null (decMean d))]
                  ++[Elem (simpleTag "Population") 
                             (map mkXmlTree (decpopu d)) 
                                | not (null (decpopu d))]                 
          Isn{}
                ->  [Elem (simpleTag "Type") [mkXmlTree (source d)]]
          Iscompl{}
                ->  [Elem (simpleTag "Type") [mkXmlTree (source d)]]
          Vs{}
                ->  Elem (simpleTag "Generic") [mkXmlTree (source d)]
                    :[Elem (simpleTag "Specific")[mkXmlTree (target d)]]
           ) 
       where
         multiplicity ms | null ([Sur,Inj]>-ms) = "1"
                         | null (    [Inj]>-ms) = "0..1"
                         | null ([Sur]    >-ms) = "1..n"
                         | otherwise            = "0..n"
         prL = decprL d
         prM = decprM d
         prR = decprR d

   instance XML Paire where
     mkTag p = Tag "link" atts
                where
                   atts = mkAttr "from" (srcPaire p)
                          :[mkAttr "to"   (trgPaire p)]
     mkXmlTree p = Elem (mkTag p) []
                        
   instance XML ConceptDef where
     mkTag f = Tag "ConceptDef" ( nameToAttr f
                                  : [mkAttr "Trace" (cdref f) |not (null (cdref f))])
     mkXmlTree f = Elem (mkTag f) (explainTree (cddef f))
   

   instance XML A_Concept where
     mkTag f = Tag "A_Concept" [nameToAttr f]
     mkXmlTree f
        = Node (mkTag f)  


   instance XML (ECArule) where
     mkTag _ = Tag "ECArule" []
     mkXmlTree _ = still2bdone "ECArule"
   
   instance XML (Declaration->ECArule) where
     mkTag _ = Tag "ECArule" []
     mkXmlTree _ = still2bdone "Declaration->ECArule"
   
   instance XML PlugSQL where --TODO151210 -> tags for BinSQL and ScalarSQL
     mkTag p = Tag "PlugSql" [ nameToAttr p]
     mkXmlTree p 
      = Elem (mkTag p) 
             [ Elem (simpleTag "Fields") (map mkXmlTree (fields p))]
   instance XML SqlField where
      mkTag x = Tag "Field" (   [mkAttr "name" (fldname x)]
                              ++[mkAttr "type" (showSQL (fldtype x))]
                              ++[mkAttr "null" (show (fldnull x))]
                              ++[mkAttr "uniq" (show (flduniq x))]
                              ++[mkAttr "auto" (show (fldauto x))]
                              )
      mkXmlTree sf = Elem (mkTag sf)
                        [Elem (simpleTag "Expression") [mkXmlTree (fldexpr sf)]]
                        
   attributesTree :: [ObjectDef] -> [XTree]
   attributesTree atts = [Elem (simpleTag "Attributes") 
                               (map mkXmlTree atts)    |not(null atts)]

   descriptionTree :: Expression -> [XTree]
   descriptionTree f = [Elem (simpleTag "Description")
                           [mkXmlTree f] ]

   explainTree :: String -> [XTree]
   explainTree str = [Elem (simpleTag "Explanation")
                           [PlainText str] | not (null str)]

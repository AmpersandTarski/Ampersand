{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS -XTypeSynonymInstances -XFlexibleInstances #-}
module XML.ShowXMLtiny (showXML)
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

   import ADL
--   import Languages
   import ShowADL
--   import Data.Explain
   import Data.Fspec
   import Time(ClockTime)
   import Version(versionbanner)
   import Data.Plug
   import XML.TinyXML
   showXML :: Fspc -> ClockTime -> String
   showXML fSpec now 
            = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" ++
              "<tns:ADL xmlns:tns=\"http://www.sig-cc.org/ADL\" "++
              "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "++
              "xsi:schemaLocation=\"http://www.sig-cc.org/AdlDocs "++
              "ADL.xsd \">"++
              "<!-- Generated with "++ versionbanner ++", at "++ show now ++" -->" ++
              showXTree ( mkXmlTree fSpec) ++
              "</tns:ADL>"   

   nameToAttr :: Identified x => x -> XAtt 
   nameToAttr x = mkAttr "name" (name x)

   ----------------------------------------------------------------------
  
   class XML a where 
    mkTag     :: a -> XTag
    mkXmlTree :: a -> XTree
   
   still2bdone :: String -> XTree
   still2bdone worktxt = Node (Tag "NotImplementedYet" [(mkAttr "work2do_in_ShowXML.hs"  worktxt)])     


   instance XML Fspc where
     mkTag f = Tag "Fspec" [nameToAttr f] 
     mkXmlTree f@(Fspc{})
        = Elem (mkTag f) (
             [ Elem (simpleTag "Plugs-In-Ampersand-Script")     (map mkXmlTree (vplugs f))]
          ++ [ Elem (simpleTag "Plugs-also-derived-ones") (map mkXmlTree (plugs f))]
          ++ [ Elem (simpleTag "Patterns") (map mkXmlTree (patterns f))] 
          ++ [ Elem (simpleTag "ServiceS") (map mkXmlTree (serviceS f))] 
          ++ [ Elem (simpleTag "ServiceG") (map mkXmlTree (serviceG f))] 
          ++ [ Elem (simpleTag "Services") (map mkXmlTree (services f))] 
          ++ [ Elem (simpleTag "Rules")    (map mkXmlTree (vrules f))] 
          ++ [ Elem (simpleTag "GRules")   (map mkXmlTree (grules f))] 
          ++ [ Elem (simpleTag "Declarations")(map mkXmlTree (vrels f))] 
          ++ [ Elem (simpleTag "Violations") (map violation2XmlTree (violations f))]
          ++ [ still2bdone "Ontology" ] -- ++ [ Elem (simpleTag "Ontology") [mkXmlTree hhh] 
          ++ [ Elem (simpleTag "Explanations")(map mkXmlTree (fSexpls f))]
                 )
             where violation2XmlTree :: (Rule (Relation Concept),Paire) -> XTree
                   violation2XmlTree (r,p) = 
                     Elem (Tag "Violation" [] )
                      (
                       [Elem (simpleTag "ViolatedRule") [mkXmlTree r]]
                     ++[Elem (simpleTag "Culprit")[mkXmlTree p]]
                      )
                     
   instance XML Fservice where
     mkTag _ = Tag "Fservice" [] 
     mkXmlTree f
        = Elem (mkTag f) (  
             [ Elem (simpleTag "Service")   [mkXmlTree (fsv_objectdef f)]] 
          ++ [ Elem (simpleTag "INSRelations") (map mkXmlTree (fsv_insrels f))|not (null (fsv_insrels  f))] 
          ++ [ Elem (simpleTag "DELRelations") (map mkXmlTree (fsv_delrels f))|not (null (fsv_delrels  f))] 
          ++ [ Elem (simpleTag "Rules")     (map mkXmlTree (fsv_rules      f))|not (null (fsv_rules    f))] 
--          ++ [ Elem (simpleTag "ECArules")  (map mkXmlTree (fsv_ecaRules f))|not (null (fsv_ecaRules f))] 
          ++ [ Elem (simpleTag "Signals")   (map mkXmlTree (fsv_signals    f))|not (null (fsv_signals  f))] 
          ++ [ Elem (simpleTag "Fields")    (map mkXmlTree (fsv_fields     f))|not (null (fsv_fields   f))] 
           )

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
               ( [ Elem (simpleTag "Expression") [mkXmlTree (fld_expr f)]] ++
                 [ Elem (simpleTag "Relation")   [mkXmlTree (fld_mph f)]]
               ) 

   instance XML Pattern where
     mkTag pat = Tag "Pattern" [ nameToAttr pat]
     mkXmlTree pat
        = Elem (mkTag pat) (  
             [ Elem (simpleTag "Rules")        (map mkXmlTree (ptrls pat))|not (null (ptrls pat))] 
          ++ [ Elem (simpleTag "Gens")         (map mkXmlTree (ptgns pat))|not (null (ptgns pat))] 
          ++ [ Elem (simpleTag "Declarations") (map mkXmlTree (ptdcs pat))|not (null (ptdcs pat))] 
          ++ [ Elem (simpleTag "Concepts")     (map mkXmlTree (ptcds pat))|not (null (ptcds pat))] 
          ++ [ Elem (simpleTag "Keys")         (map mkXmlTree (ptkds pat))|not (null (ptkds pat))] 
          ++ [ Elem (simpleTag "Explanations") (map mkXmlTree (ptxps pat))|not (null (ptxps pat))] 
           )

   instance XML (Rule (Relation Concept)) where
     mkTag r = Tag rtype extraAtts
                 where rtype = if isSignal r then "Signal" else "Rule"
                       extraAtts
                             = runumAtt ++ [ if isSignal r then nameToAttr (srrel r) else mkAttr "type" (show (rrsrt r)) ]
                       runumAtt = [mkAttr "ruleId" (show(runum r))]
     mkXmlTree r
      = Elem (mkTag r)
             ([Elem (simpleTag "Invariant") 
                            [PlainText invariantString ]
                   ]
                ++ case (rrsrt r) of 
                     Truth ->  [Elem (simpleTag "Always")
                                      [mkXmlTree (consequent r)]]
                     Implication -> [Elem (simpleTag "If")
                                      [mkXmlTree (antecedent r)]]
                                 ++ [Elem (simpleTag "Then")
                                      [mkXmlTree (consequent r)]] 
                     Equivalence -> [Elem (simpleTag "LHS")
                                      [mkXmlTree (antecedent r)]]
                                 ++ [Elem (simpleTag "RHS")
                                      [mkXmlTree (consequent r)]] 
                     Generalization -> error ("!Fatal (module XML/ShowXMLtiny 142). Consult your dealer!")
             )
      where invariantString ::  String
            invariantString = case ruleType r of
                                 Truth -> showADL (consequent r)
                                 Implication -> showADL (antecedent r)++ " |- " ++ showADL (consequent r)
                                 Equivalence -> showADL (antecedent r)++ " = "  ++showADL (consequent r)
                                 Generalization -> error ("!Fatal (module XML/ShowXMLtiny 149). Consult your dealer!")
   
   instance XML KeyDef where
     mkTag k = Tag "KeyDef" [nameToAttr k]
     mkXmlTree k = Elem (mkTag k)
                        ( [Elem (simpleTag "Key on") [mkXmlTree (kdcpt k)]]
                          ++ attributesTree (kdats k)
                        )

   
   instance XML ObjectDef where
     mkTag x = Tag "ObjectDef" [ nameToAttr x]
     mkXmlTree x@(Obj{}) 
           = Elem (mkTag x)
                      ( descriptionTree (objctx x)
                     ++ attributesTree (objats x)
                     ++ [Elem (simpleTag "Directives")
                              [PlainText (show (objstrs x))]|not (null (objstrs x))]
                      )    --TODO: De directieven moeten waarschijnlijk nog verder uitgewerkt.


   instance XML (Expression (Relation Concept)) where
     mkTag _  = error ("!Fatal (module XML/ShowXMLtiny 171): mkTag should not be used for expressions.")
     mkXmlTree expr 
         = case expr of
               (Tm mph i) | inline mph -> Node (Tag rel ( [mkAttr "Name" (name mph)]
                                                      ++[mkAttr "Source" (name(source mph))]
                                                      ++[mkAttr "Target" (name(target mph))]
                                              )        ) 
                        | otherwise -> Elem (simpleTag flip') [mkXmlTree (Tm (flp mph) i)]
               (Fux [])  -> Elem (simpleTag compl) 
                                [ Node (Tag rel [mkAttr "Name" "V"])]
               (Fux [f]) -> mkXmlTree f
               (Fux fs)  -> Elem (simpleTag union') (map mkXmlTree fs)
               (Fix [])  -> Node (Tag rel [mkAttr "Name" "V"])
               (Fix [f]) -> mkXmlTree f
               (Fix fs)  -> Elem (simpleTag inter) (map mkXmlTree fs)
               (Fdx [])  -> Elem (simpleTag compl) 
                                [ Node (Tag rel [mkAttr "Name" "I"])]
               (Fdx [f]) -> mkXmlTree f
               (Fdx fs)  -> Elem (simpleTag rAdd) (map mkXmlTree fs)
               (F  [])  -> Node (Tag rel [mkAttr "Name" "I"])
               (F  [f]) -> mkXmlTree f
               (F  fs)  -> Elem (simpleTag rMul) (map mkXmlTree fs)
               (K0x f)   -> Elem (simpleTag clos0) [mkXmlTree f]
               (K1x f)   -> Elem (simpleTag clos1) [mkXmlTree f]
               (Cpx f)   -> Elem (simpleTag compl) [mkXmlTree f]
               (Tc f)   -> mkXmlTree f
      where
      (union',inter,rAdd,rMul,clos0,clos1,compl,flip',rel)
       = ("CONJ","DISJ","RADD","RMUL","CLS0","CLS1","CMPL","CONV","REL")



   instance XML PExplanation where
     mkTag expl =
       Tag "PExpl" atts
        
--        = case expl of
--                PExplConceptDef{}  -> Tag "ExplConceptDef"  atts
--                PExplDeclaration{} -> Tag "ExplDeclaration" atts
--                PExplRule{}        -> Tag "ExplRule"        atts
--                PExplKeyDef{}      -> Tag "ExplKeyDef"      atts
--                PExplObjectDef{}   -> Tag "ExplObjectDef"   atts
--                PExplPattern{}     -> Tag "ExplPattern"     atts
--                PExplContext{}     -> Tag "ExplContext"     atts
           where
            atts ::  [XAtt]
            atts = [mkAttr "Explains" (name expl)
                   ,mkAttr "Lang" (show(pexLang expl))
                   ,mkAttr "Ref" (pexRefID expl)]
     mkXmlTree expl 
         = Elem (mkTag expl) [PlainText (show (pexExpl expl))]

   instance XML Explanation where
     mkTag expl = Tag "Expl" [mkAttr "Explains" (name expl)
                             ,mkAttr "Lang" (show (explLang expl))
                             ,mkAttr "Ref" (explRefId expl)]

--        = case expl of
--                ExplConceptDef  cdef  lang ref _ -> Tag "ExplConceptDef"  (atts (name cdef) lang ref)
--                ExplDeclaration d     lang ref _ -> Tag "ExplDeclaration" (atts (name d++name(source d)++name(target d)) lang ref)
--                ExplRule        r     lang ref _ -> Tag "ExplRule"        (atts (name r) lang ref)
--                ExplKeyDef      k     lang ref _ -> Tag "ExplKeyDef"      (atts (name k) lang ref)
--                ExplObjectDef   o     lang ref _ -> Tag "ExplObjectDef"   (atts (name o) lang ref)
--                ExplPattern     pname lang ref _ -> Tag "ExplPattern"     (atts pname lang ref)
--                ExplContext     cname lang ref _ -> Tag "ExplContext"     (atts cname lang ref)
--           where
--            atts :: String -> Lang -> String -> [XAtt]
--            atts str lang ref = [mkAttr "Explains" str
--                                ,mkAttr "Lang" (show lang)
--                                ,mkAttr "Ref" ref]
     mkXmlTree expl 
         = Elem (mkTag expl) [PlainText (show (explCont expl))]


   instance Show c => XML (Gen c) where
     mkTag g = Tag "Gen" ([mkAttr "Generic" (show (gengen g))]
                       ++ [mkAttr "Specific" (show (genspc g))]
                         )
     mkXmlTree g = Node (mkTag g) 
   

   instance XML (Relation Concept) where
     mkTag f = Tag "Relation" [nameToAttr f] 
     mkXmlTree mph = Elem (mkTag mph) 
      (case mph of  
          Mph{} ->  [Elem (simpleTag "Attributes")(map mkXmlTree (mphats mph))]
                    ++[Elem (simpleTag "Source") [mkXmlTree (source mph)]]
                    ++[Elem (simpleTag "Target") [mkXmlTree (target mph)]]                  
          I{}   ->  [still2bdone "Morphism_I"]
          V{}   ->  [still2bdone "Morphism_V"]
          Mp1{} ->  [still2bdone "Morphism_ONE"]
           ) 


   instance XML (Declaration Concept) where
     mkTag d = Tag "Association" ([nameToAttr d]
                                ++[ mkAttr "type" t]
                                ++ extraAtts )
            where t = case d of
                        Sgn{} -> "Sgn"
                        Isn{} -> "Isn"
                        Iscompl{} -> "Iscompl"
                        Vs{} -> "Vs"
                  extraAtts = case d of
                                Sgn{} -> [mkAttr "decId"    (show (decid  d))]
                                       ++[mkAttr "IsSignal" (show (deciss d))]
                                _     -> []
            
     mkXmlTree d = Elem (mkTag d)
        (case d of  
          Sgn{} 
                ->  [Node (Tag "Source" [mkAttr "concept" (name(source d))])]
                  ++[Node (Tag "Target" [mkAttr "concept" (name(target d))])]
                  ++[Elem (simpleTag "MultFrom") [PlainText (multiplicity d)]]
                  ++[Elem (simpleTag "MultTo") [PlainText (multiplicity (flp d))]]
                  ++[Elem (simpleTag "Pragma") 
                             [PlainText (show (prL++"%f"++prM++"%t"++prR))] 
                                | not (null (prL++prM++prR))]
                  ++[Elem (simpleTag "Population") 
                             (map mkXmlTree (decpopu d)) 
                                | not (null (decpopu d))]                 
          Isn _ _ 
                ->  [Elem (simpleTag "Generic") [mkXmlTree (source d)]]
                  ++[Elem (simpleTag "Specific")[mkXmlTree (target d)]]
          Iscompl _ _
                ->  [Elem (simpleTag "Generic") [mkXmlTree (source d)]]
                  ++[Elem (simpleTag "Specific")[mkXmlTree (target d)]]
          Vs _ _
                ->  [Elem (simpleTag "Generic") [mkXmlTree (source d)]]
                  ++[Elem (simpleTag "Specific")[mkXmlTree (target d)]]
           ) 
       where
         multiplicity s | isSur s && isInj s = "1"
                        |                                isInj s = "0..1"
                        | isSur s                                = "1..n"
                        | otherwise                                                  = "0..n"
         prL = decprL d
         prM = decprM d
         prR = decprR d

   instance XML Paire where
     mkTag p = Tag "link" atts
                where
                   atts = [mkAttr "from" (srcPaire p)]
                        ++[mkAttr "to"   (trgPaire p)]
     mkXmlTree p = Elem (mkTag p) []
                        
   instance XML ConceptDef where
     mkTag f = Tag "ConceptDef" ( [nameToAttr f]
                                ++[mkAttr "Trace" (cdref f) |not (null (cdref f))])
     mkXmlTree f = Elem (mkTag f) (explainTree (cddef f))
   

   instance XML Concept where
     mkTag f = Tag "Concept" [nameToAttr f]
     mkXmlTree f
        = Node (mkTag f)  


   instance XML (ECArule c) where
     mkTag _ = Tag "ECArule" []
     mkXmlTree _ = still2bdone "ECArule"
   
   instance XML (Declaration c->ECArule c) where
     mkTag _ = Tag "ECArule" []
     mkXmlTree _ = still2bdone "Declaration->ECArule"
   
   instance XML Plug where
     mkTag p = case p of
                 PlugSql x -> mkTag x
                 PlugPhp x -> mkTag x
     mkXmlTree p = case p of
                 PlugSql x -> mkXmlTree x
                 PlugPhp x -> mkXmlTree x
                 
   instance XML PlugPHP where
     mkTag p = Tag "PlugPhp" [ nameToAttr p]
     mkXmlTree p
      = Elem (mkTag p)
             [ ] -- TODO
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
                        
   attributesTree :: ObjectDefs -> [XTree]
   attributesTree atts = [Elem (simpleTag "Attributes") 
                               (map mkXmlTree atts)    |not(null atts)]

   descriptionTree :: Expression (Relation Concept) -> [XTree]
   descriptionTree f = [Elem (simpleTag "Description")
                           [mkXmlTree f] ]

   explainTree :: String -> [XTree]
   explainTree str = [Elem (simpleTag "Explanation")
                           [PlainText str] | not (null str)]
                          
                               

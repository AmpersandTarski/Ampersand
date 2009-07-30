{-# OPTIONS_GHC -Wall #-}
module ShowXML (showXML)
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
   import FspecDef
   import Adl
   import ShowADL
   import Time(ClockTime)
   import Version(versionbanner)
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

   -----------------some new data types for simple XML structures--------
   data XTree = Elem { etag  :: XTag
                     , etrees :: [XTree]
                     }
              | Node { ntag  :: XTag
                     }
              | PlainText {ptstr :: String}
   data XTag =  Tag  { tName :: String
                     , tAtts :: [XAtt]
                     }
   data XAtt = Att { attName :: String
                   , attValue :: String
                   }


   showXTree :: XTree -> String
   showXTree tree = case tree of
                        Elem{} -> showStart tag
                               ++ (foldr (++) [] (map showXTree (etrees tree)))
                               ++ showEnd tag
                                   where tag = etag tree
                        Node{} -> showNode (ntag tree)
                        PlainText{} -> show (ptstr tree)
   showStart :: XTag -> String
   showStart a = "<" ++ tName a ++ showAtts (tAtts a) ++ ">" 
  
   showAtts :: [XAtt] -> String
   showAtts xs = foldr (++) [] (map showAtt xs)
      where showAtt :: XAtt -> String
            showAtt a= " "++attName a++"="++show (attValue a)

   showEnd :: XTag -> String
   showEnd a = "</" ++ tName a ++ ">"   
   
   showNode :: XTag -> String
   showNode a = "<" ++ tName a ++ showAtts (tAtts a) ++ "/>"   

   mkAttr :: String -> String -> XAtt
   mkAttr nm value = Att nm value
   
   simpleTag :: String -> XTag
   simpleTag nm = Tag nm []
   
   nameToAttr :: Identified x => x -> XAtt 
   nameToAttr x = mkAttr "name" (name x)
   ----------------------------------------------------------------------
  
   class XML a where 
    mkTag     :: a -> XTag
    mkXmlTree :: a -> XTree
   
   still2bdone :: String -> XTree
   still2bdone worktxt = Node (Tag "NotImplementedYet" [(mkAttr "work2do_in_ShowXML.hs"  worktxt)])     


   instance XML Fspc where
     mkTag f = Tag "Fspec" [ nameToAttr f] 
     mkXmlTree f@(Fspc{})
        = Elem (mkTag f) (
             [ Elem (simpleTag "Patterns")   (map mkXmlTree (vpatterns f))] 
          ++ [ Elem (simpleTag "Datasets") (map mkXmlTree (datasets f))] 
          ++ [ Elem (simpleTag "ServiceS") (map mkXmlTree (serviceS f))] 
          ++ [ Elem (simpleTag "ServiceG") (map mkXmlTree (serviceG f))] 
          ++ [ Elem (simpleTag "Services") (map mkXmlTree (services f))] 
          ++ [ Elem (simpleTag "Rules")    (map mkXmlTree (vrules f))] 
          ++ [ Elem (simpleTag "Declarations")(map mkXmlTree (vrels f))] 
          ++ [ still2bdone "Ontology" ] -- ++ [ Elem (simpleTag "Ontology") [mkXmlTree hhh] 
                   )

   instance XML Fservice where
     mkTag _ = Tag "Fservice" [] 
     mkXmlTree f@(Fservice aaa  )
        = Elem (mkTag f) (  
             [ Elem (simpleTag "Service")   [mkXmlTree aaa]] 
           )

   instance XML Pattern where
     mkTag p = Tag "Pattern" [ nameToAttr p]
     mkXmlTree p@(Pat _ aaa bbb ccc ddd eee)
        = Elem (mkTag p) (  
             [ Elem (simpleTag "Rules")       (map mkXmlTree aaa)|not (null aaa)] 
          ++ [ Elem (simpleTag "Gens")        (map mkXmlTree bbb)|not (null bbb)] 
          ++ [ Elem (simpleTag "Declarations")(map mkXmlTree ccc)|not (null ccc)] 
          ++ [ Elem (simpleTag "Concepts")    (map mkXmlTree ddd)|not (null ddd)] 
          ++ [ Elem (simpleTag "Keys")        (map mkXmlTree eee)|not (null eee)] 
           )
           
   instance XML Rule where
     mkTag r = Tag rtype extraAtts
                 where rtype = case r of
                                  Sg{} -> "Signal"
                                  _    -> "Rule"
                       extraAtts
                             = runumAtt ++
                               case r of
                                  Sg{} -> [nameToAttr (srrel r)]
                                  _    -> [mkAttr "type" (show rtype2)]
                       runumAtt = case r of
                                    Fr{} -> []
                                    _    -> [mkAttr "ruleId" (show(runum r))]
                       rtype2 = case r of
                                 Ru{} -> rrsrt r
                                 _    -> Equivalence           
     mkXmlTree r = Elem (mkTag r)
        (case r of  
          Ru _ _ _ _ _ _ _ _ _
                -> [Elem (simpleTag "Invariant") 
                            [PlainText invariantString ]
                   ]
                ++ case (rrsrt r) of 
                     Truth ->  [Elem (simpleTag "Allways")
                                      [mkXmlTree (consequent r)]]
                     Implication -> [Elem (simpleTag "If")
                                      [mkXmlTree (antecedent r)]]
                                 ++ [Elem (simpleTag "Then")
                                      [mkXmlTree (consequent r)]] 
                     Equivalence -> [Elem (simpleTag "LHS")
                                      [mkXmlTree (antecedent r)]]
                                 ++ [Elem (simpleTag "RHS")
                                      [mkXmlTree (consequent r)]] 
                     Generalization -> undefined
                     Automatic      -> undefined    
          Sg _ _ _ _ _ _ _
                ->  explainTree (srxpl r)
                 ++ [mkXmlTree (srsig r)]
          Gc _ _ _ _ _ _ _
                ->  [still2bdone "Rule_Gc"]
          Fr _ _ _  -- represents an automatic computation, such as * or +.
                ->  [still2bdone "Rule_Fr"]
        )
      where invariantString ::  String
            invariantString = case ruleType r of
                                 Truth -> showADL (consequent r)
                                 Implication -> showADL (antecedent r)++ " |- " ++ showADL (consequent r)
                                 Equivalence -> showADL (antecedent r)++ " = "  ++showADL (consequent r)
                                 Generalization -> undefined
                                 Automatic -> undefined
   
   instance XML KeyDef where
     mkTag k = Tag "KeyDef" [nameToAttr k]
     mkXmlTree k = Elem (mkTag k)
                        ( descriptionTree (kdctx k)
                       ++ attributesTree (kdats k)
                       )

   
   instance XML ObjectDef where
     mkTag x = Tag "ObjectDef" [ nameToAttr x]
     mkXmlTree x@(Obj _ _ _ _ _ ) 
           = Elem (mkTag x)
                      ( descriptionTree (objctx x)
                     ++ attributesTree (objats x)
                     ++ [Elem (simpleTag "Directives")
                              [PlainText (show (objstrs x))]|not (null (objstrs x))]
                      )    --TODO: De directieven moeten waarschijnlijk nog verder uitgewerkt.


   instance XML Expression where
     mkTag _  = error ("(module ShowXML) Fatal: mkTag should not be used for expressions.")
     mkXmlTree expr 
         = case expr of
               (Tm mph) | inline mph -> Node (Tag rel ( [mkAttr "Name" (name mph)]
                                                      ++[mkAttr "Source" (name(source mph))]
                                                      ++[mkAttr "Target" (name(target mph))]
                                              )        ) 
                        | otherwise -> Elem (simpleTag flip') [mkXmlTree (Tm (flp mph))]
               (Fu [])  -> Elem (simpleTag compl) 
                                [ Node (Tag rel [mkAttr "Name" "V"])]
               (Fu [f]) -> mkXmlTree f
               (Fu fs)  -> Elem (simpleTag union') (map mkXmlTree fs)
               (Fi [])  -> Node (Tag rel [mkAttr "Name" "V"])
               (Fi [f]) -> mkXmlTree f
               (Fi fs)  -> Elem (simpleTag inter) (map mkXmlTree fs)
               (Fd [])  -> Elem (simpleTag compl) 
                                [ Node (Tag rel [mkAttr "Name" "I"])]
               (Fd [f]) -> mkXmlTree f
               (Fd fs)  -> Elem (simpleTag rAdd) (map mkXmlTree fs)
               (F  [])  -> Node (Tag rel [mkAttr "Name" "I"])
               (F  [f]) -> mkXmlTree f
               (F  fs)  -> Elem (simpleTag rMul) (map mkXmlTree fs)
               (K0 f)   -> Elem (simpleTag clos0) [mkXmlTree f]
               (K1 f)   -> Elem (simpleTag clos1) [mkXmlTree f]
               (Cp f)   -> Elem (simpleTag compl) [mkXmlTree f]
               (Tc f)   -> mkXmlTree f
      where
      (union',inter,rAdd,rMul,clos0,clos1,compl,flip',rel)
       = ("CONJ","DISJ","RADD","RMUL","CLS0","CLS1","CMPL","CONV","REL")


   instance XML Gen where
     mkTag g = Tag "Gen" ([mkAttr "Generic" (show (gengen g))]
                       ++ [mkAttr "Specific" (show (genspc g))]
                         )
     mkXmlTree g = Node (mkTag g) 
   

   instance XML Morphism where
     mkTag f = Tag "Morphism" [nameToAttr f] 
     mkXmlTree mph = Elem (mkTag mph) 
      (case mph of  
          Mph _ _ _ _ _ _ 
                ->  [Elem (simpleTag "Attributes")(map mkXmlTree (mphats mph))]
                  ++[Elem (simpleTag "Source") [mkXmlTree (source mph)]]
                  ++[Elem (simpleTag "Target") [mkXmlTree (target mph)]]                  
          I _ _ _ _ 
                ->  [still2bdone "Morphism_I"]
          V _ _
                ->  [still2bdone "Morphism_V"]
          Mp1 _ _
                ->  [still2bdone "Morphism_ONE"]
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
                                Sgn{} -> [mkAttr "decId"    (show (decid  d))]
                                       ++[mkAttr "IsSignal" (show (deciss d))]
                                _     -> []
            
     mkXmlTree d = Elem (mkTag d)
        (case d of  
          Sgn _ _ _ _ _ _ _ _ _ _ _ _ 
                ->  [Node (Tag "Source" [mkAttr "concept" (name(source d))])]
                  ++[Node (Tag "Target" [mkAttr "concept" (name(target d))])]
                  ++[Elem (simpleTag "MultFrom") [PlainText (multiplicity d)]]
                  ++[Elem (simpleTag "MultTo") [PlainText (multiplicity (flp d))]]
                  ++[Elem (simpleTag "Pragma") 
                             [PlainText (show (prL++"%f"++prM++"%t"++prR))] 
                                | not (null (prL++prM++prR))]
                  ++ explainTree (decexpl d)
                  ++[Elem (simpleTag "Population") 
                             (map mkXmlTreeOfPaire (decpopu d)) 
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
         multiplicity s | Sur `elem` multiplicities s && Inj `elem` multiplicities s = "1"
                        |                                Inj `elem` multiplicities s = "0..1"
                        | Sur `elem` multiplicities s                                = "1..n"
                        | otherwise                                                  = "0..n"
         prL = decprL d
         prM = decprM d
         prR = decprR d
         mkXmlTreeOfPaire :: Paire -> XTree
         mkXmlTreeOfPaire p 
             = Elem tag []
             where tag :: XTag
                   tag = Tag "link" atts
                   atts :: [XAtt]
                   atts = [mkAttr "from" (head p)]
                        ++[mkAttr "to"   (last p)]


   instance XML ConceptDef where
     mkTag f = Tag "ConceptDef" ( [nameToAttr f]
                                ++[mkAttr "Trace" (cdref f) |not (null (cdref f))])
     mkXmlTree f = Elem (mkTag f) (explainTree (cddef f))
   

   instance XML Concept where
     mkTag f = Tag "Concept" [nameToAttr f]
     mkXmlTree f
        = Node (mkTag f)  


   instance XML ECArule where
     mkTag _ = Tag "ECArule" []
     mkXmlTree _ = still2bdone "ECArule"
   
   
   attributesTree :: ObjectDefs -> [XTree]
   attributesTree atts = [Elem (simpleTag "Attributes") 
                               (map mkXmlTree atts)    |not(null atts)]

   descriptionTree :: Expression -> [XTree]
   descriptionTree f = [Elem (simpleTag "Description")
                           [mkXmlTree f] ]

   explainTree :: String -> [XTree]
   explainTree str = [Elem (simpleTag "Explanation")
                           [PlainText str] | not (null str)]
                          
                             

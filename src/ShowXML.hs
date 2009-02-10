 {-# OPTIONS -XFlexibleContexts #-}
module ShowXML (showXML)
where
--   import Text.XML.HaXml
--     --Als de compiler hierover struikelt, dan moet je xml installeren. Dat is overigens in de volgende 3 stappen:
--                             -- 1) Eerst installeer je Cabal (zie http://www.haskell.org/cabal/) en dan roep je op je command line: 
--                             -- 2) cabal-install HaXml
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
   showXML :: Fspc -> String
   showXML f = showXTree ( mkXmlTree f)
   -----------------some new data types for simple XML structures--------
   data XTree = Elem { etag  :: XTag
                     , etrees :: [XTree]
                     }
              | Node { ntag  :: XTag
                     }
              | PlainText {str :: String}
              | Dummy {str :: String} 
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
                        PlainText{} -> show (str tree)
                        Dummy{} -> str tree
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
   still2bdone str= Node (Tag "NotImplementedYet" [(mkAttr "work2do_in_ShowXML.hs"  str)])     


   instance XML Fspc where
     mkTag f = Tag "Fspec" [ nameToAttr f] 
     mkXmlTree f@(Fspc sid aaa bbb ccc ddd eee fff ggg hhh)
        = Elem (mkTag f) (
             [ Elem (simpleTag "Themes")   (map mkXmlTree aaa)] 
          ++ [ Elem (simpleTag "Datasets") (map mkXmlTree bbb)] 
          ++ [ Elem (simpleTag "ServiceS") (map mkXmlTree ccc)] 
          ++ [ Elem (simpleTag "ServiceG") (map mkXmlTree ddd)] 
          ++ [ Elem (simpleTag "Services") (map mkXmlTree eee)] 
          ++ [ Elem (simpleTag "Rules")    (map mkXmlTree fff)] 
          ++ [ Elem (simpleTag "Declarations")(map mkXmlTree ggg)] 
          ++ [ still2bdone "Ontology" ] -- ++ [ Elem (simpleTag "Ontology") [mkXmlTree hhh] 
                   )


   instance XML Ftheme where
     mkTag f = Tag "Ftheme" [nameToAttr f] 
     mkXmlTree f@(Tspc sid aaa bbb)
        = Elem (mkTag f) (
             [ Elem (simpleTag "Units")    (map mkXmlTree aaa) |not (null aaa)] 
          ++ [ Elem (simpleTag "Pattern")  [mkXmlTree bbb]]
          ) 


   instance XML Funit where
     mkTag f = Tag "Funit" [nameToAttr f] 
     mkXmlTree f@(Uspc sid aaa bbb ccc)
        = Elem (mkTag f) (
             [ Elem (simpleTag "Pattern")  [mkXmlTree aaa]] 
          ++ [ Elem (simpleTag "Views")    (map mkXmlTree bbb) |not (null bbb)] 
          ++ [ Elem (simpleTag "Services") (map mkXmlTree ccc) |not (null ccc)] 
           )


   instance XML Fservice where
     mkTag f = Tag "Fservice" [] 
     mkXmlTree f@(Fservice aaa bbb ccc ddd eee fff )
        = Elem (mkTag f) (  
             [ Elem (simpleTag "Service")   [mkXmlTree aaa]] 
          ++ [ Elem (simpleTag "TrBoundary")(map mkXmlTree bbb)] 
          ++ [ Elem (simpleTag "EcaRules") (map mkXmlTree ccc) |not (null ccc)] 
          ++ [ Elem (simpleTag "Dataset")  [mkXmlTree ddd]] 
          ++ [ Elem (simpleTag "Methods")  (map mkXmlTree eee) |not (null eee)] 
          ++ [ Elem (simpleTag "Rules")    (map mkXmlTree fff) |not (null fff)] 
           )


   instance XML FViewDef where
     mkTag f = Tag "FViewDef" [] 
     mkXmlTree f@(Vdef aaa bbb ccc)
        = Elem (mkTag f) (
             [ Elem (simpleTag "View")   [mkXmlTree aaa]] 
          ++ [ Elem (simpleTag "Morphisms") (map mkXmlTree bbb) |not (null bbb)] 
          ++ [ Elem (simpleTag "Expr_Rules")(map tuple (vdExprRules f)) |not (null ccc)] 
                )   
                where tuple :: (Expression,Rule) -> XTree
                      tuple (e,r) = Elem (simpleTag "Tuple" ) 
                                         ([mkXmlTree e]++[mkXmlTree r])

   instance XML ServiceSpec where
     mkTag f = Tag "ServiceSpec" [nameToAttr f] 
     mkXmlTree f@(Sspc sid aaa bbb ccc ddd eee fff ggg)
        = Elem (mkTag f) (
             [ Elem (simpleTag "Sees")   (map mkXmlTree aaa)] 
          ++ [ Elem (simpleTag "Changes") (map mkXmlTree bbb)] 
          ++ [ Elem (simpleTag "InputParams") (map mkXmlTree ccc)] 
          ++ [ Elem (simpleTag "OutputParams") (map mkXmlTree ddd)] 
          ++ [ Elem (simpleTag "Invariants") (map mkXmlTree eee)] 
          ++ [ Elem (simpleTag "Preconditions") [PlainText (show fff)]] 
          ++ [ Elem (simpleTag "Postconditions")[PlainText (show ggg)]] 
          )   

   instance XML ParamSpec where
     mkTag f = Tag "ParamSpec" ([ nameToAttr (pname f)]
                             ++ [ mkAttr "type" (ptype f)]
                               )
     mkXmlTree f = Node (mkTag f)  

   instance XML Pattern where
     mkTag p = Tag "Pattern" [ nameToAttr p]
     mkXmlTree p@(Pat sid aaa bbb ccc ddd eee)
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
          Ru rt antc p cons cpu expla sgn nr pn
                -> [Elem (simpleTag "Invariant") 
                            [PlainText (invariantString r)]
                   ]
                ++ case rt of 
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
                         
          Sg p rule expla sgn nr pn signal
                ->  explainTree (srxpl r)
                 ++ [mkXmlTree rule]
          Gc p antc cons cpu _ _ _
                ->  [still2bdone "Rule_Gc"]
          Fr t d expr pn  -- represents an automatic computation, such as * or +.
                ->  [still2bdone "Rule_Fr"]
        )
      where invariantString :: Rule -> String
            invariantString r = case ruleType r of
                                 Truth -> showADL (consequent r)
                                 Implication -> showADL (antecedent r)++ " |- " ++ showADL (consequent r)
                                 Equivalence -> showADL (antecedent r)++ " = "  ++showADL (consequent r)
        
   
   instance XML KeyDef where
     mkTag k = Tag "KeyDef" [nameToAttr k]
     mkXmlTree k = Elem (mkTag k)
                        ( descriptionTree (kdctx k)
                       ++ attributesTree (kdats k)
                       )

   
   instance XML ObjectDef where
     mkTag x = Tag "ObjectDef" [ nameToAttr x]
     mkXmlTree x@(Obj aaa bbb ccc ddd eee) 
           = Elem (mkTag x)
                      ( descriptionTree (objctx x)
                     ++ attributesTree (objats x)
                     ++ [Elem (simpleTag "Directives")
                              [PlainText (show (objstrs x))]|not (null (objstrs x))]
                      )    --TODO: De directieven moeten waarschijnlijk nog verder uitgewerkt.


   instance XML Expression where
     mkTag e  = error ("(module ShowXML) Fatal: mkTag should not be used for expressions.")
     mkXmlTree e 
         = case e of
               (Tm m) | inline m -> Node (Tag rel ( [mkAttr "Name" (name m)]
                                                   ++[mkAttr "Source" (name(source m))]
                                                   ++[mkAttr "Target" (name(target m))]
                                          )        ) 
                      | otherwise -> Elem (simpleTag flip) [mkXmlTree (Tm (flp m))]
               (Fu [])  -> Elem (simpleTag compl) 
                                [ Node (Tag rel [mkAttr "Name" "V"])]
               (Fu [f]) -> mkXmlTree f
               (Fu fs)  -> Elem (simpleTag union) (map mkXmlTree fs)
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
               (K0 e)   -> Elem (simpleTag clos0) [mkXmlTree e]
               (K1 e)   -> Elem (simpleTag clos1) [mkXmlTree e]
               (Cp e)   -> Elem (simpleTag compl) [mkXmlTree e]
               (Tc f)   -> mkXmlTree f
      where
      (union,inter,rAdd,rMul,clos0,clos1,compl,flip,rel)
       = ("CONJ","DISJ","RADD","RMUL","CLS0","CLS1","CMPL","CONV","REL")


   instance XML Gen where
     mkTag g = Tag "Gen" ([mkAttr "Generic" (show (gengen g))]
                       ++ [mkAttr "Specific" (show (genspc g))]
                         )
     mkXmlTree g = Node (mkTag g) 
   

   instance XML Morphism where
     mkTag f = Tag "Morphism" [nameToAttr f] 
     mkXmlTree m = Elem (mkTag m) 
      (case m of  
          Mph nm pos ats typ yin dcl
                ->  [Elem (simpleTag "Attributes")(map mkXmlTree (mphats m))]
                  ++[Elem (simpleTag "Source") [mkXmlTree (source m)]]
                  ++[Elem (simpleTag "Target") [mkXmlTree (target m)]]                  
          I ats gen spc yin
                ->  [still2bdone "Morphism_I"]
          V ats typ
                ->  [still2bdone "Morphism_V"]
          Mp1 val typ
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
          Sgn nm a b props prL prM prR cs expla pos nr sig
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
          Isn gen spc 
                ->  [Elem (simpleTag "Generic") [mkXmlTree (source d)]]
                  ++[Elem (simpleTag "Specific")[mkXmlTree (target d)]]
          Iscompl gen spc
                ->  [Elem (simpleTag "Generic") [mkXmlTree (source d)]]
                  ++[Elem (simpleTag "Specific")[mkXmlTree (target d)]]
          Vs gen spc
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
     mkTag f = Tag "ECArule" []
     mkXmlTree f = still2bdone "ECArule"
   
   
   attributesTree :: ObjectDefs -> [XTree]
   attributesTree atts = [Elem (simpleTag "Attributes") 
                               (map mkXmlTree atts)    |not(null atts)]

   descriptionTree :: Expression -> [XTree]
   descriptionTree e = [Elem (simpleTag "Description")
                           [mkXmlTree e] ]

   explainTree :: String -> [XTree]
   explainTree str = [Elem (simpleTag "Explanation")
                           [PlainText str] | not (null str)]
   
   invariantStringl :: Rule -> String
   invariantStringl r = case ruleType r of
                         Truth -> showADL (consequent r)
                         Implication -> showADL (antecedent r)++ " |- " ++ showADL (consequent r)
                         Equivalence -> showADL (antecedent r)++ " = "  ++showADL (consequent r)
                         
                             
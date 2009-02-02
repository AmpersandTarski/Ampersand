 {-# OPTIONS -XFlexibleContexts #-}
module ShowXML (showXML)
where

   import FspecDef
   import ADLdataDef

   data XmlTag = Tag {   tagNm :: String
                       , pairs :: [(String,String)]
                     }

   class ShowXML a where
    mkTag    :: a -> XmlTag
    startTag :: a -> String
    endTag   :: a -> String
    showXML  :: a -> String
    
    startTag a = "<" ++ tagNm tag ++ showpairs tag ++ ">"
      where tag = mkTag a
            showpairs t = case pairs t of
                            []       -> ""
                            (a,b):xs -> " "++a++"="++show b               
    endTag a = "</" ++ tagNm (mkTag a) ++ ">"   

   instance ShowXML a => ShowXML [a] where
     mkTag list =  case list of
             []   -> error ( "No tag defined for empty lists! Contact your ADL dealer.")
   --          x:xs -> Tag {tagNm = ("ListOf_" ++ tagNm x), pairs = []} 
             x:xs -> Tag ("ListOf_" ++ tagNm (mkTag x)) []
           
     showXML list = case list of
             [] ->  "<emptylist/>"  
             xs ->  encloseInTags xs (foldr (++) "" (map showXML xs))
                                 
   instance (ShowXML a, ShowXML b) => ShowXML (a, b) where
     mkTag (a,b) = Tag "Tuple" []
 --    showXMLstartTag (p,q) = "<Tuple>"
     showXML (aaa, bbb)
        = encloseInTags (aaa, bbb) 
           ( showXML aaa ++ 
             showXML bbb 
           )
   
     
   encloseInTags :: ShowXML a => a -> String -> String
   encloseInTags a s = startTag a ++ s ++ endTag a
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fspc                          ***
-- \***********************************************************************
   instance ShowXML Fspc where
     mkTag f = Tag "Fspec" [] 
     showXML f@(Fspc aaa bbb ccc ddd eee fff ggg hhh iii)
        = encloseInTags f 
           ( showXML aaa ++ 
             genereertLoop ++ --  showXML bbb ++ 
             showXML ccc ++
             showXML ddd ++
             showXML eee ++
             genereertLoop ++  -- showXML fff ++
             showXML ggg ++
             showXML hhh ++
             show "XXX nog uit te zoeken ISA structuur"
           ) 
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Ftheme                        ***
-- \***********************************************************************

   instance ShowXML Ftheme where
     mkTag f = Tag "Ftheme" [] 
     showXML f@(Tspc aaa bbb ccc)
        = encloseInTags f 
           ( showXML aaa ++ 
             showXML bbb ++
             showXML ccc 
           )
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Funit                         ***
-- \***********************************************************************

   instance ShowXML Funit where
     mkTag f = Tag "Funit" [] 
     showXML f@(Uspc aaa bbb ccc ddd)
        = encloseInTags f 
           ( showXML aaa ++ 
             showXML bbb ++
             showXML ccc ++
             showXML ddd 
           )
     
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fservice                      ***
-- \***********************************************************************

   instance ShowXML Fservice where
     mkTag f = Tag "Fservice" [] 
     showXML f@(Fservice aaa bbb ccc ddd eee fff)
        = encloseInTags f 
           ( showXML aaa ++ 
             showXML bbb ++
             showXML ccc ++
             showXML ddd ++
             showXML eee ++
             showXML fff
           ) 

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Frule                         ***
-- \***********************************************************************

   instance ShowXML Frule where
     mkTag f = Tag "Frule" [] 
     showXML f@(Frul aaa )
        = encloseInTags f 
           ( showXML aaa
           ) 
  
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FViewDef                      ***
-- \***********************************************************************

   instance ShowXML FViewDef where
     mkTag f = Tag "FViewDef" [] 
     showXML f@(Vdef aaa bbb ccc)
        = encloseInTags f 
           ( showXML aaa ++ 
             showXML bbb ++
             showXML ccc 
           )


-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ServiceSpec                   ***
-- \***********************************************************************
   instance ShowXML ServiceSpec where
     mkTag f = Tag "ServiceSpec" [] 
     showXML f@(Sspc aaa bbb ccc ddd eee fff ggg hhh)
        = encloseInTags f 
           ( showXML aaa ++ 
             showXML bbb ++
             showXML ccc ++
             showXML ddd ++
             showXML eee ++
             showXML fff ++
             show ggg ++
             show hhh
           ) 
       
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ParamSpec                     ***
-- \***********************************************************************
   instance ShowXML ParamSpec where
     mkTag f = Tag "ParamSpec" [] 
     showXML f@(Aspc aaa bbb)
        = encloseInTags f 
           ( showXML aaa ++ 
             show bbb
           ) 

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FSid                          ***
-- \***********************************************************************

   instance ShowXML FSid where
     mkTag f = Tag "FSid" [] 
     
     showXML x = case x of
                  NoName  -> encloseInTags x "<NoName>" 
                  FS_id s -> encloseInTags x (show s)
                    

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Architecture                  ***
-- \***********************************************************************


-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Context                       ***
-- \***********************************************************************
   
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Pattern                       ***
-- \***********************************************************************
 
   instance ShowXML Pattern where
     mkTag f = Tag "PatternXXX" [] 
     showXML f
        = encloseInTags f 
           ( show "NOG TE DOEN (Pattern)" )
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Rule                          ***
-- \***********************************************************************
   instance ShowXML Rule where
     mkTag f = Tag "Rule" [("type",rt )]
          where rt = case f of 
                       Sg{} -> "Signal_of_"++show (ruleType f)
                       _    -> show (ruleType f) 
     showXML f = encloseInTags f body 
      where body = case f of 
                    Ru aaa bbb ccc ddd eee fff ggg hhh iii
                       -> ( -- showXML aaa ++ 
                            showXML bbb ++
                            showXML ccc ++
                            showXML ddd ++
                            showXML eee ++
                            show fff ++
                            showXML ggg ++
                            show hhh ++
                            show iii
                           ) 
                    Sg aaa bbb ccc ddd eee fff ggg
                       -> ( showXML aaa ++ 
                            showXML bbb ++
                            show ccc ++
                            showXML ddd ++
                            show eee ++
                            show fff ++
                            showXML ggg
                           ) 
                    Gc aaa bbb ccc ddd eee fff ggg
                       -> ( showXML aaa ++ 
                            showXML bbb ++
                            showXML ccc ++
                            showXML ddd ++
                            showXML eee ++
                            show fff ++
                            show ggg
                           ) 
                    Fr aaa bbb ccc ddd
                       -> ( show aaa ++ 
                            showXML bbb ++
                            showXML ccc ++
                            show ddd
                           ) 
              

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: RuleType                      ***
-- \***********************************************************************
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: KeyDef                        ***
-- \***********************************************************************

   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Population                    ***
-- \***********************************************************************

   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ObjectDef                     ***
-- \***********************************************************************

   instance ShowXML ObjectDef where
     mkTag f = Tag "ObjectDef" [("name",name f )]
     showXML f@(Obj aaa bbb ccc ddd eee)
        = encloseInTags f 
           ( -- show aaa ++  (reeds opgenomen in starttag)
             showXML bbb ++
             showXML ccc ++
             showXML ddd ++
             showStringList eee 
           )
        where showStringList s = "<directives>"++show s ++"</directives>"
                                   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Expression                    ***
-- \***********************************************************************

   instance ShowXML Expression where
     mkTag f = Tag "Expression" []
     showXML f
        = encloseInTags f 
           ( show(show f) )


-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Gen                           ***
-- \***********************************************************************

   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Morphism                      ***
-- \***********************************************************************

   instance ShowXML Morphism where
     mkTag f = Tag "MorphismXXX" []
     showXML f
        = encloseInTags f 
           ( show "NOG TE DOEN (Morphism)" )
   
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Declaration                   ***
-- \***********************************************************************
   instance ShowXML Declaration where
     mkTag f = Tag "Declaration" [("type", typeOf f)]
        where typeOf decl = 
                case decl of
                       Sgn{}     -> "Sgn"
                       Isn{}     -> "Isn"
                       Iscompl{} -> "Iscompl"
                       Vs{}      -> "Vs"
             
     showXML f = encloseInTags f body 
      where body = case f of 
                    Sgn aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk lll
                       -> ( show aaa ++ 
                            showXML bbb ++
                            showXML ccc ++
                            showXML ddd ++
                            show eee ++
                            show fff ++
                            show ggg ++
                            show hhh ++
                            show iii ++
                            showXML jjj ++
                            show kkk ++
                            show lll
                           ) 
                    Isn aaa bbb
                       -> ( showXML aaa ++ 
                            showXML bbb
                          )
                    Iscompl aaa bbb
                       -> ( showXML aaa ++ 
                            showXML bbb
                          )
                    Vs aaa bbb
                       -> ( showXML aaa ++ 
                            showXML bbb
                          )
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: ConceptDef                    ***
-- \***********************************************************************

   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Concept                       ***
-- \***********************************************************************
   instance ShowXML Concept where
     mkTag f = Tag "Concept" []
     showXML f
        = encloseInTags f 
           ( show f )

   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: AutType                       ***
-- \***********************************************************************
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Prop                          ***
-- \***********************************************************************
   instance ShowXML Prop where
     mkTag f = Tag "Prop" []
     showXML f
        = encloseInTags f 
           ( show f )
   

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FilePos                       ***
-- \***********************************************************************
   instance ShowXML FilePos where
     mkTag f = Tag "FilePos" []
     showXML f 
        | f == posNone = ""
        | otherwise    = encloseInTags f ( show f )


   instance ShowXML ECArule where
     mkTag f = Tag "ECAruleXXX" []
     showXML f
        = encloseInTags f 
           ( show "NOG TE DOEN" )

   
     
   genereertLoop = show "HIER HOORT NOG IETS, maar dat genereert een -LOOP-."
   
   

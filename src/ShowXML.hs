 {-# OPTIONS -XFlexibleContexts #-}
module ShowXML (showXML)
where

   import FspecDef
   import ADLdataDef

   class ShowXML a where
    showXMLstartTag :: a -> String
    showXMLendTag   :: a -> String
    showXML     :: ShowXML a =>  a -> String
    showXMLendTag x = case showXMLstartTag x of
                       c : xs    ->    c : '/' : xs
                       otherwise -> error( "could not define endTag for " ++ showXMLstartTag x ++ "! Contact your ADL dealer.")


   instance ShowXML a => ShowXML [a] where
     showXMLstartTag list = case list of
             [] -> error ( "No tag defined for empty lists! Contact your ADL dealer.")
             x:xs -> "<ListOf_"++ removeBrackets (showXMLstartTag x) ++ ">"
        where 
           removeBrackets = reverse.tail.reverse.tail
           
     showXML list = case list of
             [] ->  "<emptylist></emptylist>" 
             xs ->  encloseInTags xs (foldr (++) "" (map showXML xs))
                                 
   instance (ShowXML a, ShowXML b) => ShowXML (a, b) where
     showXMLstartTag (p,q) = "<Tuple>"
     showXML (aaa, bbb)
        = encloseInTags (aaa, bbb) 
           ( showXML aaa ++ 
             showXML bbb 
           )
   
     
   encloseInTags :: ShowXML a => a -> String -> String
   encloseInTags a s = showXMLstartTag a ++ s ++ showXMLendTag a
   tagchar = 'a'
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fspc                          ***
-- \***********************************************************************
   instance ShowXML Fspc where
     showXMLstartTag f = "<Fspec>"
     showXML f@(Fspc aaa bbb ccc ddd eee fff ggg hhh iii)
        = encloseInTags f 
           ( showXML aaa ++ 
             genereertLoop ++ -- showXML bbb ++
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
     showXMLstartTag f = "<Ftheme>"
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
     showXMLstartTag f = "<Funit>"
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
     showXMLstartTag f = "<Fservice>"
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
     showXMLstartTag f = "<Frule>"
     showXML f@(Frul aaa )
        = encloseInTags f 
           ( showXML aaa
           ) 
  
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FViewDef                      ***
-- \***********************************************************************

   instance ShowXML FViewDef where
     showXMLstartTag f = "<FViewDef>"
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
     showXMLstartTag f = "<ServiceSpec>"
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
     showXMLstartTag f = "<ParamSpec>"
     showXML f@(Aspc aaa bbb)
        = encloseInTags f 
           ( showXML aaa ++ 
             show bbb
           ) 

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FSid                          ***
-- \***********************************************************************

   instance ShowXML FSid where
     showXMLstartTag f = "<FSid>"
     
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
     showXMLstartTag f = "<PatternXXX>"
     showXML f
        = encloseInTags f 
           ( show "NOG TE DOEN" )
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Rule                          ***
-- \***********************************************************************
   instance ShowXML Rule where
     showXMLstartTag f = "<RuleXXX>"
     showXML f
        = encloseInTags f 
           ( show "NOG TE DOEN" )

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
     showXMLstartTag f = "<ObjectDef>"
     showXML f@(Obj aaa bbb ccc ddd eee)
        = encloseInTags f 
           ( show aaa ++ 
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
     showXMLstartTag f = "<ExpressionXXX>"
     showXML f
        = encloseInTags f 
           ( show "NOG TE DOEN" )


-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Gen                           ***
-- \***********************************************************************

   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Morphism                      ***
-- \***********************************************************************

   instance ShowXML Morphism where
     showXMLstartTag f = "<MorphismXXX>"
     showXML f
        = encloseInTags f 
           ( show "NOG TE DOEN" )
   
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Declaration                   ***
-- \***********************************************************************
   instance ShowXML Declaration where
     showXMLstartTag f = case f of
             Sgn{}     -> "<DeclarationSgn>"
             Isn{}     -> "<DeclarationIsn>"
             Iscompl{} -> "<DeclarationIscompl>"
             Vs{}      -> "<DeclarationVs>"
             
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
     showXMLstartTag f = "<ConceptXXX>"
     showXML f
        = encloseInTags f 
           ( show "NOG TE DOEN" )

   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: AutType                       ***
-- \***********************************************************************
   
-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Prop                          ***
-- \***********************************************************************
   instance ShowXML Prop where
     showXMLstartTag f = "<PropXXX>"
     showXML f
        = encloseInTags f 
           ( show "NOG TE DOEN" )
   

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FilePos                       ***
-- \***********************************************************************
   instance ShowXML FilePos where
     showXMLstartTag f = "<FilePosXXX>"
     showXML f
        = encloseInTags f 
           ( show "NOG TE DOEN" )






   instance ShowXML ECArule where
     showXMLstartTag f = "<ECAruleXXX>"
     showXML f
        = encloseInTags f 
           ( show "NOG TE DOEN" )

     
   genereertLoop = "HIER HOORT NOG IETS"
   
   

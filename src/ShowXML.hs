 {-# OPTIONS -XFlexibleContexts #-}
module ShowXML (showXML)
where

   import FspecDef
   import Adl

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
             x:xs -> Tag ("ListOf_" ++ tagNm (mkTag x)) []
           
     showXML list = case list of
             [] ->  "<emptylist/>"  
             xs ->  encloseInTags xs (foldr (++) "" (map showXML xs))
                                 
   instance (ShowXML a, ShowXML b) => ShowXML (a, b) where
     mkTag (a,b) = Tag "Tuple" []
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
           ( --showXML aaa ++ 
             genereertLoop ++ --  showXML bbb ++ 
             --showXML ccc ++
             --showXML ddd ++
             --showXML eee ++
             showXML fff ++ --genereertLoop ++  --
             --showXML ggg ++
             --showXML hhh ++
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

-- HIERONDER IS NOG DE OORSPRONKELIJKE VERSIE VAN STEF:   
--   tag indent tg labels str
--    = indent++"<"++tg++ concat [" "++lbl| lbl<-labels] ++if null str then "/>" else ">"++str++"</"++tg++">"
--   tagLn indent tg labels f
--    = chain "\n" [indent++"<"++tg++ concat [" "++lbl| lbl<-labels] ++">",f ("\t"++indent),indent++"</"++tg++">"]
--
--   class XML a where
--    showXML :: a -> String -> String
--
--   instance XML a => XML [a] where
--    showXML xs indent = chain "\n" [showXML x indent| x<-xs]
--
--   instance XML Context where
--    showXML ctx indent -- (Ctx nm on isa world pats rs ds cs ks os pops)
--     = tagLn indent "CONTEXT" ["NAME="++show (name ctx)] (\indent->chain "\n" (inhoud indent))
--     where inhoud indent
--            = [showXML cd  indent| cd<-conceptDefs ctx, name cd `elem` (map name.concs) ctx]++
--              [showXML cd  indent| c<-concs ctx, cd<-[Cd posNone (name c) "" ""], not (name c `elem` map name (conceptDefs ctx))]++
--              [showXML pat indent| pat<-patterns ctx]
--
--   instance XML Pattern where
--    showXML pat indent
--     = tagLn indent "PATTERN" ["NAME="++show (ptnm pat)] (\indent->chain "\n" (inhoud indent))
--     where inhoud indent
--            = [showXML g indent| g<-rd (ptgns pat)]++  -- TODO: remove transitively redundant elements from gen.
--              [showXML d indent| d<-rd (ptdcs pat), not (isSignal d)]++
--              [showXML r indent| r<-ptrls pat]
--
--   instance XML Rule where
--    showXML r@(Sg p rule expla sgn nr pn signal) indent
--     = tagLn indent "SIGNAL"
--                   ( [ "NAME="++show (name signal)
--                     ] ++
--                     ["EXPLANATION="++show expla | not (null expla)]
--                   ) (showXML rule)
--    showXML r@(Ru c antc p cons _ expla sgn nr pn) indent
--     | ruleType r==AlwaysExpr
--       = tagLn indent "RULE"
--                   ( [ "INVARIANT="++show (showADL (consequent r))] ++
--                     [ "TYPE=\"TRUTH\""] ++
--                     [ "EXPLANATION="++show expla | not (null expla)]
--                   ) (showXML (consequent r))
--     | ruleType r==Implication
--       = tagLn indent "RULE"
--                   ( [ "INVARIANT="++show (showADL (antecedent r)++" |- "++showADL (consequent r))] ++
--                     [ "TYPE=\"IMPLICATION\""] ++
--                     [ "EXPLANATION="++show expla | not (null expla)]
--                   ) (\ind->chain "\n" [ tagLn ind "ANTECEDENT" [] (showXML (antecedent r))
--                                       , tagLn ind "CONSEQUENT" [] (showXML (consequent r))
--                                       ]
--                     )
--     | ruleType r==Equivalence
--       = tagLn indent "RULE"
--                   ( [ "INVARIANT="++show (showADL (antecedent r)++" = "++showADL (consequent r))] ++
--                     [ "TYPE=\"EQUIVALENCE\""] ++
--                     [ "EXPLANATION="++show expla | not (null expla)]
--                   ) (\ind->chain "\n" [ tagLn ind "LHS" [] (showXML (antecedent r))
--                                       , tagLn ind "RHS" [] (showXML (consequent r))
--                                       ]
--                     )
--     | otherwise
--       = tagLn indent "RULE"
--                   ( [ "INVARIANT="++show (showADL (antecedent r)++" = "++showADL (consequent r))]++
--                     [ "TYPE=\"EQUIVALENCE\""]
--                   ) (\ind->chain "\n" [ tagLn ind "LHS" [] (showXML (antecedent r))
--                                       , tagLn ind "RHS" [] (showXML (consequent r))
--                                       ]
--                     )
--
--   instance XML Declaration where
--    showXML d@(Sgn nm a b props prL prM prR cs expla pos nr sig) indent
--     = if isSignal d
--       then error ("!Fail: no XML representation for Declaration that is a signal: "++showADL d) else
--       tag indent "ASSOC"
--                   ( [ "NAME="++show (name d)
--                     , "FROM="++show (name (source d))
--                     , "MULTFROM="++show (multiplicity d)
--                     , "TO="++show (name (target d))
--                     , "MULTTO="++show (multiplicity (flp d))
--                     ] ++
--                     ["PRAGMA="++show (prL++"%f"++prM++"%t"++prR) | not (null (prL++prM++prR))]
--                   )
--                   (concat [tag "" "EXPLAIN" [] (show (explain d)) | not (null (explain d))&&explain d/="NONE"])
--       where
--         multiplicity s | Sur `elem` multiplicities s && Inj `elem` multiplicities s = "1"
--                        |                                Inj `elem` multiplicities s = "0..1"
--                        | Sur `elem` multiplicities s                                = "1..n"
--                        | otherwise                                                  = "0..n"
--
--   instance XML ConceptDef where
--    showXML c@(Cd pos nm def ref) indent
--     = tagLn indent "CONCEPT" (["NAME="++show (name c)]++["TRACE="++show ref| not (null ref)]) (\indent->concat [tag indent "EXPLAIN" [] (show def) | not (null def)&&def/="NONE"])
-- --tag indent "EXPLAIN" [] def)
--
--   instance XML Concept where
--    showXML c indent
--     = tag indent "C" ["NAME="++show (name c)] ""
--
--   instance XML Gen where
--    showXML (G pos g s) indent
--     = tag indent "GEN" ["GENERIC=\""++show g++"\"", "SPECIFIC=\""++show s++"\""] ""
--
--   instance XML Expression where
--    showXML e indent = showX e indent
--     where
--      (union,inter,rAdd,rMul,clos0,clos1,compl,flip,rel)
--       = ("CONJ","DISJ","RADD","RMUL","CLS0","CLS1","CMPL","CONV","REL")
--      showX (Tm m) ind  = if inline m then t ind else tagLn ind flip [] t
--                          where d = makeDeclaration m
--                                t ind = tag ind rel [ "NAME="++show (name d), "SRC="++(show.name.source) d, "TRG="++(show.name.target) d ] ""
--      showX (Fu []) ind = tagLn ind compl [] (\ind->tag ind rel [ "NAME=V" ] "")
--      showX (Fu[f]) ind = showX f ind
--      showX (Fu fs) ind = tagLn ind union [] (\ind->chain "\n" [showX f ind| f<-fs])
--      showX (Fi []) ind = tag ind rel [ "NAME=V" ] ""
--      showX (Fi[f]) ind = showX f ind
--      showX (Fi fs) ind = tagLn ind inter [] (\ind->chain "\n" [showX f ind| f<-fs])
--      showX (Fd []) ind = tagLn ind compl [] (\ind->tag ind rel [ "NAME=I" ] "")
--      showX (Fd[t]) ind = showX t ind
--      showX (Fd ts) ind = tagLn ind rAdd [] (\ind->chain "\n" [showX t ind| t<-ts])
--      showX (F [])  ind = tag ind rel [ "NAME=I" ] ""
--      showX (F[t])  ind = showX t ind
--      showX (F ts)  ind = tagLn ind rMul [] (\ind->chain "\n" [showX t ind| t<-ts])
--      showX (K0 e)  ind = tag ind clos0 [] (showX e ind)
--      showX (K1 e)  ind = tag ind clos1 [] (showX e ind)
--      showX (Cp e)  ind = compl++showX e ind
--      showX (Tc f)  ind = showX f ind
--   

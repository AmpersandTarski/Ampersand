  module Fspec2Xml where
   import Char (isAlphaNum, isSpace)
   import CommonClasses (  Identified(name))
   import Collection (Collection(empty, (>-),rd))
   import Auxiliaries
          ( chain, sort') 
 -- TODO: ADLdef en ShowADL hoort hier niet thuis! Deze module moet nog worden aangepast op de nieuwe architectuur. 
   import ADLdef
   import ShowADL


   import CC_aux ( explain)
   import FspecDef
   
   makeXML_depreciated context
    = putStr ("\nXML representation of "++name context++"\n")>>
      writeFile (name context++".xml") (showXML context "")>>
      putStr ("\n"++name context++".xml written\n")
    where
       rs      = rules context



   tag indent tg labels str
    = indent++"<"++tg++ concat [" "++lbl| lbl<-labels] ++if null str then "/>" else ">"++str++"</"++tg++">"
   tagLn indent tg labels f
    = chain "\n" [indent++"<"++tg++ concat [" "++lbl| lbl<-labels] ++">",f ("\t"++indent),indent++"</"++tg++">"]

   class XML a where
    showXML :: a -> String -> String

   instance XML a => XML [a] where
    showXML xs indent = chain "\n" [showXML x indent| x<-xs]

   instance XML Context where
    showXML ctx indent -- (Ctx nm on isa world pats rs ds cs ks os pops)
     = tagLn indent "CONTEXT" ["NAME="++show (name ctx)] (\indent->chain "\n" (inhoud indent))
     where inhoud indent
            = [showXML cd  indent| cd<-conceptDefs ctx, name cd `elem` (map name.concs) ctx]++
              [showXML cd  indent| c<-concs ctx, cd<-[Cd posNone (name c) "" ""], not (name c `elem` map name (conceptDefs ctx))]++
              [showXML pat indent| pat<-patterns ctx]

   instance XML Pattern where
    showXML pat indent
     = tagLn indent "PATTERN" ["NAME="++show (ptnm pat)] (\indent->chain "\n" (inhoud indent))
     where inhoud indent
            = [showXML g indent| g<-rd (ptgns pat)]++  -- TODO: remove transitively redundant elements from gen.
              [showXML d indent| d<-rd (ptdcs pat), not (isSignal d)]++
              [showXML r indent| r<-ptrls pat]

   instance XML Rule where
    showXML r@(Sg p rule expla sgn nr pn signal) indent
     = tagLn indent "SIGNAL"
                   ( [ "NAME="++show (name signal)
                     ] ++
                     ["EXPLANATION="++show expla | not (null expla)]
                   ) (showXML rule)
    showXML r@(Ru c antc p cons _ expla sgn nr pn) indent
     | ruleType r==AlwaysExpr
       = tagLn indent "RULE"
                   ( [ "INVARIANT="++show (showADL (consequent r))] ++
                     [ "TYPE=\"TRUTH\""] ++
                     [ "EXPLANATION="++show expla | not (null expla)]
                   ) (showXML (consequent r))
     | ruleType r==Implication
       = tagLn indent "RULE"
                   ( [ "INVARIANT="++show (showADL (antecedent r)++" |- "++showADL (consequent r))] ++
                     [ "TYPE=\"IMPLICATION\""] ++
                     [ "EXPLANATION="++show expla | not (null expla)]
                   ) (\ind->chain "\n" [ tagLn ind "ANTECEDENT" [] (showXML (antecedent r))
                                       , tagLn ind "CONSEQUENT" [] (showXML (consequent r))
                                       ]
                     )
     | ruleType r==Equivalence
       = tagLn indent "RULE"
                   ( [ "INVARIANT="++show (showADL (antecedent r)++" = "++showADL (consequent r))] ++
                     [ "TYPE=\"EQUIVALENCE\""] ++
                     [ "EXPLANATION="++show expla | not (null expla)]
                   ) (\ind->chain "\n" [ tagLn ind "LHS" [] (showXML (antecedent r))
                                       , tagLn ind "RHS" [] (showXML (consequent r))
                                       ]
                     )
     | otherwise
       = tagLn indent "RULE"
                   ( [ "INVARIANT="++show (showADL (antecedent r)++" = "++showADL (consequent r))]++
                     [ "TYPE=\"EQUIVALENCE\""]
                   ) (\ind->chain "\n" [ tagLn ind "LHS" [] (showXML (antecedent r))
                                       , tagLn ind "RHS" [] (showXML (consequent r))
                                       ]
                     )

   instance XML Declaration where
    showXML d@(Sgn nm a b props prL prM prR cs expla pos nr sig) indent
     = if isSignal d
       then error ("!Fail: no XML representation for Declaration that is a signal: "++showADL d) else
       tag indent "ASSOC"
                   ( [ "NAME="++show (name d)
                     , "FROM="++show (name (source d))
                     , "MULTFROM="++show (multiplicity d)
                     , "TO="++show (name (target d))
                     , "MULTTO="++show (multiplicity (flp d))
                     ] ++
                     ["PRAGMA="++show (prL++"%f"++prM++"%t"++prR) | not (null (prL++prM++prR))]
                   )
                   (concat [tag "" "EXPLAIN" [] (show (explain d)) | not (null (explain d))&&explain d/="NONE"])
       where
         multiplicity s | Sur `elem` multiplicities s && Inj `elem` multiplicities s = "1"
                        |                                Inj `elem` multiplicities s = "0..1"
                        | Sur `elem` multiplicities s                                = "1..n"
                        | otherwise                                                  = "0..n"

   instance XML ConceptDef where
    showXML c@(Cd pos nm def ref) indent
     = tagLn indent "CONCEPT" (["NAME="++show (name c)]++["TRACE="++show ref| not (null ref)]) (\indent->concat [tag indent "EXPLAIN" [] (show def) | not (null def)&&def/="NONE"])
 --tag indent "EXPLAIN" [] def)

   instance XML Concept where
    showXML c indent
     = tag indent "C" ["NAME="++show (name c)] ""

   instance XML Gen where
    showXML (G pos g s) indent
     = tag indent "GEN" ["GENERIC=\""++show g++"\"", "SPECIFIC=\""++show s++"\""] ""

   instance XML Expression where
    showXML e indent = showX e indent
     where
      (union,inter,rAdd,rMul,clos0,clos1,compl,flip,rel)
       = ("CONJ","DISJ","RADD","RMUL","CLS0","CLS1","CMPL","CONV","REL")
      showX (Tm m) ind  = if inline m then t ind else tagLn ind flip [] t
                          where d = makeDeclaration m
                                t ind = tag ind rel [ "NAME="++show (name d), "SRC="++(show.name.source) d, "TRG="++(show.name.target) d ] ""
      showX (Fu []) ind = tagLn ind compl [] (\ind->tag ind rel [ "NAME=V" ] "")
      showX (Fu[f]) ind = showX f ind
      showX (Fu fs) ind = tagLn ind union [] (\ind->chain "\n" [showX f ind| f<-fs])
      showX (Fi []) ind = tag ind rel [ "NAME=V" ] ""
      showX (Fi[f]) ind = showX f ind
      showX (Fi fs) ind = tagLn ind inter [] (\ind->chain "\n" [showX f ind| f<-fs])
      showX (Fd []) ind = tagLn ind compl [] (\ind->tag ind rel [ "NAME=I" ] "")
      showX (Fd[t]) ind = showX t ind
      showX (Fd ts) ind = tagLn ind rAdd [] (\ind->chain "\n" [showX t ind| t<-ts])
      showX (F [])  ind = tag ind rel [ "NAME=I" ] ""
      showX (F[t])  ind = showX t ind
      showX (F ts)  ind = tagLn ind rMul [] (\ind->chain "\n" [showX t ind| t<-ts])
      showX (K0 e)  ind = tag ind clos0 [] (showX e ind)
      showX (K1 e)  ind = tag ind clos1 [] (showX e ind)
      showX (Cp e)  ind = compl++showX e ind
      showX (Tc f)  ind = showX f ind

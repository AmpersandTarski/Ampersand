{-# LINE 1 "ERmodel.lhs" #-}
#line 1 "ERmodel.lhs"
  module ERmodel(erAnalysis) where
   import Char (isSpace, isAlphaNum)
   import CommonClasses ( Identified(name)) 
   import Collection (Collection(empty, (>-),rd)) 
   import Auxiliaries (chain, sort', eqCl)
   import ADLdef
   import ShowADL
   import CC_aux
   import HtmlFilenames (fnContext)

   erAnalysis :: (Language a) => a -> ([ObjectDef],[ObjectDef],[Declaration],[String])
   erAnalysis p = (datasets, generatedServices, rels, ruls)
    where
       generatedServices
        = concat
          [ [ (objdefNew (v (cptS,c)))
                 { objnm  = name c
                 , objats = [ (objdefNew (Tm m))
                                 { objnm  = show (name m++" "++name (target m))
                                 , objats = let ats = [ (objdefNew att) { objnm = showADL att++" "++name (target att) }
                                                      | att<-recur [] (target m)]
                                            in if null ats then []
                                               else ((objdefNew (Tm (mIs (target m))))
                                                        { objnm = name (target m) }):ats
                                 }
                            | m<-relsFrom c, not (isSignal m)]++
                            [ (objdefNew (notCp (normExpr (srsig s)))) {objnm=name m}
                            | m<-relsFrom c, isSignal m, s<-signals p, source m==source s, name (srrel s) == name m ]++
                            [ (objdefNew (notCp (normExpr (flp (srsig s))))) {objnm=name m}
                            | m<-relsFrom c, isSignal m, s<-signals p, source m==target s, name (srrel s) == name m ]
                 }]
            ++let ats = [ (objdefNew (Tm m))
                             { objnm  = show (name m++" "++name (target m))
                             , objats = []
                             }
                        | m<-relsFrom c, not (isSignal m), Tot `elem` multiplicities m]
              in [(objdefNew (Tm (mIs S)))
                    { objnm  = name c++"s"
                    , objats = [ (objdefNew (v(S,c)))
                                    { objnm  = name c++"s"
                                    , objats = ((objdefNew (Tm (mIs c))) { objnm = "nr" }): ats
                                    } ]
                    }| not (null ats)]
          | c<-concs p ]
          where
           relsFrom c = [Mph (name d) posNone [] (source d,target d) True d| d<-declarations p, source d == c]++
                        [flp (Mph (name d) posNone [] (source d,target d) True d)| d<-declarations p, target d == c]
           recur :: [Morphism] -> Concept -> [Expression]
           recur rs c
            = [ F [Tm m| m<-rs++[n]] | n<-new, not (n `elem` rs)] ++
              [ rs' | n<-new, not (n `elem` rs), rs' <-recur (rs++[n]) (target n) ] 
              where new = [m| m<-relsFrom c, not (isSignal m), Tot `elem` multiplicities m]
       datasets
        = [ (objdefNew (v (cptAnything,c))) { objnm  = (name c)
                                            , objats = [ (objdefNew (Tm e)) { objnm = name e }
                                                       | e<-as ]
                                            }
          | c<-concs p, as<-[[a| a<-attrs, source a <= c]], not (null as) ]
       attrs :: [Morphism]
       attrs = [Mph (name d ++ if isFlpFunction d then "Fun" else "") posNone [] (source d,target d) True d | d<-declarations p,    isFunction d] ++
               [flp (Mph (name d++if isFunction d then "Inv" else "") posNone [] (source d,target d) True d)| d<-declarations p, isFlpFunction d] ++
               [     Mph (name d) posNone [] (source d,target d) True d | d<-declarations p, isProperty d]
  --     ss = [d| d<-declarations p {- , take 5 (name d) /= "Clos_" -} ]
  --     rels' = [d| d<-declarations p,      d `elem` declarations [a| e<-datasets, a<-attributes e]]
       rels  = [d| d<-declarations p, not (d `elem` declarations [a| e<-datasets, a<-attributes e])]
       ruls = [showADL r| r<-declaredRules p]














   csvcontent contexts contextname
    = putStr ("\nCSV content for every object definition in "++name context)>>
      foldr1 (>>) [ writeFile (fnObject a) (shEnts (entConts context a))>>
                    putStr ("\n"++fnObject a++" written")
                  | a@(Obj nm pos ctx ats) <- attributes context ] >>
      putStr ("\nwritten\n")
      where
       rs      = declaredRules context
       context = if null ctxs then error ("!Mistake: "++contextname++" not encountered in input file.\n") else head ctxs
       ctxs    = [c| c<-contexts, name c==contextname]
       (datasets,generatedServices,relations,ruls) = erAnalysis context
       fnObject c = "list"++name c++".csv"
       shEnts = chain "\n" . map (chain ";")
       entConts context (Obj nm pos c ats) = [] -- moet inhoud opleveren, maar moet nog worden gebouwd.
  -- TODO: dit werkend maken:      = ([name c]++[name a| a<-ats]) : (foldr1 mrg [[[src p,trg p]| p<-(sort' src.contents) a] | a<-ats])
       mrg l@((x:xs):xss) r@((y:ys):yss)
        | x<y       = (x:xs++[""|y<-ys]) : mrg xss r
        | x>y       = (y:[""|x<-xs]++ys) : mrg l yss
        | otherwise = (x:xs++ys)         : mrg xss yss
       mrg xs [] = xs
       mrg [] ys = ys

   erModel contexts contextname
    = putStr ("\nEAR analysis for "++name context++"\n")>>







 --     (writeFile (fnContext context++"_EARD.dot"). erdDataModel) context                    >>
 --     putStr (fnContext context++"_EARD.dot written\n")                                     >>
      putStr ("Entity-relation analysis temporarily out of order\n")
 --     (writeFile (fnContext context++"_ERD.dot"). erdConceptual) context                    >>
 --     putStr (fnContext context++"_ERD.dot written\n")
      where
       rs      = declaredRules context
       context = if null ctxs then error ("!Mistake: "++contextname++" not encountered in input file.\n") else head ctxs
       ctxs    = [c| c<-contexts, name c==contextname]
       shR r   = showADL r







 --  type Entity = (Concept,[(Morphism,[Rule])])






































   erdConceptual :: Context -> String
   erdConceptual context
    = "graph "++[x|x<-name context,not (isSpace x)]++"_ERD"++
      chain sep (introG++nodesPlaces) ++
      "\n   }"
      where
        ms = declarations context >- map makeDeclaration (mors (closExprs context))
        (datasets,generatedServices,relations,ruls) = erAnalysis context
        attrs = [a| e<-datasets, a<-attributes e]
        sep = "\n   ; "
        nodesPlaces = ["node [shape=box]"]++
  -- all attrs are to be drawn as ellipses
                      ["{node [shape=ellipse,label=\""++name att++" : "++
                       (name.source.ctx) att++"\"] ATT_"++name att++"} ; "++
                       (name.target.ctx) att++" -- ATT_"++name att
                      | att<-attrs]++
                      ["edge  [len=1.5]"]++
  -- all relations are to be drawn as diamonds
                      [ "{node [shape=diamond,style=filled,color=lightgrey,label=\""++name r++
                        "\"] REL_"++showFullRelName r++"} ; REL_"++showFullRelName r++" -- "++(show.name.target) r++" [arrowhead=\"teetee\"]"
                      |r<-relations]++
                      [ "{node [shape=diamond,style=filled,color=lightgrey,label=\""++name r++
                        "\"] REL_"++showFullRelName r++"} ; "++
                        (show.name.source) r++" -- REL_"++showFullRelName r++
                        if null([Sur,Inj]>-multiplicities r) then " [arrowtail=\"teetee\"]" else ""
                      |r<-relations]++
                      [ "{node [shape=diamond,style=filled,color=lightgrey,label=\""++name r++
                        "\"] REL_"++showFullRelName r++"} ; "++
                        (show.name.source) r++" -- REL_"++showFullRelName r++" -- "++(show.name.target) r
                      |r<-relations::[Declaration]]
        introG = [ "\n   { edge [len=1.0]"
                 , "overlap = false"
 --                , "splines = true"
                 , "label   = \"Entity Attribute Relationship Diagram for "++name context++"\"" ]
        fullnames = [m | cl<-eqCl name relations, length cl>1, m<-cl]
        shrtnames = [m | [m]<-eqCl name relations]
        nameR r = [c|c<-showFullRelName r, isAlphaNum c]

   erdDataModel :: Context -> String
   erdDataModel context
    = "graph "++[x|x<-name context,not (isSpace x)]++"_ERD"++
      chain sep (introG++nodesPlaces) ++
      "\n   }"
      where
        ms = declarations context >- map makeDeclaration (mors (closExprs context))
        (datasets,generatedServices,relations,ruls) = erAnalysis context
        sep = "\n   ; "
        concepts = [nm| Obj nm pos ctx ats<-attributes context] -- all concepts
        attrs = [a| o<-datasets, a<-attributes o]
        nodesPlaces = ["node [shape=box]"]++
                      ["{node [shape=ellipse,label=\""++name att++" : "++
                       (name.source.ctx) att++"\"] ATT_"++name att++"} ; "++
                       (show.name.source.ctx) att++" -- ATT_"++name att
                      | att<-attrs]++
                      ["edge  [len=1.5]"]++
                      [ "{node [shape=diamond,style=filled,color=lightgrey,label=\""++name r++
                        "\"] REL_"++showFullRelName r++"} ; "++
                        (show.name.source) r++" -- REL_"++showFullRelName r++" -- "++(show.name.target) r
                      |r<-relations]
        introG = [ "\n   { edge [len=1.0]"
                 , "overlap = false"
 --                , "splines = true"
                 , "label   = \"Entity Attribute Relationship Diagram for "++name context++"\"" ]
        fullnames = [m | cl<-eqCl name relations, length cl>1, m<-cl]
        shrtnames = [m | [m]<-eqCl name relations]
        nameR r = [c|c<-showFullRelName r, isAlphaNum c]


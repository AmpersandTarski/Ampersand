> module ERmodel(erAnalysis) where  -- commented modules are required for testing
>  import Char (isSpace, isAlphaNum)
>  import CommonClasses ( Identified(name)) 
>  import Collection (Collection(empty, (>-),rd)) 
>  import Auxiliaries (chain, sort', eqCl)
>  import CC_aux 
>            ( Context, src, contents, trg, showADL, Concept, Morphism(Mph), Rule, Key, Concept(Anything {- ,NOthing,C,S -})
>            , Language( rules, declaredRules, objectdefs )
>            , Declaration, concs, source, target, isFunction, isFlpFunction, posNone
>            , flp, isProperty, declarations, declaration, mors, closExprs, showFullRelName
>            , Expression(F,Tm), v
>            , Prop(Sur,Inj),multiplicities
>            , ObjectDef(Obj), Object(concept, attributes, ctx)
>            )
>--  import Calc
>  import HtmlFilenames (fnContext)

>  erAnalysis :: (Language a) => a -> ([ObjectDef],[Declaration],[String])
>  erAnalysis p = (entities, rels, ruls)
>   where
>      entities 
>       | null (objectdefs p) = [ Obj (name c) posNone (v (Anything,c)) [Obj ((concat.map name.mors) e) posNone (Tm e) []| e<-as]
>                               | c<-concs p, as<-[[a| a<-attrs, source a <= c]], not (null as) ]
>       | otherwise           = objectdefs p
>      attrs :: [Morphism]
>      attrs = [Mph (name s ++ if isFlpFunction s then "Fun" else "") posNone [] (source s,target s) True s | s<-declarations p,    isFunction s] ++
>              [flp (Mph (name s++if isFunction s then "Inv" else "") posNone [] (source s,target s) True s)| s<-declarations p, isFlpFunction s] ++
>              [     Mph (name s) posNone [] (source s,target s) True s | s<-declarations p, isProperty s]
> --     ss = [d| d<-declarations p {- , take 5 (name d) /= "Clos_" -} ]
> --     ents = [concept e| e<-entities]
> --     rels' = [s| s<-declarations p,      s `elem` declarations [a| e<-entities, a<-attributes e]]
>      rels  = [s| s<-declarations p, not (s `elem` declarations [a| e<-entities, a<-attributes e])]
>      ruls = [showADL r| r<-declaredRules p]

       ruls = ["foldr f ("++showADL r++") "++show [(s,e)| (s,e)<-substitutions, s `elem` declarations r]| r<-declaredRules p]
       substitutions = [ (s,F [Tm (flp l),Tm (Mph (name s) posNone [] (source l,target s) True s)])
                       | s<-rels', target s `elem` ents
                       , l<-attrs, target l==source s ]++
                       [ (s,F [Tm (Mph (name s) posNone [] (source s,source r) True s),Tm r])
                       | s<-rels', source s `elem` ents
                       , r<-attrs, target r==target s ]++
                       [ (s,F [Tm (flp l),Tm (Mph (name s) posNone [] (source s,source r) True s),Tm r])
                       | s<-rels'
                       , not (source s `elem` ents) && not (target s `elem` ents)
                       , l<-attrs, target l==source s
                       , r<-attrs, target r==target s ]

>  csvcontent contexts contextname
>   = putStr ("\nCSV content for every object definition in "++name context)>>
>     foldr1 (>>) [ writeFile (fnObject a) (shEnts (entConts context a))>>
>                   putStr ("\n"++fnObject a++" written")
>                 | a@(Obj nm pos ctx ats) <- attributes context ] >>
>     putStr ("\nwritten\n")
>     where
>      rs      = declaredRules context
>      context = if null ctxs then error ("!Mistake: "++contextname++" not encountered in input file.\n") else head ctxs
>      ctxs    = [c| c<-contexts, name c==contextname]
>      (entities,relations,ruls) = erAnalysis context
>      fnObject c = "list"++name c++".csv"
>      shEnts = chain "\n" . map (chain ";")
>      entConts context (Obj nm pos c ats) = [] -- moet inhoud opleveren, maar moet nog worden gebouwd.
> -- TODO: dit werkend maken:      = ([name c]++[name a| a<-ats]) : (foldr1 mrg [[[src p,trg p]| p<-(sort' src.contents) a] | a<-ats])
>      mrg l@((x:xs):xss) r@((y:ys):yss)
>       | x<y       = (x:xs++[""|y<-ys]) : mrg xss r
>       | x>y       = (y:[""|x<-xs]++ys) : mrg l yss
>       | otherwise = (x:xs++ys)         : mrg xss yss
>      mrg xs [] = xs
>      mrg [] ys = ys

>  erModel contexts contextname
>   = putStr ("\nEAR analysis for "++name context++"\n")>>

             (chain "\n\n" . map shR ) rs++"\n\n"++
             chain "\n\n" [line| cl<-eqCl frMorph (dbIns rs++dbDel rs), not (null (frMorph (head cl)))
                               , line<-[dbShow cl], not (null line)]++"\n")                 >>
     (writeFile (fnContext context++"_Petri.dot"). dbGraph (name context)) context          >>
     putStr (fnContext context++"_Petri.dot written\n") >>

>--     (writeFile (fnContext context++"_EARD.dot"). erdDataModel) context                    >>
>--     putStr (fnContext context++"_EARD.dot written\n")                                     >>
>     putStr ("Entity-relation analysis temporarily out of order\n")
>--     (writeFile (fnContext context++"_ERD.dot"). erdConceptual) context                    >>
>--     putStr (fnContext context++"_ERD.dot written\n")
>     where
>      rs      = declaredRules context
>      context = if null ctxs then error ("!Mistake: "++contextname++" not encountered in input file.\n") else head ctxs
>      ctxs    = [c| c<-contexts, name c==contextname]
>      shR r   = showADL r

In the type definition, an entity is represented by a concept (for instance Person)
together with a number of attributes (e.g. name, address, ssn). An attribute (e.g. name) consists
of a morphism m. Attribute m is both Uni and Tot (i.e. a mapping).

Since the introduction of objects, the type Entity is obsolete:

>--  type Entity = (Concept,[(Morphism,[Rule])])

  dbGraph :: Language a => String -> a -> String
  dbGraph nm pat
   = "graph "++[x|x<-nm,not (isSpace x)]++"DB_Petrinet"++
     chain sep (introG++nodesPlaces++nodes++arcsPT++concat arcsTP) ++
     "\n   }"
     where
       (entities,relations,ruls) = erAnalysis pat
       rs = declaredRules pat
       dbrs = dbIns rs++dbDel rs
       sep = "\n   ; "
       dI = rd[nameG (declaration fr)| d@(DBR 'I' _ _ _)<-dbrs, fr<-frMorph d]
       dD = rd[nameG (declaration fr)| d@(DBR 'D' _ _ _)<-dbrs, fr<-frMorph d]
       nodesPlaces = ["node [shape=ellipse]"]++
                     [ "{node [label=\""++showS s++"\"] "++nameG s++"}" | s<-fullnames]++
                     [chain "; " (map nameG (shrtnames))]++
                     ["node [shape=rectangle]"]++
                     [chain "; " (map nameG (rd [s|o@(Obj nm pos c ats)<-attributes context, (e,rs)<-as, s<-declarations e]))]
       nodes = [ "node [shape=box,height=.5,width=.1,style=filled]"
               , "{node [label=\"I\"] I_"++ chain "; I_" dI++"}"
               , "{node [label=\"D\"] D_"++ chain "; D_" dD++"}"
               ]
       arcsPT = [node++" -- D_"++node | node<-dD]++[node++" -- I_"++node | node<-dI]
       arcsTP = [ rd[ "I_"++nameG (declaration fr)++" -- "++nameG (declaration to)++" [label=\""++chain "," (map show nrs)++"\"]"| d@(DBR 'I' _ _ nrs)<-cl, fr<-frMorph d, to<-toMorph d]++
                  rd[ "D_"++nameG (declaration fr)++" -- "++nameG (declaration to)++" [label=\""++chain "," (map show nrs)++"\"]"| d@(DBR 'D' _ _ nrs)<-cl, fr<-frMorph d, to<-toMorph d]
                | cl<-eqCl frMorph dbrs]
       introG = [ "\n   { edge [dir=forward,len=1.4]"
                , "overlap = false"
                , "splines = true"
                , "label   = \"Database triggers for "++nm++"\"" ]
       nameG s = if s `elem` fullnames then showFullRelName s else
                 if s `elem` shrtnames then rEncode (name s) else
                 if isFunction s then name (source s) else name (target s)
       fullnames = [s | cl<-eqCl name relations, length cl>1, s<-cl]
       shrtnames = [s | [s]<-eqCl name relations]

De nu volgende analyse produceert een conceptueel datamodel.

>  erdConceptual :: Context -> String
>  erdConceptual context
>   = "graph "++[x|x<-name context,not (isSpace x)]++"_ERD"++
>     chain sep (introG++nodesPlaces) ++
>     "\n   }"
>     where
>       ms = declarations context >- map declaration (mors (closExprs context))
>       (entities,relations,ruls) = erAnalysis context
>       attrs = [a| e<-entities, a<-attributes e]
>       sep = "\n   ; "
>       nodesPlaces = ["node [shape=box]"]++
> -- all attrs are to be drawn as ellipses
>                     ["{node [shape=ellipse,label=\""++name att++" : "++
>                      (name.source.ctx) att++"\"] ATT_"++name att++"} ; "++
>                      (name.target.ctx) att++" -- ATT_"++name att
>                     | att<-attrs]++
>                     ["edge  [len=1.5]"]++
> -- all relations are to be drawn as diamonds
>                     [ "{node [shape=diamond,style=filled,color=lightgrey,label=\""++name r++
>                       "\"] REL_"++showFullRelName r++"} ; REL_"++showFullRelName r++" -- "++(show.name.target) r++" [arrowhead=\"teetee\"]"
>                     |r<-relations]++
>                     [ "{node [shape=diamond,style=filled,color=lightgrey,label=\""++name r++
>                       "\"] REL_"++showFullRelName r++"} ; "++
>                       (show.name.source) r++" -- REL_"++showFullRelName r++
>                       if null([Sur,Inj]>-multiplicities r) then " [arrowtail=\"teetee\"]" else ""
>                     |r<-relations]++
>                     [ "{node [shape=diamond,style=filled,color=lightgrey,label=\""++name r++
>                       "\"] REL_"++showFullRelName r++"} ; "++
>                       (show.name.source) r++" -- REL_"++showFullRelName r++" -- "++(show.name.target) r
>                     |r<-relations::[Declaration]]
>       introG = [ "\n   { edge [len=1.0]"
>                , "overlap = false"
>--                , "splines = true"
>                , "label   = \"Entity Attribute Relationship Diagram for "++name context++"\"" ]
>       fullnames = [m | cl<-eqCl name relations, length cl>1, m<-cl]
>       shrtnames = [m | [m]<-eqCl name relations]
>       nameR r = [c|c<-showFullRelName r, isAlphaNum c]

>  erdDataModel :: Context -> String
>  erdDataModel context
>   = "graph "++[x|x<-name context,not (isSpace x)]++"_ERD"++
>     chain sep (introG++nodesPlaces) ++
>     "\n   }"
>     where
>       ms = declarations context >- map declaration (mors (closExprs context))
>       (entities,relations,ruls) = erAnalysis context
>       sep = "\n   ; "
>       concepts = [nm| Obj nm pos ctx ats<-attributes context] -- all concepts
>       attrs = [a| o<-entities, a<-attributes o]
>       nodesPlaces = ["node [shape=box]"]++
>                     ["{node [shape=ellipse,label=\""++name att++" : "++
>                      (name.source.ctx) att++"\"] ATT_"++name att++"} ; "++
>                      (show.name.source.ctx) att++" -- ATT_"++name att
>                     | att<-attrs]++
>                     ["edge  [len=1.5]"]++
>                     [ "{node [shape=diamond,style=filled,color=lightgrey,label=\""++name r++
>                       "\"] REL_"++showFullRelName r++"} ; "++
>                       (show.name.source) r++" -- REL_"++showFullRelName r++" -- "++(show.name.target) r
>                     |r<-relations]
>       introG = [ "\n   { edge [len=1.0]"
>                , "overlap = false"
>--                , "splines = true"
>                , "label   = \"Entity Attribute Relationship Diagram for "++name context++"\"" ]
>       fullnames = [m | cl<-eqCl name relations, length cl>1, m<-cl]
>       shrtnames = [m | [m]<-eqCl name relations]
>       nameR r = [c|c<-showFullRelName r, isAlphaNum c]


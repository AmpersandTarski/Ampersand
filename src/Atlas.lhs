> module Atlas (anal) where
>  import System.IO.Unsafe (unsafePerformIO) -- maakt het aanroepen van neato vanuit Haskell mogelijk.
>  import Char (isUpper,chr,ord)
>  import CommonClasses (  Identified(name)
>                        , Collection(empty, (>-)))
>  import Auxiliaries (chain, eqCl, rd, sort', sord', mumble) 
>  import Classification 
>           ( Classification(Cl)
>            , root, preCl, locates )
>  import Typology 
>           ( Inheritance(Isa)
>            ,Typology(Typ), Typologic(typology)
>            ,makeTrees)
>  import CC_aux 
>           (  Context(Ctx), Concept(C), Pattern(Pat)
>            , Morphism
>            , isa, showHS, concs, declarations
>            , rules, nr, source, target
>            , Prop(Uni,Tot,Inj,Sur,Sym,Asy,Trn,Rfx)
>            , Rule(Gc)
>            , showADL, explain
>            , Paire, Pairs
>            , ConceptDef(Cd)
>            , Expression( Tm,Cp)
>            , Gen(G)
>            , ruleType, specs, idsOnly, isNot, applyM, declaration
>            , consequent, antecedent, mors, isIdent
>            , multiplicities, plural
>            , Lang(English)
>            , contents
>            , Declaration(Sgn)
>            , genE, wrld, patterns, src, trg, uncomp, isSignal
>            , conts, cod, signals, clearG, conceptdefs, dom, subst
>           )
>--  import Calc  -- only because of two calls of subst
>  import Hatml
>           ( htmlPage,htmlBody,htmlDropDown,htmlname,htmlHeadinglevel
>            ,htmlAnchor,htmlBold,htmlBlueTable,htmlItalic,htmlHead,
>            ,hshow,htmlImageMap,htmlTable,htmlNumbered,htmlValNumbered,
>            ,avoidJSreservedwords,HTML)
>  import HtmlFilenames
>           ( fnContext,fnPatConcept,fnPattern,fnRule,fnConcept
>            ,fnRelation)
>  import PredLogic 
>           ( explainArt,lang,assemble,normRule,objOrShow
>            ,PredLogic,predLshow,mathVars)
>  import ClassDiagram 
>           ( cdDataModel)
>  import Graphic 
>           ( processCdDataModelFile, dotGraph, processDotgraphFile)

>  testing = False

>  anal contexts contextname predLogic graphicstyle
>   = putStr ("\nGenerating Atlas for "++name context++" in the current directory."++
>             "\n  (current directory must already contain the directory \"treemenutils\""++
>             " with its complete contents)\n")                                               >>
> -- writing the large rhs frame (concept) with empty contents
>     writeFile "Concept.html" (htmlPage "Concept" leader (htmlBody introtext))               >>
>     putStr ("Concept.html written,\n")                                                      >>
> -- writing context switching (top left)
>     writeFile ("CTX_"++fnContext context++".html") (contextFrame (Cl context world))        >>
>     putStr("HTML code for context tree "++fnContext context++".html written\n")             >>
> -- writing the main html page, containing the required frames
>     writeFile "index.html" (indexcode (fnContext context++".html"))                         >>
>     putStr ("index.html written,\n")                                                        >>
> -- writing the content for all contexts in ctxTree
>     (if testing then (writeFile "test.txt".showHS.preCl) (Cl context world) else
>      putStr "")                                                                             >>
>     putStr "\nStarting generation of navigators\n"                                          >>
>     sequence_ [navigators cTrees c predLogic graphicstyle| c<-preCl (Cl context world)]     >>
>     putStr "\nStarting generation of analysis\n"                                            >>
>     sequence_ [genAnalysis c predLogic| c<-preCl (Cl context world)]
>     where
>      context  = (head ([c| c<-contexts, name c==contextname]++
>                         [Ctx (contextname++" is not defined") [] empty [] [] [] [] []]))
>      cTrees   = makeTrees (Typ (map reverse pths))
>      Typ pths = typology (isa context)
>      gE    = genE context
>      world = wrld context
>      contextFrame ctxTree@(Cl context cls)
>       = htmlPage ("Code (0) for "++name context++" navigator")
>                  (wrapCode (name context)
>                            (hmenu ctxTree))
>                  loadcode
>          where
>           rec (Cl context cls)
>            = ("<A HREF=\"JavaScript: trigger('"++name context++".html"++
>              "');\" TITLE="++show (name context)++
>              "><IMG SRC=\"treemenutils/menu_new_root.gif\" ALIGN=\"left\" BORDER=\"0\" VSPACE=\"0\" HSPACE=\"0\" /></A>") :
>              concat (map rec cls)
>  navigators :: [Classification Concept] -> Context -> Bool -> String -> IO ()
>  navigators cTrees context predLogic graphicstyle
> -- Part 1: First generate the html code and pictures for the entire context
>       = putStr ("\nAnalyzing context "++name context++"\n")         >>
>         graphSpecs thisCtx context                                  >>
>         putStr ("\ngraphSpecs completed\n")                         >>
>         cdSpecs context (fnContext context) False context           >>
>         putStr ("cdSpecs completed\n")                              >>
>         writeFile ("NAV_"++thisCtx++".html") (navCodeCtx context)   >> -- the 'southwest' navigator for patterns, relations and concepts.
>         putStr ("NAV_"++thisCtx++".html written\n")         >>
>         writeFile (thisCtx++".html") (htmlContext thisCtx context)  >> -- the main page for this context
>         putStr ("HTML code for "++name context++" navigator ("++thisCtx++".html) written\n") >>
> -- navigator ("NAV_"++thisCtx++".html") calls each pat<-patterns context, and all concepts c<-concs (declarations context)
> -- Generate the html code and pictures for each pattern in this context
>         sequence_ [ graphSpecs (fnPattern context pat) pat                                     >> -- the graphical specifications for this pattern's main page
>                     cdSpecs context (fnPattern context pat) True pat                           >> -- generate fully fledge (full==True) class diagram
>                --     putStr (htmlPattern context (fnPattern context pat) pat)                   >> -- main page for this pattern
>                     writeFile (fnPattern context pat++".html")
>                               (htmlPattern context (fnPattern context pat) pat)                >> -- main page for this pattern
>                     writeFile ("NAV_"++fnPattern context pat++".html") (navCodePat context pat)>> -- the 'southwest' navigator for this pattern, which substitutes the context's navigator.
>                     sequence_ [ writeFile ("NAV_"++fnPatConcept context pat c++".html")
>                                (navCodePat context pat)
>                               | c<-concs (declarations pat)]                                   >>
>                     sequence_ [ traverse [] subtree
>                               | subtree<-(sort' name.cTrees.declarations) pat]                 >>
>                     putStr ("HTML code for "++name pat++": "++fnPattern context pat++".html, NAV_"++fnPattern context pat++".html, written\n")
>                   | pat<-patterns context]                       >>
> -- Now generate html code and pictures for each rule in this context
>         sequence_ [ graphSpecs (fnRule context r) (Pat ("Rule "++show (nr r)) [r] [] [] [] [])         >>
>                     putStr ("Graph for Rule "++show (nr r)++": "++fnRule context r++" written\n")      >>
>                     writeFile (fnRule context r++".html") (htmlRule context r predLogic)                       >>
>                     putStr ("HTML code for rule "++show (nr r)++": "++fnRule context r++".html written\n")
>                   | r<-rules context]              >>
> -- Part 2: Generate the html code and pictures for each concept in this context
>         sequence_ [ graphSpecs (fnConcept context c) cPat                                              >>
>                     writeFile (fnConcept context c++".html")
>                               (htmlViewpoint context (fnConcept context c) cPat c (Cl c cls) predLogic)>>
>                     putStr ("HTML code for concept "++name c++": "++fnConcept context c++".html written\n") >>
>                     writeFile ("NAV_"++fnConcept context c++".html") (navCodeVp context cPat)            >>
>                     putStr ("HTML code for navigator NAV_"++fnConcept context c++".html written\n")
>                   | c<-concs context, Cl c' cls<-take 1 [t| t'<-cTrees context, t<-locates c t']
>                   , cPat<-[viewpoint context c]]                        >>
> -- navCodePat contains an anchor for each c<-concs (declarations pat)
> -- Part 3: Now generate navigators for each concept in each pattern
>         sequence_ [ sequence_ [writeFile ("NAV_"++fnPatConcept context pat c++".html")
>                                          (navCodePop context pat c) |c<-concs pat]
>                   | pat<-patterns context]
>          where
>           cdSpecs context fnm full b
>            = writeFile (fnm++"_CD.dot") (cdDataModel context full "dot" b)      >>
>              putStrLn ("Class diagram "++fnm++"_CD.dot written... ")      >>
>              processCdDataModelFile  (fnm ++"_CD")
>           graphSpecs fnm b
>            = writeFile (fnm++".dot") (dotGraph context graphicstyle fnm b)         >>
>              putStrLn ("Graphics specification "++fnm++".dot written... ") >>
>              processDotgraphFile  fnm
>           cTrees cs
>            = (makeTrees . Typ)
>               [reverse pth| Typ pths<-[typology (isa context)], pth<-pths, last pth `elem` concs cs]
>           traverse :: [Concept] -> Classification Concept -> IO ()
>           traverse [] c@(Cl r nav) = sequence_ [traverse [r] cl| cl<-nav]
>           traverse (e:trace) c@(Cl r nav)
>            = graphSpecs thisVpt vptPat                                              >>
>              (writeFile (thisVpt++".html"). htmlPage (htmlNm r) "" . htmlBody) h    >>
>              putStr (thisVpt++".dot and "++thisVpt++".html"++" written\n")          >>
>              sequence_ [traverse (trace++[r]) cl| cl<-nav]
>              where fnm     = thisCtx++htmlNm e++htmlNm r
>                    h       = htmlViewpoint context thisVpt vptPat e c predLogic
>                    vptPat  = inhViewpoint (Cl context (wrld context)) e r
>                    thisVpt = thisCtx++htmlNm e++htmlNm r
>           thisCtx = fnContext context
>           navCodeCtx :: Context -> String
>           navCodeCtx context
>            = htmlPage ("Code (1) for "++name context++" concepts navigator")
>                       ("<SCRIPT type=\"text/javascript\">\nfunction SwitchPat(value)\n { parent.concepts.document.location.href=value+'.html'\n ; parent.menu.document.location.href='NAV_'+value+'.html'\n }\n"++(if null ms then "" else "function SwitchPop(value)\n { parent.concepts.document.location.href=value+'.html' }\n")++"</SCRIPT>")
>                       (htmlBody
>                          ("Patterns\n<BR>\n"++
>                           htmlDropDown
>                             "SwitchPattern"
>                             ["onChange=\"SwitchPat(this.value)\""]
>                             ( ("value="++show(fnContext context), "All patterns"):
>                               [("value="++show(fnPattern context pat), name pat)
>                               | pat<-patterns context]
>                             )++
>                           if null ms then "" else
>                           "\n<P>\nRelations\n<BR />\n"++
>                           htmlDropDown "Select Relation" ["onChange=\"SwitchPop(this.value)\""] (("value="++show(fnContext context),"---"):ms)++
>                           "\n<P>\nConcepts\n<BR />\n"++
>                           htmlDropDown
>                            "SwitchConcept"
>                            ["onChange=\"SwitchPat(this.value)\""]
>                            ( ("value="++show(fnContext context), "---")
>                              : [("value="++show(fnConcept context c), name c)
>                                | c<-(sort' name.concs.declarations) context]
>                            )++
>--                           "\n<P>"++htmlAnchor ("ANAL"++fnContext context++".html")  "Analysis" ["TARGET=concepts"]++
>                          "\n"
>                          )
>                       )
>              where ms = [( "value="++show(fnRelation context decl)
>                          , name decl++"["++name (source decl)++"*"++name (target decl)++"]"
>                          )
>                         | decl<-sord' name (declarations (rules context)++declarations context)]
>                    thisCtx = fnContext context
>           navCodeVp context cPat
>            = htmlPage ("Code (2) for "++name context++" concepts navigator")
>                       ("<SCRIPT type=\"text/javascript\">\nfunction SwitchPat(value)\n { parent.concepts.document.location.href=value+'.html'\n ; parent.menu.document.location.href='NAV_'+value+'.html'\n }\nfunction SwitchView(value)\n { parent.concepts.document.location.href=value+'.html' }\n"++(if null ms then "" else "function SwitchPop(value)\n { parent.concepts.document.location.href=value+'.html' }\n")++"</SCRIPT>")
>                       (htmlBody
>                          ("Patterns\n<BR />\n"++
>                           htmlDropDown
>                             "SwitchPattern"
>                             ["onChange=\"SwitchPat(this.value)\""]
>                             ( ("value="++show(fnContext context), "All patterns"):
>                               [("value="++show(fnPattern context pat), name pat)
>                               | pat<-patterns context]
>                             )++
>                           (if null ms then "" else
>                           "\n<P>\nRelations\n<BR />\n"++
>                           htmlDropDown "Select Relation" ["onChange=\"SwitchPop(this.value)\""] (("value="++show(fnPattern context cPat),"---"):ms) )++
>                           "\n<P>\nConcepts\n<BR />\n"++
>                           htmlDropDown
>                            "SwitchConcept"
>                            ["onChange=\"SwitchView(this.value)\" onClick=\"SwitchView(this.value)\""]
>                            ( ("value="++show(fnContext context), "---")
>                              : [("value="++show (fnConcept context c), name c)
>                                | c<-(sort' name.concs.declarations) context]
>                            )++
>--                           "\n<P>\n"++htmlAnchor ("ANAL"++htmlname (name context)++".html")  "Analysis" ["TARGET=concepts"]++
>                          "\n"
>                          )
>                       )
>              where ms = [( "value="++show(fnRelation context decl)
>                          , name decl++"["++name (source decl)++"*"++name (target decl)++"]"
>                          )
>                         | decl<-sord' name (declarations (rules cPat)++declarations cPat)]
>                    thisCtx = fnContext context
>                    thisPat = htmlNm cPat
>           navCodePat :: Context -> Pattern -> String
>           navCodePat context cPat
>            = htmlPage ("Code (3) for "++name context++" concepts navigator")
>                       ("<SCRIPT type=\"text/javascript\">\nfunction SwitchPat(value)\n { parent.concepts.document.location.href=value+'.html'\n ; parent.menu.document.location.href='NAV_'+value+'.html'\n }\n"++(if null ms then "" else "function SwitchPop(value)\n { parent.concepts.document.location.href=value+'.html' }\n")++"</SCRIPT>")
>                       (htmlBody
>                          ("Patterns\n<BR />\n"++
>                           htmlDropDown
>                             "SwitchPattern"
>                             ["onChange=\"SwitchPat(this.value)\""]
>                             ( ("value="++show(fnContext context), "All patterns"):
>                               [("value="++show(fnPattern context p)++
>                                  if name cPat==name p then " selected" else "",name p)
>                               | p<-patterns context]
>                             ) ++
>                           if null ms then "" else
>                           "\n<P>\nRelations\n<BR />\n"++
>                           htmlDropDown "Select Relation" ["onChange=\"SwitchPop(this.value)\""] (("value="++show(fnPattern context cPat),"---"):ms)++
>--                           "\n<P>\n"++htmlAnchor ("ANAL"++htmlname (name context)++".html")  "Analysis" ["TARGET=concepts"]++
>                          "\n"
>                          )
>                       )  
>              where ms = [( "value="++show(fnRelation context decl)
>                          , name decl++"["++name (source decl)++"*"++name (target decl)++"]"
>                          )
>                         | decl<-sord' name (declarations (rules cPat)++declarations cPat)]
>                    thisCtx = fnContext context
>                    thisPat = htmlNm cPat
>           navCodePop :: Context -> Pattern -> Concept -> String
>           navCodePop context cPat c
>            = htmlPage ("Code (4) for "++name context++" concepts navigator")
>                       ("<SCRIPT type=\"text/javascript\">\nfunction SwitchPat(value)\n { parent.concepts.document.location.href=value+'.html'\n ; parent.menu.document.location.href='NAV_'+value+'.html'\n }\nfunction SwitchView(value)\n { parent.concepts.document.location.href=value+'.html' }\n"++(if null ms then "" else "function SwitchPop(value)\n { parent.concepts.document.location.href=value+'.html'}\n")++"</SCRIPT>")
>                       (htmlBody
>                          ("Patterns\n<BR />\n"++
>                           htmlDropDown
>                             "SwitchPattern"
>                             ["onChange=\"SwitchPat(this.value)\""]
>                             ( ("value="++show(fnContext context), "All patterns"):
>                               [("value="++show(fnPattern context p)++
>                                  if name cPat==name p then " selected" else "",name p)
>                               | p<-patterns context]
>                             )++
>                           if null ms then "" else
>                           "\n<P>\nRelations\n<BR />\n"++
>                           htmlDropDown "Select Relation" ["onChange=\"SwitchPop(this.value)\""] ms++
>                           "\n<P>\nConcepts\n<BR />\n"++
>                           htmlDropDown
>                            "SwitchConcept"
>                            ["onChange=\"SwitchPat(this.value)\" onClick=\"SwitchView(this.value)\""]
>                            [("value="++show(fnPatConcept context cPat c')++
>                              if name c==name c' then " selected" else "",name c')
>                            | c'<-(sort' name.concs.declarations.rules) cPat]  -- might be 'grules' rather than 'rules'
>                          )++
>--                          "\n<P>\n"++htmlAnchor ("ANAL"++htmlname (name context)++".html")  "Analysis" ["TARGET=concepts"]++
>                          "\n"
>                       )
>              where ms = [( "value="++show(fnRelation context decl)
>                          , name decl++"["++name (source decl)++"*"++name (target decl)++"]"
>                          )
>                         | decl<-(sord' name.declarations) [r|r<-rules context, c `elem` concs r]]

>  genAnalysis context predLogic
>   = sequence_ (map (genRel context) (declarations context))>>
>     writeFile ("ANAL"++htmlname (name context)++".html")
>               (htmlPage ("Analysis of "++name context) ""
>                         ( htmlBody (htmlHeadinglevel 1 ("Analysis of "++name context) []++"\n"++
>                                     htmlDeclarations context (declarations context)++"\n<P>\n"++
>                                     if null violations then "" else
>                                     htmlHeadinglevel 2 ("Not all rules are satisfied in context "++name context) []++"\n"++
>                                     violations
>               )         )          )
>   where
>     violations = concat [ hv r| r<-rules context]++
>                  if null viol then "" else "\n<P>\n"++chain "\n\n" viol
>     hv r = if null ruleviol then "" else
>            htmlAnchor (fnRule context r++".html") ("Rule") []++
>            ": \'"++htmlBold (explainArt context English r)++"\' "++
>            " is violated in the following cases:\n<BR />"++
>            ruleviol++"<BR />\n"
>            where ruleviol = htmlViolations r
>     viol = [ "Relation "++
>              htmlAnchor (show(fnRelation context s++".html"))
>               (name s) []++" :: "++htmlAnchor (fnConcept context (source s)++".html") a []++" * "++
>                                                    htmlAnchor (fnConcept context (target s)++".html") b []++
>               " yields "++show (length mp)++" violation"++(if length mp>1 then "s" else "")++",<BR />\nbecause "++
>              sentence s a b p++htmlBlueTable "red"
>                                [htmlAnchor (fnConcept context (source s)++".html")
>                                            ("<FONT COLOR=white>"++a++"</FONT>") []
>                                ,htmlAnchor (fnConcept context (target s)++".html")
>                                            ("<FONT COLOR=white>"++b++"</FONT>") []
>                                ] mp
>            | s<-declarations context, a<-[name (source s)], b<-[name (target s)]
>            , p<-[Uni,Tot,Sur,Inj], p `elem` multiplicities s, mp<-[multViolations s p], not (null mp)]
>     sentence s a b Uni = if a==b
>                          then "each "++a++" in the left hand column may not have more than one "++b++" on the right. (This relation is univalent)"
>                          else "each "++a++" may not have more than one "++b++". (This relation is univalent)"
>     sentence s a b Tot = "there is a missing "++b++" for each of the following "++plural English a++". (This relation is total)"
>     sentence s a b Sur = "there is a missing "++a++" for each of the following "++plural English b++". (This relation is surjective)"
>     sentence s a b Inj = if a==b
>                          then "each "++b++" in the right hand column may not have more than one "++b++" to its left. (This relation is injective)"
>                          else "each "++b++" may not have more than one "++b++". (This relation is injective)"
>     
>  genRel context s
>       | otherwise = writeFile (fnRelation context s++".html")
>                      (htmlPage (name s++" :: "++a++" * "++b) ""
>                                ( htmlBody (htmlHeadinglevel 2 ("Relation: "++name s++" :: "++
>                                                                htmlAnchor (fnConcept context (source s)++".html") a []++" * "++
>                                                                htmlAnchor (fnConcept context (target s)++".html") b []) []++
>                                            nijssenZin s++
>                                            (if null cs then "" else tabl) ++ viol
>                      )         )          )>>
>                     putStr ("\nHTML code for "++fnRelation context s++".html written")
>       where a  = name (source s)
>             b  = name (target s)
>             cs = contents s
>             nijssenZin (Sgn _ _ _ _ [] [] [] _ _ _ _) = "No natural language meaning is assigned to this relation.<P>"
>             nijssenZin (Sgn _ _ _ _ l s r [] _ _ _)
>              = "A tuple <I>x</I>,<I>y</> in the following table means:<BR />"++l++"&lt;x&gt;"++s++"&lt;y&gt;"++r++".<P>"
>             nijssenZin (Sgn _ _ _ _ l s r (c:cs) _ _ _)
>              = "A tuple, for example <I>("++src c++","++trg c++")</I> in the following table means:<BR />"++l++src c++s++trg c++r++".<P>"
>             tabl = "\nThe following table displays the contents of this relation."++
>                    htmlBlueTable "darkred" [a,b] cs++"\n\n"
>             viol = chain "\n\n"
>                      [if null mp then "" 
>                       else "\n<P>\nThe following "++
>                            (if length mp>1 then show (length mp)++" instances do " else "instance does ") ++
>                            "not satisfy the rule that "++
>                            sent p++htmlBlueTable "red" [a,b] mp
>                      |p<-[Uni,Tot,Sur,Inj], p `elem` multiplicities s, mp<-[multViolations s p]]
>             sent Uni = "each "++a++" in the left hand column may not have more than one "++b++" on the right. (The relation "++htmlItalic (name s)++" is univalent.)"
>             sent Tot = "there is a missing "++b++" for each of the following "++plural English a++". (The relation "++htmlItalic (name s)++" is total.)"
>             sent Sur = "there is a missing "++a++" for each of the following "++plural English b++". (The relation "++htmlItalic (name s)++" is surjective.)"
>             sent Inj = "each "++b++" in the right hand column may not have more than one "++a++" to its left. (The relation "++htmlItalic (name s)++" is injective.)"
>  noun Uni  = "Univalence"
>  noun Tot  = "Totality"
>  noun Sur  = "Surjectivity"
>  noun Inj  = "Injectivity"
>
>  loadcode = "<BODY onload=\"MTMStartMenu()\" bgcolor=#FFFFBB link=#AA0000 alink=#AA0000 hlink=#AA0000></BODY>"
>  introtext
>      = "Use the classification of concepts (on your left) to browse."
>  leader
>      = "<SCRIPT type=\"text/javascript\">\n"++
>        "  if((navigator.appName == \"Netscape\" && parseInt(navigator.appVersion) >= 3 && navigator.userAgent.indexOf(\"Opera\") == -1 && navigator.userAgent.indexOf(\"WebTV\") == -1) || (navigator.appName == \"Microsoft Internet Explorer\" && parseInt(navigator.appVersion) >= 4)) {\n"++
>        "    for(i = 0; i < parent.frames.length; i++) {\n"++
>        "      if(parent.frames[i].name == \"code\" && parent.frames[i].MTMLoaded) {\n"++
>        "        parent.frames[i].MTMTrack = true;\n"++
>        "        setTimeout(\"parent.frames[\" + i + \"].MTMDisplayMenu()\", 250);\n"++
>        "        break;\n"++
>        "      }\n"++
>        "    }\n"++
>        "  }\n"++
>        "</SCRIPT>"
>  indexcode filename
>      = chain "\n"
>        [ "<HTML>"
>        , htmlHead ("Concepts") ""
>        , "<SCRIPT TYPE=\"text/javascript\">"
>        , "var MTMUsableBrowser = false;"
>        , "// browser sniffing routine"
>        , "browserName = navigator.appName;"
>        , "browserVersion = parseInt(navigator.appVersion);"
>        , "if(browserName == \"Netscape\" && browserVersion >= 3) {"
>        , "  MTMUsableBrowser = (navigator.userAgent.indexOf(\"Opera\") == -1) ? true : false;"
>        , "} else if(browserName == \"Microsoft Internet Explorer\" && browserVersion >= 4) {"
>        , "  MTMUsableBrowser = true;"
>        , "};"
>        , "if(!MTMUsableBrowser) alert('This page was not designed for your browser. Please use Netscape or Internet Explorer if you get inexplicable behaviour');"
>        , "document.write('<FRAMESET Cols=\"1,210,*\" border=0 frameborder=\"no\" framespacing=\"0\">');"
>        , "document.write('  <FRAME Name=\"code\" SRC=\"CTX_"++filename++"\" NORESIZE FRAMEBORDER=\"No\">');"
>        , "document.write('  <FRAMESET Rows=\"150,*\" border=0 frameborder=\"no\" framespacing=\"0\">');"
>        , "document.write('    <FRAME Name=\"context\" SRC=\"treemenutils/conceptsmenu_empty.html\" NORESIZE FRAMEBORDER=\"No\">');"
>        , "document.write('    <FRAME Name=\"menu\" SRC=\"NAV_"++filename++"\" NORESIZE FRAMEBORDER=\"No\">');"
>        , "document.write('  </FRAMESET>');"
>        , "document.write('  <FRAME Name=\"concepts\" SRC=\"Concept.html\" NORESIZE FRAMEBORDER=\"No\">');"
>        , "document.write('</FRAMESET>');"
>        , "</SCRIPT>"
>        , htmlBody "Your browser does not support frames."
>        , "</HTML>"]

  instance Identified Rule where
   name r = "Rule "++show (nr r)++" from pattern \""++patternName r++"\""


>  imageMap fnm
>      = htmlImageMap (fnm++".gif") fnm ((unsafePerformIO . readFile) (fnm++".map"))
>-- Pre: 
>  nextRule context r = if null rs then error("Fatal: no first rule in this context") else head rs
>    where rs = dropWhile (\rule->nr r>=nr rule) (rules context)++rules context
>  prevRule context r = if null rs then error("Fatal: no last rule in this context") else head rs
>    where rs = (dropWhile (\rule->nr r<=nr rule).reverse) (rules context)++reverse (rules context)
>  htmlRule :: Context -> Rule -> Bool -> String
>  htmlRule context@(Ctx nm on isa world dc ms cs ks) r predLogic
>   = htmlPage ("Code (5) for "++"Rule "++show (nr r)) ""
>                   (htmlBody ((if emptyGlossary context (Pat ("Rule "++show (nr r)) [r] [] [] [] [])
>                               then "" else "<A HREF=#REF2Glossary>Glossary</A> ")++
>                           {- "<A HREF=#REF2Relations>Relations</A>\n"++ -}
>                              (if length (rules context) <=1 then "" else
>                               "<A HREF=\""++fnRule context (nextRule context r)++".html\">Next rule</A>\n"++
>                               "<A HREF=\""++fnRule context (prevRule context r)++".html\">Previous rule</A>\n")++
>                              htmlHeadinglevel 3 ("Rule "++show (nr r)) []++"\n<P>\n"++
>                              (if null (explain r)
>                               then "Artificial explanation:<BR />\n<BLOCKQUOTE>"++(lang English .assemble.normRule) r++"</BLOCKQUOTE>\n<P>\n"
>                               else "Explanation:<BR />\n<BLOCKQUOTE>"++explain r++"</BLOCKQUOTE>\n<P>\n")++
>                              "ADL representation<BR />\n<BLOCKQUOTE>"++showADL (uncomp r)++"</BLOCKQUOTE>\n<P>\n"++
>                              (if predLogic
>                               then (if null (explain r) then "" else
>                                     "Artificial explanation:<BR />\n<BLOCKQUOTE>"    ++(lang English .assemble.normRule) r++"</BLOCKQUOTE>\n<P>\n"
>                                    )++
>--useful for diagnosis:             "Internal logic representation<BR />\n<BLOCKQUOTE>"++(show.assemble.normRule) r++"</BLOCKQUOTE>\n<P>\n"++
>                                    "Predicate logic representation<BR />\n<BLOCKQUOTE>"++(hshow.assemble.normRule) r++"</BLOCKQUOTE>\n<P>\n"++
>                                    "Object oriented representation<BR />\n<BLOCKQUOTE>"++(objOrShow.assemble.normRule) r++"</BLOCKQUOTE>\n<P>\n"
>                               else "") ++
>                              (if testing then "Test\n<P>\n"++showHS r++"\n<P>\n"++show r++"\n<P>\n" else "") ++
>                              imageMap (fnRule context r)++"\n<P>\n"++
>                              (if null ruleviol then "" else
>                               htmlHeadinglevel 2 ("Rule "++show (nr r)++" is not satisfied in the following cases") []++"\n"++ruleviol++"\n<P>\n"
>{-                             ++(if length (violations r)>1 then "Analysis of the first instance:<BR />\n" else "")++
>                               (if null antcStrands
>                                then (if length consStrands ==1
>                                      then htmlItalic "Since "++commaEng (htmlItalic "and") [x,y]++htmlItalic " are linked through  "++showADL cons++htmlItalic ", "
>                                      else commaEng (htmlItalic "and") [x,y]++htmlItalic " are linked through "++showADL antc++htmlItalic " as follows."++"<BR />\n"++htmlTable antcStrands []++"\n"
>                                     )++htmlItalic ("Rule "++show (nr r)++" prescribes that ")++commaEng (htmlItalic "and") [x,y]++htmlItalic " must be linked through "++showADL antc++". \n"++htmlItalic "However, this is not the case.\n"++
>                                     (if nullN antcNostrds then "" else
>                                      htmlItalic "Here is how far we got with the available population."++"<BR />"++
>                                      htmlTable antcNostrds [])
>                                else (if length antcStrands ==1
>                                      then htmlItalic "Since "++commaEng (htmlItalic "and") [x,y]++htmlItalic " are linked through "++showADL antc++htmlItalic ", "
>                                      else commaEng (htmlItalic "and") [x,y]++htmlItalic " are linked through "++showADL cons++htmlItalic " as follows."++"<BR />\n"++htmlTable antcStrands []++"\n"
>                                     )++htmlItalic ("Rule "++show (nr r)++" prescribes that ")++commaEng (htmlItalic "and") [x,y]++htmlItalic " must be linked through "++showADL cons++". \n"++htmlItalic "However, this is not the case.\n"++
>                                     (if nullN consNostrds then "" else
>                                      htmlItalic "Here is how far we got with the available population."++"<BR />"++
>                                      htmlTable consNostrds [])
>                               )++"\n"
>-}
>                              ){- ++
>                              htmlHeadinglevel 2 "Relations" []++"\n"++
>                              (if nr r==2 then error (showHS (declarations r)) else "")++
>                              htmlDeclarations context (declarations r)++"\n<P>\n"++
>                              if emptyGlossary context (Pat ("Rule "++show (nr r)) [r] [] [] [] []) then ""
>                              else htmlGlossary context (Pat ("Rule "++show (nr r)) [r] [] [] [] []) -}
>                   ))
>   where ruleviol    = htmlViolations r
>         [x,y]       = if null vs then error ("No violations in "++show (violations r)++".") else
>                       head vs where vs = violations r
>         nullN (x:xs)= length [e| e<-x, not (null e)]>1
>         nullN []    = False
>         gE          = genE r
>  htmlViolations :: Rule -> String
>  htmlViolations r
>   = if null vs then "" else -- No violations detected within this rule.\n
>     htmlBlueTable "red" [show (source r),show (target r)] vs++"\n\n"
>     where vs = violations r
>  violations :: Rule -> [Paire]
>  violations r | isSignal r = []
>               | ruleType r=='A' = contents (Cp (consequent r))
>               | otherwise       = [l| l<-contents (antecedent r), not (l `elem` contents (consequent r))]++
>                                   if ruleType r=='E'
>                                   then [l| l<-contents (consequent r), not (l `elem` contents (antecedent r))]
>                                   else []

>  htmlDeclarations :: Context -> [Declaration] -> String
>  htmlDeclarations context ss 
>   = htmlTable ((["","",""]++map show pTitle):[row s| s<-ss]) "BORDER=1 CELLSPACING=0"
>     where
>      row s
>       = [ if null (contents s) then name s else
>           htmlAnchor (show(fnRelation context s++".html"))
>                      (name s) []
>         ,"::"
>         , "["++htmlAnchor (htmlname (name context++name (source s))++".html") (name (source s)) []++
>           "*"++htmlAnchor (htmlname (name context++name (target s))++".html") (name (target s)) []++
>           "]"
>         ] ++
>         [if p `elem` multiplicities s
>          then (if null (multViolations s p) then "R"
>                else htmlAnchor (show(fnRelation context s++".html#REF2"++show len++" violation"++(if len==1 then "" else "s")++" of "++noun p))
>                                (show len) [])
>          else "-"
>         | p<-[Inj,Sur,Uni,Tot], len<-[length (multViolations s p)]] ++
>         [if p `elem` multiplicities s then "R" else "-"| source s==target s, p<-[Rfx,Trn,Sym,Asy]]
>      pTitle = [Inj,Sur,Uni,Tot]++if or [source s==target s| s<-ss] then [Rfx,Trn,Sym,Asy] else []

>  multViolations :: Declaration -> Prop -> Pairs
>  multViolations s Uni = [rec| cl<-eqCl src (contents s), length cl>1, rec<-cl]
>  multViolations s Inj = [rec| cl<-eqCl trg (contents s), length cl>1, rec<-cl]
>  multViolations s Tot = [[e,""]| e<-rd (conts (source s))>-dom s]
>  multViolations s Sur = [["",e]| e<-rd (conts (target s))>-cod s]

>  htmlPattern :: Context -> String -> Pattern -> String
>  htmlPattern context fnm pat@(Pat nm rs parChds pms cs ks)
>   = htmlPage ("Code (6) for "++nm) ""
>                   (htmlBody (htmlHeadinglevel 1 ("Pattern: "++nm) []++"\n"++
>                              htmlValNumbered [(nr r,explainverder context r) | r<-rules pat]++"\n"++show ((signals pat))++"\n"++
>                              htmlValNumbered [(nr r,explainverder context r) | r<-signals pat]++"\n"++
>                              imageMap fnm ++
>                              htmlHeadinglevel 1 ("Data model: "++nm) []++"\n"++
>                              imageMap (fnPattern context pat++"_CD")++
>                              htmlSignals context pat++
>                              if emptyGlossary context pat then "" else "\n"++htmlGlossary context pat))
>  htmlContext :: String -> Context -> String
>  htmlContext fnm context
>   = htmlPage ("Code (7) for "++name context) ("<SCRIPT type=\"text/javascript\">\nparent.menu.document.location.href='NAV_"++fnm++".html'\n</SCRIPT>")
>                   (--"Test: show (rules context) = "++show (rules context)++"\n"++
>                    "explaination(s) = \n>>   "++ chain "\n>>   " [explainverder context r | r<-rules context]++"\n"++
>                    htmlBody (htmlValNumbered [(nr r,explainverder context r) | r<-rules context]
>                             ++ (if length (patterns context)<=1 then "" else
>                                 "\n<BR>\n"++(imageMap (fnContext context++"_CD"))
>                              )  )
>                    )

TODO: implement signals in the atlas, but make sure it performs (the excommented code does not perform...)

>  htmlSignals context@(Ctx cnm on isa world dc ms cds ks) pat
>   = ""
>  {- (if null nss then "" else  "\n"++htmlHeadinglevel 2 "Signals" []++
>      "\n(At most three events are shown"++(if length nss==1 then "" else " per signal")++".)")++
>     chain"\n<P>" [ "\n<BR>\n"++showADL s++"\n"++htmlBlueTable "red" [show (source s),show (target s)] es
>                  | (s,es)<-nss]++
>     if null ess then "" else
>     if length ess==1 then "The signal "++commaAnd [name (signal s) | (s,es)<-ess]++" contains no events." else
>     "The signals "++commaAnd [name (signal s) | (s,es)<-ess]++" contain no events."
>     where ss = [(s, (take 3.contents.Cp .consequent) s) | s<-signals pat]
>           ess = [s| s@(_,es)<-ss, null es]
>           nss = [s| s@(_,es)<-ss, not (null es)]
>  -}

>  emptyGlossary (Ctx cnm on isa world dc ms cds ks) pat
>   = null [[c,cdef]| Cd _ c cdef _<-cds, C c (==) [] `elem` concs pat]
>  htmlGlossary context@(Ctx cnm on isa world dc ms cds ks) pat
>   = htmlHeadinglevel 2 "Glossary" []++
>     htmlTable [[c,cdef]| Cd _ c cdef _<-cds, C c (==) [] `elem` concs pat] ""

The following function makes a HTML page for one particular concept c, interpreted in the context of world.
This page is mounted in the contents frame of the architecture page, to which the navigator (left hand side of 
the screen) points.

 Pre: c<-concs (declarations context)

>  viewpoint :: Context -> Concept -> Pattern
>  viewpoint context c
>   = Pat (name c)
>         rulesV
>         (clearG [G g s| Isa ts ss<-[isa context], (g,s)<-ts, g `elem` concsV, s `elem` concsV])
>         (declarations rulesV)
>         [c| c@(Cd pos nm def ref)<-conceptdefs context, C nm (==) [] `elem` concsV]
>         []
>     where rulesV = [r| r<-rules context, c `elem` concs r]
>           concsV = concs rulesV

>  inhViewpoint :: Classification Context -> Concept -> Concept -> Pattern
>  inhViewpoint (Cl context world) specific gen
>   = Pat ("Concept "++name specific++" inherited from "++name gen)
>         (rs++[s| s<-sc, Isa ts ss<-[isa s], and[b `elem` concs rs| (a,b)<-ts]])
>         (clearG [G g s| Isa ts ss<-[isa context], (g,s)<-ts, g==gen, s `elem` concs rs])
>         (declarations rs)
>         [c| c@(Cd pos nm def ref)<-conceptdefs context, C nm (==) [] `elem` rd [c|r<-rs, c<-concs r]]
>         []
>     where
>       rs     = rd [sr | r<-rules world, gen `elem` concs r, Gc _ m expr _ _ _ _<-sc
>                       , sr<-[subst (Tm m,expr) r], specific `elem` concs sr]
>       sc     = [s| s<-specs context, gen `elem` concs s]
>--     rulesG = rd [ subsC s r | r<-rules world, gen `elem` concs r, s<-substns r]
>--     substns r
>--      = (foldr cp [[]].map (map single).eqCl fst.clear)
>--          [(g,s)| c<-concs r, Isa ts ss<-[isa context], (g,s)<-ts, c==g]
>--        where cp as bs = [a++b|a<-as,b<-bs] ; single x = [x]

>  htmlViewpoint :: Context -> String -> Pattern -> Concept -> Classification Concept -> Bool -> String
>  htmlViewpoint context@(Ctx cnm on isa world dc ms cs ks) fnm pat c (Cl r cls) predLogic
>   = (htmlPage (name c) "" . htmlBody)
>     (vptTitle (name c)                                                                            ++

Traceability (removed for now)
      "Properties of concept "++name c++" in context "++thisCtx                                     ++
      (if c==r then "" else " emerging from properties of "++name r)                                ++
      (if null cls then "" else
        "\n<BR />inheriting properties from "++
        commaAnd [htmlAnchor (thisCtx++name c++name g++".html") (name g) []|Cl g cls'<-cls]++".")   ++

>     concat ["\n<P>\n"++htmlBold "Definition"++"\n<BR />\n"++cdef| Cd _ c' cdef _<-cs, c'==name c] ++
>      (if null atoms then "" else if length atoms==1
>       then "\n<P>\n"++htmlBold ("This concept contains one atom: "++show (head atoms))
>       else "\n<P>\n"++htmlBold ("This concept contains "++show (length atoms)++" atoms.")++"<BR />\n"++
>            htmlTable [[a]| a<-atoms] [])  ++
>      (if null (rules pat) then "" else
>       "\n<FONT color=\"#AA0000\"><HR color=#AA0000></FONT>\n"++tests                               ++
>       htmlHeadinglevel 2 ("Rules applicable to "++show c) []++"\n"++
>       htmlValNumbered (map (hgenR context predLogic) (rules pat))++(imageMap fnm))
>      )

>     where
>       C nm gE atoms = c
>       thisCtx = fnContext context
>       gen = name r
>--       inheriting =  [G g s|G g s<-parChds, s==c]
>       tests
>        = if testing
>          then "Testing:"++
>               "<BR />cs = "++show (concs pat)++
>               "<BR />rs = "++show (rules pat)
>          else ""

>  explainverder :: Context -> Rule -> String
>  explainverder thisCtx r = --"\nTest: explainverder\n"++
>                            explainArt thisCtx English r++" "++ htmlAnchor (fnRule thisCtx r++".html") "More..." []
>  hgenR thisCtx predLogic r
>   = (nr r, (if predLogic then (hshow.assemble.normRule) r++"\n<BR />\n" else "")++explainverder thisCtx r++"\n")

>  vptTitle c = htmlHeadinglevel 2 ("Concept: "++c) []++"\n"

Obsolete? 

  htmlConcept :: String -> Classification Context -> [Concept] -> Classification Concept -> Bool -> String -> (String,String)
  htmlConcept fnm (Cl context world) trace t@(Cl c cls) predLogic graphicstyle
   | c `elem` concG
      = (vptTitle c'++"Properties of "++name c'++" emerging from properties of "++name c++
         ruleconc [r|r<-ruleG, c' `elem` concs r] c
        ,graph (name c') ruleG)
   | c `elem` concS
      = (vptTitle c++"Properties of concept "++name c++" in context "++name context++
         (if null cls then "" else
          "<BR />inheriting properties from "++
          commaAnd [htmlAnchor (name context++(if null trace then name c else name (head trace))++name e++".html") (name e) []|e<-cls]++".")++
         ruleconc ruleS c
        ,graph (name c) ruleS)
   | otherwise = ("Empty concept page"++
                  if testing then "\n<BR />\nconcS = "++show (map name concS)++"\n<BR />\nconcG = "++show (map name concG) else ""
                 ,dotGraph context graphicstyle (name c) (I [] c c True))
   where
       ruleconc rules c
        = if null rules then "" else
          "\n<FONT color=\"#AA0000\"><HR color=#AA0000></FONT>\n"++tests++
          htmlHeadinglevel 2 ("Rules applicable to "++show c) []++"\n"++
          htmlValNumbered (map (hgenR context predLogic) rules)++htmlImage (fnm++".png")
       c'    = if null trace then c else head trace
       concS = concs context
       ruleS = [r| r<-rules context, c `elem` concs r]
       concG = concs world
--     ruleG = rd [ subsC s r | r<-rules world, c `elem` concs r, s<-substns r]
       ruleG = rd [ foldr (.) id [subst (Tm m,expr)| Gc _ m expr _ _ _ _<-sc] r | r<-rules world] where sc=specs context
       graph nm rs = dotGraph context graphicstyle nm (Pat (name c) rs [] [] [] [])
       trs = if null trc then "concept "++name c else
             (if length trc>1 then "concepts " else "concept ")++chain "," trc
             where trc = [name c|c<-trace `isc` concG]
--     substns r = (foldr cp [[]].map (map single).eqCl fst.clear) [(g,c)| g<-concs r, c<-concS, g `gEq` c]
--                 where cp as bs = [a++b|a<-as,b<-bs] ; single x = [x]
--     subst tuples c = head ([c'| (C p _ _,C c' _ _)<-tuples, p==c]++[c])
       vptTitle c = "Concept: "++(name c)++"\n<P>\n"
       tests
        = if testing
          then "Testing:<BR />concS = "++show concS++
               "<BR />concG = "++show concG++
               "<BR />ruleS = "++show ruleS++
               "<BR />ruleG = "++show ruleG++
               "<BR />rules world = "++show (rules world)++
               "<BR />specs context = "++show (specs context)
          else ""

test:  recalc context@(Ctx nm on isa world dc ms cs ks) = Ctx (error (testC++"\n\n"++testD)) on isa world dc ms cs ks


  hsign (Sg a b) ps
        | m Uni && m Tot && m Inj && m Sur = a++" <-> "++b
        | m Uni && m Tot = a++" --> "++b
        | m Sur && m Inj = a++" <-- "++b
        | m Uni          = a++" |-> "++b
        | m Inj          = a++" <-| "++b 
        | otherwise      = "("++a++","++b++")"
        where m e = e `elem` ps

>  hmenu' :: Context -> Classification Concept -> String
>  hmenu' context (Cl r cls)
>    = chain "\n" (["\n//menu menu\n\nvar menu = null;\nmenu = new MTMenu();"] ++
>          [menuline "menu" (fnConcept context (root cl)++".html") cl| cl<-cls] ++
>          concat [recur "menu" (sh r) (show i) (Cl r cls) [r]| (i, Cl r cls)<-zip [0..] cls, not (null cls)])
>      where
>       recur nm rootname cNr (Cl r cls) trace
>        = ["\n//"++rootname++" menu\n\nvar "++varname++" = null;\n"++varname++" = new MTMenu();"] ++
>          [menuline varname (fnm (root cl)) cl| cl<-cls] ++
>          [nm++".items["++cNr++"].MTMakeSubmenu("++varname++");"] ++
>          concat [recur varname (sh r) (show i) (Cl r cls) (trace++[r])| (i, Cl r cls)<-zip [0..] cls, not (null cls)]
>          where fnm r = htmlname (name context++name r++if null trace then "" else name (head trace))++".html"
>                varname = avoidJSreservedwords rootname
>       menuline rn fnm (Cl r cls)
>        = rn++".MTMAddItem(new MTMenuItem(\""++htmlNm r++"\", \""++fnm++"\", \"concepts\"));"
>       sh root = [if isUpper c then chr(ord c - ord 'A' + ord 'a') else c| c<-mumble (name root)]

>  hmenu :: Classification Context -> String
>  hmenu (Cl r cls)
>    = chain "\n" (["\n//menu menu\n\nvar menu = null;\nmenu = new MTMenu();"] ++
>                  [menuline "menu" (htmlNm r)]   ++
>                  recur "menu" (sh r) "0" (Cl r cls) [])
>      where
>       recur nm rootname cNr (Cl r cls) trace
>        = ["\n//"++rootname++" menu\n\nvar "++varname++" = null;\n"++varname++" = new MTMenu();"] ++
>          [menuline varname (htmlNm r)| (Cl r cls')<-cls] ++
>          [nm++".items["++cNr++"].MTMakeSubmenu("++varname++");"] ++
>          concat [recur varname (sh r) (show i) (Cl r cls) (trace++[r])| (i, Cl r cls)<-zip [0..] cls, not (null cls)]
>          where varname = avoidJSreservedwords rootname
>       menuline rn fnm
>        = rn++".MTMAddItem(new MTMenuItem(\""++fnm++"\", \""++fnm++".html"++"\", \"concepts\"));"
>       sh root = [if isUpper c then chr(ord c - ord 'A' + ord 'a') else c| c<-mumble (name root)]

>  wrapCode ctxName lines
>   = chain "\n"
>     ([ "<SCRIPT type=\"text/javascript\" src=\"treemenutils/mtmcode.js\"></SCRIPT>"
>      , "<SCRIPT type=\"text/javascript\">"
>      , "// Morten's JavaScript Tree Menu"
>      , "// written by Morten Wang <morten@treemenu.com> (c) 1998-2000"
>      , "// This is version 2.2.6, dated 2000-03-30"
>      , "// The script is freely distributable"
>      , "// It may be used (and modified) as you wish, but retain this message"
>      , "// For more information about the menu visit its home page"
>      , "// http://www.treemenu.com/"
>      , "var MTMTableWidth = \"100%\";"
>      , "var MTMenuFrame = \"context\";"
>      , "var MTMSubsGetPlus = false;"
>      , "var MTMEmulateWE = true;"
>      , "var MTMenuImageDirectory = \"treemenutils/\";"
>      , "var MTMBGColor = \"#FFFFBB\";"
>      , "var MTMBackground = \"\";"
>      , "var MTMTextColor = \"#000000\";"
>      , "var MTMLinkColor = \"#AA0000\";"
>      , "var MTMAhoverColor = \"#000000\";"
>      , "var MTMTrackColor =\"#000000\";"
>      , "var MTMSubExpandColor = \"#666699\";"
>      , "var MTMSubClosedColor = \"#666699\";"
>      , "var MTMRootIcon = \"menu_new_root.gif\";"
>      , "var MTMenuText = \"Context tree\";"
>      , "var MTMRootColor = \"#000000\";"
>      , "var MTMRootFont = \"Arial, Helvetica, sans-serif\";"
>      , "var MTMRootCSSize = \"84%\";"
>      , "var MTMRootFontSize = \"-1\";"
>      , "var MTMenuFont = \"Arial, Helvetica, sans-serif\";"
>      , "var MTMenuCSSize = \"84%\";"
>      , "var MTMenuFontSize = \"-1\";"
>      , "var MTMLinkedSS = false;"
>      , "var MTMSSHREF = \"menu.css\";"
>      , "var MTMSubsAutoClose = false;"
>      , "var MTMTimeOut = 15;"
>      , "var MTMIconList = null;"
>      , "MTMIconList = new IconList();"
>      , "MTMIconList.addIcon(new MTMIcon(\"menu_link_external.gif\", \"http://\", \"pre\"));"
>      , "MTMIconList.addIcon(new MTMIcon(\"menu_link_pdf.gif\", \".pdf\", \"post\"));"
>      , lines
>      , "</SCRIPT>"])

>  instance HTML Expression where
>    hshow e | idsOnly e = "="
>            | isNot e   = "&neq;"
>            | otherwise = showADL e

>  instance HTML PredLogic where
>   hshow x = predLshow ("&forall;", "&exist;", implies, "<IMG SRC=\"treemenutils/arboth.gif\" ALT=\" <=> \">", "=", "&neq;", "&or;", "&and;", "&not;", rel, fun, mathVars, "<BR />\n", " ") x
>             where rel m lhs rhs = lhs++" "++hshow m++" "++rhs
>                   fun m x = name m++"("++x++")"
>                   implies antc cons = antc++" <IMG SRC=\"treemenutils/arright.gif\" ALT=\" ==> \"> "++cons

  helptext :: String -> [Morphism] -> Hshowable a -> String -> [Morphism] -> String

>  helptext a ams e b bms | idsOnly e = funnotate a ams ++ " "++hshow e++" " ++ funnotate b bms
>                         | otherwise = applyM s (funnotate a ams) (funnotate b bms)++shCard s
>   where
>     s = declaration (head (mors e))
>     shCard s
>      | null (multiplicities s) = ""
>      | otherwise               = "\n(Properties: "++(chain ", " . map show ) (multiplicities s)++")"

>  funnotate :: String -> [Morphism] -> String
>  funnotate x   []   = x
>  funnotate x (m:ms) = if isIdent m then funnotate x ms else funnotate (hshow m++"("++x++")") ms

>  charVars q vs
>   = if null vs then "" else
>     q++" "++chain "; " [chain ", " (map fst vs')++"::"++name (snd (head vs')) | vs'<-eqCl snd vs]++": "

>  htmlNm :: Identified a => a->String
>  htmlNm = htmlname.name

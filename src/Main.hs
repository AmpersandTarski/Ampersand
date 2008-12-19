  module Main where
   import System (getArgs,system, ExitCode(ExitSuccess,ExitFailure))
   import Char (toLower)
   import UU_Scanner(scan,initPos)
   import UU_Parsing(parseIO)
   import CommonClasses ( Identified(name))
   import Auxiliaries (chain, commaEng, adlVersion)
   import Typology (Typology(Typ), typology, makeTrees)
   import ADLdef
   import ShowADL
   import CC_aux (showHS, showHSname)
   import Languages( Lang(English,Dutch))
   import AGtry (sem_Architecture)
   import CC (pArchitecture, keywordstxt, keywordsops, specialchars, opchars)
   import Calc (deriveProofs,triggers)
   import Views (viewEstimate)
   import Fspec (projectClassic
                ,generateFspecLaTeX
                ,generateArchLaTeX
                ,generateGlossaryLaTeX
                ,funcSpec
              --  ,nDesignPr
                ,nServices
                ,nFpoints
                ,makeFspec
                )
   import HtmlFilenames(fnContext,fnPattern)
   import Graphic(processCdDataModelFile,dotGraph,processDotgraphFile)
   import Atlas (anal)
   import Xml (makeXML)
   import ERmodel (erAnalysis)
   import ClassDiagram (cdModel,cdDataModel)  
   import RelBinGen(phpServices)
   import ObjBinGen(phpObjServices)
   import MakeFspec (makeFspecNew2,Fspc)



   latexOpt :: [String] -> Bool
   latexOpt sws = "-l" `elem` sws
   splitStr :: (String -> Bool) -> [String] -> ([String], [String])
   splitStr f (x:xs) | f x  = (x:yes, no)
                     | True = (yes, x:no)
                     where (yes,no) = splitStr f xs
   splitStr _ [] = ([],[])

   main :: IO ()
   main
    = do { a <- getArgs
         ; putStr (chain ", " a++"\n")
         ; let (switches,args') = splitStr ((=="-").take 1) a
         ; let (dbArgs,args) = splitStr ((=="+").take 1) args'
         ; putStr (adlVersion++"\nArguments: "++chain ", " args++"\nSwitches: "++chain ", " switches++"\nDatabase: "++chain ", " (map tail dbArgs))
         ; if "-checker" `elem` switches && "-beeper" `elem` switches then putStr ("incompatible switches: -checker and -beeper") else
           if length args==0 then putStr ("Please provide a filename (.adl) and a context name") else
           if length dbArgs>1 then putStr (". This is confusing! please specify 1 database name only.") else
      do { let fn = args!!0; contextname = args!!1
               dbName | null dbArgs = fnOutp
                      | otherwise   = tail (head dbArgs)
               ( _ ,fnSuffix) = (take (length fn-4) fn, drop (length fn-4) fn)
               fnFull = if map Char.toLower fnSuffix /= ".adl" then (fn ++ ".adl") else fn
               fnOutp = take (length fnFull-4) fnFull
         ; inp<-readFile fnFull
         ; putStr ("\n"++fnFull++" is read.")
         ; slRes <- parseIO (pArchitecture ("-beeper" `elem` switches))(scan keywordstxt keywordsops specialchars opchars fnFull initPos inp)
         ; putStr ("\n"++fnFull++" has been parsed.")
         ; let (contexts,errs) = sem_Architecture slRes
         ; let Typ pths = if null contexts then Typ [] else
                          if length args>1 && contextname `elem` map name contexts
                          then typology (isa (head [c| c<-contexts,name c==contextname]))
                          else typology (isa (head contexts))
         ; putStr "\nConcepts:\n" >>(putStr.chain "\n".map show) (makeTrees (Typ (map reverse pths)))
         ; if null errs 
           then (putStr ("\nNo type errors or cyclic specializations were found.\n")>>
                 if length args==1 && length contexts==1
                 then build contexts switches (name (head contexts)) fnOutp dbName slRes else
                 if length args==1 && length contexts>1
                 then putStr ("\nPlease specify the name of a context."++
                              "\nAvailable contexts: "++commaEng "and" (map name contexts)++".\n") else
                 if length args>1 && contextname `elem` map name contexts
                 then build contexts switches contextname fnOutp dbName slRes
                 else putStr ("\nContext "++contextname++" not defined."++
                              "\nPlease specify the name of an available context."++
                              "\nAvailable contexts: "++commaEng "and" (map name contexts)++"."++
                              "\n(Note: context names are case sensitive).\n")
                )
           else putStr ("\nThe type analysis of "++fnFull++" yields errors.\n")>>
                putStr (concat ["!Error of type "++err| err<-errs])>>
                putStr ("Nothing generated, please correct mistake(s) first.\n")
         }}
       where build :: [Context] -> [String] -> String -> String -> String -> whatever -> IO ()
             build contexts switches contextname filename dbName hierGebeurtNietsMee
              = sequence_ 
                 ([ anal contexts contextname ("-p" `elem` switches) (lineStyle switches)
                  | null switches || "-h" `elem` switches]++
                  [ makeXML contexts contextname| "-XML" `elem` switches]++
                  [ showHaskell_new fspec | "-Haskell" `elem` switches]++ 
                  [ diagnose contexts contextname| "-diag" `elem` switches]++
                  [ functionalSpecLaTeX contexts contextname (lineStyle switches) (lang switches) filename| "-fSpec" `elem` switches]++
                  [ viewEstimates contexts contextname (lineStyle switches) (lang switches) filename| "-views" `elem` switches]++
                  [ archText contexts contextname (lineStyle switches) (lang switches) filename| "-arch" `elem` switches]++
                  [ glossary contexts contextname (lang switches) | "-g" `elem` switches]++
   -- out of order[ erModel contexts contextname | "-ER" `elem` switches]++
                  [ cdModel contexts contextname | "-CD" `elem` switches]++
                  [ phpObjServices contexts contextname filename dbName ("./"++filename++"/") | "-phpcode" `elem` switches]++
                  [ phpServices contexts contextname filename dbName True True | "-beeper" `elem` switches]++
                  [ phpServices contexts contextname filename dbName ("-notrans" `elem` switches) False| "-checker" `elem` switches]++
                  [ deriveProofs contexts contextname ("-m" `elem` switches)| "-proofs" `elem` switches]
 --               ++[ projectSpecText contexts contextname (lang switches) | "-project" `elem` switches]
 --               ++[ csvcontent contexts contextname | "-csv" `elem` switches]
 --               ++[ putStr (show slRes) | "-dump" `elem` switches ]
                 ) >>
                    putStr ("\nwriting to \\ADL.log:\nADL "++filename++" "++chain " " switches++"\n") >>
                    putStr ("  nr. of classes:                    "++show (length datasets)++"\n") >>
                    putStr ("  nr. of concepts:                   "++show (length (concs context))++"\n") >>
                    putStr ("  nr. of relations:                  "++show (length rels)++"\n") >>
                    putStr ("  nr. of invariants:                 "++show (length ruls)++"\n") >>
                    putStr ("  nr. of multiplicity rules:         "++show (length (multRules context))++"\n") >>
                    putStr ("  nr. of action rules generated:     "++show (length [ hc | rule<-declaredRules context, hc<-triggers rule])++"\n") >>
                    putStr ("  nr. of patterns:                   "++show (length (patterns context))++"\n") >>
                    putStr ("  nr. of services:                   "++show (nServices spec)++"\n") >>
                    putStr ("  nr. of function points:            "++show (nFpoints spec)++"\n") >>
                    appendFile "\\ADL.log" ("ADL "++filename++" "++chain " " switches++"\n") >>
                    appendFile "\\ADL.log" ("  nr. of classes:                    "++show (length datasets)++"\n") >>
                    appendFile "\\ADL.log" ("  nr. of concepts:                   "++show (length (concs context))++"\n") >>
                    appendFile "\\ADL.log" ("  nr. of relations:                  "++show (length rels)++"\n") >>
                    appendFile "\\ADL.log" ("  nr. of invariants:                 "++show (length ruls)++"\n") >>
                    appendFile "\\ADL.log" ("  nr. of multiplicity rules:         "++show (length (multRules context))++"\n") >>
                    appendFile "\\ADL.log" ("  nr. of action rules generated:     "++show (length [ hc | rule<-declaredRules context, hc<-triggers rule])++"\n") >>
                    appendFile "\\ADL.log" ("  nr. of patterns:                   "++show (length (patterns context))++"\n") >>
                    appendFile "\\ADL.log" ("  nr. of services:                   "++show (nServices spec)++"\n") >>
                    appendFile "\\ADL.log" ("  nr. of function points:            "++show (nFpoints spec)++"\n")
                   where
                      context = if null ctxs then error ("!Mistake: "++contextname++" not encountered in input file.\n") else head ctxs
                      ctxs    = [c| c<-contexts, name c==contextname]
                      (datasets,viewEsts,rels,ruls) = erAnalysis context
                      spec = funcSpec context (datasets,viewEsts,rels,ruls) (lang switches)
                      fspec = makeFspecNew2 context
             lineStyle switches
              | "-crowfoot" `elem` switches = "crowfoot"
              | otherwise                   = "cc"
             lang switches
              | "-NL" `elem` switches = Dutch
              | "-UK" `elem` switches = English
              | otherwise             = Dutch

   diagnose contexts contextname
    = putStr (showHS "\n>  " context)
      where
       context = if null ctxs then error ("!Mistake: "++contextname++" not encountered in input file.\n") else head ctxs
       ctxs    = [c| c<-contexts, name c==contextname]

   projectSpecText contexts contextname language
    = putStrLn ("\nGenerating project plan for "++name context)                >>
      writeFile (name context++".csv") (projectClassic context spec language)  >>
      putStr ("\nMicrosoft Project file "++name context++".csv written... ")
      where
       context = if null ctxs then error ("!Mistake: "++contextname++" not encountered in input file.\n") else head ctxs
       ctxs    = [c| c<-contexts, name c==contextname]
       spec = funcSpec context (erAnalysis context) language
       (datasets,viewEsts, relations, ruls) = erAnalysis context

   showHaskell_old :: [Context] -> String -> IO ()
   showHaskell_old contexts contextname
    = putStrLn ("\nGenerating Haskell source code for "++name context) >>
      writeFile (ctxNm++"_old.lhs")
                ("> module Main where\n>  import UU_Scanner\n>  import Classification\n>  import Typology\n>  import ADLdef\n>  import CC_aux (showHS)\n>  import Fspec\n\n"
                 ++">  main = putStr (showHS \"\\n>  \""++ctxNm++")"++"\n\n"
                 ++">  "++showHSname context++"\n>   = "++showHS "\n>     " context++"\n\n"
                 ++">  "++showHSname fspec++"\n>   = "++showHS "\n>     " fspec
                ) >>
      putStr ("\nHaskell file "++ctxNm++"_old.lhs written...\n")
      where
       fspec = makeFspec context
       ctxNm = showHSname context
       spcNm = showHSname fspec
       context = if null ctxs then error ("!Mistake: "++contextname++" not encountered in input file.\n") else head ctxs
       ctxs    = [c| c<-contexts, name c==contextname]

   showHaskell_new :: Fspc -> IO ()
   showHaskell_new fspc
    = putStrLn ("\nGenerating Haskell source code for "++name fspc) >>
      writeFile (baseName++"_new.lhs")
                ("> module Main where"
             ++"\n>  import UU_Scanner"
             ++"\n>  import Classification"
             ++"\n>  import Typology"
             ++"\n>  import ADLdef"
             ++"\n>  import CC_aux (showHS)"
             ++"\n>  import FspecDef"
             ++"\n"
             ++"\n>  main = putStr (showHS \"\\n>  \" "++baseName++")"
             ++"\n\n"
                 ++">  "++baseName++"\n>   = "++showHS "\n>     " fspc
                ) >>
      putStr ("\nHaskell file "++baseName++"_new.lhs written...\n")
      where
       baseName = "f_Ctx_"++(name fspc)















   functionalSpecLaTeX contexts contextname graphicstyle language filename
    = putStr ("\nGenerating functional specification for context "++
              name context++" in the current directory.\n")                   >>
      graphics context (fnContext context) graphicstyle False context         >>   -- generate abbreviated (full==False) class diagram
      sequence_ [ graphics context (fnPattern context pat) graphicstyle True pat   -- generate fully fledge (full==True) class diagram
                | pat<-patterns context]                                      >>
      writeFile (filename++".tex") (generateFspecLaTeX context language spec) >>   -- generate LaTeX code
      putStr ("\nLaTeX file "++filename++".tex written... ")                  >>
      processLaTeX2PDF filename                                                    -- crunch the LaTeX file into PDF.
      where
       context = if null ctxs then error ("!Mistake: "++contextname++" not encountered in input file.\n") else head ctxs
       ctxs    = [c| c<-contexts, name c==contextname]
       spec = funcSpec context (datasets,viewEsts,rels,ruls) language
       (datasets,viewEsts,rels,ruls) = erAnalysis context

   viewEstimates contexts contextname graphicstyle language filename
    = putStr (chain "\n\n" (map showADL viewEsts))
      where
       context = if null ctxs then error ("!Mistake: "++contextname++" not encountered in input file.\n") else head ctxs
       ctxs    = [c| c<-contexts, name c==contextname]
       (datasets,viewEsts,rels,ruls) = erAnalysis context

   archText contexts contextname graphicstyle language filename
    = putStr ("\nGenerating architecture document for context "++
              name context++" in the current directory.\n")                   >>
      graphics context (fnContext context) graphicstyle False context         >>   -- generate abbreviated (full==False) class diagram
      writeFile (filename++".tex") (generateArchLaTeX context language spec)  >>   -- generate LaTeX code
      putStr ("\nLaTeX file "++filename++".tex written... ")                  >>
      processLaTeX2PDF filename                                                    -- crunch the LaTeX file into PDF.
      where
       context = if null ctxs then error ("!Mistake: "++contextname++" not encountered in input file.\n") else head ctxs
       ctxs    = [c| c<-contexts, name c==contextname]
       spec = funcSpec context (datasets,viewEsts,rels,ruls) language
       (datasets,viewEsts,rels,ruls) = erAnalysis context
     -- the following is copied from Atlas.lhs. TODO: remove double code.

   glossary contexts contextname language
    = putStr ("\nGenerating Glossary for "++name context++" in the current directory.") >>
      writeFile ("gloss"++name context++".tex") (generateGlossaryLaTeX context language)           >>
      putStr ("\nLaTeX file "++"gloss"++name context++".tex written... ") >>
      processLaTeX2PDF ("gloss"++name context)
      where
       context = if null ctxs then error ("!Mistake: "++contextname++" not encountered in input file.\n") else head ctxs
       ctxs    = [c| c<-contexts, name c==contextname]

   graphics context fnm graphicstyle full b
    = writeFile (fnm++"_CD.dot") (cdDataModel context full "dot" b)  >>
      putStrLn ("Class diagram "++fnm++"_CD.dot written... ")        >>
      processCdDataModelFile  (fnm ++"_CD")                          >>
      writeFile (fnm++".dot") (dotGraph context graphicstyle fnm b)  >>
      putStrLn ("Graphics specification "++fnm++".dot written... ")  >>
      processDotgraphFile  fnm

   processLaTeX2PDF fnm =
      do putStr ("\nProcessing "++fnm++".tex ... :")
         result <- system ("pdflatex -interaction=batchmode "++fnm++".tex")
         case result of
             ExitSuccess   -> putStrLn ("  "++fnm++".pdf created.")
             ExitFailure x -> putStrLn $ "Failure: " ++ show x
         putStr ("\nReprocessing "++fnm++".tex ... :")
         result <- system ("pdflatex -interaction=nonstopmode "++fnm++".tex")
         case result of
             ExitSuccess   -> putStrLn ("  "++fnm++".pdf created.")
             ExitFailure x -> putStrLn $ "Failure: " ++ show x

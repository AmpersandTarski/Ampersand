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
   import FspecDEPRECIATED (projectClassic
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
   import Fspec2Xml (makeXML_depreciated)
   import ERmodel (erAnalysis)
   import ClassDiagram (cdModel,cdDataModel)  
   import RelBinGen(phpServices)
   import ObjBinGen(phpObjServices)
   import ADL2Fspec (makeFspecNew2,Fspc)



   latexOpt :: [String] -> Bool
   latexOpt sws = "-l" `elem` sws
   splitStr :: (String -> Bool) -> [String] -> ([String], [String])
   splitStr f (x:xs) | f x  = (x:yes, no)
                     | True = (yes, x:no)
                     where (yes,no) = splitStr f xs
   splitStr _ [] = ([],[])

-- | TODO (SJ) Het volgende is slordige code. Dat moet nog eens netjes worden neergezet, zodat we ook eenvoudig executables kunnen afleiden met een gedeelte van alle functionaliteit.

   main :: IO ()
   main
    = do { -- First, parse the options from the command line:
           a <- getArgs
         ; putStr (chain ", " a++"\n")
         ; let (switches,args') = splitStr ((=="-").take 1) a
         ; let (dbArgs,args) = splitStr ((=="+").take 1) args'
         ; putStr (adlVersion++"\nArguments: "++chain ", " args++"\nSwitches: "++chain ", " switches++"\nDatabase: "++chain ", " (map tail dbArgs))
           
           -- Next, do some checking of commandline options:
         ; if "-checker" `elem` switches && "-beeper" `elem` switches then putStr ("incompatible switches: -checker and -beeper") else
           if length args==0 then putStr ("Please provide a filename (.adl) and a context name\n"++helptext) else
           if length dbArgs>1 then putStr (". This is confusing! please specify 1 database name only.") else

           -- If no errors in the commandline options are found, continue with
           -- parsing of the import file.
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
         
           -- Now continue with typechecking of the parsetree:
         ; let (contexts,errs) = sem_Architecture slRes
         ; let Typ pths = if null contexts then Typ [] else
                          if length args>1 && contextname `elem` map name contexts
                          then typology (isa (head [c| c<-contexts,name c==contextname]))
                          else typology (isa (head contexts))
         ; putStr "\nConcepts:\n" >>(putStr.chain "\n".map show) (makeTrees (Typ (map reverse pths)))

           -- Now we have Contexts with or without errors in it. If there are no errors,
           -- AND the argument matches the context name, then the build is done for that 
           -- context
         ; if null errs 
           then (putStr ("\nNo type errors or cyclic specializations were found.\n")>>
                 if length args==1 && length contexts==1
                 then (( build_DEPRECEATED contexts switches (name (head contexts)) fnOutp dbName slRes ) >>
                       ( build_NewStyle (map makeFspecNew2 contexts) switches (name (head contexts)) fnOutp dbName slRes )) else
                 if length args==1 && length contexts>1
                 then putStr ("\nPlease specify the name of a context."++
                              "\nAvailable contexts: "++commaEng "and" (map name contexts)++".\n") else
                 if length args>1 && contextname `elem` map name contexts
                 then (( build_DEPRECEATED contexts switches contextname fnOutp dbName slRes ) >>
                       ( build_NewStyle (map makeFspecNew2 contexts) switches contextname fnOutp dbName slRes ))
                 else putStr ("\nContext "++contextname++" not defined."++
                              "\nPlease specify the name of an available context."++
                              "\nAvailable contexts: "++commaEng "and" (map name contexts)++"."++
                              "\n(Note: context names are case sensitive).\n")
                )
           else putStr ("\nThe type analysis of "++fnFull++" yields errors.\n")>>
                putStr (concat ["!Error of type "++err| err<-errs])>>
                putStr ("Nothing generated, please correct mistake(s) first.\n")
         }}
       where 
             -- TODO: De volgende build moet worden 'uitgekleed' door de verschillende 
             --       vertaalslagen via Fspec te laten verlopen. Hiervoor is een functie build_NewStyle gemaakt.
             build_DEPRECEATED :: [Context] -> [String] -> String -> String -> String -> whatever -> IO ()
             build_DEPRECEATED contexts switches contextname filename dbName hierGebeurtNietsMee
              = sequence_ 
                 ([ putStr ("Nothing generated.\n"++helptext) | null switches ] ++
                  [ anal contexts contextname ("-p" `elem` switches) (lineStyle switches)
                  | "-atlas" `elem` switches]++
                  [ makeXML_depreciated contexts contextname| "-XML" `elem` switches]++
   --               [ showHaskell_new fspec | "-Haskell" `elem` switches]++ 
                  [ diagnose contexts contextname| "-diag" `elem` switches]++
                  [ functionalSpecLaTeX contexts contextname (lineStyle switches) (lang switches) filename| "-fSpec" `elem` switches]++
                  [ viewEstimates contexts contextname (lineStyle switches) (lang switches) filename| "-serviceEsts" `elem` switches]++
                  [ archText contexts contextname (lineStyle switches) (lang switches) filename| "-arch" `elem` switches]++
                  [ glossary contexts contextname (lang switches) | "-g" `elem` switches]++
   -- out of order[ erModel contexts contextname | "-ER" `elem` switches]++
                  [ cdModel contexts contextname | "-CD" `elem` switches]++
                  [ phpObjServices contexts contextname filename dbName ("./"++filename++"/") True | "-phpcode" `elem` switches]++
                  [ phpObjServices contexts contextname filename dbName ("./"++filename++"/") False | "-serviceGen" `elem` switches]++
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
                      (datasets,generatedServices,rels,ruls) = erAnalysis context
                      spec = funcSpec context (datasets,generatedServices,rels,ruls) (lang switches)

             helptext
              = chain "\n"
                [ "The following functionalities are available:"
                , "ADL <myfile>.adl -atlas             generate an atlas, which is a website that"
                , "                                    documents your ADL-script."
                , "ADL <myfile>.adl -fSpec             generate a functional specification, which"
                , "                                    is a PDF-document called myfile.pdf."
                , "ADL <myfile>.adl -phpcode           generate a functional prototype, which is a"
                , "                                    PHP/MySQL application that resides in directory <myfile>"
                , "ADL <myfile>.adl -serviceGen        similar to -phpcode, but generates only those"
                , "                                    services specified in your ADL-script."
                , "ADL <myfile>.adl -services          generate service definitions from scratch. You may"
                , "                                    find this useful to start writing SERVICE definitions."
                , "ADL <myfile>.adl -proofs            generate correctness proofs (works partly)"
                , ""
                ]

             build_NewStyle :: [Fspc] -> [String] -> String -> String -> String -> whatever -> IO ()
             build_NewStyle fspecs switches contextname filename dbName hierGebeurtNietsMee
              = sequence_ 
                 (--[ anal contexts contextname ("-p" `elem` switches) (lineStyle switches) | null switches || "-h" `elem` switches]++
                  --[ makeXML_depreciated contexts contextname| "-XML" `elem` switches]++
                  [ showHaskell_new fspecs | "-Haskell" `elem` switches]  -- ++ 
                  --[ diagnose contexts contextname| "-diag" `elem` switches]++
                  --[ functionalSpecLaTeX contexts contextname (lineStyle switches) (lang switches) filename| "-fSpec" `elem` switches]++
                  --[ viewEstimates contexts contextname (lineStyle switches) (lang switches) filename| "-services" `elem` switches]++
                  --[ archText contexts contextname (lineStyle switches) (lang switches) filename| "-arch" `elem` switches]++
                  --[ glossary contexts contextname (lang switches) | "-g" `elem` switches]++
   -- out of order[ erModel contexts contextname | "-ER" `elem` switches]++
                  --[ cdModel contexts contextname | "-CD" `elem` switches]++
                  --[ phpObjServices contexts contextname filename dbName ("./"++filename++"/") | "-phpcode" `elem` switches]++
                  --[ phpServices contexts contextname filename dbName True True | "-beeper" `elem` switches]++
                  --[ phpServices contexts contextname filename dbName ("-notrans" `elem` switches) False| "-checker" `elem` switches]++
                  --[ deriveProofs contexts contextname ("-m" `elem` switches)| "-proofs" `elem` switches]
 --               ++[ projectSpecText contexts contextname (lang switches) | "-project" `elem` switches]
 --               ++[ csvcontent contexts contextname | "-csv" `elem` switches]
 --               ++[ putStr (show slRes) | "-dump" `elem` switches ]
                 ) 
                where 
                   fspec = [f| f<-fspecs, name f==contextname]
             lineStyle switches
              | "-crowfoot" `elem` switches = "crowfoot"
              | otherwise                   = "cc"
             lang switches
              | "-NL" `elem` switches = Dutch
              | "-UK" `elem` switches = English
              | otherwise             = Dutch

   diagnose :: Contexts -> String -> IO()
   diagnose contexts contextname
    = putStr (showHS "\n>  " context)
      where
       context = if null ctxs then error ("!Mistake: "++contextname++" not encountered in input file.\n") else head ctxs
       ctxs    = [c| c<-contexts, name c==contextname]

   projectSpecText :: Contexts -> String -> Lang -> IO()
   projectSpecText contexts contextname language
    = putStrLn ("\nGenerating project plan for "++name context)                >>
      writeFile (name context++".csv") (projectClassic context spec language)  >>
      putStr ("\nMicrosoft Project file "++name context++".csv written... ")
      where
       context = if null ctxs then error ("!Mistake: "++contextname++" not encountered in input file.\n") else head ctxs
       ctxs    = [c| c<-contexts, name c==contextname]
       spec = funcSpec context (erAnalysis context) language
       (datasets,generatedServices, relations, ruls) = erAnalysis context

   showHaskell_new :: [Fspc] -> IO()
 --  showHaskell_new [] = []
 --  showHaskell_new f:fs = do {  (showHaskell f) 
 --                             ; (showHaskell_new fs)
 --                            }
 --  showHaskell_new f:fs = 
 --       sequence_ ( [ showHaskell f ] ++
 --                   [ showHaskell_new fs]
 --                 )                         
   showHaskell_new fs = showHaskell (head fs) --TODO Bovenstaande probeersels aan de praat krijgen.
   
   
   showHaskell :: Fspc -> IO ()
   showHaskell fspc
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


   functionalSpecLaTeX :: Contexts -> String ->   String ->    Lang ->  String -> IO()
   functionalSpecLaTeX    contexts    contextname graphicstyle language filename
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
       spec = funcSpec context (datasets,generatedServices,rels,ruls) language
       (datasets,generatedServices,rels,ruls) = erAnalysis context

   viewEstimates :: Contexts -> String ->   String ->    Lang ->  String -> IO()
   viewEstimates    contexts    contextname graphicstyle language filename
    = putStr (chain "\n\n" (map showADL generatedServices))
      where
       context = if null ctxs then error ("!Mistake: "++contextname++" not encountered in input file.\n") else head ctxs
       ctxs    = [c| c<-contexts, name c==contextname]
       (datasets,generatedServices,rels,ruls) = erAnalysis context

   serviceGen :: Contexts -> String ->   String ->    Lang ->  String -> IO()
   serviceGen    contexts    contextname graphicstyle language filename
    = putStr (chain "\n\n" (map showADL generatedServices))
      where
       context = if null ctxs then error ("!Mistake: "++contextname++" not encountered in input file.\n") else head ctxs
       ctxs    = [c| c<-contexts, name c==contextname]
       (datasets,generatedServices,rels,ruls) = erAnalysis context

   archText :: Contexts -> String ->   String ->    Lang ->  String -> IO()
   archText    contexts    contextname graphicstyle language filename
    = putStr ("\nGenerating architecture document for context "++
              name context++" in the current directory.\n")                   >>
      graphics context (fnContext context) graphicstyle False context         >>   -- generate abbreviated (full==False) class diagram
      writeFile (filename++".tex") (generateArchLaTeX context language spec)  >>   -- generate LaTeX code
      putStr ("\nLaTeX file "++filename++".tex written... ")                  >>
      processLaTeX2PDF filename                                                    -- crunch the LaTeX file into PDF.
      where
       context = if null ctxs then error ("!Mistake: "++contextname++" not encountered in input file.\n") else head ctxs
       ctxs    = [c| c<-contexts, name c==contextname]
       spec = funcSpec context (datasets,generatedServices,rels,ruls) language
       (datasets,generatedServices,rels,ruls) = erAnalysis context
     -- the following is copied from Atlas.lhs. TODO: remove double code.

   glossary :: Contexts -> String -> Lang -> IO()
   glossary    contexts contextname language
    = putStr ("\nGenerating Glossary for "++name context++" in the current directory.") >>
      writeFile ("gloss"++name context++".tex") (generateGlossaryLaTeX context language)           >>
      putStr ("\nLaTeX file "++"gloss"++name context++".tex written... ") >>
      processLaTeX2PDF ("gloss"++name context)
      where
       context = if null ctxs then error ("!Mistake: "++contextname++" not encountered in input file.\n") else head ctxs
       ctxs    = [c| c<-contexts, name c==contextname]

  --  graphics ::  Context -> String -> String -> Bool ->  { Pattern of Context ??? } -> IO()
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

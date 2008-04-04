> module Main where 
>  import System (getArgs)
>  import UU_Scanner
>  import UU_Parsing
>  import CommonClasses ( Identified(name) )
>  import Auxiliaries (chain, commaEng)
>  import Typology (Typology(Typ),typology, makeTrees)
>  import CC_aux (isa)
>  import AGtry (sem_Architecture)
>  import CC (pArchitecture, keywordstxt, keywordsops,specialchars,opchars)
>  import Calc
>  import Atlas (anal)
>  import ERmodel (erModel)
>  import ClassDiagram (cdModel)  

>  latexOpt sws = "-l" `elem` sws 
>  splitStr f (x:xs) | f x  = (x:yes, no)
>                    | True = (yes, x:no)
>                    where (yes,no) = splitStr f xs
>  splitStr f [] = ([],[])

>  main
>   = do { a <- getArgs
>        ; let (switches,args) = splitStr ((=="-").take 1) a
>        ; putStr ("ADL vs. 0.8.08 (Atlas disclosure)\nArguments: "++chain ", " args++"\nSwitches: "++chain ", " switches)
>        ; if length args==0 then putStr ("Please provide a filename (.adl) and a context name") else
>     do { let fn = args!!0; contextname = args!!1
>              (fnPrefix,fnSuffix) = break ('.' ==) fn
>              fnFull = if null fnSuffix then (fn ++ ".adl") else fn
>        ; inp<-readFile fnFull
>        ; putStr ("\n"++fnFull++" is read.")
>        ; slRes <- parseIO (pArchitecture ("-beeper" `elem` switches)) (scan keywordstxt keywordsops specialchars opchars fnFull initPos inp)
>        ; putStr ("\n"++fnFull++" has been parsed.")
>        ; let (contexts,errs) = sem_Architecture slRes
>        ; let Typ pths = if null contexts then Typ [] else
>                         if length args>1 && contextname `elem` map name contexts
>                         then typology (isa (head [c| c<-contexts,name c==contextname]))
>                         else typology (isa (head contexts))
>        ; putStr "\nConcepts:\n" >>(putStr.chain "\n".map show) (makeTrees (Typ (map reverse pths)))
>        ; if null errs 
>          then (putStr ("\nNo type errors or cyclic specializations were found.\n")>>
>                if length args==1 && length contexts==1
>                then build contexts switches (name (head contexts)) slRes else
>                if length args==1 && length contexts>1
>                then putStr ("\nPlease specify the name of a context."++
>                             "\nAvailable contexts: "++commaEng "and" (map name contexts)++".\n") else
>                if length args>1 && contextname `elem` map name contexts
>                then build contexts switches contextname slRes
>                else putStr ("\nContext "++contextname++" not defined."++
>                             "\nPlease specify the name of an available context."++
>                             "\nAvailable contexts: "++commaEng "and" (map name contexts)++"."++
>                             "\n(Note: context names are case sensitive).\n")
>               )
>          else putStr ("\nThe type analysis of "++fnFull++" yields errors.\n")>>
>               putStr (concat ["!Error of type "++err| err<-errs])>>
>               putStr ("Nothing generated, please correct mistake(s) first.\n")
>        }}
>      where build contexts switches contextname slRes
>             = sequence_ 
>                ([ anal contexts contextname ("-p" `elem` switches) (if "-crowfoot" `elem` switches then "crowfoot" else "cc")
>                 | null switches || "-h" `elem` switches || "-p" `elem` switches || "-crowfoot" `elem` switches]++
>                 [ erModel contexts contextname | "-ER" `elem` switches]++
>                 [ cdModel contexts contextname | "-OO" `elem` switches]++
>                 [ putStr (show slRes) | "-dump" `elem` switches ]
>                )

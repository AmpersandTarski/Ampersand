> module Main where
>  import System (getArgs)
>  import Char (toLower)
>  import UU_Scanner
>  import UU_Parsing
>  import CommonClasses ( Identified(name), empty )
>  import Auxiliaries (chain, commaEng, adlVersion)
>  import Typology (Typology(Typ), typology, makeTrees)
>  import CC_aux (isa, Lang(English,Dutch), Context(Ctx), showHS)
>  import AGtry (sem_Architecture)
>  import CC (pArchitecture, keywordstxt, keywordsops, specialchars, opchars)
>  import Calc (deriveProofs)
>  import Fspec (zed,glossary,functionalSpecText,projectSpecText)
>  import Atlas (anal)
>  import ERmodel (erModel)
>  import ClassDiagram (cdModel)  
>  import RelBinGen


>  latexOpt sws = "-l" `elem` sws
>  splitStr f (x:xs) | f x  = (x:yes, no)
>                    | True = (yes, x:no)
>                    where (yes,no) = splitStr f xs
>  splitStr f [] = ([],[])

>  main
>   = do { a <- getArgs
>        ; putStr (chain ", " a++"\n")
>        ; let (switches,args') = splitStr ((=="-").take 1) a
>        ; let (dbArgs,args) = splitStr ((=="+").take 1) args'
>        ; putStr (adlVersion++"\nArguments: "++chain ", " args++"\nSwitches: "++chain ", " switches++"\nDatabase: "++chain ", " (map tail dbArgs))
>        ; if "-checker" `elem` switches && "-beeper" `elem` switches then putStr ("incompatible switches: -checker and -beeper") else
>          if length args==0 then putStr ("Please provide a filename (.adl) and a context name") else
>          if length dbArgs>1 then putStr (". This is confusing! please specify 1 database name only.") else
>     do { let fn = args!!0; contextname = args!!1
>              dbName | null dbArgs = fnOutp
>                     | otherwise   = tail (head dbArgs)
>              (fnPrefix,fnSuffix) = (take (length fn-4) fn, drop (length fn-4) fn)
>              fnFull = if map Char.toLower fnSuffix /= ".adl" then (fn ++ ".adl") else fn
>              fnOutp = take (length fnFull-4) fnFull
>        ; inp<-readFile fnFull
>        ; putStr ("\n"++fnFull++" is read.")
>        ; slRes <- parseIO (pArchitecture ("-beeper" `elem` switches))(scan keywordstxt keywordsops specialchars opchars fnFull initPos inp)
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
>                then build contexts switches (name (head contexts)) fnOutp dbName slRes else
>                if length args==1 && length contexts>1
>                then putStr ("\nPlease specify the name of a context."++
>                             "\nAvailable contexts: "++commaEng "and" (map name contexts)++".\n") else
>                if length args>1 && contextname `elem` map name contexts
>                then build contexts switches contextname fnOutp dbName slRes
>                else putStr ("\nContext "++contextname++" not defined."++
>                             "\nPlease specify the name of an available context."++
>                             "\nAvailable contexts: "++commaEng "and" (map name contexts)++"."++
>                             "\n(Note: context names are case sensitive).\n")
>               )
>          else putStr ("\nThe type analysis of "++fnFull++" yields errors.\n")>>
>               putStr (concat ["!Error of type "++err| err<-errs])>>
>               putStr ("Nothing generated, please correct mistake(s) first.\n")
>        }}
>      where build contexts switches contextname filename dbName slRes
>             = sequence_ 
>                ([ anal contexts contextname ("-p" `elem` switches) (if "-crowfoot" `elem` switches then "crowfoot" else "cc")
>                 | null switches || "-h" `elem` switches || "-p" `elem` switches || "-crowfoot" `elem` switches]++
>                 [ diagnose contexts contextname| "-diag" `elem` switches]++
>                 [ zed contexts contextname (if "-crowfoot" `elem` switches then "crowfoot" else "cc") (lang switches) filename| "-Z" `elem` switches]++
>                 [ glossary contexts contextname (lang switches) | "-g" `elem` switches]++
>                 [ erModel contexts contextname | "-ER" `elem` switches]++
>                 [ cdModel contexts contextname | "-CD" `elem` switches]++
>                 [ phpServices contexts contextname filename dbName True True | "-beeper" `elem` switches]++
>                 [ phpServices contexts contextname filename dbName ("-notrans" `elem` switches) False| "-checker" `elem` switches]++
>                 [ functionalSpecText contexts contextname (if "-crowfoot" `elem` switches then "crowfoot" else "cc") (lang switches) | "-f" `elem` switches]++
>                 [ deriveProofs contexts contextname ("-m" `elem` switches)| "-proofs" `elem` switches]++
>                 [ projectSpecText contexts contextname (lang switches) | "-project" `elem` switches]++
>--                 [ csvcontent contexts contextname | "-csv" `elem` switches]++
>                 [ putStr (show slRes) | "-dump" `elem` switches ]
>                )
>            lang switches
>             | "-NL" `elem` switches = Dutch
>             | "-UK" `elem` switches = English
>             | otherwise             = English

>  diagnose contexts contextname
>   = putStr (showHS context)
>     where
>      context  = (head ([c| c<-contexts, name c==contextname]++
>                        [Ctx (contextname++" is not defined") [] empty [] [] [] [] []]))

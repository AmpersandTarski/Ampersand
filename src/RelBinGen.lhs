> module RelBinGen where
>  import Auxiliaries
>  import CC_aux
>  import CommonClasses
>  import RelBinGenServiceLayer
>  import RelBinGenEntitiesLayer
>  import RelBinGenDatacloud
>--  import RelBinGenEntityScreenLayer


>  phpServices contexts        -- all contexts
>              contextname     -- the current contexts
>              filename        -- the filename to be used
>              dbName          -- the filename to be used
>              noTransactions  -- if True, transaction control is switched off. In that case, all commands will be executed, no matter which violations occur.
>              beeper          -- if True, all rules are translated as signal.
>   = putStr ("\nGenerating MySQL Repository and PHP service layer for "++name context++"\n")>>
>--     putStr ((chain "\n".map show.declarations) context++"\n")                        >>
>--     putStr                     csl                 >>
>     writeFile (filename++".php") csl                 >>
>     putStr (filename++".php written\n")                                                >>
>--     putStr                         esl   >>
>     writeFile (filename++".ent.php") esl   >>
>     putStr (filename++".ent.php written\n")                                            >>
>     writeFile (filename++".cloud.php") (dataCloud context filename)   >>
>     putStr (filename++".cloud.php written\n")                                            >>
>--     writeFile (filename++".entscr.php") (entityScreenLayer context filename noTransactions)     >>
>--     putStr (filename++".entscr.php written\n")                                         >>
>     writeFile (filename++".info") ("UI: drop,entities,vcount,adlExport,cloud\nDefaultUI: entities\nFile_entities: "++filename++".ent.php\nFile_entityScreen: "++filename++".entscr.php\nFile_vcount: "++filename++".ent.php\nFile_adlExport: "++filename++".php\nFile_drop: "++filename++".php\nFile_cloud: "++filename++".cloud.php\nName: "++name context) >>
>     putStr (filename++".info written\n") >>
>     qStat "CREATE TABLE" >>
>     qStat "INSERT IGNORE INTO" >>
>     qStat "INSERT INTO" >>
>     qStat "DELETE FROM" >>
>     qStat "UPDATE" >>
>     qStat "SELECT" >>
>     qStat "DROP" >>
>     qStat "START TRANSACTION"
>     where
>      context = if null ctxs then error ("!Mistake: "++contextname++" not encountered in input file.\n") else head ctxs
>      ctxs    = [c| c<-contexts, name c==contextname]
>      csl = serviceLayer context noTransactions beeper dbName
>      esl = entitiesLayer context filename noTransactions beeper
>      qStat str = putStr ("  nr. of "++str++" queries: "++ [' '|i<-[length str..17]]++show (count str (csl++esl))++"\n")
>      count str text = c str text ""
>       where c (s:ss) ts@(t:tx) r | s==t               = c ss tx (r++[s])
>                                  | s/=t = c str (tail (r++ts)) ""
>             c ""     ts        r = 1+c str ts ""
>             c ss     ""        r = 0

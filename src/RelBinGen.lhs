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
>--     putStr                     (serviceLayer context noTransactions beeper dbName)                 >>
>     writeFile (filename++".php") (serviceLayer context noTransactions beeper dbName)                 >>
>     putStr (filename++".php written\n")                                                >>
>--     putStr                         (entitiesLayer context filename noTransactions beeper)   >>
>     writeFile (filename++".ent.php") (entitiesLayer context filename noTransactions beeper)   >>
>     putStr (filename++".ent.php written\n")                                            >>
>     writeFile (filename++".cloud.php") (dataCloud context filename)   >>
>     putStr (filename++".cloud.php written\n")                                            >>
>--     writeFile (filename++".entscr.php") (entityScreenLayer context filename noTransactions)     >>
>--     putStr (filename++".entscr.php written\n")                                         >>
>     writeFile (filename++".info") ("UI: drop,entities,vcount,adlExport,cloud\nDefaultUI: entities\nFile_entities: "++filename++".ent.php\nFile_entityScreen: "++filename++".entscr.php\nFile_vcount: "++filename++".ent.php\nFile_adlExport: "++filename++".php\nFile_drop: "++filename++".php\nFile_cloud: "++filename++".cloud.php\nName: "++name context) >>
>     putStr (filename++".info written\n")
>     where
>      context = head ([{-recalc-} c| c<-contexts, name c==contextname]++
>                      [Ctx (contextname++" is not defined") [] empty [] [] [] [] []])
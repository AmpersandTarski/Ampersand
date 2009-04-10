{-# OPTIONS_GHC -Wall #-}
module Parser (parseADL)where


import CC (pArchitecture,keywordstxt, keywordsops, specialchars, opchars)
import Options
import UU_Scanner(scan,initPos)
import UU_Parsing(parseIO)
import AGtry
import TypeChecker
import Adl

 
parseADL :: String      -- ^ The string to be parsed
         -> Options     -- ^ flags to be taken into account
         -> String      -- ^ The name of the .adl file (used for error messages)
         -> IO(Context) -- ^ The IO modad with the context. 
parseADL adlstring flags fnFull =
    do { slRes <- parseIO (pArchitecture (beeper flags))(scan keywordstxt keywordsops specialchars opchars fnFull initPos adlstring)
	   ; case procParseRes slRes of        -- this results in a list of contexts and a list of errors. Now we will inspect the result:
	        ( _ , err:errs)-> ioError (userError ("\nThe type analysis of "++fnFull++" yields errors.\n" ++
                                                  (concat ["!Error of type "++err'| err'<-err:errs])++
                                                  "Nothing generated, please correct mistake(s) first.\n"
                                                 ))
	        ( []      ,[]) -> ioError(userError ("no context encountered in input file.\n"))
	        ( contexts,[]) -> case filteredContexts  of
	                            []   -> ioError(userError ("context "++specificName ++" was not encountered in input file.\n"))
	                            cs   -> do{ verboseLn flags (fnFull++ " has been parsed.")
	                                      ; return (head cs) -- Just take the first context encounterd. If there are more contexts no warning is generated.
                                          }
	                          where filteredContexts   = case contextName flags of
	                                                       Just cname -> [c | c <- contexts , cname == ctxnm c]
	                                                       Nothing   -> contexts
	                                specificName = case contextName flags of
	                                                       Just cname -> cname
	                                                       Nothing   -> undefined   --Nothing is niet aan de orde hier
	    }
	    where
	       --DEBUG -> the commented code is the old code to rollback to pipe typechecker -> sem_Architecture
	       --         instead of just typechecker
               procParseRes arch = if (skipTypechecker flags) then typecheck arch --DEBUG -> sem_Architecture arch
                                   else --DEBUG -> case typecheck arch of --when no type errors then AGtry
                                            --DEBUG -> (_,t_e:t_errs) -> ([],t_e:t_errs)
                                            --DEBUG -> (_,[])           -> sem_Architecture arch
                                        typecheck arch
                                         {-DEBUG -> return this to compare context of typechecker and agtry
                                         --         only for one context
                                        ([],compctxs (sem_Architecture arch) (typecheck arch))
                                        where
                                        compctxs (c,[]) (c',[]) = case (head c) of
                                          Ctx{} ->  case (head c') of
                                                    Ctx{} -> map ("\n" ++)
                                                     [ctxnm (head c), ctxnm (head c')
                                                     --,show (ctxon (head c))
                                                     --,show (ctxon (head c'))
                                                     --,show (ctxisa (head c))
                                                     --,show (ctxisa (head c'))
                                                     --,show (ctxwrld (head c))
                                                     --,show (ctxwrld (head c'))
                                                     --,show (ctxpats (head c))
                                                     --,show (ctxpats (head c'))
                                                     --,show (ctxrs (head c))
                                                     --,show (ctxrs (head c'))
                                                     --,show (ctxds (head c))
                                                     --,show (ctxds (head c'))
                                                     ---,show (ctxcs (head c))
                                                     --,show (ctxcs (head c'))
                                                     --,show (ctxks (head c))
                                                     --,show (ctxks (head c'))
                                                     ,show (ctxos (head c))
                                                     ,show (ctxos (head c'))
                                                     --,show (ctxpops (head c))
                                                     --,show (ctxpops (head c'))
                                                     ]
                                        compctxs (_,xs) (_,xs') = xs ++ xs'
                                         -}





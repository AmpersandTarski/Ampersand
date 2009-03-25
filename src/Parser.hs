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
                                        ([],"DEBUGGING - Comparing AGtry and typechecker" : compctxs (sem_Architecture arch) (typecheck arch))
                                        where
                                        compRes True msg = --msg
                                                            []
                                        compRes False msg = msg
                                        compctxs (c,[]) (c',[]) = case (head c) of
                                          Ctx{} ->  case (head c') of
                                                    Ctx{} ->
                                                     foldr (:) []
                                                     [compRes (ctxnm (head c) == ctxnm (head c')) (composemsg "name" ctxnm)
                                                     -- ,compRes (ctxon (head c) == ctxon (head c')) (composemsg "ext" ctxon)
                                                     ,compRes (ctxisa (head c) == ctxisa (head c')) (composemsg "isa" ctxisa)
                                                     -- ,compRes (ctxwrld (head c) == ctxwrld (head c')) (composemsg "wrld" ctxwrld)
                                                     -- ,compRes (ctxpats (head c) == ctxpats (head c')) (composemsg "pats" ctxpats)
                                                     ,compRes (ctxrs (head c) == ctxrs (head c')) (composemsg "rs" ctxrs)
                                                     ,compRes (ctxds (head c) == ctxds (head c')) (composemsg "ds" ctxds)
                                                     --,compRes (ctxcs (head c) == ctxcs (head c')) (composemsg "cs" ctxcs)
                                                     --,compRes (ctxks (head c) == ctxks (head c')) (composemsg "ks" ctxks)
                                                     -- De show van ctxos LOOPED!! ,compRes (ctxos (head c) == ctxos (head c')) (composemsg "os" ctxos)
                                                     -- ,compRes (ctxpops (head c) == ctxpops (head c')) (composemsg "pops" ctxpops)
                                                     ]
                                                     where composemsg part att = "DEBUG -> "++part++":\n\n" ++ show (att (head c)) ++ "\n\n" ++ show (att (head c')) ++ "\n\n"
                                        compctxs (_,ers) (_,ers') = ers ++ ers'
                                        -}





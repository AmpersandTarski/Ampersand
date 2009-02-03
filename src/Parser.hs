
module Parser (parseADL)where


import CC (pArchitecture,keywordstxt, keywordsops, specialchars, opchars)
import Options
import UU_Scanner(scan,initPos)
import UU_Parsing(parseIO)
import AGtry
import Adl

 
parseADL :: String      -- | The string to be parsed
         -> Options     -- | flags to be taken into account
         -> String      -- | The name of the .adl file (used for error messages)
         -> IO(Context) -- | The IO modad with the context. 
parseADL adlstring flags fnFull =
    do { slRes <- parseIO (pArchitecture (beeper flags))(scan keywordstxt keywordsops specialchars opchars fnFull initPos adlstring)
	   ; case sem_Architecture slRes of        -- this results in a list of contexts and a list of errors. Now we will inspect the result: 
	        ( _ , e:errs)  -> ioError (userError ("\nThe type analysis of "++fnFull++" yields errors.\n" ++
                                                  (concat ["!Error of type "++err| err<-e:errs])++
                                                  "Nothing generated, please correct mistake(s) first.\n"
                                                 ))
	        ( []      ,[]) -> ioError(userError ("no context encountered in input file.\n"))
	        ( contexts,[]) -> case filtered contexts of
	                            []   -> ioError(userError ("context "++specificName ++" was not encountered in input file.\n"))
	                            cs   -> do{ verboseLn flags (fnFull++ " has been parsed.")
	                                      ; return (head cs) -- Just take the first context encounterd. If there are more contexts no warning is generated.
                                          }
	                          where filtered contexts  = case contextName flags of
	                                                       Just name -> [c | c <- contexts , name == ctxnm c] 
	                                                       Nothing   -> contexts
	                                specificName = case contextName flags of
	                                                       Just name -> name
	                                                       --Nothing is niet aan de orde hier
	    }    

   
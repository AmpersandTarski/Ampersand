{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Input.ADL1.Parser (parseADL1,parseADL1Pop)where


import DatabaseDesign.Ampersand.Input.ADL1.CC (pArchitecture,pPopulations,keywordstxt, keywordsops, specialchars, opchars)
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner(scan,initPos)
import DatabaseDesign.Ampersand.Input.ADL1.UU_Parsing(parseIO)
import DatabaseDesign.Ampersand.Input.ADL1.TypeChecker (typecheckAdl1)
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Output (writepandoc)
import Text.Pandoc 
 
parseADL1 :: String     -- ^ The string to be parsed
         -> Populations Concept -- ^ The population derived from the import file (--import=file)
         -> Options     -- ^ flags to be taken into account
         -> String      -- ^ The name of the .adl file (used for error messages)
         -> IO(Context) -- ^ The IO monad with the context. 
parseADL1 adlstring importpop flags fnFull =
    do { slRes <- parseIO pArchitecture (scan keywordstxt keywordsops specialchars opchars fnFull initPos adlstring)
       ; case typecheckAdl1 slRes importpop of        -- this results in a list of contexts and a list of errors. Now we will inspect the result:
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
                                                               Nothing   -> error ("!Fatal (module Parser 32): Contact your dealer!")   --Nothing is niet aan de orde hier
                (_, errs)-> if not(null cs) && theme flags==ProofTheme && fspecFormat flags==FLatex
                            then do {maketex;makepdf
                                    ; fail "report written"} --Something must be returned
                            else ioError (userError ("\nThe type analysis of "++fnFull++" yields errors.\n" ++
                                                  (concat ["!Error of type "++err'| (err',_)<-errs])++
                                                  "Nothing generated, please correct mistake(s) first.\n"
                                                 ))
                            where Arch cs = slRes
                                  thepandoc = Pandoc (Meta [] [] []) (concat (map snd errs))
                                  (_,maketex,makepdf) = writepandoc flags thepandoc
       }

parseADL1Pop :: String     -- ^ The string to be parsed
         -> String      -- ^ The name of the .pop file (used for error messages)
         -> IO(Populations Concept) -- ^ The IO monad with the populations. 
parseADL1Pop popsstring fnFull = parseIO pPopulations (scan keywordstxt keywordsops specialchars opchars fnFull initPos popsstring)




module Parser (parseAdl)where


import CC()
import Options
import UU_Scanner(scan,initPos)
import UU_Parsing(parseIO)


 
parseAdl :: String      --The string to be parsed
         -> Options     --flags to be taken into account
         -> String      --The name of the .adl file (used for error messages)
         -> IO(Architecture)
parseAdl adlstring flags fnFull =
       parseIO (pArchitecture (beeper flags))(scan keywordstxt keywordsops specialchars opchars fnFull initPos adlstring)
   --      ; let (contexts,errs) = sem_Architecture slRes
   --       ; 
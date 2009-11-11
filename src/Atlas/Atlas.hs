module Atlas.Atlas where
import Adl
import Data.Fspec
import Options
import Classes.Populated
import Rendering.AdlExplanation
import Typology 
import Collection     ( Collection (rd) ) 
import Database.HDBC.ODBC 
import Database.HDBC

data ATable = ATable {tableid::ATableId, tablename::String, columns::[String]} deriving (Show)
data ATableId = 
   ATAtom
  |ATConcept
  |ATContains
  |ATContainsConcept
  |ATExplanation
  |ATExpression
  |ATMainPicture
  |ATPair
  |ATRelation
  |ATRule
  |ATThePicture
  |ATViolates deriving (Show)
tables = 
   [ATable ATAtom "atom" ["i"] 
   ,ATable ATConcept "concept" ["i"] 
   ,ATable ATContains "contains" ["relation","pair"] 
   ,ATable ATContainsConcept "containsconcept" ["concept","atom"] 
--   ,ATable ATExplanation "explanation" ["i"] 
--   ,ATable ATExpression "expression" ["i","source","target"] 
--   ,ATable ATMainPicture "mainpicture" ["i"] 
--   ,ATable ATPair "pair" ["i"] 
   ,ATable ATRelation "relation" ["i","source","target"]
   ,ATable ATRule "rule" ["i","source","target","explanation"] 
   ,ATable ATThePicture "thepicture" ["mainpicture","mainpicture1"] 
   ,ATable ATViolates "violates" ["pair","rule"]
   ]

--Atlas requires an ODBC data source named "atlas" representing the db of an Atlas.adl prototype
--hdbc and hdbc-odbc must be installed (from hackage)
fillAtlas :: Fspc -> Options -> IO()
fillAtlas fSpec flags = 
    verboseLn flags "Populating atlas for ..."
 >> do 
    --connect through ODBC data source "atlas"
    conn<-connectODBC "DSN=atlas"
    --TODO handle connection errors
    --TODO check if actual MySql tables of atlas correspond to function tables
    --delete all existing content of this ADL script of this user
    runMany conn ["DELETE FROM "++tablename x|x<-tables]
    --insert population of this ADL script of this user
    insertpops conn fSpec flags tables
    --end connection
    commit conn
    disconnect conn
----------------------------------------------------
runMany _ [] = return 1
runMany conn (x:xs)  = 
   do run conn x []
      runMany conn xs

insertpops _ _ _ [] = return 1
insertpops conn fSpec flags (x:xs) =  
   do stmt<- prepare conn$"INSERT INTO "++tablename x++" VALUES ("++placeholders(columns x)++")"
      executeMany stmt (pop$tableid x)
      insertpops conn fSpec flags xs 
   where
   pop ATRelation = rd[map toSql [name x,name$source x,name$target x]|x<-vrels fSpec]
   pop ATRule = rd[map toSql [show x,name$source x,name$target x,explainRule flags x]|x<-vrules fSpec]
   --differentiate between normal rules and multiplicities, gens and homoprops
   --convert pair to violation message
   pop ATViolates = rd[map toSql [show x,show y] | (x,y)<-violations fSpec]
   pop ATThePicture = rd[map toSql ["picture link or something","picture link or something"]]
   pop ATContains = rd[map toSql [name x,show y]| x<-vrels fSpec, y<-contents x]
   pop ATContainsConcept = rd[map toSql [x,y]|(x,y)<-cptsets]
   pop ATAtom = rd[[toSql x]|(_,x)<-cptsets]
   pop ATConcept = rd[[toSql x]|(x,_)<-cptsets]
   pop ATExplanation = error "TODO pop ATExplanation"
   pop ATExpression = error "TODO pop ATExpression"
   pop ATMainPicture = error "TODO pop ATMainPicture"
   pop ATPair = error "TODO pop ATPair"
   cptsets = (\(Isa _ cs) -> [(name c,x)|c@(C{})<-cs, x<-cptos c]) (fsisa fSpec)
placeholders [] = []
placeholders (x:[]) = "?"
placeholders (x:xs) = "?," ++ placeholders xs
   



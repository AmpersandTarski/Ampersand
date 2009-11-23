module Atlas.Atlas where
import Adl
import ShowADL
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
--  |ATExpression
  |ATMainPicture
  |ATPair
  |ATRelation
  |ATRelVar
  |ATRule
  |ATThePicture
  |ATType
  |ATViolates
  |ATViolation deriving (Eq,Show)
tables::[ATable]
tables = 
   [ATable ATAtom "atom" ["i"] 
   ,ATable ATConcept "concept" ["i"] 
   ,ATable ATContains "contains" ["relation","pair"] 
   ,ATable ATContainsConcept "containsconcept" ["concept","atom"] 
   ,ATable ATExplanation "explanation" ["i"] 
  -- ,ATable ATExpression "expression" ["i","source","target"] 
   ,ATable ATMainPicture "mainpicture" ["i"] 
   ,ATable ATPair "pair" ["i"] 
   ,ATable ATRelation "relation" ["i"]
   ,ATable ATRelVar "relvar" ["relation","type"]
   ,ATable ATRule "rule" ["i","type","explanation"] 
   ,ATable ATThePicture "thepicture" ["mainpicture","mainpicture1"] 
   ,ATable ATType "type" ["i","source","target"] 
   ,ATable ATViolates "violates" ["violation","rule"]
   ,ATable ATViolation "violation" ["i"]
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
runMany :: (IConnection conn) => conn -> [String] -> IO Integer
runMany _ [] = return 1
runMany conn (x:xs)  = 
   do run conn x []
      runMany conn xs

insertpops :: (IConnection conn) => conn -> Fspc -> Options -> [ATable] -> IO Integer
insertpops _ _ _ [] = return 1
insertpops conn fSpec flags (tbl:tbls) =  
   do stmt<- prepare conn$"INSERT INTO "++tablename tbl++" VALUES ("++placeholders(columns tbl)++")"
      executeMany stmt (pop$tableid tbl)
      insertpops conn fSpec flags tbls 
   where
   pop x = [map toSql ys|ys<-rd (pop' x)]
   pop':: ATableId -> [[String]]
   pop' ATRelation = [[name x]|x<-vrels fSpec]
   pop' ATRelVar = [[name x,name(source x)++"*"++(name$target x)]|x<-vrels fSpec]
   pop' ATRule = [[cptrule x,name(source x)++"*"++(name$target x),explainRule flags x]|x<-vrules fSpec]
   --differentiate between normal rules and multiplicities, gens and homoprops
   pop' ATType = [t x|x<-vrels fSpec] ++ [t x|x<-vrules fSpec]
        where t x = [name(source x)++"*"++(name$target x), name$source x, name$target x]
   --convert pair to violation message
   pop' ATViolates = [[show y,cptrule x] | (x,y)<-violations fSpec]
   pop' ATViolation = [[show y] | (_,y)<-violations fSpec]
   pop' ATThePicture = [[name fSpec++".png",name fSpec++".png"]]
   pop' ATContains = [[name x,show y]| x<-vrels fSpec, y<-contents x]
   pop' ATContainsConcept = [[x,y]|(x,y)<-cptsets]
   pop' ATAtom = [[x]|(_,x)<-cptsets]
   pop' ATConcept = [[x]|(x,_)<-cptsets]
   pop' ATExplanation = [[explainRule flags x]|x<-vrules fSpec]
 --  pop' ATExpression = [] --TODO - generalisation must be fixed first in -p of atlas
   pop' ATMainPicture = [[name fSpec++".png"]]
   pop' ATPair = [[show y]| x<-vrels fSpec, y<-contents x]
   cptsets = (\(Isa _ cs) -> [(name c,x)|c@(C{})<-cs, x<-cptos c]) (fsisa fSpec)
   cptrule x@(Sg{})  = "SIGNAL: " ++ (cptrule$srsig x)
   cptrule x@(Gc{})  = "Gc " ++ (printadl (Just fSpec) 0$grgen x)
   cptrule x@(Fr{})  = "Fr " ++ (printadl (Just fSpec) 0$grgen x)
   cptrule x@(Ru{}) 
       | rrsrt x==Implication = printadl (Just fSpec) 0 (rrant x) ++ " |- " ++ (printadl (Just fSpec) 0$rrcon x)
       | rrsrt x==Equivalence = printadl (Just fSpec) 0 (rrant x) ++ " = " ++ (printadl (Just fSpec) 0$rrcon x)
       | rrsrt x==Truth = printadl (Just fSpec) 0 (rrcon x)
       | otherwise = []

placeholders :: [a] -> String
placeholders [] = []
placeholders (_:[]) = "?"
placeholders (_:xs) = "?," ++ placeholders xs
   



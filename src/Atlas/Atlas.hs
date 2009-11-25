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
  |ATHomoRule
  |ATIsa
  |ATMainPicture
  |ATMultRule
  |ATPair
  |ATProp
  |ATRelation
  |ATRelVar
  |ATRule
  |ATThePicture
  |ATType
  |ATUserRule
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
   ,ATable ATHomoRule "homogeneousrule" ["i","property","on"] 
   ,ATable ATIsa "isarelation" ["i","specific","general"] 
   ,ATable ATMainPicture "mainpicture" ["i"] 
   ,ATable ATMultRule "multiplicityrule" ["i","property","on"] 
   ,ATable ATPair "pair" ["i"] 
   ,ATable ATProp "prop" ["i"] 
   ,ATable ATRelation "relation" ["i"]
   ,ATable ATRelVar "relvar" ["relation","type"]
   ,ATable ATRule "rule" ["i","type","explanation"] 
   ,ATable ATThePicture "thepicture" ["mainpicture","mainpicture1"] 
   ,ATable ATType "type" ["i","source","target"] 
   ,ATable ATUserRule "userrule" ["i"] 
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
   pop' ATAtom = [[x]|(_,x)<-cptsets]
   pop' ATConcept = [[name x]|x<-cpts]
   pop' ATContains = [[name x,show y]| x<-vrels fSpec, y<-contents x]
   pop' ATContainsConcept = [[x,y]|(x,y)<-cptsets]
   pop' ATExplanation = [[explainRule flags x]|x<-atlasrules]
 --  pop' ATExpression = [] --TODO - generalisation must be fixed first in -p of atlas
   pop' ATHomoRule = [(\(Just (p,d))->[cptrule x,show p,name d])$rrdcl x |x<-homorules]
   pop' ATIsa = [[show x,show(genspc x), show(gengen x)]|p<-vpatterns fSpec, x<-ptgns p]
   pop' ATMainPicture = [[picturelink]]
   pop' ATMultRule = [(\(Just (p,d))->[cptrule x,show p,name d])$rrdcl x |x<-multrules]
   pop' ATPair = [[show y]| x<-vrels fSpec, y<-contents x]
   pop' ATProp = [[show x]|x<-[Uni,Tot,Inj,Sur,Rfx,Sym,Asy,Trn]]
   pop' ATRelation = [[name x]|x<-vrels fSpec]
   pop' ATRelVar = [[name x,name(source x)++"*"++(name$target x)]|x<-vrels fSpec]
   pop' ATRule = [[cptrule x,name(source x)++"*"++(name$target x),explainRule flags x]|x<-atlasrules]
   pop' ATThePicture = [[picturelink,picturelink]]
   pop' ATType = [t x|x<-vrels fSpec] ++ [t x|x<-atlasrules]
        where t x = [name(source x)++"*"++(name$target x), name$source x, name$target x]
   pop' ATUserRule = [[cptrule x]|x<-userrules]
   --convert pair to violation message
   pop' ATViolates = [[show y,cptrule x] | (x,y)<-violations fSpec]
   pop' ATViolation = [[show y] | (_,y)<-violations fSpec]
   --------------------------------------------------------
   picturelink =  "./img/" ++ name fSpec++".png"
   cpts = (\(Isa _ cs) -> [c|c@(C{})<-cs]) (fsisa fSpec)
   cptsets = [(name c,x)|c@(C{})<-cpts, x<-cptos c]
   cptrule x@(Sg{})  = "SIGNAL: " ++ (cptrule$srsig x)
   cptrule x@(Gc{})  = "Gc " ++ (printadl (Just fSpec) 0$grgen x)
   cptrule x@(Fr{})  = "Fr " ++ (printadl (Just fSpec) 0$grgen x)
   cptrule x@(Ru{}) 
       | rrsrt x==Implication = printadl (Just fSpec) 0 (rrant x) ++ " |- " ++ (printadl (Just fSpec) 0$rrcon x)
       | rrsrt x==Equivalence = printadl (Just fSpec) 0 (rrant x) ++ " = " ++ (printadl (Just fSpec) 0$rrcon x)
       | rrsrt x==Truth = printadl (Just fSpec) 0 (rrcon x)
       | otherwise = []
   --DESCR -> userrules are user-defined rules, 
   --         multrules are rules defined by a multiplicity, 
   --         homorules by a homogeneous property
   --         the rule from an ISA declaration (I[spec] |- I[gen]) is not presented as a rule in the atlas
   atlasrules = userrules ++ multrules ++ homorules
   userrules = [x|x<-vrules fSpec, rrdcl x==Nothing, not (isIsaRule x)]
      where 
      isIsaRule x = rrsrt x==Implication && (isI$rrant x) && (isI$rrcon x)
      isI (Tm (I{})) = True
      isI _ = False
   multrules = [x|x<-vrules fSpec, isMultRule (rrdcl x) ]
      where 
      isMultRule (Just (p,_)) = elem p [Uni,Tot,Inj,Sur]
      isMultRule Nothing = False      
   homorules = [x|x<-vrules fSpec, isHomoRule (rrdcl x) ]
      where 
      isHomoRule (Just (p,_)) = elem p [Rfx,Sym,Asy,Trn] 
      isHomoRule Nothing = False

placeholders :: [a] -> String
placeholders [] = []
placeholders (_:[]) = "?"
placeholders (_:xs) = "?," ++ placeholders xs
   



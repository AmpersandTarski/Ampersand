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
------
import Classes.Graphics
import System (system, ExitCode(ExitSuccess,ExitFailure))
import Strings      (remSpaces)
import System.FilePath(combine,replaceExtension)
import System.Directory(createDirectoryIfMissing)

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
  |ATPicture
  |ATMultRule
  |ATPair
  |ATProp
  |ATRelation
  |ATRelVar
  |ATRule
  |ATType
  |ATUserRule
  |ATViolRule
  |ATViolHomoRule
  |ATViolMultRule
  |ATViolUserRule
  |ATViolation deriving (Eq,Show)
tables::[ATable]
tables = 
   [ATable ATAtom "atom" ["i"] 
   ,ATable ATConcept "concept" ["i"] 
   ,ATable ATContains "contains" ["relation","pair"] 
   ,ATable ATContainsConcept "containsconcept" ["concept","atom"] 
   ,ATable ATExplanation "explanation" ["i"] 
  -- ,ATable ATExpression "expression" ["i","source","target"] 
   ,ATable ATHomoRule "homogeneousrule" ["i","property","on","type","explanation"] 
   ,ATable ATIsa "isarelation" ["i","specific","general"] 
   ,ATable ATPicture "picture" ["i","thepicture"] 
   ,ATable ATMultRule "multiplicityrule" ["i","property","on","type","explanation"] 
   ,ATable ATPair "pair" ["i"] 
   ,ATable ATProp "prop" ["i"] 
   ,ATable ATRelation "relation" ["i"]
   ,ATable ATRelVar "relvar" ["relation","type"]
   ,ATable ATRule "rule" ["i","type","explanation"] 
   ,ATable ATType "type" ["i","source","target"] 
   ,ATable ATUserRule "userrule" ["i","type","explanation"] 
   ,ATable ATViolRule "violates" ["violation","rule"]
   ,ATable ATViolHomoRule "violateshomogeneousrule" ["violation","homogeneousrule"]
   ,ATable ATViolMultRule "violatesmultiplicityrule" ["violation","multiplicityrule"]
   ,ATable ATViolUserRule "violatesviolation" ["violation","userrule"]
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
    --create the pictures in a folder for this user
    runMany conn ["DELETE FROM "++tablename x|x<-tables]
    --insert population of this ADL script of this user
    insertpops conn fSpec flags tables pictlinks
    --end connection
    commit conn
    disconnect conn
 >> createDirectoryIfMissing True fpath
 >> foldr (>>) (verboseLn flags "All pictures written..") dots
    where
    pictlinks = [".\\img\\"++ (name fSpec) ++ ".png"| p<-vpatterns fSpec]
                --TODO -> patterns [".\\img\\"++ (remSpaces$name p) ++ ".png"| p<-vpatterns fSpec]
    fpath = combine (dirAtlas flags) "img/"
    outputFile fnm = combine fpath fnm
    dots = [makeGraphic (name fSpec)$ toDot fSpec flags $ 
             if length(vpatterns fSpec)==0 then error "There is no pattern to fold"
             else foldr (union) (head$vpatterns fSpec) (tail$vpatterns fSpec)]
           --TODO -> patterns [makeGraphic (remSpaces (name p)) $ toDot fSpec flags p 
           --                 | p<-vpatterns fSpec, (not.null) (concs p)] 
    makeGraphic fnm dot
      = do 
        succes <- runGraphvizCommand Neato dot Canon dotfile
        if succes
           then do
             result <- system ("neato -Tpng "++dotfile++ " -o "++pngfile)
             case result of 
                ExitSuccess   -> putStrLn (" "++pngfile++" created.")
                ExitFailure x -> putStrLn ("Failure: " ++ show x)
           else putStrLn ("Failure: could not create " ++ dotfile) 
        where
        dotfile = replaceExtension (outputFile fnm) "dot"
        pngfile = replaceExtension (outputFile fnm) "png"
----------------------------------------------------
runMany :: (IConnection conn) => conn -> [String] -> IO Integer
runMany _ [] = return 1
runMany conn (x:xs)  = 
   do run conn x []
      runMany conn xs

type PictureLinks = [String]
insertpops :: (IConnection conn) => conn -> Fspc -> Options -> [ATable] -> PictureLinks -> IO Integer
insertpops _ _ _ [] _ = return 1
insertpops conn fSpec flags (tbl:tbls) pics =  
   do stmt<- prepare conn$"INSERT INTO "++tablename tbl++" VALUES ("++placeholders(columns tbl)++")"
      executeMany stmt (pop$tableid tbl)
      insertpops conn fSpec flags tbls pics
   where
   pop x = [map toSql ys|ys<-rd (pop' x)]
   pop':: ATableId -> [[String]]
   pop' ATAtom = [[x]|(_,x)<-cptsets]
   pop' ATConcept = [[name x]|x<-cpts]
   pop' ATContains = [[name x,show y]| x<-vrels fSpec, y<-contents x]
   pop' ATContainsConcept = [[x,y]|(x,y)<-cptsets]
   pop' ATExplanation = [[explainRule flags x]|x<-atlasrules]
 --  pop' ATExpression = [] --TODO - generalisation must be fixed first in -p of atlas
   pop' ATHomoRule = [(\(Just (p,d))->[cptrule x,show p,name d,cpttype x,explainRule flags x])$rrdcl x |x<-homorules]
   pop' ATIsa = [[show x,show(genspc x), show(gengen x)]|p<-vpatterns fSpec, x<-ptgns p]
   pop' ATPicture = [[x,x]|x<-pics]
   pop' ATMultRule = [(\(Just (p,d))->[cptrule x,show p,name d,cpttype x,explainRule flags x])$rrdcl x |x<-multrules]
   pop' ATPair = [[show y]| x<-vrels fSpec, y<-contents x]
   pop' ATProp = [[show x]|x<-[Uni,Tot,Inj,Sur,Rfx,Sym,Asy,Trn]]
   pop' ATRelation = [[name x]|x<-vrels fSpec]
   pop' ATRelVar = [[name x,cpttype x]|x<-vrels fSpec]
   pop' ATRule = [[cptrule x,cpttype x,explainRule flags x]|x<-atlasrules]
   pop' ATType = [t x|x<-vrels fSpec] ++ [t x|x<-atlasrules]
        where t x = [cpttype x, name$source x, name$target x]
   pop' ATUserRule = [[cptrule x,cpttype x,explainRule flags x]|x<-userrules]
   --convert pair to violation message
   --There is overhead in the violates* tables, but that does not matter for the result. Fix SQL and generalisation instead.
   --then there will be only one table "violates" for all rules.
   pop' ATViolRule = [[show y,cptrule x] | (x,y)<-violations fSpec]
   pop' ATViolHomoRule = [[show y,cptrule x] | (x,y)<-violations fSpec]
   pop' ATViolMultRule = [[show y,cptrule x] | (x,y)<-violations fSpec]
   pop' ATViolUserRule = [[show y,cptrule x] | (x,y)<-violations fSpec]
   pop' ATViolation = [[show y] | (_,y)<-violations fSpec]
   --------------------------------------------------------
   --picturelink =  "./img/" ++ name fSpec++".png"
   cpts = (\(Isa _ cs) -> [c|c@(C{})<-cs]) (fsisa fSpec)
   cptsets = [(name c,x)|c@(C{})<-cpts, x<-cptos c]
   cptrule x@(Sg{})  = "SIGNAL: " ++ (cptrule$srsig x)
   cptrule x@(Fr{})  = "Fr "
   cptrule x@(Ru{}) 
       | rrsrt x==Implication = printadl (Just fSpec) 0 (rrant x) ++ " |- " ++ (printadl (Just fSpec) 0$rrcon x)
       | rrsrt x==Equivalence = printadl (Just fSpec) 0 (rrant x) ++ " = " ++ (printadl (Just fSpec) 0$rrcon x)
       | rrsrt x==Truth = printadl (Just fSpec) 0 (rrcon x)
       | otherwise = []
   cpttype x = name(source x)++"*"++(name$target x)
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
   


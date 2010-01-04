--running PHP in IIS on the php.exe of XAMPP requires setting "cgi.force_redirect = 0" in the php.ini
--in IIS you can enable windows authentication
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
import Classes.Morphical
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
  |ATMorphisms
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
   [ATable ATAtom "atom" ["i","user","script","display"] 
   ,ATable ATConcept "concept" ["i","user","script","display"] 
   ,ATable ATContains "contains" ["relation","pair"] 
   ,ATable ATContainsConcept "containsconcept" ["concept","atom"] 
   ,ATable ATExplanation "explanation" ["i","user","script","display"] 
  -- ,ATable ATExpression "expression" ["i","source","target"] 
   ,ATable ATHomoRule "homogeneousrule" ["i","property","on","type","explanation","user","script","display"] 
   ,ATable ATIsa "isarelation" ["i","specific","general","user","script","display"] 
   ,ATable ATPicture "picture" ["i","thepicture","user","script","display"] 
   ,ATable ATMorphisms "morphisms" ["userrule","relation"] 
   ,ATable ATMultRule "multiplicityrule" ["i","property","on","type","explanation","user","script","display"] 
   ,ATable ATPair "pair" ["i","user","script","display"] 
   ,ATable ATProp "prop" ["i","user","script","display"] 
   ,ATable ATRelation "relation" ["i","user","script","display"]
   ,ATable ATRelVar "relvar" ["relation","type"]
   ,ATable ATRule "rule" ["i","type","explanation","user","script","display"] 
   ,ATable ATType "type" ["i","source","target","user","script","display"] 
   ,ATable ATUserRule "userrule" ["i","type","explanation","user","script","display"] 
   ,ATable ATViolRule "violates" ["violation","rule"]
   ,ATable ATViolHomoRule "violateshomogeneousrule" ["violation","homogeneousrule"]
   ,ATable ATViolMultRule "violatesmultiplicityrule" ["violation","multiplicityrule"]
   ,ATable ATViolUserRule "violatesviolation" ["violation","userrule"]
   ,ATable ATViolation "violation" ["i","user","script","display"]
   ]
iscpttable tbl = elem tbl [ATAtom,ATConcept,ATExplanation,ATHomoRule,ATIsa,ATPicture,ATMultRule,ATPair,ATProp,ATRelation,ATRule,ATType,ATUserRule,ATViolation]

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
    --TODO -> DELETE only deletes concept tables, but there are no foreign keys, so relation table content will not be removed. Duplicates are allowed in those, so this gives no errors. Meterkast.adl does not clean up at all, because new compiles get new script names.
    (if islocalcompile then runMany conn ["DELETE FROM "++tablename x| x<-tables]
        else runMany conn ["DELETE FROM "++tablename x++" WHERE user='"++user++"' AND script='"++script++"'" |x<-tables, iscpttable$tableid x] )
    --insert population of this ADL script of this user
    insertpops conn fSpec flags tables pictlinks
    --end connection
    commit conn
    disconnect conn
 >> createDirectoryIfMissing True fpath
 >> foldr (>>) (verboseLn flags "All pictures written..") dots
    where
    script = adlFileName flags
    user = takeWhile (/='.') (userAtlas flags)
    islocalcompile =  dropWhile (/='.') (userAtlas flags)==".local"
    pictlinks = [".\\img\\"++user++"\\"++script++"\\"++ (name fSpec) ++ ".png"| p<-patterns fSpec]
                --TODO -> patterns [".\\img\\"++ (remSpaces$name p) ++ ".png"| p<-patterns fSpec]
    fpath = combine (dirAtlas flags) ("img/"++user++"/"++script++"/")
    outputFile fnm = combine fpath fnm
    dots = [makeGraphic (name fSpec)$ toDot fSpec flags $ 
             if length(patterns fSpec)==0 then error "There is no pattern to fold"
             else foldr (union) (head$patterns fSpec) (tail$patterns fSpec)]
           --TODO -> patterns [makeGraphic (remSpaces (name p)) $ toDot fSpec flags p 
           --                 | p<-patterns fSpec, (not.null) (concs p)] 
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

--TODO -> SIGNALs, Only Ru{} rules are considered
type PictureLinks = [String]
insertpops :: (IConnection conn) => conn -> Fspc -> Options -> [ATable] -> PictureLinks -> IO Integer
insertpops _ _ _ [] _ = return 1
insertpops conn fSpec flags (tbl:tbls) pics = 
   do stmt<- prepare conn$"INSERT INTO "++tablename tbl++" VALUES ("++placeholders(columns tbl)++")"
      executeMany stmt (pop$tableid tbl)
      insertpops conn fSpec flags tbls pics
   where
   script = adlFileName flags
   user = takeWhile (/='.') (userAtlas flags)
   islocalcompile =  dropWhile (/='.') (userAtlas flags)==".local"
   qualify = (++)$"("++user ++ "." ++ script ++ ")"
   toUserctx :: [String]->ATableId->[String]
   toUserctx [] _ = []
   toUserctx xs t = map qualify xs ++ (if iscpttable t then [user,script,head xs] else [])
   pop x = [map toSql$toUserctx ys x|ys<-rd (pop' x)]
   pop':: ATableId -> [[String]]
   pop' ATAtom = [[x]|(_,x)<-cptsets]
   pop' ATConcept = [[name x]|x<-cpts]
   pop' ATContains = [[name x,show y]| x<-vrels fSpec, y<-contents x]
   pop' ATContainsConcept = [[x,y]|(x,y)<-cptsets]
   pop' ATExplanation = [[explainRule flags x]|x<-atlasrules]
 --  pop' ATExpression = [] --TODO - generalisation must be fixed first in -p of atlas
   pop' ATHomoRule = [(\(Just (p,d))->[cptrule x,show p,name d,cpttype x,explainRule flags x])$rrdcl x |x@Ru{}<-homorules]
   pop' ATIsa = [[show x,show(genspc x), show(gengen x)]|p<-patterns fSpec, x<-ptgns p]
   pop' ATPicture = [[x,x]|x<-pics]
   pop' ATMorphisms = [[cptrule x, name y]|x<-userrules, y<-mors x]
   pop' ATMultRule = [(\(Just (p,d))->[cptrule x,show p,name d,cpttype x,explainRule flags x])$rrdcl x |x@Ru{}<-multrules]
   pop' ATPair = [[show y]| x<-vrels fSpec, y<-contents x]
   pop' ATProp = [[show x]|x<-[Uni,Tot,Inj,Sur,Rfx,Sym,Asy,Trn]]
   pop' ATRelation = ["I"]:["V"]:[[name x]|x<-vrels fSpec]
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
   cptrule x | isSignal x =  "SIGNAL: " ++ (cptrule$ x{r_sgl=False})
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
   userrules = [x|x@Ru{}<-vrules fSpec, rrdcl x==Nothing, not (isIsaRule x)]
      where 
      isIsaRule x = rrsrt x==Implication && (isI$rrant x) && (isI$rrcon x)
      isI (Tm (I{})) = True
      isI _ = False
   multrules = [x|x@Ru{}<-vrules fSpec, isMultRule (rrdcl x) ]
      where 
      isMultRule (Just (p,_)) = elem p [Uni,Tot,Inj,Sur]
      isMultRule Nothing = False      
   homorules = [x|x@Ru{}<-vrules fSpec, isHomoRule (rrdcl x) ]
      where 
      isHomoRule (Just (p,_)) = elem p [Rfx,Sym,Asy,Trn] 
      isHomoRule Nothing = False

placeholders :: [a] -> String
placeholders [] = []
placeholders (_:[]) = "?"
placeholders (_:xs) = "?," ++ placeholders xs
   


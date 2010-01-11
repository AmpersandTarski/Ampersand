{-# OPTIONS_GHC -Wall #-}  
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
import List(sort)
import Classes.ViewPoint 
------
import Classes.Graphics
import System (system, ExitCode(ExitSuccess,ExitFailure))
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
  |ATPattern
  |ATProp
  |ATRelation
  |ATRelVar
  |ATRule
  |ATService
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
   ,ATable ATHomoRule "homogeneousrule" ["i","property","on","type","explanation","pattern","user","script","display"] 
   ,ATable ATIsa "isarelation" ["i","specific","general","pattern","user","script","display"] 
   ,ATable ATPicture "picture" ["i","user","script","display"] 
   ,ATable ATMorphisms "morphisms" ["userrule","relation"] 
   ,ATable ATMultRule "multiplicityrule" ["i","property","on","type","explanation","pattern","user","script","display"] 
   ,ATable ATPair "pair" ["i","user","script","display"] 
   ,ATable ATPattern "pattern" ["i","picture","user","script","display"] 
   ,ATable ATProp "prop" ["i","user","script","display"] 
   ,ATable ATRelation "relation" ["i","pattern","user","script","display"]
   ,ATable ATRelVar "relvar" ["relation","type"]
   ,ATable ATRule "rule" ["i","type","explanation","pattern","user","script","display"] 
   ,ATable ATService "service" ["i","picture","user","script","display"] 
   ,ATable ATType "type" ["i","source","target","user","script","display"] 
   ,ATable ATUserRule "userrule" ["i","type","explanation","picture","pattern","next","previous","user","script","display"] 
   ,ATable ATViolRule "violates" ["violation","rule"]
   ,ATable ATViolHomoRule "violateshomogeneousrule" ["violation","homogeneousrule"]
   ,ATable ATViolMultRule "violatesmultiplicityrule" ["violation","multiplicityrule"]
   ,ATable ATViolUserRule "violatesviolation" ["violation","userrule"]
   ,ATable ATViolation "violation" ["i","user","script","display"]
   ]
iscpttable :: ATableId -> Bool
iscpttable tbl = elem tbl [tableid t|t<-tables, head(columns t)=="i"]

data PicLinkType = PicFS | PicPat String | PicRule String deriving (Eq)

--Atlas requires an ODBC data source named "atlas" representing the db of an Atlas.adl prototype
--hdbc and hdbc-odbc must be installed (from hackage)
fillAtlas :: Fspc -> Options -> IO()
fillAtlas fSpec flags = 
 if not(graphics flags) then do 
    verboseLn flags "Populating atlas for ..."
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
 else do
    verboseLn flags "Generating pictures for atlas..."
 >> createDirectoryIfMissing True fpath
 >> foldr (>>) (verboseLn flags "All pictures written..") ([fspecdot]++patsdot++userrulesdot)
    where
    script = adlFileName flags
    user = takeWhile (/='.') (userAtlas flags)
    islocalcompile =  dropWhile (/='.') (userAtlas flags)==".local"
    pictlinks = [(PicFS,".\\img\\"++user++"\\"++script++"\\"++ (name fSpec) ++ ".png")]
              ++[(PicPat$name p, ".\\img\\"++user++"\\"++script++"\\"++ (name p) ++ ".png")| p<-patterns fSpec]
              ++[(PicRule$name r, ".\\img\\"++user++"\\"++script++"\\"++ (name r) ++ ".png")| r<-userrules]
    fpath = combine (dirAtlas flags) ("img/"++user++"/"++script++"/")
  --  dots =  fspecdot]
           --TODO -> patterns [makeGraphic (remSpaces (name p)) $ toDot fSpec flags p 
           --                 | p<-patterns fSpec, (not.null) (concs p)]
    fspecdot = makeGraphic (name fSpec)$ toDot fSpec flags $ 
             if length(patterns fSpec)==0 then error "There is no pattern to fold"
             else foldr (union) (head$patterns fSpec) (tail$patterns fSpec)
    patsdot = [makeGraphic (name p)$ toDot fSpec flags p|p<-patterns fSpec]
    userrulesdot = [makeGraphic (name r)$ toDot fSpec flags r|r<-userrules]
    userrules = sort [x|x@Ru{}<-rules fSpec++signals fSpec, rrdcl x==Nothing, not (isIsaRule x), not(r_pat x=="")]
      where 
      isIsaRule x = rrsrt x==Implication && (isI$rrant x) && (isI$rrcon x)
      isI (Tm (I{})) = True
      isI _ = False
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
        outputFile fnm = combine fpath fnm
        dotfile = replaceExtension (outputFile fnm) "dot"
        pngfile = replaceExtension (outputFile fnm) "png"
----------------------------------------------------
runMany :: (IConnection conn) => conn -> [String] -> IO Integer
runMany _ [] = return 1
runMany conn (x:xs)  = 
   do run conn x []
      runMany conn xs

--TODO -> SIGNALs, Only Ru{} rules are considered
type PictureLinks = [(PicLinkType,String)]
insertpops :: (IConnection conn) => conn -> Fspc -> Options -> [ATable] -> PictureLinks -> IO Integer
insertpops _ _ _ [] _ = return 1
insertpops conn fSpec flags (tbl:tbls) pics = 
   do stmt<- prepare conn$"INSERT INTO "++tablename tbl++" VALUES ("++placeholders(columns tbl)++")"
      executeMany stmt (pop$tableid tbl)
      insertpops conn fSpec flags tbls pics
   where
   script = adlFileName flags
   user = takeWhile (/='.') (userAtlas flags)
--   islocalcompile =  dropWhile (/='.') (userAtlas flags)==".local"
   qualify = (++)$"("++user ++ "." ++ script ++ ")"
   toUserctx :: [String]->ATableId->[String]
   toUserctx [] _ = []
   toUserctx xs t = map qualify xs ++ (if iscpttable t then [user,script,head xs] else [])
   pop x = [map toSql$toUserctx ys x|ys<-rd (pop' x)]
   pop':: ATableId -> [[String]]
   pop' ATAtom = [[x]|(_,x)<-cptsets]
   pop' ATConcept = [[name x]|x<-cpts]
   pop' ATContains = [[relpred x,show y]| x<-declarations fSpec,not$isSignal x, y<-contents x]
   pop' ATContainsConcept = [[x,y]|(x,y)<-cptsets]
   pop' ATExplanation = [[explainRule flags x]|x<-atlasrules]
 --  pop' ATExpression = [] --TODO - generalisation must be fixed first in -p of atlas
   pop' ATHomoRule = [(\(Just (p,d))->[cptrule x,show p,relpred d,cpttype x,explainRule flags x,r_pat x])$rrdcl x |x@Ru{}<-homorules]
   pop' ATIsa = [[show x,show(genspc x), show(gengen x),name p]|p<-patterns fSpec, x<-gens p]
   pop' ATPicture = [[x]|(_,x)<-pics]
   pop' ATMorphisms = [[cptrule x, mphpred y]|x<-userrules, y<-mors x]
   pop' ATMultRule = [(\(Just (p,d))->[cptrule x,show p,relpred d,cpttype x,explainRule flags x,r_pat x])$rrdcl x |x@Ru{}<-multrules]
   pop' ATPair = [[show y]| x<-declarations fSpec,not$isSignal x, y<-contents x]
   pop' ATPattern = [[name x,pic]| x<-patterns fSpec,(PicPat pn,pic)<-pics, pn==name x]
   pop' ATProp = [[show x]|x<-[Uni,Tot,Inj,Sur,Rfx,Sym,Asy,Trn]]
   pop' ATRelation = ["I",""]:["V",""]:[[relpred x,name p]|p<-patterns fSpec, x<-declarations p,not$isSignal x] --REMARK -> decls from pat instead of fSpec!
   pop' ATRelVar = [[relpred x,cpttype x]|x<-declarations fSpec,not$isSignal x]
   pop' ATRule = [[cptrule x,cpttype x,explainRule flags x,r_pat x]|x<-atlasrules]
   pop' ATService = [[name fSpec,x]|(PicFS,x)<-pics]
   pop' ATType = [t x|x<-declarations fSpec,not$isSignal x] ++ [t x|x<-atlasrules]
        where t x = [cpttype x, name$source x, name$target x]
   pop' ATUserRule = [[cptrule x,cpttype x,explainRule flags x,pic,r_pat x,cptrule$prevrule x userrules,cptrule$nextrule x userrules]|x<-userrules,(PicRule rn,pic)<-pics, rn==name x]
   --convert pair to violation message
   --There is overhead in the violates* tables, but that does not matter for the result. Fix SQL and generalisation instead.
   --then there will be only one table "violates" for all rules.
   pop' ATViolRule = [[ y, x] | (x,y)<-identifiedviols]
   pop' ATViolHomoRule = [[ y, x] | (x,y)<-identifiedviols]
   pop' ATViolMultRule = [[ y, x] | (x,y)<-identifiedviols]
   pop' ATViolUserRule = [[ y, x] | (x,y)<-identifiedviols]
   pop' ATViolation = [[ y] | (_,y)<-identifiedviols]
   --------------------------------------------------------
   --picturelink =  "./img/" ++ name fSpec++".png"
   identifiedviols = [(cptrule x,show i++show y) |(i,(x,y))<-zip [1..] (violations fSpec)]
   nextrule r [] = r
   nextrule r rs = if null nxt then head rs else head nxt 
      where nxt = [r'|r'<-rs, runum r'==(runum r)+1]
   prevrule r [] = r
   prevrule r rs = if null prev then last rs else head prev 
      where prev = [r'|r'<-rs, runum r'==(runum r)-1]
   relpred x@(Sgn{}) = name x ++ "::" ++ cpttype x 
   relpred x = name x
   mphpred x@(Mph{}) = relpred (mphdcl x) 
   mphpred x = name x
   cpts = (\(Isa isas cs) -> rd$[c|c@(C{})<-cs]++[c|(c,_)<-isas]++[c|(_,c)<-isas]) (isa fSpec)
   cptsets = [(name c,x)|c@(C{})<-cpts, x<-cptos c]
   cptrule x | isSignal x =  "SIGNAL: " ++ (cptrule$ x{r_sgl=False})
             | rrsrt x==Implication = showADLcode fSpec (rrant x) ++ " |- " ++ showADLcode fSpec (rrcon x)
             | rrsrt x==Equivalence = showADLcode fSpec (rrant x) ++ " = " ++ showADLcode fSpec (rrcon x)
             | rrsrt x==Truth = showADLcode fSpec (rrcon x)
             | otherwise = []
   cpttype x = name(source x)++"*"++(name$target x)
   --DESCR -> userrules are user-defined rules, 
   --         multrules are rules defined by a multiplicity, 
   --         homorules by a homogeneous property
   --         the rule from an ISA declaration (I[spec] |- I[gen]) is not presented as a rule in the atlas
   --TODO -> key rules (they have been put in pattern "")
   --TODO -> rulefromProp has not been type inferred, p.e. generates I[Anything]. Will there be violations on them in fSpec????? 
   atlasrules = userrules ++ multrules ++ homorules
   userrules = sort [x|x@Ru{}<-rules fSpec++signals fSpec, rrdcl x==Nothing, not (isIsaRule x), not(r_pat x=="")]
      where 
      isIsaRule x = rrsrt x==Implication && (isI$rrant x) && (isI$rrcon x)
      isI (Tm (I{})) = True
      isI _ = False
   multrules = [rulefromProp p d|d<-declarations fSpec, p<-multiplicities d, elem p [Uni,Tot,Inj,Sur]] 
   homorules =  [rulefromProp p d|d<-declarations fSpec, p<-multiplicities d, elem p [Rfx,Sym,Asy,Trn] ]

placeholders :: [a] -> String
placeholders [] = []
placeholders (_:[]) = "?"
placeholders (_:xs) = "?," ++ placeholders xs
   


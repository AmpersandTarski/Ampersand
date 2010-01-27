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
import Auxiliaries(sort)
import Classes.ViewPoint 
import Picture
import PredLogic (applyM)
------
import Classes.Graphics
--import System (system, ExitCode(ExitSuccess,ExitFailure))
--import System.FilePath(replaceExtension,(</>))
--import System.Directory(createDirectoryIfMissing)

data ATable = ATable {tableid::ATableId, tablename::String, columns::[String]} deriving (Show)
data ATableId = 
   ATAtom
  |ATConcept
  |ATContains
  |ATContainsConcept
  |ATContainsExpr
  |ATContainsSignal
  |ATExplanation
  |ATSubExpression
  |ATHomoRule
  |ATIsa
  |ATPicture
  |ATMorphisms
  |ATMorphismsSignal
  |ATMultRule
  |ATPair
  |ATPattern
  |ATPragmaExample
  |ATProp
  |ATRelation
  |ATRelVar
  |ATRule
  |ATService
  |ATSignal
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
   ,ATable ATConcept "concept" ["i","description","user","script","display"] 
   ,ATable ATContains "contains" ["relation","pair"] 
   ,ATable ATContainsConcept "containsconcept" ["concept","atom"] 
   ,ATable ATContainsExpr "containssubexpression" ["subexpression","pair"] 
   ,ATable ATContainsSignal "containssignal" ["signal","pair"] 
   ,ATable ATExplanation "explanation" ["i","user","script","display"] 
   ,ATable ATSubExpression "subexpression" ["i","subexpressionof","user","script","display"] 
   ,ATable ATHomoRule "homogeneousrule" ["i","property","on","type","explanation","pattern","user","script","display"] 
   ,ATable ATIsa "isarelation" ["i","specific","general","pattern","user","script","display"] 
   ,ATable ATPicture "picture" ["i","user","script","display"] 
   ,ATable ATMorphisms "morphisms" ["userrule","relation"] 
   ,ATable ATMorphismsSignal "morphismssignal" ["signal","relation"] 
   ,ATable ATMultRule "multiplicityrule" ["i","property","on","type","explanation","pattern","user","script","display"] 
   ,ATable ATPair "pair" ["i","user","script","display"] 
   ,ATable ATPattern "pattern" ["i","picture","user","script","display"] 
   ,ATable ATPragmaExample "pragmaexample" ["i","user","script","display"] 
   ,ATable ATProp "prop" ["i","user","script","display"] 
   ,ATable ATRelation "relation" ["i","description","example","pattern","user","script","display"]
   ,ATable ATRelVar "relvar" ["relation","type"]
   ,ATable ATRule "rule" ["i","type","explanation","pattern","user","script","display"] 
   ,ATable ATService "service" ["i","picture","user","script","display"] 
   ,ATable ATSignal "signal" ["i","type","explanation","pattern","next","previous","user","script","display"] 
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

--data PicLinkType = PicFS | PicPat String | PicRule String deriving (Eq)

--Atlas requires an ODBC data source named "atlas" representing the db of an Atlas.adl prototype
--hdbc and hdbc-odbc must be installed (from hackage)
fillAtlas :: Fspc -> Options -> IO()
fillAtlas fSpec flags = 
 if not(graphics flags) then do initDatabase flags fSpec
 else do verboseLn flags "Generating pictures for atlas..."
         sequence_ [writePicture flags pict | pict <- picturesForAtlas flags fSpec]
 --createDirectoryIfMissing True fpath
 -- >> foldr (>>) (verboseLn flags "All pictures written..") ([fspecdot]++patsdot++userrulesdot)
--    where
--    script = adlFileName flags
--    user = takeWhile (/='.') (userAtlas flags)
--    islocalcompile =  dropWhile (/='.') (userAtlas flags)==".local"
 --   pictlinks = [(PicFS, relImgPath </> (spacesToUnderscores (name fSpec)) ++ ".png")]
 --             ++[(PicPat$name p, relImgPath </> (spacesToUnderscores(name p)) ++ ".png")| p<-patterns fSpec]
 --             ++[(PicRule$name r, relImgPath </> (spacesToUnderscores(name r) ++ ".png"))| r<-userrules]
 --   fpath = (dirAtlas flags) </> relImgPath
 --   relImgPath = "img" </> user </> (baseName flags)
    
  --  dots =  fspecdot]
           --TODO -> patterns [makeGraphic (remSpaces (name p)) $ toDot fSpec flags p 
           --                 | p<-patterns fSpec, (not.null) (concs p)]
    


--    fspecdot = makeGraphic (name fSpec)$ toDot fSpec flags $ 
--             if length(patterns fSpec)==0 then error "There is no pattern to fold"
--             else foldr (union) (head$patterns fSpec) (tail$patterns fSpec)
--    patsdot = [makeGraphic (spacesToUnderscores(name p))$ toDot fSpec flags p|p<-patterns fSpec]
--    userrulesdot = [makeGraphic (spacesToUnderscores(name r))$ toDot fSpec flags r|r<-userrules]
--    userrules = sort [x|x@Ru{}<-rules fSpec++signals fSpec, rrdcl x==Nothing, not (isIsaRule x), not(r_pat x=="")]
--      where 
--      isIsaRule x = rrsrt x==Implication && (isI$rrant x) && (isI$rrcon x)
--      isI (Tm (I{})) = True
--      isI _ = False
--    makeGraphic fnm dot
--      = do 
--        succes <- runGraphvizCommand Neato dot Canon dotfile
--        if succes
--           then do
--             result1 <- system ("neato -Tpng "++dotfile++ " -o "++pngfile)
--             case result1 of 
--                ExitSuccess   -> putStrLn (" "++pngfile++" created.")
--                ExitFailure x -> putStrLn ("Failure: " ++ show x)
--             result2 <- system ("neato -Tcmapx "++dotfile++ " -o "++mapfile)


-- Van Han aan Gerard: 
-- Hieronder vind je de code om het plaatje als imagemap te genereren. Dat is nu dus geregeld.
-- Vervolgens moet je er nog voor zorgen dat de imagemap op de juiste manier wordt gebruikt. Daarvoor
-- moet je de gegenereerde php oppoetsen. Ik heb daar geen verstand van. Bas overigens wel, maar jij wellicht ook. 
-- enige informatie hierover staat op http://www.wellho.net/resources/ex.php4?item=h112/imap.php
-- Het enige dat vervolgens geregeld moet worden is dat in DOT de nodes /edges moeten worden voorzien van de juiste URL attribuut. 
-- Dat kan voor jou niet moeilijk zijn. Als je dat regelt, dan vallen alle stukjes op z'n plaats. Succ6! 
--GMI:
--Dank Han!
--TODO -> "include (str_replace('png','map', $v0));" op een nette manier laten genereren op juiste plekken in .php zonder gebruik van str_replace natuurlijk.
--TODO -> .map files zijn nog vrij leeg (geen areas, slechts header/footer)
--             case result2 of 
--                ExitSuccess   -> putStrLn (" "++mapfile++" created.")
--                ExitFailure x -> putStrLn ("Failure: " ++ show x)
--           else putStrLn ("Failure: could not create " ++ dotfile) 
--        where
--        outputFile = fpath </> fnm
--        dotfile = replaceExtension outputFile "dot"
--        pngfile = replaceExtension outputFile "png"
--        mapfile = replaceExtension outputFile "map"
initDatabase :: Options -> Fspc -> IO() 
initDatabase flags fSpec = 
                 do verboseLn flags "Populating atlas for ..."
                   --connect through ODBC data source "atlas"
                    conn<-connectODBC "DSN=atlas"
                    --TODO handle connection errors
                    --TODO check if actual MySql tables of atlas correspond to function tables
                    --delete all existing content of this ADL script of this user
                    --create the pictures in a folder for this user
                    --TODO -> DELETE only deletes concept tables, but there are no foreign keys, so relation table content will not be removed. Duplicates are allowed in those, so this gives no errors. Meterkast.adl does not clean up at all, because new compiles get new script names.
                    (if islocalcompile 
                        then runMany conn ["DELETE FROM "++tablename x| x<-tables]
                        else runMany conn ["DELETE FROM "++tablename x++
                                           " WHERE user='"++user++"' AND script='"++script++"'" 
                                                                      |x<-tables, iscpttable$tableid x] )
                    --insert population of this ADL script of this user
                    insertpops conn fSpec flags tables pictures
                    --end connection
                    commit conn
                    disconnect conn
   where
    script = adlFileName flags
    user = takeWhile (/='.') (userAtlas flags)
    islocalcompile =  dropWhile (/='.') (userAtlas flags)==".local"
    pictures = picturesForAtlas flags fSpec

picturesForAtlas :: Options -> Fspc -> [Picture]
picturesForAtlas flags fSpec
   = [makePicture flags fSpec p | p <- patterns fSpec] ++
     [makePicture flags fSpec userRule | 
          userRule <- sort [x|x@Ru{}<-rules fSpec++signals fSpec
                                     , rrdcl x==Nothing
                                     , not (isIsaRule x)
                                     , not (r_pat x=="")
                           ]
     ]++
     [makePicture flags fSpec cpt | cpt <- (concs fSpec)]
-- HJO @ Gerard: Hier kan je nu als het goed is héél gemakkelijk plaatjes aan toevoegen... (zolang ze maar Dottable zijn)
   where 
     isIsaRule = isaRule
     -- HJO @ Gerard: WAAROM? had je zelf een functie gemaakt om te bepalen of een regel isarule is? Die bestaat gewoon in adl.rule ....
--      isIsaRule x = rrsrt x==Implication && (isI$rrant x) && (isI$rrcon x)
--      isI (Tm (I{})) = True
--      isI _ = False


----------------------------------------------------
runMany :: (IConnection conn) => conn -> [String] -> IO Integer
runMany _ [] = return 1
runMany conn (x:xs)  = 
   do run conn x []
      runMany conn xs

--TODO -> SIGNALs, Only Ru{} rules are considered
--type PictureLinks = [(PicLinkType,String)]
insertpops :: (IConnection conn) => conn -> Fspc -> Options -> [ATable] -> [Picture] -> IO Integer
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
   pop' ATConcept = [[name x,description x]|x<-cpts]
   pop' ATContains = [[relpred x,show y]| x<-declarations fSpec,decusr x, y<-contents x]
   pop' ATContainsConcept = [[x,y]|(x,y)<-cptsets] 
   pop' ATContainsExpr = [[cptexpr x,show y]| vr<-violateduserrules, x<-subexprs vr, y<-contents x]
   pop' ATContainsSignal = [[cptrule x,show y]| x<-signalrules, y<-contents (Cp$ruleexpr x)]
   pop' ATExplanation = [[explainRule flags x]|x<-atlasrules] ++ [[description x]|x<-cpts] ++ [[expl x]|p<-patterns fSpec, x<-declarations p]
   pop' ATSubExpression = [[cptexpr y,cptrule x]|x<-violateduserrules, y<-subexprs x] 
   pop' ATHomoRule = [(\(Just (p,d))->[cptrule x,show p,relpred d,cpttype x,explainRule flags x,r_pat x])$rrdcl x |x@Ru{}<-homorules]
   pop' ATIsa = [[show x,show(genspc x), show(gengen x),name p]|p<-patterns fSpec, x<-gens p]
   pop' ATPicture = [[show(imgURL pic)]|pic<-pics]
   pop' ATMorphisms = [[cptrule x, mphpred y]|x<-userrules, y<-mors x]
   pop' ATMorphismsSignal = [[cptrule x, mphpred y]|x<-signalrules, y<-mors x]
   pop' ATMultRule = [(\(Just (p,d))->[cptrule x,show p,relpred d,cpttype x,explainRule flags x,r_pat x])$rrdcl x |x@Ru{}<-multrls]
   pop' ATPair = [[show y]| x<-declarations fSpec,decusr x, y<-contents x]
   pop' ATPattern = [[name x,show(imgURL pic)]| x<-patterns fSpec,pic<-pics, origName pic==name x, pType pic == PTPattern ]
   pop' ATPragmaExample = [[example x]|p<-patterns fSpec, x<-declarations p,decusr x] 
   pop' ATProp = [[show x]|x<-[Uni,Tot,Inj,Sur,Rfx,Sym,Asy,Trn]]
   pop' ATRelation = [[relpred x,expl x,example x,name p]|p<-patterns fSpec, x<-declarations p,decusr x] --REMARK -> decls from pat instead of fSpec!
                   ++ [["I","The identity relation.","x is related to x",""],["V","The universal relation.","x is related to y",""]]
   pop' ATRelVar = [[relpred x,cpttype x]|x<-declarations fSpec,decusr x]
   pop' ATRule = [[cptrule x,cpttype x,explainRule flags x,r_pat x]|x<-atlasrules]
   pop' ATService = [[name fSpec,show(imgURL pic)]|pic<-pics, origName pic==name fSpec, pType pic == PTFservice]
   pop' ATSignal = [[cptrule x,cpttype x,explainRule flags x,r_pat x,cptrule$nextrule x signalrules,cptrule$prevrule x signalrules]|x<-signalrules]
   pop' ATType = [t x|x<-declarations fSpec,decusr x] ++ [t x|x<-atlasrules]
        where t x = [cpttype x, name$source x, name$target x]
   pop' ATUserRule = [[cptrule x,cpttype x,explainRule flags x,show(imgURL pic),r_pat x,cptrule$nextrule x userrules,cptrule$prevrule x userrules]|x<-userrules,pic<-pics, origName pic==name x, pType pic == PTRule]
   pop' ATViolRule = [[ y, x] | (x,y)<-identifiedviols]
   pop' ATViolHomoRule = [[ y, x] | (x,y)<-identifiedviols, elem x (map cptrule homorules)]
   pop' ATViolMultRule = [[ y, x] | (x,y)<-identifiedviols, elem x (map cptrule multrls)]
   pop' ATViolUserRule = [[ y, x] | (x,y)<-identifiedviols, elem x (map cptrule userrules)]
   pop' ATViolation = [[ y] | (_,y)<-identifiedviols]
   --------------------------------------------------------
   --picturelink =  "./img/" ++ name fSpec++".png"
   identifiedviols = [(cptrule x,"violation"++show i++" "++show y) |(i,(x,y))<-zip naturals (violations fSpec)]
     where naturals :: [Integer]
           naturals = [1..]
   violateduserrules = [r|r<-userrules, elem (cptrule r) [x|(x,_)<-identifiedviols]]
   subexprs x | rrsrt x==Implication = [rrant x,rrcon x]
              | rrsrt x==Equivalence = [rrant x,rrcon x]
              | rrsrt x==Truth = [rrcon x]
              | otherwise = []
   ruleexpr x | rrsrt x==Implication = Fu [Cp(rrant x), rrcon x]
              | rrsrt x==Equivalence = Fi [Fu [Cp(rrant x), rrcon x], Fu [Cp(rrcon x), rrant x]]
              | rrsrt x==Truth = rrcon x
              | otherwise = v (NOthing,NOthing) 
   nextrule r [] = r
   nextrule r (_:[]) = r
   nextrule r (r':r'':rs) | runum r'==runum r = r'' 
                      | otherwise = nextrule r (r'':rs)
   --prevrule _ rs = error$show [runum r|r<-rs]
   prevrule r [] = r
   prevrule r (_:[]) = r
   prevrule r (r':r'':rs) | runum r''==runum r = r' 
                          | otherwise = prevrule r (r'':rs)
   relpred x@(Sgn{}) = name x ++ "::" ++ cpttype x 
   relpred x = name x
   mphpred x@(Mph{}) = relpred (mphdcl x) 
   mphpred x = name x
   example d = if null (contents d) then  applyM d "x" "y" 
               else applyM d (fst$head$contents d) (snd$head$contents d)
   expl d = if null(explain flags d) then "There is no description for this relation." else explain flags d
   description::Concept->String
   description c = if null ds then "There is no description for this concept." else head ds
       where ds = [cddef cd|cd<-conceptDefs fSpec, name cd==name c]
   cptexpr = showADLcode fSpec 
   cpts = (\(Isa isas cs) -> rd$[c|c@(C{})<-cs]++[c|(c,_)<-isas]++[c|(_,c)<-isas]) (isa fSpec)
   cptsets = [(name c,x)|c@(C{})<-cpts, x<-cptos c]
   cptrule x | isSignal x =  "SIGNAL: " ++ (cptrule$ x{r_sgl=False})
             | rrsrt x==Implication = cptexpr (rrant x) ++ " |- " ++ cptexpr (rrcon x)
             | rrsrt x==Equivalence = cptexpr (rrant x) ++ " = " ++ cptexpr (rrcon x)
             | rrsrt x==Truth = cptexpr (rrcon x)
             | otherwise = []
   cpttype x = name(source x)++"*"++(name$target x)
   --DESCR -> userrules are user-defined rules, 
   --         multrls are rules defined by a multiplicity, 
   --         homorules by a homogeneous property
   --         the rule from an ISA declaration (I[spec] |- I[gen]) is not presented as a rule in the atlas
   --TODO -> key rules (they have been put in pattern "")
   --TODO -> rulefromProp has not been type inferred, p.e. generates I[Anything]. Will there be violations on them in fSpec????? 
   atlasrules = userrules ++ multrls ++ homorules ++ signalrules
   userrules = sort [x|x@Ru{}<-rules fSpec, rrdcl x==Nothing, not (isIsaRule x), not(r_pat x=="")]
      where 
      isIsaRule x = rrsrt x==Implication && (isI$rrant x) && (isI$rrcon x)
      isI (Tm (I{})) = True
      isI _ = False
   signalrules =  sort [x|x<-signals fSpec, not(r_pat x=="")]
   multrls = [rulefromProp p d |d<-declarations fSpec, p<-multiplicities d, elem p [Uni,Tot,Inj,Sur]] 
   homorules =  [rulefromProp p d|d<-declarations fSpec, p<-multiplicities d, elem p [Rfx,Sym,Asy,Trn] ]

placeholders :: [a] -> String
placeholders [] = []
placeholders (_:[]) = "?"
placeholders (_:xs) = "?," ++ placeholders xs
   


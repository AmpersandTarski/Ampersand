{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad
import Data.List
import System.FilePath        (combine,dropFileName,takeBaseName)
import System.Directory       (getDirectoryContents)
import Prelude hiding (putStr,readFile,writeFile)
import DatabaseDesign.Ampersand_Prototype.ObjBinGen    (phpObjInterfaces)
import DatabaseDesign.Ampersand_Prototype.Apps         (picturesForAtlas)
import DatabaseDesign.Ampersand_Prototype.Apps.Atlas   (atlas2context)
import DatabaseDesign.Ampersand_Prototype.CoreImporter
import DatabaseDesign.Ampersand_Prototype.Version
import DatabaseDesign.Ampersand_Prototype.Apps.ADL1Importable

fatal :: Int -> String -> a
fatal = fatalMsg "Main"

main :: IO ()
main
 = do opts <- getOptions
      if showVersion opts || showHelp opts
       then mapM_ putStr (helpNVersionTexts prototypeVersionStr opts)
       else do (cx,err) <- parseAndTypeCheck opts
               if nocxe err 
                 then let fspc = makeFspec opts cx in
                      do generateProtoStuff opts fspc
                 else putStr (show err) 
  where
  parseAndTypeCheck :: Options -> IO(A_Context,CtxError) 
  parseAndTypeCheck opts 
   = let scriptName = fileName opts
         fn = importfile opts
         thepCtx (Right pCtx) = pCtx
         thepCtx (Left [])  = fatal 34 "There should be parse errors."
         thepCtx (Left err) = error ("Parsing interrupted due to parse error:\n"++show(head err))
     in
     do scriptText <- readFile scriptName
        ePCtxErr <- parseCtxM_ scriptText opts scriptName
        pPops <- if null fn then return [] else
                 do popsText <- readFile fn
                    case importformat opts of
                       Adl1PopFormat -> parsePopsM_ popsText opts fn
                       Adl1Format -> do verbose opts ("Importing ADL1 file "++fn++"... ")
                                        cx <- parseCtxM_ popsText opts fn
                                        if nocxe (snd(typeCheck (thepCtx cx) [])) 
                                         then let (atlas,_) = typeCheck (thepCtx ePCtxErr) [] -- the atlas without the import
                                                  fspec = makeFspec opts (fst(typeCheck (thepCtx cx) [])) -- the fspec of the adl file to import as a pop of atlas.adl
                                                  fnnxt = name fspec ++ "'" -- a name for a not yet existing next version
                                                  fdir = let d=dropFileName fn in if null d then "." else d
                                                  usr= namespace opts
                                                  getr r = if length r==1 then P_Rel {rel_nm = relnm (head r), rel_pos = relpos (head r)} else error "import error: no or multiple declarations for relvar"
                                                  impctx = [makeRelation d |d<-declarations atlas,name d=="loadcontext"]
                                                  impfil = [makeRelation d |d<-declarations atlas,name d=="loadedfile"]
                                                  impupl = [makeRelation d |d<-declarations atlas,name d=="newcontext"]
                                                  usrfil = [makeRelation d |d<-declarations atlas,name d=="fileof"]
                                                  --funrld = [makeRelation d |d<-declarations atlas,name d=="reload"]
                                                  funfsp = [makeRelation d |d<-declarations atlas,name d=="funcspec"]
                                                  funrep = [makeRelation d |d<-declarations atlas,name d=="report"]
                                                  funadl = [makeRelation d |d<-declarations atlas,name d=="showadl"]
                                                  loadcontext r 
                                                   = [P_Popu{ p_popm=getr r, p_type=[], p_popps=[mkPair fn (name fspec),mkPair fnnxt fnnxt]}]
                                                  loadedfile r
                                                   = [P_Popu{ p_popm=getr r, p_type=[], p_popps=[mkPair usr fn]         } | not (null usr)]
                                                  -- uploadfile r        = [P_Popu{ p_popm=getr r, p_type=[], p_popps=[mkPair usr "browse"]   } | not (null usr)]
                                                  --TODO -> the user has more files, how do I get them in this population
                                                  fileof r myfiles
                                                   = [P_Popu{ p_popm=getr r, p_type=[], p_popps=[mkPair (combine fdir f) usr | f<-myfiles, not (null usr)] }]
                                                  contextfunction r x
                                                   = [P_Popu{ p_popm=getr r, p_type=[], p_popps=[mkPair (name fspec) x] }]
                                              in
                                              do verbose opts "writing pictures for atlas... "
                                                 sequence_ [writePicture opts pict | pict <- picturesForAtlas opts fspec]
                                                 verbose opts "pictures for atlas written... "
                                                 myfiles <- getDirectoryContents fdir >>= return . filter (`notElem` [".", ".."])
                                                 verboseLn opts "Generating pictures for atlas..."
                                                 sequence_ [writePicture opts pict | pict <- picturesForAtlas opts fspec]
                                                 return (makeADL1Populations (declarations atlas) [fspec]
                                                       ++makeADL1Populations (declarations atlas) (picturesForAtlas opts fspec)
                                                       ++loadcontext impctx
                                                       ++loadedfile impfil
                                                       ++contextfunction impupl "new context"
                                                       ++fileof usrfil myfiles
                                                       -- ++ contextfunction funrld (name fspec)
                                                       ++ contextfunction funfsp (takeBaseName fn ++ ".pdf")
                                                       ++ contextfunction funrep (name fspec)
                                                       ++ contextfunction funadl fnnxt
                                                        )
                                         else error (show (snd(typeCheck (thepCtx cx) [])))
        verboseLn opts "Type checking..."
        return (typeCheck (thepCtx ePCtxErr) pPops)
  {-              
parseFilePrototype :: Options -> ParserVersion -> IO(A_Context)
parseFilePrototype opts pv 
      = let fnFull = fileName opts in
        do verbose opts ("Parsing("++show pv++")... ")
           adlText <- readFile fnFull
           importpops <- parseImportFile adlText pv fnFull opts 
           parsedfile <- parseADL1 adlText (if null(importfile opts) then pv else PV2011) importpops opts fnFull 
           atlasfspec <- makeFspec opts parsedfile           
--           verbose opts (show[showsql(SqlSel2(selectbinary atlasfspec c)) |c<-concs atlasfspec])
  --         verbose opts (show[showsql(SqlSel1(selectvector atlasfspec "xxx" c)) |c<-concs atlasfspec])
    --       verbose opts (show[showsql(SqlSel1(selectvector atlasfspec "xxx" (makeRelation d))) |d<-declarations atlasfspec])
--           verbose opts (show[showsql(SqlSel1(selectdomain atlasfspec (makeRelation d))) |d<-declarations atlasfspec])
  --         verbose opts (show[showsql(SqlSel2(selectbinary atlasfspec ((ERel (makeRelation d))))) |d<-declarations atlasfspec])
       --    verbose opts (show[showsql(SqlSel2(selectbinary atlasfspec (EUni[ERel(makeRelation d),ERel(flp$makeRelation d)]))) |d<-declarations atlasfspec,source d==target d])
--           verbose opts (show[(showsql(SqlSel2(selectbinary atlasfspec r'))
  --                             ,showCode 0 x
    --                           ,show r') |r<-rules atlasfspec,let r'=(conjNF . ECpl . normExpr) r,head(showexpression r)=='I'
      --                                           , let Just x=getCodeFor atlasfspec [] [codeVariableForBinary "v" r']])
           if interfacesG opts then atlas2context atlasfspec opts else return parsedfile

parseImportFile :: String -> ParserVersion -> String -> Options -> IO(P_Populations Concept)  
parseImportFile adlText pv adlfn opts  
 = let fn = importfile opts 
       fnnxt fspec = name fspec ++ "'"
       fdir = let d=dropFileName fn in if null d then "." else d
       usr= namespace opts
       getr r = if length r==1 then head r else error "import error: no or multiple declarations for relvar"
       impctx atlas = [makeRelation d |d<-declarations atlas,name d=="loadcontext"]
       impfil atlas = [makeRelation d |d<-declarations atlas,name d=="loadedfile"]
       impupl atlas = [makeRelation d |d<-declarations atlas,name d=="newcontext"]
       usrfil atlas = [makeRelation d |d<-declarations atlas,name d=="fileof"]
       --funrld atlas = [makeRelation d |d<-declarations atlas,name d=="reload"]
       funfsp atlas = [makeRelation d |d<-declarations atlas,name d=="funcspec"]
       funrep atlas = [makeRelation d |d<-declarations atlas,name d=="report"]
       funadl atlas = [makeRelation d |d<-declarations atlas,name d=="showadl"]
       loadcontext r fspec = [Popu{ p_popm=getr r, p_popps=[mkPair fn (name fspec),mkPair (fnnxt fspec) (fnnxt fspec)]}]
       loadedfile r        = [Popu{ p_popm=getr r, p_popps=[mkPair usr fn]         } | not (null usr)]
      -- uploadfile r        = [Popu{ p_popm=getr r, p_popps=[mkPair usr "browse"]   } | not (null usr)]
       --TODO -> the user has more files, how do I get them in this population
       fileof r myfiles    = [Popu{ p_popm=getr r, p_popps=[mkPair (combine fdir f) usr | f<-myfiles, not (null usr)] }]
       contextfunction fspec r x
                           = [Popu{ p_popm=getr r, p_popps=[mkPair (name fspec) x] }]
   in
   if not(null fn)
   then do verbose opts "Parsing import file... "
           popText <- readFile fn
           case importformat opts of
             Adl1PopFormat -> do verbose opts "Importing ADL1 populations file... "
                                 parseADL1Pop popText fn 
             Adl1Format -> do verbose opts ("Importing ADL1 file "++fn++"... ")
                              cx <- parseADL1 popText pv [] opts fn
                              fspec <- makeFspec opts cx
                              verbose opts "writing pictures for atlas... "
                              sequence_ [writePicture opts pict | pict <- picturesForAtlas opts fspec]
                              verbose opts ("pictures for atlas written... "++show pv)
                              atlas <- parseADL1 adlText PV211 [] opts adlfn
                              myfiles <- getDirectoryContents fdir >>= return . filter (`notElem` [".", ".."])
                              verboseLn opts "Generating pictures for atlas..."
                              sequence_ [writePicture opts pict | pict <- picturesForAtlas opts fspec]
                              return (makeADL1Populations (declarations atlas) [fspec]
                                    ++makeADL1Populations (declarations atlas) (picturesForAtlas opts fspec)
                                    ++loadcontext (impctx atlas) fspec
                                    ++loadedfile (impfil atlas)
                                    ++contextfunction fspec (impupl atlas) "new context"
                                    ++fileof (usrfil atlas) myfiles
                                   -- ++ contextfunction fspec (funrld atlas) (name fspec)
                                    ++ contextfunction fspec (funfsp atlas) (takeBaseName fn ++ ".pdf")
                                    ++ contextfunction fspec (funrep atlas) (name fspec)
                                    ++ contextfunction fspec (funadl atlas) (fnnxt fspec)
                                     )
   else return []
-}
generateProtoStuff :: Options -> Fspc -> IO ()
generateProtoStuff opts fSpec = 
    sequence_ 
       ([ verboseLn     opts "Generating..."]++
        [ doGenProto    (protonm fSpec) opts | genPrototype opts] ++
        --interfacesG opts has a different meaning than in ampersand
        --in prototype it means export the DB of the prototype to .adl file
        [ exportProto  fSpec opts | interfacesG    opts] ++ 
        [ verboseLn opts "\nWARNING: There are rule violations (see above)." | (not . null $ violations fSpec) && (development opts || theme opts==StudentTheme)] ++ 
        [ ruleTest fSpec opts ruleName | Just ruleName <- [testRule opts] ] ++
        [ verboseLn opts "Done."]  -- if there are violations, but we generated anyway (ie. with --dev or --theme=student), issue a warning
       ) 
   where  
   protonm fs = rename fs ("ctx" ++ name fs) --rename to ensure unique name of php page (there can be concept names or plurals of them equal to context name)

exportProto :: Fspc -> Options -> IO ()
exportProto fSpec opts =
 do { cx<-atlas2context fSpec opts
    ; writeFile outputFile $ showADL cx
    ; verboseLn opts $ "Ampersand-script written to " ++ outputFile ++ "."
    }
 where outputFile = combine (dirOutput opts) (outputfile opts)

               
doGenProto :: Fspc -> Options -> IO ()
doGenProto fSpec opts =
 do { verboseLn opts "Checking on rule violations..."
    ; let allViolations = violations fSpec
    ; reportViolations allViolations
    
    ; if (not . null) allViolations && not (development opts) && theme opts/=StudentTheme 
      then putStrLn "\nERROR: No prototype generated because of rule violations." 
      else do { verboseLn opts "Generating prototype..."
              ; phpObjInterfaces fSpec opts  
              ; verboseLn opts $ "Prototype files have been written to " ++ dirPrototype opts ++ "."
              ; if test opts then verboseLn opts $ show (vplugInfos fSpec) else verboseLn opts ""
              }
    }
 where reportViolations []    = verboseLn opts $ "No violations found."
       reportViolations viols = putStrLn $ concat [show p++": "++showADL r++"\n" |(r,p)<-viols]


ruleTest :: Fspc -> Options -> String -> IO ()
ruleTest fSpec _ ruleName =
 do { case [ rule | rule <- grules fSpec ++ vrules fSpec, name rule == ruleName ] of
        [] -> putStrLn $ "\nRule test error: rule "++show ruleName++" not found." 
        (rule:_) -> do { putStrLn $ "\nContents of rule "++show ruleName++ ": "++showADL (rrexp rule)
                       ; putStrLn $ showContents rule
                       ; let ruleComplement = rule { rrexp = ECpl $ EBrk $rrexp rule }
                       ; putStrLn $ "\nViolations of "++show ruleName++" (contents of "++showADL (rrexp ruleComplement)++"):"
                       ; putStrLn $ showContents ruleComplement
                       } 
    }
 where showContents rule = let pairs = [ "("++f++"," ++s++")" | (f,s) <- ruleviolations $ rule { rrexp = ECpl $ rrexp rule }]
                           in  "[" ++ intercalate ", " pairs ++ "]" -- getting the contents is not implemented for rules in Rules.hs, 
                                                                    -- so we get the violations of the complement instead
       
    
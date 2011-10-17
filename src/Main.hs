{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad
import System.FilePath        (combine,dropFileName,takeBaseName)
import System.Directory       (getDirectoryContents)
import Prelude hiding (putStr,readFile,writeFile)
import DatabaseDesign.Ampersand_Prototype.ObjBinGen    (phpObjInterfaces)
import DatabaseDesign.Ampersand_Prototype.Apps         (picturesForAtlas)
import DatabaseDesign.Ampersand_Prototype.CoreImporter
import DatabaseDesign.Ampersand_Prototype.Version
import DatabaseDesign.Ampersand_Prototype.Apps.ADL1Importable

fatal :: Int -> String -> a
fatal = fatalMsg "Main"

main :: IO()
main
 = do flags <- getOptions
      if showVersion flags || showHelp flags
       then mapM_ putStr (helpNVersionTexts ("ProtoVs"++versionNumberPrototype++"ADLvs" ++ versionNumber) flags)
       else do (cx,err) <- parseAndTypeCheck flags
               if nocxe err 
                 then let fspc = makeFspec flags cx in
                      do generateProtoStuff flags fspc
                 else putStr (show err) 
  where
  parseAndTypeCheck :: Options -> IO(A_Context,CtxError) 
  parseAndTypeCheck flags 
   = let scriptName = fileName flags
         fn = importfile flags
         thepCtx (Left pCtx) = pCtx
         thepCtx (Right err) = error (show err)
     in
     do scriptText <- readFile scriptName
        ePCtxErr <- parseCtxM_ scriptText flags scriptName
        pPops <- if null fn then return [] else
                 do popsText <- readFile fn
                    case importformat flags of
                       Adl1PopFormat -> parsePopsM_ popsText flags fn
                       Adl1Format -> do verbose flags ("Importing ADL1 file "++fn++"... ")
                                        cx <- parseCtxM_ popsText flags fn
                                        if nocxe (snd(typeCheck (thepCtx cx) [])) 
                                         then let (atlas,_) = typeCheck (thepCtx ePCtxErr) [] -- the atlas without the import
                                                  fspec = makeFspec flags (fst(typeCheck (thepCtx cx) [])) -- the fspec of the adl file to import as a pop of atlas.adl
                                                  fnnxt = name fspec ++ "'" -- a name for a not yet existing next version
                                                  fdir = let d=dropFileName fn in if null d then "." else d
                                                  usr= namespace flags
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
                                              do verbose flags "writing pictures for atlas... "
                                                 sequence_ [writePicture flags pict | pict <- picturesForAtlas flags fspec]
                                                 verbose flags "pictures for atlas written... "
                                                 myfiles <- getDirectoryContents fdir >>= return . filter (`notElem` [".", ".."])
                                                 verboseLn flags "Generating pictures for atlas..."
                                                 sequence_ [writePicture flags pict | pict <- picturesForAtlas flags fspec]
                                                 return (makeADL1Populations (declarations atlas) [fspec]
                                                       ++makeADL1Populations (declarations atlas) (picturesForAtlas flags fspec)
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
        verboseLn flags "Type checking..."
        return (typeCheck (thepCtx ePCtxErr) pPops)
  {-              
parseFilePrototype :: Options -> ParserVersion -> IO(A_Context)
parseFilePrototype flags pv 
      = let fnFull = fileName flags in
        do verbose flags ("Parsing("++show pv++")... ")
           adlText <- readFile fnFull
           importpops <- parseImportFile adlText pv fnFull flags 
           parsedfile <- parseADL1 adlText (if null(importfile flags) then pv else PV2011) importpops flags fnFull 
           atlasfspec <- makeFspec flags parsedfile           
--           verbose flags (show[showsql(SqlSel2(selectbinary atlasfspec c)) |c<-concs atlasfspec])
  --         verbose flags (show[showsql(SqlSel1(selectvector atlasfspec "xxx" c)) |c<-concs atlasfspec])
    --       verbose flags (show[showsql(SqlSel1(selectvector atlasfspec "xxx" (makeRelation d))) |d<-declarations atlasfspec])
--           verbose flags (show[showsql(SqlSel1(selectdomain atlasfspec (makeRelation d))) |d<-declarations atlasfspec])
  --         verbose flags (show[showsql(SqlSel2(selectbinary atlasfspec ((ERel (makeRelation d))))) |d<-declarations atlasfspec])
       --    verbose flags (show[showsql(SqlSel2(selectbinary atlasfspec (EUni[ERel(makeRelation d),ERel(flp$makeRelation d)]))) |d<-declarations atlasfspec,source d==target d])
--           verbose flags (show[(showsql(SqlSel2(selectbinary atlasfspec r'))
  --                             ,showCode 0 x
    --                           ,show r') |r<-rules atlasfspec,let r'=(conjNF . ECpl . normExpr) r,head(showexpression r)=='I'
      --                                           , let Just x=getCodeFor atlasfspec [] [codeVariableForBinary "v" r']])
           if interfacesG flags then atlas2context atlasfspec flags else return parsedfile

parseImportFile :: String -> ParserVersion -> String -> Options -> IO(P_Populations Concept)  
parseImportFile adlText pv adlfn flags  
 = let fn = importfile flags 
       fnnxt fspec = name fspec ++ "'"
       fdir = let d=dropFileName fn in if null d then "." else d
       usr= namespace flags
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
   then do verbose flags "Parsing import file... "
           popText <- readFile fn
           case importformat flags of
             Adl1PopFormat -> do verbose flags "Importing ADL1 populations file... "
                                 parseADL1Pop popText fn 
             Adl1Format -> do verbose flags ("Importing ADL1 file "++fn++"... ")
                              cx <- parseADL1 popText pv [] flags fn
                              fspec <- makeFspec flags cx
                              verbose flags "writing pictures for atlas... "
                              sequence_ [writePicture flags pict | pict <- picturesForAtlas flags fspec]
                              verbose flags ("pictures for atlas written... "++show pv)
                              atlas <- parseADL1 adlText PV2011 [] flags adlfn
                              myfiles <- getDirectoryContents fdir >>= return . filter (`notElem` [".", ".."])
                              verboseLn flags "Generating pictures for atlas..."
                              sequence_ [writePicture flags pict | pict <- picturesForAtlas flags fspec]
                              return (makeADL1Populations (declarations atlas) [fspec]
                                    ++makeADL1Populations (declarations atlas) (picturesForAtlas flags fspec)
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
generateProtoStuff flags fSpec = 
    sequence_ 
       ([ verboseLn     flags "Generating..."]++
        [ doGenProto    (protonm fSpec) flags | genPrototype flags] ++
        [ interfaceGenProto  fSpec flags | interfacesG    flags] ++
        [ verbose flags "Done."]
       ) 
   where  
   protonm fs = rename fs ("ctx" ++ name fs) --rename to ensure unique name of php page (there can be concept names or plurals of them equal to context name)

interfaceGenProto :: Fspc -> Options -> IO()
interfaceGenProto    fSpec flags
  = (writeFile outputFile $ showADL strippedfspec)
    >> verboseLn flags ("Ampersand-script written to " ++ outputFile ++ ".")
    where   
    --do not print interfaces (yet) with prototype.exe --export.
    --prototype --export is an export of the Atlas DB.
    --use ampersand --export to get generated interfaces etc in an adl file
    strippedfspec = fSpec -- {fInterfaces=[]} 
    outputFile = combine (dirOutput flags) (outputfile flags)

               
doGenProto :: Fspc -> Options -> IO()
doGenProto fSpec flags
   =  verboseLn flags "Checking on rule violations..."
     >> if not (theme flags==StudentTheme) && (not.null) (violations fSpec) 
        then verboseLn flags explainviols else verboseLn flags "No violations found." 
     >> verboseLn flags "Generating prototype..."
     >> phpObjInterfaces fSpec flags  
     >> verboseLn flags ("Prototype files have been written to " ++  (dirPrototype flags) ++ "." )
     >> if (test flags) then verboseLn flags (show (vplugInfos fSpec)) else verboseLn flags ""
     where  
     explainviols = concat [show p++": "++showADL r++"\n" |(r,p)<-violations fSpec]


{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad
import Data.List
import Data.Function (on)
import System.FilePath        (combine,dropFileName,takeBaseName)
import System.Directory       (getDirectoryContents)
import Prelude hiding (putStr,readFile,writeFile)
import DatabaseDesign.Ampersand_Prototype.ObjBinGen    (phpObjInterfaces)
import DatabaseDesign.Ampersand_Prototype.Apps         (picturesForAtlas)
import DatabaseDesign.Ampersand_Prototype.Apps.Atlas   (atlas2context)
import DatabaseDesign.Ampersand_Prototype.CoreImporter
import DatabaseDesign.Ampersand_Prototype.Version
import DatabaseDesign.Ampersand_Prototype.Apps.ADL1Importable
import DatabaseDesign.Ampersand_Prototype.GenBericht
 
fatal :: Int -> String -> a
fatal = fatalMsg "Main"

main :: IO ()
main
 = do opts <- getOptions
      if showVersion opts || showHelp opts
       then mapM_ putStr (helpNVersionTexts prototypeVersionStr opts)
       else do (cx,err) <- parseAndTypeCheck opts
               if nocxe err 
                 then let fspc = makeFspec opts cx
                      in  generateProtoStuff opts fspc
                 else putStr (show err) 
  where
  parseAndTypeCheck :: Options -> IO(A_Context,CtxError) 
  parseAndTypeCheck opts 
   = let fn = importfile opts
         thepCtx (Right pCtx) = pCtx
         thepCtx (Left err)   = error $ "Parse error:\n"++show err
     in
     do ePCtxErr <- parseCtxM_ opts
        pPops <- if null fn then return [] else
                 do popsText <- readFile fn
                    case importformat opts of
                       Adl1PopFormat -> parsePopsM_ popsText opts fn
                       Adl1Format -> do verbose opts ("Importing ADL1 file "++fn++"... ")
                                        cx <- parseCtxM_ opts
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
                                                   = [P_Popu{ p_popm=getr r, p_type=P_Sign [], p_popps=[mkPair fn (name fspec),mkPair fnnxt fnnxt]}]
                                                  loadedfile r
                                                   = [P_Popu{ p_popm=getr r, p_type=P_Sign [], p_popps=[mkPair usr fn]         } | not (null usr)]
                                                  -- uploadfile r        = [P_Popu{ p_popm=getr r, p_type=[], p_popps=[mkPair usr "browse"]   } | not (null usr)]
                                                  --TODO -> the user has more files, how do I get them in this population
                                                  fileof r myfiles
                                                   = [P_Popu{ p_popm=getr r, p_type=P_Sign [], p_popps=[mkPair (combine fdir f) usr | f<-myfiles, not (null usr)] }]
                                                  contextfunction r x
                                                   = [P_Popu{ p_popm=getr r, p_type=P_Sign [], p_popps=[mkPair (name fspec) x] }]
                                              in
                                              do verbose opts "writing pictures for atlas... "
                                                 sequence_ [writePicture opts pict | pict <- picturesForAtlas opts fspec]
                                                 verbose opts "pictures for atlas written... "
                                                 myfiles <- liftM (filter (`notElem` [".", ".."])) (getDirectoryContents fdir)
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
    
generateProtoStuff :: Options -> Fspc -> IO ()
generateProtoStuff opts fSpec =
 do { verboseLn opts "Generating..."
    ; when (genPrototype opts) $ doGenProto fSpec opts
    ; when (genBericht opts)   $ doGenBericht fSpec opts 
    ; sequence_ [ ruleTest fSpec opts ruleName | Just ruleName <- [testRule opts]] 
    ; when ((not . null $ violations fSpec) && (development opts || theme opts==StudentTheme)) $
        verboseLn opts "\nWARNING: There are rule violations (see above)."
    ; verboseLn opts "Done."  -- if there are violations, but we generated anyway (ie. with --dev or --theme=student), issue a warning
    }

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
      then putStrLn "\nERROR: No prototype generated because of rule violations.\n(Compile with --dev to generate a prototype regardless of violations)" 
      else do { verboseLn opts "Generating prototype..."
              ; phpObjInterfaces fSpec opts  
              ; verboseLn opts $ "Prototype files have been written to " ++ dirPrototype opts ++ "."
              ; if test opts then verboseLn opts $ show (vplugInfos fSpec) else verboseLn opts ""
              }
    }
 where reportViolations []    = verboseLn opts "No violations found."
       reportViolations viols =
         let ruleNamesAndViolStrings = [ (name r, show p) | (r,p) <- viols ]
         in  putStrLn $ intercalate "\n"
                          [ "Violations of rule "++show r++":\n"++ concatMap (\(_,p) -> "- "++ p ++"\n") rps 
                          | rps@((r,_):_) <- groupBy (on (==) fst) $ sort ruleNamesAndViolStrings
                          ]


ruleTest :: Fspc -> Options -> String -> IO ()
ruleTest fSpec _ ruleName =
 case [ rule | rule <- grules fSpec ++ vrules fSpec, name rule == ruleName ] of
   [] -> putStrLn $ "\nRule test error: rule "++show ruleName++" not found." 
   (rule:_) -> do { putStrLn $ "\nContents of rule "++show ruleName++ ": "++showADL (rrexp rule)
                  ; putStrLn $ showContents rule
                  ; let ruleComplement = rule { rrexp = ECpl $ EBrk $rrexp rule }
                  ; putStrLn $ "\nViolations of "++show ruleName++" (contents of "++showADL (rrexp ruleComplement)++"):"
                  ; putStrLn $ showContents ruleComplement
                  } 
 where showContents rule = let pairs = [ "("++f++"," ++s++")" | (f,s) <- ruleviolations $ rule { rrexp = ECpl $ rrexp rule }]
                           in  "[" ++ intercalate ", " pairs ++ "]" -- getting the contents is not implemented for rules in Rules.hs, 
                                                                    -- so we get the violations of the complement instead
       
    

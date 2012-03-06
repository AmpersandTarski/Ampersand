{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad
import Data.List
import Data.Function (on)
import System.FilePath        (combine,dropFileName)
import System.Directory       (getDirectoryContents)
import Prelude hiding (putStr,readFile,writeFile)
import DatabaseDesign.Ampersand_Prototype.ObjBinGen    (phpObjInterfaces)
import DatabaseDesign.Ampersand_Prototype.Apps         (picturesForAtlas)
import DatabaseDesign.Ampersand_Prototype.Apps.RAP   (atlas2context)
import DatabaseDesign.Ampersand_Prototype.Apps.RAPImport
import DatabaseDesign.Ampersand_Prototype.CoreImporter
import DatabaseDesign.Ampersand_Prototype.Version
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
     do ePCtxErr <- parseCtxM_ opts fileName
        pPops <- if null fn then return [] else
                 do popsText <- readFile fn
                    case importformat opts of
                       Adl1PopFormat -> parsePopsM_ popsText opts fn
                       Adl1Format -> do verbose opts ("Importing "++fn++" in RAP... ")
                                        cx <- parseCtxM_ opts importfile
                                        if nocxe (snd(typeCheck (thepCtx cx) [])) 
                                         then let fspec = makeFspec opts (fst(typeCheck (thepCtx cx) [])) -- the fspec of the file to import in RAP
                                                  pics = picturesForAtlas opts fspec
                                                  fdir = let d=dropFileName fn in if null d then "." else d
                                              in
                                              do verbose opts "Writing pictures for RAP... "
                                                 sequence_ [writePicture opts pict | pict <- pics]
                                                 verbose opts "Getting files of user... "
                                                 myfiles <- liftM (filter (`notElem` [".", ".."])) (getDirectoryContents fdir)
                                                 return (makeRAPPops fspec myfiles pics)
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
       
    

{-# OPTIONS_GHC -Wall #-}
-- This module provides an interface to be able to parse a scritp and to 
-- return an Fspec, conform the user defined flags. 
-- This might include that RAP is included in the returned Fspec. 
module DatabaseDesign.Ampersand.InputProcessing (
   createFspec
)
where
import qualified DatabaseDesign.Ampersand.Basics as Basics
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.ADL1.P2A_Converters
import DatabaseDesign.Ampersand.Input.ADL1.UU_Scanner -- (scan,initPos)
import DatabaseDesign.Ampersand.Input.ADL1.UU_Parsing --  (getMsgs,parse,evalSteps,parseIO)
import DatabaseDesign.Ampersand.Input.ADL1.Parser
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Input.ADL1.CtxError (CtxError(PE))
import Data.List
import System.Directory
import System.FilePath
import Paths_ampersand
import Control.Monad
import Data.Traversable (sequenceA)

fatal :: Int -> String -> a
fatal = Basics.fatalMsg "InputProcessing"

-- | create an Fspec, based on the user defined flags. 
createFspec :: Options  -- ^The options derived from the command line 
            -> IO(Guarded Fspc) 
createFspec flags = 
  do userCtx <- parseWithIncluded flags (fileName flags)
     bothCtx <- if includeRap flags
                then do dataDir <- getDataDir
                        let rapFile = dataDir </> "AmpersandData" </> "RepoRap" </> "RAP.adl" 
                        exists <- doesFileExist rapFile
                        when (not exists) (fatal 39 $ "RAP file isn't installed properly. RAP.adl expected at:"
                                                    ++"\n  "++show rapFile
                                                    ++"\n  (You might want to reinstall ampersand...)") 
                        rapCtx <- parseWithIncluded flags rapFile
                        popsCtx <- popsCtxOf userCtx
                        case sequenceA [userCtx, rapCtx, popsCtx] of
                           Errors err -> return (Errors err)
                           Checked ps -> return (Checked 
                                               (foldr mergeContexts emptyContext ps))
                else return userCtx
     case bothCtx of
        Errors err -> return (Errors err)
        Checked pCtx
           -> do let (gaCtx) = pCtx2aCtx pCtx 
                 -- when  (typeGraphs flags) (showGraphs stTypeGraph condensedGraph)
                 case gaCtx of
                   (Errors  err ) -> return (Errors err)
                   (Checked aCtx) -> return (Checked (makeFspec flags aCtx ))
  where
-- For the purpose of debugging the type checker, or for educational purposes, the switch "--typing" can be used.
-- It prints three graphs. For an explanation of those graphs, consult the corresponding papers (yet to be written).
-- Use only for very small scripts, or else the results will not be very informative.
-- For the large scripts that are used in projects, the program may abort due to insufficient resources.
    {-
    showGraphs stTypeGraph condensedGraph
      = do condensedGraphPath<-runGraphvizCommand Dot condensedGraph Png (replaceExtension ("Condensed_Graph_of_"++baseName flags) ".png")
           verboseLn flags (condensedGraphPath++" written.")
           stDotGraphPath<-runGraphvizCommand Dot stTypeGraph Png (replaceExtension ("stGraph_of_"++baseName flags) ".png")
           verboseLn flags (stDotGraphPath++" written.") -}
    popsCtxOf :: Guarded P_Context ->IO(Guarded P_Context)
    popsCtxOf gp =
     (case gp of
       Errors _ -> return (Errors []) -- The errors are already in the error list
       Checked pCtx 
         -> case pCtx2aCtx pCtx of
              (Errors  err ) -> return (Errors err)
              (Checked aCtx)
                 -> do let fspc = makeFspec flags aCtx
                           popScript = meatGrinder flags fspc
                       when (genMeat flags) 
                          (do let (nm,content) = popScript
                                  outputFile = combine (dirOutput flags) $ replaceExtension nm ".adl"
                              writeFile outputFile content
                              verboseLn flags $ "Meta population written into " ++ outputFile ++ "."
                          )
                       case parse1File2pContext popScript of
                         (Errors  err) -> fatal 64 ("MeatGrinder has errors!" 
                                                 ++ intercalate "\n"(map showErr err))
                         (Checked (pCtx,[])) -> return (Checked pCtx)
                         (Checked (_,includes)) -> fatal 67 "Meatgrinder returns included file????"
     )

getRapCtxt :: Options ->IO(Guarded P_Context)
getRapCtxt flags = 
  do dataDir <- getDataDir
     let rapFile = dataDir </> "AmpersandData" </> "RepoRap" </> "RAP.adl" 
     exists <- doesFileExist rapFile
     when (not exists) (fatal 39 $ "RAP file isn't installed properly. RAP.adl expected at:"
                              ++"\n  "++show rapFile
                              ++"\n  (You might want to reinstall ampersand...)") 
     parseWithIncluded flags rapFile
     
-------------
type FileContent = (FilePath, String) -- The name and its contents
type ParseResult = (P_Context, [FilePath]) -- A positive parse of a single file deliverse a P_Context and a list of includedes

parseWithIncluded :: Options -> FilePath -> IO(Guarded P_Context)
parseWithIncluded flags f = tailRounds [] (emptyContext,[f])
 where
    tailRounds :: [FileContent] -- the files already processed
               -> ParseResult   -- the result so far.  
               -> IO(Guarded P_Context) 
    tailRounds dones (pCtx, names) =
     do let filesToProcessThisRound = [f | f<-names, f `notElem` map fst dones]
        case filesToProcessThisRound of 
         []    -> do return (Checked pCtx)
         newNs -> do fs <- readFiles newNs
                     res <- oneRound fs dones pCtx
                     case res of
                             Errors err -> return (Errors err)
                             Checked (pCtx',included)
                                -> tailRounds (nub ( dones++fs)) (pCtx', included)
    readFiles :: [FilePath] -> IO [FileContent]
    readFiles fs = mapM readFile fs
     where
       readFile f = 
          do verboseLn flags ("reading "++f)
             content <- Basics.readFile f
             return (f,content)
    oneRound :: [FileContent] -> [FileContent] -> P_Context -> IO(Guarded ParseResult)
    oneRound todos dones pCtx = 
      do return (parseNext todos dones pCtx)
     where 
       parseNext :: [FileContent] -- Files that have not been parsed yet
                 -> [FileContent] -- Files that have been parsed already
                 -> P_Context     -- The result so far from previous parsed files
                 -> Guarded ParseResult
       parseNext todos dones pCtx =
         case nub [f | f <- todos, f `notElem` dones] of
           [] -> Checked (pCtx,[])
           fs -> case sequenceA (map parse1File2pContext fs) of
                   Checked prs -> Checked ( foldl mergeContexts pCtx (map fst prs)
                                          , concatMap snd prs)
                   Errors errs -> Errors errs
parse1File2pContext :: FileContent -> Guarded ParseResult
parse1File2pContext (fPath, fContent) =
   let scanner = scan keywordstxt keywordsops specialchars opchars fPath initPos
       steps :: Steps (Pair ParseResult (Pair [Token] a)) Token
       steps = parse pContext (scanner fContent)
   in  case  getMsgs steps of
         []  -> let Pair (pCtx,includes) _ = evalSteps steps 
                in Checked (pCtx,map normalize includes)
         msgs-> Errors (map PE msgs)
  where
  normalize ::FilePath -> FilePath
  normalize name = (takeDirectory fPath) </> name


 
emptyContext :: P_Context
emptyContext = PCtx "" [] Nothing Nothing [] [] [] [] [] [] [] [] [] [] [] [] [] [] []

mergeContexts :: P_Context -> P_Context -> P_Context
mergeContexts (PCtx nm1 pos1 lang1 markup1 thms1 pats1 pprcs1 rs1 ds1 cs1 ks1 vs1 gs1 ifcs1 ps1 pops1 sql1 php1 metas1)
              (PCtx nm2 pos2 lang2 markup2 thms2 pats2 pprcs2 rs2 ds2 cs2 ks2 vs2 gs2 ifcs2 ps2 pops2 sql2 php2 metas2) =
  PCtx{ ctx_nm  = if null nm1 then nm2 else nm1
      , ctx_pos = pos1 ++ pos2
      , ctx_lang = lang1
      , ctx_markup = markup1
      , ctx_thms = thms1 ++ thms2
      , ctx_pats = pats1 ++ pats2
      , ctx_PPrcs = pprcs1 ++ pprcs2
      , ctx_rs = rs1 ++ rs2
      , ctx_ds = ds1 ++ ds2
      , ctx_cs = cs1 ++ cs2
      , ctx_ks = ks1 ++ ks2
      , ctx_vs = vs1 ++ vs2
      , ctx_gs = gs1 ++ gs2
      , ctx_ifcs = ifcs1 ++ ifcs2
      , ctx_ps = ps1 ++ ps2
      , ctx_pops = pops1 ++ pops2
      , ctx_sql = sql1 ++ sql2
      , ctx_php = php1 ++ php2
      , ctx_metas = metas1 ++ metas2
      }

{-
-- | Parse isolated ADL1 expression strings
parseADL1pExpr :: String -> String -> Guarded (Term PrimTerm)
parseADL1pExpr filename input = 
  let scanner = scan keywordstxt keywordsops specialchars opchars filename initPos
      steps :: Steps (Pair Term (Pair [Token] a)) Token
      steps = parse pTerm $ scanner input
  in  case  getMsgs steps of
        []   -> Checked $ let Pair result _ = evalSteps steps in result
        msgs -> Errors (map PE msgs)
-}

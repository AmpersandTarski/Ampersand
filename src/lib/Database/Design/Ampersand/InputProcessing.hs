{-# OPTIONS_GHC -Wall #-}
-- This module provides an interface to be able to parse a scritp and to 
-- return an Fspec, conform the user defined flags. 
-- This might include that RAP is included in the returned Fspec. 
module Database.Design.Ampersand.InputProcessing (
   createFspec
)
where
import qualified Database.Design.Ampersand.Basics as Basics
import Database.Design.Ampersand.Fspec
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.ADL1.P2A_Converters
import Database.Design.Ampersand.Input.ADL1.UU_Scanner -- (scan,initPos)
import Database.Design.Ampersand.Input.ADL1.UU_Parsing --  (getMsgs,parse,evalSteps,parseIO)
import Database.Design.Ampersand.Input.ADL1.Parser
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.Input.ADL1.CtxError (CtxError(PE))
import Data.List
import System.Directory
import System.FilePath
import Control.Monad
import Data.Traversable (sequenceA)
import Control.Applicative

fatal :: Int -> String -> a
fatal = Basics.fatalMsg "InputProcessing"

-- | create an Fspec, based on the user defined flags. 
createFspec :: Options  -- ^The options derived from the command line 
            -> IO(Guarded Fspc) 
createFspec flags = 
  do userCtx <- parseWithIncluded flags (fileName flags)
     bothCtx <- if includeRap flags
                then do let rapFile = ampersandDataDir flags </> "FormalAmpersand" </> "FormalAmpersand.adl" 
                        exists <- doesFileExist rapFile
                        when (not exists) (fatal 39 $ "Ampersand isn't installed properly. Formal specification of Ampersand expected at:"
                                                    ++"\n  "++show rapFile
                                                    ++"\n  (You might want to re-install ampersand...)") 
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
                 case gaCtx of
                   (Errors  err ) -> return (Errors err)
                   (Checked aCtx) -> return (Checked (makeFspec flags aCtx ))
  where
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
                         (Checked (pctx,[])) -> return (Checked pctx)
                         (Checked _ )        -> fatal 67 "Meatgrinder returns included file????"
     )

     
-------------
type FileContent = (FilePath, String) -- The name and its contents
type ParseResult = (P_Context, [FilePath]) -- A positive parse of a single file deliverse a P_Context and a list of includedes

parseWithIncluded :: Options -> FilePath -> IO(Guarded P_Context)
parseWithIncluded flags fp = tailRounds [] (emptyContext,[fp])
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
    readFiles fs = mapM readFile' fs
     where
       readFile' f = 
          do verboseLn flags ("reading "++f)
             content <- Basics.readFile f
             return (f,content)
    oneRound :: [FileContent] -- Files that have not been parsed yet
             -> [FileContent] -- Files that have been parsed already
             -> P_Context     -- The result so far from previous parsed files
             -> IO(Guarded ParseResult)
    oneRound todos dones pCtx = pure parseNext
     where 
       parseNext :: Guarded ParseResult
       parseNext  =
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
  normalize name = takeDirectory fPath </> name


 
emptyContext :: P_Context
emptyContext = PCtx "" [] Nothing Nothing [] [] [] [] [] [] [] [] [] [] [] [] [] [] []

mergeContexts :: P_Context -> P_Context -> P_Context
mergeContexts (PCtx nm1 pos1 lang1 markup1 thms1 pats1 pprcs1 rs1 ds1 cs1 ks1 vs1 gs1 ifcs1 ps1 pops1 sql1 php1 metas1)
              (PCtx nm2 pos2 lang2 markup2 thms2 pats2 pprcs2 rs2 ds2 cs2 ks2 vs2 gs2 ifcs2 ps2 pops2 sql2 php2 metas2) =
  PCtx{ ctx_nm     = if null nm1 then nm2 else nm1
      , ctx_pos    = pos1 ++ pos2
      , ctx_lang   = lang1   `orElse` lang2   `orElse` Nothing
      , ctx_markup = markup1 `orElse` markup2 `orElse` Nothing
      , ctx_thms   = thms1 ++ thms2
      , ctx_pats   = pats1 ++ pats2
      , ctx_PPrcs  = pprcs1 ++ pprcs2
      , ctx_rs     = rs1 ++ rs2
      , ctx_ds     = ds1 ++ ds2
      , ctx_cs     = cs1 ++ cs2
      , ctx_ks     = ks1 ++ ks2
      , ctx_vs     = vs1 ++ vs2
      , ctx_gs     = gs1 ++ gs2
      , ctx_ifcs   = ifcs1 ++ ifcs2
      , ctx_ps     = ps1 ++ ps2
      , ctx_pops   = pops1 ++ pops2
      , ctx_sql    = sql1 ++ sql2
      , ctx_php    = php1 ++ php2
      , ctx_metas  = metas1 ++ metas2
      }

-- | Left-biased choice on maybes
orElse :: Maybe a -> Maybe a -> Maybe a
x `orElse` y = case x of
                 Just _  -> x
                 Nothing -> y


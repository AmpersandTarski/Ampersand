{-# OPTIONS_GHC -Wall #-}
-- This module provides an interface to be able to parse a script and to
-- return an Fspec, as tuned by the command line options.
-- This might include that RAP is included in the returned Fspec.
module Database.Design.Ampersand.InputProcessing (
   createFspec
)
where
import qualified Database.Design.Ampersand.Basics as Basics
import Database.Design.Ampersand.Fspec
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.ADL1.P2A_Converters
import Database.Design.Ampersand.Input.ADL1.UU_Scanner
import UU.Parsing (getMsgs,parse,evalSteps,Pair(..))
import Database.Design.Ampersand.Input.ADL1.Parser
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.Input.ADL1.CtxError
import Database.Design.Ampersand.Fspec.ToFspec.ADL2Plug (showPlug)
import Data.List
import System.Directory
import System.FilePath
import Control.Monad
import Data.Traversable (sequenceA)

fatal :: Int -> String -> a
fatal = Basics.fatalMsg "InputProcessing"

-- | create an Fspec, based on the user defined flags.
createFspec :: Options  -- ^The options derived from the command line
            -> IO(Guarded Fspc)
createFspec opts =
  do userCtx <- parseADL opts (fileName opts)
     bothCtx <- if includeRap opts
                then do let rapFile = ampersandDataDir opts </> "FormalAmpersand" </> "FormalAmpersand.adl"
                        exists <- doesFileExist rapFile
                        when (not exists) (fatal 39 $ "Ampersand isn't installed properly. Formal specification of Ampersand expected at:"
                                                    ++"\n  "++show rapFile
                                                    ++"\n  (You might want to re-install ampersand...)")
                        rapCtx <- parseADL opts rapFile
                        popsCtx <- popsCtxOf userCtx
                        case userCtx of 
                          Errors err   -> return (Errors err)
                          Checked uCtx -> case sequenceA [rapCtx, popsCtx] of
                                            Errors err -> return (Errors err)
                                            Checked ctxs -> return (Checked $ foldr mergeContexts uCtx ctxs)
                else return userCtx
     case bothCtx of
        Errors err -> return (Errors err)
        Checked pCtx
           -> do let (gaCtx) = pCtx2aCtx opts pCtx
                 case gaCtx of
                   (Errors  err ) -> return (Errors err)
                   (Checked aCtx) -> 
                    do { let fSpec = makeFspec opts aCtx
                       ; when (development (flags fSpec)) $
                          do { putStrLn "Table structure for internal plugs:\n"
                             ; putStrLn $ (unlines . concat) [showPlug plug | InternalPlug plug <- plugInfos fSpec]
                             }
                                      
                       ; return $ Checked fSpec
                       }
  where
    popsCtxOf :: Guarded P_Context ->IO(Guarded P_Context)
    popsCtxOf gp =
     (case gp of
       Errors _ -> return (Errors []) -- The errors are already in the error list
       Checked pCtx
         -> case pCtx2aCtx opts pCtx of
              (Errors  err ) -> return (Errors err)
              (Checked aCtx)
                 -> do let fSpec = makeFspec opts aCtx
                           (popFilePath,popContents) = meatGrinder fSpec
                       when (genMeat opts) $
                          do let outputFile = combine (dirOutput opts) $ replaceExtension popContents ".adl"
                             writeFile outputFile popContents
                             verboseLn opts $ "Meta population written into " ++ outputFile ++ "."
                          
                       case runParser pContext popFilePath popContents of
                         (Errors  err) -> fatal 64 ("MeatGrinder has errors!"
                                                 ++ intercalate "\n"(map showErr err))
                         (Checked (pctx,[])) -> return (Checked pctx)
                         (Checked _ )        -> fatal 67 "Meatgrinder returns included file????"
     )

-- Parse an ADL file and all transitive includes
parseADL  :: Options -> FilePath -> IO (Guarded P_Context)
parseADL opts filePath =
  whenCheckedIO (parseSingleADL opts filePath) $ \(ctxt, filePaths) ->
    whenCheckedIO (parseADLs opts [filePath] filePaths) $ \ctxts ->
      return $ Checked $ foldl mergeContexts ctxt ctxts

parseADLs :: Options -> [FilePath] -> [FilePath] -> IO (Guarded [P_Context])
parseADLs _    _               []        = return $ Checked []
parseADLs opts parsedFilePaths filePaths =
 do { let filePathsToParse = nub filePaths \\ parsedFilePaths
    ; whenCheckedIO (fmap sequenceA $ mapM (parseSingleADL opts) filePathsToParse) $ \ctxtNewFilePathss ->
       do { let (ctxts, newFilessToParse) = unzip ctxtNewFilePathss
          ; whenCheckedIO (parseADLs opts (parsedFilePaths ++ filePaths) $ concat newFilessToParse) $ \ctxts' ->
              return $ Checked $ ctxts ++ ctxts'
          }
    }

-- Parse an ADL file, but not its includes (which are simply returned as a list)
parseSingleADL :: Options -> FilePath -> IO (Guarded (P_Context, [FilePath]))
parseSingleADL opts filePath =
 do { verboseLn opts $ "Reading file " ++ filePath
    ; fileContents <- Basics.readFile filePath
    ; whenCheckedIO (return $ runParser pContext filePath fileContents) $ \(ctxts,relativePaths) -> 
       do { filePaths <- mapM normalizePath relativePaths
          ; return $ Checked (ctxts, filePaths)
          }
    }
 where normalizePath relativePath = canonicalizePath $ takeDirectory filePath </> relativePath 

runParser :: AmpParser res -> String -> String -> Guarded res
runParser parser filename input =
  let scanner = scan keywordstxt keywordsops specialchars opchars filename initPos
      steps = parse parser (scanner input)
  in  case  getMsgs steps of
         []    -> let Pair res _ = evalSteps steps
                  in  Checked res
         msg:_ -> Errors [PE msg]

mergeContexts :: P_Context -> P_Context -> P_Context
mergeContexts (PCtx nm1 pos1 lang1 markup1 thms1 pats1 pprcs1 rs1 ds1 cs1 ks1 vs1 gs1 ifcs1 ps1 pops1 sql1 php1 metas1)
              (PCtx nm2 pos2 _     markup2 thms2 pats2 pprcs2 rs2 ds2 cs2 ks2 vs2 gs2 ifcs2 ps2 pops2 sql2 php2 metas2) =
  PCtx{ ctx_nm     = if null nm1 then nm2 else nm1
      , ctx_pos    = pos1 ++ pos2
      , ctx_lang   = lang1 -- By taking the first, we end up with the language of the top-level context
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
